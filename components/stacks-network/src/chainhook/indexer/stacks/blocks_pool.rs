use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::chainhook::types::{
    BlockIdentifier, StacksBlockData, StacksBlockUpdate, StacksChainEvent,
    StacksChainUpdatedWithBlocksData, StacksChainUpdatedWithMicroblocksData,
    StacksChainUpdatedWithMicroblocksReorgData, StacksChainUpdatedWithReorgData,
    StacksMicroblockData,
};
use hiro_system_kit::slog;

use crate::chainhook::indexer::fork_scratch_pad::CONFIRMED_SEGMENT_MINIMUM_LENGTH;
use crate::chainhook::indexer::{ChainSegment, ChainSegmentIncompatibility};
use crate::chainhook::utils::Context;

pub struct StacksBlockPool {
    canonical_fork_id: usize,
    highest_competing_fork_height_delta: Option<u16>,
    orphans: BTreeSet<BlockIdentifier>,
    block_store: HashMap<BlockIdentifier, StacksBlockData>,
    forks: BTreeMap<usize, ChainSegment>,
    microblock_store: HashMap<(BlockIdentifier, BlockIdentifier), StacksMicroblockData>,
    micro_forks: HashMap<BlockIdentifier, Vec<ChainSegment>>,
    micro_orphans: BTreeSet<(BlockIdentifier, BlockIdentifier)>,
    canonical_micro_fork_id: HashMap<BlockIdentifier, usize>,
}

impl Default for StacksBlockPool {
    fn default() -> Self {
        Self::new()
    }
}

impl StacksBlockPool {
    pub fn new() -> StacksBlockPool {
        let mut forks = BTreeMap::new();
        forks.insert(0, ChainSegment::new());
        StacksBlockPool {
            canonical_fork_id: 0,
            highest_competing_fork_height_delta: None,
            block_store: HashMap::new(),
            orphans: BTreeSet::new(),
            forks,
            microblock_store: HashMap::new(),
            micro_forks: HashMap::new(),
            micro_orphans: BTreeSet::new(),
            canonical_micro_fork_id: HashMap::new(),
        }
    }

    pub fn get_canonical_fork_chain_tip(&self) -> Option<&BlockIdentifier> {
        match self.forks.get(&self.canonical_fork_id) {
            Some(fork) => Some(fork.get_tip()),
            None => None,
        }
    }

    pub fn seed_block_pool(&mut self, blocks: Vec<StacksBlockData>, ctx: &Context) {
        ctx.try_log(|logger| {
            slog::info!(logger, "Seeding block pool with {} blocks", blocks.len())
        });
        for block in blocks.into_iter() {
            let existing_entry = self.block_store.get(&block.block_identifier.clone());
            if existing_entry.is_some() {
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Seeding block pool: Stacks {} has already been processed; skipping",
                        block.block_identifier
                    )
                });
                continue;
            }

            match self.process_block(block, ctx) {
                Ok(_) => {}
                Err(e) => {
                    ctx.try_log(|logger| slog::info!(logger, "Error seeding block pool: {}", e));
                }
            }
        }
    }

    pub fn process_block(
        &mut self,
        block: StacksBlockData,
        ctx: &Context,
    ) -> Result<Option<StacksChainEvent>, String> {
        ctx.try_log(|logger| {
            slog::info!(logger, "Start processing Stacks {}", block.block_identifier)
        });

        // Keep block data in memory
        let existing_entry = self
            .block_store
            .insert(block.block_identifier.clone(), block.clone());
        if existing_entry.is_some() {
            ctx.try_log(|logger| {
                slog::warn!(
                    logger,
                    "Stacks {} has already been processed",
                    block.block_identifier
                )
            });
            return Ok(None);
        }

        for (i, fork) in self.forks.iter() {
            ctx.try_log(|logger| slog::info!(logger, "Active fork {}: {}", i, fork));
        }
        // Retrieve previous canonical fork
        let previous_canonical_fork_id = self.canonical_fork_id;
        let previous_canonical_fork = match self.forks.get(&previous_canonical_fork_id) {
            Some(fork) => fork.clone(),
            None => {
                ctx.try_log(|logger| {
                    slog::error!(logger, "unable to retrieve previous stacks fork")
                });
                return Err("unable to retrieve previous stacks fork".to_string());
            }
        };

        let mut fork_updated = None;
        for (_, fork) in self.forks.iter_mut() {
            let (block_appended, mut new_fork) = fork.try_append_block(&block, ctx);
            if block_appended {
                if let Some(new_fork) = new_fork.take() {
                    let number_of_forks = self.forks.len();
                    let mut next_fork_id = 0;
                    for (index, (fork_id, _)) in self.forks.iter().enumerate() {
                        if (index + 1) == number_of_forks {
                            next_fork_id = fork_id + 1;
                        }
                    }
                    self.forks.insert(next_fork_id, new_fork);
                    fork_updated = self.forks.get_mut(&next_fork_id);
                } else {
                    fork_updated = Some(fork);
                }
                // A block can only be added to one segment
                break;
            }
        }

        let fork_updated = match fork_updated.take() {
            Some(fork) => {
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Stacks {} successfully appended to {}",
                        block.block_identifier,
                        fork
                    )
                });
                fork
            }
            None => {
                ctx.try_log(|logger| {
                    slog::error!(
                        logger,
                        "Unable to process Stacks {} - inboxed for later",
                        block.block_identifier
                    )
                });
                self.orphans.insert(block.block_identifier.clone());
                return Ok(None);
            }
        };

        // Process former orphans
        let orphans = self.orphans.clone();
        let mut orphans_to_untrack = HashSet::new();

        let mut at_least_one_orphan_appended = true;
        // As long as we are successful appending blocks that were previously unprocessable,
        // Keep looping on this backlog
        let mut applied = HashSet::new();
        while at_least_one_orphan_appended {
            at_least_one_orphan_appended = false;
            for orphan_block_identifier in orphans.iter() {
                if applied.contains(orphan_block_identifier) {
                    continue;
                }
                let block = match self.block_store.get(orphan_block_identifier) {
                    Some(block) => block.clone(),
                    None => continue,
                };

                let (orphan_appended, _new_fork) = fork_updated.try_append_block(&block, ctx);
                if orphan_appended {
                    applied.insert(orphan_block_identifier);
                    orphans_to_untrack.insert(orphan_block_identifier);
                }
                at_least_one_orphan_appended = at_least_one_orphan_appended || orphan_appended;
            }
        }

        // Update orphans
        for orphan in orphans_to_untrack.into_iter() {
            ctx.try_log(|logger| slog::info!(logger, "Dequeuing orphan {}", orphan));
            self.orphans.remove(orphan);
        }

        // Select canonical fork
        let mut canonical_fork_id = 0;
        let mut highest_height = 0;
        let mut highest_bitcoin_height = 0;
        // we want to track the chain tip of all of the known competing forks
        let mut highest_heights = vec![];
        for (fork_id, fork) in self.forks.iter() {
            let tip_bitcoin_height = self
                .block_store
                .get(fork.get_tip())
                .map(|b| b.metadata.bitcoin_anchor_block_identifier.index)
                .unwrap_or(0);
            ctx.try_log(|logger| {
                slog::info!(
                    logger,
                    "Active fork: {} - {} / {}",
                    fork_id,
                    fork,
                    tip_bitcoin_height
                )
            });
            let tip_height = fork.get_tip().index;
            highest_heights.push(tip_height); // todo (I think we need to double-check reasoning on this)

            // the tip of the canonical stacks chain must belong to the bitcoin fork with the highest tip
            if tip_bitcoin_height > highest_bitcoin_height
                || (tip_bitcoin_height == highest_bitcoin_height && fork_id > &canonical_fork_id)
            {
                highest_bitcoin_height = tip_bitcoin_height;
                if tip_height >= highest_height {
                    highest_height = tip_height;
                    canonical_fork_id = *fork_id;
                }
            }
        }
        highest_heights.sort();
        let len = highest_heights.len();
        self.highest_competing_fork_height_delta = if len > 1 {
            // canonical - next highest
            let t = (highest_heights[len - 1] - highest_heights[len - 2])
                .try_into()
                .map_err(|e| format!("unable to retrieve competing fork height: {}", e))?;
            Some(t)
        } else {
            None
        };
        ctx.try_log(|logger| {
            slog::info!(
                logger,
                "Highest competing fork height delta computed as {} with data {:?}",
                self.highest_competing_fork_height_delta.unwrap_or(0),
                highest_heights
            )
        });

        ctx.try_log(|logger| {
            slog::info!(
                logger,
                "Active fork selected as canonical: {}",
                canonical_fork_id
            )
        });

        self.canonical_fork_id = canonical_fork_id;
        // Generate chain event from the previous and current canonical forks
        let canonical_fork = self.forks.get(&canonical_fork_id).unwrap().clone();
        if canonical_fork.eq(&previous_canonical_fork) {
            ctx.try_log(|logger| slog::info!(logger, "Canonical fork unchanged"));
            return Ok(None);
        }

        let res = self.generate_block_chain_event(&canonical_fork, &previous_canonical_fork, ctx);
        let mut chain_event = match res {
            Ok(chain_event) => chain_event,
            Err(ChainSegmentIncompatibility::ParentBlockUnknown) => {
                self.canonical_fork_id = previous_canonical_fork_id;
                return Ok(None);
            }
            _ => return Ok(None),
        };

        match self.highest_competing_fork_height_delta {
            None => {
                self.collect_and_prune_confirmed_blocks(&mut chain_event, ctx);
            }
            Some(e) if e > 6 => {
                self.collect_and_prune_confirmed_blocks(&mut chain_event, ctx);
            }
            _ => {}
        }

        Ok(Some(chain_event))
    }

    pub fn collect_and_prune_confirmed_blocks(
        &mut self,
        chain_event: &mut StacksChainEvent,
        ctx: &Context,
    ) {
        let (tip, confirmed_blocks) = match chain_event {
            StacksChainEvent::ChainUpdatedWithBlocks(ref mut event) => {
                match event.new_blocks.last() {
                    Some(tip) => (
                        tip.block.block_identifier.clone(),
                        &mut event.confirmed_blocks,
                    ),
                    None => return,
                }
            }
            StacksChainEvent::ChainUpdatedWithReorg(ref mut event) => {
                match event.blocks_to_apply.last() {
                    Some(tip) => (
                        tip.block.block_identifier.clone(),
                        &mut event.confirmed_blocks,
                    ),
                    None => return,
                }
            }
            _ => return,
        };

        let mut forks_to_prune = vec![];
        let mut ancestor_identifier = &tip;

        // Retrieve the whole canonical segment present in memory, descending order
        // [7] ... [2] [1]
        let canonical_segment = {
            let mut segment = vec![];
            while let Some(ancestor) = self.block_store.get(ancestor_identifier) {
                ancestor_identifier = &ancestor.parent_block_identifier;
                segment.push(ancestor.block_identifier.clone());
            }
            segment
        };

        if canonical_segment.len() < CONFIRMED_SEGMENT_MINIMUM_LENGTH as usize {
            ctx.try_log(|logger| slog::info!(logger, "No block to confirm"));
            return;
        }
        // Any block beyond 6th ancestor is considered as confirmed and can be pruned
        let cut_off = &canonical_segment[(CONFIRMED_SEGMENT_MINIMUM_LENGTH - 2) as usize];

        // Prune forks using the confirmed block
        let mut blocks_to_prune = vec![];
        for (fork_id, fork) in self.forks.iter_mut() {
            let mut res = fork.prune_confirmed_blocks(cut_off);
            blocks_to_prune.append(&mut res);
            if fork.block_ids.is_empty() {
                forks_to_prune.push(*fork_id);
            }
        }

        // Prune orphans using the confirmed block
        let iter = self.orphans.clone().into_iter();
        for orphan in iter {
            if orphan.index < cut_off.index {
                self.orphans.remove(&orphan);
                blocks_to_prune.push(orphan);
            }
        }

        // Looping a first time, to collect:
        // 1) the blocks that we will be returning
        // 2) the tip of the trail confirmed by the subsequent block
        // Block 6 (index 5) is confirming transactions included in microblocks
        // that must be merged in Block 7.
        let mut blocks_to_confirm = canonical_segment[6..].to_vec();
        blocks_to_confirm.reverse();
        ctx.try_log(|logger| {
            slog::debug!(
                logger,
                "Removing {} confirmed blocks from block store.",
                blocks_to_confirm.len()
            )
        });
        for confirmed_block in blocks_to_confirm.iter() {
            let block = match self.block_store.remove(confirmed_block) {
                None => {
                    ctx.try_log(|logger| {
                        slog::error!(logger, "unable to retrieve data for {}", confirmed_block)
                    });
                    return;
                }
                Some(block) => block,
            };
            confirmed_blocks.push(block);
        }

        // Prune data
        ctx.try_log(|logger| {
            slog::debug!(
                logger,
                "Pruning {} blocks and {} forks.",
                blocks_to_prune.len(),
                forks_to_prune.len()
            )
        });
        for block_to_prune in blocks_to_prune {
            self.block_store.remove(&block_to_prune);
            self.micro_forks.remove(&block_to_prune);
            self.canonical_micro_fork_id.remove(&block_to_prune);
            // TODO(lgalabru): cascade pruning down to micro_orphans and microblocks_store
        }
        for fork_id in forks_to_prune {
            self.forks.remove(&fork_id);
        }
        // confirmed_blocks.reverse();
    }

    pub fn process_microblocks(
        &mut self,
        microblocks: Vec<StacksMicroblockData>,
        ctx: &Context,
    ) -> Result<Option<StacksChainEvent>, String> {
        if self.block_store.is_empty() {
            ctx.try_log(|logger| {
                slog::info!(
                    logger,
                    "Ignoring microblock trail, fork tracking will start with the next anchor block"
                )
            });
            return Ok(None);
        }

        ctx.try_log(|logger| {
            slog::info!(logger, "Start processing {} microblocks", microblocks.len())
        });

        let mut previous_canonical_micro_fork = None;

        let mut micro_forks_updated = HashSet::new();

        let mut anchor_block_updated = None;

        for mut microblock in microblocks.into_iter() {
            // Temporary patch: the event observer is not returning the block height of the anchor block,
            // we're using the local state to retrieve this missing piece of data.
            if let Some(block) = self
                .block_store
                .get(&microblock.metadata.anchor_block_identifier)
            {
                anchor_block_updated = Some(block.block_identifier.clone());
                microblock.metadata.anchor_block_identifier = block.block_identifier.clone();
            }
            ctx.try_log(|logger| {
                slog::info!(
                    logger,
                    "Processing microblock {}, extending anchor {}",
                    microblock.block_identifier,
                    microblock.metadata.anchor_block_identifier
                )
            });

            // Keep microblock data in memory
            self.microblock_store.insert(
                (
                    microblock.metadata.anchor_block_identifier.clone(),
                    microblock.block_identifier.clone(),
                ),
                microblock.clone(),
            );

            if let (Some(microforks), Some(micro_fork_id)) = (
                self.micro_forks
                    .get(&microblock.metadata.anchor_block_identifier),
                self.canonical_micro_fork_id
                    .get(&microblock.metadata.anchor_block_identifier),
            ) {
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Previous fork selected as canonical: {}",
                        microforks[*micro_fork_id]
                    )
                });
                previous_canonical_micro_fork = Some(microforks[*micro_fork_id].clone());
            }

            let mut micro_fork_updated = None;

            if microblock.block_identifier.index == 0 {
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Initiating new micro fork {}",
                        microblock.block_identifier
                    )
                });
                let mut microfork = ChainSegment::new();
                microfork.append_block_identifier(&microblock.block_identifier);

                match self
                    .micro_forks
                    .entry(microblock.metadata.anchor_block_identifier.clone())
                {
                    Entry::Occupied(microforks) => microforks.into_mut().push(microfork),
                    Entry::Vacant(v) => {
                        v.insert(vec![microfork]);
                    }
                };
                micro_fork_updated = self
                    .micro_forks
                    .get_mut(&microblock.metadata.anchor_block_identifier)
                    .and_then(|microfork| microfork.last_mut())
            } else if let Some(microforks) = self
                .micro_forks
                .get_mut(&microblock.metadata.anchor_block_identifier)
            {
                for micro_fork in microforks.iter_mut() {
                    let (block_appended, mut new_micro_fork) =
                        micro_fork.try_append_block(&microblock, ctx);
                    if block_appended {
                        ctx.try_log(|logger| {
                            slog::info!(
                                logger,
                                "Attempt to append micro fork {} with {} (parent = {}) succeeded",
                                micro_fork,
                                microblock.block_identifier,
                                microblock.parent_block_identifier
                            )
                        });
                        if let Some(new_micro_fork) = new_micro_fork.take() {
                            microforks.push(new_micro_fork);
                            micro_fork_updated = microforks.last_mut();
                        } else {
                            micro_fork_updated = Some(micro_fork);
                        }
                        // A block can only be added to one segment
                        break;
                    } else {
                        ctx.try_log(|logger| {
                            slog::error!(
                                logger,
                                "Attempt to append micro fork {} with {} (parent = {}) failed",
                                micro_fork,
                                microblock.block_identifier,
                                microblock.parent_block_identifier
                            )
                        });
                    }
                }
            }

            let micro_fork_updated = match micro_fork_updated.take() {
                Some(micro_fork) => micro_fork,
                None => {
                    self.micro_orphans.insert((
                        microblock.metadata.anchor_block_identifier.clone(),
                        microblock.block_identifier.clone(),
                    ));
                    continue;
                }
            };

            // Process former orphans
            let orphans = self.micro_orphans.clone();
            let mut orphans_to_untrack = HashSet::new();

            let mut at_least_one_orphan_appended = true;
            // As long as we are successful appending blocks that were previously unprocessable,
            // Keep looping on this backlog
            let mut applied = HashSet::new();
            while at_least_one_orphan_appended {
                at_least_one_orphan_appended = false;
                for orphan_key in orphans.iter() {
                    if applied.contains(orphan_key) {
                        continue;
                    }
                    let block = match self.microblock_store.get(orphan_key) {
                        Some(block) => block.clone(),
                        None => continue,
                    };

                    let (orphan_appended, _new_fork) =
                        micro_fork_updated.try_append_block(&block, ctx);
                    if orphan_appended {
                        applied.insert(orphan_key);
                        orphans_to_untrack.insert(orphan_key);
                    }
                    at_least_one_orphan_appended = at_least_one_orphan_appended || orphan_appended;
                }
            }

            // Update orphans
            for orphan in orphans_to_untrack.into_iter() {
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Dequeuing orphaned microblock ({}, {})",
                        orphan.0,
                        orphan.1
                    )
                });
                self.micro_orphans.remove(orphan);
            }

            micro_forks_updated.insert(microblock.metadata.anchor_block_identifier);
        }

        if micro_forks_updated.is_empty() {
            ctx.try_log(|logger| {
                slog::info!(logger, "Unable to process microblocks - inboxed for later")
            });
            return Ok(None);
        } else {
            ctx.try_log(|logger| slog::info!(logger, "Microblocks successfully appended"));
        }

        let anchor_block_updated = match anchor_block_updated {
            Some(anchor_block_updated) => anchor_block_updated,
            None => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Microblock was received before its anchorblock")
                });
                return Ok(None);
            }
        };

        assert_eq!(micro_forks_updated.len(), 1);
        let microforks = {
            let microforks = self
                .micro_forks
                .get(&anchor_block_updated)
                .expect("unable to retrieve microforks");
            microforks
        };

        // Select canonical fork
        let mut canonical_micro_fork_id = 0;
        let mut highest_height = 0;
        for (fork_id, fork) in microforks.iter().enumerate() {
            ctx.try_log(|logger| slog::info!(logger, "Active microfork: {} - {}", fork_id, fork));
            if fork.get_length() >= highest_height {
                highest_height = fork.get_length();
                canonical_micro_fork_id = fork_id;
            }
        }

        self.canonical_micro_fork_id
            .insert(anchor_block_updated.clone(), canonical_micro_fork_id);

        // Generate chain event from the previous and current canonical forks
        let canonical_micro_fork = microforks.get(canonical_micro_fork_id).unwrap();

        ctx.try_log(|logger| {
            slog::info!(
                logger,
                "Active microfork selected as canonical: {}",
                canonical_micro_fork
            )
        });

        let chain_event = match self.generate_microblock_chain_event(
            &anchor_block_updated,
            canonical_micro_fork,
            &previous_canonical_micro_fork,
            ctx,
        ) {
            Ok(res) => res,
            Err(e) => {
                ctx.try_log(|logger| slog::error!(logger, "Unable to detect convergence: {:?}", e));
                None
            }
        };

        Ok(chain_event)
    }

    // We got the confirmed canonical microblock trail,
    // and we want to send a diff with whatever was sent
    // in the past.
    // The issue comes from the following case. If we
    // condider the 3 following forks
    //
    // 1) A1 - B1 - a1 - b1 - c1 - C1
    //
    // 2) A1 - B1 - a1 - b1 - C2
    //
    // 3) A1 - B1 - a1 - b1 - c1 - d1 - C3
    //
    // How can we always be sending back the right diff?
    // As is, if 1) 2) 3), we will be sending:
    // - BlockUpdate(C1)
    // - BlockReorg(C2, rollback: [c1], apply: [])
    // - BlockReorg(C3, rollback: [], apply: [c1, d1])

    pub fn confirm_microblocks_for_block(
        &mut self,
        block: &StacksBlockData,
        diff_enabled: bool,
        ctx: &Context,
    ) -> Result<
        (Option<StacksChainEvent>, Option<Vec<StacksMicroblockData>>),
        ChainSegmentIncompatibility,
    > {
        match (
            &block.metadata.confirm_microblock_identifier,
            self.micro_forks.get(&block.parent_block_identifier),
        ) {
            (Some(last_microblock), Some(microforks)) => {
                let previous_canonical_segment = self
                    .canonical_micro_fork_id
                    .get(&block.parent_block_identifier)
                    .map(|id| microforks[*id].clone());

                let mut new_canonical_segment = None;
                for (microfork_id, microfork) in microforks.iter().enumerate() {
                    self.canonical_micro_fork_id
                        .insert(block.parent_block_identifier.clone(), microfork_id);
                    if microfork.block_ids.contains(last_microblock) {
                        let mut confirmed_microblocks = microfork.clone();
                        let (_, mutated) = confirmed_microblocks
                            .keep_blocks_from_oldest_to_block_identifier(last_microblock);
                        new_canonical_segment = Some((
                            confirmed_microblocks,
                            if mutated {
                                microforks.len()
                            } else {
                                microfork_id
                            },
                        ));
                        break;
                    }
                }

                if let Some((new_canonical_segment, microfork_id)) = new_canonical_segment {
                    let result = if diff_enabled {
                        let chain_event = self.generate_microblock_chain_event(
                            &block.parent_block_identifier,
                            &new_canonical_segment,
                            &previous_canonical_segment,
                            ctx,
                        )?;
                        (chain_event, None)
                    } else {
                        (
                            None,
                            Some(self.get_microblocks_data(
                                &block.parent_block_identifier,
                                &new_canonical_segment,
                                ctx,
                            )),
                        )
                    };
                    // insert confirmed_microblocks in self.micro_forks
                    self.canonical_micro_fork_id
                        .insert(block.parent_block_identifier.clone(), microfork_id);

                    // update self.canonical_micro_fork_id
                    match self
                        .micro_forks
                        .entry(block.parent_block_identifier.clone())
                    {
                        Entry::Occupied(microforks) => {
                            microforks.into_mut().push(new_canonical_segment)
                        }
                        Entry::Vacant(v) => {
                            v.insert(vec![new_canonical_segment]);
                        }
                    };
                    return Ok(result);
                }
                Ok((None, None))
            }
            _ => Ok((None, None)),
        }
    }

    pub fn get_microblocks_data(
        &self,
        anchor_block_identifier: &BlockIdentifier,
        chain_segment: &ChainSegment,
        ctx: &Context,
    ) -> Vec<StacksMicroblockData> {
        let mut microblocks = vec![];
        for i in 0..chain_segment.block_ids.len() {
            let block_identifier = &chain_segment.block_ids[chain_segment.block_ids.len() - 1 - i];
            let microblock_identifier = (anchor_block_identifier.clone(), block_identifier.clone());
            let block = match self.microblock_store.get(&microblock_identifier) {
                Some(block) => block.clone(),
                None => {
                    ctx.try_log(|logger| {
                        slog::error!(
                            logger,
                            "unable to retrieve full microblock trail {} from block store",
                            microblock_identifier.1
                        )
                    });
                    return microblocks;
                }
            };
            microblocks.push(block)
        }
        microblocks
    }

    pub fn get_confirmed_parent_microblocks(
        &self,
        block: &StacksBlockData,
        ctx: &Context,
    ) -> Vec<StacksMicroblockData> {
        match self.micro_forks.get(&block.parent_block_identifier) {
            Some(microforks) => {
                let previous_canonical_segment = match self
                    .canonical_micro_fork_id
                    .get(&block.parent_block_identifier)
                {
                    Some(id) => self.get_microblocks_data(
                        &block.parent_block_identifier,
                        &microforks[*id],
                        ctx,
                    ),
                    None => vec![],
                };
                previous_canonical_segment
            }
            _ => vec![],
        }
    }

    pub fn generate_block_chain_event(
        &mut self,
        canonical_segment: &ChainSegment,
        other_segment: &ChainSegment,
        ctx: &Context,
    ) -> Result<StacksChainEvent, ChainSegmentIncompatibility> {
        if other_segment.is_empty() {
            let mut new_blocks = vec![];
            for i in 0..canonical_segment.block_ids.len() {
                let block_identifier =
                    &canonical_segment.block_ids[canonical_segment.block_ids.len() - 1 - i];
                let block = match self.block_store.get(block_identifier) {
                    Some(block) => block.clone(),
                    None => {
                        ctx.try_log(|logger| {
                            slog::error!(
                                logger,
                                "unable to retrieve Stacks {} from block store",
                                block_identifier
                            )
                        });
                        return Err(ChainSegmentIncompatibility::BlockNotFound);
                    }
                };
                let block_update = match self.confirm_microblocks_for_block(&block, true, ctx)? {
                    (Some(ref mut chain_event), _) => {
                        let mut update = StacksBlockUpdate::new(block);
                        match chain_event {
                            StacksChainEvent::ChainUpdatedWithMicroblocks(data) => {
                                update
                                    .parent_microblocks_to_apply
                                    .append(&mut data.new_microblocks);
                            }
                            StacksChainEvent::ChainUpdatedWithMicroblocksReorg(data) => {
                                update
                                    .parent_microblocks_to_apply
                                    .append(&mut data.microblocks_to_apply);
                                update
                                    .parent_microblocks_to_rollback
                                    .append(&mut data.microblocks_to_rollback);
                            }
                            _ => unreachable!(),
                        };
                        update
                    }
                    _ => StacksBlockUpdate::new(block),
                };
                new_blocks.push(block_update)
            }
            return Ok(StacksChainEvent::ChainUpdatedWithBlocks(
                StacksChainUpdatedWithBlocksData {
                    new_blocks,
                    confirmed_blocks: vec![],
                },
            ));
        }
        if let Ok(divergence) = canonical_segment.try_identify_divergence(other_segment, false, ctx)
        {
            if divergence.block_ids_to_rollback.is_empty() {
                let mut new_blocks = vec![];
                for i in 0..divergence.block_ids_to_apply.len() {
                    let block_identifier = &divergence.block_ids_to_apply[i];
                    let block = match self.block_store.get(block_identifier) {
                        Some(block) => block.clone(),
                        None => {
                            ctx.try_log(|logger| {
                                slog::error!(
                                    logger,
                                    "unable to retrieve Stacks {} from block store",
                                    block_identifier
                                )
                            });
                            return Err(ChainSegmentIncompatibility::BlockNotFound);
                        }
                    };
                    let block_update =
                        match self.confirm_microblocks_for_block(&block, true, ctx)? {
                            (Some(ref mut chain_event), None) => {
                                let mut update = StacksBlockUpdate::new(block);
                                match chain_event {
                                    StacksChainEvent::ChainUpdatedWithMicroblocks(data) => {
                                        update
                                            .parent_microblocks_to_apply
                                            .append(&mut data.new_microblocks);
                                    }
                                    StacksChainEvent::ChainUpdatedWithMicroblocksReorg(data) => {
                                        update
                                            .parent_microblocks_to_apply
                                            .append(&mut data.microblocks_to_apply);
                                        update
                                            .parent_microblocks_to_rollback
                                            .append(&mut data.microblocks_to_rollback);
                                    }
                                    _ => unreachable!(),
                                };
                                update
                            }
                            _ => StacksBlockUpdate::new(block),
                        };
                    new_blocks.push(block_update)
                }
                return Ok(StacksChainEvent::ChainUpdatedWithBlocks(
                    StacksChainUpdatedWithBlocksData {
                        new_blocks,
                        confirmed_blocks: vec![],
                    },
                ));
            } else {
                let blocks_to_rollback = divergence
                    .block_ids_to_rollback
                    .iter()
                    .map(|block_id| {
                        let block = match self.block_store.get(block_id) {
                            Some(block) => block.clone(),
                            None => {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "unable to retrieve Stacks {} from block store",
                                        block_id
                                    )
                                });
                                return Err(ChainSegmentIncompatibility::BlockNotFound);
                            }
                        };
                        let parent_microblocks_to_rollback =
                            self.get_confirmed_parent_microblocks(&block, ctx);
                        let mut update = StacksBlockUpdate::new(block);
                        update.parent_microblocks_to_rollback = parent_microblocks_to_rollback;
                        Ok(update)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let blocks_to_apply = divergence
                    .block_ids_to_apply
                    .iter()
                    .map(|block_id| {
                        let block = match self.block_store.get(block_id) {
                            Some(block) => block.clone(),
                            None => {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "unable to retrieve Stacks {} from block store",
                                        block_id
                                    )
                                });
                                return Err(ChainSegmentIncompatibility::BlockNotFound);
                            }
                        };
                        let block_update =
                            match self.confirm_microblocks_for_block(&block, false, ctx)? {
                                (_, Some(microblocks_to_apply)) => {
                                    let mut update = StacksBlockUpdate::new(block);
                                    update.parent_microblocks_to_apply = microblocks_to_apply;
                                    update
                                }
                                _ => StacksBlockUpdate::new(block),
                            };
                        Ok(block_update)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                return Ok(StacksChainEvent::ChainUpdatedWithReorg(
                    StacksChainUpdatedWithReorgData {
                        blocks_to_rollback,
                        blocks_to_apply,
                        confirmed_blocks: vec![],
                    },
                ));
            }
        }
        ctx.try_log(|logger| {
            slog::warn!(
                logger,
                "Unable to infer chain event out of {} and {}",
                canonical_segment,
                other_segment
            )
        });
        Err(ChainSegmentIncompatibility::ParentBlockUnknown)
    }

    pub fn generate_microblock_chain_event(
        &self,
        anchor_block_identifier: &BlockIdentifier,
        new_canonical_segment: &ChainSegment,
        previous_canonical_segment: &Option<ChainSegment>,
        ctx: &Context,
    ) -> Result<Option<StacksChainEvent>, ChainSegmentIncompatibility> {
        let previous_canonical_segment = match previous_canonical_segment {
            Some(previous_canonical_segment) if !previous_canonical_segment.is_empty() => {
                previous_canonical_segment
            }
            _ => {
                let mut new_microblocks = vec![];
                for i in 0..new_canonical_segment.block_ids.len() {
                    let block_identifier = &new_canonical_segment.block_ids
                        [new_canonical_segment.block_ids.len() - 1 - i];
                    let microblock_identifier =
                        (anchor_block_identifier.clone(), block_identifier.clone());
                    let block = match self.microblock_store.get(&microblock_identifier) {
                        Some(block) => block.clone(),
                        None => {
                            ctx.try_log(|logger| {
                                slog::error!(
                                    logger,
                                    "unable to retrieve microblock {} from microblock store",
                                    microblock_identifier.1,
                                );
                            });
                            return Err(ChainSegmentIncompatibility::BlockNotFound);
                        }
                    };
                    new_microblocks.push(block)
                }
                return Ok(Some(StacksChainEvent::ChainUpdatedWithMicroblocks(
                    StacksChainUpdatedWithMicroblocksData { new_microblocks },
                )));
            }
        };

        if new_canonical_segment.eq(previous_canonical_segment) {
            return Ok(None);
        }

        if let Ok(divergence) =
            new_canonical_segment.try_identify_divergence(previous_canonical_segment, true, ctx)
        {
            if divergence.block_ids_to_rollback.is_empty() {
                let mut new_microblocks = vec![];
                for i in 0..divergence.block_ids_to_apply.len() {
                    let block_identifier = &new_canonical_segment.block_ids[i];

                    let microblock_identifier =
                        (anchor_block_identifier.clone(), block_identifier.clone());
                    let block = match self.microblock_store.get(&microblock_identifier) {
                        Some(block) => block.clone(),
                        None => {
                            ctx.try_log(|logger| {
                                slog::error!(
                                    logger,
                                    "unable to retrieve microblock {} from microblock store",
                                    microblock_identifier.1,
                                );
                            });
                            return Err(ChainSegmentIncompatibility::BlockNotFound);
                        }
                    };
                    new_microblocks.push(block);
                }
                return Ok(Some(StacksChainEvent::ChainUpdatedWithMicroblocks(
                    StacksChainUpdatedWithMicroblocksData { new_microblocks },
                )));
            } else {
                let microblocks_to_rollback = divergence
                    .block_ids_to_rollback
                    .iter()
                    .map(|microblock_identifier| {
                        let microblock_identifier = (
                            anchor_block_identifier.clone(),
                            microblock_identifier.clone(),
                        );
                        let block = match self.microblock_store.get(&microblock_identifier) {
                            Some(block) => block.clone(),
                            None => {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "unable to retrieve microblock {} from microblock store",
                                        microblock_identifier.1,
                                    )
                                });
                                return Err(ChainSegmentIncompatibility::BlockNotFound);
                            }
                        };
                        Ok(block)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let microblocks_to_apply = divergence
                    .block_ids_to_apply
                    .iter()
                    .map(|microblock_identifier| {
                        let microblock_identifier = (
                            anchor_block_identifier.clone(),
                            microblock_identifier.clone(),
                        );
                        let block = match self.microblock_store.get(&microblock_identifier) {
                            Some(block) => block.clone(),
                            None => {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "unable to retrieve microblock {} from microblock store",
                                        microblock_identifier.1,
                                    )
                                });
                                return Err(ChainSegmentIncompatibility::BlockNotFound);
                            }
                        };
                        Ok(block)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                return Ok(Some(StacksChainEvent::ChainUpdatedWithMicroblocksReorg(
                    StacksChainUpdatedWithMicroblocksReorgData {
                        microblocks_to_apply,
                        microblocks_to_rollback,
                    },
                )));
            }
        }
        Err(ChainSegmentIncompatibility::ParentBlockUnknown)
    }
}
