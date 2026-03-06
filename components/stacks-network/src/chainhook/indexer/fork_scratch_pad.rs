use std::collections::{BTreeMap, BTreeSet, HashSet};

use hiro_system_kit::slog;

use crate::chainhook::indexer::{ChainSegment, ChainSegmentIncompatibility};
use crate::chainhook::types::{
    BlockHeader, BlockIdentifier, BlockchainEvent, BlockchainUpdatedWithHeaders,
    BlockchainUpdatedWithReorg,
};
use crate::chainhook::utils::Context;

pub struct ForkScratchPad {
    canonical_fork_id: usize,
    orphans: BTreeSet<BlockIdentifier>,
    forks: BTreeMap<usize, ChainSegment>,
    headers_store: BTreeMap<BlockIdentifier, BlockHeader>,
}
pub const CONFIRMED_SEGMENT_MINIMUM_LENGTH: i32 = 7;
impl Default for ForkScratchPad {
    fn default() -> Self {
        Self::new()
    }
}

impl ForkScratchPad {
    pub fn new() -> ForkScratchPad {
        let mut forks = BTreeMap::new();
        forks.insert(0, ChainSegment::new());
        let headers_store = BTreeMap::new();
        ForkScratchPad {
            canonical_fork_id: 0,
            orphans: BTreeSet::new(),
            forks,
            headers_store,
        }
    }

    pub fn can_process_header(&self, header: &BlockHeader) -> bool {
        if self.headers_store.is_empty() {
            return true;
        }

        self.headers_store
            .contains_key(&header.parent_block_identifier)
    }

    pub fn process_header(
        &mut self,
        header: BlockHeader,
        ctx: &Context,
    ) -> Result<Option<BlockchainEvent>, String> {
        ctx.try_log(|logger| slog::info!(logger, "Start processing {}", header.block_identifier));

        // Keep block data in memory
        let entry_exists = self
            .headers_store
            .insert(header.block_identifier.clone(), header.clone());
        if entry_exists.is_some() {
            ctx.try_log(|logger| {
                slog::warn!(
                    logger,
                    "Block {} has already been processed",
                    header.block_identifier
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
                    slog::error!(logger, "unable to retrieve previous bitcoin fork")
                });
                return Ok(None);
            }
        };

        let mut fork_updated = None;
        for (_, fork) in self.forks.iter_mut() {
            let (block_appended, mut new_fork) = fork.try_append_block(&header, ctx);
            if block_appended {
                if let Some(new_fork) = new_fork.take() {
                    let fork_id = self.forks.len();
                    self.forks.insert(fork_id, new_fork);
                    fork_updated = self.forks.get_mut(&fork_id);
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
                    slog::debug!(
                        logger,
                        "Bitcoin {} successfully appended to {}",
                        header.block_identifier,
                        fork
                    )
                });
                fork
            }
            None => {
                ctx.try_log(|logger| {
                    slog::error!(
                        logger,
                        "Unable to process Bitcoin {} - inboxed for later",
                        header.block_identifier
                    )
                });
                self.orphans.insert(header.block_identifier.clone());
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
        let mut forks_created = vec![];
        while at_least_one_orphan_appended {
            at_least_one_orphan_appended = false;
            for orphan_block_identifier in orphans.iter() {
                if applied.contains(orphan_block_identifier) {
                    continue;
                }
                let block = match self.headers_store.get(orphan_block_identifier) {
                    Some(block) => block.clone(),
                    None => continue,
                };

                let (orphan_appended, mut new_fork) = fork_updated.try_append_block(&block, ctx);
                if orphan_appended {
                    applied.insert(orphan_block_identifier);
                    orphans_to_untrack.insert(orphan_block_identifier);
                    if let Some(new_fork) = new_fork.take() {
                        forks_created.push(new_fork);
                    }
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
        for (fork_id, fork) in self.forks.iter() {
            ctx.try_log(|logger| slog::info!(logger, "Active fork: {} - {}", fork_id, fork));
            if fork.get_length() >= highest_height {
                highest_height = fork.get_length();
                canonical_fork_id = *fork_id;
            }
        }
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

        self.collect_and_prune_confirmed_blocks(&mut chain_event, ctx);

        Ok(Some(chain_event))
    }

    pub fn collect_and_prune_confirmed_blocks(
        &mut self,
        chain_event: &mut BlockchainEvent,
        ctx: &Context,
    ) {
        let (tip, confirmed_blocks) = match chain_event {
            BlockchainEvent::BlockchainUpdatedWithHeaders(ref mut event) => {
                match event.new_headers.last() {
                    Some(tip) => (tip.block_identifier.clone(), &mut event.confirmed_headers),
                    None => return,
                }
            }
            BlockchainEvent::BlockchainUpdatedWithReorg(ref mut event) => {
                match event.headers_to_apply.last() {
                    Some(tip) => (tip.block_identifier.clone(), &mut event.confirmed_headers),
                    None => return,
                }
            }
        };

        let mut forks_to_prune = vec![];
        let mut ancestor_identifier = &tip;

        // Retrieve the whole canonical segment present in memory, ascending order
        // [1] ... [6] [7]
        let canonical_segment = {
            let mut segment = vec![];
            while let Some(ancestor) = self.headers_store.get(ancestor_identifier) {
                ancestor_identifier = &ancestor.parent_block_identifier;
                segment.push(ancestor.block_identifier.clone());
            }
            segment
        };
        if canonical_segment.len() < CONFIRMED_SEGMENT_MINIMUM_LENGTH as usize {
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

        ctx.try_log(|logger| {
            slog::debug!(
                logger,
                "Removing {} confirmed blocks from block store.",
                canonical_segment[6..].len()
            )
        });
        for confirmed_block in canonical_segment[6..].iter() {
            let block = match self.headers_store.remove(confirmed_block) {
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
            self.headers_store.remove(&block_to_prune);
        }
        for fork_id in forks_to_prune {
            self.forks.remove(&fork_id);
        }
        confirmed_blocks.reverse();
    }

    pub fn generate_block_chain_event(
        &mut self,
        canonical_segment: &ChainSegment,
        other_segment: &ChainSegment,
        ctx: &Context,
    ) -> Result<BlockchainEvent, ChainSegmentIncompatibility> {
        if other_segment.is_empty() {
            let mut new_headers = vec![];
            for i in 0..canonical_segment.block_ids.len() {
                let block_identifier =
                    &canonical_segment.block_ids[canonical_segment.block_ids.len() - 1 - i];
                let header = match self.headers_store.get(block_identifier) {
                    Some(block) => block.clone(),
                    None => {
                        ctx.try_log(|logger| {
                            slog::error!(
                                logger,
                                "unable to retrieve Bitcoin block {} from block store",
                                block_identifier
                            )
                        });
                        return Err(ChainSegmentIncompatibility::Unknown);
                    }
                };
                new_headers.push(header)
            }
            return Ok(BlockchainEvent::BlockchainUpdatedWithHeaders(
                BlockchainUpdatedWithHeaders {
                    new_headers,
                    confirmed_headers: vec![],
                },
            ));
        }
        if let Ok(divergence) = canonical_segment.try_identify_divergence(other_segment, false, ctx)
        {
            if divergence.block_ids_to_rollback.is_empty() {
                let mut new_headers = vec![];
                for i in 0..divergence.block_ids_to_apply.len() {
                    let block_identifier = &divergence.block_ids_to_apply[i];
                    let header = match self.headers_store.get(block_identifier) {
                        Some(header) => header.clone(),
                        None => {
                            ctx.try_log(|logger| {
                                slog::error!(
                                    logger,
                                    "unable to retrieve Bitcoin block {} from block store",
                                    block_identifier
                                )
                            });
                            return Err(ChainSegmentIncompatibility::Unknown);
                        }
                    };
                    new_headers.push(header)
                }
                return Ok(BlockchainEvent::BlockchainUpdatedWithHeaders(
                    BlockchainUpdatedWithHeaders {
                        new_headers,
                        confirmed_headers: vec![],
                    },
                ));
            } else {
                return Ok(BlockchainEvent::BlockchainUpdatedWithReorg(
                    BlockchainUpdatedWithReorg {
                        headers_to_rollback: divergence
                            .block_ids_to_rollback
                            .iter()
                            .map(|block_id| {
                                let block = match self.headers_store.get(block_id) {
                                    Some(block) => block.clone(),
                                    None => {
                                        ctx.try_log(|logger| {
                                            slog::error!(
                                            logger,
                                            "unable to retrieve Bitcoin block {} from block store",
                                            block_id
                                        )
                                        });
                                        return Err(ChainSegmentIncompatibility::Unknown);
                                    }
                                };
                                Ok(block)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                        headers_to_apply: divergence
                            .block_ids_to_apply
                            .iter()
                            .map(|block_id| {
                                let block = match self.headers_store.get(block_id) {
                                    Some(block) => block.clone(),
                                    None => {
                                        ctx.try_log(|logger| {
                                            slog::error!(
                                            logger,
                                            "unable to retrieve Bitcoin block {} from block store",
                                            block_id
                                        )
                                        });
                                        return Err(ChainSegmentIncompatibility::Unknown);
                                    }
                                };
                                Ok(block)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                        confirmed_headers: vec![],
                    },
                ));
            }
        }
        ctx.try_log(|logger| {
            slog::debug!(
                logger,
                "Unable to infer chain event out of {} and {}",
                canonical_segment,
                other_segment
            )
        });
        Err(ChainSegmentIncompatibility::ParentBlockUnknown)
    }
}
