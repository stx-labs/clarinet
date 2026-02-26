use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex, RwLock};

use axum::extract::Extension;
use axum::http::StatusCode;
use axum::response::Json;
use axum::routing::post;
use axum::Router;
use hiro_system_kit::slog;
use serde_json::{json, Value as JsonValue};

// AppState to hold all shared state for Axum
#[derive(Clone)]
pub struct AppState {
    pub indexer_rw_lock: Arc<RwLock<Indexer>>,
    pub background_job_tx: Arc<Mutex<Sender<ObserverCommand>>>,
    pub bitcoin_config: BitcoinConfig,
    pub ctx: Context,
}

use super::{
    BitcoinConfig, BitcoinRPCRequest, MempoolAdmissionData, ObserverCommand,
    StacksChainMempoolEvent,
};
use crate::chainhook::indexer::bitcoin::{
    build_http_client, download_and_parse_block_with_retry, NewBitcoinBlock,
};
use crate::chainhook::indexer::{self, Indexer};
use crate::chainhook::utils::Context;
use crate::{try_error, try_info};

fn success_response() -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    Ok(Json(json!({
        "status": 200,
        "result": "Ok",
    })))
}

fn error_response(
    message: String,
    ctx: &Context,
) -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    try_error!(ctx, "{message}");
    Err((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(json!({
            "status": 500,
            "result": message,
        })),
    ))
}

pub async fn handle_new_bitcoin_block(
    Extension(app_state): Extension<AppState>,
    Json(bitcoin_block): Json<NewBitcoinBlock>,
) -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    let AppState {
        indexer_rw_lock,
        background_job_tx,
        bitcoin_config,
        ctx,
    } = app_state;
    if bitcoin_config
        .bitcoin_block_signaling
        .should_ignore_bitcoin_block_signaling_through_stacks()
    {
        return success_response();
    }

    try_info!(ctx, "POST /new_burn_block");
    // Standardize the structure of the block, and identify the
    // kind of update that this new block would imply, taking
    // into account the last 7 blocks.

    let http_client = build_http_client();
    let block_hash = bitcoin_block.burn_block_hash.strip_prefix("0x").unwrap();
    let block =
        match download_and_parse_block_with_retry(&http_client, block_hash, &bitcoin_config, &ctx)
            .await
        {
            Ok(block) => block,
            Err(e) => {
                return error_response(format!("unable to download_and_parse_block: {e}"), &ctx)
            }
        };

    let header = block.get_block_header();
    if let Err(e) = background_job_tx.lock().map(|tx| {
        tx.send(ObserverCommand::ProcessBitcoinBlock(block))
            .map_err(|e| format!("Unable to send stacks chain event: {}", e))
    }) {
        return error_response(format!("unable to acquire background_job_tx: {e}"), &ctx);
    }

    let chain_update = match indexer_rw_lock.write() {
        Ok(mut indexer) => indexer.handle_bitcoin_header(header, &ctx),
        Err(e) => {
            return error_response(format!("Unable to acquire indexer_rw_lock: {e}"), &ctx);
        }
    };

    match chain_update {
        Ok(Some(chain_event)) => {
            if let Err(e) = background_job_tx.lock().map(|tx| {
                tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event))
                    .map_err(|e| format!("Unable to send stacks chain event: {}", e))
            }) {
                return error_response(format!("unable to acquire background_job_tx: {e}"), &ctx);
            }
        }
        Ok(None) => {
            try_info!(ctx, "No chain event was generated");
        }
        Err(e) => {
            return error_response(format!("Unable to handle bitcoin block: {e}"), &ctx);
        }
    }

    success_response()
}

pub async fn handle_new_stacks_block(
    Extension(app_state): Extension<AppState>,
    Json(marshalled_block): Json<JsonValue>,
) -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    let AppState {
        indexer_rw_lock,
        background_job_tx,
        bitcoin_config: _,
        ctx,
    } = app_state;
    try_info!(ctx, "POST /new_block");
    // Standardize the structure of the block, and identify the
    // kind of update that this new block would imply, taking
    // into account the last 7 blocks.
    // TODO(lgalabru): use _pox_config
    let (_pox_config, chain_event, _new_tip) = match indexer_rw_lock.write() {
        Ok(mut indexer) => {
            let pox_config = indexer.get_pox_config();
            let block = match indexer.standardize_stacks_marshalled_block(marshalled_block, &ctx) {
                Ok(block) => block,
                Err(e) => {
                    return error_response(format!("Unable to standardize stacks block {e}"), &ctx);
                }
            };
            let new_tip = block.block_identifier.index;
            let chain_event = indexer.process_stacks_block(block, &ctx);
            (pox_config, chain_event, new_tip)
        }
        Err(e) => {
            return error_response(format!("Unable to acquire indexer_rw_lock: {e}"), &ctx);
        }
    };

    match chain_event {
        Ok(Some(chain_event)) => {
            if let Err(e) = background_job_tx.lock().map(|tx| {
                tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event))
                    .map_err(|e| format!("Unable to send stacks chain event: {}", e))
            }) {
                return error_response(format!("unable to acquire background_job_tx: {e}"), &ctx);
            }
        }
        Ok(None) => {
            try_info!(ctx, "No chain event was generated");
        }
        Err(e) => {
            return error_response(format!("Chain event error: {e}"), &ctx);
        }
    }

    success_response()
}

#[cfg(feature = "stacks-signers")]
pub async fn handle_stackerdb_chunks(
    Extension(app_state): Extension<AppState>,
    Json(payload): Json<JsonValue>,
) -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    let AppState {
        indexer_rw_lock,
        background_job_tx,
        bitcoin_config: _,
        ctx,
    } = app_state;
    use std::time::{SystemTime, UNIX_EPOCH};

    try_info!(ctx, "POST /stackerdb_chunks");

    // Standardize the structure of the StackerDB chunk, and identify the kind of update that this new message would imply.
    let Ok(epoch) = SystemTime::now().duration_since(UNIX_EPOCH) else {
        return error_response("Unable to get system receipt_time".to_string(), &ctx);
    };
    let chain_event = match indexer_rw_lock.write() {
        Ok(mut indexer) => {
            indexer.handle_stacks_marshalled_stackerdb_chunk(payload, epoch.as_millis(), &ctx)
        }
        Err(e) => {
            return error_response(format!("Unable to acquire background_job_tx: {e}"), &ctx);
        }
    };

    match chain_event {
        Ok(Some(chain_event)) => {
            if let Err(e) = background_job_tx.lock().map(|tx| {
                tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event))
                    .map_err(|e| format!("Unable to send stacks chain event: {}", e))
            }) {
                return error_response(format!("unable to acquire background_job_tx: {e}"), &ctx);
            }
        }
        Ok(None) => {
            try_info!(ctx, "No chain event was generated");
        }
        Err(e) => {
            return error_response(format!("Chain event error: {e}"), &ctx);
        }
    }

    success_response()
}

pub async fn handle_new_microblocks(
    Extension(app_state): Extension<AppState>,
    Json(marshalled_microblock): Json<JsonValue>,
) -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    let AppState {
        indexer_rw_lock,
        background_job_tx,
        bitcoin_config: _,
        ctx,
    } = app_state;
    try_info!(ctx, "POST /new_microblocks");
    // Standardize the structure of the microblock, and identify the
    // kind of update that this new microblock would imply
    let chain_event = match indexer_rw_lock.write() {
        Ok(mut indexer) => {
            indexer.handle_stacks_marshalled_microblock_trail(marshalled_microblock, &ctx)
        }
        Err(e) => {
            return error_response(format!("Unable to acquire background_job_tx: {e}"), &ctx);
        }
    };

    match chain_event {
        Ok(Some(chain_event)) => {
            if let Err(e) = background_job_tx.lock().map(|tx| {
                tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event))
                    .map_err(|e| format!("Unable to send stacks chain event: {}", e))
            }) {
                return error_response(format!("unable to acquire background_job_tx: {e}"), &ctx);
            }
        }
        Ok(None) => {
            try_info!(ctx, "No chain event was generated");
        }
        Err(e) => {
            return error_response(format!("Chain event error: {e}"), &ctx);
        }
    }

    success_response()
}

pub async fn handle_new_mempool_tx(
    Extension(app_state): Extension<AppState>,
    Json(raw_txs): Json<Vec<String>>,
) -> Result<Json<JsonValue>, (StatusCode, Json<JsonValue>)> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx,
        bitcoin_config: _,
        ctx,
    } = app_state;
    try_info!(ctx, "POST /new_mempool_tx");
    let transactions = match raw_txs
        .iter()
        .map(|tx_data| {
            indexer::stacks::get_tx_description(tx_data, &vec![]).map(|(tx_description, ..)| {
                MempoolAdmissionData {
                    tx_data: tx_data.clone(),
                    tx_description,
                }
            })
        })
        .collect::<Result<Vec<MempoolAdmissionData>, _>>()
    {
        Ok(transactions) => transactions,
        Err(e) => {
            return error_response(format!("Failed to parse mempool transactions: {e}"), &ctx);
        }
    };

    if let Err(e) = background_job_tx.lock().map(|tx| {
        tx.send(ObserverCommand::PropagateStacksMempoolEvent(
            StacksChainMempoolEvent::TransactionsAdmitted(transactions),
        ))
        .map_err(|e| format!("Unable to send stacks chain event: {}", e))
    }) {
        return error_response(format!("unable to acquire background_job_tx: {e}"), &ctx);
    }

    success_response()
}

pub async fn handle_drop_mempool_tx(
    Extension(app_state): Extension<AppState>,
    Json(payload): Json<JsonValue>,
) -> Json<JsonValue> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx: _,
        bitcoin_config: _,
        ctx,
    } = app_state;
    ctx.try_log(|logger| slog::debug!(logger, "POST /drop_mempool_tx {:?}", payload));
    // TODO(lgalabru): use propagate mempool events
    Json(json!({
        "status": 200,
        "result": "Ok",
    }))
}

pub async fn handle_new_attachement(
    Extension(app_state): Extension<AppState>,
    Json(payload): Json<JsonValue>,
) -> Json<JsonValue> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx: _,
        bitcoin_config: _,
        ctx,
    } = app_state;
    ctx.try_log(|logger| slog::debug!(logger, "POST /attachments/new {:?}", payload));
    Json(json!({
        "status": 200,
        "result": "Ok",
    }))
}

pub async fn handle_mined_block(
    Extension(app_state): Extension<AppState>,
    Json(payload): Json<JsonValue>,
) -> Json<JsonValue> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx: _,
        bitcoin_config: _,
        ctx,
    } = app_state;
    ctx.try_log(|logger| slog::debug!(logger, "POST /mined_block {:?}", payload));
    Json(json!({
        "status": 200,
        "result": "Ok",
    }))
}

pub async fn handle_mined_microblock(
    Extension(app_state): Extension<AppState>,
    Json(payload): Json<JsonValue>,
) -> Json<JsonValue> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx: _,
        bitcoin_config: _,
        ctx,
    } = app_state;
    ctx.try_log(|logger| slog::debug!(logger, "POST /mined_microblock {:?}", payload));
    Json(json!({
        "status": 200,
        "result": "Ok",
    }))
}

pub async fn handle_bitcoin_wallet_rpc_call(
    Extension(app_state): Extension<AppState>,
    Json(bitcoin_rpc_call): Json<BitcoinRPCRequest>,
) -> Json<JsonValue> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx: _,
        bitcoin_config,
        ctx,
    } = app_state;
    ctx.try_log(|logger| slog::debug!(logger, "POST /wallet"));

    use base64::engine::general_purpose::STANDARD as BASE64;
    use base64::engine::Engine as _;
    use reqwest::Client;

    let bitcoin_rpc_call = bitcoin_rpc_call.clone();

    let body = serde_json::to_vec(&bitcoin_rpc_call).unwrap_or_default();

    let token = BASE64.encode(format!(
        "{}:{}",
        bitcoin_config.username, bitcoin_config.password
    ));

    let url = bitcoin_config.rpc_url.to_string();
    let client = Client::new();
    let builder = client
        .post(&url)
        .header("Content-Type", "application/json")
        .header("Authorization", format!("Basic {}", token))
        .timeout(std::time::Duration::from_secs(5));

    match builder.body(body).send().await {
        Ok(res) => Json(res.json().await.unwrap()),
        Err(_) => Json(json!({
            "status": 500
        })),
    }
}

pub async fn handle_bitcoin_rpc_call(
    Extension(app_state): Extension<AppState>,
    Json(bitcoin_rpc_call): Json<BitcoinRPCRequest>,
) -> Json<JsonValue> {
    let AppState {
        indexer_rw_lock: _,
        background_job_tx,
        bitcoin_config,
        ctx,
    } = app_state;
    ctx.try_log(|logger| slog::debug!(logger, "POST /"));

    use base64::engine::general_purpose::STANDARD as BASE64;
    use base64::engine::Engine as _;
    use reqwest::Client;

    let bitcoin_rpc_call = bitcoin_rpc_call.clone();
    let method = bitcoin_rpc_call.method.clone();

    let body = serde_json::to_vec(&bitcoin_rpc_call).unwrap_or_default();

    let token = BASE64.encode(format!(
        "{}:{}",
        bitcoin_config.username, bitcoin_config.password
    ));

    ctx.try_log(|logger| {
        slog::debug!(
            logger,
            "Forwarding {} request to {}",
            method,
            bitcoin_config.rpc_url
        )
    });

    let url = if method == "listunspent" {
        format!("{}/wallet/", bitcoin_config.rpc_url)
    } else {
        bitcoin_config.rpc_url.to_string()
    };

    let client = Client::new();
    let builder = client
        .post(&url)
        .header("Content-Type", "application/json")
        .header("Authorization", format!("Basic {}", token))
        .timeout(std::time::Duration::from_secs(5));

    if method == "sendrawtransaction" {
        let background_job_tx_ref = &background_job_tx;
        if let Ok(tx) = background_job_tx_ref.lock() {
            let _ = tx.send(ObserverCommand::NotifyBitcoinTransactionProxied);
        };
    }

    match builder.body(body).send().await {
        Ok(res) => {
            let payload = res.json().await.unwrap();
            ctx.try_log(|logger| slog::debug!(logger, "Responding with response {:?}", payload));
            Json(payload)
        }
        Err(_) => Json(json!({
            "status": 500
        })),
    }
}

pub fn create_router(
    indexer_rw_lock: Arc<RwLock<Indexer>>,
    background_job_tx: Arc<Mutex<Sender<ObserverCommand>>>,
    bitcoin_config: BitcoinConfig,
    ctx: Context,
    bitcoin_rpc_proxy_enabled: bool,
) -> Router {
    let app_state = AppState {
        indexer_rw_lock,
        background_job_tx,
        bitcoin_config,
        ctx,
    };

    let mut router = Router::new()
        .route("/new_burn_block", post(handle_new_bitcoin_block))
        .route("/new_block", post(handle_new_stacks_block))
        .route("/new_microblocks", post(handle_new_microblocks))
        .route("/new_mempool_tx", post(handle_new_mempool_tx))
        .route("/drop_mempool_tx", post(handle_drop_mempool_tx))
        .route("/attachments/new", post(handle_new_attachement))
        .route("/mined_block", post(handle_mined_block))
        .route("/mined_microblock", post(handle_mined_microblock));

    #[cfg(feature = "stacks-signers")]
    {
        router = router.route("/stackerdb_chunks", post(handle_stackerdb_chunks));
    }

    if bitcoin_rpc_proxy_enabled {
        router = router
            .route("/", post(handle_bitcoin_rpc_call))
            .route("/wallet/", post(handle_bitcoin_wallet_rpc_call));
    }

    router.layer(axum::Extension(app_state))
}
