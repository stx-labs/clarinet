"use strict";

// Shared SAB layout for sync_http.cjs (main) and sync_http_worker.cjs (worker).
// One source of truth — both files require() this so the constants can't drift.
//
// SAB header at offset 0, request/response data area at offset HEADER_BYTES:
//   0  Int32  signal           0=response_done (worker idle), 1=request_pending
//   4  Int32  status_code      HTTP status, or -1 for transport error
//   8  Int32  status_text_len  response status text byte length
//  12  Int32  headers_len      response headers JSON byte length
//  16  Int32  body_len         response body byte length; negative means error
//  20  Int32  url_len          request URL byte length
//  24  Int32  req_hdrs_len     request headers JSON byte length

module.exports = {
  HEADER_BYTES: 1024,
  OFFSET_STATUS: 4,
  OFFSET_STATUS_TEXT_LEN: 8,
  OFFSET_HEADERS_LEN: 12,
  OFFSET_BODY_LEN: 16,
  OFFSET_URL_LEN: 20,
  OFFSET_REQ_HDRS_LEN: 24,
  // The signal alternates between RESPONSE_DONE (worker idle, main may issue
  // next request) and REQUEST_PENDING (worker is processing). A three-state
  // scheme races: by the time the worker loops back to wait, main may already
  // have reset the signal and the worker would re-enter with stale data.
  SIGNAL_RESPONSE_DONE: 0,
  SIGNAL_REQUEST_PENDING: 1,
  FETCH_TIMEOUT_MS: 30_000,
};
