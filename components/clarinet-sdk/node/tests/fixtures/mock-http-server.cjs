// Mock HTTP server used by sync-http.test.ts. Runs in a detached child so its
// event loop isn't frozen by the main test process's Atomics.wait.
//
// Usage: node mock-http-server.cjs <recorded-headers-path>
// Prints `READY:<url>\n` on stdout once listening.

const http = require("node:http");
const fs = require("node:fs");

const RECORD = process.argv[2];
if (!RECORD) {
  process.stderr.write("usage: mock-http-server.cjs <recorded-headers-path>\n");
  process.exit(1);
}

const big = "x".repeat(200 * 1024);

const server = http.createServer((req, res) => {
  const path = req.url || "/";

  if (path.startsWith("/record")) {
    try {
      fs.writeFileSync(RECORD, JSON.stringify(req.headers));
    } catch (_) {}
    res.writeHead(200, "OK", { "content-type": "application/json" });
    res.end(JSON.stringify({ ok: true }));
    return;
  }

  if (path.startsWith("/big")) {
    res.writeHead(200, "OK", { "content-type": "text/plain" });
    res.end(big);
    return;
  }

  if (path.startsWith("/marker")) {
    res.writeHead(200, "OK", {
      "content-type": "application/json",
      "x-marker": "hello",
    });
    res.end(JSON.stringify({ ok: true, n: 42 }));
    return;
  }

  res.writeHead(200, "OK", { "content-type": "application/json" });
  res.end(JSON.stringify({ ok: true }));
});

server.listen(0, "127.0.0.1", () => {
  const addr = server.address();
  process.stdout.write("READY:http://127.0.0.1:" + addr.port + "\n");
});
