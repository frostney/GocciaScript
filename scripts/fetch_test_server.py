"""Minimal HTTP test server for fetch() end-to-end tests.

Endpoints:
  GET  /text          — 200 "hello world" (text/plain)
  GET  /json          — 200 {"url":"/json","method":"GET"} (application/json)
  GET  /echo-headers  — 200 {"headers":{...}} reflecting request headers
  GET  /redirect      — 302 → /json
  GET  /redirect-headers — 302 → /echo-headers
  GET  /redirect-host — 302 → localhost on the same port, /echo-headers
  GET  /redirect-external — 302 → http://10.255.255.1:1/json (private, off-allowlist)
  GET  /redirect-chain/<n> — 307 → /redirect-chain/<n-1>; /redirect-chain/0 → /json
  GET  /bytes/<n>     — 200 with a body of <n> "a" bytes (application/octet-stream)
  GET  /status/<code> — responds with <code> and empty body
  GET  /              — 200 "ok"
  HEAD *              — 200 with Content-Length but no body

Usage:
  python3 scripts/fetch_test_server.py <port-file>

Writes the assigned port to <port-file>, then serves until killed.
"""

import http.server
import json
import sys


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def do_GET(self):
        if self.path == "/json":
            body = json.dumps({"url": self.path, "method": "GET"}).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif self.path.startswith("/echo-headers"):
            headers_dict = {k: v for k, v in self.headers.items()}
            body = json.dumps({"headers": headers_dict}).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif self.path == "/redirect":
            self.send_response(302)
            self.send_header("Location", "/json")
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif self.path == "/redirect-headers":
            self.send_response(302)
            self.send_header("Location", "/echo-headers")
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif self.path == "/redirect-host":
            self.send_response(302)
            self.send_header(
                "Location",
                f"http://localhost:{self.server.server_address[1]}/echo-headers",
            )
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif self.path == "/redirect-external":
            # Absolute redirect to a private, non-allowlisted address. The
            # initial hop (loopback) is allowed; only per-hop revalidation can
            # catch that the redirect target is not. Lets the E2E prove the hop
            # itself is policy-checked rather than just the first request.
            self.send_response(302)
            self.send_header("Location", "http://10.255.255.1:1/json")
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif self.path.startswith("/redirect-chain/"):
            # A chain of <n> redirects ending at /json, so a test can count
            # the redirects fetch follows before it gives up.
            try:
                remaining = int(self.path.rsplit("/", 1)[-1])
            except ValueError:
                remaining = -1
            if remaining < 0:
                self.send_response(400)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            self.send_response(307)
            if remaining == 0:
                self.send_header("Location", "/json")
            else:
                self.send_header("Location", f"/redirect-chain/{remaining - 1}")
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif self.path.startswith("/bytes/"):
            # A body of <n> bytes, so a test can hold bodies against the
            # engine's memory limit.
            try:
                size = int(self.path.rsplit("/", 1)[-1])
            except ValueError:
                size = -1
            if size < 0 or size > 64 * 1024 * 1024:
                self.send_response(400)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Content-Length", str(size))
            self.end_headers()
            self.wfile.write(b"a" * size)
        elif self.path.startswith("/status/"):
            raw_code = self.path.rsplit("/", 1)[-1]
            try:
                code = int(raw_code)
            except ValueError:
                self.send_response(400)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            if code < 100 or code > 599:
                self.send_response(400)
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            self.send_response(code)
            self.send_header("Content-Length", "0")
            self.end_headers()
        elif self.path == "/text":
            body = b"hello world"
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        else:
            body = b"ok"
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

    def do_HEAD(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", "5")
        self.end_headers()


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 fetch_test_server.py <port-file>", file=sys.stderr)
        sys.exit(1)

    server = http.server.HTTPServer(("127.0.0.1", 0), Handler)
    port = server.server_address[1]

    with open(sys.argv[1], "w") as f:
        f.write(str(port))

    server.serve_forever()


if __name__ == "__main__":
    main()
