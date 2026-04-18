"""Minimal HTTP test server for fetch() end-to-end tests.

Endpoints:
  GET  /text          — 200 "hello world" (text/plain)
  GET  /json          — 200 {"url":"/json","method":"GET"} (application/json)
  GET  /echo-headers  — 200 {"headers":{...}} reflecting request headers
  GET  /redirect      — 302 → /json
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
