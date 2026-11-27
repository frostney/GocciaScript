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
  GET  /wave/<tag>    — 200 "ok" once the 16-request wave it arrived in is complete
                        (request i waits until 16 * (i // 16 + 1) have arrived, at most 10 s)
  GET  /sleep/<ms>/<tag> — 200 "ok" after <ms> milliseconds
  GET  /arrivals      — 200 {"peak":n,"arrivals":[tag,...]}: the most /wave and /sleep
                        requests held at once, and their tags in arrival order
  GET  /arrivals-reset — 200 "ok", clearing both
  GET  /              — 200 "ok"
  HEAD *              — 200 with Content-Length but no body

Usage:
  python3 scripts/fetch_test_server.py <port-file> [<ipv6-port-file>]

Writes the assigned port to <port-file>, then serves until killed.
When an IPv6 port file is supplied, also serves on ::1 if available.
"""

import errno
import faulthandler
import os
import http.server
import json
import socket
import socketserver
import sys
import threading
import time


class Arrivals:
    """The /wave and /sleep requests the server has seen, shared by its threads."""

    lock = threading.Condition()
    tags = []
    held = 0
    peak = 0

    @classmethod
    def arrive(cls, tag):
        with cls.lock:
            cls.tags.append(tag)
            cls.held += 1
            cls.peak = max(cls.peak, cls.held)
            cls.lock.notify_all()
            return len(cls.tags) - 1

    @classmethod
    def leave(cls):
        with cls.lock:
            cls.held -= 1


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def send_ok(self):
        body = b"ok"
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path.startswith("/wave/"):
            at = Arrivals.arrive(self.path[len("/wave/"):])
            wave_end = 16 * (at // 16 + 1)
            with Arrivals.lock:
                Arrivals.lock.wait_for(lambda: len(Arrivals.tags) >= wave_end, 10)
            # left before answering: the client's next request can only follow the answer
            Arrivals.leave()
            self.send_ok()
        elif self.path.startswith("/sleep/"):
            ms, _, tag = self.path[len("/sleep/"):].partition("/")
            Arrivals.arrive(tag)
            time.sleep(int(ms) / 1000)
            Arrivals.leave()
            self.send_ok()
        elif self.path == "/arrivals":
            with Arrivals.lock:
                body = json.dumps({"peak": Arrivals.peak, "arrivals": Arrivals.tags}).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif self.path == "/arrivals-reset":
            with Arrivals.lock:
                Arrivals.tags = []
                Arrivals.peak = Arrivals.held
            self.send_ok()
        elif self.path == "/json":
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


class LoopbackHTTPServer(http.server.ThreadingHTTPServer):
    # A thread per request: the /wave and /sleep endpoints hold several at once.
    daemon_threads = True

    def server_bind(self):
        # HTTPServer asks DNS for a display name even for a numeric address.
        # These loopback fixtures must be ready without a reverse lookup.
        socketserver.TCPServer.server_bind(self)
        self.server_name, self.server_port = self.server_address[:2]


class IPv6HTTPServer(LoopbackHTTPServer):
    address_family = socket.AF_INET6


def stall_ipv4(port):
    # Linux leaves new connections pending when this one-slot accept queue
    # fills. Confirm that condition before publishing the IPv6 fixture.
    listener = socket.socket()
    held = None
    try:
        listener.bind(("127.0.0.1", port))
        listener.listen(0)
        held = socket.create_connection(("127.0.0.1", port), 1)
        try:
            with socket.create_connection(("127.0.0.1", port), 0.5):
                raise RuntimeError("IPv4 fixture did not stall the connection")
        except TimeoutError:
            pass
        return listener, held
    except BaseException:
        if held is not None:
            held.close()
        listener.close()
        raise


def start_ipv6(port_file):
    try:
        server = IPv6HTTPServer(("::1", 0), Handler)
    except OSError as exc:
        if exc.errno not in (errno.EAFNOSUPPORT, errno.EADDRNOTAVAIL):
            raise
        return None
    stalled = ()
    # Only Linux leaves a connection pending behind a full accept queue: on
    # macOS the second connection completed (CI run 45949336473). Elsewhere
    # nothing listens on this port over IPv4, so the fallback check meets a
    # refused connection instead of a pending one.
    if os.environ.get("GOCCIA_FETCH_IPV6_HOST") and sys.platform.startswith("linux"):
        stalled = stall_ipv4(server.server_address[1])
    with open(port_file, "w") as f:
        f.write(str(server.server_address[1]))
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server, stalled


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 fetch_test_server.py <port-file>", file=sys.stderr)
        sys.exit(1)

    # Preserve evidence if setup stalls before the shell sees the port file.
    faulthandler.dump_traceback_later(15)
    try:
        server = LoopbackHTTPServer(("127.0.0.1", 0), Handler)
        ipv6 = start_ipv6(sys.argv[2]) if len(sys.argv) > 2 else None
        with open(sys.argv[1], "w") as f:
            f.write(str(server.server_address[1]))
    finally:
        faulthandler.cancel_dump_traceback_later()

    try:
        server.serve_forever()
    finally:
        if ipv6 is not None:
            server6, stalled = ipv6
            server6.shutdown()
            server6.server_close()
            for sock in stalled:
                sock.close()


if __name__ == "__main__":
    main()
