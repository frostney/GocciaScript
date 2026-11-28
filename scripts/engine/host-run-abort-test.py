#!/usr/bin/env python3
"""Exercise the run abort handle with an HTTP request already at the server."""

from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory
from threading import Thread

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from fetch_test_server import Handler, LoopbackHTTPServer


class AbortHandler(Handler):
    def do_GET(self):
        if self.path == "/sleep/1500/abort":
            self.server.request_ready.touch()
        try:
            super().do_GET()
        except (BrokenPipeError, ConnectionResetError):
            if self.path != "/sleep/1500/abort":
                raise


binary = Path(sys.argv[1]).resolve()
with TemporaryDirectory(dir=binary.parent) as scratch, LoopbackHTTPServer(("127.0.0.1", 0), AbortHandler) as server:
    server.request_ready = Path(scratch) / "request-ready"
    thread = Thread(target=server.serve_forever)
    thread.start()
    try:
        subprocess.run([str(binary), f"http://127.0.0.1:{server.server_port}", str(server.request_ready)],
                       check=True, timeout=20)
    finally:
        server.shutdown()
        thread.join()
