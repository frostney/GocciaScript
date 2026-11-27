#!/usr/bin/env python3
"""Run the native event-loop fixture against an isolated fetch test server."""

from pathlib import Path
import subprocess
import sys
from threading import Thread

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from fetch_test_server import Handler, LoopbackHTTPServer


with LoopbackHTTPServer(("127.0.0.1", 0), Handler) as server:
    thread = Thread(target=server.serve_forever)
    thread.start()
    try:
        subprocess.run([sys.argv[1], f"http://127.0.0.1:{server.server_port}"], check=True, timeout=20)
    finally:
        server.shutdown()
        thread.join()
