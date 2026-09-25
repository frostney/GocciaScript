#!/usr/bin/env python3
"""Shared deterministic GitHub observation mechanics for known-good-route."""

from __future__ import annotations

import hashlib
import json
import math
import os
import signal
import subprocess
import tempfile
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable


class WaitError(RuntimeError):
    """An actionable invocation or GitHub transport failure."""


class RateLimited(WaitError):
    """The primary GraphQL read is explicitly rate-limited."""


class TransientError(WaitError):
    """A temporary transport failure that may recover before the deadline."""


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def parse_time(value: str) -> float:
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as error:
        raise WaitError(f"invalid RFC 3339 timestamp: {value}") from error
    if parsed.tzinfo is None or parsed.utcoffset() is None:
        raise WaitError(f"RFC 3339 timestamp needs an explicit timezone: {value}")
    return parsed.timestamp()


def positive_interval(value: float) -> float:
    if not math.isfinite(value) or value <= 0:
        raise WaitError("poll interval must be a positive finite number")
    return value


@dataclass
class Metrics:
    started: float
    observations: int = 0
    api_requests: int = 0
    retries: int = 0
    rate_limit_fallbacks: int = 0

    def result(self) -> dict[str, int]:
        return {
            "durationMilliseconds": round((time.monotonic() - self.started) * 1000),
            "observations": self.observations,
            "apiRequests": self.api_requests,
            "retries": self.retries,
            "rateLimitFallbacks": self.rate_limit_fallbacks,
        }


class Gh:
    def __init__(self, metrics: Metrics, timeout: int = 30) -> None:
        self.metrics = metrics
        self.timeout = timeout

    def run(self, args: list[str], stdin: str | None = None) -> Any:
        self.metrics.api_requests += 1
        try:
            completed = subprocess.run(
                ["gh", *args],
                input=stdin,
                text=True,
                capture_output=True,
                timeout=self.timeout,
                check=False,
            )
        except FileNotFoundError as error:
            raise WaitError("gh is not installed or not on PATH") from error
        except subprocess.TimeoutExpired as error:
            raise TransientError(f"gh request timed out after {self.timeout}s") from error
        if completed.returncode != 0:
            message = (completed.stderr or completed.stdout).strip()
            lowered = message.lower()
            if "rate limit" in lowered or "rate-limit" in lowered or "quota" in lowered:
                raise RateLimited(message or "GitHub GraphQL rate limit exceeded")
            if any(marker in lowered for marker in ("timeout", "timed out", "connection reset", "temporary failure", "could not resolve host", "http 502", "http 503", "http 504")):
                raise TransientError(message or "temporary GitHub transport failure")
            raise WaitError(message or f"gh exited {completed.returncode}")
        try:
            payload = json.loads(completed.stdout)
        except json.JSONDecodeError as error:
            raise WaitError("gh returned malformed JSON") from error
        if isinstance(payload, dict) and payload.get("errors"):
            message = json.dumps(payload["errors"], sort_keys=True)
            lowered = message.lower()
            if "rate limit" in lowered or "rate-limit" in lowered or "quota" in lowered:
                raise RateLimited(message)
            raise WaitError(f"GitHub GraphQL error: {message}")
        return payload

    def graphql(self, query: str, variables: dict[str, str | int]) -> dict[str, Any]:
        args = ["api", "graphql", "--input", "-"]
        payload = self.run(args, json.dumps({"query": query, "variables": variables}))
        if not isinstance(payload, dict) or not isinstance(payload.get("data"), dict):
            raise WaitError("GitHub GraphQL response is missing data")
        return payload["data"]

    def rest(self, endpoint: str, method: str = "GET", fields: dict[str, str] | None = None) -> Any:
        args = ["api", "--method", method, endpoint]
        for key, value in (fields or {}).items():
            args.extend(["--raw-field", f"{key}={value}"])
        return self.run(args)

    def rest_pages(self, endpoint: str) -> list[Any]:
        payload = self.run(["api", "--paginate", "--slurp", endpoint])
        if not isinstance(payload, list):
            raise WaitError("paginated GitHub REST response is not an array")
        return payload


def stable_digest(value: Any) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def default_state_path(kind: str, identity: dict[str, Any]) -> Path:
    return Path(".agent/waits") / f"{kind}-{stable_digest(identity)[:12]}.json"


def load_state(path: Path | None) -> dict[str, Any] | None:
    if path is None or not path.exists():
        return None
    try:
        value = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise WaitError(f"cannot read checkpoint {path}: {error}") from error
    if value.get("schemaVersion") != 1:
        raise WaitError(f"unsupported checkpoint schema in {path}")
    return value


def write_state(path: Path | None, value: dict[str, Any]) -> None:
    if path is None:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    handle, temporary = tempfile.mkstemp(prefix="wait.", suffix=".tmp", dir=path.parent)
    try:
        with os.fdopen(handle, "w") as stream:
            json.dump(value, stream, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


class StateLock:
    def __init__(self, state: Path | None) -> None:
        self.path = Path(f"{state}.lock") if state else None
        self.owner = f"{os.getpid()}:{time.time_ns()}"

    @staticmethod
    def _process_exists(pid: int) -> bool:
        if os.name == "nt":
            import ctypes

            process_query_limited_information = 0x1000
            kernel32 = ctypes.windll.kernel32
            kernel32.OpenProcess.restype = ctypes.c_void_p
            handle = kernel32.OpenProcess(
                process_query_limited_information, False, pid
            )
            if not handle:
                return False
            kernel32.CloseHandle(handle)
            return True
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return False
        except PermissionError:
            return True
        return True

    def __enter__(self) -> "StateLock":
        if self.path:
            self.path.parent.mkdir(parents=True, exist_ok=True)
            for _attempt in range(2):
                try:
                    descriptor = os.open(
                        self.path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600
                    )
                    with os.fdopen(descriptor, "w") as stream:
                        stream.write(self.owner)
                        stream.write("\n")
                    break
                except FileExistsError as error:
                    try:
                        existing = self.path.read_text().strip()
                        pid = int(existing.split(":", 1)[0])
                    except (OSError, ValueError):
                        raise WaitError(
                            f"checkpoint lock is unreadable: {self.path}"
                        ) from error
                    if self._process_exists(pid):
                        raise WaitError(
                            f"checkpoint is already owned by process {pid}: {self.path}"
                        ) from error
                    try:
                        if self.path.read_text().strip() == existing:
                            self.path.unlink()
                    except FileNotFoundError:
                        pass
            else:
                raise WaitError(f"could not acquire checkpoint lock: {self.path}")
        return self

    def __exit__(self, *_: Any) -> None:
        if self.path:
            try:
                if self.path.read_text().strip() == self.owner:
                    self.path.unlink()
            except FileNotFoundError:
                pass


def result_envelope(
    kind: str,
    state: str,
    identity: dict[str, Any],
    observation: dict[str, Any],
    metrics: Metrics,
    reason: str,
) -> dict[str, Any]:
    return {
        "schemaVersion": 1,
        "kind": kind,
        "state": state,
        "reason": reason,
        "timestamp": utc_now(),
        "identity": identity,
        "observation": observation,
        "metrics": metrics.result(),
    }


def wait_for_transition(
    *,
    kind: str,
    identity: dict[str, Any],
    observe: Callable[[], dict[str, Any]],
    classify: Callable[[dict[str, Any]], tuple[str, str]],
    state_path: Path | None,
    deadline: float,
    interval: float,
    metrics: Metrics,
    transition_key: Callable[[dict[str, Any]], Any] | None = None,
    deadline_result: tuple[str, str] = ("timed-out", "deadline reached"),
    change_precedes_terminal: bool = False,
) -> dict[str, Any]:
    stopped = False

    def stop(_signum: int, _frame: Any) -> None:
        nonlocal stopped
        stopped = True

    previous_handlers = {
        number: signal.signal(number, stop) for number in (signal.SIGINT, signal.SIGTERM)
    }
    try:
        prior = load_state(state_path)
        if prior and (
            prior.get("kind") != kind or prior.get("identity") != identity
        ):
            observation = prior.get("observation", {})
            return result_envelope(
                kind,
                "invalidated",
                identity,
                observation,
                metrics,
                "checkpoint identity does not match the requested proof target",
            )
        while True:
            if stopped:
                observation = prior.get("observation", {}) if prior else {}
                result = result_envelope(kind, "cancelled", identity, observation, metrics, "interrupted")
                write_state(state_path, {"schemaVersion": 1, "kind": kind, "identity": identity, "observation": observation, "digest": prior.get("digest") if prior else None, "updatedAt": utc_now(), "terminalResult": result})
                return result
            try:
                observation = observe()
            except (RateLimited, TransientError) as error:
                metrics.retries += 1
                if time.time() >= deadline:
                    observation = prior.get("observation", {}) if prior else {}
                    result = result_envelope(
                        kind,
                        deadline_result[0],
                        identity,
                        observation,
                        metrics,
                        deadline_result[1],
                    )
                    write_state(
                        state_path,
                        {
                            "schemaVersion": 1,
                            "kind": kind,
                            "identity": identity,
                            "observation": observation,
                            "digest": prior.get("digest") if prior else None,
                            "updatedAt": utc_now(),
                            "connectivity": {
                                "state": "unavailable",
                                "error": str(error),
                            },
                            "terminalResult": result,
                        },
                    )
                    return result
                retry_at = min(deadline, time.time() + interval)
                write_state(
                    state_path,
                    {
                        "schemaVersion": 1,
                        "kind": kind,
                        "identity": identity,
                        "observation": prior.get("observation", {}) if prior else {},
                        "digest": prior.get("digest") if prior else None,
                        "updatedAt": utc_now(),
                        "connectivity": {
                            "state": "retrying",
                            "error": str(error),
                        },
                        "nextAttemptAt": datetime.fromtimestamp(
                            retry_at, timezone.utc
                        ).isoformat().replace("+00:00", "Z"),
                    },
                )
                time.sleep(min(interval, max(0.0, deadline - time.time())))
                continue
            metrics.observations += 1
            terminal, reason = classify(observation)
            digest = stable_digest(transition_key(observation) if transition_key else observation)
            checkpoint = {
                "schemaVersion": 1,
                "kind": kind,
                "identity": identity,
                "observation": observation,
                "digest": digest,
                "updatedAt": utc_now(),
                "nextAttemptAt": datetime.fromtimestamp(
                    min(deadline, time.time() + interval), timezone.utc
                ).isoformat().replace("+00:00", "Z"),
                "connectivity": {"state": "connected", "error": None},
            }
            write_state(state_path, checkpoint)
            if (
                change_precedes_terminal
                and prior
                and prior.get("identity") == identity
                and prior.get("digest") != digest
            ):
                result = result_envelope(kind, "changed", identity, observation, metrics, "authoritative state changed")
                checkpoint["terminalResult"] = result
                write_state(state_path, checkpoint)
                return result
            if terminal != "waiting":
                result = result_envelope(kind, terminal, identity, observation, metrics, reason)
                checkpoint["terminalResult"] = result
                write_state(state_path, checkpoint)
                return result
            if prior and prior.get("identity") == identity and prior.get("digest") != digest:
                result = result_envelope(kind, "changed", identity, observation, metrics, "authoritative state changed")
                checkpoint["terminalResult"] = result
                write_state(state_path, checkpoint)
                return result
            if time.time() >= deadline:
                result = result_envelope(
                    kind,
                    deadline_result[0],
                    identity,
                    observation,
                    metrics,
                    deadline_result[1],
                )
                checkpoint["terminalResult"] = result
                write_state(state_path, checkpoint)
                return result
            prior = checkpoint
            time.sleep(min(interval, max(0.0, deadline - time.time())))
    finally:
        for number, handler in previous_handlers.items():
            signal.signal(number, handler)


def emit(result: dict[str, Any], as_json: bool) -> None:
    if as_json:
        print(json.dumps(result, sort_keys=True, separators=(",", ":")))
    else:
        print(f"{result['state']}: {result['reason']}")
