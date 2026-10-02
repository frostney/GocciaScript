#!/usr/bin/env python3
"""Rewrite private GitHub references and local machine paths out of content bound
for a public repository.

The destination is the repository that receives the content: the branch's push
remote for `staged` and `outgoing` (or --remote), and the pull request's base
repository, given as --repo, for `pr`.

Local paths become neutral forms: a path inside the repository checkout or one
of its worktrees becomes ./relative, the home directory becomes ~, an agent
session or per-user temporary directory becomes /tmp, and a dash-encoded project
folder becomes example-project.

Standard library only. Exits 0 after rewriting or finding nothing to rewrite,
with any warnings in the report, and 2 without changing anything when Git or
GitHub cannot answer a question the guard depends on. The destination repository
can keep more JSON free-text lines by listing regular expressions in
.github/publication-guard.json as {"keep": ["^Ticket:"]}.
"""
from __future__ import annotations

import argparse
import bisect
import codecs
import getpass
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import time

try:
    import fcntl
except ImportError:  # Windows
    fcntl = None
    import msvcrt

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.

PLACEHOLDER_OWNER = "example-org"
KEEP_CONFIG = ".github/publication-guard.json"
STATE_FILE = "kgr-publication-guard.json"
LOCK_SECONDS = 30
STUB = "[redacted]"
OWNER = r"[A-Za-z0-9](?:[A-Za-z0-9]|-(?=[A-Za-z0-9])){0,38}"
REPO = r"[A-Za-z0-9._-]*[A-Za-z0-9_-]"
# A reference starts right after a string escape such as \n, never on the
# escape's letter, which would otherwise read as part of the name.
AFTER_ESCAPE = r"(?<=\\[ntr])"
URL_START = rf"(?:(?<![\w.@\\-])|{AFTER_ESCAPE})"
WORD_START = rf"(?:(?<![\w./@:#\\-])|{AFTER_ESCAPE})"
URL_TAIL = r"/[^\s)\]>\"'`]*"
# Every host spelling that names a repository: web, SSH, the REST API and raw files.
REPOSITORY_HOST = r"(?i:(?:www\.|ssh\.)?github\.com|api\.github\.com/repos|raw\.githubusercontent\.com)"
# Bounded, so a long unbroken line cannot make every position scan to its end.
URL_PREFIX = r"(?:(?i:[a-z][a-z0-9+.-]{0,15})://)?(?:[^\s@/:\"'`<>()\[\]]{1,100}(?::[^\s@/\"'`<>()\[\]]{0,200})?@)?"
REFERENCE = re.compile(
    rf"(?P<gist>{URL_START}(?P<gprefix>(?i:https?://)?)(?P<ghost>(?i:gist\.github(?:usercontent)?\.com))/"
    rf"(?:(?P<go>{OWNER})/)?(?P<gid>[0-9a-fA-F]{{20,32}}|\d+)(?![\w-])(?P<gtail>{URL_TAIL})?)"
    rf"|(?P<url>{URL_START}(?P<prefix>{URL_PREFIX})(?P<host>{REPOSITORY_HOST})(?P<sep>(?::\d+)?/|:(?!\d+/))"
    rf"(?P<uo>{OWNER})/(?P<ur>{REPO})(?P<tail>{URL_TAIL})?)"
    rf"|(?P<hash>{WORD_START}(?P<ho>{OWNER})/(?P<hr>{REPO})#(?P<hn>\d+)\b)"
    rf"|(?P<bare>{WORD_START}(?P<bo>{OWNER})/(?P<br>{REPO})(?![\w/#-]|\.\w))"
)
MARKDOWN_LINK = re.compile(r"\[[^\]\n]*\]\(\s*<?([^\s)<>]+)>?(?:\s+\"[^\"]*\")?\s*\)")
AUTOLINK = re.compile(r"<([^\s<>]+)>")
TAIL_NUMBER = re.compile(r"^/(pull|pulls|issues)/(\d+)")
SHA = re.compile(r"(?<![0-9A-Za-z_-])[0-9a-f]{7,64}(?![0-9A-Za-z_-])")
JSON_STRING = re.compile(r'"(?:[^"\\\n]|\\.)*"')
JSON_SPACE = re.compile(r"[ \t\n\r]*")
JSON_TOKEN = re.compile(r"true|false|null|NaN|-?Infinity|-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][-+]?\d+)?")
JSON_SUFFIXES = (".json",)
JSON_LINES_SUFFIXES = (".jsonl", ".ndjson")
PR_KEY = re.compile(r"^(?:prs?|pulls?|pull_?requests?(?:_?number)?|pr_?numbers?|number|issues?(?:_?number)?)$", re.I)
NOT_REPOSITORY_OWNERS = {
    "about", "account", "apps", "codespaces", "collections", "contact", "customer-stories", "dashboard",
    "enterprise", "explore", "features", "issues", "login", "marketplace", "new", "notifications", "orgs",
    "organizations", "pricing", "pulls", "readme", "search", "security", "settings", "site", "sponsors",
    "topics", "trending", "users",
}
REMOTE_URL = re.compile(
    r"(?:(?P<scheme>[A-Za-z][A-Za-z0-9+.-]*)://)?(?:[^@/\s]+@)?(?P<host>[A-Za-z0-9][^/:\s@]*)(?::(?P<port>\d+))?"
    r"(?P<sep>[:/])(?P<owner>[^/\s]+)/(?P<repo>[^/\s]+?)(?:\.git)?/?")
REPOSITORY_ARGUMENT = re.compile(r"(?:(?P<host>[^/\s]+)/)?(?P<owner>[^/\s]+)/(?P<repo>[^/\s]+)")
GITHUB_HOSTS = {"github.com", "www.github.com", "ssh.github.com"}
BYTE_ORDER_MARKS = (  # UTF-32 first: its little-endian mark starts with UTF-16's.
    (codecs.BOM_UTF32_LE, "utf-32-le"), (codecs.BOM_UTF32_BE, "utf-32-be"), (codecs.BOM_UTF8, "utf-8"),
    (codecs.BOM_UTF16_LE, "utf-16-le"), (codecs.BOM_UTF16_BE, "utf-16-be"),
)
# A path prefix ends where a name cannot continue; it starts where no other path or word does.
NAME_END = r"(?![\w-]|\.\w)"
PATH_START = rf"(?:(?<![\w.~-])|{AFTER_ESCAPE})"
# Any run of slashes or backslashes, so escaped (\/ or \\), doubled (//) and
# dot-segment (/./) spellings of one separator match too.
SEPARATOR = r"(?:[/\\]+(?:\.[/\\]+)*)"
# A file: URI is replaced whole, so the neutral path that replaces it stays valid.
FILE_URI = r"(?:(?i:file)://(?i:localhost)?(?:/(?=[A-Za-z]:))?)?"
SEGMENT = r"[^\s/\\\"'`)\]>|,;]"
# Per-session and per-user temporary roots. /tmp and /private/tmp on their own
# name nothing personal and code uses them literally, so they stay.
TEMP_ROOTS = re.compile(
    rf"{PATH_START}{FILE_URI}(?:(?:{SEPARATOR}private)?{SEPARATOR}tmp{SEPARATOR}claude-[\w.-]+(?=[/\\])"
    rf"|(?:{SEPARATOR}private)?{SEPARATOR}var{SEPARATOR}folders{SEPARATOR}[\w+.-]+{SEPARATOR}[\w+.-]+"
    rf"(?:{SEPARATOR}[A-Z0-9]{NAME_END})?{NAME_END})"
)
# Agent tools name session folders after the working directory with every
# separator turned into a dash, such as -Users-<name>-<project>.
ENCODED_PROJECT = re.compile(rf"(?<![\w.-])(?:[A-Za-z]-)?-(?:Users|home)-[A-Za-z0-9_.]+-{SEGMENT}*")
STATUS = r"pass(?:ed|ing)?|fail(?:ed|ing|ure)?|ok|error|success(?:ful)?|pending|skipped|cancell?ed|timed[ -]out|neutral|queued|in[ _-]progress|completed|blocked|merged|closed|open"
DEFAULT_KEEP = (
    r"^\s*(?:>\s*)?(?:\*\*[^*\n]{1,80}:\*\*|<!--.*-->|\[![A-Z]+\])",
    rf"^\s*(?:[-*+]\s+)?(?:\[[ x]\]\s+)?(?:✅|❌|⚠️|(?:{STATUS})\b)",
    rf"^\s*[\w .-]{{1,40}}:\s*(?:{STATUS})\s*\.?\s*$",
    r"\b\d+(?:[.,]\d+)?\s?(?:%|ms|s|secs?|seconds?|mins?|minutes?|h|hrs?|hours?|days?|weeks?|months?|[kmgt]i?b|bytes?|tokens?|lines?|files?|commits?|tests?|reviews?|requests?|runs?|jobs?|checks?)(?!\w)",
    r"\b\d+(?:[.,]\d+)?\s?(?:per|/)\s?(?:hour|minute|second|day|h|min|s)\b",
)


class GuardError(RuntimeError):
    pass


def run(args, *, data=None, cwd=None, check=True):
    try:
        result = subprocess.run(args, input=data, cwd=cwd, capture_output=True)
    except FileNotFoundError as error:
        raise GuardError(f"{args[0]} is not installed or not on PATH") from error
    if check and result.returncode:
        raise GuardError(f"{' '.join(args[:3])} failed: {result.stderr.decode(errors='replace').strip()}")
    return result


def git(*args, data=None, cwd=None):
    return run(["git", *args], data=data, cwd=cwd).stdout


def git_paths(*args, cwd=None):
    return [path for path in git(*args, cwd=cwd).decode("utf-8", errors="surrogateescape").split("\0") if path]


def slug_of(match):
    """The kind of reference a REFERENCE match is and the key the resolver looks it up by."""
    if match.group("gist"):
        return "gist", f"gist:{match.group('gid').lower()}"
    kind = next(k for k in ("url", "hash", "bare") if match.group(k))
    owner, repo = {"url": ("uo", "ur"), "hash": ("ho", "hr"), "bare": ("bo", "br")}[kind]
    repo_name = match.group(repo)
    if kind == "url" and repo_name.lower().endswith(".git"):
        repo_name = repo_name[:-4]
    return kind, f"{match.group(owner)}/{repo_name}"


def gh_api(*args):
    return run(["gh", "api", "--hostname", "github.com", *args], check=False)


class Resolver:
    """Answers, through `gh api` as the current user, which references are private.

    A question GitHub cannot answer stops the guard: guessing private would
    rewrite ordinary text, and guessing public would publish a private name.
    """

    def __init__(self, destination=None):
        self.cache = {destination.lower(): "public"} if destination else {}
        self.owners = None
        self.calls = 0

    def lookup(self, key):
        key = key.lower()
        if key not in self.cache:
            self.calls += 1
            gist = key.startswith("gist:")
            result = gh_api(f"gists/{key[5:]}" if gist else f"repos/{key}")
            if result.returncode == 0:
                try:
                    body = json.loads(result.stdout)
                    if gist:
                        public = body["public"] is True
                    else:
                        public = body.get("visibility", "private" if body.get("private", True) else "public") == "public"
                except (ValueError, AttributeError, KeyError, TypeError) as error:
                    raise GuardError(f"GitHub gave an unreadable answer about {key}") from error
                self.cache[key] = "public" if public else "private"
            elif b"HTTP 404" in result.stderr:
                self.cache[key] = "missing"
            else:
                raise GuardError(f"cannot ask GitHub whether {key} is private: "
                                 f"{result.stderr.decode(errors='replace').strip()}")
        return self.cache[key]

    def own_owners(self):
        if self.owners is None:
            names = []
            for args in (("user", "--jq", ".login"), ("--paginate", "user/orgs", "--jq", ".[].login")):
                result = gh_api(*args)
                if result.returncode:
                    raise GuardError(f"cannot ask GitHub which accounts the current user owns: "
                                     f"{result.stderr.decode(errors='replace').strip()}")
                names += result.stdout.decode().split()
            self.owners = {name.lower() for name in names}
        return self.owners

    def is_private(self, key, forms):
        state = self.lookup(key)
        if state == "missing" and forms == {"bare"}:
            # A bare word pair such as and/or is not a repository reference unless
            # its owner is the current user or one of their organizations.
            return key.split("/")[0].lower() in self.own_owners()
        return state != "public"


class Mapper:
    """Stable placeholders, persisted under the Git directory so later runs agree.

    Used as a context manager: loading, mapping and saving happen under one
    lock, so parallel runs from several worktrees never hand out one placeholder twice.
    """

    def __init__(self, path):
        self.path = path
        self.state = {"repos": {}, "numbers": {}, "shas": {}, "gists": {}}
        self.lock = None

    def __enter__(self):
        if self.path:
            self.lock = open(self.path.with_name(self.path.name + ".lock"), "a+b")
            deadline = time.monotonic() + LOCK_SECONDS
            while not self._try_lock():
                if time.monotonic() > deadline:
                    self.lock.close()
                    raise GuardError(f"another publication guard run still holds {self.lock.name}")
                time.sleep(0.05)
            try:
                self._load()
            except BaseException:
                self._unlock()
                raise
        return self

    def __exit__(self, kind, value, traceback):
        try:
            if kind is None:
                self.save()
        finally:
            self._unlock()

    def _try_lock(self):
        try:
            if fcntl:
                fcntl.flock(self.lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            else:
                self.lock.seek(0)
                msvcrt.locking(self.lock.fileno(), msvcrt.LK_NBLCK, 1)
            return True
        except OSError:
            return False

    def _unlock(self):
        if self.lock:
            if not fcntl:
                self.lock.seek(0)
                msvcrt.locking(self.lock.fileno(), msvcrt.LK_UNLCK, 1)
            self.lock.close()  # Closing releases an flock.
            self.lock = None

    def _load(self):
        if not self.path.is_file():
            return
        try:
            loaded = json.loads(self.path.read_text(encoding="utf-8"))
            if not isinstance(loaded, dict) or not all(isinstance(loaded.get(k, {}), dict) for k in self.state):
                raise ValueError("not a placeholder map")
        except (OSError, ValueError) as error:
            raise GuardError(f"cannot read the placeholder map {self.path}: {error}; move it aside and rerun") from error
        for name in self.state:
            self.state[name].update(loaded.get(name, {}))

    def repo(self, slug):
        repos = self.state["repos"]
        return repos.setdefault(slug.lower(), f"{PLACEHOLDER_OWNER}/example-repo-{len(repos) + 1}")

    def gist(self, key):
        gists = self.state["gists"]
        return gists.setdefault(key.lower(), f"{PLACEHOLDER_OWNER}/example-gist-{len(gists) + 1}")

    def number(self, slug, value):
        numbers = self.state["numbers"].setdefault(slug.lower(), {})
        return numbers.setdefault(str(value), len(numbers) + 1)

    def sha(self, value):
        """A fake SHA that keeps every abbreviation a prefix of the longer forms, in any order."""
        shas = self.state["shas"]
        if value not in shas:
            fresh = hashlib.sha256(f"example-sha-{len(shas) + 1}".encode()).hexdigest()
            longer = next((shas[k] for k in shas if k.startswith(value)), None)
            prefix = max((shas[k] for k in shas if value.startswith(k)), key=len, default="")
            shas[value] = longer[:len(value)] if longer else (prefix + fresh[len(prefix):])[:len(value)]
        return shas[value]

    def save(self):
        if self.path and any(self.state.values()):
            handle, temporary = tempfile.mkstemp(prefix=self.path.name + ".", suffix=".tmp", dir=self.path.parent)
            try:
                with os.fdopen(handle, "w", encoding="utf-8") as stream:
                    json.dump(self.state, stream, indent=1, sort_keys=True)
                os.replace(temporary, self.path)
            finally:
                if os.path.exists(temporary):
                    os.unlink(temporary)


def path_variants(path):
    """The spellings one directory can appear under, such as macOS /private/tmp and /tmp."""
    path = str(path).rstrip("/\\")
    if not path:
        return set()
    found = {path, os.path.realpath(path)} if path.startswith("/") else {path}
    return found | {item[len("/private"):] for item in found if item.startswith("/private/")}


def path_parts(path):
    return [part for part in re.split(r"[/\\]+", path) if part]


def prefix_pattern(path):
    parts = path_parts(path)
    drive = re.fullmatch(r"[A-Za-z]:", parts[0]) is not None
    return (PATH_START + FILE_URI + ("" if drive else SEPARATOR)
            + SEPARATOR.join(re.escape(part) for part in parts) + NAME_END)


def literal_rules(paths, replacement, alone=None):
    """Rules for each spelling of PATHS, longest first; ALONE replaces a path with nothing after it."""
    variants = {variant for path in paths for variant in path_variants(path)}
    rules = []
    for variant in sorted(variants, key=len, reverse=True):
        pattern = prefix_pattern(variant)
        if alone is None:
            rules.append((re.compile(pattern, re.I), replacement))
        else:
            rules.append((re.compile(pattern + r"(?=[/\\])", re.I), replacement))
            rules.append((re.compile(pattern + r"(?![/\\])", re.I), alone))
    return rules


class LocalPaths:
    """Rewrites absolute paths that name this machine's user, folders or agent session."""

    def __init__(self, main_checkout=None, linked_worktrees=(), temp_dirs=(), homes=(), username=None):
        homes = {home for home in homes if home and home.strip("/\\")}
        self.rules = literal_rules(temp_dirs, "/tmp")
        self.rules.append((TEMP_ROOTS, "/tmp"))
        if username:
            self.rules.append((re.compile(
                rf"{PATH_START}{FILE_URI}(?:[A-Za-z]:|{SEPARATOR}mnt{SEPARATOR}[A-Za-z]|{SEPARATOR}[A-Za-z](?=[/\\]))?"
                rf"{SEPARATOR}(?:Users|home){SEPARATOR}{re.escape(username)}{NAME_END}", re.I), "~"))
        self.rules += literal_rules(homes, "~")
        for home in sorted(homes):
            # A one-segment home such as /root encodes to -root, which is also a
            # command-line flag, so only longer homes get the encoded rule.
            if len(path_parts(home)) < 2:
                continue
            encoded = re.escape(re.sub(r"[^A-Za-z0-9]", "-", home.rstrip("/\\")))
            self.rules.append((re.compile(rf"(?<![\w.-]){encoded}(?:-{SEGMENT}*)?(?!{SEGMENT})"), "example-project"))
        self.rules.append((ENCODED_PROJECT, "example-project"))
        # A main checkout outside the home and temporary folders, such as /app in
        # a container, names nothing personal, and files there often mean that
        # path literally (WORKDIR /app), so only checkouts these rules would
        # rewrite anyway become relative.
        checkouts = list(linked_worktrees)
        if main_checkout and any(self.rewrite(variant)[1] or re.match(r"(?:/private)?(?:/var)?/tmp/.", variant)
                                 for variant in path_variants(main_checkout)):
            checkouts.append(main_checkout)
        # ./ for the checkout on its own, so "in <checkout>." does not read as "in .."
        self.rules[:0] = literal_rules(checkouts, ".", alone="./")

    @classmethod
    def from_environment(cls, root):
        worktrees = [str(root)]
        result = run(["git", "worktree", "list", "--porcelain", "-z"], cwd=root, check=False)
        if result.returncode == 0:
            worktrees = [line[len("worktree "):] for line in result.stdout.decode(errors="replace").split("\0")
                         if line.startswith("worktree ")] or worktrees
        homes = [str(Path.home()), os.environ.get("HOME", ""), os.environ.get("USERPROFILE", "")]
        try:
            username = getpass.getuser()
        except Exception:  # getuser raises OSError or KeyError when no user can be named.
            username = None
        temp_dirs = [os.environ.get(name, "") for name in ("TMPDIR", "TEMP", "TMP")]
        return cls(worktrees[0], worktrees[1:], temp_dirs, homes, username)

    def rewrite(self, text):
        count = 0

        def substitute(replacement):
            def apply(match):
                nonlocal count
                count += match.group(0) != replacement
                return replacement
            return apply

        for pattern, replacement in self.rules:
            text = pattern.sub(substitute(replacement), text)
        return text, count

    def present(self, texts):
        return any(self.rewrite(text)[1] for text in texts)


class Sanitiser:
    def __init__(self, private, public, mapper, keep, local=None):
        self.private = private
        self.public = public
        self.mapper = mapper
        self.keep = keep
        self.local = local
        self.counts = {}
        self.placeholders = set()

    def fresh(self):
        """The same rules with separate counts, for a pass the report must not count twice."""
        return Sanitiser(self.private, self.public, self.mapper, self.keep, self.local)

    def bump(self, name, amount=1):
        self.counts[name] = self.counts.get(name, 0) + amount

    def private_slug(self, match):
        _, key = slug_of(match)
        return key if key.lower() in self.private else None

    def neutral(self, match):
        kind, _ = slug_of(match)
        if kind == "gist":
            return "a private gist"
        if kind == "hash":
            return "an issue or pull request in a private repository"
        tail = (match.group("tail") or "") if kind == "url" else ""
        section = tail.split("/")[1] if tail.count("/") >= 2 else ""
        return {"pull": "a pull request in a private repository", "pulls": "a pull request in a private repository",
                "issues": "an issue in a private repository", "commit": "a commit in a private repository",
                "commits": "a commit in a private repository"}.get(section, "a private repository")

    def prose(self, text):
        def whole(match):
            # A link or <autolink> whose target is a private reference becomes
            # the neutral phrase, brackets included, so no broken markup is left.
            inner = REFERENCE.match(match.group(1))
            if inner and inner.end() == len(match.group(1)) and self.private_slug(inner):
                self.bump("references")
                return self.neutral(inner)
            return match.group(0)

        def reference(match):
            if not self.private_slug(match):
                return match.group(0)
            tail = match.group("tail") or match.group("gtail")
            trailing = ""
            if tail:
                stripped = tail.rstrip(".,;:!?")
                trailing = tail[len(stripped):]
            self.bump("references")
            return self.neutral(match) + trailing

        return REFERENCE.sub(reference, AUTOLINK.sub(whole, MARKDOWN_LINK.sub(whole, text)))

    def local_paths(self, text):
        if self.local is None:
            return text
        text, count = self.local.rewrite(text)
        if count:
            self.bump("paths", count)
        return text

    def metadata(self, text):
        return self.prose(self.local_paths(text))

    def data_string(self, text, context, stub=True):
        """One decoded JSON string or key, which gets the same protection as raw text."""
        def sha(match):
            value = match.group(0)
            if not (re.search(r"\d", value) and re.search(r"[a-f]", value)):
                return value
            self.bump("shas")
            return self.mapper.sha(value)

        def reference(match):
            private = self.private_slug(match)
            if not private:
                return match.group(0)
            self.bump("references")
            kind, _ = slug_of(match)
            if kind == "gist":
                placeholder = self.mapper.gist(private)
                self.placeholders.add(placeholder)
                return f"{match.group('gprefix')}{match.group('ghost')}/{placeholder}{match.group('gtail') or ''}"
            placeholder = self.mapper.repo(private)
            self.placeholders.add(placeholder)
            if kind == "hash":
                self.bump("numbers")
                return f"{placeholder}#{self.mapper.number(private, int(match.group('hn')))}"
            if kind == "bare":
                return placeholder
            suffix = match.group("ur")[len(private.split("/")[1]):]  # Keeps a .git suffix.
            tail = match.group("tail") or ""
            numbered = TAIL_NUMBER.match(tail)
            if numbered:
                self.bump("numbers")
                tail = f"/{numbered.group(1)}/{self.mapper.number(private, int(numbered.group(2)))}" + tail[numbered.end():]
            return f"{match.group('prefix')}{match.group('host')}{match.group('sep')}{placeholder}{suffix}{tail}"

        text = REFERENCE.sub(reference, self.local_paths(text))
        if context is None:
            return text
        text = SHA.sub(sha, text)
        if stub and ("\n" in text or len(text) >= 40) and re.search(r"\s", text.strip()):
            lines = text.split("\n")
            kept = [line if not line.strip() or any(p.search(line) for p in self.keep) else STUB for line in lines]
            self.bump("stubbedLines", sum(1 for old, new in zip(lines, kept) if old != new))
            text = "\n".join(kept)
        return text

    def context(self, values, inherited):
        """The private repository an object or list describes, from its own strings.

        An object that names a public repository and no private one starts a
        public context, so its PR numbers and SHAs keep their real values.
        """
        strings = [value for value in values if isinstance(value, str)]
        matches = [match for value in strings for match in REFERENCE.finditer(value)]
        for match in matches:
            found = self.private_slug(match)
            if found:
                return found
        if any(slug_of(match)[1].lower() in self.public for match in matches):
            return None
        return inherited

    def json_edits(self, node, editable, edits, context=None, key=None):
        """Collects (start, end, replacement) spans for the strings, keys and PR numbers EDITABLE allows."""
        if node.members is not None:
            context = self.context([*node.value.keys(), *node.value.values()], context)
            renamed = []
            for name, _ in node.members:
                new = self.data_string(name.value, context, stub=False) if editable(name) else name.value
                if new != name.value:
                    renamed.append((name, new))
            # Two keys can now read the same; the renamed one gets a numbered
            # suffix so no value is lost.
            taken = {name.value for name, _ in node.members} - {name.value for name, _ in renamed}
            for name, new in renamed:
                unique, number = new, 2
                while unique in taken:
                    unique, number = f"{new} ({number})", number + 1
                taken.add(unique)
                edits.append((name.start, name.end, json.dumps(unique, ensure_ascii=False)))
            for name, item in node.members:
                self.json_edits(item, editable, edits, context, name.value)
        elif node.items is not None:
            context = self.context(node.value, context)
            for item in node.items:
                self.json_edits(item, editable, edits, context, key)
        elif editable(node):
            value, new = node.value, node.value
            if isinstance(value, str):
                new = self.data_string(value, context)
            elif (isinstance(value, int) and not isinstance(value, bool) and context is not None
                    and key is not None and PR_KEY.match(key)):
                new = self.mapper.number(context, value)
                if new != value:
                    self.bump("numbers")
            if new != value:
                edits.append((node.start, node.end, json.dumps(new, ensure_ascii=False)))

    def json_document(self, text, editable=lambda node: True):
        """TEXT with edits made in place, so formatting and untouched lines stay byte for byte; None when not JSON."""
        root = parse_json(text)
        if root is None:
            return None
        edits = []
        self.json_edits(root, editable, edits)
        for start, end, replacement in sorted(edits, reverse=True):
            text = text[:start] + replacement + text[end:]
        return text

    def document(self, path, text, base_lines=None):
        """TEXT rewritten only on lines absent from BASE_LINES, the lines the base already publishes."""
        lines = text.split("\n")  # Not splitlines: a JSON string may hold U+2028.
        fresh = [base_lines is None or line.rstrip("\r") not in base_lines for line in lines]
        if not any(fresh):
            return text
        lower = path.lower()
        if lower.endswith(JSON_SUFFIXES):
            starts = [0]
            for line in lines[:-1]:
                starts.append(starts[-1] + len(line) + 1)
            rewritten = self.json_document(text, lambda node: fresh[bisect.bisect_right(starts, node.start) - 1])
            if rewritten is not None:
                return rewritten
        jsonl = lower.endswith(JSON_LINES_SUFFIXES)
        out = []
        for line, new in zip(lines, fresh):
            if new and line.strip():
                rewritten = self.json_document(line) if jsonl else None
                line = self.metadata(line) if rewritten is None else rewritten
            out.append(line)
        return "\n".join(out)


class JsonNode:
    """One parsed JSON value and its span in the text."""

    def __init__(self, start):
        self.start, self.end, self.value, self.members, self.items = start, start, None, None, None


def parse_json(text):
    """TEXT as a JsonNode tree when it holds exactly one JSON value, else None."""
    def skip(at):
        return JSON_SPACE.match(text, at).end()

    def value(at):
        node, char = JsonNode(at), text[at:at + 1]
        if char in ("{", "["):
            close, at = "}" if char == "{" else "]", skip(at + 1)
            children = []
            while text[at:at + 1] != close:
                if children:
                    if text[at:at + 1] != ",":
                        raise ValueError("expected a comma")
                    at = skip(at + 1)
                if char == "{":
                    if text[at:at + 1] != '"':
                        raise ValueError("expected a key")
                    name = value(at)
                    at = skip(name.end)
                    if text[at:at + 1] != ":":
                        raise ValueError("expected a colon")
                    item = value(skip(at + 1))
                    children.append((name, item))
                else:
                    item = value(at)
                    children.append(item)
                at = skip(item.end)
            if char == "{":
                node.members, node.value = children, {name.value: item.value for name, item in children}
            else:
                node.items, node.value = children, [item.value for item in children]
            at += 1
        elif char == '"':
            node.value, at = json.decoder.scanstring(text, at + 1)
        else:
            token = JSON_TOKEN.match(text, at)
            if not token:
                raise ValueError("expected a value")
            node.value, at = json.loads(token.group(0)), token.end()
        node.end = at
        return node

    try:
        root = value(skip(0))
    except (ValueError, RecursionError):
        return None
    return root if skip(root.end) == len(text) else None


def decode(data):
    """(text, byte-order mark, codec) for UTF-8 or a BOM-marked Unicode encoding, else None."""
    for mark, codec in BYTE_ORDER_MARKS:
        if data.startswith(mark):
            try:
                return data[len(mark):].decode(codec), mark, codec
            except UnicodeDecodeError:
                return None
    try:
        return data.decode("utf-8"), b"", "utf-8"
    except UnicodeDecodeError:
        return None


def blob(root, spec):
    result = run(["git", "cat-file", "blob", spec], cwd=root, check=False)
    return result.stdout if result.returncode == 0 else None


def scan_texts(path, lines):
    """LINES plus every JSON string literal in them decoded, for JSON files."""
    if not path.lower().endswith(JSON_SUFFIXES + JSON_LINES_SUFFIXES):
        return lines
    decoded = []
    for line in lines:
        for literal in JSON_STRING.findall(line):
            try:
                decoded.append(json.loads(literal))
            except ValueError:
                pass
    return lines + decoded


def changes(root, paths, new, olds):
    """Per path, the lines its NEW version adds and the lines all its OLDS versions already have,
    plus the paths it cannot read.

    NEW and OLDS turn a path into a `git cat-file` object name. Comparing whole
    blobs line by line covers binary-classified text, merges against every
    parent and any file name, with no patch to parse.
    """
    added, unreadable = {}, []
    for path in paths:
        data = blob(root, new(path))
        if data is None:  # Deleted, or a submodule.
            continue
        current = decode(data)
        if current is None:
            unreadable.append(path)
            continue
        seen = set()
        for old in olds:
            previous = blob(root, old(path))
            decoded = decode(previous) if previous is not None else None
            if decoded:
                seen.update(line.rstrip("\r") for line in decoded[0].split("\n"))
        lines = [line for line in current[0].split("\n") if line.strip() and line.rstrip("\r") not in seen]
        if lines:
            added[path] = (lines, seen)
    return added, unreadable


def has_ref(root, name):
    return run(["git", "rev-parse", "-q", "--verify", f"{name}^{{commit}}"], cwd=root, check=False).returncode == 0


def staged_changes(root):
    paths = git_paths("diff", "--cached", "--name-only", "-z", "--no-renames", cwd=root)
    olds = [lambda path: f"HEAD:{path}"]
    if has_ref(root, "MERGE_HEAD"):
        # During a merge, lines the incoming side already has are published
        # there, so a path that matches it adds nothing to check.
        olds.append(lambda path: f"MERGE_HEAD:{path}")
    return changes(root, paths, lambda path: f":0:{path}", olds)


def outgoing_changes(root, base):
    fork = git("merge-base", base, "HEAD", cwd=root).decode().strip()
    paths = git_paths("diff", "--name-only", "-z", "--no-renames", f"{base}...HEAD", cwd=root)
    return changes(root, paths, lambda path: f"HEAD:{path}", [lambda path: f"{fork}:{path}"])


def outgoing_commits(root, base):
    """Each commit after BASE, merges included, with its message, added lines per file and unreadable files."""
    commits = []
    for sha in git("rev-list", "--reverse", f"{base}..HEAD", cwd=root).decode().split():
        parents = git("rev-list", "--parents", "-n", "1", sha, cwd=root).decode().split()[1:]
        message = git("show", "-s", "--format=%B", sha, cwd=root).decode("utf-8", errors="replace")
        # -c lists only the paths a merge result changes against every parent.
        paths = git_paths("diff-tree", "-r", "--no-commit-id", "--name-only", "-z", "--no-renames",
                          "-c" if len(parents) > 1 else "--root", sha, cwd=root)
        olds = [lambda path, parent=parent: f"{parent}:{path}" for parent in parents]
        added, unreadable = changes(root, paths, lambda path: f"{sha}:{path}", olds)
        commits.append((sha, message, {path: lines for path, (lines, _) in added.items()}, unreadable))
    return commits


def candidates(texts, root, base_dir=""):
    found = {}
    for text in texts:
        for match in REFERENCE.finditer(text):
            kind, key = slug_of(match)
            if kind == "gist":
                if (match.group("go") or "").lower() != PLACEHOLDER_OWNER:
                    found.setdefault(key, set()).add(kind)
                continue
            owner, repo = key.split("/")
            if owner.lower() == PLACEHOLDER_OWNER:
                continue
            if kind == "url" and owner.lower() in NOT_REPOSITORY_OWNERS:
                continue
            if kind == "bare" and (owner.isdigit() or repo.isdigit() or (root / key).exists()
                                   or (root / base_dir / key).exists()):
                continue
            found.setdefault(key, set()).add(kind)
    return found


def load_keep(root, warnings):
    patterns = list(DEFAULT_KEEP)
    config = root / KEEP_CONFIG
    if config.is_file():
        try:
            extra = json.loads(config.read_text(encoding="utf-8")).get("keep", [])
            patterns += [p for p in extra if isinstance(p, str)]
        except (OSError, ValueError, AttributeError):
            warnings.append(f"{KEEP_CONFIG} is not valid JSON; using the default keep list")
    compiled = []
    for pattern in patterns:
        try:
            compiled.append(re.compile(pattern, re.I))
        except re.error:
            warnings.append(f"skipped invalid keep pattern {pattern!r}")
    return compiled


def repository_root():
    result = run(["git", "rev-parse", "--show-toplevel", "--git-common-dir"], check=False)
    if result.returncode:
        return Path.cwd(), None
    top, common = result.stdout.decode().strip().split("\n")
    return Path(top), (Path(top) / common).resolve() / STATE_FILE


def push_remote(root):
    """The remote `git push` sends this branch to, in Git's own order of settings."""
    branch = run(["git", "symbolic-ref", "-q", "--short", "HEAD"], cwd=root, check=False).stdout.decode().strip()
    keys = ([f"branch.{branch}.pushRemote"] if branch else []) + ["remote.pushDefault"]
    keys += [f"branch.{branch}.remote"] if branch else []
    for key in keys:
        result = run(["git", "config", "--get", key], cwd=root, check=False)
        if result.returncode not in (0, 1):
            raise GuardError(f"cannot read git config {key}: {result.stderr.decode(errors='replace').strip()}")
        if result.stdout.strip():
            return result.stdout.decode().strip()
    return "origin"


def ssh_hostname(alias):
    result = run(["ssh", "-G", alias], check=False) if re.fullmatch(r"[\w.-]+", alias) else None
    if result is None or result.returncode:
        return alias
    for line in result.stdout.decode(errors="replace").splitlines():
        if line.lower().startswith("hostname "):
            return line.split(None, 1)[1].strip()
    return alias


def remote_repository(root, remote):
    result = run(["git", "remote", "get-url", "--push", remote], cwd=root, check=False)
    if result.returncode:
        raise GuardError(f"the push remote {remote!r} does not exist; add it or pass --remote NAME")
    url = result.stdout.decode().strip()
    match = REMOTE_URL.fullmatch(url)
    if not match:
        raise GuardError(f"cannot tell which GitHub repository {url!r} is")
    host = match.group("host").lower()
    ssh = (match.group("scheme") or "").lower() in {"ssh", "git+ssh"} or (not match.group("scheme") and match.group("sep") == ":")
    if ssh and host not in GITHUB_HOSTS:
        host = ssh_hostname(host).lower()  # A ~/.ssh/config alias such as github-work.
    return ("github.com" if host in GITHUB_HOSTS else host), f"{match.group('owner')}/{match.group('repo')}"


def destination(args, root):
    if args.operation == "pr":
        match = REPOSITORY_ARGUMENT.fullmatch(args.repo)
        if not match:
            raise GuardError(f"--repo takes [HOST/]OWNER/REPO, not {args.repo!r}")
        host, slug = (match.group("host") or "github.com").lower(), f"{match.group('owner')}/{match.group('repo')}"
    else:
        host, slug = remote_repository(root, args.remote or push_remote(root))
    name = slug if host == "github.com" else f"{host}/{slug}"
    result = run(["gh", "api", "--hostname", host, f"repos/{slug}", "--jq", ".visibility"], check=False)
    if result.returncode:
        raise GuardError(f"cannot ask GitHub whether {name} is public: {result.stderr.decode(errors='replace').strip()}")
    return host, slug, name, result.stdout.decode().strip()


def read_argument(path):
    try:
        return Path(path).read_bytes().decode("utf-8")
    except (OSError, UnicodeDecodeError) as error:
        raise GuardError(f"cannot read {path} as UTF-8 text: {error}") from error


def rewrite_index(root, path, sanitiser, base_lines):
    entry = git("ls-files", "-s", "-z", "--", f":(literal){path}", cwd=root).decode(errors="surrogateescape")
    staged = blob(root, f":0:{path}")
    decoded = decode(staged) if staged is not None and entry else None
    if decoded is None:
        return False
    original, mark, codec = decoded
    rewritten = sanitiser.document(path, original, base_lines)
    data = mark + rewritten.encode(codec)
    if rewritten != original:
        # --no-filters keeps the bytes as given, so line endings and encodings stay.
        object_id = git("hash-object", "-w", "--stdin", "--no-filters", data=data, cwd=root).decode().strip()
        git("update-index", "--cacheinfo", f"{entry.split()[0]},{object_id},{path}", cwd=root)
    worktree = root / path
    # A symlink is never written through: its target can be any file on the machine.
    if worktree.is_file() and not worktree.is_symlink():
        current = worktree.read_bytes()
        if current == staged:
            updated = data
        else:
            # Unstaged edits stay; the same rules apply to them, uncounted.
            unstaged = decode(current)
            if unstaged is None:
                return rewritten != original
            updated = unstaged[1] + sanitiser.fresh().document(path, unstaged[0], base_lines).encode(unstaged[2])
        if updated != current:
            worktree.write_bytes(updated)
    return rewritten != original


def guard(args):
    root, state_path = repository_root()
    host, slug, name, visibility = destination(args, root)
    report = {"destination": name, "public": visibility == "public", "rewritten": [], "warnings": [], "unchecked": []}
    if visibility != "public":
        return report
    texts = {key: read_argument(getattr(args, key)) for key in ("message_file", "title_file", "body_file")
             if getattr(args, key, None)}
    outgoing = args.operation == "outgoing"
    if args.operation == "staged":
        changed, unreadable = staged_changes(root)
    elif outgoing:
        changed, unreadable = outgoing_changes(root, args.base)
    else:
        changed, unreadable = {}, []
    files = {path: lines for path, (lines, _) in changed.items()}
    history = outgoing_commits(root, args.base) if outgoing else []
    unchecked = dict.fromkeys(unreadable)
    for _, _, _, more in history:
        unchecked.update(dict.fromkeys(more))
    report["unchecked"] = list(unchecked)
    found = {}
    for path, lines in [*files.items(), *(item for _, _, added, _ in history for item in added.items())]:
        for key, forms in candidates(scan_texts(path, lines), root, str(Path(path).parent)).items():
            found.setdefault(key, set()).update(forms)
    for key, forms in candidates(list(texts.values()) + [message for _, message, _, _ in history], root).items():
        found.setdefault(key, set()).update(forms)
    resolver = Resolver(slug if host == "github.com" else None)
    private = {key.lower() for key, forms in found.items() if resolver.is_private(key, forms)}
    public = {key for key, state in resolver.cache.items() if state == "public"}
    local = LocalPaths.from_environment(root)

    def carries(path, lines):
        texts_to_check = scan_texts(path, lines)
        return local.present(texts_to_check) or any(
            slug_of(match)[1].lower() in private for text in texts_to_check for match in REFERENCE.finditer(text))

    for sha, message, added, _ in history:
        targets = [path for path, lines in added.items() if carries(path, lines)]
        targets += ["its message"] if carries("", [message]) else []
        if targets:
            report["warnings"].append(f"commit {sha[:7]} adds a private reference or local path to "
                                      f"{', '.join(targets)}; pushing publishes that commit unchanged")
    report["warnings"] += [f"{path} was not checked: it is not UTF-8 or BOM-marked Unicode text" for path in unchecked]
    if not any(carries(path, lines) for path, lines in files.items()) and not carries("", list(texts.values())):
        return report
    keep = load_keep(root, report["warnings"])
    placeholders = set()
    with Mapper(state_path) as mapper:
        for path in files:
            sanitiser = Sanitiser(private, public, mapper, keep, local)
            if rewrite_index(root, path, sanitiser, changed[path][1]):
                report["rewritten"].append({"target": path, "staged": True, **sanitiser.counts})
                placeholders |= sanitiser.placeholders
        labels = {"message_file": "commit message", "title_file": "PR title", "body_file": "PR body"}
        for key, text in texts.items():
            sanitiser = Sanitiser(private, public, mapper, keep, local)
            rewritten = sanitiser.metadata(text)
            if rewritten != text:
                Path(getattr(args, key)).write_bytes(rewritten.encode("utf-8"))
                report["rewritten"].append({"target": labels[key], **sanitiser.counts})
    report["placeholders"] = sorted(placeholders)
    return report


def render(report):
    if not report["public"]:
        return f"publication guard: {report['destination']} is not public; nothing checked"
    lines = [f"publication guard: {report['destination']} is public"]
    if not report["rewritten"]:
        lines.append("nothing to rewrite in the files it could read" if report["unchecked"] else "nothing to rewrite")
    for row in report["rewritten"]:
        details = ", ".join(f"{label} {row[k]}" for k, label in (
            ("references", "references"), ("paths", "local paths"), ("numbers", "PR numbers"), ("shas", "SHAs"),
            ("stubbedLines", "stubbed lines"))
            if row.get(k))
        lines.append(f"rewrote {row['target']}{' (staged)' if row.get('staged') else ''}: {details}")
    for name in report.get("placeholders", []):
        kind = "gist" if "example-gist-" in name else "repository"
        lines.append(f"placeholder {name} stands for a private {kind}")
    lines += [f"warning: {warning}" for warning in report["warnings"]]
    return "\n".join(lines)


def main(argv=None):
    if sys.version_info < (3, 11):
        raise GuardError("Python 3.11 or newer is required")
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="operation", required=True)
    staged = commands.add_parser("staged", help="rewrite the staged diff and, optionally, the commit message file")
    staged.add_argument("--message-file")
    outgoing = commands.add_parser("outgoing", help="rewrite and stage files the branch changes since BASE")
    outgoing.add_argument("--base", required=True)
    for command in (staged, outgoing):
        command.add_argument("--remote", help="the remote being pushed to; defaults to the branch's push remote")
    pr = commands.add_parser("pr", help="rewrite PR title and body files in place")
    pr.add_argument("--repo", required=True, help="the pull request's base repository, [HOST/]OWNER/REPO")
    pr.add_argument("--title-file")
    pr.add_argument("--body-file")
    for command in (staged, outgoing, pr):
        command.add_argument("--json", action="store_true")
    args = parser.parse_args(argv)
    report = guard(args)
    print(json.dumps(report) if args.json else render(report))
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except GuardError as error:
        print("Error: publication guard: " + str(error), file=sys.stderr)
        sys.exit(2)
