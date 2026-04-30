"use client";

import { useSearchParams } from "next/navigation";
import { useCallback, useEffect, useId, useRef, useState } from "react";
import { AnimatedOutput } from "@/components/animated-output";
import { useRunShortcut } from "@/components/command-tabs";
import { ConsolePanel } from "@/components/console-panel";
import { HighlightedCode } from "@/components/highlighted-code";
import {
  CopyIcon,
  FilePlusIcon,
  FileTestIcon,
  GithubIcon,
  RunIcon,
  SidebarIcon,
  SparkleIcon,
  TerminalIcon,
} from "@/components/icons";
import {
  buildAutocompleteTokens,
  EXAMPLES,
  type OutputLine,
} from "@/lib/examples";
import { formatError } from "@/lib/format-error";
import { formatMemorySegments, type MemoryJson } from "@/lib/format-memory";
import { GITHUB_REPO_URL } from "@/lib/github";
import { validateGocciaToolInput } from "@/lib/goccia-tool-schema";
import { loadCode, saveCode } from "@/lib/playground-storage";
import { decodeShare, encodeShare } from "@/lib/share";

type Backend = "interpreted" | "bytecode";
type Runner = "execute" | "test";

const MIN_PLAYGROUND_PANE_SIZE = 0.65;
const PLAYGROUND_RESIZE_STEP = 0.12;

function pickExampleId(requested: string | null): string {
  if (requested && EXAMPLES.some((e) => e.id === requested)) return requested;
  return EXAMPLES[0].id;
}

/** Normalize a release tag (`0.6.1` or `v0.6.1`) to the displayed form `v0.6.1`.
 *  `nightly` and any non-semver tag pass through unchanged. */
function displayVersion(tag: string): string {
  if (tag === "nightly") return tag;
  return tag.startsWith("v") ? tag : `v${tag}`;
}

/** Strip the leading `v` so semver tags compare equal regardless of whether
 *  the source used the prefix. `"nightly"` and other non-semver tags pass
 *  through unchanged. */
function canonicalVersion(tag: string): string {
  return tag.startsWith("v") ? tag.slice(1) : tag;
}

/** Find the manifest tag that matches `wanted` ignoring `v`-prefix differences.
 *  Used when restoring a share-link selection: old links encoded `v0.7.0`,
 *  but the manifest now stores the raw form. */
function matchVendoredVersion(
  versions: string[],
  wanted: string,
): string | null {
  const target = canonicalVersion(wanted);
  for (const v of versions) {
    if (canonicalVersion(v) === target) return v;
  }
  return null;
}

type SourceTestCase = {
  name: string;
  suite: string | null;
  status: "only" | "skip" | "todo" | null;
  assertions: number;
};
type SourceTestStatus = SourceTestCase["status"];

function blankOutRange(source: string, start: number, end: number) {
  let result = "";
  for (let i = start; i < end; i++) {
    result += source[i] === "\n" ? "\n" : " ";
  }
  return result;
}

function stripCommentsAndStringLiterals(source: string): string {
  let stripped = "";
  let i = 0;

  while (i < source.length) {
    const char = source[i];
    const next = source[i + 1];

    if (char === "/" && next === "/") {
      const start = i;
      i += 2;
      while (i < source.length && source[i] !== "\n") i++;
      stripped += blankOutRange(source, start, i);
      continue;
    }

    if (char === "/" && next === "*") {
      const start = i;
      i += 2;
      while (
        i < source.length &&
        !(source[i] === "*" && source[i + 1] === "/")
      ) {
        i++;
      }
      i = Math.min(i + 2, source.length);
      stripped += blankOutRange(source, start, i);
      continue;
    }

    if (char === '"' || char === "'" || char === "`") {
      const quote = char;
      const start = i;
      i++;
      while (i < source.length) {
        if (source[i] === "\\") {
          i += 2;
          continue;
        }
        if (source[i] === quote) {
          i++;
          break;
        }
        i++;
      }
      stripped += blankOutRange(source, start, i);
      continue;
    }

    stripped += char;
    i++;
  }

  return stripped;
}

function readStringLiteral(
  source: string,
  cursor: number,
): { value: string; end: number } | null {
  while (/\s/.test(source[cursor] ?? "")) cursor++;
  const quote = source[cursor];
  if (quote !== '"' && quote !== "'" && quote !== "`") return null;

  let value = "";
  cursor++;
  while (cursor < source.length) {
    const char = source[cursor];
    if (char === "\\") {
      value += source.slice(cursor, cursor + 2);
      cursor += 2;
      continue;
    }
    if (char === quote) return { value, end: cursor + 1 };
    value += char;
    cursor++;
  }

  return null;
}

function findCallEnd(source: string, openParen: number): number {
  let depth = 0;
  for (let i = openParen; i < source.length; i++) {
    const char = source[i];
    if (char === "(") depth++;
    if (char === ")") {
      depth--;
      if (depth === 0) return i;
    }
  }
  return source.length - 1;
}

function skipPredicateChains(source: string, cursor: number): number {
  while (true) {
    while (/\s/.test(source[cursor] ?? "")) cursor++;
    const match = source.slice(cursor).match(/^\.(?:runIf|skipIf|each)\s*\(/);
    if (!match) return cursor;
    cursor += match[0].length - 1;
    cursor = findCallEnd(source, cursor) + 1;
    while (/\s/.test(source[cursor] ?? "")) cursor++;
    if (source[cursor] === "(") cursor++;
  }
}

function countExpectCalls(source: string): number {
  return (
    stripCommentsAndStringLiterals(source).match(/\bexpect\s*\(/g)?.length ?? 0
  );
}

function findBlockEnd(source: string, bodyStart: number): number {
  let depth = 0;
  for (let i = bodyStart; i < source.length; i++) {
    const char = source[i];
    if (char === "{") depth++;
    if (char === "}") {
      depth--;
      if (depth === 0) return i;
    }
  }
  return source.length;
}

function findExpressionEnd(source: string, bodyStart: number): number {
  let parenDepth = 0;
  let bracketDepth = 0;
  let braceDepth = 0;
  for (let i = bodyStart; i < source.length; i++) {
    const char = source[i];
    if (char === "(") parenDepth++;
    if (char === ")") {
      if (parenDepth === 0 && bracketDepth === 0 && braceDepth === 0) {
        return Math.max(bodyStart, i - 1);
      }
      parenDepth--;
    }
    if (char === "[") bracketDepth++;
    if (char === "]") bracketDepth--;
    if (char === "{") braceDepth++;
    if (char === "}") {
      if (braceDepth === 0 && parenDepth === 0 && bracketDepth === 0) {
        return Math.max(bodyStart, i - 1);
      }
      braceDepth--;
    }
    if (
      (char === "," || char === ";") &&
      parenDepth === 0 &&
      bracketDepth === 0 &&
      braceDepth === 0
    ) {
      return Math.max(bodyStart, i - 1);
    }
  }
  return source.length - 1;
}

function findCallbackBody(
  source: string,
  cursor: number,
): { start: number; end: number } | null {
  let parenDepth = 1;
  while (cursor < source.length) {
    const char = source[cursor];
    const next = source[cursor + 1];
    if (char === "{") {
      return { start: cursor, end: findBlockEnd(source, cursor) };
    }
    if (char === "=" && next === ">") {
      cursor += 2;
      while (/\s/.test(source[cursor] ?? "")) cursor++;
      if (source[cursor] === "{") {
        return { start: cursor, end: findBlockEnd(source, cursor) };
      }
      return { start: cursor, end: findExpressionEnd(source, cursor) };
    }
    if (char === "(") parenDepth++;
    if (char === ")") {
      parenDepth--;
      if (parenDepth === 0) return null;
    }
    if (char === ";") return null;
    cursor++;
  }
  return null;
}

function extractSourceTests(source: string): SourceTestCase[] {
  const structureSource = stripCommentsAndStringLiterals(source);
  const tests: SourceTestCase[] = [];
  const suites: {
    name: string;
    bodyStart: number;
    bodyEnd: number;
    status: SourceTestStatus;
  }[] = [];
  const suiteStart =
    /\bdescribe(?:\.(?:runIf|skipIf|each)\s*\([^)]*\)|\.(?:only|skip|todo))*\s*\(/g;
  let suiteMatch = suiteStart.exec(structureSource);

  while (suiteMatch) {
    const titleStart = skipPredicateChains(
      structureSource,
      suiteStart.lastIndex,
    );
    const suiteName = readStringLiteral(source, titleStart);
    const status =
      (suiteMatch[0].match(/\.(only|skip|todo)\s*\(/)?.[1] as
        | "only"
        | "skip"
        | "todo"
        | undefined) ?? null;
    const body = findCallbackBody(structureSource, titleStart);
    if (suiteName && body) {
      suites.push({
        name: suiteName.value,
        bodyStart: body.start,
        bodyEnd: body.end,
        status,
      });
    }
    suiteMatch = suiteStart.exec(structureSource);
  }

  const testStart =
    /\b(?:test|it)(?:\.(?:runIf|skipIf|each)\s*\([^)]*\)|\.(?:only|skip|todo))*\s*\(/g;
  let match = testStart.exec(structureSource);
  const findSuite = (start: number, end: number) =>
    suites
      .filter(
        (candidate) => start > candidate.bodyStart && end < candidate.bodyEnd,
      )
      .sort((a, b) => b.bodyStart - a.bodyStart || a.bodyEnd - b.bodyEnd)[0] ??
    null;

  while (match) {
    const titleStart = skipPredicateChains(
      structureSource,
      testStart.lastIndex,
    );
    const testName = readStringLiteral(source, titleStart);
    const status =
      (match[0].match(/\.(only|skip|todo)\s*\(/)?.[1] as
        | "only"
        | "skip"
        | "todo"
        | undefined) ?? null;
    const body = findCallbackBody(structureSource, titleStart);
    if (!testName) {
      match = testStart.exec(structureSource);
      continue;
    }

    if (!body) {
      if (status === "skip" || status === "todo") {
        const suite = findSuite(match.index, match.index);
        tests.push({
          name: testName.value,
          suite: suite?.name ?? null,
          status: status ?? suite?.status ?? null,
          assertions: 0,
        });
      }
      match = testStart.exec(structureSource);
      continue;
    }

    const suite = findSuite(body.start, body.end);
    tests.push({
      name: testName.value,
      suite: suite?.name ?? null,
      status: status ?? suite?.status ?? null,
      assertions: countExpectCalls(source.slice(body.start, body.end + 1)),
    });
    testStart.lastIndex = body.end + 1;
    match = testStart.exec(structureSource);
  }

  return tests;
}

function testStatusKey(test: Pick<SourceTestCase, "name" | "suite">) {
  return `${test.suite ?? ""}\u0000${test.name}`;
}

function failedTestNames(data: {
  files?: unknown[];
  results?: unknown[];
  failedTests?: unknown[];
}) {
  const names = new Set<string>();
  const rows = Array.isArray(data.files)
    ? data.files
    : Array.isArray(data.results)
      ? data.results
      : [];
  const addFailedTest = (failed: unknown) => {
    if (typeof failed !== "string") return;
    const quoted = failed.match(/Test "([^"]+)"(?: in suite "([^"]+)")?/);
    names.add(
      quoted
        ? testStatusKey({ name: quoted[1], suite: quoted[2] ?? null })
        : failed,
    );
  };
  if (Array.isArray(data.failedTests)) {
    for (const failed of data.failedTests) addFailedTest(failed);
  }
  for (const row of rows) {
    if (!row || typeof row !== "object") continue;
    const failedTests = (row as { failedTests?: unknown }).failedTests;
    if (!Array.isArray(failedTests)) continue;
    for (const failed of failedTests) addFailedTest(failed);
  }
  return names;
}

type PlaygroundProps = {
  /** Engine release tags vendored at build time, sourced from
   *  `vendor/manifest.json`. Order matches what the dropdown should
   *  display: newest stable picks first, `nightly` last. */
  versions?: string[];
  /** Tag the API picks when the request omits `version` — used to
   *  initialize the dropdown when no share-link or saved selection
   *  is present. */
  defaultVersion?: string;
};

export function Playground({
  versions: vendoredVersions = [],
  defaultVersion,
}: PlaygroundProps) {
  const params = useSearchParams();

  // Stable per-render IDs so `<label htmlFor>` / `aria-labelledby` reliably
  // associate the visible label with each control for screen readers.
  const backendId = useId();
  const asiId = useId();
  const compatVarId = useId();
  const compatFunctionId = useId();
  const versionId = useId();
  const editorId = useId();

  // The dropdown shows whatever `vendor/manifest.json` advertised, in the
  // server's order. Fall back to a single `nightly` entry when the manifest
  // is empty (local dev with no `prebuild`); the API's dev fallback chain
  // resolves it to `../build/<binary>`.
  const versions: string[] =
    vendoredVersions.length > 0 ? vendoredVersions : ["nightly"];

  const [exId, setExId] = useState(() =>
    pickExampleId(params?.get("example") ?? null),
  );
  const example = EXAMPLES.find((e) => e.id === exId) ?? EXAMPLES[0];

  const [code, setCode] = useState(example.code);
  const [output, setOutput] = useState<OutputLine[]>([]);
  const [running, setRunning] = useState(false);
  const [backend, setBackend] = useState<Backend>("interpreted");
  const [version, setVersion] = useState<string>(
    () => defaultVersion ?? versions[0],
  );
  const [asi, setAsi] = useState(true);
  const [compatVar, setCompatVar] = useState(false);
  const [compatFunction, setCompatFunction] = useState(false);
  const [runner, setRunner] = useState<Runner>(example.runner ?? "execute");
  const [examplesOpen, setExamplesOpen] = useState(true);
  const [hoveredExampleId, setHoveredExampleId] = useState<string | null>(null);
  const [paneCols, setPaneCols] = useState<[number, number]>([1, 1]);
  const [shareTick, setShareTick] = useState(0);

  // Ghost-text autocomplete: track caret + derive a single suggested
  // completion that's painted faded into the highlight overlay at the
  // caret position. Tab accepts.
  const [caret, setCaret] = useState(0);
  const [ghostDismissed, setGhostDismissed] = useState(false);
  const taRef = useRef<HTMLTextAreaElement>(null);
  const hlRef = useRef<HTMLPreElement>(null);
  const gutterRef = useRef<HTMLPreElement>(null);
  const runningRef = useRef(false);
  const hydratedRef = useRef(false);
  const runShortcut = useRunShortcut();
  // Skip the example-restore effect's first fire so it doesn't clobber the
  // share/saved code that the mount effect just set.
  const initialMountRef = useRef(true);

  /** Suggest a single completion for the word the caret is currently inside. */
  function ghostFor(value: string, pos: number): string {
    const before = value.slice(0, pos);
    const after = value.slice(pos);
    // Only show ghost when the next character isn't part of an identifier —
    // otherwise we'd be inserting in the middle of a word.
    if (after && /^[A-Za-z0-9_$.]/.test(after)) return "";
    const match = before.match(/([A-Za-z_$][\w$.]*)$/);
    if (!match) return "";
    const prefix = match[1];
    if (prefix.length < 2) return "";
    const token = buildAutocompleteTokens({
      runner,
      compatVar,
      compatFunction,
    }).find(
      (t) => t.toLowerCase().startsWith(prefix.toLowerCase()) && t !== prefix,
    );
    if (!token) return "";
    return token.slice(prefix.length);
  }
  const ghost = ghostFor(code, caret);
  const visibleGhost = ghostDismissed ? "" : ghost;

  // On mount: prefer ?share=, then localStorage, then the example's default.
  // biome-ignore lint/correctness/useExhaustiveDependencies: mount-only sync
  useEffect(() => {
    const shareParam = params?.get("share");
    if (shareParam) {
      const payload = decodeShare(shareParam);
      if (payload?.code) {
        setCode(payload.code);
        if (payload.mode === "interpreted" || payload.mode === "bytecode") {
          setBackend(payload.mode);
        }
        if (typeof payload.asi === "boolean") setAsi(payload.asi);
        if (payload.runner === "execute" || payload.runner === "test") {
          setRunner(payload.runner);
        }
        if (typeof payload.compatVar === "boolean") {
          setCompatVar(payload.compatVar);
        }
        if (typeof payload.compatFunction === "boolean") {
          setCompatFunction(payload.compatFunction);
        }
        if (payload.version) {
          const matched = matchVendoredVersion(versions, payload.version);
          if (matched) setVersion(matched);
        }
        hydratedRef.current = true;
        return;
      }
    }
    const saved = loadCode(exId);
    setCode(saved ?? example.code);
    hydratedRef.current = true;
  }, []);

  // Switch examples: clear output, no auto-run, restore saved code if present.
  // Skip the first run on mount — the share/saved load above has already
  // populated `code`, and re-running this would overwrite it with the
  // current example's default.
  useEffect(() => {
    if (initialMountRef.current) {
      initialMountRef.current = false;
      return;
    }
    const saved = loadCode(exId);
    setCode(saved ?? example.code);
    setGhostDismissed(false);
    if (exId !== "blank") {
      setRunner(example.runner ?? "execute");
    }
    setOutput([]);
  }, [exId, example.code, example.runner]);

  // Persist edits per example.
  useEffect(() => {
    if (!hydratedRef.current) return;
    saveCode(exId, code);
  }, [exId, code]);

  const run = useCallback(async () => {
    if (runningRef.current) return;
    runningRef.current = true;
    setRunning(true);
    const flagText = [
      asi ? "--asi" : "",
      compatVar ? "--compat-var" : "",
      compatFunction ? "--compat-function" : "",
    ]
      .filter(Boolean)
      .join(" ");
    const banner = `GocciaScriptLoader --mode=${
      backend === "bytecode" ? "bytecode" : "interpreted"
    }${flagText ? ` ${flagText}` : ""} ${version}`;
    const runnerBanner =
      runner === "test"
        ? `GocciaTestRunner --mode=${
            backend === "bytecode" ? "bytecode" : "interpreted"
          }${flagText ? ` ${flagText}` : ""} ${version}`
        : banner;
    setOutput([{ kind: "meta", text: runnerBanner }]);

    try {
      const payload = validateGocciaToolInput({
        code,
        mode: backend,
        asi,
        compatVar,
        compatFunction,
        version,
      });
      if (!payload.ok) {
        setOutput([
          { kind: "meta", text: runnerBanner },
          { kind: "err", text: payload.error.message },
        ]);
        return;
      }

      const res = await fetch(
        runner === "test" ? "/api/test" : "/api/execute",
        {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify(payload.value),
        },
      );
      // Server tags repeat-input responses with `X-Cache: HIT` so we can
      // surface "(cached)" in the exit line. Anything else (MISS, missing,
      // unexpected value) is treated as a fresh run.
      const cached = res.headers.get("X-Cache") === "HIT";
      if (res.status === 429) {
        const retry = res.headers.get("Retry-After") ?? "60";
        setOutput([
          { kind: "meta", text: runnerBanner },
          {
            kind: "err",
            text: `Rate limit exceeded — try again in ${retry}s.`,
          },
        ]);
        return;
      }
      // Two error shapes are possible:
      //   • Transport errors (rate limit, bad body, oversize, spawn failed)
      //     short-circuit with `error: { message, code }` and no other fields.
      //   • Successful runner invocations attach the runner's own structured
      //     error (different schema) inside the same `error` slot.
      // Disambiguate by checking for runner-only fields.
      const data = (await res.json()) as {
        ok?: boolean;
        output?: string;
        value?: unknown;
        error?: {
          type?: string;
          name?: string;
          message: string;
          line?: number | null;
          column?: number | null;
          fileName?: string | null;
          code?: string;
        } | null;
        timing?: { total_ms: number };
        memory?: MemoryJson | null;
        exitCode?: number | null;
        truncated?: boolean;
        stderr?: string;
        rawStdout?: string;
        totalTests?: number;
        passed?: number;
        failed?: number;
        skipped?: number;
        assertions?: number;
        stdout?: string;
        files?: unknown[];
        results?: unknown[];
      };

      const lines: OutputLine[] = [{ kind: "meta", text: runnerBanner }];
      if (data.output) {
        for (const line of data.output.replace(/\n$/, "").split("\n")) {
          lines.push({ kind: "log", text: line });
        }
      }
      if (runner === "test" && data.stdout) {
        for (const line of data.stdout.replace(/\n$/, "").split("\n")) {
          lines.push({ kind: line.includes("❌") ? "err" : "log", text: line });
        }
      }
      if (data.error) {
        for (const line of formatError(data.error, code)) {
          lines.push(line);
        }
      }
      if (data.value !== null && data.value !== undefined) {
        lines.push({
          kind: "result",
          text: JSON.stringify(data.value),
        });
      }
      if (runner === "test" && typeof data.totalTests === "number") {
        const failures = failedTestNames(data);
        const sourceTests = extractSourceTests(code);
        const statusKeyCounts = new Map<string, number>();
        const hasOnly = sourceTests.some(
          (sourceTest) => sourceTest.status === "only",
        );
        for (const sourceTest of sourceTests) {
          const key = testStatusKey(sourceTest);
          statusKeyCounts.set(key, (statusKeyCounts.get(key) ?? 0) + 1);
        }
        for (const sourceTest of sourceTests) {
          const key = testStatusKey(sourceTest);
          if ((statusKeyCounts.get(key) ?? 0) > 1) continue;
          const failed = failures.has(key);
          const focusedOut = hasOnly && sourceTest.status !== "only";
          const statusText =
            sourceTest.status === "skip"
              ? "SKIP"
              : sourceTest.status === "todo"
                ? "TODO"
                : focusedOut
                  ? "SKIP"
                  : failed
                    ? "FAIL"
                    : "PASS";
          lines.push({
            kind: failed && !focusedOut ? "err" : "log",
            text: `${statusText} ${
              sourceTest.suite ? `${sourceTest.suite} > ` : ""
            }${sourceTest.name}${
              sourceTest.status === "only" ? " [only]" : ""
            } (${sourceTest.assertions} assertions)`,
          });
        }
        lines.push({
          kind: data.ok === false || data.failed ? "err" : "log",
          text: `${data.passed ?? 0}/${data.totalTests} tests passed${
            data.skipped ? ` · ${data.skipped} skipped` : ""
          } · ${data.assertions ?? 0} assertions`,
        });
      }
      if (data.truncated) {
        lines.push({
          kind: "meta",
          text: "⚠ output truncated (limit reached)",
        });
      }
      if (data.stderr) {
        lines.push({ kind: "err", text: data.stderr.trimEnd() });
      }
      if (data.rawStdout) {
        lines.push({ kind: "err", text: data.rawStdout.trimEnd() });
      }
      const totalMs = data.timing?.total_ms;
      const tail = `— exit ${data.exitCode ?? "?"}${
        totalMs !== undefined ? ` · ${totalMs.toFixed(2)}ms` : ""
      }${formatMemorySegments(data.memory)}${cached ? " · (cached)" : ""}`;
      lines.push({ kind: "meta", text: tail });
      setOutput(lines);
    } catch (err) {
      setOutput([
        { kind: "meta", text: runnerBanner },
        { kind: "err", text: `network error: ${(err as Error).message}` },
      ]);
    } finally {
      setRunning(false);
      runningRef.current = false;
    }
  }, [code, backend, version, runner, asi, compatVar, compatFunction]);

  const buildShareLink = useCallback(() => {
    const url = new URL(window.location.href);
    url.search = "";
    url.searchParams.set(
      "share",
      encodeShare({
        code,
        mode: backend,
        runner,
        asi,
        compatVar,
        compatFunction,
        version,
      }),
    );
    return url.toString();
  }, [code, backend, runner, asi, compatVar, compatFunction, version]);

  const share = useCallback(async () => {
    const link = buildShareLink();
    let ok = false;
    try {
      if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(link);
        ok = true;
      }
    } catch {}
    if (ok) setShareTick((t) => t + 1);
  }, [buildShareLink]);

  const reportIssue = useCallback(() => {
    const link = buildShareLink();
    const onOff = (b: boolean) => (b ? "on" : "off");
    const body = [
      `[Open in playground](${link})`,
      "",
      "| Setting | Value |",
      "|---|---|",
      `| Backend | \`${backend}\` |`,
      `| Runner | \`${runner}\` |`,
      `| Version | \`${version}\` |`,
      `| ASI | ${onOff(asi)} |`,
      `| Compat \`var\` | ${onOff(compatVar)} |`,
      `| Compat \`function\` | ${onOff(compatFunction)} |`,
      "",
      "---",
      "",
      "### What did you expect to happen?",
      "",
      "<!-- Describe the expected behavior -->",
      "",
      "### What actually happened?",
      "",
      "<!-- Describe the actual behavior, paste any error output -->",
      "",
    ].join("\n");
    const issueUrl = new URL(`${GITHUB_REPO_URL}/issues/new`);
    issueUrl.searchParams.set("title", "Playground report: ");
    issueUrl.searchParams.set("body", body);
    window.open(issueUrl.toString(), "_blank", "noopener,noreferrer");
  }, [
    buildShareLink,
    backend,
    runner,
    version,
    asi,
    compatVar,
    compatFunction,
  ]);

  useEffect(() => {
    if (shareTick === 0) return;
    const id = setTimeout(() => setShareTick(0), 1500);
    return () => clearTimeout(id);
  }, [shareTick]);

  // Mirror the textarea's scroll onto the highlight overlay AND the line-
  // number gutter. Without the gutter sync, the line numbers stay frozen
  // at line 1 while the user scrolls the source, so the visible gutter
  // labels no longer match the code beside them and clicks "look like"
  // they hit the wrong line. The gutter needs `overflow: hidden` for
  // `scrollTop` to take effect — see `.pg-gutter` in `globals.css`.
  const syncScroll = () => {
    if (taRef.current && hlRef.current) {
      hlRef.current.scrollTop = taRef.current.scrollTop;
      hlRef.current.scrollLeft = taRef.current.scrollLeft;
    }
    if (taRef.current && gutterRef.current) {
      gutterRef.current.scrollTop = taRef.current.scrollTop;
    }
  };

  const lineCount = code.split("\n").length;
  const editorFilename =
    exId === "blank" && runner === "test"
      ? "blank-test.js"
      : `${example.id}.${example.ext ?? "js"}`;
  const gutter = Array.from({ length: lineCount }, (_, i) =>
    String(i + 1),
  ).join("\n");
  const paneTotal = paneCols[0] + paneCols[1];
  const minPanePercent = Math.round(
    (MIN_PLAYGROUND_PANE_SIZE / paneTotal) * 100,
  );

  const acceptGhost = () => {
    if (!visibleGhost) return false;
    const next = code.slice(0, caret) + visibleGhost + code.slice(caret);
    setCode(next);
    setGhostDismissed(false);
    const newCaret = caret + visibleGhost.length;
    setCaret(newCaret);
    requestAnimationFrame(() => {
      if (taRef.current) {
        taRef.current.selectionStart = taRef.current.selectionEnd = newCaret;
        taRef.current.focus();
      }
    });
    return true;
  };

  const selectExample = (id: string) => {
    setExId(id);
    setHoveredExampleId(null);
  };

  const createBlank = () => {
    const blankExample = EXAMPLES.find((e) => e.id === "blank");
    const blankCode = blankExample?.code ?? "";
    saveCode("blank", blankCode);
    setExId("blank");
    setCode(blankCode);
    setGhostDismissed(false);
    setRunner("execute");
    setOutput([]);
    setExamplesOpen(false);
    requestAnimationFrame(() => taRef.current?.focus());
  };

  const createBlankTest = () => {
    const testCode = `describe("new test", () => {
  test("works", () => {
    expect(1 + 1).toBe(2);
  });
});
`;
    saveCode("blank", testCode);
    setExId("blank");
    setCode(testCode);
    setGhostDismissed(false);
    setRunner("test");
    setOutput([]);
    setExamplesOpen(false);
    requestAnimationFrame(() => taRef.current?.focus());
  };

  const resizePanePair = useCallback(
    (
      mode:
        | { kind: "set"; value: number }
        | { kind: "step"; direction: -1 | 1 }
        | { kind: "edge"; side: "start" | "end" },
    ) => {
      setPaneCols((current) => {
        const combined = current[0] + current[1];
        let nextLeft = current[0];

        if (mode.kind === "set") {
          nextLeft = mode.value;
        } else if (mode.kind === "step") {
          nextLeft += mode.direction * PLAYGROUND_RESIZE_STEP;
        } else {
          nextLeft =
            mode.side === "start"
              ? MIN_PLAYGROUND_PANE_SIZE
              : combined - MIN_PLAYGROUND_PANE_SIZE;
        }

        nextLeft = Math.min(
          Math.max(nextLeft, MIN_PLAYGROUND_PANE_SIZE),
          combined - MIN_PLAYGROUND_PANE_SIZE,
        );
        return [nextLeft, combined - nextLeft];
      });
    },
    [],
  );

  const startPaneResize = useCallback(
    (event: React.PointerEvent<HTMLElement>) => {
      event.preventDefault();
      const el = event.currentTarget;
      const pointerId = event.pointerId;
      const container = el.closest(".pg-panes") as HTMLElement | null;
      if (!container) return;
      const { left, width } = container.getBoundingClientRect();
      const startColumns = paneCols;
      const total = startColumns[0] + startColumns[1];

      el.setPointerCapture(pointerId);
      const move = (moveEvent: PointerEvent) => {
        const x = Math.min(Math.max(moveEvent.clientX - left, 0), width);
        resizePanePair({
          kind: "set",
          value: Math.min(
            Math.max((x / width) * total, MIN_PLAYGROUND_PANE_SIZE),
            total - MIN_PLAYGROUND_PANE_SIZE,
          ),
        });
      };
      const cleanup = () => {
        window.removeEventListener("pointermove", move);
        window.removeEventListener("pointerup", cleanup);
        window.removeEventListener("pointercancel", cleanup);
        el.removeEventListener("lostpointercapture", cleanup);
        if (el.hasPointerCapture(pointerId)) {
          el.releasePointerCapture(pointerId);
        }
      };
      window.addEventListener("pointermove", move);
      window.addEventListener("pointerup", cleanup, { once: true });
      window.addEventListener("pointercancel", cleanup, { once: true });
      el.addEventListener("lostpointercapture", cleanup, { once: true });
    },
    [paneCols, resizePanePair],
  );

  const handlePaneResizeKeyDown = useCallback(
    (event: React.KeyboardEvent<HTMLElement>) => {
      if (
        event.key !== "ArrowLeft" &&
        event.key !== "ArrowRight" &&
        event.key !== "ArrowUp" &&
        event.key !== "ArrowDown" &&
        event.key !== "Home" &&
        event.key !== "End"
      ) {
        return;
      }

      event.preventDefault();
      if (event.key === "Home") {
        resizePanePair({ kind: "edge", side: "start" });
        return;
      }
      if (event.key === "End") {
        resizePanePair({ kind: "edge", side: "end" });
        return;
      }

      resizePanePair({
        kind: "step",
        direction:
          event.key === "ArrowLeft" || event.key === "ArrowUp" ? -1 : 1,
      });
    },
    [resizePanePair],
  );

  return (
    <div className="pg-shell pg-full">
      <div className="pg-toolbar">
        <div className="pg-toolbar-icons">
          <button
            type="button"
            className="pg-icon-button"
            data-active={examplesOpen}
            aria-label={examplesOpen ? "Hide examples" : "Show examples"}
            aria-expanded={examplesOpen}
            onClick={() => setExamplesOpen((open) => !open)}
          >
            <SidebarIcon size={16} />
          </button>
          <button
            type="button"
            className="pg-icon-button"
            aria-label="Create blank code"
            title="Create blank code"
            onClick={createBlank}
          >
            <FilePlusIcon size={16} />
          </button>
          <button
            type="button"
            className="pg-icon-button"
            aria-label="Create blank test file"
            title="Create blank test file"
            onClick={createBlankTest}
          >
            <FileTestIcon size={16} />
          </button>
        </div>

        <fieldset
          className="pg-segmented"
          data-active={backend}
          id={backendId}
          aria-label="Execution mode"
        >
          <span className="pg-segmented-indicator" aria-hidden="true" />
          <button
            type="button"
            aria-pressed={backend === "interpreted"}
            onClick={() => setBackend("interpreted")}
          >
            Interpreter
          </button>
          <button
            type="button"
            aria-pressed={backend === "bytecode"}
            onClick={() => setBackend("bytecode")}
          >
            Bytecode
          </button>
        </fieldset>

        <fieldset className="pg-flags" aria-label="Compilation flags">
          <span className="pg-toolbar-label">Flags</span>
          <label
            className="pg-flag-toggle"
            htmlFor={asiId}
            title="Automatic semicolon insertion (--asi)"
          >
            <input
              id={asiId}
              type="checkbox"
              checked={asi}
              onChange={(e) => setAsi(e.target.checked)}
            />
            <span>ASI</span>
          </label>
          <label
            className="pg-flag-toggle"
            htmlFor={compatVarId}
            title="Enable var declarations (--compat-var)"
          >
            <input
              id={compatVarId}
              type="checkbox"
              checked={compatVar}
              onChange={(e) => setCompatVar(e.target.checked)}
            />
            <span>var</span>
          </label>
          <label
            className="pg-flag-toggle"
            htmlFor={compatFunctionId}
            title="Enable function declarations and expressions (--compat-function)"
          >
            <input
              id={compatFunctionId}
              type="checkbox"
              checked={compatFunction}
              onChange={(e) => setCompatFunction(e.target.checked)}
            />
            <span>function</span>
          </label>
        </fieldset>

        {/* The dropdown enumerates whatever `vendor/manifest.json` advertised
            at build time. The selection rides along to `/api/execute` and
            `/api/test` so the server dispatches to the matching binary; the
            display label adds a `v` prefix for semver tags only. */}
        <label className="pg-toolbar-label ml-3" htmlFor={versionId}>
          Version
        </label>
        <select
          id={versionId}
          className="pg-select min-w-[110px]"
          value={version}
          onChange={(e) => setVersion(e.target.value)}
        >
          {versions.map((v, i) => (
            <option key={v} value={v}>
              {v === "nightly"
                ? "nightly"
                : i === 0
                  ? `${displayVersion(v)} · latest`
                  : displayVersion(v)}
            </option>
          ))}
        </select>

        <div className="ml-auto flex items-center gap-2">
          <button
            type="button"
            className="pg-share"
            onClick={reportIssue}
            title="Open a GitHub issue with a link to this playground"
          >
            <GithubIcon size={14} />
            <span>Report issue</span>
          </button>
          <button
            type="button"
            className="pg-share"
            onClick={share}
            title="Copy a sharable URL of the current code"
          >
            <CopyIcon size={14} />
            {shareTick > 0 ? (
              <span key={shareTick} className="copied-flash">
                link copied
              </span>
            ) : (
              <span>Share</span>
            )}
          </button>
          <button
            type="button"
            className="pg-run"
            disabled={running}
            onClick={run}
            title={`Run · ${runShortcut.long}`}
          >
            <RunIcon size={14} />
            <span>{running ? "Running…" : "Run"}</span>
            <span className="pg-run-kbd" aria-hidden="true">
              {runShortcut.short}
            </span>
          </button>
        </div>
      </div>

      <div className="pg-workspace" data-examples-open={examplesOpen}>
        <aside
          className="pg-examples"
          aria-label="Playground examples"
          aria-hidden={!examplesOpen}
          inert={examplesOpen ? undefined : true}
        >
          <div className="pg-examples-head">
            <span>Examples</span>
          </div>
          <div className="pg-example-list">
            {EXAMPLES.filter((e) => e.id !== "blank").map((e) => (
              <button
                key={e.id}
                type="button"
                className="pg-example-button"
                data-active={e.id === exId}
                tabIndex={examplesOpen ? 0 : -1}
                onClick={() => selectExample(e.id)}
                onFocus={() => setHoveredExampleId(e.id)}
                onMouseEnter={() => setHoveredExampleId(e.id)}
                onBlur={() => setHoveredExampleId(null)}
                onMouseLeave={() => setHoveredExampleId(null)}
              >
                <span>{e.label}</span>
                <small>{e.desc}</small>
              </button>
            ))}
          </div>
          {(() => {
            const hoveredExample = EXAMPLES.find(
              (e) => e.id === hoveredExampleId && e.id !== "blank",
            );
            if (!hoveredExample) return null;
            return (
              <div className="pg-example-minimap" aria-hidden="true">
                <div className="pg-example-minimap-head">
                  {hoveredExample.id}.{hoveredExample.ext ?? "js"}
                </div>
                <pre>
                  <code>
                    <HighlightedCode
                      code={hoveredExample.code
                        .split("\n")
                        .slice(0, 26)
                        .join("\n")}
                    />
                  </code>
                </pre>
              </div>
            );
          })()}
        </aside>

        <div
          className="pg-panes pg-panes-full"
          style={
            {
              "--pg-col-1": `${paneCols[0]}fr`,
              "--pg-col-2": `${paneCols[1]}fr`,
            } as React.CSSProperties
          }
        >
          <div className="pg-pane">
            <div className="pg-pane-head">
              <TerminalIcon size={14} /> {editorFilename}
              <span className="ml-auto text-[0.7rem]">{lineCount} lines</span>
            </div>
            <p className="pg-example-desc">{example.desc}</p>
            <div className="pg-editor">
              <pre className="pg-gutter" ref={gutterRef} aria-hidden="true">
                {gutter}
              </pre>
              <div className="pg-editor-body">
                <pre className="pg-hl" ref={hlRef} aria-hidden="true">
                  <code>
                    <HighlightedCode code={code.slice(0, caret)} />
                    {visibleGhost && (
                      <span className="pg-ghost">{visibleGhost}</span>
                    )}
                    <HighlightedCode code={`${code.slice(caret)}\n`} />
                  </code>
                </pre>
                <textarea
                  ref={taRef}
                  id={editorId}
                  aria-label={`GocciaScript editor — ${example.label}`}
                  spellCheck={false}
                  value={code}
                  onChange={(e) => {
                    setCode(e.target.value);
                    setGhostDismissed(false);
                    setCaret(e.target.selectionStart);
                  }}
                  onSelect={(e) => {
                    setGhostDismissed(false);
                    setCaret(e.currentTarget.selectionStart);
                  }}
                  onScroll={syncScroll}
                  onKeyDown={(e) => {
                    const target = e.currentTarget;
                    const s = target.selectionStart;
                    const en = target.selectionEnd;
                    const value = target.value;

                    // Tab accepts the ghost when one is showing.
                    if (e.key === "Tab" && visibleGhost) {
                      e.preventDefault();
                      acceptGhost();
                      return;
                    }
                    // Tab without ghost = indent two spaces.
                    if (e.key === "Tab") {
                      e.preventDefault();
                      const next = `${value.slice(0, s)}  ${value.slice(en)}`;
                      setCode(next);
                      setGhostDismissed(false);
                      requestAnimationFrame(() => {
                        target.selectionStart = target.selectionEnd = s + 2;
                        setCaret(s + 2);
                      });
                      return;
                    }
                    if (e.key === "Escape" && visibleGhost) {
                      setGhostDismissed(true);
                    }
                    if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
                      e.preventDefault();
                      run();
                      return;
                    }

                    // ── Auto-indent on Enter ────────────────────────────
                    // Mirrors the leading whitespace of the current line, with
                    // an extra two spaces after an opener (`{`, `[`, `(`).
                    // When the caret sits between a matching opener/closer
                    // (`{|}`, `[|]`, `(|)`), splits into three lines and
                    // places the caret on the indented middle line.
                    if (
                      e.key === "Enter" &&
                      !e.shiftKey &&
                      !e.altKey &&
                      !e.metaKey &&
                      !e.ctrlKey
                    ) {
                      const lineStart = value.lastIndexOf("\n", s - 1) + 1;
                      const indentMatch = value
                        .slice(lineStart, s)
                        .match(/^[ \t]*/);
                      const indent = indentMatch ? indentMatch[0] : "";
                      const prevCh = s > 0 ? value[s - 1] : "";
                      const nextCh = value[s] ?? "";
                      const OPEN_TO_CLOSE: Record<string, string> = {
                        "{": "}",
                        "[": "]",
                        "(": ")",
                      };
                      const opensBlock = prevCh in OPEN_TO_CLOSE;
                      const splitsPair =
                        opensBlock && OPEN_TO_CLOSE[prevCh] === nextCh;
                      const extra = opensBlock ? "  " : "";
                      e.preventDefault();
                      if (splitsPair) {
                        const insert = `\n${indent}${extra}\n${indent}`;
                        const next =
                          value.slice(0, s) + insert + value.slice(en);
                        setCode(next);
                        setGhostDismissed(false);
                        const caretPos = s + 1 + indent.length + extra.length;
                        requestAnimationFrame(() => {
                          target.selectionStart = target.selectionEnd =
                            caretPos;
                          setCaret(caretPos);
                        });
                        return;
                      }
                      const insert = `\n${indent}${extra}`;
                      const next = value.slice(0, s) + insert + value.slice(en);
                      setCode(next);
                      setGhostDismissed(false);
                      const caretPos = s + insert.length;
                      requestAnimationFrame(() => {
                        target.selectionStart = target.selectionEnd = caretPos;
                        setCaret(caretPos);
                      });
                      return;
                    }

                    // ── Smart bracket / quote handling ──────────────────
                    // Skip when modifier keys are held — let normal browser
                    // shortcuts (Cmd+A, Ctrl+C, etc.) through untouched.
                    if (e.metaKey || e.ctrlKey || e.altKey) return;
                    const PAIRS: Record<string, string> = {
                      "(": ")",
                      "[": "]",
                      "{": "}",
                      '"': '"',
                      "'": "'",
                      "`": "`",
                    };
                    const CLOSERS = new Set([")", "]", "}", '"', "'", "`"]);
                    // 1. Auto-close: typing an opener inserts the matching
                    //    closer and leaves the caret between them. With a
                    //    selection, wrap it.
                    if (PAIRS[e.key]) {
                      e.preventDefault();
                      const open = e.key;
                      const close = PAIRS[open];
                      // For symmetric quotes, don't auto-close if we're
                      // already next to one (typing `"` after `"foo` should
                      // just close the string).
                      if (open === close) {
                        if (value[s] === open && s === en) {
                          target.selectionStart = target.selectionEnd = s + 1;
                          setGhostDismissed(false);
                          setCaret(s + 1);
                          return;
                        }
                      }
                      const selected = value.slice(s, en);
                      const next = `${value.slice(0, s)}${open}${selected}${close}${value.slice(en)}`;
                      setCode(next);
                      setGhostDismissed(false);
                      requestAnimationFrame(() => {
                        const newPos = s + 1 + selected.length;
                        target.selectionStart = s + 1;
                        target.selectionEnd = newPos;
                        setCaret(newPos);
                      });
                      return;
                    }
                    // 2. Close-over: typing a closer when the next character
                    //    is already that closer just steps over it.
                    if (CLOSERS.has(e.key) && s === en && value[s] === e.key) {
                      e.preventDefault();
                      setGhostDismissed(false);
                      target.selectionStart = target.selectionEnd = s + 1;
                      setCaret(s + 1);
                      return;
                    }
                    // 3. Backspace over an empty pair removes both halves:
                    //    `(|)` + Backspace → `|`.
                    if (e.key === "Backspace" && s === en && s > 0) {
                      const prev = value[s - 1];
                      const nextCh = value[s];
                      if (PAIRS[prev] && PAIRS[prev] === nextCh) {
                        e.preventDefault();
                        const next = value.slice(0, s - 1) + value.slice(s + 1);
                        setCode(next);
                        setGhostDismissed(false);
                        requestAnimationFrame(() => {
                          target.selectionStart = target.selectionEnd = s - 1;
                          setCaret(s - 1);
                        });
                        return;
                      }
                    }
                  }}
                />
              </div>
            </div>
          </div>

          {/* biome-ignore lint/a11y/useSemanticElements: this is an interactive splitter; button keeps pointer dragging reliable while ARIA exposes separator semantics. */}
          <button
            type="button"
            className="pg-resizer"
            role="separator"
            aria-label="Resize editor and output panes"
            aria-orientation="vertical"
            aria-valuemin={minPanePercent}
            aria-valuemax={100 - minPanePercent}
            aria-valuenow={Math.round((paneCols[0] / paneTotal) * 100)}
            onPointerDown={startPaneResize}
            onKeyDown={handlePaneResizeKeyDown}
          />

          <div className="pg-pane">
            <div className="pg-pane-head">
              <SparkleIcon size={14} /> output
              <span className="ml-auto text-[0.7rem]">
                {output.length} lines
              </span>
            </div>
            <ConsolePanel>
              <AnimatedOutput
                // Run id derived from the output identity — bumping the
                // banner / clearing for a new execution remounts the
                // component and replays the line-stagger reveal.
                runKey={
                  output.length === 0 ? "empty" : (output[0]?.text ?? "x")
                }
                lines={output}
                showCaret={!running ? output.length > 0 : true}
              />
            </ConsolePanel>
          </div>
        </div>
      </div>
    </div>
  );
}
