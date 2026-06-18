import { readDocSource } from "@/lib/doc-source";
import { DOC_PAGES, type DocPage } from "@/lib/docs-data";
import { EXAMPLES, type Example } from "@/lib/examples";
import {
  fetchLatestRelease,
  GITHUB_RELEASES_URL,
  GITHUB_REPO_URL,
  isPreStable,
  type ReleaseInfo,
} from "@/lib/github";
import { BUILTINS, EXCLUDED, FEATURES } from "@/lib/landing-data";
import { CANONICAL_SITE_URL, SITE_DESCRIPTION } from "@/lib/site-url";
import { TOOL_CALL_FLOWS, TOOL_CALL_TASK } from "@/lib/tool-call-comparison";

export type MarkdownRoute =
  | { kind: "home" }
  | { kind: "docs"; id: string }
  | { kind: "installation" }
  | { kind: "compatibility" }
  | { kind: "playground" }
  | { kind: "sandbox" };

const EMPTY_EXAMPLE: Example = {
  id: "",
  label: "",
  desc: "",
  code: "",
};

export function yamlScalar(value: string): string {
  return JSON.stringify(value);
}

function frontmatter(title: string, description: string): string {
  return [
    "---",
    `title: ${yamlScalar(title)}`,
    `description: ${yamlScalar(description)}`,
    "---",
  ].join("\n");
}

function fence(code: string, language = ""): string {
  return [`\`\`\`${language}`, code.trimEnd(), "```"].join("\n");
}

function list(items: string[]): string {
  return items.map((item) => `- ${item}`).join("\n");
}

function releaseSummary(release: ReleaseInfo | null): string {
  if (!release) {
    return `Latest release: see [GitHub Releases](${GITHUB_RELEASES_URL}).`;
  }

  const published = release.publishedAt
    ? `, published ${release.publishedAt.slice(0, 10)}`
    : "";
  const prestable = isPreStable(release)
    ? " Pre-1.0 releases may include breaking changes before 1.0.0."
    : "";
  return `Latest release: [${release.tagName}](${release.htmlUrl})${published}.${prestable}`;
}

function docPageForId(id: string): DocPage | undefined {
  return DOC_PAGES.find((page) => page.id === id);
}

function pickExample(
  preferredId: string | null | undefined,
  fallbackId?: string,
): Example {
  if (EXAMPLES.length === 0) return EMPTY_EXAMPLE;
  return (
    EXAMPLES.find((example) => example.id === preferredId) ??
    EXAMPLES.find((example) => example.id === fallbackId) ??
    EXAMPLES[0] ??
    EMPTY_EXAMPLE
  );
}

export function resolveMarkdownRoute(
  segments: readonly string[] | undefined,
): MarkdownRoute | null {
  const path = segments ?? [];
  if (path.length === 0) return { kind: "home" };

  const [section, id] = path;
  if (section === "docs") {
    if (path.length === 1) return { kind: "docs", id: "readme" };
    if (path.length === 2 && id && docPageForId(id)) {
      return { kind: "docs", id };
    }
    return null;
  }
  if (path.length !== 1) return null;
  if (section === "installation") return { kind: "installation" };
  if (section === "compatibility") return { kind: "compatibility" };
  if (section === "playground") return { kind: "playground" };
  if (section === "sandbox") return { kind: "sandbox" };
  return null;
}

async function docsMarkdown(id: string): Promise<string | null> {
  const page = docPageForId(id);
  if (!page) return null;

  const source = await readDocSource(id);
  if (source !== null) return source;

  return [
    frontmatter(
      `${page.title} - Docs - GocciaScript`,
      page.desc ?? "Reference documentation for GocciaScript.",
    ),
    "",
    `# ${page.title}`,
    "",
    page.desc ?? "Reference documentation for GocciaScript.",
    "",
    `The markdown source file for this page was not found at \`content/docs/${page.file}\`.`,
    "",
    "Run `bun run sync-docs` from the website directory to sync the docs source.",
  ].join("\n");
}

async function homeMarkdown(): Promise<string> {
  const release = await fetchLatestRelease();
  const heroExample = pickExample("coffee-typed");

  return [
    frontmatter("GocciaScript", SITE_DESCRIPTION),
    "",
    "# GocciaScript",
    "",
    SITE_DESCRIPTION,
    "",
    releaseSummary(release),
    "",
    "## Quick install",
    "",
    "macOS / Linux:",
    "",
    fence(`curl -fsSL ${CANONICAL_SITE_URL}/install | sh`, "sh"),
    "",
    "Windows PowerShell:",
    "",
    fence(`irm ${CANONICAL_SITE_URL}/install.ps1 | iex`, "powershell"),
    "",
    "## Start here",
    "",
    list([
      "[Open the Playground](/playground?example=coffee-typed)",
      "[Read the docs](/docs)",
      "[Check ECMAScript compatibility](/compatibility)",
      "[Installation options](/installation)",
      `[Source on GitHub](${GITHUB_REPO_URL})`,
    ]),
    "",
    "## Design principles",
    "",
    list(FEATURES.map((feature) => `**${feature.title}** - ${feature.body}`)),
    "",
    "## Intentionally excluded",
    "",
    "Dynamic code construction and scope-changing syntax are left out of the recommended defaults to keep execution predictable in embedded runtimes. Compatibility flags primarily exist for ECMAScript conformance and legacy code; they can opt back into selected syntax such as `var`, `function`, and ASI via CLI or config flags.",
    "",
    list(EXCLUDED.map((item) => `\`${item.name}\` - ${item.why}`)),
    "",
    "## Runtime surface",
    "",
    "GocciaScript includes structured-data imports and parsers, console output, SemVer helpers, import maps, and a built-in test runner with `test`, `describe`, and `expect`. GocciaSandboxRunner adds import-only `fs` and `goccia` modules backed by a seeded virtual filesystem, sandbox shell commands, nested execution, and explicit diffs.",
    "",
    list(BUILTINS.map((item) => `\`${item.name}\``)),
    "",
    "## Example",
    "",
    heroExample.desc,
    "",
    fence(heroExample.code, heroExample.ext ?? "js"),
  ].join("\n");
}

async function installationMarkdown(): Promise<string> {
  const release = await fetchLatestRelease();

  return [
    frontmatter(
      "Installation - GocciaScript",
      "Install GocciaScript with a one-line installer, prebuilt binaries, or a source build.",
    ),
    "",
    "# Installation",
    "",
    "Pick the install method that fits your platform. The runtime is a single self-contained binary with no Node.js requirement.",
    "",
    releaseSummary(release),
    "",
    "## Quick install",
    "",
    "macOS / Linux:",
    "",
    fence(`curl -fsSL ${CANONICAL_SITE_URL}/install | sh`, "sh"),
    "",
    "Windows PowerShell:",
    "",
    fence(`irm ${CANONICAL_SITE_URL}/install.ps1 | iex`, "powershell"),
    "",
    "You can inspect the install scripts directly at `/install` and `/install.ps1`.",
    "",
    "## Pre-built binaries",
    "",
    `Download release archives from [GitHub Releases](${GITHUB_RELEASES_URL}). Each archive includes \`GocciaScriptLoader\`, \`GocciaTestRunner\`, and \`GocciaREPL\`.`,
    "",
    "## Build from source",
    "",
    "FreePascal is required.",
    "",
    fence(
      [
        "git clone https://github.com/frostney/GocciaScript",
        "cd GocciaScript",
        "./build.pas loader testrunner repl",
        "./build/GocciaScriptLoader --help",
      ].join("\n"),
      "sh",
    ),
    "",
    "## Next",
    "",
    list([
      "[Read the docs](/docs)",
      "[Try the playground](/playground?example=coffee-typed)",
      `[Source on GitHub](${GITHUB_REPO_URL})`,
    ]),
  ].join("\n");
}

function compatibilityMarkdown(): string {
  return [
    frontmatter(
      "ECMAScript Compatibility - GocciaScript",
      "test262 compatibility dashboard generated from main-branch CI reports.",
    ),
    "",
    "# ECMAScript Compatibility",
    "",
    "GocciaScript tracks ECMAScript compatibility with generated test262 reports from main-branch CI. The HTML dashboard shows the latest available result for each day, pass-rate and runtime timelines, top-level test groups, and the five least-covered test262 path groups.",
    "",
    "## Links",
    "",
    list([
      "[Open the dashboard](/compatibility)",
      "[Latest test262 JSON](/api/test262/latest)",
      "[test262 harness docs](/docs/test262)",
      `[CI workflow](${GITHUB_REPO_URL}/actions/workflows/ci.yml)`,
    ]),
    "",
    "The JSON endpoints build the test262 view from Vercel Blob daily reports at request time with CDN caching; route requests do not download GitHub Actions artifacts.",
  ].join("\n");
}

function playgroundMarkdown(searchParams: URLSearchParams): string {
  const requested = searchParams.get("example");
  const example = pickExample(requested, "coffee-typed");

  return [
    frontmatter(
      "Playground - GocciaScript",
      "Run GocciaScript in the browser with example programs and selectable backends.",
    ),
    "",
    "# Playground",
    "",
    "Run GocciaScript in the browser. Choose an example, select the tree-walk or bytecode VM backend, toggle ASI, and execute the script.",
    "",
    "## Examples",
    "",
    list(
      EXAMPLES.map((item) => `\`${item.id}\` - ${item.label}: ${item.desc}`),
    ),
    "",
    "## Selected example",
    "",
    `**${example.label}**`,
    "",
    example.desc,
    "",
    fence(example.code, example.ext ?? "js"),
  ].join("\n");
}

function sandboxMarkdown(): string {
  const gocciaFlow = TOOL_CALL_FLOWS.goccia;
  const runnerCommand = `./build/GocciaSandboxRunner /main.js \\
  --seed-config=./sandbox.seed.json \\
  --mode=bytecode \\
  --diff`;
  const seedConfig = `{
  "files": [
    { "from": "./project", "to": "/" },
    { "from": "./tools", "to": "/tools" },
    { "path": "/main.js", "text": "import fs from \\"fs\\";\\nconsole.log(fs.readdirSync('/'));" },
    { "path": "/data.bin", "base64": "AQID" }
  ]
}`;
  const vfsScript = `import fs from "fs";
import { $, runScript } from "goccia";

fs.mkdirSync("/out", { recursive: true });
fs.writeFileSync("/out/summary.txt", "ready\\n");

const audit = runScript("/tools/audit.js", {
  sandbox: true,
  seed: ["/tools/audit.js", { from: "/out", to: "/input" }],
  diff: true,
});

console.log(await $\`cat /out/summary.txt\`.text());
console.log(audit.diff);`;

  return [
    frontmatter(
      "Sandbox - GocciaScript",
      "A runtime designed for untrusted code, host-provided globals, AI agent tool calls, and seeded virtual filesystems.",
    ),
    "",
    "# Sandbox",
    "",
    "GocciaScript is meant for running untrusted or user-provided code in a host-controlled environment. Scripts receive only the data and capabilities the host provides, run with explicit limits, and return structured results that the host can inspect.",
    "",
    "Filesystem workflows use GocciaSandboxRunner. Host paths are copied into a virtual filesystem as seed baselines, sandbox writes stay in that filesystem, and changes are surfaced as explicit diffs.",
    "",
    "## Agent flow",
    "",
    list([
      "AI agent emits GocciaScript via a tool call.",
      "Goccia sandbox runs the script with explicit globals, seed baselines, capability gates, timeout, and memory cap.",
      "The host receives a structured JSON result.",
    ]),
    "",
    "## Virtual filesystem runner",
    "",
    'GocciaSandboxRunner executes an entry path inside an isolated virtual filesystem. Seed paths and JSON seed config copy files into the sandbox before execution; they are import baselines, not live mounts. Source can import `"fs"` for sandbox filesystem operations and `"goccia"` for shell commands or nested execution.',
    "",
    fence(runnerCommand, "bash"),
    "",
    "Seed config:",
    "",
    fence(seedConfig, "json"),
    "",
    "Inside the sandbox:",
    "",
    fence(vfsScript, "javascript"),
    "",
    "## Tool-call comparison",
    "",
    `Task: ${TOOL_CALL_TASK}`,
    "",
    "A typical shell-tool flow takes several dependent calls. The GocciaScript flow handles the same task in one sandboxed call:",
    "",
    fence(gocciaFlow.steps[0]?.call ?? "", "js"),
    "",
    "Result:",
    "",
    fence(gocciaFlow.steps[0]?.result ?? "", "json"),
    "",
    "## Sandbox preview",
    "",
    "The interactive page lets you edit a script and JSON globals, execute the runtime, and inspect the host result.",
  ].join("\n");
}

export async function createSiteMarkdown(
  segments: readonly string[] | undefined,
  searchParams = new URLSearchParams(),
): Promise<string | null> {
  const route = resolveMarkdownRoute(segments);
  if (!route) return null;

  switch (route.kind) {
    case "home":
      return homeMarkdown();
    case "docs":
      return docsMarkdown(route.id);
    case "installation":
      return installationMarkdown();
    case "compatibility":
      return compatibilityMarkdown();
    case "playground":
      return playgroundMarkdown(searchParams);
    case "sandbox":
      return sandboxMarkdown();
  }
}
