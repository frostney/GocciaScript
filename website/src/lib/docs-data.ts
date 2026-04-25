export type DocPage = {
  /** URL slug; the `/docs/<id>` route. */
  id: string;
  /** Sidebar / metadata title. */
  title: string;
  /** Sidebar group heading. */
  group: string;
  /** Short description used on stub fallbacks. */
  desc?: string;
  /** Path of the markdown file under `content/docs/`, relative to that root. */
  file: string;
};

export const DOC_PAGES: DocPage[] = [
  {
    id: "readme",
    title: "Overview",
    group: "Start here",
    file: "readme.md",
  },
  {
    id: "tutorial",
    title: "Tutorial",
    group: "Start here",
    desc: "A guided walkthrough writing your first GocciaScript program.",
    file: "tutorial.md",
  },
  {
    id: "goals",
    title: "Project Goals",
    group: "Start here",
    desc: "Sandboxed AI agent runtime and embeddable desktop platform.",
    file: "goals.md",
  },
  {
    id: "language",
    title: "Language",
    group: "Language",
    desc: "Complete ECMAScript subset specification, excluded features, and rationale.",
    file: "language.md",
  },
  {
    id: "language-tables",
    title: "Language Tables",
    group: "Language",
    desc: "Quick-reference: feature matrix and TC39 proposal status.",
    file: "language-tables.md",
  },
  {
    id: "built-ins",
    title: "Built-in Objects",
    group: "Language",
    desc: "Complete API reference for every built-in global.",
    file: "built-ins.md",
  },
  {
    id: "built-ins-temporal",
    title: "Temporal Built-ins",
    group: "Language",
    desc: "Temporal API: dates, times, durations, time zones.",
    file: "built-ins-temporal.md",
  },
  {
    id: "built-ins-binary-data",
    title: "Binary Data Built-ins",
    group: "Language",
    desc: "ArrayBuffer, SharedArrayBuffer, TypedArray API.",
    file: "built-ins-binary-data.md",
  },
  {
    id: "built-ins-data-formats",
    title: "Data Format Built-ins",
    group: "Language",
    desc: "JSON, JSON5, TOML, YAML, JSONL, CSV, TSV parsers.",
    file: "built-ins-data-formats.md",
  },
  {
    id: "errors",
    title: "Errors",
    group: "Language",
    desc: "Error types, parser/runtime display, JSON output, Error.cause.",
    file: "errors.md",
  },
  {
    id: "architecture",
    title: "Architecture",
    group: "Internals",
    desc: "Pipelines, main layers, design direction, duplication boundaries.",
    file: "architecture.md",
  },
  {
    id: "interpreter",
    title: "Interpreter",
    group: "Internals",
    desc: "Tree-walk backend.",
    file: "interpreter.md",
  },
  {
    id: "bytecode-vm",
    title: "Bytecode VM",
    group: "Internals",
    desc: "Register-based bytecode backend and VM.",
    file: "bytecode-vm.md",
  },
  {
    id: "core-patterns",
    title: "Core Patterns",
    group: "Internals",
    desc: "Recurring implementation patterns and internal terminology.",
    file: "core-patterns.md",
  },
  {
    id: "value-system",
    title: "Value System",
    group: "Internals",
    desc: "Type hierarchy, virtual property access, primitives, objects.",
    file: "value-system.md",
  },
  {
    id: "garbage-collector",
    title: "Garbage Collector",
    group: "Internals",
    desc: "Mark-and-sweep GC: architecture, contributor rules, design rationale.",
    file: "garbage-collector.md",
  },
  {
    id: "decision-log",
    title: "Decision Log",
    group: "Internals",
    desc: "Architectural decisions and the trade-offs behind them.",
    file: "decision-log.md",
  },
  {
    id: "adding-built-in-types",
    title: "Adding Built-in Types",
    group: "Internals",
    desc: "How to add new built-in types to the engine.",
    file: "adding-built-in-types.md",
  },
  {
    id: "embedding",
    title: "Embedding the Engine",
    group: "Tooling",
    desc: "Embedding GocciaScript in FreePascal applications.",
    file: "embedding.md",
  },
  {
    id: "testing",
    title: "Testing",
    group: "Tooling",
    desc: "Test organization, running tests, coverage, CI.",
    file: "testing.md",
  },
  {
    id: "testing-api",
    title: "Testing API",
    group: "Tooling",
    desc: "test/describe/expect API — Vitest/Jest-compatible.",
    file: "testing-api.md",
  },
  {
    id: "benchmarks",
    title: "Benchmarks",
    group: "Tooling",
    desc: "Benchmark runner, output formats, writing benchmarks.",
    file: "benchmarks.md",
  },
  {
    id: "build-system",
    title: "Build System",
    group: "Tooling",
    desc: "Build commands, compiler configuration, CI/CD.",
    file: "build-system.md",
  },
  {
    id: "profiling",
    title: "Profiling",
    group: "Tooling",
    desc: "Bytecode VM profiling — opcodes, functions, flame graphs.",
    file: "profiling.md",
  },
  {
    id: "contributing-workflow",
    title: "Contributing — Workflow",
    group: "Community",
    desc: "Branching, commits, PRs, testing, formatting.",
    file: "contributing/workflow.md",
  },
  {
    id: "contributing-code-style",
    title: "Contributing — Code Style",
    group: "Community",
    desc: "FreePascal style, naming, structure.",
    file: "contributing/code-style.md",
  },
  {
    id: "contributing-tooling",
    title: "Contributing — Tooling",
    group: "Community",
    desc: "Build tools, formatter, IDE setup.",
    file: "contributing/tooling.md",
  },
];

// Mapping used by markdown.tsx to resolve cross-doc links like
// `docs/architecture.md` (or `CONTRIBUTING.md`) → /docs/<id>.
export const DOC_HREF_MAP: Record<string, string> = {
  language: "language",
  "language-tables": "language-tables",
  "built-ins": "built-ins",
  "built-ins-temporal": "built-ins-temporal",
  "built-ins-binary-data": "built-ins-binary-data",
  "built-ins-data-formats": "built-ins-data-formats",
  errors: "errors",
  architecture: "architecture",
  interpreter: "interpreter",
  "bytecode-vm": "bytecode-vm",
  "core-patterns": "core-patterns",
  "value-system": "value-system",
  "garbage-collector": "garbage-collector",
  "decision-log": "decision-log",
  "adding-built-in-types": "adding-built-in-types",
  embedding: "embedding",
  testing: "testing",
  "testing-api": "testing-api",
  benchmarks: "benchmarks",
  "build-system": "build-system",
  profiling: "profiling",
  goals: "goals",
  tutorial: "tutorial",
  "contributing/workflow": "contributing-workflow",
  "contributing/code-style": "contributing-code-style",
  "contributing/tooling": "contributing-tooling",
  CONTRIBUTING: "contributing-workflow",
  AGENTS: "contributing-workflow",
  LICENSE: "contributing-workflow",
};
