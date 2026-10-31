#!/usr/bin/env bun
/** Invocation-wide bundler output planning through the public CLI. */
import {
  existsSync, mkdirSync, readFileSync, readdirSync, realpathSync,
  symlinkSync, writeFileSync,
} from "fs";
import { dirname, join, resolve } from "path";
import { BUNDLER, RUNNER } from "./test-cli/binaries";
import { normalizeLineEndings } from "./test-cli/assertions";
import { clean, mkdtemp } from "./test-cli/tmpdir";

const jsx = 'const createElement = (t, p, ...c) => ({ t, p, c });\nconst el = <div>hello</div>;\n';

function write(path: string, text = "42;\n"): string {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, text);
  return path;
}

function snapshot(root: string): Record<string, string> {
  const result: Record<string, string> = {};
  for (const entry of readdirSync(root, { withFileTypes: true })) {
    const path = join(root, entry.name);
    if (entry.isDirectory()) {
      for (const [name, bytes] of Object.entries(snapshot(path)))
        result[join(entry.name, name)] = bytes;
    } else if (entry.isFile()) result[entry.name] = readFileSync(path).toString("base64");
    else result[entry.name] = "link";
  }
  return result;
}

export function verifyBundlerLayout(bundler = BUNDLER, runner = RUNNER): void {
  const failures: string[] = [];
  function check(name: string, body: (tmp: string) => void): void {
    const tmp = realpathSync.native(mkdtemp("goccia-bundler-layout-"));
    try {
      body(tmp);
    } catch (error) {
      failures.push(`${name}: ${error instanceof Error ? error.message : error}`);
    } finally {
      clean(tmp);
    }
  }
  function invoke(binary: string, args: string[], input?: string, cwd?: string) {
    const result = Bun.spawnSync([resolve(binary), ...args], {
      cwd, stdin: input === undefined ? "ignore" : Buffer.from(input),
      stdout: "pipe", stderr: "pipe", timeout: 60_000,
    });
    const output = normalizeLineEndings(result.stdout.toString() + result.stderr.toString());
    if (result.signalCode) throw new Error(`Process received ${result.signalCode}\n${output}`);
    return { code: result.exitCode, output };
  }
  function succeeds(args: string[], input?: string): string {
    const result = invoke(bundler, args, input);
    if (result.code !== 0) throw new Error(`exit ${result.code}: ${args.join(" ")}\n${result.output}`);
    return result.output;
  }
  function collision(root: string, args: string[], sources: string[], destination: string, input?: string): void {
    const before = JSON.stringify(snapshot(root));
    const result = invoke(bundler, args, input);
    if (result.code === 0 || !result.output.includes("Output collision"))
      throw new Error(`Expected preflight collision refusal, got exit ${result.code}\n${result.output}`);
    for (const text of [...sources, destination])
      if (!result.output.includes(text)) throw new Error(`Diagnostic omits ${text}\n${result.output}`);
    if (JSON.stringify(snapshot(root)) !== before)
      throw new Error(`Collision changed existing files or emitted outputs\n${result.output}`);
    if (result.output.includes("Compiling"))
      throw new Error(`Collision was discovered after compilation started\n${result.output}`);
  }
  function has(path: string): void {
    if (!existsSync(path)) throw new Error(`Missing ${path}`);
  }

  for (const jobs of [1, 4]) {
    for (const extension of ["gbc", "map"]) {
      check(`file destination is another output's parent (${extension}), jobs=${jobs}`, (tmp) => {
        const source = join(tmp, "src");
        write(join(source, "aaa.js"));
        const first = write(join(source, "first.jsx"), jsx);
        const nested = write(join(source, `first.${extension}`, "inner.jsx"), jsx);
        const out = join(tmp, "out");
        write(join(out, "aaa.gbc"), "existing bytecode");
        collision(tmp, [source, `--output=${out}`, "--source-map", `--jobs=${jobs}`],
          [first, nested], join(out, `first.${extension}`));
      });
    }
    check(`same-stem directory collision, jobs=${jobs}`, (tmp) => {
      const source = join(tmp, "src");
      const first = write(join(source, "index.js"));
      const second = write(join(source, "index.ts"));
      write(join(source, "aaa.js")); // Must not write before a later collision.
      const out = join(tmp, "out");
      const destination = write(join(out, "index.gbc"), "existing bytecode");
      write(join(out, "index.map"), "existing map");
      collision(tmp, [source, `--output=${out}`, "--source-map", `--jobs=${jobs}`], [first, second], destination);
    });
    check(`positional files and duplicate paths, jobs=${jobs}`, (tmp) => {
      const first = write(join(tmp, "one", "index.js"));
      const second = write(join(tmp, "two", "index.js"));
      const early = write(join(tmp, "early.js"));
      const out = join(tmp, "out");
      mkdirSync(out);
      const destination = join(out, "index.gbc");
      collision(tmp, [early, first, second, `--output=${out}`, `--jobs=${jobs}`], [first, second], destination);
      collision(tmp, [first, first, `--jobs=${jobs}`], [first], first.replace(/\.js$/, ".gbc"));
    });
    check(`multiple roots and overlapping root/file, jobs=${jobs}`, (tmp) => {
      const left = join(tmp, "left");
      const right = join(tmp, "right");
      const first = write(join(left, "nested", "index.js"));
      const second = write(join(right, "nested", "index.ts"));
      const out = join(tmp, "out");
      mkdirSync(out);
      collision(tmp, [left, right, `--output=${out}`, `--jobs=${jobs}`], [first, second], join(out, "nested", "index.gbc"));
      collision(tmp, [left, first, `--jobs=${jobs}`], [first], first.replace(/\.js$/, ".gbc"));
    });
    check(`multifile expansion collision, jobs=${jobs}`, (tmp) => {
      const source = join(tmp, "src");
      const file = write(join(source, "bundle.js"), "1;\n---\n2;\n");
      const actual = write(join(source, "bundle[part1].ts"));
      const out = join(tmp, "out");
      mkdirSync(out);
      collision(tmp, [file, actual, "--multifile", `--output=${out}`, `--jobs=${jobs}`], [file.replace(/\.js$/, "[part1].js"), actual], join(out, "bundle[part1].gbc"));
    });
    check(`nested directory layout and maps, jobs=${jobs}`, (tmp) => {
      const source = join(tmp, "src");
      write(join(source, "left", "index.jsx"), jsx);
      write(join(source, "right", "deep", "index.jsx"), jsx);
      write(join(source, "right", "deep", "other.jsx"), jsx);
      const out = join(tmp, "out");
      mkdirSync(out);
      succeeds([source, `--output=${out}`, "--source-map", `--jobs=${jobs}`]);
      for (const relative of ["left/index", "right/deep/index", "right/deep/other"]) {
        has(join(out, `${relative}.gbc`));
        const map = JSON.parse(readFileSync(join(out, `${relative}.map`), "utf8"));
        if (map.version !== 3 || !map.mappings || !map.file.endsWith(".gbc") || !map.sources[0].endsWith(".jsx"))
          throw new Error(`Invalid nested map ${relative}: ${JSON.stringify(map)}`);
      }
      if (existsSync(join(out, "index.gbc"))) throw new Error("Directory output was flattened");
    });
    check(`multiple noncolliding roots and explicit file naming, jobs=${jobs}`, (tmp) => {
      const left = join(tmp, "left");
      const right = join(tmp, "right");
      write(join(left, "a", "index.js"));
      write(join(right, "b", "index.js"));
      const explicit = write(join(tmp, "explicit", "nested", "entry.js"));
      const out = join(tmp, "out");
      mkdirSync(out);
      succeeds([left, right, explicit, `--output=${out}`, `--jobs=${jobs}`]);
      for (const file of ["a/index.gbc", "b/index.gbc", "entry.gbc"]) has(join(out, file));
    });
    check(`nested multifile and unchanged companion paths, jobs=${jobs}`, (tmp) => {
      const source = join(tmp, "src");
      write(join(source, "nested", "bundle.js"), "1;\n---\n2;\n");
      const out = join(tmp, "out");
      mkdirSync(out);
      succeeds([source, "--multifile", `--output=${out}`, `--jobs=${jobs}`]);
      succeeds([source, "--multifile", `--jobs=${jobs}`]);
      for (const root of [source, out])
        for (const part of [1, 2]) has(join(root, "nested", `bundle[part${part}].gbc`));
    });
    check(`configured nested output, jobs=${jobs}`, (tmp) => {
      const project = join(tmp, "project");
      const source = join(project, "src");
      write(join(source, "nested", "index.jsx"), jsx);
      write(join(source, "nested", "other.jsx"), jsx);
      const out = join(project, "out");
      mkdirSync(out);
      const config = write(join(project, "goccia.json"), JSON.stringify({ output: "out", "source-map": "" }));
      succeeds([source, `--config=${config}`, `--jobs=${jobs}`]);
      has(join(out, "nested", "index.gbc"));
      has(join(out, "nested", "index.map"));
    });
  }

  check("output and source map share a destination", (tmp) => {
    const input = write(join(tmp, "input.jsx"), jsx);
    const out = write(join(tmp, "result.gbc"), "keep me");
    collision(tmp, [input, `--output=${out}`, `--source-map=${out}`], [input, "bytecode", "source map"], out);
    const map = write(join(tmp, "result.map"), "keep map");
    collision(tmp, [input, `--output=${map}`, "--source-map"], [input], map);
  });
  check("explicit shared source map", (tmp) => {
    const first = write(join(tmp, "first.jsx"), jsx);
    const second = write(join(tmp, "second.jsx"), jsx);
    const map = write(join(tmp, "result.map"), "keep map");
    collision(tmp, [first, second, `--source-map=${map}`], [first, second], map);
  });
  check("stdin multifile planning and naming", (tmp) => {
    const out = join(tmp, "out");
    mkdirSync(out);
    succeeds(["--multifile", `--output=${out}`], "1;\n---\n2;\n");
    has(join(out, "_stdin_[part1].gbc"));
    has(join(out, "_stdin_[part2].gbc"));
    const file = write(join(tmp, "_stdin_[part1].js"));
    collision(tmp, [file, "-", "--multifile", `--output=${out}`], [file, "<stdin>[part1]"], join(out, "_stdin_[part1].gbc"), "3;\n---\n4;\n");
  });
  check("relocated module tree retains companion resolution", (tmp) => {
    const source = join(tmp, "src");
    write(join(source, "nested", "value.mjs"), "export const value = 42;\n");
    write(join(source, "entry.mjs"), 'import { value } from "./nested/value.mjs"; console.log("LAYOUT:" + value);\n');
    const out = join(tmp, "out");
    mkdirSync(out);
    succeeds([source, `--output=${out}`]);
    clean(source);
    const result = invoke(runner, [join(out, "entry.gbc"), "--prefer-bytecode"]);
    if (result.code !== 0 || !result.output.split("\n").includes("LAYOUT:42"))
      throw new Error(`Relocated companions failed\n${result.output}`);
  });
  check("configured output outside its root is rejected", (tmp) => {
    const project = join(tmp, "project");
    const source = write(join(project, "src", "nested", "input.js"));
    const out = join(tmp, "outside");
    mkdirSync(out);
    const config = write(join(project, "goccia.json"), JSON.stringify({ output: "../outside" }));
    const before = JSON.stringify(snapshot(tmp));
    const result = invoke(bundler, [source, `--config=${config}`]);
    if (result.code === 0 || !result.output.includes("inside its own directory") || JSON.stringify(snapshot(tmp)) !== before)
      throw new Error(`Config confinement failed\n${result.output}`);
  });
  if (process.platform !== "win32") {
    check("canonical aliases collide before writing", (tmp) => {
      const source = join(tmp, "src");
      const first = write(join(source, "first", "index.js"));
      const second = write(join(source, "second", "index.js"));
      const out = join(tmp, "out");
      mkdirSync(join(out, "first"), { recursive: true });
      symlinkSync(join(out, "first"), join(out, "second"), "dir");
      collision(tmp, [source, `--output=${out}`], [first, second], join(out, "second", "index.gbc"));
    });
    check("configured nested symlink cannot create outside parents", (tmp) => {
      const project = join(tmp, "project");
      const source = join(project, "src");
      write(join(source, "nested", "deep", "input.js"));
      const out = join(project, "out");
      const outside = join(tmp, "outside");
      mkdirSync(out);
      mkdirSync(outside);
      write(join(outside, "sentinel"), "keep");
      symlinkSync(outside, join(out, "nested"), "dir");
      const config = write(join(project, "goccia.json"), JSON.stringify({ output: "out" }));
      const before = JSON.stringify(snapshot(tmp));
      const result = invoke(bundler, [source, `--config=${config}`]);
      if (result.code === 0 || !result.output.includes("Refusing to write") || JSON.stringify(snapshot(tmp)) !== before)
        throw new Error(`Nested config link was not refused without changes\n${result.output}`);
    });
    check("configured nested map symlink preserves victim", (tmp) => {
      const project = join(tmp, "project");
      const source = join(project, "src");
      write(join(source, "nested", "input.jsx"), jsx);
      const out = join(project, "out");
      mkdirSync(join(out, "nested"), { recursive: true });
      const victim = write(join(tmp, "victim"), "keep");
      symlinkSync(victim, join(out, "nested", "input.map"));
      const config = write(join(project, "goccia.json"), JSON.stringify({ output: "out", "source-map": "" }));
      const result = invoke(bundler, [source, `--config=${config}`]);
      if (result.code === 0 || readFileSync(victim, "utf8") !== "keep")
        throw new Error(`Map symlink write was not refused or changed its target\n${result.output}`);
    });
  }
  if (failures.length) throw new Error(failures.join("\n\n"));
}

if (import.meta.main) {
  verifyBundlerLayout(process.argv[2] ?? BUNDLER, process.argv[3] ?? RUNNER);
  console.log("Bundler output layout checks passed");
}
