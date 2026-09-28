import { afterEach, describe, expect, test } from "bun:test";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

// Runs public/install.sh against a fake `curl` that hands it a local
// archive, so the install step can be checked without the network.

const INSTALL_SCRIPT = resolve(import.meta.dir, "../../public/install.sh");
const isWindows = process.platform === "win32";
const isMac = process.platform === "darwin";

const roots: string[] = [];
afterEach(() => {
  for (const root of roots.splice(0))
    rmSync(root, { recursive: true, force: true });
});

function makeRoot(): string {
  const root = mkdtempSync(join(tmpdir(), "goccia-install-"));
  roots.push(root);
  return root;
}

function writeExecutable(path: string, text: string): void {
  writeFileSync(path, text);
  chmodSync(path, 0o755);
}

/** A release archive holding the given runner and its two siblings. */
function buildArchive(root: string, version: string, runner: string): string {
  const stage = join(root, "stage");
  const dir = join(stage, `gocciascript-${version}-fake`);
  mkdirSync(dir, { recursive: true });
  for (const bin of [runner, "GocciaTestRunner", "GocciaREPL"])
    writeExecutable(join(dir, bin), `#!/bin/sh\necho "${bin} ${version}"\n`);
  const archive = join(root, isMac ? "release.zip" : "release.tar.gz");
  const pack = isMac
    ? Bun.spawnSync(["zip", "-qr", archive, `gocciascript-${version}-fake`], {
        cwd: stage,
      })
    : Bun.spawnSync(["tar", "czf", archive, `gocciascript-${version}-fake`], {
        cwd: stage,
      });
  if (pack.exitCode !== 0)
    throw new Error(`packing failed: ${pack.stderr.toString()}`);
  return archive;
}

/** A `curl` that writes ARCHIVE to its -o target and ignores the URL. */
function fakeCurlDir(root: string, archive: string): string {
  const bin = join(root, "fakebin");
  mkdirSync(bin, { recursive: true });
  writeExecutable(
    join(bin, "curl"),
    [
      "#!/bin/sh",
      "out=''",
      "while [ $# -gt 0 ]; do",
      '  case "$1" in',
      '    -o) out="$2"; shift 2 ;;',
      "    *) shift ;;",
      "  esac",
      "done",
      `cp "${archive}" "$out"`,
      "",
    ].join("\n"),
  );
  return bin;
}

function runInstaller(
  root: string,
  version: string,
  runner: string,
  installDir: string,
) {
  const archive = buildArchive(root, version, runner);
  const fakeBin = fakeCurlDir(root, archive);
  const proc = Bun.spawnSync(["sh", INSTALL_SCRIPT], {
    env: {
      ...process.env,
      PATH: `${fakeBin}:${process.env.PATH ?? ""}`,
      INSTALL_DIR: installDir,
      GOCCIA_VERSION: `v${version}`,
    },
    stdout: "pipe",
    stderr: "pipe",
  });
  return {
    exitCode: proc.exitCode,
    output: proc.stdout.toString() + proc.stderr.toString(),
  };
}

describe.skipIf(isWindows)("install.sh", () => {
  test("a pinned pre-0.14 install removes a newer GocciaRunner from its directory", () => {
    const root = makeRoot();
    const installDir = join(root, "bin");
    const elsewhere = join(root, "elsewhere");
    mkdirSync(installDir);
    mkdirSync(elsewhere);
    writeExecutable(join(installDir, "GocciaRunner"), "#!/bin/sh\necho new\n");
    writeExecutable(join(elsewhere, "GocciaRunner"), "#!/bin/sh\necho other\n");

    const run = runInstaller(root, "0.13.0", "GocciaScriptLoader", installDir);
    expect(run.exitCode).toBe(0);
    expect(existsSync(join(installDir, "GocciaRunner"))).toBe(false);
    expect(
      readFileSync(join(installDir, "GocciaScriptLoader"), "utf8"),
    ).toContain("0.13.0");
    expect(run.output).toContain(
      `Removed ${installDir}/GocciaRunner, which belongs to a different GocciaScript release than 0.13.0.`,
    );
    // Only the installer's own directory is touched.
    expect(existsSync(join(elsewhere, "GocciaRunner"))).toBe(true);
  });

  test("a 0.14 install removes the retired runner names from its directory", () => {
    const root = makeRoot();
    const installDir = join(root, "bin");
    mkdirSync(installDir);
    writeExecutable(
      join(installDir, "GocciaScriptLoader"),
      "#!/bin/sh\necho old\n",
    );
    writeExecutable(
      join(installDir, "GocciaSandboxRunner"),
      "#!/bin/sh\necho old\n",
    );
    writeExecutable(
      join(installDir, "unrelated-tool"),
      "#!/bin/sh\necho keep\n",
    );

    const run = runInstaller(root, "0.14.0", "GocciaRunner", installDir);
    expect(run.exitCode).toBe(0);
    expect(readFileSync(join(installDir, "GocciaRunner"), "utf8")).toContain(
      "0.14.0",
    );
    expect(existsSync(join(installDir, "GocciaScriptLoader"))).toBe(false);
    expect(existsSync(join(installDir, "GocciaSandboxRunner"))).toBe(false);
    expect(existsSync(join(installDir, "unrelated-tool"))).toBe(true);
    expect(run.output).toContain(`Removed ${installDir}/GocciaScriptLoader`);
    expect(run.output).toContain(`Removed ${installDir}/GocciaSandboxRunner`);
  });

  test("the same generation is replaced in place with nothing removed", () => {
    const root = makeRoot();
    const installDir = join(root, "bin");
    mkdirSync(installDir);
    writeExecutable(
      join(installDir, "GocciaRunner"),
      "#!/bin/sh\necho older\n",
    );

    const run = runInstaller(root, "0.14.1", "GocciaRunner", installDir);
    expect(run.exitCode).toBe(0);
    expect(readFileSync(join(installDir, "GocciaRunner"), "utf8")).toContain(
      "0.14.1",
    );
    expect(run.output).not.toContain("Removed");
  });
});
