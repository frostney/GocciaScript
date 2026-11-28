#!/usr/bin/env -S npx tsx
/**
 * ADR numbers that parallel branches cannot share (docs/adr/README.md#numbering).
 *
 * A number is taken by creating the ref refs/adr/<NNNN> on the remote, pointing at a commit whose subject names the
 * ADR's file. A ref can be created only once: when two branches create the same one at the same time, the remote
 * accepts one and refuses the other with "reference already exists", and `new` moves on to the next number. So the
 * file is written with a number nobody else holds, and nothing is renamed later.
 *
 *   npx tsx scripts/adr.ts new "<title>" [--slug <slug>]
 *       reserves the next number, writes docs/adr/<NNNN>-<slug>.md from the template, and adds it to the index
 *   npx tsx scripts/adr.ts move <docs/adr/NNNN-old.md> "<title>" [--slug <slug>]
 *       renames an ADR this branch reserved, keeping its number, and moves the reservation to the new file name
 *   npx tsx scripts/adr.ts check [--offline]
 *       fails on two ADRs with one number, on an ADR after 0202 whose reservation is missing or names another file,
 *       and on an index (docs/adr/README.md) that does not list every ADR exactly once
 *   npx tsx scripts/adr.ts --self-test
 *
 * --remote <name> (default origin) names the remote that holds the reservations. ADRs up to 0202 were numbered before
 * reservations and hold none.
 */

import { execFileSync, spawnSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, renameSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const REPO_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
const ADR_DIR = "docs/adr";
const INDEX = `${ADR_DIR}/README.md`;
const RESERVATIONS = "refs/adr";
/** the last ADR numbered before reservations: every later one holds one */
const LAST_UNRESERVED = 202;
const FILE_NAME = /^(\d{4,})-([a-z0-9]+(?:-[a-z0-9]+)*)\.md$/;
const INDEX_ENTRY = /^- \[(\d{4,}) — (.+)\]\(([^)\s]+)\)$/;
const MAX_ATTEMPTS = 50;

type Context = { root: string; remote: string };
type Adr = { number: number; file: string };
type Reservation = { file: string; commit: string };

function git(ctx: Context, args: string[], input?: string): string {
  return execFileSync("git", args, { cwd: ctx.root, encoding: "utf8", input, stdio: ["pipe", "pipe", "pipe"] }).trim();
}

function pad(n: number): string {
  return String(n).padStart(4, "0");
}

function slugify(title: string): string {
  const words = title
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[̀-ͯ]/g, "")
    .replace(/[^a-z0-9]+/g, " ")
    .trim()
    .split(/\s+/)
    .filter(Boolean);
  let slug = "";
  for (const word of words) {
    if (slug && slug.length + 1 + word.length > 72) break;
    slug = slug ? `${slug}-${word}` : word;
  }
  return slug;
}

function adrFiles(ctx: Context): { adrs: Adr[]; misnamed: string[] } {
  const adrs: Adr[] = [];
  const misnamed: string[] = [];
  for (const name of readdirSync(join(ctx.root, ADR_DIR)).sort()) {
    if (name === "README.md") continue;
    const m = FILE_NAME.exec(name);
    if (m) adrs.push({ number: Number(m[1]), file: `${ADR_DIR}/${name}` });
    else misnamed.push(`${ADR_DIR}/${name}`);
  }
  return { adrs, misnamed };
}

/** the numbers reserved on the remote, read live */
function remoteReservations(ctx: Context): Map<number, string> {
  const out = new Map<number, string>();
  for (const line of git(ctx, ["ls-remote", ctx.remote, `${RESERVATIONS}/*`]).split("\n")) {
    const m = /^([0-9a-f]+)\s+refs\/adr\/(\d+)$/.exec(line);
    if (m) out.set(Number(m[2]), m[1]);
  }
  return out;
}

/** the reservations, each with the file it names and its commit, fetched from the remote in one snapshot */
function fetchReservations(ctx: Context): Map<number, Reservation> {
  git(ctx, ["fetch", "--quiet", "--no-tags", "--prune", ctx.remote, `+${RESERVATIONS}/*:${RESERVATIONS}/*`]);
  const out = new Map<number, Reservation>();
  const listing = git(ctx, ["for-each-ref", "--format=%(refname) %(objectname) %(contents:subject)", RESERVATIONS]);
  for (const line of listing.split("\n")) {
    const m = /^refs\/adr\/(\d+) ([0-9a-f]+) ADR \d+ (\S+)$/.exec(line);
    if (m) out.set(Number(m[1]), { commit: m[2], file: m[3] });
  }
  return out;
}

/** a commit naming the file, unique to this attempt: an identical commit already on the remote would push as "up to
    date" and pass for a second create of the same ref */
function reservationCommit(ctx: Context, n: number, file: string): string {
  const emptyTree = git(ctx, ["mktree"], "");
  const nonce = randomBytes(16).toString("hex");
  return git(ctx, ["commit-tree", emptyTree, "-m", `ADR ${pad(n)} ${file}`, "-m", `attempt ${nonce}`]);
}

function checkSlug(slug: string): void {
  if (!/^[a-z0-9]+(?:-[a-z0-9]+)*$/.test(slug)) throw new Error(`"${slug}" is not a slug: lower-case words joined by -`);
}

/** whether the remote's default branch already has the file: the ADR has merged */
function published(ctx: Context, file: string): boolean {
  const head = /^ref: (refs\/heads\/\S+)\s+HEAD$/m.exec(git(ctx, ["ls-remote", "--symref", ctx.remote, "HEAD"]));
  if (!head) throw new Error(`${ctx.remote} has no default branch`);
  git(ctx, ["fetch", "--quiet", "--no-tags", ctx.remote, head[1]]);
  return spawnSync("git", ["cat-file", "-e", `FETCH_HEAD:${file}`], { cwd: ctx.root }).status === 0;
}

/** creates refs/adr/<n> on the remote when nobody holds it (expected undefined), or moves it from expected */
function pushReservation(ctx: Context, n: number, commit: string, expected?: string): boolean {
  const ref = `${RESERVATIONS}/${pad(n)}`;
  const push = spawnSync(
    "git",
    // no + on the refspec: it would force the update past the lease
    ["push", "--porcelain", `--force-with-lease=${ref}:${expected ?? ""}`, ctx.remote, `${commit}:${ref}`],
    { cwd: ctx.root, encoding: "utf8" },
  );
  if (push.status === 0) return true;
  const holder = remoteReservations(ctx).get(n);
  if (holder === commit) return true;
  if (holder !== undefined && holder !== expected) return false;
  throw new Error(`could not push ${ref}:\n${push.stdout}${push.stderr}`);
}

function addIndexEntry(ctx: Context, n: number, title: string, fileName: string): void {
  const path = join(ctx.root, INDEX);
  const lines = readFileSync(path, "utf8").split("\n");
  const entry = `- [${pad(n)} — ${title}](${fileName})`;
  let insertAt = -1;
  for (let i = 0; i < lines.length; i++) {
    const m = INDEX_ENTRY.exec(lines[i]);
    if (m && Number(m[1]) < n) insertAt = i + 1;
  }
  if (insertAt < 0) throw new Error(`${INDEX} has no ADR list to add ${fileName} to`);
  lines.splice(insertAt, 0, entry);
  writeFileSync(path, lines.join("\n"));
}

function template(title: string): string {
  const date = new Date().toISOString().slice(0, 10);
  return `# ${title}

**Date:** ${date}
**Area:** TODO
**Related:** TODO

## Context

TODO

## Decision

TODO

## Consequences

TODO
`;
}

function parseSlug(args: string[]): string | undefined {
  const i = args.indexOf("--slug");
  return i >= 0 ? args[i + 1] : undefined;
}

function create(ctx: Context, title: string, slugOption?: string): string {
  const slug = slugOption ?? slugify(title);
  checkSlug(slug);
  const highestFile = Math.max(0, ...adrFiles(ctx).adrs.map((a) => a.number));
  const highestReserved = Math.max(0, ...remoteReservations(ctx).keys());
  let n = Math.max(highestFile, highestReserved, LAST_UNRESERVED) + 1;
  for (let attempt = 0; attempt < MAX_ATTEMPTS; attempt++, n++) {
    const fileName = `${pad(n)}-${slug}.md`;
    if (pushReservation(ctx, n, reservationCommit(ctx, n, `${ADR_DIR}/${fileName}`))) {
      writeFileSync(join(ctx.root, ADR_DIR, fileName), template(title));
      addIndexEntry(ctx, n, title, fileName);
      return `${ADR_DIR}/${fileName}`;
    }
  }
  throw new Error(`no free ADR number after ${MAX_ATTEMPTS} attempts`);
}

function move(ctx: Context, oldFile: string, title: string, slugOption?: string): string {
  const name = oldFile.replace(/^\.\//, "").replace(/^docs\/adr\//, "");
  const m = FILE_NAME.exec(name);
  if (!m || !existsSync(join(ctx.root, ADR_DIR, name))) throw new Error(`${oldFile} is not an ADR in ${ADR_DIR}`);
  const n = Number(m[1]);
  const from = `${ADR_DIR}/${name}`;
  const slug = slugOption ?? slugify(title);
  checkSlug(slug);
  const toName = `${pad(n)}-${slug}.md`;
  const to = `${ADR_DIR}/${toName}`;
  const held = fetchReservations(ctx).get(n);
  if (held?.file !== from) throw new Error(`ADR ${pad(n)} is not reserved for ${from}`);
  // a merged ADR is a record: renaming it would leave the default branch's file without its reservation
  if (published(ctx, from)) throw new Error(`${from} has merged; an ADR is renamed only before it merges`);
  // the lease is the commit just validated, so a move by another branch since then refuses this one
  if (!pushReservation(ctx, n, reservationCommit(ctx, n, to), held.commit)) {
    throw new Error(`ADR ${pad(n)}'s reservation changed while it was being moved`);
  }
  if (to !== from) renameSync(join(ctx.root, from), join(ctx.root, to));
  const path = join(ctx.root, to);
  const body = readFileSync(path, "utf8").replace(/^# .*$/m, `# ${title}`);
  writeFileSync(path, body);
  const indexPath = join(ctx.root, INDEX);
  const index = readFileSync(indexPath, "utf8")
    .split("\n")
    .map((line) => {
      const e = INDEX_ENTRY.exec(line);
      return e && e[3] === name ? `- [${pad(n)} — ${title}](${toName})` : line;
    })
    .join("\n");
  writeFileSync(indexPath, index);
  return to;
}

function check(ctx: Context, offline: boolean): string[] {
  const problems: string[] = [];
  const { adrs, misnamed } = adrFiles(ctx);
  for (const file of misnamed) problems.push(`${file}: an ADR file is named NNNN-slug.md`);

  const byNumber = new Map<number, string[]>();
  for (const adr of adrs) byNumber.set(adr.number, [...(byNumber.get(adr.number) ?? []), adr.file]);
  for (const [n, files] of byNumber) {
    if (files.length > 1) problems.push(`ADR ${pad(n)} is used by ${files.length} files: ${files.join(", ")}`);
  }

  const listed = new Map<string, number>();
  readFileSync(join(ctx.root, INDEX), "utf8")
    .split("\n")
    .forEach((line, i) => {
      const m = INDEX_ENTRY.exec(line);
      if (!m) return;
      const target = m[3];
      listed.set(target, (listed.get(target) ?? 0) + 1);
      if (!existsSync(join(ctx.root, ADR_DIR, target))) problems.push(`${INDEX}:${i + 1}: ${target} does not exist`);
      else if (!target.startsWith(`${m[1]}-`)) problems.push(`${INDEX}:${i + 1}: entry ${m[1]} links to ${target}`);
    });
  for (const adr of adrs) {
    const name = adr.file.slice(ADR_DIR.length + 1);
    const count = listed.get(name) ?? 0;
    if (count !== 1) problems.push(`${INDEX} lists ${name} ${count} times; it lists every ADR once`);
  }

  if (!offline) {
    const reserved = fetchReservations(ctx);
    for (const adr of adrs) {
      if (adr.number <= LAST_UNRESERVED) continue;
      const holder = reserved.get(adr.number)?.file;
      if (holder === undefined) {
        problems.push(`${adr.file}: ADR ${pad(adr.number)} was never reserved; take a number with ` +
          `\`npx tsx scripts/adr.ts new "<title>"\` (docs/adr/README.md#numbering)`);
      } else if (holder !== adr.file) {
        problems.push(`${adr.file}: ADR ${pad(adr.number)} is reserved for ${holder}; take a number of your own with ` +
          `\`npx tsx scripts/adr.ts new "<title>"\`, or after renaming your own ADR, ` +
          `\`npx tsx scripts/adr.ts move\` (docs/adr/README.md#numbering)`);
      }
    }
  }
  return problems;
}

function selfTest(): number {
  const dir = mkdtempSync(join(tmpdir(), "adr-self-test-"));
  let failures = 0;
  const expect = (ok: boolean, what: string) => {
    if (!ok) {
      failures++;
      console.error(`self-test: ${what}`);
    }
  };
  try {
    const remote = join(dir, "remote.git");
    execFileSync("git", ["init", "--quiet", "--bare", "--initial-branch=main", remote]);
    const clone = (name: string): Context => {
      const root = join(dir, name);
      execFileSync("git", ["clone", "--quiet", remote, root], { stdio: "ignore" });
      git({ root, remote: "origin" }, ["config", "user.name", "adr self-test"]);
      git({ root, remote: "origin" }, ["config", "user.email", "adr-self-test@example.invalid"]);
      return { root, remote: "origin" };
    };
    const seed = clone("seed");
    mkdirSync(join(seed.root, ADR_DIR), { recursive: true });
    const legacy = `${pad(LAST_UNRESERVED)}-the-last-legacy-decision.md`;
    writeFileSync(join(seed.root, ADR_DIR, legacy), "# The last legacy decision\n");
    writeFileSync(join(seed.root, INDEX), `# ADRs\n\n## Records\n\n- [${pad(LAST_UNRESERVED)} — The last legacy decision](${legacy})\n`);
    git(seed, ["add", "."]);
    git(seed, ["commit", "--quiet", "-m", "seed"]);
    git(seed, ["push", "--quiet", "origin", "HEAD:main"]);

    // two branches that both scan the same files take different numbers
    const a = clone("a");
    const b = clone("b");
    const fileA = create(a, "Lane A decides");
    const fileB = create(b, "Lane B decides");
    expect(fileA === `${ADR_DIR}/${pad(LAST_UNRESERVED + 1)}-lane-a-decides.md`, `lane A took ${fileA}`);
    expect(fileB === `${ADR_DIR}/${pad(LAST_UNRESERVED + 2)}-lane-b-decides.md`, `lane B took ${fileB}`);
    expect(check(a, false).length === 0, `lane A's check: ${check(a, false).join("; ")}`);
    expect(check(b, false).length === 0, `lane B's check: ${check(b, false).join("; ")}`);

    // a race for the same number: the remote accepts one create and refuses the other
    const n = LAST_UNRESERVED + 10;
    const first = pushReservation(a, n, reservationCommit(a, n, `${ADR_DIR}/${pad(n)}-x.md`));
    const second = pushReservation(b, n, reservationCommit(b, n, `${ADR_DIR}/${pad(n)}-y.md`));
    expect(first && !second, `a second create of refs/adr/${pad(n)} was accepted`);

    // a branch that picks the highest number in its own tree plus one takes lane A's number: the check fails
    const c = clone("c");
    const picked = `${pad(LAST_UNRESERVED + 1)}-lane-c-guesses.md`;
    writeFileSync(join(c.root, ADR_DIR, picked), "# Lane C guesses\n");
    writeFileSync(join(c.root, INDEX), readFileSync(join(c.root, INDEX), "utf8") + `- [${pad(LAST_UNRESERVED + 1)} — Lane C guesses](${picked})\n`);
    const guessed = check(c, false);
    expect(guessed.some((p) => p.includes("is reserved for")), `a guessed number passed: ${guessed.join("; ")}`);

    // two files with one number, and an index that misses a file
    writeFileSync(join(a.root, ADR_DIR, `${pad(LAST_UNRESERVED + 1)}-twin.md`), "# Twin\n");
    const twin = check(a, true);
    expect(twin.some((p) => p.includes("is used by 2 files")), `a duplicate number passed: ${twin.join("; ")}`);
    expect(twin.some((p) => p.includes("lists 0203-twin.md 0 times")), `an unlisted ADR passed: ${twin.join("; ")}`);
    rmSync(join(a.root, ADR_DIR, `${pad(LAST_UNRESERVED + 1)}-twin.md`));

    // a branch renames its own ADR: the reservation follows, and nobody else can move it
    const moved = move(a, fileA, "Lane A decides otherwise");
    expect(moved === `${ADR_DIR}/${pad(LAST_UNRESERVED + 1)}-lane-a-decides-otherwise.md`, `moved to ${moved}`);
    expect(check(a, false).length === 0, `after the move: ${check(a, false).join("; ")}`);
    let refused = false;
    try {
      move(c, `${ADR_DIR}/${picked}`, "Lane C takes it");
    } catch {
      refused = true;
    }
    expect(refused, "a branch moved a reservation it does not hold");
    const throws = (f: () => unknown): boolean => {
      try {
        f();
        return false;
      } catch {
        return true;
      }
    };

    // a move to an invalid slug fails before the reservation changes
    const heldBefore = remoteReservations(a).get(LAST_UNRESERVED + 1);
    expect(throws(() => move(a, moved, "Whatever", "bad_slug")), "a move to an invalid slug was accepted");
    expect(remoteReservations(a).get(LAST_UNRESERVED + 1) === heldBefore, "an invalid move changed the reservation");

    // two attempts with one file name, identity and second still push different commits, so only one takes it
    const same = LAST_UNRESERVED + 20;
    const pinned = { GIT_AUTHOR_DATE: "2026-01-01T00:00:00Z", GIT_COMMITTER_DATE: "2026-01-01T00:00:00Z" };
    Object.assign(process.env, pinned);
    const sameFile = `${ADR_DIR}/${pad(same)}-same.md`;
    const one = pushReservation(a, same, reservationCommit(a, same, sameFile));
    const two = pushReservation(b, same, reservationCommit(b, same, sameFile));
    for (const key of Object.keys(pinned)) delete process.env[key];
    expect(one && !two, `two identical attempts both took refs/adr/${pad(same)}`);

    // a move leased on a reservation another move has replaced since is refused
    const nB = LAST_UNRESERVED + 2;
    const stale = fetchReservations(b).get(nB)?.commit;
    const movedB = move(b, fileB, "Lane B decides again");
    expect(!pushReservation(b, nB, reservationCommit(b, nB, fileB), stale), "a move on a stale lease was accepted");
    expect(check(b, false).length === 0, `after lane B's move: ${check(b, false).join("; ")}`);

    // once an ADR has merged, it keeps its name: a move would leave the default branch's file unreserved
    git(b, ["add", "-A"]);
    git(b, ["commit", "--quiet", "-m", "lane B merges"]);
    git(b, ["push", "--quiet", "origin", "HEAD:main"]);
    const mergedHeld = remoteReservations(b).get(nB);
    expect(throws(() => move(b, movedB, "Lane B renames a record")), "a merged ADR was moved");
    expect(remoteReservations(b).get(nB) === mergedHeld, "a refused move of a merged ADR changed its reservation");
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
  return failures;
}

function main(argv: string[]): number {
  const remoteAt = argv.indexOf("--remote");
  const ctx: Context = { root: REPO_ROOT, remote: remoteAt >= 0 ? argv[remoteAt + 1] : "origin" };
  const [command, ...rest] = remoteAt < 0 ? argv : argv.filter((_, i) => i !== remoteAt && i !== remoteAt + 1);
  switch (command) {
    case "new": {
      if (!rest[0] || rest[0].startsWith("--")) break;
      console.log(create(ctx, rest[0], parseSlug(rest)));
      return 0;
    }
    case "move": {
      if (!rest[0] || !rest[1] || rest[1].startsWith("--")) break;
      console.log(move(ctx, rest[0], rest[1], parseSlug(rest)));
      return 0;
    }
    case "check": {
      const problems = check(ctx, rest.includes("--offline"));
      for (const p of problems) console.error(p);
      if (problems.length) return 1;
      console.log("ADR numbers: every ADR has a number of its own and is listed once");
      return 0;
    }
    case "--self-test": {
      const failures = selfTest();
      if (failures) return 1;
      console.log("ADR numbers: self-test passed");
      return 0;
    }
  }
  console.error('usage: adr.ts new "<title>" [--slug s] | move <file> "<title>" [--slug s] | check [--offline] | --self-test');
  return 2;
}

process.exit(main(process.argv.slice(2)));
