/**
 * Markdown file discovery shared by the scripts/check-doc-*.ts checks.
 *
 * Inside a git checkout the candidates are the files git would see: tracked
 * files plus untracked files that .gitignore does not exclude. A gitignored
 * tree such as a repository copy under tmp/ is therefore never scanned. Outside
 * a git checkout (an exported source tree, or git not installed) the checks
 * fall back to walking the directory.
 */

import { existsSync, lstatSync, readdirSync, realpathSync } from "fs";
import { spawnSync } from "child_process";
import { join, relative } from "path";

const EXTENSIONS = new Set([".md", ".mdx"]);
const IGNORE_DIRS = new Set(["node_modules", ".git", ".agents", ".claude", "dist", "build", ".next", "vendor"]);
// Build artifacts whose contents are synced from elsewhere and validated at
// the source location. Path-prefix matched against repo-relative paths.
const IGNORE_PATH_PREFIXES = ["website/content/docs/"];

const toSlashes = (path: string): string => path.split("\\").join("/");

const hasMarkdownExtension = (name: string): boolean =>
  EXTENSIONS.has(name.slice(name.lastIndexOf(".")));

/**
 * Lists the files under `dir` that git tracks or would add, relative to `dir`,
 * or returns null when git is not installed or `dir` is not inside a git work
 * tree. Any other git failure throws: walking instead would scan the ignored
 * trees this listing exists to skip.
 */
const listGitFiles = (dir: string): string[] | null => {
  const pathspecs = [...EXTENSIONS].map((ext) => `*${ext}`);
  const result = spawnSync("git", ["ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", ...pathspecs], {
    cwd: dir,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    // Keep git's messages in English so "not a git repository" can be matched.
    env: { ...process.env, LC_ALL: "C" },
  });
  if (result.error) {
    if ((result.error as NodeJS.ErrnoException).code === "ENOENT") return null;
    throw result.error;
  }
  if (result.status === 0) return result.stdout.split("\0").filter((path) => path.length > 0);
  if (/not a git repository/i.test(result.stderr)) return null;
  throw new Error(`git ls-files failed in ${dir} (exit ${result.status}): ${result.stderr.trim()}`);
};

const walkFiles = (dir: string): string[] => {
  const results: string[] = [];
  const walk = (d: string): void => {
    for (const entry of readdirSync(d, { withFileTypes: true })) {
      const full = join(d, entry.name);
      if (entry.isDirectory()) {
        if (IGNORE_DIRS.has(entry.name)) continue;
        walk(full);
      } else {
        results.push(relative(dir, full));
      }
    }
  };
  walk(dir);
  return results;
};

/**
 * Returns the markdown files under `dir` as absolute paths, skipping
 * IGNORE_DIRS at any depth and IGNORE_PATH_PREFIXES relative to `root`.
 * A symlink to a file that is also listed is dropped in favour of the file,
 * so no file is scanned twice. Paths are returned in sorted order.
 */
export const findMarkdownFiles = (root: string, dir: string): string[] => {
  const files: string[] = [];
  const links: string[] = [];
  const candidates = (listGitFiles(dir) ?? walkFiles(dir)).map(toSlashes).sort();
  for (const candidate of candidates) {
    const segments = candidate.split("/");
    const name = segments[segments.length - 1];
    if (!hasMarkdownExtension(name)) continue;
    if (segments.slice(0, -1).some((segment) => IGNORE_DIRS.has(segment))) continue;
    const full = join(dir, candidate);
    const rel = toSlashes(relative(root, full));
    if (IGNORE_PATH_PREFIXES.some((p) => rel.startsWith(p))) continue;
    // git ls-files --cached still lists a tracked file deleted from the tree.
    if (!existsSync(full)) continue;
    if (lstatSync(full).isSymbolicLink()) links.push(full);
    else files.push(full);
  }
  const seen = new Set(files);
  for (const link of links) {
    const real = realpathSync(link);
    if (!seen.has(real)) {
      seen.add(real);
      files.push(link);
    }
  }
  return files;
};
