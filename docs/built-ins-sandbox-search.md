# Sandbox Search Built-ins

*Searching the sandbox virtual filesystem by name (`fs.globSync`), by content (`grep`) and by fuzzy name (`fuzzyFind`).*

## Executive Summary

- **Sandbox mode only** — `fs.globSync`, `fs.glob` and `fs.promises.glob` extend the sandbox `"fs"` module; `grep` and `fuzzyFind` are named exports of the `goccia:search` module, which only sandbox mode installs.
- **One walk, natively** — The search walks the virtual filesystem once in native code and reads each file once; it never changes a timestamp.
- **Node-shaped names** — `fs.globSync` follows Node's `fs.globSync` for the subset below: results relative to `cwd`, sorted, with directories included.
- **The engine's own RegExp** — `grep` matches with the same bounded RegExp engine scripts use, so a pattern behaves as it does in `String.prototype.match`.
- **Shared test vectors** — `fixtures/sandbox-search/vectors.json` pins every rule on this page; an embedder that searches the same files outside the sandbox can hold its results to the same corpus.

## Names: `fs.globSync`

```javascript
import fs from "fs";

fs.globSync("src/**/*.ts");                      // ["src/index.ts", "src/user/model.ts"]
fs.globSync(["*.md", "docs/*.md"], { cwd: "/" });
fs.globSync("**/*.ts", { exclude: ["**/*.test.ts"] });
```

| Form | Result |
|------|--------|
| `fs.globSync(pattern, options?)` | An array of paths |
| `fs.glob(pattern, options?, callback)` | Calls `callback(null, paths)`, or `callback(error)` |
| `fs.promises.glob(pattern, options?)` | An async iterator over the paths, as Node's |

`pattern` is a string or an array of strings. The options are `cwd` (a sandbox path, default the sandbox's working directory) and `exclude` (an array of patterns). Any other option is a `TypeError`.

Results are deduplicated and sorted in UTF-16 code unit order, JavaScript's default sort. A relative pattern's results are relative to `cwd`; an absolute pattern's are absolute and ignore `cwd`. A `cwd` that does not exist gives an empty array. Files and directories both match; `cwd` itself is never a result, so `**` lists everything below it (Node also lists `"."`).

### Pattern syntax

Matching is case-sensitive and works on UTF-16 code units, as Node's does.

| Syntax | Matches |
|--------|---------|
| `*` | Any run of characters within one path segment |
| `?` | One character (one UTF-16 code unit) within a segment |
| `[abc]`, `[a-z]` | One character from the class; `[!…]` or `[^…]` negates it. A `]` right after the opening bracket (or its `!`/`^`) is a member, and a `[` with no closing `]` is literal |
| `**` | As a whole segment, zero or more directories; inside a segment it is `*` |
| `{a,b}` | Each alternative in turn; braces nest, and a brace with no comma (`{a}`) is literal |
| `/` at the end | Directories only |
| `/` at the start | An absolute pattern |
| `./` at the start | Ignored |

A backslash is a path separator, as everywhere in the sandbox filesystem, so there are no escapes: match a literal `*` with `[*]`. Empty segments (`a//b`) and `.` segments are ignored. A `..` segment is a `TypeError`.

**Hidden entries.** A name that starts with `.` is matched only by a pattern segment that starts with a literal `.`: `*` and `?` never match the leading dot, and `**` does not enter a hidden directory. `**/.*` finds the hidden entries, and `.github/**` searches inside one.

**Exclude.** An entry is left out when its path, or the path of any directory above it, matches an `exclude` pattern. Exclude patterns follow the same rules and are relative to `cwd` (absolute ones to the root): `exclude: ["src/user"]` removes the directory and everything below it, and `exclude: ["*.ts"]` removes only `.ts` files directly in `cwd`.

### Differences from Node

Extended globs (`+(a|b)`, `!(x)`, `@(…)`), POSIX classes (`[[:alpha:]]`), numeric brace ranges (`{1..3}`) and `..` segments are not supported; the first three are literal text. `exclude` takes an array of patterns, not a function, and there is no `withFileTypes`.

## Content: `grep`

```javascript
import { grep } from "goccia:search";

const { matches, truncated } = grep("TODO", { glob: "src/**/*.ts", context: 1 });
// matches: [{ path: "/src/user/controller.ts", line: 5, column: 6, text: "  // TODO: paginate",
//             before: ["…"], after: ["…"] }]
```

`grep(pattern, options?)` searches the files below `cwd`, in path order, and returns `{ matches, truncated }`.

| Option | Default | Meaning |
|--------|---------|---------|
| `regex` | `false` | `pattern` is a regular expression source; otherwise it is literal text |
| `ignoreCase` | `false` | Match with the `i` flag |
| `cwd` | the working directory | Search the files below this directory |
| `glob` | every file | Only files whose path matches this pattern or array of patterns (relative to `cwd`, with the hidden-entry rule) |
| `exclude` | none | Leave out files as `fs.globSync` does |
| `context` | `0` | Lines of context to return before and after each match |
| `maxResults` | `1000` | Stop after this many matches |
| `maxBytes` | no limit | Stop before the file that would take the bytes read past this |

`pattern` must be a non-empty string. With `regex: true` it compiles as `new RegExp(pattern, ignoreCase ? "i" : "")` would, so an invalid source is a `SyntaxError`; literal text is matched as an escaped pattern with the same flags. Without `glob`, hidden files are searched too.

Each file is decoded as UTF-8. A file that contains a NUL byte or is not valid UTF-8 is skipped. A byte order mark at the start is dropped, lines split at `\n`, and a `\r` before the `\n` is not part of the line. A final `\n` does not start another line. The pattern is matched against each line on its own, so `^` and `$` are the line's start and end.

A line with a match gives one result, at its first match: `path` (absolute), `line` (1-based), `column` (1-based, in UTF-16 code units, as a string index plus one) and `text` (the whole line). With `context` above zero every result also has `before` and `after`, arrays of up to that many neighbouring lines, shorter at the edges of a file.

`truncated` is `true` when the search stopped at `maxResults` with another match left, or at `maxBytes` with another file left to read; it never returns fewer results silently. A binary or non-UTF-8 file counts towards `maxBytes`.

The RegExp step limit, the run's timeout and its instruction limit bound the work; a pattern that exceeds the step limit throws as it would in `RegExp.prototype.exec`. The decoded text of a file is charged to the run's memory limit while it is searched.

## Fuzzy names: `fuzzyFind`

```javascript
import { fuzzyFind } from "goccia:search";

fuzzyFind("usrctrl");
// { matches: [{ path: "/src/user/controller.ts", score: 139, positions: [5, 6, 8, 10, 13, 14, 16] }, …],
//   truncated: false }
```

`fuzzyFind(query, options?)` ranks the files below `cwd` whose absolute path contains the query's characters in order, and returns `{ matches, truncated }`. The options are `cwd`, `glob` and `exclude`, as for `grep`, and `limit` (default `100`). `query` must be a non-empty string.

A match has the absolute `path`, its `score` and the `positions` of the matched characters in `path` (UTF-16 code unit indexes). Matches are sorted by score (highest first), then by path length (shortest first), then by path. `truncated` is `true` when more files matched than `limit`.

### Scoring

The score follows fzf's version 1 algorithm with its path scheme, over UTF-16 code units, with ASCII-only case folding (`A`-`Z` match `a`-`z`; nothing else folds):

1. Scan forwards for the first position where every query character has been seen in order. No such position: no match.
2. From there, scan backwards for the latest start that still holds the whole query.
3. Walk that range forwards, matching query characters greedily. A matched character scores `16` plus a bonus; an unmatched one costs `3` when it opens a gap and `1` when it extends one.

The bonus depends on the character's class and the class of the character before it (before the first character of the path, a delimiter). The classes are: whitespace (space, `\t` to `\r`); the delimiter `/`; lower case `a`-`z`; upper case `A`-`Z`; digits; letters (any code unit from `0x80` up); and non-word (every other ASCII character).

| Character | After | Bonus |
|-----------|-------|-------|
| Any class except whitespace and non-word | whitespace | `8` |
| Any class except whitespace and non-word | the delimiter | `9` |
| Any class except whitespace and non-word | non-word | `8` |
| Upper case | lower case | `7` |
| A digit | anything but a digit | `7` |
| Non-word or the delimiter | anything else | `8` |
| Whitespace | anything else | `8` |
| Anything else | | `0` |

The rows are tried in order. The first matched character's bonus counts twice. Inside a run of consecutive matches each bonus is raised to at least `4` and to at least the bonus of the run's first character; a boundary bonus (`8` or more) that beats the run's first bonus becomes the new first bonus.

## Errors

A pattern, query or option of the wrong type is a `TypeError`, as is a `..` glob segment, an empty `grep` pattern or `fuzzyFind` query, and a `maxResults`, `maxBytes`, `limit` or `context` that is not a non-negative integer (`maxResults` and `limit` at least `1`). An invalid regular expression is a `SyntaxError`. `fs.glob` and `fs.promises.glob` check their arguments and patterns before any work and throw these synchronously, as `fs.globSync` does. A glob whose braces expand to more than 1024 alternatives is a `TypeError`.

## Shared test vectors

`fixtures/sandbox-search/vectors.json` holds trees (files as UTF-8 text or `{ base64 }` bytes; directories are the files' parents) and cases for `glob`, `fuzzyFind`, `grep` and the errors above, each with its exact expected result. `scripts/test-cli-sandbox-search.ts` runs every case through `GocciaRunner` in sandbox mode. The glob cases agree with Node's `fs.globSync` except where this page says otherwise (the `cwd` itself, backslashes).
