// Two blank-line house rules a formatter cannot express and a type checker
// does not care about: a blank line before a `return` that follows another
// statement, and a blank line after a run of variable declarations.
//
// A lint rule as a GocciaScript program. `goccia:ast` supplies the statement
// structure and the comments; GocciaRunner's sandbox mode supplies the files,
// over a virtual filesystem copied from explicit host paths. Output is one
// `path:line  rule` per finding, then a count.
//
// It also fixes what it finds, by writing the corrected file back into the
// sandbox filesystem — which is its own copy and reaches nothing. Whether any
// of that becomes a change to a real file is the host's decision and the
// host's command line: copying the sources with `--copy-rw` instead of `--copy`
// keeps the result, and otherwise the sandbox filesystem is discarded when the
// run ends. There is no flag here to ask for, and nothing this program can do
// to make one appear.
//
// Run:
//   ./build.pas runner
//   ./build/GocciaRunner examples/_experimental/blank-lines.mjs \
//     --experimental-ast --copy <your-source-dir>=/src
//
// Use --copy-rw in place of --copy to keep the fixes. Add the --compat-* flags
// the checked sources need: `parse` accepts the language the host is
// configured for, not a second one of its own.
//
// Written in the default profile — no `while`, no C-style `for` — so the only
// flags a run needs are the ones the checked sources need.

import fs from "fs";
import { parse } from "goccia:ast";

const ROOT = "/src";

const RULES = {
  blankAfterDeclarations: "a run of variable declarations needs a blank line after it",
  blankBeforeReturn: "a `return` that follows another statement needs a blank line before it",
};

// The kinds that answer "is this a variable statement?". TypeScript has one
// `VariableStatement` node; Goccia splits the same source form across a plain
// declaration, a destructuring declaration, `using`, and the exported
// spellings of each, so the rule names the whole family.
const VARIABLE_KINDS = [
  "VariableDeclaration",
  "DestructuringDeclaration",
  "UsingDeclaration",
  "ExportVariableDeclaration",
  "ExportDestructuringDeclaration",
];

// The kinds whose children are a sibling statement list.
const LIST_KINDS = ["Program", "BlockStatement", "SwitchCase"];

const isVariable = (node) => VARIABLE_KINDS.includes(node.kind);

const statementLists = (root) => {
  const lists = [];
  const walk = (node) => {
    if (LIST_KINDS.includes(node.kind)) {
      lists.push(node.children);
    }

    node.children.forEach(walk);
  };

  walk(root);

  return lists;
};

/** The blank line goes above an own-line comment attached to the statement, not below it. */
const anchorOf = (comments, prev, cur) => {
  const first = comments.findIndex((comment) => comment.start >= prev.end);

  if (first < 0) {
    return cur.loc.start.line;
  }

  const own = comments
    .slice(first)
    .find(
      (comment) =>
        comment.start < cur.start && comment.loc.start.line > prev.loc.end.line,
    );

  return own ? own.loc.start.line : cur.loc.start.line;
};

/** The offset each line starts at, so a line number can become an edit. */
const lineStartsOf = (source) =>
  source.split("\n").reduce(
    (starts, line) => {
      starts.push(starts[starts.length - 1] + line.length + 1);

      return starts;
    },
    [0],
  );

/**
 * Insert the missing blank lines. Back to front, so an earlier insertion does
 * not move a later offset. The blank line uses the file's own line ending, so
 * a CRLF file stays CRLF.
 */
const fixed = (source, offsets) => {
  const eol = source.includes("\r\n") ? "\r\n" : "\n";

  return offsets
    .slice()
    .sort((left, right) => right - left)
    .reduce(
      (text, offset) => `${text.slice(0, offset)}${eol}${text.slice(offset)}`,
      source,
    );
};

const findingsIn = (file, source) => {
  const { root, comments } = parse(source, {
    jsx: file.endsWith(".tsx") || file.endsWith(".jsx"),
    fileName: file,
  });
  const lineStarts = lineStartsOf(source);
  const found = [];

  statementLists(root).forEach((list) => {
    list.forEach((cur, index) => {
      if (index === 0) {
        return;
      }

      const prev = list[index - 1];
      const isReturn = cur.kind === "ReturnStatement";
      const afterRun = isVariable(prev) && !isVariable(cur) && !isReturn;

      if (!isReturn && !afterRun) {
        return;
      }

      const anchorLine = anchorOf(comments, prev, cur);

      // Same line: a blank line cannot be inserted without reflowing the code.
      if (anchorLine !== prev.loc.end.line + 1) {
        return;
      }

      found.push({
        file,
        line: anchorLine,
        offset: lineStarts[anchorLine - 1],
        rule: isReturn ? RULES.blankBeforeReturn : RULES.blankAfterDeclarations,
      });
    });
  });

  return found;
};

const sourceFiles = (dir) => {
  const found = [];

  fs.readdirSync(dir).forEach((name) => {
    const path = `${dir}/${name}`;

    if (fs.statSync(path).isDirectory()) {
      found.push(...sourceFiles(path));
    } else if (name.endsWith(".ts") || name.endsWith(".tsx")) {
      found.push(path);
    }
  });

  return found;
};

const files = sourceFiles(ROOT).sort();
const all = [];
const failures = [];
let rewritten = 0;

files.forEach((file) => {
  try {
    const source = fs.readFileSync(file, "utf8");
    const found = findingsIn(file, source);

    if (found.length === 0) {
      return;
    }

    all.push(...found);
    fs.writeFileSync(file, fixed(source, found.map((finding) => finding.offset)));
    rewritten = rewritten + 1;
  } catch (error) {
    failures.push(`${file}: ${error.message}`);
  }
});

all.forEach((finding) => {
  console.log(`${finding.file}:${finding.line}  ${finding.rule}`);
});

failures.forEach((failure) => {
  console.log(`PARSE FAILURE ${failure}`);
});

console.log(
  `${all.length} finding(s) in ${files.length} file(s), ${rewritten} rewritten`,
);
