// Two blank-line house rules a formatter cannot express and a type checker
// does not care about: a blank line before a `return` that follows another
// statement, and a blank line after a run of variable declarations.
//
// A lint rule as a GocciaScript program. `goccia:ast` supplies the statement
// structure and the comments; GocciaSandboxRunner supplies the files, over a
// virtual filesystem seeded from explicit host paths. Output is one
// `path:line  rule` per finding, then a count.
//
// Run:
//   ./build.pas sandboxrunner
//   ./build/GocciaSandboxRunner /blank-lines.mjs --experimental-ast \
//     --seed examples/_experimental/blank-lines.mjs=/blank-lines.mjs \
//     --seed <your-source-dir>=/src
//
// Add the --compat-* flags the checked sources need: `parse` accepts the
// language the host is configured for, not a second one of its own.
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

const findingsIn = (file, source) => {
  const { root, comments } = parse(source, {
    jsx: file.endsWith(".tsx") || file.endsWith(".jsx"),
    fileName: file,
  });
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

files.forEach((file) => {
  try {
    all.push(...findingsIn(file, fs.readFileSync(file, "utf8")));
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

console.log(`${all.length} finding(s) in ${files.length} file(s)`);
