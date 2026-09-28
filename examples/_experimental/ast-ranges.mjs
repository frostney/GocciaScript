// Does a node's range really select the node's text in the file the caller
// passed? For a `.ts` file that is trivially true — nothing rewrote the text.
// For a `.tsx` file the parser never saw the file: the JSX transformer did,
// and the parser measured the transform. This checks the mapping back.
//
// Four claims, per node, over a whole tree of sources:
//
//   1. The range is inside the source and not inverted.
//   2. Children nest inside their parent, and siblings do not overlap.
//   3. `loc` is the line and column of `start` and `end` in that same
//      source — one coordinate system, not two that agree by luck.
//   4. Re-parsing the text the range selects yields a statement of the same
//      kind covering exactly that text. This is the one that catches an
//      offset that is merely plausible: a range one character wide of the
//      truth still passes 1-3 and fails here.
//
// Run:
//   ./build.pas runner
//   ./build/GocciaRunner examples/_experimental/ast-ranges.mjs \
//     --experimental-ast --copy <your-source-dir>=/src
//
// Written in the default profile, so the only flags a run needs are the ones
// the checked sources need.

import fs from "fs";
import { parse } from "goccia:ast";

const ROOT = "/src";

// Kinds that only parse at the top level of a module, so their round trip is
// the slice on its own.
const TOP_LEVEL_KINDS = [
  "ImportDeclaration",
  "ExportDeclaration",
  "ExportDefaultDeclaration",
  "ExportVariableDeclaration",
  "ExportDestructuringDeclaration",
  "ExportFunctionDeclaration",
  "ExportClassDeclaration",
  "ExportEnumDeclaration",
  "ReExportDeclaration",
];

// Kinds that only parse inside a loop.
const LOOP_KINDS = ["BreakStatement", "ContinueStatement"];

// The one kind with nothing to re-parse: the whole file is already the thing
// being parsed.
const UNREPARSABLE_KINDS = ["Program"];

const FUNCTION_PREFIX = "const __probe = async () => {\n";
const FUNCTION_SUFFIX = "\n};\n";
const LOOP_PREFIX = `${FUNCTION_PREFIX}for (const __item of []) {\n`;
const LOOP_SUFFIX = `\n}${FUNCTION_SUFFIX}`;
const SWITCH_PREFIX = `${FUNCTION_PREFIX}switch (0) {\n`;
const SWITCH_SUFFIX = `\n}${FUNCTION_SUFFIX}`;

const isJsxPath = (path) => path.endsWith(".tsx") || path.endsWith(".jsx");

/** Line and column of an offset, counted the way the engine counts them. */
const positionAt = (text, offset) => {
  const before = text.slice(0, offset);
  const lastBreak = before.lastIndexOf("\n");

  return {
    line: before.split("\n").length,
    column: offset - lastBreak,
  };
};

const nodesOf = (root) => {
  const all = [];
  const walk = (node) => {
    all.push(node);
    node.children.forEach(walk);
  };

  walk(root);

  return all;
};

const wrapperFor = (kind) => {
  if (TOP_LEVEL_KINDS.includes(kind)) {
    return { prefix: "", suffix: "" };
  }

  if (LOOP_KINDS.includes(kind)) {
    return { prefix: LOOP_PREFIX, suffix: LOOP_SUFFIX };
  }

  if (kind === "SwitchCase") {
    return { prefix: SWITCH_PREFIX, suffix: SWITCH_SUFFIX };
  }

  return { prefix: FUNCTION_PREFIX, suffix: FUNCTION_SUFFIX };
};

/**
 * The range says the node covers this text. Parse that text on its own and
 * see whether the same node comes back out of it, at the same width.
 */
const roundTrip = (file, text, node) => {
  const slice = text.slice(node.start, node.end);
  const wrapper = wrapperFor(node.kind);
  const wrapped = wrapper.prefix + slice + wrapper.suffix;
  const start = wrapper.prefix.length;
  const end = start + slice.length;
  let reparsed = null;

  try {
    reparsed = parse(wrapped, { jsx: isJsxPath(file), fileName: file });
  } catch (error) {
    return `re-parsing the range's text failed: ${error.message}`;
  }

  const match = nodesOf(reparsed.root).find(
    (candidate) =>
      candidate.kind === node.kind &&
      candidate.start === start &&
      candidate.end === end,
  );

  return match ? null : "the range's text does not re-parse to the same node";
};

const checkNode = (file, text, node, parent) => {
  const problems = [];
  const where = `${file}:${node.loc.start.line}:${node.loc.start.column} ${node.kind}`;

  if (node.start < 0 || node.end > text.length || node.start > node.end) {
    problems.push(`${where}  range [${node.start},${node.end}] is not inside the source`);

    return problems;
  }

  if (parent && (node.start < parent.start || node.end > parent.end)) {
    problems.push(`${where}  range escapes its ${parent.kind} parent`);
  }

  const start = positionAt(text, node.start);
  const end = positionAt(text, node.end);

  if (start.line !== node.loc.start.line || start.column !== node.loc.start.column) {
    problems.push(
      `${where}  loc.start ${node.loc.start.line}:${node.loc.start.column} is not offset ${node.start} (${start.line}:${start.column})`,
    );
  }

  if (end.line !== node.loc.end.line || end.column !== node.loc.end.column) {
    problems.push(
      `${where}  loc.end ${node.loc.end.line}:${node.loc.end.column} is not offset ${node.end} (${end.line}:${end.column})`,
    );
  }

  node.children.forEach((child, index) => {
    if (index === 0) {
      return;
    }

    if (child.start < node.children[index - 1].end) {
      problems.push(`${where}  child ${index} overlaps the one before it`);
    }
  });

  return problems;
};

const extensionOf = (file) => file.slice(file.lastIndexOf("."));

const checkFile = (file, counts) => {
  const text = fs.readFileSync(file, "utf8");
  const problems = [];
  let result = null;

  try {
    result = parse(text, { jsx: isJsxPath(file), fileName: file });
  } catch (error) {
    return [`${file}  parse failed: ${error.message}`];
  }

  if (result.source !== text) {
    problems.push(`${file}  the returned source is not the text that was passed`);
  }

  const extension = extensionOf(file);
  const visit = (node, parent) => {
    counts.nodes = counts.nodes + 1;
    counts[extension] = (counts[extension] ?? 0) + 1;
    problems.push(...checkNode(file, text, node, parent));

    if (UNREPARSABLE_KINDS.includes(node.kind)) {
      counts.skipped = counts.skipped + 1;
    } else {
      const failure = roundTrip(file, text, node);

      if (failure) {
        problems.push(`${file}:${node.loc.start.line} ${node.kind}  ${failure}`);
      } else {
        counts.roundTripped = counts.roundTripped + 1;
      }
    }

    node.children.forEach((child) => visit(child, node));
  };

  visit(result.root, null);

  return problems;
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
const counts = { nodes: 0, roundTripped: 0, skipped: 0 };
const extensions = [".ts", ".tsx"];
const problems = [];

files.forEach((file) => {
  problems.push(...checkFile(file, counts));
});

problems.forEach((problem) => {
  console.log(problem);
});

extensions.forEach((extension) => {
  const matching = files.filter((file) => extensionOf(file) === extension);

  console.log(
    `${extension}: ${counts[extension] ?? 0} node(s) in ${matching.length} file(s)`,
  );
});

console.log(
  `${counts.nodes} node(s) in ${files.length} file(s): ${counts.roundTripped} round-tripped, ${counts.skipped} not re-parsable on their own, ${problems.length} problem(s)`,
);
