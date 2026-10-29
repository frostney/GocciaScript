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
//   4. Re-parsing the text the range selects yields a node of the same
//      kind covering exactly that text. This is the one that catches an
//      offset that is merely plausible: a range one character wide of the
//      truth still passes 1-3 and fails here.
//
// Each file is checked twice: once as the statement tree `parse` returns by
// default, and once with `expressions: true`, where every expression,
// object property and parameter is a node as well.
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
  "ExportTypeAliasDeclaration",
  "ExportInterfaceDeclaration",
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

// Expression-level kinds that a re-parse cannot place on their own: the
// patterns, which mean something else outside a binding or assignment target.
const UNREPARSABLE_EXPRESSION_KINDS = [
  "ArrayPattern",
  "ObjectPattern",
  "RestElement",
  "AssignmentPattern",
];

// Expression-level kinds, and the text to re-parse each one inside. An
// expression goes in parentheses inside an async generator method, which
// accepts `await`, `yield`, `new.target` and `super.x`; `super()` needs a
// derived constructor.
const EXPRESSION_PREFIX = "class __Probe extends Object {\nasync *probe() {\n(";
const EXPRESSION_SUFFIX = ");\n}\n}\n";
const SUPER_PREFIX = "class __Probe extends Object {\nconstructor() {\n(";
const SUPER_SUFFIX = ");\n}\n}\n";

const CLASS_MEMBER_KINDS = [
  "MethodDefinition",
  "PropertyDefinition",
  "AccessorProperty",
  "StaticBlock",
];

// Kinds that are a piece of an import or export declaration: each is
// re-parsed inside the smallest declaration that holds one.
const SPECIFIER_WRAPPERS = {
  ImportDefaultSpecifier: { prefix: "import ", suffix: ' from "m";\n' },
  ImportNamespaceSpecifier: { prefix: "import ", suffix: ' from "m";\n' },
  ImportSpecifier: { prefix: "import {", suffix: '} from "m";\n' },
  ExportSpecifier: { prefix: "export {", suffix: "};\n" },
};

// A static key is a name or a literal, and so is an expression, except where
// the name is a reserved word or a private name. It is re-parsed as an
// expression first, and as the key of a class member if that does not place
// it: a class member accepts every spelling a key can have.
const KEY_KINDS = ["Identifier", "StringLiteral", "NumericLiteral", "BigIntLiteral"];
const KEY_WRAPPER = { prefix: "class __Probe {\n", suffix: "() {}\n}\n" };

const expressionWrapperFor = (kind, slice) => {
  if (SPECIFIER_WRAPPERS[kind]) {
    return SPECIFIER_WRAPPERS[kind];
  }

  if (kind === "Property") {
    return { prefix: `${EXPRESSION_PREFIX}{`, suffix: `}${EXPRESSION_SUFFIX}` };
  }

  if (kind === "SpreadElement") {
    return { prefix: `${EXPRESSION_PREFIX}[`, suffix: `]${EXPRESSION_SUFFIX}` };
  }

  if (kind === "Parameter") {
    return { prefix: "class __Probe {\nprobe(", suffix: ") {}\n}\n" };
  }

  if (CLASS_MEMBER_KINDS.includes(kind)) {
    return { prefix: "class __Probe {\n", suffix: "\n}\n" };
  }

  // In parentheses of its own, the outer pair would be the one reported.
  if (kind === "ParenthesizedExpression") {
    return { prefix: EXPRESSION_PREFIX.slice(0, -1), suffix: `;${EXPRESSION_SUFFIX.slice(2)}` };
  }

  if (slice.startsWith("super(")) {
    return { prefix: SUPER_PREFIX, suffix: SUPER_SUFFIX };
  }

  return { prefix: EXPRESSION_PREFIX, suffix: EXPRESSION_SUFFIX };
};

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

// A statement kind, as opposed to one the expression view adds.
const isStatementKind = (kind) =>
  kind === "Program" ||
  kind === "SwitchCase" ||
  kind === "Statement" ||
  kind.endsWith("Statement") ||
  kind.endsWith("Declaration");

const wrapperFor = (kind, slice) => {
  if (!isStatementKind(kind)) {
    return expressionWrapperFor(kind, slice);
  }

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
const roundTrip = (file, text, node, expressions) => {
  const slice = text.slice(node.start, node.end);
  const wrappers = [wrapperFor(node.kind, slice)];

  if (expressions && KEY_KINDS.includes(node.kind)) {
    wrappers.push(KEY_WRAPPER);
  }

  let failure = null;

  for (const wrapper of wrappers) {
    const wrapped = wrapper.prefix + slice + wrapper.suffix;
    const start = wrapper.prefix.length;
    const end = start + slice.length;
    let reparsed = null;

    try {
      reparsed = parse(wrapped, { jsx: isJsxPath(file), fileName: file, expressions });
    } catch (error) {
      failure = `re-parsing the range's text failed: ${error.message}`;
      continue;
    }

    const match = nodesOf(reparsed.root).find(
      (candidate) =>
        candidate.kind === node.kind &&
        candidate.start === start &&
        candidate.end === end,
    );

    if (match) {
      return null;
    }

    failure = "the range's text does not re-parse to the same node";
  }

  return failure;
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

const checkFile = (file, counts, expressions) => {
  const text = fs.readFileSync(file, "utf8");
  const problems = [];
  let result = null;

  try {
    result = parse(text, { jsx: isJsxPath(file), fileName: file, expressions });
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

    if (
      UNREPARSABLE_KINDS.includes(node.kind) ||
      UNREPARSABLE_EXPRESSION_KINDS.includes(node.kind)
    ) {
      counts.skipped = counts.skipped + 1;
    } else {
      const failure = roundTrip(file, text, node, expressions);

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
const extensions = [".ts", ".tsx"];

[false, true].forEach((expressions) => {
  const counts = { nodes: 0, roundTripped: 0, skipped: 0 };
  const problems = [];
  const view = expressions ? "expressions" : "statements";

  files.forEach((file) => {
    problems.push(...checkFile(file, counts, expressions));
  });

  problems.forEach((problem) => {
    console.log(`${view}  ${problem}`);
  });

  extensions.forEach((extension) => {
    const matching = files.filter((file) => extensionOf(file) === extension);

    console.log(
      `${view}  ${extension}: ${counts[extension] ?? 0} node(s) in ${matching.length} file(s)`,
    );
  });

  console.log(
    `${view}  ${counts.nodes} node(s) in ${files.length} file(s): ${counts.roundTripped} round-tripped, ${counts.skipped} not re-parsable on their own, ${problems.length} problem(s)`,
  );
});
