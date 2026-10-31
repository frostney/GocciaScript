// Three expression-level style rules, enforced from `goccia:ast`'s expression
// view (`parse(text, { expressions: true })`):
//
//   1. A string literal is written in single quotes, when that needs no
//      change to its escaping: `"a"` becomes `'a'`, but `"it's"` and
//      `"say \"hi\""` stay as they are.
//   2. A multi-line array, object, argument list or parameter list ends with
//      a trailing comma, except after a rest parameter or a setter's
//      parameter, where one is a syntax error.
//   3. A call that sits on one line longer than WIDTH puts its arguments one
//      per line, indented one step past the line it starts on, with a
//      trailing comma. A call inside a template literal's `${}` is left
//      alone: the long line is the template's, and it is shortened by
//      changing the template, not by spreading a call across its text.
//
// Each rule dispatches on the tree: a node's `kind`, its flags (`rest` on a
// parameter), and its range. What the tree does not hold — the text of a
// literal, the comma after the last element — the rule reads out of
// `source` between two offsets, which is what the ranges are for.
//
// Output is one `path:line:column  rule` per finding, then a count. Each
// fixed file is written back into the sandbox filesystem, which is GocciaRunner's
// own copy: copying the sources with `--copy-rw` instead of `--copy` keeps
// the result, and otherwise it is discarded when the run ends.
//
// Run:
//   ./build.pas runner
//   ./build/GocciaRunner examples/_experimental/expression-style.mjs \
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
const WIDTH = 80;
const INDENT = "  ";

// A fix can make another rule apply (a wrapped call is now a multi-line
// argument list) or can overlap another fix, which then waits for the next
// pass, and each pass wraps one more level of nested calls. Fixing repeats
// until a pass finds nothing. The cap only stops rules that undo each other:
// a file that reaches it is reported and left as it was.
const MAX_PASSES = 100;

const RULES = {
  singleQuotes: "a string literal should use single quotes",
  trailingComma: "a multi-line list should end with a trailing comma",
  wrapArguments: `a call on a line over ${WIDTH} columns should put one argument per line`,
};

const FUNCTION_KINDS = [
  "ArrowFunctionExpression",
  "FunctionExpression",
  "FunctionDeclaration",
  "MethodDefinition",
  "Property",
];

const CALL_KINDS = ["CallExpression", "NewExpression"];

const isJsxPath = (path) => path.endsWith(".tsx") || path.endsWith(".jsx");

const nodesOf = (root) => {
  const all = [];
  const walk = (node) => {
    all.push(node);
    node.children.forEach(walk);
  };

  walk(root);

  return all;
};

/** The offset each line starts at. */
const lineStartsOf = (source) =>
  source.split("\n").reduce(
    (starts, line) => {
      starts.push(starts[starts.length - 1] + line.length + 1);

      return starts;
    },
    [0],
  );

const lineText = (source, lineStarts, line) =>
  source.slice(lineStarts[line - 1], lineStarts[line] - 1);

const WHITESPACE = /[ \t\r\n]*/y;

/**
 * The first character at or after `offset` that is not whitespace or inside
 * a comment, and where it is. Comments come from the parse, so a `//` inside
 * a string is never mistaken for one; `commentEnds` maps where each comment
 * starts to where it ends.
 */
const nextSignificant = (source, commentEnds, offset) => {
  WHITESPACE.lastIndex = offset;
  WHITESPACE.exec(source);
  const at = WHITESPACE.lastIndex;

  if (commentEnds.has(at)) {
    return nextSignificant(source, commentEnds, commentEnds.get(at));
  }

  return { char: at < source.length ? source[at] : "", offset: at };
};

// -- Rule 1: single quotes -----------------------------------------------------

const quoteEdits = (source, nodes) =>
  nodes
    .filter((node) => node.kind === "StringLiteral" && source[node.start] === '"')
    .filter((node) => {
      const body = source.slice(node.start + 1, node.end - 1);

      return !body.includes("'") && !body.includes('\\"');
    })
    .map((node) => ({
      node,
      rule: RULES.singleQuotes,
      start: node.start,
      end: node.end,
      text: `'${source.slice(node.start + 1, node.end - 1)}'`,
    }));

// -- Rule 2: trailing commas -----------------------------------------------------

// `set` as a whole word, followed by the start of a key rather than by the
// `(` of a method named `set`: a name after a space, or a `[`, `#` or quote.
const SETTER_HEAD = /(?:^|[^\w$.@#'"`])set(?:\s+[\w$#'"[]|[#'"[])/;

/**
 * Whether an object property or class member is a setter. The view carries
 * no accessor flag, so this reads the text from the member's start (its
 * decorators and modifiers included) up to its first parameter.
 */
const isSetter = (source, node, firstParameter) =>
  (node.kind === "Property" || node.kind === "MethodDefinition") &&
  SETTER_HEAD.test(source.slice(node.start, firstParameter.start));

/**
 * The lists a node owns, as { items, close }: the elements in order and the
 * offset of the delimiter that closes them. Null when the node has no list
 * or the list cannot take a comma.
 */
const listOf = (source, commentEnds, node) => {
  if (node.kind === "ArrayExpression" || node.kind === "ObjectExpression") {
    return { items: node.children, close: node.end - 1 };
  }

  if (CALL_KINDS.includes(node.kind) && source[node.end - 1] === ")") {
    // The callee is the first child; the arguments are the rest.
    return { items: node.children.slice(1), close: node.end - 1 };
  }

  if (FUNCTION_KINDS.includes(node.kind)) {
    const items = node.children.filter((child) => child.kind === "Parameter");

    if (items.length === 0) {
      return null;
    }

    // Past the last parameter come its `,` if any, then the `)`. A lone
    // arrow parameter written without parentheses has neither.
    const last = items[items.length - 1];
    const after = nextSignificant(source, commentEnds, last.end);
    const close =
      after.char === ","
        ? nextSignificant(source, commentEnds, after.offset + 1)
        : after;

    if (close.char !== ")") {
      return null;
    }

    // A setter takes exactly one parameter and no comma after it.
    return isSetter(source, node, items[0])
      ? null
      : { items, close: close.offset };
  }

  return null;
};

const commaEdits = (source, commentEnds, nodes) =>
  nodes.flatMap((node) => {
    const list = listOf(source, commentEnds, node);

    if (!list || list.items.length === 0) {
      return [];
    }

    const last = list.items[list.items.length - 1];

    if (last.kind === "Parameter" && last.rest) {
      return [];
    }

    const after = nextSignificant(source, commentEnds, last.end);

    if (after.char === ",") {
      return [];
    }

    // Multi-line means the closing delimiter is not on the last item's line.
    if (!source.slice(last.end, list.close).includes("\n")) {
      return [];
    }

    return [
      {
        node: last,
        rule: RULES.trailingComma,
        start: last.end,
        end: last.end,
        text: ",",
      },
    ];
  });

// -- Rule 3: wrap long calls --------------------------------------------------------

const indentOf = (line) => line.slice(0, line.length - line.trimStart().length);

const wrapEdits = (source, comments, lineStarts, nodes) => {
  // A comment among the arguments has no node to carry it to its new line,
  // so a call holding one is left for a person to wrap.
  const holdsComment = (call) =>
    comments.some(
      (comment) => comment.start >= call.children[1].start && comment.end <= call.end,
    );
  // The only expressions inside a template literal are its substitutions.
  const templates = nodes.filter((node) => node.kind === "TemplateLiteral");
  const inTemplate = (call) =>
    templates.some(
      (template) => template.start < call.start && call.end < template.end,
    );
  const calls = nodes.filter(
    (node) =>
      CALL_KINDS.includes(node.kind) &&
      node.children.length > 1 &&
      source[node.end - 1] === ")" &&
      node.loc.start.line === node.loc.end.line &&
      lineText(source, lineStarts, node.loc.start.line).length > WIDTH &&
      !holdsComment(node) &&
      !inTemplate(node),
  );

  // Only the outermost call on a line: wrapping it shortens the line, and a
  // call nested inside it is an argument it already moved.
  const outermost = calls.filter(
    (call) =>
      !calls.some(
        (other) =>
          other !== call && other.start <= call.start && call.end <= other.end,
      ),
  );

  return outermost.map((call) => {
    const args = call.children.slice(1);
    const indent = indentOf(lineText(source, lineStarts, call.loc.start.line));
    const inner = `${indent}${INDENT}`;
    const open = args[0].start;
    const body = args
      .map((arg) => `${inner}${source.slice(arg.start, arg.end)},`)
      .join("\n");

    return {
      node: call,
      rule: RULES.wrapArguments,
      start: open,
      end: call.end - 1,
      text: `\n${body}\n${indent}`,
    };
  });
};

// -- Applying -------------------------------------------------------------------

/** Keeps the edits that do not overlap one already kept, in source order. */
const disjoint = (edits) =>
  edits
    .slice()
    .sort((left, right) => left.start - right.start || left.end - right.end)
    .reduce((kept, edit) => {
      const previous = kept[kept.length - 1];

      if (previous && edit.start < previous.end) {
        return kept;
      }

      kept.push(edit);

      return kept;
    }, []);

/** Back to front, so an earlier edit does not move a later offset. */
const applied = (source, edits) =>
  edits
    .slice()
    .sort((left, right) => right.start - left.start)
    .reduce(
      (text, edit) => `${text.slice(0, edit.start)}${edit.text}${text.slice(edit.end)}`,
      source,
    );

const editsIn = (file, source) => {
  const { root, comments } = parse(source, {
    jsx: isJsxPath(file),
    fileName: file,
    expressions: true,
  });
  const commentEnds = new Map(comments.map((comment) => [comment.start, comment.end]));
  const nodes = nodesOf(root);
  const lineStarts = lineStartsOf(source);

  return [
    ...wrapEdits(source, comments, lineStarts, nodes),
    ...quoteEdits(source, nodes),
    ...commaEdits(source, commentEnds, nodes),
  ];
};

/**
 * Fixes a file to a fixed point. Findings are what the first pass saw, which
 * is what the file as given breaks; the later passes only finish the fixing.
 */
const fixFile = (file, source) => {
  const findings = editsIn(file, source);

  const settle = (text, edits, pass) => {
    if (edits.length === 0) {
      return text;
    }

    if (pass > MAX_PASSES) {
      throw new Error(`fixes did not settle after ${MAX_PASSES} passes`);
    }

    const next = applied(text, disjoint(edits));

    return settle(next, editsIn(file, next), pass + 1);
  };

  return { findings, fixed: settle(source, findings, 1) };
};

const sourceFiles = (dir) => {
  const found = [];

  fs.readdirSync(dir).forEach((name) => {
    const path = `${dir}/${name}`;

    if (fs.statSync(path).isDirectory()) {
      found.push(...sourceFiles(path));
    } else if (/\.(js|mjs|jsx|ts|tsx)$/.test(name)) {
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
    const { findings, fixed } = fixFile(file, source);

    findings
      .slice()
      .sort((left, right) => left.node.start - right.node.start)
      .forEach((finding) => {
        all.push(
          `${file}:${finding.node.loc.start.line}:${finding.node.loc.start.column}  ${finding.rule}`,
        );
      });

    if (fixed !== source) {
      fs.writeFileSync(file, fixed);
      rewritten = rewritten + 1;
    }
  } catch (error) {
    failures.push(`${file}: ${error.message}`);
  }
});

all.forEach((line) => {
  console.log(line);
});

failures.forEach((failure) => {
  console.log(`FAILURE ${failure}`);
});

console.log(
  `${all.length} finding(s) in ${files.length} file(s), ${rewritten} rewritten`,
);
