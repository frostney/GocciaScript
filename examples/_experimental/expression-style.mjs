// Three expression-level style rules, enforced from `goccia:ast`'s expression
// view (`parse(text, { expressions: true })`):
//
//   1. A string literal is written in single quotes unless its text holds
//      more single quotes than double ones, escaping only the quote that
//      delimits it: `"a"` becomes `'a'` and `"say \"hi\""` becomes
//      `'say "hi"'`, but `"it's"` stays as it is.
//   2. A multi-line array, object, argument list, parameter list (of a
//      function, an exported function, a function type or a method or call
//      signature), destructuring pattern, braced import or export specifier
//      list, tuple type or type parameter list ends with a trailing comma,
//      except after a rest parameter or element or a setter's parameter, where
//      one is a syntax error, and after an array pattern's trailing hole. A type
//      argument list (`Map<K, V>`) is left alone, as Biome and Prettier leave
//      it.
//   3. A line longer than WIDTH is laid out again the way Biome and Prettier
//      lay it out: the outermost expression or statement on it is broken at
//      its outermost group that does not fit — the arguments of a call, the
//      body of an arrow, a logical chain, a member chain, an array, object,
//      type argument list, type literal or union — and inner groups only
//      where they still do not fit. A node holding a comment, one inside a template
//      literal's `${}`, the head of a call or member access that goes on past
//      its line, a declaration's type annotation and a function's return type
//      are left alone, as Biome leaves them.
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
  layout: `a line over ${WIDTH} columns should be broken the way Biome breaks it`,
};

const SPECIFIER_KINDS = ["ImportSpecifier", "ExportSpecifier"];

const FUNCTION_KINDS = [
  "ArrowFunctionExpression",
  "FunctionExpression",
  "FunctionDeclaration",
  "ExportFunctionDeclaration",
  "TSFunctionType",
  "TSConstructorType",
  "TSMethodSignature",
  "TSCallSignatureDeclaration",
  "TSConstructSignatureDeclaration",
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

/**
 * A double-quoted string literal in single quotes, as Prettier and Biome
 * choose them: single quotes unless the text holds more single quotes than
 * double ones, escaping only the quote that delimits it. Null when it stays.
 */
const requoted = (literal) => {
  const body = literal.slice(1, -1);
  const singles = (body.match(/'/g) ?? []).length;
  const doubles = (body.match(/(?<!\\)\\"|"/g) ?? []).length;

  if (singles > doubles) {
    return null;
  }

  // Unescape `\"`, escape a bare `'`; every other escape stays as written.
  const text = body.replace(/\\(.)|(')/gs, (match, escaped, single) => {
    if (single) {
      return "\\'";
    }

    return escaped === '"' ? '"' : match;
  });

  return `'${text}'`;
};

const quoteEdits = (source, nodes) =>
  nodes
    .filter((node) => node.kind === "StringLiteral" && source[node.start] === '"')
    .map((node) => ({ node, text: requoted(source.slice(node.start, node.end)) }))
    .filter(({ text }) => text !== null)
    .map(({ node, text }) => ({
      node,
      rule: RULES.singleQuotes,
      start: node.start,
      end: node.end,
      text,
    }));

// -- Rule 2: trailing commas -----------------------------------------------------

// `set` as a whole word, followed by the start of a key rather than by the
// `(` of a method named `set`: a name after whitespace or comments, or a `[`,
// `#` or quote.
const SETTER_HEAD =
  /(?:^|[^\w$.@#'"`])set(?:(?:\s|\/\*[\s\S]*?\*\/|\/\/[^\n]*\n)+[\w$#'"[]|[#'"[])/;

const MEMBER_KINDS = ["Property", "MethodDefinition", "TSMethodSignature"];

/**
 * Whether an object property, class member, or interface or type literal
 * member is a setter. The view carries no accessor flag, so this reads the
 * text from the member's start (its decorators and modifiers included) up to
 * its first parameter.
 */
const isSetter = (source, node, firstParameter) =>
  MEMBER_KINDS.includes(node.kind) &&
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

  // The braced specifiers of an import or export, `import type` and
  // `export type` included. A default or namespace binding is not in the
  // braces, so it is not one of the list's items.
  const specifiers = node.children.filter((child) =>
    SPECIFIER_KINDS.includes(child.kind),
  );

  if (specifiers.length > 0) {
    // Past the last specifier come its `,` if any, then the `}`.
    const last = specifiers[specifiers.length - 1];
    const after = nextSignificant(source, commentEnds, last.end);
    const close =
      after.char === ","
        ? nextSignificant(source, commentEnds, after.offset + 1)
        : after;

    return close.char === "}" ? { items: specifiers, close: close.offset } : null;
  }

  // A destructuring pattern's children are its elements, or for an object
  // pattern each property's value, which ends where the property does. Past
  // the last come its `,` if any, then the `}` or `]`; a second `,` there is a
  // hole, which a trailing comma would turn into one more element.
  if (node.kind === "ObjectPattern" || node.kind === "ArrayPattern") {
    if (node.children.length === 0) {
      return null;
    }

    const last = node.children[node.children.length - 1];
    const after = nextSignificant(source, commentEnds, last.end);
    const close =
      after.char === ","
        ? nextSignificant(source, commentEnds, after.offset + 1)
        : after;
    const closer = node.kind === "ObjectPattern" ? "}" : "]";

    return close.char === closer && close.offset === node.end - 1
      ? { items: node.children, close: close.offset }
      : null;
  }

  // A tuple type closes with `]`, a type parameter list with `>`; every child
  // is an element or a parameter.
  if (node.kind === "TSTupleType" && source[node.end - 1] === "]") {
    return { items: node.children, close: node.end - 1 };
  }

  if (
    node.kind === "TSTypeParameterDeclaration" &&
    source[node.end - 1] === ">"
  ) {
    return { items: node.children, close: node.end - 1 };
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

    if ((last.kind === "Parameter" && last.rest) || last.kind === "RestElement") {
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

// -- Rule 3: lay out long lines ----------------------------------------------
//
// A line longer than WIDTH is laid out again the way Prettier and Biome lay
// out code: the expression on it becomes a document of text, groups, indents
// and line breaks, and the printer keeps each group on one line when it fits
// and breaks it otherwise, outermost first.

// The default profile has no `while`; `for (const _ of forever)` is its
// `while (true)`.
const forever = {
  [Symbol.iterator]: () => ({ next: () => ({ done: false, value: undefined }) }),
};

const MODE_BREAK = 1;
const MODE_FLAT = 2;

const line = { type: "line" };
const softline = { type: "line", soft: true };
const hardline = { type: "line", hard: true };
let nextGroupId = 0;
const group = (contents, options = {}) => ({
  type: "group",
  contents,
  shouldBreak: Boolean(options.shouldBreak),
  expandedStates: options.expandedStates,
  id: options.id,
});
const conditionalGroup = (states, options = {}) =>
  group(states[0], { ...options, expandedStates: states });
const indent = (contents) => ({ type: "indent", contents });
const ifBreak = (breakContents, flatContents = "", groupId) => ({
  type: "ifBreak",
  breakContents,
  flatContents,
  groupId,
});
const indentIfBreak = (contents, groupId) =>
  ifBreak(indent(contents), contents, groupId);
const join = (separator, docs) =>
  docs.flatMap((doc, index) => (index === 0 ? [doc] : [separator, doc]));

const willBreak = (doc) => {
  if (typeof doc === "string") {
    return false;
  }

  if (Array.isArray(doc)) {
    return doc.some(willBreak);
  }

  switch (doc.type) {
    case "line":
      return Boolean(doc.hard);
    case "group":
      return doc.shouldBreak || willBreak(doc.contents);
    case "indent":
      return willBreak(doc.contents);
    case "ifBreak":
      return willBreak(doc.breakContents) || willBreak(doc.flatContents);
    default:
      return false;
  }
};

/** A group holding a hard line break is broken, as are the groups around it. */
const propagateBreaks = (doc) => {
  if (typeof doc === "string") {
    return false;
  }

  if (Array.isArray(doc)) {
    return doc.map(propagateBreaks).some(Boolean);
  }

  switch (doc.type) {
    case "line":
      return Boolean(doc.hard);
    case "group": {
      const inner = propagateBreaks(doc.contents);
      (doc.expandedStates ?? []).slice(1).forEach(propagateBreaks);

      if (inner) {
        doc.shouldBreak = true;
      }

      return doc.shouldBreak;
    }
    case "indent":
      return propagateBreaks(doc.contents);
    case "ifBreak":
      return [propagateBreaks(doc.breakContents), propagateBreaks(doc.flatContents)].some(Boolean);
    default:
      return false;
  }
};

const fits = (next, restCommands, startWidth, groupModes, mustBeFlat) => {
  let width = startWidth;
  let restIndex = restCommands.length;
  const commands = [next];

  for (const _ of forever) {
    if (width < 0) {
      return false;
    }

    if (commands.length === 0) {
      if (restIndex === 0) {
        return true;
      }

      restIndex = restIndex - 1;
      commands.push(restCommands[restIndex]);
      continue;
    }

    const { mode, doc } = commands.pop();

    if (typeof doc === "string") {
      width = width - doc.length;
    } else if (Array.isArray(doc)) {
      doc
        .slice()
        .reverse()
        .forEach((part) => commands.push({ mode, doc: part }));
    } else if (doc.type === "indent") {
      commands.push({ mode, doc: doc.contents });
    } else if (doc.type === "group") {
      if (mustBeFlat && doc.shouldBreak) {
        return false;
      }

      const groupMode = doc.shouldBreak ? MODE_BREAK : mode;
      const contents =
        doc.expandedStates && groupMode === MODE_BREAK
          ? doc.expandedStates[doc.expandedStates.length - 1]
          : doc.contents;
      commands.push({ mode: groupMode, doc: contents });
    } else if (doc.type === "ifBreak") {
      const groupMode = doc.groupId ? groupModes.get(doc.groupId) ?? MODE_FLAT : mode;
      const contents = groupMode === MODE_BREAK ? doc.breakContents : doc.flatContents;

      if (contents) {
        commands.push({ mode, doc: contents });
      }
    } else if (doc.type === "line") {
      if (mode === MODE_BREAK || doc.hard) {
        return true;
      }

      if (!doc.soft) {
        width = width - 1;
      }
    }
  }
};

/** The document as text, starting at column `start` with `indentation`. */
const printDoc = (doc, indentation, start) => {
  propagateBreaks(doc);
  const groupModes = new Map();
  const commands = [{ indentation, mode: MODE_BREAK, doc }];
  let out = "";
  let column = start;

  for (const _ of forever) {
    if (commands.length === 0) {
      break;
    }

    const command = commands.pop();
    const { mode, doc: current } = command;

    if (typeof current === "string") {
      out = out + current;
      column = column + current.length;
    } else if (Array.isArray(current)) {
      current
        .slice()
        .reverse()
        .forEach((part) => commands.push({ ...command, doc: part }));
    } else if (current.type === "indent") {
      commands.push({
        indentation: command.indentation + INDENT.length,
        mode,
        doc: current.contents,
      });
    } else if (current.type === "group") {
      let chosen;

      if (mode === MODE_FLAT) {
        chosen = {
          ...command,
          mode: current.shouldBreak ? MODE_BREAK : MODE_FLAT,
          doc: current.contents,
        };
      } else {
        const flat = { ...command, mode: MODE_FLAT, doc: current.contents };
        const room = WIDTH - column;

        if (!current.shouldBreak && fits(flat, commands, room, groupModes, false)) {
          chosen = flat;
        } else if (current.expandedStates) {
          const states = current.expandedStates;
          const mostExpanded = { ...command, mode: MODE_BREAK, doc: states[states.length - 1] };

          chosen = current.shouldBreak
            ? mostExpanded
            : states
                .slice(1)
                .map((state) => ({ ...command, mode: MODE_FLAT, doc: state }))
                .find((state) => fits(state, commands, room, groupModes, false)) ??
              mostExpanded;
        } else {
          chosen = { ...command, mode: MODE_BREAK, doc: current.contents };
        }
      }

      if (current.id) {
        groupModes.set(current.id, chosen.mode);
      }

      commands.push(chosen);
    } else if (current.type === "ifBreak") {
      const groupMode = current.groupId
        ? groupModes.get(current.groupId) ?? MODE_FLAT
        : mode;
      const contents =
        groupMode === MODE_BREAK ? current.breakContents : current.flatContents;

      if (contents) {
        commands.push({ ...command, doc: contents });
      }
    } else if (current.type === "line") {
      if (mode === MODE_FLAT && !current.hard) {
        if (!current.soft) {
          out = out + " ";
          column = column + 1;
        }
      } else {
        out = `${out.replace(/[ \t]+$/, "")}\n${" ".repeat(command.indentation)}`;
        column = command.indentation;
      }
    }
  }

  return out;
};

// The documents for the expressions, after Prettier's printers. What is not
// laid out here is printed as written.

const CALL_LIKE = ["CallExpression", "NewExpression"];
const BINARYISH = ["BinaryExpression", "LogicalExpression"];
const PRECEDENCE = new Map(
  [
    ["??"],
    ["||"],
    ["&&"],
    ["|"],
    ["^"],
    ["&"],
    ["==", "===", "!=", "!=="],
    ["<", ">", "<=", ">=", "in", "instanceof"],
    [">>", "<<", ">>>"],
    ["+", "-"],
    ["*", "/", "%"],
    ["**"],
  ].flatMap((operators, level) => operators.map((operator) => [operator, level])),
);
const EQUALITY = ["==", "===", "!=", "!=="];
const MULTIPLICATIVE = ["*", "/", "%"];
const BITSHIFT = [">>", ">>>", "<<"];

const shouldFlatten = (parentOperator, operator) => {
  if (PRECEDENCE.get(operator) !== PRECEDENCE.get(parentOperator)) {
    return false;
  }

  if (parentOperator === "**") {
    return false;
  }

  if (EQUALITY.includes(parentOperator) && EQUALITY.includes(operator)) {
    return false;
  }

  if (
    (operator === "%" && MULTIPLICATIVE.includes(parentOperator)) ||
    (parentOperator === "%" && MULTIPLICATIVE.includes(operator))
  ) {
    return false;
  }

  if (operator !== parentOperator && MULTIPLICATIVE.includes(operator) && MULTIPLICATIVE.includes(parentOperator)) {
    return false;
  }

  return !(BITSHIFT.includes(parentOperator) && BITSHIFT.includes(operator));
};

const makeLayout = (source, commentEnds) => {
  const parents = new Map();
  const raw = (node) => source.slice(node.start, node.end);
  const between = (from, to) => source.slice(from, to).trim();
  const isLiteralish = (node) =>
    ["StringLiteral", "NumericLiteral", "BooleanLiteral", "NullLiteral", "BigIntLiteral"].includes(node.kind);
  const callArgs = (node) => {
    // Only `?.` and type arguments can come between the callee and the `(`,
    // so it is found forward: scanning back would stop at a `(` in a string.
    const callee = node.children[0];
    const typeArguments = node.children.find(
      (child) => child.kind === "TSTypeParameterInstantiation" && child.start >= callee.end,
    );
    const afterCallee = nextSignificant(source, commentEnds, (typeArguments ?? callee).end);
    const next = afterCallee.char === "?" ? nextSignificant(source, commentEnds, afterCallee.offset + 2) : afterCallee;
    const open = next.char === "(" && next.offset < node.end ? next.offset : node.end - 1;

    return {
      callee,
      open,
      args: node.children.slice(1).filter((child) => child.start > open),
    };
  };

  const isSimpleCallArgument = (node, depth = 0) => {
    const simpleChild = (child) => isSimpleCallArgument(child, depth + 1);

    if (isLiteralish(node) || node.kind === "RegExpLiteral" || node.kind === "Identifier" || node.kind === "ThisExpression") {
      return true;
    }

    if (node.kind === "TemplateLiteral") {
      return node.children.every((child) => child.kind === "Identifier" || child.kind === "ThisExpression");
    }

    if (node.kind === "ObjectExpression") {
      return node.children.every(
        (property) =>
          property.kind === "Property" &&
          !property.computed &&
          (property.shorthand || simpleChild(property.children[property.children.length - 1])),
      );
    }

    if (node.kind === "ArrayExpression") {
      return node.children.every(simpleChild);
    }

    if (CALL_LIKE.includes(node.kind)) {
      const { callee, args } = callArgs(node);

      return depth < 2 && isSimpleCallArgument(callee, depth) && args.every(simpleChild);
    }

    if (node.kind === "MemberExpression") {
      return !node.computed
        ? isSimpleCallArgument(node.children[0], depth)
        : isSimpleCallArgument(node.children[0], depth) && isSimpleCallArgument(node.children[1], depth);
    }

    if (node.kind === "UnaryExpression" && ["!", "-"].includes(node.operator)) {
      return isSimpleCallArgument(node.children[0], depth);
    }

    if (node.kind === "TSNonNullExpression") {
      return isSimpleCallArgument(node.children[0], depth);
    }

    return false;
  };

  // An arrow's body; a conditional one is read through the parentheses Biome
  // prints around it on one line, which the layout adds back itself.
  const bodyOf = (arrow) => {
    const body = arrow.children[arrow.children.length - 1];

    return body.kind === "ParenthesizedExpression" && body.children[0]?.kind === "ConditionalExpression"
      ? body.children[0]
      : body;
  };

  const couldExpandArg = (node, arrowChain = false) =>
    (node.kind === "ObjectExpression" && node.children.length > 0) ||
    (node.kind === "ArrayExpression" && node.children.length > 0) ||
    ((node.kind === "TSAsExpression" || node.kind === "TSSatisfiesExpression") &&
      couldExpandArg(node.children[0])) ||
    node.kind === "FunctionExpression" ||
    (node.kind === "ArrowFunctionExpression" &&
      [
        "BlockStatement",
        "ArrowFunctionExpression",
        "ObjectExpression",
        "ArrayExpression",
        "TSAsExpression",
        "TSSatisfiesExpression",
        "JSXElement",
        ...(arrowChain ? [] : ["CallExpression", "NewExpression", "ConditionalExpression"]),
      ].includes(bodyOf(node).kind));

  const shouldGroupLast = (args) => {
    const last = args[args.length - 1];
    const penultimate = args[args.length - 2];

    return (
      couldExpandArg(last) &&
      (!penultimate || penultimate.kind !== last.kind) &&
      (args.length !== 2 ||
        penultimate.kind !== "ArrowFunctionExpression" ||
        last.kind !== "ArrayExpression")
    );
  };

  const printArguments = (node) => {
    const { args } = callArgs(node);

    if (args.length === 0) {
      return "()";
    }

    const printed = args.map((arg) => print(arg));
    const separated = join([",", line], printed);
    const allArgsBrokenOut = () =>
      group(["(", indent([line, ...separated, ","]), line, ")"], { shouldBreak: true });

    if (printed.slice(0, -1).some(willBreak)) {
      return allArgsBrokenOut();
    }

    if (shouldGroupLast(args)) {
      const expanded = print(args[args.length - 1], { expandLastArg: true });
      const leading = printed.slice(0, -1).flatMap((doc) => [doc, ", "]);

      return conditionalGroup([
        ["(", ...leading, expanded, ")"],
        ["(", ...leading, group(expanded, { shouldBreak: true }), ")"],
        allArgsBrokenOut(),
      ]);
    }

    return group(["(", indent([softline, ...separated]), ifBreak(","), softline, ")"]);
  };

  // A non-computed property is not a node of its own: its name is the text
  // after the `.` or `?.`.
  const propertyName = (member) =>
    source.slice(member.children[0].end, member.end).replace(/^\s*\??\.\s*/, "");
  const isMemberish = (node) => node.kind === "MemberExpression" || node.kind === "TSNonNullExpression";
  const isFactory = (name) => /^[A-Z]|^[$_]+$/.test(name);

  const printMemberChain = (node) => {
    const parent = parents.get(node);
    const isExpressionStatement = parent?.kind === "ExpressionStatement";
    const printedNodes = [];
    const visit = (current) => {
      if (CALL_LIKE.includes(current.kind) && current.kind === "CallExpression") {
        const { callee, open } = callArgs(current);

        if (isMemberish(callee) || callee.kind === "CallExpression") {
          printedNodes.unshift({ node: current, printed: [between(callee.end, open), printArguments(current)] });
          visit(callee);

          return;
        }
      } else if (current.kind === "MemberExpression") {
        const [object] = current.children;
        printedNodes.unshift({ node: current, printed: between(object.end, current.end) });
        visit(object);

        return;
      } else if (current.kind === "TSNonNullExpression") {
        printedNodes.unshift({ node: current, printed: "!" });
        visit(current.children[0]);

        return;
      }

      printedNodes.unshift({ node: current, printed: print(current) });
    };
    const { callee, open } = callArgs(node);
    printedNodes.unshift({ node, printed: [between(callee.end, open), printArguments(node)] });
    visit(callee);

    const groups = [];
    let currentGroup = [printedNodes[0]];
    let index = 1;

    for (const _ of forever) {
      const current = printedNodes[index]?.node;

      if (
        current &&
        (current.kind === "TSNonNullExpression" ||
          current.kind === "CallExpression" ||
          (current.kind === "MemberExpression" && current.computed && isLiteralish(current.children[1])))
      ) {
        currentGroup.push(printedNodes[index]);
        index = index + 1;
      } else {
        break;
      }
    }

    if (printedNodes[0].node.kind !== "CallExpression") {
      for (const _ of forever) {
        if (
          index + 1 < printedNodes.length &&
          isMemberish(printedNodes[index].node) &&
          isMemberish(printedNodes[index + 1].node)
        ) {
          currentGroup.push(printedNodes[index]);
          index = index + 1;
        } else {
          break;
        }
      }
    }

    groups.push(currentGroup);
    currentGroup = [];
    let hasSeenCall = false;

    printedNodes.slice(index).forEach((tuple) => {
      const current = tuple.node;

      if (hasSeenCall && isMemberish(current)) {
        if (current.computed && !isLiteralish(current.children[1])) {
          currentGroup.push(tuple);

          return;
        }

        groups.push(currentGroup);
        currentGroup = [];
        hasSeenCall = false;
      }

      if (current.kind === "CallExpression") {
        hasSeenCall = true;
      }

      currentGroup.push(tuple);
    });

    if (currentGroup.length > 0) {
      groups.push(currentGroup);
    }

    const shouldNotWrap = () => {
      const hasComputed =
        groups[1]?.[0]?.node.computed && isLiteralish(groups[1][0].node.children[1]);

      if (groups[0].length === 1) {
        const first = groups[0][0].node;

        return (
          first.kind === "ThisExpression" ||
          (first.kind === "Identifier" &&
            (isFactory(raw(first)) ||
              (isExpressionStatement && raw(first).length <= INDENT.length) ||
              hasComputed))
        );
      }

      const last = groups[0][groups[0].length - 1].node;

      return (
        last.kind === "MemberExpression" &&
        !last.computed &&
        (isFactory(propertyName(last)) || hasComputed)
      );
    };

    const shouldMerge = groups.length >= 2 && groups[1].length > 0 && shouldNotWrap();
    const printGroup = (printedGroup) => printedGroup.map((tuple) => tuple.printed);
    const printedGroups = groups.map(printGroup);
    const oneLine = printedGroups;
    const cutoff = shouldMerge ? 3 : 2;

    if (groups.length <= cutoff && !printedGroups.slice(cutoff).some(willBreak)) {
      return group(oneLine);
    }

    const rest = groups.slice(shouldMerge ? 2 : 1);
    const expanded = [
      printGroup(groups[0]),
      shouldMerge ? printGroup(groups[1]) : "",
      rest.length === 0 ? "" : indent(group([hardline, join(hardline, rest.map(printGroup))])),
    ];
    const calls = printedNodes.map((tuple) => tuple.node).filter((current) => current.kind === "CallExpression");
    const lastGroupWillBreakAndOtherCallsAreFunctionArguments = () => {
      const lastGroupNode = groups[groups.length - 1][0].node;

      return (
        lastGroupNode.kind === "CallExpression" &&
        willBreak(printedGroups[printedGroups.length - 1]) &&
        calls
          .slice(0, -1)
          .some((call) =>
            callArgs(call).args.some((arg) => ["ArrowFunctionExpression", "FunctionExpression"].includes(arg.kind)),
          )
      );
    };

    if (
      (calls.length > 2 &&
        calls.some((call) => !callArgs(call).args.every((arg) => isSimpleCallArgument(arg)))) ||
      printedGroups.slice(0, -1).some(willBreak) ||
      lastGroupWillBreakAndOtherCallsAreFunctionArguments()
    ) {
      return group(expanded);
    }

    return conditionalGroup([oneLine, expanded]);
  };

  const printCall = (node) => {
    const { callee, open } = callArgs(node);

    if (node.kind === "CallExpression" && isMemberish(callee)) {
      return printMemberChain(node);
    }

    const contents = [
      source.slice(node.start, callee.start),
      print(callee),
      between(callee.end, open),
      printArguments(node),
    ];

    return callee.kind === "CallExpression" ? group(contents) : contents;
  };

  const shouldInlineLogical = (node) => {
    if (node.kind !== "LogicalExpression") {
      return false;
    }

    const right = node.children[1];

    return (
      (right.kind === "ObjectExpression" || right.kind === "ArrayExpression") &&
      right.children.length > 0
    );
  };

  const operatorOf = (node) => between(node.children[0].end, node.children[1].start);

  const printBinaryishParts = (node, isNested, insideParens) => {
    const [left, right] = node.children;
    const operator = operatorOf(node);
    let parts;

    if (BINARYISH.includes(left.kind) && shouldFlatten(operator, operatorOf(left))) {
      parts = printBinaryishParts(left, true, insideParens);
    } else {
      parts = [group(print(left))];
    }

    const shouldInline = shouldInlineLogical(node);
    const rightDoc = shouldInline ? [operator, " ", print(right)] : [operator, line, print(right)];
    const shouldGroup =
      !(insideParens && node.kind === "LogicalExpression") &&
      parentOutsideParens(node).kind !== node.kind &&
      left.kind !== node.kind &&
      right.kind !== node.kind;

    parts.push(" ", shouldGroup ? group(rightDoc) : rightDoc);

    return parts;
  };

  // Prettier's tree has no parenthesized expressions: a node's parent is
  // the one outside any parentheses around it.
  const parentOutsideParens = (node) => {
    const parent = parents.get(node);

    return parent?.kind === "ParenthesizedExpression" ? parentOutsideParens(parent) : parent;
  };

  const printBinaryish = (node) => {
    const parent = parentOutsideParens(node);
    const grandparent = parent && parents.get(parent);
    const insideParens = ["IfStatement", "WhileStatement", "SwitchStatement", "DoWhileStatement"].includes(parent.kind);
    const parts = printBinaryishParts(node, false, insideParens);

    if (insideParens) {
      return parts;
    }

    if (
      (CALL_LIKE.includes(parent.kind) && parent.children[0] === node) ||
      parent.kind === "UnaryExpression" ||
      (parent.kind === "MemberExpression" && !parent.computed)
    ) {
      return group([indent([softline, ...parts]), softline]);
    }

    const shouldNotIndent =
      parent.kind === "ReturnStatement" ||
      parent.kind === "ThrowStatement" ||
      (parent.kind === "ArrowFunctionExpression" && bodyOf(parent) === node) ||
      (parent.kind === "ConditionalExpression" &&
        grandparent &&
        !["ReturnStatement", "ThrowStatement", ...CALL_LIKE].includes(grandparent.kind)) ||
      parent.kind === "TemplateLiteral";
    const shouldIndentIfInlining = ["AssignmentExpression", "VariableDeclaration", "PropertyDefinition", "Property"].includes(parent.kind);
    const [left] = node.children;
    const samePrecedenceSubExpression =
      BINARYISH.includes(left.kind) && shouldFlatten(operatorOf(node), operatorOf(left));

    if (
      shouldNotIndent ||
      (shouldInlineLogical(node) && !samePrecedenceSubExpression) ||
      (!shouldInlineLogical(node) && shouldIndentIfInlining)
    ) {
      return group(parts);
    }

    const firstGroupIndex = parts.findIndex((part) => typeof part === "object" && !Array.isArray(part) && part.type === "group");
    const head = parts.slice(0, firstGroupIndex === -1 ? 1 : firstGroupIndex + 1);

    return group([...head, indent(parts.slice(head.length))]);
  };

  const printArrow = (node, options) => {
    const body = bodyOf(node);
    const written = node.children[node.children.length - 1];
    const head = source.slice(node.start, written.start).trimEnd();

    if (
      ["BlockStatement", "ObjectExpression", "ArrayExpression", "ArrowFunctionExpression", "JSXElement"].includes(body.kind)
    ) {
      return [head, " ", print(body)];
    }

    const shouldAddSoftLine = Boolean(options.expandLastArg);
    const shouldAddParens = body.kind === "ConditionalExpression";

    return group([
      head,
      group([
        indent([line, shouldAddParens ? ifBreak("", "(") : "", print(body), shouldAddParens ? ifBreak("", ")") : ""]),
        shouldAddSoftLine ? [ifBreak(","), softline] : "",
      ]),
    ]);
  };

  const printConditional = (node) => {
    const [test, consequent, alternate] = node.children;

    return group([
      print(test),
      indent([line, "? ", print(consequent), line, ": ", print(alternate)]),
    ]);
  };

  const printObject = (node) => {
    if (node.children.length === 0) {
      return raw(node);
    }

    // A pattern's children are its bindings alone: a key, `...` or `:` before
    // each one is text between the previous entry and this one.
    const entry = (child, index) => {
      if (node.kind !== "ObjectPattern") {
        return print(child);
      }

      const from = index === 0 ? node.start + 1 : node.children[index - 1].end;
      const prefix = source
        .slice(from, child.start)
        .replace(/^[\s,]*/, "")
        .replace(/\s*:\s*$/, ": ");

      return [prefix, print(child)];
    };

    return group([
      "{",
      indent([line, ...join([",", line], node.children.map(entry))]),
      // Nothing may follow a rest element, not even a comma.
      ifBreak(node.children[node.children.length - 1].kind === "RestElement" ? "" : ","),
      line,
      "}",
    ]);
  };

  const printArray = (node) => {
    if (node.children.length === 0) {
      return raw(node);
    }

    // A hole has no node; it is printed as written.
    const holes = node.children.some(
      (child, index) => index > 0 && /,\s*,/.test(source.slice(node.children[index - 1].end, child.start)),
    );

    if (holes || /,\s*,|\[\s*,/.test(raw(node))) {
      return raw(node);
    }

    const elements = node.children;
    const shouldBreak =
      elements.length > 1 &&
      elements.every(
        (element, index) =>
          (element.kind === "ObjectExpression" || element.kind === "ArrayExpression") &&
          element.children.length > 1 &&
          (index === elements.length - 1 || elements[index + 1].kind === element.kind),
      );

    return group(
      ["[", indent([softline, ...join([",", line], elements.map((child) => print(child)))]), ifBreak(","), softline, "]"],
      { shouldBreak },
    );
  };

  const isMemberChainOnly = (node) => {
    if (node.kind === "MemberExpression") {
      return isMemberChainOnly(node.children[0]);
    }

    return node.kind === "Identifier" || node.kind === "ThisExpression";
  };

  const isLoneShortArgument = (node) => {
    const threshold = WIDTH / 4;

    if (node.kind === "Identifier" || node.kind === "ThisExpression") {
      return raw(node).length <= threshold;
    }

    if (node.kind === "NumericLiteral" || node.kind === "BooleanLiteral" || node.kind === "NullLiteral") {
      return true;
    }

    if (node.kind === "StringLiteral" || node.kind === "TemplateLiteral") {
      return raw(node).length <= threshold && !raw(node).includes("${");
    }

    if (node.kind === "UnaryExpression") {
      return isLoneShortArgument(node.children[0]);
    }

    return (node.kind === "ObjectExpression" || node.kind === "ArrayExpression") && node.children.length === 0;
  };

  const isPoorlyBreakableChain = (node, deep = false) => {
    if (node.kind === "TSNonNullExpression") {
      return isPoorlyBreakableChain(node.children[0], true);
    }

    if (node.kind === "MemberExpression") {
      return isPoorlyBreakableChain(node.children[0], true);
    }

    if (node.kind === "CallExpression") {
      const { callee, args } = callArgs(node);
      const poorlyBreakable = args.length === 0 || (args.length === 1 && isLoneShortArgument(args[0]));

      return poorlyBreakable && callee.end === node.children[0].end && isPoorlyBreakableChain(callee, true);
    }

    return deep && (node.kind === "Identifier" || node.kind === "ThisExpression");
  };

  const assignmentLayout = (right, keyLength) => {
    // A key shorter than an indent and three columns gains too little from
    // moving its value to the next line.
    const hasShortKey = keyLength !== undefined && keyLength < INDENT.length + 3;

    if (
      (BINARYISH.includes(right.kind) && !shouldInlineLogical(right)) ||
      (right.kind === "StringLiteral" && !hasShortKey) ||
      (right.kind === "MemberExpression" && isMemberChainOnly(right)) ||
      (right.kind === "ConditionalExpression" &&
        BINARYISH.includes(right.children[0].kind) &&
        !shouldInlineLogical(right.children[0]))
    ) {
      return "break-after-operator";
    }

    if (
      right.kind === "TemplateLiteral" ||
      right.kind === "TaggedTemplateExpression" ||
      right.kind === "BooleanLiteral" ||
      right.kind === "NumericLiteral" ||
      right.kind === "ClassExpression" ||
      (right.kind === "CallExpression" && raw(right.children[0]) === "require") ||
      hasShortKey
    ) {
      return "never-break-after-operator";
    }

    if (isPoorlyBreakableChain(right)) {
      return "break-after-operator";
    }

    return "fluid";
  };

  const printAssignment = (left, operator, right, keyLength) => {
    const rightDoc = print(right);

    switch (assignmentLayout(right, keyLength)) {
      case "break-after-operator":
        return group([left, operator, group(indent([line, rightDoc]))]);
      case "never-break-after-operator":
        return group([left, operator, " ", rightDoc]);
      default: {
        nextGroupId = nextGroupId + 1;
        const id = `assignment-${nextGroupId}`;

        return group([left, operator, group(indent(line), { id }), indentIfBreak(rightDoc, id)]);
      }
    }
  };

  const printProperty = (node) => {
    if (node.shorthand || node.method || node.computed || node.children.length < 2) {
      return raw(node);
    }

    const value = node.children[node.children.length - 1];
    const key = source.slice(node.start, value.start).replace(/\s*:\s*$/, "");

    return printAssignment(key, ":", value, key.length);
  };

  const print = (node, options = {}) => {
    switch (node.kind) {
      case "CallExpression":
      case "NewExpression":
        return printCall(node);
      case "BinaryExpression":
      case "LogicalExpression":
        return printBinaryish(node);
      case "ArrowFunctionExpression":
        return printArrow(node, options);
      case "ConditionalExpression":
        return printConditional(node);
      case "ObjectExpression":
      case "ObjectPattern":
        return printObject(node);
      case "ArrayExpression":
        return printArray(node);
      case "Property":
        return printProperty(node);
      case "ParenthesizedExpression":
        return ["(", print(node.children[0]), ")"];
      case "AwaitExpression":
        return [source.slice(node.start, node.children[0].start), print(node.children[0])];
      case "SpreadElement":
        return ["...", print(node.children[0])];
      case "MemberExpression": {
        const [object] = node.children;

        if (node.computed) {
          return raw(node);
        }

        // `a.b` alone stays together; a longer access may break before `.b`.
        const shouldInline =
          object.kind === "Identifier" && parents.get(node)?.kind !== "MemberExpression";
        const lookup = between(object.end, node.end);

        return [print(object), shouldInline ? lookup : group(indent([softline, lookup]))];
      }
      case "TSNonNullExpression":
        return [print(node.children[0]), "!"];
      case "TSTypeAnnotation": {
        const [type] = node.children;

        return type ? [source.slice(node.start, type.start), print(type)] : raw(node);
      }
      case "TSTypeReference": {
        const typeArguments = node.children.find((child) => child.kind === "TSTypeParameterInstantiation");

        return typeArguments
          ? [source.slice(node.start, typeArguments.start), print(typeArguments), source.slice(typeArguments.end, node.end)]
          : raw(node);
      }
      case "UnaryExpression": {
        const [argument] = node.children;

        return argument ? [source.slice(node.start, argument.start), print(argument)] : raw(node);
      }
      case "TSUnionType": {
        const parent = parents.get(node);
        // A union is indented under what holds it, except inside type
        // arguments, where the argument list already indents it.
        const shouldIndent = !["TSTypeParameterInstantiation", "TSTypeReference", "TSTupleType"].includes(parent?.kind);
        const code = [
          ifBreak([shouldIndent ? line : "", "| "]),
          ...join([line, "| "], node.children.map((child) => print(child))),
        ];

        return group(shouldIndent ? indent(code) : code);
      }
      case "TSTypeParameter": {
        const constraint = node.children.find((child) => child.start > node.start);

        return constraint
          ? [source.slice(node.start, constraint.start), print(constraint), source.slice(constraint.end, node.end)]
          : raw(node);
      }
      case "TSTypeParameterInstantiation":
        return group(["<", indent([softline, ...join([",", line], node.children.map((child) => print(child)))]), softline, ">"]);
      case "TSTypeLiteral":
        if (node.children.length === 0 || /\n/.test(raw(node))) {
          return raw(node);
        }

        return group([
          "{",
          indent([line, ...join([";", line], node.children.map((child) => raw(child)))]),
          ifBreak(";"),
          line,
          "}",
        ]);
      case "Parameter": {
        const annotation = node.children.find((child) => child.kind === "TSTypeAnnotation");
        const target = node.children[0] !== annotation ? node.children[0] : undefined;
        const binding = target && target.end <= (annotation ?? node).start && target.start === node.start ? target : undefined;
        const head = binding ? print(binding) : source.slice(node.start, annotation ? annotation.start : node.end);

        if (!annotation) {
          return binding ? [head, source.slice(binding.end, node.end)] : raw(node);
        }

        const after = node.children[node.children.length - 1];

        // A destructured parameter typed by a type literal breaks its pattern
        // and keeps the type on one line, as Biome does.
        if (binding?.kind === "ObjectPattern" && annotation.children[0]?.kind === "TSTypeLiteral") {
          return [head, source.slice(binding.end, node.end)];
        }

        return [
          head,
          binding ? source.slice(binding.end, annotation.start) : "",
          print(annotation),
          after !== annotation && after !== binding
            ? [source.slice(annotation.end, after.start), print(after)]
            : source.slice(annotation.end, node.end),
        ];
      }
      case "TSAsExpression":
      case "TSSatisfiesExpression": {
        const [expression] = node.children;

        const type = node.children[1];

        return type
          ? [print(expression), source.slice(expression.end, type.start), print(type)]
          : [print(expression), source.slice(expression.end, node.end)];
      }
      default:
        return raw(node);
    }
  };

  /**
   * The document for a node that sits on one line: a statement it heads, or
   * the expression itself.
   */
  const printRoot = (node) => {
    // A statement's own text after its last child, its `;`, stays.
    const tail = (child) => source.slice(child.end, node.end);

    if (node.kind === "ExpressionStatement") {
      return [print(node.children[0]), tail(node.children[0])];
    }

    if (node.kind === "ReturnStatement" || node.kind === "ThrowStatement") {
      const [argument] = node.children;

      if (!argument) {
        return null;
      }

      const keyword = source.slice(node.start, argument.start);
      const argumentDoc = print(argument);

      return BINARYISH.includes(argument.kind)
        ? [keyword, group([ifBreak("("), indent([softline, argumentDoc]), softline, ifBreak(")")]), tail(argument)]
        : [keyword, argumentDoc, tail(argument)];
    }

    if (node.kind === "VariableDeclaration") {
      // A name binding is not a node: one declarator `const x: T = init` has
      // its annotation, if any, and its initializer as children.
      const init = node.children[node.children.length - 1];
      const beforeInit = init ? source.slice(node.start, init.start) : "";
      const left = beforeInit.replace(/\s*=\s*$/, "");

      // `let state: T;` has an annotation and no initializer: only its type
      // can break.
      if (init?.kind === "TSTypeAnnotation" && node.children.length === 1 && /^(let|var)\s+[\w$]+$/.test(beforeInit)) {
        return [beforeInit, print(init), tail(init)];
      }

      if (
        !init ||
        init.kind === "TSTypeAnnotation" ||
        left === beforeInit ||
        !node.children.slice(0, -1).every((child) => child.kind === "TSTypeAnnotation") ||
        !/^(const|let|var)\s+[\w$]+(\s*:[^=]*)?$/.test(left)
      ) {
        return null;
      }

      return [printAssignment(left, " =", init), tail(init)];
    }

    return print(node);
  };

  return { parents, printRoot, printArguments, callArgs };
};

const layoutEdits = (source, comments, lineStarts, nodes, root) => {
  const longLines = new Set();

  lineStarts.slice(0, -1).forEach((start, index) => {
    if (lineText(source, lineStarts, index + 1).length > WIDTH) {
      longLines.add(index + 1);
    }
  });

  if (longLines.size === 0) {
    return [];
  }

  const layout = makeLayout(
    source,
    new Map(comments.map((comment) => [comment.start, comment.end])),
  );
  const walk = (node) => {
    node.children.forEach((child) => {
      layout.parents.set(child, node);
      walk(child);
    });
  };
  walk(root);

  const PRINTABLE_ROOTS = [
    "ExpressionStatement",
    "ReturnStatement",
    "ThrowStatement",
    "VariableDeclaration",
    "Property",
    "CallExpression",
    "NewExpression",
    "BinaryExpression",
    "LogicalExpression",
    "ArrowFunctionExpression",
    "ConditionalExpression",
    "ObjectExpression",
    "ArrayExpression",
    "MemberExpression",
    "AwaitExpression",
    "SpreadElement",
    "ParenthesizedExpression",
    "Parameter",
    "TSTypeReference",
    "TSTypeAnnotation",
    "TSAsExpression",
    "TSTypeParameter",
    "UnaryExpression",
  ];

  // The outermost node that starts and ends on each long line.
  const onLine = nodes.filter(
    (node) =>
      node !== root &&
      node.loc.start.line === node.loc.end.line &&
      longLines.has(node.loc.start.line),
  );
  const outermost = onLine.filter(
    (node) =>
      !onLine.some(
        (other) => other !== node && other.start <= node.start && node.end <= other.end,
      ),
  );

  // A template literal's substitutions are part of its text.
  const templates = nodes.filter((node) => node.kind === "TemplateLiteral");
  const inTemplate = (node) =>
    templates.some((template) => template.start < node.start && node.end < template.end);

  // The head of a call or member access that goes on past its line is laid
  // out with the rest of it, so it is not laid out alone.
  const isHeadOfLongerExpression = (node) => {
    const parent = layout.parents.get(node);

    return (
      parent !== undefined &&
      (([...CALL_LIKE, "MemberExpression", "TaggedTemplateExpression", "AssignmentExpression"].includes(parent.kind) &&
        parent.children[0] === node) ||
        // A declaration's left side is never broken, nor a function's return
        // type on the line of its head.
        (node.kind === "TSTypeAnnotation" &&
          ["VariableDeclaration", "FunctionDeclaration", "ExportFunctionDeclaration", "FunctionExpression", "ArrowFunctionExpression", "MethodDefinition"].includes(parent.kind)))
    );
  };

  // A call that spans lines, as a link of a broken member chain does, whose
  // argument list sits on one long line: that list is laid out alone.
  const argumentLists = nodes
    .filter((node) => node.kind === "CallExpression" && node.loc.start.line !== node.loc.end.line)
    .filter((node) => longLines.has(node.loc.end.line) && !inTemplate(node))
    .map((node) => ({ node, open: layout.callArgs(node).open }))
    .filter(({ node, open }) => lineStarts.findLastIndex((start) => start <= open) + 1 === node.loc.end.line)
    .filter(({ node, open }) => !comments.some((comment) => comment.start >= open && comment.end <= node.end))
    .flatMap(({ node, open }) => {
      const lineNumber = node.loc.end.line;
      const text = lineText(source, lineStarts, lineNumber);
      const suffix = source.slice(node.end, lineStarts[lineNumber] - 1);
      const column = open - lineStarts[lineNumber - 1];
      const withSuffix = printDoc([layout.printArguments(node), suffix], indentOf(text).length, column);
      const printed = withSuffix.slice(0, withSuffix.length - suffix.length);

      return printed.includes("\n") && printed !== source.slice(open, node.end)
        ? [{ node, rule: RULES.layout, start: open, end: node.end, text: printed }]
        : [];
    });

  return [
    ...argumentLists,
    ...outermost
    .filter(
      (node) =>
        PRINTABLE_ROOTS.includes(node.kind) && !inTemplate(node) && !isHeadOfLongerExpression(node),
    )
    .filter(
      (node) => !comments.some((comment) => comment.start >= node.start && comment.end <= node.end),
    )
    .flatMap((node) => {
      const lineNumber = node.loc.start.line;
      const text = lineText(source, lineStarts, lineNumber);
      const doc = layout.printRoot(node);

      if (doc === null) {
        return [];
      }

      // What follows the node on its line counts towards whether it fits.
      const suffix = source.slice(node.end, lineStarts[lineNumber] - 1);
      const withSuffix = printDoc([doc, suffix], indentOf(text).length, node.loc.start.column - 1);
      const printed = withSuffix.slice(0, withSuffix.length - suffix.length);

      if (!printed.includes("\n") || printed === source.slice(node.start, node.end)) {
        return [];
      }

      return [{ node, rule: RULES.layout, start: node.start, end: node.end, text: printed }];
    }),
  ];
};

const indentOf = (line) => line.slice(0, line.length - line.trimStart().length);

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
    ...layoutEdits(source, comments, lineStarts, nodes, root),
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

/**
 * The line ending a file is written with: its first one, or LF when it has
 * none. A file that mixes them is written back with that first one.
 */
const lineEndingOf = (text) => {
  const match = /\r\n?|\n/.exec(text);

  return match === null ? "\n" : match[0];
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
    // `parse` measures every offset in its LF-normalized `source`, so the
    // rules read that text, and a fixed file gets its line ending back.
    const written = fs.readFileSync(file, "utf8");
    const lineEnding = lineEndingOf(written);
    const source = written.replace(/\r\n?/g, "\n");
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
      fs.writeFileSync(file, fixed.replace(/\n/g, lineEnding));
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
