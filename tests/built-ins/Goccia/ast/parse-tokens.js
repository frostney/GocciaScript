/*---
description: goccia:ast parse() with tokens reports every token and comment of the file as a typed range
features: [Goccia, experimental-ast]
---*/

import { parse } from "goccia:ast";

// Each token as [type, the text its range selects], which is what a consumer
// reads: the type to dispatch on, the range to slice.
const lexed = (source, options = {}) => {
  const result = parse(source, { ...options, tokens: true });
  return result.tokens.map((token) => [
    token.type,
    result.source.slice(token.start, token.end),
  ]);
};

const textsOf = (source, type, options = {}) =>
  lexed(source, options)
    .filter(([tokenType]) => tokenType === type)
    .map(([, text]) => text);

// The claim a consumer that blanks non-code ranges relies on: tokens are in
// order, do not overlap, and leave nothing but whitespace uncovered.
const uncovered = (result) => {
  const gaps = [];
  let covered = 0;
  result.tokens.forEach((token) => {
    if (token.start < covered || token.end <= token.start) {
      gaps.push(`token ${token.start}-${token.end} overlaps or is empty`);
    }
    const gap = result.source.slice(covered, token.start);
    if (/\S/.test(gap)) {
      gaps.push(JSON.stringify(gap));
    }
    covered = token.end;
  });
  const tail = result.source.slice(covered);
  if (/\S/.test(tail)) {
    gaps.push(JSON.stringify(tail));
  }
  return gaps;
};

const NON_CODE = [
  "StringLiteral",
  "NoSubstitutionTemplate",
  "TemplateHead",
  "TemplateMiddle",
  "TemplateTail",
  "RegularExpressionLiteral",
  "LineComment",
  "BlockComment",
  "HashbangComment",
  "JSXText",
];

// Replaces every non-code range with spaces, keeping offsets, which is the
// whole job of a hand-written stripping lexer.
const blankNonCode = (result) =>
  result.tokens.reduce(
    (text, token) =>
      NON_CODE.includes(token.type)
        ? text.slice(0, token.start) +
          " ".repeat(token.end - token.start) +
          text.slice(token.end)
        : text,
    result.source,
  );

describe("ast.parse tokens", () => {
  test("tokens are opt-in, and absent when not asked for", () => {
    expect(Object.hasOwn(parse("a;\n"), "tokens")).toBe(false);
    expect(Object.hasOwn(parse("a;\n", { tokens: false }), "tokens")).toBe(false);
    expect(Array.isArray(parse("a;\n", { tokens: true }).tokens)).toBe(true);
  });

  test("each token is a type and a range into the source, nothing else", () => {
    const result = parse("const a = 1;", { tokens: true });

    expect(result.tokens).toEqual([
      { type: "Keyword", start: 0, end: 5 },
      { type: "Identifier", start: 6, end: 7 },
      { type: "Punctuator", start: 8, end: 9 },
      { type: "NumericLiteral", start: 10, end: 11 },
      { type: "Punctuator", start: 11, end: 12 },
    ]);
  });

  test("an empty source has no tokens", () => {
    expect(parse("", { tokens: true }).tokens).toEqual([]);
  });

  test("a keyword is a reserved word by spelling, whatever its role", () => {
    expect(lexed("let x = o.default ?? await0 ?? this;")).toEqual([
      ["Identifier", "let"],
      ["Identifier", "x"],
      ["Punctuator", "="],
      ["Identifier", "o"],
      ["Punctuator", "."],
      ["Keyword", "default"],
      ["Punctuator", "??"],
      ["Identifier", "await0"],
      ["Punctuator", "??"],
      ["Keyword", "this"],
      ["Punctuator", ";"],
    ]);
  });

  test("contextual words are identifiers, and a reserved word spelled with an escape is not a keyword", () => {
    expect(lexed("const of = async; o.\\u0069f;")).toEqual([
      ["Keyword", "const"],
      ["Identifier", "of"],
      ["Punctuator", "="],
      ["Identifier", "async"],
      ["Punctuator", ";"],
      ["Identifier", "o"],
      ["Punctuator", "."],
      ["Identifier", "\\u0069f"],
      ["Punctuator", ";"],
    ]);
  });

  test("true, false and null are keywords", () => {
    expect(textsOf("[true, false, null];", "Keyword")).toEqual(["true", "false", "null"]);
  });

  test("numeric literals keep their own spelling", () => {
    expect(textsOf("[1_000, 0x1F, 10n, .5, 2e-3];", "NumericLiteral")).toEqual([
      "1_000",
      "0x1F",
      "10n",
      ".5",
      "2e-3",
    ]);
  });

  test("a string literal covers its quotes, and quotes or slashes inside it start nothing", () => {
    const source = '["it\'s // not", \'say "/*" and \\\'`\', "`"];';

    expect(lexed(source)).toEqual([
      ["Punctuator", "["],
      ["StringLiteral", '"it\'s // not"'],
      ["Punctuator", ","],
      ["StringLiteral", '\'say "/*" and \\\'`\''],
      ["Punctuator", ","],
      ["StringLiteral", '"`"'],
      ["Punctuator", "]"],
      ["Punctuator", ";"],
    ]);
  });

  test("a template without substitutions is one token", () => {
    expect(lexed("const t = `a \\${x} b`;")).toEqual([
      ["Keyword", "const"],
      ["Identifier", "t"],
      ["Punctuator", "="],
      ["NoSubstitutionTemplate", "`a \\${x} b`"],
      ["Punctuator", ";"],
    ]);
  });

  test("a template's parts hold its text and delimiters, and the substitutions are code", () => {
    expect(lexed("t = `a${b}c${d + 1}e`;")).toEqual([
      ["Identifier", "t"],
      ["Punctuator", "="],
      ["TemplateHead", "`a${"],
      ["Identifier", "b"],
      ["TemplateMiddle", "}c${"],
      ["Identifier", "d"],
      ["Punctuator", "+"],
      ["NumericLiteral", "1"],
      ["TemplateTail", "}e`"],
      ["Punctuator", ";"],
    ]);
  });

  test("a template nested in a substitution is tokenized inside its parent's code", () => {
    expect(lexed("t = `a${`b${`c${d}`}`}e${f}`;")).toEqual([
      ["Identifier", "t"],
      ["Punctuator", "="],
      ["TemplateHead", "`a${"],
      ["TemplateHead", "`b${"],
      ["TemplateHead", "`c${"],
      ["Identifier", "d"],
      ["TemplateTail", "}`"],
      ["TemplateTail", "}`"],
      ["TemplateMiddle", "}e${"],
      ["Identifier", "f"],
      ["TemplateTail", "}`"],
      ["Punctuator", ";"],
    ]);
  });

  test("braces inside a substitution are punctuators, not the end of it", () => {
    expect(lexed("t = `${ {a: 1}.a }${() => { return 2; }}`;")).toEqual([
      ["Identifier", "t"],
      ["Punctuator", "="],
      ["TemplateHead", "`${"],
      ["Punctuator", "{"],
      ["Identifier", "a"],
      ["Punctuator", ":"],
      ["NumericLiteral", "1"],
      ["Punctuator", "}"],
      ["Punctuator", "."],
      ["Identifier", "a"],
      ["TemplateMiddle", "}${"],
      ["Punctuator", "("],
      ["Punctuator", ")"],
      ["Punctuator", "=>"],
      ["Punctuator", "{"],
      ["Keyword", "return"],
      ["NumericLiteral", "2"],
      ["Punctuator", ";"],
      ["Punctuator", "}"],
      ["TemplateTail", "}`"],
      ["Punctuator", ";"],
    ]);
  });

  test("a tagged template's parts are template tokens", () => {
    expect(lexed("tag`a${b}c`;").map(([type]) => type)).toEqual([
      "Identifier",
      "TemplateHead",
      "Identifier",
      "TemplateTail",
      "Punctuator",
    ]);
  });

  test("a slash after an operand is division", () => {
    expect(textsOf("q = a / b / c; r = (a) / 2; s = a++ / 2; s /= 2; u = x[0] / y;", "Punctuator")
      .filter((text) => text.includes("/"))).toEqual(["/", "/", "/", "/", "/=", "/"]);
    expect(textsOf("q = a / b / c;", "RegularExpressionLiteral")).toEqual([]);
  });

  test("a slash where an expression starts is a regular expression literal", () => {
    expect(textsOf("r = /ab+c/gi; s = [/[/]/, /\\//]; t = !/x/.test(y);", "RegularExpressionLiteral")).toEqual([
      "/ab+c/gi",
      "/[/]/",
      "/\\//",
      "/x/",
    ]);
  });

  test("the parser's goal decides a slash after a closing parenthesis", () => {
    // After `if (...)` a statement starts, so the slash opens a regex; after a
    // parenthesized expression it divides. A lexer that looks only at the
    // previous token cannot tell these apart.
    expect(lexed("if (ok) /x/g.test(s);\nconst q = (ok) / 2;\n")
      .filter(([type]) => type === "RegularExpressionLiteral" || type === "Punctuator")
      .filter(([, text]) => text.startsWith("/"))).toEqual([
      ["RegularExpressionLiteral", "/x/g"],
      ["Punctuator", "/"],
    ]);
  });

  test("a slash inside a substitution follows the substitution's own goal", () => {
    expect(lexed("t = `${a / b}${/re/.source}`;")
      .filter(([, text]) => text.includes("/"))).toEqual([
      ["Punctuator", "/"],
      ["RegularExpressionLiteral", "/re/"],
    ]);
  });

  test("comments are tokens, at the same ranges as in comments", () => {
    const source = "a; // one\n/* two */ b; /** three\n */\n";
    const result = parse(source, { tokens: true });
    const commentTokens = result.tokens.filter((token) => token.type.endsWith("Comment"));

    expect(commentTokens.map((token) => [token.type, source.slice(token.start, token.end)])).toEqual([
      ["LineComment", "// one"],
      ["BlockComment", "/* two */"],
      ["BlockComment", "/** three\n */"],
    ]);
    expect(commentTokens.map((token) => [token.start, token.end])).toEqual(
      result.comments.map((comment) => [comment.start, comment.end]),
    );
  });

  test("a hashbang is a comment token, and comments keeps only line and block comments", () => {
    const result = parse("#!/usr/bin/env goccia\nrun();\n", { tokens: true });

    expect(result.tokens[0].type).toBe("HashbangComment");
    expect(result.source.slice(result.tokens[0].start, result.tokens[0].end)).toBe(
      "#!/usr/bin/env goccia",
    );
    expect(result.comments).toEqual([]);
  });

  test("a private name is one token", () => {
    expect(lexed("class A { #p = 1; has(o) { return #p in o && this.#p; } }")
      .filter(([type]) => type === "PrivateIdentifier")).toEqual([
      ["PrivateIdentifier", "#p"],
      ["PrivateIdentifier", "#p"],
      ["PrivateIdentifier", "#p"],
    ]);
  });

  test("type annotations are code, so their tokens are reported", () => {
    expect(lexed("const m: Map<string, number> = new Map();", { fileName: "a.ts" })
      .slice(0, 9)).toEqual([
      ["Keyword", "const"],
      ["Identifier", "m"],
      ["Punctuator", ":"],
      ["Identifier", "Map"],
      ["Punctuator", "<"],
      ["Identifier", "string"],
      ["Punctuator", ","],
      ["Identifier", "number"],
      ["Punctuator", ">"],
    ]);
  });

  test("a >> that closes two type-argument lists is one punctuator, as the lexer scanned it", () => {
    expect(textsOf("const m: Array<Array<number>> = [];", "Punctuator", { fileName: "a.ts" }))
      .toEqual([":", "<", "<", ">>", "=", "[", "]", ";"]);
  });

  test("a CRLF source is normalized first, so offsets index the source returned", () => {
    const result = parse("const a = 'x';\r\nconst t = `1\r\n2`; // c\r\nb;\r\n", { tokens: true });
    const sliced = result.tokens.map((token) => result.source.slice(token.start, token.end));

    expect(result.source).toBe("const a = 'x';\nconst t = `1\n2`; // c\nb;\n");
    expect(sliced).toEqual([
      "const", "a", "=", "'x'", ";",
      "const", "t", "=", "`1\n2`", ";", "// c",
      "b", ";",
    ]);
    expect(uncovered(result)).toEqual([]);
  });

  test("tokens leave nothing but whitespace uncovered", () => {
    const source = [
      "#!/usr/bin/env goccia",
      "import { a } from './a.js';",
      "// line",
      "export const f = async (x = /=+/g, { y } = {}) => {",
      "  const t = `${x / 2}px ${`in${y}`} \\` }`;",
      "  return t?.length ?? 0n; /* block */",
      "};",
      "class C { static #n = 0; get n() { return C.#n; } }",
      "",
    ].join("\n");

    expect(uncovered(parse(source, { tokens: true }))).toEqual([]);
  });

  test("blanking the non-code tokens leaves only code to search", () => {
    const source = [
      "const a = process.env.API_KEY;",
      "const b = 'process.env.IN_STRING';",
      "// process.env.IN_COMMENT",
      "const c = `process.env.IN_TEMPLATE ${process.env.IN_SUBSTITUTION}`;",
      "const d = /process.env.IN_REGEX/;",
      "/* process.env.IN_BLOCK */ const e = x / process.env.AFTER_DIVISION;",
      "",
    ].join("\n");
    const code = blankNonCode(parse(source, { tokens: true }));

    expect(code.length).toBe(source.length);
    expect(code.match(/process\.env\.[A-Z_]+/g)).toEqual([
      "process.env.API_KEY",
      "process.env.IN_SUBSTITUTION",
      "process.env.AFTER_DIVISION",
    ]);
  });

  test("with jsx on, JSX's own names, punctuation, strings and text are tokens in the file", () => {
    expect(lexed('const el = <div className="x" id={`a${b}`}>Hi {name}</div>;', {
      jsx: true,
      fileName: "a.tsx",
    })).toEqual([
      ["Keyword", "const"],
      ["Identifier", "el"],
      ["Punctuator", "="],
      ["Punctuator", "<"],
      ["JSXIdentifier", "div"],
      ["JSXIdentifier", "className"],
      ["Punctuator", "="],
      ["StringLiteral", '"x"'],
      ["JSXIdentifier", "id"],
      ["Punctuator", "="],
      ["Punctuator", "{"],
      ["TemplateHead", "`a${"],
      ["Identifier", "b"],
      ["TemplateTail", "}`"],
      ["Punctuator", "}"],
      ["Punctuator", ">"],
      ["JSXText", "Hi "],
      ["Punctuator", "{"],
      ["Identifier", "name"],
      ["Punctuator", "}"],
      ["Punctuator", "<"],
      ["Punctuator", "/"],
      ["JSXIdentifier", "div"],
      ["Punctuator", ">"],
      ["Punctuator", ";"],
    ]);
  });

  test("with jsx on, nothing the transformer synthesized is a token", () => {
    const source = "const el = <A.B {...props} on>text<>{x}</></A.B>;\n";
    const texts = lexed(source, { jsx: true, fileName: "a.tsx" }).map(([, text]) => text);

    expect(texts.includes("createElement")).toBe(false);
    expect(texts.includes("Fragment")).toBe(false);
    expect(texts.includes("null")).toBe(false);
    expect(texts.includes('"text"')).toBe(false);
    expect(textsOf(source, "JSXIdentifier", { jsx: true, fileName: "a.tsx" })).toEqual([
      "A", "B", "on", "A", "B",
    ]);
    expect(textsOf(source, "JSXText", { jsx: true, fileName: "a.tsx" })).toEqual(["text"]);
    expect(uncovered(parse(source, { jsx: true, fileName: "a.tsx", tokens: true }))).toEqual([]);
  });

  test("with jsx on, comments the transformer drops are still tokens", () => {
    const source = [
      "const el = (",
      "  <div",
      "    // biome-ignore lint: reason",
      "    /* note */ id=\"a\"",
      "  >",
      "    {/* only a comment */}",
      "    {value /* kept */}",
      "  </div>",
      ");",
      "",
    ].join("\n");
    const result = parse(source, { jsx: true, fileName: "a.tsx", tokens: true });
    const commentTexts = result.tokens
      .filter((token) => token.type.endsWith("Comment"))
      .map((token) => source.slice(token.start, token.end));

    expect(commentTexts).toEqual([
      "// biome-ignore lint: reason",
      "/* note */",
      "/* only a comment */",
      "/* kept */",
    ]);
    expect(uncovered(result)).toEqual([]);
  });

  test("with jsx on, an element inside an attribute expression is measured in the file", () => {
    const source = "const el = <Box icon={<Icon name=\"x\" />} />;\n";

    expect(textsOf(source, "JSXIdentifier", { jsx: true, fileName: "a.tsx" })).toEqual([
      "Box", "icon", "Icon", "name",
    ]);
    expect(textsOf(source, "StringLiteral", { jsx: true, fileName: "a.tsx" })).toEqual(['"x"']);
    expect(uncovered(parse(source, { jsx: true, fileName: "a.tsx", tokens: true }))).toEqual([]);
  });

  test("with jsx on, a slash in JSX text is text, and one in a container is still decided by the parser", () => {
    const source = "const el = <p>a / b // c {x / 2}{/re/.source}</p>;\n";

    expect(lexed(source, { jsx: true, fileName: "a.tsx" })
      .filter(([, text]) => text.includes("/"))).toEqual([
      ["JSXText", "a / b // c "],
      ["Punctuator", "/"],
      ["RegularExpressionLiteral", "/re/"],
      ["Punctuator", "/"],
    ]);
  });

  test("with jsx on, shorthand, hyphenated and single-quoted attributes and padded expressions are measured in the file", () => {
    const source = [
      "const el = (",
      "  <Box {ok} data-x='1' icon={  <Icon />  }>",
      "    {// only a line comment",
      "    }",
      "    {/* one */ /* two */}",
      "    {ok && <b>x</b>}",
      "  </Box>",
      ");",
      "",
    ].join("\n");
    const options = { jsx: true, fileName: "a.tsx" };

    expect(textsOf(source, "JSXIdentifier", options)).toEqual([
      "Box", "data-x", "icon", "Icon", "b", "b", "Box",
    ]);
    expect(textsOf(source, "StringLiteral", options)).toEqual(["'1'"]);
    expect(textsOf(source, "Identifier", options)).toEqual(["el", "ok", "ok"]);
    expect(lexed(source, options)
      .filter(([type]) => type.endsWith("Comment"))
      .map(([, text]) => text)).toEqual(["// only a line comment", "/* one */", "/* two */"]);
    expect(textsOf(source, "JSXText", options).map((text) => text.trim()).filter(Boolean))
      .toEqual(["x"]);
    expect(uncovered(parse(source, { ...options, tokens: true }))).toEqual([]);
  });

  test("a construct this profile does not support is a SyntaxError, so no skipped code is tokenized", () => {
    expect(() => parse("var q = a / b / c;\n", { tokens: true })).toThrow(SyntaxError);
  });

  test("a TypeScript type declaration the parser skips is still tokenized whole", () => {
    expect(lexed("type T = `a${B}c`;\nconst x = 1;", { fileName: "a.ts" }).slice(0, 7)).toEqual([
      ["Identifier", "type"],
      ["Identifier", "T"],
      ["Punctuator", "="],
      ["TemplateHead", "`a${"],
      ["Identifier", "B"],
      ["TemplateTail", "}c`"],
      ["Punctuator", ";"],
    ]);
  });

  test("a syntax error still throws when tokens are asked for", () => {
    expect(() => parse("const a = ;\n", { tokens: true })).toThrow(SyntaxError);
  });

  // The tokens array is new, so its elements are created with
  // CreateDataPropertyOrThrow (ES2026 §7.3.6), which never consults the
  // prototype chain.
  test("tokens ignore an index setter or a read-only index on Array.prototype", () => {
    let calls = 0;
    Object.defineProperty(Array.prototype, "0", {
      set() { calls += 1; },
      configurable: true,
    });
    Object.defineProperty(Array.prototype, "1", {
      value: "inherited",
      writable: false,
      configurable: true,
    });
    let result;
    try {
      result = parse("a;\n", { tokens: true });
    } finally {
      delete Array.prototype[0];
      delete Array.prototype[1];
    }
    expect(calls).toBe(0);
    expect(Object.hasOwn(result.tokens, 0)).toBe(true);
    expect(Object.hasOwn(result.tokens, 1)).toBe(true);
    expect(result.tokens.map((token) => token.type)).toEqual(["Identifier", "Punctuator"]);
  });
});
