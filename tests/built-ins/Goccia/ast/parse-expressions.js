/*---
description: goccia:ast with expressions on reports expressions, properties, parameters and class members with ranges and the flags a layout rule dispatches on
features: [Goccia, experimental-ast]
---*/

import { parse } from "goccia:ast";

const parseExpressions = (source, options = {}) =>
  parse(source, { ...options, expressions: true });

/** The first node of a kind, depth first, in source order. */
const find = (node, kind) => {
  if (node.kind === kind) {
    return node;
  }

  return node.children.reduce(
    (found, child) => found ?? find(child, kind),
    undefined,
  );
};

const findAll = (node, kind) => [
  ...(node.kind === kind ? [node] : []),
  ...node.children.flatMap((child) => findAll(child, kind)),
];

const kinds = (node) => node.children.map((child) => child.kind);

const text = (result, node) => result.source.slice(node.start, node.end);

/** The only expression of the first statement. */
const firstExpression = (result) => result.root.children[0].children[0];

describe("ast.parse with expressions", () => {
  test("without the option the tree is statements only", () => {
    const result = parse("call(1, 2);\nconst a = [1];\n");

    expect(kinds(result.root)).toEqual([
      "ExpressionStatement",
      "VariableDeclaration",
    ]);
    expect(result.root.children[0].children).toEqual([]);
    expect(result.root.children[1].children).toEqual([]);
  });

  test("statements keep their kinds and ranges, and gain their expressions", () => {
    const source = "const a = f(1);\nreturnless(a);\n";
    const plain = parse(source);
    const result = parseExpressions(source);

    expect(kinds(result.root)).toEqual(kinds(plain.root));
    result.root.children.forEach((statement, index) => {
      expect(statement.start).toBe(plain.root.children[index].start);
      expect(statement.end).toBe(plain.root.children[index].end);
    });
    expect(kinds(result.root.children[0])).toEqual(["CallExpression"]);
  });

  test("a call reports its callee and arguments in source order", () => {
    const result = parseExpressions("f(a, 'b', ...c);\n");
    const call = firstExpression(result);

    expect(call.kind).toBe("CallExpression");
    expect(text(result, call)).toBe("f(a, 'b', ...c)");
    expect(call.optional).toBe(false);
    expect(kinds(call)).toEqual([
      "Identifier",
      "Identifier",
      "StringLiteral",
      "SpreadElement",
    ]);
  });

  test("an optional call is flagged", () => {
    const result = parseExpressions("f?.(1);\n");

    expect(firstExpression(result).optional).toBe(true);
  });

  test("a member expression reports computed and optional", () => {
    const result = parseExpressions("a.b;\na?.[c];\n");
    const dot = firstExpression(result);
    const bracket = result.root.children[1].children[0];

    expect(dot.kind).toBe("MemberExpression");
    expect(dot.computed).toBe(false);
    expect(dot.optional).toBe(false);
    expect(text(result, dot)).toBe("a.b");
    expect(bracket.computed).toBe(true);
    expect(bracket.optional).toBe(true);
    expect(text(result, bracket)).toBe("a?.[c]");
    expect(kinds(bracket)).toEqual(["Identifier", "Identifier"]);
  });

  test("a private member is a member expression that is not computed", () => {
    const result = parseExpressions(
      "class K {\n  #x = 1;\n  m() { return this.#x; }\n}\n",
    );
    const member = find(result.root, "MemberExpression");

    expect(text(result, member)).toBe("this.#x");
    expect(member.computed).toBe(false);
  });

  test("a brand check's private name has no receiver", () => {
    const result = parseExpressions(
      "class K {\n  #x;\n  static has(o) { return #x in o; }\n}\n",
    );
    const check = find(result.root, "BinaryExpression");
    const name = check.children[0];

    expect(text(result, check)).toBe("#x in o");
    expect(name.kind).toBe("MemberExpression");
    expect(text(result, name)).toBe("#x");
    expect(name.children).toEqual([]);
    expect(findAll(result.root, "ThisExpression")).toEqual([]);
  });

  test("an arrow function reports async and its body kind", () => {
    const result = parseExpressions(
      "const f = async (a) => a;\nconst g = () => {\n  return 1;\n};\n",
    );
    const concise = find(result.root.children[0], "ArrowFunctionExpression");
    const block = find(result.root.children[1], "ArrowFunctionExpression");

    expect(text(result, concise)).toBe("async (a) => a");
    expect(concise.async).toBe(true);
    expect(concise.bodyKind).toBe("expression");
    expect(block.async).toBe(false);
    expect(block.bodyKind).toBe("block");
    expect(kinds(block)).toEqual(["BlockStatement"]);
  });

  test("a generic arrow function starts at its type parameter list", () => {
    const result = parseExpressions("const f = <T>(v: T): T => v;\n", {
      fileName: "a.ts",
    });
    const arrow = find(result.root, "ArrowFunctionExpression");

    expect(text(result, arrow)).toBe("<T>(v: T): T => v");
  });

  test("an unparenthesized arrow parameter is a parameter", () => {
    const result = parseExpressions("const f = x => x;\n");
    const arrow = find(result.root, "ArrowFunctionExpression");

    expect(kinds(arrow)).toEqual(["Parameter", "Identifier"]);
    expect(text(result, arrow.children[0])).toBe("x");
  });

  test("parameters cover their default, and a rest parameter is flagged", () => {
    const result = parseExpressions("const f = (a, b = 2, ...c) => 0;\n");
    const parameters = findAll(result.root, "Parameter");

    expect(parameters.map((parameter) => text(result, parameter))).toEqual([
      "a",
      "b = 2",
      "...c",
    ]);
    expect(parameters.map((parameter) => parameter.rest)).toEqual([
      false,
      false,
      true,
    ]);
    expect(kinds(parameters[1])).toEqual(["NumericLiteral"]);
  });

  test("a parameter's range includes its type annotation", () => {
    const result = parseExpressions("const f = (a: string, b?: number) => a;\n", {
      fileName: "probe.ts",
    });
    const parameters = findAll(result.root, "Parameter");

    expect(parameters.map((parameter) => text(result, parameter))).toEqual([
      "a: string",
      "b?: number",
    ]);
  });

  test("a destructuring parameter holds its pattern", () => {
    const result = parseExpressions("const f = ({ a, b }, [c]) => a;\n");
    const parameters = findAll(result.root, "Parameter");

    expect(parameters.map((parameter) => kinds(parameter)[0])).toEqual([
      "ObjectPattern",
      "ArrayPattern",
    ]);
  });

  test("an object reports each member as a Property with its flags", () => {
    const source =
      "const o = { a: 1, b, [c]: 2, d() { return 1; }, get e() { return 2; }, ...f };\n";
    const result = parseExpressions(source);
    const object = find(result.root, "ObjectExpression");

    expect(text(result, object)).toBe(source.slice(10, source.length - 2));
    expect(kinds(object)).toEqual([
      "Property",
      "Property",
      "Property",
      "Property",
      "Property",
      "SpreadElement",
    ]);

    const [plain, shorthand, computed, method, getter] = object.children;

    expect(text(result, plain)).toBe("a: 1");
    expect([plain.computed, plain.shorthand, plain.method]).toEqual([
      false,
      false,
      false,
    ]);
    expect(shorthand.shorthand).toBe(true);
    expect(computed.computed).toBe(true);
    expect(kinds(computed)).toEqual(["Identifier", "NumericLiteral"]);
    expect(method.method).toBe(true);
    expect(text(result, method)).toBe("d() { return 1; }");
    expect(kinds(method)).toEqual(["BlockStatement"]);
    expect(text(result, getter)).toBe("get e() { return 2; }");
    expect(getter.method).toBe(true);
  });

  test("an object with a spread still starts at its brace", () => {
    const result = parseExpressions("const o = { ...a, b: 1 };\n");

    expect(text(result, find(result.root, "ObjectExpression"))).toBe(
      "{ ...a, b: 1 }",
    );
  });

  test("an array reports its elements and skips holes", () => {
    const result = parseExpressions("x = [1, , 'two', ...rest];\n");
    const array = find(result.root, "ArrayExpression");

    expect(text(result, array)).toBe("[1, , 'two', ...rest]");
    expect(kinds(array)).toEqual([
      "NumericLiteral",
      "StringLiteral",
      "SpreadElement",
    ]);
  });

  test("binary and logical expressions report their operator", () => {
    const result = parseExpressions("x = a + b * c;\ny = a && b || c;\n");
    const sum = find(result.root.children[0], "BinaryExpression");
    const logical = find(result.root.children[1], "LogicalExpression");

    expect(sum.operator).toBe("+");
    expect(text(result, sum)).toBe("a + b * c");
    expect(sum.children[1].operator).toBe("*");
    expect(logical.operator).toBe("||");
    expect(logical.children[0].operator).toBe("&&");
  });

  test("nullish coalescing is a logical expression", () => {
    const result = parseExpressions("x = a ?? b;\n");

    expect(find(result.root, "LogicalExpression").operator).toBe("??");
  });

  test("parentheses are a node of their own", () => {
    const result = parseExpressions("x = (a + b) * c;\n");
    const product = find(result.root, "BinaryExpression");
    const parenthesized = product.children[0];

    expect(product.operator).toBe("*");
    expect(text(result, product)).toBe("(a + b) * c");
    expect(parenthesized.kind).toBe("ParenthesizedExpression");
    expect(text(result, parenthesized)).toBe("(a + b)");
    expect(text(result, parenthesized.children[0])).toBe("a + b");
  });

  test("nested parentheses are reported once, at the outer pair", () => {
    const result = parseExpressions("x = ((a));\n");
    const parenthesized = find(result.root, "ParenthesizedExpression");

    expect(text(result, parenthesized)).toBe("((a))");
    expect(kinds(parenthesized)).toEqual(["Identifier"]);
  });

  test("a TypeScript assertion covers its type, around the expression it asserts", () => {
    const source =
      "sendParcel(\n  box.parcelId as ParcelId,\n  shapeOf(\n    box\n  ) as Record<string, string>,\n  box as Tally & {\n    extra: boolean;\n  },\n  box.tally satisfies Tally,\n  box.tally!,\n  'p-1' as const\n);\n";
    const result = parseExpressions(source, { fileName: "parcels.ts" });
    const call = find(result.root, "CallExpression");
    const args = call.children.slice(1);

    expect(kinds(call).slice(1)).toEqual([
      "TSAsExpression",
      "TSAsExpression",
      "TSAsExpression",
      "TSSatisfiesExpression",
      "TSNonNullExpression",
      "TSAsExpression",
    ]);
    expect(args.map((arg) => text(result, arg))).toEqual([
      "box.parcelId as ParcelId",
      "shapeOf(\n    box\n  ) as Record<string, string>",
      "box as Tally & {\n    extra: boolean;\n  }",
      "box.tally satisfies Tally",
      "box.tally!",
      "'p-1' as const",
    ]);
    expect(text(result, args[0].children[0])).toBe("box.parcelId");
    expect(args[5].children[0].kind).toBe("StringLiteral");
    expect(text(result, args[5].children[0])).toBe("'p-1'");
  });

  test("assertions and parentheses nest in the order they are written", () => {
    const source = "f((x as A)!, (x) as A, ((x) as A), x as A as B, a.b!.c);\n";
    const result = parseExpressions(source, { fileName: "nesting.ts" });
    const args = find(result.root, "CallExpression").children.slice(1);
    const shape = (node) =>
      `${node.kind}(${text(result, node)})${node.children.length > 0 ? ` > ${shape(node.children[0])}` : ""}`;

    expect(args.map(shape)).toEqual([
      "TSNonNullExpression((x as A)!) > ParenthesizedExpression((x as A)) > TSAsExpression(x as A) > Identifier(x)",
      "TSAsExpression((x) as A) > ParenthesizedExpression((x)) > Identifier(x)",
      "ParenthesizedExpression(((x) as A)) > TSAsExpression((x) as A) > Identifier(x)",
      "TSAsExpression(x as A as B) > TSAsExpression(x as A) > Identifier(x)",
      "MemberExpression(a.b!.c) > TSNonNullExpression(a.b!) > MemberExpression(a.b) > Identifier(a)",
    ]);
  });

  test("an assertion is not reported without expressions", () => {
    const result = parse("f(x as T);\n", { fileName: "plain.ts" });

    expect(findAll(result.root, "TSAsExpression")).toEqual([]);
  });

  test("parentheses around an optional chain are reported", () => {
    const result = parseExpressions("x = (a?.b).c;\n");
    const parenthesized = find(result.root, "ParenthesizedExpression");

    expect(text(result, parenthesized)).toBe("(a?.b)");
    expect(kinds(parenthesized)).toEqual(["MemberExpression"]);
  });

  test("a conditional reports its three operands", () => {
    const result = parseExpressions("x = a ? b : c;\n");
    const conditional = find(result.root, "ConditionalExpression");

    expect(text(result, conditional)).toBe("a ? b : c");
    expect(kinds(conditional)).toEqual(["Identifier", "Identifier", "Identifier"]);
  });

  test("assignments report their operator and their target", () => {
    const result = parseExpressions("x = 1;\no.p += 2;\na[i] ??= 3;\n");
    const assignments = findAll(result.root, "AssignmentExpression");

    expect(assignments.map((assignment) => assignment.operator)).toEqual([
      "=",
      "+=",
      "??=",
    ]);
    expect(assignments.map((assignment) => text(result, assignment))).toEqual([
      "x = 1",
      "o.p += 2",
      "a[i] ??= 3",
    ]);
    expect(kinds(assignments[1])).toEqual(["MemberExpression", "NumericLiteral"]);
  });

  test("unary and update expressions report their operator", () => {
    const result = parseExpressions("x = !a;\ni++;\n--j;\ny = typeof z;\n");
    const updates = findAll(result.root, "UpdateExpression");
    const unaries = findAll(result.root, "UnaryExpression");

    expect(unaries.map((unary) => unary.operator)).toEqual(["!", "typeof"]);
    expect(updates.map((update) => [update.operator, update.prefix])).toEqual([
      ["++", false],
      ["--", true],
    ]);
  });

  test("literals are reported by kind, with their quotes in the range", () => {
    const result = parseExpressions(
      "x = ['a', \"b\", 1, 2n, true, null, /re/g, `t`];\n",
    );

    expect(kinds(find(result.root, "ArrayExpression"))).toEqual([
      "StringLiteral",
      "StringLiteral",
      "NumericLiteral",
      "BigIntLiteral",
      "BooleanLiteral",
      "NullLiteral",
      "RegExpLiteral",
      "TemplateLiteral",
    ]);
    expect(
      findAll(result.root, "StringLiteral").map((literal) => text(result, literal)),
    ).toEqual(["'a'", '"b"']);
  });

  test("a template literal covers its whole text and holds its substitutions", () => {
    const result = parseExpressions("x = `a${b}c${d + 1}e`;\n");
    const template = find(result.root, "TemplateLiteral");

    expect(text(result, template)).toBe("`a${b}c${d + 1}e`");
    expect(kinds(template)).toEqual(["Identifier", "BinaryExpression"]);
  });

  test("a tagged template reports its tag and substitutions", () => {
    const result = parseExpressions("x = tag`a${b}`;\n");
    const tagged = find(result.root, "TaggedTemplateExpression");

    expect(text(result, tagged)).toBe("tag`a${b}`");
    expect(kinds(tagged)).toEqual(["Identifier", "Identifier"]);
  });

  test("a tagged template that spans lines covers its whole template", () => {
    const result = parseExpressions("x = String.raw`\nfirst\nsecond`;\n");

    expect(text(result, find(result.root, "TaggedTemplateExpression"))).toBe(
      "String.raw`\nfirst\nsecond`",
    );
  });

  test("new, this, sequence and await are reported", () => {
    const result = parseExpressions(
      "x = new Foo(this, 1);\ny = (a, b);\nawait z;\n",
    );

    expect(text(result, find(result.root, "NewExpression"))).toBe(
      "new Foo(this, 1)",
    );
    expect(find(result.root, "ThisExpression")).toBeDefined();
    expect(text(result, find(result.root, "SequenceExpression"))).toBe("a, b");
    expect(text(result, find(result.root, "AwaitExpression"))).toBe("await z");
  });

  test("the module path of an import or re-export is a string literal", () => {
    const result = parseExpressions(
      "import a from \"./a.js\";\nexport { b } from './b.js';\n",
    );

    expect(kinds(result.root.children[0])).toEqual(["StringLiteral"]);
    expect(text(result, result.root.children[0].children[0])).toBe('"./a.js"');
    expect(text(result, result.root.children[1].children[0])).toBe("'./b.js'");
  });

  test("class members are reported with static and computed", () => {
    const source = [
      "class K extends Base {",
      "  #p = 1;",
      "  static s = 'q';",
      "  constructor(a) { super(a); }",
      "  async *m(x) { yield x; }",
      "  get g() { return 1; }",
      "  static [key]() {}",
      "}",
      "",
    ].join("\n");
    const result = parseExpressions(source);
    const declaration = result.root.children[0];

    expect(kinds(declaration)).toEqual([
      "Identifier",
      "PropertyDefinition",
      "PropertyDefinition",
      "MethodDefinition",
      "MethodDefinition",
      "MethodDefinition",
      "MethodDefinition",
    ]);

    const members = declaration.children.slice(1);

    expect(members.map((member) => text(result, member))).toEqual([
      "#p = 1;",
      "static s = 'q';",
      "constructor(a) { super(a); }",
      "async *m(x) { yield x; }",
      "get g() { return 1; }",
      "static [key]() {}",
    ]);
    expect(members.map((member) => member.static)).toEqual([
      false,
      true,
      false,
      false,
      false,
      true,
    ]);
    expect(members[5].computed).toBe(true);
    expect(kinds(members[2])).toEqual(["Parameter", "BlockStatement"]);
    expect(find(members[2], "Super")).toBeDefined();
    expect(find(members[3], "YieldExpression")).toBeDefined();
  });

  test("a class member's range starts at its decorator", () => {
    const result = parseExpressions(
      "const dec = (v) => v;\nclass K {\n  @dec m() {}\n}\n",
    );
    const method = find(result.root, "MethodDefinition");

    expect(text(result, method)).toBe("@dec m() {}");
    expect(kinds(method)).toEqual(["Identifier", "BlockStatement"]);
  });

  test("a decorated field is reported once", () => {
    const result = parseExpressions(
      "const keep = (v) => v;\nclass K {\n  @keep x = 1;\n  @keep #y = 2;\n}\n",
    );
    const fields = findAll(result.root, "PropertyDefinition");

    expect(fields.map((field) => text(result, field))).toEqual([
      "@keep x = 1;",
      "@keep #y = 2;",
    ]);
    expect(fields.map((field) => kinds(field))).toEqual([
      ["Identifier", "NumericLiteral"],
      ["Identifier", "NumericLiteral"],
    ]);
  });

  test("an accessor field reports only what the source holds", () => {
    const result = parseExpressions("class A {\n  accessor z = 1;\n}\n");
    const accessor = find(result.root, "AccessorProperty");

    expect(text(result, accessor)).toBe("accessor z = 1;");
    expect(kinds(accessor)).toEqual(["NumericLiteral"]);
    expect(findAll(result.root, "BlockStatement")).toEqual([]);
  });

  test("a decorated class starts at its decorator and holds it", () => {
    const result = parseExpressions(
      "const d = (v) => v;\n@d class C {}\nconst e = @d class {};\n",
    );
    const declaration = result.root.children[1];
    const expression = find(result.root.children[2], "ClassExpression");

    expect(declaration.kind).toBe("ClassDeclaration");
    expect(text(result, declaration)).toBe("@d class C {}");
    expect(kinds(declaration)).toEqual(["Identifier"]);
    expect(text(result, expression)).toBe("@d class {}");
  });

  test("a destructuring assignment's defaults are located at their targets", () => {
    const result = parseExpressions("[a = 1, o.b = 2] = [];\n({ c = 3 } = {});\n");
    const defaults = findAll(result.root, "AssignmentPattern");

    expect(defaults.map((pattern) => text(result, pattern))).toEqual([
      "a = 1",
      "o.b = 2",
      "c = 3",
    ]);
    expect(defaults.map((pattern) => kinds(pattern))).toEqual([
      ["Identifier", "NumericLiteral"],
      ["MemberExpression", "NumericLiteral"],
      ["Identifier", "NumericLiteral"],
    ]);
    expect(text(result, defaults[0].children[0])).toBe("a");
    expect(text(result, defaults[1].children[0])).toBe("o.b");
  });

  test("meta properties and import() cover their whole text", () => {
    const result = parseExpressions(
      "const u = import.meta.url;\nconst m = import('./m.js');\n",
    );

    expect(text(result, find(result.root, "MetaProperty"))).toBe("import.meta");
    expect(text(result, find(result.root, "ImportExpression"))).toBe(
      "import('./m.js')",
    );
  });

  test("a static block is reported", () => {
    const result = parseExpressions("class K {\n  static { init(); }\n}\n");
    const block = find(result.root, "StaticBlock");

    expect(text(result, block)).toBe("static { init(); }");
    expect(kinds(block)).toEqual(["BlockStatement"]);
  });

  test("a function expression reports async, generator, parameters and body", () => {
    const result = parseExpressions("export const f = function named(a) { return a; };\n");
    const fn = find(result.root, "FunctionExpression");

    expect(text(result, fn)).toBe("function named(a) { return a; }");
    expect(fn.async).toBe(false);
    expect(fn.generator).toBe(false);
    expect(kinds(fn)).toEqual(["Parameter", "BlockStatement"]);
  });

  test("children nest inside their parent and siblings do not overlap", () => {
    const source =
      "const r = items.filter((i) => i.ok).map((i) => ({ id: i.id, tags: [...i.tags] }));\n";
    const result = parseExpressions(source);
    const check = (node) => {
      node.children.forEach((child, index) => {
        expect(child.start >= node.start).toBe(true);
        expect(child.end <= node.end).toBe(true);
        if (index > 0) {
          expect(child.start >= node.children[index - 1].end).toBe(true);
        }
        check(child);
      });
    };

    check(result.root);
  });

  test("loc is the position of the range in the source", () => {
    const result = parseExpressions("const a = 1;\nconst b = f(\n  2,\n);\n");
    const call = find(result.root, "CallExpression");

    expect(call.loc.start).toEqual({ line: 2, column: 11 });
    expect(call.loc.end).toEqual({ line: 4, column: 2 });
  });

  test("comments are unaffected by the option", () => {
    const source = "// one\nf(/* two */ 1);\n";

    expect(parseExpressions(source).comments).toEqual(parse(source).comments);
  });

  test("with jsx on, an element is a JSXElement measured in the file", () => {
    const source =
      "const view = (\n  <div className=\"a\" onClick={() => go(1)}>\n    {items.map((i) => <Item key={i} />)}\n  </div>\n);\n";
    const result = parseExpressions(source, { jsx: true, fileName: "view.tsx" });
    const elements = findAll(result.root, "JSXElement");

    expect(elements.map((element) => text(result, element))).toEqual([
      source.slice(source.indexOf("<div"), source.indexOf("</div>") + 6),
      "<Item key={i} />",
    ]);
    expect(kinds(elements[0])).toEqual([
      "ArrowFunctionExpression",
      "CallExpression",
    ]);
    expect(text(result, find(elements[0], "CallExpression"))).toBe("go(1)");
  });

  test("with jsx on, text the transformer wrote is not reported", () => {
    const source = "const view = <a href=\"x\" title='y' />;\n";
    const result = parseExpressions(source, { jsx: true, fileName: "view.tsx" });

    expect(findAll(result.root, "StringLiteral")).toEqual([]);
    expect(findAll(result.root, "ObjectExpression")).toEqual([]);
  });

  test("a deeply nested expression does not exhaust the stack", () => {
    const depth = 400;
    const source = `x = ${"(".repeat(depth)}1${")".repeat(depth)};\n`;
    const result = parseExpressions(source);

    expect(find(result.root, "NumericLiteral")).toBeDefined();
  });

  test("a type assertion chain taller than the tree height limit is a SyntaxError", () => {
    for (const link of ["!", " as T", " satisfies T"]) {
      expect(() =>
        parseExpressions(`a${link.repeat(30000)};\n`, { fileName: "a.ts" }),
      ).toThrow(SyntaxError);
    }
  });

  test("a long operator chain is reported link by link", () => {
    const terms = Array.from({ length: 300 }, (_, index) => `a${index}`);
    const result = parseExpressions(`x = ${terms.join(" + ")};\n`);

    expect(findAll(result.root, "BinaryExpression").length).toBe(299);
    expect(findAll(result.root, "Identifier").length).toBe(301);
  });
});

describe("ast.parse with expressions beside type-level declarations and tokens", () => {
  const lines = [
    'import type { A } from "./a.ts";',
    'import { type C, D } from "./c.ts";',
    "export type { A };",
    'export type * from "./star.ts";',
    "export type T = string | number;",
    "export interface I { x: number }",
    "type U = { a: 1 };",
    "interface J { y: string }",
    "const sum = (a: number) => a + 1;",
  ];
  const source = lines.join("\n") + "\n";

  test("the statements keep the kinds and typeOnly the statement view gives them, with no children of their own", () => {
    const statements = parseExpressions(source, { fileName: "types.ts" }).root
      .children;

    expect(
      statements.map((node) => [node.kind, node.typeOnly, node.children.length]),
    ).toEqual([
      ["ImportDeclaration", true, 0],
      ["ImportDeclaration", false, 1],
      ["ExportDeclaration", true, 0],
      ["ReExportDeclaration", true, 0],
      ["ExportTypeAliasDeclaration", true, 0],
      ["ExportInterfaceDeclaration", true, 0],
      ["TypeAliasDeclaration", true, 0],
      ["InterfaceDeclaration", true, 0],
      ["VariableDeclaration", undefined, 1],
    ]);
    statements.slice(0, 8).forEach((node, index) => {
      expect(text({ source }, node)).toBe(lines[index]);
    });
  });

  test("expression nodes carry no typeOnly", () => {
    const result = parseExpressions(source, { fileName: "types.ts" });
    const expressionKinds = findAll(result.root, "ArrowFunctionExpression");

    expect(expressionKinds.length).toBe(1);
    expect(expressionKinds[0].typeOnly).toBeUndefined();
  });

  test("tokens come back with the expression view, and are the same without it", () => {
    const withExpressions = parseExpressions(source, {
      fileName: "types.ts",
      tokens: true,
    });
    const statementsOnly = parse(source, { fileName: "types.ts", tokens: true });

    expect(withExpressions.tokens.length).toBeGreaterThan(0);
    expect(withExpressions.tokens).toEqual(statementsOnly.tokens);
    expect(parseExpressions(source, { fileName: "types.ts" }).tokens)
      .toBeUndefined();
  });
});
