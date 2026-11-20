/*---
description: goccia:ast with expressions on reports erased type syntax as nodes with ranges, children of what they annotate
features: [Goccia, experimental-ast]
---*/

import { parse } from "goccia:ast";

const parseTypes = (source, options = {}) =>
  parse(source, { fileName: "types.ts", ...options, expressions: true });

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

const all = (node) => [node, ...node.children.flatMap(all)];

const text = (result, node) => result.source.slice(node.start, node.end);

/** `kind(text)` of every child, in order. */
const shapeOf = (result, node) =>
  node.children.map((child) => `${child.kind}(${text(result, child)})`);

/** `kind(text)` of a node and everything beneath it, depth first. */
const treeOf = (result, node) =>
  all(node).map((each) => `${each.kind}(${text(result, each)})`);

describe("ast.parse with expressions: type syntax", () => {
  test("a union of string literal types reports each literal with its quotes", () => {
    const result = parseTypes("type Shade = 'light' | \"dark\";\n");
    const alias = result.root.children[0];

    expect(alias.kind).toBe("TypeAliasDeclaration");
    expect(treeOf(result, alias.children[0])).toEqual([
      "TSUnionType('light' | \"dark\")",
      "TSLiteralType('light')",
      "StringLiteral('light')",
      "TSLiteralType(\"dark\")",
      "StringLiteral(\"dark\")",
    ]);
  });

  test("a leading `|` belongs to the union", () => {
    const result = parseTypes("type Mode =\n  | 'read'\n  | 'write';\n");
    const union = find(result.root, "TSUnionType");

    expect(text(result, union)).toBe("| 'read'\n  | 'write'");
    expect(union.children.map((child) => child.kind)).toEqual([
      "TSLiteralType",
      "TSLiteralType",
    ]);
  });

  test("an object type reports its members, each ending before its separator", () => {
    const source =
      "type Crate = {\n  readonly id: string;\n  'shelf-label'?: number,\n  2: boolean;\n  [slot]: Item;\n  open(lid: Lid): void;\n  (count: number): Crate;\n  new (size: number): Crate;\n  [key: string]: unknown,\n};\n";
    const result = parseTypes(source);
    const literal = find(result.root, "TSTypeLiteral");

    expect(shapeOf(result, literal)).toEqual([
      "TSPropertySignature(readonly id: string)",
      "TSPropertySignature('shelf-label'?: number)",
      "TSPropertySignature(2: boolean)",
      "TSPropertySignature([slot]: Item)",
      "TSMethodSignature(open(lid: Lid): void)",
      "TSCallSignatureDeclaration((count: number): Crate)",
      "TSConstructSignatureDeclaration(new (size: number): Crate)",
      "TSIndexSignature([key: string]: unknown)",
    ]);
    expect(
      result.source.slice(literal.children.at(-1).end, literal.end),
    ).toBe(",\n}");
  });

  test("a member's static key is a node, and a computed member is flagged", () => {
    const result = parseTypes(
      "type Crate = { id: 1; 'shelf-label': 2; 3: 3; [slot]: 4; get size(): number };\n",
    );
    const members = findAll(result.root, "TSPropertySignature").concat(
      findAll(result.root, "TSMethodSignature"),
    );

    expect(members.map((member) => member.computed)).toEqual([
      false,
      false,
      false,
      true,
      false,
    ]);
    expect(members.map((member) => member.children[0].kind)).toEqual([
      "Identifier",
      "StringLiteral",
      "NumericLiteral",
      "TSTypeAnnotation",
      "Identifier",
    ]);
    expect(text(result, members[1].children[0])).toBe("'shelf-label'");
  });

  test("a tuple reports its elements: named, optional and rest", () => {
    const result = parseTypes(
      "type Row = [first: string, second?: number, Flag?, ...rest: boolean[]];\n",
    );
    const tuple = find(result.root, "TSTupleType");

    expect(shapeOf(result, tuple)).toEqual([
      "TSNamedTupleMember(first: string)",
      "TSNamedTupleMember(second?: number)",
      "TSOptionalType(Flag?)",
      "TSRestType(...rest: boolean[])",
    ]);
    expect(treeOf(result, tuple.children[3]).slice(1)).toEqual([
      "TSNamedTupleMember(rest: boolean[])",
      "TSArrayType(boolean[])",
      "TSBooleanKeyword(boolean)",
    ]);
  });

  test("type arguments are a list of types, and `>>` closes two of them", () => {
    const result = parseTypes("let shelves: Map<string, Array<Crate>>;\n");
    const lists = findAll(result.root, "TSTypeParameterInstantiation");

    expect(lists.map((list) => text(result, list))).toEqual([
      "<string, Array<Crate>>",
      "<Crate>",
    ]);
    expect(shapeOf(result, lists[0])).toEqual([
      "TSStringKeyword(string)",
      "TSTypeReference(Array<Crate>)",
    ]);
  });

  test("a call's and a constructor's type arguments are their children, between callee and arguments", () => {
    const result = parseTypes(
      "const crates = load<Crate, 'fresh'>(shelf);\nconst index = new Map<string, Crate>();\n",
    );
    const call = find(result.root, "CallExpression");
    const construct = find(result.root, "NewExpression");

    expect(shapeOf(result, call)).toEqual([
      "Identifier(load)",
      "TSTypeParameterInstantiation(<Crate, 'fresh'>)",
      "Identifier(shelf)",
    ]);
    expect(shapeOf(result, construct)).toEqual([
      "Identifier(Map)",
      "TSTypeParameterInstantiation(<string, Crate>)",
    ]);
    expect(find(call, "StringLiteral")).toBeDefined();
  });

  test("the type arguments of a call inside another call's arguments stay with their own call", () => {
    const result = parseTypes("pack<Box>(wrap<Paper>(item));\n");
    const calls = findAll(result.root, "CallExpression");

    expect(calls.map((call) => shapeOf(result, call)[1])).toEqual([
      "TSTypeParameterInstantiation(<Box>)",
      "TSTypeParameterInstantiation(<Paper>)",
    ]);
  });

  test("type parameters report their constraint and default", () => {
    const result = parseTypes(
      "function stack<const T extends Crate = Crate, U>(top: T): U { return top as U; }\n",
    );
    const declaration = find(result.root, "TSTypeParameterDeclaration");

    expect(treeOf(result, declaration)).toEqual([
      "TSTypeParameterDeclaration(<const T extends Crate = Crate, U>)",
      "TSTypeParameter(const T extends Crate = Crate)",
      "TSTypeReference(Crate)",
      "TSTypeReference(Crate)",
      "TSTypeParameter(U)",
    ]);
  });

  test("functions, arrows, methods and classes hold their type parameters and return type", () => {
    const source = [
      "function first<T>(items: T[]): T { return items[0]; }",
      "const last = <T,>(items: T[]): T => items[items.length - 1];",
      "class Shelf<T> extends Rack<T> implements Store<T>, Labelled {",
      "  label: string = '';",
      "  get size(): number { return 0; }",
      "  take<K extends keyof T>(key: K): T[K] { return this.items[key]; }",
      "}",
    ].join("\n");
    const result = parseTypes(`${source}\n`);
    const [declaration, variable, shelf] = result.root.children;
    const arrow = find(variable, "ArrowFunctionExpression");
    const members = shelf.children.filter((child) =>
      ["PropertyDefinition", "MethodDefinition"].includes(child.kind),
    );

    expect(shapeOf(result, declaration)).toEqual([
      "TSTypeParameterDeclaration(<T>)",
      "Parameter(items: T[])",
      "TSTypeAnnotation(: T)",
      "BlockStatement({ return items[0]; })",
    ]);
    expect(shapeOf(result, arrow).slice(0, 3)).toEqual([
      "TSTypeParameterDeclaration(<T,>)",
      "Parameter(items: T[])",
      "TSTypeAnnotation(: T)",
    ]);
    expect(shapeOf(result, shelf).slice(0, 5)).toEqual([
      "TSTypeParameterDeclaration(<T>)",
      "Identifier(Rack)",
      "TSTypeParameterInstantiation(<T>)",
      "TSClassImplements(Store<T>)",
      "TSClassImplements(Labelled)",
    ]);
    expect(members.map((member) => shapeOf(result, member))).toEqual([
      ["Identifier(label)", "TSTypeAnnotation(: string)", "StringLiteral('')"],
      ["Identifier(size)", "TSTypeAnnotation(: number)", "BlockStatement({ return 0; })"],
      [
        "Identifier(take)",
        "TSTypeParameterDeclaration(<K extends keyof T>)",
        "Parameter(key: K)",
        "TSTypeAnnotation(: T[K])",
        "BlockStatement({ return this.items[key]; })",
      ],
    ]);
  });

  test("a parameter's annotation is its child, after its pattern and before its default", () => {
    const result = parseTypes(
      "const open = ({ lid }: Crate, depth: number = 1, ...rest: Item[]) => lid;\n",
    );
    const parameters = findAll(result.root, "Parameter");

    expect(parameters.map((parameter) => shapeOf(result, parameter))).toEqual([
      ["ObjectPattern({ lid })", "TSTypeAnnotation(: Crate)"],
      ["TSTypeAnnotation(: number)", "NumericLiteral(1)"],
      ["TSTypeAnnotation(: Item[])"],
    ]);
  });

  test("a variable's annotation is a child of its declaration, between its name and initializer", () => {
    const result = parseTypes("let count: number = 1, label: string;\n");

    expect(shapeOf(result, result.root.children[0])).toEqual([
      "TSTypeAnnotation(: number)",
      "NumericLiteral(1)",
      "TSTypeAnnotation(: string)",
    ]);
  });

  test("an assertion's type follows the expression it asserts", () => {
    const result = parseTypes(
      "use(crate as Crate<'fresh'>, list satisfies Item[], 'k' as const);\n",
    );
    const args = find(result.root, "CallExpression").children.slice(1);

    expect(args.map((arg) => shapeOf(result, arg))).toEqual([
      ["Identifier(crate)", "TSTypeReference(Crate<'fresh'>)"],
      ["Identifier(list)", "TSArrayType(Item[])"],
      ["StringLiteral('k')", "TSTypeReference(const)"],
    ]);
  });

  test("function and constructor types report their parameters and the type after `=>`", () => {
    const result = parseTypes(
      "type Handler = (this: Window, event: Event, ...extra: unknown[]) => void;\ntype Maker = abstract new <T>(size: T) => Crate;\n",
    );
    const [handler, maker] = result.root.children.map((alias) => alias.children[0]);

    expect(shapeOf(result, handler)).toEqual([
      "Parameter(this: Window)",
      "Parameter(event: Event)",
      "Parameter(...extra: unknown[])",
      "TSTypeAnnotation(=> void)",
    ]);
    expect(findAll(handler, "Parameter").map((parameter) => parameter.rest)).toEqual([
      false,
      false,
      true,
    ]);
    expect(maker.kind).toBe("TSConstructorType");
    expect(shapeOf(result, maker)).toEqual([
      "TSTypeParameterDeclaration(<T>)",
      "Parameter(size: T)",
      "TSTypeAnnotation(=> Crate)",
    ]);
  });

  test("an indexed access reports its object type and its key", () => {
    const result = parseTypes("type Label = Crate[\"label\"][number];\n");
    const outer = find(result.root, "TSIndexedAccessType");

    expect(treeOf(result, outer)).toEqual([
      "TSIndexedAccessType(Crate[\"label\"][number])",
      "TSIndexedAccessType(Crate[\"label\"])",
      "TSTypeReference(Crate)",
      "TSLiteralType(\"label\")",
      "StringLiteral(\"label\")",
      "TSNumberKeyword(number)",
    ]);
  });

  test("an interface reports its type parameters, heritage and body", () => {
    const result = parseTypes(
      "export interface Crate<T> extends Box<T>, Labelled {\n  contents: T;\n}\n",
    );
    const crate = result.root.children[0];

    expect(crate.kind).toBe("ExportInterfaceDeclaration");
    expect(shapeOf(result, crate)).toEqual([
      "TSTypeParameterDeclaration(<T>)",
      "TSInterfaceHeritage(Box<T>)",
      "TSInterfaceHeritage(Labelled)",
      "TSInterfaceBody({\n  contents: T;\n})",
    ]);
  });

  test("an interface whose type parameter defaults to an object type ends at its own body", () => {
    const result = parseTypes(
      "interface Box<T = {}> extends Base<{ id: string }> {\n  contents: T;\n}\nconst after = 1;\n",
    );
    const [box, after] = result.root.children;

    expect(box.kind).toBe("InterfaceDeclaration");
    expect(shapeOf(result, box)).toEqual([
      "TSTypeParameterDeclaration(<T = {}>)",
      "TSInterfaceHeritage(Base<{ id: string }>)",
      "TSInterfaceBody({\n  contents: T;\n})",
    ]);
    expect(after.kind).toBe("VariableDeclaration");
  });

  test("a mapped type reports its constraint, name type and value type", () => {
    const result = parseTypes(
      "type Getters = { readonly [K in keyof Crate as `get${K}`]?: Crate[K] };\n",
    );

    expect(treeOf(result, result.root.children[0].children[0])).toEqual([
      "TSMappedType({ readonly [K in keyof Crate as `get${K}`]?: Crate[K] })",
      "TSTypeOperator(keyof Crate)",
      "TSTypeReference(Crate)",
      "TSTemplateLiteralType(`get${K}`)",
      "TSTypeReference(K)",
      "TSIndexedAccessType(Crate[K])",
      "TSTypeReference(Crate)",
      "TSTypeReference(K)",
    ]);
  });

  test("a conditional type reports its four types, and infer its parameter", () => {
    const result = parseTypes("type Item<T> = T extends Array<infer U> ? U : never;\n");

    expect(treeOf(result, find(result.root, "TSConditionalType"))).toEqual([
      "TSConditionalType(T extends Array<infer U> ? U : never)",
      "TSTypeReference(T)",
      "TSTypeReference(Array<infer U>)",
      "TSTypeParameterInstantiation(<infer U>)",
      "TSInferType(infer U)",
      "TSTypeParameter(U)",
      "TSTypeReference(U)",
      "TSNeverKeyword(never)",
    ]);
  });

  test("type queries, import types and type operators", () => {
    const result = parseTypes(
      "type A = typeof shelf.crate;\ntype B = import(\"./crate\").Crate<string>;\ntype C = unique symbol;\ntype D = readonly (string | number)[];\n",
    );
    const types = result.root.children.map((alias) => treeOf(result, alias.children[0]));

    expect(types).toEqual([
      ["TSTypeQuery(typeof shelf.crate)"],
      [
        "TSImportType(import(\"./crate\").Crate<string>)",
        "TSLiteralType(\"./crate\")",
        "StringLiteral(\"./crate\")",
        "TSTypeParameterInstantiation(<string>)",
        "TSStringKeyword(string)",
      ],
      ["TSTypeOperator(unique symbol)", "TSSymbolKeyword(symbol)"],
      [
        "TSTypeOperator(readonly (string | number)[])",
        "TSArrayType((string | number)[])",
        "TSParenthesizedType((string | number))",
        "TSUnionType(string | number)",
        "TSStringKeyword(string)",
        "TSNumberKeyword(number)",
      ],
    ]);
  });

  test("literal types hold their literal; a negative number's literal is the number", () => {
    const result = parseTypes("type Flag = -1 | 2n | true | `plain` | null;\n");
    const union = find(result.root, "TSUnionType");

    expect(union.children.map((member) => treeOf(result, member))).toEqual([
      ["TSLiteralType(-1)", "NumericLiteral(1)"],
      ["TSLiteralType(2n)", "BigIntLiteral(2n)"],
      ["TSLiteralType(true)", "BooleanLiteral(true)"],
      ["TSLiteralType(`plain`)", "TemplateLiteral(`plain`)"],
      ["TSNullKeyword(null)"],
    ]);
  });

  test("a type predicate is the return type's type", () => {
    const result = parseTypes(
      "function isCrate(value: unknown): value is Crate { return true; }\nfunction check(value: unknown): asserts value is string {}\n",
    );

    expect(
      findAll(result.root, "TSTypePredicate").map((node) => treeOf(result, node)),
    ).toEqual([
      ["TSTypePredicate(value is Crate)", "TSTypeReference(Crate)"],
      ["TSTypePredicate(asserts value is string)", "TSStringKeyword(string)"],
    ]);
  });

  test("a tagged template's and a bare `new`'s type arguments are their children", () => {
    const result = parseTypes("const query = sql<Row>`select 1`;\nconst box = new Box<Crate>;\n");

    expect(shapeOf(result, find(result.root, "TaggedTemplateExpression"))).toEqual([
      "Identifier(sql)",
      "TSTypeParameterInstantiation(<Row>)",
    ]);
    expect(shapeOf(result, find(result.root, "NewExpression"))).toEqual([
      "Identifier(Box)",
      "TSTypeParameterInstantiation(<Crate>)",
    ]);
  });

  test("`>>>` closes three type argument lists, each ending at its own `>`", () => {
    const result = parseTypes("let deep: A<B<C<D>>>;\n");

    expect(
      findAll(result.root, "TSTypeParameterInstantiation").map((list) => text(result, list)),
    ).toEqual(["<B<C<D>>>", "<C<D>>", "<D>"]);
  });

  test("array and indexed access chains apply left to right", () => {
    const result = parseTypes("let grid: Cell[][];\nlet pick: Crate['a'][0];\n");

    expect(findAll(result.root, "TSArrayType").map((node) => text(result, node))).toEqual([
      "Cell[][]",
      "Cell[]",
    ]);
    expect(
      findAll(result.root, "TSIndexedAccessType").map((node) => text(result, node)),
    ).toEqual(["Crate['a'][0]", "Crate['a']"]);
  });

  test("a computed key reports its literal, and only a literal", () => {
    const result = parseTypes("type Crate = { ['lid-color']: string; [Symbol.iterator](): void };\n");
    const literal = find(result.root, "TSTypeLiteral");

    expect(literal.children.map((member) => shapeOf(result, member))).toEqual([
      ["StringLiteral('lid-color')", "TSTypeAnnotation(: string)"],
      ["TSTypeAnnotation(: void)"],
    ]);
    expect(literal.children.map((member) => member.computed)).toEqual([true, true]);
  });

  test("an infer constraint in a conditional's extends clause keeps the branches", () => {
    const result = parseTypes("type Pick<T> = T extends infer U extends string ? 'a' : 'b';\n");

    expect(shapeOf(result, find(result.root, "TSConditionalType"))).toEqual([
      "TSTypeReference(T)",
      "TSInferType(infer U extends string)",
      "TSLiteralType('a')",
      "TSLiteralType('b')",
    ]);
  });

  test("intersections, function types, this and the predefined types", () => {
    const result = parseTypes(
      "type A = Crate & { lid: this } & ((size: bigint) => void);\ntype B = [any, unknown, object, undefined, never, null, intrinsic];\n",
    );
    const [intersection, tuple] = result.root.children.map((alias) => alias.children[0]);

    expect(shapeOf(result, intersection)).toEqual([
      "TSTypeReference(Crate)",
      "TSTypeLiteral({ lid: this })",
      "TSParenthesizedType(((size: bigint) => void))",
    ]);
    expect(find(intersection, "TSThisType")).toBeDefined();
    expect(treeOf(result, find(intersection, "TSFunctionType")).slice(1)).toEqual([
      "Parameter(size: bigint)",
      "TSTypeAnnotation(: bigint)",
      "TSBigIntKeyword(bigint)",
      "TSTypeAnnotation(=> void)",
      "TSVoidKeyword(void)",
    ]);
    expect(tuple.children.map((element) => element.kind)).toEqual([
      "TSAnyKeyword",
      "TSUnknownKeyword",
      "TSObjectKeyword",
      "TSUndefinedKeyword",
      "TSNeverKeyword",
      "TSNullKeyword",
      "TSIntrinsicKeyword",
    ]);
  });

  test("annotations sit under what they annotate, wherever the parser accepts them", () => {
    const cases = [
      ["const f = function (a: A): R { return a; };", "FunctionExpression", ["Parameter", "TSTypeAnnotation", "BlockStatement"]],
      ["const f = async function* (a: A): R {};", "FunctionExpression", ["Parameter", "TSTypeAnnotation", "BlockStatement"]],
      ["const o = { get size(): number { return 1; }, set size(v: number) {} };", "ObjectExpression", null],
      ["class K { static #count: number = 0; accessor label: string = ''; }", "ClassDeclaration", null],
      ["const f = (r: Res) => { using handle: Handle = open(r); };", "UsingDeclaration", ["TSTypeAnnotation", "CallExpression"]],
      ["let pending!: number;", "VariableDeclaration", ["TSTypeAnnotation"]],
      ["const v = shelf?.pick<Crate>(1);", "CallExpression", ["MemberExpression", "TSTypeParameterInstantiation", "NumericLiteral"]],
    ];

    cases.forEach(([source, kind, shape]) => {
      const result = parseTypes(`${source}\n`);
      const node = find(result.root, kind);

      if (shape) {
        expect(node.children.map((child) => child.kind)).toEqual(shape);
      }
    });

    const object = find(parseTypes(`${cases[2][0]}\n`).root, "ObjectExpression");
    expect(findAll(object, "TSTypeAnnotation").length).toBe(2);
    const members = find(parseTypes(`${cases[3][0]}\n`).root, "ClassDeclaration").children;
    expect(members.map((member) => member.children.map((child) => child.kind))).toEqual([
      ["Identifier", "TSTypeAnnotation", "NumericLiteral"],
      ["Identifier", "TSTypeAnnotation", "StringLiteral"],
    ]);
  });

  test("shapes the parser reads speculatively still parse with types recorded", () => {
    [
      "x = c ? (a, b) : d << 2;",
      "x = a < b > c;",
      "x = c ? (y) : z;",
      "x = a < b && c > (d);",
      "x = c ? (l) => ({ k: 1 }) : (l) => 2;",
    ].forEach((source) => {
      const result = parseTypes(`${source}\n`);

      expect(result.root.children[0].kind).toBe("ExpressionStatement");
      expect(all(result.root).filter((node) => node.kind.startsWith("TS"))).toEqual([]);
    });
  });

  test("a template literal type inside brackets does not end them early", () => {
    const result = parseTypes(
      "type Handler = (id: `user-${string}`, [key]: [`k-${number}`]) => void;\ntype Keyed = { [`k-${1}`]: string };\n",
    );
    const handler = result.root.children[0].children[0];

    expect(handler.kind).toBe("TSFunctionType");
    expect(shapeOf(result, handler)).toEqual([
      "Parameter(id: `user-${string}`)",
      "Parameter([key]: [`k-${number}`])",
      "TSTypeAnnotation(=> void)",
    ]);
    expect(text(result, find(result.root.children[1], "TSPropertySignature"))).toBe(
      "[`k-${1}`]: string",
    );
  });

  test("a catch parameter's annotation is a child of the try statement", () => {
    const result = parseTypes("try { open(); } catch (error: unknown) { log(error); }\n");

    expect(shapeOf(result, result.root.children[0]).map((shape) => shape.split("(")[0])).toEqual([
      "BlockStatement",
      "TSTypeAnnotation",
      "BlockStatement",
    ]);
  });

  test("with jsx on, types are measured in the file", () => {
    const source =
      "const Row = ({ label }: { label: string }): Element => <span title={label}>{label}</span>;\n";
    const result = parseTypes(source, { jsx: true, fileName: "row.tsx" });

    expect(findAll(result.root, "TSTypeAnnotation").map((node) => text(result, node))).toEqual([
      ": { label: string }",
      ": string",
      ": Element",
    ]);
  });

  test("children nest inside their parent and siblings do not overlap", () => {
    const source = [
      "type Deep = { a: Map<string, [x: number, y?: Array<{ z: 'q' }>]>; b(c: () => void): Promise<void> };",
      "const f = <T extends object>(x: T, y: Partial<T> = {}): Readonly<T> => ({ ...x, ...y });",
    ].join("\n");
    const result = parseTypes(`${source}\n`);
    const check = (node) => {
      node.children.forEach((child, index) => {
        expect(child.start >= node.start && child.end <= node.end).toBe(true);
        if (index > 0) {
          expect(child.start >= node.children[index - 1].end).toBe(true);
        }
        check(child);
      });
    };

    check(result.root);
  });

  test("a deeply nested type does not exhaust the stack", () => {
    const depth = 20000;
    [
      `${"Array<".repeat(depth)}number${">".repeat(depth)}`,
      `${"keyof ".repeat(depth)}Crate`,
      `Cell${"[]".repeat(depth)}`,
      `${"(".repeat(depth)}Cell${")".repeat(depth)}`,
    ].forEach((type) => {
      const result = parseTypes(`let deep: ${type};\n`);

      expect(text(result, find(result.root, "TSTypeAnnotation"))).toBe(`: ${type}`);
    });
  });

  test("an overload signature reports its type parameters, parameters and return type", () => {
    const result = parseTypes(
      [
        'function pick<T>(this: Window, text: "a", ...rest: T[]): string;',
        'export function pick(count: { n: "b" }): "c";',
        "function pick(value: unknown) { return value; }",
        "",
      ].join("\n"),
    );
    const [overload, exported] = result.root.children;

    expect(overload.kind).toBe("OverloadDeclaration");
    expect(shapeOf(result, overload)).toEqual([
      "TSTypeParameterDeclaration(<T>)",
      "Parameter(this: Window)",
      'Parameter(text: "a")',
      "Parameter(...rest: T[])",
      "TSTypeAnnotation(: string)",
    ]);
    expect(exported.kind).toBe("ExportOverloadDeclaration");
    expect(shapeOf(result, exported)).toEqual([
      'Parameter(count: { n: "b" })',
      'TSTypeAnnotation(: "c")',
    ]);
    expect(findAll(exported, "StringLiteral").map((node) => text(result, node))).toEqual(['"b"', '"c"']);
  });

  test("a this parameter is a Parameter holding its annotation, after the type parameters", () => {
    const result = parseTypes(
      [
        "function total<T>(this: Window, extra: T) {}",
        "class Tally { add(this: Tally, step: number) {} }",
        "const counter = { bump(this: Window) {} };",
        "",
      ].join("\n"),
    );
    const [declaration, klass, variable] = result.root.children;
    const shapes = [declaration, find(klass, "MethodDefinition"), find(variable, "Property")]
      .map((node) => shapeOf(result, node).filter((shape) => !shape.startsWith("BlockStatement")));

    expect(shapes).toEqual([
      [
        "TSTypeParameterDeclaration(<T>)",
        "Parameter(this: Window)",
        "Parameter(extra: T)",
      ],
      ["Identifier(add)", "Parameter(this: Tally)", "Parameter(step: number)"],
      ["Identifier(bump)", "Parameter(this: Window)"],
    ]);
    expect(treeOf(result, find(declaration, "Parameter"))).toEqual([
      "Parameter(this: Window)",
      "TSTypeAnnotation(: Window)",
      "TSTypeReference(Window)",
    ]);
  });

  test("declare module reports its name, and other ambient declarations nothing", () => {
    const result = parseTypes(
      [
        'declare module "*.svg" {',
        "  const url: string;",
        "}",
        'declare module "*.css";',
        'export declare module "virtual:a" {}',
        "declare global { interface Window { app: unknown } }",
        'declare const mode: "light";',
        "",
      ].join("\n"),
    );

    expect(result.root.children.map((node) => shapeOf(result, node))).toEqual([
      ['StringLiteral("*.svg")'],
      ['StringLiteral("*.css")'],
      ['StringLiteral("virtual:a")'],
      [],
      [],
    ]);
  });

  test("without the option an overload signature and an ambient declaration have no children", () => {
    const source = 'function pick(text: "a"): string;\ndeclare module "*.css";\n';

    expect(parse(source, { fileName: "types.ts" }).root.children.map((node) => node.children.length)).toEqual([0, 0]);
  });

  test("without the option no type syntax is reported and the statements are unchanged", () => {
    const source = "type Shade = 'light' | 'dark';\nconst pick = (s: Shade): Shade => s;\n";
    const plain = parse(source, { fileName: "types.ts" });

    expect(all(plain.root).filter((node) => node.kind.startsWith("TS"))).toEqual([]);
    expect(plain.root.children.map((node) => node.children.length)).toEqual([0, 0]);
  });
});
