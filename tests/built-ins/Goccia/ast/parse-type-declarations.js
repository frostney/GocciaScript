/*---
description: goccia:ast reports type-level declarations by what they were written as, not as empty statements
features: [Goccia, experimental-ast]
---*/

import { parse } from "goccia:ast";

const describeNode = (source, node) => ({
  kind: node.kind,
  typeOnly: node.typeOnly,
  text: source.slice(node.start, node.end),
});

const topLevel = (source) => {
  const result = parse(source, { fileName: "types.ts" });
  return result.root.children.map((node) => describeNode(source, node));
};

describe("ast.parse type-level declarations", () => {
  test("each type-level form has its own kind and covers its own text", () => {
    const lines = [
      'import type { A } from "./a.ts";',
      'import type * as NS from "./ns.ts";',
      "export type { A };",
      'export type { A as B } from "./a.ts";',
      'export type * from "./star.ts";',
      'export type * as Star from "./star.ts";',
      "export type T = string | number;",
      "export interface I { x: number }",
      "type U = { a: 1 };",
      "interface J { y: string }",
    ];

    expect(topLevel(lines.join("\n") + "\n")).toEqual([
      { kind: "ImportDeclaration", typeOnly: true, text: lines[0] },
      { kind: "ImportDeclaration", typeOnly: true, text: lines[1] },
      { kind: "ExportDeclaration", typeOnly: true, text: lines[2] },
      { kind: "ReExportDeclaration", typeOnly: true, text: lines[3] },
      { kind: "ReExportDeclaration", typeOnly: true, text: lines[4] },
      { kind: "ReExportDeclaration", typeOnly: true, text: lines[5] },
      { kind: "ExportTypeAliasDeclaration", typeOnly: true, text: lines[6] },
      { kind: "ExportInterfaceDeclaration", typeOnly: true, text: lines[7] },
      { kind: "TypeAliasDeclaration", typeOnly: true, text: lines[8] },
      { kind: "InterfaceDeclaration", typeOnly: true, text: lines[9] },
    ]);
  });

  test("the runtime module forms carry typeOnly false", () => {
    const lines = [
      'import { B } from "./b.ts";',
      'import { type C, D } from "./c.ts";',
      'import { type E } from "./e.ts";',
      "export { D };",
      "export { type C as F };",
      'export { D as G } from "./d.ts";',
      'export * from "./star.ts";',
    ];

    expect(topLevel(lines.join("\n") + "\n")).toEqual([
      { kind: "ImportDeclaration", typeOnly: false, text: lines[0] },
      { kind: "ImportDeclaration", typeOnly: false, text: lines[1] },
      { kind: "ImportDeclaration", typeOnly: false, text: lines[2] },
      { kind: "ExportDeclaration", typeOnly: false, text: lines[3] },
      { kind: "ExportDeclaration", typeOnly: false, text: lines[4] },
      { kind: "ReExportDeclaration", typeOnly: false, text: lines[5] },
      { kind: "ReExportDeclaration", typeOnly: false, text: lines[6] },
    ]);
  });

  test("a lone semicolon is the only empty statement, and it carries no typeOnly", () => {
    const source = "type A = 1;\n;\nconst b = 2;\n";
    const [alias, empty, declaration] = parse(source).root.children;

    expect(alias.kind).toBe("TypeAliasDeclaration");
    expect(empty.kind).toBe("EmptyStatement");
    expect(source.slice(empty.start, empty.end)).toBe(";");
    expect("typeOnly" in empty).toBe(false);
    expect("typeOnly" in declaration).toBe(false);
  });

  test("a type declaration spanning lines keeps the whole declaration", () => {
    const source = [
      "export type GroupId =",
      "  | 'names'",
      "  | 'functions';",
      "interface Shape {",
      "  area(): number;",
      "}",
      "",
    ].join("\n");

    expect(topLevel(source)).toEqual([
      {
        kind: "ExportTypeAliasDeclaration",
        typeOnly: true,
        text: "export type GroupId =\n  | 'names'\n  | 'functions';",
      },
      {
        kind: "InterfaceDeclaration",
        typeOnly: true,
        text: "interface Shape {\n  area(): number;\n}",
      },
    ]);
  });

  test("type declarations inside a block, a function body and a switch case keep their kinds", () => {
    const source = [
      "{ type V = 1; interface K {} }",
      "const f = () => {",
      "  type Local = string;",
      "  return 1;",
      "};",
      "switch (x) {",
      "  case 1:",
      "    interface InCase {}",
      "    break;",
      "}",
      "",
    ].join("\n");
    const [block, declaration, statement] = parse(source).root.children;
    const kinds = (node) => node.children.map((child) => child.kind);

    expect(kinds(block)).toEqual(["TypeAliasDeclaration", "InterfaceDeclaration"]);
    expect(kinds(declaration.children[0])).toEqual([
      "TypeAliasDeclaration",
      "ReturnStatement",
    ]);
    expect(kinds(statement.children[0])).toEqual([
      "InterfaceDeclaration",
      "BreakStatement",
    ]);
  });

  test("a type-level node has no children and withholds its names", () => {
    const [alias] = parse("type Point = { x: number, y: number };\n").root.children;

    expect(alias.children).toEqual([]);
    expect(Object.keys(alias).sort()).toEqual([
      "children",
      "end",
      "kind",
      "loc",
      "start",
      "typeOnly",
    ]);
  });

  test("re-parsing a type-level node's own text gives the same kind over the same text", () => {
    const source = [
      'import type { A } from "./a.ts";',
      'export type { A } from "./a.ts";',
      "export type { A };",
      "export type T = A;",
      "export interface I {}",
      "type U = A;",
      "interface J {}",
      "",
    ].join("\n");

    for (const node of parse(source).root.children) {
      const text = source.slice(node.start, node.end);
      const [again] = parse(text).root.children;

      expect([again.kind, again.typeOnly, text.slice(again.start, again.end)]).toEqual([
        node.kind,
        node.typeOnly,
        text,
      ]);
    }
  });
});
