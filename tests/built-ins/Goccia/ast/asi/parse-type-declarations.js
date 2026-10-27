/*---
description: goccia:ast reports semicolon-less type-level declarations by kind when the host enables ASI
features: [Goccia, experimental-ast, compat-asi]
---*/

import { parse } from "goccia:ast";

describe("ast.parse type-level declarations under ASI", () => {
  test("each semicolon-less type-level form ends at its line break and keeps its kind", () => {
    const lines = [
      'import type { A } from "./a.ts"',
      "export type { A }",
      'export type { A as B } from "./a.ts"',
      'export type * from "./star.ts"',
      "export type T = string | number",
      "type U = A",
      "const value = 1",
    ];
    const source = lines.join("\n") + "\n";
    const nodes = parse(source).root.children;

    expect(nodes.map((node) => [node.kind, node.typeOnly, source.slice(node.start, node.end)])).toEqual([
      ["ImportDeclaration", true, lines[0]],
      ["ExportDeclaration", true, lines[1]],
      ["ReExportDeclaration", true, lines[2]],
      ["ReExportDeclaration", true, lines[3]],
      ["ExportTypeAliasDeclaration", true, lines[4]],
      ["TypeAliasDeclaration", true, lines[5]],
      ["VariableDeclaration", undefined, lines[6]],
    ]);
  });

  test("a multi-line union without semicolons stays one declaration", () => {
    const source = "export type GroupId =\n  | 'names'\n  | 'functions'\nconst value = 1\n";
    const [alias, declaration] = parse(source).root.children;

    expect(alias.kind).toBe("ExportTypeAliasDeclaration");
    expect(source.slice(alias.start, alias.end)).toBe(
      "export type GroupId =\n  | 'names'\n  | 'functions'",
    );
    expect(declaration.kind).toBe("VariableDeclaration");
  });
});
