/*---
description: goccia:ast exposes statement structure, positions, and comment trivia
features: [Goccia, experimental-ast]
---*/

import { parse } from "goccia:ast";

const kinds = (node) => node.children.map((child) => child.kind);

describe("ast.parse", () => {
  test("returns the source it parsed alongside the tree", () => {
    const source = "const a = 1;\n";
    const result = parse(source);

    expect(result.source).toBe(source);
    expect(result.root.kind).toBe("Program");
  });

  test("a source with no trailing newline is returned unchanged", () => {
    expect(parse("const a = 1;").source).toBe("const a = 1;");
  });

  test("line terminators are normalized, so offsets index what is returned", () => {
    const result = parse("const a = 1;\r\nconst bb = 2;\r\n");
    const second = result.root.children[1];

    expect(result.source).toBe("const a = 1;\nconst bb = 2;\n");
    expect(result.source.slice(second.start, second.end)).toBe("const bb = 2;");
    expect(second.loc.start.line).toBe(2);
  });

  test("an empty source is an empty program", () => {
    const result = parse("");

    expect(result.root.kind).toBe("Program");
    expect(result.root.children).toEqual([]);
    expect(result.comments).toEqual([]);
  });

  test("top-level statements are the program's children, in source order", () => {
    const result = parse("const a = 1;\na;\nconst b = 2;\n");

    expect(kinds(result.root)).toEqual([
      "VariableDeclaration",
      "ExpressionStatement",
      "VariableDeclaration",
    ]);
  });

  test("a statement's offsets slice its own source text", () => {
    const source = "const a = 1;\nconst bb = 2;\n";
    const result = parse(source);
    const second = result.root.children[1];

    expect(source.slice(second.start, second.end)).toBe("const bb = 2;");
  });

  test("positions are one-based lines and columns", () => {
    const result = parse("const a = 1;\nconst b = 2;\n");
    const second = result.root.children[1];

    expect(second.loc.start.line).toBe(2);
    expect(second.loc.start.column).toBe(1);
    expect(second.loc.end.line).toBe(2);
  });

  test("a block is a node whose children are its statement list", () => {
    const result = parse("{\n  const a = 1;\n  a;\n}\n");
    const block = result.root.children[0];

    expect(block.kind).toBe("BlockStatement");
    expect(kinds(block)).toEqual(["VariableDeclaration", "ExpressionStatement"]);
  });

  test("an arrow function's block is reached through the declaration", () => {
    const result = parse("const f = () => {\n  const a = 1;\n  return a;\n};\n");
    const declaration = result.root.children[0];

    expect(declaration.kind).toBe("VariableDeclaration");
    expect(kinds(declaration)).toEqual(["BlockStatement"]);
    expect(kinds(declaration.children[0])).toEqual([
      "VariableDeclaration",
      "ReturnStatement",
    ]);
  });

  test("a concise arrow body owns no statement list", () => {
    const result = parse("const f = () => 1;\n");

    expect(kinds(result.root.children[0])).toEqual([]);
  });

  test("each switch case is its own statement list", () => {
    const result = parse("switch (a) {\n  case 1:\n    b;\n    c;\n  default:\n    d;\n}\n");
    const statement = result.root.children[0];

    expect(statement.kind).toBe("SwitchStatement");
    expect(kinds(statement)).toEqual(["SwitchCase", "SwitchCase"]);
    expect(kinds(statement.children[0])).toEqual([
      "ExpressionStatement",
      "ExpressionStatement",
    ]);
    expect(kinds(statement.children[1])).toEqual(["ExpressionStatement"]);
  });

  test("try, catch, and finally are three separate lists", () => {
    const result = parse("try {\n  a;\n} catch (e) {\n  b;\n} finally {\n  c;\n}\n");

    expect(kinds(result.root.children[0])).toEqual([
      "BlockStatement",
      "BlockStatement",
      "BlockStatement",
    ]);
  });

  test("a class method body is reached through the declaration", () => {
    const result = parse("class A {\n  m() {\n    const a = 1;\n  }\n}\n");
    const declaration = result.root.children[0];

    expect(declaration.kind).toBe("ClassDeclaration");
    expect(kinds(declaration)).toEqual(["BlockStatement"]);
    expect(kinds(declaration.children[0])).toEqual(["VariableDeclaration"]);
  });

  test("an object method body is reached through the declaration", () => {
    const result = parse("const o = {\n  m() {\n    const a = 1;\n  },\n};\n");

    expect(kinds(result.root.children[0])).toEqual(["BlockStatement"]);
  });

  test("an export keeps its own kind and its nested list", () => {
    const result = parse("export const f = () => {\n  return 1;\n};\n");
    const declaration = result.root.children[0];

    expect(declaration.kind).toBe("ExportVariableDeclaration");
    expect(kinds(declaration.children[0])).toEqual(["ReturnStatement"]);
  });

  test("comments are reported in source order with kind and position", () => {
    const source = "// first\nconst a = 1;\n/* second */\n";
    const result = parse(source);

    expect(result.comments.length).toBe(2);
    expect(result.comments[0].kind).toBe("Line");
    expect(result.comments[0].loc.start.line).toBe(1);
    expect(source.slice(result.comments[0].start, result.comments[0].end)).toBe(
      "// first",
    );
    expect(result.comments[1].kind).toBe("Block");
    expect(result.comments[1].loc.start.line).toBe(3);
  });

  test("a comment between two statements sits between their offsets", () => {
    const source = "const a = 1;\n// why\nconst b = 2;\n";
    const result = parse(source);
    const [first, second] = result.root.children;
    const comment = result.comments[0];

    expect(comment.start >= first.end).toBe(true);
    expect(comment.end <= second.start).toBe(true);
  });

  test("no comments means an empty array, not a missing one", () => {
    expect(parse("const a = 1;\n").comments).toEqual([]);
  });

  test("jsx is off unless asked for", () => {
    expect(() => parse("const a = <div />;\n")).toThrow();
  });

  test("with jsx on, a jsx source parses and keeps its line numbers", () => {
    const source = [
      "const f = () => (",
      "  <button",
      '    type="button"',
      "    onClick={() => {",
      "      const next = 1;",
      "      use(next);",
      "    }}",
      "  />",
      ");",
      "",
    ].join("\n");
    const result = parse(source, { jsx: true });
    const handler = result.root.children[0].children[0];

    expect(handler.kind).toBe("BlockStatement");
    expect(kinds(handler)).toEqual([
      "VariableDeclaration",
      "ExpressionStatement",
    ]);
    expect(handler.children[0].loc.start.line).toBe(5);
    expect(handler.children[1].loc.start.line).toBe(6);
  });

  test("with jsx on, the source is still the text that was passed", () => {
    const source = "const a = <div>hi</div>;\n";

    expect(parse(source, { jsx: true }).source).toBe(source);
  });

  test("with jsx on, offsets index the file and not the transform", () => {
    const source = [
      "const f = () => (",
      "  <button",
      "    onClick={() => {",
      "      const next = 1;",
      "      use(next);",
      "    }}",
      "  />",
      ");",
      "",
    ].join("\n");
    const result = parse(source, { jsx: true });
    const handler = result.root.children[0].children[0];

    expect(source.slice(handler.children[0].start, handler.children[0].end)).toBe(
      "const next = 1;",
    );
    expect(source.slice(handler.children[1].start, handler.children[1].end)).toBe(
      "use(next);",
    );
  });

  test("a statement whose text ends at jsx ends where the element does", () => {
    const source = "const a = <div>hi</div>;\nconst b = 2;\n";
    const result = parse(source, { jsx: true });

    expect(source.slice(result.root.children[0].start, result.root.children[0].end)).toBe(
      "const a = <div>hi</div>;",
    );
    expect(source.slice(result.root.children[1].start, result.root.children[1].end)).toBe(
      "const b = 2;",
    );
  });

  test("with jsx on, a comment inside a copied expression keeps its offsets", () => {
    const source = "const a = <div>{/* gone */ value /* kept */}</div>;\n";
    const result = parse(source, { jsx: true });
    const kept = result.comments[result.comments.length - 1];

    expect(source.slice(kept.start, kept.end)).toBe("/* kept */");
  });

  test("an expression statement starts at its first token", () => {
    const source = "use(next);\nx = 1;\n";
    const result = parse(source);

    expect(result.root.children.map((node) => source.slice(node.start, node.end))).toEqual([
      "use(next);",
      "x = 1;",
    ]);
  });

  test("a case clause covers itself, not the switch it is in", () => {
    const source = "switch (x) {\n  case 1:\n    a();\n    break;\n  default:\n    b();\n}\n";
    const result = parse(source);
    const cases = result.root.children[0].children;

    expect(cases.map((node) => node.kind)).toEqual(["SwitchCase", "SwitchCase"]);
    expect(source.slice(cases[0].start, cases[0].end)).toBe("case 1:\n    a();\n    break;");
    expect(source.slice(cases[1].start, cases[1].end)).toBe("default:\n    b();");
  });

  test("a body block covers its braces and nothing else", () => {
    const source = "class C {\n  m() {\n    return 1;\n  }\n}\n";
    const result = parse(source);
    const body = result.root.children[0].children[0];

    expect(body.kind).toBe("BlockStatement");
    expect(source.slice(body.start, body.end)).toBe("{\n    return 1;\n  }");
  });

  test("a syntax error is a SyntaxError naming its position", () => {
    expect(() => parse("const a = ;\n")).toThrow(SyntaxError);
  });

  test("the source argument must be a string", () => {
    expect(() => parse(42)).toThrow(TypeError);
  });
});
