describe("YAML.parseDocuments", () => {
  test("always returns an array for a single document", () => {
    const parsed = YAML.parse("name: app");
    const documents = YAML.parseDocuments("name: app");

    expect(parsed.name).toBe("app");
    expect(documents.length).toBe(1);
    expect(documents[0].name).toBe("app");
  });

  test("matches YAML.parse for explicit document streams", () => {
    const stream = `
---
name: first
count: 1
---
name: second
items:
  - x
  - y
`;
    const parsed = YAML.parse(stream);
    const documents = YAML.parseDocuments(stream);

    expect(parsed.length).toBe(2);
    expect(documents.length).toBe(2);
    expect(documents[0].name).toBe(parsed[0].name);
    expect(documents[0].count).toBe(parsed[0].count);
    expect(documents[1].items[1]).toBe(parsed[1].items[1]);
  });

  test("returns an empty array for empty input", () => {
    const documents = YAML.parseDocuments("");

    expect(documents.length).toBe(0);
  });

  test("parses explicit empty documents as null entries", () => {
    const documents = YAML.parseDocuments(`
---
...
---
...
`);

    expect(documents.length).toBe(2);
    expect(documents[0]).toBe(null);
    expect(documents[1]).toBe(null);
  });

  test("applies directives per document", () => {
    const documents = YAML.parseDocuments(`
%TAG !a! tag:example.com,a:
---
node: !a!x value
...
%TAG !b! tag:example.com,b:
---
node: !b!y value
`);

    expect(documents.length).toBe(2);
    expect(documents[0].node.tagName).toBe("tag:example.com,a:x");
    expect(documents[0].node.value).toBe("value");
    expect(documents[1].node.tagName).toBe("tag:example.com,b:y");
    expect(documents[1].node.value).toBe("value");
  });

  test("does not leak directives across documents", () => {
    expect(() => YAML.parseDocuments(`
%TAG !a! tag:example.com,a:
---
node: !a!x value
---
node: !a!x value
`)).toThrow(SyntaxError);
  });

  test("rejects directives after document content without a footer", () => {
    expect(() => YAML.parseDocuments(`
---
scalar1
%YAML 1.2
---
scalar2
`)).toThrow(SyntaxError);

    expect(() => YAML.parseDocuments(`
!foo "bar"
%TAG ! tag:example.com,2000:app/
---
!foo "bar"
`)).toThrow(SyntaxError);
  });
});
