import { loop, pair, root } from "./helpers/alias-graph.yaml";

describe("YAML import with alias graphs", () => {
  test("imports self-referential mappings and sequences", () => {
    expect(root.self === root).toBe(true);
    expect(loop[0] === loop).toBe(true);
    expect(pair[0] === pair[1]).toBe(true);
  });
});
