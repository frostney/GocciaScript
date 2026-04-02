import { description, summary } from "./helpers/block-scalars.yaml";

describe("YAML import with block scalars", () => {
  test("imports literal and folded block scalars", () => {
    expect(description).toBe("first line\nsecond line\n\nthird line\n");
    expect(summary).toBe("folded text\nparagraph");
  });
});
