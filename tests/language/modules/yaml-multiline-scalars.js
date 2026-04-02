import { plain, quoted, single } from "./helpers/multiline-scalars.yaml";

describe("YAML import with multiline scalars", () => {
  test("imports folded plain and quoted scalars", () => {
    expect(plain).toBe("first second");
    expect(quoted).toBe("hello world");
    expect(single).toBe("left right");
  });
});
