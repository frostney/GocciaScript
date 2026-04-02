import { ascii, continued, emoji } from "./helpers/quoted-escapes.yaml";

describe("YAML import with quoted escapes", () => {
  test("imports escaped double-quoted scalars", () => {
    expect(ascii).toBe("A");
    expect(emoji).toBe("😀");
    expect(continued).toBe("firstsecond");
  });
});
