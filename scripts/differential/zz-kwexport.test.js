import { describe, expect, test, vi } from "vitest";
import * as target from "./mods/kwtarget.js";
vi.mock("./mods/kwtarget.js", () => ({ await: 1, yield: 2, eval: 3, arguments: 4, value: "MOCKED" }));
describe("keyword factory keys under real vitest", () => {
  test("keyword-named exports are exposed", () => {
    expect(target.value).toBe("MOCKED");
    expect(target["await"]).toBe(1);
    expect(target["yield"]).toBe(2);
    expect(target["eval"]).toBe(3);
    expect(target["arguments"]).toBe(4);
  });
});
