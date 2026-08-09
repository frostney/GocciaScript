import { describe, expect, test, vi } from "vitest";
import * as target from "./mods/kwtarget.js";
vi.mock("./mods/kwtarget.js", () => ({ class: 1, static: 2, import: 3, function: 4 }));
describe("hard reserved words as factory keys", () => {
  test("exposed too?", () => {
    expect(target["class"]).toBe(1);
    expect(target["static"]).toBe(2);
    expect(target["import"]).toBe(3);
    expect(target["function"]).toBe(4);
  });
});
