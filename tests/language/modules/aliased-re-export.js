import { sum, yell } from "./helpers/aliased-re-exporter.js";

describe("aliased re-export", () => {
  test("re-exported with alias: add as sum", () => {
    expect(sum(10, 5)).toBe(15);
  });

  test("re-exported with alias: shout as yell", () => {
    expect(yell("hello")).toBe("HELLO");
  });
});
