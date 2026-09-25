/*---
description: AbortController constructor behavior
features: [AbortController, AbortSignal]
---*/

describe("AbortController constructor", () => {
  test("is exposed with AbortSignal", () => {
    expect(typeof AbortController).toBe("function");
    expect(typeof AbortSignal).toBe("function");
    expect(Goccia.runtimeGlobals.includes("AbortController")).toBe(true);
    expect(Goccia.runtimeGlobals.includes("AbortSignal")).toBe(true);
  });

  test("creates an AbortController instance", () => {
    const controller = new AbortController();
    expect(controller instanceof AbortController).toBe(true);
    expect(Object.prototype.toString.call(controller)).toBe(
      "[object AbortController]"
    );
  });

  test("requires construction with new", () => {
    expect(() => AbortController()).toThrow(TypeError);
  });

  test("AbortSignal cannot be constructed directly", () => {
    expect(() => new AbortSignal()).toThrow(TypeError);
  });
});
