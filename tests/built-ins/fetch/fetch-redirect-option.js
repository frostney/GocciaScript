/*---
description: fetch's redirect option accepts the RequestRedirect values and refuses others
features: [fetch]
---*/

describe("fetch redirect option", () => {
  const unreachable = "http://0.0.0.0:1/";

  test.each(["follow", "error", "manual"])("accepts %s", (mode) => {
    const p = fetch(unreachable, { redirect: mode });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("undefined selects the default", () => {
    const p = fetch(unreachable, { redirect: undefined });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test("converts the value to a string first", () => {
    const p = fetch(unreachable, { redirect: { toString: () => "manual" } });
    p.catch(() => {});
    expect(typeof p.then).toBe("function");
  });

  test.each(["", "Follow", "MANUAL", "deny", "null"])(
    "throws TypeError for %s",
    (mode) => {
      expect(() => fetch(unreachable, { redirect: mode })).toThrow(TypeError);
    },
  );

  test("throws TypeError for null, which converts to \"null\"", () => {
    expect(() => fetch(unreachable, { redirect: null })).toThrow(TypeError);
  });

  test("names the refused value", () => {
    let message = "";
    try {
      fetch(unreachable, { redirect: "deny" });
    } catch (error) {
      message = error.message;
    }
    expect(message).toContain("'deny'");
  });

  test("an invalid value throws synchronously, before any request starts", () => {
    let promise;
    expect(() => {
      promise = fetch(unreachable, { redirect: "deny" });
    }).toThrow(TypeError);
    expect(promise).toBeUndefined();
  });
});
