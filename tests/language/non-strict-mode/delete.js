/*---
description: Non-strict compatibility delete semantics
features: [compat-non-strict-mode]
---*/

describe("non-strict delete", () => {
  test("delete identifier returns false for bindings and true for unresolved names", () => {
    let lexical = 1;
    const immutable = 2;
    const deleteParam = (param) => delete param;

    expect(delete lexical).toBe(false);
    expect(delete immutable).toBe(false);
    expect(deleteParam(3)).toBe(false);
    expect(delete definitelyMissingDeleteBinding).toBe(true);
  });

  test("delete non-configurable properties returns false instead of throwing", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      configurable: false
    });

    expect(delete obj.fixed).toBe(false);
    expect(delete obj["fixed"]).toBe(false);
    expect(obj.fixed).toBe(1);
  });

  test("delete configurable and missing properties returns true", () => {
    const obj = { value: 1 };

    expect(delete obj.value).toBe(true);
    expect(delete obj["missing"]).toBe(true);
    expect(Object.hasOwn(obj, "value")).toBe(false);
  });

  test("delete primitive string exotic properties returns false", () => {
    expect(delete "abc".length).toBe(false);
    expect(delete "abc"[0]).toBe(false);
    expect(delete "abc"[3]).toBe(true);
  });

  test("delete identifier removes configurable global object properties", () => {
    globalThis.__gocciaConfigurableDelete = 1;

    expect(delete __gocciaConfigurableDelete).toBe(true);
    expect(Object.hasOwn(globalThis, "__gocciaConfigurableDelete")).toBe(false);
  });

  test("delete identifier keeps non-configurable global object properties", () => {
    Object.defineProperty(globalThis, "__gocciaFixedDelete", {
      value: 1,
      configurable: false,
    });

    expect(delete __gocciaFixedDelete).toBe(false);
    expect(globalThis.__gocciaFixedDelete).toBe(1);
  });

  test("delete identifier targets with object bindings", () => {
    const obj = { value: 1 };
    let result = false;

    with (obj) {
      result = delete value;
    }

    expect(result).toBe(true);
    expect(Object.hasOwn(obj, "value")).toBe(false);
  });

  test("delete identifier honors Symbol.unscopables in with environments", () => {
    const obj = { value: 1 };
    obj[Symbol.unscopables] = { value: true };
    let result = false;

    with (obj) {
      result = delete value;
    }

    expect(result).toBe(true);
    expect(obj.value).toBe(1);
  });
});
