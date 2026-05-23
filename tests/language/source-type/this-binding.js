/*---
description: >
  --source-type=module loads the entry file as module source. Per
  ES2026 §16.2.1.6.4, a Module Environment Record's [[ThisValue]] is
  undefined, so top-level `this` resolves to undefined rather than
  globalThis. The directory-level goccia.json sets the source type so
  every file in this directory uses module source.
---*/

const topLevelThis = this;
const topLevelThisType = typeof this;
const arrowThis = (() => this)();
const arrowThisType = (() => typeof this)();

describe("source-type=module entry", () => {
  test("top-level this is undefined", () => {
    expect(topLevelThis).toBeUndefined();
  });

  test("top-level typeof this is 'undefined'", () => {
    expect(topLevelThisType).toBe("undefined");
  });

  test("top-level this is not globalThis", () => {
    expect(topLevelThis === globalThis).toBe(false);
  });

  test("arrow function inherits module-level undefined this", () => {
    expect(arrowThis).toBeUndefined();
  });

  test("arrow function typeof this is 'undefined' in module", () => {
    expect(arrowThisType).toBe("undefined");
  });

  test("arrow function this is not globalThis in module", () => {
    expect(arrowThis === globalThis).toBe(false);
  });

  test("nested arrow inherits module-level undefined this", () => {
    const outer = () => {
      const inner = () => this;
      return inner();
    };
    expect(outer()).toBeUndefined();
  });

  test("arrow this is immune to .call()", () => {
    const arrow = () => this;
    expect(arrow.call({ x: 1 })).toBeUndefined();
  });

  test("arrow this is immune to .apply()", () => {
    const arrow = () => this;
    expect(arrow.apply({ x: 1 }, [])).toBeUndefined();
  });

  test("arrow this is immune to .bind()", () => {
    const arrow = () => this;
    const bound = arrow.bind({ x: 1 });
    expect(bound()).toBeUndefined();
  });

  test("arrow as object method inherits module undefined this", () => {
    const obj = {
      value: 42,
      arrow: () => this,
    };
    expect(obj.arrow()).toBeUndefined();
  });
});
