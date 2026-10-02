/*---
description: Annotated let bindings and parameters used as operands keep their runtime type checks on every assignment
features: [types-as-comments, strict-type-enforcement]
---*/

const pass = (value) => value;

describe("typed bindings as operands", () => {
  test("arithmetic on annotated let bindings", () => {
    const run = (seed: number) => {
      let count: number = seed;
      let scale: number = 2.5;
      let total: number = count * scale + count;
      count = count + 1;
      total += count;
      return [count + 1, total, count < total, total - count * scale];
    };

    expect(run(pass(2))).toEqual([4, 10, true, 2.5]);
  });

  test("a rejected assignment leaves the operand's value in place", () => {
    const run = (seed: number, wrong) => {
      let count: number = seed;
      let failed = false;
      try {
        count = wrong;
      } catch (error) {
        failed = error instanceof TypeError;
      }
      return [failed, count + 1, count * 2];
    };

    expect(run(pass(2), pass("text"))).toEqual([true, 3, 4]);
    expect(run(pass(2), pass(5))).toEqual([false, 6, 10]);
  });

  test("a rejected compound assignment leaves the operand's value in place", () => {
    const run = (seed: number, wrong) => {
      let count: number = seed;
      let failed = false;
      try {
        count += wrong;
      } catch (error) {
        failed = error instanceof TypeError;
      }
      return [failed, count + 1];
    };

    expect(run(pass(2), pass("text"))).toEqual([true, 3]);
    expect(run(pass(2), pass(5))).toEqual([false, 8]);
  });

  test("an annotated parameter is checked at the call and on assignment", () => {
    const add = (a: number, b: number) => a + b * 2;
    const reassign = (a: number, next) => {
      const before = a + 1;
      a = next;
      return [before, a + 1];
    };

    expect(add(pass(1), pass(2))).toBe(5);
    expect(() => add(pass("1"), pass(2))).toThrow(TypeError);
    expect(reassign(pass(1), pass(5))).toEqual([2, 6]);
    expect(() => reassign(pass(1), pass("5"))).toThrow(TypeError);
  });

  test("an operand read before a later operand reassigns the typed binding", () => {
    const run = (seed: number) => {
      let count: number = seed;
      const first = count + (count = 10);
      const second = count * (count += 1);
      return [first, second, count];
    };

    expect(run(pass(2))).toEqual([12, 110, 11]);
  });

  test("string and boolean annotations", () => {
    const run = (name: string, flag: boolean) => {
      let label: string = name + "!";
      let done: boolean = !flag;
      label = label + label;
      return [label + 1, done === flag, label < "b"];
    };

    expect(run(pass("a"), pass(true))).toEqual(["a!a!1", false, true]);
  });
});
