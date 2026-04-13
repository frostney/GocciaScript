/*---
description: OP_CHECK_TYPE runtime guards align with compiler strict-type expectations (bytecode)
features: [types-as-comments, strict-type-enforcement]
---*/

describe.skipIf(!Goccia.strictTypes)("OP_CHECK_TYPE compiler and VM alignment", () => {

  test("typed local reassignment from closure uses runtime guard", () => {
    let x: number = 1;
    const f = () => {
      x = "bad";
    };
    expect(() => f()).toThrow(TypeError);
  });

  test("typed upvalue reassignment rejects wrong primitive", () => {
    let s: string = "a";
    const f = () => {
      s = 1;
    };
    expect(() => f()).toThrow(TypeError);
  });

  test("arrow returning wrong type for call-site const signature still throws at assignment", () => {
    const id = (v: number) => v;
    const x: number = id(1);
    expect(x).toBe(1);
    expect(() => {
      const y: string = id(2);
      void y;
    }).toThrow(TypeError);
  });

  test("number annotation rejects object at reassignment", () => {
    let x: number = 0;
    expect(() => {
      x = {};
    }).toThrow(TypeError);
  });

  test("boolean annotation rejects string", () => {
    let b: boolean = true;
    expect(() => {
      b = "false";
    }).toThrow(TypeError);
  });

});
