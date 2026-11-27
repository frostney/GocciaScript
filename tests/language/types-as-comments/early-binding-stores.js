/*---
description: Stores into a typed lexical binding are checked even when the storing closure precedes its declaration
features: [types-as-comments, strict-type-enforcement]
---*/

describe("stores compiled before a typed lexical declaration", () => {
  test("an early closure keeps TDZ and type checks distinct", () => {
    const assign = (next) => { value = next; };
    expect(() => assign(5)).toThrow(ReferenceError);
    let value: string = "original";
    expect(() => assign(5)).toThrow(TypeError);
    expect(value).toBe("original");
    assign("accepted");
    expect(value).toBe("accepted");
  });

  test("compound, update and destructuring stores are checked", () => {
    const subtract = () => { value -= 1; };
    const decrement = () => { value--; };
    const unpack = () => { [value = 5] = []; };
    let value: string = "2";
    expect(subtract).toThrow(TypeError);
    expect(decrement).toThrow(TypeError);
    expect(unpack).toThrow(TypeError);
    expect(value).toBe("2");
  });

  test("a short-circuited assignment leaves the value alone", () => {
    const fallback = () => { value ??= 5; };
    let value: string = "present";
    fallback();
    expect(value).toBe("present");
  });

  test("an early closure also checks an inferred binding", () => {
    const assign = () => { count = "wrong"; };
    let count = 2;
    expect(assign).toThrow(TypeError);
    expect(count).toBe(2);
  });

  test("an early closure checks the first store into a declaration without an initializer", () => {
    const assign = () => { value = 5; };
    let value: string;
    expect(value).toBeUndefined();
    expect(assign).toThrow(TypeError);
    expect(value).toBeUndefined();
  });

  test("a failed initializer leaves the captured binding uninitialized", () => {
    let read;
    const declare = () => {
      read = () => value;
      let value: string = 5;
    };
    expect(declare).toThrow(TypeError);
    expect(read).toThrow(ReferenceError);
  });

  test("a nested closure keeps the binding's type when it outlives the scope", () => {
    const create = () => {
      const makeSetter = () => (next) => { value = next; };
      let value: string = "original";
      return [makeSetter(), () => value];
    };
    const [assign, read] = create();
    expect(() => assign(5)).toThrow(TypeError);
    expect(read()).toBe("original");
  });

  test("an async closure checks the store after resumption", async () => {
    const assign = async () => {
      await Promise.resolve();
      value = 5;
    };
    let value: string = "original";
    await expect(assign()).rejects.toThrow(TypeError);
    expect(value).toBe("original");
  });
});
