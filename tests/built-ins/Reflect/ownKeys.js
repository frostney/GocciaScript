/*---
description: Reflect.ownKeys
features: [Reflect]
---*/

describe("Reflect.ownKeys", () => {
  test("returns own string keys", () => {
    const obj = { a: 1, b: 2, c: 3 };
    const keys = Reflect.ownKeys(obj);
    expect(keys.length).toBe(3);
    expect(keys).toContain("a");
    expect(keys).toContain("b");
    expect(keys).toContain("c");
  });

  test("includes non-enumerable properties", () => {
    const obj = {};
    Object.defineProperty(obj, "hidden", {
      value: 42,
      enumerable: false,
    });
    obj.visible = 1;
    const keys = Reflect.ownKeys(obj);
    expect(keys).toContain("hidden");
    expect(keys).toContain("visible");
  });

  test("includes symbol keys", () => {
    const sym = Symbol("myKey");
    const obj = { [sym]: "value", name: "test" };
    const keys = Reflect.ownKeys(obj);
    expect(keys).toContain("name");
    expect(keys).toContain(sym);
  });

  test("returns empty array for empty object", () => {
    const obj = Object.create(null);
    expect(Reflect.ownKeys(obj).length).toBe(0);
  });

  test("does not include inherited properties", () => {
    const proto = { inherited: true };
    const obj = Object.create(proto);
    obj.own = 1;
    const keys = Reflect.ownKeys(obj);
    expect(keys).toContain("own");
    expect(keys.length).toBe(1);
  });

  test("throws TypeError if target is not an object", () => {
    expect(() => Reflect.ownKeys(42)).toThrow(TypeError);
    expect(() => Reflect.ownKeys("str")).toThrow(TypeError);
    expect(() => Reflect.ownKeys(null)).toThrow(TypeError);
  });
});

describe("Reflect.ownKeys on a function or class whose length or name is defined again", () => {
  const redefine = (target, key, value) =>
    Object.defineProperty(target, key, { value, configurable: true });

  test("a class lists a re-created length or name after the keys that existed", () => {
    class ReLength {}
    delete ReLength.length;
    redefine(ReLength, "length", 0);
    expect(Reflect.ownKeys(ReLength)).toEqual(["name", "prototype", "length"]);

    class ReName {}
    delete ReName.name;
    redefine(ReName, "name", "ReName");
    expect(Reflect.ownKeys(ReName)).toEqual(["length", "prototype", "name"]);

    class Extra {}
    delete Extra.name;
    Extra.extra = 1;
    redefine(Extra, "name", "Extra");
    expect(Reflect.ownKeys(Extra)).toEqual(["length", "prototype", "extra", "name"]);
    expect(Object.getOwnPropertyNames(Extra)).toEqual(["length", "prototype", "extra", "name"]);
    expect(Object.keys(Object.getOwnPropertyDescriptors(Extra))).toEqual([
      "length",
      "prototype",
      "extra",
      "name",
    ]);
  });

  test("a class whose length or name is assigned after re-creation keeps the new position", () => {
    class Writable {}
    delete Writable.name;
    Object.defineProperty(Writable, "name", { value: "A", writable: true, configurable: true });
    Writable.name = "B";
    expect(Writable.name).toBe("B");
    expect(Reflect.ownKeys(Writable)).toEqual(["length", "prototype", "name"]);
  });

  test("a re-created class name can be deleted and re-created again", () => {
    class Twice {}
    delete Twice.name;
    redefine(Twice, "name", "first");
    Twice.extra = 1;
    delete Twice.name;
    expect(Object.hasOwn(Twice, "name")).toBe(false);
    expect(Reflect.ownKeys(Twice)).toEqual(["length", "prototype", "extra"]);
    redefine(Twice, "name", "second");
    expect(Twice.name).toBe("second");
    expect(Reflect.ownKeys(Twice)).toEqual(["length", "prototype", "extra", "name"]);
  });

  test("a method, an arrow and a built-in list a re-created length or name last", () => {
    const method = { m() {} }.m;
    delete method.name;
    method.extra = 1;
    redefine(method, "name", "m");
    expect(Reflect.ownKeys(method)).toEqual(["length", "extra", "name"]);

    const arrow = () => {};
    delete arrow.length;
    redefine(arrow, "length", 0);
    expect(Reflect.ownKeys(arrow)).toEqual(["name", "length"]);
    expect(arrow.length).toBe(0);

    const max = Math.max;
    delete max.length;
    redefine(max, "length", 0);
    expect(Reflect.ownKeys(max)).toEqual(["name", "length"]);
    expect(max.length).toBe(0);
  });

  test("a function's re-created name can be deleted again", () => {
    const arrow = () => {};
    delete arrow.name;
    redefine(arrow, "name", "again");
    expect(arrow.name).toBe("again");
    expect(delete arrow.name).toBe(true);
    expect(Object.hasOwn(arrow, "name")).toBe(false);
    expect(Reflect.ownKeys(arrow)).toEqual(["length"]);
  });

  test("length and name that were redefined without a delete keep their first position", () => {
    class Kept {
      static extra = 1;
    }
    Object.defineProperty(Kept, "name", { enumerable: true });
    Object.defineProperty(Kept, "length", { value: 4 });
    expect(Reflect.ownKeys(Kept)).toEqual(["length", "name", "prototype", "extra"]);

    const arrow = () => {};
    arrow.extra = 1;
    Object.defineProperty(arrow, "name", { value: "renamed" });
    expect(Reflect.ownKeys(arrow)).toEqual(["length", "name", "extra"]);
  });
});
