/*---
description: One property read or store site gives the right result for every kind of receiver that reaches it
features: [class, Proxy, Map, Symbol]
---*/

test("a read site warmed on plain objects reads arrays, functions, strings and exotic objects", () => {
  const readLength = (o) => o.length;
  const plain = Array.from({ length: 64 }, (_, i) => ({ length: i }));

  plain.forEach((o, i) => {
    expect(readLength(o)).toBe(i);
  });
  expect(readLength([1, 2, 3])).toBe(3);
  expect(readLength((a, b) => a + b)).toBe(2);
  expect(readLength("four")).toBe(4);
  expect(readLength(new Uint8Array(5))).toBe(5);
  expect(readLength(new Proxy({}, { get: () => 6 }))).toBe(6);
  expect(readLength(Object.create({ length: 7 }))).toBe(7);
  expect(readLength(7)).toBeUndefined();
  expect(readLength(true)).toBeUndefined();
  expect(readLength(Symbol("s"))).toBeUndefined();
  expect(() => readLength(null)).toThrow(TypeError);
  expect(() => readLength(undefined)).toThrow(TypeError);
  expect(readLength(plain[5])).toBe(5);
});

test("a read site warmed on class instances reads the same name from the class, a Map and an array", () => {
  class Box {
    size;
    static size = "static";
    constructor(size) {
      this.size = size;
    }
  }
  const readSize = (o) => o.size;
  const boxes = Array.from({ length: 64 }, (_, i) => new Box(i));

  boxes.forEach((b, i) => {
    expect(readSize(b)).toBe(i);
  });
  expect(readSize(Box)).toBe("static");
  expect(readSize(new Map([[1, 2]]))).toBe(1);
  expect(readSize(new Set([1, 2, 3]))).toBe(3);
  expect(readSize([])).toBeUndefined();
  expect(readSize(boxes[8])).toBe(8);
});

test("a store site warmed on plain objects stores to arrays, functions, class values and a Proxy", () => {
  const setTag = (o, v) => {
    o.tag = v;
    return o;
  };
  const plain = Array.from({ length: 64 }, () => ({ tag: 0 }));

  plain.forEach((o, i) => {
    expect(setTag(o, i).tag).toBe(i);
  });
  expect(setTag([], "array").tag).toBe("array");
  expect(setTag(() => 1, "function").tag).toBe("function");
  class Tagged {}
  expect(setTag(Tagged, "class").tag).toBe("class");
  expect(Object.keys(Tagged)).toEqual(["tag"]);
  const seen = [];
  const proxy = new Proxy(
    {},
    {
      set(target, key, value) {
        seen.push([key, value]);
        target[key] = value;
        return true;
      },
    },
  );
  setTag(proxy, "proxy");
  expect(seen).toEqual([["tag", "proxy"]]);
  expect(setTag(plain[2], "again").tag).toBe("again");
});

test("a read site that resolves on a prototype for ordinary receivers reads each other kind of receiver by its own rules", () => {
  // The holder is an ordinary object, so the site's prototype cache serves the
  // warm-up receivers. Every receiver below has the same holder as its
  // prototype and answers the name itself, or through a trap, before the
  // prototype is consulted.
  const holder = Object.create(Object.prototype);
  holder.length = "holder.length";
  holder.name = "holder.name";
  holder.size = "holder.size";
  holder.x = "holder.x";
  const readLength = (o) => o.length;
  const readName = (o) => o.name;
  const readSize = (o) => o.size;
  const readX = (o) => o.x;

  Array.from({ length: 64 }).forEach(() => {
    const o = Object.create(holder);
    expect(readLength(o)).toBe("holder.length");
    expect(readName(o)).toBe("holder.name");
    expect(readSize(o)).toBe("holder.size");
    expect(readX(o)).toBe("holder.x");
  });

  const array = [1, 2, 3];
  Object.setPrototypeOf(array, holder);
  expect(readLength(array)).toBe(3);
  expect(readX(array)).toBe("holder.x");

  class Extended extends Array {}
  Extended.prototype.length = "Extended.prototype.length";
  expect(readLength(new Extended())).toBe(0);

  const fn = (a, b) => a;
  Object.setPrototypeOf(fn, holder);
  expect(readLength(fn)).toBe(2);
  expect(readName(fn)).toBe("fn");
  expect(readX(fn)).toBe("holder.x");

  const bound = ((a) => a).bind(null);
  Object.setPrototypeOf(bound, holder);
  expect(readLength(bound)).toBe(1);
  expect(readName(bound)).toBe("bound ");

  const text = new String("abc");
  Object.setPrototypeOf(text, holder);
  expect(readLength(text)).toBe(3);
  expect(readX(text)).toBe("holder.x");

  class Klass {
    static x = "Klass.x";
  }
  Object.setPrototypeOf(Klass, holder);
  expect(readX(Klass)).toBe("Klass.x");
  expect(readName(Klass)).toBe("Klass");
  expect(readLength(Klass)).toBe(0);
  expect(readSize(Klass)).toBe("holder.size");

  const trapping = new Proxy(Object.create(holder), {
    get: (target, key) => "trap:" + String(key),
  });
  expect(readX(trapping)).toBe("trap:x");
  expect(readLength(trapping)).toBe("trap:length");
  const transparent = new Proxy(Object.create(holder), {});
  expect(readX(transparent)).toBe("holder.x");

  // These have no own property of the name, so the prototype answers.
  const map = new Map([[1, 2]]);
  Object.setPrototypeOf(map, holder);
  expect(readSize(map)).toBe("holder.size");
  const error = new Error("boom");
  Object.setPrototypeOf(error, holder);
  expect(readName(error)).toBe("holder.name");
  const date = new Date(0);
  Object.setPrototypeOf(date, holder);
  expect(readX(date)).toBe("holder.x");

  expect(readLength(Object.create(holder))).toBe("holder.length");
});

test("a method stored on a class through a warmed store site still resolves super through its own home object", () => {
  class Base {
    static describe() {
      return "base";
    }
  }
  class Derived extends Base {}
  const install = (target, fn) => {
    target.describe = fn;
  };
  const holder = {
    describe() {
      return "derived over " + super.describe();
    },
  };
  Object.setPrototypeOf(holder, Base);

  Array.from({ length: 64 }).forEach(() => {
    install({}, () => "plain");
  });
  install(Derived, holder.describe);
  expect(Derived.describe()).toBe("derived over base");
});
