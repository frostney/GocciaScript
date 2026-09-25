/*---
description: Repeated property writes through one call site observe accessors, deletion, prototype mutation, non-writable, and proxies
features: [Object.defineProperty, Object.freeze, Proxy, property-descriptors]
---*/

test("repeated writes through one site update an own writable data property", () => {
  const obj = { x: 0 };
  const writeX = (o, value) => {
    o.x = value;
  };

  writeX(obj, 1);
  writeX(obj, 2);
  expect(obj.x).toBe(2);
  writeX(obj, 3);
  expect(obj.x).toBe(3);
});

test("repeated writes through one site still invoke a later setter", () => {
  const obj = { x: 1 };
  const writeX = (o, value) => {
    o.x = value;
  };
  const received = [];

  writeX(obj, 2);
  writeX(obj, 3);
  Object.defineProperty(obj, "x", {
    set(value) {
      received.push(value);
    },
    get() {
      return received[received.length - 1];
    },
    configurable: true,
  });
  writeX(obj, 10);
  writeX(obj, 11);
  expect(received).toEqual([10, 11]);
});

test("repeated writes through one site observe deletion then re-addition", () => {
  const obj = { x: "first" };
  const writeX = (o, value) => {
    o.x = value;
  };

  writeX(obj, "second");
  expect(obj.x).toBe("second");
  delete obj.x;
  expect(Object.prototype.hasOwnProperty.call(obj, "x")).toBe(false);
  writeX(obj, "third");
  expect(obj.x).toBe("third");
  expect(Object.prototype.hasOwnProperty.call(obj, "x")).toBe(true);
});

test("own write through one site does not mask a later prototype setter", () => {
  const proto = {};
  const writeX = (o, value) => {
    o.x = value;
  };
  const own = Object.create(proto);
  own.x = "own";
  const received = [];

  writeX(own, "warm");
  delete own.x;
  Object.defineProperty(proto, "x", {
    set(value) {
      received.push(value);
    },
    configurable: true,
  });
  writeX(own, "from setter");
  expect(received).toEqual(["from setter"]);
  expect(Object.prototype.hasOwnProperty.call(own, "x")).toBe(false);
});

test("warmed write of a non-writable own data property throws TypeError", () => {
  const obj = { x: 1 };
  const writeX = (o, value) => {
    o.x = value;
  };

  writeX(obj, 2);
  Object.defineProperty(obj, "x", {
    value: 2,
    writable: false,
    configurable: true,
  });
  expect(() => {
    writeX(obj, 3);
  }).toThrow(TypeError);
  expect(obj.x).toBe(2);
});

test("warmed write after Object.freeze throws TypeError", () => {
  const obj = { x: 1 };
  const writeX = (o, value) => {
    o.x = value;
  };

  writeX(obj, 2);
  Object.freeze(obj);
  expect(() => {
    writeX(obj, 3);
  }).toThrow(TypeError);
  expect(obj.x).toBe(2);
});

test("one site writing many ordinary receivers still traps a later proxy", () => {
  const writeX = (o, value) => {
    o.x = value;
  };
  const log = [];
  Array.from({ length: 64 }, (_, i) => ({ x: i })).forEach((o, i) => {
    writeX(o, i + 1);
    expect(o.x).toBe(i + 1);
  });
  const target = { x: 0 };
  const proxy = new Proxy(target, {
    set(t, prop, value) {
      log.push(String(prop) + "=" + value);
      t[prop] = value;
      return true;
    },
  });
  writeX(proxy, 99);
  expect(log).toEqual(["x=99"]);
  expect(target.x).toBe(99);
});

test("one site writing class instances updates per-instance fields", () => {
  class Point {
    x;
    constructor(x) {
      this.x = x;
    }
  }
  const writeX = (o, value) => {
    o.x = value;
  };
  const a = new Point(1);
  const b = new Point(2);

  writeX(a, 10);
  writeX(b, 20);
  expect(a.x).toBe(10);
  expect(b.x).toBe(20);
  writeX(a, 11);
  expect(a.x).toBe(11);
  expect(b.x).toBe(20);
});
