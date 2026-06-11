/*---
description: Repeated method loads through one call site reflect prototype mutation, shadowing, and prototype replacement
features: [Object.create, Object.setPrototypeOf, Object.defineProperty]
---*/

test("warmed method load observes a replaced prototype method", () => {
  const proto = {
    greet() {
      return "old";
    },
  };
  const objects = Array.from({ length: 64 }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("old");
  }
  proto.greet = () => "new";
  for (const o of objects) {
    expect(callGreet(o)).toBe("new");
  }
});

test("warmed method load observes a later own shadow on one receiver", () => {
  const proto = {
    greet() {
      return "proto";
    },
  };
  const objects = Array.from({ length: 64 }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("proto");
  }
  objects[7].greet = () => "own";
  expect(callGreet(objects[7])).toBe("own");
  expect(callGreet(objects[8])).toBe("proto");
});

test("warmed method load follows setPrototypeOf to a different prototype", () => {
  const protoA = {
    label() {
      return "a";
    },
  };
  const protoB = {
    label() {
      return "b";
    },
  };
  const obj = Object.create(protoA);
  const readLabel = (o) => o.label();

  Array.from({ length: 64 }).forEach(() => {
    expect(readLabel(obj)).toBe("a");
  });
  Object.setPrototypeOf(obj, protoB);
  expect(readLabel(obj)).toBe("b");
});

test("warmed two-level method load observes a closer shadow added later", () => {
  const grandProto = {
    speak() {
      return "grand";
    },
  };
  const middleProto = Object.create(grandProto);
  const objects = Array.from({ length: 64 }, () => Object.create(middleProto));
  const callSpeak = (o) => o.speak();

  for (const o of objects) {
    expect(callSpeak(o)).toBe("grand");
  }
  middleProto.speak = () => "middle";
  for (const o of objects) {
    expect(callSpeak(o)).toBe("middle");
  }
});

test("warmed class method load observes a method swapped on the prototype", () => {
  class Counter {
    n;
    constructor(n) {
      this.n = n;
    }
    value() {
      return this.n;
    }
  }
  const counters = Array.from({ length: 64 }, (_, i) => new Counter(i));
  const readValue = (c) => c.value();

  counters.forEach((c, i) => {
    expect(readValue(c)).toBe(i);
  });
  Object.defineProperty(Counter.prototype, "value", {
    value() {
      return this.n * 10;
    },
    configurable: true,
    writable: true,
  });
  counters.forEach((c, i) => {
    expect(readValue(c)).toBe(i * 10);
  });
});

test("warmed method load on a frozen prototype keeps resolving", () => {
  const proto = Object.freeze({
    greet() {
      return "frozen";
    },
  });
  const objects = Array.from({ length: 64 }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("frozen");
  }
});

test("warmed field read keeps working after delete flips a receiver to dictionary mode", () => {
  const objects = Array.from({ length: 64 }, (_, i) => ({ x: i, extra: true }));
  const readX = (o) => o.x;

  objects.forEach((o, i) => {
    expect(readX(o)).toBe(i);
  });
  delete objects[5].extra;
  expect(readX(objects[5])).toBe(5);
  expect(readX(objects[6])).toBe(6);
});
