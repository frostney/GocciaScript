/*---
description: A method load that one call site has resolved on a prototype many times still reflects every later change to the receiver, the prototypes between and the holder
features: [class, Object.create, Object.defineProperty, Object.setPrototypeOf]
---*/

// A prototype built by Object.create and assignment is an ordinary object, as
// a class prototype is. An object literal is not: the bytecode VM gives it its
// own class, and a load that resolves on one is never served from the site's
// prototype cache.
const ordinary = (parent, members) => {
  const o = Object.create(parent);
  for (const key of Object.keys(members)) {
    o[key] = members[key];
  }
  return o;
};

const WARM = 64;

test("an own property added to a receiver that had none shadows the cached method", () => {
  const proto = ordinary(Object.prototype, { greet: () => "proto" });
  const objects = Array.from({ length: WARM }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("proto");
  }
  objects[7].greet = () => "own";
  expect(callGreet(objects[7])).toBe("own");
  expect(callGreet(objects[8])).toBe("proto");
});

test("an own property defined on a receiver that had none shadows the cached method", () => {
  const proto = ordinary(Object.prototype, { greet: () => "proto" });
  const objects = Array.from({ length: WARM }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("proto");
  }
  Object.defineProperty(objects[7], "greet", {
    value: () => "own",
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(callGreet(objects[7])).toBe("own");
  expect(callGreet(objects[8])).toBe("proto");
});

test("an own property added to an instance of a class without fields shadows the cached method", () => {
  class Greeter {
    greet() {
      return "class";
    }
  }
  const greeters = Array.from({ length: WARM }, () => new Greeter());
  const callGreet = (g) => g.greet();

  for (const g of greeters) {
    expect(callGreet(g)).toBe("class");
  }
  greeters[5].greet = () => "own";
  expect(callGreet(greeters[5])).toBe("own");
  expect(callGreet(greeters[6])).toBe("class");
});

test("an own property added to an instance that has fields shadows the cached method", () => {
  class Point {
    x;
    y;
    constructor(x, y) {
      this.x = x;
      this.y = y;
    }
    sum() {
      return this.x + this.y;
    }
  }
  const points = Array.from({ length: WARM }, (_, i) => new Point(i, 1));
  const callSum = (p) => p.sum();

  points.forEach((p, i) => {
    expect(callSum(p)).toBe(i + 1);
  });
  points[9].sum = () => "own";
  expect(callSum(points[9])).toBe("own");
  expect(callSum(points[10])).toBe(11);
});

test("an unrelated property added to a receiver leaves the method resolving, and a later shadow is seen", () => {
  const proto = ordinary(Object.prototype, { greet: () => "proto" });
  const objects = Array.from({ length: WARM }, () => ordinary(proto, { id: 1 }));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("proto");
  }
  objects[3].extra = true;
  expect(callGreet(objects[3])).toBe("proto");
  expect(callGreet(objects[3])).toBe("proto");
  objects[3].greet = () => "own";
  expect(callGreet(objects[3])).toBe("own");
  expect(callGreet(objects[4])).toBe("proto");
});

test("a property added to the holder leaves the method resolving, and a later swap is seen", () => {
  const proto = ordinary(Object.prototype, { greet: () => "old" });
  const objects = Array.from({ length: WARM }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("old");
  }
  proto.added = 1;
  for (const o of objects) {
    expect(callGreet(o)).toBe("old");
  }
  proto.greet = () => "new";
  for (const o of objects) {
    expect(callGreet(o)).toBe("new");
  }
});

test("a method deleted from its holder is looked up further along the chain", () => {
  const grandProto = ordinary(Object.prototype, { greet: () => "grand" });
  const proto = ordinary(grandProto, { greet: () => "proto" });
  const objects = Array.from({ length: WARM }, () => Object.create(proto));
  const callGreet = (o) => o.greet();

  for (const o of objects) {
    expect(callGreet(o)).toBe("proto");
  }
  delete proto.greet;
  for (const o of objects) {
    expect(callGreet(o)).toBe("grand");
  }
  delete grandProto.greet;
  expect(() => callGreet(objects[0])).toThrow(TypeError);
});

test("an unrelated property added to the prototype between leaves a two-level load resolving, and a later shadow there is seen", () => {
  const grandProto = ordinary(Object.prototype, { speak: () => "grand" });
  const middleProto = ordinary(grandProto, { kind: "middle" });
  const objects = Array.from({ length: WARM }, () => Object.create(middleProto));
  const callSpeak = (o) => o.speak();

  for (const o of objects) {
    expect(callSpeak(o)).toBe("grand");
  }
  middleProto.other = 1;
  for (const o of objects) {
    expect(callSpeak(o)).toBe("grand");
  }
  middleProto.speak = () => "middle";
  for (const o of objects) {
    expect(callSpeak(o)).toBe("middle");
  }
});

test("a shadow added to an empty prototype between is seen by a two-level load", () => {
  const grandProto = ordinary(Object.prototype, { speak: () => "grand" });
  const middleProto = Object.create(grandProto);
  const objects = Array.from({ length: WARM }, () => Object.create(middleProto));
  const callSpeak = (o) => o.speak();

  for (const o of objects) {
    expect(callSpeak(o)).toBe("grand");
  }
  middleProto.speak = () => "middle";
  for (const o of objects) {
    expect(callSpeak(o)).toBe("middle");
  }
});

test("an override added to a subclass prototype is seen by a load cached on the superclass method", () => {
  class Animal {
    name;
    constructor(name) {
      this.name = name;
    }
    speak() {
      return this.name + " makes a sound";
    }
  }
  class Dog extends Animal {
    fetch() {
      return this.name + " fetches";
    }
  }
  const dogs = Array.from({ length: WARM }, (_, i) => new Dog("d" + i));
  const callSpeak = (d) => d.speak();

  dogs.forEach((d, i) => {
    expect(callSpeak(d)).toBe("d" + i + " makes a sound");
  });
  Dog.prototype.speak = () => "woof";
  for (const d of dogs) {
    expect(callSpeak(d)).toBe("woof");
  }
});

test("one site serves prototypes with the same layout and different methods", () => {
  const protoA = ordinary(Object.prototype, { first: () => "a1", second: () => "a2" });
  const protoB = ordinary(Object.prototype, { first: () => "b1", second: () => "b2" });
  const callSecond = (o) => o.second();
  const results = [];

  Array.from({ length: WARM }).forEach(() => {
    results.push(callSecond(Object.create(protoA)));
    results.push(callSecond(Object.create(protoB)));
  });
  expect(results.filter((r) => r === "a2").length).toBe(WARM);
  expect(results.filter((r) => r === "b2").length).toBe(WARM);
});

test("one site serves prototypes that hold the method at different positions", () => {
  const protoA = ordinary(Object.prototype, { first: () => "a1", second: () => "a2" });
  const protoB = ordinary(Object.prototype, { second: () => "b2", first: () => "b1" });
  const protoC = ordinary(Object.prototype, { zero: () => "c0", first: () => "c1", second: () => "c2" });
  const callSecond = (o) => o.second();
  const callFirst = (o) => o.first();
  const a = Object.create(protoA);
  const b = Object.create(protoB);
  const c = Object.create(protoC);

  Array.from({ length: WARM }).forEach(() => {
    expect(callSecond(a)).toBe("a2");
    expect(callFirst(a)).toBe("a1");
  });
  expect(callSecond(b)).toBe("b2");
  expect(callFirst(b)).toBe("b1");
  expect(callSecond(c)).toBe("c2");
  expect(callFirst(c)).toBe("c1");
  expect(callSecond(a)).toBe("a2");
  expect(callSecond(b)).toBe("b2");
});

test("a receiver moved to a prototype with another layout resolves there", () => {
  const protoA = ordinary(Object.prototype, { label: () => "a", other: () => "a-other" });
  const protoB = ordinary(Object.prototype, { other: () => "b-other", label: () => "b" });
  const obj = Object.create(protoA);
  const readLabel = (o) => o.label();

  Array.from({ length: WARM }).forEach(() => {
    expect(readLabel(obj)).toBe("a");
  });
  Object.setPrototypeOf(obj, protoB);
  expect(readLabel(obj)).toBe("b");
  Object.setPrototypeOf(obj, protoA);
  expect(readLabel(obj)).toBe("a");
});

test("a method replaced by an accessor on the holder runs the getter", () => {
  class Counter {
    value() {
      return "method";
    }
  }
  const counters = Array.from({ length: WARM }, () => new Counter());
  const readValue = (c) => c.value;

  for (const c of counters) {
    expect(typeof readValue(c)).toBe("function");
  }
  Object.defineProperty(Counter.prototype, "value", {
    get() {
      return "getter";
    },
    configurable: true,
  });
  for (const c of counters) {
    expect(readValue(c)).toBe("getter");
  }
});
