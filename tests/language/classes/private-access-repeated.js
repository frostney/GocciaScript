/*---
description: Private member access stays correct when repeated, across many private names, and across class evaluations
features: [classes, private-fields, private-methods, private-static-fields]
---*/

test("repeated private field, method, accessor and static access", () => {
  class Counter {
    #count = 0;
    static #instances = 0;

    constructor() {
      Counter.#instances = Counter.#instances + 1;
    }

    get #doubled() {
      return this.#count * 2;
    }

    set #doubled(value) {
      this.#count = value / 2;
    }

    #step() {
      this.#count = this.#count + 1;
    }

    tick() {
      this.#step();
      return this.#count;
    }

    roundTrip() {
      this.#doubled = this.#doubled + 2;
      return #count in this;
    }

    static instances() {
      return Counter.#instances;
    }
  }

  const first = new Counter();
  const second = new Counter();
  let ticks = 0;
  let checks = 0;
  for (const _ of Array.from({ length: 200 })) {
    ticks = first.tick();
    if (second.roundTrip()) {
      checks++;
    }
  }

  expect(ticks).toBe(200);
  expect(checks).toBe(200);
  expect(second.tick()).toBe(201);
  expect(Counter.instances()).toBe(2);
});

test("a class with more private names than are remembered per class", () => {
  class Wide {
    #a = 1; #b = 2; #c = 3; #d = 4; #e = 5; #f = 6; #g = 7; #h = 8; #i = 9;
    #j = 10; #k = 11; #l = 12; #m = 13; #n = 14; #o = 15; #p = 16; #q = 17;
    #r = 18; #s = 19; #t = 20;

    #twice(value) {
      return value * 2;
    }

    sum() {
      return this.#a + this.#b + this.#c + this.#d + this.#e + this.#f +
        this.#g + this.#h + this.#i + this.#j + this.#k + this.#l + this.#m +
        this.#n + this.#o + this.#p + this.#q + this.#r + this.#s + this.#t;
    }

    bump() {
      this.#a++; this.#b++; this.#c++; this.#d++; this.#e++; this.#f++;
      this.#g++; this.#h++; this.#i++; this.#j++; this.#k++; this.#l++;
      this.#m++; this.#n++; this.#o++; this.#p++; this.#q++; this.#r++;
      this.#s++; this.#t++;
      return this.#twice(this.#t);
    }

    static read(receiver) {
      return receiver.#t;
    }
  }

  const wide = new Wide();
  let last = 0;
  for (const _ of Array.from({ length: 50 })) {
    expect(wide.sum()).toBe(210 + last * 20);
    last++;
    expect(wide.bump()).toBe((20 + last) * 2);
  }
  expect(Wide.read(wide)).toBe(70);
  expect(() => Wide.read({})).toThrow(TypeError);
});

test("brand checks still reject another evaluation after repeated access", () => {
  const createClass = () => class {
    #value;
    static #created = 0;

    constructor(value) {
      this.#value = value;
    }

    #read() {
      return this.#value;
    }

    read(receiver) {
      return receiver.#read();
    }

    readField(receiver) {
      return receiver.#value;
    }

    static count(receiver) {
      receiver.#created = receiver.#created + 1;
      return receiver.#created;
    }
  };

  const First = createClass();
  const Second = createClass();
  const first = new First("first");
  const second = new Second("second");

  for (const _ of Array.from({ length: 50 })) {
    expect(first.read(first)).toBe("first");
    expect(second.readField(second)).toBe("second");
    First.count(First);
  }

  expect(First.count(First)).toBe(51);
  expect(() => first.read(second)).toThrow(TypeError);
  expect(() => first.readField(second)).toThrow(TypeError);
  expect(() => second.read(first)).toThrow(TypeError);
  expect(() => First.count(Second)).toThrow(TypeError);
});

test("an inner class reads the outer class's private names", () => {
  class Outer {
    #secret = "outer";

    static #tag = "static";

    makeReader() {
      return class Inner {
        #own = "inner";

        read(outer) {
          return outer.#secret + ":" + this.#own + ":" + Outer.#tag;
        }
      };
    }
  }

  const outer = new Outer();
  const Reader = outer.makeReader();
  const reader = new Reader();
  for (const _ of Array.from({ length: 20 })) {
    expect(reader.read(outer)).toBe("outer:inner:static");
  }
  expect(() => reader.read(reader)).toThrow(TypeError);
});
