/*---
description: Literal property names in class elements follow ECMA-262 PropertyName grammar
features: [class, class-methods, class-getters, class-setters]
---*/

test("string and numeric literal method names in classes", () => {
  class C {
    "dash-key"() {
      return "string method";
    }

    1() {
      return "numeric method";
    }

    if() {
      return "keyword method";
    }
  }

  const c = new C();
  expect(c["dash-key"]()).toBe("string method");
  expect(c[1]()).toBe("numeric method");
  expect(c.if()).toBe("keyword method");
});

test("string and numeric literal accessor names in classes", () => {
  class C {
    constructor() {
      this.last = undefined;
    }

    get "dash-key"() {
      return "string getter";
    }

    get 1() {
      return "numeric getter";
    }

    set 2(value) {
      this.last = value;
    }

    get if() {
      return "keyword getter";
    }
  }

  const c = new C();
  expect(c["dash-key"]).toBe("string getter");
  expect(c[1]).toBe("numeric getter");
  c[2] = 42;
  expect(c.last).toBe(42);
  expect(c.if).toBe("keyword getter");
});

test("static literal accessor names in classes", () => {
  let stored = 0;

  class C {
    static get "dash-key"() {
      return "static string getter";
    }

    static get 1() {
      return "static numeric getter";
    }

    static set 2(value) {
      stored = value;
    }

    static get if() {
      return "static keyword getter";
    }
  }

  expect(C["dash-key"]).toBe("static string getter");
  expect(C[1]).toBe("static numeric getter");
  C[2] = 7;
  expect(stored).toBe(7);
  expect(C.if).toBe("static keyword getter");
});
