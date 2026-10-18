/*---
description: An inherited setter that deletes itself from the prototype runs once and the assignment completes
features: [class-fields-public]
---*/

test("an instance assignment through a setter that deletes itself", () => {
  class A {
    set x(v) {
      delete A.prototype.x;
      this.y = v;
    }
  }
  const a = new A();
  a.x = 1;
  expect(a.y).toBe(1);
  expect("x" in A.prototype).toBe(false);
  a.x = 2;
  expect(a.y).toBe(1);
  expect(a.x).toBe(2);
});

test("an instance assignment through a setter that redefines itself", () => {
  class B {
    set x(v) {
      Object.defineProperty(B.prototype, "x", {
        set(w) {
          this.z = w;
        },
        configurable: true,
      });
      this.y = v;
    }
  }
  const b = new B();
  b.x = 1;
  b.x = 2;
  expect(b.y).toBe(1);
  expect(b.z).toBe(2);
});
