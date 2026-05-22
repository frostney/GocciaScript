/*---
description: Computed public class field names evaluate in source order
features: [computed-property-names, class-fields-public, class-static-fields-public, generators]
---*/

test("computed public field names follow source order", () => {
  const obj = {
    *makeClass() {
      let C = class {
        static [yield "static-before"] = 7;
        [yield "field"] = 42;
      };
      let c = new C();
      return [C.staticBefore, c.field];
    },
  };

  const iter = obj.makeClass();

  expect(iter.next()).toEqual({ value: "static-before", done: false });
  expect(iter.next("staticBefore")).toEqual({ value: "field", done: false });
  expect(iter.next("field")).toEqual({
    value: [7, 42],
    done: true,
  });
});

test("mixed computed public field names resume in source order", () => {
  const obj = {
    *makeClass() {
      let C = class {
        static [yield "static-before"] = 7;
        [yield "field"] = 42;
        static [yield "static-after"] = 9;
      };
      let c = new C();
      return [C.staticBefore, c.field, C.staticAfter];
    },
  };

  const iter = obj.makeClass();

  expect(iter.next()).toEqual({ value: "static-before", done: false });
  expect(iter.next("staticBefore")).toEqual({ value: "field", done: false });
  expect(iter.next("field")).toEqual({ value: "static-after", done: false });
  expect(iter.next("staticAfter")).toEqual({
    value: [7, 42, 9],
    done: true,
  });
});

test("computed public instance fields define own properties", () => {
  let calls = 0;

  class Base {
    set value(next) {
      calls = calls + next;
    }
  }

  class Derived extends Base {
    ["value"] = 11;
  }

  const instance = new Derived();

  expect(calls).toBe(0);
  expect(instance.value).toBe(11);
});
