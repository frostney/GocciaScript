/*---
description: A var initializer whose update expression throws while storing leaves the var with the value it had
features: [compat-var, update-expressions]
---*/

describe("var initializer with an update whose store throws", () => {
  test("a postfix update of a property whose setter throws", () => {
    const object = { get p() { return 1; }, set p(value) { throw new RangeError("no"); } };
    const f = (y) => {
      try { var y = object.p++; } catch (e) { return [e instanceof RangeError, y]; }
      return [false, y];
    };
    expect(f("keep")).toEqual([true, "keep"]);
  });

  test("a prefix update of a property whose setter throws", () => {
    const object = { get p() { return 1; }, set p(value) { throw new RangeError("no"); } };
    const f = (y) => {
      try { var y = ++object.p; } catch (e) { return [e instanceof RangeError, y]; }
      return [false, y];
    };
    expect(f("keep")).toEqual([true, "keep"]);
  });

  test("an update of a const", () => {
    const fixed = 1;
    const f = (y) => {
      try { var y = fixed++; } catch (e) { return [e instanceof TypeError, y]; }
      return [false, y];
    };
    expect(f("keep")).toEqual([true, "keep"]);
  });
});
