/*---
description: A var initializer inside a catch block assigns the catch parameter it redeclares (ES2026 B.3.4)
features: [compat-var]
---*/

var __gocciaCatchRedeclaredTopLevel = "outer";
let __gocciaCatchRedeclaredTopLevelInner;
try {
  throw "thrown";
} catch (__gocciaCatchRedeclaredTopLevel) {
  var __gocciaCatchRedeclaredTopLevel = "assigned";
  __gocciaCatchRedeclaredTopLevelInner = __gocciaCatchRedeclaredTopLevel;
}

let __gocciaCatchRedeclaredUnsetInner;
try {
  throw 1;
} catch (__gocciaCatchRedeclaredUnset) {
  var __gocciaCatchRedeclaredUnset = 2;
  __gocciaCatchRedeclaredUnsetInner = __gocciaCatchRedeclaredUnset;
}

describe("var redeclaring a catch parameter", () => {
  test("at script top level the initializer assigns the catch parameter", () => {
    expect(__gocciaCatchRedeclaredTopLevelInner).toBe("assigned");
    expect(__gocciaCatchRedeclaredTopLevel).toBe("outer");
    expect(globalThis.__gocciaCatchRedeclaredTopLevel).toBe("outer");
  });

  test("at script top level the var still exists when only the catch parameter was assigned", () => {
    expect(__gocciaCatchRedeclaredUnsetInner).toBe(2);
    expect(__gocciaCatchRedeclaredUnset).toBeUndefined();
    expect(Object.hasOwn(globalThis, "__gocciaCatchRedeclaredUnset")).toBe(true);
  });

  test("in a function body the initializer assigns the catch parameter", () => {
    var x = 1;
    let inner;
    try {
      throw 0;
    } catch (x) {
      var x = 5;
      inner = x;
    }
    expect(inner).toBe(5);
    expect(x).toBe(1);
  });

  test("a var with no other declaration stays undefined", () => {
    const read = () => {
      try {
        throw "thrown";
      } catch (x) {
        var x = 5;
      }
      return x;
    };
    expect(read()).toBeUndefined();
  });

  test("closures over the var and over the catch parameter see their own binding", () => {
    var x = "var";
    const readVar = () => x;
    let readCatch;
    try {
      throw "thrown";
    } catch (x) {
      readCatch = () => x;
      var x = "assigned";
    }
    expect(readVar()).toBe("var");
    expect(readCatch()).toBe("assigned");
  });

  test("a var in a block nested in the catch block assigns the catch parameter", () => {
    var x = 1;
    let inner;
    try {
      throw 0;
    } catch (x) {
      {
        var x = 5;
      }
      inner = x;
    }
    expect(inner).toBe(5);
    expect(x).toBe(1);
  });

  test("a var in a nested catch block assigns the outer catch parameter", () => {
    var x = 1;
    let inner;
    try {
      throw 0;
    } catch (x) {
      try {
        throw 2;
      } catch (y) {
        var x = 5;
      }
      inner = x;
    }
    expect(inner).toBe(5);
    expect(x).toBe(1);
  });

  test("later declarators read the assigned catch parameter", () => {
    var x;
    var y;
    let inner;
    try {
      throw 1;
    } catch (x) {
      var x = 2, y = x + 1;
      inner = [x, y];
    }
    expect(inner).toEqual([2, 3]);
    expect(x).toBeUndefined();
    expect(y).toBe(3);
  });

  test("an anonymous function initializer is named after the binding", () => {
    var x = 1;
    let inner;
    try {
      throw 0;
    } catch (x) {
      var x = () => 1;
      inner = [typeof x, x.name];
    }
    expect(inner).toEqual(["function", "x"]);
    expect(x).toBe(1);
  });

  test("arithmetic reads the assigned value, not the thrown one", () => {
    const add = () => {
      try {
        throw "a";
      } catch (x) {
        var x = 5;
        return x + 1;
      }
    };
    expect(add()).toBe(6);
  });

  test("a var without an initializer leaves the catch parameter unchanged", () => {
    var x = 1;
    let inner;
    try {
      throw 9;
    } catch (x) {
      var x;
      inner = x;
    }
    expect(inner).toBe(9);
    expect(x).toBe(1);
  });
});
