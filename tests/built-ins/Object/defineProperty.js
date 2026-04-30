/*---
description: Object property descriptors edge cases and configurations
features: [Object.defineProperty, Object.getOwnPropertyDescriptor, property-descriptors]
---*/

test("Object.defineProperty with writable attribute", () => {
  const obj = {};

  // Define writable property
  Object.defineProperty(obj, "writable", {
    value: "initial",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  // Define non-writable property
  Object.defineProperty(obj, "readonly", {
    value: "constant",
    writable: false,
    enumerable: true,
    configurable: true,
  });

  // Writable property can be changed
  obj.writable = "modified";
  expect(obj.writable).toBe("modified");

  // Non-writable property cannot be changed
  expect(() => {
    obj.readonly = "try to change";
  }).toThrow(TypeError);
  expect(obj.readonly).toBe("constant");

  // Verify descriptors
  const writableDesc = Object.getOwnPropertyDescriptor(obj, "writable");
  expect(writableDesc.writable).toBe(true);

  const readonlyDesc = Object.getOwnPropertyDescriptor(obj, "readonly");
  expect(readonlyDesc.writable).toBe(false);
});

test("Object.defineProperty with enumerable attribute", () => {
  const obj = {};

  // Define enumerable property
  Object.defineProperty(obj, "visible", {
    value: "shown",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  // Define non-enumerable property
  Object.defineProperty(obj, "hidden", {
    value: "secret",
    writable: true,
    enumerable: false,
    configurable: true,
  });

  // Both properties exist and are accessible
  expect(obj.visible).toBe("shown");
  expect(obj.hidden).toBe("secret");

  // Only enumerable properties show up in enumeration
  const keys = Object.keys(obj);
  expect(keys).toContain("visible");
  expect(keys).not.toContain("hidden");

  // Object.getOwnPropertyNames returns all properties (enumerable and non-enumerable)
  const allKeys = Object.getOwnPropertyNames(obj);
  expect(allKeys).toContain("visible");
  expect(allKeys).toContain("hidden"); // getOwnPropertyNames should include non-enumerable properties

  // Object.values and Object.entries also respect enumerable
  const values = Object.values(obj);
  expect(values).toContain("shown");
  expect(values).not.toContain("secret");
});

test("Object.defineProperty with configurable attribute", () => {
  const obj = {};

  // Define configurable property
  Object.defineProperty(obj, "flexible", {
    value: "changeable",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  // Define non-configurable property
  Object.defineProperty(obj, "fixed", {
    value: "permanent",
    writable: true,
    enumerable: true,
    configurable: false,
  });

  // Configurable property can be reconfigured
  Object.defineProperty(obj, "flexible", {
    enumerable: false,
  });

  const flexibleDesc = Object.getOwnPropertyDescriptor(obj, "flexible");
  expect(flexibleDesc.enumerable).toBe(false);

  // Non-configurable property cannot be reconfigured
  expect(() => {
    Object.defineProperty(obj, "fixed", {
      enumerable: false,
    });
  }).toThrow(TypeError);

  // Configurable property can be deleted
  delete obj.flexible;
  expect(obj.flexible).toBeUndefined();

  // Non-configurable property cannot be deleted (strict mode throws TypeError)
  expect(() => {
    delete obj.fixed;
  }).toThrow(TypeError);
  expect(obj.fixed).toBe("permanent");
});

test("Object.defineProperty with getter and setter", () => {
  const obj = {};
  let internalValue = 0;
  let getterCallCount = 0;
  let setterCallCount = 0;

  Object.defineProperty(obj, "computed", {
    get() {
      getterCallCount++;
      return internalValue * 2;
    },
    set(value) {
      setterCallCount++;
      internalValue = value / 2;
    },
    enumerable: true,
    configurable: true,
  });

  // Test getter
  expect(obj.computed).toBe(0); // 0 * 2
  expect(getterCallCount).toBe(1);

  // Test setter
  obj.computed = 10;
  expect(setterCallCount).toBe(1);
  expect(internalValue).toBe(5); // 10 / 2

  // Test getter again
  expect(obj.computed).toBe(10); // 5 * 2
  expect(getterCallCount).toBe(2);

  // Getter/setter descriptors
  const desc = Object.getOwnPropertyDescriptor(obj, "computed");
  expect(desc.get).toBeInstanceOf(Function);
  expect(desc.set).toBeInstanceOf(Function);
  expect(desc.value).toBeUndefined();
  expect(desc.writable).toBeUndefined();
});

test("Object.defineProperty with inheritance and shadowing", () => {
  const parent = {};
  Object.defineProperty(parent, "inherited", {
    value: "from parent",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  const child = Object.create(parent);

  // Child inherits parent's property
  expect(child.inherited).toBe("from parent");

  // But descriptor is not inherited
  const childDesc = Object.getOwnPropertyDescriptor(child, "inherited");
  expect(childDesc).toBeUndefined();

  // Parent's descriptor is still there
  const parentDesc = Object.getOwnPropertyDescriptor(parent, "inherited");
  expect(parentDesc.value).toBe("from parent");

  // Child can shadow with its own descriptor
  Object.defineProperty(child, "inherited", {
    value: "from child",
    writable: false,
    enumerable: false,
    configurable: true,
  });

  expect(child.inherited).toBe("from child");
  expect(parent.inherited).toBe("from parent");

  const newChildDesc = Object.getOwnPropertyDescriptor(child, "inherited");
  expect(newChildDesc.value).toBe("from child");
  expect(newChildDesc.writable).toBe(false);
  expect(newChildDesc.enumerable).toBe(false);
});

test("Object.defineProperty with symbol keys", () => {
  const obj = {};
  const sym1 = Symbol("test1");
  const sym2 = Symbol("test2");

  Object.defineProperty(obj, sym1, {
    value: "symbol value 1",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  Object.defineProperty(obj, sym2, {
    value: "symbol value 2",
    writable: false,
    enumerable: false,
    configurable: false,
  });

  expect(obj[sym1]).toBe("symbol value 1");
  expect(obj[sym2]).toBe("symbol value 2");

  // Symbol properties have their own enumeration methods
  const symbolKeys = Object.getOwnPropertySymbols(obj);
  expect(symbolKeys).toContain(sym1);
  expect(symbolKeys).toContain(sym2);

  // Regular enumeration doesn't include symbols
  const regularKeys = Object.keys(obj);
  expect(regularKeys).not.toContain(sym1);
  expect(regularKeys).not.toContain(sym2);
});

test("Object.defineProperty with default values", () => {
  const obj = {};

  // Using Object.defineProperty without specifying all attributes
  Object.defineProperty(obj, "minimal", {
    value: "test",
    // writable, enumerable, configurable default to false
  });

  const desc = Object.getOwnPropertyDescriptor(obj, "minimal");
  expect(desc.value).toBe("test");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);

  // Property exists but is not enumerable
  expect(obj.minimal).toBe("test");
  expect(Object.keys(obj)).not.toContain("minimal");

  // Property is not writable - should throw TypeError in strict mode
  expect(() => {
    obj.minimal = "try to change";
  }).toThrow(TypeError);
  expect(obj.minimal).toBe("test");
});

test("Object.defineProperty with modification constraints", () => {
  const obj = {};

  // Define a non-configurable, non-writable property
  Object.defineProperty(obj, "constant", {
    value: "unchangeable",
    writable: false,
    enumerable: true,
    configurable: false,
  });

  // Cannot change value
  expect(() => {
    Object.defineProperty(obj, "constant", {
      value: "new value",
    });
  }).toThrow(TypeError);

  // Cannot change writable from false to true
  expect(() => {
    Object.defineProperty(obj, "constant", {
      writable: true,
    });
  }).toThrow(TypeError);

  // Cannot change enumerable
  expect(() => {
    Object.defineProperty(obj, "constant", {
      enumerable: false,
    });
  }).toThrow(TypeError);

  // Cannot change configurable from false to true
  expect(() => {
    Object.defineProperty(obj, "constant", {
      configurable: true,
    });
  }).toThrow(TypeError);

  // Property remains unchanged
  expect(obj.constant).toBe("unchangeable");
  const desc = Object.getOwnPropertyDescriptor(obj, "constant");
  expect(desc.writable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("Object.defineProperty merges with existing descriptor", () => {
  const obj = {};

  Object.defineProperty(obj, "merged", {
    value: "original",
    writable: true,
    enumerable: true,
    configurable: true,
  });

  // Update only enumerable — other attributes should be preserved
  Object.defineProperty(obj, "merged", { enumerable: false });

  const desc = Object.getOwnPropertyDescriptor(obj, "merged");
  expect(desc.value).toBe("original");
  expect(desc.writable).toBe(true);
  expect(desc.configurable).toBe(true);
  expect(desc.enumerable).toBe(false);

  // Update only value — other attributes should be preserved
  Object.defineProperty(obj, "merged", { value: "updated" });

  const desc2 = Object.getOwnPropertyDescriptor(obj, "merged");
  expect(desc2.value).toBe("updated");
  expect(desc2.writable).toBe(true);
  expect(desc2.configurable).toBe(true);
  expect(desc2.enumerable).toBe(false);

  // Update only writable — other attributes should be preserved
  Object.defineProperty(obj, "merged", { writable: false });

  const desc3 = Object.getOwnPropertyDescriptor(obj, "merged");
  expect(desc3.value).toBe("updated");
  expect(desc3.writable).toBe(false);
  expect(desc3.configurable).toBe(true);
  expect(desc3.enumerable).toBe(false);
});

test("Object.defineProperty with empty object", () => {
  const obj = {};
  Object.defineProperty(obj, "empty", {});
  expect(obj.empty).toBeUndefined();
});

test("Object.defineProperty rejects mixed data and accessor descriptors", () => {
  const obj = {};

  // value + get is invalid
  expect(() => {
    Object.defineProperty(obj, "mixed1", {
      value: 1,
      get: () => 2,
    });
  }).toThrow(TypeError);

  // value + set is invalid
  expect(() => {
    Object.defineProperty(obj, "mixed2", {
      value: 1,
      set: (v) => {},
    });
  }).toThrow(TypeError);

  // writable + get is invalid
  expect(() => {
    Object.defineProperty(obj, "mixed3", {
      writable: true,
      get: () => 2,
    });
  }).toThrow(TypeError);

  // writable + set is invalid
  expect(() => {
    Object.defineProperty(obj, "mixed4", {
      writable: false,
      set: (v) => {},
    });
  }).toThrow(TypeError);
});

test("Object.defineProperty rejects non-callable getter and setter", () => {
  const obj = {};

  // getter must be a function or undefined
  expect(() => {
    Object.defineProperty(obj, "badGet", {
      get: 42,
    });
  }).toThrow(TypeError);

  expect(() => {
    Object.defineProperty(obj, "badGet2", {
      get: "not a function",
    });
  }).toThrow(TypeError);

  // setter must be a function or undefined
  expect(() => {
    Object.defineProperty(obj, "badSet", {
      set: 42,
    });
  }).toThrow(TypeError);

  expect(() => {
    Object.defineProperty(obj, "badSet2", {
      set: "not a function",
    });
  }).toThrow(TypeError);

  // undefined getter/setter is allowed (getter-only or setter-only)
  Object.defineProperty(obj, "getOnly", {
    get: () => 99,
    set: undefined,
    configurable: true,
  });
  expect(obj.getOnly).toBe(99);

  Object.defineProperty(obj, "setOnly", {
    get: undefined,
    set: (v) => {},
    configurable: true,
  });
  expect(obj.setOnly).toBeUndefined();
});

// Non-configurable property redefinition — allowed cases (§10.1.6.3)

test("non-configurable writable data: value change allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 1, writable: true, configurable: false });
  Object.defineProperty(obj, "x", { value: 2 });
  expect(obj.x).toBe(2);
});

test("non-configurable writable data: writable false transition allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", { value: 1, writable: true, configurable: false });
  Object.defineProperty(obj, "x", { writable: false });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.writable).toBe(false);
  expect(desc.value).toBe(1);
});

test("non-configurable data: identical redefinition is no-op", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 42,
    writable: false,
    enumerable: true,
    configurable: false,
  });
  Object.defineProperty(obj, "x", {
    value: 42,
    writable: false,
    enumerable: true,
    configurable: false,
  });
  expect(obj.x).toBe(42);
});

test("non-configurable non-writable data: value change rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: false,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { value: 2 });
  }).toThrow(TypeError);
  expect(obj.x).toBe(1);
});

test("non-configurable: writable false to true rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: false,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { writable: true });
  }).toThrow(TypeError);
});

test("non-configurable: data to accessor transition rejected", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", {
      get: () => 2,
    });
  }).toThrow(TypeError);
});

test("non-configurable accessor: getter change rejected", () => {
  const getter1 = () => 1;
  const getter2 = () => 2;
  const obj = {};
  Object.defineProperty(obj, "x", {
    get: getter1,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { get: getter2 });
  }).toThrow(TypeError);
});

test("non-configurable accessor: setter change rejected", () => {
  const setter1 = (v) => {};
  const setter2 = (v) => {};
  const obj = {};
  Object.defineProperty(obj, "x", {
    set: setter1,
    configurable: false,
  });
  expect(() => {
    Object.defineProperty(obj, "x", { set: setter2 });
  }).toThrow(TypeError);
});

test("non-configurable accessor: identical redefinition is no-op", () => {
  const getter = () => 42;
  const setter = (v) => {};
  const obj = {};
  Object.defineProperty(obj, "x", {
    get: getter,
    set: setter,
    enumerable: true,
    configurable: false,
  });
  Object.defineProperty(obj, "x", {
    get: getter,
    set: setter,
    enumerable: true,
    configurable: false,
  });
  expect(obj.x).toBe(42);
});

// Data-to-accessor descriptor transitions (configurable)

test("configurable: data to accessor transition allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    configurable: true,
  });
  Object.defineProperty(obj, "x", {
    get: () => 2,
    configurable: true,
  });
  expect(obj.x).toBe(2);
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(typeof desc.get).toBe("function");
  expect(desc.value).toBeUndefined();
});

test("configurable: accessor to data transition allowed", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    get: () => 1,
    configurable: true,
  });
  Object.defineProperty(obj, "x", {
    value: 2,
    writable: true,
    configurable: true,
  });
  expect(obj.x).toBe(2);
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.writable).toBe(true);
  expect(desc.get).toBeUndefined();
});

// Descriptor flag defaults — absent fields preserve existing values

test("redefine preserves flags when only value changes", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  Object.defineProperty(obj, "x", { value: 2 });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.value).toBe(2);
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(true);
  expect(desc.configurable).toBe(true);
});

test("redefine on non-configurable writable preserves flags", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    enumerable: true,
    configurable: false,
  });
  Object.defineProperty(obj, "x", { value: 2 });
  const desc = Object.getOwnPropertyDescriptor(obj, "x");
  expect(desc.value).toBe(2);
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(true);
  expect(desc.configurable).toBe(false);
});

// Reflect.defineProperty boolean variant

test("Reflect.defineProperty returns false for non-configurable violations", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: false,
    configurable: false,
  });
  expect(Reflect.defineProperty(obj, "x", { value: 2 })).toBe(false);
  expect(obj.x).toBe(1);
});

test("Reflect.defineProperty returns true for allowed non-configurable updates", () => {
  const obj = {};
  Object.defineProperty(obj, "x", {
    value: 1,
    writable: true,
    configurable: false,
  });
  expect(Reflect.defineProperty(obj, "x", { value: 2 })).toBe(true);
  expect(obj.x).toBe(2);
});

// Array length + defineProperty interaction (§10.4.2)

test("defineProperty on array length truncates array", () => {
  const arr = [1, 2, 3, 4, 5];
  Object.defineProperty(arr, "length", { value: 2 });
  expect(arr.length).toBe(2);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBeUndefined();
});

test("defineProperty on array length extends array", () => {
  const arr = [1, 2];
  Object.defineProperty(arr, "length", { value: 5 });
  expect(arr.length).toBe(5);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe(2);
  expect(arr[2]).toBeUndefined();
});

test("defineProperty on array length to 0 empties array", () => {
  const arr = [1, 2, 3];
  Object.defineProperty(arr, "length", { value: 0 });
  expect(arr.length).toBe(0);
  expect(arr[0]).toBeUndefined();
});

test("defineProperty on numeric index updates element", () => {
  const arr = [1, 2, 3];
  Object.defineProperty(arr, "0", { value: 99 });
  expect(arr[0]).toBe(99);
  expect(arr.length).toBe(3);
});

test("defineProperty on numeric index beyond length extends array", () => {
  const arr = [1, 2];
  Object.defineProperty(arr, "5", { value: 42 });
  expect(arr[5]).toBe(42);
  expect(arr.length).toBe(6);
  expect(arr[2]).toBeUndefined();
  expect(arr[3]).toBeUndefined();
  expect(arr[4]).toBeUndefined();
});

test("defineProperty on array non-index property", () => {
  const arr = [1, 2, 3];
  Object.defineProperty(arr, "foo", {
    value: "bar",
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(arr.foo).toBe("bar");
  expect(arr.length).toBe(3);
});

test("defineProperty installs accessor descriptor on array index", () => {
  const arr = ["foo", "bar"];
  let calls = 0;
  Object.defineProperty(arr, "0", {
    get: () => {
      calls = calls + 1;
      return "from-getter";
    },
    configurable: true,
  });
  expect(arr[0]).toBe("from-getter");
  expect(calls).toBe(1);
  expect(arr[1]).toBe("bar");
  expect(arr.length).toBe(2);
});

test("defineProperty accessor on array index propagates getter throws", () => {
  const sentinel = new Error("getter abrupt");
  const arr = ["foo", "bar"];
  Object.defineProperty(arr, "0", {
    get: () => {
      throw sentinel;
    },
    configurable: true,
  });
  // Asserting on the exact thrown instance (and its message) verifies the
  // getter's abrupt completion is propagated unchanged — `.toThrow(Error)`
  // alone would also pass for any unrelated Error from elsewhere in the
  // [[Get]] path.
  let caught;
  try {
    arr[0];
  } catch (e) {
    caught = e;
  }
  expect(caught).toBe(sentinel);
  expect(caught.message).toBe("getter abrupt");
  // Other indices remain accessible.
  expect(arr[1]).toBe("bar");
});

test("defineProperty accessor on out-of-range array index extends length", () => {
  const arr = ["a", "b"];
  expect(arr.length).toBe(2);
  Object.defineProperty(arr, "5", {
    get: () => "from-getter",
    configurable: true,
  });
  // Per ES2026 §10.4.2.1, the array's length must reflect the new own property.
  expect(arr.length).toBe(6);
  expect(arr[5]).toBe("from-getter");
  // Intermediate slots must be holes (no own property), not explicit
  // undefined values. Use the `in` operator to enforce sparse semantics —
  // a plain `arr[i] === undefined` check would also accept an own
  // `{ value: undefined }` data property.
  expect(2 in arr).toBe(false);
  expect(3 in arr).toBe(false);
  expect(4 in arr).toBe(false);
});

test("defineProperty accessor on array rolls back when redefinition is rejected", () => {
  // When the inherited call rejects an accessor redefinition (e.g., the
  // existing descriptor is non-configurable), the array's backing storage
  // must not be mutated — length and existing values must stay intact.
  const arr = ["a", "b"];
  Object.defineProperty(arr, "5", {
    get: () => "first",
    configurable: false,
  });
  expect(arr.length).toBe(6);
  expect(arr[5]).toBe("first");

  // Per ES §10.1.6.3, redefining a non-configurable own property must throw
  // TypeError specifically — assert the type, not just that something throws.
  expect(() => {
    Object.defineProperty(arr, "5", {
      get: () => "second",
      configurable: true,
    });
  }).toThrow(TypeError);

  // No partial state mutation: length unchanged, original getter still in place.
  expect(arr.length).toBe(6);
  expect(arr[5]).toBe("first");
});
