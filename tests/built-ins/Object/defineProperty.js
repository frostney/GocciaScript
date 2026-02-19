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
