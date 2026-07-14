/*---
description: Object creation, property access, and object operations work correctly
features: [Object, property-access, object-literals]
---*/

test("basic object creation and property access", () => {
  const obj = { name: "Alice", age: 30, active: true };
  expect(obj.name).toBe("Alice");
  expect(obj.age).toBe(30);
  expect(obj.active).toBeTruthy();
  expect(obj.nonExistent).toBeUndefined();
});

test("object creation with various property types", () => {
  const complexObj = {
    string: "text",
    number: 42,
    boolean: true,
    null: null,
    undefined: undefined,
    array: [1, 2, 3],
    nested: { inner: "value" },
    method() {
      return "method result";
    },
    arrow: () => "arrow result",
    computed: 10 + 20,
  };

  expect(complexObj.string).toBe("text");
  expect(complexObj.number).toBe(42);
  expect(complexObj.boolean).toBe(true);
  expect(complexObj.null).toBe(null);
  expect(complexObj.undefined).toBe(undefined);
  expect(complexObj.array).toEqual([1, 2, 3]);
  expect(complexObj.nested.inner).toBe("value");
  expect(complexObj.method()).toBe("method result");
  expect(complexObj.arrow()).toBe("arrow result");
  expect(complexObj.computed).toBe(30);
});

test("object property enumeration and inspection", () => {
  const obj = { a: 1, b: 2, c: 3 };

  const keys = Object.keys(obj);
  expect(keys).toEqual(["a", "b", "c"]);

  const values = Object.values(obj);
  expect(values).toEqual([1, 2, 3]);

  const entries = Object.entries(obj);
  expect(entries).toEqual([
    ["a", 1],
    ["b", 2],
    ["c", 3],
  ]);

  expect(Object.hasOwn(obj, "a")).toBe(true);
  expect(Object.hasOwn(obj, "d")).toBe(false);
});

test("object literals define own data properties over inherited descriptors", () => {
  Object.defineProperty(Object.prototype, "literalReadOnly", {
    value: 100,
    writable: false,
    configurable: true,
  });
  Object.defineProperty(Object.prototype, "literalAccessor", {
    get: () => false,
    configurable: true,
  });

  try {
    const staticObj = { literalReadOnly: 12 };
    expect(Object.hasOwn(staticObj, "literalReadOnly")).toBe(true);
    expect(staticObj.literalReadOnly).toBe(12);

    const accessorKey = "literalAccessor";
    const computedObj = { [accessorKey]: true };
    expect(Object.hasOwn(computedObj, "literalAccessor")).toBe(true);
    expect(computedObj.literalAccessor).toBe(true);
  } finally {
    delete Object.prototype.literalReadOnly;
    delete Object.prototype.literalAccessor;
  }
});

test("duplicate static object keys evaluate final definition once", () => {
  let calls = 0;
  const mark = (value) => {
    calls += 1;
    return value;
  };

  const obj = {
    value() {
      return "method";
    },
    value: mark("data"),
  };

  expect(calls).toBe(1);
  expect(obj.value).toBe("data");
});

test("duplicate static object keys evaluate every source-order initializer", () => {
  const order = [];
  const mark = (label, value) => {
    order.push(label);
    return value;
  };

  const obj = {
    value: mark("first", "first"),
    other: mark("middle", "middle"),
    value: mark("second", "second"),
  };

  expect(order).toEqual(["first", "middle", "second"]);
  expect(obj.value).toBe("second");
  expect(obj.other).toBe("middle");
});

test("duplicate static object definitions take effect in source order", () => {
  const first = () => "first";
  const second = () => "second";
  const object = {
    value: first,
    get value() {
      return second;
    },
    value: 42,
  };

  expect(object.value).toBe(42);
  expect(Object.getOwnPropertyDescriptor(object, "value").value).toBe(42);
});

test("object spread defines own data properties over inherited descriptors", () => {
  const source = { spreadReadOnly: 42 };
  Object.defineProperty(Object.prototype, "spreadReadOnly", {
    value: 100,
    writable: false,
    configurable: true,
  });

  try {
    const obj = { ...source };
    expect(Object.hasOwn(obj, "spreadReadOnly")).toBe(true);
    expect(obj.spreadReadOnly).toBe(42);
  } finally {
    delete Object.prototype.spreadReadOnly;
  }
});

// TODO: We don't have a test to support creating objects with prototypes because we only support arrow functions.
test("object creation with arrow function should throw a type error", () => {
  expect(() => {
    const Person = (name, age) => {
      this.name = name;
      this.age = age;
    };

    Person.prototype.greet = () => {
      return `Hello, I'm ${this.name}`;
    };

    Person.prototype.getAge = () => {
      return this.age;
    };

    const person1 = new Person("Alice", 30);
    const person2 = new Person("Bob", 25);
  }).toThrow(TypeError);
});
