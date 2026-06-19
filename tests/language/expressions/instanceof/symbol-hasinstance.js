test("instanceof calls Symbol.hasInstance on object right-hand side", () => {
  let thisValue;
  let argument;
  let callCount = 0;

  const right = {
    [Symbol.hasInstance](value) {
      thisValue = this;
      argument = value;
      callCount++;
      return value === 42;
    }
  };

  expect(42 instanceof right).toBe(true);
  expect(thisValue).toBe(right);
  expect(argument).toBe(42);
  expect(callCount).toBe(1);

  expect(41 instanceof right).toBe(false);
  expect(callCount).toBe(2);
});

test("instanceof requires callable fallback when Symbol.hasInstance is nullish", () => {
  const right = {};
  right[Symbol.hasInstance] = null;

  expect(() => 0 instanceof right).toThrow(TypeError);
});

test("Function.prototype Symbol.hasInstance has intrinsic descriptor", () => {
  const descriptor = Object.getOwnPropertyDescriptor(
    Function.prototype,
    Symbol.hasInstance
  );

  expect(typeof descriptor.value).toBe("function");
  expect(descriptor.writable).toBe(false);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(false);
});
