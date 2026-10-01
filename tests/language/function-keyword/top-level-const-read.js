/*---
description: A hoisted function declaration reads a top-level const declared after its first call
features: [compat-function, const-declaration, temporal-dead-zone]
---*/

const outcome = (read) => {
  try {
    return read();
  } catch (error) {
    return error.constructor.name;
  }
};

const hoistedBeforeInitialization = outcome(readLimit);
const nestedBeforeInitialization = outcome(readLimitThroughArrow);
const expressionBeforeInitialization = outcome(() => readLimitExpression());

const LIMIT = 16;

function readLimit() {
  return LIMIT;
}

function readLimitThroughArrow() {
  const read = () => LIMIT;
  return read();
}

const readLimitExpression = function () {
  return LIMIT;
};

describe("top-level const read from function declarations", () => {
  test("a hoisted function called before the declaration throws", () => {
    expect(hoistedBeforeInitialization).toBe("ReferenceError");
    expect(nestedBeforeInitialization).toBe("ReferenceError");
  });

  test("a function expression is itself uninitialized that early", () => {
    expect(expressionBeforeInitialization).toBe("ReferenceError");
  });

  test("the same functions return the value after initialization", () => {
    expect(readLimit()).toBe(16);
    expect(readLimitThroughArrow()).toBe(16);
    expect(readLimitExpression()).toBe(16);
  });

  test("a function declared in a later block reads the value", () => {
    {
      function readInBlock() {
        return LIMIT + 1;
      }
      expect(readInBlock()).toBe(17);
    }
  });
});
