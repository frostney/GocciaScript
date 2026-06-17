/*---
description: Numeric arithmetic operators complete ToNumeric(left) before ToNumeric(right)
features: [arithmetic-operators, Symbol]
---*/

const assertLeftNumericErrorPrecedesRightCoercion = (label, evaluate) => {
  let trace = "";
  const left = () => {
    trace += "1";
    return {
      valueOf() {
        trace += "3";
        return Symbol(label);
      },
    };
  };
  const right = () => {
    trace += "2";
    return {
      valueOf() {
        trace += "4";
        throw new Error("right operand must not be coerced");
      },
    };
  };

  expect(() => evaluate(left(), right())).toThrow(TypeError);
  expect(trace).toBe("123");
};

test("multiplicative operators coerce lhs numerically before rhs", () => {
  assertLeftNumericErrorPrecedesRightCoercion("*", (left, right) => left * right);
  assertLeftNumericErrorPrecedesRightCoercion("/", (left, right) => left / right);
  assertLeftNumericErrorPrecedesRightCoercion("%", (left, right) => left % right);
});

test("exponentiation coerces lhs numerically before rhs", () => {
  assertLeftNumericErrorPrecedesRightCoercion("**", (left, right) => left ** right);
});
