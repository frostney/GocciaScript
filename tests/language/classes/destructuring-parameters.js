/*---
description: Class methods support destructuring parameters
features: [class-declaration, destructuring, default-parameters]
---*/

describe("class method destructuring parameters", () => {
  test("instance methods bind array destructuring parameters", () => {
    class Calculator {
      sum([a, b, c]) {
        return a + b + c;
      }
    }

    expect(new Calculator().sum([1, 2, 3])).toBe(6);
  });

  test("computed instance methods bind object destructuring parameters", () => {
    class Calculator {
      ["sum"]({ left, right }) {
        return left + right;
      }
    }

    expect(new Calculator().sum({ left: 2, right: 5 })).toBe(7);
  });

  test("static methods bind nested destructuring parameters with defaults", () => {
    class Calculator {
      static sum({ values: [a, b = 10] }) {
        return a + b;
      }
    }

    expect(Calculator.sum({ values: [4] })).toBe(14);
    expect(Calculator.sum({ values: [4, 6] })).toBe(10);
  });

  test("methods bind rest elements in destructuring parameters", () => {
    class Calculator {
      count([first, ...rest]) {
        return first + rest.length;
      }
    }

    expect(new Calculator().count([10, 20, 30, 40])).toBe(13);
  });

  test("synthetic pattern parameter locals do not shadow source parameters", () => {
    class Calculator {
      beforePattern(__param1, [value]) {
        return __param1 + value;
      }

      afterPattern([value], __param0) {
        return value + __param0;
      }
    }

    const calculator = new Calculator();

    expect(calculator.beforePattern(3, [4])).toBe(7);
    expect(calculator.afterPattern([5], 6)).toBe(11);
  });
});
