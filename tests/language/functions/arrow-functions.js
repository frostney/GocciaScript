/*---
description: Arrow function semantics
features: [arrow-function]
---*/

describe("arrow functions", () => {
  test("cannot be used with new", () => {
    const Foo = () => {};

    expect(() => {
      new Foo();
    }).toThrow(TypeError);
  });

  test("support expression and block bodies", () => {
    const add = (a, b) => a + b;
    const noReturn = (x) => {
      x + 1;
    };

    expect(add(2, 3)).toBe(5);
    expect(noReturn(5)).toBe(undefined);
  });

  test("support empty, rest, and default parameters", () => {
    const greet = () => "hello";
    const sum = (...nums) => nums.reduce((a, b) => a + b, 0);
    const greetName = (name = "world") => `hello ${name}`;

    expect(greet()).toBe("hello");
    expect(sum(1, 2, 3)).toBe(6);
    expect(greetName()).toBe("hello world");
    expect(greetName("Alice")).toBe("hello Alice");
  });

  test("support immediate invocation, object returns, and currying", () => {
    const makeObj = (x) => ({ value: x });
    const curriedAdd = (a) => (b) => a + b;

    expect(((x) => x * x)(5)).toBe(25);
    expect(makeObj(10).value).toBe(10);
    expect(curriedAdd(3)(4)).toBe(7);
  });

  test("work as callbacks and closures", () => {
    const numbers = [1, 2, 3, 4, 5];
    const doubled = numbers.map((n) => n * 2);

    const makeCounter = () => {
      let count = 0;
      return () => {
        count = count + 1;
        return count;
      };
    };

    const counter = makeCounter();

    expect(doubled).toEqual([2, 4, 6, 8, 10]);
    expect(counter()).toBe(1);
    expect(counter()).toBe(2);
    expect(counter()).toBe(3);
  });

  test("unparenthesized single parameter", () => {
    const double = x => x * 2;
    expect(double(5)).toBe(10);

    // As callback argument
    expect([1, 2, 3].map(n => n + 10)).toEqual([11, 12, 13]);

    // In new expression arguments
    const p = new Promise(resolve => resolve(42));
    expect(p instanceof Promise).toBe(true);

    // Nested unparenthesized
    const add = a => b => a + b;
    expect(add(3)(4)).toBe(7);

    // With block body
    const greet = name => { return `hi ${name}` };
    expect(greet("world")).toBe("hi world");
  });

  test("async unparenthesized single parameter", async () => {
    // Standalone
    const doubleAsync = async x => x * 2;
    expect(await doubleAsync(5)).toBe(10);

    // In new expression arguments (sync executor, not async anti-pattern)
    const p = new Promise(resolve => resolve(99));
    expect(await p).toBe(99);

    // As callback
    const results = await Promise.all([1, 2, 3].map(async n => n * 10));
    expect(results).toEqual([10, 20, 30]);
  });

  test("support object and array destructuring parameters", () => {
    const getX = ({ x }) => x;
    const first = ([a]) => a;

    expect(getX({ x: 42, y: 10 })).toBe(42);
    expect(first([10, 20, 30])).toBe(10);
  });
});
