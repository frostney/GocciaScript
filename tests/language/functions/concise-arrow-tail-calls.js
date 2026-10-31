/*---
description: A call in tail position of a concise arrow body reuses the caller's frame, so recursion through it is not bounded by the stack limit
features: [arrow-function, tail-call-optimization]
---*/

// ES2026 §15.10.1 IsInTailPosition treats a ConciseBody like a FunctionBody,
// and §15.10.2 HasCallInTailPosition places the call, both branches of a
// conditional, the right operand of &&, || and ??, and the last operand of a
// comma expression in tail position. The default stack limit allows far fewer
// nested calls than these tests make.

const DEPTH = 100000;

describe("concise arrow bodies call in tail position", () => {
  test("a plain call", () => {
    let countDown;
    const relay = (n) => countDown(n);
    countDown = (n) => (n === 0 ? "done" : relay(n - 1));

    expect(relay(DEPTH)).toBe("done");
  });

  test("the alternate of a conditional", () => {
    const countDown = (n) => (n === 0 ? "done" : countDown(n - 1));

    expect(countDown(DEPTH)).toBe("done");
  });

  test("the consequent of a conditional", () => {
    const countDown = (n) => (n !== 0 ? countDown(n - 1) : "done");

    expect(countDown(DEPTH)).toBe("done");
  });

  test("the right operand of &&", () => {
    const countDown = (n) => n !== 0 && countDown(n - 1);

    expect(countDown(DEPTH)).toBe(false);
  });

  test("the right operand of ||", () => {
    const countDown = (n) => n === 0 || countDown(n - 1);

    expect(countDown(DEPTH)).toBe(true);
  });

  test("the right operand of ??", () => {
    const countDown = (n) => (n === 0 ? "done" : null) ?? countDown(n - 1);

    expect(countDown(DEPTH)).toBe("done");
  });

  test("the last operand of a comma expression", () => {
    let steps = 0;
    const countDown = (n) => (n === 0 ? steps : (steps++, countDown(n - 1)));

    expect(countDown(DEPTH)).toBe(DEPTH);
  });

  test("a parenthesized call", () => {
    const countDown = (n) => (n === 0 ? "done" : (countDown(n - 1)));

    expect(countDown(DEPTH)).toBe("done");
  });

  test("a method call", () => {
    const counter = {
      countDown: (n) => (n === 0 ? "done" : counter.countDown(n - 1)),
    };

    expect(counter.countDown(DEPTH)).toBe("done");
  });

  test("a tagged template", () => {
    const countDown = (strings, n) => (n === 0 ? "done" : countDown`${n - 1}`);

    expect(countDown`${DEPTH}`).toBe("done");
  });

  test("nested tail forms", () => {
    const countDown = (n) =>
      n === 0
        ? "done"
        : n % 2 === 0
          ? (n < 0 ? "never" : null) ?? countDown(n - 1)
          : (n, n > 0 && countDown(n - 1));

    expect(countDown(DEPTH)).toBe("done");
  });

  test("a numeric self-recursion", () => {
    const run = () => {
      const countDown = (n) => (n <= 0 ? 0 : countDown(n - 1));
      return countDown(100000);
    };

    expect(run()).toBe(0);
  });

  test("a numeric self-recursion mixing tail and non-tail calls", () => {
    const run = () => {
      const steps = (n) => (n <= 0 ? 0 : n < 5 ? steps(n - 1) : steps(n - 1) + 1);
      return steps(10);
    };

    expect(run()).toBe(6);
  });
});

describe("calls outside tail position keep their frames", () => {
  test("an operand of an operator is not a tail call", () => {
    const count = (n) => (n === 0 ? 0 : 1 + count(n - 1));

    expect(() => count(DEPTH)).toThrow(RangeError);
  });

  test("an async concise body is not a tail position", async () => {
    const countDown = async (n) => (n === 0 ? "done" : countDown(n - 1));

    await expect(countDown(DEPTH)).rejects.toThrow(RangeError);
  });
});
