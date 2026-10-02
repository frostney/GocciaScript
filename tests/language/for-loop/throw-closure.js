/*---
description: throw inside for-loop leaves per-iteration bindings with their closures and keeps later variables independent
features: [compat-traditional-for-loop]
---*/

const id = (value) => value;

describe("for-loop throw with closure capture", () => {
  test("each iteration that throws keeps its own bindings", () => {
    const getters = [];
    for (let i = 0; i < 3; i++) {
      try {
        let captured = i * 10;
        getters.push(() => i, () => captured);
        throw new Error("leave the block");
      } catch (error) {}
    }
    expect(getters.map((getter) => getter())).toEqual([0, 0, 1, 10, 2, 20]);
  });

  test("closures survive register reuse after a throw leaves the loop", () => {
    const callbacks = [];
    try {
      for (let i = 0; i < 3; i++) {
        let captured = id(i + 1);
        callbacks.push(
          () => captured,
          () => { captured = captured + 100; },
          () => i,
          () => { i = i + 50; },
        );
        if (i === 1) throw new Error("leave the loop");
      }
    } catch (error) {}
    {
      const first = id(7);
      const second = id(8);
      const third = id(9);
      const fourth = id(10);
      const fifth = id(11);
      const sixth = id(12);
      callbacks[5]();
      callbacks[7]();
      expect([first, second, third, fourth, fifth, sixth])
        .toEqual([7, 8, 9, 10, 11, 12]);
      expect(callbacks[4]()).toBe(102);
      expect(callbacks[6]()).toBe(51);
      expect(callbacks[0]()).toBe(1);
      expect(callbacks[2]()).toBe(0);
    }
  });
});
