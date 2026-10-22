/*---
description: A closure created before a let or const declaration and called during its initializer finds the binding in its TDZ
features: [let, const, arrow-function, class-static-block]
---*/

const outcome = (read) => {
  try {
    return read();
  } catch (error) {
    return error.constructor.name;
  }
};

describe("closures called during a lexical initializer", () => {
  test("in a block, an earlier closure called from the initializer throws", () => {
    let seen;
    {
      const read = () => value;
      let value = [outcome(read)];
      seen = value[0];
    }
    expect(seen).toBe("ReferenceError");
  });

  test("at the start of a function body, an earlier initializer's closure throws", () => {
    const run = () => {
      const read = () => later;
      const later = [outcome(read)];
      return later[0];
    };
    expect(run()).toBe("ReferenceError");
  });

  test("in a class static block", () => {
    class Holder {
      static {
        const read = () => value;
        const value = [outcome(read)];
        Holder.seen = value[0];
      }
    }
    expect(Holder.seen).toBe("ReferenceError");
  });

  test("in a switch clause", () => {
    let seen;
    switch (1) {
      case 1:
        const read = () => value;
        let value = [outcome(read)];
        seen = value[0];
    }
    expect(seen).toBe("ReferenceError");
  });

  test("on every loop iteration, a read before the declaration throws", () => {
    const seen = [];
    for (const step of [1, 2, 3]) {
      const read = () => value;
      seen.push(outcome(read));
      let value = step;
      seen.push(outcome(read));
    }
    expect(seen).toEqual([
      "ReferenceError", 1, "ReferenceError", 2, "ReferenceError", 3,
    ]);
  });
});
