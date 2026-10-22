/*---
description: A hoisted function called from a let or const initializer finds the binding in its TDZ
features: [let, const, function-declarations, class-static-block]
---*/

const outcome = (read) => {
  try {
    return read();
  } catch (error) {
    return error.constructor.name;
  }
};

describe("hoisted functions called during a lexical initializer", () => {
  test("in a block", () => {
    let seen;
    {
      let value = [outcome(read)];
      seen = value[0];
      function read() { return value; }
    }
    expect(seen).toBe("ReferenceError");
  });

  test("in a function body", () => {
    function run() {
      const value = [outcome(read)];
      return value[0];
      function read() { return value; }
    }
    expect(run()).toBe("ReferenceError");
  });

  test("in a class static block", () => {
    class Holder {
      static {
        const value = [outcome(read)];
        Holder.seen = value[0];
        function read() { return value; }
      }
    }
    expect(Holder.seen).toBe("ReferenceError");
  });
});
