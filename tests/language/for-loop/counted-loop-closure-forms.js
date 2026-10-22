/*---
description: A counted for loop keeps a binding per iteration for every closure form its body creates, and shares its counter when the body creates none
features: [compat-traditional-for-loop, let, closures, class, accessor-properties]
---*/

describe("counted for loop bindings", () => {
  test("a class method created in the body sees its own iteration's binding", () => {
    const readers = [];
    for (let i = 0; i < 3; i++) {
      class Reader { read() { return i; } }
      readers.push(new Reader());
    }
    expect(readers.map((reader) => reader.read())).toEqual([0, 1, 2]);
  });

  test("a getter created in the body sees its own iteration's binding", () => {
    const holders = [];
    for (let i = 0; i < 3; i++) {
      holders.push({ get value() { return i; } });
    }
    expect(holders.map((holder) => holder.value)).toEqual([0, 1, 2]);
  });

  test("a closure created on some iterations only keeps its iteration's value", () => {
    const readers = [];
    for (let i = 0; i < 6; i++) {
      if (i % 2 === 0) {
        readers.push(() => i);
      }
    }
    expect(readers.map((read) => read())).toEqual([0, 2, 4]);
  });

  test("a body without closures reads the counter at every step", () => {
    const seen = [];
    for (let i = 0; i < 5; i++) {
      if (i === 2) {
        continue;
      }
      seen.push(i * 10);
    }
    expect(seen).toEqual([0, 10, 30, 40]);
  });
});
