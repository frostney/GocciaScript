/*---
description: >
  A function stored on a class keeps resolving private names against the class
  it was created in; the assignment does not make the target class its home.
features: [compat-function, compat-non-strict-mode]
---*/

const createReader = () => {
  class Source {
    #value = "source";
    static reader() {
      return function (receiver) {
        return receiver.#value;
      };
    }
  }
  return { Source, reader: Source.reader() };
};

test("a function from one class stored on another with a dot assignment reads the first class's field", () => {
  const { Source, reader } = createReader();
  class Target {
    #value = "target";
  }
  Target.read = reader;

  expect(Target.read(new Source())).toBe("source");
});

test("a function from one class stored on another with a computed assignment reads the first class's field", () => {
  const { Source, reader } = createReader();
  class Target {
    #value = "target";
  }
  const key = "read";
  Target[key] = reader;

  expect(Target[key](new Source())).toBe("source");
});
