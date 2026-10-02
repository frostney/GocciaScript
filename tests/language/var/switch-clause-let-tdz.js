/*---
description: A var declared after a switch clause's let does not let a later clause read or assign that let before its declaration ran
features: [compat-var]
---*/

const pass = (value) => value;

describe("switch clause let followed by a var", () => {
  test("a later clause observes the TDZ inside a function", () => {
    const read = (which) => {
      switch (which) {
        case 0:
          let scale = which + 5;
          var late = 1;
          return scale + late;
        case 1:
          return scale * 2;
      }
    };

    expect(read(pass(0))).toBe(6);
    expect(() => read(pass(1))).toThrow(ReferenceError);
  });

  test("a later clause cannot assign the binding inside a function", () => {
    const write = (which) => {
      switch (which) {
        case 0:
          let slot = which + 5;
          var late = 1;
          slot = slot + late;
          return slot;
        case 1:
          slot = 2;
          return slot;
        case 2:
          slot += 2;
          return slot;
      }
    };

    expect(write(pass(0))).toBe(6);
    expect(() => write(pass(1))).toThrow(ReferenceError);
    expect(() => write(pass(2))).toThrow(ReferenceError);
  });

  test("a later clause observes the TDZ inside a class static block", () => {
    const read = (which) => {
      let outcome;
      class Reader {
        static {
          switch (which) {
            case 0:
              let scale = which + 5;
              var late = 1;
              outcome = scale + late;
              break;
            case 1:
              outcome = scale * 2;
          }
        }
      }
      return outcome;
    };

    expect(read(pass(0))).toBe(6);
    expect(() => read(pass(1))).toThrow(ReferenceError);
  });

  test("a later clause cannot assign the binding inside a class static block", () => {
    const write = (which) => {
      let outcome;
      class Writer {
        static {
          switch (which) {
            case 0:
              let slot = which + 5;
              var late = 1;
              slot = slot + late;
              outcome = slot;
              break;
            case 1:
              slot = 2;
              outcome = slot;
              break;
            default:
              slot += 2;
              outcome = slot;
          }
        }
      }
      return outcome;
    };

    expect(write(pass(0))).toBe(6);
    expect(() => write(pass(1))).toThrow(ReferenceError);
    expect(() => write(pass(2))).toThrow(ReferenceError);
  });

  test("falling through from the declaring clause still reads the let", () => {
    const read = (which) => {
      let outcome = 0;
      class Reader {
        static {
          switch (which) {
            case 0:
              let scale = which + 5;
              var late = 1;
              outcome += late;
            case 1:
              outcome += scale;
          }
        }
      }
      return outcome;
    };

    expect(read(pass(0))).toBe(6);
    expect(() => read(pass(1))).toThrow(ReferenceError);
  });
});
