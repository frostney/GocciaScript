/*---
description: A var declared after a switch clause's const does not let a later clause read that const before its declaration ran
features: [compat-var]
---*/

describe("switch clause const followed by a var", () => {
  test("a later clause observes the TDZ inside a function", () => {
    const read = (which) => {
      switch (which) {
        case 0:
          const scale = 5;
          var late = 1;
          return scale + late;
        case 1:
          return scale * 2;
      }
    };

    expect(read(0)).toBe(6);
    expect(() => read(1)).toThrow(ReferenceError);
  });

  test("a later clause observes the TDZ inside a class static block", () => {
    const read = (which) => {
      let outcome;
      class Reader {
        static {
          switch (which) {
            case 0:
              const scale = 5;
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

    expect(read(0)).toBe(6);
    expect(() => read(1)).toThrow(ReferenceError);
  });

  test("a computed initializer in a class static block keeps the TDZ too", () => {
    const read = (which) => {
      let outcome;
      class Reader {
        static {
          switch (which) {
            case 0:
              const scale = which + 5;
              var late = 1;
              outcome = scale + late;
              break;
            default:
              outcome = scale * 2;
          }
        }
      }
      return outcome;
    };

    expect(read(0)).toBe(6);
    expect(() => read(1)).toThrow(ReferenceError);
  });

  test("falling through from the declaring clause still reads the const", () => {
    const read = (which) => {
      let outcome = 0;
      class Reader {
        static {
          switch (which) {
            case 0:
              const scale = 5;
              var late = 1;
              outcome += late;
            case 1:
              outcome += scale;
          }
        }
      }
      return outcome;
    };

    expect(read(0)).toBe(6);
    expect(() => read(1)).toThrow(ReferenceError);
  });
});
