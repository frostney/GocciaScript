/*---
description: A call of an identifier inside a with statement, used as a branch condition, decides on the value the call returned
features: [with, compat-non-strict-mode]
---*/

describe("call conditions inside a with statement", () => {
  test("a function supplied by the with object decides on its result", () => {
    const run = (scope) => {
      const seen = [];
      with (scope) {
        if (pick()) { seen.push("T"); } else { seen.push("F"); }
        seen.push(pick() ? "T" : "F");
        seen.push(!pick() ? "T" : "F");
      }
      return seen.join("");
    };
    expect(run({ pick: () => 0 })).toBe("FFT");
    expect(run({ pick: () => "x" })).toBe("TTF");
  });

  test("a function the with object does not supply is called from the enclosing scope", () => {
    const pick = () => null;
    const seen = [];
    with ({ other: 1 }) {
      if (pick()) { seen.push("T"); } else { seen.push("F"); }
    }
    expect(seen.join("")).toBe("F");
  });
});
