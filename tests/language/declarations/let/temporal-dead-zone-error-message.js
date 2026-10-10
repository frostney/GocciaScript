/*---
description: A temporal-dead-zone ReferenceError names the binding for every kind of read and write
features: [let, const-declaration, temporal-dead-zone]
---*/

const messageOf = (run) => {
  try {
    run();
  } catch (error) {
    return `${error.constructor.name}: ${error.message}`;
  }
  return "no error";
};

const tdz = (name) => `ReferenceError: Cannot access '${name}' before initialization`;

describe("temporal dead zone error message", () => {
  test("a read of a local names it", () => {
    expect(messageOf(() => {
      const read = early;
      let early = 1;
    })).toBe(tdz("early"));
    expect(messageOf(() => {
      const read = fixed;
      const fixed = 1;
    })).toBe(tdz("fixed"));
    expect(messageOf(() => {
      let self = self;
    })).toBe(tdz("self"));
  });

  test("typeof still throws and names it", () => {
    expect(messageOf(() => {
      const kind = typeof early;
      let early = 1;
    })).toBe(tdz("early"));
    expect(messageOf(() => {
      const kind = () => typeof captured;
      kind();
      let captured = 1;
    })).toBe(tdz("captured"));
  });

  test("assignment, update, compound and logical assignment name it", () => {
    expect(messageOf(() => {
      target = 2;
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      target++;
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      --target;
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      target += 1;
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      target ||= 1;
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      target &&= 1;
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      target ??= 1;
      let target = 1;
    })).toBe(tdz("target"));
  });

  test("destructuring assignment targets and defaults name it", () => {
    expect(messageOf(() => {
      [target] = [1];
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      ({ key: target } = { key: 1 });
      let target = 1;
    })).toBe(tdz("target"));
    expect(messageOf(() => {
      const { value = fallback } = {};
      let fallback = 1;
    })).toBe(tdz("fallback"));
    expect(messageOf(() => {
      const [value = fallback] = [];
      let fallback = 1;
    })).toBe(tdz("fallback"));
  });

  test("a binding captured by a closure is named on read and write", () => {
    expect(messageOf(() => {
      const read = () => captured;
      read();
      let captured = 1;
    })).toBe(tdz("captured"));
    expect(messageOf(() => {
      const write = () => {
        captured = 2;
      };
      write();
      let captured = 1;
    })).toBe(tdz("captured"));
    expect(messageOf(() => {
      const bump = () => {
        captured++;
      };
      bump();
      let captured = 1;
    })).toBe(tdz("captured"));
    expect(messageOf(() => {
      const add = () => {
        captured += 1;
      };
      add();
      let captured = 1;
    })).toBe(tdz("captured"));
    expect(messageOf(() => {
      const fill = () => {
        captured ??= 1;
      };
      fill();
      let captured = 1;
    })).toBe(tdz("captured"));
    expect(messageOf(() => {
      const spread = () => {
        [captured] = [1];
      };
      spread();
      let captured = 1;
    })).toBe(tdz("captured"));
    expect(messageOf(() => {
      const outer = () => () => deep;
      outer()();
      let deep = 1;
    })).toBe(tdz("deep"));
  });

  test("a class binding is named inside its own heritage and body", () => {
    expect(messageOf(() => {
      class Self extends Self {}
    })).toBe(tdz("Self"));
    expect(messageOf(() => {
      class Keyed {
        [Keyed] = 1;
      }
    })).toBe(tdz("Keyed"));
    expect(messageOf(() => {
      class Named {
        static [Named.name] = 1;
      }
    })).toBe(tdz("Named"));
    expect(messageOf(() => {
      const made = new Later();
      class Later {}
    })).toBe(tdz("Later"));
  });

  test("for-of and for-in heads name their own binding", () => {
    expect(messageOf(() => {
      for (const item of [item]) {
      }
    })).toBe(tdz("item"));
    expect(messageOf(() => {
      for (let item of item) {
      }
    })).toBe(tdz("item"));
  });

  test("a default parameter that reads a later parameter names it", () => {
    expect(messageOf(() => ((first = second, second = 1) => first)())).toBe(tdz("second"));
    expect(messageOf(() => ((only = only) => only)())).toBe(tdz("only"));
  });

  test("a block reusing an earlier block's slot names its own binding", () => {
    expect(messageOf(() => {
      {
        let earlier = 1;
        const copy = earlier;
      }
      {
        const read = later;
        let later = 2;
      }
    })).toBe(tdz("later"));
    expect(messageOf(() => {
      let shadowed = 1;
      {
        const read = shadowed;
        let shadowed = 2;
      }
    })).toBe(tdz("shadowed"));
  });

  test("blocks of loops, catch clauses, switch cases and generators name it", () => {
    expect(messageOf(() => {
      for (const step of [1, 2]) {
        const read = inside;
        let inside = step;
      }
    })).toBe(tdz("inside"));
    expect(messageOf(() => {
      try {
        throw new Error("boom");
      } catch (error) {
        const read = handled;
        let handled = 1;
      }
    })).toBe(tdz("handled"));
    expect(messageOf(() => {
      switch (1) {
        case 0:
          let caseBinding = 1;
          break;
        case 1:
          caseBinding;
      }
    })).toBe(tdz("caseBinding"));
    expect(messageOf(() => {
      const source = {
        *values() {
          yield pending;
          let pending = 1;
        },
      };
      source.values().next();
    })).toBe(tdz("pending"));
  });
});
