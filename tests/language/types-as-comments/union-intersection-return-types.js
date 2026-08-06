/*---
description: Union and intersection return types whose members are object types are parsed and ignored at runtime
features: [types-as-comments]
---*/

describe("union and intersection return types", () => {

test("class method union return type with an object member", () => {
  class Registry {
    lookup(flag: boolean): string | { code: number } {
      return flag ? "ok" : { code: 7 };
    }
  }

  const registry = new Registry();

  expect(registry.lookup(true)).toBe("ok");
  expect(registry.lookup(false).code).toBe(7);
});

test("class method intersection return type", () => {
  class Builder {
    build(): { name: string } & { size: number } {
      return { name: "box", size: 2 };
    }
  }

  const built = new Builder().build();

  expect(built.name).toBe("box");
  expect(built.size).toBe(2);
});

test("object type leading a union return type", () => {
  class Reader {
    read(): { value: number } | null {
      return { value: 3 };
    }
  }

  expect(new Reader().read().value).toBe(3);
});

test("object method with a structured union return type", () => {
  const factory = {
    make(): { kind: string } | string {
      return { kind: "leaf" };
    },
  };

  expect(factory.make().kind).toBe("leaf");
});

test("getter with a structured union return type", () => {
  class Cell {
    get current(): number | { boxed: number } {
      return 9;
    }
  }

  expect(new Cell().current).toBe(9);
});

test("function-type return type followed by the body brace", () => {
  class Maker {
    make(): () => { tag: string } {
      return () => ({ tag: "leaf" });
    }
  }

  expect(new Maker().make()().tag).toBe("leaf");
});

test("conditional return type with object branches", () => {
  class Picker {
    pick(): string extends string ? { hit: boolean } : { miss: boolean } {
      return { hit: true };
    }
  }

  expect(new Picker().pick().hit).toBe(true);
});

test("nested unions and intersections", () => {
  class Deep {
    shape(): { a: { b: string } } & ({ c: number } | { d: number }) {
      return { a: { b: "x" }, c: 1 };
    }
  }

  expect(new Deep().shape().a.b).toBe("x");
});

test("union members separated across lines", () => {
  class Wrapped {
    read():
      | string
      | { code: number } {
      return "ok";
    }
  }

  expect(new Wrapped().read()).toBe("ok");
});

test("arrow function union return type is unchanged", () => {
  const pick = (flag: boolean): string | { code: number } =>
    flag ? "ok" : { code: 1 };

  expect(pick(true)).toBe("ok");
  expect(pick(false).code).toBe(1);
});

});
