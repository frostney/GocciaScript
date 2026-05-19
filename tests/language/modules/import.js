import { localFunction, localValue } from "./local-module.js";
import defer * as deferredMath from "./helpers/missing-defer-fixture.js";
import defer * as attributedDeferredMath from "./helpers/missing-defer-fixture.js" with { };

describe("basic import", () => {
  test("import function from local module", () => {
    expect(localFunction()).toBe(42);
  });

  test("import value from local module", () => {
    expect(localValue).toBe("hello");
  });

  test("unsupported import defer namespace syntax is accepted as a no-op", () => {
    expect(typeof deferredMath).toBe("undefined");
    expect(typeof attributedDeferredMath).toBe("undefined");
  });
});

import { add, multiply } from "./helpers/math-utils.js";

describe("import from subdirectory", () => {
  test("import add from math-utils", () => {
    expect(add(2, 3)).toBe(5);
  });

  test("import multiply from math-utils", () => {
    expect(multiply(4, 5)).toBe(20);
  });
});

import { greet as sayHello } from "./helpers/string-utils.js";

describe("aliased import", () => {
  test("import with alias", () => {
    expect(sayHello("World")).toBe("Hello, World!");
  });
});
