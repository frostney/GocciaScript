/*---
description: >
  Default exports expose the module's "default" binding for expressions,
  anonymous functions, and anonymous classes.
features: [modules, default-exports]
---*/

import defaultValue from "./helpers/default-value.js";
import defaultAdd from "./helpers/default-arrow.js";
import DefaultClass from "./helpers/default-class.js";
import returnedFunction from "./helpers/default-call-result.js";
import NamedDefaultClass, {
  localNamedDefaultClassName,
  localNamedDefaultClassType
} from "./helpers/default-named-class.js";
import DefaultClassBeforeStatement, {
  defaultClassFollowingStatementHit
} from "./helpers/default-class-following-statement.js";
import NamedDefaultClassBeforeStatement, {
  defaultNamedClassFollowingStatementHit,
  localFollowingNamedDefaultClassName
} from "./helpers/default-named-class-following-statement.js";
import defaultAwaitRegexp from "./helpers/default-await-regexp.js";
import defaultAwaitObjectWithFunctionMethod from "./helpers/default-await-object-function-method.js";

describe("default export", () => {
  test("exports an expression as default", () => {
    expect(defaultValue).toBe(42);
  });

  test("exports an anonymous arrow function as default", () => {
    expect(defaultAdd(2, 3)).toBe(5);
    expect(defaultAdd.name).toBe("default");
  });

  test("exports an anonymous class as default", () => {
    expect(DefaultClass.name).toBe("default");
    expect(DefaultClass.label()).toBe("anonymous default class");
  });

  test("does not infer a default name for call result exports", () => {
    expect(returnedFunction()).toBe("call result");
    expect(returnedFunction.name === "default").toBe(false);
  });

  test("binds named default class declarations locally", () => {
    expect(NamedDefaultClass.name).toBe("NamedDefaultClass");
    expect(localNamedDefaultClassType).toBe("function");
    expect(localNamedDefaultClassName).toBe("NamedDefaultClass");
  });

  test("allows default class declarations before a following statement", () => {
    expect(DefaultClassBeforeStatement.name).toBe("default");
    expect(defaultClassFollowingStatementHit).toBe(1);
  });

  test("allows named default class declarations before a following statement", () => {
    expect(NamedDefaultClassBeforeStatement.name).toBe("FollowingNamedDefaultClass");
    expect(localFollowingNamedDefaultClassName).toBe("FollowingNamedDefaultClass");
    expect(defaultNamedClassFollowingStatementHit).toBe(1);
  });

  test("exports top-level await expressions with regex and object method operands", () => {
    expect(defaultAwaitRegexp.test("1")).toBe(true);
    expect(defaultAwaitObjectWithFunctionMethod.function()).toBe("method");
  });
});
