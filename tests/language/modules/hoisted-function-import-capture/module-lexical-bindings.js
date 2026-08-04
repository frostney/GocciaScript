/*---
description: Imported module functions capture initialized module lexical bindings
features: [modules, compat-function]
---*/

import defaultExpressionReadsConst, {
  arrowReadsConst,
  expressionReadsLet,
  readConst,
  readLet,
  readReassignedLet,
  readViaExportList,
  setLet,
} from "./helpers/module-lexical-bindings.js";
import mutableDefaultFunction from "./helpers/module-default-declaration.js";
import {
  linkedFunctionMatches,
  observedCycleRead,
  readCycleValue,
} from "./helpers/module-lexical-cycle-a.js";

describe("imported module functions capture module lexical bindings", () => {
  test("directly exported functions read const and let bindings", () => {
    expect(readConst()).toBe("const-ready");
    expect(readLet()).toBe("let-ready");
    expect(readReassignedLet()).toBe("let-reassigned");
    setLet("let-updated");
    expect(readLet()).toBe("let-updated");
    setLet("let-ready");
  });

  test("functions exported through an export list read lexical bindings", () => {
    expect(readViaExportList()).toBe("const-ready:let-ready");
  });

  test("arrow and function expression exports remain correct", () => {
    expect(arrowReadsConst()).toBe("const-ready");
    expect(expressionReadsLet()).toBe("let-ready");
    expect(defaultExpressionReadsConst()).toBe("const-ready");
    expect(defaultExpressionReadsConst.name).toBe("default");
  });

  test("named default declarations retain their mutable module binding", () => {
    expect(mutableDefaultFunction()).toBe("initial");
    expect(mutableDefaultFunction).toBe("changed");
  });

  test("cyclic reads preserve TDZ and initialize for later calls", () => {
    expect(observedCycleRead).toBe("ReferenceError");
    expect(readCycleValue()).toBe("cycle-ready");
    expect(linkedFunctionMatches()).toBe(true);
  });
});
