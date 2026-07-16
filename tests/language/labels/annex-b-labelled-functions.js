/*---
description: Annex B labelled function declarations
features: [compat-label, compat-function, compat-non-strict-mode]
---*/

describe("Annex B labelled function declarations", () => {
  test("sloppy direct labelled function declarations remain valid", () => {
    expect(new Function("l: function f() { return 1; } return f();")()).toBe(1);
    expect(new Function("l0: l1: function f() { return 2; } return f();")()).toBe(2);
  });

  test("block-wrapped labelled function declarations remain valid statement bodies", () => {
    expect(typeof new Function("if (true) { l: function f() {} }")).toBe("function");
    expect(typeof new Function("while (false) { l: function f() {} }")).toBe("function");
    expect(typeof new Function("with ({}) { l: function f() {} }")).toBe("function");
  });
});

var __gocciaAnnexBLabelledBlockFunction = () => "outer";
{
  __gocciaAnnexBLabelledBlockFunction();
  l: function __gocciaAnnexBLabelledBlockFunction() {
    return "inner";
  }
}
const __gocciaAnnexBLabelledBlockResult = __gocciaAnnexBLabelledBlockFunction();

switch (1) {
  case 1:
    __gocciaAnnexBLabelledSwitchFunction();
  case 2:
    l: function __gocciaAnnexBLabelledSwitchFunction() {
      return "switch";
    }
}
const __gocciaAnnexBLabelledSwitchResult = __gocciaAnnexBLabelledSwitchFunction();

describe("Annex B labelled block function bindings", () => {
  test("sloppy block labelled function declarations update var bindings", () => {
    expect(__gocciaAnnexBLabelledBlockResult).toBe("inner");
  });

  test("sloppy switch labelled function declarations update var bindings", () => {
    expect(__gocciaAnnexBLabelledSwitchResult).toBe("switch");
  });
});
