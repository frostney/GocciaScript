/*---
description: Auto-accessor decorators receive {get, set} and can return {get?, set?, init?}
features: [decorators, auto-accessor]
---*/

describe("auto-accessor decorators", () => {
  test("accessor decorator receives get/set object and context", () => {
    let receivedValue;
    let receivedContext;

    const log = (value, context) => {
      receivedValue = value;
      receivedContext = context;
    };

    class C {
      @log
      accessor x = 42;
    }

    expect(receivedContext.kind).toBe("accessor");
    expect(receivedContext.name).toBe("x");
    expect(typeof receivedValue).toBe("object");
  });
});
