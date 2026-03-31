/*---
description: >
  Default exports are parsed as no-ops with a warning. Code after the skipped
  export must still execute correctly.
features: [parser-warnings, unsupported-features]
---*/

export default 42;

const x = 1;

describe.runIf(typeof GocciaScript !== "undefined")("default export is skipped", () => {
  test("code after skipped default export executes", () => {
    expect(x).toBe(1);
  });
});
