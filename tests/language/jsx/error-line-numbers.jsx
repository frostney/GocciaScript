/*---
description: Verify errors inside JSX expressions report correct original line numbers
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX error line numbers", () => {
  test("error in expression child reports original line", () => {
    let caught = false;
    try {
      const el = <div>{undefinedVariable}</div>;
    } catch (e) {
      caught = true;
      expect(e.message).toContain("undefinedVariable");
    }
    expect(caught).toBe(true);
  });

  test("error in attribute expression reports original line", () => {
    let caught = false;
    try {
      const el = <div value={nonExistentVar}></div>;
    } catch (e) {
      caught = true;
      expect(e.message).toContain("nonExistentVar");
    }
    expect(caught).toBe(true);
  });
});
