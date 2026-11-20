/*---
description: Using bindings named of keep traditional for headers distinct from for-of targets
features: [explicit-resource-management, traditional-for-loop]
---*/

describe("using bindings in traditional for headers", () => {
  test("ordinary and contextual binding names accept empty condition and update", () => {
    let visits = 0;
    for (using x = null;;) {
      expect(x).toBe(null);
      visits++;
      break;
    }
    for (using of = null;;) {
      expect(of).toBe(null);
      visits++;
      break;
    }
    expect(visits).toBe(2);
  });

  test("a contextual binding with condition and update disposes once", () => {
    const log = [];
    const resource = { [Symbol.dispose]() { log.push("disposed"); } };
    let i = 0;
    for (using of = resource; i < 2; i++) {
      expect(of).toBe(resource);
      log.push(i);
    }
    expect(log).toEqual([0, 1, "disposed"]);
  });

  test("declarations and a using assignment target remain distinct", () => {
    let visits = 0;
    for (using of = null, x = null; visits < 1; visits++) {
      expect(of).toBe(null);
      expect(x).toBe(null);
    }
    let using;
    for (using of /a>/.exec("a>")) {}
    expect(using).toBe("a>");
    expect(visits).toBe(1);
  });
});
