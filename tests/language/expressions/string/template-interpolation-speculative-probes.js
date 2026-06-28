/*---
description: Template literals with substitutions survive speculative parenthesized-group probes
features: [template-literals, template-interpolation, pattern-matching]
---*/

describe("template interpolation inside parenthesized expressions", () => {
  test("parenthesized template with division in substitution", () => {
    const x = (`d=${6 / 2}`);
    expect(x).toBe("d=3");
  });

  test("parenthesized template with regex literal in substitution", () => {
    const x = (`m=${/a.c/.test("abc")}`);
    expect(x).toBe("m=true");
  });

  test("parenthesized template is not mistaken for an arrow head", () => {
    const x = (`v${1 + 1}`) + "!";
    expect(x).toBe("v2!");
  });
});

describe("template interpolation as a match discriminant", () => {
  test("match discriminant with division in substitution", () => {
    const r = match (`n=${4 / 2}`) {
      "n=2": "two";
      default: "other";
    };
    expect(r).toBe("two");
  });

  test("match discriminant with a call and brackets in substitution", () => {
    const r = match (`${[10, 20][1] / 2}`) {
      "10": "ten";
      default: "no";
    };
    expect(r).toBe("ten");
  });

  test("match discriminant with regex literal in substitution", () => {
    const r = match (`${/x/.test("x")}`) {
      "true": "hit";
      default: "miss";
    };
    expect(r).toBe("hit");
  });

  test("match discriminant with a no-substitution template", () => {
    const r = match (`plain`) {
      "plain": "ok";
      default: "no";
    };
    expect(r).toBe("ok");
  });
});
