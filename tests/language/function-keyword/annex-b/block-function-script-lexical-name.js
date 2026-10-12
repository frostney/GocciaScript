/*---
description: At script top level, a block-level function named like a top-level let stays block-scoped
features: [compat-function, compat-non-strict-mode, compat-var]
---*/

let blockShadowed = 1;
{
  function blockShadowed() {}
}

let caseShadowed = 2;
switch (1) {
  case 1:
    function caseShadowed() {}
}

{
  function scriptBlockFunction() {
    return "var binding";
  }
}

test("a block function leaves a top-level let of the same name alone", () => {
  expect(blockShadowed).toBe(1);
  expect(caseShadowed).toBe(2);
});

test("a block function with another name still updates its var binding", () => {
  expect(scriptBlockFunction()).toBe("var binding");
});
