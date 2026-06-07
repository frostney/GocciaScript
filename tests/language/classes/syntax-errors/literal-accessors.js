/*---
description: Literal accessor names require accessor method syntax
features: [class, class-getters, class-setters, Function, unsafe-function-constructor]
---*/

test("literal accessor names cannot be parsed as fields", () => {
  const cases = [
    'class C { get "dash-key" = 1; }',
    "class C { get 1 = 1; }",
    'class C { set "dash-key" = 1; }',
    "class C { set 1; }",
  ];

  cases.forEach((source) => {
    expect(() => new Function(source)).toThrow(SyntaxError);
  });
});
