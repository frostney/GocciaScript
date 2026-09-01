/*---
description: Identifier self-increment assignment (id = id + 1) matches ++ for numeric locals
features: [assignment-operators]
---*/

test("self-increment assignment adds one", () => {
  let i = 0;
  i = i + 1;
  expect(i).toBe(1);
  i = i + 1;
  expect(i).toBe(2);
});

test("commutative self-increment assignment adds one", () => {
  let i = 0;
  i = 1 + i;
  expect(i).toBe(1);
  i = 1 + i;
  expect(i).toBe(2);
});

test("self-increment assignment produces the new value", () => {
  let i = 4;
  expect(i = i + 1).toBe(5);
  expect(i).toBe(5);
  expect(i = 1 + i).toBe(6);
  expect(i).toBe(6);
});

test("self-increment assignment preserves a fractional part", () => {
  let x = 1.5;
  x = x + 1;
  expect(x).toBe(2.5);
  x = 1 + x;
  expect(x).toBe(3.5);
});

test("string self-addition still concatenates", () => {
  let i = "1";
  i = i + 1;
  expect(i).toBe("11");
  i = 1 + i;
  expect(i).toBe("111");
});

test("self-increment assignment on captured local syncs upvalue cell", () => {
  const f = () => {
    let i = 0;
    const get = () => i;
    i = i + 1;
    i = 1 + i;
    return [get(), i];
  };
  expect(f()).toEqual([2, 2]);
});

test("self-increment assignment result can be captured after writes", () => {
  const f = () => {
    let i = 10;
    const get = () => i;
    const a = i = i + 1;
    const b = i = 1 + i;
    return [a, b, get()];
  };
  expect(f()).toEqual([11, 12, 12]);
});
