/*---
description: continue in while loops closes captured block locals in bytecode
features: [compat-while-loops]
---*/

test("continue in while closes captured block locals", () => {
  const fns = [];
  let i = 0;

  while (i < 3) {
    let x = i;
    i++;
    fns.push(() => x);
    continue;
  }

  expect(fns.map((fn) => fn())).toEqual([0, 1, 2]);
});

test("continue through switch preserves loop continue cleanup", () => {
  const fns = [];
  let i = 0;

  while (i < 3) {
    switch (i) {
      case 0:
      case 1:
      case 2: {
        let x = i;
        i++;
        fns.push(() => x);
        continue;
      }
    }
  }

  expect(fns.map((fn) => fn())).toEqual([0, 1, 2]);
});

test("continue in do...while closes captured block locals", () => {
  const fns = [];
  let i = 0;

  do {
    let x = i;
    i++;
    fns.push(() => x);
    continue;
  } while (i < 3);

  expect(fns.map((fn) => fn())).toEqual([0, 1, 2]);
});
