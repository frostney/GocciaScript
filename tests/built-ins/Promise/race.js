/*---
description: Promise.race resolves or rejects with the first settled promise
features: [Promise.race]
---*/

test("Promise.race with first fulfilled", () => {
  return Promise.race([
    Promise.resolve("first"),
    Promise.resolve("second")
  ]).then((v) => {
    expect(v).toBe("first");
  });
});

test("Promise.race with first rejected", () => {
  return Promise.race([
    Promise.reject("err"),
    Promise.resolve("ok")
  ]).catch((e) => {
    expect(e).toBe("err");
  });
});

test("Promise.race with non-promise values", () => {
  return Promise.race([1, 2, 3]).then((v) => {
    expect(v).toBe(1);
  });
});

test("Promise.race with single element", () => {
  return Promise.race([Promise.resolve(42)]).then((v) => {
    expect(v).toBe(42);
  });
});

test("Promise.race with mixed resolved and rejected", () => {
  return Promise.race([
    Promise.resolve("winner"),
    Promise.reject("loser")
  ]).then((v) => {
    expect(v).toBe("winner");
  });
});

test("Promise.race with empty array returns forever-pending promise", () => {
  const p = Promise.race([]);
  let settled = false;
  p.then(() => { settled = true; });
  p.catch(() => { settled = true; });
  return Promise.resolve().then(() => {
    expect(settled).toBe(false);
  });
});

test("Promise.race with single rejected element", () => {
  return Promise.race([Promise.reject("only")]).catch((e) => {
    expect(e).toBe("only");
  });
});
