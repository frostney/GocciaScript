/*---
description: Promise.reject creates rejected promises correctly
features: [Promise.reject]
---*/

test("Promise.reject with string reason", () => {
  return Promise.reject("error").catch((e) => {
    expect(e).toBe("error");
  });
});

test("Promise.reject with number reason", () => {
  return Promise.reject(42).catch((e) => {
    expect(e).toBe(42);
  });
});

test("Promise.reject with object reason", () => {
  const reason = { code: 404 };
  return Promise.reject(reason).catch((e) => {
    expect(e).toBe(reason);
  });
});

test("Promise.reject with undefined", () => {
  return Promise.reject(undefined).catch((e) => {
    expect(e).toBeUndefined();
  });
});

test("Promise.reject with a Promise does not unwrap it", () => {
  const inner = Promise.resolve(42);
  return Promise.reject(inner).catch((e) => {
    expect(e).toBe(inner);
  });
});

test("Promise.reject with no argument rejects with undefined", () => {
  return Promise.reject().catch((e) => {
    expect(e).toBeUndefined();
  });
});
