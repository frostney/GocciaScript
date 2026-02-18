/*---
description: Promise combinator methods reject with TypeError for non-iterable arguments
features: [Promise]
---*/

// Promise.all

test("Promise.all with number rejects with TypeError", () => {
  return Promise.all(42).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with string rejects with TypeError", () => {
  return Promise.all("hello").catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with null rejects with TypeError", () => {
  return Promise.all(null).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with undefined rejects with TypeError", () => {
  return Promise.all(undefined).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with boolean rejects with TypeError", () => {
  return Promise.all(true).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with object rejects with TypeError", () => {
  return Promise.all({ length: 2 }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with no arguments rejects with TypeError", () => {
  return Promise.all().catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

// Promise.allSettled

test("Promise.allSettled with number rejects with TypeError", () => {
  return Promise.allSettled(42).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.allSettled with null rejects with TypeError", () => {
  return Promise.allSettled(null).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.allSettled with no arguments rejects with TypeError", () => {
  return Promise.allSettled().catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

// Promise.race

test("Promise.race with number rejects with TypeError", () => {
  return Promise.race(42).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.race with null rejects with TypeError", () => {
  return Promise.race(null).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.race with no arguments rejects with TypeError", () => {
  return Promise.race().catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

// Promise.any

test("Promise.any with number rejects with TypeError", () => {
  return Promise.any(42).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.any with null rejects with TypeError", () => {
  return Promise.any(null).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.any with no arguments rejects with TypeError", () => {
  return Promise.any().catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});
