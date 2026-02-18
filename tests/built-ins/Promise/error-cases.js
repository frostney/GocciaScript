/*---
description: Promise combinator methods reject with TypeError for non-iterable arguments
features: [Promise]
---*/

// Promise.all

test("Promise.all with number rejects with TypeError", () => {
  return Promise.all(42).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with null rejects with TypeError", () => {
  return Promise.all(null).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with undefined rejects with TypeError", () => {
  return Promise.all(undefined).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with boolean rejects with TypeError", () => {
  return Promise.all(true).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with object rejects with TypeError", () => {
  return Promise.all({ length: 2 }).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.all with no arguments rejects with TypeError", () => {
  return Promise.all().then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

// Promise.allSettled

test("Promise.allSettled with number rejects with TypeError", () => {
  return Promise.allSettled(42).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.allSettled with null rejects with TypeError", () => {
  return Promise.allSettled(null).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.allSettled with no arguments rejects with TypeError", () => {
  return Promise.allSettled().then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

// Promise.race

test("Promise.race with number rejects with TypeError", () => {
  return Promise.race(42).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.race with null rejects with TypeError", () => {
  return Promise.race(null).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.race with no arguments rejects with TypeError", () => {
  return Promise.race().then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

// Promise.any

test("Promise.any with number rejects with TypeError", () => {
  return Promise.any(42).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.any with null rejects with TypeError", () => {
  return Promise.any(null).then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});

test("Promise.any with no arguments rejects with TypeError", () => {
  return Promise.any().then(() => {
    throw new Error("Expected rejection");
  }).catch((e) => {
    expect(e.name).toBe("TypeError");
  });
});
