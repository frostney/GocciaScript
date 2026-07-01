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

for (const method of ["all", "allSettled", "any", "race"]) {
  test("Promise." + method + " skips IteratorClose when done getter throws", () => {
    let returnCalled = 0;
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            return {
              get done() {
                throw new Error("done-boom");
              },
              value: 1
            };
          },
          return() {
            returnCalled++;
            return { done: true };
          }
        };
      }
    };

    return Promise[method](iterable).then(() => {
      throw new Error("Expected rejection");
    }, (e) => {
      expect(e.message).toBe("done-boom");
      expect(returnCalled).toBe(0);
    });
  });

  test("Promise." + method + " skips IteratorClose when value getter throws", () => {
    let returnCalled = 0;
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            return {
              done: false,
              get value() {
                throw new Error("value-boom");
              }
            };
          },
          return() {
            returnCalled++;
            return { done: true };
          }
        };
      }
    };

    return Promise[method](iterable).then(() => {
      throw new Error("Expected rejection");
    }, (e) => {
      expect(e.message).toBe("value-boom");
      expect(returnCalled).toBe(0);
    });
  });
}

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

test("Promise capability executor throws when called twice by constructor", () => {
  class CallsExecutorTwice {
    constructor(executor) {
      const resolve = () => {};
      const reject = () => {};
      executor(resolve, reject);
      executor(resolve, reject);
    }
  }

  try {
    Promise.resolve.call(CallsExecutorTwice, 1);
    throw new Error("Expected TypeError");
  } catch (e) {
    expect(e.name).toBe("TypeError");
  }
});

test("Promise capability executor can be called again after undefined resolving functions", () => {
  class CallsExecutorWithUndefinedFirst {
    constructor(executor) {
      executor(undefined, undefined);
      executor(() => {}, () => {});
    }
  }

  expect(Promise.resolve.call(CallsExecutorWithUndefinedFirst, 1)).toBeInstanceOf(
    CallsExecutorWithUndefinedFirst
  );
});

test("Promise capability executor throws when only one resolving function was captured", () => {
  class CallsExecutorWithRejectFirst {
    constructor(executor) {
      executor(undefined, () => {});
      executor(() => {}, () => {});
    }
  }

  try {
    Promise.resolve.call(CallsExecutorWithRejectFirst, 1);
    throw new Error("Expected TypeError");
  } catch (e) {
    expect(e.name).toBe("TypeError");
  }
});
