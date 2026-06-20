/*---
description: Promise.all waits for all promises to resolve
features: [Promise.all]
---*/

test("Promise.all with empty array", () => {
  return Promise.all([]).then((v) => {
    expect(v).toEqual([]);
  });
});

test("Promise.all with all resolved", () => {
  return Promise.all([
    Promise.resolve(1),
    Promise.resolve(2),
    Promise.resolve(3)
  ]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all preserves order", () => {
  return Promise.all([
    Promise.resolve("a"),
    Promise.resolve("b"),
    Promise.resolve("c")
  ]).then((v) => {
    expect(v).toEqual(["a", "b", "c"]);
  });
});

test("Promise.all with non-promise values", () => {
  return Promise.all([1, 2, 3]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all rejects on first rejection", () => {
  return Promise.all([
    Promise.resolve(1),
    Promise.reject("err"),
    Promise.resolve(3)
  ]).catch((e) => {
    expect(e).toBe("err");
  });
});

test("Promise.all with mixed promises and values", () => {
  return Promise.all([
    1,
    Promise.resolve(2),
    3
  ]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all with single element", () => {
  return Promise.all([Promise.resolve(42)]).then((v) => {
    expect(v).toEqual([42]);
  });
});

test("Promise.all with multiple rejections only catches first", () => {
  return Promise.all([
    Promise.reject("first"),
    Promise.reject("second"),
    Promise.reject("third")
  ]).catch((e) => {
    expect(e).toBe("first");
  });
});

test("Promise.all with string iterates characters", () => {
  return Promise.all("hello").then((result) => {
    expect(result).toEqual(["h", "e", "l", "l", "o"]);
  });
});

test("Promise.all with Set iterates values", () => {
  return Promise.all(new Set([1, 2, 3])).then((result) => {
    expect(result).toEqual([1, 2, 3]);
  });
});

test("Promise.all with Set of promises", () => {
  const s = new Set([Promise.resolve("a"), Promise.resolve("b")]);
  return Promise.all(s).then((result) => {
    expect(result).toEqual(["a", "b"]);
  });
});

test("Promise.all with Map iterates entries", () => {
  const m = new Map([["x", 1], ["y", 2]]);
  return Promise.all(m).then((result) => {
    expect(result).toEqual([["x", 1], ["y", 2]]);
  });
});

test("Promise.all accepts deferred thenable rejection", () => {
  const thenable = {
    then(resolve, reject) {
      Promise.resolve().then(() => reject("deferred"));
    }
  };
  return Promise.all([thenable]).then(() => {
    throw new Error("expected rejection");
  }, (reason) => {
    expect(reason).toBe("deferred");
  });
});

test("Promise.all ignores late thenable rejection after resolve", () => {
  const thenable = {
    then(resolve, reject) {
      Promise.resolve().then(() => {
        resolve(9);
        reject("late");
      });
    }
  };
  return Promise.all([thenable]).then((result) => {
    expect(result).toEqual([9]);
  });
});

test("Promise.all resolve element functions are anonymous built-ins", () => {
  let resolveElement;

  class NotPromise {
    constructor(executor) {
      executor(() => {}, () => {});
    }

    static resolve(value) {
      return value;
    }
  }

  Promise.all.call(NotPromise, [{
    then(resolve) {
      resolveElement = resolve;
    }
  }]);

  const nameDescriptor = Object.getOwnPropertyDescriptor(resolveElement, "name");
  const propertyNames = Object.getOwnPropertyNames(resolveElement);
  expect(nameDescriptor.value).toBe("");
  expect(nameDescriptor.writable).toBe(false);
  expect(nameDescriptor.enumerable).toBe(false);
  expect(nameDescriptor.configurable).toBe(true);
  expect(propertyNames.indexOf("name")).toBe(propertyNames.indexOf("length") + 1);
});

test("Promise.all resolve element functions ignore repeated calls", () => {
  let callCount = 0;

  class NotPromise {
    constructor(executor) {
      executor((values) => {
        callCount += 1;
        expect(values).toEqual(["first", "second"]);
      }, () => {
        throw new Error("unexpected rejection");
      });
    }

    static resolve(value) {
      return value;
    }
  }

  Promise.all.call(NotPromise, [
    {
      then(resolve) {
        resolve("first");
        resolve("bad");
      }
    },
    {
      then(resolve) {
        resolve("second");
        resolve("worse");
      }
    }
  ]);

  expect(callCount).toBe(1);
});
