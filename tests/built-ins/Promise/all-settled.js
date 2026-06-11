/*---
description: Promise.allSettled waits for all promises to settle
features: [Promise.allSettled]
---*/

test("Promise.allSettled with empty array", () => {
  return Promise.allSettled([]).then((v) => {
    expect(v).toEqual([]);
  });
});

test("Promise.allSettled with all fulfilled", () => {
  return Promise.allSettled([
    Promise.resolve(1),
    Promise.resolve(2)
  ]).then((results) => {
    expect(results[0].status).toBe("fulfilled");
    expect(results[0].value).toBe(1);
    expect(results[0] instanceof Object).toBe(true);
    expect(results[1].status).toBe("fulfilled");
    expect(results[1].value).toBe(2);
    expect(results[1] instanceof Object).toBe(true);
  });
});

test("Promise.allSettled with all rejected", () => {
  return Promise.allSettled([
    Promise.reject("err1"),
    Promise.reject("err2")
  ]).then((results) => {
    expect(results[0].status).toBe("rejected");
    expect(results[0].reason).toBe("err1");
    expect(results[1].status).toBe("rejected");
    expect(results[1].reason).toBe("err2");
  });
});

test("Promise.allSettled with mixed outcomes", () => {
  return Promise.allSettled([
    Promise.resolve("ok"),
    Promise.reject("err"),
    Promise.resolve(42)
  ]).then((results) => {
    expect(results[0].status).toBe("fulfilled");
    expect(results[0].value).toBe("ok");
    expect(results[1].status).toBe("rejected");
    expect(results[1].reason).toBe("err");
    expect(results[2].status).toBe("fulfilled");
    expect(results[2].value).toBe(42);
  });
});

test("Promise.allSettled never rejects", () => {
  return Promise.allSettled([
    Promise.reject("a"),
    Promise.reject("b")
  ]).then((results) => {
    expect(results.length).toBe(2);
    expect(results[0].status).toBe("rejected");
    expect(results[1].status).toBe("rejected");
  });
});

test("Promise.allSettled with single fulfilled element", () => {
  return Promise.allSettled([Promise.resolve(1)]).then((results) => {
    expect(results.length).toBe(1);
    expect(results[0].status).toBe("fulfilled");
    expect(results[0].value).toBe(1);
  });
});

test("Promise.allSettled with single rejected element", () => {
  return Promise.allSettled([Promise.reject("err")]).then((results) => {
    expect(results.length).toBe(1);
    expect(results[0].status).toBe("rejected");
    expect(results[0].reason).toBe("err");
  });
});

test("Promise.allSettled creates data properties for result records", () => {
  const calls = [];
  const statusDescriptor = Object.create(null);
  statusDescriptor.set = () => { calls.push("status"); };
  statusDescriptor.configurable = true;
  const valueDescriptor = Object.create(null);
  valueDescriptor.set = () => { calls.push("value"); };
  valueDescriptor.configurable = true;
  const reasonDescriptor = Object.create(null);
  reasonDescriptor.set = () => { calls.push("reason"); };
  reasonDescriptor.configurable = true;
  Object.defineProperty(Object.prototype, "status", statusDescriptor);
  Object.defineProperty(Object.prototype, "value", valueDescriptor);
  Object.defineProperty(Object.prototype, "reason", reasonDescriptor);

  return Promise.allSettled([
    Promise.resolve("ok"),
    Promise.reject("bad")
  ]).then((results) => {
    expect(calls).toEqual([]);
    expect(results[0].status).toBe("fulfilled");
    expect(results[0].value).toBe("ok");
    expect(results[1].status).toBe("rejected");
    expect(results[1].reason).toBe("bad");
  }).finally(() => {
    delete Object.prototype.status;
    delete Object.prototype.value;
    delete Object.prototype.reason;
  });
});
