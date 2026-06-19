test("object spread copies enumerable proxy properties through descriptors", () => {
  const events = [];
  const source = new Proxy({ a: 1, b: 2 }, {
    ownKeys() {
      events.push("ownKeys");
      return ["a", "b"];
    },
    getOwnPropertyDescriptor(target, key) {
      events.push("desc:" + key);
      return {
        configurable: true,
        enumerable: key === "a",
        value: target[key]
      };
    },
    get(target, key) {
      events.push("get:" + key);
      return target[key] * 10;
    }
  });

  const result = { ...source };

  expect(result.a).toBe(10);
  expect("b" in result).toBe(false);
  expect(events.join("|")).toBe("ownKeys|desc:a|get:a|desc:b");
});

test("object spread copies enumerable symbol properties", () => {
  const symbolKey = Symbol("spread");
  const result = { ...{ [symbolKey]: 7, plain: 3 } };

  expect(result[symbolKey]).toBe(7);
  expect(result.plain).toBe(3);
});

test("object spread copies enumerable proxy symbol properties", () => {
  const symbolKey = Symbol("proxy-spread");
  const events = [];
  const source = new Proxy({}, {
    ownKeys() {
      events.push("ownKeys");
      return [symbolKey];
    },
    getOwnPropertyDescriptor(target, key) {
      events.push(key === symbolKey ? "desc:symbol" : "desc:string");
      return {
        configurable: true,
        enumerable: true,
        value: 12
      };
    },
    get(target, key) {
      events.push(key === symbolKey ? "get:symbol" : "get:string");
      return 12;
    }
  });

  const result = { ...source };

  expect(result[symbolKey]).toBe(12);
  expect(events.join("|")).toBe("ownKeys|desc:symbol|get:symbol");
});

test("object rest excludes computed symbol keys before descriptors", () => {
  const symbolKey = Symbol("rest");
  const events = [];
  const source = new Proxy({}, {
    ownKeys() {
      events.push("ownKeys");
      return [symbolKey, "plain"];
    },
    getOwnPropertyDescriptor(target, key) {
      events.push(key === symbolKey ? "desc:symbol" : "desc:" + key);
      return {
        configurable: true,
        enumerable: true,
        value: key === symbolKey ? "skip" : "keep"
      };
    },
    get(target, key) {
      events.push(key === symbolKey ? "get:symbol" : "get:" + key);
      return key === symbolKey ? "skip" : "keep";
    }
  });

  const { [symbolKey]: skipped, ...rest } = source;

  expect(skipped).toBe("skip");
  expect(rest.plain).toBe("keep");
  expect(rest[symbolKey]).toBe(undefined);
  expect(events.join("|")).toBe("get:symbol|ownKeys|desc:plain|get:plain");
});
