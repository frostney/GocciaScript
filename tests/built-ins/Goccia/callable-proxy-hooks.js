/*---
description: >
  The lifecycle hooks and onTestFinished take any callable, a callable Proxy
  included, and the runner calls it.
features: [Proxy]
---*/

const log = [];
const proxied = (name) => new Proxy(() => { log.push(name); }, {});

describe("lifecycle hooks given a callable Proxy", () => {
  beforeAll(proxied("beforeAll"));
  beforeEach(proxied("beforeEach"));
  afterEach(proxied("afterEach"));
  afterAll(proxied("afterAll"));

  test("beforeAll and beforeEach run before the first test", () => {
    expect(log).toEqual(["beforeAll", "beforeEach"]);
  });

  test("afterEach runs after each test", () => {
    expect(log).toEqual(["beforeAll", "beforeEach", "afterEach", "beforeEach"]);
  });
});

describe("after a suite whose afterAll is a callable Proxy", () => {
  test("afterAll ran", () => {
    expect(log.at(-1)).toBe("afterAll");
  });
});

describe("onTestFinished given a callable Proxy", () => {
  let finished = 0;

  test("registers it", () => {
    onTestFinished(new Proxy(() => {}, {
      apply: () => { finished++; },
    }));
    expect(finished).toBe(0);
  });

  test("its apply trap ran when the previous test finished", () => {
    expect(finished).toBe(1);
  });
});

describe("a hook that is not callable", () => {
  test("a Proxy of a plain object is still rejected", () => {
    expect(() => beforeEach(new Proxy({}, {}))).toThrow(TypeError);
    expect(() => onTestFinished(new Proxy({}, {}))).toThrow(TypeError);
  });
});
