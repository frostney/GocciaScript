// Battery F — hook and describe accounting. Vitest is the oracle: a failed
// beforeAll skips the suite's tests rather than failing them, a failed
// afterAll leaves the already-passed tests alone, and a failed
// beforeEach/afterEach fails the test it wraps. Counts alone cannot tell a
// skipped test from a test whose body ran and passed, so every case records
// what actually executed in a module-level marker array and a following suite
// asserts on it.
//
// EXCLUDED — a describe body that throws. Vitest aborts collection for the
// whole file (it reports "no tests"), while goccia registers and runs the
// sibling describes. The queued describe-body collection-abort layer is what
// makes that case comparable; it cannot be written to pass under both current
// behaviours, so it stays out of this battery until that layer lands.
//
// A file-level (top-level) beforeAll cascade cannot live here either: it would
// skip every test in the file. It is battery G.

const beforeAllOrder = [];
const nestedOrder = [];
const asyncOrder = [];
const afterAllOrder = [];
const beforeEachOrder = [];
const afterEachOrder = [];
const passingOrder = [];

describe("failing beforeAll", () => {
  beforeAll(() => {
    beforeAllOrder.push("hook");
    throw new Error("beforeAll boom");
  });

  test("first test under a failed beforeAll", () => {
    beforeAllOrder.push("first-body");
  });

  test("second test under a failed beforeAll", () => {
    beforeAllOrder.push("second-body");
  });
});

describe("failing beforeAll observation", () => {
  test("the failed beforeAll ran and neither test body did", () => {
    expect(beforeAllOrder).toEqual(["hook"]);
  });
});

describe("failing beforeAll with a nested suite", () => {
  beforeAll(() => {
    nestedOrder.push("hook");
    throw new Error("nested beforeAll boom");
  });

  describe("nested child suite", () => {
    test("nested test under the outer failed beforeAll", () => {
      nestedOrder.push("nested-body");
    });
  });

  test("direct test under the outer failed beforeAll", () => {
    nestedOrder.push("direct-body");
  });
});

describe("nested cascade observation", () => {
  test("the cascade reaches nested suites, not just direct children", () => {
    expect(nestedOrder).toEqual(["hook"]);
  });
});

describe("failing async beforeAll", () => {
  beforeAll(async () => {
    asyncOrder.push("hook");
    throw new Error("async beforeAll boom");
  });

  test("test under a rejected async beforeAll", () => {
    asyncOrder.push("body");
  });
});

describe("async cascade observation", () => {
  test("a rejected async beforeAll cascades like a thrown one", () => {
    expect(asyncOrder).toEqual(["hook"]);
  });
});

describe("failing afterAll", () => {
  afterAll(() => {
    afterAllOrder.push("hook");
    throw new Error("afterAll boom");
  });

  test("test that completes before its afterAll fails", () => {
    afterAllOrder.push("body");
    expect(afterAllOrder).toEqual(["body"]);
  });
});

describe("failing afterAll observation", () => {
  test("the body passed and the afterAll failure came after it", () => {
    expect(afterAllOrder).toEqual(["body", "hook"]);
  });
});

describe("failing beforeEach", () => {
  beforeEach(() => {
    beforeEachOrder.push("hook");
    throw new Error("beforeEach boom");
  });

  test("first test under a failed beforeEach", () => {
    beforeEachOrder.push("first-body");
  });

  test("second test under a failed beforeEach", () => {
    beforeEachOrder.push("second-body");
  });
});

describe("failing beforeEach observation", () => {
  test("the hook ran once per test and no test body ran", () => {
    expect(beforeEachOrder).toEqual(["hook", "hook"]);
  });
});

describe("failing afterEach", () => {
  afterEach(() => {
    afterEachOrder.push("hook");
    throw new Error("afterEach boom");
  });

  test("test whose body runs before its afterEach fails", () => {
    afterEachOrder.push("body");
  });
});

describe("failing afterEach observation", () => {
  test("a failed afterEach fails a test whose body already ran", () => {
    expect(afterEachOrder).toEqual(["body", "hook"]);
  });
});

describe("passing hooks", () => {
  beforeAll(() => {
    passingOrder.push("beforeAll");
  });
  beforeEach(() => {
    passingOrder.push("beforeEach");
  });
  afterEach(() => {
    passingOrder.push("afterEach");
  });
  afterAll(() => {
    passingOrder.push("afterAll");
  });

  test("first test with passing hooks", () => {
    passingOrder.push("first-body");
  });

  test("second test with passing hooks", () => {
    passingOrder.push("second-body");
  });
});

describe("passing hook order observation", () => {
  test("hooks wrap each test in the documented order", () => {
    expect(passingOrder).toEqual([
      "beforeAll",
      "beforeEach",
      "first-body",
      "afterEach",
      "beforeEach",
      "second-body",
      "afterEach",
      "afterAll",
    ]);
  });
});
