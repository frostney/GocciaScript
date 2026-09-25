class BatchError extends Error {}
class NamedError extends Error {
  constructor(message) {
    super(message);
    this.name = "NamedError";
  }
}
class NestedError extends BatchError {}
class Plain {}
class Sibling {}

describe("toBeInstanceOf", () => {
  test("matches a user-defined Error subclass", () => {
    expect(new BatchError("x")).toBeInstanceOf(BatchError);
  });

  test("matches a subclass that sets its own name", () => {
    expect(new NamedError("x")).toBeInstanceOf(NamedError);
  });

  test("matches every constructor along the prototype chain", () => {
    const nested = new NestedError("x");

    expect(nested).toBeInstanceOf(NestedError);
    expect(nested).toBeInstanceOf(BatchError);
    expect(nested).toBeInstanceOf(Error);
  });

  test("matches an Object.assign-extended subclass instance", () => {
    const decorated = Object.assign(new BatchError("x"), { code: "E_BATCH" });

    expect(decorated).toBeInstanceOf(BatchError);
    expect(decorated.code).toBe("E_BATCH");
  });

  test("agrees with the instanceof operator and toThrow", () => {
    const failing = () => {
      throw new BatchError("x");
    };

    expect(new BatchError("x") instanceof BatchError).toBe(true);
    expect(failing).toThrow(BatchError);
    expect(new BatchError("x")).toBeInstanceOf(BatchError);
  });

  test("matches built-in error constructors", () => {
    expect(new TypeError("x")).toBeInstanceOf(TypeError);
    expect(new TypeError("x")).toBeInstanceOf(Error);
  });

  test("matches plain classes", () => {
    expect(new Plain()).toBeInstanceOf(Plain);
  });

  test("negates for an unrelated class", () => {
    expect(new Plain()).not.toBeInstanceOf(Error);
    expect(new BatchError("x")).not.toBeInstanceOf(Sibling);
    expect(new Plain()).not.toBeInstanceOf(Sibling);
  });

  test("negates for a sibling error subclass", () => {
    expect(new BatchError("x")).not.toBeInstanceOf(NamedError);
    expect(new TypeError("x")).not.toBeInstanceOf(BatchError);
  });

  test("negates for a superclass asked of a superclass instance", () => {
    expect(new BatchError("x")).not.toBeInstanceOf(NestedError);
  });

  test("matches through rejects", async () => {
    const failing = async () => {
      throw new BatchError("x");
    };

    await expect(failing()).rejects.toBeInstanceOf(BatchError);
    await expect(failing()).rejects.toBeInstanceOf(Error);
  });

  // The text of the failure message is pinned in scripts/test-cli.ts: a failed
  // assertion is recorded rather than thrown, so it cannot be observed from
  // inside a test.
});
