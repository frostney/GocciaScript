/*---
description: PermissionDenied is the Error subclass capability denials throw
features: [PermissionDenied]
---*/

describe("PermissionDenied", () => {
  test("is a global constructor", () => {
    expect(typeof PermissionDenied).toBe("function");
    expect(PermissionDenied.name).toBe("PermissionDenied");
    expect(PermissionDenied.length).toBe(1);
  });

  test("constructs an Error subclass", () => {
    const error = new PermissionDenied("read: ./data.txt");
    expect(error instanceof PermissionDenied).toBe(true);
    expect(error instanceof Error).toBe(true);
    expect(error.name).toBe("PermissionDenied");
    expect(error.message).toBe("read: ./data.txt");
    expect(Error.isError(error)).toBe(true);
  });

  test("can be called without new", () => {
    const error = PermissionDenied("net: example.com");
    expect(error instanceof PermissionDenied).toBe(true);
    expect(error.message).toBe("net: example.com");
  });

  test("prototype chain links to Error.prototype", () => {
    expect(Object.getPrototypeOf(PermissionDenied.prototype)).toBe(
      Error.prototype,
    );
    expect(Object.getPrototypeOf(PermissionDenied)).toBe(Error);
    expect(PermissionDenied.prototype.constructor).toBe(PermissionDenied);
  });

  test("supports a cause option", () => {
    const cause = new Error("inner");
    const error = new PermissionDenied("ffi: ./lib.so", { cause });
    expect(error.cause).toBe(cause);
  });

  test("subclasses keep their own prototype", () => {
    class MyDenial extends PermissionDenied {}
    const error = new MyDenial("net: example.com");
    expect(error instanceof MyDenial).toBe(true);
    expect(error instanceof PermissionDenied).toBe(true);
    expect(error.name).toBe("PermissionDenied");
  });
});
