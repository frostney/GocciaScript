/*---
description: Error and NativeError prototype name properties
features: [Error]
---*/

test.each([Error, TypeError, RangeError, ReferenceError, SyntaxError, URIError])(
  "%s.prototype.name has the standard descriptor",
  (ErrorType) => {
    const descriptor = Object.getOwnPropertyDescriptor(ErrorType.prototype, "name");
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.writable).toBe(true);
    expect(descriptor.configurable).toBe(true);
  },
);
