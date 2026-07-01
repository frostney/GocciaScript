describe("class constructor callable dispatch", () => {
  class CallbackClass {
    constructor() {
      return 0;
    }
  }

  const expectClassCallTypeError = (callback) => {
    expect(callback).toThrow(TypeError);
  };

  it("throws TypeError when Array sort comparators call class constructors", () => {
    expectClassCallTypeError(() => [2, 1].sort(CallbackClass));
    expectClassCallTypeError(() => [2, 1].toSorted(CallbackClass));
  });

  it("throws TypeError when iterator methods are class constructors", () => {
    const iterable = {};
    iterable[Symbol.iterator] = CallbackClass;

    expectClassCallTypeError(() => Array.from(iterable));
    expectClassCallTypeError(() => {
      for (const value of iterable) {
        value;
      }
    });
  });

  it("throws TypeError when accessor setters are class constructors", () => {
    const object = {};
    Object.defineProperty(object, "value", { set: CallbackClass });

    expectClassCallTypeError(() => {
      object.value = 1;
    });
  });

  it("throws TypeError when accessor getters are class constructors", () => {
    const object = {};
    const prototype = {};
    const symbol = Symbol("getter");

    Object.defineProperty(object, "value", { get: CallbackClass });
    Object.defineProperty(prototype, "inherited", { get: CallbackClass });
    Object.defineProperty(object, symbol, { get: CallbackClass });
    Object.setPrototypeOf(object, prototype);

    expectClassCallTypeError(() => object.value);
    expectClassCallTypeError(() => object.inherited);
    expectClassCallTypeError(() => object[symbol]);
  });

  it("throws TypeError when Reflect.apply calls a class constructor target", () => {
    expectClassCallTypeError(() => Reflect.apply(CallbackClass, undefined, []));
  });

  it("throws TypeError when Map upsert callbacks are class constructors", () => {
    const map = new Map();
    if (typeof map.getOrInsertComputed === "function") {
      expectClassCallTypeError(() => map.getOrInsertComputed("key", CallbackClass));
    }
  });
});
