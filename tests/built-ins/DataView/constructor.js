/*---
description: DataView constructor
features: [DataView, ArrayBuffer, SharedArrayBuffer]
---*/

describe("DataView constructor", () => {
  test("creates a view over an ArrayBuffer", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    expect(view.buffer).toBe(buffer);
    expect(view.byteOffset).toBe(0);
    expect(view.byteLength).toBe(8);
    expect(view instanceof DataView).toBe(true);
  });

  test("creates a view with byteOffset and byteLength", () => {
    const buffer = new ArrayBuffer(16);
    const view = new DataView(buffer, 4, 8);
    expect(view.buffer).toBe(buffer);
    expect(view.byteOffset).toBe(4);
    expect(view.byteLength).toBe(8);
  });

  test("supports SharedArrayBuffer backing storage", () => {
    const buffer = new SharedArrayBuffer(8);
    const view = new DataView(buffer, 2, 4);
    expect(view.buffer).toBe(buffer);
    expect(view.byteOffset).toBe(2);
    expect(view.byteLength).toBe(4);
  });

  test("auto-length view over resizable ArrayBuffer tracks resize", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 16 });
    const view = new DataView(buffer, 1);
    expect(view.byteLength).toBe(3);
    buffer.resize(8);
    expect(view.byteLength).toBe(7);
  });

  test("fixed-length view over resizable ArrayBuffer keeps byteLength", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 16 });
    const view = new DataView(buffer, 1, 2);
    buffer.resize(8);
    expect(view.byteLength).toBe(2);
  });
});

describe("DataView constructor error cases", () => {
  test("requires new", () => {
    expect(() => DataView(new ArrayBuffer(1))).toThrow(TypeError);
  });

  test("requires ArrayBuffer-compatible backing storage", () => {
    expect(() => new DataView({})).toThrow(TypeError);
    expect(() => new DataView()).toThrow(TypeError);
  });

  test("throws RangeError when offset is outside the buffer", () => {
    const buffer = new ArrayBuffer(4);
    expect(() => new DataView(buffer, 5)).toThrow(RangeError);
  });

  test("validates offset before resolving newTarget prototype", () => {
    const buffer = new ArrayBuffer(0, { maxByteLength: 1 });
    const events = [];
    class NewTargetBase {}
    const NewTarget = new Proxy(NewTargetBase, {
      get(target, property) {
        if (property === "prototype") {
          events.push("prototype");
          throw new Error("prototype");
        }
        return target[property];
      },
    });

    expect(() => Reflect.construct(DataView, [buffer, 1], NewTarget)).toThrow(RangeError);
    expect(events).toEqual([]);
  });

  test("subclass validates offset before resolving newTarget prototype", () => {
    const buffer = new ArrayBuffer(0, { maxByteLength: 1 });
    const events = [];
    class Sub extends DataView {}
    class NewTargetBase {}
    const NewTarget = new Proxy(NewTargetBase, {
      get(target, property) {
        if (property === "prototype") {
          events.push("prototype");
          throw new Error("prototype");
        }
        return target[property];
      },
    });

    expect(() => Reflect.construct(Sub, [buffer, 1], NewTarget)).toThrow(RangeError);
    expect(events).toEqual([]);
  });

  test("throws RangeError when byteLength extends past the buffer", () => {
    const buffer = new ArrayBuffer(4);
    expect(() => new DataView(buffer, 2, 3)).toThrow(RangeError);
  });

  test("throws TypeError when constructed over a detached ArrayBuffer", () => {
    const buffer = new ArrayBuffer(4);
    buffer.transfer();
    expect(() => new DataView(buffer)).toThrow(TypeError);
  });
});
