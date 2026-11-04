/*---
description: Blob.prototype.slice (File API §3.3.1 and §2 slice blob)
features: [Blob]
---*/

describe("Blob.prototype.slice", () => {
  const b = new Blob(["0123456789"], { type: "text/plain" });

  test("with no arguments copies every byte with an empty type", async () => {
    const s = b.slice();
    expect(s).not.toBe(b);
    expect(s.size).toBe(10);
    expect(s.type).toBe("");
    expect(await s.text()).toBe("0123456789");
  });

  test("takes start up to but not including end", async () => {
    expect(await b.slice(2, 5).text()).toBe("234");
  });

  test("negative positions count from the end", async () => {
    expect(await b.slice(-3).text()).toBe("789");
    expect(await b.slice(1, -1).text()).toBe("12345678");
    expect(await b.slice(-100, 2).text()).toBe("01");
  });

  test("positions are clamped to the size and an inverted range is empty", () => {
    expect(b.slice(5, 100).size).toBe(5);
    expect(b.slice(100).size).toBe(0);
    expect(b.slice(6, 3).size).toBe(0);
  });

  test("positions convert as [Clamp] long long", () => {
    expect(b.slice(NaN).size).toBe(10);
    expect(b.slice(0, Infinity).size).toBe(10);
    expect(b.slice(-Infinity).size).toBe(10);
    expect(b.slice("2", "4").size).toBe(2);
    // Rounded to the nearest integer, ties to even.
    expect(b.slice(1.5).size).toBe(8);
    expect(b.slice(2.5).size).toBe(8);
    expect(b.slice(2.6).size).toBe(7);
    expect(b.slice(-2.5).size).toBe(2);
  });

  test("undefined arguments are not given", () => {
    expect(b.slice(undefined, undefined).size).toBe(10);
    expect(b.slice(undefined, 3).size).toBe(3);
  });

  test("contentType is normalised as the constructor's type is", () => {
    expect(b.slice(0, 1, "Image/PNG").type).toBe("image/png");
    expect(b.slice(0, 1, "aÿ").type).toBe("");
  });

  test("arguments convert in order after the receiver check", () => {
    const order = [];
    const arg = (name, value) => ({ valueOf() { order.push(name); return value; }, toString() { order.push(name); return String(value); } });
    b.slice(arg("start", 0), arg("end", 1), arg("type", "x"));
    expect(order).toEqual(["start", "end", "type"]);
    expect(() => Blob.prototype.slice.call({}, arg("never", 0))).toThrow(TypeError);
    expect(order.length).toBe(3);
  });

  test("a Symbol position is a TypeError", () => {
    expect(() => b.slice(Symbol())).toThrow(TypeError);
  });

  test("the slice of a slice", async () => {
    expect(await b.slice(2, 8).slice(1, -1).text()).toBe("3456");
  });
});
