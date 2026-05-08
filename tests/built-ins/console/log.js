test("console.log exists and returns undefined", () => {
  expect(typeof console.log).toBe("function");
  expect(console.log("test")).toBeUndefined();
});

test("console.log handles prototype-less objects", () => {
  expect(console.log(Object.create(null))).toBeUndefined();
});

test("console.log handles prototype-less objects with properties", () => {
  const obj = Object.create(null);
  obj.a = 1;
  obj.b = "hello";
  expect(console.log(obj)).toBeUndefined();
});

test("console.log does not invoke user toString on objects", () => {
  let called = false;
  const obj = { toString() { called = true; return "custom"; } };
  console.log(obj);
  expect(called).toBe(false);
});

test("console.log handles Symbol values", () => {
  expect(console.log(Symbol("test"))).toBeUndefined();
});

test("console.log handles circular references without crashing", () => {
  const obj = { a: 1 };
  obj.self = obj;
  expect(console.log(obj)).toBeUndefined();
});

test("console.log handles deeply nested objects", () => {
  const obj = {
    a: { b: { c: { d: { e: { f: { g: { h: "deep" } } } } } } },
  };
  expect(console.log(obj)).toBeUndefined();
});
