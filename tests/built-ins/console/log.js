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
