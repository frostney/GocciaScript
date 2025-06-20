test("basic try-catch with parameter", () => {
  let result1 = "";
  try {
    throw "test error";
  } catch (e) {
    result1 = e;
  }
  expect(result1).toBe("test error");
});

test("try-catch without parameter", () => {
  let executed = false;
  try {
    throw "ignored error";
  } catch {
    executed = true;
  }
  expect(executed).toBe(true);
});

test("catch different value types", () => {
  let numberResult = 0;
  try {
    throw 42;
  } catch (num) {
    numberResult = num;
  }
  expect(numberResult).toBe(42);

  let objectResult = null;
  try {
    throw { message: "error", code: 404 };
  } catch (obj) {
    objectResult = obj;
  }
  expect(typeof objectResult).toBe("object");
  expect(objectResult.message).toBe("error");
  expect(objectResult.code).toBe(404);

  let boolResult = null;
  try {
    throw true;
  } catch (bool) {
    boolResult = bool;
  }
  expect(boolResult).toBe(true);
});

test("no exception thrown - catch should not execute", () => {
  let shouldNotExecute = false;
  try {
    let x = 1 + 1;
  } catch (e) {
    shouldNotExecute = true;
  }
  expect(shouldNotExecute).toBe(false);
});

test("no exception thrown - catch without parameter should not execute", () => {
  let shouldNotExecute2 = false;
  try {
    let y = 2 + 2;
  } catch {
    shouldNotExecute2 = true;
  }
  expect(shouldNotExecute2).toBe(false);
});
