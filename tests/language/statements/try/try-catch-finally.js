test("try-catch-finally with exception and parameter", () => {
  let finallyExecuted1 = false;
  let caught1 = null;
  try {
    throw "error1";
  } catch (e) {
    caught1 = e;
  } finally {
    finallyExecuted1 = true;
  }
  expect(caught1).toBe("error1");
  expect(finallyExecuted1).toBe(true);
});

test("try-catch-finally with exception but no parameter", () => {
  let finallyExecuted2 = false;
  let catchExecuted2 = false;
  try {
    throw "error2";
  } catch {
    catchExecuted2 = true;
  } finally {
    finallyExecuted2 = true;
  }
  expect(catchExecuted2).toBe(true);
  expect(finallyExecuted2).toBe(true);
});

test("try-finally without catch (exception should bubble up", () => {
  let finallyExecuted3 = false;
  let outerCaught = null;
  try {
    try {
      throw "bubbled error";
    } finally {
      finallyExecuted3 = true;
    }
  } catch (e) {
    outerCaught = e;
  }
  expect(finallyExecuted3).toBe(true);
  expect(outerCaught).toBe("bubbled error");
});

test("finally executes even without exception", () => {
  let finallyExecuted4 = false;
  let normalResult = null;
  try {
    normalResult = 100;
  } catch (e) {
    // Should not execute
    normalResult = -1;
  } finally {
    finallyExecuted4 = true;
  }
  expect(normalResult).toBe(100);
  expect(finallyExecuted4).toBe(true);
});

test("finally executes without exception, catch without parameter", () => {
  let finallyExecuted5 = false;
  let catchExecuted5 = false;
  let normalResult5 = null;
  try {
    normalResult5 = 200;
  } catch {
    catchExecuted5 = true;
  } finally {
    finallyExecuted5 = true;
  }
  expect(normalResult5).toBe(200);
  expect(catchExecuted5).toBe(false);
  expect(finallyExecuted5).toBe(true);
});

test("nested try-catch-finally", () => {
  let innerFinally = false;
  let outerFinally = false;
  let result6 = null;
  try {
    try {
      throw "nested error";
    } catch (e) {
      result6 = e;
    } finally {
      innerFinally = true;
    }
  } finally {
    outerFinally = true;
  }
  expect(result6).toBe("nested error");
  expect(innerFinally).toBe(true);
  expect(outerFinally).toBe(true);
});
