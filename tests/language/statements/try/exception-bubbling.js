// Tests for exception bubbling and nested try-catch behavior

test("exception bubbles up from nested try without catch", () => {
  let outerCaught1 = null;
  try {
    try {
      throw "bubbled from inner";
    } finally {
      // Finally executes but doesn't catch
    }
  } catch (e) {
    outerCaught1 = e;
  }
  expect(outerCaught1).toBe("bubbled from inner");
});

test("exception caught in inner try doesn't bubble", () => {
  let outerExecuted = false;
  let innerCaught = null;
  try {
    try {
      throw "caught in inner";
    } catch (e) {
      innerCaught = e;
    }
  } catch (e) {
    outerExecuted = true;
  }
  expect(innerCaught).toBe("caught in inner");
  expect(outerExecuted).toBe(false);
});

test("exception thrown in catch block bubbles up", () => {
  let outerCaught3 = null;
  try {
    try {
      throw "first error";
    } catch (e) {
      throw "second error from catch";
    }
  } catch (e) {
    outerCaught3 = e;
  }
  expect(outerCaught3).toBe("second error from catch");
});

test("exception thrown in catch block without parameter bubbles up", () => {
  let outerCaught4 = null;
  try {
    try {
      throw "original error";
    } catch {
      throw "new error from catch";
    }
  } catch (e) {
    outerCaught4 = e;
  }
  expect(outerCaught4).toBe("new error from catch");
});

test("multiple levels of nesting", () => {
  let level3Caught = null;
  try {
    try {
      try {
        throw "deep error";
      } finally {
        // Executes but doesn't catch
      }
    } finally {
      // Also executes but doesn't catch
    }
  } catch (e) {
    level3Caught = e;
  }
  expect(level3Caught).toBe("deep error");
});

test("mixed catch with and without parameters in chain", () => {
  let finalCaught = null;
  try {
    try {
      try {
        throw "chain error";
      } catch {
        // Catch without parameter - re-throw
        throw "modified in first catch";
      }
    } catch (e) {
      // Catch with parameter - re-throw
      throw "modified in second catch";
    }
  } catch (final) {
    finalCaught = final;
  }
  expect(finalCaught).toBe("modified in second catch");
});
