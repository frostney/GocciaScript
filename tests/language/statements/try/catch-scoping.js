test("catch parameter shadows outer variable", () => {
  let outerVar = "outer_value";
  let caughtValue = null;
  try {
    throw "test_value";
  } catch (outerVar) {
    // outerVar in catch block should be the caught exception
    caughtValue = outerVar;
  }
  // Outer variable should be unchanged
  expect(caughtValue).toBe("test_value");
  expect(outerVar).toBe("outer_value");
});

test("catch without parameter doesn't affect outer variables", () => {
  let outerVar2 = "original";
  let catchExecuted = false;
  try {
    throw "some error";
  } catch {
    // No parameter - outer variables accessible normally
    catchExecuted = true;
    expect(outerVar2).toBe("original");
  }
  expect(catchExecuted).toBe(true);
  expect(outerVar2).toBe("original");
});

test("nested catch parameters with same name", () => {
  let finalResult = "";
  try {
    try {
      throw "inner_error";
    } catch (e) {
      finalResult += e + "_";
      try {
        throw "nested_error";
      } catch (e) {
        // Inner e shadows outer e
        finalResult += e;
      }
    }
  } catch (e) {
    finalResult += "_should_not_execute";
  }
  expect(finalResult).toBe("inner_error_nested_error");
});

test("catch parameter doesn't leak outside catch block", () => {
  let leakTest = "initial";
  try {
    throw "leaked?";
  } catch (leakTest) {
    // leakTest parameter shadows outer variable
    expect(leakTest).toBe("leaked?");
  }
  // Outer leakTest should be unchanged
  expect(leakTest).toBe("initial");
});

test("mixed parameter and no-parameter catches in nested structure", () => {
  let param_result = "";
  let no_param_executed = false;
  try {
    try {
      throw "error1";
    } catch (e) {
      param_result += e + "_";
      try {
        throw "error2";
      } catch {
        // No parameter catch
        no_param_executed = true;
        param_result += "no_param_";
      }
    }
  } catch {
    param_result += "outer_no_param";
  }
  expect(param_result).toBe("error1_no_param_");
  expect(no_param_executed).toBe(true);
});

let global_test = "global";

test("catch parameter with same name as global variable", () => {
  global_test = "global_value";
  let caught_global = null;
  try {
    throw "caught_value";
  } catch (global_test) {
    caught_global = global_test;
  }
  expect(caught_global).toBe("caught_value");
  expect(global_test).toBe("global_value");
});
