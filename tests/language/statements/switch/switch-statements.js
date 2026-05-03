/*---
description: Switch statements work correctly including fall-through behavior
features: [switch-statement]
---*/

test("basic switch statement", () => {
  const getDayType = (day) => {
    switch (day) {
      case "Monday":
      case "Tuesday":
      case "Wednesday":
      case "Thursday":
      case "Friday":
        return "weekday";
      case "Saturday":
      case "Sunday":
        return "weekend";
      default:
        return "unknown";
    }
  };

  expect(getDayType("Monday")).toBe("weekday");
  expect(getDayType("Saturday")).toBe("weekend");
  expect(getDayType("InvalidDay")).toBe("unknown");
});

test("switch with fall-through", () => {
  const getCategory = (value) => {
    let result = "";
    switch (value) {
      case 1:
        result += "one ";
      case 2:
        result += "small ";
      case 3:
        result += "number";
        break;
      case 10:
        result += "ten";
        break;
      default:
        result += "other";
    }
    return result;
  };

  expect(getCategory(1)).toBe("one small number");
  expect(getCategory(2)).toBe("small number");
  expect(getCategory(3)).toBe("number");
  expect(getCategory(10)).toBe("ten");
  expect(getCategory(99)).toBe("other");
});

test("switch with different types", () => {
  const checkType = (value) => {
    switch (typeof value) {
      case "number":
        return "numeric";
      case "string":
        return "textual";
      case "boolean":
        return "logical";
      default:
        return "other type";
    }
  };

  expect(checkType(42)).toBe("numeric");
  expect(checkType("hello")).toBe("textual");
  expect(checkType(true)).toBe("logical");
  expect(checkType(null)).toBe("other type");
});

test("switch uses strict equality", () => {
  let result;
  switch (1) {
    case "1":
      result = "string match";
      break;
    case 1:
      result = "number match";
      break;
    default:
      result = "no match";
  }
  expect(result).toBe("number match");
});

test("switch with expression discriminants and middle default fall-through", () => {
  let result = "";
  switch (1 + 4) {
    case 1:
      result += "one";
      break;
    case 5:
      result += "five";
    default:
      result += " default";
    case 8:
      result += " eight";
  }
  expect(result).toBe("five default eight");
});

test("switch with block scoping in cases", () => {
  let result;
  switch (2) {
    case 1: {
      const msg = "one";
      result = msg;
      break;
    }
    case 2: {
      const msg = "two";
      result = msg;
      break;
    }
    case 3: {
      const msg = "three";
      result = msg;
      break;
    }
  }
  expect(result).toBe("two");
});

test("switch case lexical binding is in TDZ before declaration", () => {
  let value = "outer";

  switch (0) {
    case 0:
      expect(() => value).toThrow(ReferenceError);
      let value = "inner";
      expect(value).toBe("inner");
      break;
  }

  expect(value).toBe("outer");
});

test("switch shared lexical prelude initializes selected case binding", () => {
  let result;

  switch (0) {
    case 0:
      let value = 42;
      result = value;
      break;
  }

  expect(result).toBe(42);
});

test("switch with null and boolean discriminants", () => {
  let nullResult;
  switch (null) {
    case undefined:
      nullResult = "undefined";
      break;
    case null:
      nullResult = "null";
      break;
    default:
      nullResult = "default";
  }

  let boolResult;
  switch (true) {
    case false:
      boolResult = "false";
      break;
    case true:
      boolResult = "true";
      break;
    default:
      boolResult = "default";
  }

  expect(nullResult).toBe("null");
  expect(boolResult).toBe("true");
});
