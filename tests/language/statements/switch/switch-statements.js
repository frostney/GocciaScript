/*---
description: Switch statements work correctly including fall-through behavior
features: [switch-statement]
---*/

test("basic switch statement", () => {
  function getDayType(day) {
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
  }

  expect(getDayType("Monday")).toBe("weekday");
  expect(getDayType("Saturday")).toBe("weekend");
  expect(getDayType("InvalidDay")).toBe("unknown");
});

test("switch with fall-through", () => {
  function getCategory(value) {
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
  }

  expect(getCategory(1)).toBe("one small number");
  expect(getCategory(2)).toBe("small number");
  expect(getCategory(3)).toBe("number");
  expect(getCategory(10)).toBe("ten");
  expect(getCategory(99)).toBe("other");
});

test("switch with different types", () => {
  function checkType(value) {
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
  }

  expect(checkType(42)).toBe("numeric");
  expect(checkType("hello")).toBe("textual");
  expect(checkType(true)).toBe("logical");
  expect(checkType(null)).toBe("other type");
});

runTests();
