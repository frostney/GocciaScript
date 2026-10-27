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

test("a clause entered directly sees an earlier clause's const in TDZ", () => {
  const read = (which) => {
    switch (which) {
      case 0:
        const scale = 5;
        return scale * 2;
      case 1:
        return scale * 2;
    }
    return "none";
  };

  expect(read(0)).toBe(10);
  expect(() => read(1)).toThrow(ReferenceError);
});

test("a clause reached by falling through sees the earlier clause's const", () => {
  const read = (which) => {
    switch (which) {
      case 0:
        const scale = 5;
      case 1:
        return scale + 1;
    }
    return "none";
  };

  expect(read(0)).toBe(6);
  expect(() => read(1)).toThrow(ReferenceError);
});

test("a nested block in a later clause sees the earlier clause's const in TDZ", () => {
  const read = (which) => {
    switch (which) {
      case 0:
        const label = "ready";
        return label;
      default: {
        return label + "!";
      }
    }
  };

  expect(read(0)).toBe("ready");
  expect(() => read(1)).toThrow(ReferenceError);
});

test("switch case test expression sees case lexical TDZ", () => {
  let value = "outer";
  let caught;

  try {
    switch (0) {
      case value:
        break;
      case 1:
        let value = "inner";
        break;
    }
  } catch (e) {
    caught = e;
  }

  expect(caught instanceof ReferenceError).toBe(true);
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

// A let and an inner switch, read from clauses entered directly or reached
// by falling through. The clauses share one scope (ES2026 §14.12.4,
// BlockDeclarationInstantiation of the CaseBlock), so each binding is in its
// TDZ until its own declaration runs.
const readLets = (which) => {
  const seen = [];
  switch (which) {
    case 0:
      let a = 1;
      a += 1;
      seen.push(a);
    case 1:
      let b = 10;
      seen.push(b);
    case 2:
      seen.push(b + a);
  }
  return seen;
};

test("a let updated in its clause keeps its value through fall-through", () => {
  expect(readLets(0)).toEqual([2, 10, 12]);
});

test("a clause entered directly sees an earlier clause's let in TDZ", () => {
  expect(() => readLets(2)).toThrow(ReferenceError);
});

const readNested = (outer, inner) => {
  switch (outer) {
    case 0:
      const a = 1;
      switch (inner) {
        case 0:
          const b = 2;
          return a + b;
        case 1:
          return b;
        case 2:
          const c = 3;
      }
      const after = a + 10;
    case 1:
      return after;
    case 2:
      return a;
  }
};

test("an inner switch's clause reads the const of the outer clause around it", () => {
  expect(readNested(0, 0)).toBe(3);
});

test("an inner switch's clause entered directly sees an earlier inner const in TDZ", () => {
  expect(() => readNested(0, 1)).toThrow(ReferenceError);
});

test("leaving an inner switch keeps the outer clause's bindings for the clauses after it", () => {
  expect(readNested(0, 2)).toBe(11);
});

test("an outer clause entered directly sees in TDZ a const declared before an inner switch", () => {
  expect(() => readNested(2, 0)).toThrow(ReferenceError);
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
