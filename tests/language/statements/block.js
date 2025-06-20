test("variable declared in inner scope, assigned in outer scope", () => {
  let testVar = "initial";

  expect(testVar).toBe("initial");

  {
    expect(testVar).toBe("initial");
  }

  expect(testVar).toBe("initial");
});

test("variable declared in outer scope, assigned in inner scope", () => {
  let testVar = "initial";

  expect(testVar).toBe("initial");

  {
    expect(testVar).toBe("initial");
    testVar = "modified";
    expect(testVar).toBe("modified");
  }

  expect(testVar).toBe("modified");
});

test("variable declared in outer scope, and declared again in inner scope", () => {
  let testVar = "initial";
  expect(testVar).toBe("initial");
  {
    let testVar = "modified";
    expect(testVar).toBe("modified");
  }
  expect(testVar).toBe("initial");
});

test("variable defined in inner scope, only available in inner scope", () => {
  {
    let testVar = "initial";
    expect(testVar).toBe("initial");
  }

  expect(() => {
    testVar;
  }).toThrow();
});
