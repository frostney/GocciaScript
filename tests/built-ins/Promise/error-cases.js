/*---
description: Promise combinator methods throw TypeError for invalid arguments
features: [Promise]
---*/

test("Promise.all with non-array throws TypeError", () => {
  let caught = false;
  try {
    Promise.all(42);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.all with string throws TypeError", () => {
  let caught = false;
  try {
    Promise.all("hello");
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.all with null throws TypeError", () => {
  let caught = false;
  try {
    Promise.all(null);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.all with undefined throws TypeError", () => {
  let caught = false;
  try {
    Promise.all(undefined);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.allSettled with non-array throws TypeError", () => {
  let caught = false;
  try {
    Promise.allSettled(42);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.race with non-array throws TypeError", () => {
  let caught = false;
  try {
    Promise.race(42);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.any with non-array throws TypeError", () => {
  let caught = false;
  try {
    Promise.any(42);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise.all with object throws TypeError", () => {
  let caught = false;
  try {
    Promise.all({ length: 2 });
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});
