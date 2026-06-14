/*---
description: for-of using disposal state survives generator body suspension
features: [explicit-resource-management, generators]
---*/

test("generator for-of using keeps pending disposal across body yields", () => {
  const disposed = [];
  const resources = [
    {
      name: "first",
      [Symbol.dispose]() { disposed.push(this.name); }
    }
  ];

  function* run() {
    for (using resource of resources) {
      yield resource.name;
      yield resource.name + "-again";
    }
    return disposed.join(",");
  }

  const iterator = run();
  expect(iterator.next()).toEqual({ value: "first", done: false });
  expect(disposed).toEqual([]);
  expect(iterator.next()).toEqual({ value: "first-again", done: false });
  expect(disposed).toEqual([]);
  expect(iterator.next()).toEqual({ value: "first", done: true });
  expect(disposed).toEqual(["first"]);
});

test("generator return disposes suspended for-of using resource", () => {
  const disposed = [];
  const resources = [
    {
      name: "first",
      [Symbol.dispose]() { disposed.push(this.name); }
    }
  ];

  function* run() {
    for (using resource of resources) {
      yield resource.name;
      yield "unreachable";
    }
  }

  const iterator = run();
  expect(iterator.next()).toEqual({ value: "first", done: false });
  expect(disposed).toEqual([]);
  expect(iterator.return("done")).toEqual({ value: "done", done: true });
  expect(disposed).toEqual(["first"]);
});
