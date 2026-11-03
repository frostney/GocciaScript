/*---
description: Overload signatures are erased; the implementation keeps its own parameters
features: [types-as-comments]
---*/

function describeValue(text: string): string;
function describeValue(count: number): string;
function describeValue(value: string | number): string {
  return `${typeof value}:${value}`;
}

async function load(id: string): Promise<string>;
async function load(key: string | number): Promise<string> {
  return `loaded ${key}`;
}

class Store {
  get(name: string): string;
  get(index: number): string;
  get(key: string | number): string {
    return `got ${key}`;
  }

  static make(): Store;
  static make(): Store {
    return new Store();
  }
}

test("a function overload's parameter names do not replace the implementation's", () => {
  expect(describeValue("a")).toBe("string:a");
  expect(describeValue(2)).toBe("number:2");
  expect(describeValue.length).toBe(1);
});

test("async function overloads", async () => {
  expect(await load("x")).toBe("loaded x");
});

test("class method overloads keep the implementation", () => {
  expect(new Store().get(3)).toBe("got 3");
  expect(Store.make() instanceof Store).toBe(true);
});
