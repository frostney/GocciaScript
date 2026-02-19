import { valueA, receivedB } from "./helpers/circular-a.js";
import { valueB } from "./helpers/circular-b.js";

describe("circular imports", () => {
  test("module A exports its own value", () => {
    expect(valueA).toBe("from A");
  });

  test("module A received value from B", () => {
    expect(receivedB).toBe("from B");
  });

  test("module B exports its own value", () => {
    expect(valueB).toBe("from B");
  });
});
