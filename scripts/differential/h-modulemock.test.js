import { vi } from "vitest";

import identify, { compute, label } from "./mods/mockable.js";
import { callCompute, callDefault, readLabel } from "./mods/mockable-consumer.js";
import { tag as unmockLastTag } from "./mods/unmock-last.js";
import { tag as mockLastTag } from "./mods/mock-last.js";

// Every `vi.mock` below is written AFTER the imports it affects. That placement
// is the point: both runtimes must hoist these above the import statements, so
// a differential suite that sees the mocked values has proved hoisting rather
// than merely proved that assignment order happened to work out.
vi.mock("./mods/mockable.js", () => ({
  label: "MOCK_LABEL",
  compute: vi.fn(() => 99),
  default: () => "MOCK_DEFAULT",
}));

// Mock then unmock: the unmock is last in source order, so the real module wins.
vi.mock("./mods/unmock-last.js", () => ({ tag: "MOCK_UNMOCK_LAST" }));
vi.unmock("./mods/unmock-last.js");

// Unmock then mock: the mock is last in source order, so the mock wins.
vi.unmock("./mods/mock-last.js");
vi.mock("./mods/mock-last.js", () => ({ tag: "MOCK_MOCK_LAST" }));

describe("vi.mock module mocking", () => {
  test("a factory replaces the module for the test file's own import", () => {
    expect(label).toBe("MOCK_LABEL");
  });

  test("the factory's default key becomes the default export", () => {
    expect(identify()).toBe("MOCK_DEFAULT");
  });

  test("code under test importing the same module sees the mock", () => {
    expect(readLabel()).toBe("MOCK_LABEL");
    expect(callDefault()).toBe("MOCK_DEFAULT");
  });

  test("a factory export can be a vi.fn spy", () => {
    expect(compute(1, 2)).toBe(99);
    expect(compute).toHaveBeenCalledWith(1, 2);
  });

  test("the test binding and the consumer binding are the same mock", () => {
    // Call through the consumer, observe through the test file's own binding.
    // This only holds if both resolved to one shared module instance.
    const before = compute.mock.calls.length;
    callCompute(7, 8);
    expect(compute.mock.calls.length).toBe(before + 1);
    expect(compute).toHaveBeenCalledWith(7, 8);
  });

  test("unmock last in source order leaves the real module", () => {
    expect(unmockLastTag).toBe("REAL_UNMOCK_LAST");
  });

  test("mock last in source order applies the mock", () => {
    expect(mockLastTag).toBe("MOCK_MOCK_LAST");
  });
});
