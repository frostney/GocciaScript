/*---
description: fetch throws TypeError when no allowed hosts are configured
features: [fetch]
---*/

describe("fetch without allowed hosts", () => {
  test("throws TypeError when no allowed hosts are configured", () => {
    expect(() => fetch("http://example.com")).toThrow(TypeError);
  });

  test("error message mentions allowed hosts", () => {
    let caught = false;
    try {
      fetch("http://example.com");
    } catch (e) {
      caught = true;
      expect(e.message).toContain("allowed hosts");
    }
    expect(caught).toBe(true);
  });

  test("throws TypeError for any URL when no hosts allowed", () => {
    expect(() => fetch("http://localhost:3000/api")).toThrow(TypeError);
  });

  test("throws TypeError with URL object when no hosts allowed", () => {
    const url = new URL("http://example.com/path");
    expect(() => fetch(url)).toThrow(TypeError);
  });
});
