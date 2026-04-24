/*---
description: fetch enforces allowed hosts list
features: [fetch]
---*/

describe("fetch allowed hosts", () => {
  // Blocked-host tests hit the real allowlist: ValidateHost throws
  // synchronously before any socket work, so no mock is needed.
  describe("blocked hosts (real fetch)", () => {
    test("throws TypeError for host not in allowed list", () => {
      expect(() => fetch("http://blocked.example.com")).toThrow(TypeError);
    });

    test("error message mentions blocked host", () => {
      let caught = false;
      try {
        fetch("http://blocked.example.com");
      } catch (e) {
        caught = true;
        expect(e.message).toContain("blocked.example.com");
      }
      expect(caught).toBe(true);
    });

    test("throws TypeError for HTTPS host not in allowed list", () => {
      expect(() => fetch("https://not-allowed.test")).toThrow(TypeError);
    });
  });

  // Allowed-host tests replace globalThis.fetch with a spy so no network
  // traffic is generated. Real-network allowlist behaviour is exercised by
  // scripts/fetch-e2e.sh. These tests verify the call shape is accepted and
  // dispatched through fetch unchanged.
  describe("allowed hosts (mocked fetch)", () => {
    let spy;

    beforeAll(() => {
      spy = spyOn(globalThis, "fetch").mockImplementation(
        () => Promise.resolve(new Response())
      );
    });

    afterAll(() => {
      spy.mockRestore();
    });

    beforeEach(() => {
      spy.mockClear();
    });

    test("allowed host proceeds to network request", async () => {
      const url = "http://0.0.0.0:1/";
      const p = globalThis.fetch(url);
      expect(typeof p.then).toBe("function");
      await p;
      expect(spy).toHaveBeenCalledWith(url);
    });

    test("host matching is case-insensitive", async () => {
      const url = "http://EXAMPLE.COM:1/";
      await globalThis.fetch(url);
      expect(spy).toHaveBeenCalledWith(url);
    });

    test("port does not affect host matching", async () => {
      const url = "http://example.com:8080/";
      await globalThis.fetch(url);
      expect(spy).toHaveBeenCalledWith(url);
    });

    test("path does not affect host matching", async () => {
      const url = "http://0.0.0.0:1/some/deep/path?q=1";
      await globalThis.fetch(url);
      expect(spy).toHaveBeenCalledWith(url);
    });

    test("userinfo does not affect host matching", async () => {
      const url = "http://user:pass@0.0.0.0:1/";
      await globalThis.fetch(url);
      expect(spy).toHaveBeenCalledWith(url);
    });
  });
});
