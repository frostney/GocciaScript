describe("computed accessor key evaluation order", () => {
  test("keys are evaluated in source declaration order", () => {
    const log = [];
    const k = (n, name) => { log.push(n); return name; };

    class C {
      get [k(1, "a")]() { return 1; }
      set [k(2, "b")](v) {}
      get [k(3, "c")]() { return 3; }
      set [k(4, "a")](v) {}
    }

    expect(log).toEqual([1, 2, 3, 4]);
  });

  test("static and instance keys interleave in source order", () => {
    const log = [];
    const k = (n, name) => { log.push(n); return name; };

    class C {
      get [k(1, "x")]() { return 1; }
      static set [k(2, "y")](v) {}
      set [k(3, "x")](v) {}
      static get [k(4, "z")]() { return 4; }
    }

    expect(log).toEqual([1, 2, 3, 4]);
  });

  test("getter+setter merge with same computed symbol key", () => {
    const sym = Symbol("merged");
    const log = [];
    const ks = (n) => { log.push(n); return sym; };

    class C {
      get [ks(1)]() { return 42; }
      set [ks(2)](v) {}
    }

    expect(log).toEqual([1, 2]);
    const inst = new C();
    expect(inst[sym]).toBe(42);
  });

  test("getter+setter merge with same computed string key", () => {
    const log = [];
    const k = (n, name) => { log.push(n); return name; };

    class C {
      get [k(1, "prop")]() { return 99; }
      set [k(2, "prop")](v) {}
    }

    expect(log).toEqual([1, 2]);
    const inst = new C();
    expect(inst.prop).toBe(99);
  });
});
