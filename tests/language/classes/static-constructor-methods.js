describe("static constructor methods", () => {
  test("static constructor method coexists with instance constructor", () => {
    class C {
      static constructor() {
        return "static";
      }

      constructor() {
        this.value = "instance";
      }
    }

    const instance = new C();
    expect(Object.prototype.hasOwnProperty.call(C, "constructor")).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(C.prototype, "constructor")).toBe(true);
    expect(C.constructor()).toBe("static");
    expect(C.prototype.constructor === C.constructor).toBe(false);
    expect(instance.value).toBe("instance");
  });

  test("static generator constructor method coexists with instance constructor", () => {
    class C {
      static *constructor() {
        yield "static";
      }

      constructor() {
        this.value = "instance";
      }
    }

    const result = C.constructor().next();
    expect(Object.prototype.hasOwnProperty.call(C, "constructor")).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(C.prototype, "constructor")).toBe(true);
    expect(result).toEqual({ value: "static", done: false });
    expect(new C().value).toBe("instance");
  });

  test("static async constructor method coexists with instance constructor", async () => {
    class C {
      static async constructor() {
        return "static";
      }

      constructor() {
        this.value = "instance";
      }
    }

    expect(Object.prototype.hasOwnProperty.call(C, "constructor")).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(C.prototype, "constructor")).toBe(true);
    expect(await C.constructor()).toBe("static");
    expect(new C().value).toBe("instance");
  });

  test("static async generator constructor method coexists with instance constructor", async () => {
    class C {
      static async *constructor() {
        yield "static";
      }

      constructor() {
        this.value = "instance";
      }
    }

    const result = await C.constructor().next();
    expect(Object.prototype.hasOwnProperty.call(C, "constructor")).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(C.prototype, "constructor")).toBe(true);
    expect(result).toEqual({ value: "static", done: false });
    expect(new C().value).toBe("instance");
  });
});
