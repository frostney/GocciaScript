describe("Date constructor", () => {
  test("typeof Date is function", () => {
    expect(typeof Date).toBe("function");
  });

  test("Date has the standard constructor name descriptor", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Date, "name");

    expect(Date.name).toBe("Date");
    expect(descriptor.value).toBe("Date");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });

  test("Date called without new returns a string", () => {
    expect(typeof Date()).toBe("string");
  });

  test("new Date() returns current time", () => {
    const before = Date.now();
    const d = new Date();
    const after = Date.now();
    expect(d.getTime() >= before).toBe(true);
    expect(d.getTime() <= after).toBe(true);
  });

  test("new Date(ms) creates from epoch milliseconds", () => {
    const d = new Date(0);
    expect(d.getTime()).toBe(0);
    expect(d.getUTCFullYear()).toBe(1970);
    expect(d.getUTCMonth()).toBe(0);
    expect(d.getUTCDate()).toBe(1);
  });

  test("new Date(ms) TimeClip canonicalizes negative zero", () => {
    expect(Object.is(new Date(-0).getTime(), +0)).toBe(true);
    expect(Object.is(new Date(-1.23e-15).valueOf(), +0)).toBe(true);
  });

  test("Date instances allow own Symbol.toStringTag overrides", () => {
    const d = new Date(0);
    expect(Object.prototype.toString.call(d)).toBe("[object Date]");

    d[Symbol.toStringTag] = "test262";
    expect(Object.hasOwn(d, Symbol.toStringTag)).toBe(true);
    expect(Object.prototype.toString.call(d)).toBe("[object test262]");
  });

  test("Reflect.construct initializes Date internal slot with custom newTarget", () => {
    class NewTarget {}

    const d = Reflect.construct(Date, [0], NewTarget);
    expect(Object.getPrototypeOf(d)).toBe(NewTarget.prototype);
    expect(d instanceof NewTarget).toBe(true);
    expect(d instanceof Date).toBe(false);
    expect(Date.prototype.getTime.call(d)).toBe(0);
  });

  test("new Date(string) parses ISO string", () => {
    const d = new Date("2024-06-15T12:30:00Z");
    expect(d.getUTCFullYear()).toBe(2024);
    expect(d.getUTCMonth()).toBe(5);
    expect(d.getUTCDate()).toBe(15);
    expect(d.getUTCHours()).toBe(12);
    expect(d.getUTCMinutes()).toBe(30);
  });

  test("new Date(year, month) with component args", () => {
    // Month is 0-indexed, so month=0 is January
    const d = new Date(2024, 0, 15, 0, 0, 0, 0);
    expect(d.getFullYear()).toBe(2024);
    expect(d.getMonth()).toBe(0);
    expect(d.getDate()).toBe(15);
  });

  test("normalizes overflowing component fields", () => {
    const value = new Date(2020, 12, 1, 24, 60, 60, 1000);
    expect(value.getFullYear()).toBe(2021);
    expect(value.getMonth()).toBe(0);
    expect(value.getDate()).toBe(2);
    expect(value.getHours()).toBe(1);
    expect(value.getMinutes()).toBe(1);
    expect(value.getSeconds()).toBe(1);
    expect(value.getMilliseconds()).toBe(0);
  });

  test("copies the time value from a Date argument and clips numeric values", () => {
    const source = new Date(1718451045123);
    expect(new Date(source).getTime()).toBe(1718451045123);
    expect(new Date(2000).getTime()).toBe(2000);
    expect(Number.isNaN(new Date(NaN).getTime())).toBe(true);
  });

  test("year 0-99 maps to 1900-1999", () => {
    const d = new Date(95, 0, 1, 0, 0, 0, 0);
    expect(d.getFullYear()).toBe(1995);
  });

  test("Date default primitive hint prefers string", () => {
    const d = new Date(0);
    expect(d + 0).toBe(d.toString() + "0");
    expect(d + d).toBe(d.toString() + d.toString());
    expect(Number(d)).toBe(0);
  });

  test("Date numeric operators use the number primitive hint", () => {
    const d = new Date(2026, 0, 1);
    const time = d.valueOf();

    expect(+d).toBe(time);
    expect(d - 0).toBe(time);
    expect(0 - d).toBe(-time);
  });

});
