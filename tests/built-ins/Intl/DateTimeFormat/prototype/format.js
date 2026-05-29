/*---
description: Intl.DateTimeFormat.prototype.format
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

const expectCurrentDateResult = (readActual, formatAt) => {
  const before = Date.now();
  const actual = readActual();
  const after = Date.now();
  const beforeFormatted = formatAt(before);
  const afterFormatted = formatAt(after);

  expect(actual === beforeFormatted || actual === afterFormatted).toBe(true);
};

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.format", () => {
  test("format is an accessor property on the prototype", () => {
    const desc = Object.getOwnPropertyDescriptor(Intl.DateTimeFormat.prototype, "format");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.value).toBe(undefined);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
  });

  test("format getter returns a cached bound function", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    const first = dtf.format;
    const second = dtf.format;

    expect(typeof first).toBe("function");
    expect(first).toBe(second);
    expect(first(0)).toBe(dtf.format(0));
    expect(Object.prototype.hasOwnProperty.call(dtf, "format")).toBe(false);
  });

  test("format getter creates distinct functions for distinct instances", () => {
    const first = new Intl.DateTimeFormat("en-US").format;
    const second = new Intl.DateTimeFormat("en-US").format;

    expect(first === second).toBe(false);
  });

  test("format returns a string for a Date object", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    const result = dtf.format(new Date(2025, 0, 1));
    expect(typeof result).toBe("string");
    expect(result.length > 0).toBe(true);
  });

  test("format uses the current date for omitted and undefined values", () => {
    const dtf = new Intl.DateTimeFormat("en-US", { timeZone: "UTC" });
    expectCurrentDateResult(() => dtf.format(), (value) => dtf.format(value));
    expectCurrentDateResult(() => dtf.format(undefined), (value) => dtf.format(value));
  });

  test("format rejects invalid numeric dates", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(() => dtf.format(NaN)).toThrow(RangeError);
  });
});
