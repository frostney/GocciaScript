describe("Date methods", () => {
  // 2024-06-15T11:30:45.123Z (Saturday)
  const epoch = 1718451045123;
  const d = new Date(epoch);

  test("getTime and valueOf return epoch ms", () => {
    expect(d.getTime()).toBe(epoch);
    expect(d.valueOf()).toBe(epoch);
  });

  test("UTC getters", () => {
    expect(d.getUTCFullYear()).toBe(2024);
    expect(d.getUTCMonth()).toBe(5);
    expect(d.getUTCDate()).toBe(15);
    expect(d.getUTCDay()).toBe(6);
    expect(d.getUTCHours()).toBe(11);
    expect(d.getUTCMinutes()).toBe(30);
    expect(d.getUTCSeconds()).toBe(45);
    expect(d.getUTCMilliseconds()).toBe(123);
  });

  test("toISOString returns ISO 8601", () => {
    expect(d.toISOString()).toBe("2024-06-15T11:30:45.123Z");
  });

  test("toJSON returns same as toISOString", () => {
    expect(d.toJSON()).toBe(d.toISOString());
  });

  test("toString returns formatted string", () => {
    const str = d.toString();
    expect(str).toContain("2024");
    expect(str).toContain("GMT");
  });

  test("Date.now() returns a number", () => {
    expect(typeof Date.now()).toBe("number");
  });

  test("Date.parse() parses ISO string to epoch ms", () => {
    expect(Date.parse("2024-06-15T11:30:45.123Z")).toBe(epoch);
  });

  test("Date.parse() rejects invalid ISO fields instead of normalizing", () => {
    const invalid = [
      "2024-13-01",
      "2024-00-01",
      "2024-02-30",
      "2024-+1-01",
      "2024-01-01T25:00:00Z",
      "2024-01-01T24:01:00Z",
      "2024-01-01T00:60:00Z",
      "2024-01-01T00:00:60Z",
      "2024-01-01T00:00:00Zgarbage",
      "2024-01-01T00:00:00Z+01:00",
      "2024-01-01T00:00:00+99:99"
    ];

    for (const value of invalid) {
      expect(Number.isNaN(Date.parse(value))).toBe(true);
    }
  });

  test("Date.parse() accepts 24:00 only as midnight at the end of the day", () => {
    expect(Date.parse("1995-02-04T24:00Z")).toBe(Date.parse("1995-02-05T00:00Z"));
    expect(Number.isNaN(Date.parse("1995-02-04T24:00:00.001Z"))).toBe(true);
  });

  test("Date.parse() handles legacy slash and month-name dates", () => {
    expect(new Date("5/1/49").getFullYear()).toBe(2049);
    expect(new Date("5/1/50").getFullYear()).toBe(1950);
    expect(new Date("49/5/1").getMonth()).toBe(4);
    expect(new Date("may 1 49").getTime()).toBe(new Date("5/1/49").getTime());
    expect(new Date("1 49 may").getTime()).toBe(new Date("5/1/49").getTime());
    expect(Number.isNaN(new Date("13/13/13").getTime())).toBe(true);
  });

  test("Date.UTC() returns an epoch millisecond timestamp", () => {
    expect(Date.UTC(2024, 5, 15, 11, 30, 45, 123)).toBe(epoch);
  });

  test("Date.UTC() normalizes overflowing date and time fields", () => {
    expect(Date.UTC(2020, 12, 1)).toBe(Date.UTC(2021, 0, 1));
    expect(Date.UTC(2020, 0, 32)).toBe(Date.UTC(2020, 1, 1));
    expect(Date.UTC(2020, -1, 1)).toBe(Date.UTC(2019, 11, 1));
    expect(Date.UTC(2020, 0, 1, 24, 60, 60, 1000)).toBe(Date.UTC(2020, 0, 2, 1, 1, 1, 0));
  });

  test("Date.UTC() returns NaN when normalized fields exceed TimeClip", () => {
    expect(Number.isNaN(Date.UTC(1970, 0, -99999999, 0, -60, 0, -1))).toBe(true);
  });

  test("Date.UTC() treats omitted and explicit undefined arguments differently", () => {
    expect(Number.isNaN(Date.UTC())).toBe(true);
    expect(Date.UTC(2020)).toBe(Date.UTC(2020, 0, 1));
    expect(Number.isNaN(Date.UTC(2020, undefined))).toBe(true);
  });

  test("multi-argument Date constructor normalizes overflowing fields", () => {
    const value = new Date(2020, 12, 1, 24, 60, 60, 1000);
    expect(value.getFullYear()).toBe(2021);
    expect(value.getMonth()).toBe(0);
    expect(value.getDate()).toBe(2);
    expect(value.getHours()).toBe(1);
    expect(value.getMinutes()).toBe(1);
    expect(value.getSeconds()).toBe(1);
    expect(value.getMilliseconds()).toBe(0);
  });

  test("toISOString throws when normalized Date constructor fields exceed TimeClip", () => {
    const timeZoneMinutes = new Date(0).getTimezoneOffset() * -1;
    let value;

    if (timeZoneMinutes > 0) {
      value = new Date(1970, 0, -99999999, 0, 0, 0, -1);
    } else {
      value = new Date(1970, 0, -99999999, 0, timeZoneMinutes - 60, 0, -1);
    }

    expect(() => value.toISOString()).toThrow(RangeError);
  });

  test("setDate() updates the local day and returns epoch ms", () => {
    const value = new Date(2024, 0, 1);
    const result = value.setDate(value.getDate() + 1);
    expect(result).toBe(value.getTime());
    expect(value.getDate()).toBe(2);
  });

  test("UTC setters update fields and return epoch ms", () => {
    const value = new Date(0);
    expect(value.setTime(epoch)).toBe(epoch);
    expect(value.setUTCMilliseconds(456)).toBe(value.getTime());
    expect(value.getUTCMilliseconds()).toBe(456);
    expect(value.setUTCFullYear(2025, 0, 2)).toBe(value.getTime());
    expect(value.getUTCFullYear()).toBe(2025);
    expect(value.getUTCMonth()).toBe(0);
    expect(value.getUTCDate()).toBe(2);
  });

  test("setTime() TimeClip canonicalizes negative zero", () => {
    const value = new Date(1);
    expect(Object.is(value.setTime(-0), +0)).toBe(true);
    expect(Object.is(value.getTime(), +0)).toBe(true);
    expect(Object.is(value.setTime(-1.23e-15), +0)).toBe(true);
    expect(Object.is(value.valueOf(), +0)).toBe(true);
  });

  test("multi-argument setters distinguish omitted fields from explicit undefined", () => {
    const base = Date.UTC(2024, 0, 2, 3, 4, 5, 6);

    const localSeconds = new Date(base);
    const localMilliseconds = localSeconds.getMilliseconds();
    expect(localSeconds.setSeconds(10)).toBe(localSeconds.getTime());
    expect(localSeconds.getSeconds()).toBe(10);
    expect(localSeconds.getMilliseconds()).toBe(localMilliseconds);

    const explicitLocalSeconds = new Date(base);
    expect(Number.isNaN(explicitLocalSeconds.setSeconds(10, undefined))).toBe(true);
    expect(Number.isNaN(explicitLocalSeconds.getTime())).toBe(true);

    const utcSeconds = new Date(base);
    expect(utcSeconds.setUTCSeconds(10)).toBe(utcSeconds.getTime());
    expect(utcSeconds.getUTCSeconds()).toBe(10);
    expect(utcSeconds.getUTCMilliseconds()).toBe(6);

    const explicitUTCSeconds = new Date(base);
    expect(Number.isNaN(explicitUTCSeconds.setUTCSeconds(10, undefined))).toBe(true);

    const utcMinutes = new Date(base);
    expect(utcMinutes.setUTCMinutes(20)).toBe(utcMinutes.getTime());
    expect(utcMinutes.getUTCMinutes()).toBe(20);
    expect(utcMinutes.getUTCSeconds()).toBe(5);
    expect(utcMinutes.getUTCMilliseconds()).toBe(6);

    const explicitUTCMinutes = new Date(base);
    expect(Number.isNaN(explicitUTCMinutes.setUTCMinutes(20, undefined))).toBe(true);

    const utcHours = new Date(base);
    expect(utcHours.setUTCHours(9)).toBe(utcHours.getTime());
    expect(utcHours.getUTCHours()).toBe(9);
    expect(utcHours.getUTCMinutes()).toBe(4);
    expect(utcHours.getUTCSeconds()).toBe(5);
    expect(utcHours.getUTCMilliseconds()).toBe(6);

    const explicitUTCHours = new Date(base);
    expect(Number.isNaN(explicitUTCHours.setUTCHours(9, undefined))).toBe(true);

    const utcMonth = new Date(base);
    expect(utcMonth.setUTCMonth(6)).toBe(utcMonth.getTime());
    expect(utcMonth.getUTCMonth()).toBe(6);
    expect(utcMonth.getUTCDate()).toBe(2);

    const explicitUTCMonth = new Date(base);
    expect(Number.isNaN(explicitUTCMonth.setUTCMonth(6, undefined))).toBe(true);

    const utcYear = new Date(base);
    expect(utcYear.setUTCFullYear(2025)).toBe(utcYear.getTime());
    expect(utcYear.getUTCFullYear()).toBe(2025);
    expect(utcYear.getUTCMonth()).toBe(0);
    expect(utcYear.getUTCDate()).toBe(2);

    const explicitUTCYear = new Date(base);
    expect(Number.isNaN(explicitUTCYear.setUTCFullYear(2025, undefined))).toBe(true);
  });

  test("prototype methods reject nullish receivers", () => {
    const methods = [
      "getTime",
      "valueOf",
      "getFullYear",
      "getMonth",
      "getDate",
      "getDay",
      "getHours",
      "getMinutes",
      "getSeconds",
      "getMilliseconds",
      "getUTCFullYear",
      "getUTCMonth",
      "getUTCDate",
      "getUTCDay",
      "getUTCHours",
      "getUTCMinutes",
      "getUTCSeconds",
      "getUTCMilliseconds",
      "setTime",
      "setMilliseconds",
      "setUTCMilliseconds",
      "setSeconds",
      "setUTCSeconds",
      "setMinutes",
      "setUTCMinutes",
      "setHours",
      "setUTCHours",
      "setDate",
      "setUTCDate",
      "setMonth",
      "setUTCMonth",
      "setFullYear",
      "setUTCFullYear",
      "getTimezoneOffset",
      "toISOString",
      "toJSON",
      "toString",
      "toUTCString",
      "toGMTString",
      "toLocaleString",
      "toLocaleDateString",
      "toLocaleTimeString",
    ];

    for (const method of methods) {
      const fn = Date.prototype[method];
      expect(() => fn.call(null)).toThrow(TypeError);
      expect(() => fn.call(undefined)).toThrow(TypeError);
      expect(() => fn()).toThrow(TypeError);
    }
  });

  test("slot-requiring methods reject a plain non-Date object receiver", () => {
    const slotMethods = [
      "getTime",
      "valueOf",
      "getFullYear",
      "getUTCFullYear",
      "getTimezoneOffset",
      "setTime",
      "setFullYear",
      "toISOString",
      "toString",
    ];

    for (const method of slotMethods) {
      const fn = Date.prototype[method];
      expect(() => fn.call({})).toThrow(TypeError);
      expect(() => fn.call(Object.create(Date.prototype))).toThrow(TypeError);
    }
  });

  test("constructor reads the time value from a Date argument and clips numbers", () => {
    const source = new Date(1718451045123);
    expect(new Date(source).getTime()).toBe(1718451045123);
    expect(new Date(2000).getTime()).toBe(2000);
    expect(Number.isNaN(new Date(NaN).getTime())).toBe(true);
    expect(Number.isNaN(new Date(NaN).getTimezoneOffset())).toBe(true);
  });

  test("toJSON remains generic for object receivers", () => {
    const receiver = {
      valueOf() { return 1; },
      toISOString() { return "generic"; },
    };

    expect(Date.prototype.toJSON.call(receiver)).toBe("generic");
  });

  test("toJSON only returns null for non-finite number primitives", () => {
    const stringPrimitiveReceiver = {
      valueOf() { return "not-a-number"; },
      toISOString() { return "generic"; },
    };
    const nonFiniteNumberReceiver = {
      valueOf() { return NaN; },
      toISOString() { return "unreachable"; },
    };

    expect(Date.prototype.toJSON.call(stringPrimitiveReceiver)).toBe("generic");
    expect(Date.prototype.toJSON.call(nonFiniteNumberReceiver)).toBe(null);
  });
});
