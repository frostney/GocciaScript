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
