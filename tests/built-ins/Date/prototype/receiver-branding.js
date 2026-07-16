/*---
description: Date prototype receiver branding
features: [Date]
---*/

test("slot-requiring methods reject non-Date receivers", () => {
  for (const method of [
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
    "getTimezoneOffset",
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
    "toISOString",
    "toString",
    "toUTCString",
    "toGMTString",
    "toLocaleString",
    "toLocaleDateString",
    "toLocaleTimeString",
  ]) {
    const functionValue = Date.prototype[method];
    expect(() => functionValue.call({})).toThrow(TypeError);
    expect(() => functionValue.call(Object.create(Date.prototype))).toThrow(TypeError);
  }
});

test("prototype methods reject nullish receivers", () => {
  for (const method of [
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
  ]) {
    const functionValue = Date.prototype[method];
    expect(() => functionValue.call(null)).toThrow(TypeError);
    expect(() => functionValue.call(undefined)).toThrow(TypeError);
  }
});
