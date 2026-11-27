/*---
description: Prelude built-ins build their descriptors, records and lists so that a field or index a script adds to Object.prototype or Array.prototype is never read or written
features: [Intl, Temporal, Response, Uint8Array, structuredClone, iterator-helpers, explicit-resource-management]
---*/

import * as CSV from "goccia:csv";
import * as JSON5 from "goccia:json5";
import { withPrototypeAdditions } from "./helpers/tripwire.js";

const clean = (fn) => {
  const r = withPrototypeAdditions(fn);
  if (r.threw) throw r.error;
  expect(r.hits).toEqual([]);
  return r.value;
};

test("descriptor fields on Object.prototype", () => {
  expect(clean(() => new Intl.Locale("en-US").baseName)).toBe("en-US");
  expect(clean(() => new Response("hi").status)).toBe(200);
  expect(clean(() => new Headers({ a: "1" }).get("a"))).toBe("1");
  expect(clean(() => structuredClone({ a: [1] }))).toEqual({ a: [1] });
});

test("record fields on Object.prototype", () => {
  // the property bags have no prototype: the specification reads their fields with Get, which would reach
  // Object.prototype for a field the bag lacks; only the built-ins' own records are under test
  expect(clean(() => new Temporal.Duration(0, 0, 0, 0, 1).toString())).toBe("PT1H");
  expect(clean(() => Temporal.PlainDate.from("2020-01-15").with({ __proto__: null, day: 1 }).toString())).toBe("2020-01-01");
  expect(clean(() => Temporal.Duration.from("PT1.5S").with({ __proto__: null, minutes: 2 }).toString())).toBe("PT2M1.5S");
  expect(clean(() => CSV.parse("a,b\n1,2"))).toEqual([{ a: "1", b: "2" }]);
  const time = new Temporal.PlainTime(1, 2);
  expect(clean(() => time.with({ __proto__: null, hour: 3 }).toString())).toBe("03:02:00");
});

test("options, results and lookup tables", () => {
  expect(clean(() => atob("YQ=="))).toBe("a");
  expect(clean(() => new Uint8Array(1).setFromHex("ff"))).toEqual({ read: 2, written: 1 });
  expect(clean(() => JSON5.stringify("ab"))).toBe("'ab'");
});

test("the helpers' own lists", () => {
  expect(clean(() => [1, 2][Symbol.iterator]().toArray())).toEqual([1, 2]);
  expect(clean(() => [1, 2, 3].values().chunks(2).toArray())).toEqual([[1, 2], [3]]);
  const stack = new DisposableStack();
  expect(clean(() => { stack.defer(() => {}); stack.dispose(); return stack.disposed; })).toBe(true);
});

test("array indices on Array.prototype and Object.prototype", () => {
  // the decoding table has no prototype: a code unit outside the alphabet is refused, not decoded by an inherited
  // entry
  const r = withPrototypeAdditions(() => Uint8Array.fromBase64("!!!!"));
  expect(r.threw).toBe(true);
  expect(r.error instanceof SyntaxError).toBe(true);
  expect(r.hits).toEqual([]);
  expect(Array.from(clean(() => Uint8Array.fromBase64("AQID")))).toEqual([1, 2, 3]);
});
