// Battery B — module-linking edges around the §2 TDZ fix.
import { viaB, fromA } from "./mods/circA.js";
import { viaA } from "./mods/circB.js";
import readSecret, { counter, bump } from "./mods/live.js";
import { Tagger, asyncReader, outer } from "./mods/klass.js";
import { reFromA, Tagger as ReTagger } from "./mods/rex.js";

describe("module linking edges", () => {
  test("circular imports: A -> B -> A both resolve", () => {
    expect(viaB()).toBe(101);
    expect(viaA()).toBe(12);
    expect(fromA()).toBe(10);
  });

  test("default-export function reads module const", () => {
    expect(readSecret()).toBe("s3cret");
  });

  test("live binding: importer sees exported let update", () => {
    const before = counter;
    bump();
    bump();
    expect(counter).toBe(before + 2);
  });

  test("class declaration: methods, private fields, static from module consts", () => {
    const t = new Tagger();
    expect(t.next()).toBe("id-1");
    expect(t.next()).toBe("id-2");
    expect(Tagger.max).toBe(3);
    expect(t.atLimit).toBe(false);
    t.next();
    expect(t.atLimit).toBe(true);
  });

  test("async function declaration reads module const", async () => {
    expect(await asyncReader()).toBe("id-async");
  });

  test("hoisted function calls file-local non-exported hoisted function", () => {
    expect(outer()).toBe(6);
  });

  test("named re-export and export * both link", () => {
    expect(reFromA()).toBe(10);
    const t = new ReTagger();
    expect(t.next()).toBe("id-1");
  });
});
