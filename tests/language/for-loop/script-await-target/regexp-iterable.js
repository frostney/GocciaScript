/*---
description: Script await identifiers remain assignment targets before regexp iterables
features: [traditional-for-loop]
---*/

const holder = {};
const await = () => holder;
for (await[0] of /a>/.exec("a>")) {}
for (await().called of /a</.exec("a<")) {}
for (await`tag`.tagged of /a=/.exec("a=")) {}
for (await`tag${1}`.substituted of /a+/.exec("aaa")) {}

describe("top-level Script await assignment targets", () => {
  test("computed member", () => {
    expect(await[0]).toBe("a>");
  });

  test("call followed by a member", () => {
    expect(holder.called).toBe("a<");
  });

  test("tagged template followed by a member", () => {
    expect(holder.tagged).toBe("a=");
  });

  test("template substitutions followed by a member", () => {
    expect(holder.substituted).toBe("aaa");
  });
});
