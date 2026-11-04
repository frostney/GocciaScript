/*---
description: A generator's return() at a yield in a for statement whose head is a using declaration disposes the head's resources (ES2027 §27.8.3.4 GeneratorResumeAbrupt, §7.5.4 DisposeResources)
features: [explicit-resource-management, generators]
---*/

const resource = (name, log, throws = false) => ({
  [Symbol.dispose]() {
    log.push("dispose " + name);
    if (throws) {
      throw new Error(name);
    }
  },
});

describe("generator return() in a for statement with a using head", () => {
  test("disposes the head's resources in reverse order", () => {
    const log = [];
    const holder = {
      *run() {
        for (using a = resource("a", log), b = resource("b", log); ;) {
          yield 1;
        }
      },
    };
    const it = holder.run();
    it.next();
    expect(it.return(9)).toEqual({ value: 9, done: true });
    expect(log).toEqual(["dispose b", "dispose a"]);
  });

  test("a dispose error replaces the return completion", () => {
    const log = [];
    const holder = {
      *run() {
        for (using a = resource("a", log, true); ;) {
          yield 1;
        }
      },
    };
    const it = holder.run();
    it.next();
    let caught;
    try {
      it.return(9);
    } catch (e) {
      caught = e;
    }
    expect(caught instanceof SuppressedError).toBe(false);
    expect(caught.message).toBe("a");
    expect(log).toEqual(["dispose a"]);
  });
});
