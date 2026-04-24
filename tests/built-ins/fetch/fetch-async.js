/*---
description: fetch returns a pending promise before network completion
features: [fetch, Promise]
---*/

describe("fetch async behavior", () => {
  test("Promise.race observes fetch as pending before network completion", async () => {
    const winner = await Promise.race([
      fetch("http://0.0.0.0:1/").then(
        () => "fetch-fulfilled",
        () => "fetch-rejected"
      ),
      Promise.resolve("microtask"),
    ]);

    expect(winner).toBe("microtask");
  });

  test("fetch rejection handler runs after already queued microtasks", async () => {
    const events = [];
    const request = fetch("http://0.0.0.0:1/").catch(() => {
      events.push("fetch");
    });

    Promise.resolve().then(() => {
      events.push("microtask");
    });

    await Promise.resolve();
    expect(events).toEqual(["microtask"]);

    await request;
    expect(events).toEqual(["microtask", "fetch"]);
  });
});
