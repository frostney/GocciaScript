/*---
description: Goccia.commit exposes the short git commit hash
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.commit", () => {
  test("commit is a string", () => {
    expect(typeof Goccia.commit).toBe("string");
  });

  test("commit is a short hex hash", () => {
    expect(Goccia.commit.length > 0).toBe(true);
    expect(Goccia.commit.length <= 12).toBe(true);
  });
});
