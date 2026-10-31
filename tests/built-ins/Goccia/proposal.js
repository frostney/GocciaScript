/*---
description: Goccia.proposal exposes implemented TC39 proposals keyed by stage
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.proposal", () => {
  test("proposal is an object", () => {
    expect(typeof Goccia.proposal).toBe("object");
    expect(Goccia.proposal !== null).toBe(true);
  });

  test("proposal has stage keys", () => {
    const keys = Object.keys(Goccia.proposal);
    expect(keys.length > 0).toBe(true);
    expect(keys.includes("stage-4")).toBe(true);
  });

  test("stage keys use the stage- prefix", () => {
    const keys = Object.keys(Goccia.proposal);
    keys.forEach((key) => {
      expect(key.startsWith("stage-")).toBe(true);
    });
  });

  test("each stage maps to an array of proposal entries", () => {
    const keys = Object.keys(Goccia.proposal);
    keys.forEach((stage) => {
      const proposals = Goccia.proposal[stage];
      expect(Array.isArray(proposals)).toBe(true);
      expect(proposals.length > 0).toBe(true);
      proposals.forEach((proposal) => {
        expect(typeof proposal.name).toBe("string");
        expect(typeof proposal.link).toBe("string");
        expect(proposal.link.startsWith("https://")).toBe(true);
      });
    });
  });

  test("stage 4 includes Iterator Includes", () => {
    const proposal = Goccia.proposal["stage-4"].find(
      (entry) => entry.name === "Iterator Includes",
    );

    expect(proposal.link).toBe(
      "https://github.com/tc39/proposal-iterator-includes",
    );
  });

  test("a stage with no listed proposal has no key", () => {
    expect(Object.keys(Goccia.proposal).includes("stage-3")).toBe(false);
    expect(Goccia.proposal["stage-3"]).toBeUndefined();
  });

  test("proposal is read-only", () => {
    const original = Goccia.proposal;
    expect(() => { Goccia.proposal = "overwritten"; }).toThrow(TypeError);
    expect(Goccia.proposal).toBe(original);
  });
});
