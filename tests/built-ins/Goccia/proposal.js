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
    expect(keys.includes("stage-3")).toBe(true);
  });

  test("stage keys follow the stage-N format", () => {
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

  test("proposal is read-only", () => {
    const original = Goccia.proposal;
    expect(() => { Goccia.proposal = "overwritten"; }).toThrow(TypeError);
    expect(Goccia.proposal).toBe(original);
  });
});
