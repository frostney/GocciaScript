export enum EntrySelfReferenceEnum {
  Ready = "ready",
}

test("entry module registers enum exports for dynamic self-reference", async () => {
  const self = await import("./entry-self-reference-enum.js");
  expect(self.EntrySelfReferenceEnum.Ready).toBe("ready");
  expect(self.EntrySelfReferenceEnum[Symbol.toStringTag]).toBe("EntrySelfReferenceEnum");
});
