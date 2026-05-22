import { destructuredHit, hit } from "./helpers/live-top-level-let.js";

describe("module live bindings", () => {
  test("exports reassigned top-level lexical bindings", () => {
    expect(hit).toBe(1);
    expect(destructuredHit).toBe(1);
  });
});
