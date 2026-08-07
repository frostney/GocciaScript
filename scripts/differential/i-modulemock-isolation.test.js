import identify, { compute, label } from "./mods/mockable.js";
import { readLabel } from "./mods/mockable-consumer.js";

// The companion half of the per-file isolation check. `h-modulemock.test.js`
// mocks `./mods/mockable.js` with a factory; this file never mocks anything and
// must therefore see the real module.
//
// Under vitest the two files run in ONE `vitest run` invocation, so this is a
// genuine cross-file check of vitest's own per-file mock registry. Under goccia
// the differential harness spawns one process per battery file, so goccia's
// side is trivially isolated here — the load-bearing goccia isolation test is
// the pair under tests/language/modules/, where the whole directory runs in a
// single runner process with parallel worker threads and a leaking registry
// would actually surface.
describe("vi.mock does not leak across test files", () => {
  test("an unmocking file sees the real module", () => {
    expect(label).toBe("REAL_LABEL");
    expect(identify()).toBe("REAL_DEFAULT");
  });

  test("the real implementation runs, not a spy", () => {
    expect(compute(1, 2)).toBe(3);
    expect(compute.mock).toBe(undefined);
  });

  test("code under test also sees the real module", () => {
    expect(readLabel()).toBe("REAL_LABEL");
  });
});
