// Battery G — the file-level (top-level) beforeAll cascade, which needs a file
// of its own: a failing top-level beforeAll skips every test in the file, so it
// cannot share one with battery F.
//
// Vitest reports the file as failed with every test skipped and none failed.
// The test bodies assert something false on purpose: a runtime that runs them
// instead of skipping them shows up as a failed test rather than as a skipped
// one, so the divergence is visible in the counts without needing a marker.

beforeAll(() => {
  throw new Error("file-level beforeAll boom");
});

describe("first suite under the file-level cascade", () => {
  test("skipped rather than run", () => {
    expect("this body").toBe("never reached");
  });
});

describe("second suite under the file-level cascade", () => {
  test("skipped rather than run, in a later suite", () => {
    expect("this body").toBe("never reached");
  });
});
