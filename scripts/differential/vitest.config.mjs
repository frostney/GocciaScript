// Vitest configuration for the differential battery lane.
//
// Vitest is the semantic oracle for goccia's testing API, so this config exists
// only to let the batteries run unmodified: `globals: true` injects the same
// describe/test/expect/hook globals goccia and bun inject, so a battery file
// needs no runtime-specific imports.
//
// Which batteries are actually handed to vitest is decided by the
// classification table in scripts/test-cli-differential.ts, which passes the
// eligible files as filters; `include` only has to be wide enough to match them.
export default {
  test: {
    globals: true,
    include: ["*.test.js", "*.test.ts"],
    // Batteries observe hook and body execution through in-file markers, never
    // through stdout, but console interception would still reorder output when
    // a battery is debugged by hand.
    disableConsoleIntercept: true,
  },
};
