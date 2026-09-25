// Vitest configuration for the differential suite lane.
//
// Vitest is the semantic oracle for goccia's testing API, so this config exists
// only to let the differential suites run unmodified: `globals: true` injects
// the same describe/test/expect/hook globals goccia and bun inject, so a suite
// file needs no runtime-specific imports.
//
// Which differential suites are actually handed to vitest is decided by the
// classification table in scripts/test-cli-differential.ts, which passes the
// eligible files as filters; `include` only has to be wide enough to match them.
export default {
  test: {
    globals: true,
    include: ["*.test.js", "*.test.ts"],
    // Differential suites observe hook and body execution through in-file
    // markers, never through stdout, but console interception would still
    // reorder output when a differential suite is debugged by hand.
    disableConsoleIntercept: true,
  },
};
