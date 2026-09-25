// Real module that the vi.mock tests replace with a factory mock.
// Imported through several spellings on purpose, so the tests can prove that
// mocks are keyed by resolved address rather than by the written specifier.
export const label = "REAL";
export const add = (a, b) => a + b;
export default { kind: "real" };
