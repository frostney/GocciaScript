import { state, readState } from "./helpers/heap-snapshot-state.js";

test("observed objects retain shared module state at file idle", () => {
  const first = { state, readState };
  const second = { state, first };
  expect(first).toBe(first);
  expect(second).toBe(second);
  state.value = 42;
  expect(readState()).toBe(42);
});
