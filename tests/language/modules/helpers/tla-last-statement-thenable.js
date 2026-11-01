// A module with top-level await whose last statement leaves a thenable.
// Evaluation does not call its then method.
export let thenCalls = 0;
export const thenableValue = "loaded";
await 0;
({
  then() {
    thenCalls = thenCalls + 1;
  },
});
