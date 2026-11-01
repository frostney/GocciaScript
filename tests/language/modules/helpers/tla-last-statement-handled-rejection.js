// A module with top-level await whose last statement leaves a handled,
// rejected promise. Its evaluation still succeeds.
export const handledValue = "loaded";
await 0;
const rejected = Promise.reject(new Error("handled"));
rejected.catch(() => {});
rejected;
