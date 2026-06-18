export let resolveDone;
export let rejectDone;
export const done = new Promise((resolve, reject) => {
  resolveDone = resolve;
  rejectDone = reject;
});

export let resolveFirst;
export const first = new Promise((resolve) => {
  resolveFirst = resolve;
});

export let resolveSecond;
export const second = new Promise((resolve) => {
  resolveSecond = resolve;
});

export let resolveThird;
export const third = new Promise((resolve) => {
  resolveThird = resolve;
});
