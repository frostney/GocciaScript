export function wordCount(value) {
  return value.split(" ").length;
}

export let counter = 0;

export const increment = () => {
  counter += 1;
};

export const setCounter = (value) => {
  counter = value;
};

export const resetCounter = () => {
  counter = 0;
};
