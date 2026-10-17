import { entryValue } from "../../tests/language/modules/entry-tla-cycle.js";

const readEntry = () => {
  try {
    return entryValue;
  } catch (error) {
    return error.constructor.name;
  }
};

export const beforeAwait = readEntry();
export let afterAwait = "not resumed";

await Promise.resolve();

afterAwait = readEntry();
