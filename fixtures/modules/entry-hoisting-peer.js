import entryDefault, { entryFunction, entryLet, entryVar } from "../../tests/language/modules/entry-cycle-hoisting/entry-initialized-exports.js";

const read = (fn) => {
  try {
    return String(fn());
  } catch (error) {
    return error.constructor.name;
  }
};

export const readsAtPeerEvaluation = {
  varValue: read(() => entryVar),
  functionResult: read(() => entryFunction()),
  defaultResult: read(() => entryDefault()),
  letValue: read(() => entryLet),
};
