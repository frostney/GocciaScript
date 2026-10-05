import entryDefault, { entryCounter, entryValue } from "../../tests/language/modules/entry-import-cycle.js";

globalThis.entryCycleOrder = [...(globalThis.entryCycleOrder ?? []), "helper"];

const errorName = (read) => {
  try {
    read();
    return "no error";
  } catch (error) {
    return error.constructor.name;
  }
};

export const entryReadsAtHelperEvaluation = {
  value: errorName(() => entryValue),
  counter: errorName(() => entryCounter),
  default: errorName(() => entryDefault),
};

export const readEntryNow = () => ({
  value: entryValue,
  counter: entryCounter,
  default: entryDefault,
});
