import {
  bumpImportedCounter,
  importedCounter,
} from "./top-level-binding-read-dependency.js";

export const readLate = () => late;

const describeRead = () => {
  try {
    return String(readLate());
  } catch (error) {
    return error.constructor.name;
  }
};

export const readBeforeInitialization = describeRead();
let late = "first";
export const readAfterInitialization = describeRead();

export const setLate = (value) => {
  late = value;
};

const LIMIT = 16;
const settings = { scale: 3 };
export const readLimit = () => LIMIT;
export const readSettings = () => settings;
export const readImportedCounter = () => importedCounter;
export { bumpImportedCounter };
