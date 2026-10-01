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

export const readLimitEarly = () => LIMIT;
const describeLimitRead = () => {
  try {
    return String(readLimitEarly());
  } catch (error) {
    return error.constructor.name;
  }
};
export const limitBeforeInitialization = describeLimitRead();
const LIMIT = 16;
export const limitAfterInitialization = describeLimitRead();

export const EXPORTED_LIMIT = 32;
export const HALF_LIMIT = EXPORTED_LIMIT / 2;
export const readExportedLimit = () => EXPORTED_LIMIT + HALF_LIMIT;

const settings = { scale: 3 };
export const readLimit = () => LIMIT;
export const readSettings = () => settings;
export const readImportedCounter = () => importedCounter;
export { bumpImportedCounter };
