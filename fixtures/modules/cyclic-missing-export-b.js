// Linking skips the missing-name check against cyclic-missing-export-a.js,
// which is still evaluating, so the read below is what rejects it.
import { thisExportDoesNotExist } from "./cyclic-missing-export-a.js";

export const value = thisExportDoesNotExist;
