import { entryFunction } from "../../tests/language/source-type-detection/function-hoisting/entry-function-cycle.js";

export const peerValue = "y" + entryFunction();
