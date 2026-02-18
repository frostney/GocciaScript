import { innerValue, innerGreet } from "./nested-inner.js";

export const middleValue = innerValue + 1;
export const middleGreet = (name) => innerGreet(name) + "!";
