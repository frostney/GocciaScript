import { duplicate } from "./import-access-ambiguous-barrel.js";

globalThis.__importAccessEvaluationCount += 100;
export const observed = duplicate;
