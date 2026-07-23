import {
  counter,
  resetCounter,
  setCounter,
  wordCount,
} from "./live-bindings-leaf.js";

export function twoHopWordCount(value) {
  return wordCount(value);
}

export const readCounter = () => counter;

export { counter as forwardedCounter, resetCounter, setCounter };
