// Evaluated after instantiation-cycle-b.js, which imports this module back
// and calls every function below before this body has run.
import { seed, seedValue } from "./instantiation-seed.js";
import * as seedNamespace from "./instantiation-seed.js";
import { earlyCalls, earlyAsync, earlyReadSeed } from "./instantiation-cycle-b.js";

let counter = 0;

export function readSeed() {
  return seed();
}

export function readSeedBinding() {
  return seedValue;
}

export function readSeedThroughNamespace() {
  return seedNamespace.seed();
}

export function bump() {
  counter += 1;
  return counter;
}

export function readCounter() {
  return counter;
}

export default function () {
  return "default:" + seed();
}

export async function asyncSeed() {
  await Promise.resolve();
  return "async:" + seed();
}

export function* seedGenerator() {
  yield seed();
  yield readInternal();
}

export function receiver() {
  return this;
}

export function writeFrozen() {
  const frozen = Object.freeze({ value: 1 });
  frozen.value = 2;
  return frozen.value;
}

function readInternal() {
  return "internal:" + seed();
}

export { earlyCalls, earlyAsync, earlyReadSeed };
