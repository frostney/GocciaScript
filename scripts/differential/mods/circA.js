import { fromB } from "./circB.js";

const baseA = 10;

export function fromA() {
  return baseA;
}

export function viaB() {
  return fromB() + 1;
}
