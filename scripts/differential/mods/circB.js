import { fromA } from "./circA.js";

const baseB = 100;

export function fromB() {
  return baseB;
}

export function viaA() {
  return fromA() + 2;
}
