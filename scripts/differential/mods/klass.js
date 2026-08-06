const PREFIX = "id-";
const LIMIT = 3;

export class Tagger {
  static max = LIMIT;

  #n = 0;

  next() {
    this.#n += 1;
    return PREFIX + this.#n;
  }

  get atLimit() {
    return this.#n >= LIMIT;
  }
}

export async function asyncReader() {
  await Promise.resolve();
  return PREFIX + "async";
}

export function outer() {
  return inner() * 2;
}

function inner() {
  return LIMIT;
}
