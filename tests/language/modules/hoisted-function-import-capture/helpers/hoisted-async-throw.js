export class Boom extends Error {
  constructor(message) {
    super(message);
    this.name = "Boom";
  }
}

export const boom = new Boom("boom");

const raise = () => {
  throw boom;
};

export async function throwsAfterAwait() {
  await Promise.resolve(1);
  raise();
}

export async function catchesAfterAwait() {
  await Promise.resolve(1);
  try {
    raise();
    return null;
  } catch (error) {
    return error;
  }
}

export async function throwsBeforeAwait() {
  raise();
  await Promise.resolve(1);
}
