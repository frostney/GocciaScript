// Runs before instantiation-cycle-a.js, which imports it. Every function
// declaration of that module already exists here (ES2026 §16.2.1.7.3.1
// InitializeEnvironment), though its body has not run yet.
import describeDefault, {
  asyncSeed,
  bump,
  readSeed,
  readSeedBinding,
  readSeedThroughNamespace,
  receiver,
  seedGenerator,
} from "./instantiation-cycle-a.js";

const errorName = (fn) => {
  try {
    fn();
    return "no error";
  } catch (error) {
    return error.name;
  }
};

export const earlyCalls = {
  readSeed: readSeed(),
  readSeedBinding: readSeedBinding(),
  readSeedThroughNamespace: readSeedThroughNamespace(),
  defaultName: describeDefault.name,
  defaultResult: describeDefault(),
  generator: [...seedGenerator()],
  receiver: receiver(),
  // `counter` is declared by the module's body, which has not run yet.
  bumpBeforeBody: errorName(bump),
};

export const earlyAsync = asyncSeed();

export const earlyReadSeed = readSeed;
