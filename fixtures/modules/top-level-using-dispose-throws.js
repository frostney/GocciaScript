// A module whose top-level resource throws when it is disposed.
globalThis.topLevelUsingDisposeThrowsLog = [];

using resource = {
  [Symbol.dispose]() {
    globalThis.topLevelUsingDisposeThrowsLog.push("dispose");
    throw new Error("dispose");
  },
};

globalThis.topLevelUsingDisposeThrowsLog.push("body");
