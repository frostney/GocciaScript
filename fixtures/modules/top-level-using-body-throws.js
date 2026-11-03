// A module whose body throws after a top-level using declaration ran.
globalThis.topLevelUsingBodyThrowsLog = [];

using resource = {
  [Symbol.dispose]() {
    globalThis.topLevelUsingBodyThrowsLog.push("dispose");
    throw new Error("dispose");
  },
};

globalThis.topLevelUsingBodyThrowsLog.push("body");
throw new Error("body");
