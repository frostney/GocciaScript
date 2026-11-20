// A module whose top level declares a using and an await using resource.
export const log = [];

await using later = {
  async [Symbol.asyncDispose]() {
    await null;
    log.push("dispose async");
  },
};
using first = {
  value: "still bound",
  [Symbol.dispose]() {
    log.push("dispose sync");
  },
};

export const read = () => first.value;

log.push("body");
