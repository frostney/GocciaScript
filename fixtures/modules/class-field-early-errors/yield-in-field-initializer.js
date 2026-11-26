// `yield` in a field initializer inside a generator (tc39/ecma262#3333).
export const make = {
  *values() {
    return class {
      field = yield 42;
    };
  },
};
