// A class expression inside a static field initializer: the initializer is a
// function body of its own, so `yield` in the nested class's computed key is a
// reserved word, not a YieldExpression of the enclosing generator (tc39/ecma262#3333;
// V8, SpiderMonkey and JavaScriptCore reject it).
export const make = {
  *values() {
    return class {
      static keyed = class {
        static [yield "key"] = 1;
      };
    };
  },
};
