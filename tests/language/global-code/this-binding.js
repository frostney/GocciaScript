/*---
description: >
  At the top level of a script (non-module entry point), `this` resolves to
  the global object via §9.4.3 ResolveThisBinding ->
  GlobalEnvironmentRecord.GetThisBinding (§9.1.1.4.4), whose [[ThisValue]]
  is the global object set up in §9.1.2.5 NewGlobalEnvironment.
---*/

let __gocciaTopLevelLexicalCounter = 0;
const __gocciaIncrementTopLevelLexicalCounter = () => {
  __gocciaTopLevelLexicalCounter++;
  if (__gocciaTopLevelLexicalCounter < 5) {
    __gocciaIncrementTopLevelLexicalCounter();
  }
};
__gocciaIncrementTopLevelLexicalCounter();
const __gocciaObservedTopLevelLexicalCounter = __gocciaTopLevelLexicalCounter;

test("script-level this is globalThis", () => {
  expect(this).toBe(globalThis);
});

test("script-level this is an object", () => {
  expect(typeof this).toBe("object");
  expect(this === undefined).toBe(false);
});

test("script-level this exposes globals", () => {
  expect(this.globalThis).toBe(globalThis);
});

test("script-level lexical closure writes are visible to later top-level reads", () => {
  expect(__gocciaObservedTopLevelLexicalCounter).toBe(5);
});

test("script-level global object properties are visible as identifiers", () => {
  const hadPrevious = "__gocciaGlobalObjectBinding" in globalThis;
  const previous = globalThis.__gocciaGlobalObjectBinding;
  try {
    globalThis.__gocciaGlobalObjectBinding = 41;
    expect(__gocciaGlobalObjectBinding).toBe(41);
  } finally {
    if (hadPrevious) {
      globalThis.__gocciaGlobalObjectBinding = previous;
    } else {
      delete globalThis.__gocciaGlobalObjectBinding;
    }
  }
});
