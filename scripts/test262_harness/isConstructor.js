// test262 isConstructor.js -- GocciaScript-compatible reimplementation
// Checks if a value is a constructor by attempting new.

const isConstructor = (f) => {
  if (typeof f !== "function") {
    return false;
  }
  try {
    Reflect.construct(Object, [], f);
    return true;
  } catch (e) {
    return false;
  }
};
