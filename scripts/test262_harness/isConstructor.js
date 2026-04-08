// test262 isConstructor.js -- GocciaScript-compatible reimplementation
// GocciaScript does not have Reflect.construct, so we approximate by
// attempting `new f()` and catching any TypeError.

const isConstructor = (f) => {
  if (typeof f !== "function") {
    return false;
  }
  try {
    new f();
    return true;
  } catch (e) {
    // TypeError from non-constructor, other errors from constructor body
    if (e instanceof TypeError && /is not a constructor/.test(e.message)) {
      return false;
    }
    // If it threw something else, it IS a constructor (the body just errored)
    return true;
  }
};
