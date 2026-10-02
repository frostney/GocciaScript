// Functions for error-stack.js that live in another module than their caller.
// Each performs one operation on request, then returns an error that a nested
// call caught, so the error's stack lists the frame the operation ran in. The
// error is bound before it is returned: a call in tail position would replace
// that frame.
const holder = { missing: null };

const failInNestedCall = () => {
  const fail = () => {
    try {
      holder.missing.x;
    } catch (e) {
      return e;
    }
  };
  const error = fail();
  return error;
};

const abs = Math.abs;

export const importedAfterFunctionCall = (perform) => {
  if (perform) abs(-1);
  const error = failInNestedCall();
  return error;
};

export const importedAfterMethodCall = (perform) => {
  if (perform) Math.max(1, 2);
  const error = failInNestedCall();
  return error;
};

export const importedAfterConstruction = (perform) => {
  if (perform) new Map();
  const error = failInNestedCall();
  return error;
};
