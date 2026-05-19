export default function namedDefaultFunction(value) {
  return value * 3;
};

export const localNamedDefaultFunctionType = typeof namedDefaultFunction;
export const localNamedDefaultFunctionResult = namedDefaultFunction(3);
