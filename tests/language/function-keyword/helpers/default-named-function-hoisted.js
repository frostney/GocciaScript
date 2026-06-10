export const beforeType = typeof hoistedNamedDefault;
export const beforeCall = hoistedNamedDefault(6);

export default function hoistedNamedDefault(value) {
  return value + 1;
}

export const afterCall = hoistedNamedDefault(7);
