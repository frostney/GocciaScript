export default function* followingNamedDefaultGenerator() {
  yield "named generator";
} if (true) {}

export const localFollowingNamedDefaultGeneratorType = typeof followingNamedDefaultGenerator;
export const defaultNamedGeneratorFollowingStatementHit = 1;
