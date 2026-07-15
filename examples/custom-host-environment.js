let epochNanosecondsValue = 1700000000000000000n;
let monotonicNanosecondsValue = 0n;
const randomStates = new Map();

export const epochNanoseconds = () => {
  const current = epochNanosecondsValue;
  epochNanosecondsValue += 1000000n;
  return current;
};

export const monotonicNanoseconds = () => {
  const current = monotonicNanosecondsValue;
  monotonicNanosecondsValue += 1000000n;
  return current;
};

export const timeZoneIdentifier = () => "Europe/London";

export const random = (streamId) => {
  const previous = randomStates.get(streamId) ?? (42n + streamId);
  const next = (previous * 1664525n + 1013904223n) % 4294967296n;
  randomStates.set(streamId, next);
  return Number(next) / 4294967296;
};
