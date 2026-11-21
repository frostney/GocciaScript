// A top-level catch clause that reads the caught value through a module
// binding, then drops it.
const SIZE = 32 * 1024 * 1024;
const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};
const baseline = collect();
const fail = async () => {
  throw new ArrayBuffer(SIZE);
};
let caught = null;
try {
  await fail();
} catch (error) {
  caught = error;
}
const length = caught.byteLength;
caught = null;
let grown = 0;
try {
  grown = collect() - baseline;
} catch (error) {
  throw error;
}
export { SIZE, grown, length };
