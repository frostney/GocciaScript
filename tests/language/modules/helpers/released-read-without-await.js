// The value-read-drop shape of released-read-after-await.js with no await:
// a top-level statement of module code that awaits nothing still releases
// what it read.
const SIZE = 32 * 1024 * 1024;
const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};
const baseline = collect();
let buffer = new ArrayBuffer(SIZE);
const length = buffer.byteLength;
buffer = null;
let grown = 0;
try {
  grown = collect() - baseline;
} catch (error) {
  throw error;
}
export { SIZE, grown, length };
