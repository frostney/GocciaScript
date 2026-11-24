// The value-read-drop shape with the read in a destructuring declaration.
const SIZE = 32 * 1024 * 1024;
const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};
const baseline = collect();
const make = async () => new ArrayBuffer(SIZE);
let buffer = await make();
const { byteLength: length } = buffer;
buffer = null;
let grown = 0;
try {
  grown = collect() - baseline;
} catch (error) {
  throw error;
}
export { SIZE, grown, length };
