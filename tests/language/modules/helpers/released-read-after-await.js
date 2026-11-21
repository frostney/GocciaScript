// Awaits a value, reads it through its module binding, drops it, then
// measures. The read copies the binding into a register of the module's
// frame. The measurement runs in a try block, whose registers start above
// that one, so the statements after the drop do not overwrite it.
const SIZE = 32 * 1024 * 1024;
const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};
const baseline = collect();
const make = async () => new ArrayBuffer(SIZE);
let buffer = await make();
const length = buffer.byteLength;
buffer = null;
let grown = 0;
try {
  grown = collect() - baseline;
} catch (error) {
  throw error;
}
export { SIZE, grown, length };
