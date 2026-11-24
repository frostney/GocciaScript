// A bare awaited expression statement. An imported module has no completion
// value, so nothing keeps the awaited value once the statement ends. The
// statements after it are declarations, which would not replace a completion
// value.
const SIZE = 32 * 1024 * 1024;
const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};
const baseline = collect();
const make = async () => new ArrayBuffer(SIZE);
await make();
const grown = collect() - baseline;
export { SIZE, grown };
