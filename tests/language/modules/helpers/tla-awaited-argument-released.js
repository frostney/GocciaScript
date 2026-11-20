const SIZE = 32 * 1024 * 1024;

const measure = (a, b, buffer) => buffer.byteLength;
const make = async () => new ArrayBuffer(SIZE);

Goccia.gc();
const baseline = Goccia.gc.bytesAllocated;

// The container Promise.all resolved with stays in a register above the live
// bindings unless the statement clears it. The call has completed when the
// module suspends on the next await, and nothing reads the buffer again.
measure(1, 2, (await Promise.all([make()]))[0]);
await null;
await null;
Goccia.gc();

export const grown = Goccia.gc.bytesAllocated - baseline;
export const size = SIZE;
