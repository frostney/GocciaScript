// Heap snapshot workload: canvas-free image and text processing that
// suspends.
//
// Generates a 192x192 grayscale image in a Uint8Array, blurs it with a 3x3
// box filter, computes Sobel edge magnitudes into a Float32Array and a
// histogram into a Uint32Array; tokenizes a generated corpus and counts word
// frequencies in a Map. Then it waits on the host for the edge threshold.
// While it waits, the pixel buffers, the histogram and the frequency table
// are live.

const W = 192;
const H = 192;
const pixels = new Uint8Array(W * H);
const rows = Array.from({ length: H }, (_, y) => y);
const cols = Array.from({ length: W }, (_, x) => x);
for (const y of rows) for (const x of cols) {
  const cx = x - W / 2;
  const cy = y - H / 2;
  pixels[y * W + x] = (Math.sqrt(cx * cx + cy * cy) < 60 ? 200 : 40) + ((x * 7 + y * 13) % 23);
}

const blurred = new Uint8Array(W * H);
for (const y of rows) for (const x of cols) {
  let sum = 0;
  let n = 0;
  for (const dy of [-1, 0, 1]) for (const dx of [-1, 0, 1]) {
    const yy = y + dy;
    const xx = x + dx;
    if (yy >= 0 && yy < H && xx >= 0 && xx < W) { sum += pixels[yy * W + xx]; n += 1; }
  }
  blurred[y * W + x] = Math.round(sum / n);
}

const edges = new Float32Array(W * H);
const at = (x, y) => blurred[Math.min(H - 1, Math.max(0, y)) * W + Math.min(W - 1, Math.max(0, x))];
for (const y of rows) for (const x of cols) {
  const gx = -at(x - 1, y - 1) - 2 * at(x - 1, y) - at(x - 1, y + 1) + at(x + 1, y - 1) + 2 * at(x + 1, y) + at(x + 1, y + 1);
  const gy = -at(x - 1, y - 1) - 2 * at(x, y - 1) - at(x + 1, y - 1) + at(x - 1, y + 1) + 2 * at(x, y + 1) + at(x + 1, y + 1);
  edges[y * W + x] = Math.sqrt(gx * gx + gy * gy);
}

const histogram = new Uint32Array(64);
for (const v of blurred) histogram[v >> 2] += 1;

const LEXICON = ["the", "quick", "engine", "pauses", "while", "waiting", "for", "an",
  "answer", "from", "its", "host", "and", "resumes", "later", "on", "another", "machine",
  "with", "every", "closure", "intact"];
const corpus = Array.from({ length: 3000 }, (_, i) =>
  Array.from({ length: 8 + (i % 9) }, (_, j) => LEXICON[(i * 3 + j * j) % LEXICON.length]).join(" ")).join(". ");
const frequencies = new Map();
for (const word of corpus.toLowerCase().split(/[^a-z]+/)) {
  if (word) frequencies.set(word, (frequencies.get(word) ?? 0) + 1);
}

print("image", pixels.length, blurred.length, edges.length, "words", frequencies.size);

const finish = async () => {
  const threshold = await hostCall("edgeThreshold", 0) * 100;
  let edgePixels = 0;
  let checksum = 0;
  for (const v of edges) {
    if (v > threshold) edgePixels += 1;
  }
  for (const [i, v] of blurred.entries()) checksum = (checksum + v * (i % 251)) % 1000000007;
  const top = [...frequencies.entries()].sort((a, b) => b[1] - a[1] || (a[0] < b[0] ? -1 : 1)).slice(0, 5);
  return { edgePixels, checksum, peak: histogram.indexOf(Math.max(...histogram)), top };
};

finish().then((r) => {
  print("edges", r.edgePixels, "checksum", r.checksum, "peak", r.peak);
  print("top", r.top.map(([w, n]) => `${w}:${n}`).join(" "));
});
