// Object.freeze, seal and preventExtensions return their argument after redefining its properties, which
// allocates: a collection in the middle moves a young object, and the native must return where it went (it
// returned the old address, so the frozen object read back with no properties)
function Node(x, y) { this.x = x; this.y = y; }
let bad = 0;
const kinds = [o => Object.freeze(o), o => Object.seal(o), o => Object.preventExtensions(o)];
for (let k = 0; k < 30000; k++) {
  const o = kinds[k % 3](k & 1 ? new Node(1, 2) : { x: 1, y: 2 });
  for (let j = 0; j < 4; j++) ({ j });
  if (o.x !== 1 || o.y !== 2 || Object.keys(o).join() !== 'x,y') bad++;
}
console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
