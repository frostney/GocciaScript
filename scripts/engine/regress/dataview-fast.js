// DataView gets and sets on their fast path (an int offset, a Number, a fixed buffer): every non-BigInt kind, little-
// and big-endian, unaligned offsets, signed extremes, Uint32 past 2^31, Float32 rounding, NaN and -0, and bytes
// written by one kind read back through others, in compiled loops
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

const buf = new ArrayBuffer(64);
const dv = new DataView(buf, 3);   // an unaligned view
const u8 = new Uint8Array(buf);
const kinds = [
  ['Int8', [-128, 127, -1, 0]], ['Uint8', [0, 255, 128]], ['Int16', [-32768, 32767, -2, 258]],
  ['Uint16', [0, 65535, 258]], ['Int32', [-2147483648, 2147483647, -3, 16909060]],
  ['Uint32', [0, 4294967295, 2147483648, 16909060]], ['Float32', [1.5, -0, 3.1415927410125732, Infinity, 16777217]],
  ['Float64', [1.5, -0, Math.PI, -1e300, 5e-324]],
];
function roundTrip(kind, vals, n) {
  const get = dv['get' + kind].bind(dv), set = dv['set' + kind].bind(dv);
  let out = '';
  for (let r = 0; r < n; r++)
    for (let i = 0; i < vals.length; i++)
      for (const little of [true, false, undefined]) {
        const off = (i * 5 + r) % 40;
        set(off, vals[i], little);
        out = String(Object.is(get(off, little), kind === 'Float32' ? Math.fround(vals[i]) : vals[i]));
        if (out !== 'true') return kind + ' ' + vals[i] + ' ' + little + ' ' + get(off, little);
      }
  return 'ok';
}
for (const [k, vals] of kinds) check(k, roundTrip(k, vals, 300), 'ok');

// byte order checked against the raw bytes, and one kind's bytes through others
function bytesAt(off, n) { return Array.from(u8.slice(3 + off, 3 + off + n)).join(','); }
dv.setUint32(1, 0x01020304); check('big-endian bytes', bytesAt(1, 4), '1,2,3,4');
dv.setUint32(1, 0x01020304, true); check('little-endian bytes', bytesAt(1, 4), '4,3,2,1');
dv.setFloat64(9, 1, false); check('float64 big', bytesAt(9, 8), '63,240,0,0,0,0,0,0');
check('float64 as two uint32', dv.getUint32(9) + ':' + dv.getUint32(13), '1072693248:0');
dv.setInt16(20, -2, true); check('int16 as uint16', dv.getUint16(20, true), 65534);
dv.setFloat32(24, NaN); check('NaN', Number.isNaN(dv.getFloat32(24)), true);
function sumLoop(n) { let s = 0; for (let i = 0; i < n; i++) { dv.setUint8(i % 50, i & 255); s += dv.getUint8(i % 50) + dv.getInt8(i % 50); } return s; }
let want = 0; for (let i = 0; i < 20000; i++) { const b = i & 255; want += b + (b > 127 ? b - 256 : b); }
check('uint8 loop', sumLoop(20000), want);
// out of range and odd arguments still throw or convert as before
check('out of range', (() => { try { dv.getFloat64(57); return 'none'; } catch (e) { return e.constructor.name; } })(), 'RangeError');
check('string offset', (dv.setUint8('2', 7), dv.getUint8(2)), 7);
check('little as 1', (dv.setUint16(30, 0x0102, 1), dv.getUint16(30, true)), 0x0102);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
