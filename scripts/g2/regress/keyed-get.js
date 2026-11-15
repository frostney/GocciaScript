// o[name] reads with string keys at hot sites see what o.name would: many shapes, keys added later on the
// object or a prototype, getters, a proxy on the chain, length and prototype by key, a String object, mapped
// arguments, and canonical numeric keys ("NaN", "-0", "1.5") past a typed array on the chain
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function rd(o, k) { return o[k]; }
const keys = ['type', 'name', 'value', 'Identifier', 'NumericLiteral', 'missing', 'length', 'prototype', '0', 'NaN'];
const objs = [];
for (let i = 0; i < 10; i++) { const o = { type: 't' + i }; for (let j = 0; j < i; j++) o['p' + j] = j; if (i & 1) o.name = 'n' + i; objs.push(o); }
for (let r = 0; r < 200; r++) for (const o of objs) for (const k of keys) rd(o, k);
check('own', rd(objs[3], 'type'), 't3');
check('absent', rd(objs[2], 'name'), undefined);
objs[2].name = 'late';
check('added later', rd(objs[2], 'name'), 'late');
Object.prototype.missing = 'proto';
check('added to Object.prototype', rd(objs[4], 'missing'), 'proto');
delete Object.prototype.missing;
check('deleted again', rd(objs[4], 'missing'), undefined);
const withGetter = Object.create({ get value() { return this.type + '!'; } });
withGetter.type = 'g';
for (let r = 0; r < 300; r++) rd(withGetter, 'value');
check('getter with receiver', rd(withGetter, 'value'), 'g!');
const prox = Object.create(new Proxy({}, { get(t, k, rcv) { return k === 'Identifier' ? 'trap' : undefined; } }));
for (let r = 0; r < 300; r++) rd(prox, 'Identifier');
check('proxy on the chain', rd(prox, 'Identifier'), 'trap');
check('array length by key', rd([1, 2, 3], 'length'), 3);
function F() {}
check('function prototype by key', rd(F, 'prototype'), F.prototype);
check('String object index by key', rd(new String('abc'), '0'), 'a');
check('String object length by key', rd(new String('abcd'), 'length'), 4);
function args(a) { arguments[0] = 'changed'; return rd(arguments, '0'); }
check('mapped arguments', args('x'), 'changed');
const ta = new Float64Array(4);
const overTA = Object.create(ta);
Object.prototype.NaN = 'op'; Object.prototype['-0'] = 'op'; Object.prototype['1.5'] = 'op';
for (let r = 0; r < 300; r++) { rd(overTA, 'NaN'); rd({}, 'NaN'); }
check('NaN past a typed array', rd(overTA, 'NaN'), undefined);
check('-0 past a typed array', rd(overTA, '-0'), undefined);
check('1.5 past a typed array', rd(overTA, '1.5'), undefined);
check('NaN on a plain object', rd({}, 'NaN'), 'op');
delete Object.prototype.NaN; delete Object.prototype['-0']; delete Object.prototype['1.5'];
check('index key on an array', rd(['a', 'b'], '1'), 'b');
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
