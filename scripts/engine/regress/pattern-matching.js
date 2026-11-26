// pattern matching (is, match, if / for-of / catch patterns) lowered to control flow and opIntrinsic helpers:
// bindings, or-alternatives, closures on the failing path, rest, extractor and custom matcher patterns
const out = [];
// an alternative that fails part-way leaves its bindings as they were
const r1 = [5, 2] is ([const a, 1] or [const a, 2]);
out.push(r1);
if ([5, 2] is ([const a, 1] or [const a, 2])) out.push(String(a));
// closures over bindings on the failing path, in a loop
const fns = [];
for (const i of [0, 1, 2]) {
  if ({ x: i } is { x: const x } && (fns.push(() => x), x > 1)) out.push("hit" + x);
}
out.push(fns.map((f) => f()).join(","));
// a clause that always matches, then others
out.push(match (3) { _: "first"; 3: "second"; });
out.push(match ({ k: 1 }) { { k: const k } if (k > 5): "big"; { k: let k }: "k" + k; default: "none"; });
try { match (1) { 2: 0; } } catch (e) { out.push(e.constructor.name + ":" + e.message); }
// rest patterns
if ({ a: 1, b: 2, c: 3 } is { a: 1, ...const rest }) out.push(JSON.stringify(rest));
if ([1, 2, 3, 4] is [1, ...const tail]) out.push(JSON.stringify(tail));
out.push("abc" is ["a", "b", "c"], "abc" is ["a", ..._]);
out.push(new Map([[1, 2]]) is Map, [] is Array, 1 is Number, "s" is String, null is Object);
class P { static [Symbol.customMatcher](v) { return v.p === 1 ? [v.q] : false; } }
if ({ p: 1, q: 9 } is P(const q)) out.push("q" + q);
class C {}
out.push(new C() is C, {} is C, new C() is C());
out.push(NaN is NaN, 0 is 0);
// catch pattern rethrow
try { try { throw 1; } catch (e is 2) { out.push("no"); } finally { out.push("fin"); } } catch (e) { out.push("re" + e); }
out.push(typeof Symbol.customMatcher, String(Symbol.customMatcher));
const expected = "true | 5 | hit2 | 0,1,2 | first | k1 | TypeError:No pattern matched | {\"b\":2,\"c\":3} | [2,3,4] | true | true | true | true | true | true | false | q9 | true | false | true | true | true | fin | re1 | symbol | Symbol(Symbol.customMatcher)";
const got = out.join(" | ");
console.log(got === expected ? "ok" : "got " + got);
