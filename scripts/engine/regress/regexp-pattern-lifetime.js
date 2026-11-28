// goccia-flags: --gc-stress
// A compiled RegExp pattern lives while a RegExp uses it, and native code pins one it uses with no RegExp:
// "abc".match() builds its match array from a RegExp only the native holds, a string pattern's search has none,
// and compile() moves a RegExp to another pattern. Every allocation collects (--gc-stress); prints ok last.
let n = 0;
for (let i = 0; i < 100; i++) {
  const m = "abc".match();          // RegExp(""), held only by the native while the match array is built
  if (m[0] !== "") throw new Error("match()");
  if ("xyzabc".search("b" + (i % 7 ? "c" : "")) !== 4) throw new Error("search");
  const g = "a1b22c333".match(new RegExp("\\d+", "g"));
  if (g.length !== 3) throw new Error("global");
  const r = /(\w)(\d)/;
  r.compile("(\\d)(\\w)?");
  if (r.exec("x9y")[1] !== "9") throw new Error("compile");
  if ("a,b;c".split(new RegExp("[,;]")).length !== 3) throw new Error("split");
  if ("aXbX".replace(/X/g, (s) => s.toLowerCase()) !== "axbx") throw new Error("replace");
  new RegExp("throwaway" + i);
  n++;
}
if (n !== 100) throw new Error("count");
console.log("ok");
