// test262 $262 host hooks: G2's adaptation of GocciaScript's scripts/test262_harness/$262.js.
//
// `g2 --test262` installs the hidden G2Host object with the hooks the engine implements, then evaluates this
// file as the first harness Script. $262 is the only global binding it creates, so a test may declare any
// other name at its top level (ES2026 16.1.7 GlobalDeclarationInstantiation). A hook the engine does not
// implement is left out, so a test that needs it fails honestly.

if (typeof G2Host === "undefined") {
  throw new Error("$262 host hooks require g2 --test262");
}

var $262 = (function () {
  var H = G2Host;
  var host262 = {
    global: globalThis,
    evalScript: function (sourceText) { return H.evalScript(sourceText); },
    gc: function () { H.gc(); },
    agent: H.agent,
  };
  if (typeof H.detachArrayBuffer === "function") {
    host262.detachArrayBuffer = function (buffer) { return H.detachArrayBuffer(buffer); };
  }
  if (typeof H.createRealm === "function") {
    host262.createRealm = function () { return H.createRealm(); };
  }
  if ("IsHTMLDDA" in H) host262.IsHTMLDDA = H.IsHTMLDDA;
  return host262;
})();
