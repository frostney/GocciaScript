// test262 $262 host hooks — GocciaScript adaptation.
//
// The test262 convention expects engines to provide a $262 global with
// host-specific capabilities.  Stock harness files (e.g. detachArrayBuffer.js)
// call methods on it.  This file is always loaded before other harness
// includes so that stock files work unchanged.
//
// Tests that need to mock $262 (e.g. harness self-tests) redefine it with
// var, which overrides this definition.
//
// Only methods Goccia can implement are provided.  Missing methods cause
// the stock harness to throw Test262Error, which the runner classifies as
// an honest FAIL.

var $262 = {
  detachArrayBuffer(buffer) {
    buffer.transfer();
  },
};
