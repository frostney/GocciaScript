// A using declaration directly in a default clause's statement list: an early
// SyntaxError (ES2027 draft §14.3.1.1), so importing this module rejects.
switch (1) {
  default:
    using resource = null;
}
