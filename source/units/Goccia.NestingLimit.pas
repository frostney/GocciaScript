unit Goccia.NestingLimit;

{$I Goccia.inc}

interface

const
  // How deeply source code and regular expression patterns may nest. The
  // parser and the RegExp pattern compiler are recursive descent: each level
  // of nesting costs native stack frames, and the production build has no
  // stack check of its own, so a deep enough program or pattern would run
  // into the guard page and kill the process. Both count their nesting and
  // report a SyntaxError once the count passes this limit.
  //
  // The parser counts one level for each nested statement, each
  // AssignmentExpression (an array element, a property value, a call
  // argument, a template substitution, a parenthesized expression, an arrow
  // body, an assignment's right-hand side, a branch of a conditional), each
  // prefix operator (`!`, `typeof`, `await`, ...), each `new` and `**`
  // operand, and each `extends` clause, binding pattern and match pattern.
  // The RegExp compiler counts each group, lookaround and nested class.
  // Operator, member and call chains (`a + b + c`, `a.b().c`) are parsed in
  // a loop and do not count here; MAX_TREE_HEIGHT bounds them.
  //
  // Real code stays far below the limit: the deepest nesting in the bundles
  // the benchmarks run (Web Tooling, JetStream, are-we-fast-yet) and in about
  // 900 npm files (bundled webpack, babel, TypeScript, Next.js) is 65 levels,
  // and test262 stays below 130. The costliest constructs (computed member
  // keys, binding patterns, spreads) take about 3.5 KiB of stack per level
  // to parse in a production build and 4 KiB in a development build, so the
  // deepest accepted source parses in about half of an 8 MiB stack, the size
  // of the main thread and of the test runner's workers.
  MAX_NESTING_DEPTH = 1024;

  // How tall the syntax tree of a statement or expression may be, counting
  // each level of nesting and each link of an operator, member or call chain.
  // The parser builds a chain in a loop, but every pass after it (the
  // bytecode compiler and its analyses, the tree-walk evaluator, freeing the
  // tree) recurses once per link, so a chain long enough would overflow the
  // stack there. The parser bounds the height bottom up (see
  // TGocciaParser.LeaveNesting) and reports a SyntaxError past this limit.
  //
  // The tallest tree in the corpus above is 1,339, from the Unicode tables of
  // regenerate in Babel's bundle: a chain of 658 `.addRange(a, b)` calls,
  // each a member access and a call. A level of a tree this tall costs at
  // most about 0.9 KiB of stack in the later passes of a development build,
  // so the tallest accepted tree, on top of the deepest accepted nesting,
  // still fits an 8 MiB stack.
  MAX_TREE_HEIGHT = 4096;

  // Nesting no deeper than this is not checked against the native stack:
  // it uses a few hundred kilobytes at most, less than the NATIVE_STACK_RESERVE
  // the VM keeps free below the deepest native re-entry. So code that does
  // not nest deeply never looks up the bounds of the thread's stack.
  NESTING_UNCHECKED_DEPTH = 32;

// True when ADepth levels of nesting are more than the engine accepts: more
// than MAX_NESTING_DEPTH, or, past NESTING_UNCHECKED_DEPTH, so many that less
// than NATIVE_STACK_RESERVE of the calling thread's native stack is left. The
// second case covers source parsed or a pattern compiled while the native
// stack is already deep, such as `new RegExp` in a getter that recursion has
// re-entered many times.
function NestingDepthExceeded(const ADepth: Integer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
// True when, past NESTING_UNCHECKED_DEPTH levels, less than
// NATIVE_STACK_RESERVE of the calling thread's native stack is left. The
// bytecode compiler checks its walk over the tree with this: the parser has
// already bounded how tall the tree is, so only a compile that starts on an
// already deep native stack can come near its end.
function NestingStackExhausted(const ADepth: Integer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
// True when less than NATIVE_STACK_RESERVE of the calling thread's native
// stack is left. False where the bounds of the stack cannot be found.
function NativeStackNearlyExhausted: Boolean;

implementation

uses
  Goccia.StackLimit;

function NestingDepthExceeded(const ADepth: Integer): Boolean;
begin
  Result := (ADepth > NESTING_UNCHECKED_DEPTH) and
    ((ADepth > MAX_NESTING_DEPTH) or NativeStackNearlyExhausted);
end;

function NestingStackExhausted(const ADepth: Integer): Boolean;
begin
  Result := (ADepth > NESTING_UNCHECKED_DEPTH) and NativeStackNearlyExhausted;
end;

function NativeStackNearlyExhausted: Boolean;
var
  Limit: NativeUInt;
begin
  Limit := NativeStackLimit;
  // Stacks grow down on every target the engine builds for.
  Result := (Limit <> 0) and (NativeStackPosition < Limit);
end;

end.
