unit Goccia.Spec.Data;

{ The ECMAScript editions, WHATWG APIs and TC39 proposals GocciaScript
  implements: the data behind Goccia.spec and Goccia.proposal, free of any
  engine's values so every engine builds its objects from the same tables. }

{$I Goccia.inc}

interface

type
  TGocciaFeatureEntry = record
    Name: string;
    Link: string;
  end;

  TGocciaFeatureGroup = record
    Key: string;
    Entries: array of TGocciaFeatureEntry;
  end;

  TGocciaFeatureGroups = array of TGocciaFeatureGroup;

{ Goccia.spec's groups: one per edition year, then "whatwg" }
function SpecFeatureGroups: TGocciaFeatureGroups;
{ Goccia.proposal's groups: one per stage ("stage-4", "stage-2.7", ...) }
function ProposalFeatureGroups: TGocciaFeatureGroups;

implementation

const
  // ---------------------------------------------------------------------------
  // ES2015
  // ---------------------------------------------------------------------------
  ES2015_FEATURES: array[0..21] of TGocciaFeatureEntry = (
    (Name: 'Let and Const';           Link: 'https://tc39.es/ecma262/#sec-let-and-const-declarations'),
    (Name: 'Arrow Functions';         Link: 'https://tc39.es/ecma262/#sec-arrow-function-definitions'),
    (Name: 'Classes';                 Link: 'https://tc39.es/ecma262/#sec-class-definitions'),
    (Name: 'Destructuring';           Link: 'https://tc39.es/ecma262/#sec-destructuring-assignment'),
    (Name: 'Template Literals';       Link: 'https://tc39.es/ecma262/#sec-template-literals'),
    (Name: 'Tagged Templates';        Link: 'https://tc39.es/ecma262/#sec-tagged-templates'),
    (Name: 'For...Of';               Link: 'https://tc39.es/ecma262/#sec-for-in-and-for-of-statements'),
    (Name: 'Symbol';                  Link: 'https://tc39.es/ecma262/#sec-symbol-objects'),
    (Name: 'Promise';                 Link: 'https://tc39.es/ecma262/#sec-promise-objects'),
    (Name: 'Map';                     Link: 'https://tc39.es/ecma262/#sec-map-objects'),
    (Name: 'Set';                     Link: 'https://tc39.es/ecma262/#sec-set-objects'),
    (Name: 'WeakMap';                 Link: 'https://tc39.es/ecma262/#sec-weakmap-objects'),
    (Name: 'WeakSet';                 Link: 'https://tc39.es/ecma262/#sec-weakset-objects'),
    (Name: 'Proxy';                   Link: 'https://tc39.es/ecma262/#sec-proxy-objects'),
    (Name: 'Reflect';                 Link: 'https://tc39.es/ecma262/#sec-reflect-object'),
    (Name: 'Default Parameters';      Link: 'https://tc39.es/ecma262/#sec-function-definitions'),
    (Name: 'Rest Parameters';         Link: 'https://tc39.es/ecma262/#sec-function-definitions'),
    (Name: 'Spread Operator';         Link: 'https://tc39.es/ecma262/#sec-array-initializer'),
    (Name: 'Computed Property Names'; Link: 'https://tc39.es/ecma262/#sec-object-initializer'),
    (Name: 'Shorthand Methods';       Link: 'https://tc39.es/ecma262/#sec-method-definitions'),
    (Name: 'Modules';                 Link: 'https://tc39.es/ecma262/#sec-modules'),
    (Name: 'Iterators';              Link: 'https://tc39.es/ecma262/#sec-iteration')
  );

  // ---------------------------------------------------------------------------
  // ES2016
  // ---------------------------------------------------------------------------
  ES2016_FEATURES: array[0..1] of TGocciaFeatureEntry = (
    (Name: 'Exponentiation Operator';    Link: 'https://tc39.es/ecma262/#sec-exp-operator'),
    (Name: 'Array.prototype.includes';   Link: 'https://tc39.es/ecma262/#sec-array.prototype.includes')
  );

  // ---------------------------------------------------------------------------
  // ES2017
  // ---------------------------------------------------------------------------
  ES2017_FEATURES: array[0..4] of TGocciaFeatureEntry = (
    (Name: 'Async Functions';                      Link: 'https://tc39.es/ecma262/#sec-async-function-definitions'),
    (Name: 'Object.entries';                       Link: 'https://tc39.es/ecma262/#sec-object.entries'),
    (Name: 'Object.values';                        Link: 'https://tc39.es/ecma262/#sec-object.values'),
    (Name: 'Object.getOwnPropertyDescriptors';     Link: 'https://tc39.es/ecma262/#sec-object.getownpropertydescriptors'),
    (Name: 'Trailing Commas in Parameters';        Link: 'https://tc39.es/ecma262/#sec-function-definitions')
  );

  // ---------------------------------------------------------------------------
  // ES2018
  // ---------------------------------------------------------------------------
  ES2018_FEATURES: array[0..3] of TGocciaFeatureEntry = (
    (Name: 'Async Iteration';                Link: 'https://tc39.es/ecma262/#sec-for-in-and-for-of-statements'),
    (Name: 'Rest/Spread Properties';         Link: 'https://tc39.es/ecma262/#sec-object-initializer'),
    (Name: 'Promise.prototype.finally';      Link: 'https://tc39.es/ecma262/#sec-promise.prototype.finally'),
    (Name: 'RegExp Named Capture Groups';    Link: 'https://tc39.es/ecma262/#sec-regexp-regular-expression-objects')
  );

  // ---------------------------------------------------------------------------
  // ES2019
  // ---------------------------------------------------------------------------
  ES2019_FEATURES: array[0..5] of TGocciaFeatureEntry = (
    (Name: 'Array.prototype.flat';        Link: 'https://tc39.es/ecma262/#sec-array.prototype.flat'),
    (Name: 'Array.prototype.flatMap';     Link: 'https://tc39.es/ecma262/#sec-array.prototype.flatmap'),
    (Name: 'Object.fromEntries';          Link: 'https://tc39.es/ecma262/#sec-object.fromentries'),
    (Name: 'String.prototype.trimStart';  Link: 'https://tc39.es/ecma262/#sec-string.prototype.trimstart'),
    (Name: 'String.prototype.trimEnd';    Link: 'https://tc39.es/ecma262/#sec-string.prototype.trimend'),
    (Name: 'Optional Catch Binding';      Link: 'https://tc39.es/ecma262/#sec-try-statement')
  );

  // ---------------------------------------------------------------------------
  // ES2020
  // ---------------------------------------------------------------------------
  ES2020_FEATURES: array[0..7] of TGocciaFeatureEntry = (
    (Name: 'Optional Chaining';            Link: 'https://tc39.es/ecma262/#sec-optional-chains'),
    (Name: 'Nullish Coalescing';           Link: 'https://tc39.es/ecma262/#sec-binary-logical-operators'),
    (Name: 'BigInt';                       Link: 'https://tc39.es/ecma262/#sec-bigint-objects'),
    (Name: 'Promise.allSettled';           Link: 'https://tc39.es/ecma262/#sec-promise.allsettled'),
    (Name: 'globalThis';                   Link: 'https://tc39.es/ecma262/#sec-globalthis'),
    (Name: 'String.prototype.matchAll';    Link: 'https://tc39.es/ecma262/#sec-string.prototype.matchall'),
    (Name: 'import.meta';                  Link: 'https://tc39.es/ecma262/#sec-import.meta'),
    (Name: 'Dynamic import()';             Link: 'https://tc39.es/ecma262/#sec-import-calls')
  );

  // ---------------------------------------------------------------------------
  // ES2021
  // ---------------------------------------------------------------------------
  ES2021_FEATURES: array[0..4] of TGocciaFeatureEntry = (
    (Name: 'String.prototype.replaceAll';    Link: 'https://tc39.es/ecma262/#sec-string.prototype.replaceall'),
    (Name: 'Promise.any';                    Link: 'https://tc39.es/ecma262/#sec-promise.any'),
    (Name: 'AggregateError';                 Link: 'https://tc39.es/ecma262/#sec-aggregate-error-objects'),
    (Name: 'Logical Assignment Operators';   Link: 'https://tc39.es/ecma262/#sec-assignment-operators'),
    (Name: 'Numeric Separators';             Link: 'https://tc39.es/ecma262/#sec-literals-numeric-literals')
  );

  // ---------------------------------------------------------------------------
  // ES2022
  // ---------------------------------------------------------------------------
  ES2022_FEATURES: array[0..6] of TGocciaFeatureEntry = (
    (Name: 'Class Fields';            Link: 'https://tc39.es/ecma262/#sec-class-definitions'),
    (Name: 'Private Methods';         Link: 'https://tc39.es/ecma262/#sec-class-definitions'),
    (Name: 'Top-Level Await';         Link: 'https://tc39.es/ecma262/#sec-modules'),
    (Name: 'Object.hasOwn';           Link: 'https://tc39.es/ecma262/#sec-object.hasown'),
    (Name: 'Array.prototype.at';      Link: 'https://tc39.es/ecma262/#sec-array.prototype.at'),
    (Name: 'Error Cause';             Link: 'https://tc39.es/ecma262/#sec-error-objects'),
    (Name: 'Class Static Blocks';     Link: 'https://tc39.es/ecma262/#sec-class-definitions')
  );

  // ---------------------------------------------------------------------------
  // ES2023
  // ---------------------------------------------------------------------------
  ES2023_FEATURES: array[0..5] of TGocciaFeatureEntry = (
    (Name: 'Array.prototype.findLast';       Link: 'https://tc39.es/ecma262/#sec-array.prototype.findlast'),
    (Name: 'Array.prototype.findLastIndex';  Link: 'https://tc39.es/ecma262/#sec-array.prototype.findlastindex'),
    (Name: 'Array.prototype.toSorted';       Link: 'https://tc39.es/ecma262/#sec-array.prototype.tosorted'),
    (Name: 'Array.prototype.toReversed';     Link: 'https://tc39.es/ecma262/#sec-array.prototype.toreversed'),
    (Name: 'Array.prototype.toSpliced';      Link: 'https://tc39.es/ecma262/#sec-array.prototype.tospliced'),
    (Name: 'Array.prototype.with';           Link: 'https://tc39.es/ecma262/#sec-array.prototype.with')
  );

  // ---------------------------------------------------------------------------
  // ES2024
  // ---------------------------------------------------------------------------
  ES2024_FEATURES: array[0..6] of TGocciaFeatureEntry = (
    (Name: 'Promise.withResolvers';            Link: 'https://tc39.es/ecma262/#sec-promise.withresolvers'),
    (Name: 'Object.groupBy';                   Link: 'https://tc39.es/ecma262/#sec-object.groupby'),
    (Name: 'Map.groupBy';                      Link: 'https://tc39.es/ecma262/#sec-map.groupby'),
    (Name: 'Resizable ArrayBuffer';            Link: 'https://tc39.es/ecma262/#sec-arraybuffer-objects'),
    (Name: 'String.prototype.isWellFormed';    Link: 'https://tc39.es/ecma262/#sec-string.prototype.iswellformed'),
    (Name: 'String.prototype.toWellFormed';    Link: 'https://tc39.es/ecma262/#sec-string.prototype.towellformed'),
    (Name: 'RegExp v Flag';                    Link: 'https://tc39.es/ecma262/#sec-regexp-regular-expression-objects')
  );

  // ---------------------------------------------------------------------------
  // ES2025
  // ---------------------------------------------------------------------------
  ES2025_FEATURES: array[0..6] of TGocciaFeatureEntry = (
    (Name: 'Set Methods';                     Link: 'https://tc39.es/ecma262/#sec-set-objects'),
    (Name: 'Iterator Helpers';                Link: 'https://tc39.es/ecma262/#sec-iterator-objects'),
    (Name: 'Promise.try';                     Link: 'https://tc39.es/ecma262/#sec-promise.try'),
    (Name: 'RegExp Modifiers';                Link: 'https://tc39.es/ecma262/#sec-regexp-regular-expression-objects'),
    (Name: 'Duplicate Named Capture Groups';  Link: 'https://tc39.es/ecma262/#sec-regexp-regular-expression-objects'),
    (Name: 'Float16Array';                    Link: 'https://tc39.es/ecma262/#sec-typedarray-objects'),
    (Name: 'Math.f16round';                   Link: 'https://tc39.es/ecma262/#sec-math.f16round')
  );

  // ---------------------------------------------------------------------------
  // ES2026
  // ---------------------------------------------------------------------------
  ES2026_FEATURES: array[0..8] of TGocciaFeatureEntry = (
    (Name: 'JSON.parse Source Text Access';         Link: 'https://tc39.es/ecma262/#sec-json.parse'),
    (Name: 'RegExp.escape';                         Link: 'https://tc39.es/ecma262/#sec-regexp.escape'),
    (Name: 'Error.isError';                         Link: 'https://tc39.es/ecma262/#sec-error.iserror'),
    (Name: 'Uint8Array Base64/Hex';                 Link: 'https://tc39.es/ecma262/#sec-typedarray-objects'),
    (Name: 'Array.fromAsync';                       Link: 'https://tc39.es/ecma262/#sec-array.fromasync'),
    (Name: 'Math.sumPrecise';                       Link: 'https://tc39.es/ecma262/#sec-math.sumprecise'),
    (Name: 'Map.prototype.getOrInsert / getOrInsertComputed'; Link: 'https://tc39.es/ecma262/#sec-map.prototype.getorinsert'),
    (Name: 'WeakMap.prototype.getOrInsert / getOrInsertComputed'; Link: 'https://tc39.es/ecma262/#sec-weakmap.prototype.getorinsert'),
    (Name: 'Iterator.concat';                       Link: 'https://tc39.es/ecma262/#sec-iterator.concat')
  );

  // ---------------------------------------------------------------------------
  // ES2027 — merged into the ECMA-262 working draft, not yet ratified
  // ---------------------------------------------------------------------------
  ES2027_FEATURES: array[0..1] of TGocciaFeatureEntry = (
    (Name: 'Explicit Resource Management';  Link: 'https://tc39.es/ecma262/#sec-let-and-const-declarations'),
    (Name: 'Joint Iteration';               Link: 'https://tc39.es/ecma262/#sec-iterator.zip')
  );

  // ---------------------------------------------------------------------------
  // WHATWG / W3C Web Platform APIs
  // ---------------------------------------------------------------------------
  WHATWG_FEATURES: array[0..15] of TGocciaFeatureEntry = (
    (Name: 'console';           Link: 'https://console.spec.whatwg.org/'),
    (Name: 'structuredClone';   Link: 'https://html.spec.whatwg.org/multipage/structured-data.html#dom-structuredclone'),
    (Name: 'DOMException';      Link: 'https://webidl.spec.whatwg.org/#idl-DOMException'),
    (Name: 'atob / btoa';       Link: 'https://html.spec.whatwg.org/multipage/webappapis.html#atob'),
    (Name: 'queueMicrotask';    Link: 'https://html.spec.whatwg.org/multipage/timers-and-user-prompts.html#dom-queuemicrotask'),
    (Name: 'URL';               Link: 'https://url.spec.whatwg.org/#url-class'),
    (Name: 'URLSearchParams';   Link: 'https://url.spec.whatwg.org/#urlsearchparams'),
    (Name: 'TextEncoder';       Link: 'https://encoding.spec.whatwg.org/#textencoder'),
    (Name: 'TextDecoder';       Link: 'https://encoding.spec.whatwg.org/#textdecoder'),
    (Name: 'Performance';       Link: 'https://w3c.github.io/hr-time/#dom-performance-now'),
    (Name: 'AbortController';   Link: 'https://dom.spec.whatwg.org/#abortcontroller'),
    (Name: 'AbortSignal';       Link: 'https://dom.spec.whatwg.org/#abortsignal'),
    (Name: 'fetch';             Link: 'https://fetch.spec.whatwg.org/#fetch-method'),
    (Name: 'Headers';           Link: 'https://fetch.spec.whatwg.org/#headers-class'),
    (Name: 'Response';          Link: 'https://fetch.spec.whatwg.org/#response-class'),
    (Name: 'Blob';              Link: 'https://w3c.github.io/FileAPI/#blob-section')
  );

  // ---------------------------------------------------------------------------
  // TC39 Stage 4 — finished, not yet in a ratified ECMA-262 edition
  // ---------------------------------------------------------------------------
  STAGE4_PROPOSALS: array[0..1] of TGocciaFeatureEntry = (
    (Name: 'Temporal';           Link: 'https://tc39.es/proposal-temporal/'),
    (Name: 'Iterator Includes';  Link: 'https://github.com/tc39/proposal-iterator-includes')
  );

  // ---------------------------------------------------------------------------
  // TC39 Stage 2.7
  // ---------------------------------------------------------------------------
  STAGE27_PROPOSALS: array[0..1] of TGocciaFeatureEntry = (
    (Name: 'Decorators';          Link: 'https://github.com/tc39/proposal-decorators'),
    (Name: 'Decorator Metadata';  Link: 'https://github.com/tc39/proposal-decorator-metadata')
  );

  // ---------------------------------------------------------------------------
  // TC39 Stage 2
  // ---------------------------------------------------------------------------
  STAGE2_PROPOSALS: array[0..0] of TGocciaFeatureEntry = (
    (Name: 'Math.clamp'; Link: 'https://github.com/tc39/proposal-math-clamp')
  );

  // ---------------------------------------------------------------------------
  // TC39 Stage 1
  // ---------------------------------------------------------------------------
  STAGE1_PROPOSALS: array[0..2] of TGocciaFeatureEntry = (
    (Name: 'Types as Comments'; Link: 'https://tc39.es/proposal-type-annotations/'),
    (Name: 'Pattern Matching'; Link: 'https://tc39.es/proposal-pattern-matching/'),
    (Name: 'Enums'; Link: 'https://github.com/tc39/proposal-enum')
  );

function Group(const AKey: string;
  const AEntries: array of TGocciaFeatureEntry): TGocciaFeatureGroup;
var
  I: Integer;
begin
  Result.Key := AKey;
  SetLength(Result.Entries, Length(AEntries));
  for I := 0 to High(AEntries) do
    Result.Entries[I] := AEntries[I];
end;

function SpecFeatureGroups: TGocciaFeatureGroups;
begin
  SetLength(Result, 14);
  Result[0] := Group('2015', ES2015_FEATURES);
  Result[1] := Group('2016', ES2016_FEATURES);
  Result[2] := Group('2017', ES2017_FEATURES);
  Result[3] := Group('2018', ES2018_FEATURES);
  Result[4] := Group('2019', ES2019_FEATURES);
  Result[5] := Group('2020', ES2020_FEATURES);
  Result[6] := Group('2021', ES2021_FEATURES);
  Result[7] := Group('2022', ES2022_FEATURES);
  Result[8] := Group('2023', ES2023_FEATURES);
  Result[9] := Group('2024', ES2024_FEATURES);
  Result[10] := Group('2025', ES2025_FEATURES);
  Result[11] := Group('2026', ES2026_FEATURES);
  Result[12] := Group('2027', ES2027_FEATURES);
  Result[13] := Group('whatwg', WHATWG_FEATURES);
end;

function ProposalFeatureGroups: TGocciaFeatureGroups;
begin
  SetLength(Result, 4);
  Result[0] := Group('stage-4', STAGE4_PROPOSALS);
  Result[1] := Group('stage-2.7', STAGE27_PROPOSALS);
  Result[2] := Group('stage-2', STAGE2_PROPOSALS);
  Result[3] := Group('stage-1', STAGE1_PROPOSALS);
end;

end.
