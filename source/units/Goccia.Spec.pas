unit Goccia.Spec;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

type
  TGocciaFeatureEntry = record
    Name: string;
    Link: string;
  end;

function CreateSpecObject: TGocciaObjectValue;
function CreateProposalObject: TGocciaObjectValue;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

const
  PROP_LINK = 'link';

const
  // ---------------------------------------------------------------------------
  // ES2015
  // ---------------------------------------------------------------------------
  ES2015_FEATURES: array[0..19] of TGocciaFeatureEntry = (
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
  ES2020_FEATURES: array[0..6] of TGocciaFeatureEntry = (
    (Name: 'Optional Chaining';            Link: 'https://tc39.es/ecma262/#sec-optional-chains'),
    (Name: 'Nullish Coalescing';           Link: 'https://tc39.es/ecma262/#sec-binary-logical-operators'),
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
    (Name: 'Explicit Resource Management';          Link: 'https://tc39.es/ecma262/#sec-using-declaration'),
    (Name: 'RegExp.escape';                         Link: 'https://tc39.es/ecma262/#sec-regexp.escape'),
    (Name: 'Error.isError';                         Link: 'https://tc39.es/ecma262/#sec-error.iserror'),
    (Name: 'Uint8Array Base64/Hex';                 Link: 'https://tc39.es/ecma262/#sec-typedarray-objects'),
    (Name: 'Array.fromAsync';                       Link: 'https://tc39.es/ecma262/#sec-array.fromasync'),
    (Name: 'Math.sumPrecise';                       Link: 'https://tc39.es/ecma262/#sec-math.sumprecise'),
    (Name: 'Map.prototype.getOrInsert / getOrInsertComputed'; Link: 'https://tc39.es/ecma262/#sec-map.prototype.getorinsert'),
    (Name: 'Iterator.concat';                       Link: 'https://tc39.es/ecma262/#sec-iterator.concat')
  );

  // ---------------------------------------------------------------------------
  // ES2027
  // ---------------------------------------------------------------------------
  ES2027_FEATURES: array[0..0] of TGocciaFeatureEntry = (
    (Name: 'Temporal';  Link: 'https://tc39.es/ecma262/#sec-temporal-objects')
  );

  // ---------------------------------------------------------------------------
  // WHATWG / W3C Web Platform APIs
  // ---------------------------------------------------------------------------
  WHATWG_FEATURES: array[0..12] of TGocciaFeatureEntry = (
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
    (Name: 'fetch';             Link: 'https://fetch.spec.whatwg.org/#fetch-method'),
    (Name: 'Headers';           Link: 'https://fetch.spec.whatwg.org/#headers-class'),
    (Name: 'Response';          Link: 'https://fetch.spec.whatwg.org/#response-class')
  );

  // ---------------------------------------------------------------------------
  // TC39 Stage 3
  // ---------------------------------------------------------------------------
  STAGE3_PROPOSALS: array[0..2] of TGocciaFeatureEntry = (
    (Name: 'Decorators';            Link: 'https://github.com/tc39/proposal-decorators'),
    (Name: 'Decorator Metadata';    Link: 'https://github.com/tc39/proposal-decorator-metadata'),
    (Name: 'Joint Iteration';       Link: 'https://github.com/tc39/proposal-joint-iteration')
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
  STAGE1_PROPOSALS: array[0..0] of TGocciaFeatureEntry = (
    (Name: 'Types as Comments'; Link: 'https://tc39.es/proposal-type-annotations/')
  );

  // ---------------------------------------------------------------------------
  // TC39 Stage 0
  // ---------------------------------------------------------------------------
  STAGE0_PROPOSALS: array[0..0] of TGocciaFeatureEntry = (
    (Name: 'Enums'; Link: 'https://github.com/nicolo-ribaudo/proposal-enum')
  );

function CreateFeatureEntryObject(const AEntry: TGocciaFeatureEntry): TGocciaObjectValue;
var
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  Obj.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AEntry.Name));
  Obj.AssignProperty(PROP_LINK, TGocciaStringLiteralValue.Create(AEntry.Link));
  Result := Obj;
end;

function CreateFeatureArray(const AEntries: array of TGocciaFeatureEntry): TGocciaArrayValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  Arr := TGocciaArrayValue.Create;
  for I := Low(AEntries) to High(AEntries) do
    Arr.Elements.Add(CreateFeatureEntryObject(AEntries[I]));
  Result := Arr;
end;

procedure DefineReadOnlyProperty(const AParent: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue);
begin
  AParent.DefineProperty(AName,
    TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable]));
end;

function CreateSpecObject: TGocciaObjectValue;
var
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  DefineReadOnlyProperty(Obj, '2015', CreateFeatureArray(ES2015_FEATURES));
  DefineReadOnlyProperty(Obj, '2016', CreateFeatureArray(ES2016_FEATURES));
  DefineReadOnlyProperty(Obj, '2017', CreateFeatureArray(ES2017_FEATURES));
  DefineReadOnlyProperty(Obj, '2018', CreateFeatureArray(ES2018_FEATURES));
  DefineReadOnlyProperty(Obj, '2019', CreateFeatureArray(ES2019_FEATURES));
  DefineReadOnlyProperty(Obj, '2020', CreateFeatureArray(ES2020_FEATURES));
  DefineReadOnlyProperty(Obj, '2021', CreateFeatureArray(ES2021_FEATURES));
  DefineReadOnlyProperty(Obj, '2022', CreateFeatureArray(ES2022_FEATURES));
  DefineReadOnlyProperty(Obj, '2023', CreateFeatureArray(ES2023_FEATURES));
  DefineReadOnlyProperty(Obj, '2024', CreateFeatureArray(ES2024_FEATURES));
  DefineReadOnlyProperty(Obj, '2025', CreateFeatureArray(ES2025_FEATURES));
  DefineReadOnlyProperty(Obj, '2026', CreateFeatureArray(ES2026_FEATURES));
  DefineReadOnlyProperty(Obj, '2027', CreateFeatureArray(ES2027_FEATURES));
  DefineReadOnlyProperty(Obj, 'whatwg', CreateFeatureArray(WHATWG_FEATURES));
  Result := Obj;
end;

function CreateProposalObject: TGocciaObjectValue;
var
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  DefineReadOnlyProperty(Obj, 'stage-3', CreateFeatureArray(STAGE3_PROPOSALS));
  DefineReadOnlyProperty(Obj, 'stage-2', CreateFeatureArray(STAGE2_PROPOSALS));
  DefineReadOnlyProperty(Obj, 'stage-1', CreateFeatureArray(STAGE1_PROPOSALS));
  DefineReadOnlyProperty(Obj, 'stage-0', CreateFeatureArray(STAGE0_PROPOSALS));
  Result := Obj;
end;

end.
