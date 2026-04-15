unit Goccia.Error.Suggestions;

{$I Goccia.inc}

interface

resourcestring
  // Semicolons
  SSuggestAddSemicolon = 'Add a '';'' at the end of the statement';

  // Closing delimiters
  SSuggestCloseBlock = 'Add a matching ''}'' to close the block';
  SSuggestCloseObject = 'Add a matching ''}'' to close the object';
  SSuggestCloseClassBody = 'Add a matching ''}'' to close the class body';
  SSuggestCloseEnum = 'Add a matching ''}'' to close the enum';
  SSuggestCloseImportList = 'Add a matching ''}'' to close the import list';
  SSuggestCloseExportList = 'Add a matching ''}'' to close the export list';
  SSuggestCloseSwitchBody = 'Add a matching ''}'' to close the switch body';
  SSuggestCloseDestructuringPattern = 'Add a matching ''}'' to close the destructuring pattern';
  SSuggestCloseParenArguments = 'Add a '')'' to close the argument list';
  SSuggestCloseParenExpression = 'Add a '')'' to close the expression';
  SSuggestCloseParenCondition = 'Add a '')'' to close the condition';
  SSuggestCloseParenParameterList = 'Add a '')'' to close the parameter list';
  SSuggestCloseParenSetterParameter = 'Add a '')'' to close the setter parameter list';
  SSuggestCloseParenForOf = 'Add a '')'' to close the for...of expression';
  SSuggestCloseParenCatchClause = 'Add a '')'' to close the catch clause';
  SSuggestCloseParenDecoratorExpression = 'Add a '')'' to close the decorator expression';
  SSuggestCloseParenConstructorArguments = 'Add a '')'' to close the constructor arguments';
  SSuggestCloseParenSwitchExpression = 'Add a '')'' to close the switch expression';
  SSuggestCloseBracketArray = 'Add a '']'' to close the array';
  SSuggestCloseBracketComputedProperty = 'Add a '']'' to close the computed property access';
  SSuggestCloseBracketComputedPropertyName = 'Add a '']'' to close the computed property name';
  SSuggestCloseBracketComputedPropertyKey = 'Add a '']'' to close the computed property key';
  SSuggestCloseBracketDestructuringPattern = 'Add a '']'' to close the destructuring pattern';

  // Opening delimiters
  SSuggestOpenParenCondition = 'Add ''('' before the condition';
  SSuggestOpenParenExpression = 'Add ''('' before the expression';
  SSuggestOpenParenGetterParameterList = 'Add ''('' to start the getter parameter list';
  SSuggestOpenParenSetterParameterList = 'Add ''('' to start the setter parameter list';
  SSuggestOpenParenMethodParameterList = 'Add ''('' to start the method parameter list';
  SSuggestOpenParenCatchClause = 'Add ''('' to start the catch clause';
  SSuggestOpenParenSwitchExpression = 'Add ''('' before the switch expression';
  SSuggestOpenBraceBlock = 'Add ''{'' to start the block';
  SSuggestOpenBraceGetterBody = 'Add ''{'' to start the getter body';
  SSuggestOpenBraceSetterBody = 'Add ''{'' to start the setter body';
  SSuggestOpenBraceMethodBody = 'Add ''{'' to start the method body';
  SSuggestOpenBraceTryBlock = 'Add ''{'' to start the try block';
  SSuggestOpenBraceCatchBlock = 'Add ''{'' to start the catch block';
  SSuggestOpenBraceFinallyBlock = 'Add ''{'' to start the finally block';
  SSuggestOpenBraceClassBody = 'Add ''{'' to start the class body';
  SSuggestOpenBraceEnumBody = 'Add ''{'' to start the enum body (e.g., enum Color { Red = 0, Green = 1 })';
  SSuggestOpenBraceImportList = 'Add ''{'' to start the import list';
  SSuggestOpenBraceExportOrDeclaration = 'Add ''{'' or a declaration after export';
  SSuggestOpenBraceSwitchBody = 'Add ''{'' to start the switch body';
  SSuggestAddWhileAfterDo = 'Add ''while (condition)'' after the do block';

  // Identifiers and names
  SSuggestProvideVariableName = 'Provide a name for the variable (e.g., let myVar = ...)';
  SSuggestProvideParameterName = 'Provide a name for the parameter';
  SSuggestProvideRestParameterName = 'Provide a name for the rest parameter (e.g., ...args)';
  SSuggestProvideClassName = 'Provide a name for the class';
  SSuggestProvideSuperclassName = 'Provide the name of the class to extend (e.g., class Foo extends Bar { ... })';
  SSuggestProvideGetterPropertyName = 'Provide the getter property name';
  SSuggestProvideSetterPropertyName = 'Provide the setter property name';
  SSuggestProvideImportName = 'Provide the name of the binding to import';
  SSuggestProvideLocalName = 'Provide a local name for the imported/exported binding';
  SSuggestProvideExportName = 'Provide the name of the binding to export';
  SSuggestProvideExportedName = 'Provide a name for the exported binding';
  SSuggestProvideModulePath = 'Provide the module path as a string (e.g., from "module-name")';
  SSuggestProvideCatchParameter = 'Provide a name for the error variable (e.g., catch (e) { ... })';
  SSuggestPrivateFieldMustFollow = 'Private field names must follow the # symbol';
  SSuggestPropertyNameIdentifier = 'Property names must be identifiers. Use bracket notation for special names (e.g., obj["name"])';
  SSuggestPropertyKeysFormat = 'Property keys can be identifiers, strings, numbers, or computed [expression]';

  // Keywords
  SSuggestTernaryColon = 'The ternary operator requires '':'' between the two values (condition ? yes : no)';
  SSuggestAddColonPropertyValue = 'Add '':'' between the property name and value';
  SSuggestAddColonAfterCase = 'Add '':'' after the case value';
  SSuggestAddColonAfterDefault = 'Add '':'' after default';
  SSuggestArrowFunctionSyntax = 'Arrow functions use ''=> '' after the parameter list';
  SSuggestNamespaceImportAs = 'Namespace imports require ''as'' and a local name (e.g., import * as name from ...)';
  SSuggestAddFromAfterImport = 'Add ''from "module-path"'' after the import';
  SSuggestAddFromAfterImportList = 'Add ''from "module-path"'' after the import list';

  // Expressions and operators
  SSuggestExpressionExpected = 'Expected a value, variable, or expression here';
  SSuggestValidAssignmentTarget = 'Only variables and properties can be assigned to';
  SSuggestValidIncrementTarget = 'Only variables and properties can be incremented or decremented (e.g., x++ or obj.x--)';

  // Enums
  SSuggestEnumName = 'Enums require a name: enum MyEnum { ... }';
  SSuggestEnumMemberName = 'Enum members must be identifiers (e.g., enum Color { Red = 0 })';
  SSuggestEnumExplicitValues = 'GocciaScript enums require explicit values: MemberName = value';

  // Classes and types
  SSuggestClassMemberSyntax = 'Class members can be methods, properties, or accessors. Type modifiers (public, private, readonly) are supported as comments';
  SSuggestClassMemberExpectedSyntax = 'Class members can be: methods name() {}, properties name = value, or typed declarations name: Type;';
  SSuggestDecoratorsOnlyClasses = 'Decorators can only be applied to class declarations or class elements';
  SSuggestStaticBlockNoDecorators = 'Remove the decorator — static blocks cannot be decorated';
  SSuggestGetterNoParameters = 'Getters must have no parameters — remove the parameters';
  SSuggestSetterOneParameter = 'Setters take exactly one parameter: set name(value) { ... }';
  SSuggestProvideSetterParameterName = 'Provide a name for the setter parameter';
  SSuggestAddPropertyInitializer = 'Add ''= value'' to initialize the property';
  SSuggestAddConstInitializer = 'Add ''= value'' after the variable name';
  SSuggestComputedPropertyNeedsValue = 'Add '': value'' after the computed property (e.g., [key]: value)';

  // Imports and exports
  SSuggestImportMetaSyntax = 'Use import.meta to access module metadata (e.g., import.meta.url)';
  SSuggestDynamicImportSyntax = 'Use import("./module-path") to dynamically load a module';
  SSuggestStringImportAs = 'Use: import { "name" as localName } from "module"';
  SSuggestStringExportAs = 'Use: export { localName as "export-name" }';
  SSuggestDestructuringExportDeclareFirst = 'Declare first, then export: const x = ...; export { x };';

  // Destructuring
  SSuggestDestructuringRequiresInitializer = 'Destructuring declarations require an initializer — add ''= expression'' after the pattern';
  SSuggestDestructuringArrayOrObject = 'Use array destructuring [a, b] or object destructuring { x, y }';
  SSuggestDestructuringInvalidTarget = 'Destructuring targets must be variables or properties, not expressions';
  SSuggestObjectPatternPropertyName = 'Use: { name }, { name: alias }, or { name = default }';

  // Control flow
  SSuggestSwitchCaseOrDefault = 'Switch bodies contain case/default labels: case value: ... or default: ...';
  SSuggestMissingCatchOrFinally = 'Add catch (e) { ... } or finally { ... } after the try block';
  SSuggestDeclarePrivateField = 'Declare the private field in the class body, or move this code inside the class';
  SSuggestNumberFormatValid = 'Valid formats: integers (123), decimals (1.23), hex (0xFF), binary (0b101), octal (0o777). Numeric separators allowed (1_000)';

  // Lexer errors — strings, templates, regex
  SSuggestCloseBlockComment = 'Add "*/" to close the block comment';
  SSuggestCloseString = 'Add a closing quote to end the string';
  SSuggestCloseTemplate = 'Add a closing backtick to end the template literal';
  SSuggestCloseRegex = 'Add a closing / to end the regex, followed by optional flags (g, i, m, s, u, y)';
  SSuggestValidRegexFlags = 'Valid regex flags are: g (global), i (case-insensitive), m (multiline), s (dotAll), u (unicode), y (sticky)';
  SSuggestDuplicateRegexFlag = 'Each regex flag can only appear once';
  SSuggestRegexSuffixFlags = 'Only flag characters (g, i, m, s, u, y) are allowed after the closing /';

  // Lexer errors — numbers
  SSuggestHexNumberFormat = 'Hex numbers start with 0x followed by hex digits (e.g., 0xFF)';
  SSuggestBinaryNumberFormat = 'Binary numbers start with 0b followed by 0 or 1 (e.g., 0b1010)';
  SSuggestOctalNumberFormat = 'Octal numbers start with 0o followed by digits 0-7 (e.g., 0o755)';
  SSuggestScientificNotation = 'Scientific notation needs digits after e (e.g., 1e10 or 3.14e-2)';
  SSuggestNumericSeparator = 'Numeric separators (_) must be placed between digits (e.g., 1_000, 0xFF_FF)';

  // Lexer errors — escapes and characters
  SSuggestUnicodeEscapeFormat = 'Unicode escapes use \uXXXX (4 hex digits) or \u{XXXXXX} (1-6 hex digits)';
  SSuggestUnicodeHexDigits = 'Unicode escapes must contain only hex digits (0-9, a-f, A-F)';
  SSuggestUnicodeCodePointRange = 'Code points must be in range U+0000 to U+10FFFF';
  SSuggestHexEscapeFormat = 'Hex escapes use \xXX (exactly 2 hex digits)';
  SSuggestInvalidCharacter = 'This character is not valid in GocciaScript. Check for typos';
  SSuggestInvalidDoubleDot = 'Did you mean "..." (spread operator)? Two dots is not valid syntax';

  // Runtime errors — scope
  SSuggestTemporalDeadZone = 'the variable is in the temporal dead zone until its declaration is evaluated';
  SSuggestDeclareBeforeUse = 'check the spelling or declare the variable with const, let, or var before use';
  SSuggestUseLetNotConst = 'declare with ''let'' instead of ''const'' if it needs to be reassigned';
  SSuggestAlreadyDeclared = 'the variable was already declared with let or const in this scope';

  // Runtime errors — type and call
  SSuggestSymbolNoImplicitConversion = 'use String(symbol) or symbol.toString() for explicit conversion';
  SSuggestDecoratorFunction = 'decorators must be expressions that evaluate to a function';
  SSuggestDisposable = 'the value must have a [Symbol.dispose] method to be used with ''using''';
  SSuggestNotFunctionType = 'the value is not callable — only functions can be invoked';
  SSuggestNotConstructorType = 'only classes and constructor functions can be used with ''new''';
  SSuggestPropertyOfNullish = 'the value is null or undefined and has no properties';
  SSuggestNotIterable = 'only arrays, strings, sets, maps, and objects with Symbol.iterator can be iterated';
  SSuggestSpreadRequiresIterable = 'only arrays, strings, sets, maps, and objects with Symbol.iterator can be spread';
  SSuggestTaggedTemplateCallable = 'tagged template literals require the tag to be a callable function';
  SSuggestDestructureRequiresIterable = 'array destructuring requires an iterable value';
  SSuggestDestructureRequiresObject = 'object destructuring requires an object value';
  SSuggestIteratorProtocol = 'the value does not implement the iterator protocol';
  SSuggestCheckNullBeforeAccess = 'check that the value is not null or undefined before accessing properties';
  SSuggestCannotDeleteNonConfigurable = 'non-configurable properties cannot be deleted';
  SSuggestPrivateFieldAccess = 'private fields and accessors are only accessible within the class that defines them';
  SSuggestRequiresNew = 'class constructors must be called with ''new''';
  SSuggestEnumValueType = 'enum member values must be number, string, or symbol literals';
  SSuggestUsingInsideBlock = '''using'' declarations must be inside a block (e.g., { using x = ...; })';
  SSuggestAwaitMicrotaskDrain = 'the promise did not resolve during the microtask queue drain cycle';
  SSuggestAsyncIteratorProtocol = 'the async iterator must have a callable next() method';
  SSuggestIteratorResultObject = 'iterator next() must return an object with ''done'' and ''value'' properties';
  SSuggestTypeEnforcement = 'the value type does not match the declared type';

  // Runtime errors — array
  SSuggestSpeciesConstructor = 'Symbol.species must return a constructor function';
  SSuggestArrayLengthRange = 'array length must be a non-negative integer less than 2^32';
  SSuggestReduceInitialValue = 'provide an initial value as the second argument to reduce()';

  // Runtime errors — promises
  SSuggestPromiseThisType = 'Promise prototype methods must be called on a Promise object';
  SSuggestPromiseResolver = 'pass a function to the Promise constructor: new Promise((resolve, reject) => { ... })';
  SSuggestPromiseAnyRejected = 'Promise.any() requires at least one promise to fulfill';
  SSuggestCallbackRequired = 'pass a function as the argument';

  // Runtime errors — ArrayBuffer
  SSuggestArrayBufferThisType = 'ArrayBuffer methods must be called on an ArrayBuffer instance';
  SSuggestArrayBufferDetached = 'the ArrayBuffer has been detached (transferred) and can no longer be used';
  SSuggestArrayBufferResizable = 'create a resizable ArrayBuffer with: new ArrayBuffer(length, { maxByteLength: ... })';

  // Runtime errors — DisposableStack
  SSuggestDisposableStackThisType = 'DisposableStack methods must be called on a DisposableStack instance';
  SSuggestDisposableStackAlreadyDisposed = 'the DisposableStack has already been disposed and cannot be used again';

  // Runtime errors — iterator helpers
  SSuggestIteratorThisType = 'iterator helper methods must be called on an iterator object';
  SSuggestIteratorZipMode = 'valid modes are "shortest", "longest", or "strict"';
  SSuggestIteratorCallable = 'pass a function as the first argument';
  SSuggestIteratorFlatMapCallable = 'pass a function that returns an iterable';
  SSuggestIteratorNonNegative = 'pass a non-negative integer as the argument';
  SSuggestIteratorFromArg = 'pass an array, string, set, map, or object with Symbol.iterator or a next() method';
  SSuggestIteratorZipOptions = 'pass an options object with mode and/or padding properties';
  SSuggestIteratorZipKeyedArg = 'pass an object whose values are iterables';
  SSuggestIteratorZipKeyedPadding = 'pass an object whose keys match the iterable keys';
  SSuggestArrayFromMapFn = 'usage: Array.from(iterable [, mapFn [, thisArg]])';

  // Runtime errors — object property constraints
  SSuggestPropertyHasOnlyGetter = 'the property has only a getter and no setter defined';
  SSuggestObjectNotExtensible = 'the object has been frozen, sealed, or marked non-extensible with Object.preventExtensions()';
  SSuggestPrototypeChainTooDeep = 'check for circular prototype chains';
  SSuggestPropertyDescriptorObject = 'pass an object with writable, enumerable, configurable, value, get, or set properties';

  // Runtime errors — string
  SSuggestWellKnownSymbolCallable = 'the well-known Symbol method must be a function on the object';
  SSuggestReplaceAllGlobalFlag = 'add the ''g'' flag to the RegExp: /pattern/g';
  SSuggestRepeatCountRange = 'the count must be a non-negative finite integer';
  SSuggestNormalizationForm = 'valid forms are NFC, NFD, NFKC, or NFKD';

  // Runtime errors — regexp
  SSuggestRegExpEscapeString = 'pass a string to escape special regex characters';
  SSuggestRegExpThisType = 'RegExp prototype methods must be called on a RegExp object';

  // Runtime errors — code points
  SSuggestCodePointRange = 'code points must be integers between 0 and 0x10FFFF';

  // Runtime errors — Temporal
  SSuggestTemporalISOFormat = 'provide a valid ISO 8601 string (e.g., "2024-01-15", "2024-01-15T10:30:00")';
  SSuggestTemporalFromArg = 'pass a string in ISO 8601 format, an existing Temporal object, or a property bag';
  SSuggestTemporalCompareArg = 'both arguments must be the same Temporal type or valid ISO strings';
  SSuggestTemporalWithObject = 'pass an object with the date/time fields to update (e.g., { year: 2025 })';
  SSuggestTemporalDurationArg = 'pass a Temporal.Duration, a duration string (e.g., "P1Y2M"), or a duration-like object';
  SSuggestTemporalRoundArg = 'pass a unit string (e.g., "hour") or an options object with smallestUnit';
  SSuggestTemporalNoValueOf = 'Temporal objects cannot be implicitly converted; use toString() or compare() instead';
  SSuggestTemporalValidUnits = 'valid units are: year, month, week, day, hour, minute, second, millisecond, microsecond, nanosecond';
  SSuggestTemporalDurationSigns = 'all duration fields must have the same sign (all positive or all negative)';
  SSuggestTemporalRelativeTo = 'pass a relativeTo option with a PlainDate or ZonedDateTime for calendar-sensitive operations';
  SSuggestTemporalOverflow = 'valid overflow options are "constrain" or "reject"';
  SSuggestTemporalRoundingMode = 'valid modes: ceil, floor, expand, trunc, halfCeil, halfFloor, halfExpand, halfTrunc, halfEven';
  SSuggestTemporalTimezone = 'provide a timezone string (e.g., "UTC", "America/New_York") or a Temporal.TimeZone';
  SSuggestTemporalDateRange = 'check that month is 1-12 and day is valid for the month';
  SSuggestTemporalMonthCode = 'valid monthCodes are M01 through M12';
  SSuggestTemporalThisType = 'this method must be called on a Temporal object of the correct type';
  SSuggestTemporalToPlainDateYear = 'pass an object with a year property (e.g., { year: 2024 })';

  // Runtime errors — Proxy
  SSuggestProxyRevoked = 'the proxy has been revoked and can no longer be used';
  SSuggestProxyTrapInvariant = 'proxy trap must respect the invariants of the target object''s property configuration';
  SSuggestProxyTrapReturnType = 'the proxy trap must return the correct type for the operation';
  SSuggestProxyTargetType = 'Proxy requires both target and handler to be objects';

  // Runtime errors — TypedArray
  SSuggestTypedArrayThisType = 'TypedArray methods must be called on a TypedArray instance';
  SSuggestTypedArrayCallable = 'pass a function as the callback argument';
  SSuggestTypedArraySetSource = 'pass an Array or TypedArray as the source';
  SSuggestTypedArrayLength = 'the length or offset is outside the valid range for this buffer';
  SSuggestTypedArrayAlignment = 'the byte offset must be aligned to the element size of the TypedArray';

  // Runtime errors — URL
  SSuggestURLThisType = 'URL prototype methods must be called on a URL object';
  SSuggestURLFormat = 'provide a valid URL string (e.g., "https://example.com/path")';

  // Runtime errors — URLSearchParams
  SSuggestURLSearchParamsThisType = 'URLSearchParams methods must be called on a URLSearchParams instance';
  SSuggestURLSearchParamsInit = 'pass a string, array of pairs, or object to construct URLSearchParams';

  // Runtime errors — array methods
  SSuggestArrayThisType = 'Array prototype methods must be called on an Array';

  // Runtime errors — Uint8Array encoding
  SSuggestBase64Format = 'provide a valid base64 string using the standard or base64url alphabet';
  SSuggestHexFormat = 'provide a valid hex string with an even number of hex digits (0-9, a-f)';
  SSuggestUint8ArrayThisType = 'Uint8Array encoding methods must be called on a Uint8Array instance';

  // Runtime errors — FFI
  SSuggestFFIUsage = 'check the FFI function signature and argument types';
  SSuggestFFILibraryOpen = 'open a library first with FFI.open("path/to/library")';

  // Runtime errors — test framework
  SSuggestTestUsage = 'check the test framework API documentation for correct usage';

  // Runtime errors — Object.*
  SSuggestObjectArgType = 'pass an object as the argument';

  // Runtime errors — JSON/JSON5/JSONL
  SSuggestJSONFormat = 'ensure the string contains valid JSON';

  // Runtime errors — URI encoding
  SSuggestURIEncoding = 'check that the URI string contains only valid percent-encoded sequences';

  // Runtime errors — TextDecoder/TextEncoder
  SSuggestTextDecoderThisType = 'TextDecoder methods must be called on a TextDecoder instance';
  SSuggestTextEncoderThisType = 'TextEncoder methods must be called on a TextEncoder instance';

  // Runtime errors — SharedArrayBuffer
  SSuggestSharedArrayBufferThisType = 'SharedArrayBuffer methods must be called on a SharedArrayBuffer instance';

  // Runtime errors — Reflect
  SSuggestReflectObjectArg = 'Reflect methods require the target to be an object';

  // Runtime errors — Symbol
  SSuggestSymbolThisType = 'Symbol prototype methods must be called on a Symbol';
  SSuggestSymbolKeyForArg = 'pass a Symbol value to Symbol.keyFor (e.g., Symbol.for("key"))';

  // Runtime errors — Performance
  SSuggestPerformanceThisType = 'Performance methods must be called on a Performance instance';

  // Runtime errors — string argument required
  SSuggestStringArgRequired = 'pass a string as the argument';

  // Runtime errors — Semver
  SSuggestSemverUsage = 'check the semver API documentation for correct usage';

  // Runtime errors — import.meta
  SSuggestImportMetaUsage = 'import.meta.resolve() requires a string specifier argument';

  // Runtime errors — Map
  SSuggestMapCallbackRequired = 'pass a function as the callback argument';

  // Runtime errors — Number
  SSuggestNumberRange = 'the argument must be within the valid range';

  // Runtime errors — ToPrimitive
  SSuggestToPrimitive = 'define a valueOf() or toString() method on the object';

  // Runtime errors — Function
  SSuggestFunctionApply = 'Function.prototype.apply requires a callable this value';

  // Runtime errors — illegal constructor
  SSuggestIllegalConstructor = 'this constructor cannot be called directly';

  // Runtime errors — structuredClone
  SSuggestStructuredClone = 'only plain objects, arrays, primitives, and built-in types can be cloned';

  // Runtime errors — TOML
  SSuggestTOMLSyntax = 'check that the input is valid TOML syntax';

  // Runtime errors — YAML
  SSuggestYAMLSyntax = 'check that the input is valid YAML syntax';

  // Runtime errors — TextEncoder
  SSuggestTextEncoderEncodeIntoArgs = 'encodeInto requires a string source and a Uint8Array destination';

  // Runtime errors — read-only property
  SSuggestReadOnlyProperty = 'the property is read-only and cannot be assigned to';

implementation

end.
