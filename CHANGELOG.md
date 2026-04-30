# Changelog

All notable changes to GocciaScript are documented in this file.

## [0.7.1] - 2026-04-29

### 🌐 Website

- Add version dispatch to playground engine (#467)
- Use static Vercel ignore config (#462)
- Refresh docs and website to match recent feature set (#460)
- Add website agent discovery metadata (#453)
- Scope Vercel builds to website changes (#455)

### 🏗️ Internal

- Update CHANGELOG.md for 0.7.0 (#457)

### 🐛 Fixed

- Fix microtask drain re-entrancy crashing async-generator yield rejection (#465)
- Fix generator loop resume and async iterator lookup (#456)
- Fix changelog website grouping (#459)

### 📖 Other

- Unify stdin handling across all three runners (#461)

## [0.7.0] - 2026-04-29

### ✨ Improved

- Make fetch asynchronous with fetch manager (#396)
- Expand Test262 support for classes, functions, and typed arrays (#378)
- Expand typed array coverage across core methods (#367)
- Improve Temporal conformance and enable BigInt in test262 (#354)
- Include tests, benchmarks, and examples in nightly archives (#355)
- Report wall-clock timing for parallel runs (#318)
- Improve REPL object previews with depth-aware formatting (#296)
- Make import.meta tests account for Windows drive letters (#274)

### 🌐 Website

- Fix CSV and TSV header option examples (#452)
- Add agent discovery Link headers (#450)
- Declare Content Signals in robots.txt (#449)
- Enable markdown negotiation for agents (#451)
- Update playground examples and controls (#447)
- Add website Browserslist config (#446)
- Refine website landing and sandbox copy (#445)
- Update website API execution endpoints (#444)
- Fix Vercel ignore diff for shallow clones (#443)
- Fix website labels, hero console alignment, and example file extensions (#423)
- Unify website console output with shared ConsolePanel (#418)
- Skip Vercel preview builds for non-website changes (#414)
- Add Next.js 16 website (docs, playground, sandbox, install) (#407)

### 🏗️ Internal

- Extract shared text semantics utilities (#439)
- Update copyright year in LICENSE file (#384)
- Refactor engine around pluggable executors (#332)
- Update ECMAScript edition references to 2027 (#323)
- Refactor CLIs onto shared application framework (#299)
- Update docs (#302)
- Update CHANGELOG.md for 0.6.1 (#293)
- Refactor GC into `Goccia.GarbageCollector` (#254)

### 🐛 Fixed

- Fix generator continuation replay (#448)
- Fix CI cross toolchain resource compiler (#442)
- Fix ZonedDateTime DST-aware diff rounding (#435)
- Fix Temporal BigInt duration overflow (#431)
- Fix Windows parser CLI output assertions (#430)
- Fix unsupported var recovery before block close (#427)
- Fix for-of `continue` raising ReferenceError instead of skipping iteration (#425)
- Thread options through Temporal *.until / *.since methods (#417)
- Fix Number toString scientific notation threshold and format (#415)
- Fix decorator wrapper this-binding test gap and misdirecting warning (#416)
- Fix `Array.prototype.slice` for high-index array-like values (#411)
- Fix Object.isFrozen and Object.isSealed integrity checks (#412)
- Fix spyOn restore descriptor for re-spying (#397)
- Fix async parser scoping for default-parameter await (#393)
- Fix property redefinition rules for objects and arrays (#381)
- Handle EOF before REPL prompt on Windows (#385)
- Fix global descriptors for NaN, Infinity, and undefined (#380)
- Fix version constant propagation in CI build (#363)
- Fix BigInt Test262 compliance gaps (#361)
- Fix implicit Symbol→string coercion gap in ToStringLiteral (#357) (#359)
- Fix trailing-dot numeric literals and spread tokenization (#345)
- Handle Infinity in Number parsing tests (#331)
- Fix Temporal invalid-input errors to RangeError (#319)
- Fix lexically aware template boundaries and line terminators (#287)
- Fix nested backtick formatting in template literal docs (#284)
- Fix template escaping at interpolation boundaries (#278)
- Fixes tagged template object identity (#275)

### 📖 Other

- Embed IANA timezone data for Temporal (#440)
- Unify CLI JSON reporting (#433)
- Strengthen test262 runner and ECMAScript edge-case coverage (#402)
- Skip unsupported generator methods during parsing (#388)
- Infer names for anonymous classes and object functions (#387)
- Honor root config precedence for per-file CLI options (#382)
- Apply per-file config across all CLI apps (#373)
- Integrate REPL with CLI application framework (#372)
- Integrate JSX source map for coverage branch hit positions (#202) (#364)
- Recompute ZonedDateTime UTC offset after rounding (#358) (#362)
- Expose build date in version metadata (#360)
- Require BigInt for Temporal epoch nanoseconds (#346)
- Simplify hasOwnProperty shim binding (#347)
- Reject mixed stdout benchmark report formats (#340)
- Suppress progress output in structured and worker paths (#334)
- Restructure sources under source/ and rename build artifacts (#333)
- Validate Temporal.Duration field magnitudes (#328)
- Use shared Temporal property names in PlainDate parsing (#315)
- Use ConstructorToIndex for ArrayBuffer length (#316)
- Align error suggestions across builtins and runners (#308)
- Suppress CLI output from worker threads (#307)
- Deduplicate JSON string escaping helpers (#305)
- Docs, compiler type rules, and VM OP_CHECK_TYPE helper (#300)
- await-using tests: add microtask-boundary ordering assertions (#298)
- Recognize LS/PS as line terminators in lexer (ES2026 §12.3) (#285)
- Restructure built-in registration (#286)

### 🚀 Added

- Implement WeakMap and WeakSet built-ins (#437)
- Implement TC39 pattern matching (#438)
- Add generator and async generator support (#432)
- Add generated sources to CI FPC search path (#441)
- Add native TLS transport backends (#428)
- Support set-like operands for Set methods (#429)
- Implement smallestUnit, roundingMode, roundingIncrement for Temporal until/since (#424)
- Add receiver brand checks to Set/Map prototype methods (#421)
- Support destructuring into member expression targets (#413)
- Add fetch host allowlist support (#395)
- Add fetch host allowlist support (#391)
- Add opt-in `function` keyword support (`--compat-function`) (#390)
- Add unsafe Function constructor support (#386)
- Add `void` operator support (#389)
- Support array methods on array-like receivers (#383)
- Add compat var declarations (#368)
- Add Function.prototype.constructor per ES spec (#370)
- Implement Function.prototype.toString (#366)
- Add BigInt64Array and BigUint64Array support (#365)
- Add call stack depth limit with bytecode VM trampoline (#356)
- Add config file discovery for CLI apps (#353)
- Add GC memory ceiling and exposed counters (#351)
- Add nightly release and update artifact actions to v7 (#352)
- Add console output callback and remove Silence() anti-pattern (#350)
- Add instruction counter / interrupt handler with configurable limit (#349)
- Add `--unsafe-ffi` CLI toggle for FFI (#348)
- Implement BigInt spec support (#343)
- Add Object.hasOwn-backed hasOwnProperty shim (#344)
- Add Test262 bytecode support gaps (#341)
- Add Goccia.gc manual garbage collection hook (#342)
- Add fcl-net and OpenSSL to cross-build toolchain (#339)
- Add native fetch, Headers, and Response support (#338)
- Add CSV and TSV built-ins with module imports (#337)
- Enable strict types by default for bytecode executor (#336)
- Add default builtin shims for legacy globals (#330)
- Support ZonedDateTime relativeTo in Temporal duration ops (#329)
- Add Temporal.Duration.round support for relativeTo (#327)
- Add Temporal.Duration.prototype.total relativeTo support (#324)
- Add standalone GocciaBundler build target and CLI (#322)
- Support iterable inputs in Map.groupBy (#317)
- Add watchdog timeout to worker-thread test runs (#314)
- Add parallel execution for multi-file runners (#306)
- Add JSONC support for structured module globals (#304)
- Add explicit resource management support (#290)
- Add per-file timeouts and test262 harness updates (#297)
- Add support for RegExp modifier groups (#295)
- Implement Source Maps (TC39 Source Map v3) (#289)
- Support regex duplicate named capture groups (#291)
- Add JSON.rawJSON and JSON.isRawJSON (#288)
- Add dynamic import support to the runtime (#282)
- Implement TC39 Template Literal Revision for tagged templates (#281)
- Add encodeURI / decodeURI / encodeURIComponent / decodeURIComponent (#280)
- Add missing Temporal built-ins and options support (#277)
- Add URL and URLSearchParams built-ins (#273)
- Add ToObject coercion for primitives across all Object.* static methods (#271)
- Add Goccia.build platform metadata (#276)
- Add TextEncoder and TextDecoder built-ins (#272)
- Implement import.meta support (#265)
- Add tagged template literal support (#267)
- Add Float16Array and Math.f16round (ES2025) (#262)
- Add resizable ArrayBuffer support and transfer APIs (#266)
- Add support for class static blocks (#264)
- Add Object.getOwnPropertyDescriptors support (#261)
- Add logical assignment operators (#259)
- Add numeric separator support in number literals (#258)
- Add Uint8Array Base64 and Hex APIs (#260)
- Add Iterator.zip and zipKeyed helpers (#257)
- Add `atob` and `btoa` global functions (#256)
- Add reviver source context to JSON.parse and JSON5.parse (#255)
- Add Iterator.concat support (#253)
- Add RegExp.escape support (#252)
- Add top-level await and for-await-of support (#251)

## [0.6.1] - 2026-04-09

### 🏗️ Internal

- Strip symbols before staging build artifacts (#245)
- Extract shared ToPropertyDescriptor helper (#239)

### 🐛 Fixed

- Route Reflect.set receiver [[DefineOwnProperty]] through TryDefineProperty (#247)
- Thread optional receiver through Reflect.get and Reflect.set (#243)
- Fix Reflect.construct prototype setup (#241)

### 📖 Other

- Streamline project docs and contributor guidance (#250)
- VM improvements including fixing AArch64 Int64-to-Double conversion paths (#242)

### 🚀 Added

- Add automated changelog generation with git-cliff (#248)
- Add Error.cause support (#249)
- Support array-like arguments for apply and Reflect (#244)

## [0.6.0] - 2026-04-09

### ⚡ Performance

- Replace per-call dynamic array allocation with contiguous register stack (#235)

### ✨ Improved

- Include zero-hit executable lines in coverage reports (#219)
- Improve parser error context and diagnostics (#205)
- Make String.prototype.matchAll return a lazy iterator (#206)
- Merge GocciaScript metadata into `Goccia` global (#204)

### 🏗️ Internal

- Build 32-bit FFI fixture DLL on Windows CI (#227)
- Strip release artifacts and bake production version info (#211)

### 🐛 Fixed

- Fix YAML harness cleanup on parse failure (#218)

### 🗑️ Removed

- Remove test262 conformance job from CI (#234)
- Remove dead code and unused imports (#212)
- Remove unused multihelper mode switch (#213)

### 🚀 Added

- Add bytecode VM profiling support (#237)
- Add unsupported-CPU guard to build_target wrapper selection (#217)
- Add Reflect API (#228)
- Implement Proxy builtin and traps (#226)
- Add a test262 conformance harness (#222)
- Add explicit FFI support for native shared libraries (#209)
- Add `onTestFinished` cleanup hook to test framework (#223)
- Add mock() and spyOn() test APIs (#210)
- Add opt-in automatic semicolon insertion (#220)
- Implement full ECMAScript Unicode RegExp semantics (#208)
- Add named capture groups and Unicode sets flag for RegExp (#207)
- Add coverage reporting to ScriptLoader and TestRunner (#201)
- Add public `Goccia.semver` API namespace (#192)

## [0.5.1] - 2026-04-07

### ✨ Improved

- Report bytecode timing phases separately (#198)

### 🏗️ Internal

- Update repository skills (#199)

### 🐛 Fixed

- Handle mixed type-only imports and exports (#194)
- Unify newline handling for text file inputs (#193)

### 🚀 Added

- Add bytecode REPL timing mode (#200)
- Add CLAUDE.md symlink to AGENTS.md (#197)
- Add pull request template checklist (#196)
- Add default GitHub issue template (#195)

## [0.5.0] - 2026-04-06

### 🏗️ Internal

- Stage CI and release artifacts from top-level dpr binaries (#169)

### 🐛 Fixed

- Preserve UTF-8 parser inputs on Windows (#188)
- Normalize TOML multiline strings to LF (#186)
- Fix cross-build linking for RegExpr units (#180)
- Fix cross builds for fcl-base units (#171)

### 📖 Other

- Enforce private static brand checks (#168)

### 🚀 Added

- Add `.txt` and `.md` text asset imports (#190)
- Ignore leading shebang lines in the lexer (#191)
- Add JSON5 parsing and stringify support (#189)
- Add namespace imports for script and data modules (#185)
- Add TOML parsing, globals, and compliance checks (#183)
- Add Vitest lifecycle and structure APIs (#184)
- Add JSONL runtime parsing and module imports (#182)
- Allow string-literal module export names (#181)
- Add RegExp support and regex-aware expect.toMatch (#172)
- Add YAML parsing and module imports (#166)

## [0.4.0] - 2026-04-02

### ⚡ Performance

- Eliminate interpreter bridge for module loading in bytecode mode (#107)
- Eliminate interpreter bridge for async/await in bytecode mode (#110)
- Update hashmap hierarchy to improve performance (#66)
- Bypass interpreter bridge for blueprint-backed class construction (#87)
- Experiment: SOUFFLE_INLINE_STRING_MAX = 13 (#96)
- Optimize bridge cache: dirty flag and single-lookup reverse map (#89)
- Eliminate redundant OP_MOVE in plain function call compilation (#86)
- Promote 10 hot opcodes into main VM dispatch loop (#81)
- Extend constant folding with boolean NOT, typeof, and logical operators (#85)
- Pre-materialize constant pool into TSouffleValue array on template load (#82)
- Replace double-negation unary + with OP_RT_TO_NUMBER (#80)
- Mark SouffleIsTrue as inline for hot-path branch elimination (#73)
- Garbage collector memory optimization (#67)
- Replace TStringBuilder with TStringBuffer (#65)
- for of array fast path (#64)
- Promise and iterator optimisations (#58)
- Inline paths where possible (#43)

### ✨ Improved

- Refresh JS conformance suite and tighten runtime parity (#147)
- Expand JSON.stringify error coverage (#145)

### 🏗️ Internal

- Extract standalone module loader for module content providers (#162)
- Use range-based benchmark comparison in PR comments (#144)
- Test JSON output via public CLI surface (#142)
- Fold Souffle VM directly into GocciaScript (#137)
- Refactor built-in object model registration (#136)
- Convert spike PDFs to Markdown, update VM docs, add optimization log (#105)
- Set prototype.constructor on native blueprint construct path (#102)
- Unify blueprint bridge on ConvertBlueprintToClassValue (#100)
- High benchmark variance (#69)
- Update Github action steps to include test matrix for bytecode and interpreter (#68)
- Unify garbage collector (#60)
- CI job independence (#59)
- GocciaScript bytecode benchmarks in PRs (#55)
- Refactor dispatch evaluator (#51)
- De-couple control flow from exception flow (#45)
- Update documentation (#44)

### 🐛 Fixed

- Fix JSON stringify round-trip precision (#163)
- Fix module alias segment boundary matching (#149)
- Harden OrderedStringMap tombstone compaction (#148)
- Fix Failed Tests summary missing assertion failures (#111)
- Fix FPC 3.2.2 internal error on CI prod builds (#98)
- Fix benchmark result reporting (#62)
- Fix to resolve Windows 32-bit Out Of Memory benchmark tests (#57)
- Fix class expression super() resolution in bytecode mode (#56)
- Fix increment/decrement for computed member expressions  (#54)
- Fix Number.parseInt crash on Linux when radix is undefined or NaN (#48)
- Fix switch default clause not participating in fallthrough (#47)

### 📖 Other

- BenchmarkRunner: exit with code 1 on benchmark failures (#90)
- Pin materialized heap strings to prevent GC sweep during benchmarks (#92)
- Bytecode endianness portability (#72)
- First draft of WAM output through Souffle VM (#53)
- Strict types in bytecode VM (#42)
- More bytecode optimisation (#40)
- Feature parity between bytecode mode and interpreter mode (#39)

### 🗑️ Removed

- Remove unused typed local opcodes from compiler and VM (#74)
- Remove dead decorator class bridge code (#75)
- Remove FPC install from CI (#61)
- Remove type annotations on enum field level (#41)

### 🚀 Added

- Add Vitest-style matcher support for test assertions (#164)
- Add ScriptLoader import maps and goccia.json discovery (#151)
- Add nullish coalescing assignment operator (#146)
- Add ScriptLoader execution timeout support (#141)
- Add ScriptLoader CLI globals injection (#140)
- Allow CRLF in ScriptLoader CLI checks (#143)
- Add structured JSON output to ScriptLoader (#139)
- Support reading ScriptLoader source from stdin (#138)
- Add Souffle-value-backed Map/Set types and native callback fast paths (#109)
- Add record fast path for OP_RT_SET_PROP in VM dispatch (#108)
- Add cross-compilation toolchain workflow (#93)
- Add record/delegate fast path for OP_RT_GET_PROP in VM (#83)
- Add Tier 1 OP_NOT and OP_TO_BOOL opcodes for inline boolean coercion (#76)
- Enable autoInline optimization (#70)
- Add Performance built-in API (#63)
- Add SuppressOutput to TGocciaTestAssertions to silence expected failure messages in native Pascal tests (#49)
- Add bytecode and vm (#38)
- Add `[Shared]ArrayBuffer` and `TypedArray` implementations (#37)
- Add `async`, `await` and `for [await] ... of` (#36)

## [0.3.0] - 2026-02-23

### ⚡ Performance

- Disable runtime checks for production builds (#28)

### 🏗️ Internal

- Refactor: Split constants (#25)
- Print LOC and SLOC as part of the build process (#22)

### 🐛 Fixed

- Fix windows build on line breaks (#27)

### 📖 Other

- Enum support (#33)
- First draft of re-ordering maps functionality (#30)
- REPL History + Color formatting (#19)
- fix import clauses (remove or move from `interface` to `implementation`) (#18)
- JSX parsing as source-to-source pre-pass (#16)
- Warning when using unsupported features (#15)

### 🗑️ Removed

- Remove the usage of interfaces (#21)

### 🚀 Added

- Add decorators (#35)
- Add `Map#getOrInsert[Computed]`, `Error.isError`, `Math.sumPrecise` (#34)
- Add wildcard, function, loose equality and label to unsupported features (#32)
- Add command-line flags to TestRunner (#31)
- Create BindingMap for Scope and pre-typed array callbacks (#29)
- Add `stucturedClone` (#24)
- Add tests for nested loops (#26)
- Add stack property for errors (#23)
- Add iterators (#20)
- Add module resolver (#17)

## [0.2.0] - 2026-02-19

### ⚡ Performance

- Switch to microsecond timing and improve benchmarks (#1)

### 🏗️ Internal

- Update ci.yml (#4)

### 🐛 Fixed

- Fix edge cases for special values (#11)
- Fixes Temporal Duration total edge case and GC collection (#10)
- Fix test suites for further ECMAScript compatibility (#5)

### 📖 Other

- Type annotations as comments (#14)
- Named exports modules (#13)
- Refine code style guide and auto-fix files (#7)
- Promises (#2)

### 🚀 Added

- Add code convention on abbreviations (#12)
- Add native tests for TestAssertions (#9)
- Add workspace settings and extensions for VSCode (#8)
- Add `queueMicrotask` functionality (#6)
- Add Temporal API (#3)

## [0.1.0] - 2026-02-17

### ⚡ Performance

- Optimize GC membership checks from O(n) to O(1) with TDictionary
- Replace direct TGocciaScope.Create with CreateChild
- Optimize arrays, functions, scope & benchmark
- Inline hot paths

### ✨ Improved

- Make benchmark params configurable via env vars
- Improve module test comments with path resolution issue
- Make property access virtual and document accessors
- Improve `Number` built-in static methods
- Improve exception handling for test runner

### 🏗️ Internal

- Update build.pas
- Update AGENTS.md
- Update documentation
- Refactor: add RunScripts and centralize entrypoint
- Update environment variable (skip 64-bit windows build for now)
- Update Goccia.Values.FunctionValue.pas
- Update documentation
- Extract ParseObjectMethodBody helper in ObjectLiteral
- Extract shared ParseGetterExpression/ParseSetterExpression in parser
- Extract SpreadIterableInto helpers to deduplicate spread expansion
- Extract CreateArrayCallbackArgs helper for array iterator methods
- Extract EvaluateSimpleNumericBinaryOp for subtraction, multiplication, power
- Extract MarkPropertyDescriptor helper in GCMarkReferences
- Extract shared ProcessEscapeSequence in lexer
- Extract ParseBinaryExpression to deduplicate 11 binary operator parsers
- Extract EvaluateStatementsSafe to eliminate duplicated exception handling
- Update logo.png
- Update documentation
- Update build.yml
- Update README.md
- Update build.yml
- Update build.yml
- Refactor error handling; remove debug logging
- Update build.yml
- Update error handling/throwing
- Update object creation test to be ECMAScript compatible
- Update `toThrow` assertion to compare error class names
- Update array link in global object
- Update comparisons
- Update compound-arithmetic.js
- Refactor TGocciaArguments to TGocciaArgumentsCollection
- Refactoring
- Update function and class calls
- Refactor Number values and string prototype built-ins
- Update `String.prototype` boxed methods
- Refactor method calls
- Update test debug output
- Update ty-catch implementation
- Update Goccia.Values.FunctionValue.Test.pas
- Refactor native function to reduce duplication
- Update `Math.clamp`
- Update class instantiation
- Update template evaluation and property initialization
- Update test cases
- Update expression tests
- Update tests
- Update tests and testrunner
- Update Github actions
- Update example.js
- Update initial test suite
- Update built-in constructor signature
- Update class definitions and class expressions
- Update compound assignments
- Update logger
- Update tests and script loading functionality
- Update FunctionTest
- Update workflow
- Update build and build-and-run commands
- Update gitignore

### 🐛 Fixed

- Fixes method definitions
- Fixes native test suite
- Fixes spread operator test throwing correct type error
- Fixes string conversion for arrays
- Fixes `charAt` and `charCodeAt` string tests
- Fixes property access on string boxing with shared prototypes
- Fixes compound assignments for arrays
- Fixes number literal to string literal conversion
- Fixes boolean literal to string literal conversion
- Fixes Math tests
- Fixes comparison with special values
- Fixes `ToString` calls
- Fixes getter and setter pair definition in objects
- Fixes class inheritance
- Fixes TestRunner access violation if there are no tests declared
- Fixes incorrect test expectation
- Fixes if/else comparison tests
- Fixes `Object` built-in methods
- Fixes signed zero and NaN calls in same value comparison
- Fixes NaN assignments
- Fixes `Math` methods with full IEEE 754 compatibility
- Fixes `Object.defineProperties`
- Fixes `Object.create`
- Fixes edge cases with `JSON.stringify` and `undefined`
- Fixes `.not` property for `expect(...).not`
- Fixes property order in spread operator
- Fixes an issue where `const x = undefined` would fail
- Fixes property sort order in spread operators
- Preserve property order
- Fixes `instanceof` against prototype chain
- Fixes floating point number literal parsing regression
- Fix typo
- Fixes double method call evaluation
- Fixes `Array.prototype.reduce`
- Fixes `Array.prototype.toSorted`
- Fixes `Array.prototype.pop`
- Fixes `Array.prototype.includes`
- Fixes output for template string test
- Fixes unary operators
- Fixes `toBeNaN` assertion
- Fix typo
- Fix issues for Math built-in funcitons
- Fix logical or and logical and
- Fixes issues in logger
- Fixes scope handling
- Fixes arrow functions
- Fixes `#Array.map` call
- Fixes constructor calls and prototype allocation
- Fix function tests
- Fix Integer Expect generic
- Fix typo

### 📖 Other

- Use VMT methods for type checks
- Rename variancePct to variancePercentage
- Split out methods, class methods and arrow functions explicitly
- Move basic example into examples folder
- perf: use singleton values throughout the codebase instead of allocating
- docs: document Boolean singleton, fn.apply fast path, and Array TStringBuilder optimizations
- perf: use TStringBuilder in Array.ToStringLiteral
- perf: fast path in fn.apply for TGocciaArrayValue arguments
- perf: return singleton ZeroValue/OneValue from Boolean.ToNumberLiteral
- Use shared prototype
- Use StringBuilder instead of string concat
- Address TODO comments: fix, improve, or resolve 11 items
- Consolidate Engine built-in registration methods
- Share string prototype singleton across all string boxing
- Standardize error handling: use ThrowTypeError in destructuring
- Simplify Engine singleton pinning and destructor cleanup
- Use TStringBuilder for template literal evaluation
- Simplify GetProperty/SetProperty calls
- refactor: standardise argument validation in TestAssertions
- perf: use TStringBuilder for string and template literal scanning
- perf: use TDictionary for keyword lookup in lexer
- fix: isolate module scope so module variables don't leak into global
- refactor: add virtual GetProperty/SetProperty on TGocciaValue base class
- refactor: extract CopyStatementList helper in evaluator
- refactor: remove DefineBuiltin legacy method from TGocciaScope
- refactor: remove unused IPropertyMethods and IIndexMethods interfaces
- refactor: extract ParseParameterList in the parser
- refactor: eliminate GlobalEvaluationContext mutable global state
- refactor: remove legacy object evaluation path in EvaluateObject
- refactor: extract GetPropertyFromValue/SetPropertyOnValue/DefinePropertyOnValue helpers
- Trying to get Windows64 build to work
- 32-bit compatibility and compile x64 on Windows
- Delete BenchmarkRunner
- Use RuntimeCopy mechanism instead of marking values permanent in the GC
- Rename Goccia.GC to Goccia.GarbageCollector
- Spread iterables in tests; disable Vitest watch
- Delete WARP.md
- Try/finally fix; function prototype and parser updates
- Simplify Number builtin to use FBuiltinObject
- Updated documentation
- Uses `TGocciaArgumentValidator` for global object and array
- Uses `TGocciaArgumentValidator` where possible
- Use matrix for os and arch
- Uses `TGocciaArguments`
- Introduce TGocciaEngine
- Restructure type coercion
- More refactoring
- Restructure primitives
- Updates test files
- Add` toHaveLength` support for arrays, objects and strings
- Skip tests for unimplemented features
- Merge and fix getter and setter tests
- Merge `defineProperty` and `defineProperties` tests
- Hack to fix `this` in scope
- Separate between variable declarations and destructuring assignment
- Use `Number` static methods `parseInt`, `parseFloat`, `isNaN` and `isFinite`
- Track insertion order for `RegisterNativeMethod` and `RegisterContant`
- Adds `delete` keyword
- First round of TGocciaScope refactors
- Restructured Evaluator calls
- Updates to property descriptors
- Simplify compound operation
- Initial try-catch implementation
- Workaround for closure capturing
- Parser support for for statements
- Adding initial getter and setter support
- Adding spread operator
- Restructure object tests
- Updated computed properties
- Simplify array includes checks
- Removed unnecessary casts to Array
- Simplify function calling further
- Simplify function calling
- Restructure object tests
- Further test fixes
- Build in silent mode
- Updates tests
- Delete TestUtils.pas
- Expose built-in objects from the interpreter
- Simplify valid identifier check
- Adds native testing library
- Initial version of test handlers
- Added `instanceof`
- Spec compliance
- Small fixes
- Simplify MethodValue based off FunctionValue
- Restructured interpreter
- Move built-ins into separate units
- Cursor rules
- Dump
- Initial commit

### 🗑️ Removed

- Remove dead code: TGocciaGlobalScope, TGocciaClassScope, unused tokens
- Remove duplicate AssignComputedProperty function
- Remove backwards compatibility in variable declarations
- Remove `substr` as we won't implement deprecated functions
- Remove `runTests` call

### 🚀 Added

- Add build clean target; simplify FunctionValue logic
- Add PR workflow and expand CI pipeline
- Add dev/prod build modes and update CI
- Add benchmark reporter and refactor runner
- Add variance and microsecond timing to benchmarks
- Add typed scope classes and keyword constants
- Create private-fields-in-callbacks.js
- Add Number shared prototype
- Add class example from README
- Add edge case tests for try-catch-finally, bitwise, optional chaining
- Add embedding guide
- Add benchmarks/tests; fix class new/super handling
- Add logo
- Add more tests
- Add first draft of GarbageCollector
- Add benchmark runner and CI integration
- Add gitattributes; display fail builds; normalize CRLF
- Add Array/Object/Number features, ToPrimitive & tests
- Add JS tests and implement optional/rest/escapes
- Add JS built-in tests and update engine behavior
- Add Symbol, Set, and Map built-ins
- Support Unicode escapes and update docs
- Support private class members and init order
- Add shortcut to access prototype in classes / class constructors
- Add Typeof for FunctionBase
- Create CLAUDE.md
- Implement `Array.prototype.slice`
- Implement `toBe` and `toEqual` to use strict equality and deep equality respectively
- Add further test assertions for `JSON.stringify`
- Add guard rails for `AssignProperty` and `DefineProperty`
- Add property descriptors
- Add `String.prototype` built-in methods
- Add skip test tracking
- Add global number object
- Add more Object built-in methods
- Allow destructuring assignment for function parameters
- Add test for block scope definition
- Add parser support for `switch - case` statements
- Add parser support for `while` and `do while` loops
- Add `in` operator
- Add destructuring syntax
- Add nullish coalescing
- Implement `SetProperty` in TGocciaArrayValue
- Allow for object methods
- Add `Array.prototype.flatMap`
- Add test for creating nested arrays
- Add `Array.prototype.toSpliced`
- Add `Array.prototype.flat`
- Add computed and compound property assignments
- Add test for `Array.prototype.filter`
- Add support for sparse arrays
- Implement `fromIndex` for `Array.prototype.includes`
- Add tests for Array prototype functions
- Implement default parameters
- Add bitwise and bitwise compound operators
- Add private static fields
- Add support for multiple variable declarations
- Add template string expressions
- Add support for simple template parsing
- Add check for creating string literal
- Add more unicode tests
- Allow unicode characters
- Add more built-in functions
- Add edge caes to `Math.abs`
- Add more test assertions
- Allow compound expressions for private fields
- Add TestRunner
- Add more tests
- Allow testparam to show exit on failure and show test results
- Allow for private properties and private methods
- Add JSON parser and stringifier
- Add more built-in functions for Array
- Add more `Math` built-in functions
- Add block comments
- Add remaining assignment operators
- Add increment and decrement
- Add instance properties
- Add support for static properties
- Add support for static methods
- Add support for `super`
- Add typeof handling
- Add and update built-in methods
- Add assertion checking for test suites
- Add helper function to help with undefined casting
- Add ExitCode functionality for CI/CD
- Add Function Test
- Add ObjectValue test
- Add fpc installation to workflow
- Add tests and test runner
- Add Github workflow
- Add optional value
- Add `TObjectValue.HasOwnProperty`
- Add logging


