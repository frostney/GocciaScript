# Changelog

All notable changes to GocciaScript are documented in this file.

## [0.15.0] - 2026-10-22

### ⚡ Performance

- perf(compiler): a branch condition that is a call tests the result where the call left it (#1887)
- perf(compiler): let and const bindings set their TDZ hole once, and an initializer that cannot reach its binding writes it directly (#1882)
- perf(compiler): test &&, ||, ! and strict null or undefined comparisons in a branch condition as jumps (#1881)
- perf(compiler): assignments, compound assignments and calls write registers directly instead of through copies (#1885)
- perf(array): element stores write dense slots directly, and fill a hole when no prototype can intercept it (#1884)
- perf(vm): a[k] op= v and a[k]++ keep an Int key instead of converting it to a string (#1880)
- perf(compiler): a counted for loop whose body creates no closure uses its counter as the iteration binding (#1883)
- perf(vm): constructing an instance of a class without private names skips the brand-stamping pass (#1879)
- perf(vm): classify an arithmetic result as int32 or float without calling IsNaN, IsInfinite and Frac (#1877)
- perf(vm): the bytecode dispatch loop uses no goto, so every target compiles one dispatch path (#1859)
- perf(gc): reserve and release external bytes without an exception frame (#1863)
- perf(vm): poll a timeout and an instruction limit without repeated thread-variable lookups (#1862)
- perf(proxy): a Proxy records [[Call]] and [[Construct]] when it is created, so a call through a chain of Proxies is linear in its depth (#1810)
- perf(engine): top-level declarations parse, compile and link in linear time (#1789)
- perf(vm): decode the opcode once per instruction and widen operands only after an OP_WIDE prefix (#1698)
- perf(runtime): allocate number boxes, property descriptors and class-instance properties without implicit exception frames (#1697)
- perf(sandbox): fs.readFileSync, fs.writeFileSync and the sandbox diff stop copying and comparing whole files a byte at a time (#1732)
- perf(gc): register each allocated value with one thread-variable lookup and an inline list append (#1700)
- perf(typed-array): set, slice, copyWithin and the copying constructor move same-type data as one block (#1659)
- perf(text): TextEncoder.encode and string file writes size their UTF-8 output once, 8-15x faster on large strings (#1691)
- perf: for-await-of over arrays and async generators runs 2–5x faster in bytecode mode (#1621)
- perf(vm): private fields and private methods run in about half the instructions in bytecode mode, now ahead of the interpreter (#1622)
- perf: spreading an array into a new array runs faster in bytecode than in the interpreter (#1619)
- perf(eval): a direct eval that repeats its source reuses its compiled code instead of keeping a new copy per call (#1617)
- perf: cut empty-script boot by 17% in both modes, so bytecode boots below the interpreted default on main (#1613)
- perf(compiler): keep constant folding in sloppy functions without direct eval, and give each eval call an OP_DIRECT_EVAL site (bytecode v91) (#1600)
- perf(engine): cut engine boot by about 4%, so bytecode mode boots in fewer instructions than interpreted mode on main (#1483)
- perf(regexp): give global replace, match, split and matchAll a fixed, lower cost per match (#1417)
- perf(runtime): read one thread-local word per allocation and none per bytecode call for the checks that are normally off (#1397)
- perf(vm): set up and tear down a bytecode call frame in a third of the instructions (#1362)
- perf(runtime): validate a shape, allocate a value and check the stack depth without an exception frame (#1339)
- perf(vm): serve a prototype-cached property read without recomputing shapes or walking the class chain (#1372)
- perf(compiler): read let bindings and parameters from their registers as operands (#1367)
- perf(builtins): read a string primitive's method without boxing it, and format integer keys as UTF-16 (#1351)
- perf(vm): decide === and !== on the registers instead of materializing both operands (#1343)
- perf(vm): apply bitwise and shift operators to a non-int32 number without boxing it (#1338)
- perf(vm): read, write and iterate array and typed-array elements directly (#1312)
- perf(vm): move a value into a register without walking its class hierarchy (#1318)
- perf(vm): halve the cost of native calls and cut FFI call and struct field overhead (#1317)
- perf(vm): resolve the execution context stack once per call instead of nine thread-local lookups (#1313)
- perf(vm): read top-level bindings and bytecode constants in place, and propagate top-level const values (#1319)
- perf(vm): read an upvalue without copying its descriptor (#1316)
- perf(compiler): use initialized const locals as operands without copying them (#1314)

### ✨ Improved

- Make the console-rendering and config-globals CLI checks pass on Windows and macOS, turning main green (#1594)

### 🌐 Website

- docs: README and site lead with what GocciaScript is for and the evidence (#1835)
- fix(website): the site describes one bytecode engine and keeps the mode toggle only for releases up to 0.14 (#1662)
- fix(website): build the site when docs/ or README.md change, so docs-only merges reach gocciascript.dev (#1597)

### 🏗️ Internal

- test(thread-polls): read the poll states through a pointer, so i386 at -O2 calls the slow path with the right state (#1878)
- ci(gc-stress): the weekly GC-stress job covers every tests/ file, with large cases sized down under stress (#1865)
- test(cli): config priority checks fail when TOML loses, and shard merge rejects non-bytecode reports (#1858)
- chore(skills): sync provisioned skills, adding capability tiers to deliver and milestone-rush (#1842)
- chore(skills): sync provisioned skills (#1808)
- chore(test262): bump pin to d53ebc8b (#1806)
- test(snapshot): bound the exercise-reach check by instructions (#1828)
- test(snapshot): hardening for the heap snapshot reader, with full sweeps and heaptrc in the scheduled fuzz workflow (#1823)
- docs(adr): ADR 0148, a paused run's heap can be snapshotted at an idle point and restored in a fresh engine (#1795)
- docs(agents): CLAUDE.md imports AGENTS.md with @AGENTS.md instead of a symlink (#1793)
- ci(memory-safety): suppress memcheck's report on the runtime library's aligned over-read in IndexByte and IndexWord (#1780)
- docs: the docs describe one bytecode engine, and ADR 0138 records the interpreter's removal (#1661)
- refactor(engine)!: embedders create an engine without choosing an executor (#1656)
- refactor(engine)!: delete the tree-walk interpreter, so every binary is about 575 KB smaller (#1649)
- refactor(engine): the evaluation context, decorators and deep equality move out of the tree-walk evaluator's namespace (#1645)
- ci: a weekly GC-stress run reports tests that fail only when the VM collects every 8 instructions (#1638)
- refactor(engine): function and generator values no longer need the tree-walk evaluator, and bytecode array callbacks run fewer instructions (#1639)
- chore(skills): sync provisioned skills (#1610)
- chore(tc39-mcp): bump pin to v0.6.5 (#1609)
- chore(test262): bump pin to 2e0a5676 (#1606)
- ci: cross-compile the Linux and Windows targets on Linux runners, halving full CI's macOS runner time (#1603)
- test(ffi): stop the callback-slot exhaustion test failing when another FFI test holds a callback at the same time (#1599)
- docs(workflow): dispatch full CI for test-cli checks that compare output text or reported paths (#1596)
- docs(adr): accept compiling direct eval as a closure of its call site, after completing the bridge's call-site record first (#1577)
- chore(skills): sync provisioned skills, adding the green default-branch gate to deliver and git-workflow (#1507)
- chore(skills): sync provisioned skills (#1502)
- ci(nightly): check every three hours and publish the newest passing build when it is ahead of the last nightly (#1371)
- chore(skills): sync vendored skills so the CodeRabbit adapter gates each head on its own statuses (#1341)
- chore(repo): stop tracking the agent handoff file (#1335)
- test(modules): fail when a data-module loader reads a provider file without verifying it (#1328)
- chore(timezone): bump data to 2026e (#1301)
- ci(nightly): expect the macOS nightly archives as .zip, which is what packaging produces (#1296)
- chore(skills): sync known-good-route skills so the CodeRabbit adapter budgets reviews and recognises finished ones (#1297)
- docs: link known-good-route rules instead of restating them, and gate platform-sensitive changes on full CI (#1292)
- ci: stop a hung test job or superseded main run from holding macOS runners for hours (#1291)

### 🐛 Fixed

- fix(windows): file names and printed text outside the ANSI code page survive, so sandbox search passes on Windows (#1889)
- fix(array): an index store reaches the prototype chain exactly when the array does not own the index (#1888)
- fix(fetch): charge response bodies to the engine memory limit (#1874)
- fix(ci): skip PR report and comment jobs in a cancelled run (#1857)
- fix(fetch): honour the redirect option and fail on a 21st redirect (#1872)
- fix(engine): own properties named like private storage keys enumerate and answer `in` like any other (#1864)
- fix(compiler): a var in a class static block is scoped to that block (#1861)
- fix(test): load CLI globals modules before running tests (#1855)
- fix(array): use undefined for omitted with arguments (#1856)
- fix(test): round-trip module snapshots without restore failures (#1850)
- fix: scope fetch credentials across redirects (#1846)
- fix(math): preserve the full Number range in Math.round (#1841)
- fix(gc): root number receivers, Proxy trap receivers, primitive wrappers and key lists that builtins read after guest code (#1834) (#1844)
- fix(tls): SChannel keeps its extra input instead of copying it from freed memory, and TLS sends end by the request's deadline (#1840)
- fix(fuzz): afl-fuzz gets coverage from an afl-as instrumented harness, so its queue grows past the seeds (#1839)
- fix(fetch): a write to a reset connection fails the fetch instead of ending the process with SIGPIPE (#1838)
- fix(gc): builtins root what they made or read before a collection can free it, and a second import of an errored module no longer rethrows a freed error (#1833)
- fix(fuzz): the fuzz harness shuts its runtime down before it exits, so the heap snapshot replay ends with nothing allocated (#1836)
- fix(engine): every public evaluation entry establishes the floating-point mode (#1825)
- fix(regexp): compiling alternatives and named groups costs time linear in the pattern (#1826)
- fix(snapshot): the heap snapshot reader builds for 32-bit targets again, and no allocation sized from its input can wrap there (#1832)
- fix(sandbox): a --deterministic run stamps its files at the epoch, and an embedder can give the sandbox its clock (ADR 0151) (#1830)
- fix(url): the URL parser percent-encodes non-ASCII code points as UTF-8 (#1820)
- fix(function): a bound function's prototype is the target's [[GetPrototypeOf]] (#1812) (#1819)
- fix(fetch): a response body cut short is a network error, and every TLS call ends at the request's deadline (#1813)
- fix(fetch): a redirect's Location resolves against the response's URL and the request URL sent has no dot segments (#1809)
- fix(threading): a worker the watchdog abandoned takes no further file and records nothing (#1801)
- fix(fetch): Response.url is the URL serialized without its fragment, on the built-in client and a transport (#1804)
- fix(vm): a throw leaving the VM through native code no longer runs the error's name and message getters (#1803)
- fix(memory-safety): the scheduled heaptrc and memcheck steps fit their limits: no per-character or per-digit allocations, single-worker memcheck shards (#1802)
- fix(atomics): a wait that Atomics.notify counted returns "ok" even when its timeout passed first (#1797)
- fix(gc): a small live set collects as its heap grows under a large --max-memory ceiling, and dropped new Array(n) storage reaches the heap trigger (#1772)
- fix(gc): marking no longer recurses once per object, so a collection over a deep chain cannot run out of native stack (#1792)
- fix(shadowrealm): an early error keeps its message in the caller realm's SyntaxError (#1791)
- fix(cli): --print and --output=json read the last value while its engine is alive (#1775)
- fix(engine): a growable SharedArrayBuffer keeps one data block, so a grow loses no agent's writes (#1783)
- fix(runtime): TextDecoder.decode and TextEncoder.encodeInto refuse resizable and growable buffers, as Web IDL requires (#1778)
- fix(atomics): a wait interrupted by a timeout leaves the waiter list as it found it (#1779)
- fix(parser): a line break between yield and *, before =>, or after a method's async is checked (#1773)
- fix(modules)!: an entry with import or export runs as a module, so a cycle through it evaluates each module once (#1683)
- fix(semver): an empty comparator carries the options it was given instead of reading uninitialised memory (#1774)
- fix(engine): a run frees everything it allocated, so the scheduled memory-safety job's heaptrc step passes and its Valgrind step runs (#1722)
- fix(runtime): typed arrays hand native readers the current data block, and Atomics revalidates after converting its arguments (#1751)
- fix(date): toString and toTimeString write a local mean time offset as hours and minutes (#1728)
- fix(temporal): named time zones keep daylight saving time after 2037 by following the TZif footer rule (#1733)
- fix(cli): a sandbox run no longer copies its last value out of the engine unless --print asks for it (#1766)
- fix(date): dates near the time value range keep their valid instant off UTC (#1729)
- fix(vm): a call that runs guest code stores its result where the register stack now is (#1711)
- fix(parser): with --compat-asi, return, throw and yield keep a template or string that spans lines (#1760)
- fix(modules): reloading a changed module again no longer crashes the host, and old functions keep their version (#1669)
- fix(regexp): v-flag classes follow the ClassSetExpression grammar, so syntax characters, doubled punctuators and range operands of && or -- are a SyntaxError (#1762)
- fix(parser): deeply nested source, long operator and call chains, and deeply nested RegExp patterns throw a SyntaxError instead of crashing (#1664)
- fix(parser): accept as, from and async as labels, and let and static in sloppy code (#1759)
- fix(date): Date.parse reads legacy date strings without an offset as local time (#1720)
- fix(ffi): variadic calls on Windows ARM64 pass floats and structs in integer registers, so the callee reads the right values (#1718)
- fix(cli): Windows trust saves no longer race on a new store directory, and full CI runs the source/app Pascal tests on every target (#1721)
- fix(cli): `--output=json` keeps an empty last console line in `output` instead of dropping it (#1758)
- fix(modules): defining a property through a failed deferred namespace no longer leaks its descriptor (#1756)
- fix(compiler): a top-level `const [a, b = a] = [1]` gives b = 1 instead of throwing a ReferenceError (#1755)
- fix(parser): accept `from` and `as` as names in destructuring binding patterns (#1752)
- fix(gc): scripts whose live data sits close to the --max-memory ceiling run at normal speed instead of collecting at every checkpoint (#1682)
- fix(reflect): Reflect.getOwnPropertyDescriptor and Proxy defineProperty trap descriptors no longer run Object.prototype setters (#1757)
- fix(decorators): a decorated class defined inside a decorator no longer ends the outer class's decoration (bytecode v95) (#1684)
- fix(bigint): ToDouble keeps the full mantissa when built with FPC 3.2.4 (#1709)
- fix(sandbox): a whole-file write holds exactly its bytes instead of the next power of two (#1743)
- fix(sandbox): fs writes a TypedArray's or DataView's byteLength bytes, not one byte per element (#1731)
- fix(regexp): exec, test, search and match no longer charge the subject again per match (#1753)
- fix(proxy): getOwnPropertyDescriptor and defineProperty invariant checks no longer read a target descriptor freed by guest code (#1754)
- fix(runtime): decode atob in linear time and memory (#1742)
- fix(engine): a fetch awaited after a top-level await that settles at once completes instead of ending the run silently (#1702)
- fix(decorators): addInitializer throws a TypeError instead of crashing when called after its decorator returned, and for a non-callable argument (#1671)
- fix(engine): Array, Set, RegExp, Proxy.revocable, TypedArray and Math built-ins no longer use freed values after a collection, and FinalizationRegistry.unregister finds a collected registration (#1701)
- fix(decorators): a decorator on a private method, getter or setter gets the element and its replacement takes effect (#1650)
- fix(function): bind keeps its bound this and arguments when the target's length or name getter collects (#1713)
- fix(modules): a collection during an import cycle no longer fails the module with a TDZ ReferenceError (#1695)
- fix(fetch): concurrent first HTTPS requests on Linux no longer fail with "HTTPS requires OpenSSL" (#1694)
- fix(engine): an anonymous class with extends in a block no longer overwrites a later variable, so a closure sees the class it was assigned (#1675)
- fix(engine): a callable Proxy as toPrimitive, valueOf, a typed-array callback or a decorator no longer crashes, and deep Proxy/bound-function chains throw RangeError (#1663)
- fix(typed-array): slice throws when its species result views the source's own immutable buffer (#1689)
- fix(parser): ternaries whose branches are arrows returning parenthesized objects parse, so prettier 3.9's standalone bundle compiles (#1660)
- fix(engine): constructing the last of 30,000 chained subclasses no longer crashes in bytecode mode (#1677)
- fix(engine): Iterator.zip and zipKeyed no longer crash or pad with freed values, and structuredClone gives each getter result its own clone, when guest code collects (#1658)
- fix(test262): negative parse and resolution tests pass only when the engine rejects the source before running it (#1670)
- fix(engine): disposeAsync, object literals with a computed __proto__ key, and assignments that first touch a legacy member no longer crash when a collection runs (#1696)
- fix(fetch): host names resolve through the system resolver on Linux and macOS, so /etc/hosts names and IPv6-only nameservers work (#1681)
- fix(modules): a module-source entry waits for a dependency's top-level await in bytecode mode (#1654)
- fix(vm): adding or comparing an Int16 immediate to a proven Number holding NaN, an infinity or -0 no longer aborts the process (#1686)
- fix(engine): builtins survive a collection during startup, and Iterator.concat, Iterator.from and three other built-ins no longer use freed values when a getter, trap or shim load collects (#1678)
- fix(modules): export * from the entry in an import cycle forwards the entry's own exports in bytecode mode (#1653)
- fix(engine): for await no longer calls return() when next() throws or rejects in bytecode mode (#1643)
- fix(promise): Promise.try and finally on a subclass no longer crash or hang when guest code they run collects (#1630)
- fix(gc): an over-budget --max-memory run refuses with a catchable RangeError again on development builds (#1632)
- fix(engine): an async function survives a collection while await reads the awaited value's then (#1627)
- fix(engine): a thrown error survives a collection in the iterator return() a built-in calls before rethrowing it (#1631)
- fix(vm): a long-running bytecode script no longer grows by about 64 bytes for every call that captures a variable (#1624)
- fix(modules): a module's function declarations run as compiled bytecode instead of tree-walking on every call (#1623)
- fix(vm): `#m in C` is true for C's own static private methods and accessors in bytecode mode, in and out of eval (#1616)
- fix(engine): stop failing a module with the rejected promise its last statement leaves, so the max-memory sweep reports a RangeError in both modes (#1614)
- fix(vm): throw ReferenceError when bytecode reads a script var whose configurable global property was deleted (bytecode v94) (#1611)
- fix(vm): let bytecode direct eval see earlier eval vars, class-body strictness and TDZ conflicts, and its caller's bindings after the call returns (#1586)
- fix(eval): run eval found on a with object, or a saved eval after the global is replaced, as a direct eval (#1598)
- fix(errors): locate a call at its callee's name and a failed property read at its property, as V8 does, and a missing module at the import that asked for it (#1579)
- fix(vm): count constructor, async, generator and accessor calls against --max-stack in bytecode, and stop them before the native stack runs out (#1482)
- fix(cli): stop the test runner leaking its discovered file list, removing the runner's share of the memory-safety heaptrc report (#1593)
- fix(vm): let a nested class's field initializers read the outer class's private names in bytecode mode (#1591)
- fix(regexp): keep quantifier bounds above 1,000,000 instead of clamping them, and reject counts no program can hold (#1590)
- fix(runtime): clone typed arrays, DataView, Date, RegExp, primitive wrappers and DOMException as their own kinds in structuredClone (#1576)
- fix(vm): run a built-in subclass's own set/add from super(iterable) and give it the built-in's prototype when new.target has none (#1588)
- fix(bytecode): carry a closure's write to a traditional for loop's let binding into the next iteration (#1585)
- fix(bytecode): throw ReferenceError when a default parameter initializer reads or assigns its own parameter (#1584)
- fix(compiler): run static field initializers as methods of the class in bytecode mode (#1434)
- fix(runtime): resolve a private name through the class body the code is in, in nested functions, computed methods and object-literal methods (#1433)
- fix(regexp): match a greedy single-character loop over more than 10 million characters, and report the step limit with its own message (#1458)
- fix(regexp): reject a pattern whose compiled program does not fit the VM's 24-bit operands instead of truncating jump targets (#1431)
- fix(runtime): read an ArrayBuffer's and a SharedArrayBuffer's byteLength, maxByteLength, resizable, detached, immutable and growable through its prototype chain (#1438)
- fix(modules): keep a module's value exports alive without its namespace object (#1573)
- fix(intl): stop later engines on a thread from reading a freed Intl.Segmenter toStringTag (#1572)
- fix(vm): locate each bytecode caller frame at the call it is making, and report a thrown error's line and column in JSON (#1496)
- fix(vm): stop a collection during a numeric self-recursion from following freed objects in bytecode mode (#1454)
- fix(runtime): keep an auto-accessor's value in a private name and initialize it in order with fields and static elements (#1426)
- fix(bytecode): keep a parameter or let/const when a sloppy block function shares its name (#1493)
- fix(runtime): take Object.prototype.toString's builtin tag only from the internal slots ECMA-262 lists (#1437)
- fix(vm): stop a --max-stack=0 recursion with peak memory near --max-memory instead of 2.2 times it (#1506)
- fix(errors): keep a stack trace to its innermost 100 frames so a 2M-frame RangeError is built in 0.56 s instead of 15 s (#1505)
- fix(gc): keep garbage near the --max-memory ceiling instead of letting it reach about six times the ceiling (#1471)
- fix(test-cli): stop the entry self-import test from failing on macOS, where the runner names the temp directory /private/var (#1499)
- fix(runtime): read a Proxy target's keys once, after IsExtensible, in the ownKeys invariant check, so d nested ownKeys traps run 2^d - 1 times instead of (3^d - 1)/2 (#1495)
- fix(runtime): throw RangeError instead of crashing when deeply nested Proxies forward any internal method (#1432)
- fix(bytecode): start every function-body var as undefined instead of a surplus argument or a destructuring temporary (#1492)
- fix(bytecode): give a body var named like a parameter its own binding when the parameter list has a default (#1489)
- fix(compiler): keep an imported binding visible to later inputs in the bytecode REPL (#1361)
- fix(vm): give -0 for an integer product of zero and a negative number (#1344)
- fix(runtime): give a structuredClone object clone Object.prototype as its prototype (#1422)
- fix(bytecode): give an imported module's top level the whole --max-stack limit (#1486)
- fix(compiler): enforce inferred `let` types only for literal initializers under --strict-types in bytecode mode (#1363)
- fix(bytecode): name the binding in TDZ and const-assignment errors, as the interpreter does (#1450)
- fix(runtime): list a function's or class's re-created length or name after the keys that existed before it (#1428)
- fix(runtime): list typed array elements in JSON.stringify and compare them in toEqual and toStrictEqual (#1424)
- fix(bytecode): assign a var initializer inside catch (x) to the catch parameter it redeclares (#1488)
- fix(bytecode): apply a compound assignment's operator to a const before throwing the assignment TypeError (#1449)
- fix(modules): evaluate an entry module once when it imports itself or sits in an import cycle (#1484)
- fix(bytecode): bind destructured parameters left to right, each in its TDZ until initialized (#1453)
- fix(bytecode): compile a default parameter or var initializer that applies postfix ++ or -- to its own binding (#1445)
- fix(vm): stop a bytecode recursion at the --max-memory ceiling with a catchable RangeError instead of growing until the OS kills it (#1485)
- fix(runtime): give an anonymous class its contextual name before its static fields and static blocks run (#1429)
- fix(bytecode): run array binding defaults and nested patterns before closing the iterator (#1457)
- fix(coverage): report the branches of a ternary or logical expression a constant decides (#1452)
- fix(cli): compile constant expressions that fold to Infinity or NaN in GocciaBundler instead of aborting (#1358)
- fix(vm): stop giving a decorator's replacement method a home object, so a direct eval inside it cannot use super (#1435)
- fix(vm): assign to a class property instead of defining it when the function has more than 255 constants (#1430)
- fix(runtime): decide how String, Number, Boolean, Array and Object are called from the constructor, not from its name (#1427)
- fix(runtime): read a typed array element through Reflect.get, a Proxy without a get trap, and an object that inherits from a typed array (#1425)
- fix(runtime): enumerate a prototype chain of any length in for...in (#1423)
- fix(runtime): find a symbol key on the prototype chain in `in` and Reflect.has (#1420)
- fix(test-runner): parse each shim once per thread and collect after every file a worker runs (#1440)
- fix(compiler): stop a call to a return-annotated arrow from aborting or giving a wrong result in bytecode mode (#1466)
- fix(scripts): make the doc checks skip gitignored files, so a repo copy under tmp/ is no longer scanned (#1444)
- fix(compiler): stop a `let` reassigned to a non-number from giving a wrong sum or aborting in bytecode mode (#1460)
- fix(bytecode): allow exactly --max-stack nested calls, not one fewer (#1468)
- fix(test262): stop a directory race between workers from failing the conformance run (#1340)
- fix(modules): reject an unresolvable import in a bytecode entry before any module evaluates (#1459)
- fix(vm): stop a caught throw from leaving its position on the frame that catches it (#1451)
- fix(fetch): report Response.length as 0, as WebIDL derives from the Fetch constructor (#1421)
- fix(nightly): restore the executable bit on the binaries before archiving them (#1419)
- fix(runtime): walk prototype chains in a loop, call a Proxy in the chain through its traps, and throw RangeError on a Proxy cycle (#1396)
- fix(regexp): give a subject of 21,474,837 code units or more its full step budget (#1385)
- fix(runtime): freeze and seal a class, and keep the attributes of its length and name when they are redefined or assigned (#1387)
- fix(runtime): read a typed array's length, byteLength, byteOffset, buffer and BYTES_PER_ELEMENT through its prototype chain (#1383)
- fix(runtime): call the own setter of a class instance, array, Map, typed array or String object instead of replacing it (#1382)
- fix(vm): assigning a function to a property no longer lets a direct eval inside it use super (#1388)
- fix(nightly): find the newest passing build by walking main's commits, not the runs list (#1386)
- fix(nightly): pick the newest passing build from the newest runs, not through the API's status filter (#1384)
- fix(benchmark): time samples on macOS with a clock that resolves below a microsecond (#1366)
- fix(vm): throw ReferenceError for `++` and `--` on a binding in its temporal dead zone in bytecode mode (#1354)
- fix(vm): accept a sloppy direct eval var that shadows an enclosing function's const in bytecode mode (#1360)
- fix(compiler): resolve names in a finally block run by return, break or continue outside the try block in bytecode mode (#1355)
- fix(vm): stop a variable captured in a block left by an exception from aliasing a later variable (#1352)
- fix(compiler): keep the binding of a for...of var head visible after the loop in bytecode mode (#1353)
- fix(vm): throw TypeError for `in` with a BigInt or Symbol on the right (#1350)
- fix(compiler): keep the TDZ for a const read from a later switch clause in bytecode mode (#1315)
- fix(fetch): end the run with a TimeoutError when --timeout elapses during a fetch (#1327)
- fix(testrunner): stop a failed file's queued jobs from crashing the next file (#1311)
- fix(runtime)!: report an unhandled promise rejection and fail the run (#1302)
- fix(microtasks): keep a caller's pending jobs when a nested run drains or fails (#1299)

### 📖 Other

- Date and the other legacy shims run as bytecode, and an empty script boots in half the instructions (#1633)
- console.log shows functions, classes and errors, and bytecode errors keep their suggestion and location (#1578)
- Name private members as written in bytecode-mode errors, and stop private-method assignment from replacing the method (#1581)

### 🚀 Added

- feat(sandbox): search the virtual filesystem by name, content and fuzzy name (#1875)
- feat(snapshot): module records, module scopes and namespace objects, restored without running a module body (ADR 0148, ADR 0150) (#1827)
- feat(test): verify heap snapshot round trips across the suite (#1837)
- feat(bytecode): load precompiled modules from buffers with profile verification (#1845)
- feat(snapshot): restore built-in differences atomically
- feat(snapshot): every built-in slot resolves by its name, and shims' internals pass the accounting check without hiding the run's objects (ADR 0148) (#1824)
- feat(snapshot): functions, closures and suspended generators, named by the bytecode of their module (ADR 0148) (#1807)
- feat(snapshot): the heap snapshot format, codec registry, writer and reader, built-in naming and the pause checkpoint (ADR 0148) (#1796)
- feat(fetch): let an embedder answer fetch requests through its own transport (#1784)
- feat(sandbox): an embedder can mark virtual filesystem paths read-only, and a script's writes there fail with EROFS (#1750)
- feat(cli)!: every binary runs bytecode only, and --mode or a "mode" config key now exits with status 2 (#1644)
- feat!: run scripts, tests, benchmarks and the REPL in bytecode mode by default (#1481)
- feat(eval): compile sloppy direct eval to bytecode with a hidden variable object per function, and delete the tree-walk eval bridge (bytecode v93) (#1607)
- feat(eval): compile direct eval at strict call sites to bytecode as a closure of the call site (bytecode v92) (#1604)
- feat(eval): compile indirect eval and ShadowRealm.prototype.evaluate to bytecode instead of running them on the tree-walk evaluator (#1601)
- feat(scripts): probe engine depth limits by hand, with every run memory-capped and timed out (#1448)
- feat(engine): report every unhandled rejection to a host through OnUnhandledRejection (#1336)

## [0.14.0] - 2026-09-28

### ⚡ Performance

- perf(strings): defer copies in bytecode accumulators (#1235)
- perf(array): reuse live callback argument roots (#1236)
- perf(vm): keep execution contexts unmanaged on the call path (#1219)
- perf(bytecode): fuse less-than compares into OP_JUMP_IF_NOT_LT (#1217)
- perf(vm): skip coverage and limit polls on production dispatch (#1216)
- perf(bytecode): fuse increment, add-immediate, write IC, and local property reads (#1215)
- perf(gc): keep worker allocations running during another worker's collection (#1213)

### 🌐 Website

- perf(website): cache dashboard history in immutable snapshots (#1232)
- refactor(website): centralize report storage and publication (#1228)
- perf(website): throttle rate-limit bucket cleanup (#1227)
- fix(website): print install commands without a double slash (#1253)
- fix(website): resolve AEO leftovers on 404 and canonical URL consistency (#1240)
- fix(seo): fix 404 metadata leak and align H1 with engine-first positioning (#1225)

### 🏗️ Internal

- chore(skills): sync known-good-route skills so skipped or rate-limited reviews count as missing (#1287)
- docs: verify every docs page and the website against the 0.14 binaries (#1272)
- chore(test262): bump pin to 7ab7fafa (#1284)
- docs(orchestration): add a repository orchestration policy (#1280)
- docs(adr): record the unified allow/deny capability model (#1256)
- ci(coderabbit): stop reviewing vendored Agent Skills (#1263)
- refactor(ci): share report publication setup (#1229)
- docs(sandbox): scope the write-back flush guarantee to POSIX and Windows (#1252)
- chore(toml-test): bump pin to ff49d109 (#1246)
- chore(skills): sync provisioned skills and replace removed workflow skills (#1249)
- chore(test262): bump pin to 0a888ef3 (#1242)
- chore(tc39-mcp): bump pin to v0.6.3 (#1239)
- chore(test262): bump pin to aae8cf6e (#1237)
- refactor(builtins): release temporary member definitions after construction (#1231)
- refactor(runtime): remove unused ASCII memo cleanup (#1226)
- refactor(runtime): centralize ordered property keys (#1234)
- refactor(csv-tsv): share callback conversion and chunk results (#1233)
- chore(toml-test): bump pin to bc8f2c2c (#1223)
- chore(jetstream): bump pin to c603c04d (#1224)
- chore(test262): bump pin to 77100523 (#1222)

### 🐛 Fixed

- fix(modules): verify provider data modules on load, and gate reviews on CodeRabbit completion (#1285)
- fix(cli): read the value of a short option such as -j 4 (#1283)
- fix(temporal): detect the system time zone on Linux and macOS (#1282)
- fix(modules): keep host paths out of script-visible module-loading errors (#1281)
- fix(capabilities): close four ways guest code could bypass its capability set (#1261)
- fix(fetch): keep an engine's fetch policy when a nested run ends (#1258)
- fix(cli): keep --no-host-filesystem in force when a JavaScript manifest is loaded (#1257) — `--no-host-filesystem` is removed in this release: host reads are denied by default, and `--deny-read` also refuses the project's own imports
- fix(profiling): count allocations in host-run bytecode callbacks (#1230)
- fix: recognise `of` only in for-of headers, flush write-back before replacing, keep retained `parse` callable (#1251)
- fix: refuse symlinked write-back temporaries, parse JSX `of` as an identifier, bound goccia:ast hosts (#1250)
- fix(parser): JSX comment trivia and ASI inside type declarations (#1243)
- fix(cli): materialize parked parse docs before growth-gate parking (#1241)
- fix(compliance): use detected CPU count for default workers (#1238)
- fix(compiler): evaluate const increment RHS and recheck global for-limits (#1220)
- fix(gc): make BytesAllocated reads tear-free on 32-bit targets (#1214)
- fix(runtime): correctness, GC-safety, and conformance fixes across the new stack layers (#1212)
- fix(runtime): root the instanceof prototype-chain walk across proxy traps (#1211)
- fix(runtime): complete EGocciaBytecodeThrow boundary handling (#1210)
- fix(shadowrealm): unpin ShadowRealm.prototype at host teardown (#1209)
- fix(modules): close class-body strictness leaks and enforce symlink containment (#1208)
- fix(diagnostics): render identical runtime-error diagnostics in both modes with sandbox-safe code frames (#1206)
- fix(runtime): root binary-operator operands across coercion re-entry (#1205)
- fix(classes): run the implicit-constructor chain per spec on every construction path (#1204)
- fix(runtime): reject async promises with the thrown value across the executor boundary (#1203)
- fix(interpreter): root async module evaluation across its synchronous prefix (#1202)
- fix(modules): classify stripped source by UTF-16 code units for bare-specifier detection (#1201)
- fix(runtime): GC-rooting, class-construction, and sandbox-capability corrections across the stack (#1200)
- fix(interpreter): let the GC own pattern-matching scopes and root match temporaries (#1198)
- fix(bytecode): root the function object of suspended async continuations (#1197)
- fix(interpreter): run instance elements when classes are constructed reflectively (#1196)
- fix(runtime): resolve argument-array holes through the prototype chain (#1195)
- fix(classes): run field initializers in the definition environment at spec-mandated points (#1194)
- fix(bytecode): match call/apply/bind intrinsics by identity in method-call fast paths (#1193)
- fix(testing): render Error-like rejection reasons as Name: message (#1191)
- fix(bytecode): run compiled class constructors when instantiated from evaluator paths (#1189)

### 🚀 Added

- feat(runner): add, update and verify provider package pins with an install mode (#1267)
- feat(modules): import github: packages pinned by a lockfile, under the import capability (#1266)
- feat(cli)!: merge the script loader and sandbox runner into GocciaRunner with a sandbox mode (#1265)
- feat(cli)!: require trust before a config file can grant permissions (#1264)
- feat(cli)!: grant host access with --allow-/--deny- flags and deny everything else by default (#1262)
- feat(runtime)!: enforce an engine-owned capability set for read, net, ffi and import (#1259)
- feat(test262): add native sharded runner (#1050)
- feat(sandbox): apply a sandbox run's changes with --write-back (#1248) — shipped as `--copy-rw`: name each input the run may write back with `--copy-rw` (or `"copy-rw"` in the `sandbox` section)
- feat(runtime): map goccia:ast ranges back to the original file (#1247)
- feat(runtime): expose the parse tree to JavaScript as goccia:ast (#1244)
- feat(testing): add deterministic virtual timers matching the Vitest fake-timer surface (#1207)
- feat(runtime): node:async_hooks with engine-propagated async context (#1192)
- feat(modules): opt-in bare-specifier resolution against node_modules (#1190) — the opt-in is `--allow-import=node_modules` (the interim `--allow-node-modules` flag was removed before release)

## [0.13.0] - 2026-08-19

### ⚡ Performance

- perf(gc): O(1) active-root frame ops; descriptors learn PushRoots (#1169)
- perf(objects): sort array-index keys in O(n log n) during own-key ordering (#1144)

### 🌐 Website

- docs(aeo): name GocciaScript as a JavaScript engine in citation surfaces (#1186)
- fix(website): make the installers work against the real release-archive layout (#1150)

### 🏗️ Internal

- chore(toml-test): bump pin to d168c2a4 (#1176)
- chore(skills): sync provisioned skills (#1178)
- chore(test262): bump pin to 3655e746 (#1175)
- chore(docs): enforce the no-hand-typed-test262-number rule mechanically (#1149)
- docs: sync docs and website claims to shipped reality for release prep (#1147)
- docs(sandbox): specify the out-of-process execution protocol (WP-5 Phase A) (#1138)
- docs(sandbox): record the hardening scope decisions and limit vocabulary (#1134)
- docs(contributing): record the probe invariant and the PR-title rule (#1127)
- style(pascal): quote JavaScript in star-parenthesis comments, drop the switch (#1125)
- test(cli): report every failing section instead of stopping at the first (#1123)
- chore: install run-retro, and apply its findings for stacked PR automation (#1122)
- chore(jetstream): bump pin to 7769b693 (#1121)
- chore(skills): sync provisioned skills (#1120)
- chore(tc39-mcp): bump pin to v0.6.2 (#1119)
- chore(test262): bump pin to be13516f (#1118)

### 🐛 Fixed

- fix(spec): true up TC39 stages, spec anchors, and shipped-surface claims (#1185)
- fix(ci): coverage CLI checks no longer flake on broken-pipe races (#1183)
- fix(test): pool-cancel bound asserts a design quantity, not a scheduler race (#1177)
- fix(gc): computed class keys survive collections during static initializers (#1182)
- fix(gc): builders and proxy traps root their in-flight values (#1179)
- fix(gc): root the evaluator's remaining guest-call windows (#1174)
- fix(gc): the element-storage gate roots its store window (#1172)
- fix(gc): the property-map growth gate roots its store window (#1170)
- fix(testrunner): parallel aborts report exactly once; iterator close can no longer swallow ceilings (#1165)
- fix(engine): fault unwinds no longer leak iterator roots or pending host work (#1164)
- fix(sandbox): stop promise boundaries from swallowing uncatchable faults (#1162)
- fix(vm): read a compound assignment's target before the right-hand side (#1161)
- fix(interpreter): perform compound assignment's GetValue exactly once (#1160)
- fix(gc): root evaluator temporaries and make engine-integrity faults uncatchable (#1158)
- fix(testing): replace growth-gate probe calibration with construction and measurement (#1155)
- fix: gate the install path and fuzz exits, harden the sentinel, true up vi claims (#1154)
- fix(ci): create the heaptrc -FU dir and hard-bound the fuzz timeout (#1148)
- fix(gc): attempt a collection before refusing a charged reservation (#1152)
- fix(testing): lift the parked parser-probe ceilings clear of the refusal bands (#1151)
- fix(gc): root parser and builtin containers across collecting safe points (#1146)
- fix(modules): keep expanded host paths out of script-visible module errors (#1141)
- fix(testing): detect terminal limit unwind with a flag, not ExceptObject (#1145)
- fix(sandbox): clear pending work on memory-limit unwind, bound AFL timeout, specify pipe I/O (#1142)
- fix(gc): root builtin result containers across allocation safe points (#1140)
- fix(sandbox): harden fuzz-input bounds, fetch URL validation, and resource-limit propagation (#1139)
- fix(modules): report a missing content provider as a coded JavaScript Error (#1136)
- fix(sandbox): apply engine options in the sandbox runner and classify failures (#1135)

### 🚀 Added

- feat(gc): the growth gate collects once before refusing (#1181)
- feat(testrunner): abort the run when an engine-integrity fault escapes (#1163)
- feat(memory): gate object property storage growth against the budget (#1137)
- feat(memory): gate uncharged allocation paths and add a host-catchable limit (#1132)
- feat(fetch): pin resolved addresses and add private-range and size policy (#1131)
- feat(fuzz): add fuzz harness and memory-safety CI (#1130)
- feat(vitest): close the drop-in gap in module resolution, generics, and the vi surface (#1126)

## [0.12.0] - 2026-08-10

### 🌐 Website

- feat(website): Vitest drop-in direction on VISION and the homepage (#1096)
- fix(website): vendored playground versions reach the page and stay fresh (#1087)

### 🏗️ Internal

- test(cli): look up coverage entries by basename, not native path (#1111)
- test(cli): match boundary lines in containsLine and drop the workarounds (#1110)
- docs: call them differential suites, not batteries (#1108)
- test(cli): split coverage keys on both path separators (#1100)
- ci(testing): gate testing-API suites on a pinned Vitest oracle (#1097)
- ci(testing): differential suites gate goccia against bun (#1082)

### 🐛 Fixed

- fix(changelog): require website predominance before the Website group (#1115)
- fix(review): CodeRabbit round-7 findings on the round-6 layer (#1113)
- fix(review): CodeRabbit round-6 findings across the five unreviewed layers (#1112)
- fix(review): CodeRabbit round-5 findings across the stack (#1109)
- fix(coverage): canonicalize report path keys and document the parallel bail (#1107)
- fix(testing): close the last Vitest and ECMA-262 semantic divergences (#1104)
- fix(threading): give each run its own cancellation flag after abandonment (#1103)
- fix(threading): guard the pool cancellation flag behind a lock (#1102)
- fix(review): CodeRabbit round-2 findings across the stack (#1101)
- fix(cli): print usage instead of blocking when run without input at a TTY (#1098)
- fix(gc): root builtin argument collections and re-entry temporaries (#1095)
- fix(testing): Vitest comparison parity across equality, Set/Map, and errors (#1093)
- fix(testing): count beforeAll/afterAll hook failures as failures (#1090)
- fix(parser): bound the arrow return-type probe; hard-gate web-tooling outcomes (#1089)
- fix(testing): count throwing describe callbacks as failures (#1088)
- fix(cli): keep JSON envelope stdout clean for every failure shape (#1086)
- fix(review): CodeRabbit round-1 findings across the stack (#1085)
- fix(testing): compare Error objects by value in deep equality (#1084)
- fix(coverage): imply bytecode mode, sum parallel merges, register declaration lines (#1081)
- fix(testing): bring deep-equality and property matchers to Vitest parity (#1078)
- fix(testing): apply negation and full argument semantics to toThrow (#1077)
- fix(engine): check nullish base before property-key coercion (#1076)
- fix(jsx): guarantee transformer scan progress and positioned errors (#1075)

### 🚀 Added

- feat(testing): make goccia:test importable without injecting globals (#1106)
- feat(testing): support vi.mock factory-form module mocking (#1105)
- feat(testing): goccia:test module and default-on vitest compatibility shim (#1099)
- feat(testing): abort file collection when a describe callback throws (#1092)
- feat(testing): Vitest-exact suite-error attribution and hook control flow (#1091)
- feat(runtime): WHATWG EventTarget base with AbortSignal abort events (#1080)
- feat(parser): parse TS type-syntax edges as annotations (#1079)

## [0.11.0] - 2026-08-05

### ⚡ Performance

- perf(compiler): avoid redundant this property moves (#1051)
- perf(array): fast-path guarded dense appends (#1049)
- perf(vm): avoid per-opcode instruction-limit TLS lookup (#1048)
- perf(typed-array): specialize fixed-length indexed access (#1046)
- perf(modules): retain resolved import bindings (#1045)
- perf(json): recover UTF-16 parser hot paths (#1000)

### 🌐 Website

- docs: sync release preparation claims (#1073)
- fix(security): harden untrusted execution boundaries (#1057)
- docs(positioning): lead with agent sandboxing (#1052)
- feat(website): add Homebrew install and polish navigation (#1027)

### 🏗️ Internal

- test(coverage): handle Windows LCOV line endings (#1069)
- chore(skills): sync provisioned skills (#1060)
- chore(test262): bump pin to b363f29d (#1059)
- chore(timezone): bump data to 2026c (#1058)
- chore(skills): sync provisioned skills (#1056)
- chore(test262): bump pin to 07dbcbca (#1055)
- refactor(compliance): native TOML runner and generated JSON5 tests (#1047)
- docs(examples): add experimental runtime demos (#1028)

### 🐛 Fixed

- fix(parser): parse structured function return types (#1061)
- fix(modules): root relative CLI aliases (#1068)
- fix(coverage): exclude parallel runner warm-up (#1066)
- fix(bytecode): preserve module lexical captures (#1063)
- fix(modules): preserve live import bindings across module graphs (#999)
- fix(conformance): preserve ceil boundaries and pin truthful library probes (#998)

### 🚀 Added

- feat(runtime): add fetch abort signals (#1067)
- feat(testing): add async mock helpers (#1065)
- feat(coverage): report function hits (#1064)
- feat(parser): erase TypeScript expression types (#1062)
- feat(ffi): expand native call capabilities (#1034)
- feat(iterator): implement Iterator.prototype.includes (#1026)

## [0.10.0] - 2026-07-21

### ⚡ Performance

- perf(bytecode): specialize proven numeric recursion (#994)
- perf(regexp): avoid discarded match arrays (#981)

### 🌐 Website

- docs(release): sync source truth and generated data (#995)
- refactor(website): adopt Fumadocs and Astryx (#992)
- feat(website): clarify ECMAScript compatibility positioning (#982)

### 🏗️ Internal

- chore(skills): sync provisioned skills (#996)
- chore(skills): sync provisioned skills (#990)
- chore(test262): bump pin to 9e61c128 (#988)
- test(suite): deepen coverage and enforce structure (#983)

### 🐛 Fixed

- fix(format): keep inline directives on declarations (#991)
- fix(evaluator): bound lexical for-loop roots (#984)
- fix(cli): preserve quoted arguments on Windows (#979)

### 🚀 Added

- feat(delphi): support Win32 and Win64 builds (#989)
- feat(engine): add capability audit events (#985)
- feat(sandbox): add Node-style fs callbacks (#987)
- feat(testing): add Vitest-compatible snapshots (#973)
- feat(ffi): add bidirectional ABI engine (#967)
- Wasm support (#957)
- feat(modules): add configurable virtual modules (#974)
- feat(engine): add deterministic and custom host environments (#968)

## [0.9.0] - 2026-07-15

### ⚡ Performance

- perf(vm): reduce dispatch loop overhead (#949)

### 🌐 Website

- docs: sync release preparation source truth (#969)
- feat(benchmarks): add JetStream performance barometer (#956)
- feat(benchmarks): add web tooling benchmark lane (#946)
- feat(benchmarks): add AWFY cross-engine driver (#938)

### 🏗️ Internal

- ci(web-tooling): automate weekly pin bumps (#977)
- ci(jetstream): automate weekly pin bumps (#976)
- ci(timezone): automate monthly data bumps (#975)
- chore(skills): sync provisioned skills and automate updates (#970)
- chore(timezone): bump data to 2026c (#971)
- chore(jetstream): bump pinned corpus (#972)
- chore(toml-test): bump pin to 9eef1b95 (#962)
- chore(tc39-mcp): bump pin to v0.6.1 (#963)
- chore(test262): bump pin to d1d583db (#961)
- test(cli): make GC pressure check architecture-independent (#959)
- docs(perf): record call argument overhead investigation (#954)
- test(math): allow portable hyperbolic approximations (#952)
- ci(awfy): compare PR AWFY against main (#950)
- chore(tc39-mcp): bump pin to v0.6.0 (#942)
- chore(test262): bump pin to 64ff467c (#941)
- docs(annex-b): record deferred support policy (#924)

### 🐛 Fixed

- fix(built-ins): close remaining test262 gaps (#966)
- fix(language): complete core semantics tail (#964)
- fix(temporal): complete test262 conformance (#958)
- fix(collections): complete test262 conformance (#955)
- fix(test262): make eval and disposable behavior spec-compliant (#944)
- fix(math): close Math gaps and follow-up regressions (#947)
- fix(function): close core Function conformance gaps (#940)
- fix(runtime): improve real-world library compatibility (#939)
- fix(parser): error on unsupported default syntax (#936)
- fix(map): align collection constructor iteration (#935)
- fix(proxy): close Proxy conformance gaps (#931)
- fix(typedarray): close buffer conformance gaps (#934)
- fix(iterator): complete iterator helper conformance (#932)
- fix(reflect): close remaining test262 conformance gaps (#933)
- fix(class): close class super private conformance gaps (#930)
- fix(string): complete String test262 conformance (#929)
- fix(generators): align generator prototype methods (#928)
- Complete Array test262 conformance (#926)
- fix(regexp): close remaining test262 gaps (#927)
- fix(built-ins): complete Number BigInt Boolean test262 conformance (#925)
- fix(test262): close promise, module-source, and bytecode gaps (#923)
- fix(error): complete error built-ins conformance (#922)
- fix(object): close built-ins Object test262 gaps (#919)
- fix(runtime): dispatch callable values through ES Call (#921)
- fix(json): reach full JSON test262 conformance (#920)
- fix(test262): classify engine timeouts distinctly (#918)
- fix(test262): surface thrown object messages (#917)

### 🚀 Added

- feat(sandbox): track filesystem timestamps (#965)
- feat(sandbox): add Node.js-shaped filesystem errors (#960)
- feat(benchmarks): report percentile distributions (#953)
- feat(runtime): add goccia runtime modules (#943)
- feat(benchmarks): add module microbench API (#945)

## [0.8.1] - 2026-07-01

### ⚡ Performance

- perf(engine): materialize built-ins, runtime globals, and shims lazily at boot (#790)
- perf(vm): unbox typed-array element reads and writes in the bytecode VM (#900)
- perf(engine): speed up FormatDouble shortest-representation scan (#899)
- perf(parser): keep parenthesized-group probe tokens instead of re-lexing (#896)
- perf(engine): replace O(F^2) EntryAt(I) class-field loops with the enumerator (#898)
- perf(engine): trim Date shim slot fan-out and binary-search timezone lookup (#897)
- perf(regexp): serve case-fold tables lock-free on the icase hot path (#895)
- perf(engine): back strong Map/Set with a SameValueZero-keyed ordered hash (#890)
- perf(engine): make for-in key dedup O(k) with a hash set (#888)
- perf(json): make deeply nested JSON.stringify O(depth^2) (#887)
- perf(string): ASCII byte-op fast paths for the UTF-16 string helpers (#806)

### 🌐 Website

- docs: sync release claims to shipped reality (test262 %, runtime size, unit-test count) (#915)

### 🏗️ Internal

- docs(skills): review the full retained window in profile-report-review (#914)
- docs(skills): add prepare-release skill (#909)
- refactor(engine): migrate per-thread cache clears into the cleanup registry (#893) (#903)
- chore(test262): bump pin to 020cb740 (#905)
- chore(toml-test): bump pin to 4d77658d (#906)
- chore(tc39-mcp): bump pin to v0.5.0 (#907)
- refactor(engine): unify embedded-data caches onto a lock-free publication primitive (#902)
- ci(benchmark): compare PR against a same-runner main build (#886)

### 🐛 Fixed

- fix(engine): audit object-reference threadvars and rebuild prototype members per realm (#904)
- fix(intl): discover the installed ICU version at runtime instead of a hardcoded max (#908)
- fix(parser): make speculative paren-group probes template-aware (#901)
- fix(engine): release managed threadvars on worker-thread exit (#891)
- fix(parser): skip interpolated templates in disabled-feature recovery (#889)
- fix(engine): map ShadowRealm.evaluate declaration early errors to SyntaxError (#817)
- fix(engine): map ShadowRealm eval instantiation errors to SyntaxError (#816)

## [0.8.0] - 2026-06-26

### ⚡ Performance

- perf(regexp): memoize the decoded subject across regex VM calls (#805)
- perf(engine): elide unused arguments objects and speed typed-array element-set (#818)
- perf(interpreter): skip redundant per-call hoisting work for function bodies (#804)
- perf(engine): skip GC weak-reference passes when no weak container holds data (#803)
- perf(vm): reduce bytecode-VM per-call overhead (argument arena + deferred stack frames) (#802)
- perf(regexp): optimize VM scan path (#763)
- Shape-validated inline caches for VM property reads (#757)
- Speed up global reads, lex regexes in template interpolations, make --timeout a hard deadline (#744)
- Cache bytecode RegExp literal programs (#731)
- Cache compiled RegExp programs (#677)
- Replace TRegExpr with purpose-built backtracking bytecode VM regex engine (#585)
- Optimize parser hot token dispatch (#466)
- Optimize compiler local lookup (#484)
- Optimize lexer hot paths (#458)

### ✨ Improved

- Improve test262 language feature coverage (#732)
- Improve test262 eval and resource conformance (#725)
- Make destructuring parameter binding spec-compliant (#722)
- Improve test262 high-pass built-in coverage (#716)
- Improve test262 stock harness compliance (#713)
- Make TGocciaLexerError inherit from TGocciaSyntaxError (#627)
- Make ToNumberLiteral ES2026-compliant for objects (#547)
- Make ToStringLiteral ES2026-compliant; add ToPropertyKey (#527)
- Make CLI test substring matches CRLF-tolerant on Windows (#512)
- Make `Goccia.gc` safe during active execution (#497)
- Make strict-types runtime enforcement an opt-in --strict-types flag (#479)

### 🌐 Website

- fix(website): patch Next.js and Mermaid advisories (#789)
- feat(ci): publish profile reports from main (#774)
- feat(sandbox): add GocciaSandboxRunner (#772)
- feat(website): improve seo and social metadata (#771)
- fix(website): expand test262 chart tooltips (#764)
- Update skills and documentation (#767)
- fix(website): serve test262 dashboard from blob at request time (#759)
- fix(website): unbreak test262 blob publish on public stores (#746)
- fix(website): refresh test262 blob manifest before publish (#742)
- feat(website): add test262 compatibility dashboard (#741)
- Clean up logo artifacts (#684)
- Centralize source pipeline parser policy (#682)
- Support default module imports and exports (#659)
- Sync playground gutter scroll with textarea (#481)
- Show script memory usage on the playground tail line (#478)
- Add Report issue button to playground (#477)
- Add resource management example to playground (#472)
- Cache playground runner responses (#464)

### 🏗️ Internal

- ci(workflows): scope GitHub Actions token permissions to jobs that need them (#791)
- refactor(runtime): share indexed data module loading (#781)
- test(benchmarks): add temporal intl atomics coverage (#783)
- docs: consolidate command reference (#782)
- docs(agents): document source layout search paths (#779)
- chore(test262): bump pin to de8e621c (#766)
- refactor(runtime): simplify runtime surface (#765)
- chore(test262): bump pin to 05bb0329 (#737)
- chore(ci): upgrade workflow actions to Node 24 (#736)
- chore(tc39-mcp): bump pin to v0.4.0 (#734)
- Extract shared resource container infrastructure (#715)
- chore(tc39-mcp): add project MCP config (#719)
- Update skills (#701)
- chore(agent): add tc39-mcp spec lookup workflow (#695)
- chore(toml-test): bump pin to af5f8052 (#683)
- Centralize CLI source pipeline handling (#685)
- Unify runtime terminology and executor units (#681)
- Simplify and harden lexer internals (#674)
- Split runtime globals into installable extensions (#666)
- Extract shared spawn helper from CLI test harnesses (#629)
- Centralize FExecuting re-entrancy guard into TGocciaIteratorHelperValue (#581) (#589)
- Consolidate engine semantics and test262 roadmap (#494)
- Split engine and runtime surfaces (#493)

### 🐛 Fixed

- fix(modules): reject missing named imports in bytecode mode (#799)
- fix(engine): eliminate test262 timeouts (tail calls, recursion bound, perf) (#797)
- fix(sandbox): reject host symlinks during seed imports (#793)
- fix(engine): deduplicate string-to-number across runtime and compiler (#792)
- fix(test262): close intl402 and date conformance gaps (#778)
- fix(ci): restore parallel benchmark stability (#780)
- fix(test262): close built-ins conformance clusters (#776)
- fix(test262): reach supported language conformance (#770)
- fix(sandbox): normalize Windows-style virtual paths (#773)
- fix(parser): drive slash lexing from parser context (#760)
- fix(bytecode): isolate cross-realm shape ownership (#762)
- fix(engine): remove class method fallback (#761)
- fix(json): align stringify root and array semantics (#758)
- fix(builtins): guard script-controlled Trunc sites per spec integer coercions (#750)
- fix(json): conform JSON.stringify array replacer to ES2026 §25.5.4 step 5 (#754)
- fix(json): spec-conformant boxed Number/String coercion during stringify serialization (#756)
- fix(json5): coerce boxed space, quote, and allow-list arguments in JSON5.stringify (#753)
- fix(scripts): avoid FPC ICE 200611011 in run_json5_test_suite.py builds (#755)
- fix(vm): resolve computed property access on primitive receivers (#748)
- fix(json): spec-conformant space argument coercion in JSON.stringify (#749)
- fix(test262): close language conformance gaps (#739)
- fix(test262): pin suite outside workflows (#735)
- Fix Windows CLI multiline output checks (#733)
- Fix Annex B labelled function compatibility (#730)
- fix(intl): cache supported locale resolution (#728)
- Fix concrete iterator prototype ancestry (#723)
- Fix class method destructuring params in bytecode (#718)
- fix(interpreter): collect under memory pressure (#714)
- Fix private elements on constructor return overrides (#708)
- fix(intl): format BigInt locale strings (#711)
- fix(builtins): reject nullish prototype receivers (#712)
- fix(interpreter): root loop scopes during explicit gc (#710)
- fix(typedarray): use spec descriptors for built-ins (#706)
- fix(bytecode): preserve VM roots during explicit GC (#705)
- Fix detached buffer revalidation in binary data methods (#707)
- fix(intl): delegate locale prototype methods (#700)
- fix(intl): parse Locale unicode extension keys (#696)
- fix(intl): apply Collator comparison options (#694)
- fix(intl): localize DisplayNames currency codes (#693)
- fix(intl): repair RelativeTimeFormat output (#692)
- fix(regexp): guard huge replace expansions (#691)
- Fix Intl format and compare accessors (#686)
- Fix GC roots for VM saved state and native callbacks (#680)
- Fix NumberFormat digit option resolution (#679)
- Fix bytecode top-level lexical binding semantics (#675)
- Fix for-loop update closure environments (#676)
- Fix computed class field yield evaluation (#672)
- Fix RegExp lookbehind direction (#673)
- Fix bytecode object literal define semantics (#669)
- Fix RegExp modifier word escapes (#671)
- Fix Goccia.gc test runner result rooting (#667)
- Fix static class setter assignment (#662)
- Fix TypedArray detach-safe element access (#661)
- Fix defineProperty descriptor semantics (#657)
- Fix generator loop closure resumes (#644)
- Preserve flatMap TypeError when source iterator Close throws (#628)
- Fix for-await-of IteratorClose on abrupt completion (#620)
- Fix bytecode VM Range-check error draining top-level Promise.then continuation (#588)
- Fix Function() constructor this-binding and strict-mode delete gaps (#584)
- Fix object literal accessor register clobber in bytecode compiler (#583)
- Fix console.log TypeError on prototype-less objects, add --inspect-depth (#578)
- Fix iterator helper SIGSEGV: re-entrancy guard, generator Close, AcquireExceptionObject (#514) (#579)
- Fix TOML compliance and pin TOML/YAML test suites to specific SHAs (#576)
- Fix arrow function this binding in module mode (#574)
- Route array length setter through ArraySetLength (#566)
- Handle iterable and array-like arguments in TypedArray constructor, from(), and set() (#558)
- Fix use-after-free in class field initializer scopes (#560)
- Fix ++/-- on BigInt operands; add OP_INC, OP_DEC, OP_TO_NUMERIC (#548)
- Fix performance regressions from #496 (#508)
- Fix language/types Test262 coverage (#491)
- Fix `new` on compat-function constructors (#501, #502) (#505)
- Fix built-ins Symbol test262 coverage (#490)
- Fix Test262 line terminator handling (#492)
- Fix IteratorClose for abrupt for-of completion (#488)
- Fix statementList comma expression parsing (#489)
- Fix if condition double evaluation (#487)
- Fix test262 literal conformance (#486)
- Fix benchmark failure JSON output (#485)
- Fix WeakMap/WeakSet test262 edge cases (#470)

### 📖 Other

- Skill updates (#738)
- Validate RegExp named group identifiers (#668)
- Intl: implement formatRange support (#658)
- Intl: return all ECMA-402 required fields from resolvedOptions() (#631)
- Persist for-loop iteration state across generator resumes (#633)
- Generate full Unicode property tables for regex \p{} escapes (#630)
- Reject unescaped line terminators in string literals (#623)
- Close source iterator when take limit is reached (#591) (#593)
- Emit OP_CLOSE_UPVALUE on break so closures pin per-iteration values (#573)
- Sync upvalue cell after increment/decrement on captured locals (#570)
- Honor explicit object returns from class constructors in Instantiate (#571)
- Clarify implement-issue skill to invoke /create-pr skill (#568)
- Reject `-000000` as an invalid extended year in ISO date parser (#564)
- Define prototype.constructor on native Error hierarchy (#565)
- Stop Find* scope walks at ordinary function boundaries (#559)
- Propagate newTarget through native constructors for Reflect.construct (#556)
- Propagate newTarget through class [[Construct]] for new.target (#549)
- Throw TypeError on delete with null/undefined base (#546)
- Accept all constructable function forms in Reflect.construct (#528)
- Block PR merge on test262 regression vs main baseline (#535)
- Move test262 to ScriptLoaderBare and add weekly bump (#513)
- Serialize benchmark progress output (#498)
- Expose heap memory in timing PR comment (#454)
- Extend compact-json output to TestRunner and BenchmarkRunner (#474)
- Exclude changelog update PR from generated changelog (#473)

### 🗑️ Removed

- Remove --compat-all meta-flag (#632)
- Remove disable-model-invocation from skill frontmatter (#572)

### 🚀 Added

- feat(engine): complete built-ins/ShadowRealm test262 conformance (64/64) (#814)
- feat(engine): implement ShadowRealm.prototype.importValue (#801)
- feat(engine): add ShadowRealm behind --unsafe-shadowrealm (#795)
- feat(modules): support TC39 Import Bytes with immutable backing (#794)
- feat(modules): implement test262 module import semantics (#740)
- Support literal class property names (#729)
- Implement function object intrinsics (#724)
- Implement Atomics for SharedArrayBuffer (#717)
- Implement ECMAScript DataView (#709)
- Implement ECMAScript realm execution contexts (#703)
- feat(gc): add WeakRef and FinalizationRegistry (#702)
- Add for-in compatibility flag (#697)
- Implement Intl formatToParts decomposition (#690)
- Support labeled break and continue targets (#688)
- Support dynamic import call phases (#687)
- Add GocciaScript issue validation skill (#689)
- Add top-level await while condition regressions (#670)
- Implement RegExp match indices (#664)
- Implement Intl.Locale info methods (#663)
- Implement spec RegExp symbol methods (#656)
- Add compat while loop support (#651)
- Implement non-strict arguments and with support (#645)
- Allow RegExp symbol protocol objects (#642)
- Add RegExp accessor receiver regression tests (#641)
- Add loose equality compatibility flag and test262 fixes (#638)
- Implement Unicode RegExp simple case folding (#640)
- Implement IsRegExp check via Symbol.match (#624)
- Add ECMA-402 Intl namespace with ICU-first data strategy (#542) (#590)
- Add engine to decision-log area tags and sort alphabetically (#622)
- Implement v flag (UnicodeSets) for RegExp (#621)
- Add chained-helper re-entrancy tests for iterator helpers (#582) (#587)
- Allow var/function declarations to shadow built-in globals in script mode (#577)
- Add +000000 acceptance tests for Temporal types and strengthen Date assertion (#569)
- Create per-class init scope in InitializeInstanceProperties (#562)
- Implement Error.prototype.toString (ES2026 §20.5.3.4) (#557)
- Implement @@toPrimitive probe; fix Symbol.IsPrimitive (#555)
- Add --compat-traditional-for-loop opt-in flag (#531)
- Add project command skills and contribution guidance (#525)
- Add --print flag, switch loaders to silent-by-default (#509) (#510)
- Add --compat-all meta-flag across CLIs (#507)
- Add --mode=interpreted|bytecode flag to GocciaScriptLoaderBare (#506)
- Add deterministic benchmark profile diffs to CI (#500)
- Add CLI-local print to GocciaScriptLoaderBare (#499)
- Add compile-time constant folding to bytecode compiler (#496)
- Add test262 conformance lane to CI with PR comment (#471)
- Add --multifile CLI flag for splitting inputs on --- (#468)
- Add --source-type CLI flag and config to load entry as a module (#480)
- Add own `prototype` to compat-function functions and generators (#475)
- Add --config flag to override config auto-discovery (#476)
- Add compact-json output format to GocciaScriptLoader (#463)

## [0.7.1] - 2026-04-29

### 🌐 Website

- Add version dispatch to playground engine (#467)
- Use static Vercel ignore config (#462)
- Refresh docs and website to match recent feature set (#460)
- Add website agent discovery metadata (#453)
- Scope Vercel builds to website changes (#455)

### 🏗️ Internal

- Unify stdin handling across all three runners (#461)

### 🐛 Fixed

- Fix microtask drain re-entrancy crashing async-generator yield rejection (#465)
- Fix generator loop resume and async iterator lookup (#456)
- Fix changelog website grouping (#459)

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
- Unify CLI JSON reporting (#433)
- Update copyright year in LICENSE file (#384)
- Simplify hasOwnProperty shim binding (#347)
- Refactor engine around pluggable executors (#332)
- Update ECMAScript edition references to 2027 (#323)
- Refactor CLIs onto shared application framework (#299)
- Update docs (#302)
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

- perf: use singleton values throughout the codebase instead of allocating
- perf: use TStringBuilder in Array.ToStringLiteral
- perf: fast path in fn.apply for TGocciaArrayValue arguments
- perf: return singleton ZeroValue/OneValue from Boolean.ToNumberLiteral
- Optimize GC membership checks from O(n) to O(1) with TDictionary
- Replace direct TGocciaScope.Create with CreateChild
- perf: use TStringBuilder for string and template literal scanning
- perf: use TDictionary for keyword lookup in lexer
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
- Split out methods, class methods and arrow functions explicitly
- docs: document Boolean singleton, fn.apply fast path, and Array TStringBuilder optimizations
- Update environment variable (skip 64-bit windows build for now)
- Update Goccia.Values.FunctionValue.pas
- Update documentation
- Extract ParseObjectMethodBody helper in ObjectLiteral
- Consolidate Engine built-in registration methods
- Extract shared ParseGetterExpression/ParseSetterExpression in parser
- Extract SpreadIterableInto helpers to deduplicate spread expansion
- Simplify Engine singleton pinning and destructor cleanup
- Extract CreateArrayCallbackArgs helper for array iterator methods
- Extract EvaluateSimpleNumericBinaryOp for subtraction, multiplication, power
- Extract MarkPropertyDescriptor helper in GCMarkReferences
- Extract shared ProcessEscapeSequence in lexer
- Extract ParseBinaryExpression to deduplicate 11 binary operator parsers
- Extract EvaluateStatementsSafe to eliminate duplicated exception handling
- Update logo.png
- Update documentation
- Simplify GetProperty/SetProperty calls
- refactor: standardise argument validation in TestAssertions
- refactor: add virtual GetProperty/SetProperty on TGocciaValue base class
- refactor: extract CopyStatementList helper in evaluator
- refactor: remove DefineBuiltin legacy method from TGocciaScope
- refactor: remove unused IPropertyMethods and IIndexMethods interfaces
- refactor: extract ParseParameterList in the parser
- refactor: eliminate GlobalEvaluationContext mutable global state
- refactor: remove legacy object evaluation path in EvaluateObject
- refactor: extract GetPropertyFromValue/SetPropertyOnValue/DefinePropertyOnValue helpers
- Update build.yml
- Update README.md
- Update build.yml
- Update build.yml
- Refactor error handling; remove debug logging
- Update build.yml
- Simplify Number builtin to use FBuiltinObject
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
- Simplify compound operation
- Update ty-catch implementation
- Update Goccia.Values.FunctionValue.Test.pas
- Refactor native function to reduce duplication
- Update `Math.clamp`
- Simplify array includes checks
- Simplify function calling further
- Simplify function calling
- Update class instantiation
- Update template evaluation and property initialization
- Update test cases
- Update expression tests
- Update tests
- Update tests and testrunner
- Update Github actions
- Update example.js
- Update initial test suite
- Simplify valid identifier check
- Update built-in constructor signature
- Update class definitions and class expressions
- Update compound assignments
- Update logger
- Update tests and script loading functionality
- Simplify MethodValue based off FunctionValue
- Update FunctionTest
- Update workflow
- Update build and build-and-run commands
- Update gitignore

### 🐛 Fixed

- Fixes method definitions
- fix: isolate module scope so module variables don't leak into global
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
- Move basic example into examples folder
- Use shared prototype
- Use StringBuilder instead of string concat
- Address TODO comments: fix, improve, or resolve 11 items
- Share string prototype singleton across all string boxing
- Standardize error handling: use ThrowTypeError in destructuring
- Use TStringBuilder for template literal evaluation
- Trying to get Windows64 build to work
- 32-bit compatibility and compile x64 on Windows
- Delete BenchmarkRunner
- Use RuntimeCopy mechanism instead of marking values permanent in the GC
- Rename Goccia.GC to Goccia.GarbageCollector
- Spread iterables in tests; disable Vitest watch
- Delete WARP.md
- Try/finally fix; function prototype and parser updates
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
- Initial try-catch implementation
- Workaround for closure capturing
- Parser support for for statements
- Adding initial getter and setter support
- Adding spread operator
- Restructure object tests
- Updated computed properties
- Removed unnecessary casts to Array
- Restructure object tests
- Further test fixes
- Build in silent mode
- Updates tests
- Delete TestUtils.pas
- Expose built-in objects from the interpreter
- Adds native testing library
- Initial version of test handlers
- Added `instanceof`
- Spec compliance
- Small fixes
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


