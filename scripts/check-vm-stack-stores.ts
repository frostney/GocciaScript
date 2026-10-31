#!/usr/bin/env npx tsx
/**
 * check-vm-stack-stores.ts
 *
 * Rejects a store into a bytecode VM stack slot whose right-hand side (or
 * index) calls anything that could run guest code.
 *
 * FRegisters, FLocalCells and FArguments point into FRegisterStack,
 * FLocalCellStack and FArgumentStack, dynamic arrays that the VM reallocates
 * when a frame needs more room or an idle stack shrinks. Pascal does not say
 * whether `FRegisters[A] := F(...)` computes the slot's address before or
 * after calling F. FPC 3.2.2 calls F first; FPC 3.2.4-rc2 and trunk can
 * compute the address first, so if F runs guest code that moves the stack,
 * the result is written into the freed block and the register keeps its old
 * value (test262 against a build from those compilers failed with
 * `ReferenceError: Date is not defined`). Evaluate such a call into a local,
 * then store the local.
 *
 * The check reads statement text. It does not see a slot reached through a
 * pointer alias or pointer arithmetic, a slot passed as a `var` or `out`
 * argument, or a getter behind a default array property; keep such code out
 * of the VM, or check it by hand.
 *
 * A store may call only the helpers in ALLOWED_CALLS, each of which builds or
 * reads a value without running guest code or touching a VM stack. Adding a
 * name requires that every routine of that name meets that bar.
 *
 * A "call" is an identifier followed by `(` that is not a type cast, or any
 * identifier that names a routine, or a property read through one, declared
 * anywhere under source/ (Pascal calls a routine without parentheses too).
 *
 * Usage:
 *   npx tsx scripts/check-vm-stack-stores.ts
 *   npx tsx scripts/check-vm-stack-stores.ts --verbose
 *   npx tsx scripts/check-vm-stack-stores.ts --self-test
 */

import { readFileSync, readdirSync } from "fs";
import { join, relative, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, "..");
const SOURCE_DIRS = [
  join(ROOT, "source", "units"),
  join(ROOT, "source", "shared"),
  join(ROOT, "source", "app"),
];
const VERBOSE = process.argv.includes("--verbose");
const SELF_TEST = process.argv.includes("--self-test");

// The VM fields that point into, or are, a stack the VM reallocates.
const STACK_FIELDS = [
  "FRegisters",
  "FLocalCells",
  "FArguments",
  "FRegisterStack",
  "FLocalCellStack",
  "FArgumentStack",
  "FFrameStack",
  "FClosedNumericFrameStack",
];

// Routines a stack-slot store may call: register constructors and readers,
// numeric conversions, string comparison and allocation of a fresh value.
// None runs guest code; allocation never collects
// (TGarbageCollector.RegisterObject only accounts), and collection does not
// move a VM stack anyway. Matched against `Name` or `Qualifier.Name`.
const ALLOWED_CALLS = new Set(
  [
    // Goccia.VM.Registers
    "RegisterUndefined",
    "RegisterNull",
    "RegisterHole",
    "RegisterBoolean",
    "RegisterInt",
    "RegisterFloat",
    "RegisterFromDouble",
    "RegisterObject",
    "ValueToRegister",
    "RegisterToValue",
    "RegisterToBoolean",
    "NumericScalarToDouble",
    // Goccia.VM register and number helpers
    "VMValueToRegisterFast",
    "VMNumberRegister",
    "VMIntResult",
    "VMIntProductResult",
    "VMModuloRegister",
    "VMPowerRegister",
    "VMRegisterToInt32",
    "NumberToInt32",
    "SignedRightShiftInt32",
    "GetLocalRegister",
    // Operands and constants of the running template
    "DecodeBx",
    "DecodesBx",
    "GetConstantUnchecked",
    "ConstantToValue",
    "BuildTemplateObjectConstant",
    "BuildRegExpLiteralConstant",
    "CachedASCIIStringValue",
    "UTF16CodeUnitImmediateToString",
    // Strings
    "UTF16StringsEqual",
    "UTF16CodeUnitLength",
    "CompareStringValues",
    // Fresh values
    "TGocciaStringLiteralValue.Create",
    "TGocciaVMLiteralObjectValue.Create",
    "TGocciaVMClassValue.Create",
    "TGocciaBytecodeCell.Create",
    // Realm and shared singletons
    "TGocciaObjectValue.SharedObjectPrototype",
    "TGocciaNumberLiteralValue.NaNValue",
    "TGocciaNumberLiteralValue.NegativeZeroValue",
    "TGocciaUndefinedLiteralValue.UndefinedValue",
    "TGocciaNullLiteralValue.NullValue",
    "TGocciaBooleanLiteralValue.TrueValue",
    "TGocciaBooleanLiteralValue.FalseValue",
    // Plain reads that share a name with a routine elsewhere
    "Assigned",
    "Value",
    "Count",
    "Elements",
    "GetElement",
    "GetLength",
    "StoredLength",
    "ThisValue",
    "IntValue",
    "FloatValue",
    "ObjectValue",
    "StringValue",
  ].map((name) => name.toLowerCase()),
);

// Identifiers followed by `(` that are casts rather than calls.
const ORDINAL_CASTS = new Set(
  [
    "Byte",
    "ShortInt",
    "Word",
    "SmallInt",
    "Int16",
    "UInt16",
    "LongInt",
    "LongWord",
    "Integer",
    "Cardinal",
    "Int32",
    "UInt32",
    "Int64",
    "UInt64",
    "NativeInt",
    "NativeUInt",
    "PtrInt",
    "PtrUInt",
    "Pointer",
    "Boolean",
    "Double",
    "Char",
    "WideChar",
  ].map((name) => name.toLowerCase()),
);

const KEYWORDS = new Set(
  [
    "and",
    "or",
    "not",
    "xor",
    "div",
    "mod",
    "shl",
    "shr",
    "nil",
    "true",
    "false",
    "is",
    "as",
    "in",
  ].map((name) => name.toLowerCase()),
);

const walkSource = (dir: string, out: string[]): void => {
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const full = join(dir, entry.name);
    if (entry.isDirectory()) walkSource(full, out);
    else if (/\.(pas|inc|dpr)$/.test(entry.name)) out.push(full);
  }
};

// Blanks comments and string literals, keeping offsets and line breaks.
const stripCommentsAndStrings = (text: string): string =>
  text.replace(
    /\{[\s\S]*?\}|\(\*[\s\S]*?\*\)|\/\/[^\n]*|'(?:[^'\n]|'')*'/g,
    (match) =>
      match.startsWith("'")
        ? "'" + " ".repeat(match.length - 2) + "'"
        : match.replace(/[^\n]/g, " "),
  );

// Names of every routine, and of every property read through one.
const collectRoutineNames = (texts: string[]): Set<string> => {
  const routines = new Set<string>();
  const routinePattern =
    /\b(?:function|procedure|constructor|destructor)\s+(?:\w+\s*\.\s*)*(\w+)/gi;
  for (const text of texts)
    for (const match of text.matchAll(routinePattern))
      routines.add(match[1].toLowerCase());
  const propertyPattern = /\bproperty\s+(\w+)[^;]*?\bread\s+(\w+)/gi;
  for (const text of texts)
    for (const match of text.matchAll(propertyPattern))
      if (routines.has(match[2].toLowerCase()))
        routines.add(match[1].toLowerCase());
  return routines;
};

type Store = { offset: number; target: string; expression: string };

// Every `<stack field>[index](.member|^)* := rhs;` statement.
const findStores = (code: string): Store[] => {
  const stores: Store[] = [];
  const head = new RegExp(
    String.raw`\b(?:\w+\s*\.\s*)*(?:${STACK_FIELDS.join("|")})\s*\[`,
    // Pascal identifiers are case-insensitive.
    "gi",
  );
  for (const match of code.matchAll(head)) {
    const start = match.index!;
    // `@FRegisters[...]` takes an address; it stores nothing.
    const before = code.slice(Math.max(0, start - 1), start);
    if (before === "@") continue;
    let i = start + match[0].length;
    let depth = 1;
    while (i < code.length && depth > 0) {
      if (code[i] === "[") depth++;
      else if (code[i] === "]") depth--;
      i++;
    }
    const tail = /^(?:\s*\[[^\]]*\]|\s*\.\s*\w+|\s*\^)*\s*:=/.exec(
      code.slice(i),
    );
    if (!tail) continue;
    const rhsStart = i + tail[0].length;
    let end = rhsStart;
    let parens = 0;
    while (end < code.length) {
      const ch = code[end];
      if (ch === "(" || ch === "[") parens++;
      else if (ch === ")" || ch === "]") parens--;
      else if (ch === ";" && parens === 0) break;
      else if (
        parens === 0 &&
        /^(?:else|end|until)\b/i.test(code.slice(end, end + 6)) &&
        !/\w/.test(code[end - 1])
      )
        break;
      end++;
    }
    stores.push({
      offset: start,
      target: code.slice(start, rhsStart - 2).replace(/\s+/g, " ").trim(),
      // The index is checked with the right-hand side: the slot's address
      // may be computed before either is evaluated.
      expression: code.slice(start + match[0].length - 1, end),
    });
  }
  return stores;
};

// Calls in an expression that are not on the allowlist.
const disallowedCalls = (
  expression: string,
  routines: Set<string>,
): string[] => {
  const found: string[] = [];
  const token = /((?:\b\w+\s*\.\s*)*)\b(\w+)(\s*[([])?/g;
  for (const match of expression.matchAll(token)) {
    const name = match[2];
    const lower = name.toLowerCase();
    if (/^\d/.test(name) || KEYWORDS.has(lower)) continue;
    // `@Name` takes a routine's address without calling it.
    if (/@\s*$/.test(expression.slice(0, match.index))) continue;
    // `Name[...]` indexes an array; only a call has parentheses.
    const indexed = match[3] !== undefined && match[3].trim() === "[";
    const qualifiers = match[1]
      .split(".")
      .map((part) => part.trim())
      .filter((part) => part !== "");
    const qualified =
      qualifiers.length > 0
        ? `${qualifiers[qualifiers.length - 1]}.${name}`.toLowerCase()
        : lower;
    // Qualifiers are themselves identifiers; check each of them as well.
    for (const qualifier of qualifiers) {
      const q = qualifier.toLowerCase();
      if (routines.has(q) && !ALLOWED_CALLS.has(q)) found.push(qualifier);
    }
    if (indexed) continue;
    const called = match[3] !== undefined;
    const isRoutine = routines.has(lower);
    if (!called && !isRoutine) continue;
    if (ALLOWED_CALLS.has(qualified) || ALLOWED_CALLS.has(lower)) continue;
    if (called && !isRoutine && (ORDINAL_CASTS.has(lower) || /^T[A-Z]/.test(name)))
      continue;
    found.push(qualifiers.length > 0 ? `${qualifiers.join(".")}.${name}` : name);
  }
  return found;
};

const check = (
  files: { rel: string; code: string }[],
  routines: Set<string>,
): string[] => {
  const problems: string[] = [];
  for (const { rel, code } of files) {
    for (const store of findStores(code)) {
      const calls = disallowedCalls(store.expression, routines);
      if (calls.length === 0) continue;
      const line = code.slice(0, store.offset).split("\n").length;
      problems.push(
        `${rel}:${line}: ${store.target} := ... calls ${[...new Set(calls)].join(", ")}`,
      );
    }
  }
  return problems;
};

const selfTest = (): void => {
  const routines = new Set(
    ["contains", "registerboolean", "getproperty", "instantiate", "next",
      "vmrootedbinarypredicate", "registertodouble"],
  );
  const cases: { code: string; bad: boolean }[] = [
    {
      code: "FRegisters[A] := RegisterBoolean((Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName)));",
      bad: true,
    },
    { code: "FRegisters[A] := RegisterBoolean(GuestCallBoolean);", bad: false },
    { code: "FVM.FRegisters[I] := Obj.GetProperty(Name);", bad: true },
    { code: "Self.FRegisters[A] := Obj.GetProperty(Name);", bad: true },
    { code: "fregisters[A] := Obj.GetProperty(Name);", bad: true },
    { code: "Self.FLOCALCELLS[A] := Obj.GetProperty(Name);", bad: true },
    { code: "FRegisters[A] := RegisterToDouble(FRegisters[B]);", bad: true },
    { code: "FRegisters[A] := Iterator.Next;", bad: true },
    { code: "FRegisters[Next(A)] := RegisterUndefined;", bad: true },
    { code: "FRegisters[A] := FRegisters[B];", bad: false },
    { code: "FRegisters[A] := RegisterInt(LongInt(FRegisters[B].IntValue));", bad: false },
    { code: "FLocalCells[A].Value := FRegisters[A];", bad: false },
    { code: "if Ok then FRegisters[A] := Instantiate(X) else Y := 1;", bad: true },
    { code: "Saved := @FFrameStack[FFrameStackCount];", bad: false },
    { code: "FRegisters[I + 1] := Next[I];", bad: false },
    { code: "FRegisters[A] := RegisterBoolean(VMRootedBinaryPredicate(@Next, L, R));", bad: true },
  ];
  let failed = 0;
  for (const { code, bad } of cases) {
    const flagged =
      check([{ rel: "self-test", code: stripCommentsAndStrings(code) }], routines)
        .length > 0;
    if (flagged !== bad) {
      failed++;
      console.error(`self-test: expected ${bad ? "a finding" : "none"}: ${code}`);
    }
  }
  if (failed > 0) process.exit(1);
  console.log(`${cases.length} self-test case(s) passed.`);
};

if (SELF_TEST) {
  selfTest();
} else {
  const paths: string[] = [];
  for (const dir of SOURCE_DIRS) walkSource(dir, paths);
  const files = paths.map((path) => ({
    rel: relative(ROOT, path).split("\\").join("/"),
    code: stripCommentsAndStrings(readFileSync(path, "utf8")),
  }));
  const routines = collectRoutineNames(files.map((file) => file.code));
  if (VERBOSE) {
    let stores = 0;
    for (const { code } of files) stores += findStores(code).length;
    console.log(`${stores} VM stack-slot store(s) found.`);
  }
  const problems = check(files, routines);
  if (problems.length > 0) {
    for (const problem of problems) console.error(problem);
    console.error(
      "\nA call that can run guest code can move the VM stacks, and the " +
        "compiler may compute the slot's address before making the call. " +
        "Evaluate the call into a local and store the local (see the header " +
        "of scripts/check-vm-stack-stores.ts), or add a routine that cannot " +
        "run guest code to ALLOWED_CALLS.",
    );
    process.exit(1);
  }
  console.log(
    `${files.length} Pascal file(s) checked. No VM stack-slot store calls guest code.`,
  );
}
