# Type Annotations

*GocciaScript's TypeScript-compatible type syntax: the TC39 Type Annotations proposal, the `--strict-types` runtime extension, and the parsing rules that govern them.*

## Executive Summary

- **Types as comments by default** — Supported annotations follow the [TC39 Type Annotations](https://tc39.es/proposal-type-annotations/) proposal's types-as-comments model and have no runtime effect
- **Optional enforcement** — `--strict-types` checks supported annotations at runtime in both execution modes; it is a runtime contract, not a static structural checker such as `tsc`
- **Extension decides `<`** — A leading `<` is read as JSX or as a type parameter list by file extension, exactly as TypeScript does
- **Restricted `!` productions** — Definite assignment and non-null assertions must sit on the same line as their operand, so ASI code keeps its usual meaning
- **Bounded surface** — Namespaces, parameter properties, angle-bracket assertions, class-field definite assignment, and generic async arrows are deliberately unsupported

The annotation surface is part of GocciaScript's language profile; see
[Language](language.md) for the ECMAScript baseline it sits on and
[Language Tables](language-tables.md) for the proposal-status matrix.

## Proposal and Runtime Model

GocciaScript implements the [TC39 Type Annotations](https://tc39.es/proposal-type-annotations/) proposal. Supported annotations follow its types-as-comments model and have no runtime effect by default. Raw type strings are preserved on AST nodes for potential future optimization.

**GocciaScript runtime extension** — pass `--strict-types` (or set `"strict-types": true` in `goccia.json`) to enforce supported annotations at runtime in both interpreter and bytecode modes. Annotated variables, function parameters, and primitive-literal-inferred types are checked on initial value and on every assignment; incompatible values throw `TypeError`. Union (`string | number`), `any`, and `unknown` annotations remain unenforced. This optional runtime contract is not a replacement for a static structural checker such as `tsc`.

## Supported Syntax

```javascript
// Variable type annotations
let x: number = 42;
const name: string = "hello";
let value: string | number = "test";

// Parameter type annotations (simple, optional, rest, destructuring)
const add = (a: number, b: number): number => a + b;
const greet = (name?: string) => name === undefined ? "hi" : "hi " + name;
const sum = (...nums: number[]) => nums.reduce((a, b) => a + b, 0);
const first = ({ name, age }: { name: string, age: number }) => name;

// Return type annotations, including unions and intersections of object types
const double = (x: number): number => x * 2;
class Registry {
  lookup(flag: boolean): string | { code: number } { return flag ? "ok" : { code: 7 }; }
  build(): { name: string } & { size: number } { return { name: "box", size: 2 }; }
}

// Template literal types
const id: `id-${number}` = "id-42";
const also = "id-42" as `id-${number}`;

// Definite assignment assertions (require an annotation, forbid an initializer)
let pending!: number;
pending = 5;

// Non-null assertions (postfix !, erased — the chain continues unchanged)
const nonNull = { v: "x" };
nonNull.v!.length;
const first = [{ y: 1 }][0]!.y;

// Generic arrow function expressions
const identity = <T,>(v: T): T => v;
const withDefault = <T = string,>(v: T): T => v;

// Type and interface declarations (skipped entirely)
type Point = { x: number, y: number };
interface Animal { name: string; speak(): string; }

// import type / export type (skipped entirely)
import type { Foo } from './types.js';
export type { Bar };

// Mixed type/value named bindings keep runtime imports/exports for value bindings
import { parseSourceFile, type SourceFile } from "./parser.js";
export { value, type ValueShape };

// export interface declarations are skipped entirely
export interface Serializable {
  toJSON(): string;
}

// Runtime-erased as, satisfies, and generic constructor syntax
const x = 42 as number;
const colors = ["red", "green"] as const satisfies Array<string>;
const names = new Map<string, string>();

// Class annotations: field types, generics, implements, access modifiers
class Box<T> implements Container {
  public value: T;
  private label?: string;
  readonly id: number = 1;
  constructor(value: T) { this.value = value; }
  get(): T { return this.value; }
}

// Catch parameter type annotation
try { throw new Error("oops"); } catch (e: Error) { }
```

## Angle Brackets and JSX

A leading `<` is ambiguous: it can open a JSX element or a type parameter list. GocciaScript resolves this by file extension, the same way TypeScript does.

- **`.ts`** — JSX is never recognised, so `<` is always type syntax. Generic function type annotations and every generic arrow form parse here:

  ```typescript
  const identity: <T>(x: T) => T = (x) => x;
  const wrap = <T>(v: T): T[] => [v];
  const idOf = <T extends { id: number }>(v: T): number => v.id;
  ```

- **`.js`, `.jsx`, `.tsx`, `.mjs`** — JSX is recognised, so a bare `<T>` or `<T extends U>` is read as a JSX element. Generic arrows still parse when the type parameter list cannot be mistaken for a tag, which means a trailing comma, a default, or more than one parameter:

  ```javascript
  const identity = <T,>(v: T): T => v;
  const withDefault = <T = string,>(v: T): T => v;
  const pair = <A, B>(a: A, b: B) => [a, b];
  ```

  Use a `.ts` source when an annotation needs the bare `<T>` form.

## Automatic Semicolon Insertion and Type Syntax

Type-level declarations that are skipped rather than parsed (`type`, `import type`, `export type`) end at the first line break that is a legal ASI point. A type argument list wrapped across lines is fine, because its breaks fall after a `<` or `,` or before a `>`; a break at another operator, such as a union `|` leading the next line, ends the declaration early. Terminate multi-line type declarations with an explicit `;`, or keep the operator at the end of the line.

A type query keeps its operand across a line break the same way an operator does, so `let value: typeof` followed by `source` on the next line reads as one annotation.

This only applies when ASI is enabled; see [Language](language.md#automatic-semicolon-insertion) for the opt-in flag and the general rules.

Both `!` forms are restricted productions: the `!` must appear on the same line as the binding name (definite assignment) or its operand (non-null assertion). A `!` at the start of the next line begins a new expression statement, so ASI code such as `let x` followed by `!fn()` keeps its usual meaning. `!=` and `!==` lex as single tokens and are never assertions.

## Not Supported

- Namespaces (`namespace Foo { ... }`).
- Parameter properties in constructors (`constructor(public x: number)`).
- Angle-bracket type assertions (`<string>value`) — use `value as string` instead.
- Definite assignment assertions on class fields (`class C { x!: number }`) — only variable declarations accept `!`.
- Generic **async** arrow expressions (`async <T,>(v: T) => v`) — the generic form is wired into primary-expression position only, so the `async` prefix is not recognised ahead of it.
