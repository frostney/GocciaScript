// Battery A — type-annotation parser edges around the 0.11.0 fixes.
// Runs under goccia (types-as-comments) and bun (TS transpile) identically.

type Pair = { a: string; b: number };
type Cond<T> = T extends string ? "s" : "n";
type Keys = keyof Pair;
type Mapped = { [K in keyof Pair]: Pair[K] | null };
type Tuple = [string, number, boolean?];

const nestedGenericNew = new Map<string, Map<string, number[]>>();
nestedGenericNew.set("outer", new Map([["inner", [1, 2]]]));

const tripleNested = new Map<string, Map<string, Map<string, number>>>();

function unionReturn(flag: boolean): string | { a: number } {
  return flag ? "s" : { a: 1 };
}

function intersectionReturn(): { a: string } & { b: number } {
  return { a: "x", b: 1 };
}

function genericConstraint<T extends { id: string }>(v: T): T["id"] {
  return v.id;
}

const genericFnType: <T>(x: T) => T = (x) => x;

const withDefault = <T = string,>(v: T): T => v;

const satisfiesNested = {
  outer: { inner: [1, 2, 3] },
} as const satisfies { outer: { inner: readonly number[] } };

const tup: Tuple = ["x", 1];
const indexSig: { [key: string]: number } = { a: 1 };
const templateLit: `id-${number}` = "id-42" as `id-${number}`;
let definite!: number;
definite = 5;

const maybe: { v?: string } = {};
const nonNull = { v: "x" as string | undefined };

describe("type-syntax edges", () => {
  test("nested generic new — >> disambiguation", () => {
    expect(nestedGenericNew.get("outer")?.get("inner")).toEqual([1, 2]);
  });

  test("triple-nested generic new — >>> disambiguation", () => {
    expect(tripleNested.size).toBe(0);
  });

  test("union return type on function declaration", () => {
    expect(unionReturn(true)).toBe("s");
    const o = unionReturn(false);
    expect(typeof o).toBe("object");
  });

  test("intersection return type", () => {
    expect(intersectionReturn().b).toBe(1);
  });

  test("generic constraint with indexed-access return type", () => {
    expect(genericConstraint({ id: "abc" })).toBe("abc");
  });

  test("generic function-type annotation on const", () => {
    expect(genericFnType(7)).toBe(7);
  });

  test("generic arrow with default type param", () => {
    expect(withDefault("hi")).toBe("hi");
  });

  test("satisfies with nested readonly shape", () => {
    expect(satisfiesNested.outer.inner[2]).toBe(3);
  });

  test("tuple / index-signature / template-literal / definite assignment", () => {
    expect(tup[1]).toBe(1);
    expect(indexSig.a).toBe(1);
    expect(templateLit).toBe("id-42");
    expect(definite).toBe(5);
  });

  test("optional property and non-null assertion", () => {
    expect(maybe.v).toBeUndefined();
    expect(nonNull.v!.length).toBe(1);
  });

  test("legitimate comparison a < b > c still comparison", () => {
    const a = 1;
    const b = 2;
    const c = false;
    // (1 < 2) => true; true > false => true
    expect(a < b > c).toBe(true);
  });

  test("legitimate less-than before parenthesised expression", () => {
    const f = 3;
    const g = 5;
    expect(f < (g + 1)).toBe(true);
  });

  test("conditional/mapped/keyof types accepted as annotations", () => {
    const k: Keys = "a";
    const c: Cond<string> = "s";
    const m: Mapped = { a: null, b: 2 };
    expect(k).toBe("a");
    expect(c).toBe("s");
    expect(m.b).toBe(2);
  });
});
