/*---
description: Annotated top-level consts keep their value and type when read from functions under strict types
features: [types-as-comments, strict-type-enforcement, const-declaration]
---*/

const TYPED_LIMIT: number = 16;
const TYPED_LABEL: string = "limit";
const TYPED_FLAG: boolean = true;
const TYPED_TOTAL: number = TYPED_LIMIT * 2;

describe("annotated top-level const read", () => {
  test("functions read the annotated value", () => {
    const double = (): number => TYPED_LIMIT * 2;
    const label = (): string => TYPED_LABEL + "!";
    expect(double()).toBe(32);
    expect(label()).toBe("limit!");
    expect((() => TYPED_FLAG)()).toBe(true);
    expect((() => TYPED_TOTAL)()).toBe(32);
  });

  test("the value passes a matching annotation and fails a mismatching one", () => {
    const accept = (): number => {
      const local: number = TYPED_LIMIT;
      return local;
    };
    const reject = () => {
      const local: string = TYPED_LIMIT;
      return local;
    };
    const takesString = (value: string) => value;
    expect(accept()).toBe(16);
    expect(reject).toThrow(TypeError);
    expect(() => takesString(TYPED_LIMIT)).toThrow(TypeError);
    expect(takesString(TYPED_LABEL)).toBe("limit");
  });

  test("assignment throws and leaves the value unchanged", () => {
    expect(() => {
      TYPED_LIMIT = 5;
    }).toThrow(TypeError);
    expect((() => TYPED_LIMIT)()).toBe(16);
  });
});
