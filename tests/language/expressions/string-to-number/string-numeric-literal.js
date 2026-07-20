/*---
description: StringNumericLiteral conversion agrees across runtime coercion and bytecode constant folding
features: [type-coercion, unary-plus]
---*/

// `+"literal"` is constant-folded by the bytecode compiler and evaluated at
// runtime by the interpreter, so the same assertion guards both paths once the
// suite runs in --mode=bytecode as well. `dyn` takes its operand as a parameter
// that cannot be folded, pinning the runtime VM path even in bytecode mode.
const dyn = (s) => Number(s);

describe('StringNumericLiteral conversion', () => {
  describe('whitespace and empty input', () => {
    test('empty string converts to +0', () => {
      expect(+"").toBe(0);
      expect(dyn("")).toBe(0);
    });

    test('whitespace-only string converts to +0', () => {
      expect(+" \t\n\r ").toBe(0);
      expect(dyn(" \t\n\r ")).toBe(0);
    });

    test('leading and trailing whitespace is trimmed', () => {
      expect(+"  42  ").toBe(42);
      expect(dyn("\t 42 \n")).toBe(42);
    });
  });

  describe('signed Infinity', () => {
    test('Infinity converts to Infinity', () => {
      expect(+"Infinity").toBe(Infinity);
      expect(dyn("Infinity")).toBe(Infinity);
    });

    test('+Infinity converts to Infinity', () => {
      expect(+"+Infinity").toBe(Infinity);
      expect(dyn("+Infinity")).toBe(Infinity);
    });

    test('-Infinity converts to -Infinity', () => {
      expect(+"-Infinity").toBe(-Infinity);
      expect(dyn("-Infinity")).toBe(-Infinity);
    });

    test('abbreviated Inf is not a numeric literal', () => {
      expect(Number.isNaN(+"Inf")).toBe(true);
      expect(Number.isNaN(dyn("Inf"))).toBe(true);
    });
  });

  describe('non-decimal integer literals', () => {
    test('hex prefix converts in either case', () => {
      expect(+"0xFF").toBe(255);
      expect(+"0X10").toBe(16);
      expect(dyn("0xff")).toBe(255);
    });

    test('binary prefix converts in either case', () => {
      expect(+"0b101").toBe(5);
      expect(+"0B11").toBe(3);
      expect(dyn("0b101")).toBe(5);
    });

    test('octal prefix converts in either case', () => {
      expect(+"0o17").toBe(15);
      expect(+"0O20").toBe(16);
      expect(dyn("0o17")).toBe(15);
    });

    test('large hex keeps full magnitude without wrapping', () => {
      expect(+"0xABCDEF12345").toBe(11806310474565);
      expect(dyn("0xABCDEF12345")).toBe(11806310474565);
    });

    test('non-decimal literals reject a leading sign', () => {
      expect(Number.isNaN(+"+0x10")).toBe(true);
      expect(Number.isNaN(+"-0x10")).toBe(true);
      expect(Number.isNaN(dyn("+0b1"))).toBe(true);
    });

    test('a prefix with no digits is NaN', () => {
      expect(Number.isNaN(+"0x")).toBe(true);
      expect(Number.isNaN(+"0b")).toBe(true);
      expect(Number.isNaN(+"0o")).toBe(true);
    });

    test('digits out of range for the radix are NaN', () => {
      expect(Number.isNaN(+"0b12")).toBe(true);
      expect(Number.isNaN(+"0o18")).toBe(true);
    });
  });

  describe('decimal literals', () => {
    test('integers, fractions, and exponents', () => {
      expect(+"00123").toBe(123);
      expect(+".5e3").toBe(500);
      expect(+"5.").toBe(5);
      expect(dyn("+.5")).toBe(0.5);
    });

    test('magnitude overflow becomes signed Infinity', () => {
      expect(+"1e400").toBe(Infinity);
      expect(+"-1e400").toBe(-Infinity);
    });

    test('decimal text rounds once to the nearest binary64 value', () => {
      expect(dyn("0.30000000000000004")).toBe(0.1 + 0.2);
      expect(String(dyn("0.30e-143"))).toBe("3e-144");
      expect(dyn("9007199254740993")).toBe(9007199254740992);
      expect(dyn("2.4703282292062327e-324")).toBe(0);
      expect(dyn("2.4703282292062328e-324")).toBe(Number.MIN_VALUE);
      expect(dyn("1.7976931348623158e308")).toBe(Number.MAX_VALUE);
      expect(dyn("1.7976931348623159e308")).toBe(Infinity);
    });
  });

  describe('signed zero', () => {
    test('a negative-zero string preserves -0', () => {
      expect(Object.is(+"-0", -0)).toBe(true);
      expect(Object.is(+"-0.00", -0)).toBe(true);
      expect(Object.is(dyn("-0"), -0)).toBe(true);
    });

    test('positive and unsigned zero stay +0', () => {
      expect(Object.is(+"0", 0)).toBe(true);
      expect(Object.is(+"+0", 0)).toBe(true);
    });
  });

  describe('inputs that are not numeric literals', () => {
    test('letters convert to NaN', () => {
      expect(Number.isNaN(+"abc")).toBe(true);
    });

    test('lone punctuation converts to NaN', () => {
      expect(Number.isNaN(+".")).toBe(true);
      expect(Number.isNaN(+"+")).toBe(true);
      expect(Number.isNaN(+"-")).toBe(true);
    });

    test('numeric separators and grouping convert to NaN', () => {
      expect(Number.isNaN(+"1_000")).toBe(true);
      expect(Number.isNaN(+"1,000")).toBe(true);
    });

    test('trailing garbage converts to NaN', () => {
      expect(Number.isNaN(+"1.5d")).toBe(true);
      expect(Number.isNaN(+"0x1p4")).toBe(true);
    });
  });
});
