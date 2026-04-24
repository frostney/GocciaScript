unit Goccia.Constants.NumericLimits;

{$I Goccia.inc}

// ECMAScript-defined numeric limits used across the engine.
//
// Names and values match the ECMA-262 specification directly so that call
// sites can be read against the spec text without translating magic
// literals.  Floating-point forms (`*_F`) are provided for call sites that
// need a `Double` to compare against raw-length values that have not been
// truncated to Integer.

interface

const
  // Number.MAX_SAFE_INTEGER — ES2026 §21.1.2.6.  Also the upper bound on
  // the length of any spec-defined array-like object (see ES2026 §7.1.22
  // ToLength and §23.1.3.* length-modifying Array methods).
  MAX_SAFE_INTEGER              = 9007199254740991;
  MAX_SAFE_INTEGER_F: Double    = 9007199254740991.0;

  // Number.MIN_SAFE_INTEGER — ES2026 §21.1.2.8.
  MIN_SAFE_INTEGER              = -9007199254740991;
  MIN_SAFE_INTEGER_F: Double    = -9007199254740991.0;

  // 2^32 - 1.  Upper bound accepted by ArrayCreate (ES2026 §10.4.2.2) and
  // the `length` of an Array exotic object.  Also `ToUint32(x) + 1` when
  // `x` is the maximum representable UInt32.
  MAX_ARRAY_LENGTH              = 4294967295;
  MAX_ARRAY_LENGTH_F: Double    = 4294967295.0;

  // 2^32.  Modulus used by ToUint32 (ES2026 §7.1.7) and by Temporal
  // calendar-unit overflow checks.
  UINT32_MODULUS                = 4294967296;
  UINT32_MODULUS_F: Double      = 4294967296.0;

  // Number.MAX_VALUE — ES2026 §21.1.2.7.  Largest finite positive IEEE 754
  // double-precision value.
  NUMBER_MAX_VALUE: Double      = 1.7976931348623157e+308;

  // Number.MIN_VALUE — ES2026 §21.1.2.9.  Smallest positive subnormal
  // IEEE 754 double-precision value.
  NUMBER_MIN_VALUE: Double      = 5e-324;

  // Number.EPSILON — ES2026 §21.1.2.1.  Difference between 1 and the next
  // representable IEEE 754 double.
  NUMBER_EPSILON: Double        = 2.2204460492503131e-16;

implementation

end.
