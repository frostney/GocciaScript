# BigInt primitive support

**Date:** 2026-04-18
**Area:** `runtime`

BigInt primitive type (ES2020). Previously excluded from scope because GocciaScript's use cases did not demand arbitrary-precision integers. Reconsidered because (1) test262 tests for the Temporal API require BigInt as a prerequisite — nanosecond-precision epochs (`Temporal.Instant.epochNanoseconds`) return BigInt values, and (2) several Temporal internals are simplified with BigInt primitives available rather than workarounds. The implementation is self-contained (`TBigInteger` record in `BigInteger.pas`, `TGocciaBigIntValue` wrapping it) with no impact on existing Number paths. [built-ins.md § BigInt](../built-ins.md#bigint-gocciabuiltinsglobalbigintpas). [language-tables.md](../language-tables.md).
