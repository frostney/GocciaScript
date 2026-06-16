# BigInt typed arrays

**Date:** 2026-04-20
**Area:** `runtime`
**Pull Request:** [#365](https://github.com/frostney/GocciaScript/pull/365)

BigInt64Array/BigUint64Array (ES2020). Adds the two BigInt-typed array constructors, completing typed array coverage. Elements are BigInt values; setting non-BigInt throws TypeError. All prototype methods (sort, indexOf, includes, fill, find, map, filter, reduce, etc.) handle BigInt kinds correctly. [built-ins-binary-data.md](../built-ins-binary-data.md).
