# Layered Temporal time zone providers

**Date:** 2026-04-28
**Area:** `runtime`
**Pull Request:** [#440](https://github.com/frostney/GocciaScript/pull/440)

Temporal named time zones use layered providers in this order: `GOCCIA_TZDIR`, Unix TZif files, Windows ICU, then embedded TZif resource fallback. The embedded IANA payload is generated as `Generated.TimeZoneData.res` with a small linker unit instead of a large Pascal array, keeping portability while leaving lookup and parsing logic in hand-authored units. [Temporal Time Zone Data](../built-ins-temporal.md#time-zone-data). [Generated Timezone Data](../build-system.md#generated-timezone-data).
