# Reject UPX packing

**Date:** 2026-05-18
**Area:** `build`

Do not add UPX packing to the default release pipeline. A local binary-size spike showed large compressed file-size reductions after the existing production build and CI artifact strip, but macOS required UPX packing to be forced and the packed executable was killed at runtime; the remaining cross-platform upside does not outweigh code-signing, antivirus/reputation, and support friction for public release artifacts. Continue relying on FPC production flags plus release stripping, and pursue source-level/runtime-surface reductions instead. [build-system.md](../build-system.md).
