# Per-engine realm isolation

**Date:** 2026-04-25
**Area:** `runtime`
**Pull Request:** [#403](https://github.com/frostney/GocciaScript/pull/403)

Per-engine realm for built-in prototype isolation. `TGocciaRealm` (`Goccia.Realm.pas`) owns the mutable intrinsic state — `Array.prototype`, `Object.prototype`, `Map.prototype`, every error prototype, every Temporal prototype, etc. Each `TGocciaEngine` constructs its own realm and frees it in `Destroy`, which unpins every prototype the realm owns; the next engine on the same worker thread sees pristine intrinsics. Replaces the unit-level `threadvar` caches that previously survived engine teardown and the `prototypeIsolation.js` harness that tried to undo mutations from JS (and could not reverse non-configurable property additions). Two slot kinds: `TGocciaRealmSlotId` for `TGCManagedObject` prototypes (pinned in `SetSlot`, unpinned at tear-down), and `TGocciaRealmOwnedSlotId` for plain-`TObject` helpers like `TGocciaSharedPrototype` (Free-d at tear-down before pin release so destructors can still unpin owned objects). [core-patterns.md § Realm Ownership & Slot Registration](../core-patterns.md#realm-ownership--slot-registration). [embedding.md § Engine Lifecycle & Realm Isolation](../embedding.md#engine-lifecycle--realm-isolation).
