# The growth gate collects before refusing, and store paths root their temporaries

**Date:** 2026-08-17
**Area:** `gc`, `sandbox`
**Related:** [ADR 0105](0105-argument-collections-root-their-elements.md), [ADR 0106](0106-sandbox-hardening-scope.md), [ADR 0109](0109-engine-integrity-faults-are-uncatchable.md)
**Extends:** [ADR 0106](0106-sandbox-hardening-scope.md) — its gating decision stands; this records the refusal *procedure* that decision left unspecified

## Context

[ADR 0106](0106-sandbox-hardening-scope.md) decided that native growth points
with no owning value are **gated**: "checked before allocating, never charged",
because a charge would leak budget the engine could never give back. That
decision is not in question here and is not changed by this one — the gate
still checks before allocating and still charges nothing.

What 0106 did not settle is *how* a gated request that does not fit is refused,
and the answer the code gave was an accident of what was safe rather than a
choice. The **charged** path — one with an owner that can release the bytes
again — forces a collection, re-tests, and only then raises the guest-catchable
`RangeError`. The **gated** path refused against instantaneous `BytesAllocated`
without collecting at all. That asymmetry was never argued for anywhere; it was
stated as fact in `docs/garbage-collector.md`, `docs/errors.md` and the gate's
own contract comment, and in no decision record. This is the record it should
have had.

What the asymmetry cost is that the gate's answer did not describe the program.
`RequireNativeBytes` compared a storage doubling against instantaneous
`BytesAllocated`, so whether a doubling was refused depended on how much
collectable garbage happened to be resident at that instant. Measured on a
4 MiB-ceiling property-growth loop, the byte count in the refusal message names
the doubling that lost:

| Workload | Interpreted | Bytecode |
| --- | --- | --- |
| plain loop | refused at 2,359,200 B (C = 32,766) | refused at 1,179,552 B (C = 16,382) |
| same loop with a periodic `Goccia.gc()` | refused at 4,718,496 B (C = 65,534) | refused at 4,718,496 B (C = 65,534) |

Two things are wrong there, and the second is worse than the first. The plain
loop is refused with roughly half the ceiling reclaimable — a guest that
sprinkles `Goccia.gc()` through its own code gets twice the capacity out of the
same budget, which makes the ceiling a measure of the collector's recent luck
rather than of the program. And the two execution modes disagree: automatic
collection is disabled during bytecode execution, so the compiled run reaches
the gate with a dirtier heap and is refused a doubling *earlier* than the
interpreted one. A resource ceiling that moves with the execution mode is a
ceiling neither mode's number means anything in.

The fix is obvious and was implemented, measured to work, and then proven to be
a **use-after-free**. The gate sits *inside* the insertion window. At the moment
`TOrderedStringMap.Add` asks whether the entry array may double, the incoming
property value is held only in a Pascal local and in a `TGocciaPropertyDescriptor`
that no map holds yet — a plain class the collector does not trace. It is
reachable from no GC root at all. A deterministic canary whose `Recycle` records
the sweep showed the collection taking it:

```text
with gate-collect:  [probe] collections during gated growth: 1
                    [probe] in-flight value swept by the gate: TRUE
baseline:           TGocciaMemoryLimitError (0 collections)   # non-vacuity
```

Bytecode mode survived these sites only because `TGocciaVMStackRoot` marks the
whole register stack. Mode-dependent memory safety is not shippable, and native
builders run with Pascal locals in both modes regardless.

So the posture change and the rooting discipline are not two changes that
happened to land together. The first is unsafe without the second, and the
second has no observable payoff without the first. This record covers both.

## Decision

**The growth gate collects once and re-tests before refusing, and every store
path that can reach the gate makes its temporaries reachable first.**

### The rooting half

Rooting is at the **growth gate**, not at the store entry points.
`TGocciaShapedPropertyMap.RequireStorageBytes` and its element-side twin
`TGocciaElementList.RequireStorageBytes` each open a `TGocciaActiveRootFrame`
over two things the collector could otherwise not see — the value that owns the
storage, and the pending value the in-flight store has not written yet — and
consult the budget from inside that window through a virtual
`ConsultStorageBudget`. `TOrderedStringMap.Add` threads the pending value down
through `Grow`, `Compact` and `GateStorageGrowth` for exactly this reason.

The gate rather than the entry points, for two reasons. The gate is consulted
O(log Count) times per map while `DefineProperty` / `TryDefineProperty` /
`AssignProperty` run once per store; measuring the entry-point variant against
the store-hot-path benchmark shapes put it at +2.4%..+4.0%. And rooting at the
map needs no per-call-site work at all — every store that bypasses the
`DefineProperty` family is covered by construction, including the intrinsic
`length`/`name` seeding, native-method registration, the array index-key store,
the class `prototype` store, and the object-literal builders in both executors.

Where there is no storage gate to root at, the caller roots. A **proxy receiver
has no property map**, so `TGocciaProxyValue.PushDefineTrapRoots` covers the
proxy, handler, target and descriptor across a `[[DefineOwnProperty]]` dispatch
that is three guest-code safe points in a row. And a **native builder** roots
what it will read again after the next accessor, trap or callback — a staged
descriptor batch (`Object.defineProperties` collects every descriptor before it
defines any, §20.1.2.3.1), field-by-field descriptor extraction, a detached
side table such as `JSON.parse`'s parse records. `docs/garbage-collector.md`
carries the full contributor-facing statement of all of this; it is not
duplicated here.

`TGocciaPropertyDescriptor.PushRoots` exists so a descriptor can contribute its
roots without becoming a container. [ADR 0105](0105-argument-collections-root-their-elements.md)
made `TGocciaArgumentCollection` a `TGCRootSource`, and the obvious symmetry
would have been to do the same to descriptors. It was rejected: there are ~377
`TGocciaPropertyDescriptorData.Create(` sites, descriptors are created and
discarded on the hot store path, and registering each one with the collector
would put a root-source registration on every property write to buy protection
that is consumed only at the gate. `PushRoots` is the same guarantee paid for
only where it is used.

### The posture half

`TGarbageCollector.TryCollectForLimitedBytes` is the shared last resort: it
forces one collection when a request does not currently fit and a collection
could plausibly change that, then re-tests the fit once. It charges nothing —
the charged path adds the bytes itself once it returns True, and the gate
permits the growth without ever charging — so the same routine serves
`TryReserveExternalBytes` (which previously carried this logic inline) and
`Goccia.MemoryLimit.RequireNativeBytes`.

Three shapes are still refused without walking the heap, because for them no
collection could change the answer:

1. **A request larger than the whole budget.** It never fits however much is
   reclaimed. This is the shape a runaway `new Array(1e8)` produces, and it
   keeps the cheap answer it always had.
2. **A repeat of a request the last forced collection already refused.** Absent
   an intervening collection or counter drop, no collection gets below the
   level the last forced one left, so a request that does not fit beside
   `FForcedCollectFloor` cannot be made to fit either. The precondition is
   load-bearing: objects that merely *become* unreachable do not move the
   counters, so a guest that drops a large structure and retries can be refused
   from the record when a fresh collection would fit it — the guest-side escape
   is `Goccia.gc()`, which clears the floor, exactly as
   `docs/garbage-collector.md` advises before retrying.
3. **A re-entrant call from inside the collector** (`FCollecting` or
   `FMemoryLimitFiring`), which means the caller is already on the collector's
   own path.

**The floor is shared by both refusal paths on purpose.** It records a fact
about the heap — "the last forced collection left this many bytes live" — not
about the caller, and both paths force the same full collection and then apply
the same fit test, so a level that defeated one defeats the other at the same
request size. A second, gate-private floor would only buy each path the right
to re-learn what the other just proved. The floor is per request size, so a
smaller request it does not rule out still forces its own collection, and any
ordinary collection — threshold, young-generation, pressure checkpoint, or an
explicit `Goccia.gc()` — clears it and re-arms forcing.

Point 2 is what keeps a guest retry loop at O(1) per attempt instead of a full
mark-and-sweep each time. It is a real attack surface and not a micro-optimisation:
without it, a script that catches the charged `RangeError` and retries in a loop
bills the engine an unbounded number of heap walks for free.

### `AProtect` stays nil at the gate's call site

`TryCollectForLimitedBytes` takes an optional `AProtect` for callers whose
stack-held value nothing else roots; `TryReserveExternalBytes` passes its
caller's through. `Goccia.MemoryLimit.RequireNativeBytes` passes **nil**, and
that is a decision rather than an omission.

A single `AProtect` is one object. What is live across a gated growth is at
least two — the storage's owner and the pending value — and on the property
side the pending value arrives inside a descriptor that may carry a getter and
a setter as well. `AProtect` cannot express that. More importantly, the gate is
the wrong place to learn it: `RequireNativeBytes` is handed a byte count and
knows nothing about the store that produced it. The callers do, and they have
already opened a frame over exactly the right set before consulting the budget.
Threading a value down to the gate as well would duplicate a guarantee that is
already established, in a weaker form, at the point with the least context.

So the invariant is stated where it can be checked: **anything that calls
`RequireNativeBytes` must have made its temporaries reachable first.** The unit's
contract comment says so, and both gates satisfy it by construction because
`RequireStorageBytes` is the only route to `ConsultStorageBudget`.

### What does not change

Two things, stated because a reader could reasonably expect either to have
moved. **The gate still charges nothing** — [ADR 0106](0106-sandbox-hardening-scope.md)'s
actual decision is untouched; a gated growth is checked before allocating and
never enters `BytesAllocated`. And **the refusal stays uncatchable**: when the
post-collection heap genuinely cannot fit the growth, the gate raises
`TGocciaMemoryLimitError`, which is opaque to the guest at every boundary that
converts a Pascal exception into a script value — a ceiling the guest can catch
is a ceiling the guest can ignore in a loop.

## Consequences

**Capacity goes up, and stops depending on luck.** All four cells of the table
above now read 4,718,496 B (C = 65,534): the arithmetic crossing, reached
whether or not the guest collects for itself and whichever executor runs it.
That is a 2x capacity gain on the interpreted path and a 4x one on the compiled
path, and the asymmetry in the gain is the point — the mode that was worst off
is the one that gains most, because its disadvantage was exactly the garbage the
gate now collects. A CLI probe asserts the equality directly — the same workload
with and without a periodic `Goccia.gc()` must be refused the *same* byte count
— which is a sharper statement than any single number and needs no re-tuning per
pointer width.

**It does not follow that collecting always buys capacity, and the suite says
so.** The gain above is large because that workload's live set is near zero.
Where the live set dominates — the parked-heap assignment probe in
`scripts/test-cli.ts`, whose ballast is live by construction and whose loop
holds a 4000-element iterable throughout — the forced collection finds little
and the crossing does not move at all: measured, the same 73,632-byte doubling
is refused before and after, at parked slack 136,498 and 133,802 respectively.
Both shapes are kept deliberately. A suite carrying only the first would license
the reading that the gate now makes budgets elastic, which it does not: it makes
them describe the live set instead of the recent allocation history.

**The mode asymmetry disappears, and that is the part worth watching.** Its
cause has not gone away: automatic collection is still disabled during bytecode
execution, so the compiled run still arrives at the gate with a dirtier heap
than the interpreted one. What changed is that the gate no longer *reads* that
difference. The interpreter's pressure checkpoints (`CollectForMemoryPressure`
at a fixed reserve below the ceiling) frequently collect before the gate is
reached at all, so on the interpreted path the forced collection is often
redundant; **bytecode mode is where the gate genuinely bites.** Any future
measurement of this gate should be taken in bytecode mode, or it will
under-report what the gate is doing.

**[ADR 0109](0109-engine-integrity-faults-are-uncatchable.md)'s guarantee
becomes load-bearing.** Before this change, a missed root on a store path was
latent: the gate never collected, so nothing dispatched through a freed value.
It is now a live safe point that ordinary guest code reaches under memory
pressure. A root missed anywhere in the discipline above surfaces as
`EObjectCheck` in a development build and a read into freed memory in a
production one — and 0109 is what stops `catch (e)` from absorbing it and
carrying on. The two changes are complementary: this one adds the safe point,
0109 makes a mistake at it fail loudly instead of silently.

**Refusals cost one heap walk each, bounded by the floor.** The forced
collection fires only on paths that were about to refuse, so no hot path pays
for it: a growth that fits is answered by the same counter comparison as before.

**ADR 0106's aggregate caveat is untouched.** The budget still bounds the size
of any one gated allocation and not resident memory, for the reason
[Amendment 1](0106-sandbox-hardening-scope.md#amendment-1--the-aggregate-caveat-measured)
measures: property descriptors and key strings are not GC-registered, so the
figure the gate compares against barely moves as properties accumulate.
Collecting first makes the gate's answer *honest*; it does not make it
*complete*.

It does move resident memory, and in the direction the amendment predicts. A
larger reachable capacity means a refused workload is holding more descriptors
and keys at the moment it is finally refused — none of which the budget sees.
Measured on the amendment's own 4M-property workload at a 16 MiB ceiling,
production builds, before -> after: interpreted 80.5 -> 101.7 MiB, compiled
73.8 -> 73.9 MiB. The asymmetry is fully explained by the refusal point:
interpreted was previously refused a 4,718,496-byte doubling and is now refused
an 18,874,272-byte one, two doublings later (4,718,496 → 9,437,088 →
18,874,272), while compiled already reached the
later doubling and does not move. Both stay far under the 384 MiB ceiling
`scripts/test-cli-apps.ts` asserts, which is the check to watch if the aggregate
hole is ever narrowed.

**Both gates' rooting windows were re-audited and hold.** On the property side
the argument is short: nothing after the consult allocates a managed object at
all — `Grow`'s `Rehash` and `Compact`'s copy are `SetLength` on plain dynamic
arrays and record moves — so there is no allocation for a collection to hang
off even in principle.

The element side needed the longer check.
`TGocciaElementList.RequireStorageBytes` closes its frame when the consult
returns — before the hole fill and the caller's post-gate store — on the
invariant that the gate is the only prospective collection point in the
extension window. That invariant was a precaution while the gate could not
collect; it is now what the store's safety rests on, so it was re-checked
directly. It holds, and the argument is unchanged in shape: the collection the
gate now takes happens *inside* the frame, in `ConsultStorageBudget`, with the
owner and pending value already pushed. Everything after the frame closes only
allocates — the fill appends the pinned hole singleton and grows a plain dynamic
array, the timeout poll raises but never collects, and
`TGocciaValue.AfterConstruction` registers and may raise but never collects — so
no post-gate step can reclaim what the caller reads back.

## Coverage

`Goccia.MemoryLimit.Test.pas` carries the contract:

- both gates collect once and re-test before refusing, asserted as an exact
  `TotalCollections` delta;
- an over-whole-budget request refuses with the delta unchanged at zero;
- a retry loop of eight identical refused requests collects exactly once, and an
  ordinary `Collect` re-arms forcing;
- the canary suites, in two variants each. The **seam** variant injects a
  collection at `ConsultStorageBudget` — deterministic on every pointer width,
  and it pins the window rather than the budget arithmetic. The **real-gate**
  variant arms a ceiling the growth cannot fit and lets `RequireNativeBytes`
  itself take the collection. Neither substitutes for the other: a seam that
  drifted from the gate would still pass, and a real-gate run alone could not
  distinguish "collected and everything survived" from "never collected". Both
  carry the vacuity control — a value never handed to the store *must* be swept
  by the same collection.

`scripts/test-cli.ts` asserts the capacity result end to end in both execution
modes, and the existing growth-gate opacity family continues to assert that a
refusal never becomes guest-visible.
