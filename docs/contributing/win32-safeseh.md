# Win32 exception metadata

## Executive Summary

- Every FPC Win32 executable gets a checked SafeSEH table after linking.
- The table lists the handlers from reviewed, pinned toolchain inputs.
- Unsupported images or changed audited inputs fail the build.
- Ordinary project routines with handler-like names are pinned to their own source text.
- Windows CI compares exception behavior before and after the metadata change.

## Build behavior

Every FPC `i386-win32` executable built by `build.pas` or the full CI compiler
driver receives a SafeSEH table before the build succeeds. Windows can then
validate exception handlers against the table instead of querying virtual memory
for each frame. The post-link step is Pascal and adds no Python requirement to
local builds. Other targets, including `x86_64-win64`, keep their existing output.

The build requests FPC's linker map, verifies the audited toolchain and handler
manifest, and appends a read-only load-configuration section containing sorted
handler RVAs. Original code and data sections remain unchanged. Unsupported
images fail the build: signed or relocatable images, existing load configuration,
unknown handlers, changed audited objects, and malformed or unsupported PE layouts
are never silently accepted. Sign an executable only after this step.

For cross-compilation, `FPC_TARGET_CPU=i386 FPC_TARGET_OS=win32 ./build.pas --prod`
selects the target; install that target's FPC units first. CI uses the same
post-link implementation and checks every staged `.exe` before upload.

Each unpinned input must be a compiler output in the build's designated unit
directory and match a unique repository source. Finalization records source and
object hashes in `<map>.safeseh-inputs`, bound to the map, image and compiler
version. Keep this receipt beside the map for staged checks after compiler
scratch files are deleted. Receipts are local build evidence, not signatures;
the sources and pinned toolchain objects must remain available for checking.

## Reviewed registrations

The manifest records the exact `-iW` version (`3.2.4-rc2+1`) and complete object
SHA-256 values from the `3.2.4-rc2-b1` toolchain release. It covers supported
linked units that register handlers as well as the objects defining them.
The map supplies their final addresses; it is not the completeness proof.

| Entry | Registration or replacement |
|---|---|
| `__FPC_finally_handler` | Compiler-generated `try/finally` frames |
| `__FPC_except_handler` | Compiler-generated untyped `try/except` frames |
| `__FPC_on_handler` | Typed `on` clauses |
| `__FPC_except_safecall` | `safecall` exception frames |
| `__FPC_DEFAULT_HANDLER` | Startup frame and hardware exception translation |
| `NullHandler` | Existing-frame replacement during raise and cleanup |
| `NestedHandler` | Nested unwind protection through a frame-pointer store |
| `Signals_exception_handler` | `signals.o` replaces the existing system frame's handler |

`CommonHandler` is called directly, and `API_signals_exception_handler` is an
unhandled-exception filter callback; neither is a separate SEH frame entry.
Their ordinary-callback classifications are explicit and object-hash-bound.
Chain-restoration operations reuse saved frames instead of adding new entries.
An unlinked optional unit needs no table entry. A linked handler can be absent
only when the linker explicitly records its section as removed.

## Ordinary project helper names

A project routine whose name contains `handler` fails the build as a possible
SEH handler until the manifest classifies it. An ordinary routine gets a
`source-nonseh` entry that names its source file, the SHA-256 of its own
implementation, and its section:

```text
source-nonseh ../../source/units/Goccia.VM.pas <sha256> .text.n_goccia.vm$_$tgocciavm_$__$$_growhandlerstack$longint
```

The digest covers the routine from its header through the semicolon after its
final `end`, with line endings normalised to LF. Edits elsewhere in the file
need no manifest change. When the routine itself changes, finalization and
`--check` fail naming the routine, file and lines; when it is renamed or
removed, they fail naming the routine and file. Then:

1. Review the routine again: it must not be, or be installed as, an OS
   exception handler.
2. Run the helper, which prints every entry with its routine's current digest
   and status and never edits the manifest:

   ```bash
   ./safeseh.pas --digests
   ```

3. Copy the reported line for the routine you reviewed into
   `config/win32-safeseh/fpc-3.2.4.manifest`.

Do not refresh a digest you have not reviewed, and do not turn an unknown-name
refusal into a broad exclusion. A new handler-like routine needs its own entry.

The locator refuses whatever it cannot pin exactly, and the build fails
instead of falling back to a wider span. [ADR 0180](../adr/0180-reviewed-non-seh-routines-are-pinned-to-their-own-source.md)
lists the cases, among them overloads of a reviewed name, conditionals that
change what the parse sees, and includes that change the block structure.
Rename or restructure the code, or extend `source/shared/FPCRoutineSource.pas`
with tests. An error that says a file cannot be parsed names the line to fix;
until then every entry in that file fails, whether or not its routine changed.

`FPCSafeSEH.Test` runs the same check on the repository manifest in pull
request CI, so an edited pinned routine fails there before the Win32 build.

## Re-auditing a toolchain update

The reviewed inputs are recorded in [`config/win32-safeseh/fpc-3.2.4.manifest`](../../config/win32-safeseh/fpc-3.2.4.manifest). A toolchain bump requires a new audit, not merely replacing hashes. Inspect the
new RTL object code and relocations for frame installation, handler replacement,
and restoration, including registrations through ordinary pointer stores.
Reconcile every handler symbol with the live linker maps of all executables,
update the reviewed manifest's version and object SHA-256 values, and rerun the
PE refusal tests plus full Windows CI. CI compares Pascal exceptions, hardware
faults, nested exceptions, and the Proxy recursion case with a temporary copy
whose SafeSEH directory reference is removed. Both variants must preserve the
same outcomes. See [the decision](../adr/0169-win32-safeseh-post-link.md).

A toolchain bump also re-checks the
[reviewed project routines](#ordinary-project-helper-names): run
`./safeseh.pas --digests`, and confirm that the new compiler still names
project sections in the shapes the routine locator accepts.

Retain the new compiler and unit archive, query the actual compiler with
`fpc -Pi386 -Twin32 -iW`, and inspect object disassembly with relocations
(`objdump -dr -Mintel`). Check both direct and register-indexed `FS` writes,
ordinary stores to existing frame fields, and callback replacement/restoration.
Review new linked units explicitly. Confirm symbol aliases and defining-object
ownership against production and development maps before approving new hashes.
CI runs the paired exception probes for both production and development images,
including images containing retained debug metadata.

The binary layout follows Microsoft's [SafeSEH documentation](https://learn.microsoft.com/en-us/cpp/build/reference/safeseh-image-has-safe-exception-handlers)
and [load-configuration definition](https://learn.microsoft.com/en-us/windows/win32/api/winnt/ns-winnt-image_load_config_directory32).
