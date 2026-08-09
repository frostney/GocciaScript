# CLI Conventions

*Argument, stdin, exit-code, stream, and help conventions shared by the Goccia command-line binaries.*

## Executive Summary

- **No-argument rule** — a command that defaults its input to stdin must print help and quit when run with no input at an interactive terminal, never block on `ReadLn`
- **Stdin** — implicit for pipes and redirects, explicit via `-`, and only ever the sole input
- **Exit codes** — `0` success, `1` failure, `2` unusable invocation; `124` is reserved for the test262 timeout marker
- **Streams** — machine-readable output modes own stdout; new diagnostics go to stderr
- **Help** — every option is described by its own declaration, so `--help` is generated, not hand-maintained

## The no-argument rule

Several binaries take their program source from standard input when no path is given. Without a guard, running one bare at a terminal blocks inside `ReadLn` until the user presses Ctrl-D — the command looks hung, with no output explaining why.

[The Command Line Interface Guidelines](https://clig.dev/) state the rule plainly: if a command expects something piped to it and stdin is an interactive terminal, it should display help immediately and quit, rather than just hanging the way `cat` does.

So, for every stdin-defaulting binary:

| Invocation | stdin | Behavior |
|---|---|---|
| no input arguments | interactive terminal | print help + hint to **stderr**, exit `2` |
| no input arguments | pipe, redirect, or closed | read the program from stdin |
| `-` as the sole input | anything, terminal included | read the program from stdin |
| one or more paths | anything | read those paths, ignore stdin |

`-` is the documented escape hatch: passing it opts back in to reading the terminal, so the blocking behavior is still reachable on purpose.

### Where the rule lives

The decision is a pure function in `Goccia.CLI.Stdin`, taking three booleans — has input arguments, has an explicit `-`, stdin is a terminal — and returning what the command should do. Keeping it free of I/O is what makes the whole matrix unit-testable without a real terminal; the platform TTY probe sits beside it but is never called from it.

`TGocciaCLIApplication` applies the decision once for every subclass. A command opts in by overriding `StdinUsage`; the default opts out, so commands that never source a program from stdin are untouched. Binaries with their own argument parser call the same decision function directly rather than restating the policy.

Do not re-derive the rule per binary. A second copy will drift.

### Testing it

CI cannot allocate a pseudo-terminal from the CLI harness, so the split is:

- The decision matrix is covered by Pascal unit tests over the pure function.
- `scripts/test-cli-apps.ts` covers every **non**-terminal path — piped stdin, closed stdin, explicit `-` — plus the `--help` text. These are the paths the rest of the test harness depends on, and they must stay byte-for-byte unchanged.
- The terminal path is verified by hand under a pty. The `script` invocation differs by platform:
  - BSD/macOS: `script -q /dev/null ./build/GocciaTestRunner < /dev/null`
  - Linux (util-linux): `script -q -c "./build/GocciaTestRunner" /dev/null < /dev/null`

## Stdin conventions

- **`-` means standard input.** It is recognized by `IsStdinPath` and is the only spelling; a file literally named `-` is not addressable and must be passed as `./-`.
- **Stdin is only ever the sole input.** Mixing `-` with file paths is rejected rather than silently interleaved, because the ordering of a stream against on-disk files is not meaningful.
- **Source read from stdin is named `<stdin>`.** Diagnostics, JSON envelopes, and source maps all use that name, so error output is stable regardless of how the source arrived.
- **Implicit stdin is for pipes.** It exists so `goccia < app.js` and `producer | goccia` work without ceremony. It is not an interactive input mode — that is what `GocciaREPL` is for.

## Exit codes

The codes actually in use, which new commands should follow:

| Code | Meaning |
|---|---|
| `0` | Success |
| `1` | The work was attempted and failed — a script threw, a test failed, a path did not exist, an option value was invalid |
| `2` | The command could not be run as invoked — no input at a terminal, a missing required argument |
| `124` | test262 timeout marker, `GocciaScriptLoaderBare` only |

Code `2` is the narrower one: it means the process did no work because the invocation itself was unusable. `GocciaWasmTestRunner` has used it for a missing manifest since it was introduced, and `GocciaTOMLComplianceRunner` uses it for an unusable invocation.

Be aware that the boundary is not clean everywhere yet. Errors raised out of `Execute` are caught centrally and become exit `1` regardless of whether they were a bad flag or a failed script, so some invocation errors — an unknown option, a rejected flag combination — still exit `1`. Do not treat that as the pattern to copy; route genuinely new usage errors to `2`.

## Streams

- **A machine-readable output mode owns stdout.** When `--output=json` or another structured mode is active, stdout must contain the envelope and nothing else — no progress markers, no per-test symbols, no summaries. Anything a human would want during such a run goes to stderr or is suppressed. This is why the runners gate reporter output on the output mode rather than only gating the final summary.
- **New diagnostics go to stderr.** The no-argument help, path-not-found errors in the runners, and configuration warnings are all written there.
- **Existing placement is uneven.** `GocciaScriptLoader` routes uncaught errors, syntax errors, and option errors through the shared error handler, which writes to stdout; the runners write their inline errors to stderr. Match the surrounding code when editing an existing path, and prefer stderr for anything new.

## Help output

- `--help` (and `-h` where the shared option set is used) prints to **stdout** and exits `0`. Help that is *requested* is the command's output. Help printed *because* the invocation was wrong is an error, so it goes to stderr with a non-zero exit.
- Help text is generated from the option declarations. Adding an option with `AddFlag`, `AddString`, `AddInteger`, or `AddRepeatable` and giving it a help string is all that is required — there is no second list to update, and option help should not be duplicated in prose.
- A command whose input can come from stdin also gets an `Input:` section describing the pipe form, the `-` escape hatch, and the exit code, so `--help` alone answers "why did this just quit?".
- Every option's help string is a sentence fragment in the imperative or descriptive mood, without a trailing period, matching the existing entries.

## Adding a new CLI

1. Derive from `TGocciaCLIApplication` unless there is a concrete reason not to — it supplies option parsing, config discovery, `--help`, logging, the multifile split, and the no-argument rule.
2. Implement `Configure` (declare options), `UsageLine`, and `ExecuteWithPaths`.
3. If the command reads a program from stdin when given no path, override `StdinUsage`. Point users at `GocciaREPL` only where an interactive session is a sensible alternative.
4. Register the build target in `build.pas` and add the binary path to `scripts/test-cli/binaries.ts`.
5. Add CLI behavior coverage under `scripts/test-cli-*.ts`; see [Testing](../testing.md) for which harness owns what.
