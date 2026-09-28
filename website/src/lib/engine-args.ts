import {
  hostFilesystemBoundaryFlag,
  isFlagSupported,
  resolveAsiFlag,
  type VendorFeatureSet,
} from "@/lib/vendor-manifest";

export const TIMEOUT_MS = 5_000;
export const MAX_MEMORY_BYTES = 32 * 1024 * 1024;
export const MAX_INSTRUCTIONS = 50_000_000;
export const STACK_SIZE = 2_000;
export const ALLOWED_HOSTS = ["icanhazdadjoke.com"];

export type EngineArgsOptions = {
  mode?: "interpreted" | "bytecode";
  asi: boolean;
  compatVar: boolean;
  compatFunction: boolean;
};

/** Build the per-request arg list. Sandbox/infrastructure flags are
 *  filtered against the binary's probed `features` so older engines that
 *  don't recognize them (`--max-memory`, `--allow-net`, …) still execute
 *  instead of erroring on first unknown-option. Flags renamed in 0.14.0
 *  (ADR 0122) use whichever spelling the binary advertises.
 *
 *  `--ignore-config-permissions` (0.14.0+) keeps a `goccia.*` config
 *  discovered from the server's working directory from granting the guest
 *  anything, or from stopping the run because its requests are untrusted.
 *
 *  User-toggled compat flags (`--compat-var`, `--compat-function`) are an
 *  exception: when the user explicitly opted in, we want the engine's own
 *  "Unknown option" error to surface so they know the toggle had no effect.
 *  Silently dropping them would be misleading. */
export function buildEngineArgs(
  options: EngineArgsOptions,
  features: VendorFeatureSet | undefined,
  kind: "loader" | "testRunner",
): string[] {
  const args: string[] = [];
  const accept = (arg: string) => {
    if (isFlagSupported(features, arg, kind)) args.push(arg);
  };
  const boundaryFlag = features
    ? hostFilesystemBoundaryFlag(features[kind])
    : "--no-host-filesystem";
  if (boundaryFlag) args.push(boundaryFlag);
  if (features) accept("--ignore-config-permissions");
  accept(`--timeout=${TIMEOUT_MS}`);
  accept(`--max-memory=${MAX_MEMORY_BYTES}`);
  accept(`--max-instructions=${MAX_INSTRUCTIONS}`);
  if (isFlagSupported(features, "--max-stack", kind) && features) {
    args.push(`--max-stack=${STACK_SIZE}`);
  } else {
    accept(`--stack-size=${STACK_SIZE}`);
  }
  if (isFlagSupported(features, "--allow-net", kind) && features) {
    args.push(`--allow-net=${ALLOWED_HOSTS.join(",")}`);
  } else if (isFlagSupported(features, "--allowed-host", kind)) {
    for (const host of ALLOWED_HOSTS) {
      args.push("--allowed-host", host);
    }
  }
  // ASI's engine flag was renamed `--asi` -> `--compat-asi` after 0.7.x.
  // resolveAsiFlag returns the name this binary advertises (or null when it
  // advertises neither, in which case ASI is omitted entirely).
  if (options.asi) {
    const asiFlag = resolveAsiFlag(features, kind);
    if (asiFlag) args.unshift(asiFlag);
  }
  // Compat flags pass through unconditionally — the engine errors when it
  // doesn't recognize them, and that's the desired UX (the user toggled it).
  if (options.compatVar) args.push("--compat-var");
  if (options.compatFunction) args.push("--compat-function");
  if (
    options.mode === "bytecode" &&
    isFlagSupported(features, "--mode", kind)
  ) {
    args.push("--mode=bytecode");
  }
  return args;
}
