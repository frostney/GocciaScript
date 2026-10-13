// Encode/decode playground state for share links via URL search params.
// Uses base64url (web-safe) so the result is copy-pasteable.

import type { ExecutionMode } from "@/lib/vendor-manifest";

export type SharePayload = {
  code: string;
  mode?: string;
  runner?: "execute" | "test";
  asi?: boolean;
  compatVar?: boolean;
  compatFunction?: boolean;
  version?: string;
};

function toBase64Url(bytes: Uint8Array): string {
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

function fromBase64Url(s: string): Uint8Array {
  const padded = s.replace(/-/g, "+").replace(/_/g, "/");
  const pad = padded.length % 4 ? 4 - (padded.length % 4) : 0;
  const bin = atob(padded + "=".repeat(pad));
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

export function encodeShare(payload: SharePayload): string {
  const json = JSON.stringify(payload);
  return toBase64Url(new TextEncoder().encode(json));
}

export function decodeShare(s: string): SharePayload | null {
  try {
    const json = new TextDecoder().decode(fromBase64Url(s));
    const parsed = JSON.parse(json) as Partial<SharePayload>;
    if (typeof parsed.code !== "string") return null;
    return {
      code: parsed.code,
      mode: typeof parsed.mode === "string" ? parsed.mode : undefined,
      runner:
        parsed.runner === "execute" || parsed.runner === "test"
          ? parsed.runner
          : undefined,
      asi: typeof parsed.asi === "boolean" ? parsed.asi : undefined,
      compatVar:
        typeof parsed.compatVar === "boolean" ? parsed.compatVar : undefined,
      compatFunction:
        typeof parsed.compatFunction === "boolean"
          ? parsed.compatFunction
          : undefined,
      version: typeof parsed.version === "string" ? parsed.version : undefined,
    };
  } catch {
    return null;
  }
}

/** Per-version `--mode` support for the playground's two runners, probed from
 *  each vendored binary's help text. */
export type PlaygroundModeSupport = Record<
  string,
  { loader: boolean; testRunner: boolean }
>;

/** Whether the playground can pass `--mode` to the binary that runs `runner`
 *  for `version`. A version missing from the table (a locally built engine)
 *  is a current one, which takes `--mode`. */
export function isModeSelectable(
  modeSupport: PlaygroundModeSupport | undefined,
  version: string,
  runner: "execute" | "test",
): boolean {
  return (
    modeSupport?.[version]?.[runner === "test" ? "testRunner" : "loader"] ??
    true
  );
}

/** The mode a playground request or share link carries: a valid selected mode
 *  when the version's binary takes `--mode`, otherwise none. A hidden backend
 *  therefore never splits the API's response cache, and a share link never
 *  records or restores a mode its version cannot run. */
export function effectiveMode(
  mode: unknown,
  modeSelectable: boolean,
): ExecutionMode | undefined {
  if (!modeSelectable) return undefined;
  return mode === "interpreted" || mode === "bytecode" ? mode : undefined;
}
