// Encode/decode playground state for share links via URL search params.
// Uses base64url (web-safe) so the result is copy-pasteable.

export type SharePayload = {
  code: string;
  mode?: string;
  asi?: boolean;
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
      mode: parsed.mode,
      asi: typeof parsed.asi === "boolean" ? parsed.asi : undefined,
    };
  } catch {
    return null;
  }
}
