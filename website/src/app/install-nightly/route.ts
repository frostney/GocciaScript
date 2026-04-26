import { readFile } from "node:fs/promises";
import path from "node:path";

/**
 * Serve `install-nightly.sh` at the bare `/install-nightly` path —
 * mirror of `app/install/route.ts` for the rolling-nightly channel.
 *
 *   curl -fsSL https://gocciascript.dev/install-nightly | sh
 *
 * Same content as `public/install-nightly.sh` (which is also
 * available at the static `/install-nightly.sh` URL); reading it
 * from disk at request time keeps the script editable as a normal
 * file without rebuilds. */
export const runtime = "nodejs";

export async function GET() {
  const filePath = path.join(process.cwd(), "public", "install-nightly.sh");
  const body = await readFile(filePath, "utf8");
  return new Response(body, {
    headers: {
      "content-type": "text/plain; charset=utf-8",
      "cache-control": "public, max-age=300, s-maxage=3600",
    },
  });
}
