import { readFile } from "node:fs/promises";
import path from "node:path";

/**
 * Serve `install.sh` at the bare `/install` path. The Quick install
 * one-liner pipes this URL into `sh`:
 *
 *   curl -fsSL https://gocciascript.dev/install | sh
 *
 * Browsers fetching `/install` see the script as plain text — the
 * "View install script" link on the install/landing pages points
 * here so users can audit before executing.
 *
 * The file at `public/install.sh` is the canonical source; we read
 * it at request time so a single edit to that file updates both
 * `/install` (this handler) and `/install.sh` (Next's static-file
 * serving) without a rebuild.
 */
export const runtime = "nodejs";

export async function GET() {
  const filePath = path.join(process.cwd(), "public", "install.sh");
  const body = await readFile(filePath, "utf8");
  return new Response(body, {
    headers: {
      "content-type": "text/plain; charset=utf-8",
      // Match Vercel's default static-asset cache so curl -L pipelines
      // hit the CDN instead of cold-spinning the route per visitor.
      "cache-control": "public, max-age=300, s-maxage=3600",
    },
  });
}
