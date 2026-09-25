import {
  listPlaygroundVersions,
  resolvePublicDefaultVersion,
} from "@/lib/vendor-manifest";
import { getVendorManifest } from "@/lib/vendor-manifest-server";

export const runtime = "nodejs";
export const dynamic = "force-dynamic";

/** What this deployment actually vendored.
 *
 *  Same data the playground page renders, in a shape a build step can read:
 *  `scripts/vercel-ignore.sh` compares `vendored` against the newest published
 *  GitHub release to decide whether the live site has fallen behind a release.
 *  `no-store` keeps that comparison from reading a CDN copy of the previous
 *  deployment.
 *
 *  - `vendored`  — every staged tag, including ones the playground hides.
 *  - `playground` — the subset offered in the version picker (engines that
 *                   advertise the `--no-host-filesystem` boundary). */
export function GET() {
  const manifest = getVendorManifest();
  return Response.json(
    {
      defaultVersion: resolvePublicDefaultVersion(manifest),
      vendored: manifest.versions.map((entry) => entry.tag),
      playground: listPlaygroundVersions(manifest),
    },
    { headers: { "cache-control": "no-store" } },
  );
}
