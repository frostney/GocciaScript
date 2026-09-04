#!/usr/bin/env bun
import { rebuildAwfyBlobHistory } from "../src/lib/awfy-blob-store";
import { rebuildJetStreamBlobHistory } from "../src/lib/jetstream-blob-store";
import { rebuildTest262BlobHistory } from "../src/lib/test262-blob-store";

const args = process.argv.slice(2);
if (args.length === 1 && (args[0] === "--help" || args[0] === "-h")) {
  console.log(
    "Usage: bun run rebuild-history\n\nRebuild optional AWFY, JetStream, and test262 history snapshots.\nRequires BLOB_READ_WRITE_TOKEN; uses each report's existing BLOB_PREFIX and BLOB_ACCESS settings.",
  );
  process.exit(0);
}
if (args.length || !process.env.BLOB_READ_WRITE_TOKEN) {
  console.error(
    "rebuild-history takes no arguments and requires BLOB_READ_WRITE_TOKEN; see --help",
  );
  process.exit(2);
}

for (const [name, rebuild] of [
  ["AWFY", rebuildAwfyBlobHistory],
  ["JetStream", rebuildJetStreamBlobHistory],
  ["test262", rebuildTest262BlobHistory],
] as const) {
  console.error(
    `[rebuild-history] ${name}: wrote ${await rebuild()} snapshots`,
  );
}
console.error(
  "[rebuild-history] Existing, unstable, and oversized generations are skipped; raw reports are unchanged.",
);
