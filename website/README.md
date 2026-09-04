# GocciaScript website

The marketing/docs site for GocciaScript — a Next.js 16 app under `website/`. Fumadocs reads `../README.md` and `../docs/**/*.md` directly, while Astryx provides the shared shell and generic controls. The warm-paper GocciaScript theme remains project-owned.

Bootstrapped with [`create-next-app`](https://nextjs.org/docs/app/api-reference/cli/create-next-app).

## Getting Started

First, run the development server:

```bash
bun install --frozen-lockfile
bun run dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

Edit route UI under `src/app/` and `src/components/`. Edit documentation in the repository root; `source.config.ts` defines the collection and generated routes update automatically.

Fonts (Instrument Serif, IBM Plex Sans, and JetBrains Mono) are loaded through `next/font` in `src/app/layout.tsx`; the OG image generator (`src/app/opengraph-image.tsx`) embeds the matching display and body fonts.

## Playground engines

The playground runs real engine binaries, vendored at build time by
`scripts/fetch-binaries.ts` (`prebuild`): the rolling `nightly` plus the top
three stable precedence picks. Each run produces two descriptions of the same
set.

| Artifact | Reached by | Why |
| --- | --- | --- |
| `vendor/manifest.json` + the binaries | `/api/execute`, `/api/test` | `next.config.mjs` traces `vendor/**` into those two route bundles, which spawn the binaries. |
| `src/generated/vendor-manifest.json` | every bundle | A **static import** (`src/lib/vendor-manifest-server.ts`). The playground page renders in its own bundle, which does not trace `vendor/**`, so a `process.cwd()` read there finds nothing. |

Both are generated, never committed. `postinstall` creates the second one
empty (`scripts/ensure-generated-manifest.ts`) so a fresh checkout typechecks
and builds; `prebuild` fills it in. `/api/versions` reports what a deployment
actually vendored.

A build **fails** when the vendored set has no stable release, or none that
advertises `--no-host-filesystem` on both binaries — a playground offering
only `nightly` is a broken playground, not a degraded one. Individual tag
failures stay warnings. Override with `ALLOW_NIGHTLY_ONLY_PLAYGROUND=1` only
when shipping without a stable engine is intended.

### Releases reach the site

A release commit touches only `CHANGELOG.md`, so `scripts/vercel-ignore.sh`
sees no website change and Vercel skips the build — the site keeps serving the
previously vendored engines. Two pieces close that loop:

1. The `release` job in `.github/workflows/ci.yml` POSTs a Vercel deploy hook
   once the release archives exist.
2. `scripts/vercel-ignore.sh` also builds when GitHub's newest stable release
   is missing from the live site's `/api/versions`, which is what lets that
   deployment through — and self-corrects if the site falls behind any other
   way.

Two settings live outside this repo:

- **Vercel → project → Environment Variables → `GITHUB_TOKEN`** (optional but
  recommended): used for GitHub API calls while vendoring and in the ignore
  step. Without it those calls are limited to 60/hour per IP; the ignore step
  fails toward building, so exhaustion costs build minutes rather than
  freshness.
- **GitHub → repository → Secrets → `VERCEL_DEPLOY_HOOK_URL`**: a Vercel deploy
  hook for the production branch (Vercel → project → Settings → Git → Deploy
  Hooks). Treat the URL as a credential. Without the secret the workflow step
  logs that it skipped, and the release reaches the site on its next
  deployment instead.

## Report publication

CI publisher commands under `scripts/publish-*.ts` normalize report files and
compute report-specific summaries before uploading them. `scripts/lib/report-publishing.ts` supplies the
shared GitHub run metadata, timestamp parsing, and profile file arguments.
`src/lib/report-blob.ts` owns byte reads, compression, upload options, and
profile attachments; each `*-blob-store.ts` adapter owns its schema, environment
settings, and storage paths. Requests continue reading published Blob data;
publisher metadata and file processing remain in CI scripts.

AWFY and Web Tooling overwrite one pointer per UTC day. JetStream retains a
pointer per run within each day. Test262 keeps its distinct report path and
profile namespace, with profile access controlled by `TEST262_BLOB_ACCESS`;
benchmark profiles use `BENCHMARK_PROFILE_BLOB_ACCESS`. See the
[benchmark guide](../docs/benchmarks.md) and [test262 guide](../docs/test262.md)
for the report and publication contracts.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/app/building-your-application/deploying) for more details.
