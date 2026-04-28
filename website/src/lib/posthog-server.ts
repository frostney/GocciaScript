import "server-only";
import { PostHog } from "posthog-node";

/**
 * Server-side PostHog client. Used by the API Route Handlers
 * (and any other server code that needs telemetry) to capture
 * uncaught exceptions and significant operational events that the
 * client-side SDK can't see.
 *
 * Behaviour:
 *   - Returns `null` when `NEXT_PUBLIC_POSTHOG_KEY` (or
 *     `POSTHOG_KEY` as a server-only override) isn't configured —
 *     local dev and forks without an analytics project keep working.
 *   - Reuses the host configured for the client SDK so the project
 *     ingestion endpoint stays consistent across surfaces.
 *   - Memoises the client across module reloads (Next dev) via a
 *     `globalThis` symbol so HMR doesn't open new connections per
 *     edit.
 *   - `flushAt: 1` and `flushInterval: 0` mean events ship the moment
 *     they're queued — short-lived serverless functions otherwise
 *     finish before PostHog's normal batch timer fires and lose the
 *     event.
 */

const KEY = process.env.POSTHOG_KEY ?? process.env.NEXT_PUBLIC_POSTHOG_KEY;
const HOST =
  process.env.POSTHOG_HOST ??
  process.env.NEXT_PUBLIC_POSTHOG_HOST ??
  "https://eu.i.posthog.com";

const SLOT = Symbol.for("goccia.posthog-server");
type Slot = typeof globalThis & { [SLOT]?: PostHog | null };

export function getPostHogServer(): PostHog | null {
  if (!KEY) return null;
  const g = globalThis as Slot;
  if (g[SLOT] !== undefined) return g[SLOT];
  const client = new PostHog(KEY, {
    host: HOST,
    flushAt: 1,
    flushInterval: 0,
  });
  g[SLOT] = client;
  return client;
}

/** Convenience: capture an `$exception` event with stack + extras.
 *  No-ops when PostHog isn't configured. The error object gets
 *  serialised in the shape the PostHog Error Tracking dashboard
 *  expects (`$exception_list` array + top-level `$exception_message`
 *  / `$exception_type`). */
export function captureServerException(
  err: unknown,
  context: {
    distinctId?: string;
    /** Where the error originated — e.g. `"/api/execute"`. Surfaced as
     *  the `path` property and used as the default distinct id when
     *  none is provided. */
    path: string;
    /** Extra properties merged into the captured event. Anything
     *  serialisable; avoid PII (request bodies, auth tokens, etc.). */
    extra?: Record<string, unknown>;
  },
): void {
  const ph = getPostHogServer();
  if (!ph) return;
  const e = err instanceof Error ? err : new Error(String(err));
  ph.capture({
    distinctId: context.distinctId ?? `server:${context.path}`,
    event: "$exception",
    properties: {
      $exception_list: [
        {
          type: e.name,
          value: e.message,
          mechanism: { handled: true, type: "generic" },
          stacktrace: e.stack
            ? { type: "raw", frames: [{ raw: e.stack }] }
            : undefined,
        },
      ],
      $exception_type: e.name,
      $exception_message: e.message,
      $exception_source: context.path,
      path: context.path,
      ...context.extra,
    },
  });
}

/** Capture a non-error operational event (spawn failed, rate limit
 *  hit, oversize body, etc.) so the dashboards have a signal even
 *  when the response was a clean 4xx/5xx rather than a thrown
 *  exception. */
export function captureServerEvent(
  event: string,
  context: {
    distinctId?: string;
    path: string;
    properties?: Record<string, unknown>;
  },
): void {
  const ph = getPostHogServer();
  if (!ph) return;
  ph.capture({
    distinctId: context.distinctId ?? `server:${context.path}`,
    event,
    properties: { path: context.path, ...context.properties },
  });
}
