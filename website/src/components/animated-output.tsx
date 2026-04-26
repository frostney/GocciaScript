"use client";

import type { ReactNode } from "react";

/** Per-line entry rendered inside `<AnimatedOutput>`. Mirrors the
 *  shape the playground / sandbox / hero outputs already use. */
export type AnimatedOutputLine = {
  kind: "meta" | "log" | "out" | "err" | "result";
  text: ReactNode;
};

/** Console-style output panel with a synchronized line-by-line
 *  reveal animation: each line fades + slides in with a small
 *  stagger, and a blinking caret tails the final line.
 *
 *  The animation is driven entirely by CSS `animation-delay` so we
 *  don't need any state or timers — just remount on each new run by
 *  bumping the `runKey` prop, which lifts to a `key` on the wrapper
 *  and resets the animation cleanly.
 *
 *  Used across the playground, sandbox preview, hero runnable card,
 *  and Compiler Pipeline result so the same "code just executed,
 *  output is materializing" feel applies everywhere. */
export function AnimatedOutput({
  lines,
  runKey,
  className = "",
  showCaret = true,
  emptyState,
}: {
  lines: AnimatedOutputLine[];
  /** Bump this on each new run to restart the line-stagger animation. */
  runKey: number | string;
  className?: string;
  /** When false, the trailing caret is hidden — useful for finalised
   *  output that's no longer "live". */
  showCaret?: boolean;
  /** Rendered when `lines` is empty. */
  emptyState?: ReactNode;
}) {
  if (lines.length === 0) {
    return (
      <div className={`anim-output ${className}`}>{emptyState ?? null}</div>
    );
  }
  return (
    <div key={runKey} className={`anim-output ${className}`}>
      {lines.map((l, i) => (
        <div
          key={i}
          className={`anim-output-line log-${
            l.kind === "meta"
              ? "meta"
              : l.kind === "err"
                ? "err"
                : l.kind === "result"
                  ? "result"
                  : "out"
          }`}
          style={{ animationDelay: `${i * 60}ms` }}
        >
          <span className="pg-log-gutter" aria-hidden="true">
            {l.kind === "meta"
              ? ""
              : l.kind === "err"
                ? "✗"
                : l.kind === "result"
                  ? "↳"
                  : "›"}
          </span>
          <span className="anim-output-text">{l.text}</span>
        </div>
      ))}
      {showCaret && (
        <span
          className="anim-output-caret"
          style={{ animationDelay: `${lines.length * 60}ms` }}
          aria-hidden="true"
        />
      )}
    </div>
  );
}
