"use client";

import type { ReactNode } from "react";
import { useRunShortcut } from "@/components/command-tabs";

/** Per-line entry rendered inside `<AnimatedOutput>`. Mirrors the
 *  shape the playground / sandbox / hero outputs already use. */
export type AnimatedOutputLine = {
  kind: "meta" | "log" | "out" | "err" | "result";
  text: ReactNode;
};

/** Duration of the command-line typewriter reveal (ms). Output lines
 *  stagger after this so the command finishes "typing" first.
 *
 *  These values must stay in sync with the CSS in globals.css:
 *  - TYPEWRITER_MS ↔ `typewriter-expand` duration and
 *    `caret-typing-hide` delay (both `0.6s`)
 *  - STAGGER_MS ↔ `animationDelay` on `.anim-output-line` (inline) */
const TYPEWRITER_MS = 600;
const STAGGER_MS = 60;

/** Console-style output panel with a synchronized line-by-line
 *  reveal animation: each line fades + slides in with a small
 *  stagger, and a blinking caret tails the final line.
 *
 *  The first meta line is treated as the "command" — it renders
 *  with a ➜ prompt and a typewriter reveal so the caret appears
 *  to type the command in real time. Subsequent lines fade in
 *  after the typewriter finishes.
 *
 *  Used across the playground, sandbox preview, hero runnable card,
 *  and Compiler Pipeline result so the same "code just executed,
 *  output is materializing" feel applies everywhere. */
export function AnimatedOutput({
  lines,
  runKey,
  className = "",
  showCaret = true,
}: {
  lines: AnimatedOutputLine[];
  /** Bump this on each new run to restart the line-stagger animation. */
  runKey: number | string;
  className?: string;
  /** When false, the trailing caret is hidden — useful for finalised
   *  output that's no longer "live". */
  showCaret?: boolean;
}) {
  const runShortcut = useRunShortcut();
  if (lines.length === 0) {
    return (
      <div className={`anim-output ${className}`}>
        <div className="anim-output-line log-command">
          <span className="pg-log-gutter" aria-hidden="true">
            ➜
          </span>
          <span className="anim-output-text">
            <span
              className="anim-output-caret anim-output-caret-idle"
              aria-hidden="true"
            />
            <span className="anim-output-hint" role="status">
              press Run or {runShortcut.long}
            </span>
          </span>
        </div>
      </div>
    );
  }

  const hasCommand = lines[0]?.kind === "meta";

  return (
    <div key={runKey} className={`anim-output ${className}`}>
      {lines.map((l, i) => {
        const isCmd = i === 0 && hasCommand;

        if (isCmd) {
          const cmdOnly = lines.length === 1;
          return (
            <div key={i} className="anim-output-line log-command">
              <span className="pg-log-gutter" aria-hidden="true">
                ➜
              </span>
              <span className="anim-output-text">
                <span className="anim-output-typed">{l.text}</span>
                {cmdOnly && (
                  <span
                    className="anim-output-caret anim-output-caret-typing"
                    aria-hidden="true"
                  />
                )}
              </span>
            </div>
          );
        }

        const delay = hasCommand
          ? TYPEWRITER_MS + (i - 1) * STAGGER_MS
          : i * STAGGER_MS;
        const kind =
          l.kind === "meta"
            ? "meta"
            : l.kind === "err"
              ? "err"
              : l.kind === "result"
                ? "result"
                : "out";

        return (
          <div
            key={i}
            className={`anim-output-line log-${kind}`}
            style={{ animationDelay: `${delay}ms` }}
          >
            <span className="pg-log-gutter" aria-hidden="true">
              {l.kind === "err" ? "✗" : l.kind === "result" ? "↳" : ""}
            </span>
            <span className="anim-output-text">{l.text}</span>
          </div>
        );
      })}
      {showCaret && (
        <span
          className="anim-output-caret"
          style={{
            animationDelay: `${
              hasCommand
                ? TYPEWRITER_MS + (lines.length - 1) * STAGGER_MS
                : lines.length * STAGGER_MS
            }ms`,
          }}
          aria-hidden="true"
        />
      )}
    </div>
  );
}
