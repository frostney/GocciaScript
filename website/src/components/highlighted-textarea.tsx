"use client";

import { useRef } from "react";
import { highlightGoccia, highlightJson, type Token } from "@/lib/highlight";

type Language = "goccia" | "json";

export function HighlightedTextarea({
  value,
  onChange,
  language = "goccia",
  className = "",
  lineNumbers = false,
  onSubmit,
}: {
  value: string;
  onChange: (next: string) => void;
  language?: Language;
  className?: string;
  /** Show a line-number gutter alongside the editor. */
  lineNumbers?: boolean;
  /** Called when the user presses ⌘/Ctrl+Enter inside the editor. Lets a
   *  caller wire the editor to its primary action (e.g. Execute / Run)
   *  without having to attach a global key listener. */
  onSubmit?: () => void;
}) {
  const taRef = useRef<HTMLTextAreaElement>(null);
  const hlRef = useRef<HTMLPreElement>(null);
  const gutterRef = useRef<HTMLPreElement>(null);

  const sync = () => {
    if (taRef.current && hlRef.current) {
      hlRef.current.scrollTop = taRef.current.scrollTop;
      hlRef.current.scrollLeft = taRef.current.scrollLeft;
    }
    if (taRef.current && gutterRef.current) {
      gutterRef.current.scrollTop = taRef.current.scrollTop;
    }
  };

  const tokens: Token[] =
    language === "json"
      ? highlightJson(`${value}\n`)
      : highlightGoccia(`${value}\n`);

  const lineCount = value.split("\n").length;
  const gutter = lineNumbers
    ? Array.from({ length: lineCount }, (_, i) => String(i + 1)).join("\n")
    : null;

  return (
    <div className={`hta ${className}`}>
      {gutter !== null && (
        <pre className="hta-gutter" ref={gutterRef} aria-hidden="true">
          {gutter}
        </pre>
      )}
      <div className="hta-editor">
        <pre className="hta-hl" ref={hlRef} aria-hidden="true">
          <code>
            {tokens.map((t, i) => (
              <span
                key={i}
                className={
                  t.cls
                    ? t.cls.startsWith("tok-")
                      ? t.cls
                      : `tk-${t.cls}`
                    : undefined
                }
              >
                {t.text}
              </span>
            ))}
          </code>
        </pre>
        <textarea
          ref={taRef}
          className="hta-ta"
          spellCheck={false}
          value={value}
          onChange={(e) => onChange(e.target.value)}
          onScroll={sync}
          onKeyDown={(e) => {
            if (e.key === "Tab") {
              e.preventDefault();
              const target = e.currentTarget;
              const { selectionStart: s, selectionEnd: en, value: v } = target;
              const next = `${v.slice(0, s)}  ${v.slice(en)}`;
              onChange(next);
              requestAnimationFrame(() => {
                target.selectionStart = target.selectionEnd = s + 2;
              });
            } else if (
              onSubmit &&
              e.key === "Enter" &&
              (e.metaKey || e.ctrlKey)
            ) {
              // ⌘/Ctrl+Enter fires the caller's primary action (e.g. Execute).
              // preventDefault stops the newline from being inserted into the
              // editor before submission.
              e.preventDefault();
              onSubmit();
            }
          }}
        />
      </div>
    </div>
  );
}
