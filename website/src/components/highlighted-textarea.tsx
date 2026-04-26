"use client";

import { useRef } from "react";
import { highlightGoccia, highlightJson, type Token } from "@/lib/highlight";

type Language = "goccia" | "json";

export function HighlightedTextarea({
  value,
  onChange,
  language = "goccia",
  className = "",
}: {
  value: string;
  onChange: (next: string) => void;
  language?: Language;
  className?: string;
}) {
  const taRef = useRef<HTMLTextAreaElement>(null);
  const hlRef = useRef<HTMLPreElement>(null);

  const sync = () => {
    if (taRef.current && hlRef.current) {
      hlRef.current.scrollTop = taRef.current.scrollTop;
      hlRef.current.scrollLeft = taRef.current.scrollLeft;
    }
  };

  const tokens: Token[] =
    language === "json"
      ? highlightJson(`${value}\n`)
      : highlightGoccia(`${value}\n`);

  return (
    <div className={`hta ${className}`}>
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
          }
        }}
      />
    </div>
  );
}
