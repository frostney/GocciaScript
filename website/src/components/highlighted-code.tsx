import {
  highlightGeneric,
  highlightGoccia,
  highlightJson,
  highlightShell,
} from "@/lib/highlight";

export function HighlightedCode({ code }: { code: string }) {
  const tokens = highlightGoccia(code);
  return (
    <>
      {tokens.map((t, idx) => (
        <span key={idx} className={t.cls ? `tk-${t.cls}` : undefined}>
          {t.text}
        </span>
      ))}
    </>
  );
}

export function HighlightedGeneric({
  code,
  language,
}: {
  code: string;
  language: string;
}) {
  const tokens = highlightGeneric(code, language);
  return (
    <>
      {tokens.map((t, idx) => (
        <span key={idx} className={t.cls ? `tk-${t.cls}` : undefined}>
          {t.text}
        </span>
      ))}
    </>
  );
}

/** Shell / Bash highlighter. The generic highlighter would mistake the
 *  `//` inside URLs (`https://…`) for a C-family line comment and
 *  swallow the rest of the line — the dedicated shell tokenizer never
 *  considers `//` a comment. */
export function HighlightedShell({ code }: { code: string }) {
  const tokens = highlightShell(code);
  return (
    <>
      {tokens.map((t, idx) => (
        <span key={idx} className={t.cls ? `tk-${t.cls}` : undefined}>
          {t.text}
        </span>
      ))}
    </>
  );
}

/** Dedicated JSON highlighter — distinguishes object keys from strings via
 *  the lookahead-for-`:` trick that the generic highlighter can't do. The
 *  `highlightJson` function emits `tok-*` class names directly (matching
 *  the existing `.tok-key` / `.tok-string` / `.tok-number` / `.tok-kw` CSS),
 *  so we don't apply the `tk-` prefix here. */
export function HighlightedJson({ code }: { code: string }) {
  const tokens = highlightJson(code);
  return (
    <>
      {tokens.map((t, idx) => (
        <span key={idx} className={t.cls || undefined}>
          {t.text}
        </span>
      ))}
    </>
  );
}
