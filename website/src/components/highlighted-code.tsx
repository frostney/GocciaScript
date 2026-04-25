import { highlightGeneric, highlightGoccia } from "@/lib/highlight";

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
