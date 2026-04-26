import type { ReactNode } from "react";

/** Code block with a left-side line-number gutter — used across the
 *  homepage so all rendered code samples have the same look. The
 *  `lineCount` is supplied by the caller (from the source string or
 *  rendered text) so we don't have to introspect React children to
 *  count newlines.
 *
 *  Wrap the actual highlighted content as `children`; the component
 *  provides only the gutter + container chrome. */
export function NumberedCode({
  children,
  lineCount,
  className = "",
}: {
  children: ReactNode;
  lineCount: number;
  className?: string;
}) {
  const gutter = Array.from({ length: lineCount }, (_, i) => i + 1).join("\n");
  return (
    <div className={`numbered-code ${className}`}>
      <pre className="numbered-code-gutter" aria-hidden="true">
        {gutter}
      </pre>
      <pre className="numbered-code-body">
        <code>{children}</code>
      </pre>
    </div>
  );
}
