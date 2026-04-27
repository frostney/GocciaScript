import type { OutputLine } from "@/lib/examples";

export type RunnerError = {
  type?: string;
  name?: string;
  message: string;
  line?: number | null;
  column?: number | null;
  fileName?: string | null;
};

export function formatError(err: RunnerError, source: string): OutputLine[] {
  const out: OutputLine[] = [];
  const kind = err.type ?? err.name ?? "Error";

  if (err.line && err.line > 0) {
    const lines = source.split("\n");
    const idx = err.line - 1;
    const lineText = lines[idx] ?? "";
    const col = err.column && err.column > 0 ? err.column : 1;

    if (err.fileName && err.fileName !== "<stdin>") {
      out.push({ kind: "meta", text: `${err.fileName}:${err.line}:${col}` });
    }
    const gutter = `${err.line} | `;
    out.push({ kind: "err", text: `${gutter}${lineText}` });
    out.push({
      kind: "err",
      text: `${" ".repeat(gutter.length + col - 1)}^`,
    });
  }

  out.push({ kind: "err", text: `${kind}: ${err.message}` });
  return out;
}
