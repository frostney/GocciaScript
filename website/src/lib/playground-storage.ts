const STORAGE_KEY = "goccia.playground.code.v1";

type CodeMap = Record<string, string>;

function read(): CodeMap {
  if (typeof window === "undefined") return {};
  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    if (!raw) return {};
    const parsed = JSON.parse(raw) as unknown;
    if (parsed && typeof parsed === "object") return parsed as CodeMap;
  } catch {}
  return {};
}

export function loadCode(exampleId: string): string | null {
  const map = read();
  const v = map[exampleId];
  return typeof v === "string" ? v : null;
}

export function saveCode(exampleId: string, code: string): void {
  if (typeof window === "undefined") return;
  try {
    const map = read();
    map[exampleId] = code;
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(map));
  } catch {}
}

export function resetCode(exampleId: string): void {
  if (typeof window === "undefined") return;
  try {
    const map = read();
    delete map[exampleId];
    window.localStorage.setItem(STORAGE_KEY, JSON.stringify(map));
  } catch {}
}
