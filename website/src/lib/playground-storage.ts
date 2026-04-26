const STORAGE_KEY = "goccia.playground.code.v1";

type CodeMap = Record<string, string>;

function read(): CodeMap {
  if (typeof window === "undefined") return {};
  try {
    const raw = window.localStorage.getItem(STORAGE_KEY);
    if (!raw) return {};
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
      return {};
    }
    // Build a fresh, prototype-less Record by copying only own string-valued
    // entries. Defends against:
    //   • arrays (length keys, etc.)
    //   • objects with __proto__ / inherited properties
    //   • non-string values stored under our key by stale clients
    const map: CodeMap = Object.create(null);
    for (const [k, v] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof v === "string") map[k] = v;
    }
    return map;
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
