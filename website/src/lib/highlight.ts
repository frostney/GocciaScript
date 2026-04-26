const GOC_KEYWORDS = new Set([
  "const",
  "let",
  "import",
  "export",
  "from",
  "as",
  "return",
  "if",
  "else",
  "for",
  "of",
  "in",
  "while",
  "class",
  "extends",
  "new",
  "this",
  "super",
  "async",
  "await",
  "try",
  "catch",
  "finally",
  "throw",
  "typeof",
  "instanceof",
  "true",
  "false",
  "null",
  "undefined",
  "get",
  "set",
  "static",
  "yield",
  "break",
  "continue",
]);

export type Token = { cls: string; text: string };

export function highlightGoccia(src: string): Token[] {
  const out: Token[] = [];
  let i = 0;
  const push = (cls: string, text: string) => out.push({ cls, text });
  while (i < src.length) {
    const ch = src[i];
    if (ch === "/" && src[i + 1] === "/") {
      let j = i;
      while (j < src.length && src[j] !== "\n") j++;
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    if (ch === "/" && src[i + 1] === "*") {
      let j = i + 2;
      while (j < src.length - 1 && !(src[j] === "*" && src[j + 1] === "/")) j++;
      j = Math.min(j + 2, src.length);
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    if (ch === '"' || ch === "'" || ch === "`") {
      const q = ch;
      let j = i + 1;
      while (j < src.length && src[j] !== q) {
        if (src[j] === "\\") j += 2;
        else j++;
      }
      j = Math.min(j + 1, src.length);
      push("s", src.slice(i, j));
      i = j;
      continue;
    }
    if (/[0-9]/.test(ch) && !/[A-Za-z_$]/.test(src[i - 1] || "")) {
      let j = i;
      while (j < src.length && /[0-9._n]/.test(src[j])) j++;
      push("n", src.slice(i, j));
      i = j;
      continue;
    }
    if (/[A-Za-z_$#]/.test(ch)) {
      let j = i;
      while (j < src.length && /[A-Za-z0-9_$#]/.test(src[j])) j++;
      const word = src.slice(i, j);
      let k = j;
      while (k < src.length && /\s/.test(src[k])) k++;
      if (GOC_KEYWORDS.has(word)) push("k", word);
      else if (/^[A-Z]/.test(word)) push("pr", word);
      else if (src[k] === "(") push("f", word);
      else push("o", word);
      i = j;
      continue;
    }
    if (/[{}[\]().,;:]/.test(ch)) {
      push("p", ch);
      i++;
      continue;
    }
    if (/[+\-*/%=<>!&|^?~]/.test(ch)) {
      push("o", ch);
      i++;
      continue;
    }
    push("o", ch);
    i++;
  }
  return out;
}

const LANG_KEYWORDS: Record<string, Set<string>> = {
  json: new Set(["true", "false", "null"]),
  ts: new Set([
    "abstract",
    "any",
    "as",
    "async",
    "await",
    "boolean",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "declare",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "from",
    "function",
    "get",
    "if",
    "implements",
    "import",
    "in",
    "instanceof",
    "interface",
    "is",
    "keyof",
    "let",
    "new",
    "null",
    "number",
    "of",
    "package",
    "private",
    "protected",
    "public",
    "readonly",
    "return",
    "set",
    "static",
    "string",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "type",
    "typeof",
    "undefined",
    "void",
    "while",
    "yield",
    "never",
    "unknown",
    "Promise",
    "satisfies",
  ]),
  pascal: new Set([
    "uses",
    "var",
    "begin",
    "end",
    "try",
    "finally",
    "if",
    "then",
    "else",
    "do",
    "for",
    "while",
    "to",
    "downto",
    "function",
    "procedure",
    "unit",
    "interface",
    "implementation",
    "program",
    "const",
    "type",
    "record",
    "class",
    "object",
    "case",
    "of",
    "with",
    "raise",
    "not",
    "and",
    "or",
    "xor",
    "mod",
    "div",
    "nil",
    "true",
    "false",
    "constructor",
    "destructor",
    "inherited",
    "out",
    "override",
    "virtual",
    "abstract",
    "private",
    "protected",
    "public",
    "published",
    "property",
    "read",
    "write",
    "default",
    "specialize",
    "generic",
    "in",
    "is",
    "as",
  ]),
  cpp: new Set([
    "auto",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "constexpr",
    "continue",
    "default",
    "delete",
    "do",
    "double",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "nullptr",
    "operator",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "while",
    "co_await",
    "co_return",
    "co_yield",
    "requires",
  ]),
  csharp: new Set([
    "abstract",
    "as",
    "base",
    "bool",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "checked",
    "class",
    "const",
    "continue",
    "decimal",
    "default",
    "delegate",
    "do",
    "double",
    "else",
    "enum",
    "event",
    "explicit",
    "extern",
    "false",
    "finally",
    "fixed",
    "float",
    "for",
    "foreach",
    "goto",
    "if",
    "implicit",
    "in",
    "int",
    "interface",
    "internal",
    "is",
    "lock",
    "long",
    "namespace",
    "new",
    "null",
    "object",
    "operator",
    "out",
    "override",
    "params",
    "private",
    "protected",
    "public",
    "readonly",
    "ref",
    "return",
    "sbyte",
    "sealed",
    "short",
    "sizeof",
    "stackalloc",
    "static",
    "string",
    "struct",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "uint",
    "ulong",
    "unchecked",
    "unsafe",
    "ushort",
    "using",
    "var",
    "virtual",
    "void",
    "volatile",
    "while",
    "yield",
    "async",
    "await",
    "record",
    "init",
    "with",
    "nameof",
  ]),
  crystal: new Set([
    "abstract",
    "alias",
    "begin",
    "break",
    "case",
    "class",
    "def",
    "do",
    "else",
    "elsif",
    "end",
    "ensure",
    "enum",
    "false",
    "for",
    "fun",
    "if",
    "in",
    "include",
    "initialize",
    "is_a?",
    "lib",
    "macro",
    "module",
    "next",
    "nil",
    "of",
    "out",
    "pointerof",
    "private",
    "protected",
    "public",
    "raise",
    "require",
    "rescue",
    "return",
    "self",
    "sizeof",
    "struct",
    "then",
    "true",
    "type",
    "typeof",
    "unless",
    "until",
    "when",
    "while",
    "yield",
    "with",
    "as",
    "extend",
    "instance_sizeof",
  ]),
};

export function highlightGeneric(src: string, lang: string): Token[] {
  const kws = LANG_KEYWORDS[lang] || new Set<string>();
  const out: Token[] = [];
  let i = 0;
  const push = (cls: string, text: string) => out.push({ cls, text });
  while (i < src.length) {
    const ch = src[i];
    if (ch === "/" && src[i + 1] === "/") {
      let j = i;
      while (j < src.length && src[j] !== "\n") j++;
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    if (ch === "/" && src[i + 1] === "*") {
      let j = i + 2;
      while (j < src.length - 1 && !(src[j] === "*" && src[j + 1] === "/")) j++;
      j = Math.min(j + 2, src.length);
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    // Pascal block-comment forms — gated to `pascal` so we don't swallow
    // object literals in C-family code.
    if (lang === "pascal" && ch === "{") {
      let j = i + 1;
      while (j < src.length && src[j] !== "}") j++;
      j = Math.min(j + 1, src.length);
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    if (lang === "pascal" && ch === "(" && src[i + 1] === "*") {
      let j = i + 2;
      while (j < src.length - 1 && !(src[j] === "*" && src[j + 1] === ")")) j++;
      j = Math.min(j + 2, src.length);
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    if (ch === '"' || ch === "'" || ch === "`") {
      const q = ch;
      let j = i + 1;
      while (j < src.length && src[j] !== q) {
        if (src[j] === "\\") j += 2;
        else j++;
      }
      j = Math.min(j + 1, src.length);
      push("s", src.slice(i, j));
      i = j;
      continue;
    }
    if (/[0-9]/.test(ch) && !/[A-Za-z_$]/.test(src[i - 1] || "")) {
      let j = i;
      while (j < src.length && /[0-9._xXa-fA-F]/.test(src[j])) j++;
      push("n", src.slice(i, j));
      i = j;
      continue;
    }
    // Identifier-start charset must be a SUBSET of the continuation charset,
    // otherwise `j` never advances past `i` (we'd match `@` as a starter, then
    // the inner regex wouldn't accept it, and `i = j` produces an infinite
    // loop). Pascal `@` is an address-of *operator*, not part of an identifier
    // — falls through to the operator/fallback branches below.
    if (/[A-Za-z_$]/.test(ch)) {
      let j = i;
      while (j < src.length && /[A-Za-z0-9_$?!]/.test(src[j])) j++;
      const word = src.slice(i, j);
      let k = j;
      while (k < src.length && /\s/.test(src[k])) k++;
      // Pascal keywords are case-insensitive.
      const lookup = lang === "pascal" ? word.toLowerCase() : word;
      if (kws.has(lookup)) push("k", word);
      else if (/^[A-Z]/.test(word)) push("pr", word);
      else if (src[k] === "(") push("f", word);
      else push("o", word);
      i = j;
      continue;
    }
    if (/[{}[\]().,;:]/.test(ch)) {
      push("p", ch);
      i++;
      continue;
    }
    if (/[+\-*/%=<>!&|^?~]/.test(ch)) {
      push("o", ch);
      i++;
      continue;
    }
    push("o", ch);
    i++;
  }
  return out;
}

export function highlightJson(src: string): Token[] {
  const out: Token[] = [];
  let i = 0;
  const push = (cls: string, text: string) => out.push({ cls, text });
  while (i < src.length) {
    const ch = src[i];
    if (ch === '"') {
      let j = i + 1;
      while (j < src.length && src[j] !== '"') {
        if (src[j] === "\\") j += 2;
        else j++;
      }
      j = Math.min(j + 1, src.length);
      let k = j;
      while (k < src.length && /\s/.test(src[k])) k++;
      const cls = src[k] === ":" ? "tok-key" : "tok-string";
      push(cls, src.slice(i, j));
      i = j;
      continue;
    }
    if (/[-0-9]/.test(ch) && !/[A-Za-z_$]/.test(src[i - 1] || "")) {
      let j = i;
      while (j < src.length && /[-0-9.eE+]/.test(src[j])) j++;
      push("tok-number", src.slice(i, j));
      i = j;
      continue;
    }
    if (/[A-Za-z_]/.test(ch)) {
      let j = i;
      while (j < src.length && /[A-Za-z0-9_]/.test(src[j])) j++;
      const word = src.slice(i, j);
      if (word === "true" || word === "false" || word === "null") {
        push("tok-kw", word);
      } else {
        push("", word);
      }
      i = j;
      continue;
    }
    push("", ch);
    i++;
  }
  return out;
}

/** Shell / Bash highlighter. Distinct from `highlightGeneric` because:
 *   - Comment marker is `#` (at line start or after whitespace), not
 *     `//` — and crucially we must NOT match `//` inside URLs as a
 *     C-family line comment, which `highlightGeneric` did.
 *   - Variable expansions `$VAR` and `${VAR}` get their own emphasis.
 *   - Long/short flags (`--foo`, `-fsSL`) are highlighted as keywords.
 *   - Bash control-flow keywords (`if`, `then`, `for`, …) are
 *     classified, but URLs/paths/commands fall through as default. */
const SHELL_KEYWORDS = new Set([
  "if",
  "then",
  "else",
  "elif",
  "fi",
  "case",
  "esac",
  "for",
  "while",
  "until",
  "do",
  "done",
  "function",
  "return",
  "break",
  "continue",
  "exit",
  "in",
  "select",
  "time",
  "local",
  "export",
]);

export function highlightShell(src: string): Token[] {
  const out: Token[] = [];
  let i = 0;
  const push = (cls: string, text: string) => out.push({ cls, text });
  while (i < src.length) {
    const ch = src[i];
    // `#` line comment — only when it begins a token (start of input
    // or after whitespace), so URLs containing `#` fragments aren't
    // truncated.
    if (ch === "#" && (i === 0 || /\s/.test(src[i - 1]))) {
      let j = i;
      while (j < src.length && src[j] !== "\n") j++;
      push("c", src.slice(i, j));
      i = j;
      continue;
    }
    // Strings — single, double, backtick. Backslash escapes one char.
    if (ch === '"' || ch === "'" || ch === "`") {
      const q = ch;
      let j = i + 1;
      while (j < src.length && src[j] !== q) {
        if (src[j] === "\\") j += 2;
        else j++;
      }
      j = Math.min(j + 1, src.length);
      push("s", src.slice(i, j));
      i = j;
      continue;
    }
    // Variables: `$VAR`, `${...}`
    if (ch === "$" && i + 1 < src.length) {
      let j = i + 1;
      if (src[j] === "{") {
        while (j < src.length && src[j] !== "}") j++;
        j = Math.min(j + 1, src.length);
        push("n", src.slice(i, j));
        i = j;
        continue;
      }
      if (/[A-Za-z_]/.test(src[j])) {
        while (j < src.length && /\w/.test(src[j])) j++;
        push("n", src.slice(i, j));
        i = j;
        continue;
      }
      push("", "$");
      i++;
      continue;
    }
    // Long/short flags at token boundaries: `--foo`, `-fsSL`
    if (
      ch === "-" &&
      (i === 0 || /\s/.test(src[i - 1])) &&
      /[-A-Za-z]/.test(src[i + 1] || "")
    ) {
      let j = i;
      while (j < src.length && /[-A-Za-z0-9_=]/.test(src[j])) j++;
      push("k", src.slice(i, j));
      i = j;
      continue;
    }
    // Identifiers / paths / commands. Continuation includes `.`/`/`/`-`
    // so URLs and paths come through as a single token rather than
    // being shredded into per-character noise.
    if (/[A-Za-z_]/.test(ch)) {
      let j = i;
      while (j < src.length && /[\w./-]/.test(src[j])) j++;
      const word = src.slice(i, j);
      push(SHELL_KEYWORDS.has(word) ? "k" : "", word);
      i = j;
      continue;
    }
    if (/[0-9]/.test(ch)) {
      let j = i;
      while (j < src.length && /[0-9.]/.test(src[j])) j++;
      push("n", src.slice(i, j));
      i = j;
      continue;
    }
    push("", ch);
    i++;
  }
  return out;
}
