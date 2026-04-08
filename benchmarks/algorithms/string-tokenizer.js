/*---
description: String tokenizer benchmarks — lexer/tokenizer over string input
---*/

const TOKEN_IDENT = "ident";
const TOKEN_NUMBER = "number";
const TOKEN_STRING = "string";
const TOKEN_OP = "operator";
const TOKEN_PAREN = "paren";
const TOKEN_PUNCT = "punctuation";
const TOKEN_WS = "whitespace";

const isDigit = (ch) => ch >= "0" && ch <= "9";
const isAlpha = (ch) => (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z") || ch === "_" || ch === "$";
const isAlphaNum = (ch) => isAlpha(ch) || isDigit(ch);
const isWhitespace = (ch) => ch === " " || ch === "\t" || ch === "\n" || ch === "\r";
const isOperator = (ch) => ch === "+" || ch === "-" || ch === "*" || ch === "/" || ch === "=" || ch === "<" || ch === ">" || ch === "!" || ch === "&" || ch === "|" || ch === "%" || ch === "^";

const tokenize = (source) => {
  const tokens = [];
  const chars = [...source];
  let pos = 0;
  const len = chars.length;

  const advance = () => {
    const ch = chars[pos];
    pos = pos + 1;
    return ch;
  };

  const peek = () => pos < len ? chars[pos] : "";

  const readWhile = (pred) => {
    let result = "";
    const scan = () => {
      if (pos >= len) { return; }
      if (!pred(chars[pos])) { return; }
      result = result + chars[pos];
      pos = pos + 1;
      scan();
    };
    scan();
    return result;
  };

  const processToken = () => {
    if (pos >= len) { return false; }
    const ch = peek();

    if (isWhitespace(ch)) {
      readWhile(isWhitespace);
      return true;
    }

    if (isAlpha(ch)) {
      const value = readWhile(isAlphaNum);
      tokens.push({ type: TOKEN_IDENT, value });
      return true;
    }

    if (isDigit(ch)) {
      let value = readWhile(isDigit);
      if (peek() === ".") {
        value = value + advance();
        value = value + readWhile(isDigit);
      }
      tokens.push({ type: TOKEN_NUMBER, value });
      return true;
    }

    if (ch === '"' || ch === "'") {
      const quote = advance();
      const value = readWhile((c) => c !== quote);
      if (pos < len) { advance(); } // closing quote
      tokens.push({ type: TOKEN_STRING, value });
      return true;
    }

    if (isOperator(ch)) {
      let value = advance();
      // Two-character operators
      const next = peek();
      if ((ch === "=" && next === "=") || (ch === "!" && next === "=") ||
          (ch === "<" && next === "=") || (ch === ">" && next === "=") ||
          (ch === "&" && next === "&") || (ch === "|" && next === "|") ||
          (ch === "=" && next === ">") || (ch === "*" && next === "*")) {
        value = value + advance();
        // Three-character operators (===, !==)
        if ((value === "==" || value === "!=") && peek() === "=") {
          value = value + advance();
        }
      }
      tokens.push({ type: TOKEN_OP, value });
      return true;
    }

    if (ch === "(" || ch === ")" || ch === "[" || ch === "]" || ch === "{" || ch === "}") {
      tokens.push({ type: TOKEN_PAREN, value: advance() });
      return true;
    }

    // Punctuation: semicolons, commas, dots, colons
    tokens.push({ type: TOKEN_PUNCT, value: advance() });
    return true;
  };

  const tokenizeAll = () => {
    if (!processToken()) { return; }
    tokenizeAll();
  };
  tokenizeAll();

  return tokens;
};

const SIMPLE_EXPR = 'const x = 42 + y * (z - 3);';

const MEDIUM_SOURCE = `const add = (a, b) => a + b;
const multiply = (x, y) => x * y;
const result = add(multiply(3, 4), 5);
const greeting = "hello world";
const check = result >= 10 && result <= 100;
const items = [1, 2, 3, 4, 5];
const first = items[0];
const obj = { name: "test", value: 42 };`;

const LARGE_SOURCE = `class Vector {
  constructor(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }
  add(other) {
    return new Vector(this.x + other.x, this.y + other.y, this.z + other.z);
  }
  scale(factor) {
    return new Vector(this.x * factor, this.y * factor, this.z * factor);
  }
  dot(other) {
    return this.x * other.x + this.y * other.y + this.z * other.z;
  }
  magnitude() {
    return Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z);
  }
}
const v1 = new Vector(1, 2, 3);
const v2 = new Vector(4, 5, 6);
const v3 = v1.add(v2);
const scaled = v3.scale(2.5);
const d = v1.dot(v2);
const mag = v3.magnitude();
const result = mag > 10 ? "large" : "small";
const arr = [v1, v2, v3, scaled];
const check = arr.length === 4 && d === 32;`;

suite("string tokenizer", () => {
  bench("simple expression", {
    run: () => {
      tokenize(SIMPLE_EXPR);
    },
  });

  bench("medium source (8 lines)", {
    run: () => {
      tokenize(MEDIUM_SOURCE);
    },
  });

  bench("large source (class definition)", {
    run: () => {
      tokenize(LARGE_SOURCE);
    },
  });
});
