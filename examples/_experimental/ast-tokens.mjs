// Do a parse result's tokens describe the file exactly? A consumer that
// blanks every string, template part, regex literal and comment to search
// what is left only gets the right answer if the tokens leave nothing out
// and claim nothing that is not there. For a `.tsx` file the lexer never saw
// the file — the JSX transformer did — so this also checks the mapping back
// and that nothing the transformer synthesized is reported.
//
// Four claims, per file:
//
//   1. Tokens are in source order, inside the source, non-empty, and do not
//      overlap.
//   2. Every character no token covers is whitespace or a line terminator:
//      the tokens leave no code, literal or comment out.
//   3. Each token's text has the shape its type says — a string starts and
//      ends with the same quote, a TemplateMiddle starts with `}` and ends
//      with `${`, a comment starts with `//` or `/*`, and so on. A token
//      that is merely the right length in the wrong place fails here.
//   4. Every entry of `comments` is a comment token with the same range.
//
// Run:
//   ./build.pas runner
//   ./build/GocciaRunner examples/_experimental/ast-tokens.mjs \
//     --experimental-ast --copy <your-source-dir>=/src
//
// Files that need flags the run was not given fail to parse; they are
// counted and skipped, not reported as problems.

import fs from "fs";
import { parse } from "goccia:ast";

const ROOT = "/src";
const EXTENSIONS = [".js", ".mjs", ".jsx", ".ts", ".tsx"];

const isJsxPath = (path) => path.endsWith(".tsx") || path.endsWith(".jsx");

// The first character between two offsets that is neither whitespace nor a
// line terminator. `\s` is exactly those two (ES2026 §22.2.2.9).
const firstCodeIn = (source, from, to) => {
  const match = /\S/.exec(source.slice(from, to));
  return match ? from + match.index : -1;
};

const SHAPES = {
  StringLiteral: (text) =>
    text.length >= 2 && (text[0] === '"' || text[0] === "'") &&
    text[text.length - 1] === text[0],
  NoSubstitutionTemplate: (text) =>
    text.length >= 2 && text.startsWith("`") && text.endsWith("`"),
  TemplateHead: (text) => text.startsWith("`") && text.endsWith("${"),
  TemplateMiddle: (text) => text.startsWith("}") && text.endsWith("${"),
  TemplateTail: (text) =>
    text.length >= 2 && text.startsWith("}") && text.endsWith("`"),
  RegularExpressionLiteral: (text) =>
    text.startsWith("/") && text.lastIndexOf("/") > 0,
  LineComment: (text) => text.startsWith("//") && !text.includes("\n"),
  BlockComment: (text) =>
    text.length >= 4 && text.startsWith("/*") && text.endsWith("*/"),
  HashbangComment: (text) => text.startsWith("#!") && !text.includes("\n"),
  PrivateIdentifier: (text) => text.startsWith("#") && text.length > 1,
  NumericLiteral: (text) => /^\.?[0-9]/.test(text),
};

const checkFile = (file, counts) => {
  const text = fs.readFileSync(file, "utf8");
  let result = null;

  try {
    result = parse(text, { jsx: isJsxPath(file), fileName: file, tokens: true });
  } catch (error) {
    counts.unparsed = counts.unparsed + 1;
    return [];
  }

  const problems = [];
  const { source, tokens, comments } = result;
  let covered = 0;

  tokens.forEach((token, index) => {
    const label = `${file}  token ${index} ${token.type} ${token.start}-${token.end}`;

    if (token.start < covered || token.end <= token.start ||
        token.end > source.length) {
      problems.push(`${label}  out of order, empty, overlapping or outside the source`);
      return;
    }

    const uncovered = firstCodeIn(source, covered, token.start);

    if (uncovered >= 0) {
      problems.push(`${file}  offset ${uncovered} ${JSON.stringify(source[uncovered])} is in no token`);
    }

    const shape = SHAPES[token.type];
    const slice = source.slice(token.start, token.end);

    if (shape && !shape(slice)) {
      problems.push(`${label}  ${JSON.stringify(slice)} does not have that shape`);
    }

    counts.tokens = counts.tokens + 1;
    counts.byType[token.type] = (counts.byType[token.type] ?? 0) + 1;
    covered = token.end;
  });

  const trailing = firstCodeIn(source, covered, source.length);

  if (trailing >= 0) {
    problems.push(`${file}  offset ${trailing} ${JSON.stringify(source[trailing])} is in no token`);
  }

  const commentTokens = tokens.filter(
    (token) => token.type === "LineComment" || token.type === "BlockComment",
  );

  comments.forEach((comment) => {
    const match = commentTokens.find(
      (token) => token.start === comment.start && token.end === comment.end,
    );

    if (!match) {
      problems.push(`${file}  comment ${comment.start}-${comment.end} has no token`);
    }
  });

  counts.files = counts.files + 1;

  return problems;
};

const sourceFiles = (dir) => {
  const found = [];

  fs.readdirSync(dir).forEach((name) => {
    const path = `${dir}/${name}`;

    if (fs.statSync(path).isDirectory()) {
      found.push(...sourceFiles(path));
    } else if (EXTENSIONS.some((extension) => name.endsWith(extension))) {
      found.push(path);
    }
  });

  return found;
};

const files = sourceFiles(ROOT).sort();
const counts = { files: 0, unparsed: 0, tokens: 0, byType: {} };
const problems = [];

files.forEach((file) => {
  problems.push(...checkFile(file, counts));
});

problems.forEach((problem) => {
  console.log(problem);
});

console.log(
  `${counts.tokens} tokens in ${counts.files} files (${counts.unparsed} skipped: did not parse with this run's flags)`,
);
Object.keys(counts.byType).sort().forEach((type) => {
  console.log(`  ${type.padEnd(26)}${counts.byType[type]}`);
});
console.log(problems.length === 0 ? "every claim holds" : `${problems.length} problems`);
