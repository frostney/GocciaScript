#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const { downloadText } = require("./lib/resource-container");

const REPO_ROOT = path.resolve(__dirname, "..");
const DEFAULT_UNICODE_VERSION = "17.0.0";
const DEFAULT_OUTPUT = path.join(
  REPO_ROOT,
  "source",
  "generated",
  "Generated.UnicodeNormalizationData.pas",
);

function parseArguments() {
  if (process.argv.length > 4) {
    throw new Error(
      "Usage: node scripts/generate-unicode-normalization-data.js " +
      "[unicode-version] [output-file]",
    );
  }
  return {
    unicodeVersion: process.argv[2] || DEFAULT_UNICODE_VERSION,
    outputFile: process.argv[3]
      ? path.resolve(process.argv[3])
      : DEFAULT_OUTPUT,
  };
}

async function downloadUCDFile(version, name) {
  const url = `https://www.unicode.org/Public/${version}/ucd/${name}`;
  console.log(`Downloading ${name}...`);
  return downloadText(url);
}

function parseCodePointSequence(value) {
  if (!value.trim()) {
    return [];
  }
  return value.trim().split(/\s+/).map((item) => parseInt(item, 16));
}

function parseUnicodeData(text) {
  const combiningClasses = new Map();
  const decompositions = new Map();
  let rangeStart = null;
  let rangeClass = 0;

  for (const line of text.split(/\r?\n/)) {
    if (!line) {
      continue;
    }
    const fields = line.split(";");
    const codePoint = parseInt(fields[0], 16);
    const combiningClass = parseInt(fields[3], 10);
    if (fields[1].endsWith(", First>")) {
      rangeStart = codePoint;
      rangeClass = combiningClass;
    } else if (fields[1].endsWith(", Last>")) {
      if (rangeClass !== 0) {
        for (let value = rangeStart; value <= codePoint; value += 1) {
          combiningClasses.set(value, rangeClass);
        }
      }
      rangeStart = null;
    } else if (combiningClass !== 0) {
      combiningClasses.set(codePoint, combiningClass);
    }

    let mappingText = fields[5].trim();
    if (!mappingText) {
      continue;
    }
    let compatibility = false;
    if (mappingText.startsWith("<")) {
      compatibility = true;
      mappingText = mappingText.slice(mappingText.indexOf(">") + 1).trim();
    }
    decompositions.set(codePoint, {
      compatibility,
      mapping: parseCodePointSequence(mappingText),
    });
  }
  if (rangeStart !== null) {
    throw new Error("Unterminated UnicodeData range");
  }
  return { combiningClasses, decompositions };
}

function parsePropertySet(text, propertyName) {
  const values = new Set();
  for (const rawLine of text.split(/\r?\n/)) {
    const line = rawLine.replace(/#.*/, "").trim();
    if (!line) {
      continue;
    }
    const [rangeText, property] = line.split(";").map((part) => part.trim());
    if (property !== propertyName) {
      continue;
    }
    const bounds = rangeText.split("..").map((item) => parseInt(item, 16));
    const end = bounds.length === 1 ? bounds[0] : bounds[1];
    for (let value = bounds[0]; value <= end; value += 1) {
      values.add(value);
    }
  }
  return values;
}

function buildCompositions(decompositions, exclusions) {
  const compositions = [];
  const seenPairs = new Set();
  for (const [composite, entry] of decompositions) {
    if (entry.compatibility || entry.mapping.length !== 2 ||
        exclusions.has(composite)) {
      continue;
    }
    const key = `${entry.mapping[0]}:${entry.mapping[1]}`;
    if (seenPairs.has(key)) {
      throw new Error(`Duplicate canonical composition pair ${key}`);
    }
    seenPairs.add(key);
    compositions.push([entry.mapping[0], entry.mapping[1], composite]);
  }
  compositions.sort((left, right) =>
    left[0] - right[0] || left[1] - right[1]);
  return compositions;
}

function pascalNumber(value) {
  return `$${value.toString(16).toUpperCase()}`;
}

function emitArray(lines, name, type, values, perLine = 12) {
  lines.push(`  ${name}: array[0..${values.length - 1}] of ${type} = (`);
  for (let start = 0; start < values.length; start += perLine) {
    const row = values.slice(start, start + perLine).map(pascalNumber);
    const suffix = start + perLine < values.length ? "," : "";
    lines.push(`    ${row.join(", ")}${suffix}`);
  }
  lines.push("  );");
}

function generatePascal(version, combiningClasses, decompositions,
  compositions) {
  const decompositionSources = [];
  const decompositionOffsets = [];
  const decompositionLengths = [];
  const decompositionCompatibility = [];
  const decompositionCodePoints = [];
  for (const [source, entry] of [...decompositions].sort((a, b) => a[0] - b[0])) {
    decompositionSources.push(source);
    decompositionOffsets.push(decompositionCodePoints.length);
    decompositionLengths.push(entry.mapping.length);
    decompositionCompatibility.push(entry.compatibility ? 1 : 0);
    decompositionCodePoints.push(...entry.mapping);
  }

  const combiningSources = [...combiningClasses.keys()].sort((a, b) => a - b);
  const combiningValues = combiningSources.map((source) => combiningClasses.get(source));
  const compositionFirst = compositions.map((entry) => entry[0]);
  const compositionSecond = compositions.map((entry) => entry[1]);
  const compositionResults = compositions.map((entry) => entry[2]);

  const lines = [
    "unit Generated.UnicodeNormalizationData;",
    "",
    "{$I Shared.inc}",
    "",
    "// Generated by scripts/generate-unicode-normalization-data.js",
    `// Source: Unicode ${version} UnicodeData.txt and`,
    "// DerivedNormalizationProps.txt.",
    "",
    "interface",
    "",
    `const GeneratedUnicodeNormalizationDataVersion = '${version}';`,
    "",
    "function UnicodeCanonicalCombiningClass(const ACodePoint: Cardinal): Byte;",
    "function TryGetUnicodeDecomposition(const ACodePoint: Cardinal;",
    "  const ACompatibility: Boolean; out AOffset, ALength: Integer): Boolean;",
    "function UnicodeDecompositionCodePoint(const AIndex: Integer): Cardinal;",
    "function TryGetUnicodeComposition(const AFirst, ASecond: Cardinal;",
    "  out AComposite: Cardinal): Boolean;",
    "",
    "implementation",
    "",
    "const",
  ];

  emitArray(lines, "COMBINING_SOURCES", "Cardinal", combiningSources);
  emitArray(lines, "COMBINING_VALUES", "Byte", combiningValues);
  emitArray(lines, "DECOMPOSITION_SOURCES", "Cardinal", decompositionSources);
  emitArray(lines, "DECOMPOSITION_OFFSETS", "Cardinal", decompositionOffsets);
  emitArray(lines, "DECOMPOSITION_LENGTHS", "Byte", decompositionLengths);
  emitArray(lines, "DECOMPOSITION_COMPATIBILITY", "Byte",
    decompositionCompatibility);
  emitArray(lines, "DECOMPOSITION_CODE_POINTS", "Cardinal",
    decompositionCodePoints);
  emitArray(lines, "COMPOSITION_FIRST", "Cardinal", compositionFirst);
  emitArray(lines, "COMPOSITION_SECOND", "Cardinal", compositionSecond);
  emitArray(lines, "COMPOSITION_RESULTS", "Cardinal", compositionResults);

  lines.push(`
function FindCodePoint(const ACodePoint: Cardinal;
  const AValues: array of Cardinal): Integer;
var
  LowIndex, HighIndex, MiddleIndex: Integer;
begin
  LowIndex := 0;
  HighIndex := Length(AValues) - 1;
  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if AValues[MiddleIndex] = ACodePoint then
      Exit(MiddleIndex);
    if AValues[MiddleIndex] < ACodePoint then
      LowIndex := MiddleIndex + 1
    else
      HighIndex := MiddleIndex - 1;
  end;
  Result := -1;
end;

function UnicodeCanonicalCombiningClass(const ACodePoint: Cardinal): Byte;
var
  Index: Integer;
begin
  Index := FindCodePoint(ACodePoint, COMBINING_SOURCES);
  if Index >= 0 then
    Result := COMBINING_VALUES[Index]
  else
    Result := 0;
end;

function TryGetUnicodeDecomposition(const ACodePoint: Cardinal;
  const ACompatibility: Boolean; out AOffset, ALength: Integer): Boolean;
var
  Index: Integer;
begin
  Index := FindCodePoint(ACodePoint, DECOMPOSITION_SOURCES);
  Result := (Index >= 0) and
    (ACompatibility or (DECOMPOSITION_COMPATIBILITY[Index] = 0));
  if Result then
  begin
    AOffset := DECOMPOSITION_OFFSETS[Index];
    ALength := DECOMPOSITION_LENGTHS[Index];
  end
  else
  begin
    AOffset := 0;
    ALength := 0;
  end;
end;

function UnicodeDecompositionCodePoint(const AIndex: Integer): Cardinal;
begin
  Result := DECOMPOSITION_CODE_POINTS[AIndex];
end;

function TryGetUnicodeComposition(const AFirst, ASecond: Cardinal;
  out AComposite: Cardinal): Boolean;
var
  Comparison: Integer;
  LowIndex, HighIndex, MiddleIndex: Integer;
begin
  LowIndex := 0;
  HighIndex := Length(COMPOSITION_FIRST) - 1;
  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    if COMPOSITION_FIRST[MiddleIndex] < AFirst then
      Comparison := -1
    else if COMPOSITION_FIRST[MiddleIndex] > AFirst then
      Comparison := 1
    else if COMPOSITION_SECOND[MiddleIndex] < ASecond then
      Comparison := -1
    else if COMPOSITION_SECOND[MiddleIndex] > ASecond then
      Comparison := 1
    else
    begin
      AComposite := COMPOSITION_RESULTS[MiddleIndex];
      Exit(True);
    end;
    if Comparison < 0 then
      LowIndex := MiddleIndex + 1
    else
      HighIndex := MiddleIndex - 1;
  end;
  AComposite := 0;
  Result := False;
end;

end.`);
  return `${lines.join("\n")}\n`;
}

async function main() {
  const { unicodeVersion, outputFile } = parseArguments();
  const [unicodeDataText, derivedNormalizationText] = await Promise.all([
    downloadUCDFile(unicodeVersion, "UnicodeData.txt"),
    downloadUCDFile(unicodeVersion, "DerivedNormalizationProps.txt"),
  ]);
  const { combiningClasses, decompositions } =
    parseUnicodeData(unicodeDataText);
  const exclusions = parsePropertySet(
    derivedNormalizationText,
    "Full_Composition_Exclusion",
  );
  const compositions = buildCompositions(decompositions, exclusions);
  fs.writeFileSync(
    outputFile,
    generatePascal(
      unicodeVersion,
      combiningClasses,
      decompositions,
      compositions,
    ),
    "utf8",
  );
  console.log(`Wrote ${outputFile}`);
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
