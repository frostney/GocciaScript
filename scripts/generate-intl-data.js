#!/usr/bin/env node

const fs = require("fs");
const os = require("os");
const path = require("path");
const { execFileSync } = require("child_process");

const REPO_ROOT = path.resolve(__dirname, "..");
const DEFAULT_OUTPUT = path.join(
  REPO_ROOT,
  "source",
  "generated",
  "Generated.IntlData.pas",
);
const DEFAULT_CLDR_VERSION = "45.0.0";
const CLDR_PACKAGES = [
  "cldr-core",
  "cldr-numbers-modern",
  "cldr-misc-modern",
  "cldr-dates-modern",
  "cldr-localenames-modern",
];
const TARGET_LOCALES = [
  "ar", "cs", "de", "en", "es", "fr", "hi", "id", "it", "ja",
  "ko", "nl", "pl", "pt", "ru", "sv", "th", "tr", "vi", "zh",
];
const RELATIVE_TIME_UNITS = [
  "year", "quarter", "month", "week", "day", "hour", "minute", "second",
];
const RESOURCE_NAME = "GOCCIA_CLDR";
const RESOURCE_MAGIC = Buffer.from("GOCCIACL", "ascii");
const RESOURCE_FORMAT_VERSION = 1;
const RESOURCE_HEADER_SIZE = RESOURCE_MAGIC.length + 6 * 4;
const RESOURCE_ENTRY_SIZE = 4 * 4;
const PASCAL_UNIT_IDENTIFIER_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;

function usage() {
  console.error("Usage: node scripts/generate-intl-data.js [cldr-version] [output-file]");
  process.exit(1);
}

function parseArguments() {
  if (process.argv.length > 4) {
    usage();
  }

  const cldrVersion = process.argv[2] || DEFAULT_CLDR_VERSION;
  const outputFile = process.argv[3] ? path.resolve(process.argv[3]) : DEFAULT_OUTPUT;
  const outputResourceFile = resourceFileForOutput(outputFile);
  const unitName = pascalUnitNameForOutput(outputFile);

  if (path.resolve(outputFile) === path.resolve(outputResourceFile)) {
    throw new Error(`Refusing to use ${outputFile} for both Pascal and resource output`);
  }

  return { cldrVersion, outputFile, outputResourceFile, unitName };
}

function pascalUnitNameForOutput(outputFile) {
  if (path.extname(outputFile).toLowerCase() !== ".pas") {
    throw new Error(`Intl data output must be a .pas file: ${outputFile}`);
  }

  const unitName = path.basename(outputFile, path.extname(outputFile));
  if (unitName.length === 0) {
    throw new Error(`Intl data output must have a non-empty Pascal unit name: ${outputFile}`);
  }

  const unitNameParts = unitName.split(".");
  if (
    unitNameParts.length === 0 ||
    unitNameParts.some((unitNamePart) => !PASCAL_UNIT_IDENTIFIER_PATTERN.test(unitNamePart))
  ) {
    throw new Error(`Intl data output basename is not a valid Pascal unit name: ${unitName}`);
  }

  return unitName;
}

function resourceFileForOutput(outputFile) {
  if (path.extname(outputFile).toLowerCase() !== ".pas") {
    throw new Error(`Intl data output must be a .pas file: ${outputFile}`);
  }

  return path.join(
    path.dirname(outputFile),
    `${path.basename(outputFile, path.extname(outputFile))}.res`,
  );
}

function downloadCldrPackages(cldrVersion, outputDirectory) {
  fs.mkdirSync(outputDirectory, { recursive: true });

  for (const packageName of CLDR_PACKAGES) {
    const spec = `${packageName}@${cldrVersion}`;
    console.log(`Downloading ${spec}...`);
    execFileSync("npm", ["pack", spec, "--pack-destination", outputDirectory], {
      stdio: ["ignore", "ignore", "inherit"],
    });

    const tarballName = `${packageName}-${cldrVersion}.tgz`;
    const tarballPath = path.join(outputDirectory, tarballName);
    const extractDirectory = path.join(outputDirectory, packageName);
    fs.mkdirSync(extractDirectory, { recursive: true });
    execFileSync("tar", ["-xzf", tarballPath, "-C", extractDirectory, "--strip-components=1"], {
      stdio: "inherit",
    });
    fs.unlinkSync(tarballPath);
  }
}

function readJson(filePath) {
  return JSON.parse(fs.readFileSync(filePath, "utf8"));
}

function extractLikelySubtags(cldrDir) {
  const data = readJson(path.join(cldrDir, "cldr-core", "supplemental", "likelySubtags.json"));
  return data.supplemental.likelySubtags;
}

function extractAliases(cldrDir) {
  const data = readJson(path.join(cldrDir, "cldr-core", "supplemental", "aliases.json"));
  const alias = data.supplemental.metadata.alias;

  const languageAliases = {};
  for (const [key, value] of Object.entries(alias.languageAlias)) {
    languageAliases[key] = value._replacement;
  }

  const territoryAliases = {};
  for (const [key, value] of Object.entries(alias.territoryAlias)) {
    territoryAliases[key] = value._replacement;
  }

  const scriptAliases = {};
  for (const [key, value] of Object.entries(alias.scriptAlias)) {
    scriptAliases[key] = value._replacement;
  }

  return { languageAliases, territoryAliases, scriptAliases };
}

function extractGrandfatheredTags(languageAliases) {
  const grandfathered = {};
  for (const [key, replacement] of Object.entries(languageAliases)) {
    if (key.includes("-")) {
      grandfathered[key] = replacement;
    }
  }
  return grandfathered;
}

function extractPluralRules(cldrDir, type) {
  const fileName = type === "cardinal" ? "plurals.json" : "ordinals.json";
  const data = readJson(path.join(cldrDir, "cldr-core", "supplemental", fileName));
  const pluralType = type === "cardinal" ? "plurals-type-cardinal" : "plurals-type-ordinal";
  const rules = data.supplemental[pluralType];

  const result = {};
  for (const [locale, localeRules] of Object.entries(rules)) {
    const entries = {};
    for (const [ruleKey, ruleValue] of Object.entries(localeRules)) {
      const category = ruleKey.replace("pluralRule-count-", "");
      entries[category] = ruleValue;
    }
    result[locale] = entries;
  }
  return result;
}

function extractNumberSymbols(cldrDir, locale) {
  const data = readJson(path.join(cldrDir, "cldr-numbers-modern", "main", locale, "numbers.json"));
  const numbers = data.main[locale].numbers;
  const numSys = numbers.defaultNumberingSystem;
  const symbols = numbers[`symbols-numberSystem-${numSys}`];

  return {
    decimal: symbols.decimal,
    group: symbols.group,
    percentSign: symbols.percentSign,
    plusSign: symbols.plusSign,
    minusSign: symbols.minusSign,
    exponential: symbols.exponential,
    perMille: symbols.perMille,
    infinity: symbols.infinity,
    nan: symbols.nan,
    numberingSystem: numSys,
  };
}

function extractNumberPatterns(cldrDir, locale) {
  const data = readJson(path.join(cldrDir, "cldr-numbers-modern", "main", locale, "numbers.json"));
  const numbers = data.main[locale].numbers;
  const numSys = numbers.defaultNumberingSystem;

  const decimalFormats = numbers[`decimalFormats-numberSystem-${numSys}`];
  const currencyFormats = numbers[`currencyFormats-numberSystem-${numSys}`];
  const percentFormats = numbers[`percentFormats-numberSystem-${numSys}`];

  return {
    decimal: typeof decimalFormats === "string" ? decimalFormats : decimalFormats.standard,
    currency: typeof currencyFormats === "string" ? currencyFormats : currencyFormats.standard,
    percent: typeof percentFormats === "string" ? percentFormats : percentFormats.standard,
    minimumGroupingDigits: numbers.minimumGroupingDigits || "1",
  };
}

function extractListPatterns(cldrDir, locale) {
  const data = readJson(path.join(cldrDir, "cldr-misc-modern", "main", locale, "listPatterns.json"));
  const patterns = data.main[locale].listPatterns;

  const result = {};
  const typeMap = {
    conjunction: "listPattern-type-standard",
    disjunction: "listPattern-type-or",
    unit: "listPattern-type-unit",
  };

  for (const [name, cldrKey] of Object.entries(typeMap)) {
    const pattern = patterns[cldrKey];
    if (pattern) {
      result[name] = {
        pair: pattern["2"],
        start: pattern.start,
        middle: pattern.middle,
        end: pattern.end,
      };
    }
  }

  return result;
}

function extractRelativeTimePatterns(cldrDir, locale) {
  const data = readJson(path.join(cldrDir, "cldr-dates-modern", "main", locale, "dateFields.json"));
  const fields = data.main[locale].dates.fields;

  const result = {};
  for (const unit of RELATIVE_TIME_UNITS) {
    const field = fields[unit];
    if (!field) {
      continue;
    }

    const unitPatterns = {};

    const futurePatterns = field["relativeTime-type-future"];
    if (futurePatterns) {
      for (const [key, value] of Object.entries(futurePatterns)) {
        const count = key.replace("relativeTimePattern-count-", "");
        unitPatterns[`future-${count}`] = value;
      }
    }

    const pastPatterns = field["relativeTime-type-past"];
    if (pastPatterns) {
      for (const [key, value] of Object.entries(pastPatterns)) {
        const count = key.replace("relativeTimePattern-count-", "");
        unitPatterns[`past-${count}`] = value;
      }
    }

    if (field["relative-type--1"]) {
      unitPatterns["relative--1"] = field["relative-type--1"];
    }
    if (field["relative-type-0"]) {
      unitPatterns["relative-0"] = field["relative-type-0"];
    }
    if (field["relative-type-1"]) {
      unitPatterns["relative-1"] = field["relative-type-1"];
    }

    result[unit] = unitPatterns;
  }

  return result;
}

function extractDisplayNames(cldrDir, locale, category) {
  if (category === "languages") {
    const data = readJson(path.join(cldrDir, "cldr-localenames-modern", "main", locale, "languages.json"));
    return data.main[locale].localeDisplayNames.languages;
  }

  if (category === "territories") {
    const data = readJson(path.join(cldrDir, "cldr-localenames-modern", "main", locale, "territories.json"));
    return data.main[locale].localeDisplayNames.territories;
  }

  if (category === "scripts") {
    const data = readJson(path.join(cldrDir, "cldr-localenames-modern", "main", locale, "scripts.json"));
    return data.main[locale].localeDisplayNames.scripts;
  }

  throw new Error(`Unknown display name category: ${category}`);
}

function extractCurrencyData(cldrDir) {
  const data = readJson(path.join(cldrDir, "cldr-core", "supplemental", "currencyData.json"));
  const fractions = data.supplemental.currencyData.fractions;

  const result = {};
  for (const [currency, info] of Object.entries(fractions)) {
    result[currency] = `${info._digits || "2"}:${info._rounding || "0"}`;
  }

  return result;
}

function buildKeyValueBlob(entries) {
  const keys = Object.keys(entries).sort();
  const count = keys.length;
  const strings = [];
  let stringsOffset = 0;
  const indexEntries = [];

  for (const key of keys) {
    const value = String(entries[key]);
    const keyOffset = stringsOffset;
    const keyBytes = Buffer.from(key, "utf8");
    stringsOffset += keyBytes.length;
    const valueOffset = stringsOffset;
    const valueBytes = Buffer.from(value, "utf8");
    stringsOffset += valueBytes.length;

    indexEntries.push({ keyOffset, keyLength: keyBytes.length, valueOffset, valueLength: valueBytes.length });
    strings.push(keyBytes, valueBytes);
  }

  const headerSize = 4 + count * 16;
  const stringsBlob = Buffer.concat(strings);
  const totalSize = headerSize + stringsBlob.length;
  const buffer = Buffer.alloc(totalSize);

  let offset = 0;
  buffer.writeUInt32LE(count, offset); offset += 4;
  for (const entry of indexEntries) {
    buffer.writeUInt32LE(entry.keyOffset, offset); offset += 4;
    buffer.writeUInt32LE(entry.keyLength, offset); offset += 4;
    buffer.writeUInt32LE(entry.valueOffset, offset); offset += 4;
    buffer.writeUInt32LE(entry.valueLength, offset); offset += 4;
  }
  stringsBlob.copy(buffer, offset);

  return buffer;
}

function flattenRelativeTime(relativeTimeData) {
  const flat = {};
  for (const [unit, patterns] of Object.entries(relativeTimeData)) {
    for (const [key, value] of Object.entries(patterns)) {
      flat[`${unit}/${key}`] = value;
    }
  }
  return flat;
}

function collectAllSections(cldrDir) {
  console.log("Extracting CLDR data...");

  const sections = [];

  console.log("  likely subtags...");
  const likelySubtags = extractLikelySubtags(cldrDir);
  sections.push({ name: "likely-subtags", data: likelySubtags });

  console.log("  aliases...");
  const { languageAliases, territoryAliases, scriptAliases } = extractAliases(cldrDir);
  sections.push({ name: "language-aliases", data: languageAliases });
  sections.push({ name: "territory-aliases", data: territoryAliases });
  sections.push({ name: "script-aliases", data: scriptAliases });

  console.log("  grandfathered tags...");
  const grandfathered = extractGrandfatheredTags(languageAliases);
  sections.push({ name: "grandfathered-tags", data: grandfathered });

  console.log("  plural rules (cardinal)...");
  const cardinalRules = extractPluralRules(cldrDir, "cardinal");
  const flatCardinal = {};
  for (const [locale, rules] of Object.entries(cardinalRules)) {
    for (const [category, rule] of Object.entries(rules)) {
      flatCardinal[`${locale}/${category}`] = rule;
    }
  }
  sections.push({ name: "plural-rules-cardinal", data: flatCardinal });

  console.log("  plural rules (ordinal)...");
  const ordinalRules = extractPluralRules(cldrDir, "ordinal");
  const flatOrdinal = {};
  for (const [locale, rules] of Object.entries(ordinalRules)) {
    for (const [category, rule] of Object.entries(rules)) {
      flatOrdinal[`${locale}/${category}`] = rule;
    }
  }
  sections.push({ name: "plural-rules-ordinal", data: flatOrdinal });

  console.log("  currency data...");
  const currencyData = extractCurrencyData(cldrDir);
  sections.push({ name: "currency-data", data: currencyData });

  for (const locale of TARGET_LOCALES) {
    console.log(`  locale ${locale}...`);

    const symbols = extractNumberSymbols(cldrDir, locale);
    sections.push({ name: `number-symbols/${locale}`, data: symbols });

    const patterns = extractNumberPatterns(cldrDir, locale);
    sections.push({ name: `number-patterns/${locale}`, data: patterns });

    const listPatterns = extractListPatterns(cldrDir, locale);
    for (const [type, patternData] of Object.entries(listPatterns)) {
      sections.push({ name: `list-patterns/${locale}/${type}`, data: patternData });
    }

    const relativeTime = extractRelativeTimePatterns(cldrDir, locale);
    const flatRelativeTime = flattenRelativeTime(relativeTime);
    sections.push({ name: `relative-time/${locale}`, data: flatRelativeTime });

    for (const category of ["languages", "territories", "scripts"]) {
      const names = extractDisplayNames(cldrDir, locale, category);
      sections.push({ name: `display-names/${locale}/${category}`, data: names });
    }
  }

  sections.sort((left, right) => {
    if (left.name < right.name) {
      return -1;
    }
    if (left.name > right.name) {
      return 1;
    }
    return 0;
  });

  return sections;
}

function packSections(sections) {
  const indexedEntries = [];
  const dataBuffers = [];
  let dataOffset = 0;

  for (const section of sections) {
    const dataBuffer = buildKeyValueBlob(section.data);
    indexedEntries.push({
      name: section.name,
      offset: dataOffset,
      length: dataBuffer.length,
    });
    dataBuffers.push(dataBuffer);
    dataOffset += dataBuffer.length;
  }

  return {
    indexedEntries,
    blob: Buffer.concat(dataBuffers),
  };
}

function writeUInt32LE(buffer, value, offset) {
  if (value < 0 || value > 0xffffffff) {
    throw new Error(`Resource integer ${value} is outside UInt32 range`);
  }
  buffer.writeUInt32LE(value, offset);
}

function buildResourceContainer(version, entries, blob) {
  const versionBuffer = Buffer.from(version, "utf8");
  const nameBuffers = entries.map((entry) => Buffer.from(entry.name, "utf8"));
  const namesByteCount = nameBuffers.reduce((total, nameBuffer) => total + nameBuffer.length, 0);
  const entryTableByteCount = entries.length * RESOURCE_ENTRY_SIZE;
  const totalByteCount =
    RESOURCE_HEADER_SIZE +
    versionBuffer.length +
    entryTableByteCount +
    namesByteCount +
    blob.length;

  const resource = Buffer.alloc(totalByteCount);
  let offset = 0;

  RESOURCE_MAGIC.copy(resource, offset);
  offset += RESOURCE_MAGIC.length;
  writeUInt32LE(resource, RESOURCE_FORMAT_VERSION, offset); offset += 4;
  writeUInt32LE(resource, versionBuffer.length, offset); offset += 4;
  writeUInt32LE(resource, entries.length, offset); offset += 4;
  writeUInt32LE(resource, namesByteCount, offset); offset += 4;
  writeUInt32LE(resource, blob.length, offset); offset += 4;
  writeUInt32LE(resource, 0, offset); offset += 4;

  versionBuffer.copy(resource, offset);
  offset += versionBuffer.length;

  let nameOffset = 0;
  const namesOffset = RESOURCE_HEADER_SIZE + versionBuffer.length + entryTableByteCount;
  for (let index = 0; index < entries.length; index += 1) {
    const entry = entries[index];
    const nameBuffer = nameBuffers[index];
    writeUInt32LE(resource, nameOffset, offset); offset += 4;
    writeUInt32LE(resource, nameBuffer.length, offset); offset += 4;
    writeUInt32LE(resource, entry.offset, offset); offset += 4;
    writeUInt32LE(resource, entry.length, offset); offset += 4;
    nameBuffer.copy(resource, namesOffset + nameOffset);
    nameOffset += nameBuffer.length;
  }

  blob.copy(resource, namesOffset + namesByteCount);
  return resource;
}

function generateResourceFile(resourceBytes, outputResourceFile) {
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), "goccia-cldrresource-"));
  const resourceDataFile = path.join(temporaryDirectory, "intl-data.bin");
  const resourceScriptFile = path.join(temporaryDirectory, "intl-data.rc");

  try {
    fs.writeFileSync(resourceDataFile, resourceBytes);
    fs.writeFileSync(
      resourceScriptFile,
      `${RESOURCE_NAME} RCDATA "${resourceDataFile.replaceAll("\\", "\\\\")}"\n`,
      "utf8",
    );
    execFileSync("fpcres", ["-of", "res", resourceScriptFile, "-o", outputResourceFile], {
      stdio: "inherit",
    });
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
}

function generatePascalUnit(cldrVersion, sectionCount, blobByteCount, unitName, resourceReference) {
  return `unit ${unitName};

{$I Goccia.inc}

// Generated by scripts/generate-intl-data.js
// Source: Unicode CLDR ${cldrVersion}
// Resource: ${resourceReference}

interface

const
  GeneratedIntlDataVersion = '${cldrVersion.replaceAll("'", "''")}';
  GeneratedIntlDataResourceName = '${RESOURCE_NAME}';
  GeneratedIntlDataSectionCount = ${sectionCount};
  GeneratedIntlDataBlobByteCount = ${blobByteCount};

implementation

{$IFDEF GOCCIA_INTL_EMBEDDED_CLDR}
{$R ${resourceReference}}
{$ENDIF}

end.
`;
}

function main() {
  const { cldrVersion, outputFile, outputResourceFile, unitName } = parseArguments();
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), "goccia-cldrdata-"));

  try {
    downloadCldrPackages(cldrVersion, temporaryDirectory);

    const sections = collectAllSections(temporaryDirectory);
    if (sections.length === 0) {
      console.error("No CLDR data sections extracted");
      process.exit(1);
    }

    const { indexedEntries, blob } = packSections(sections);
    const resource = buildResourceContainer(
      cldrVersion,
      indexedEntries,
      blob,
    );
    const pascal = generatePascalUnit(
      cldrVersion,
      indexedEntries.length,
      blob.length,
      unitName,
      path.basename(outputResourceFile),
    );

    fs.mkdirSync(path.dirname(outputFile), { recursive: true });
    fs.writeFileSync(outputFile, pascal, "utf8");
    generateResourceFile(resource, outputResourceFile);

    console.log(
      `Generated ${path.relative(REPO_ROOT, outputFile)} and ${path.relative(REPO_ROOT, outputResourceFile)} with ${indexedEntries.length} sections, ${blob.length} bytes, CLDR ${cldrVersion}`,
    );
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
}

main();
