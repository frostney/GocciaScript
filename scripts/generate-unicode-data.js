#!/usr/bin/env node

const fs = require("fs");
const os = require("os");
const path = require("path");
const {
  buildResourceContainer,
  downloadText,
  generateResourceFile,
  pascalUnitNameForOutput,
  resourceFileForOutput,
} = require("./lib/resource-container");

const REPO_ROOT = path.resolve(__dirname, "..");
const DEFAULT_OUTPUT = path.join(
  REPO_ROOT,
  "source",
  "generated",
  "Generated.UnicodeData.pas",
);
const DEFAULT_UNICODE_VERSION = "17.0.0";
const UCD_BASE_URL = "https://www.unicode.org/Public";

const UCD_FILES = [
  "extracted/DerivedGeneralCategory.txt",
  "Scripts.txt",
  "ScriptExtensions.txt",
  "PropList.txt",
  "DerivedCoreProperties.txt",
  "extracted/DerivedBinaryProperties.txt",
  "PropertyValueAliases.txt",
  "PropertyAliases.txt",
  "emoji/emoji-data.txt",
  "CaseFolding.txt",
  "UnicodeData.txt",
];

const EMOJI_SEQUENCE_FILES = [
  "emoji-sequences.txt",
  "emoji-zwj-sequences.txt",
];

const RESOURCE_NAME = "GOCCIA_UCD";
const RESOURCE_MAGIC = Buffer.from("GOCCIAUC", "ascii");

const GC_GROUPS = {
  LC: ["Lu", "Ll", "Lt"],
  L: ["Lu", "Ll", "Lt", "Lm", "Lo"],
  M: ["Mn", "Mc", "Me"],
  N: ["Nd", "Nl", "No"],
  P: ["Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po"],
  S: ["Sm", "Sc", "Sk", "So"],
  Z: ["Zs", "Zl", "Zp"],
  C: ["Cc", "Cf", "Cs", "Co", "Cn"],
};

function usage() {
  console.error(
    "Usage: node scripts/generate-unicode-data.js [unicode-version] [output-file]",
  );
  process.exit(1);
}

function parseArguments() {
  if (process.argv.length > 4) {
    usage();
  }

  const unicodeVersion = process.argv[2] || DEFAULT_UNICODE_VERSION;
  const outputFile = process.argv[3]
    ? path.resolve(process.argv[3])
    : DEFAULT_OUTPUT;
  const outputResourceFile = resourceFileForOutput(outputFile, "Unicode data");
  const unitName = pascalUnitNameForOutput(outputFile, "Unicode data");

  if (path.resolve(outputFile) === path.resolve(outputResourceFile)) {
    throw new Error(
      `Refusing to use ${outputFile} for both Pascal and resource output`,
    );
  }

  return { unicodeVersion, outputFile, outputResourceFile, unitName };
}

async function downloadUCDFile(unicodeVersion, filePath) {
  const url = `${UCD_BASE_URL}/${unicodeVersion}/ucd/${filePath}`;
  console.log(`  Downloading ${filePath}...`);
  return downloadText(url);
}

async function downloadEmojiFile(unicodeVersion, filePath) {
  const url = `${UCD_BASE_URL}/${unicodeVersion}/emoji/${filePath}`;
  console.log(`  Downloading emoji/${filePath}...`);
  return downloadText(url);
}

function parseRange(rangePart) {
  const dotDot = rangePart.indexOf("..");
  if (dotDot >= 0) {
    return { lo: parseInt(rangePart.slice(0, dotDot), 16), hi: parseInt(rangePart.slice(dotDot + 2), 16) };
  }
  const cp = parseInt(rangePart, 16);
  return { lo: cp, hi: cp };
}

function forEachUCDLine(text, minFields, callback) {
  for (const line of text.split("\n")) {
    const commentIndex = line.indexOf("#");
    const content = (commentIndex >= 0 ? line.slice(0, commentIndex) : line).trim();
    if (content.length === 0) {
      continue;
    }

    const parts = content.split(";").map((part) => part.trim());
    if (parts.length < minFields) {
      continue;
    }

    callback(parts);
  }
}

function parseUCDRangeFile(text) {
  const properties = new Map();

  forEachUCDLine(text, 2, (parts) => {
    const propertyValue = parts[1];
    if (!properties.has(propertyValue)) {
      properties.set(propertyValue, []);
    }
    properties.get(propertyValue).push(parseRange(parts[0]));
  });

  for (const [key, ranges] of properties) {
    properties.set(key, mergeRanges(ranges));
  }

  return properties;
}

function parsePropertyValueAliases(text) {
  const aliases = new Map();

  forEachUCDLine(text, 3, (parts) => {
    const propertyAbbr = parts[0];
    if (!aliases.has(propertyAbbr)) {
      aliases.set(propertyAbbr, new Map());
    }

    const valueMap = aliases.get(propertyAbbr);
    const shortName = parts[1];
    for (const name of parts.slice(1)) {
      if (name.length > 0 && !valueMap.has(name)) {
        valueMap.set(name, shortName);
      }
    }
  });

  return aliases;
}

function parsePropertyAliases(text) {
  const aliases = new Map();

  forEachUCDLine(text, 2, (parts) => {
    const shortName = parts[0];
    for (const name of parts) {
      if (name.length > 0) {
        aliases.set(name, shortName);
      }
    }
  });

  return aliases;
}

function parseCaseFolding(text) {
  const pairs = [];

  forEachUCDLine(text, 3, (parts) => {
    const status = parts[1];
    if (status !== "C" && status !== "S") {
      return;
    }

    const source = parseInt(parts[0], 16);
    const mapping = parts[2].split(/\s+/).filter((part) => part.length > 0);
    if (mapping.length !== 1) {
      return;
    }

    pairs.push({ lo: source, hi: parseInt(mapping[0], 16) });
  });

  pairs.sort((a, b) => a.lo - b.lo);
  return pairs;
}

function parseRegExpNonUnicodeUppercase(text) {
  const pairs = [];

  forEachUCDLine(text, 13, (parts) => {
    if (parts[12].length === 0) {
      return;
    }

    const source = parseInt(parts[0], 16);
    const target = parseInt(parts[12], 16);
    if (source >= 0x80 && target < 0x80) {
      return;
    }

    pairs.push({ lo: source, hi: target });
  });

  pairs.sort((a, b) => a.lo - b.lo);
  return pairs;
}

function mergeRanges(ranges) {
  if (ranges.length === 0) {
    return [];
  }

  ranges.sort((a, b) => a.lo - b.lo);
  const merged = [{ lo: ranges[0].lo, hi: ranges[0].hi }];

  for (let i = 1; i < ranges.length; i += 1) {
    const current = ranges[i];
    const last = merged[merged.length - 1];
    if (current.lo <= last.hi + 1) {
      last.hi = Math.max(last.hi, current.hi);
    } else {
      merged.push({ lo: current.lo, hi: current.hi });
    }
  }

  return merged;
}

function unionRanges(rangeArrays) {
  const all = [];
  for (const ranges of rangeArrays) {
    all.push(...ranges);
  }
  return mergeRanges(all);
}

function complementRanges(ranges) {
  const result = [];
  let cursor = 0;

  for (const range of ranges) {
    if (range.lo > cursor) {
      result.push({ lo: cursor, hi: range.lo - 1 });
    }
    cursor = range.hi + 1;
  }

  if (cursor <= 0x10ffff) {
    result.push({ lo: cursor, hi: 0x10ffff });
  }

  return result;
}

function subtractRanges(baseRanges, removeRanges) {
  const base = mergeRanges([...baseRanges]);
  const remove = mergeRanges([...removeRanges]);
  const result = [];
  let removeIndex = 0;

  for (const baseRange of base) {
    let cursor = baseRange.lo;

    while (removeIndex < remove.length && remove[removeIndex].hi < cursor) {
      removeIndex += 1;
    }

    let scanIndex = removeIndex;
    while (scanIndex < remove.length && remove[scanIndex].lo <= baseRange.hi) {
      const removeRange = remove[scanIndex];
      if (removeRange.lo > cursor) {
        result.push({ lo: cursor, hi: removeRange.lo - 1 });
      }
      cursor = Math.max(cursor, removeRange.hi + 1);
      if (cursor > baseRange.hi) {
        break;
      }
      scanIndex += 1;
    }

    if (cursor <= baseRange.hi) {
      result.push({ lo: cursor, hi: baseRange.hi });
    }
  }

  return result;
}

function buildRangeDataBlob(ranges) {
  const buffer = Buffer.alloc(ranges.length * 8);
  for (let i = 0; i < ranges.length; i += 1) {
    buffer.writeUInt32LE(ranges[i].lo, i * 8);
    buffer.writeUInt32LE(ranges[i].hi, i * 8 + 4);
  }
  return buffer;
}

function buildStringSequenceBlob(sequences) {
  let wordCount = 1;
  for (const sequence of sequences) {
    wordCount += 1 + sequence.length;
  }

  const buffer = Buffer.alloc(wordCount * 4);
  let offset = 0;
  buffer.writeUInt32LE(sequences.length, offset);
  offset += 4;
  for (const sequence of sequences) {
    buffer.writeUInt32LE(sequence.length, offset);
    offset += 4;
    for (const codePoint of sequence) {
      buffer.writeUInt32LE(codePoint, offset);
      offset += 4;
    }
  }
  return buffer;
}

function parseEmojiSequenceCodePoints(value) {
  if (value.includes("..")) {
    const range = parseRange(value);
    const sequences = [];
    for (let codePoint = range.lo; codePoint <= range.hi; codePoint += 1) {
      sequences.push([codePoint]);
    }
    return sequences;
  }

  return [
    value
      .split(/\s+/)
      .filter((part) => part.length > 0)
      .map((part) => parseInt(part, 16)),
  ];
}

function parseEmojiSequenceProperties(text) {
  const properties = new Map();

  forEachUCDLine(text, 2, (parts) => {
    const sequences = parseEmojiSequenceCodePoints(parts[0]);
    const property = parts[1];
    if (!properties.has(property)) {
      properties.set(property, []);
    }
    properties.get(property).push(...sequences);
  });

  return properties;
}

function collectEmojiStringProperties(sequenceFiles) {
  const properties = new Map();

  function addSequence(property, sequence) {
    if (!properties.has(property)) {
      properties.set(property, []);
    }
    properties.get(property).push(sequence);
  }

  for (const text of sequenceFiles) {
    const parsed = parseEmojiSequenceProperties(text);
    for (const [property, sequences] of parsed) {
      for (const sequence of sequences) {
        addSequence(property, sequence);
      }
    }
  }

  const rgiParts = [
    "Basic_Emoji",
    "Emoji_Keycap_Sequence",
    "RGI_Emoji_Flag_Sequence",
    "RGI_Emoji_Modifier_Sequence",
    "RGI_Emoji_Tag_Sequence",
    "RGI_Emoji_ZWJ_Sequence",
  ];
  for (const property of rgiParts) {
    for (const sequence of properties.get(property) || []) {
      addSequence("RGI_Emoji", sequence);
    }
  }

  for (const [property, sequences] of properties) {
    const seen = new Set();
    const unique = [];
    for (const sequence of sequences) {
      const key = sequence.join(",");
      if (!seen.has(key)) {
        seen.add(key);
        unique.push(sequence);
      }
    }
    unique.sort((a, b) => {
      if (a.length !== b.length) {
        return a.length - b.length;
      }
      const length = Math.min(a.length, b.length);
      for (let i = 0; i < length; i += 1) {
        if (a[i] !== b[i]) {
          return a[i] - b[i];
        }
      }
      return 0;
    });
    properties.set(property, unique);
  }

  return properties;
}

function generatePascalUnit(
  unicodeVersion,
  entryCount,
  blobByteCount,
  unitName,
  resourceReference,
) {
  return `unit ${unitName};

{$I Goccia.inc}

// Generated by scripts/generate-unicode-data.js
// Source: Unicode ${unicodeVersion} UCD
// Resource: ${resourceReference}

interface

const
  GeneratedUnicodeDataVersion = '${unicodeVersion.replaceAll("'", "''")}';
  GeneratedUnicodeDataResourceName = '${RESOURCE_NAME}';
  GeneratedUnicodeDataEntryCount = ${entryCount};
  GeneratedUnicodeDataBlobByteCount = ${blobByteCount};

implementation

{$IFDEF GOCCIA_REGEXP_EMBEDDED_UCD}
{$R ${resourceReference}}
{$ENDIF}

end.
`;
}

function findAllAliases(aliasMap, value) {
  const canonicalShort = aliasMap.get(value);
  if (!canonicalShort) {
    return [value];
  }

  const aliases = new Set();
  for (const [alias, short] of aliasMap) {
    if (short === canonicalShort) {
      aliases.add(alias);
    }
  }
  aliases.add(value);
  return [...aliases];
}

function collectAllEntries(
  gcData,
  scriptData,
  scxData,
  binaryProperties,
  caseFoldingPairs,
  nonUnicodeUppercasePairs,
  stringProperties,
  pvAliases,
  pAliases,
) {
  const entries = new Map();

  function addEntry(key, ranges) {
    if (ranges.length === 0) {
      return;
    }
    const blob = buildRangeDataBlob(ranges);
    if (!entries.has(key)) {
      entries.set(key, { ranges, blob });
    }
  }

  function addAliasEntry(key, canonicalKey) {
    const canonical = entries.get(canonicalKey);
    if (canonical && !entries.has(key)) {
      entries.set(key, canonical);
    }
  }

  function addBlobEntry(key, blob) {
    if (blob.length === 0 || entries.has(key)) {
      return;
    }
    entries.set(key, { blob });
  }

  function registerProperty(data, prefix, aliasMap, addBareAlias) {
    for (const [value, ranges] of data) {
      const canonicalKey = `${prefix}/${value}`;
      addEntry(canonicalKey, ranges);

      if (addBareAlias) {
        addAliasEntry(value, canonicalKey);
      }

      for (const alias of findAllAliases(aliasMap, value)) {
        addAliasEntry(`${prefix}/${alias}`, canonicalKey);
        if (addBareAlias) {
          addAliasEntry(alias, canonicalKey);
        }
      }
    }
  }

  const gcAliases = pvAliases.get("gc") || new Map();
  registerProperty(gcData, "gc", gcAliases, true);

  for (const [group, members] of Object.entries(GC_GROUPS)) {
    const memberRanges = members
      .filter((m) => gcData.has(m))
      .map((m) => gcData.get(m));
    if (memberRanges.length > 0) {
      registerProperty(new Map([[group, unionRanges(memberRanges)]]), "gc", gcAliases, true);
    }
  }

  registerProperty(scriptData, "sc", pvAliases.get("sc") || new Map(), false);
  registerProperty(scxData, "scx", pvAliases.get("scx") || pvAliases.get("sc") || new Map(), false);

  for (const [value, ranges] of binaryProperties) {
    addEntry(value, ranges);
    for (const alias of findAllAliases(pAliases, value)) {
      addAliasEntry(alias, value);
    }
  }

  addEntry("ASCII", [{ lo: 0, hi: 0x7f }]);
  addEntry("Any", [{ lo: 0, hi: 0x10ffff }]);
  addEntry("CaseFolding/Simple", caseFoldingPairs);
  addEntry("CaseMapping/RegExpNonUnicodeUppercase", nonUnicodeUppercasePairs);

  for (const [property, sequences] of stringProperties) {
    addBlobEntry(`strings/${property}`, buildStringSequenceBlob(sequences));
  }

  const cnRanges = gcData.get("Cn");
  if (cnRanges) {
    addEntry("Assigned", complementRanges(cnRanges));
  }

  const prefixPropNames = [
    { prefix: "gc", short: "gc" },
    { prefix: "sc", short: "sc" },
    { prefix: "scx", short: "scx" },
  ];

  for (const { prefix, short } of prefixPropNames) {
    const propNames = new Set();
    for (const [alias, canonical] of pAliases) {
      if (canonical === short && alias !== short) {
        propNames.add(alias);
      }
    }

    for (const key of [...entries.keys()]) {
      if (key.startsWith(`${prefix}/`)) {
        const valuePart = key.slice(prefix.length + 1);
        for (const propName of propNames) {
          addAliasEntry(`${propName}/${valuePart}`, key);
        }
      }
    }
  }

  return entries;
}

function buildResourceFromEntries(allEntries) {
  const sortedKeys = [...allEntries.keys()].sort();

  const blobByContent = new Map();
  const dataBuffers = [];
  let dataOffset = 0;

  const indexedEntries = [];

  for (const key of sortedKeys) {
    const entry = allEntries.get(key);
    const contentKey = entry.blob.toString("base64");

    if (!blobByContent.has(contentKey)) {
      blobByContent.set(contentKey, { offset: dataOffset, length: entry.blob.length });
      dataBuffers.push(entry.blob);
      dataOffset += entry.blob.length;
    }

    const blobInfo = blobByContent.get(contentKey);
    indexedEntries.push({
      name: key,
      offset: blobInfo.offset,
      length: blobInfo.length,
    });
  }

  return {
    indexedEntries,
    blob: Buffer.concat(dataBuffers),
  };
}

function parseScriptExtensions(text, scriptData, scAliases) {
  const properties = new Map();

  const shortToDataKey = new Map();
  if (scAliases) {
    for (const [alias, short] of scAliases) {
      if (scriptData.has(alias) && !shortToDataKey.has(short)) {
        shortToDataKey.set(short, alias);
      }
    }
  }

  forEachUCDLine(text, 2, (parts) => {
    const range = parseRange(parts[0]);
    const scripts = parts[1].split(/\s+/);

    for (const script of scripts) {
      if (!properties.has(script)) {
        let baseRanges = scriptData.get(script);
        if (!baseRanges) {
          const dataKey = shortToDataKey.get(script);
          if (dataKey) {
            baseRanges = scriptData.get(dataKey);
          }
        }
        properties.set(script, baseRanges ? [...baseRanges] : []);
      }
      properties.get(script).push(range);
    }
  });

  for (const [key, ranges] of properties) {
    properties.set(key, mergeRanges(ranges));
  }

  return properties;
}

async function main() {
  const { unicodeVersion, outputFile, outputResourceFile, unitName } =
    parseArguments();

  console.log(
    `Generating Unicode data for version ${unicodeVersion}...`,
  );

  console.log("Downloading UCD files...");
  const [
    gcText,
    scriptsText,
    scxText,
    propListText,
    derivedCoreText,
    derivedBinaryText,
    pvAliasesText,
    pAliasesText,
    emojiText,
    caseFoldingText,
    unicodeDataText,
  ] = await Promise.all(
    UCD_FILES.map((file) => downloadUCDFile(unicodeVersion, file)),
  );
  const emojiSequenceTexts = await Promise.all(
    EMOJI_SEQUENCE_FILES.map((file) => downloadEmojiFile(unicodeVersion, file)),
  );

  console.log("Parsing UCD data...");
  const pvAliases = parsePropertyValueAliases(pvAliasesText);
  const pAliases = parsePropertyAliases(pAliasesText);
  const gcData = parseUCDRangeFile(gcText);
  const scriptData = parseUCDRangeFile(scriptsText);
  scriptData.set(
    "Unknown",
    complementRanges(unionRanges(
      [...scriptData]
        .filter(([script]) => script !== "Unknown")
        .map(([, ranges]) => ranges),
    )),
  );
  const scAliasMap = pvAliases.get("sc") || new Map();
  const scxData = parseScriptExtensions(scxText, scriptData, scAliasMap);
  scxData.set("Unknown", scriptData.get("Unknown"));
  scxData.set("Zzzz", scriptData.get("Unknown"));
  for (const commonKey of ["Common", "Zyyy"]) {
    if (scxData.has(commonKey)) {
      scxData.set(commonKey, subtractRanges(
        scxData.get(commonKey),
        scriptData.get("Unknown"),
      ));
    }
  }
  const propListData = parseUCDRangeFile(propListText);
  const derivedCoreData = parseUCDRangeFile(derivedCoreText);
  const derivedBinaryData = parseUCDRangeFile(derivedBinaryText);
  const emojiData = parseUCDRangeFile(emojiText);
  const caseFoldingPairs = parseCaseFolding(caseFoldingText);
  const nonUnicodeUppercasePairs = parseRegExpNonUnicodeUppercase(unicodeDataText);
  const stringProperties = collectEmojiStringProperties(emojiSequenceTexts);

  const binaryProperties = new Map([
    ...propListData, ...derivedCoreData, ...derivedBinaryData, ...emojiData,
  ]);

  console.log("Building property tables...");
  console.log(`  General_Category: ${gcData.size} values`);
  console.log(`  Script: ${scriptData.size} values`);
  console.log(`  Script_Extensions: ${scxData.size} values`);
  console.log(`  Binary properties: ${binaryProperties.size} values`);
  console.log(`  Simple case folding pairs: ${caseFoldingPairs.length}`);
  console.log(`  Non-Unicode uppercase pairs: ${nonUnicodeUppercasePairs.length}`);
  console.log(`  String properties: ${stringProperties.size} values`);

  const allEntries = collectAllEntries(
    gcData,
    scriptData,
    scxData,
    binaryProperties,
    caseFoldingPairs,
    nonUnicodeUppercasePairs,
    stringProperties,
    pvAliases,
    pAliases,
  );
  console.log(`  Total entries (with aliases): ${allEntries.size}`);

  console.log("Building resource...");
  const { indexedEntries, blob } = buildResourceFromEntries(allEntries);
  const resource = buildResourceContainer(
    RESOURCE_MAGIC,
    unicodeVersion,
    indexedEntries,
    blob,
  );

  const resourceReference = path.basename(outputResourceFile);
  console.log(`Writing ${resourceReference}...`);
  generateResourceFile(resource, outputResourceFile, RESOURCE_NAME, {
    temporaryDirectoryPrefix: "goccia-ucdresource-",
    dataFileName: "unicode-data.bin",
    scriptFileName: "unicode-data.rc",
  });

  console.log(`Writing ${path.basename(outputFile)}...`);
  const pascalSource = generatePascalUnit(
    unicodeVersion,
    indexedEntries.length,
    blob.length,
    unitName,
    resourceReference,
  );
  fs.writeFileSync(outputFile, pascalSource, "utf8");

  console.log(
    `Done: ${indexedEntries.length} entries, ${blob.length} bytes of range data`,
  );
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
