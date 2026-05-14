#!/usr/bin/env node

const fs = require("fs");
const https = require("https");
const os = require("os");
const path = require("path");
const { execFileSync } = require("child_process");

const REPO_ROOT = path.resolve(__dirname, "..");
const DEFAULT_OUTPUT = path.join(
  REPO_ROOT,
  "source",
  "generated",
  "Generated.UnicodeData.pas",
);
const DEFAULT_UNICODE_VERSION = "16.0.0";
const UCD_BASE_URL = "https://www.unicode.org/Public";
const DOWNLOAD_TIMEOUT_MS = 30000;
const MAX_REDIRECTS = 5;

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

const RESOURCE_NAME = "GOCCIA_UCD";
const RESOURCE_MAGIC = Buffer.from("GOCCIAUC", "ascii");
const RESOURCE_FORMAT_VERSION = 1;
const RESOURCE_HEADER_SIZE = RESOURCE_MAGIC.length + 6 * 4;
const RESOURCE_ENTRY_SIZE = 4 * 4;
const PASCAL_UNIT_IDENTIFIER_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;

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
  const outputResourceFile = resourceFileForOutput(outputFile);
  const unitName = pascalUnitNameForOutput(outputFile);

  if (path.resolve(outputFile) === path.resolve(outputResourceFile)) {
    throw new Error(
      `Refusing to use ${outputFile} for both Pascal and resource output`,
    );
  }

  return { unicodeVersion, outputFile, outputResourceFile, unitName };
}

function pascalUnitNameForOutput(outputFile) {
  if (path.extname(outputFile).toLowerCase() !== ".pas") {
    throw new Error(
      `Unicode data output must be a .pas file: ${outputFile}`,
    );
  }

  const unitName = path.basename(outputFile, path.extname(outputFile));
  if (unitName.length === 0) {
    throw new Error(
      `Unicode data output must have a non-empty Pascal unit name: ${outputFile}`,
    );
  }

  const unitNameParts = unitName.split(".");
  if (
    unitNameParts.length === 0 ||
    unitNameParts.some(
      (unitNamePart) => !PASCAL_UNIT_IDENTIFIER_PATTERN.test(unitNamePart),
    )
  ) {
    throw new Error(
      `Unicode data output basename is not a valid Pascal unit name: ${unitName}`,
    );
  }

  return unitName;
}

function resourceFileForOutput(outputFile) {
  const directory = path.dirname(outputFile);
  const base = path.basename(outputFile, path.extname(outputFile));
  return path.join(directory, base + ".res");
}

function downloadFile(url, redirectCount) {
  if (redirectCount === undefined) {
    redirectCount = 0;
  }

  return new Promise((resolve, reject) => {
    if (redirectCount > MAX_REDIRECTS) {
      reject(new Error(`Too many redirects downloading ${url}`));
      return;
    }

    const request = https.get(url, { timeout: DOWNLOAD_TIMEOUT_MS }, (response) => {
      if (response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
        resolve(downloadFile(response.headers.location, redirectCount + 1));
        return;
      }

      if (response.statusCode !== 200) {
        reject(new Error(`HTTP ${response.statusCode} downloading ${url}`));
        return;
      }

      const chunks = [];
      response.on("data", (chunk) => chunks.push(chunk));
      response.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
      response.on("error", reject);
    });

    request.on("error", reject);
    request.on("timeout", () => {
      request.destroy();
      reject(new Error(`Timeout downloading ${url}`));
    });
  });
}

async function downloadUCDFile(unicodeVersion, filePath) {
  const url = `${UCD_BASE_URL}/${unicodeVersion}/ucd/${filePath}`;
  console.log(`  Downloading ${filePath}...`);
  return downloadFile(url);
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

function buildRangeDataBlob(ranges) {
  const buffer = Buffer.alloc(ranges.length * 8);
  for (let i = 0; i < ranges.length; i += 1) {
    buffer.writeUInt32LE(ranges[i].lo, i * 8);
    buffer.writeUInt32LE(ranges[i].hi, i * 8 + 4);
  }
  return buffer;
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
  const namesByteCount = nameBuffers.reduce(
    (total, nameBuffer) => total + nameBuffer.length,
    0,
  );
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
  const namesOffset =
    RESOURCE_HEADER_SIZE + versionBuffer.length + entryTableByteCount;
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
  const temporaryDirectory = fs.mkdtempSync(
    path.join(os.tmpdir(), "goccia-ucdresource-"),
  );
  const resourceDataFile = path.join(temporaryDirectory, "unicode-data.bin");
  const resourceScriptFile = path.join(temporaryDirectory, "unicode-data.rc");

  try {
    fs.writeFileSync(resourceDataFile, resourceBytes);
    fs.writeFileSync(
      resourceScriptFile,
      `${RESOURCE_NAME} RCDATA "${resourceDataFile.replaceAll("\\", "\\\\")}"\n`,
      "utf8",
    );
    execFileSync(
      "fpcres",
      ["-of", "res", resourceScriptFile, "-o", outputResourceFile],
      { stdio: "inherit" },
    );
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
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

  console.log("Parsing UCD data...");
  const pvAliases = parsePropertyValueAliases(pvAliasesText);
  const pAliases = parsePropertyAliases(pAliasesText);
  const gcData = parseUCDRangeFile(gcText);
  const scriptData = parseUCDRangeFile(scriptsText);
  const scAliasMap = pvAliases.get("sc") || new Map();
  const scxData = parseScriptExtensions(scxText, scriptData, scAliasMap);
  const propListData = parseUCDRangeFile(propListText);
  const derivedCoreData = parseUCDRangeFile(derivedCoreText);
  const derivedBinaryData = parseUCDRangeFile(derivedBinaryText);
  const emojiData = parseUCDRangeFile(emojiText);
  const caseFoldingPairs = parseCaseFolding(caseFoldingText);
  const nonUnicodeUppercasePairs = parseRegExpNonUnicodeUppercase(unicodeDataText);

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

  const allEntries = collectAllEntries(
    gcData,
    scriptData,
    scxData,
    binaryProperties,
    caseFoldingPairs,
    nonUnicodeUppercasePairs,
    pvAliases,
    pAliases,
  );
  console.log(`  Total entries (with aliases): ${allEntries.size}`);

  console.log("Building resource...");
  const { indexedEntries, blob } = buildResourceFromEntries(allEntries);
  const resource = buildResourceContainer(
    unicodeVersion,
    indexedEntries,
    blob,
  );

  const resourceReference = path.basename(outputResourceFile);
  console.log(`Writing ${resourceReference}...`);
  generateResourceFile(resource, outputResourceFile);

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
