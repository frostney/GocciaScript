#!/usr/bin/env node

const fs = require("fs");
const http = require("http");
const https = require("https");
const os = require("os");
const path = require("path");
const { execFileSync } = require("child_process");

const REPO_ROOT = path.resolve(__dirname, "..");
const DEFAULT_OUTPUT = path.join(
  REPO_ROOT,
  "source",
  "generated",
  "Generated.TimeZoneData.pas",
);
const IANA_TZDATA_LATEST_URL = "https://data.iana.org/time-zones/tzdata-latest.tar.gz";
const TIME_ZONE_SOURCE_FILES = [
  "africa",
  "antarctica",
  "asia",
  "australasia",
  "europe",
  "northamerica",
  "southamerica",
  "etcetera",
  "backward",
  "factory",
];
const SKIPPED_ROOT_DIRECTORIES = new Set(["posix", "right"]);
const SKIPPED_FILES = new Set(["localtime", "posixrules"]);
const TZIF_MAGIC = Buffer.from("TZif");
const BYTES_PER_WORD = 8;
const WORDS_PER_LINE = 4;
const ENTRIES_PER_LINE = 1;

function usage() {
  console.error("Usage: node scripts/generate-timezone-data.js [zoneinfo-dir|tzdata.tar.gz|url] [output-file]");
  process.exit(1);
}

function parseArguments() {
  if (process.argv.length > 4) {
    usage();
  }

  const source = process.argv[2] || IANA_TZDATA_LATEST_URL;
  const outputFile = process.argv[3] ? path.resolve(process.argv[3]) : DEFAULT_OUTPUT;

  return { source, outputFile };
}

function pascalString(value) {
  return `'${value.replaceAll("'", "''")}'`;
}

function isTimeZoneInformationFile(buffer) {
  return buffer.length >= TZIF_MAGIC.length && buffer.subarray(0, TZIF_MAGIC.length).equals(TZIF_MAGIC);
}

function readVersion(zoneInfoDir) {
  const sourceVersionPath = path.join(zoneInfoDir, "version");
  if (fs.existsSync(sourceVersionPath)) {
    return fs.readFileSync(sourceVersionPath, "utf8").trim();
  }

  const versionPath = path.join(zoneInfoDir, "+VERSION");
  if (fs.existsSync(versionPath)) {
    return fs.readFileSync(versionPath, "utf8").trim();
  }

  const tzdataPath = path.join(zoneInfoDir, "tzdata.zi");
  if (fs.existsSync(tzdataPath)) {
    const firstLine = fs.readFileSync(tzdataPath, "utf8").split(/\r?\n/, 1)[0];
    const match = firstLine.match(/^#\s+version\s+(.+)$/);
    if (match) {
      return match[1].trim();
    }
  }

  return "unknown";
}

function isUrl(source) {
  return /^https?:\/\//.test(source);
}

function downloadFile(url, outputFile) {
  return new Promise((resolve, reject) => {
    function get(currentUrl) {
      const protocol = new URL(currentUrl).protocol;
      const client = protocol === "http:" ? http : protocol === "https:" ? https : null;
      if (!client) {
        reject(new Error(`Unsupported URL protocol ${protocol} while downloading ${currentUrl}`));
        return;
      }

      client
        .get(currentUrl, (response) => {
          if (
            response.statusCode >= 300 &&
            response.statusCode < 400 &&
            response.headers.location
          ) {
            response.resume();
            get(new URL(response.headers.location, currentUrl).toString());
            return;
          }

          if (response.statusCode !== 200) {
            response.resume();
            reject(new Error(`HTTP ${response.statusCode} while downloading ${currentUrl}`));
            return;
          }

          const file = fs.createWriteStream(outputFile);
          response.pipe(file);
          file.on("finish", () => file.close(resolve));
          file.on("error", reject);
        })
        .on("error", reject);
    }

    get(url);
  });
}

function extractTarball(tarballPath, outputDirectory) {
  fs.mkdirSync(outputDirectory, { recursive: true });
  execFileSync("tar", ["-xzf", tarballPath, "-C", outputDirectory], { stdio: "inherit" });
}

function compileIanaSource(sourceDirectory, outputDirectory) {
  fs.mkdirSync(outputDirectory, { recursive: true });
  const sourceFiles = TIME_ZONE_SOURCE_FILES.filter((fileName) =>
    fs.existsSync(path.join(sourceDirectory, fileName)),
  );

  if (sourceFiles.length === 0) {
    throw new Error(`No IANA timezone source files found in ${sourceDirectory}`);
  }

  try {
    execFileSync("zic", ["-b", "fat", "-d", outputDirectory, ...sourceFiles], {
      cwd: sourceDirectory,
      stdio: "inherit",
    });
  } catch (error) {
    execFileSync("zic", ["-d", outputDirectory, ...sourceFiles], {
      cwd: sourceDirectory,
      stdio: "inherit",
    });
  }
}

async function prepareZoneInfoSource(source) {
  if (!isUrl(source)) {
    const absoluteSource = path.resolve(source);
    if (fs.existsSync(absoluteSource) && fs.statSync(absoluteSource).isDirectory()) {
      return {
        zoneInfoDir: absoluteSource,
        version: readVersion(absoluteSource),
        sourceDescription: absoluteSource,
        cleanup: () => {},
      };
    }
  }

  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), "goccia-tzdata-"));
  const tarballPath = path.join(temporaryDirectory, "tzdata.tar.gz");
  const sourceDirectory = path.join(temporaryDirectory, "source");
  const zoneInfoDirectory = path.join(temporaryDirectory, "zoneinfo");

  try {
    if (isUrl(source)) {
      await downloadFile(source, tarballPath);
    } else {
      fs.copyFileSync(path.resolve(source), tarballPath);
    }

    extractTarball(tarballPath, sourceDirectory);
    compileIanaSource(sourceDirectory, zoneInfoDirectory);

    return {
      zoneInfoDir: zoneInfoDirectory,
      version: readVersion(sourceDirectory),
      sourceDescription: source,
      cleanup: () => fs.rmSync(temporaryDirectory, { recursive: true, force: true }),
    };
  } catch (error) {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
    throw error;
  }
}

function shouldSkipRelativePath(relativePath, directoryEntry) {
  const pathParts = relativePath.split(path.sep);
  if (pathParts.length > 0 && SKIPPED_ROOT_DIRECTORIES.has(pathParts[0])) {
    return true;
  }

  if (!directoryEntry.isDirectory() && SKIPPED_FILES.has(pathParts[pathParts.length - 1])) {
    return true;
  }

  return false;
}

function collectTimeZoneFiles(zoneInfoDir) {
  const entries = [];

  function visit(directory) {
    const directoryEntries = fs.readdirSync(directory, { withFileTypes: true });
    for (const directoryEntry of directoryEntries) {
      const absolutePath = path.join(directory, directoryEntry.name);
      const relativePath = path.relative(zoneInfoDir, absolutePath);

      if (shouldSkipRelativePath(relativePath, directoryEntry)) {
        continue;
      }

      if (directoryEntry.isDirectory()) {
        visit(absolutePath);
        continue;
      }

      if (directoryEntry.isSymbolicLink()) {
        const targetStats = fs.statSync(absolutePath);
        if (!targetStats.isFile()) {
          continue;
        }
      } else if (!directoryEntry.isFile()) {
        continue;
      }

      const buffer = fs.readFileSync(absolutePath);
      if (!isTimeZoneInformationFile(buffer)) {
        continue;
      }

      entries.push({
        name: relativePath.split(path.sep).join("/"),
        buffer,
      });
    }
  }

  visit(zoneInfoDir);
  entries.sort((left, right) => {
    if (left.name < right.name) {
      return -1;
    }
    if (left.name > right.name) {
      return 1;
    }
    return 0;
  });
  return entries;
}

function packEntries(entries) {
  const indexedEntries = [];
  let offset = 0;
  const buffers = [];

  for (const entry of entries) {
    indexedEntries.push({
      name: entry.name,
      offset,
      length: entry.buffer.length,
    });
    buffers.push(entry.buffer);
    offset += entry.buffer.length;
  }

  return {
    indexedEntries,
    blob: Buffer.concat(buffers),
  };
}

function wordAt(buffer, offset) {
  let word = 0n;
  for (let index = 0; index < BYTES_PER_WORD; index += 1) {
    const byte = offset + index < buffer.length ? buffer[offset + index] : 0;
    word |= BigInt(byte) << BigInt(index * 8);
  }
  return word;
}

function formatWord(word) {
  const signedLimit = 1n << 63n;
  const wordLimit = 1n << 64n;
  if (word >= signedLimit) {
    return (word - wordLimit).toString();
  }

  return word.toString();
}

function formatWords(blob) {
  const words = [];
  for (let offset = 0; offset < blob.length; offset += BYTES_PER_WORD) {
    words.push(formatWord(wordAt(blob, offset)));
  }

  if (words.length === 0) {
    return "    0";
  }

  const lines = [];
  for (let index = 0; index < words.length; index += WORDS_PER_LINE) {
    lines.push(`    ${words.slice(index, index + WORDS_PER_LINE).join(", ")}`);
  }
  return lines.join(",\n");
}

function formatEntries(entries) {
  const lines = [];
  for (let index = 0; index < entries.length; index += ENTRIES_PER_LINE) {
    const chunk = entries.slice(index, index + ENTRIES_PER_LINE);
    lines.push(
      `    ${chunk
        .map((entry) => `(Name: ${pascalString(entry.name)}; Offset: ${entry.offset}; Length: ${entry.length})`)
        .join(", ")}`,
    );
  }
  return lines.join(",\n");
}

function generatePascalUnit(sourceDescription, version, entries, blob) {
  const wordCount = Math.max(1, Math.ceil(blob.length / BYTES_PER_WORD));
  const entryCount = entries.length;

  return `unit Generated.TimeZoneData;

{$I Goccia.inc}

// Generated by scripts/generate-timezone-data.js
// Source: ${sourceDescription}
// IANA tzdata version: ${version}

interface

type
  TGeneratedTimeZoneDataEntry = record
    Name: string;
    Offset: Integer;
    Length: Integer;
  end;

const
  GeneratedTimeZoneDataVersion = ${pascalString(version)};
  GeneratedTimeZoneDataWordSize = ${BYTES_PER_WORD};
  GeneratedTimeZoneDataEntryCount = ${entryCount};
  GeneratedTimeZoneDataBlobByteCount = ${blob.length};
  GeneratedTimeZoneDataWords: array[0..${wordCount - 1}] of Int64 = (
${formatWords(blob)}
  );
  GeneratedTimeZoneDataEntries: array[0..${entryCount - 1}] of TGeneratedTimeZoneDataEntry = (
${formatEntries(entries)}
  );

implementation

end.
`;
}

async function main() {
  const { source, outputFile } = parseArguments();
  const preparedSource = await prepareZoneInfoSource(source);

  try {
    const entries = collectTimeZoneFiles(preparedSource.zoneInfoDir);
    if (entries.length === 0) {
      console.error(`No TZif files found in ${preparedSource.zoneInfoDir}`);
      process.exit(1);
    }

    const { indexedEntries, blob } = packEntries(entries);
    const pascal = generatePascalUnit(
      preparedSource.sourceDescription,
      preparedSource.version,
      indexedEntries,
      blob,
    );

    fs.mkdirSync(path.dirname(outputFile), { recursive: true });
    fs.writeFileSync(outputFile, pascal, "utf8");

    console.log(
      `Generated ${path.relative(REPO_ROOT, outputFile)} with ${indexedEntries.length} zones, ${blob.length} bytes, tzdata ${preparedSource.version}`,
    );
  } finally {
    preparedSource.cleanup();
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
