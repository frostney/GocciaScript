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
const RESOURCE_NAME = "GOCCIA_TZDATA";
const RESOURCE_MAGIC = Buffer.from("GOCCIATZ", "ascii");
const RESOURCE_FORMAT_VERSION = 1;
const RESOURCE_HEADER_SIZE = RESOURCE_MAGIC.length + 6 * 4;
const RESOURCE_ENTRY_SIZE = 4 * 4;

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

function resourceFileForOutput(outputFile) {
  return outputFile.replace(/\.pas$/i, ".res");
}

function generateResourceFile(resourceBytes, outputResourceFile) {
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), "goccia-tzresource-"));
  const resourceDataFile = path.join(temporaryDirectory, "timezone-data.bin");
  const resourceScriptFile = path.join(temporaryDirectory, "timezone-data.rc");

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

function generatePascalUnit(sourceDescription, version, entryCount, blobByteCount) {
  return `unit Generated.TimeZoneData;

{$I Goccia.inc}

// Generated by scripts/generate-timezone-data.js
// Source: ${sourceDescription}
// IANA tzdata version: ${version}
// Resource: Generated.TimeZoneData.res

interface

const
  GeneratedTimeZoneDataVersion = '${version.replaceAll("'", "''")}';
  GeneratedTimeZoneDataResourceName = '${RESOURCE_NAME}';
  GeneratedTimeZoneDataEntryCount = ${entryCount};
  GeneratedTimeZoneDataBlobByteCount = ${blobByteCount};

implementation

{$IFDEF GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
{$R Generated.TimeZoneData.res}
{$ENDIF}

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
    const resource = buildResourceContainer(
      preparedSource.version,
      indexedEntries,
      blob,
    );
    const pascal = generatePascalUnit(
      preparedSource.sourceDescription,
      preparedSource.version,
      indexedEntries.length,
      blob.length,
    );
    const outputResourceFile = resourceFileForOutput(outputFile);

    fs.mkdirSync(path.dirname(outputFile), { recursive: true });
    fs.writeFileSync(outputFile, pascal, "utf8");
    generateResourceFile(resource, outputResourceFile);

    console.log(
      `Generated ${path.relative(REPO_ROOT, outputFile)} and ${path.relative(REPO_ROOT, outputResourceFile)} with ${indexedEntries.length} zones, ${blob.length} bytes, tzdata ${preparedSource.version}`,
    );
  } finally {
    preparedSource.cleanup();
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
