#!/usr/bin/env node

const fs = require("fs");
const os = require("os");
const path = require("path");
const { execFileSync } = require("child_process");
const {
  buildResourceContainer,
  downloadFileToDisk,
  generateResourceFile,
  isUrl,
  pascalUnitNameForOutput,
  resourceFileForOutput,
} = require("./lib/resource-container");

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
  const outputResourceFile = resourceFileForOutput(outputFile, "Timezone data");
  const unitName = pascalUnitNameForOutput(outputFile, "Timezone data");

  if (path.resolve(outputFile) === path.resolve(outputResourceFile)) {
    throw new Error(`Refusing to use ${outputFile} for both Pascal and resource output`);
  }

  return { source, outputFile, outputResourceFile, unitName };
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
      await downloadFileToDisk(source, tarballPath);
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
        let targetStats;
        try {
          targetStats = fs.statSync(absolutePath);
        } catch (error) {
          if (error && error.code === "ENOENT") {
            console.warn(`Warning: skipping broken timezone symlink ${absolutePath}: ${error}`);
            continue;
          }
          throw error;
        }

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

function generatePascalUnit(sourceDescription, version, entryCount, blobByteCount, unitName, resourceReference) {
  return `unit ${unitName};

{$I Goccia.inc}

// Generated by scripts/generate-timezone-data.js
// Source: ${sourceDescription}
// IANA tzdata version: ${version}
// Resource: ${resourceReference}

interface

const
  GeneratedTimeZoneDataVersion = '${version.replaceAll("'", "''")}';
  GeneratedTimeZoneDataResourceName = '${RESOURCE_NAME}';
  GeneratedTimeZoneDataEntryCount = ${entryCount};
  GeneratedTimeZoneDataBlobByteCount = ${blobByteCount};

implementation

{$IFDEF GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
{$R ${resourceReference}}
{$ENDIF}

end.
`;
}

async function main() {
  const { source, outputFile, outputResourceFile, unitName } = parseArguments();
  const preparedSource = await prepareZoneInfoSource(source);

  try {
    const entries = collectTimeZoneFiles(preparedSource.zoneInfoDir);
    if (entries.length === 0) {
      console.error(`No TZif files found in ${preparedSource.zoneInfoDir}`);
      process.exit(1);
    }

    const { indexedEntries, blob } = packEntries(entries);
    const resource = buildResourceContainer(
      RESOURCE_MAGIC,
      preparedSource.version,
      indexedEntries,
      blob,
    );
    const pascal = generatePascalUnit(
      preparedSource.sourceDescription,
      preparedSource.version,
      indexedEntries.length,
      blob.length,
      unitName,
      path.basename(outputResourceFile),
    );

    fs.mkdirSync(path.dirname(outputFile), { recursive: true });
    fs.writeFileSync(outputFile, pascal, "utf8");
    generateResourceFile(resource, outputResourceFile, RESOURCE_NAME, {
      temporaryDirectoryPrefix: "goccia-tzresource-",
      dataFileName: "timezone-data.bin",
      scriptFileName: "timezone-data.rc",
    });

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
