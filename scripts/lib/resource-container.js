const fs = require("fs");
const http = require("http");
const https = require("https");
const os = require("os");
const path = require("path");
const { execFileSync } = require("child_process");

const RESOURCE_FORMAT_VERSION = 1;
const RESOURCE_HEADER_FIELD_SIZE = 4;
const RESOURCE_HEADER_FIELD_COUNT = 6;
const RESOURCE_ENTRY_SIZE = 4 * RESOURCE_HEADER_FIELD_SIZE;
const DEFAULT_MAX_REDIRECTS = 5;
const DEFAULT_DOWNLOAD_TIMEOUT_MS = 30000;
const PASCAL_UNIT_IDENTIFIER_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;

function isUrl(source) {
  return /^https?:\/\//.test(source);
}

function getClient(url) {
  const protocol = new URL(url).protocol;
  if (protocol === "http:") {
    return http;
  }
  if (protocol === "https:") {
    return https;
  }
  throw new Error(`Unsupported URL protocol ${protocol} while downloading ${url}`);
}

function fetchUrl(url, handler, options) {
  const timeoutMs = options && options.timeoutMs !== undefined
    ? options.timeoutMs
    : DEFAULT_DOWNLOAD_TIMEOUT_MS;
  const maxRedirects = options && options.maxRedirects !== undefined
    ? options.maxRedirects
    : DEFAULT_MAX_REDIRECTS;

  return new Promise((resolve, reject) => {
    function get(currentUrl, redirectsLeft) {
      if (redirectsLeft < 0) {
        reject(new Error(`Too many redirects while downloading ${currentUrl}`));
        return;
      }

      let settled = false;
      let response = null;
      let request = null;

      function cleanup() {
        if (request) {
          request.setTimeout(0);
          request.removeListener("timeout", onTimeout);
          request.removeListener("error", onRequestError);
        }
        if (response) {
          response.removeListener("error", onResponseError);
        }
      }

      function fail(error) {
        if (settled) {
          return;
        }
        settled = true;
        cleanup();
        reject(error);
      }

      function finish(value) {
        if (settled) {
          return;
        }
        settled = true;
        cleanup();
        resolve(value);
      }

      function onTimeout() {
        request.destroy();
        fail(new Error(`Timeout downloading ${currentUrl}`));
      }

      function onRequestError(error) {
        fail(error);
      }

      function onResponseError(error) {
        fail(error);
      }

      try {
        request = getClient(currentUrl)
          .get(currentUrl, (downloadResponse) => {
            response = downloadResponse;
            response.on("error", onResponseError);

            if (
              response.statusCode >= 300 &&
              response.statusCode < 400 &&
              response.headers.location
            ) {
              cleanup();
              response.resume();
              get(new URL(response.headers.location, currentUrl).toString(), redirectsLeft - 1);
              return;
            }

            if (response.statusCode !== 200) {
              cleanup();
              response.resume();
              fail(new Error(`HTTP ${response.statusCode} while downloading ${currentUrl}`));
              return;
            }

            Promise.resolve(handler(response, currentUrl, request))
              .then(finish)
              .catch(fail);
          })
          .on("error", onRequestError);

        request.setTimeout(timeoutMs, onTimeout);
      } catch (error) {
        fail(error);
      }
    }

    get(url, maxRedirects);
  });
}

function downloadText(url, options) {
  return fetchUrl(
    url,
    (response) => new Promise((resolve, reject) => {
      const chunks = [];
      response.on("data", (chunk) => chunks.push(chunk));
      response.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
      response.on("error", reject);
    }),
    options,
  );
}

function downloadFileToDisk(url, outputFile, options) {
  return fetchUrl(
    url,
    (response, currentUrl, request) => new Promise((resolve, reject) => {
      const file = fs.createWriteStream(outputFile);

      function cleanup() {
        file.removeListener("finish", onFinish);
        file.removeListener("error", onFileError);
      }

      function onFinish() {
        cleanup();
        file.close((error) => {
          if (error) {
            reject(error);
            return;
          }
          resolve();
        });
      }

      function onFileError(error) {
        cleanup();
        request.destroy();
        reject(error);
      }

      file.on("finish", onFinish);
      file.on("error", onFileError);
      response.pipe(file);
    }),
    options,
  );
}

function pascalUnitNameForOutput(outputFile, outputDescription) {
  if (path.extname(outputFile).toLowerCase() !== ".pas") {
    throw new Error(`${outputDescription} output must be a .pas file: ${outputFile}`);
  }

  const unitName = path.basename(outputFile, path.extname(outputFile));
  if (unitName.length === 0) {
    throw new Error(
      `${outputDescription} output must have a non-empty Pascal unit name: ${outputFile}`,
    );
  }

  const unitNameParts = unitName.split(".");
  if (
    unitNameParts.length === 0 ||
    unitNameParts.some((unitNamePart) => !PASCAL_UNIT_IDENTIFIER_PATTERN.test(unitNamePart))
  ) {
    throw new Error(
      `${outputDescription} output basename is not a valid Pascal unit name: ${unitName}`,
    );
  }

  return unitName;
}

function resourceFileForOutput(outputFile, outputDescription) {
  if (path.extname(outputFile).toLowerCase() !== ".pas") {
    throw new Error(`${outputDescription} output must be a .pas file: ${outputFile}`);
  }

  return path.join(
    path.dirname(outputFile),
    `${path.basename(outputFile, path.extname(outputFile))}.res`,
  );
}

function writeUInt32LE(buffer, value, offset) {
  if (value < 0 || value > 0xffffffff) {
    throw new Error(`Resource integer ${value} is outside UInt32 range`);
  }

  buffer.writeUInt32LE(value, offset);
}

function buildResourceContainer(resourceMagic, version, entries, blob) {
  if (!Buffer.isBuffer(resourceMagic) || resourceMagic.length !== 8) {
    throw new Error("Resource magic must be an 8-byte Buffer");
  }

  if (RESOURCE_ENTRY_SIZE !== 16) {
    throw new Error(`Resource entry size must be 16 bytes, got ${RESOURCE_ENTRY_SIZE}`);
  }

  if (!Buffer.isBuffer(blob)) {
    throw new Error("Resource blob must be a Buffer");
  }

  let previousName = null;
  for (const entry of entries) {
    if (typeof entry.name !== "string" || entry.name.length === 0) {
      throw new Error("Resource entry name must be a non-empty string");
    }

    if (previousName !== null && entry.name < previousName) {
      throw new Error(
        `Resource entries must be sorted by name: ${entry.name} appears after ${previousName}`,
      );
    }

    if (
      !Number.isInteger(entry.offset) ||
      !Number.isInteger(entry.length) ||
      entry.offset < 0 ||
      entry.length < 0 ||
      entry.offset > blob.length - entry.length
    ) {
      throw new Error(`Invalid resource entry bounds for ${entry.name}`);
    }

    previousName = entry.name;
  }

  const versionBuffer = Buffer.from(version, "utf8");
  const nameBuffers = entries.map((entry) => Buffer.from(entry.name, "utf8"));
  const namesByteCount = nameBuffers.reduce((total, nameBuffer) => total + nameBuffer.length, 0);
  const entryTableByteCount = entries.length * RESOURCE_ENTRY_SIZE;
  const headerSize = resourceMagic.length + RESOURCE_HEADER_FIELD_COUNT * RESOURCE_HEADER_FIELD_SIZE;
  const totalByteCount =
    headerSize +
    versionBuffer.length +
    entryTableByteCount +
    namesByteCount +
    blob.length;

  const resource = Buffer.alloc(totalByteCount);
  let offset = 0;

  resourceMagic.copy(resource, offset);
  offset += resourceMagic.length;
  writeUInt32LE(resource, RESOURCE_FORMAT_VERSION, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
  writeUInt32LE(resource, versionBuffer.length, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
  writeUInt32LE(resource, entries.length, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
  writeUInt32LE(resource, namesByteCount, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
  writeUInt32LE(resource, blob.length, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
  writeUInt32LE(resource, 0, offset); offset += RESOURCE_HEADER_FIELD_SIZE;

  versionBuffer.copy(resource, offset);
  offset += versionBuffer.length;

  let nameOffset = 0;
  const namesOffset = headerSize + versionBuffer.length + entryTableByteCount;
  for (let index = 0; index < entries.length; index += 1) {
    const entry = entries[index];
    const nameBuffer = nameBuffers[index];
    writeUInt32LE(resource, nameOffset, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
    writeUInt32LE(resource, nameBuffer.length, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
    writeUInt32LE(resource, entry.offset, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
    writeUInt32LE(resource, entry.length, offset); offset += RESOURCE_HEADER_FIELD_SIZE;
    nameBuffer.copy(resource, namesOffset + nameOffset);
    nameOffset += nameBuffer.length;
  }

  blob.copy(resource, namesOffset + namesByteCount);
  return resource;
}

function generateResourceFile(resourceBytes, outputResourceFile, resourceName, options) {
  const temporaryDirectoryPrefix = options && options.temporaryDirectoryPrefix
    ? options.temporaryDirectoryPrefix
    : "goccia-resource-";
  const dataFileName = options && options.dataFileName ? options.dataFileName : "resource.bin";
  const scriptFileName = options && options.scriptFileName ? options.scriptFileName : "resource.rc";
  const temporaryDirectory = fs.mkdtempSync(path.join(os.tmpdir(), temporaryDirectoryPrefix));
  const resourceDataFile = path.join(temporaryDirectory, dataFileName);
  const resourceScriptFile = path.join(temporaryDirectory, scriptFileName);

  try {
    fs.writeFileSync(resourceDataFile, resourceBytes);
    fs.writeFileSync(
      resourceScriptFile,
      `${resourceName} RCDATA "${resourceDataFile.replaceAll("\\", "\\\\")}"\n`,
      "utf8",
    );
    execFileSync("fpcres", ["-of", "res", resourceScriptFile, "-o", outputResourceFile], {
      stdio: "inherit",
    });
  } finally {
    fs.rmSync(temporaryDirectory, { recursive: true, force: true });
  }
}

module.exports = {
  DEFAULT_DOWNLOAD_TIMEOUT_MS,
  DEFAULT_MAX_REDIRECTS,
  buildResourceContainer,
  downloadFileToDisk,
  downloadText,
  generateResourceFile,
  isUrl,
  pascalUnitNameForOutput,
  resourceFileForOutput,
};
