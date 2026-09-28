// A data module that fails to load rejects with a message naming the
// specifier as the import wrote it. The expanded host path stays host-side
// (ADR 0108), so nothing else in the message may look like a filesystem path.

const importFailureMessage = async (specifier) => {
  try {
    await import(specifier);
  } catch (e) {
    return e.message;
  }
  return undefined;
};

// Every fixture keeps path separators out of its own parse-error text, so any
// separator left once the quoted specifier is removed came from a host path.
const expectSpecifierOnly = (message, prefix, specifier) => {
  expect(message.indexOf(`${prefix} "${specifier}"`)).toBe(0);
  const detail = message.replace(`"${specifier}"`, "");
  expect(detail.includes("/")).toBe(false);
  expect(detail.includes("\\")).toBe(false);
};

describe("data module load failures", () => {
  test("TOML parse failure names the specifier, not the host path", async () => {
    const specifier = "../../../fixtures/modules/malformed-toml-module.toml";
    expectSpecifierOnly(await importFailureMessage(specifier),
      "Failed to parse TOML module", specifier);
  });

  test("YAML parse failure names the specifier, not the host path", async () => {
    const specifier = "../../../fixtures/modules/malformed-yaml-module.yaml";
    expectSpecifierOnly(await importFailureMessage(specifier),
      "Failed to parse YAML module", specifier);
  });

  test("YAML module without a document names the specifier, not the host path", async () => {
    const specifier = "../../../fixtures/modules/empty-yaml-module.yaml";
    expectSpecifierOnly(await importFailureMessage(specifier),
      "YAML module", specifier);
  });

  test("JSON5 parse failure names the specifier, not the host path", async () => {
    const specifier = "../../../fixtures/modules/malformed-json5-module.json5";
    expectSpecifierOnly(await importFailureMessage(specifier),
      "Failed to parse JSON5 module", specifier);
  });

  test("CSV parse failure names the specifier, not the host path", async () => {
    const specifier = "../../../fixtures/modules/malformed-csv-module.csv";
    expectSpecifierOnly(await importFailureMessage(specifier),
      "Failed to parse CSV module", specifier);
  });

  test("JSONL parse failure names the specifier, not the host path", async () => {
    const specifier = "../../../fixtures/modules/malformed-jsonl-module.jsonl";
    expectSpecifierOnly(await importFailureMessage(specifier),
      "Failed to parse JSONL module", specifier);
  });
});
