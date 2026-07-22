import { unzipWith } from "es-toolkit/compat/unzipWith";

const marker = "GocciaEsToolkitResult:";

try {
  const actual = unzipWith([]);
  if (!Array.isArray(actual) || actual.length !== 0) {
    throw new Error(`expected an empty array, got ${JSON.stringify(actual)}`);
  }
  console.log(marker + JSON.stringify({ id: "empty-unzip-with", status: "pass" }));
} catch (error) {
  console.log(marker + JSON.stringify({ id: "empty-unzip-with", status: "fail", error: String(error) }));
}
