import { words } from "es-toolkit/compat/words";

const marker = "GocciaEsToolkitResult:";

try {
  const actual = words("1st 2nd+3rd--4th@1ST*2ND-3RD_4TH");
  const expected = ["1st", "2nd", "3rd", "4th", "1ST", "2ND", "3RD", "4TH"];
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
  console.log(marker + JSON.stringify({ id: "ordinal-words", status: "pass" }));
} catch (error) {
  console.log(marker + JSON.stringify({ id: "ordinal-words", status: "fail", error: String(error) }));
}
