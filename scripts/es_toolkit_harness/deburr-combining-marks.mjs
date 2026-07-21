import { deburr } from "es-toolkit/compat/deburr";

const marker = "GocciaEsToolkitResult:";

try {
  const ranges = [[0x0300, 0x036f], [0xfe20, 0xfe23]];
  ranges.forEach(([start, end]) => {
    for (let codePoint = start; codePoint <= end; codePoint++) {
      const input = `e${String.fromCharCode(codePoint)}i`;
      if (deburr(input) !== "ei") {
        throw new Error(`combining mark U+${codePoint.toString(16)} was not removed`);
      }
    }
  });
  console.log(marker + JSON.stringify({ id: "deburr-combining-marks", status: "pass" }));
} catch (error) {
  console.log(marker + JSON.stringify({
    id: "deburr-combining-marks",
    status: "fail",
    error: String(error),
  }));
}
