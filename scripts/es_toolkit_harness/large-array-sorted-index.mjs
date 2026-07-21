import { sortedIndexBy } from "es-toolkit/compat/sortedIndexBy";
import { sortedLastIndexBy } from "es-toolkit/compat/sortedLastIndexBy";

const marker = "GocciaEsToolkitResult:";

try {
  const maximumArrayLength = 4294967295;
  const maximumArrayIndex = maximumArrayLength - 1;
  const lengths = [Math.ceil(maximumArrayLength / 2), maximumArrayLength];

  lengths.forEach((length) => {
    const array = [];
    array.length = length;

    [maximumArrayLength, NaN, undefined].forEach((value) => {
      const first = sortedIndexBy(array, value, (entry) => entry);
      const last = sortedLastIndexBy(array, value, (entry) => entry);
      const expectedFirst = !Number.isNaN(value) ? 0 : Math.min(length, maximumArrayIndex);
      const expectedLast = Number.isFinite(value) ? 0 : Math.min(length, maximumArrayIndex);

      if (first !== expectedFirst || last !== expectedLast) {
        throw new Error(
          `length ${length}: expected [${expectedFirst}, ${expectedLast}], got [${first}, ${last}]`
        );
      }
    });
  });

  console.log(marker + JSON.stringify({ id: "large-array-sorted-index", status: "pass" }));
} catch (error) {
  console.log(marker + JSON.stringify({
    id: "large-array-sorted-index",
    status: "fail",
    error: String(error),
  }));
}
