// test262 assertRelativeDateMs -- GocciaScript-compatible reimplementation

function assertRelativeDateMs(date, expectedMs) {
  const actualMs = date.valueOf();
  const localOffset = date.getTimezoneOffset() * 60000;

  assert.sameValue(
    actualMs - localOffset,
    expectedMs,
    "expected " + date + " to be " + expectedMs +
      " milliseconds from the Unix epoch"
  );
}
