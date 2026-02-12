describe('Hex Escape Sequences', () => {
  test('\\x41 produces A', () => {
    expect("\x41").toBe('A');
  });

  test('\\x61 produces a', () => {
    expect("\x61").toBe('a');
  });

  test('\\x20 produces space', () => {
    expect("\x20").toBe(' ');
  });

  test('\\x00 produces null character', () => {
    expect("\x00".length).toBe(1);
  });

  test('hex escape in template literal', () => {
    expect(`\x41\x42\x43`).toBe('ABC');
  });

  test('mixed hex and regular characters', () => {
    expect("Hello\x21").toBe('Hello!');
  });
});
