__gocciaRegisterProbe({
  name: "string-append-30k",
  run: (innerIterations) => {
    let s = "";
    for (let i = 0; i < innerIterations; i = i + 1) {
      s = s + "chunk" + i + ";";
    }
    let h = 0;
    for (let i = 0; i < s.length; i = i + 791) {
      h = (h + s.charCodeAt(i)) | 0;
    }
    return s.length + ":" + h;
  },
  verify: (checksum, innerIterations) => {
    const chunks = [];
    for (let i = 0; i < innerIterations; i = i + 1) {
      chunks.push("chunk", String(i), ";");
    }
    const expected = chunks.join("");
    let hash = 0;
    for (let i = 0; i < expected.length; i = i + 791) {
      hash = (hash + expected.charCodeAt(i)) | 0;
    }
    return checksum === expected.length + ":" + hash;
  },
});
