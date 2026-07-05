__gocciaRegisterProbe({
  name: "string-builder-shaped",
  run: (innerIterations) => {
    const chunks = [];
    for (let i = 0; i < innerIterations; i = i + 1) {
      chunks.push("chunk");
      chunks.push(String(i));
      chunks.push(";");
    }
    const s = chunks.join("");
    let h = 0;
    for (let i = 0; i < s.length; i = i + 791) {
      h = (h + s.charCodeAt(i)) | 0;
    }
    return s.length + ":" + h;
  },
  verify: (checksum) => typeof checksum === "string" && checksum.indexOf(":") > 0,
});
