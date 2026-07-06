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
  verify: (checksum) => typeof checksum === "string" && checksum.indexOf(":") > 0,
});
