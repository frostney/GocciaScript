__gocciaRegisterProbe({
  name: "regexp-corpus-build",
  run: (innerIterations) => {
    let corpus = "";
    for (let i = 0; i < innerIterations; i = i + 1) {
      corpus = corpus + "user" + i + "@example" + (i % 7) + ".com text " + i + "\n";
    }
    return corpus.length + ":" + corpus.charCodeAt(corpus.length - 2);
  },
  verify: (checksum) => typeof checksum === "string" && checksum.indexOf(":") > 0,
});
