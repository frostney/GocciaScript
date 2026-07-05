__gocciaRegisterProbe({
  name: "regexp-exec-replace",
  run: (innerIterations) => {
    let corpus = "";
    for (let i = 0; i < innerIterations; i = i + 1) {
      corpus = corpus + "user" + i + "@example" + (i % 7) + ".com text " + i + "\n";
    }
    const re = /([a-z]+)(\d+)@example(\d)\.com/g;
    let count = 0;
    re.lastIndex = 0;
    let match = re.exec(corpus);
    while (match !== null) {
      count = count + 1;
      match = re.exec(corpus);
    }
    const replaced = corpus.replace(/text/g, "T");
    return count + ":" + replaced.length;
  },
  verify: (checksum) => typeof checksum === "string" && checksum.indexOf(":") > 0,
});
