const marker = "GocciaEsToolkitEnvironment:";

let functionConstructor = { status: "enabled" };
try {
  Function("return 1")();
} catch (error) {
  functionConstructor = { status: "disabled", error: String(error) };
}

console.log(marker + JSON.stringify({
  functionConstructor,
  hostGlobals: {
    Buffer: typeof Buffer,
    Blob: typeof Blob,
    process: typeof process,
  },
}));
