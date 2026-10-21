# smol-toml bytecode regression fixture

This is the browser ESM bundle of [smol-toml 1.8.0](https://github.com/squirrelchat/smol-toml), a TOML parser and serializer distributed under [BSD-3-Clause](LICENSE). It exercises source and precompiled bytecode execution, including the BigInt constant that previously disappeared during serialization and shifted every following constant slot.

The fixture is generated from the published npm package, with no source edits. Its license headers and the complete license are retained. Tests compile it locally; no generated `.gbc` file is committed.

To reproduce `index.js` using Bun 1.4.2, extract the [published tarball](https://registry.npmjs.org/smol-toml/-/smol-toml-1.8.0.tgz) into a temporary directory, leaving its `package/` directory intact, and run there:

```sh
bun build ./package/dist/index.js --outfile=smol-toml.js --target=browser
```

Copy `smol-toml.js` to `index.js`. Relative input paths keep the generated module comments reproducible. The tarball's SHA-512 integrity and the bundle's SHA-256 are recorded in [provenance.json](provenance.json). All 19 files in the local input package were checked against that tarball.

Compile and execute this fixture with `--compat-var --compat-function --compat-traditional-for-loop --compat-while-loops --compat-for-in-loop --compat-loose-equality --compat-asi --compat-arguments-object --compat-label`. Its exports are `parse`, `stringify`, `TomlDate`, `TomlError`, and a default object containing those four values.
