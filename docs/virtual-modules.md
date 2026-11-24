# Virtual Module Configuration

Virtual modules let a host supply ECMAScript modules without writing them to
the ambient filesystem. Use them for host-provided application dependencies;
use global injection only when application code must consume an implicit
global binding.

Virtual modules use the ordinary module loader. Static and dynamic imports,
import attributes, Import Bytes, source and defer phases, relative imports,
`import.meta`, module caching, and the bytecode VM all follow
the same paths as filesystem modules where the module kind and enabled flags
support them.

## Definition Schema

A module map is an object whose keys are module addresses and whose values are
descriptors. Every descriptor has `content` and may have `type`:

```json
{
  "host:settings": { "type": "json", "content": { "version": 3 } },
  "host:database": { "content": "export const connect = () => 'ok';" },
  "host:asset": { "type": "bytes", "content": "AQID" }
}
```

| Type | Content |
|------|---------|
| `javascript` | ECMAScript module source; this is the default |
| `typescript` | Alias for `javascript` |
| `json` | Any JSON value |
| `text` | UTF-8 text exposed as the default export |
| `bytes` | Base64 decoded and exposed as an immutable byte array |

Descriptor structure and base64 are validated while configuration is applied.
JavaScript and TypeScript source is parsed and linked only when the module is
loaded.

## CLI and Config Files

Use `--module` for an inline definition:

```bash
./build/GocciaRunner app.mjs \
  --module='host:config=export const answer = 42;'

./build/GocciaRunner app.mjs \
  --module='host:data={"type":"json","content":{"answer":42}}'
```

Use repeatable `--modules` options for bulk manifests:

```bash
./build/GocciaRunner app.mjs \
  --modules=modules.json \
  --modules=generated-modules.ts
```

Bulk manifests may be JSON, JSON5, TOML, YAML, JavaScript, or TypeScript.
JavaScript and TypeScript manifests must default-export the module map. A
manifest is evaluated before its definitions are registered, so it cannot
import modules that it defines itself.

Who named the manifest decides how much it may do:

- A manifest named on the command line (`--modules`) is the user's own choice.
  It is a host file, always read from the filesystem, and it and its own
  imports are host requests the `read` capability does not check while the
  manifest is being loaded. Code it leaves behind — a global function that
  calls `import()`, say — runs later as guest code, so such an import is a
  guest read: it needs a `read` grant outside the module graph and is refused
  with `PermissionDenied` without one. Under `--deny-read` (an outright `read`
  deny) the manifest is evaluated in an isolated loader. An engine whose
  module provider does not read the host filesystem (`GocciaRunner`'s sandbox
  mode) always evaluates it in an isolated loader.
- A manifest a config file names (`"modules": "./manifest.js"`, or an array of
  paths, of any format) is the repository's choice, so it runs under the
  capability set of the script the config governs
  ([ADR 0122](adr/0122-unified-capability-model.md)). Reading it is a guest
  read, judged with the directory of the config that names it as the project:
  a manifest inside that directory is covered as the module graph is, so a
  root config's manifest also serves a subfolder with a config of its own;
  anything else needs a read grant; and a read deny refuses it with `PermissionDenied`
  and a `read.file` audit event. A JavaScript or TypeScript manifest is
  evaluated in an engine of its own with that capability set, so its imports
  are judged like the script's even while it loads, and whatever it leaves on
  its global object stays in that engine; only its default export reaches the
  script, as data.

Project config may contain a module map directly:

```json
{
  "modules": {
    "host:config": { "content": "export const answer = 42;" }
  }
}
```

The singular `module` config key accepts one inline definition or an array of
definitions. The `modules` key also accepts one manifest path or an array of
paths. Paths and relative definition addresses are resolved from the config or
manifest that declares them, including declarations inherited through
`extends`.

Configuration precedence is CLI, then per-file config, then root config.
Within one level, a later definition may replace an earlier definition until
the module is first loaded.

The shared CLI hosts expose these options. In sandbox mode `GocciaRunner`
applies them after installing its runtime modules, so attempts to shadow `fs` or `goccia`
are configuration errors.

## Resolution and Collisions

Configured names are canonical module addresses. Relative imports inside a
virtual JavaScript module resolve from that module's address. `import.meta.url`
returns the canonical address, and `import.meta.resolve()` uses the same loader
resolution seam.

Virtual definitions take precedence over import maps and filesystem modules.
When both a virtual definition and filesystem resolution match, the loader
emits a warning and uses the virtual definition. A collision with a registered
host module or `goccia:` runtime module is a configuration error.

ShadowRealm children inherit the definition set but create separate module
records, values, evaluation state, and caches.

## Embedding API

`TG2Engine` (`G2.Embed`) defines virtual modules before `Run`, after
`PrepareHost`; each call returns False with the reason in `Err`:

```pascal
var
  Err: UnicodeString;
begin
  { one definition: "address=source", or address= followed by a JSON descriptor }
  if not Engine.DefineInlineVirtualModule(
    'host:database=export const connect = () => "connected";', '', Err) then
    WriteLn(Err);
  { a module map in JSON, JSON5, TOML or YAML }
  if not Engine.DefineManifestVirtualModules('modules.json', Err) then
    WriteLn(Err);
end;
```

`DefineConfigVirtualModules` reads a config file's `modules` and `module`
keys, and `DefineModuleManifestVirtualModules` takes the module map from a
JavaScript or TypeScript module's default export. Relative addresses and paths
resolve from the declaring file's directory, or from the `Base` argument of
`DefineInlineVirtualModule`.

See [ADR 0094](adr/0094-virtual-modules-use-the-module-loader.md) for the
architectural decision that keeps virtual modules inside the ordinary loader.
