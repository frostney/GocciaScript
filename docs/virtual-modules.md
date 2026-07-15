# Virtual Module Configuration

Virtual modules let a host supply ECMAScript modules without writing them to
the ambient filesystem. Use them for host-provided application dependencies;
use global injection only when application code must consume an implicit
global binding.

Virtual modules use the ordinary module loader. Static and dynamic imports,
import attributes, Import Bytes, source and defer phases, relative imports,
`import.meta`, module caching, the interpreter, and the bytecode VM all follow
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
./build/GocciaScriptLoader app.mjs \
  --module='host:config=export const answer = 42;'

./build/GocciaScriptLoader app.mjs \
  --module='host:data={"type":"json","content":{"answer":42}}'
```

Inline descriptors accept JSON5 syntax. On Windows, FPC 3.2.2 removes double
quotes while parsing all command-line arguments. Use single-quoted JSON5
strings inside descriptor arguments:

```powershell
.\build\GocciaScriptLoader.exe app.mjs --module="host:data={type:'json',content:{answer:42}}"
```

The same restriction applies to inline JavaScript source. Use single-quoted
JavaScript strings inside the PowerShell argument:

```powershell
.\build\GocciaScriptLoader.exe app.mjs --module="host:message=export default 'hello';"
```

For definitions that need more complex quoting, prefer a manifest passed with
`--modules`.

Use repeatable `--modules` options for bulk manifests:

```bash
./build/GocciaScriptLoader app.mjs \
  --modules=modules.json \
  --modules=generated-modules.ts
```

Bulk manifests may be JSON, JSON5, TOML, YAML, JavaScript, or TypeScript.
JavaScript and TypeScript manifests must default-export the module map. A
manifest is evaluated before its definitions are registered, so it cannot
import modules that it defines itself.

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

The shared CLI hosts expose these options. `GocciaSandboxRunner` applies them
after installing its runtime modules, so attempts to shadow `fs` or `goccia`
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

Register one definition directly:

```pascal
Engine.InjectModule('host:database',
  'export const connect = () => "connected";');
```

Register a module map from serialized configuration:

```pascal
Engine.InjectModulesFromJSON(
  '{"host:settings":{"type":"json","content":{"version":3}}}');
```

`InjectModule` accepts optional content type and base-address arguments.
`InjectModulesFromJSON`, `InjectModulesFromJSON5`, `InjectModulesFromTOML`, and
`InjectModulesFromYAML` accept an optional base address. Strict JSON is part of
the core engine; the other formats require their matching runtime extensions.
`InjectModulesFromModule` loads a JavaScript or TypeScript default-exported map.

Programmatically registered host modules use `RegisterHostModule` or
`RegisterHostModuleProvider`. The historical `RegisterGlobalModule` names
remain supported as compatibility aliases without warnings.

See [ADR 0094](adr/0094-virtual-modules-use-the-module-loader.md) for the
architectural decision that keeps virtual modules inside the ordinary loader.
