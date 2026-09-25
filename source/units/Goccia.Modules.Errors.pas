unit Goccia.Modules.Errors;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue;

const
  { Stable `code` for a module load attempted on an engine that was never given
    a module content provider.

    ECMA-262 sec-HostLoadImportedModule (ES2026 §16.2.1.10) requires a throw
    completion but mandates no error type, and for this exact case the major
    engines agree on a plain Error: V8 rejects with "Not supported" when no host
    import callback is set, JavaScriptCore's default loader with "Could not open
    the module", and SpiderMonkey with "Module load hook not set". TypeError is
    the convention for a *configured* loader that tried and failed (HTML, Deno),
    which is a different condition — so the constructor cannot separate the two
    and the code is what a host branches on. A future provider-backed "module
    not found" must therefore carry its own code, never this one. }
  MODULE_ERROR_CODE_LOADING_UNSUPPORTED = 'ERR_MODULE_LOADING_UNSUPPORTED';

{ Builds the plain Error reported when a module load is attempted with no
  content provider configured, carrying the stable `code` above.

  It deliberately carries no `path`, and names no module address in its message.
  The only address available at the refusal site is the *resolved* one: refusal
  happens inside the content provider, which is only ever handed the address the
  resolver produced, and the default TGocciaModuleResolver expands every
  specifier against the real host filesystem. An engine with no provider is
  exactly the configuration an embedder runs untrusted source in, so a
  structured, enumerable `path` — visible to that source through
  JSON.stringify(error) and object spread — would hand it a host layout detail
  it did not supply. The specifier as written is not reachable from here without
  widening the provider interface, and the address adds nothing diagnostically:
  with no provider installed the refusal is unconditional for every module, so
  the fix never depends on which one was requested. Contrast ADR 0092, where
  `path` carries a sandbox VFS address the guest itself named. }
function CreateModuleLoadingUnsupportedError: TGocciaObjectValue;

{ Raises that Error as a script-visible throw, so `import()` rejects with it and
  a static import surfaces it as a JavaScript error rather than letting an RTL
  exception cross the embedder's engine boundary. }
procedure ThrowModuleLoadingUnsupported;

implementation

uses
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Primitives;

function CreateModuleLoadingUnsupportedError: TGocciaObjectValue;
begin
  Result := CreateErrorObject(ERROR_NAME, SErrorModuleLoadingUnsupported);
  Result.AssignProperty(PROP_CODE, TGocciaStringLiteralValue.Create(
    MODULE_ERROR_CODE_LOADING_UNSUPPORTED));
end;

procedure ThrowModuleLoadingUnsupported;
begin
  raise TGocciaThrowValue.Create(CreateModuleLoadingUnsupportedError,
    SSuggestConfigureModuleContentProvider);
end;

end.
