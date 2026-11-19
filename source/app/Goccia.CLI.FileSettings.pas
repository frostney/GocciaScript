unit Goccia.CLI.FileSettings;

{ The engine settings one entry file runs under, resolved from the command
  line, the file's own config and the root config, with no engine involved:
  each engine applies the record to itself. The priority of every setting is
  the command line, then the per-file config, then the root config, then the
  default. }

{$I Goccia.inc}

interface

uses
  CLI.ConfigFile,
  CLI.Options,

  Goccia.CLI.Options,
  Goccia.CLI.Permissions,
  Goccia.SourcePipeline;

type
  { how the entry is compiled: as the source shows (ADR 0140), a Script or a
    Module }
  TGocciaFileSourceType = (fstDetect, fstScript, fstModule);

  TGocciaFileSettings = record
    SourceType: TGocciaFileSourceType;
    Compatibility: TGocciaCompatibilityFlags;
    LabelStatements: Boolean;
    ForInLoops: Boolean;
    ExperimentalJSModuleSource: Boolean;
    WarningUnsupportedFeatures: Boolean;
    StrictTypes: Boolean;
    { unsafe-*: the command-line flag, or the governing config's request once
      it is trusted or accepted (ADR 0122) }
    FunctionConstructor: Boolean;
    ShadowRealm: Boolean;
    { the heap limit in bytes when one is given; otherwise the engine's own
      suggested limit applies }
    MaxMemorySet: Boolean;
    MaxMemory: Int64;
    { the largest fetch response in bytes, 0 for the default }
    FetchMaxResponseBytes: Integer;
  end;

{ AFileConfig is the file's own config (empty when it has none); the root
  config has already been applied to AEngineOptions. AAcceptedUnsafe holds
  the unsafe-* requests of the governing config that are trusted or
  accepted. }
function ResolveFileSettings(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AFileName: string;
  const AAcceptedUnsafe: TGocciaUnsafeRequests): TGocciaFileSettings;

{ source-type alone, by the same rules: a per-file value other than script or
  module warns on stderr and falls back to the file-extension default (.mjs
  and .mts are modules) }
function ResolveFileSourceType(
  const AOption: TEnumOption<Goccia.CLI.Options.TGocciaSourceType>;
  const AFileConfig: TConfigEntryArray;
  const AFileName: string): TGocciaFileSourceType;

{ a per-file config value of a limit, parsed by the option itself with its
  units and bounds }
function ParseConfigValue(const AOption: TInt64Option;
  const AEntry: TConfigEntry): Int64;

implementation

uses
  SysUtils,

  Goccia.FileExtensions;

function DefaultSourceTypeForFileName(
  const AFileName: string): TGocciaFileSourceType;
begin
  if IsModuleSourceFileName(AFileName) then
    Exit(fstModule);
  Result := fstDetect;
end;

function ResolveFileSourceType(
  const AOption: TEnumOption<Goccia.CLI.Options.TGocciaSourceType>;
  const AFileConfig: TConfigEntryArray;
  const AFileName: string): TGocciaFileSourceType;
var
  ValueStr, NormalizedValue: string;
begin
  { command-line and root-config values were validated by TEnumOption.Apply;
    per-file values arrive as raw strings }
  if AOption.FromCommandLine then
  begin
    if AOption.Matches(Goccia.CLI.Options.stModule) then
      Exit(fstModule);
    Exit(fstScript);
  end;

  if FindConfigEntry(AFileConfig, 'source-type', ValueStr) then
  begin
    NormalizedValue := LowerCase(Trim(ValueStr));
    if NormalizedValue = 'module' then
      Exit(fstModule);
    if NormalizedValue = 'script' then
      Exit(fstScript);
    WriteLn(ErrOutput, Format(
      'Warning: invalid per-file config value for "source-type": %s '
      + '(valid: script, module). Falling back to file extension default.',
      [ValueStr]));
    Exit(DefaultSourceTypeForFileName(AFileName));
  end;

  if AOption.Present then
  begin
    if AOption.Matches(Goccia.CLI.Options.stModule) then
      Exit(fstModule);
    Exit(fstScript);
  end;

  Result := DefaultSourceTypeForFileName(AFileName);
end;

function ParseConfigValue(const AOption: TInt64Option;
  const AEntry: TConfigEntry): Int64;
begin
  try
    Result := AOption.Parse(AEntry.Value);
  except
    on E: EOptionValueError do
      raise TParseError.CreateFmt('Invalid value for "%s" in %s: %s (%s)',
        [AEntry.Key, AEntry.SourcePath, ConfigEntryText(AEntry), E.Reason]);
    on E: TParseError do
      raise TParseError.CreateFmt('%s: %s', [AEntry.SourcePath, E.Message]);
  end;
end;

function ResolveFileSettings(const AEngineOptions: TGocciaEngineOptions;
  const AFileConfig: TConfigEntryArray; const AFileName: string;
  const AAcceptedUnsafe: TGocciaUnsafeRequests): TGocciaFileSettings;
var
  Entry: TConfigEntry;
begin
  Result := Default(TGocciaFileSettings);
  Result.SourceType := ResolveFileSourceType(AEngineOptions.SourceType,
    AFileConfig, AFileName);

  ResolveCompatibilityFlags(AEngineOptions, AFileConfig,
    Result.Compatibility);
  Result.LabelStatements := ResolveFlagOption(
    AEngineOptions.CompatibilityFlagOption(cfLabel), AFileConfig);
  Result.ForInLoops := ResolveFlagOption(
    AEngineOptions.CompatibilityFlagOption(cfForIn), AFileConfig);
  Result.ExperimentalJSModuleSource := ResolveFlagOption(
    AEngineOptions.CompatibilityFlagOption(cfExperimentalJSModuleSource),
    AFileConfig);
  Result.WarningUnsupportedFeatures := ResolveFlagOption(
    AEngineOptions.WarningUnsupportedFeatures, AFileConfig);
  Result.StrictTypes := ResolveFlagOption(AEngineOptions.StrictTypes,
    AFileConfig);

  { The options are RequiresTrust, so a root config never sets them. }
  Result.FunctionConstructor :=
    AEngineOptions.UnsafeFunctionConstructor.Present or
    (gurFunctionConstructor in AAcceptedUnsafe);
  Result.ShadowRealm := AEngineOptions.UnsafeShadowRealm.Present or
    (gurShadowRealm in AAcceptedUnsafe);

  if AEngineOptions.MaxMemory.FromCommandLine then
  begin
    Result.MaxMemorySet := True;
    Result.MaxMemory := AEngineOptions.MaxMemory.Value;
  end
  else if (not AEngineOptions.MaxMemory.ConfigIgnored) and
    TryFindConfigEntry(AFileConfig, AEngineOptions.MaxMemory.LongName,
    Entry) then
  begin
    Result.MaxMemorySet := True;
    Result.MaxMemory := ParseConfigValue(AEngineOptions.MaxMemory, Entry);
  end
  else if AEngineOptions.MaxMemory.Present then
  begin
    Result.MaxMemorySet := True;
    Result.MaxMemory := AEngineOptions.MaxMemory.Value;
  end;

  { An engine setting, not a capability; the option's Maximum keeps the value
    within Integer. }
  if AEngineOptions.MaxFetchBytes.FromCommandLine then
    Result.FetchMaxResponseBytes :=
      Integer(AEngineOptions.MaxFetchBytes.Value)
  else if (not AEngineOptions.MaxFetchBytes.ConfigIgnored) and
    TryFindConfigEntry(AFileConfig, AEngineOptions.MaxFetchBytes.LongName,
    Entry) then
    Result.FetchMaxResponseBytes := Integer(ParseConfigValue(
      AEngineOptions.MaxFetchBytes, Entry))
  else if AEngineOptions.MaxFetchBytes.Present then
    Result.FetchMaxResponseBytes :=
      Integer(AEngineOptions.MaxFetchBytes.Value);
end;

end.
