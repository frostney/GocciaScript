unit Goccia.CLI.SandboxMode;

{ GocciaRunner's sandbox mode as policy: whether a run is in sandbox mode and
  why, which host inputs it copies and where, which entry it runs, and how it
  reports the diff (ADR 0122). Pure: nothing here touches a virtual
  filesystem or an engine, so the whole decision is unit-testable.
  Goccia.CLI.SandboxHost carries the decision out.

  Sandbox mode turns on with --sandbox, --copy, --copy-rw, or a trusted
  `sandbox` section in the root config. Command-line inputs are added to the
  config's; one with the same sandbox path as a config input replaces it, so
  `--copy out` turns a config's `copy-rw: ["out"]` into a read-only dry run. }

{$I Goccia.inc}

interface

uses
  Classes,

  CLI.Options,
  SandboxHostInputs,

  Goccia.CLI.Options,
  Goccia.CLI.Permissions;

type
  TGocciaSandboxDiffFormat = (sdfJson, sdfUnified);

  { One host input: a --copy or --copy-rw argument, or a `copy` / `copy-rw`
    entry of the config's sandbox section. }
  TGocciaSandboxInput = record
    { As written, for messages. }
    Spec: string;
    { Where it was given: `--copy`, `--copy-rw`, or the declaring config. }
    Origin: string;
    { Absolute host path. }
    HostPath: string;
    { Absolute sandbox path it is copied to. }
    SandboxPath: string;
    ReadWrite: Boolean;
    FromConfig: Boolean;
    { How messages name the input: `--copy src`, or `<config>:
      "sandbox.copy" entry "src"`. }
    Description: string;
    { For a config-named read-write input, where write-back may reach it
      (TSandboxHostPin), pinned when the config is checked. Unset for the
      command line's inputs, which are the user's own choice. }
    Pin: TSandboxHostPin;
  end;
  TGocciaSandboxInputs = array of TGocciaSandboxInput;

  TGocciaSandboxModeRequest = record
    Active: Boolean;
    { What switched sandbox mode on, for messages: `--sandbox`, `--copy`,
      `--copy-rw`, or `the "sandbox" section of <config>`. }
    Reason: string;
    Inputs: TGocciaSandboxInputs;
    { The host file to copy in and run; '' when EntrySandbox names the
      entry. }
    EntryHost: string;
    { A path inside the sandbox to run (--entry, or the config's `entry`). }
    EntrySandbox: string;
    DiffRequested: Boolean;
    DiffFormat: TGocciaSandboxDiffFormat;
    { Absolute host path the diff is written to; '' prints it. }
    DiffFile: string;
    { Set when the config named the diff file: the write goes through it.
      A command-line --diff-file is written as the user named it. }
    DiffFilePin: TSandboxHostPin;
    MaxFsBytes: Int64;
    MaxFsNodes: Integer;
    { Lines for stderr about how the request was read, such as a config
      entry the command line overrode. }
    Notes: array of string;
  end;

  { The command-line facts ResolveSandboxMode needs beyond the sandbox
    options. }
  TGocciaSandboxCommandLine = record
    { Positional arguments. }
    Paths: TStrings;
    { `--allow-<cap>` flags given for capabilities other than net. }
    DeniedAllowFlags: array of string;
    { Host-mode-only options given on the command line (`--output`, ...). }
    HostOnlyOptions: array of string;
    WorkingDirectory: string;
  end;

const
  { The format each --diff-file extension implies (ADR 0122, R1). Any other
    extension needs an explicit --diff=<format>. }
  DIFF_FILE_EXTENSION_JSON = '.json';
  DIFF_FILE_EXTENSION_UNIFIED = '.diff';

{ Splits `<host>[=<sandbox>]` and resolves it: the host part against
  ABaseDirectory, the sandbox part as an absolute sandbox path. Without a
  sandbox part the input lands at `/<basename>`. Raises TCLIUsageError for an
  empty host part, or for a host path with no name to default to (`/`). }
function ParseCopySpec(const ASpec, ABaseDirectory, AOrigin: string;
  const AReadWrite: Boolean): TGocciaSandboxInput;

{ The sandbox path a host path lands at by default: `/<basename>`. Raises
  TCLIUsageError when the host path has no name (a filesystem root). }
function DefaultSandboxPathFor(const AHostPath, ASpec, AOrigin: string): string;

{ The diff format a --diff-file name implies: `.json` is json and `.diff` is
  unified. False for any other extension. }
function TryInferDiffFormat(const AFileName: string;
  out AFormat: TGocciaSandboxDiffFormat): Boolean;

{ `json` or `unified`. False for anything else. }
function TryParseDiffFormat(const AValue: string;
  out AFormat: TGocciaSandboxDiffFormat): Boolean;

{ AConfigInputs followed by ACommandLineInputs, where a command-line input
  replaces the config input with the same sandbox path. }
function MergeSandboxInputs(const AConfigInputs,
  ACommandLineInputs: TGocciaSandboxInputs): TGocciaSandboxInputs;

{ Decides the run's mode and, in sandbox mode, everything the host needs.
  AConfig is the root config's sandbox request; it counts only when
  AConfigAccepted (trusted, or accepted with -P). Raises TCLIUsageError for
  an unusable combination (sandbox-only options without sandbox mode, host
  options or host capabilities in sandbox mode, no entry or several) and
  TParseError for an invalid value (a --diff format, a zero limit). }
function ResolveSandboxMode(const AOptions: TGocciaSandboxOptions;
  const AConfig: TGocciaSandboxRequest; const AConfigAccepted: Boolean;
  const ACommandLine: TGocciaSandboxCommandLine): TGocciaSandboxModeRequest;

{ Raises TCLIUsageError naming the first of AFlags (`--allow-read`, ...):
  sandbox mode, switched on by AReason, has no host filesystem to grant. }
procedure RejectHostCapabilityFlags(const AFlags: array of string;
  const AReason: string);

{ Raises TCLIUsageError naming the first of AOptions (`--output`, ...),
  which only apply in host mode; sandbox mode was switched on by AReason. }
procedure RejectHostModeOptions(const AOptions: array of string;
  const AReason: string);

{ The help note that follows the options list. }
function SandboxModeHelpNote: string;

implementation

uses
  SysUtils,

  FileUtils,

  Goccia.CLI.Application,
  Goccia.FileExtensions,
  Goccia.Sandbox.Context,
  Goccia.ScriptLoader.Input;

const
  SANDBOX_ROOT = '/';
  COPY_FLAG = '--copy';
  COPY_READ_WRITE_FLAG = '--copy-rw';
  ENABLE_SANDBOX_HINT =
    'enable sandbox mode with --sandbox or --copy <host>[=<sandbox>]';
  NO_HOST_FILESYSTEM_HINT =
    'the sandbox has no host filesystem; copy inputs with --copy';

{ A sandbox path as given for a copy target or an entry: absolute, and not
  climbing above the sandbox root (the virtual filesystem would clamp it
  silently). Backslashes are separators. AWhere names the value in an error,
  such as `--entry` or `--copy src=x`. }
function CheckedSandboxPath(const APath, AWhere: string): string;
var
  Depth, Start, I: Integer;
  Segment: string;
begin
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  if Result = '' then
    raise TCLIUsageError.CreateFmt('%s: the sandbox path is empty; give an ' +
      'absolute sandbox path such as /name', [AWhere]);
  if Result[1] <> '/' then
    raise TCLIUsageError.CreateFmt('%s: "%s" is not an absolute sandbox ' +
      'path; start it with /', [AWhere, APath]);
  Depth := 0;
  Start := 2;
  for I := 2 to Length(Result) + 1 do
    if (I > Length(Result)) or (Result[I] = '/') then
    begin
      Segment := Copy(Result, Start, I - Start);
      if Segment = '..' then
      begin
        if Depth = 0 then
          raise TCLIUsageError.CreateFmt('%s: "%s" climbs above the sandbox ' +
            'root', [AWhere, APath]);
        Dec(Depth);
      end
      else if (Segment <> '') and (Segment <> '.') then
        Inc(Depth);
      Start := I + 1;
    end;
end;

function HostAbsolutePath(const APath, ABaseDirectory: string): string;
begin
  if IsAbsoluteHostPath(APath) then
    Result := ExpandHostFileName(APath)
  else
    Result := ExpandHostFileName(IncludeTrailingPathDelimiter(ABaseDirectory) +
      APath);
end;

function DefaultSandboxPathFor(const AHostPath, ASpec, AOrigin: string): string;
var
  Name: string;
begin
  Name := ExtractFileName(ExcludeTrailingPathDelimiter(AHostPath));
  if (Name = '') or (Name = '.') or (Name = '..') then
    raise TCLIUsageError.CreateFmt(
      '%s %s: the host path has no name to use as its sandbox path; give ' +
      'one with %s=<sandbox>', [AOrigin, ASpec, ASpec]);
  Result := SANDBOX_ROOT + Name;
end;

function ParseCopySpec(const ASpec, ABaseDirectory, AOrigin: string;
  const AReadWrite: Boolean): TGocciaSandboxInput;
var
  Separator: Integer;
  HostPart, SandboxPart: string;
begin
  Separator := Pos('=', ASpec);
  if Separator > 0 then
  begin
    HostPart := Copy(ASpec, 1, Separator - 1);
    SandboxPart := Copy(ASpec, Separator + 1, MaxInt);
  end
  else
  begin
    HostPart := ASpec;
    SandboxPart := '';
  end;
  if Trim(HostPart) = '' then
    raise TCLIUsageError.CreateFmt(
      '%s needs a host path: %s <host>[=<sandbox>]', [AOrigin, AOrigin]);

  Result.Spec := ASpec;
  Result.Origin := AOrigin;
  Result.HostPath := ExcludeTrailingPathDelimiter(HostAbsolutePath(HostPart,
    ABaseDirectory));
  if Result.HostPath = '' then
    Result.HostPath := PathDelim;
  if Separator > 0 then
    Result.SandboxPath := CheckedSandboxPath(SandboxPart, AOrigin + ' ' + ASpec)
  else
    Result.SandboxPath := DefaultSandboxPathFor(Result.HostPath, ASpec,
      AOrigin);
  Result.ReadWrite := AReadWrite;
  Result.FromConfig := False;
  Result.Description := AOrigin + ' ' + ASpec;
  Result.Pin := Default(TSandboxHostPin);
end;

function TryInferDiffFormat(const AFileName: string;
  out AFormat: TGocciaSandboxDiffFormat): Boolean;
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(AFileName));
  Result := True;
  AFormat := sdfJson;
  if Extension = DIFF_FILE_EXTENSION_JSON then
    AFormat := sdfJson
  else if Extension = DIFF_FILE_EXTENSION_UNIFIED then
    AFormat := sdfUnified
  else
    Result := False;
end;

function TryParseDiffFormat(const AValue: string;
  out AFormat: TGocciaSandboxDiffFormat): Boolean;
begin
  Result := True;
  AFormat := sdfJson;
  if AValue = 'json' then
    AFormat := sdfJson
  else if AValue = 'unified' then
    AFormat := sdfUnified
  else
    Result := False;
end;

{ Sandbox paths compare after collapsing `.` and `..` and trailing
  separators, so `/out/` and `/out` name one target. }
function SandboxTargetKey(const APath: string): string;
var
  Parts, Kept: TStringList;
  I: Integer;
begin
  Parts := TStringList.Create;
  Kept := TStringList.Create;
  try
    Parts.StrictDelimiter := True;
    Parts.Delimiter := '/';
    Parts.DelimitedText := APath;
    for I := 0 to Parts.Count - 1 do
      if Parts[I] = '..' then
      begin
        if Kept.Count > 0 then
          Kept.Delete(Kept.Count - 1);
      end
      else if (Parts[I] <> '') and (Parts[I] <> '.') then
        Kept.Add(Parts[I]);
    Result := '';
    for I := 0 to Kept.Count - 1 do
      Result := Result + SANDBOX_ROOT + Kept[I];
    if Result = '' then
      Result := SANDBOX_ROOT;
  finally
    Kept.Free;
    Parts.Free;
  end;
end;

function MergeSandboxInputs(const AConfigInputs,
  ACommandLineInputs: TGocciaSandboxInputs): TGocciaSandboxInputs;
var
  I, J, Count: Integer;
  Replaced: Boolean;
begin
  Result := nil;
  SetLength(Result, Length(AConfigInputs) + Length(ACommandLineInputs));
  Count := 0;
  for I := 0 to High(AConfigInputs) do
  begin
    Replaced := False;
    for J := 0 to High(ACommandLineInputs) do
      if SandboxTargetKey(ACommandLineInputs[J].SandboxPath) =
         SandboxTargetKey(AConfigInputs[I].SandboxPath) then
      begin
        Replaced := True;
        Break;
      end;
    if Replaced then
      Continue;
    Result[Count] := AConfigInputs[I];
    Inc(Count);
  end;
  for I := 0 to High(ACommandLineInputs) do
  begin
    Result[Count] := ACommandLineInputs[I];
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

{ Maintainer decision B on #1255: a path a config writes to stays inside the
  config's own directory tree, compared canonically so a symbolic link cannot
  lead out of it. The command line can write anywhere. }
procedure RaiseOutsideConfigDirectory(const APath, AConfigPath, AKey,
  AFlag, AProblem: string);
begin
  raise TParseError.CreateFmt('%s: "%s" writes to %s, which %s; a ' +
    'config may only write inside its own directory (pass %s on the ' +
    'command line to write elsewhere)',
    [AConfigPath, AKey, APath, AProblem, AFlag]);
end;

{ Pins APinTarget under the config's directory first, then checks APath:
  the pin is taken as the path is judged, so an ancestor swapped for a link
  afterwards leads nowhere — every later write walks the pinned route
  without following links. }
function PinInsideConfigDirectory(const APath, APinTarget, AConfigPath, AKey,
  AFlag: string): TSandboxHostPin;
var
  Problem: string;
begin
  if not TryPinBeneath(ExtractFileDir(ExpandHostFileName(AConfigPath)),
     APinTarget, Result, Problem) then
    RaiseOutsideConfigDirectory(APath, AConfigPath, AKey, AFlag, Problem);
  Problem := ConfigOutputPathProblem(APath, AConfigPath);
  if Problem <> '' then
    RaiseOutsideConfigDirectory(APath, AConfigPath, AKey, AFlag, Problem);
end;

function ConfigInputs(const AConfig: TGocciaSandboxRequest): TGocciaSandboxInputs;
var
  I: Integer;
  Input: TGocciaSandboxInput;
  Key: string;
begin
  Result := nil;
  SetLength(Result, Length(AConfig.Inputs));
  for I := 0 to High(AConfig.Inputs) do
  begin
    if AConfig.Inputs[I].ReadWrite then
      Key := SANDBOX_CONFIG_KEY + '.copy-rw'
    else
      Key := SANDBOX_CONFIG_KEY + '.copy';
    Input.Spec := AConfig.Inputs[I].Spec;
    Input.Origin := AConfig.Inputs[I].SourcePath;
    Input.HostPath := ExcludeTrailingPathDelimiter(AConfig.Inputs[I].HostPath);
    if Input.HostPath = '' then
      Input.HostPath := PathDelim;
    if AConfig.Inputs[I].SandboxPath <> '' then
      Input.SandboxPath := CheckedSandboxPath(AConfig.Inputs[I].SandboxPath,
        AConfig.Inputs[I].SourcePath + ': "' + Key + '" entry "' +
        AConfig.Inputs[I].Spec + '"')
    else
      Input.SandboxPath := DefaultSandboxPathFor(Input.HostPath,
        AConfig.Inputs[I].Spec, AConfig.Inputs[I].SourcePath + ' "' + Key +
        '"');
    Input.ReadWrite := AConfig.Inputs[I].ReadWrite;
    Input.FromConfig := True;
    Input.Description := Format('%s: "%s" entry "%s"',
      [AConfig.Inputs[I].SourcePath, Key, AConfig.Inputs[I].Spec]);
    Input.Pin := Default(TSandboxHostPin);
    { Write-back writes under the input's directory: the input itself, or
      a file input's directory. }
    if Input.ReadWrite then
      if HostDirectoryExists(Input.HostPath) then
        Input.Pin := PinInsideConfigDirectory(Input.HostPath, Input.HostPath,
          AConfig.Inputs[I].SourcePath, Key, COPY_READ_WRITE_FLAG)
      else
        Input.Pin := PinInsideConfigDirectory(Input.HostPath,
          ExtractFileDir(Input.HostPath), AConfig.Inputs[I].SourcePath, Key,
          COPY_READ_WRITE_FLAG);
    Result[I] := Input;
  end;
end;

{ Where an input lands: its target, or, for a file whose target is the root
  or ends in `/`, the file's name inside it. }
function EffectiveTargetKey(const AInput: TGocciaSandboxInput): string;
var
  Target: string;
begin
  Target := AInput.SandboxPath;
  if (not HostDirectoryExists(AInput.HostPath)) and ((Target = SANDBOX_ROOT) or
     (Target[Length(Target)] = '/')) then
    Target := Target + '/' + ExtractFileName(AInput.HostPath);
  Result := SandboxTargetKey(Target);
end;

{ Two inputs from one source (the command line, or config) that land on the
  same sandbox path would leave the run with whichever was copied last, so
  they are refused. }
procedure RejectDuplicateTargets(const AInputs: TGocciaSandboxInputs);
var
  I, J: Integer;
begin
  for I := 0 to High(AInputs) do
    for J := I + 1 to High(AInputs) do
      { A file copied after a directory to the directory's own path lands
        inside it, as it would with a trailing `/`. }
      if (EffectiveTargetKey(AInputs[I]) = EffectiveTargetKey(AInputs[J])) and
         not (HostDirectoryExists(AInputs[I].HostPath) and
              not HostDirectoryExists(AInputs[J].HostPath)) then
        raise TCLIUsageError.CreateFmt('%s and %s both copy to %s; give ' +
          'one of them an explicit =<sandbox> path',
          [AInputs[I].Description, AInputs[J].Description,
           EffectiveTargetKey(AInputs[I])]);
end;

function CommandLineInputs(const AOptions: TGocciaSandboxOptions;
  const AWorkingDirectory: string): TGocciaSandboxInputs;
var
  I, Count: Integer;
begin
  Result := nil;
  if not AOptions.Copy.FromCommandLine and
     not AOptions.CopyReadWrite.FromCommandLine then
    Exit;
  SetLength(Result, AOptions.Copy.Values.Count +
    AOptions.CopyReadWrite.Values.Count);
  Count := 0;
  for I := 0 to AOptions.Copy.Values.Count - 1 do
  begin
    Result[Count] := ParseCopySpec(AOptions.Copy.Values[I],
      AWorkingDirectory, COPY_FLAG, False);
    Inc(Count);
  end;
  for I := 0 to AOptions.CopyReadWrite.Values.Count - 1 do
  begin
    Result[Count] := ParseCopySpec(AOptions.CopyReadWrite.Values[I],
      AWorkingDirectory, COPY_READ_WRITE_FLAG, True);
    Inc(Count);
  end;
  RejectDuplicateTargets(Result);
end;

procedure ResolveDiff(const AOptions: TGocciaSandboxOptions;
  const AConfig: TGocciaSandboxRequest; const AUseConfig: Boolean;
  const AWorkingDirectory: string; var ARequest: TGocciaSandboxModeRequest);
var
  Explicit: string;
  ExplicitSource: string;
begin
  ARequest.DiffRequested := False;
  ARequest.DiffFormat := sdfJson;
  ARequest.DiffFile := '';

  if AOptions.DiffFile.FromCommandLine then
  begin
    if AOptions.DiffFile.Value = '' then
      raise TParseError.Create('--diff-file needs a file path');
    ARequest.DiffFile := HostAbsolutePath(AOptions.DiffFile.Value,
      AWorkingDirectory);
  end
  else if AUseConfig and (AConfig.DiffFile <> '') then
  begin
    ARequest.DiffFilePin := PinInsideConfigDirectory(AConfig.DiffFile,
      AConfig.DiffFile, AConfig.DiffFileSourcePath,
      SANDBOX_CONFIG_KEY + '.diff-file', '--diff-file');
    ARequest.DiffFile := AConfig.DiffFile;
  end;

  Explicit := '';
  ExplicitSource := '';
  if AOptions.Diff.FromCommandLine then
  begin
    ARequest.DiffRequested := True;
    if AOptions.Diff.Value <> '' then
    begin
      Explicit := AOptions.Diff.Value;
      ExplicitSource := '--diff';
    end;
  end;
  if AUseConfig and (AConfig.Diff <> '') then
  begin
    ARequest.DiffRequested := True;
    if (Explicit = '') and (AConfig.Diff <> SANDBOX_DIFF_DEFAULT) then
    begin
      Explicit := AConfig.Diff;
      ExplicitSource := AConfig.SourcePath;
    end;
  end;
  if ARequest.DiffFile <> '' then
    ARequest.DiffRequested := True;
  if not ARequest.DiffRequested then
    Exit;

  if Explicit <> '' then
  begin
    if not TryParseDiffFormat(Explicit, ARequest.DiffFormat) then
      raise TParseError.CreateFmt(
        'Invalid value for --diff: %s (use --diff=json or --diff=unified)',
        [Explicit]);
  end
  else if (ARequest.DiffFile <> '') and
    not TryInferDiffFormat(ARequest.DiffFile, ARequest.DiffFormat) then
    raise TParseError.CreateFmt(
      'Cannot tell the diff format from "%s": name the file .json or ' +
      '.diff, or pass --diff=json or --diff=unified',
      [ExtractFileName(ARequest.DiffFile)]);
end;

function ResolveSandboxMode(const AOptions: TGocciaSandboxOptions;
  const AConfig: TGocciaSandboxRequest; const AConfigAccepted: Boolean;
  const ACommandLine: TGocciaSandboxCommandLine): TGocciaSandboxModeRequest;
var
  UseConfig: Boolean;
  SandboxOnly, Path, ConfigEntry: string;
  FromConfig: TGocciaSandboxInputs;
  I: Integer;
begin
  Result := Default(TGocciaSandboxModeRequest);
  UseConfig := AConfig.Declared and AConfigAccepted;
  Result.Reason := AOptions.CommandLineActivation;
  if (Result.Reason = '') and UseConfig then
    Result.Reason := Format('the "%s" section of %s',
      [SANDBOX_CONFIG_KEY, AConfig.SourcePath]);
  Result.Active := Result.Reason <> '';

  if not Result.Active then
  begin
    { Sandbox-only options from config are ignored in host mode: a project
      config may serve both modes. From the command line they are a
      mistake. }
    SandboxOnly := AOptions.FirstSandboxOnlyCommandLineOption;
    if SandboxOnly <> '' then
      raise TCLIUsageError.CreateFmt('%s only applies in sandbox mode; %s',
        [SandboxOnly, ENABLE_SANDBOX_HINT]);
    Exit;
  end;

  RejectHostCapabilityFlags(ACommandLine.DeniedAllowFlags, Result.Reason);
  RejectHostModeOptions(ACommandLine.HostOnlyOptions, Result.Reason);

  { The entry. A config's entry is checked even when the command line names
    another, so a malformed config fails wherever it is used. }
  ConfigEntry := '';
  if UseConfig and (AConfig.Entry <> '') then
    ConfigEntry := CheckedSandboxPath(AConfig.Entry, AConfig.SourcePath +
      ': "' + SANDBOX_CONFIG_KEY + '.entry"');
  if AOptions.Entry.FromCommandLine then
  begin
    if ACommandLine.Paths.Count > 0 then
      raise TCLIUsageError.CreateFmt(
        '--entry names the sandbox entry, so a host file cannot be given ' +
        'too; drop %s or --entry', [ACommandLine.Paths[0]]);
    Result.EntrySandbox := CheckedSandboxPath(AOptions.Entry.Value,
      '--entry');
  end
  else if ACommandLine.Paths.Count > 1 then
    raise TCLIUsageError.CreateFmt(
      'sandbox mode runs one entry file; got %d inputs',
      [ACommandLine.Paths.Count])
  else if ACommandLine.Paths.Count = 1 then
  begin
    Path := ACommandLine.Paths[0];
    if IsStdinPath(Path) then
      raise TCLIUsageError.Create('sandbox mode does not read stdin; pass ' +
        'a host file, or --entry <sandbox-path> for a copied one');
    Result.EntryHost := HostAbsolutePath(Path,
      ACommandLine.WorkingDirectory);
    if HostDirectoryExists(Result.EntryHost) then
      raise TCLIUsageError.CreateFmt(
        'sandbox mode runs one entry file, not a directory: %s (copy it ' +
        'with --copy %s and name the entry with --entry)', [Path, Path]);
    if LowerCase(ExtractFileExt(Path)) = EXT_GBC then
      raise TCLIUsageError.CreateFmt(
        'sandbox mode runs source files, not bytecode: %s', [Path]);
    if not HostFileExists(Result.EntryHost) then
      raise Exception.Create('Path not found: ' + Path);
    { The command line beats config. }
    if UseConfig and (AConfig.Entry <> '') then
    begin
      SetLength(Result.Notes, Length(Result.Notes) + 1);
      Result.Notes[High(Result.Notes)] := Format('Note: %s: "%s.entry" %s ' +
        'is not used; the command line names the entry (%s)',
        [AConfig.SourcePath, SANDBOX_CONFIG_KEY, AConfig.Entry, Path]);
    end;
  end
  else if ConfigEntry <> '' then
    Result.EntrySandbox := ConfigEntry
  else
    raise TCLIUsageError.Create('sandbox mode needs an entry: pass a host ' +
      'file, or --entry <sandbox-path> for a copied one (sandbox mode does ' +
      'not read stdin)');

  { The inputs. }
  if UseConfig then
  begin
    FromConfig := ConfigInputs(AConfig);
    RejectDuplicateTargets(FromConfig);
    Result.Inputs := MergeSandboxInputs(FromConfig,
      CommandLineInputs(AOptions, ACommandLine.WorkingDirectory));
  end
  else
    Result.Inputs := CommandLineInputs(AOptions,
      ACommandLine.WorkingDirectory);
  for I := 0 to High(Result.Inputs) do
    if not HostFileExists(Result.Inputs[I].HostPath) and
       not HostDirectoryExists(Result.Inputs[I].HostPath) then
      raise Exception.CreateFmt('%s %s: copy path does not exist: %s',
        [Result.Inputs[I].Origin, Result.Inputs[I].Spec,
         Result.Inputs[I].HostPath]);

  ResolveDiff(AOptions, AConfig, UseConfig, ACommandLine.WorkingDirectory,
    Result);

  if AOptions.MaxFsBytes.Present and (AOptions.MaxFsBytes.Value <= 0) then
    raise TParseError.Create('--max-fs-bytes must be greater than 0.');
  if AOptions.MaxFsNodes.Present and (AOptions.MaxFsNodes.Value <= 0) then
    raise TParseError.Create('--max-fs-nodes must be greater than 0.');
  Result.MaxFsBytes := AOptions.MaxFsBytes.ValueOr(DEFAULT_SANDBOX_BYTE_QUOTA);
  { MaxFsNodes.Maximum keeps the value within Integer. }
  Result.MaxFsNodes := Integer(AOptions.MaxFsNodes.ValueOr(
    DEFAULT_SANDBOX_NODE_QUOTA));
end;

procedure RejectHostCapabilityFlags(const AFlags: array of string;
  const AReason: string);
begin
  if Length(AFlags) > 0 then
    raise TCLIUsageError.CreateFmt(
      '%s cannot be used in sandbox mode (enabled by %s): %s',
      [AFlags[0], AReason, NO_HOST_FILESYSTEM_HINT]);
end;

procedure RejectHostModeOptions(const AOptions: array of string;
  const AReason: string);
begin
  if Length(AOptions) > 0 then
    raise TCLIUsageError.CreateFmt(
      '%s cannot be used in sandbox mode (enabled by %s)',
      [AOptions[0], AReason]);
end;

function SandboxModeHelpNote: string;
begin
  Result :=
    'Sandbox mode:' + sLineBreak +
    '  Enabled by --sandbox, --copy, --copy-rw, or a trusted "sandbox" ' +
    'section in goccia.json.' + sLineBreak +
    '  Scripts see only the virtual filesystem and may import "fs" and ' +
    '"goccia" ($, runScript).' + sLineBreak +
    '  Only --allow-net applies; --allow-read, --allow-import and ' +
    '--allow-ffi are errors.' + sLineBreak +
    '  Nothing reaches the host unless an input was copied with --copy-rw ' +
    'and the run succeeded.' + sLineBreak;
end;

end.
