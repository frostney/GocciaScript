unit Goccia.Sandbox.Context;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  SandboxShell,
  SandboxVirtualFileSystem,

  Goccia.Values.Primitives;

type
  TGocciaSandboxContext = class;

  TGocciaSandboxSeedKind = (
    sskParentPath,
    sskText,
    sskBytes
  );

  TGocciaSandboxSeedSpec = record
    Kind: TGocciaSandboxSeedKind;
    FromPath: string;
    ToPath: string;
    ToDirectory: Boolean;
    Path: string;
    Text: string;
    Bytes: TBytes;
  end;

  TGocciaSandboxSeedSpecArray = array of TGocciaSandboxSeedSpec;

  TGocciaSandboxRunOptions = record
    Isolated: Boolean;
    Seeds: TGocciaSandboxSeedSpecArray;
    IncludeDiff: Boolean;
    DiffFormat: string;
  end;

  TGocciaSandboxRunResult = record
    Ok: Boolean;
    ExitCode: Integer;
    Output: string;
    ErrorOutput: string;
    ErrorMessage: string;
    Diff: string;
    DiffRequested: Boolean;
    ResultValue: TGocciaValue;
  end;

  TGocciaSandboxRunScriptCallback = function(
    const AContext: TGocciaSandboxContext; const AEntryPath: string;
    const AOptions: TGocciaSandboxRunOptions): TGocciaSandboxRunResult of object;

  TGocciaSandboxContext = class
  private
    FFs: TSandboxVirtualFileSystem;
    FBaseline: TSandboxVirtualFileSystem;
    FShell: TSandboxShell;
    FRunScriptCallback: TGocciaSandboxRunScriptCallback;

    function HandleShellCommand(const ACommand: string;
      const AArgs: TStringArray; var AResult: TExecResult): Boolean;
    procedure AddParentPathSeed(var AOptions: TGocciaSandboxRunOptions;
      const ASpec: string);
    function ParseShellRunOptions(const AArgs: TStringArray;
      out AEntryPath: string; out AOptions: TGocciaSandboxRunOptions;
      out AError: string): Boolean;
    procedure CollectPaths(const AFs: TSandboxVirtualFileSystem;
      const APath: string; const APaths: TStrings);
    function BytesEqual(const ALeft, ARight: TBytes): Boolean;
    function JsonEscape(const AValue: string): string;
    procedure AppendJsonChange(const ABuilder: TStrings; var AFirst: Boolean;
      const AKind, APath: string; const AOldBytes, ANewBytes: Int64);
  public
    constructor Create(const AQuotaBytes: Int64 = 0);
    destructor Destroy; override;

    procedure CaptureBaseline;
    function DiffJson: string;
    function DiffUnified: string;

    property Fs: TSandboxVirtualFileSystem read FFs;
    property Baseline: TSandboxVirtualFileSystem read FBaseline;
    property Shell: TSandboxShell read FShell;
    property RunScriptCallback: TGocciaSandboxRunScriptCallback
      read FRunScriptCallback write FRunScriptCallback;
  end;

function DefaultSandboxRunOptions: TGocciaSandboxRunOptions;

implementation

function BytesLength(const ABytes: TBytes): Int64;
begin
  Result := Length(ABytes);
end;

function DefaultSandboxRunOptions: TGocciaSandboxRunOptions;
begin
  Result.Isolated := False;
  Result.Seeds := nil;
  Result.IncludeDiff := False;
  Result.DiffFormat := 'json';
end;

procedure AppendSeedSpec(var AOptions: TGocciaSandboxRunOptions;
  const ASeed: TGocciaSandboxSeedSpec);
var
  Index: Integer;
begin
  Index := Length(AOptions.Seeds);
  SetLength(AOptions.Seeds, Index + 1);
  AOptions.Seeds[Index] := ASeed;
end;

function HasPrefix(const AValue, APrefix: string): Boolean;
begin
  Result := Copy(AValue, 1, Length(APrefix)) = APrefix;
end;

function IsDirectoryTargetPath(const APath: string): Boolean;
var
  Path: string;
begin
  Path := NormalizeSandboxPathSeparators(APath);
  Result := (Path = '/') or ((Path <> '') and (Path[Length(Path)] = '/'));
end;

{ TGocciaSandboxContext }

constructor TGocciaSandboxContext.Create(const AQuotaBytes: Int64);
begin
  inherited Create;
  FFs := TSandboxVirtualFileSystem.Create(AQuotaBytes);
  FShell := TSandboxShell.Create(FFs, False);
  FShell.CommandHandler := HandleShellCommand;
end;

destructor TGocciaSandboxContext.Destroy;
begin
  FBaseline.Free;
  FShell.Free;
  FFs.Free;
  inherited;
end;

procedure TGocciaSandboxContext.CaptureBaseline;
begin
  FBaseline.Free;
  FBaseline := FFs.Fork;
end;

procedure TGocciaSandboxContext.AddParentPathSeed(
  var AOptions: TGocciaSandboxRunOptions; const ASpec: string);
var
  SeparatorIndex: Integer;
  SourcePath: string;
  TargetPath: string;
  Seed: TGocciaSandboxSeedSpec;
begin
  SeparatorIndex := Pos('=', ASpec);
  if SeparatorIndex > 0 then
  begin
    SourcePath := Copy(ASpec, 1, SeparatorIndex - 1);
    TargetPath := Copy(ASpec, SeparatorIndex + 1, MaxInt);
  end
  else
  begin
    SourcePath := ASpec;
    TargetPath := '';
  end;

  Seed.Kind := sskParentPath;
  Seed.FromPath := FFs.Normalize(SourcePath, FShell.WorkingDirectory);
  if TargetPath = '' then
    Seed.ToPath := Seed.FromPath
  else
    Seed.ToPath := FFs.Normalize(TargetPath, '/');
  Seed.ToDirectory := IsDirectoryTargetPath(TargetPath);
  Seed.Path := '';
  Seed.Text := '';
  Seed.Bytes := nil;
  AppendSeedSpec(AOptions, Seed);
  AOptions.Isolated := True;
end;

function TGocciaSandboxContext.ParseShellRunOptions(
  const AArgs: TStringArray; out AEntryPath: string;
  out AOptions: TGocciaSandboxRunOptions; out AError: string): Boolean;
var
  Index: Integer;
  Arg: string;
begin
  Result := False;
  AEntryPath := '';
  AError := '';
  AOptions := DefaultSandboxRunOptions;

  Index := 0;
  while Index <= High(AArgs) do
  begin
    Arg := AArgs[Index];
    if Arg = '--sandbox' then
      AOptions.Isolated := True
    else if Arg = '--diff' then
    begin
      AOptions.IncludeDiff := True;
      AOptions.Isolated := True;
    end
    else if Arg = '--seed' then
    begin
      Inc(Index);
      if Index > High(AArgs) then
      begin
        AError := '--seed requires a value';
        Exit;
      end;
      AddParentPathSeed(AOptions, AArgs[Index]);
    end
    else if HasPrefix(Arg, '--seed=') then
      AddParentPathSeed(AOptions, Copy(Arg, Length('--seed=') + 1, MaxInt))
    else if Arg = '--diff-format' then
    begin
      Inc(Index);
      if Index > High(AArgs) then
      begin
        AError := '--diff-format requires json or unified';
        Exit;
      end;
      AOptions.DiffFormat := AArgs[Index];
    end
    else if HasPrefix(Arg, '--diff-format=') then
      AOptions.DiffFormat := Copy(Arg, Length('--diff-format=') + 1, MaxInt)
    else if AEntryPath = '' then
      AEntryPath := Arg
    else
    begin
      AError := 'unexpected argument: ' + Arg;
      Exit;
    end;
    Inc(Index);
  end;

  if AEntryPath = '' then
  begin
    AError :=
      'expected: goccia [--sandbox] [--seed <from[=to]>] <sandbox-entry.js>';
    Exit;
  end;

  if (AOptions.DiffFormat <> 'json') and (AOptions.DiffFormat <> 'unified') then
  begin
    AError := '--diff-format must be json or unified';
    Exit;
  end;

  AEntryPath := FFs.Normalize(AEntryPath, FShell.WorkingDirectory);
  Result := True;
end;

function TGocciaSandboxContext.HandleShellCommand(const ACommand: string;
  const AArgs: TStringArray; var AResult: TExecResult): Boolean;
var
  EntryPath: string;
  Options: TGocciaSandboxRunOptions;
  RunResult: TGocciaSandboxRunResult;
  Error: string;
begin
  Result := ACommand = 'goccia';
  if not Result then
    Exit;

  if not ParseShellRunOptions(AArgs, EntryPath, Options, Error) then
  begin
    AResult.ErrorOutput := AResult.ErrorOutput +
      'goccia: ' + Error + LineEnding;
    AResult.ExitCode := 2;
    Exit;
  end;

  if not Assigned(FRunScriptCallback) then
  begin
    AResult.ErrorOutput := AResult.ErrorOutput +
      'goccia: nested script execution is not configured' + LineEnding;
    AResult.ExitCode := 127;
    Exit;
  end;

  RunResult := FRunScriptCallback(Self, EntryPath, Options);
  AResult.Output := AResult.Output + RunResult.Output;
  if RunResult.Diff <> '' then
    AResult.Output := AResult.Output + RunResult.Diff;
  AResult.ErrorOutput := AResult.ErrorOutput + RunResult.ErrorOutput;
  if not RunResult.Ok then
  begin
    if RunResult.ErrorMessage <> '' then
      AResult.ErrorOutput := AResult.ErrorOutput + 'goccia: ' +
        RunResult.ErrorMessage + LineEnding;
    AResult.ExitCode := RunResult.ExitCode;
  end;
end;

procedure TGocciaSandboxContext.CollectPaths(
  const AFs: TSandboxVirtualFileSystem; const APath: string;
  const APaths: TStrings);
var
  Entries: TSandboxFsStatArray;
  I: Integer;
begin
  APaths.Add(APath);
  if not AFs.IsDirectory(APath) then
    Exit;

  Entries := AFs.List(APath);
  for I := 0 to High(Entries) do
    CollectPaths(AFs, Entries[I].Path, APaths);
end;

function TGocciaSandboxContext.BytesEqual(const ALeft,
  ARight: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then
      Exit(False);
  Result := True;
end;

function TGocciaSandboxContext.JsonEscape(const AValue: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    case Ch of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if Ord(Ch) < 32 then
        Result := Result + '\u' + IntToHex(Ord(Ch), 4)
      else
        Result := Result + Ch;
    end;
  end;
end;

procedure TGocciaSandboxContext.AppendJsonChange(const ABuilder: TStrings;
  var AFirst: Boolean; const AKind, APath: string; const AOldBytes,
  ANewBytes: Int64);
begin
  if not AFirst then
    ABuilder[ABuilder.Count - 1] := ABuilder[ABuilder.Count - 1] + ',';
  ABuilder.Add('    { "kind": "' + JsonEscape(AKind) + '", "path": "' +
    JsonEscape(APath) + '", "oldBytes": ' + IntToStr(AOldBytes) +
    ', "newBytes": ' + IntToStr(ANewBytes) + ' }');
  AFirst := False;
end;

function TGocciaSandboxContext.DiffJson: string;
var
  Builder: TStringList;
  CurrentPaths: TStringList;
  BaselinePaths: TStringList;
  First: Boolean;
  I: Integer;
  Path: string;
  CurrentStat: TSandboxFsStat;
  BaselineStat: TSandboxFsStat;
  CurrentBytes: TBytes;
  BaselineBytes: TBytes;
begin
  Builder := TStringList.Create;
  CurrentPaths := TStringList.Create;
  BaselinePaths := TStringList.Create;
  try
    CurrentPaths.Sorted := True;
    BaselinePaths.Sorted := True;
    if Assigned(FBaseline) then
      CollectPaths(FBaseline, '/', BaselinePaths);
    CollectPaths(FFs, '/', CurrentPaths);

    Builder.Add('{');
    Builder.Add('  "changes": [');
    First := True;

    for I := 0 to CurrentPaths.Count - 1 do
    begin
      Path := CurrentPaths[I];
      if Path = '/' then
        Continue;
      CurrentStat := FFs.Stat(Path);
      if (not Assigned(FBaseline)) or (not FBaseline.Exists(Path)) then
      begin
        if CurrentStat.Kind = nkFile then
          AppendJsonChange(Builder, First, 'create', Path, 0,
            CurrentStat.Size)
        else
          AppendJsonChange(Builder, First, 'mkdir', Path, 0, 0);
        Continue;
      end;

      BaselineStat := FBaseline.Stat(Path);
      if CurrentStat.Kind <> BaselineStat.Kind then
      begin
        AppendJsonChange(Builder, First, 'replace', Path,
          BaselineStat.Size, CurrentStat.Size);
        Continue;
      end;

      if CurrentStat.Kind = nkFile then
      begin
        CurrentBytes := FFs.ReadAllBytes(Path);
        BaselineBytes := FBaseline.ReadAllBytes(Path);
        if not BytesEqual(CurrentBytes, BaselineBytes) then
          AppendJsonChange(Builder, First, 'modify', Path,
            BytesLength(BaselineBytes), BytesLength(CurrentBytes));
      end;
    end;

    for I := 0 to BaselinePaths.Count - 1 do
    begin
      Path := BaselinePaths[I];
      if (Path <> '/') and (not FFs.Exists(Path)) then
      begin
        BaselineStat := FBaseline.Stat(Path);
        AppendJsonChange(Builder, First, 'delete', Path,
          BaselineStat.Size, 0);
      end;
    end;

    Builder.Add('  ]');
    Builder.Add('}');
    Result := Builder.Text;
  finally
    BaselinePaths.Free;
    CurrentPaths.Free;
    Builder.Free;
  end;
end;

function TGocciaSandboxContext.DiffUnified: string;
var
  CurrentPaths: TStringList;
  BaselinePaths: TStringList;
  I: Integer;
  Path: string;
begin
  CurrentPaths := TStringList.Create;
  BaselinePaths := TStringList.Create;
  try
    CurrentPaths.Sorted := True;
    BaselinePaths.Sorted := True;
    CollectPaths(FFs, '/', CurrentPaths);
    if Assigned(FBaseline) then
      CollectPaths(FBaseline, '/', BaselinePaths);
    Result := '';
    for I := 0 to CurrentPaths.Count - 1 do
    begin
      Path := CurrentPaths[I];
      if (Path = '/') or (not FFs.IsFile(Path)) then
        Continue;
      if Assigned(FBaseline) and FBaseline.IsFile(Path) and
         BytesEqual(FBaseline.ReadAllBytes(Path), FFs.ReadAllBytes(Path)) then
        Continue;
      Result := Result + '--- ' + Path + LineEnding;
      Result := Result + '+++ ' + Path + LineEnding;
      Result := Result + '@@ sandbox file changed @@' + LineEnding;
      if Assigned(FBaseline) and FBaseline.IsFile(Path) then
        Result := Result + '-' + FBaseline.ReadAllText(Path) + LineEnding;
      Result := Result + '+' + FFs.ReadAllText(Path) + LineEnding;
    end;

    for I := 0 to BaselinePaths.Count - 1 do
    begin
      Path := BaselinePaths[I];
      if (Path = '/') or (not FBaseline.IsFile(Path)) or FFs.Exists(Path) then
        Continue;
      Result := Result + '--- ' + Path + LineEnding;
      Result := Result + '+++ ' + Path + LineEnding;
      Result := Result + '@@ sandbox file deleted @@' + LineEnding;
      Result := Result + '-' + FBaseline.ReadAllText(Path) + LineEnding;
    end;
  finally
    BaselinePaths.Free;
    CurrentPaths.Free;
  end;
end;

end.
