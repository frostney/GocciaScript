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
  TGocciaSandboxRunResult = record
    Ok: Boolean;
    ExitCode: Integer;
    Output: string;
    ErrorOutput: string;
    ErrorMessage: string;
    ResultValue: TGocciaValue;
  end;

  TGocciaSandboxRunScriptCallback = function(const AEntryPath: string):
    TGocciaSandboxRunResult of object;

  TGocciaSandboxContext = class
  private
    FFs: TSandboxVirtualFileSystem;
    FBaseline: TSandboxVirtualFileSystem;
    FShell: TSandboxShell;
    FRunScriptCallback: TGocciaSandboxRunScriptCallback;

    function HandleShellCommand(const ACommand: string;
      const AArgs: TStringArray; var AResult: TExecResult): Boolean;
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

implementation

function BytesLength(const ABytes: TBytes): Int64;
begin
  Result := Length(ABytes);
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

function TGocciaSandboxContext.HandleShellCommand(const ACommand: string;
  const AArgs: TStringArray; var AResult: TExecResult): Boolean;
var
  RunResult: TGocciaSandboxRunResult;
begin
  Result := ACommand = 'goccia';
  if not Result then
    Exit;

  if Length(AArgs) <> 1 then
  begin
    AResult.ErrorOutput := AResult.ErrorOutput +
      'goccia: expected: goccia <sandbox-entry.js>' + LineEnding;
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

  RunResult := FRunScriptCallback(FFs.Normalize(AArgs[0],
    FShell.WorkingDirectory));
  AResult.Output := AResult.Output + RunResult.Output;
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
  I: Integer;
  Path: string;
begin
  CurrentPaths := TStringList.Create;
  try
    CurrentPaths.Sorted := True;
    CollectPaths(FFs, '/', CurrentPaths);
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
  finally
    CurrentPaths.Free;
  end;
end;

end.
