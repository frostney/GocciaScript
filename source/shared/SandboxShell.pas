{ SandboxShell - isolated command execution over SandboxVirtualFileSystem.

  A small coreutils-flavoured interpreter whose entire world is one
  TSandboxVirtualFileSystem instance. Every command is a builtin; there is no
  process spawning and no host filesystem access, so an unknown
  command is exit 127, not a security hole.

    Shell := TSandboxShell.Create(Fs);
    R := Shell.Exec('echo "hello" > /work/greeting.txt');
    // R.ExitCode = 0, captured Output / ErrorOutput, no host I/O

  Supported: pwd cd ls mkdir touch cat echo rm cp mv stat tree df
  help, plus '>' / '>>' stdout redirection and single/double-quote
  tokenisation. Currently unsupported: pipes, globbing,
  variables, stderr redirection. }

unit SandboxShell;

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils,

  SandboxVirtualFileSystem;

type
  TExecResult = record
    ExitCode: Integer;
    Output: string;
    ErrorOutput: string;
  end;

  TSandboxShellCommandHandler = function(const ACommand: string;
    const AArgs: TStringArray; var AResult: TExecResult): Boolean of object;

  TSandboxShell = class
  private
    FFs: TSandboxVirtualFileSystem;
    FOwnsFs: Boolean;
    FCwd: string;
    FCommandHandler: TSandboxShellCommandHandler;
    function Tokenize(const ALine: string; out ATokens: TStringArray;
      out AError: string): Boolean;
    function StripRedirection(var ATokens: TStringArray;
      out ATargetPath: string; out AAppend: Boolean;
      out AError: string): Boolean;
    function SplitFlags(const ACommand: string;
      const AArgs: TStringArray; const AAllowedFlags: string;
      out AFlags: string; out APositional: TStringArray;
      var AResult: TExecResult): Boolean;
    procedure OutLine(var AResult: TExecResult; const ALine: string);
    procedure ErrLine(var AResult: TExecResult; const ACommand: string;
      const AMessage: string);
    procedure UsageError(var AResult: TExecResult;
      const ACommand, AMessage: string);
    function FormatEntry(const AStat: TSandboxFsStat;
      const ALong: Boolean): string;
    procedure AppendTree(var AResult: TExecResult; const APath: string;
      const APrefix: string);
    procedure DispatchCommand(const ACommand: string;
      const AArgs: TStringArray; var AResult: TExecResult);
    procedure CmdPwd(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdCd(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdLs(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdMkdir(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdTouch(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdCat(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdEcho(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdRm(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdCp(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdMv(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdStat(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdTree(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdDf(const AArgs: TStringArray;
      var AResult: TExecResult);
    procedure CmdHelp(const AArgs: TStringArray;
      var AResult: TExecResult);
  public
    constructor Create(const AFs: TSandboxVirtualFileSystem;
      const AOwnsFs: Boolean = False);
    destructor Destroy; override;

    function Exec(const ACommandLine: string): TExecResult;

    property Fs: TSandboxVirtualFileSystem read FFs;
    property WorkingDirectory: string read FCwd;
    property CommandHandler: TSandboxShellCommandHandler
      read FCommandHandler write FCommandHandler;
  end;

implementation

const
  ExitOk = 0;
  ExitFailure = 1;
  ExitUsage = 2;
  ExitUnknownCommand = 127;

{ TSandboxShell }

constructor TSandboxShell.Create(const AFs: TSandboxVirtualFileSystem;
  const AOwnsFs: Boolean);
begin
  inherited Create;
  FFs := AFs;
  FOwnsFs := AOwnsFs;
  FCwd := '/';
end;

destructor TSandboxShell.Destroy;
begin
  if FOwnsFs then
    FFs.Free;
  inherited Destroy;
end;

procedure TSandboxShell.OutLine(var AResult: TExecResult;
  const ALine: string);
begin
  AResult.Output := AResult.Output + ALine + LineEnding;
end;

procedure TSandboxShell.ErrLine(var AResult: TExecResult;
  const ACommand: string; const AMessage: string);
begin
  AResult.ErrorOutput := AResult.ErrorOutput + ACommand + ': ' +
    AMessage + LineEnding;
end;

procedure TSandboxShell.UsageError(var AResult: TExecResult;
  const ACommand, AMessage: string);
begin
  ErrLine(AResult, ACommand, AMessage);
  AResult.ExitCode := ExitUsage;
end;

function TSandboxShell.Tokenize(const ALine: string;
  out ATokens: TStringArray; out AError: string): Boolean;
var
  Index: Integer;
  Current: string;
  HasToken: Boolean;
  Ch: Char;

  procedure Flush;
  var
    Count: Integer;
  begin
    if not HasToken then
      Exit;
    Count := Length(ATokens);
    SetLength(ATokens, Count + 1);
    ATokens[Count] := Current;
    Current := '';
    HasToken := False;
  end;

  procedure Append(const AValue: Char);
  begin
    Current := Current + AValue;
    HasToken := True;
  end;

begin
  ATokens := nil;
  AError := '';
  Current := '';
  HasToken := False;
  Index := 1;
  while Index <= Length(ALine) do
  begin
    Ch := ALine[Index];
    case Ch of
      ' ', #9:
        Flush;
      '''':
      begin
        HasToken := True;
        Inc(Index);
        while (Index <= Length(ALine)) and (ALine[Index] <> '''') do
        begin
          Current := Current + ALine[Index];
          Inc(Index);
        end;
        if Index > Length(ALine) then
        begin
          AError := 'unterminated single quote';
          Exit(False);
        end;
      end;
      '"':
      begin
        HasToken := True;
        Inc(Index);
        while (Index <= Length(ALine)) and (ALine[Index] <> '"') do
        begin
          if (ALine[Index] = '\') and (Index < Length(ALine)) and
            (ALine[Index + 1] in ['"', '\']) then
            Inc(Index);
          Current := Current + ALine[Index];
          Inc(Index);
        end;
        if Index > Length(ALine) then
        begin
          AError := 'unterminated double quote';
          Exit(False);
        end;
      end;
      '\':
      begin
        if Index = Length(ALine) then
        begin
          AError := 'trailing backslash';
          Exit(False);
        end;
        Inc(Index);
        Append(ALine[Index]);
      end;
      '>':
      begin
        Flush;
        if (Index < Length(ALine)) and (ALine[Index + 1] = '>') then
        begin
          Append('>');
          Append('>');
          Inc(Index);
        end
        else
          Append('>');
        Flush;
      end;
    else
      Append(Ch);
    end;
    Inc(Index);
  end;
  Flush;
  Result := True;
end;

function TSandboxShell.StripRedirection(var ATokens: TStringArray;
  out ATargetPath: string; out AAppend: Boolean;
  out AError: string): Boolean;
var
  Kept: TStringArray;
  Index: Integer;
  Count: Integer;
begin
  ATargetPath := '';
  AAppend := False;
  AError := '';
  Kept := nil;
  Count := 0;
  Index := 0;
  while Index <= High(ATokens) do
  begin
    if (ATokens[Index] = '>') or (ATokens[Index] = '>>') then
    begin
      if ATargetPath <> '' then
      begin
        AError := 'ambiguous redirect (multiple targets)';
        Exit(False);
      end;
      if (Index = High(ATokens)) or (ATokens[Index + 1] = '>') or
        (ATokens[Index + 1] = '>>') then
      begin
        AError := 'redirect is missing a target path';
        Exit(False);
      end;
      AAppend := ATokens[Index] = '>>';
      ATargetPath := ATokens[Index + 1];
      Inc(Index, 2);
      Continue;
    end;
    SetLength(Kept, Count + 1);
    Kept[Count] := ATokens[Index];
    Inc(Count);
    Inc(Index);
  end;
  ATokens := Kept;
  Result := True;
end;

function TSandboxShell.SplitFlags(const ACommand: string;
  const AArgs: TStringArray; const AAllowedFlags: string;
  out AFlags: string; out APositional: TStringArray;
  var AResult: TExecResult): Boolean;
var
  Index: Integer;
  CharIndex: Integer;
  Count: Integer;
  FlagsDone: Boolean;
  Arg: string;
begin
  AFlags := '';
  APositional := nil;
  Count := 0;
  FlagsDone := False;
  for Index := 0 to High(AArgs) do
  begin
    Arg := AArgs[Index];
    if (not FlagsDone) and (Arg = '--') then
    begin
      FlagsDone := True;
      Continue;
    end;
    if (not FlagsDone) and (Length(Arg) > 1) and (Arg[1] = '-') then
    begin
      for CharIndex := 2 to Length(Arg) do
      begin
        if Pos(Arg[CharIndex], AAllowedFlags) = 0 then
        begin
          UsageError(AResult, ACommand,
            Format('unknown option -%s', [Arg[CharIndex]]));
          Exit(False);
        end;
        if Pos(Arg[CharIndex], AFlags) = 0 then
          AFlags := AFlags + Arg[CharIndex];
      end;
      Continue;
    end;
    SetLength(APositional, Count + 1);
    APositional[Count] := Arg;
    Inc(Count);
  end;
  Result := True;
end;

function TSandboxShell.Exec(
  const ACommandLine: string): TExecResult;
var
  Tokens: TStringArray;
  Args: TStringArray;
  TokenError: string;
  RedirectPath: string;
  RedirectAppend: Boolean;
  RedirectHandle: TSandboxFsFile;
  Command: string;
  Index: Integer;
begin
  Result.ExitCode := ExitOk;
  Result.Output := '';
  Result.ErrorOutput := '';

  if not Tokenize(ACommandLine, Tokens, TokenError) then
  begin
    UsageError(Result, 'shell', 'syntax error: ' + TokenError);
    Exit;
  end;
  if not StripRedirection(Tokens, RedirectPath, RedirectAppend,
    TokenError) then
  begin
    UsageError(Result, 'shell', 'syntax error: ' + TokenError);
    Exit;
  end;
  if Length(Tokens) = 0 then
    Exit;

  Command := Tokens[0];
  SetLength(Args, Length(Tokens) - 1);
  for Index := 1 to High(Tokens) do
    Args[Index - 1] := Tokens[Index];

  { faithful to POSIX: the redirect target opens (and truncates)
    before the command runs, and open failure aborts the command }
  RedirectHandle := nil;
  if RedirectPath <> '' then
    try
      if RedirectAppend then
        RedirectHandle := FFs.Open(FFs.Normalize(RedirectPath, FCwd),
          omAppend)
      else
        RedirectHandle := FFs.Open(FFs.Normalize(RedirectPath, FCwd),
          omWrite);
    except
      on E: ESandboxFsError do
      begin
        ErrLine(Result, 'shell', E.Message);
        Result.ExitCode := ExitFailure;
        Exit;
      end;
    end;

  try
    DispatchCommand(Command, Args, Result);
    if Assigned(RedirectHandle) then
    begin
      try
        RedirectHandle.WriteText(Result.Output);
      except
        on E: ESandboxFsError do
        begin
          ErrLine(Result, 'shell', E.Message);
          Result.ExitCode := ExitFailure;
        end;
      end;
      Result.Output := '';
    end;
  finally
    RedirectHandle.Free;
  end;
end;

procedure TSandboxShell.DispatchCommand(const ACommand: string;
  const AArgs: TStringArray; var AResult: TExecResult);
begin
  try
    if Assigned(FCommandHandler) and
       FCommandHandler(ACommand, AArgs, AResult) then
      Exit;

    if ACommand = 'pwd' then
      CmdPwd(AArgs, AResult)
    else if ACommand = 'cd' then
      CmdCd(AArgs, AResult)
    else if ACommand = 'ls' then
      CmdLs(AArgs, AResult)
    else if ACommand = 'mkdir' then
      CmdMkdir(AArgs, AResult)
    else if ACommand = 'touch' then
      CmdTouch(AArgs, AResult)
    else if ACommand = 'cat' then
      CmdCat(AArgs, AResult)
    else if ACommand = 'echo' then
      CmdEcho(AArgs, AResult)
    else if ACommand = 'rm' then
      CmdRm(AArgs, AResult)
    else if ACommand = 'cp' then
      CmdCp(AArgs, AResult)
    else if ACommand = 'mv' then
      CmdMv(AArgs, AResult)
    else if ACommand = 'stat' then
      CmdStat(AArgs, AResult)
    else if ACommand = 'tree' then
      CmdTree(AArgs, AResult)
    else if ACommand = 'df' then
      CmdDf(AArgs, AResult)
    else if ACommand = 'help' then
      CmdHelp(AArgs, AResult)
    else
    begin
      ErrLine(AResult, ACommand,
        'command not found (sandboxed shell - host commands are unreachable)');
      AResult.ExitCode := ExitUnknownCommand;
    end;
  except
    on E: ESandboxFsError do
    begin
      ErrLine(AResult, ACommand, E.Message);
      AResult.ExitCode := ExitFailure;
    end;
  end;
end;

procedure TSandboxShell.CmdPwd(const AArgs: TStringArray;
  var AResult: TExecResult);
begin
  if Length(AArgs) > 0 then
  begin
    UsageError(AResult, 'pwd', 'takes no arguments');
    Exit;
  end;
  OutLine(AResult, FCwd);
end;

procedure TSandboxShell.CmdCd(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Target: string;
begin
  if Length(AArgs) > 1 then
  begin
    UsageError(AResult, 'cd', 'too many arguments');
    Exit;
  end;
  if Length(AArgs) = 0 then
    Target := '/'
  else
    Target := FFs.Normalize(AArgs[0], FCwd);
  if not FFs.IsDirectory(Target) then
  begin
    if FFs.Exists(Target) then
      ErrLine(AResult, 'cd', Target + ': not a directory')
    else
      ErrLine(AResult, 'cd', Target + ': no such file or directory');
    AResult.ExitCode := ExitFailure;
    Exit;
  end;
  FCwd := Target;
end;

function TSandboxShell.FormatEntry(const AStat: TSandboxFsStat;
  const ALong: Boolean): string;
var
  DisplayName: string;
  KindChar: string;
begin
  DisplayName := AStat.Name;
  if AStat.Kind = nkDirectory then
    DisplayName := DisplayName + '/';
  if not ALong then
    Exit(DisplayName);
  if AStat.Kind = nkDirectory then
    KindChar := 'd'
  else
    KindChar := '-';
  Result := Format('%s %10d  %s  %s', [KindChar, AStat.Size,
    FormatDateTime('yyyy-mm-dd hh:nn', AStat.ModifiedAt),
    DisplayName]);
end;

procedure TSandboxShell.CmdLs(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Flags: string;
  Paths: TStringArray;
  Entries: TSandboxFsStatArray;
  Header: Boolean;
  Index: Integer;
  EntryIndex: Integer;
  Target: string;
  Info: TSandboxFsStat;
begin
  if not SplitFlags('ls', AArgs, 'l', Flags, Paths, AResult) then
    Exit;
  if Length(Paths) = 0 then
  begin
    SetLength(Paths, 1);
    Paths[0] := FCwd;
  end;
  Header := Length(Paths) > 1;
  for Index := 0 to High(Paths) do
  begin
    Target := FFs.Normalize(Paths[Index], FCwd);
    Info := FFs.Stat(Target);
    if Header then
    begin
      if Index > 0 then
        OutLine(AResult, '');
      OutLine(AResult, Target + ':');
    end;
    if Info.Kind = nkFile then
    begin
      Info.Name := Paths[Index];
      OutLine(AResult, FormatEntry(Info, Pos('l', Flags) > 0));
      Continue;
    end;
    Entries := FFs.List(Target);
    for EntryIndex := 0 to High(Entries) do
      OutLine(AResult,
        FormatEntry(Entries[EntryIndex], Pos('l', Flags) > 0));
  end;
end;

procedure TSandboxShell.CmdMkdir(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Flags: string;
  Paths: TStringArray;
  Index: Integer;
begin
  if not SplitFlags('mkdir', AArgs, 'p', Flags, Paths, AResult) then
    Exit;
  if Length(Paths) = 0 then
  begin
    UsageError(AResult, 'mkdir', 'missing operand');
    Exit;
  end;
  for Index := 0 to High(Paths) do
    FFs.MakeDirectory(FFs.Normalize(Paths[Index], FCwd),
      Pos('p', Flags) > 0);
end;

procedure TSandboxShell.CmdTouch(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Index: Integer;
begin
  if Length(AArgs) = 0 then
  begin
    UsageError(AResult, 'touch', 'missing operand');
    Exit;
  end;
  for Index := 0 to High(AArgs) do
    FFs.Touch(FFs.Normalize(AArgs[Index], FCwd));
end;

procedure TSandboxShell.CmdCat(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Index: Integer;
begin
  if Length(AArgs) = 0 then
  begin
    UsageError(AResult, 'cat', 'missing operand');
    Exit;
  end;
  for Index := 0 to High(AArgs) do
    try
      AResult.Output := AResult.Output +
        FFs.ReadAllText(FFs.Normalize(AArgs[Index], FCwd));
    except
      on E: ESandboxFsError do
      begin
        ErrLine(AResult, 'cat', E.Message);
        AResult.ExitCode := ExitFailure;
      end;
    end;
end;

procedure TSandboxShell.CmdEcho(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Flags: string;
  Words: TStringArray;
  Text: string;
  Index: Integer;
begin
  if not SplitFlags('echo', AArgs, 'n', Flags, Words, AResult) then
    Exit;
  Text := '';
  for Index := 0 to High(Words) do
  begin
    if Index > 0 then
      Text := Text + ' ';
    Text := Text + Words[Index];
  end;
  AResult.Output := AResult.Output + Text;
  if Pos('n', Flags) = 0 then
    AResult.Output := AResult.Output + LineEnding;
end;

procedure TSandboxShell.CmdRm(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Flags: string;
  Paths: TStringArray;
  Index: Integer;
begin
  if not SplitFlags('rm', AArgs, 'r', Flags, Paths, AResult) then
    Exit;
  if Length(Paths) = 0 then
  begin
    UsageError(AResult, 'rm', 'missing operand');
    Exit;
  end;
  for Index := 0 to High(Paths) do
    try
      FFs.DeletePath(FFs.Normalize(Paths[Index], FCwd),
        Pos('r', Flags) > 0);
    except
      on E: ESandboxFsError do
      begin
        ErrLine(AResult, 'rm', E.Message);
        AResult.ExitCode := ExitFailure;
      end;
    end;
end;

procedure TSandboxShell.CmdCp(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Flags: string;
  Paths: TStringArray;
begin
  if not SplitFlags('cp', AArgs, 'r', Flags, Paths, AResult) then
    Exit;
  if Length(Paths) <> 2 then
  begin
    UsageError(AResult, 'cp', 'expected: cp [-r] <source> <dest>');
    Exit;
  end;
  FFs.CopyPath(FFs.Normalize(Paths[0], FCwd),
    FFs.Normalize(Paths[1], FCwd), Pos('r', Flags) > 0);
end;

procedure TSandboxShell.CmdMv(const AArgs: TStringArray;
  var AResult: TExecResult);
begin
  if Length(AArgs) <> 2 then
  begin
    UsageError(AResult, 'mv', 'expected: mv <source> <dest>');
    Exit;
  end;
  FFs.MovePath(FFs.Normalize(AArgs[0], FCwd),
    FFs.Normalize(AArgs[1], FCwd));
end;

procedure TSandboxShell.CmdStat(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Info: TSandboxFsStat;
  SizeText: string;
begin
  if Length(AArgs) <> 1 then
  begin
    UsageError(AResult, 'stat', 'expected: stat <path>');
    Exit;
  end;
  Info := FFs.Stat(FFs.Normalize(AArgs[0], FCwd));
  if Info.Kind = nkDirectory then
    SizeText := Format('%d entries', [Info.Size])
  else
    SizeText := SandboxHumanBytes(Info.Size);
  OutLine(AResult, 'path:     ' + Info.Path);
  OutLine(AResult, 'kind:     ' + SandboxFsKindName(Info.Kind));
  OutLine(AResult, 'size:     ' + SizeText);
  OutLine(AResult, 'modified: ' +
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Info.ModifiedAt));
end;

procedure TSandboxShell.AppendTree(var AResult: TExecResult;
  const APath: string; const APrefix: string);
var
  Entries: TSandboxFsStatArray;
  Index: Integer;
  IsLast: Boolean;
  Connector: string;
  ChildPrefix: string;
  DisplayName: string;
begin
  Entries := FFs.List(APath);
  for Index := 0 to High(Entries) do
  begin
    IsLast := Index = High(Entries);
    if IsLast then
    begin
      Connector := '`-- ';
      ChildPrefix := APrefix + '    ';
    end
    else
    begin
      Connector := '|-- ';
      ChildPrefix := APrefix + '|   ';
    end;
    DisplayName := Entries[Index].Name;
    if Entries[Index].Kind = nkDirectory then
      DisplayName := DisplayName + '/';
    OutLine(AResult, APrefix + Connector + DisplayName);
    if Entries[Index].Kind = nkDirectory then
      AppendTree(AResult, Entries[Index].Path, ChildPrefix);
  end;
end;

procedure TSandboxShell.CmdTree(const AArgs: TStringArray;
  var AResult: TExecResult);
var
  Target: string;
begin
  if Length(AArgs) > 1 then
  begin
    UsageError(AResult, 'tree', 'expected: tree [path]');
    Exit;
  end;
  if Length(AArgs) = 0 then
    Target := FCwd
  else
    Target := FFs.Normalize(AArgs[0], FCwd);
  if not FFs.IsDirectory(Target) then
  begin
    ErrLine(AResult, 'tree', Target + ': not a directory');
    AResult.ExitCode := ExitFailure;
    Exit;
  end;
  OutLine(AResult, Target);
  AppendTree(AResult, Target, '');
end;

procedure TSandboxShell.CmdDf(const AArgs: TStringArray;
  var AResult: TExecResult);
begin
  if Length(AArgs) > 0 then
  begin
    UsageError(AResult, 'df', 'takes no arguments');
    Exit;
  end;
  OutLine(AResult, 'used:  ' + SandboxHumanBytes(FFs.UsedBytes));
  if FFs.QuotaBytes > 0 then
    OutLine(AResult, Format('quota: %s (%.1f%% used)',
      [SandboxHumanBytes(FFs.QuotaBytes),
      FFs.UsedBytes * 100.0 / FFs.QuotaBytes]))
  else
    OutLine(AResult, 'quota: unlimited');
  OutLine(AResult, Format('nodes: %d', [FFs.NodeCount]));
end;

procedure TSandboxShell.CmdHelp(const AArgs: TStringArray;
  var AResult: TExecResult);
begin
  OutLine(AResult, 'sandbox builtins:');
  OutLine(AResult, '  pwd                     print working directory');
  OutLine(AResult, '  cd [path]               change directory');
  OutLine(AResult, '  ls [-l] [path...]       list directory contents');
  OutLine(AResult, '  mkdir [-p] <path...>    create directories');
  OutLine(AResult, '  touch <path...>         create empty files / bump mtime');
  OutLine(AResult, '  cat <path...>           print file contents');
  OutLine(AResult, '  echo [-n] [words...]    print arguments');
  OutLine(AResult, '  rm [-r] <path...>       delete files / directories');
  OutLine(AResult, '  cp [-r] <src> <dest>    copy');
  OutLine(AResult, '  mv <src> <dest>         move / rename');
  OutLine(AResult, '  stat <path>             show metadata');
  OutLine(AResult, '  tree [path]             render the namespace');
  OutLine(AResult, '  df                      quota and node usage');
  OutLine(AResult,
    '  redirection: cmd > file (truncate), cmd >> file (append)');
end;

end.
