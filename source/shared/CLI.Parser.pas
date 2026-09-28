unit CLI.Parser;

{$I Shared.inc}

interface

uses
  Classes,

  CLI.Options;

type
  TCommandLineArguments = array of string;

{ Parses command-line arguments against the given option definitions.
  Returns a TStringList of positional (non-option) arguments; the caller
  owns the returned list.  Raises TParseError for unknown options. }
function ParseArguments(const AArgs: array of string;
  const AOptions: TOptionArray): TStringList;
function GetCommandLineArguments: TCommandLineArguments;
function ParseCommandLine(const AOptions: TOptionArray): TStringList;

implementation

uses
  SysUtils
  {$IFDEF MSWINDOWS},
  Windows{$ENDIF};

{$IFDEF MSWINDOWS}
type
  PWideCharPointer = ^PWideChar;

function CommandLineToArgvW(ACommandLine: PWideChar;
  AArgumentCount: PInteger): PWideCharPointer; stdcall;
  external 'shell32' name 'CommandLineToArgvW';
{$ENDIF}

const
  LONG_FLAG_PREFIX = '--';
  SHORT_FLAG_CHAR = '-';
  FLAG_VALUE_SEPARATOR = '=';

procedure SplitFlag(const AArg: string; out AName, AValue: string);
var
  EqualPos: Integer;
  Body: string;
begin
  Body := Copy(AArg, Length(LONG_FLAG_PREFIX) + 1, MaxInt);
  EqualPos := Pos(FLAG_VALUE_SEPARATOR, Body);
  if EqualPos > 0 then
  begin
    AName := Copy(Body, 1, EqualPos - 1);
    AValue := Copy(Body, EqualPos + 1, MaxInt);
  end
  else
  begin
    AName := Body;
    AValue := '';
  end;
end;

function FindOption(const AOptions: TOptionArray;
  const AName: string): TOptionBase;
var
  I: Integer;
begin
  for I := 0 to High(AOptions) do
    if AOptions[I].LongName = AName then
      Exit(AOptions[I]);
  Result := nil;
end;

function FindOptionShort(const AOptions: TOptionArray;
  const AShortName: Char): TOptionBase;
var
  I: Integer;
begin
  for I := 0 to High(AOptions) do
    if (AOptions[I].ShortName <> '') and (AOptions[I].ShortName[1] = AShortName) then
      Exit(AOptions[I]);
  Result := nil;
end;

function LooksLikeOptionToken(const AArg: string): Boolean;
begin
  if Copy(AArg, 1, Length(LONG_FLAG_PREFIX)) = LONG_FLAG_PREFIX then
    Exit(True);

  Result := (Length(AArg) = 2) and
    (AArg[1] = SHORT_FLAG_CHAR) and
    not (AArg[2] in ['0'..'9']);
end;

{ Whether AArg is a short option that takes a value, spelled with the value
  attached (`-j2`, `-j=2`). A flag's short name never matches, so `-P=1`
  stays the usage error ParseArguments reports for it. }
function IsAttachedShortOptionValue(const AArg: string;
  const AOptions: TOptionArray; out AOption: TOptionBase): Boolean;
begin
  AOption := nil;
  if (Length(AArg) <= 2) or (AArg[1] <> SHORT_FLAG_CHAR) or
     (AArg[2] = SHORT_FLAG_CHAR) then
    Exit(False);
  AOption := FindOptionShort(AOptions, AArg[2]);
  Result := Assigned(AOption) and AOption.ConsumesSeparateValue;
  if not Result then
    AOption := nil;
end;

{ Whether the option at AIndex has no separate value to take: it is the last
  argument, or the next one is an option of its own (`--name`, `-x`, `-j2`). }
function MissingSeparateValue(const AArgs: array of string;
  const AIndex: Integer; const AOptions: TOptionArray): Boolean;
var
  AttachedOption: TOptionBase;
begin
  Result := (AIndex >= High(AArgs)) or
    LooksLikeOptionToken(AArgs[AIndex + 1]) or
    IsAttachedShortOptionValue(AArgs[AIndex + 1], AOptions, AttachedOption);
end;

function ParseArguments(const AArgs: array of string;
  const AOptions: TOptionArray): TStringList;
var
  I: Integer;
  Arg, Name, Value: string;
  Option: TOptionBase;
  HasEquals: Boolean;
begin
  Result := TStringList.Create;
  try
    I := 0;
    while I <= High(AArgs) do
    begin
      Arg := AArgs[I];

      if Copy(Arg, 1, Length(LONG_FLAG_PREFIX)) = LONG_FLAG_PREFIX then
      begin
        HasEquals := Pos(FLAG_VALUE_SEPARATOR,
          Copy(Arg, Length(LONG_FLAG_PREFIX) + 1, MaxInt)) > 0;
        SplitFlag(Arg, Name, Value);
        Option := FindOption(AOptions, Name);
        if Option = nil then
          raise TParseError.CreateFmt('Unknown option: --%s', [Name]);

        if (Value = '') and (not HasEquals) and
           Option.ConsumesSeparateValue then
        begin
          if MissingSeparateValue(AArgs, I, AOptions) then
            raise TParseError.CreateFmt(
              '--%s requires a value', [Name]);
          Inc(I);
          Value := AArgs[I];
        end;

        Option.ApplyExplicit(Value, HasEquals);
      end
      else if (Length(Arg) > 2) and (Arg[1] = SHORT_FLAG_CHAR) and
              (Arg[3] = FLAG_VALUE_SEPARATOR) and
              (FindOptionShort(AOptions, Arg[2]) is TFlagOption) then
        { `-P=1`: a short flag given a value, the same mistake as
          `--flag=value`, rather than an input path. }
        raise TCLIUsageError.CreateFmt(
          '-%s does not take a value; got "%s". Omit the flag to leave it off',
          [Arg[2], Copy(Arg, 4, MaxInt)])
      else if (Length(Arg) = 2) and
              (Arg[1] = SHORT_FLAG_CHAR) and
              (Arg[2] <> SHORT_FLAG_CHAR) then
      begin
        Option := FindOptionShort(AOptions, Arg[2]);
        if Option = nil then
          raise TParseError.CreateFmt('Unknown option: %s', [Arg]);
        if Option.ConsumesSeparateValue then
        begin
          { `-j 2`: a valued short option takes the next argument, as its
            long form does. }
          if MissingSeparateValue(AArgs, I, AOptions) then
            raise TParseError.CreateFmt('%s requires a value', [Arg]);
          Inc(I);
          Option.ApplyExplicit(AArgs[I], False);
        end
        else
          Option.Apply('');
      end
      else if IsAttachedShortOptionValue(Arg, AOptions, Option) then
      begin
        { `-j2` or `-j=2`: a valued short option with its value attached. }
        Value := Copy(Arg, 3, MaxInt);
        if Value[1] = FLAG_VALUE_SEPARATOR then
          Delete(Value, 1, 1);
        Option.ApplyExplicit(Value, False);
      end
      else
        Result.Add(Arg);

      Inc(I);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function GetCommandLineArguments: TCommandLineArguments;
{$IFDEF MSWINDOWS}
var
  ArgumentCount, I: Integer;
  ArgumentList, ArgumentPointer: PWideCharPointer;
begin
  ArgumentList := CommandLineToArgvW(GetCommandLineW, @ArgumentCount);
  if not Assigned(ArgumentList) then
    RaiseLastOSError;
  try
    if ArgumentCount <= 1 then
      Exit;

    SetLength(Result, ArgumentCount - 1);
    ArgumentPointer := ArgumentList;
    Inc(ArgumentPointer);
    for I := 0 to High(Result) do
    begin
      Result[I] := string(string(ArgumentPointer^));
      Inc(ArgumentPointer);
    end;
  finally
    LocalFree(HLOCAL(ArgumentList));
  end;
end;
{$ELSE}
var
  I: Integer;
begin
  SetLength(Result, ParamCount);
  for I := 0 to High(Result) do
    Result[I] := ParamStr(I + 1);
end;
{$ENDIF}

function ParseCommandLine(const AOptions: TOptionArray): TStringList;
var
  Arguments: TCommandLineArguments;
begin
  Arguments := GetCommandLineArguments;
  Result := ParseArguments(Arguments, AOptions);
end;

end.
