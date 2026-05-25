unit CLI.Parser;

{$I Shared.inc}

interface

uses
  Classes,

  CLI.Options;

{ Parses command-line arguments against the given option definitions.
  Returns a TStringList of positional (non-option) arguments; the caller
  owns the returned list.  Raises TParseError for unknown options. }
function ParseArguments(const AArgs: array of string;
  const AOptions: TOptionArray): TStringList;
function ParseCommandLine(const AOptions: TOptionArray): TStringList;

implementation

uses
  SysUtils;

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
          if (I >= High(AArgs)) or LooksLikeOptionToken(AArgs[I + 1]) then
            raise TParseError.CreateFmt(
              '--%s requires a value', [Name]);
          Inc(I);
          Value := AArgs[I];
        end;

        Option.Apply(Value);
      end
      else if (Length(Arg) = 2) and
              (Arg[1] = SHORT_FLAG_CHAR) and
              (Arg[2] <> SHORT_FLAG_CHAR) then
      begin
        Option := FindOptionShort(AOptions, Arg[2]);
        if Option = nil then
          raise TParseError.CreateFmt('Unknown option: %s', [Arg]);
        Option.Apply('');
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

function ParseCommandLine(const AOptions: TOptionArray): TStringList;
var
  Args: array of string;
  I: Integer;
begin
  SetLength(Args, ParamCount);
  for I := 0 to High(Args) do
    Args[I] := ParamStr(I + 1);
  Result := ParseArguments(Args, AOptions);
end;

end.
