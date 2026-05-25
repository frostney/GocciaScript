unit CLI.Options;

{$I Shared.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,
  TypInfo;

type
  TParseError = class(Exception);

  TOptionBase = class
  private
    FLongName: string;
    FShortName: string;
    FConfigName: string;
    FHelpText: string;
    FGroup: string;
    FPresent: Boolean;
    FFromCommandLine: Boolean;
  public
    constructor Create(const ALongName, AHelpText: string; const AGroup: string = '');

    procedure Apply(const AValue: string); virtual; abstract;
    function ConsumesSeparateValue: Boolean; virtual;
    function FormatForHelp: string; virtual; abstract;
    function ValidValues: string; virtual;

    { Mark this option as present without applying a value.
      Used by the config layer to record that a key existed in the
      config file even when no concrete values were produced (e.g.
      an empty array). }
    procedure MarkPresent; virtual;

    { Mark this option as having been set by the command line.
      Called after ParseCommandLine so that per-file config can
      distinguish CLI-set values from root config values. }
    procedure MarkFromCommandLine;

    property LongName: string read FLongName;
    property ShortName: string read FShortName write FShortName;
    { Alternate name used in config files when different from LongName.
      When empty, config files use LongName as usual. }
    property ConfigName: string read FConfigName write FConfigName;
    property HelpText: string read FHelpText;
    property Group: string read FGroup;
    property Present: Boolean read FPresent;
    property FromCommandLine: Boolean read FFromCommandLine;
  end;

  TOptionArray = array of TOptionBase;

  TFlagOption = class(TOptionBase)
  public
    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;
  end;

  TStringOption = class(TOptionBase)
  private
    FValue: string;
  public
    procedure MarkPresent; override;
    procedure Apply(const AValue: string); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: string): string;

    property Value: string read FValue;
  end;

  TOptionalStringOption = class(TStringOption)
  public
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;
  end;

  TIntegerOption = class(TOptionBase)
  private
    FValue: Integer;
  public
    procedure Apply(const AValue: string); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: Integer): Integer;

    property Value: Integer read FValue;
  end;

  TInt64Option = class(TOptionBase)
  private
    FValue: Int64;
  public
    procedure Apply(const AValue: string); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: Int64): Int64;

    property Value: Int64 read FValue;
  end;

  TRepeatableOption = class(TOptionBase)
  private
    FValues: TStringList;
  public
    constructor Create(const ALongName, AHelpText: string; const AGroup: string = '');
    destructor Destroy; override;

    procedure Apply(const AValue: string); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;

    property Values: TStringList read FValues;
  end;

  TEnumOption<T> = class(TOptionBase)
  private
    FOrdinal: Integer;
    FPrefixLength: Integer;
    function JoinStrippedNames(const ASeparator: string): string;
  public
    constructor Create(const ALongName, AHelpText: string; const AGroup: string = '';
      const APrefixLength: Integer = 2);

    procedure Apply(const AValue: string); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;
    function ValidValues: string; override;

    function Value: T;
    function ValueOr(const ADefault: T): T;
    function Matches(const AValue: T): Boolean;
  end;

  TOptionBaseList = TObjectList<TOptionBase>;

  TOptionList = class
  private
    FItems: TOptionBaseList;
  public
    constructor Create;
    destructor Destroy; override;

    function AddFlag(const ALongName, AHelpText: string;
      const AGroup: string = ''): TFlagOption;
    function AddString(const ALongName, AHelpText: string;
      const AGroup: string = ''): TStringOption;
    function AddInteger(const ALongName, AHelpText: string;
      const AGroup: string = ''): TIntegerOption;
    function AddRepeatable(const ALongName, AHelpText: string;
      const AGroup: string = ''): TRepeatableOption;
    function Add(const AOption: TOptionBase): TOptionBase;

    function Options: TOptionArray;
  end;

function ConcatOptions(const AArrays: array of TOptionArray): TOptionArray;

implementation

{ ConcatOptions }

function ConcatOptions(const AArrays: array of TOptionArray): TOptionArray;
var
  TotalLength: Integer;
  I, J, Offset: Integer;
begin
  TotalLength := 0;
  for I := 0 to High(AArrays) do
    TotalLength := TotalLength + Length(AArrays[I]);

  SetLength(Result, TotalLength);
  Offset := 0;
  for I := 0 to High(AArrays) do
    for J := 0 to High(AArrays[I]) do
    begin
      Result[Offset] := AArrays[I][J];
      Inc(Offset);
    end;
end;

{ TOptionBase }

constructor TOptionBase.Create(const ALongName, AHelpText: string;
  const AGroup: string);
begin
  inherited Create;
  FLongName := ALongName;
  FShortName := '';
  FConfigName := '';
  FHelpText := AHelpText;
  FGroup := AGroup;
  FPresent := False;
  FFromCommandLine := False;
end;

procedure TOptionBase.MarkFromCommandLine;
begin
  FFromCommandLine := True;
end;

procedure TOptionBase.MarkPresent;
begin
  FPresent := True;
end;

function TOptionBase.ValidValues: string;
begin
  Result := '';
end;

function TOptionBase.ConsumesSeparateValue: Boolean;
begin
  Result := False;
end;

{ TFlagOption }

procedure TFlagOption.Apply(const AValue: string);
begin
  FPresent := True;
end;

function TFlagOption.FormatForHelp: string;
begin
  Result := '--' + LongName;
end;

{ TStringOption }

procedure TStringOption.MarkPresent;
begin
  FValue := '';
  inherited MarkPresent;
end;

procedure TStringOption.Apply(const AValue: string);
begin
  FValue := AValue;
  FPresent := True;
end;

function TStringOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<value>';
end;

function TStringOption.ValueOr(const ADefault: string): string;
begin
  if FPresent then
    Result := FValue
  else
    Result := ADefault;
end;

function TStringOption.ConsumesSeparateValue: Boolean;
begin
  Result := True;
end;

{ TOptionalStringOption }

function TOptionalStringOption.ConsumesSeparateValue: Boolean;
begin
  Result := False;
end;

function TOptionalStringOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '[=<value>]';
end;

{ TIntegerOption }

procedure TIntegerOption.Apply(const AValue: string);
var
  Parsed: Integer;
begin
  if not TryStrToInt(AValue, Parsed) then
    raise TParseError.CreateFmt('Invalid integer value for --%s: %s',
      [LongName, AValue]);
  FValue := Parsed;
  FPresent := True;
end;

function TIntegerOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<N>';
end;

function TIntegerOption.ValueOr(const ADefault: Integer): Integer;
begin
  if FPresent then
    Result := FValue
  else
    Result := ADefault;
end;

function TIntegerOption.ConsumesSeparateValue: Boolean;
begin
  Result := True;
end;

{ TInt64Option }

procedure TInt64Option.Apply(const AValue: string);
var
  Parsed: Int64;
begin
  if not TryStrToInt64(AValue, Parsed) then
    raise TParseError.CreateFmt('Invalid integer value for --%s: %s',
      [LongName, AValue]);
  FValue := Parsed;
  FPresent := True;
end;

function TInt64Option.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<N>';
end;

function TInt64Option.ValueOr(const ADefault: Int64): Int64;
begin
  if FPresent then
    Result := FValue
  else
    Result := ADefault;
end;

function TInt64Option.ConsumesSeparateValue: Boolean;
begin
  Result := True;
end;

{ TRepeatableOption }

constructor TRepeatableOption.Create(const ALongName, AHelpText: string;
  const AGroup: string);
begin
  inherited Create(ALongName, AHelpText, AGroup);
  FValues := TStringList.Create;
end;

destructor TRepeatableOption.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TRepeatableOption.Apply(const AValue: string);
begin
  FValues.Add(AValue);
  FPresent := True;
end;

function TRepeatableOption.FormatForHelp: string;
begin
  Result := '--' + LongName + ' <value>';
end;

function TRepeatableOption.ConsumesSeparateValue: Boolean;
begin
  Result := True;
end;

{ TEnumOption<T> }

constructor TEnumOption<T>.Create(const ALongName, AHelpText: string;
  const AGroup: string; const APrefixLength: Integer);
begin
  inherited Create(ALongName, AHelpText, AGroup);
  FOrdinal := 0;
  FPrefixLength := APrefixLength;
end;

function TEnumOption<T>.JoinStrippedNames(
  const ASeparator: string): string;
var
  TypeData: PTypeData;
  I: Integer;
  EnumName: string;
  Stripped: string;
begin
  TypeData := GetTypeData(TypeInfo(T));
  Result := '';
  for I := TypeData^.MinValue to TypeData^.MaxValue do
  begin
    EnumName := GetEnumName(TypeInfo(T), I);
    Stripped := LowerCase(Copy(EnumName, FPrefixLength + 1,
      Length(EnumName) - FPrefixLength));
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + Stripped;
  end;
end;

procedure TEnumOption<T>.Apply(const AValue: string);
var
  TypeData: PTypeData;
  I: Integer;
  EnumName: string;
  LowerValue: string;
begin
  TypeData := GetTypeData(TypeInfo(T));
  LowerValue := LowerCase(AValue);

  for I := TypeData^.MinValue to TypeData^.MaxValue do
  begin
    EnumName := GetEnumName(TypeInfo(T), I);
    if LowerCase(Copy(EnumName, FPrefixLength + 1,
       Length(EnumName) - FPrefixLength)) = LowerValue then
    begin
      FOrdinal := I;
      FPresent := True;
      Exit;
    end;
  end;

  raise TParseError.CreateFmt('Invalid value for --%s: %s (valid: %s)',
    [LongName, AValue, JoinStrippedNames(', ')]);
end;

function TEnumOption<T>.Value: T;
begin
  Move(FOrdinal, Result, SizeOf(T));
end;

function TEnumOption<T>.ValueOr(const ADefault: T): T;
begin
  if FPresent then
    Result := Value
  else
    Result := ADefault;
end;

function TEnumOption<T>.Matches(const AValue: T): Boolean;
var
  OrdinalValue: Integer;
begin
  OrdinalValue := 0;
  Move(AValue, OrdinalValue, SizeOf(T));
  Result := FPresent and (FOrdinal = OrdinalValue);
end;

function TEnumOption<T>.ValidValues: string;
begin
  Result := JoinStrippedNames(', ');
end;

function TEnumOption<T>.FormatForHelp: string;
begin
  Result := '--' + LongName + '=' + JoinStrippedNames('|');
end;

function TEnumOption<T>.ConsumesSeparateValue: Boolean;
begin
  Result := True;
end;

{ TOptionList }

constructor TOptionList.Create;
begin
  inherited Create;
  FItems := TOptionBaseList.Create(True);
end;

destructor TOptionList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TOptionList.AddFlag(const ALongName, AHelpText: string;
  const AGroup: string): TFlagOption;
begin
  Result := TFlagOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TOptionList.AddString(const ALongName, AHelpText: string;
  const AGroup: string): TStringOption;
begin
  Result := TStringOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TOptionList.AddInteger(const ALongName, AHelpText: string;
  const AGroup: string): TIntegerOption;
begin
  Result := TIntegerOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TOptionList.AddRepeatable(const ALongName, AHelpText: string;
  const AGroup: string): TRepeatableOption;
begin
  Result := TRepeatableOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TOptionList.Add(const AOption: TOptionBase): TOptionBase;
begin
  FItems.Add(AOption);
  Result := AOption;
end;

function TOptionList.Options: TOptionArray;
var
  I: Integer;
begin
  SetLength(Result, FItems.Count);
  for I := 0 to FItems.Count - 1 do
    Result[I] := FItems[I];
end;

end.
