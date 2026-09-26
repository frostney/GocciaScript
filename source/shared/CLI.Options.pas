unit CLI.Options;

{$I Shared.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,
  TypInfo;

const
  { The release that removed the pre-ADR-0122 flags; removed-option errors
    name it. }
  OPTIONS_REMOVED_IN_VERSION = '0.14.0';
  { Exit status for an unusable invocation (TCLIUsageError, and the
    no-argument rule); script and run failures exit 1. }
  EXIT_CODE_USAGE = 2;

type
  TParseError = class(Exception);
  { The invocation itself is unusable: a removed option, a value given to a
    flag, a capability the binary cannot grant, or a command-line-only key in
    a config file. Applications exit with status 2 for it. }
  TCLIUsageError = class(TParseError);

  { An option value its parser rejected, with the parts a config-file
    message restates in config spelling. }
  EOptionValueError = class(TParseError)
  private
    FValue: string;
    FReason: string;
  public
    constructor CreateValue(const AOptionName, AValue, AReason: string);
    property Value: string read FValue;
    property Reason: string read FReason;
  end;

  TOptionBase = class
  private
    FLongName: string;
    FShortName: string;
    FConfigName: string;
    FHelpText: string;
    FGroup: string;
    FPresent: Boolean;
    FFromCommandLine: Boolean;
    FHidden: Boolean;
    FCommandLineOnly: Boolean;
    FRequiresTrust: Boolean;
    FConfigIgnored: Boolean;
    FAcceptsObject: Boolean;
    FConfigHint: string;
  public
    constructor Create(const ALongName, AHelpText: string; const AGroup: string = '');

    procedure Apply(const AValue: string); virtual; abstract;
    { Applies a command-line occurrence. AHasEquals distinguishes `--x=` from
      `--x`, which both arrive with an empty AValue. The default forwards to
      Apply. }
    procedure ApplyExplicit(const AValue: string;
      const AHasEquals: Boolean); virtual;
    { Raises TParseError when AValue is not a valid value, without applying
      it. Used to validate config files read through lookups rather than
      applied. The default accepts every value. }
    procedure CheckValue(const AValue: string); virtual;
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
    { Omitted from --help. Still parsed. }
    property Hidden: Boolean read FHidden write FHidden;
    { A config file that sets this key is a usage error. }
    property CommandLineOnly: Boolean read FCommandLineOnly
      write FCommandLineOnly;
    { Appended to the command-line-only error, e.g. where the config form
      lives instead. }
    property ConfigHint: string read FConfigHint write FConfigHint;
    { Skipped when a config file is applied: the value only takes effect once
      the config is trusted (ADR 0122). }
    property RequiresTrust: Boolean read FRequiresTrust write FRequiresTrust;
    { Config files may name the key, but the binary does not use it: it is
      neither validated nor applied (a limit a binary does not honor). }
    property ConfigIgnored: Boolean read FConfigIgnored write FConfigIgnored;
    { A config file may give the key an object, read by the option's owner
      (a virtual modules descriptor map). Any other option rejects one. }
    property AcceptsObject: Boolean read FAcceptsObject write FAcceptsObject;
  end;

  TOptionArray = array of TOptionBase;

  TFlagOption = class(TOptionBase)
  public
    procedure Apply(const AValue: string); override;
    { `--flag=anything`, including `--flag=` and `--flag=false`, is a usage
      error: a flag is either given or not. }
    procedure ApplyExplicit(const AValue: string;
      const AHasEquals: Boolean); override;
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
    FMaximum: Int64;
  protected
    { Parses AValue or raises TParseError; the unit-aware subclasses override
      it. Enforces Maximum. }
    function ParseValue(const AValue: string): Int64; virtual;
    procedure RaiseInvalidValue(const AValue, AReason: string);
  public
    procedure Apply(const AValue: string); override;
    procedure CheckValue(const AValue: string); override;
    function ConsumesSeparateValue: Boolean; override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: Int64): Int64;
    { Parses a value the way Apply does, for a value read from a config
      entry rather than applied. }
    function Parse(const AValue: string): Int64;

    property Value: Int64 read FValue;
    { The largest accepted value; 0 means no bound beyond Int64. }
    property Maximum: Int64 read FMaximum write FMaximum;
  end;

  { A byte size: a whole number of bytes, or KiB/MiB/GiB (CLI.Units). }
  TByteSizeOption = class(TInt64Option)
  protected
    function ParseValue(const AValue: string): Int64; override;
  public
    function FormatForHelp: string; override;
  end;

  { A duration in milliseconds: plain milliseconds, or ms/s/m (CLI.Units). }
  TDurationOption = class(TInt64Option)
  protected
    function ParseValue(const AValue: string): Int64; override;
  public
    function FormatForHelp: string; override;
    function Milliseconds(const ADefault: Integer): Integer;
  end;

  { A non-negative whole number. }
  TCountOption = class(TInt64Option)
  protected
    function ParseValue(const AValue: string): Int64; override;
  end;

  { `--name[=scope,...]`: an unscoped occurrence, a comma-separated scope
    list, or both, accumulated across repeats. The option never consumes the
    next argument, so `--allow-read foo.js` is an unscoped grant followed by
    an input file. }
  TScopeListOption = class(TOptionBase)
  private
    FScopePlaceholder: string;
    FRequiresScope: Boolean;
    FUnscopedMeaning: string;
    FScopeRequirement: string;
    FUnscoped: Boolean;
    FScopes: TStringList;
  public
    { AUnscopedMeaning completes the empty-list error: `omit "=" to ...`.
      AScopeRequirement completes the missing-scope error of an option that
      requires a scope: `--x needs a scope: ...`. }
    constructor Create(const ALongName, AHelpText, AScopePlaceholder: string;
      const AGroup: string = ''; const ARequiresScope: Boolean = False;
      const AUnscopedMeaning: string = '';
      const AScopeRequirement: string = '');
    destructor Destroy; override;

    procedure Apply(const AValue: string); override;
    procedure ApplyExplicit(const AValue: string;
      const AHasEquals: Boolean); override;
    function FormatForHelp: string; override;

    property Unscoped: Boolean read FUnscoped;
    property Scopes: TStringList read FScopes;
    property RequiresScope: Boolean read FRequiresScope;
    property ScopePlaceholder: string read FScopePlaceholder;
  end;

  { An option that no longer exists. It stays parseable, hidden from help,
    so using it fails with a message naming its replacement rather than an
    "unknown option" error. AConfigName is its config key ('' when it never
    had one). }
  TRemovedOption = class(TOptionBase)
  private
    FFlagReplacement: string;
    FConfigReplacement: string;
  public
    constructor Create(const ALongName, AConfigName, AFlagReplacement,
      AConfigReplacement: string);
    procedure Apply(const AValue: string); override;
    procedure ApplyExplicit(const AValue: string;
      const AHasEquals: Boolean); override;
    function FormatForHelp: string; override;
    function RemovedFlagMessage: string;
    function RemovedConfigMessage(const AConfigPath: string): string;
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

uses
  CLI.Units;

{ EOptionValueError }

constructor EOptionValueError.CreateValue(const AOptionName, AValue,
  AReason: string);
begin
  inherited CreateFmt('Invalid value for --%s: %s (%s)',
    [AOptionName, AValue, AReason]);
  FValue := AValue;
  FReason := AReason;
end;

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
  FHidden := False;
  FCommandLineOnly := False;
  FRequiresTrust := False;
  FConfigIgnored := False;
  FAcceptsObject := False;
  FConfigHint := '';
end;

procedure TOptionBase.CheckValue(const AValue: string);
begin
end;

procedure TOptionBase.ApplyExplicit(const AValue: string;
  const AHasEquals: Boolean);
begin
  Apply(AValue);
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

procedure TFlagOption.ApplyExplicit(const AValue: string;
  const AHasEquals: Boolean);
begin
  if AHasEquals then
    raise TCLIUsageError.CreateFmt(
      '--%s does not take a value; got "%s". Omit the flag to leave it off',
      [LongName, AValue]);
  Apply(AValue);
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

procedure TInt64Option.RaiseInvalidValue(const AValue, AReason: string);
begin
  raise EOptionValueError.CreateValue(LongName, AValue, AReason);
end;

function TInt64Option.ParseValue(const AValue: string): Int64;
begin
  if not TryStrToInt64(AValue, Result) then
    raise TParseError.CreateFmt('Invalid integer value for --%s: %s',
      [LongName, AValue]);
  if (FMaximum > 0) and (Result > FMaximum) then
    RaiseInvalidValue(AValue, TOO_LARGE_ERROR);
end;

function TInt64Option.Parse(const AValue: string): Int64;
begin
  Result := ParseValue(AValue);
end;

procedure TInt64Option.Apply(const AValue: string);
begin
  FValue := ParseValue(AValue);
  FPresent := True;
end;

procedure TInt64Option.CheckValue(const AValue: string);
begin
  ParseValue(AValue);
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

{ TByteSizeOption }

function TByteSizeOption.ParseValue(const AValue: string): Int64;
var
  ErrorText: string;
begin
  if not TryParseByteSize(AValue, Result, ErrorText) then
    RaiseInvalidValue(AValue, ErrorText);
  if (Maximum > 0) and (Result > Maximum) then
    RaiseInvalidValue(AValue, TOO_LARGE_ERROR);
end;

function TByteSizeOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<bytes>';
end;

{ TDurationOption }

function TDurationOption.ParseValue(const AValue: string): Int64;
var
  ErrorText: string;
begin
  if not TryParseDurationMilliseconds(AValue, Result, ErrorText) then
    RaiseInvalidValue(AValue, ErrorText);
  if (Maximum > 0) and (Result > Maximum) then
    RaiseInvalidValue(AValue, TOO_LARGE_ERROR);
end;

function TDurationOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<duration>';
end;

{ TryParseDurationMilliseconds caps a duration at High(Integer), so the
  narrowing is lossless. }
function TDurationOption.Milliseconds(const ADefault: Integer): Integer;
begin
  if FPresent then
    Result := Integer(FValue)
  else
    Result := ADefault;
end;

{ TCountOption }

function TCountOption.ParseValue(const AValue: string): Int64;
var
  ErrorText: string;
begin
  if not TryParseNonNegativeCount(AValue, Result, ErrorText) then
    RaiseInvalidValue(AValue, ErrorText);
  if (Maximum > 0) and (Result > Maximum) then
    RaiseInvalidValue(AValue, TOO_LARGE_ERROR);
end;

{ TScopeListOption }

constructor TScopeListOption.Create(const ALongName, AHelpText,
  AScopePlaceholder: string; const AGroup: string;
  const ARequiresScope: Boolean; const AUnscopedMeaning: string;
  const AScopeRequirement: string);
begin
  inherited Create(ALongName, AHelpText, AGroup);
  FScopePlaceholder := AScopePlaceholder;
  FRequiresScope := ARequiresScope;
  FUnscopedMeaning := AUnscopedMeaning;
  FScopeRequirement := AScopeRequirement;
  FUnscoped := False;
  FScopes := TStringList.Create;
end;

destructor TScopeListOption.Destroy;
begin
  FScopes.Free;
  inherited Destroy;
end;

procedure TScopeListOption.Apply(const AValue: string);
begin
  ApplyExplicit(AValue, AValue <> '');
end;

procedure TScopeListOption.ApplyExplicit(const AValue: string;
  const AHasEquals: Boolean);
var
  Items: TStringList;
  I: Integer;
begin
  if (not AHasEquals) or (FRequiresScope and (AValue = '')) then
  begin
    if FRequiresScope then
    begin
      if FScopeRequirement <> '' then
        raise TParseError.CreateFmt('--%s needs a scope: %s',
          [LongName, FScopeRequirement]);
      raise TParseError.CreateFmt('--%s needs a scope', [LongName]);
    end;
    FUnscoped := True;
    FPresent := True;
    Exit;
  end;

  if AValue = '' then
  begin
    if FUnscopedMeaning = '' then
      raise TParseError.CreateFmt('--%s= has an empty scope list',
        [LongName]);
    raise TParseError.CreateFmt(
      '--%s= has an empty scope list; omit "=" to %s',
      [LongName, FUnscopedMeaning]);
  end;

  Items := TStringList.Create;
  try
    Items.StrictDelimiter := True;
    Items.Delimiter := ',';
    Items.QuoteChar := #0;
    Items.DelimitedText := AValue;
    { DelimitedText drops a trailing empty item, so count separators. }
    if (AValue[Length(AValue)] = ',') or (AValue[1] = ',') then
      raise TParseError.CreateFmt('Empty scope in --%s=%s',
        [LongName, AValue]);
    for I := 0 to Items.Count - 1 do
      if Items[I] = '' then
        raise TParseError.CreateFmt('Empty scope in --%s=%s',
          [LongName, AValue]);
    FScopes.AddStrings(Items);
  finally
    Items.Free;
  end;
  FPresent := True;
end;

function TScopeListOption.FormatForHelp: string;
begin
  if FRequiresScope then
    Result := '--' + LongName + '=' + FScopePlaceholder + ',...'
  else
    Result := '--' + LongName + '[=' + FScopePlaceholder + ',...]';
end;

{ TRemovedOption }

constructor TRemovedOption.Create(const ALongName, AConfigName,
  AFlagReplacement, AConfigReplacement: string);
begin
  inherited Create(ALongName, '');
  if AConfigName <> ALongName then
    ConfigName := AConfigName;
  FFlagReplacement := AFlagReplacement;
  FConfigReplacement := AConfigReplacement;
  Hidden := True;
end;

procedure TRemovedOption.Apply(const AValue: string);
begin
  raise TCLIUsageError.Create(RemovedFlagMessage);
end;

procedure TRemovedOption.ApplyExplicit(const AValue: string;
  const AHasEquals: Boolean);
begin
  raise TCLIUsageError.Create(RemovedFlagMessage);
end;

function TRemovedOption.FormatForHelp: string;
begin
  Result := '--' + LongName;
end;

function TRemovedOption.RemovedFlagMessage: string;
begin
  Result := Format('--%s was removed in GocciaScript %s; %s',
    [LongName, OPTIONS_REMOVED_IN_VERSION, FFlagReplacement]);
end;

function TRemovedOption.RemovedConfigMessage(
  const AConfigPath: string): string;
var
  Key: string;
begin
  if ConfigName <> '' then
    Key := ConfigName
  else
    Key := LongName;
  Result := Format('%s: "%s" was removed in GocciaScript %s; %s',
    [AConfigPath, Key, OPTIONS_REMOVED_IN_VERSION, FConfigReplacement]);
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
