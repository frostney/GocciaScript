unit CLI.Options;

{$I Shared.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,
  TypInfo;

type
  TGocciaParseError = class(Exception);

  TGocciaOptionBase = class
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
    function FormatForHelp: string; virtual; abstract;
    function ValidValues: string; virtual;

    { Mark this option as having been set by the command line.
      Called after ParseCommandLine so that per-file config can
      distinguish CLI-set values from root-config-set values. }
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

  TGocciaOptionArray = array of TGocciaOptionBase;

  TGocciaFlagOption = class(TGocciaOptionBase)
  public
    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;
  end;

  TGocciaStringOption = class(TGocciaOptionBase)
  private
    FValue: string;
  public
    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: string): string;

    property Value: string read FValue;
  end;

  TGocciaIntegerOption = class(TGocciaOptionBase)
  private
    FValue: Integer;
  public
    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: Integer): Integer;

    property Value: Integer read FValue;
  end;

  TGocciaInt64Option = class(TGocciaOptionBase)
  private
    FValue: Int64;
  public
    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;

    function ValueOr(const ADefault: Int64): Int64;

    property Value: Int64 read FValue;
  end;

  TGocciaRepeatableOption = class(TGocciaOptionBase)
  private
    FValues: TStringList;
  public
    constructor Create(const ALongName, AHelpText: string; const AGroup: string = '');
    destructor Destroy; override;

    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;

    property Values: TStringList read FValues;
  end;

  TGocciaEnumOption<T> = class(TGocciaOptionBase)
  private
    FOrdinal: Integer;
    FPrefixLength: Integer;
    function JoinStrippedNames(const ASeparator: string): string;
  public
    constructor Create(const ALongName, AHelpText: string; const AGroup: string = '';
      const APrefixLength: Integer = 2);

    procedure Apply(const AValue: string); override;
    function FormatForHelp: string; override;
    function ValidValues: string; override;

    function Value: T;
    function ValueOr(const ADefault: T): T;
    function Matches(const AValue: T): Boolean;
  end;

  TGocciaOptionBaseList = TObjectList<TGocciaOptionBase>;

  TGocciaOptionList = class
  private
    FItems: TGocciaOptionBaseList;
  public
    constructor Create;
    destructor Destroy; override;

    function AddFlag(const ALongName, AHelpText: string;
      const AGroup: string = ''): TGocciaFlagOption;
    function AddString(const ALongName, AHelpText: string;
      const AGroup: string = ''): TGocciaStringOption;
    function AddInteger(const ALongName, AHelpText: string;
      const AGroup: string = ''): TGocciaIntegerOption;
    function AddRepeatable(const ALongName, AHelpText: string;
      const AGroup: string = ''): TGocciaRepeatableOption;
    function Add(const AOption: TGocciaOptionBase): TGocciaOptionBase;

    function Options: TGocciaOptionArray;
  end;

  TGocciaExecutionMode = (emInterpreted, emBytecode);

  TGocciaEngineOptions = class
  private
    FMode: TGocciaEnumOption<TGocciaExecutionMode>;
    FASI: TGocciaFlagOption;
    FImportMap: TGocciaStringOption;
    FAliases: TGocciaRepeatableOption;
    FTimeout: TGocciaIntegerOption;
    FMaxMemory: TGocciaInt64Option;
    FMaxInstructions: TGocciaInt64Option;
    FUnsafeFFI: TGocciaFlagOption;
    FStackSize: TGocciaIntegerOption;
    FCompatVar: TGocciaFlagOption;
    FAllowedHosts: TGocciaRepeatableOption;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TGocciaOptionArray;

    property Mode: TGocciaEnumOption<TGocciaExecutionMode> read FMode;
    property ASI: TGocciaFlagOption read FASI;
    property ImportMap: TGocciaStringOption read FImportMap;
    property Aliases: TGocciaRepeatableOption read FAliases;
    property Timeout: TGocciaIntegerOption read FTimeout;
    property MaxMemory: TGocciaInt64Option read FMaxMemory;
    property MaxInstructions: TGocciaInt64Option read FMaxInstructions;
    property UnsafeFFI: TGocciaFlagOption read FUnsafeFFI;
    property StackSize: TGocciaIntegerOption read FStackSize;
    property CompatVar: TGocciaFlagOption read FCompatVar;
    property AllowedHosts: TGocciaRepeatableOption read FAllowedHosts;
  end;

  TGocciaCoverageFormat = (cfLcov, cfJson);

  TGocciaCoverageOptions = class
  private
    FEnabled: TGocciaFlagOption;
    FFormat: TGocciaEnumOption<TGocciaCoverageFormat>;
    FOutputPath: TGocciaStringOption;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TGocciaOptionArray;

    property Enabled: TGocciaFlagOption read FEnabled;
    property Format: TGocciaEnumOption<TGocciaCoverageFormat> read FFormat;
    property OutputPath: TGocciaStringOption read FOutputPath;
  end;

  TGocciaProfileMode = (pmOpcodes, pmFunctions, pmAll);
  TGocciaProfileFormat = (pfFlamegraph);

  TGocciaProfilerOptions = class
  private
    FMode: TGocciaEnumOption<TGocciaProfileMode>;
    FOutputPath: TGocciaStringOption;
    FFormat: TGocciaEnumOption<TGocciaProfileFormat>;
  public
    constructor Create;
    destructor Destroy; override;

    function Options: TGocciaOptionArray;

    property Mode: TGocciaEnumOption<TGocciaProfileMode> read FMode;
    property OutputPath: TGocciaStringOption read FOutputPath;
    property Format: TGocciaEnumOption<TGocciaProfileFormat> read FFormat;
  end;

function ConcatOptions(const AArrays: array of TGocciaOptionArray): TGocciaOptionArray;

implementation

{ ConcatOptions }

function ConcatOptions(const AArrays: array of TGocciaOptionArray): TGocciaOptionArray;
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

{ TGocciaOptionBase }

constructor TGocciaOptionBase.Create(const ALongName, AHelpText: string;
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

procedure TGocciaOptionBase.MarkFromCommandLine;
begin
  FFromCommandLine := True;
end;

function TGocciaOptionBase.ValidValues: string;
begin
  Result := '';
end;

{ TGocciaFlagOption }

procedure TGocciaFlagOption.Apply(const AValue: string);
begin
  FPresent := True;
end;

function TGocciaFlagOption.FormatForHelp: string;
begin
  Result := '--' + LongName;
end;

{ TGocciaStringOption }

procedure TGocciaStringOption.Apply(const AValue: string);
begin
  FValue := AValue;
  FPresent := True;
end;

function TGocciaStringOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<value>';
end;

function TGocciaStringOption.ValueOr(const ADefault: string): string;
begin
  if FPresent then
    Result := FValue
  else
    Result := ADefault;
end;

{ TGocciaIntegerOption }

procedure TGocciaIntegerOption.Apply(const AValue: string);
var
  Parsed: Integer;
begin
  if not TryStrToInt(AValue, Parsed) then
    raise TGocciaParseError.CreateFmt('Invalid integer value for --%s: %s',
      [LongName, AValue]);
  FValue := Parsed;
  FPresent := True;
end;

function TGocciaIntegerOption.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<N>';
end;

function TGocciaIntegerOption.ValueOr(const ADefault: Integer): Integer;
begin
  if FPresent then
    Result := FValue
  else
    Result := ADefault;
end;

{ TGocciaInt64Option }

procedure TGocciaInt64Option.Apply(const AValue: string);
var
  Parsed: Int64;
begin
  if not TryStrToInt64(AValue, Parsed) then
    raise TGocciaParseError.CreateFmt('Invalid integer value for --%s: %s',
      [LongName, AValue]);
  FValue := Parsed;
  FPresent := True;
end;

function TGocciaInt64Option.FormatForHelp: string;
begin
  Result := '--' + LongName + '=<N>';
end;

function TGocciaInt64Option.ValueOr(const ADefault: Int64): Int64;
begin
  if FPresent then
    Result := FValue
  else
    Result := ADefault;
end;

{ TGocciaRepeatableOption }

constructor TGocciaRepeatableOption.Create(const ALongName, AHelpText: string;
  const AGroup: string);
begin
  inherited Create(ALongName, AHelpText, AGroup);
  FValues := TStringList.Create;
end;

destructor TGocciaRepeatableOption.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TGocciaRepeatableOption.Apply(const AValue: string);
begin
  FValues.Add(AValue);
  FPresent := True;
end;

function TGocciaRepeatableOption.FormatForHelp: string;
begin
  Result := '--' + LongName + ' <value>';
end;

{ TGocciaEnumOption<T> }

constructor TGocciaEnumOption<T>.Create(const ALongName, AHelpText: string;
  const AGroup: string; const APrefixLength: Integer);
begin
  inherited Create(ALongName, AHelpText, AGroup);
  FOrdinal := 0;
  FPrefixLength := APrefixLength;
end;

function TGocciaEnumOption<T>.JoinStrippedNames(
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

procedure TGocciaEnumOption<T>.Apply(const AValue: string);
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

  raise TGocciaParseError.CreateFmt('Invalid value for --%s: %s (valid: %s)',
    [LongName, AValue, JoinStrippedNames(', ')]);
end;

function TGocciaEnumOption<T>.Value: T;
begin
  Move(FOrdinal, Result, SizeOf(T));
end;

function TGocciaEnumOption<T>.ValueOr(const ADefault: T): T;
begin
  if FPresent then
    Result := Value
  else
    Result := ADefault;
end;

function TGocciaEnumOption<T>.Matches(const AValue: T): Boolean;
var
  OrdinalValue: Integer;
begin
  OrdinalValue := 0;
  Move(AValue, OrdinalValue, SizeOf(T));
  Result := FPresent and (FOrdinal = OrdinalValue);
end;

function TGocciaEnumOption<T>.ValidValues: string;
begin
  Result := JoinStrippedNames(', ');
end;

function TGocciaEnumOption<T>.FormatForHelp: string;
begin
  Result := '--' + LongName + '=' + JoinStrippedNames('|');
end;

{ TGocciaOptionList }

constructor TGocciaOptionList.Create;
begin
  inherited Create;
  FItems := TGocciaOptionBaseList.Create(True);
end;

destructor TGocciaOptionList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TGocciaOptionList.AddFlag(const ALongName, AHelpText: string;
  const AGroup: string): TGocciaFlagOption;
begin
  Result := TGocciaFlagOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TGocciaOptionList.AddString(const ALongName, AHelpText: string;
  const AGroup: string): TGocciaStringOption;
begin
  Result := TGocciaStringOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TGocciaOptionList.AddInteger(const ALongName, AHelpText: string;
  const AGroup: string): TGocciaIntegerOption;
begin
  Result := TGocciaIntegerOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TGocciaOptionList.AddRepeatable(const ALongName, AHelpText: string;
  const AGroup: string): TGocciaRepeatableOption;
begin
  Result := TGocciaRepeatableOption.Create(ALongName, AHelpText, AGroup);
  FItems.Add(Result);
end;

function TGocciaOptionList.Add(const AOption: TGocciaOptionBase): TGocciaOptionBase;
begin
  FItems.Add(AOption);
  Result := AOption;
end;

function TGocciaOptionList.Options: TGocciaOptionArray;
var
  I: Integer;
begin
  SetLength(Result, FItems.Count);
  for I := 0 to FItems.Count - 1 do
    Result[I] := FItems[I];
end;

{ TGocciaEngineOptions }

constructor TGocciaEngineOptions.Create;
begin
  inherited Create;
  FMode := TGocciaEnumOption<TGocciaExecutionMode>.Create('mode',
    'Execution mode', 'Engine');
  FASI := TGocciaFlagOption.Create('asi',
    'Enable automatic semicolon insertion', 'Engine');
  FImportMap := TGocciaStringOption.Create('import-map',
    'Path to import map JSON file', 'Engine');
  FAliases := TGocciaRepeatableOption.Create('alias',
    'Import alias (e.g. @/=./src/)', 'Engine');
  FTimeout := TGocciaIntegerOption.Create('timeout',
    'Per-file timeout in milliseconds', 'Engine');
  FMaxMemory := TGocciaInt64Option.Create('max-memory',
    'GC heap byte limit (RangeError on exceed)', 'Engine');
  FMaxInstructions := TGocciaInt64Option.Create('max-instructions',
    'Maximum execution steps before aborting', 'Engine');
  FUnsafeFFI := TGocciaFlagOption.Create('unsafe-ffi',
    'Enable the FFI global (foreign function interface)', 'Engine');
  FStackSize := TGocciaIntegerOption.Create('stack-size',
    'Maximum call stack depth (0 = no limit)', 'Engine');
  FCompatVar := TGocciaFlagOption.Create('compat-var',
    'Enable var declarations (compatibility)', 'Engine');
  FAllowedHosts := TGocciaRepeatableOption.Create('allowed-host',
    'Hostname allowed for fetch requests (repeatable)', 'Engine');
  FAllowedHosts.ConfigName := 'allowed-hosts';
end;

destructor TGocciaEngineOptions.Destroy;
begin
  FMode.Free;
  FASI.Free;
  FImportMap.Free;
  FAliases.Free;
  FTimeout.Free;
  FMaxMemory.Free;
  FMaxInstructions.Free;
  FUnsafeFFI.Free;
  FStackSize.Free;
  FCompatVar.Free;
  FAllowedHosts.Free;
  inherited Destroy;
end;

function TGocciaEngineOptions.Options: TGocciaOptionArray;
begin
  SetLength(Result, 11);
  Result[0] := FMode;
  Result[1] := FASI;
  Result[2] := FImportMap;
  Result[3] := FAliases;
  Result[4] := FTimeout;
  Result[5] := FMaxMemory;
  Result[6] := FMaxInstructions;
  Result[7] := FUnsafeFFI;
  Result[8] := FStackSize;
  Result[9] := FCompatVar;
  Result[10] := FAllowedHosts;
end;

{ TGocciaCoverageOptions }

constructor TGocciaCoverageOptions.Create;
begin
  inherited Create;
  FEnabled := TGocciaFlagOption.Create('coverage',
    'Enable line and branch coverage', 'Coverage');
  FFormat := TGocciaEnumOption<TGocciaCoverageFormat>.Create('coverage-format',
    'Coverage output format', 'Coverage');
  FOutputPath := TGocciaStringOption.Create('coverage-output',
    'Coverage output file path', 'Coverage');
end;

destructor TGocciaCoverageOptions.Destroy;
begin
  FEnabled.Free;
  FFormat.Free;
  FOutputPath.Free;
  inherited Destroy;
end;

function TGocciaCoverageOptions.Options: TGocciaOptionArray;
begin
  SetLength(Result, 3);
  Result[0] := FEnabled;
  Result[1] := FFormat;
  Result[2] := FOutputPath;
end;

{ TGocciaProfilerOptions }

constructor TGocciaProfilerOptions.Create;
begin
  inherited Create;
  FMode := TGocciaEnumOption<TGocciaProfileMode>.Create('profile',
    'Profiling mode', 'Profiler');
  FOutputPath := TGocciaStringOption.Create('profile-output',
    'Profile output file path', 'Profiler');
  FFormat := TGocciaEnumOption<TGocciaProfileFormat>.Create('profile-format',
    'Profile output format', 'Profiler');
end;

destructor TGocciaProfilerOptions.Destroy;
begin
  FMode.Free;
  FOutputPath.Free;
  FFormat.Free;
  inherited Destroy;
end;

function TGocciaProfilerOptions.Options: TGocciaOptionArray;
begin
  SetLength(Result, 3);
  Result[0] := FMode;
  Result[1] := FOutputPath;
  Result[2] := FFormat;
end;

end.
