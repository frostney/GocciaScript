unit Goccia.Arguments;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue,
  Goccia.Values.NativeFunction, Goccia.Values.ClassValue,
  Goccia.Error, Generics.Collections, SysUtils;

type
  TGocciaThrowError = procedure(const Message: string; Line, Column: Integer) of object;

  TGocciaArguments = class
  private
    FArgs: TObjectList<TGocciaValue>;
    FFunctionName: string;
    FThrowError: TGocciaThrowError;
    FOwnsArgs: Boolean;

    procedure ThrowValidationError(const Message: string);
  public
    // Create from existing TObjectList (most common - doesn't take ownership)
    constructor Create(Args: TObjectList<TGocciaValue>; const FunctionName: string;
      ThrowError: TGocciaThrowError); overload;

    // Create empty (takes ownership)
    constructor Create(const FunctionName: string; ThrowError: TGocciaThrowError); overload;
    
    // Create from array of values (takes ownership)
    constructor Create(const Values: array of TGocciaValue); overload;

    destructor Destroy; override;

    // Basic properties
    function Count: Integer;
    function IsEmpty: Boolean;

    // Count validation
    procedure RequireExactly(ExpectedCount: Integer);
    procedure RequireAtLeast(MinCount: Integer);
    procedure RequireAtMost(MaxCount: Integer);
    procedure RequireBetween(MinCount, MaxCount: Integer);
    procedure RequireNone; // Requires zero arguments

    // Quick type checks
    function HasString(Index: Integer): Boolean;
    function HasNumber(Index: Integer): Boolean;
    function HasBoolean(Index: Integer): Boolean;
    function HasObject(Index: Integer): Boolean;
    function HasArray(Index: Integer): Boolean;
    function HasFunction(Index: Integer): Boolean;

    // Argument access with automatic type conversion
     function Get(Index: Integer): TGocciaValue; // Returns undefined if missing
     function GetString(Index: Integer): TGocciaStringLiteralValue; // Auto-converts via ToStringLiteral
     function GetNumber(Index: Integer): TGocciaNumberLiteralValue; // Auto-converts via ToNumberLiteral
     function GetBoolean(Index: Integer): TGocciaBooleanLiteralValue; // Auto-converts via ToBooleanLiteral
     function GetObject(Index: Integer): TGocciaObjectValue; // Validates type
     function GetArray(Index: Integer): TGocciaArrayValue; // Validates type
     function GetFunction(Index: Integer): TGocciaValue; // Validates type (either kind of function)

     // Argument access with defaults
     function GetStringOr(Index: Integer; const DefaultValue: string): TGocciaStringLiteralValue;
     function GetNumberOr(Index: Integer; DefaultValue: Double): TGocciaNumberLiteralValue;
     function GetBooleanOr(Index: Integer; DefaultValue: Boolean): TGocciaBooleanLiteralValue;
  end;

implementation

uses
  Goccia.Values.FunctionValue, Goccia.Values.ArrayValue;

{ TGocciaArguments }

constructor TGocciaArguments.Create(Args: TObjectList<TGocciaValue>; const FunctionName: string;
  ThrowError: TGocciaThrowError);
begin
  FArgs := Args;
  FFunctionName := FunctionName;
  FThrowError := ThrowError;
  FOwnsArgs := False; // Don't take ownership of passed-in args
end;

constructor TGocciaArguments.Create(const FunctionName: string; ThrowError: TGocciaThrowError);
begin
  FArgs := TObjectList<TGocciaValue>.Create(False); // Don't own the values
  FFunctionName := FunctionName;
  FThrowError := ThrowError;
  FOwnsArgs := True; // We created it, so we own it
end;

constructor TGocciaArguments.Create(const Values: array of TGocciaValue);
var
  I: Integer;
begin
  FArgs := TObjectList<TGocciaValue>.Create(False); // Don't own the values
  FFunctionName := '';
  FThrowError := nil;
  FOwnsArgs := True; // We created it, so we own it
  
  for I := 0 to High(Values) do
    FArgs.Add(Values[I]);
end;

destructor TGocciaArguments.Destroy;
begin
  if FOwnsArgs then
    FArgs.Free;
  inherited;
end;

procedure TGocciaArguments.ThrowValidationError(const Message: string);
begin
  FThrowError(Message, 0, 0);
end;

function TGocciaArguments.Count: Integer;
begin
  Result := FArgs.Count;
end;

function TGocciaArguments.IsEmpty: Boolean;
begin
  Result := FArgs.Count = 0;
end;

procedure TGocciaArguments.RequireExactly(ExpectedCount: Integer);
begin
  if FArgs.Count <> ExpectedCount then
  begin
    if ExpectedCount = 0 then
      ThrowValidationError(Format('%s expects no arguments', [FFunctionName]))
    else if ExpectedCount = 1 then
      ThrowValidationError(Format('%s expects exactly 1 argument', [FFunctionName]))
    else
      ThrowValidationError(Format('%s expects exactly %d arguments', [FFunctionName, ExpectedCount]));
  end;
end;

procedure TGocciaArguments.RequireAtLeast(MinCount: Integer);
begin
  if FArgs.Count < MinCount then
  begin
    if MinCount = 1 then
      ThrowValidationError(Format('%s expects at least 1 argument', [FFunctionName]))
    else
      ThrowValidationError(Format('%s expects at least %d arguments', [FFunctionName, MinCount]));
  end;
end;

procedure TGocciaArguments.RequireAtMost(MaxCount: Integer);
begin
  if FArgs.Count > MaxCount then
  begin
    if MaxCount = 0 then
      ThrowValidationError(Format('%s expects no arguments', [FFunctionName]))
    else if MaxCount = 1 then
      ThrowValidationError(Format('%s expects at most 1 argument', [FFunctionName]))
    else
      ThrowValidationError(Format('%s expects at most %d arguments', [FFunctionName, MaxCount]));
  end;
end;

procedure TGocciaArguments.RequireBetween(MinCount, MaxCount: Integer);
begin
  if (FArgs.Count < MinCount) or (FArgs.Count > MaxCount) then
  begin
    if MinCount = MaxCount then
      RequireExactly(MinCount)
    else
      ThrowValidationError(Format('%s expects %d-%d arguments', [FFunctionName, MinCount, MaxCount]));
  end;
end;

procedure TGocciaArguments.RequireNone;
begin
  RequireExactly(0);
end;



function TGocciaArguments.HasString(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FArgs.Count) and (FArgs[Index] is TGocciaStringLiteralValue);
end;

function TGocciaArguments.HasNumber(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FArgs.Count) and (FArgs[Index] is TGocciaNumberLiteralValue);
end;

function TGocciaArguments.HasBoolean(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FArgs.Count) and (FArgs[Index] is TGocciaBooleanLiteralValue);
end;

function TGocciaArguments.HasObject(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FArgs.Count) and (FArgs[Index] is TGocciaObjectValue);
end;

function TGocciaArguments.HasArray(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FArgs.Count) and (FArgs[Index] is TGocciaArrayValue);
end;

function TGocciaArguments.HasFunction(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < FArgs.Count) and
            ((FArgs[Index] is TGocciaFunctionValue) or (FArgs[Index] is TGocciaNativeFunctionValue));
end;

function TGocciaArguments.Get(Index: Integer): TGocciaValue;
begin
  if (Index >= 0) and (Index < FArgs.Count) then
    Result := FArgs[Index]
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArguments.GetString(Index: Integer): TGocciaStringLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := Get(Index);
  Result := Arg.ToStringLiteral;
end;

function TGocciaArguments.GetNumber(Index: Integer): TGocciaNumberLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := Get(Index);
  Result := Arg.ToNumberLiteral;
end;

function TGocciaArguments.GetBoolean(Index: Integer): TGocciaBooleanLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := Get(Index);
  Result := Arg.ToBooleanLiteral;
end;

function TGocciaArguments.GetObject(Index: Integer): TGocciaObjectValue;
var
  Arg: TGocciaValue;
begin
  Arg := Get(Index);
  if Arg is TGocciaObjectValue then
    Result := TGocciaObjectValue(Arg)
  else
  begin
    ThrowValidationError(Format('%s argument %d must be an object', [FFunctionName, Index + 1]));
    Result := nil; // This won't be reached due to exception
  end;
end;

function TGocciaArguments.GetArray(Index: Integer): TGocciaArrayValue;
var
  Arg: TGocciaValue;
begin
  Arg := Get(Index);
  if Arg is TGocciaArrayValue then
    Result := TGocciaArrayValue(Arg)
  else
  begin
    ThrowValidationError(Format('%s argument %d must be an array', [FFunctionName, Index + 1]));
    Result := nil; // This won't be reached due to exception
  end;
end;

function TGocciaArguments.GetFunction(Index: Integer): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := Get(Index);
  if (Arg is TGocciaFunctionValue) or (Arg is TGocciaNativeFunctionValue) then
    Result := Arg
  else
  begin
    ThrowValidationError(Format('%s argument %d must be a function', [FFunctionName, Index + 1]));
    Result := nil; // This won't be reached due to exception
  end;
end;

function TGocciaArguments.GetStringOr(Index: Integer; const DefaultValue: string): TGocciaStringLiteralValue;
begin
  if (Index >= 0) and (Index < FArgs.Count) then
    Result := GetString(Index)
  else
    Result := TGocciaStringLiteralValue.Create(DefaultValue);
end;

function TGocciaArguments.GetNumberOr(Index: Integer; DefaultValue: Double): TGocciaNumberLiteralValue;
begin
  if (Index >= 0) and (Index < FArgs.Count) then
    Result := GetNumber(Index)
  else
    Result := TGocciaNumberLiteralValue.Create(DefaultValue);
end;

function TGocciaArguments.GetBooleanOr(Index: Integer; DefaultValue: Boolean): TGocciaBooleanLiteralValue;
begin
  if (Index >= 0) and (Index < FArgs.Count) then
    Result := GetBoolean(Index)
  else
  begin
    if DefaultValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

end.
