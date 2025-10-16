unit Goccia.Arguments.Converter;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection, Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.ArrayValue,
  Goccia.Values.FunctionValue, Goccia.Values.NativeFunction, Goccia.Error.ThrowErrorCallback, SysUtils;

type
  // Type conversion and checking logic
  TGocciaArgumentConverter = class
  public
    // Type checks
    class function HasString(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
    class function HasNumber(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
    class function HasBoolean(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
    class function HasObject(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
    class function HasArray(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
    class function HasFunction(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
    
    // Type conversions (always convert via ToXLiteral)
    class function GetString(Collection: TGocciaArgumentsCollection; Index: Integer): TGocciaStringLiteralValue;
    class function GetNumber(Collection: TGocciaArgumentsCollection; Index: Integer): TGocciaNumberLiteralValue;
    class function GetBoolean(Collection: TGocciaArgumentsCollection; Index: Integer): TGocciaBooleanLiteralValue;
    class function GetObject(Collection: TGocciaArgumentsCollection; Index: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback): TGocciaValue;
    class function GetArray(Collection: TGocciaArgumentsCollection; Index: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback): TGocciaValue;
    class function GetFunction(Collection: TGocciaArgumentsCollection; Index: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback): TGocciaValue;
    
    // Type conversions with defaults
    class function GetStringOr(Collection: TGocciaArgumentsCollection; Index: Integer; const DefaultValue: string): TGocciaStringLiteralValue;
    class function GetNumberOr(Collection: TGocciaArgumentsCollection; Index: Integer; DefaultValue: Double): TGocciaNumberLiteralValue;
    class function GetBooleanOr(Collection: TGocciaArgumentsCollection; Index: Integer; DefaultValue: Boolean): TGocciaBooleanLiteralValue;
  end;

implementation

{ TGocciaArgumentConverter }

class function TGocciaArgumentConverter.HasString(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Collection.Length) and (Collection.GetElement(Index) is TGocciaStringLiteralValue);
end;

class function TGocciaArgumentConverter.HasNumber(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Collection.Length) and (Collection.GetElement(Index) is TGocciaNumberLiteralValue);
end;

class function TGocciaArgumentConverter.HasBoolean(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Collection.Length) and (Collection.GetElement(Index) is TGocciaBooleanLiteralValue);
end;

class function TGocciaArgumentConverter.HasObject(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Collection.Length) and (Collection.GetElement(Index) is TGocciaObjectValue);
end;

class function TGocciaArgumentConverter.HasArray(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Collection.Length) and (Collection.GetElement(Index) is TGocciaArrayValue);
end;

class function TGocciaArgumentConverter.HasFunction(Collection: TGocciaArgumentsCollection; Index: Integer): Boolean;
var
  Arg: TGocciaValue;
begin
  Result := False;
  if (Index >= 0) and (Index < Collection.Length) then
  begin
    Arg := Collection.GetElement(Index);
    Result := (Arg is TGocciaFunctionValue) or (Arg is TGocciaNativeFunctionValue);
  end;
end;

class function TGocciaArgumentConverter.GetString(Collection: TGocciaArgumentsCollection; Index: Integer): TGocciaStringLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := Collection.GetElement(Index); // Returns undefined if out of bounds
  Result := Arg.ToStringLiteral;
end;

class function TGocciaArgumentConverter.GetNumber(Collection: TGocciaArgumentsCollection; Index: Integer): TGocciaNumberLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := Collection.GetElement(Index); // Returns undefined if out of bounds
  Result := Arg.ToNumberLiteral;
end;

class function TGocciaArgumentConverter.GetBoolean(Collection: TGocciaArgumentsCollection; Index: Integer): TGocciaBooleanLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := Collection.GetElement(Index); // Returns undefined if out of bounds
  Result := Arg.ToBooleanLiteral;
end;

class function TGocciaArgumentConverter.GetObject(Collection: TGocciaArgumentsCollection; Index: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := Collection.GetElement(Index); // Returns undefined if out of bounds
  if Arg is TGocciaObjectValue then
    Result := TGocciaObjectValue(Arg)
  else
  begin
    ThrowError(Format('%s argument %d must be an object', [FunctionName, Index + 1]), 0, 0);
    Result := nil; // This won't be reached due to exception
  end;
end;

class function TGocciaArgumentConverter.GetArray(Collection: TGocciaArgumentsCollection; Index: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := Collection.GetElement(Index); // Returns undefined if out of bounds
  if Arg is TGocciaArrayValue then
    Result := TGocciaArrayValue(Arg)
  else
  begin
    ThrowError(Format('%s argument %d must be an array', [FunctionName, Index + 1]), 0, 0);
    Result := nil; // This won't be reached due to exception
  end;
end;

class function TGocciaArgumentConverter.GetFunction(Collection: TGocciaArgumentsCollection; Index: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := Collection.GetElement(Index); // Returns undefined if out of bounds
  if (Arg is TGocciaFunctionValue) or (Arg is TGocciaNativeFunctionValue) then
    Result := Arg
  else
  begin
    ThrowError(Format('%s argument %d must be a function', [FunctionName, Index + 1]), 0, 0);
    Result := nil; // This won't be reached due to exception
  end;
end;

class function TGocciaArgumentConverter.GetStringOr(Collection: TGocciaArgumentsCollection; Index: Integer; const DefaultValue: string): TGocciaStringLiteralValue;
begin
  if (Index >= 0) and (Index < Collection.Length) then
    Result := GetString(Collection, Index)
  else
    Result := TGocciaStringLiteralValue.Create(DefaultValue);
end;

class function TGocciaArgumentConverter.GetNumberOr(Collection: TGocciaArgumentsCollection; Index: Integer; DefaultValue: Double): TGocciaNumberLiteralValue;
begin
  if (Index >= 0) and (Index < Collection.Length) then
    Result := GetNumber(Collection, Index)
  else
    Result := TGocciaNumberLiteralValue.Create(DefaultValue);
end;

class function TGocciaArgumentConverter.GetBooleanOr(Collection: TGocciaArgumentsCollection; Index: Integer; DefaultValue: Boolean): TGocciaBooleanLiteralValue;
begin
  if (Index >= 0) and (Index < Collection.Length) then
    Result := GetBoolean(Collection, Index)
  else
  begin
    if DefaultValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

end.