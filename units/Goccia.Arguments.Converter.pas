unit Goccia.Arguments.Converter;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // Type conversion and checking logic
  TGocciaArgumentConverter = class
  public
    // Type checks
    class function HasString(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
    class function HasNumber(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
    class function HasBoolean(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
    class function HasObject(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
    class function HasArray(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
    class function HasFunction(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
    
    // Type conversions (always convert via ToXLiteral)
    class function GetString(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaStringLiteralValue;
    class function GetNumber(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaNumberLiteralValue;
    class function GetBoolean(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaBooleanLiteralValue;
    class function GetObject(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback): TGocciaValue;
    class function GetArray(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback): TGocciaValue;
    class function GetFunction(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback): TGocciaValue;
    
    // Type conversions with defaults
    class function GetStringOr(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const ADefaultValue: string): TGocciaStringLiteralValue;
    class function GetNumberOr(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const ADefaultValue: Double): TGocciaNumberLiteralValue;
    class function GetBooleanOr(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const ADefaultValue: Boolean): TGocciaBooleanLiteralValue;
  end;

implementation

{ TGocciaArgumentConverter }

class function TGocciaArgumentConverter.HasString(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < ACollection.Length) and (ACollection.GetElement(AIndex) is TGocciaStringLiteralValue);
end;

class function TGocciaArgumentConverter.HasNumber(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < ACollection.Length) and (ACollection.GetElement(AIndex) is TGocciaNumberLiteralValue);
end;

class function TGocciaArgumentConverter.HasBoolean(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < ACollection.Length) and (ACollection.GetElement(AIndex) is TGocciaBooleanLiteralValue);
end;

class function TGocciaArgumentConverter.HasObject(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < ACollection.Length) and (ACollection.GetElement(AIndex) is TGocciaObjectValue);
end;

class function TGocciaArgumentConverter.HasArray(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < ACollection.Length) and (ACollection.GetElement(AIndex) is TGocciaArrayValue);
end;

class function TGocciaArgumentConverter.HasFunction(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): Boolean;
var
  Arg: TGocciaValue;
begin
  Result := False;
  if (AIndex >= 0) and (AIndex < ACollection.Length) then
  begin
    Arg := ACollection.GetElement(AIndex);
    Result := (Arg is TGocciaFunctionValue) or (Arg is TGocciaNativeFunctionValue);
  end;
end;

class function TGocciaArgumentConverter.GetString(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaStringLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := ACollection.GetElement(AIndex); // Returns undefined if out of bounds
  Result := Arg.ToStringLiteral;
end;

class function TGocciaArgumentConverter.GetNumber(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaNumberLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := ACollection.GetElement(AIndex); // Returns undefined if out of bounds
  Result := Arg.ToNumberLiteral;
end;

class function TGocciaArgumentConverter.GetBoolean(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer): TGocciaBooleanLiteralValue;
var
  Arg: TGocciaValue;
begin
  Arg := ACollection.GetElement(AIndex); // Returns undefined if out of bounds
  Result := Arg.ToBooleanLiteral;
end;

class function TGocciaArgumentConverter.GetObject(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := ACollection.GetElement(AIndex); // Returns undefined if out of bounds
  if Arg is TGocciaObjectValue then
    Result := TGocciaObjectValue(Arg)
  else
  begin
    AThrowError(Format('%s argument %d must be an object', [AFunctionName, AIndex + 1]), 0, 0);
    Result := nil; // This won't be reached due to exception
  end;
end;

class function TGocciaArgumentConverter.GetArray(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := ACollection.GetElement(AIndex); // Returns undefined if out of bounds
  if Arg is TGocciaArrayValue then
    Result := TGocciaArrayValue(Arg)
  else
  begin
    AThrowError(Format('%s argument %d must be an array', [AFunctionName, AIndex + 1]), 0, 0);
    Result := nil; // This won't be reached due to exception
  end;
end;

class function TGocciaArgumentConverter.GetFunction(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback): TGocciaValue;
var
  Arg: TGocciaValue;
begin
  Arg := ACollection.GetElement(AIndex); // Returns undefined if out of bounds
  if (Arg is TGocciaFunctionValue) or (Arg is TGocciaNativeFunctionValue) then
    Result := Arg
  else
  begin
    AThrowError(Format('%s argument %d must be a function', [AFunctionName, AIndex + 1]), 0, 0);
    Result := nil; // This won't be reached due to exception
  end;
end;

class function TGocciaArgumentConverter.GetStringOr(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const ADefaultValue: string): TGocciaStringLiteralValue;
begin
  if (AIndex >= 0) and (AIndex < ACollection.Length) then
    Result := GetString(ACollection, AIndex)
  else
    Result := TGocciaStringLiteralValue.Create(ADefaultValue);
end;

class function TGocciaArgumentConverter.GetNumberOr(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const ADefaultValue: Double): TGocciaNumberLiteralValue;
begin
  if (AIndex >= 0) and (AIndex < ACollection.Length) then
    Result := GetNumber(ACollection, AIndex)
  else
    Result := TGocciaNumberLiteralValue.Create(ADefaultValue);
end;

class function TGocciaArgumentConverter.GetBooleanOr(const ACollection: TGocciaArgumentsCollection; const AIndex: Integer; const ADefaultValue: Boolean): TGocciaBooleanLiteralValue;
begin
  if (AIndex >= 0) and (AIndex < ACollection.Length) then
    Result := GetBoolean(ACollection, AIndex)
  else
  begin
    if ADefaultValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end;
end;

end.