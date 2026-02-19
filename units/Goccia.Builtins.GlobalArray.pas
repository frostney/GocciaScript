unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  protected
    function IsArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.FunctionValue,
  Goccia.Values.NativeFunction;

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFrom, 'from', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayOf, 'of', -1));
end;

function TGocciaGlobalArray.IsArray(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  // Array.isArray accepts 0 or more arguments; returns false if no argument provided
  if AArgs.Length < 1 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    if AArgs.GetElement(0) is TGocciaArrayValue then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalArray.ArrayFrom(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Source: TGocciaValue;
  SourceArr: TGocciaArrayValue;
  SourceStr: string;
  I: Integer;
begin
  if AArgs.Length < 1 then
  begin
    Result := TGocciaArrayValue.Create;
    Exit;
  end;

  Source := AArgs.GetElement(0);
  ResultArray := TGocciaArrayValue.Create;

  // Array-like or iterable
  if Source is TGocciaArrayValue then
  begin
    SourceArr := TGocciaArrayValue(Source);
    for I := 0 to SourceArr.Elements.Count - 1 do
      ResultArray.Elements.Add(SourceArr.Elements[I]);
  end
  // String: split into characters
  else if Source is TGocciaStringLiteralValue then
  begin
    SourceStr := TGocciaStringLiteralValue(Source).Value;
    for I := 1 to Length(SourceStr) do
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(SourceStr[I]));
  end
  else
  begin
    // For non-iterable values, return empty array
    Result := ResultArray;
    Exit;
  end;

  // If a map function is provided, apply it
  if (AArgs.Length > 1) and ((AArgs.GetElement(1) is TGocciaFunctionValue) or (AArgs.GetElement(1) is TGocciaNativeFunctionValue)) then
  begin
    for I := 0 to ResultArray.Elements.Count - 1 do
    begin
      if AArgs.GetElement(1) is TGocciaFunctionValue then
        ResultArray.Elements[I] := TGocciaFunctionValue(AArgs.GetElement(1)).Call(
          TGocciaArgumentsCollection.Create([ResultArray.Elements[I], TGocciaNumberLiteralValue.Create(I)]), AThisValue)
      else
        ResultArray.Elements[I] := TGocciaNativeFunctionValue(AArgs.GetElement(1)).Call(
          TGocciaArgumentsCollection.Create([ResultArray.Elements[I], TGocciaNumberLiteralValue.Create(I)]), AThisValue);
    end;
  end;

  Result := ResultArray;
end;

function TGocciaGlobalArray.ArrayOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to AArgs.Length - 1 do
    ResultArray.Elements.Add(AArgs.GetElement(I));

  Result := ResultArray;
end;

end.
