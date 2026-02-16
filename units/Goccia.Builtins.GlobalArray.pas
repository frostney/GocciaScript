unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ArrayValue, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.Primitives,
  Goccia.Builtins.Base, Goccia.Arguments.Collection, Generics.Collections, Goccia.Scope,
  Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.ObjectPropertyDescriptor, Goccia.Values.ClassHelper,
  Goccia.Values.FunctionValue;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  protected
    function IsArray(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ArrayOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayFrom, 'from', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ArrayOf, 'of', -1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

function TGocciaGlobalArray.IsArray(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  // Array.isArray accepts 0 or more arguments; returns false if no argument provided
  if Args.Length < 1 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.Create(Args.GetElement(0) is TGocciaArrayValue);
end;

function TGocciaGlobalArray.ArrayFrom(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  Source: TGocciaValue;
  SourceArr: TGocciaArrayValue;
  SourceStr: string;
  I: Integer;
begin
  if Args.Length < 1 then
  begin
    Result := TGocciaArrayValue.Create;
    Exit;
  end;

  Source := Args.GetElement(0);
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
  if (Args.Length > 1) and ((Args.GetElement(1) is TGocciaFunctionValue) or (Args.GetElement(1) is TGocciaNativeFunctionValue)) then
  begin
    for I := 0 to ResultArray.Elements.Count - 1 do
    begin
      if Args.GetElement(1) is TGocciaFunctionValue then
        ResultArray.Elements[I] := TGocciaFunctionValue(Args.GetElement(1)).Call(
          TGocciaArgumentsCollection.Create([ResultArray.Elements[I], TGocciaNumberLiteralValue.Create(I)]), ThisValue)
      else
        ResultArray.Elements[I] := TGocciaNativeFunctionValue(Args.GetElement(1)).Call(
          TGocciaArgumentsCollection.Create([ResultArray.Elements[I], TGocciaNumberLiteralValue.Create(I)]), ThisValue);
    end;
  end;

  Result := ResultArray;
end;

function TGocciaGlobalArray.ArrayOf(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ResultArray: TGocciaArrayValue;
  I: Integer;
begin
  ResultArray := TGocciaArrayValue.Create;

  for I := 0 to Args.Length - 1 do
    ResultArray.Elements.Add(Args.GetElement(I));

  Result := ResultArray;
end;

end.
