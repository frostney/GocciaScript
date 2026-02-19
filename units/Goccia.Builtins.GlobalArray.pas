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
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.SymbolValue;

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
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  IteratorMethod: TGocciaValue;
  IteratorObj: TGocciaValue;
  LengthVal: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  MapCallback: TGocciaValue;
  I, Len: Integer;
begin
  if AArgs.Length < 1 then
  begin
    Result := TGocciaArrayValue.Create;
    Exit;
  end;

  Source := AArgs.GetElement(0);
  ResultArray := TGocciaArrayValue.Create;

  if Source is TGocciaArrayValue then
  begin
    SourceArr := TGocciaArrayValue(Source);
    for I := 0 to SourceArr.Elements.Count - 1 do
      ResultArray.Elements.Add(SourceArr.Elements[I]);
  end
  else if Source is TGocciaStringLiteralValue then
  begin
    SourceStr := TGocciaStringLiteralValue(Source).Value;
    for I := 1 to Length(SourceStr) do
      ResultArray.Elements.Add(TGocciaStringLiteralValue.Create(SourceStr[I]));
  end
  else if Source is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(Source).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) and IteratorMethod.IsCallable then
    begin
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, Source);
      finally
        CallArgs.Free;
      end;
      if IteratorObj is TGocciaIteratorValue then
        Iterator := TGocciaIteratorValue(IteratorObj)
      else if (IteratorObj is TGocciaObjectValue) and
              Assigned(IteratorObj.GetProperty('next')) and
              not (IteratorObj.GetProperty('next') is TGocciaUndefinedLiteralValue) and
              IteratorObj.GetProperty('next').IsCallable then
        Iterator := TGocciaGenericIteratorValue.Create(IteratorObj)
      else
        Iterator := nil;

      if Assigned(Iterator) then
      begin
        TGocciaGarbageCollector.Instance.AddTempRoot(Iterator);
        try
          IterResult := Iterator.AdvanceNext;
          while not TGocciaBooleanLiteralValue(IterResult.GetProperty('done')).Value do
          begin
            ResultArray.Elements.Add(IterResult.GetProperty('value'));
            IterResult := Iterator.AdvanceNext;
          end;
        finally
          TGocciaGarbageCollector.Instance.RemoveTempRoot(Iterator);
        end;
      end;
    end
    else
    begin
      LengthVal := Source.GetProperty('length');
      if Assigned(LengthVal) and not (LengthVal is TGocciaUndefinedLiteralValue) then
      begin
        Len := Trunc(LengthVal.ToNumberLiteral.Value);
        for I := 0 to Len - 1 do
          ResultArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end;
  end;

  if (AArgs.Length > 1) and AArgs.GetElement(1).IsCallable then
  begin
    MapCallback := AArgs.GetElement(1);
    for I := 0 to ResultArray.Elements.Count - 1 do
    begin
      CallArgs := TGocciaArgumentsCollection.Create([ResultArray.Elements[I], TGocciaNumberLiteralValue.Create(I)]);
      try
        ResultArray.Elements[I] := TGocciaFunctionBase(MapCallback).Call(CallArgs, AThisValue);
      finally
        CallArgs.Free;
      end;
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
