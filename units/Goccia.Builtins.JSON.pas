unit Goccia.Builtins.JSON;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.JSON,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaJSONBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaJSONParser;
    FStringifier: TGocciaJSONStringifier;

    function ApplyReviver(const AHolder: TGocciaValue; const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
    function ApplyReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function ResolveGap(const ASpaceArg: TGocciaValue): string;
    function TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
    function StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
    function StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaArrayValue; const AGap: string): string;
  protected
    function JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue;

constructor TGocciaJSONBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaJSONParser.Create;
  FStringifier := TGocciaJSONStringifier.Create;

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONParse, 'parse', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONStringify, 'stringify', 1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaJSONBuiltin.Destroy;
begin
  FParser.Free;
  FStringifier.Free;
  inherited;
end;

function TGocciaJSONBuiltin.ApplyReviver(const AHolder: TGocciaValue; const AKey: string; const AReviver: TGocciaValue): TGocciaValue;
var
  Value, NewValue: TGocciaValue;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  I: Integer;
  PropKey: string;
  Args: TGocciaArgumentsCollection;
begin
  Value := AHolder.GetProperty(AKey);

  if Value is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(Value);
    if Obj is TGocciaArrayValue then
    begin
      Arr := TGocciaArrayValue(Obj);
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        NewValue := ApplyReviver(Arr, IntToStr(I), AReviver);
        if NewValue is TGocciaUndefinedLiteralValue then
          Arr.Elements[I] := TGocciaUndefinedLiteralValue.UndefinedValue
        else
          Arr.Elements[I] := NewValue;
      end;
    end
    else
    begin
      for PropKey in Obj.GetEnumerablePropertyNames do
      begin
        NewValue := ApplyReviver(Obj, PropKey, AReviver);
        if NewValue is TGocciaUndefinedLiteralValue then
          Obj.DeleteProperty(PropKey)
        else
          Obj.AssignProperty(PropKey, NewValue);
      end;
    end;
  end;

  Args := TGocciaArgumentsCollection.Create;
  Args.Add(TGocciaStringLiteralValue.Create(AKey));
  Args.Add(Value);
  Result := TGocciaFunctionBase(AReviver).Call(Args, AHolder);
end;

function TGocciaJSONBuiltin.JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Reviver: TGocciaValue;
  Root: TGocciaObjectValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('JSON.parse: argument must be a string', 0, 0);

  try
    Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;

  if AArgs.Length >= 2 then
  begin
    Reviver := AArgs.GetElement(1);
    if Reviver.IsCallable then
    begin
      Root := TGocciaObjectValue.Create;
      Root.AssignProperty('', Result);
      Result := ApplyReviver(Root, '', Reviver);
    end;
  end;
end;

function TGocciaJSONBuiltin.ResolveGap(const ASpaceArg: TGocciaValue): string;
var
  SpaceCount: Integer;
  I: Integer;
begin
  Result := '';
  if ASpaceArg is TGocciaNumberLiteralValue then
  begin
    SpaceCount := Trunc(ASpaceArg.ToNumberLiteral.Value);
    if SpaceCount > 10 then
      SpaceCount := 10;
    if SpaceCount < 1 then
      Exit;
    for I := 1 to SpaceCount do
      Result := Result + ' ';
  end
  else if ASpaceArg is TGocciaStringLiteralValue then
  begin
    Result := ASpaceArg.ToStringLiteral.Value;
    if Length(Result) > 10 then
      Result := Copy(Result, 1, 10);
  end;
end;

function TGocciaJSONBuiltin.ApplyReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.Create;
  Args.Add(TGocciaStringLiteralValue.Create(AKey));
  Args.Add(AValue);
  Result := TGocciaFunctionBase(AReplacer).Call(Args, AHolder);
end;

function TGocciaJSONBuiltin.TransformWithReplacer(const AHolder: TGocciaValue; const AKey: string; const AValue: TGocciaValue; const AReplacer: TGocciaValue): TGocciaValue;
var
  Replaced, PropValue, TransformedProp: TGocciaValue;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
  NewObj: TGocciaObjectValue;
  NewArr: TGocciaArrayValue;
  Key: string;
  I: Integer;
begin
  Replaced := ApplyReplacer(AHolder, AKey, AValue, AReplacer);

  if Replaced is TGocciaUndefinedLiteralValue then
  begin
    Result := Replaced;
    Exit;
  end;

  if Replaced is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(Replaced);
    NewArr := TGocciaArrayValue.Create;
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      PropValue := Arr.Elements[I];
      TransformedProp := TransformWithReplacer(Arr, IntToStr(I), PropValue, AReplacer);
      NewArr.Elements.Add(TransformedProp);
    end;
    Result := NewArr;
  end
  else if (Replaced is TGocciaObjectValue) and not (Replaced is TGocciaArrayValue) then
  begin
    Obj := TGocciaObjectValue(Replaced);
    NewObj := TGocciaObjectValue.Create;
    for Key in Obj.GetEnumerablePropertyNames do
    begin
      PropValue := Obj.GetProperty(Key);
      TransformedProp := TransformWithReplacer(Obj, Key, PropValue, AReplacer);
      if not (TransformedProp is TGocciaUndefinedLiteralValue) then
        NewObj.AssignProperty(Key, TransformedProp);
    end;
    Result := NewObj;
  end
  else
    Result := Replaced;
end;

function TGocciaJSONBuiltin.StringifyWithReplacer(const AValue: TGocciaValue; const AReplacer: TGocciaValue; const AGap: string): string;
var
  Root: TGocciaObjectValue;
  Transformed: TGocciaValue;
begin
  Root := TGocciaObjectValue.Create;
  Root.AssignProperty('', AValue);
  Transformed := TransformWithReplacer(Root, '', AValue, AReplacer);

  if Transformed is TGocciaUndefinedLiteralValue then
  begin
    Result := '';
    Exit;
  end;

  Result := FStringifier.Stringify(Transformed, AGap);
end;

function TGocciaJSONBuiltin.StringifyWithAllowList(const AValue: TGocciaValue; const AAllowList: TGocciaArrayValue; const AGap: string): string;
var
  Obj: TGocciaObjectValue;
  Filtered: TGocciaObjectValue;
  I: Integer;
  Key: string;
  PropValue: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) or (AValue is TGocciaArrayValue) then
  begin
    Result := FStringifier.Stringify(AValue, AGap);
    Exit;
  end;

  Obj := TGocciaObjectValue(AValue);
  Filtered := TGocciaObjectValue.Create;

  for I := 0 to AAllowList.Elements.Count - 1 do
  begin
    Key := AAllowList.Elements[I].ToStringLiteral.Value;
    PropValue := Obj.GetProperty(Key);
    if (PropValue <> nil) and not (PropValue is TGocciaUndefinedLiteralValue) then
      Filtered.AssignProperty(Key, PropValue);
  end;

  Result := FStringifier.Stringify(Filtered, AGap);
end;

function TGocciaJSONBuiltin.JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value, ReplacerArg, SpaceArg: TGocciaValue;
  Gap: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON.stringify', ThrowError);

  Value := AArgs.GetElement(0);

  if Value is TGocciaUndefinedLiteralValue then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Gap := '';
  if AArgs.Length >= 3 then
  begin
    SpaceArg := AArgs.GetElement(2);
    Gap := ResolveGap(SpaceArg);
  end;

  try
    if AArgs.Length >= 2 then
    begin
      ReplacerArg := AArgs.GetElement(1);
      if ReplacerArg.IsCallable then
      begin
        Result := TGocciaStringLiteralValue.Create(StringifyWithReplacer(Value, ReplacerArg, Gap));
        Exit;
      end
      else if ReplacerArg is TGocciaArrayValue then
      begin
        Result := TGocciaStringLiteralValue.Create(StringifyWithAllowList(Value, TGocciaArrayValue(ReplacerArg), Gap));
        Exit;
      end;
    end;

    Result := TGocciaStringLiteralValue.Create(FStringifier.Stringify(Value, Gap));
  except
    on E: Exception do
      ThrowError('JSON.stringify error: ' + E.Message, 0, 0);
  end;
end;

end.
