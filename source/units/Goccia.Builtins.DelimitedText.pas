unit Goccia.Builtins.DelimitedText;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

function GetDelimitedTextCallback(const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer): TGocciaValue;
function BuildDelimitedTextChunkResult(const AValues: TGocciaArrayValue;
  const ARead: Integer; const ADone: Boolean;
  const AErrorMessage: string): TGocciaValue;
function InvokeDelimitedTextReviver(const AReviver, AKey: TGocciaValue;
  const AValue: string; const AContext: TGocciaObjectValue): TGocciaValue;
// The caller roots the returned array across its format-specific stringifier.
function ApplyDelimitedTextReplacer(const AData: TGocciaArrayValue;
  const AReplacer: TGocciaValue): TGocciaArrayValue;

implementation

uses
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ErrorHelper;

function GetDelimitedTextCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AIndex: Integer): TGocciaValue;
begin
  Result := nil;
  if AArgs.Length > AIndex then
  begin
    Result := AArgs.GetElement(AIndex);
    if not Result.IsCallable then
      Result := nil;
  end;
end;

function BuildDelimitedTextChunkResult(const AValues: TGocciaArrayValue;
  const ARead: Integer; const ADone: Boolean;
  const AErrorMessage: string): TGocciaValue;
var
  ErrorValue: TGocciaValue;
  ResultObject: TGocciaObjectValue;
  ValuesRoot: TGocciaTempRoot;
  ResultRoot: TGocciaTempRoot;
  ErrorRoot: TGocciaTempRoot;
begin
  { CreateErrorObject allocates and property storage is charged on
    assignment — both GC safe points — so the parsed values array, the result
    object, and the error object are all rooted until the assignments below
    store them. }
  InitializeTempRoot(ValuesRoot);
  InitializeTempRoot(ResultRoot);
  InitializeTempRoot(ErrorRoot);
  AddTempRootIfNeeded(ValuesRoot, AValues);
  try
    ResultObject := TGocciaObjectValue.Create;
    AddTempRootIfNeeded(ResultRoot, ResultObject);
    if AErrorMessage = '' then
      ErrorValue := TGocciaNullLiteralValue.NullValue
    else
    begin
      ErrorValue := CreateErrorObject(SYNTAX_ERROR_NAME,
        AErrorMessage, 1);
      AddTempRootIfNeeded(ErrorRoot, ErrorValue);
    end;

    ResultObject.AssignProperty(PROP_VALUES, AValues);
    ResultObject.AssignProperty(PROP_READ,
      TGocciaNumberLiteralValue.Create(ARead));
    ResultObject.AssignProperty(PROP_DONE,
      TGocciaBooleanLiteralValue.Create(ADone));
    ResultObject.AssignProperty(PROP_ERROR, ErrorValue);
    Result := ResultObject;
  finally
    RemoveTempRootIfNeeded(ErrorRoot);
    RemoveTempRootIfNeeded(ResultRoot);
    RemoveTempRootIfNeeded(ValuesRoot);
  end;
end;

function InvokeDelimitedTextReviver(const AReviver, AKey: TGocciaValue;
  const AValue: string; const AContext: TGocciaObjectValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
begin
  Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
  try
    // Root the key before allocating the field value; the caller roots context.
    Args.Add(AKey);
    Args.Add(TGocciaStringLiteralValue.Create(AValue));
    Args.Add(AContext);
    Result := InvokeCallable(AReviver, Args,
      TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    Args.Free;
  end;
end;

function ApplyDelimitedTextReplacer(const AData: TGocciaArrayValue;
  const AReplacer: TGocciaValue): TGocciaArrayValue;
var
  Args: TGocciaArgumentsCollection;
  I, J: Integer;
  Item: TGocciaValue;
  Key: string;
  Keys: TArray<string>;
  Obj: TGocciaObjectValue;
  ReplacerResult: TGocciaValue;
  ReplacedArr: TGocciaArrayValue;
  ReplacedObj: TGocciaObjectValue;
  ReplacedRow: TGocciaArrayValue;
  Row: TGocciaArrayValue;
  ReplacedArrRoot: TGocciaTempRoot;
  ReplacedRowRoot: TGocciaTempRoot;
  ItemRoot: TGocciaTempRoot;
begin
  // Callbacks and getters can collect before a converted row is stored.
  InitializeTempRoot(ReplacedArrRoot);
  InitializeTempRoot(ReplacedRowRoot);
  InitializeTempRoot(ItemRoot);
  try
  ReplacedArr := TGocciaArrayValue.Create;
  AddTempRootIfNeeded(ReplacedArrRoot, ReplacedArr);

  if (AData.Elements.Count > 0) and
     (AData.Elements[0] is TGocciaObjectValue) and
     not (AData.Elements[0] is TGocciaArrayValue) then
  begin
    Keys := TGocciaObjectValue(AData.Elements[0]).GetOwnPropertyKeys;
    for I := 0 to AData.Elements.Count - 1 do
    begin
      if not (AData.Elements[I] is TGocciaObjectValue) then
        Continue;
      Obj := TGocciaObjectValue(AData.Elements[I]);
      ReplacedObj := TGocciaObjectValue.Create;
      AddTempRootIfNeeded(ReplacedRowRoot, ReplacedObj);
      for J := 0 to Length(Keys) - 1 do
      begin
        Key := Keys[J];
        Item := Obj.GetProperty(Key);
        if not Assigned(Item) then
          Item := TGocciaUndefinedLiteralValue.UndefinedValue;

        AddTempRootIfNeeded(ItemRoot, Item);
        Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
        try
          Args.Add(TGocciaStringLiteralValue.Create(Key));
          Args.Add(Item);
          RemoveTempRootIfNeeded(ItemRoot);
          ReplacerResult := InvokeCallable(AReplacer, Args,
            TGocciaUndefinedLiteralValue.UndefinedValue);
        finally
          Args.Free;
        end;
        ReplacedObj.AssignProperty(Key, ReplacerResult);
      end;
      ReplacedArr.Elements.Add(ReplacedObj);
    end;
  end
  else
  begin
    for I := 0 to AData.Elements.Count - 1 do
    begin
      if AData.Elements[I] is TGocciaArrayValue then
      begin
        Row := TGocciaArrayValue(AData.Elements[I]);
        ReplacedRow := TGocciaArrayValue.Create;
        AddTempRootIfNeeded(ReplacedRowRoot, ReplacedRow);
        for J := 0 to Row.Elements.Count - 1 do
        begin
          Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
          try
            Args.Add(TGocciaNumberLiteralValue.Create(J));
            Args.Add(Row.Elements[J]);
            ReplacerResult := InvokeCallable(AReplacer, Args,
              TGocciaUndefinedLiteralValue.UndefinedValue);
          finally
            Args.Free;
          end;
          ReplacedRow.Elements.Add(ReplacerResult);
        end;
        ReplacedArr.Elements.Add(ReplacedRow);
      end;
    end;
  end;

  Result := ReplacedArr;
  finally
    RemoveTempRootIfNeeded(ItemRoot);
    RemoveTempRootIfNeeded(ReplacedRowRoot);
    RemoveTempRootIfNeeded(ReplacedArrRoot);
  end;
end;

end.
