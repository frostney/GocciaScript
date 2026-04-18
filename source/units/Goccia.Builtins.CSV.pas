unit Goccia.Builtins.CSV;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.CSV,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaCSVBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaCSVParser;

    procedure ReadOptions(const AArgs: TGocciaArgumentsCollection;
      const AOptionsIndex: Integer; out ADelimiter: Char;
      out AHeaders: Boolean; out ASkipEmptyLines: Boolean);
    function GetReviver(const AArgs: TGocciaArgumentsCollection;
      const AReviverIndex: Integer): TGocciaValue;
    function GetReplacer(const AArgs: TGocciaArgumentsCollection;
      const AReplacerIndex: Integer): TGocciaValue;
    function BuildChunkResultObject(
      const AChunkResult: TGocciaCSVChunkParseResult): TGocciaValue;
  published
    function CSVParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CSVParseChunk(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CSVStringify(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaCSVBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaCSVParser.Create;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('parse', CSVParse, 1, gmkStaticMethod);
    Members.AddNamedMethod('parseChunk', CSVParseChunk, 1, gmkStaticMethod);
    Members.AddNamedMethod('stringify', CSVStringify, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('CSV'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaCSVBuiltin.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure TGocciaCSVBuiltin.ReadOptions(
  const AArgs: TGocciaArgumentsCollection; const AOptionsIndex: Integer;
  out ADelimiter: Char; out AHeaders: Boolean;
  out ASkipEmptyLines: Boolean);
var
  Obj: TGocciaObjectValue;
  Prop: TGocciaValue;
begin
  ADelimiter := ',';
  AHeaders := True;
  ASkipEmptyLines := False;

  if AArgs.Length <= AOptionsIndex then
    Exit;

  if not (AArgs.GetElement(AOptionsIndex) is TGocciaObjectValue) then
    Exit;

  Obj := TGocciaObjectValue(AArgs.GetElement(AOptionsIndex));

  Prop := Obj.GetProperty('delimiter');
  if Assigned(Prop) and (Prop is TGocciaStringLiteralValue) then
  begin
    if Length(Prop.ToStringLiteral.Value) > 0 then
      ADelimiter := Prop.ToStringLiteral.Value[1];
  end;

  Prop := Obj.GetProperty('headers');
  if Assigned(Prop) and (Prop is TGocciaBooleanLiteralValue) then
    AHeaders := TGocciaBooleanLiteralValue(Prop).Value;

  Prop := Obj.GetProperty('skipEmptyLines');
  if Assigned(Prop) and (Prop is TGocciaBooleanLiteralValue) then
    ASkipEmptyLines := TGocciaBooleanLiteralValue(Prop).Value;
end;

function TGocciaCSVBuiltin.GetReviver(
  const AArgs: TGocciaArgumentsCollection;
  const AReviverIndex: Integer): TGocciaValue;
begin
  Result := nil;
  if AArgs.Length > AReviverIndex then
  begin
    Result := AArgs.GetElement(AReviverIndex);
    if not Result.IsCallable then
      Result := nil;
  end;
end;

function TGocciaCSVBuiltin.GetReplacer(
  const AArgs: TGocciaArgumentsCollection;
  const AReplacerIndex: Integer): TGocciaValue;
begin
  Result := nil;
  if AArgs.Length > AReplacerIndex then
  begin
    Result := AArgs.GetElement(AReplacerIndex);
    if not Result.IsCallable then
      Result := nil;
  end;
end;

function TGocciaCSVBuiltin.BuildChunkResultObject(
  const AChunkResult: TGocciaCSVChunkParseResult): TGocciaValue;
var
  ErrorValue: TGocciaValue;
  ResultObject: TGocciaObjectValue;
begin
  ResultObject := TGocciaObjectValue.Create;
  if AChunkResult.ErrorMessage = '' then
    ErrorValue := TGocciaNullLiteralValue.NullValue
  else
    ErrorValue := CreateErrorObject(SYNTAX_ERROR_NAME,
      AChunkResult.ErrorMessage, 1);

  ResultObject.AssignProperty(PROP_VALUES, AChunkResult.Values);
  ResultObject.AssignProperty(PROP_READ,
    TGocciaNumberLiteralValue.Create(AChunkResult.Read));
  ResultObject.AssignProperty(PROP_DONE,
    TGocciaBooleanLiteralValue.Create(AChunkResult.Done));
  ResultObject.AssignProperty(PROP_ERROR, ErrorValue);
  Result := ResultObject;
end;

function TGocciaCSVBuiltin.CSVParse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  Context: TGocciaObjectValue;
  Delimiter: Char;
  FieldInfoRows: TArray<TArray<TGocciaCSVFieldInfo>>;
  Headers: Boolean;
  HeaderRow: TArray<TGocciaCSVFieldInfo>;
  I, J: Integer;
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedResult: TGocciaArrayValue;
  Reviver: TGocciaValue;
  ReviverResult: TGocciaValue;
  Row: TGocciaArrayValue;
  SkipEmptyLines: Boolean;
  Text: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'CSV.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('CSV.parse: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  ReadOptions(AArgs, 1, Delimiter, Headers, SkipEmptyLines);
  Reviver := GetReviver(AArgs, 2);

  try
    if Assigned(Reviver) then
    begin
      FieldInfoRows := FParser.ParseWithFieldInfo(Text, Delimiter, False,
        SkipEmptyLines);

      ParsedResult := TGocciaArrayValue.Create;
      if Length(FieldInfoRows) = 0 then
      begin
        Result := ParsedResult;
        Exit;
      end;

      if Headers then
      begin
        HeaderRow := FieldInfoRows[0];
        for I := 1 to Length(FieldInfoRows) - 1 do
        begin
          Obj := TGocciaObjectValue.Create;
          for J := 0 to Length(HeaderRow) - 1 do
          begin
            Key := HeaderRow[J].Value;
            Context := TGocciaObjectValue.Create;
            Context.AssignProperty('quoted',
              TGocciaBooleanLiteralValue.Create(
                (J < Length(FieldInfoRows[I])) and
                FieldInfoRows[I][J].Quoted));
            Context.AssignProperty('row',
              TGocciaNumberLiteralValue.Create(I - 1));
            Context.AssignProperty('column',
              TGocciaNumberLiteralValue.Create(J));

            Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
            Args.Add(TGocciaStringLiteralValue.Create(Key));
            if J < Length(FieldInfoRows[I]) then
              Args.Add(
                TGocciaStringLiteralValue.Create(FieldInfoRows[I][J].Value))
            else
              Args.Add(TGocciaStringLiteralValue.Create(''));
            Args.Add(Context);

            ReviverResult := InvokeCallable(Reviver, Args,
              TGocciaUndefinedLiteralValue.UndefinedValue);
            Obj.AssignProperty(Key, ReviverResult);
          end;
          ParsedResult.Elements.Add(Obj);
        end;
      end
      else
      begin
        for I := 0 to Length(FieldInfoRows) - 1 do
        begin
          Row := TGocciaArrayValue.Create;
          for J := 0 to Length(FieldInfoRows[I]) - 1 do
          begin
            Context := TGocciaObjectValue.Create;
            Context.AssignProperty('quoted',
              TGocciaBooleanLiteralValue.Create(FieldInfoRows[I][J].Quoted));
            Context.AssignProperty('row',
              TGocciaNumberLiteralValue.Create(I));
            Context.AssignProperty('column',
              TGocciaNumberLiteralValue.Create(J));

            Args := TGocciaArgumentsCollection.CreateWithCapacity(3);
            Args.Add(TGocciaNumberLiteralValue.Create(J));
            Args.Add(
              TGocciaStringLiteralValue.Create(FieldInfoRows[I][J].Value));
            Args.Add(Context);

            ReviverResult := InvokeCallable(Reviver, Args,
              TGocciaUndefinedLiteralValue.UndefinedValue);
            Row.Elements.Add(ReviverResult);
          end;
          ParsedResult.Elements.Add(Row);
        end;
      end;

      Result := ParsedResult;
    end
    else
      Result := FParser.Parse(Text, Delimiter, Headers, SkipEmptyLines);
  except
    on E: EGocciaCSVParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaCSVBuiltin.CSVParseChunk(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ChunkResult: TGocciaCSVChunkParseResult;
  Delimiter: Char;
  EndOffset: Integer;
  Headers: Boolean;
  SkipEmptyLines: Boolean;
  StartOffset: Integer;
  Text: string;
  TextLength: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'CSV.parseChunk',
    ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('CSV.parseChunk: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  TextLength := Length(Text);
  ReadOptions(AArgs, 1, Delimiter, Headers, SkipEmptyLines);

  StartOffset := 0;
  EndOffset := TextLength;

  if AArgs.Length >= 3 then
    StartOffset := Trunc(AArgs.GetElement(2).ToNumberLiteral.Value);
  if AArgs.Length >= 4 then
    EndOffset := Trunc(AArgs.GetElement(3).ToNumberLiteral.Value);

  try
    ChunkResult := FParser.ParseChunk(Text, Delimiter, Headers,
      SkipEmptyLines, StartOffset, EndOffset);
    Result := BuildChunkResultObject(ChunkResult);
  except
    on E: EGocciaCSVParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaCSVBuiltin.CSVStringify(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
  Data: TGocciaValue;
  Delimiter: Char;
  Headers: Boolean;
  I, J: Integer;
  Item: TGocciaValue;
  Key: string;
  Keys: TArray<string>;
  Obj: TGocciaObjectValue;
  Replacer: TGocciaValue;
  ReplacerResult: TGocciaValue;
  ReplacedArr: TGocciaArrayValue;
  ReplacedObj: TGocciaObjectValue;
  ReplacedRow: TGocciaArrayValue;
  Row: TGocciaArrayValue;
  SkipEmptyLines: Boolean;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'CSV.stringify',
    ThrowError);

  Data := AArgs.GetElement(0);
  ReadOptions(AArgs, 1, Delimiter, Headers, SkipEmptyLines);
  Replacer := GetReplacer(AArgs, 2);

  if Assigned(Replacer) and (Data is TGocciaArrayValue) then
  begin
    Arr := TGocciaArrayValue(Data);
    ReplacedArr := TGocciaArrayValue.Create;

    if (Arr.Elements.Count > 0) and
       (Arr.Elements[0] is TGocciaObjectValue) and
       not (Arr.Elements[0] is TGocciaArrayValue) then
    begin
      Keys := TGocciaObjectValue(Arr.Elements[0]).GetOwnPropertyKeys;
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if not (Arr.Elements[I] is TGocciaObjectValue) then
          Continue;
        Obj := TGocciaObjectValue(Arr.Elements[I]);
        ReplacedObj := TGocciaObjectValue.Create;
        for J := 0 to Length(Keys) - 1 do
        begin
          Key := Keys[J];
          Item := Obj.GetProperty(Key);
          if not Assigned(Item) then
            Item := TGocciaUndefinedLiteralValue.UndefinedValue;

          Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
          Args.Add(TGocciaStringLiteralValue.Create(Key));
          Args.Add(Item);
          ReplacerResult := InvokeCallable(Replacer, Args,
            TGocciaUndefinedLiteralValue.UndefinedValue);
          ReplacedObj.AssignProperty(Key, ReplacerResult);
        end;
        ReplacedArr.Elements.Add(ReplacedObj);
      end;
    end
    else
    begin
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if Arr.Elements[I] is TGocciaArrayValue then
        begin
          Row := TGocciaArrayValue(Arr.Elements[I]);
          ReplacedRow := TGocciaArrayValue.Create;
          for J := 0 to Row.Elements.Count - 1 do
          begin
            Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
            Args.Add(TGocciaNumberLiteralValue.Create(J));
            Args.Add(Row.Elements[J]);
            ReplacerResult := InvokeCallable(Replacer, Args,
              TGocciaUndefinedLiteralValue.UndefinedValue);
            ReplacedRow.Elements.Add(ReplacerResult);
          end;
          ReplacedArr.Elements.Add(ReplacedRow);
        end;
      end;
    end;

    Result := TGocciaStringLiteralValue.Create(
      TGocciaCSVStringifier.Stringify(ReplacedArr, Delimiter, Headers));
  end
  else
    Result := TGocciaStringLiteralValue.Create(
      TGocciaCSVStringifier.Stringify(Data, Delimiter, Headers));
end;

end.
