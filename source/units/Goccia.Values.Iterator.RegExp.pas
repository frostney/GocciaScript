unit Goccia.Values.Iterator.RegExp;

{$I Goccia.inc}

interface

uses
  Goccia.RegExp.Engine,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaRegExpMatchAllIteratorValue = class(TGocciaIteratorValue)
  private
    FRegExp: TGocciaObjectValue;
    FInput: string;
    FGlobal: Boolean;
    FUnicode: Boolean;
    FSingleMatchReturned: Boolean;
    // FRegExp came from %RegExp% itself and has not been passed to user
    // code, so its lastIndex can live in FLastIndex between scanner matches.
    FMatcherIsPrivate: Boolean;
    FScanner: TGocciaRegExpScanner;
    // Not traced: cleared at every collection (see MarkReferences).
    FInputValue: TGocciaStringLiteralValue;
    FLastIndex: Integer;
    FLastIndexPending: Boolean;
    FFlagsChecked: Boolean;
    FSticky: Boolean;
    function CanScan: Boolean;
    function ScanNext(out AMatchValue: TGocciaValue): Boolean;
    function MatchNext(out AMatchValue: TGocciaValue): Boolean;
  public
    { AInputValue, when assigned, is a string value holding AInput that the
      results use as their "input" until the next collection, so they share
      the caller's value. }
    constructor Create(const ARegExp: TGocciaObjectValue; const AInput: string;
      const AGlobal, AUnicode, AMatcherIsPrivate: Boolean;
      const AInputValue: TGocciaStringLiteralValue = nil);
    destructor Destroy; override;
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.RegExp.Runtime,
  Goccia.RegExp.VM,
  Goccia.Values.ErrorHelper;

const
  REGEXP_STRING_ITERATOR_TAG = 'RegExp String Iterator';
  MATCH_TEXT_PROPERTY = '0';

var
  GRegExpStringIteratorPrototypeSlot: TGocciaRealmSlotId;

{ TGocciaRegExpMatchAllIteratorValue }

constructor TGocciaRegExpMatchAllIteratorValue.Create(
  const ARegExp: TGocciaObjectValue; const AInput: string;
  const AGlobal, AUnicode, AMatcherIsPrivate: Boolean;
  const AInputValue: TGocciaStringLiteralValue);
var
  SharedPrototype: TGocciaObjectValue;
begin
  inherited Create;
  SharedPrototype := EnsureConcreteIteratorPrototype(
    GRegExpStringIteratorPrototypeSlot, REGEXP_STRING_ITERATOR_TAG);
  if Assigned(SharedPrototype) then
    FPrototype := SharedPrototype;
  FRegExp := ARegExp;
  FInput := AInput;
  FGlobal := AGlobal;
  FUnicode := AUnicode;
  FSingleMatchReturned := False;
  FMatcherIsPrivate := AMatcherIsPrivate;
  FInputValue := AInputValue;
end;

destructor TGocciaRegExpMatchAllIteratorValue.Destroy;
begin
  FScanner.Free;
  inherited;
end;

// A private matcher was built by %RegExp% from the flags the iterator read,
// so its internal flags match FGlobal and FUnicode and cannot change; only
// its sticky flag is read, once. Whether exec is still the built-in is
// checked on every step.
function TGocciaRegExpMatchAllIteratorValue.CanScan: Boolean;
begin
  if not FMatcherIsPrivate or not FGlobal or
     not IsBuiltinExecRegExp(FRegExp) then
    Exit(False);
  if not FFlagsChecked then
  begin
    FSticky := HasInternalRegExpFlag(FRegExp, 'y');
    FFlagsChecked := True;
  end;
  Result := True;
end;

// RegExpExec plus the empty-match lastIndex advance of
// %RegExpStringIteratorPrototype%.next(), with the matcher's lastIndex kept
// in FLastIndex.
function TGocciaRegExpMatchAllIteratorValue.ScanNext(
  out AMatchValue: TGocciaValue): Boolean;
var
  InputRoot: TGocciaTempRoot;
  InputValue: TGocciaStringLiteralValue;
  LastIndex: Double;
  MatchResult: TGocciaRegExpMatchResult;
  StartIndex: Integer;
begin
  if not Assigned(FScanner) then
    FScanner := CreateRegExpScanner(FRegExp, FInput);
  if FLastIndexPending then
    StartIndex := FLastIndex
  else
  begin
    LastIndex := GetRegExpLastIndexLength(FRegExp);
    if LastIndex > FScanner.InputLength then
      StartIndex := FScanner.InputLength + 1
    else
      StartIndex := Trunc(LastIndex);
  end;

  try
    try
      Result := FScanner.Exec(StartIndex, FSticky);
    except
      on E: ERegExpRuntimeError do
        ThrowError(E.Message);
    end;
  finally
    // next() calls may be far apart, and an unfinished iterator may live
    // long: keep only the scanner object between them. The next call finds
    // the decoded subject in the per-thread memo unless another subject was
    // matched in between.
    FScanner.ReleaseBuffers;
  end;
  if not Result then
  begin
    AMatchValue := nil;
    FLastIndex := 0;
    FLastIndexPending := True;
    Exit;
  end;

  if FScanner.MatchEnd = FScanner.MatchIndex then
    FLastIndex := AdvanceUTF16StringIndex(FInput, FScanner.MatchEnd, FUnicode)
  else
    FLastIndex := FScanner.MatchEnd;
  FLastIndexPending := True;
  // FInputValue is not marked (see MarkReferences), so a collection while
  // the match array is built could free it: root it until the array holds it.
  if not Assigned(FInputValue) then
    FInputValue := TGocciaStringLiteralValue.Create(FInput);
  InputValue := FInputValue;
  InitializeTempRoot(InputRoot);
  AddTempRootIfNeeded(InputRoot, InputValue);
  try
    FScanner.GetMatchResult(MatchResult);
    AMatchValue := BuildRegExpMatchArray(InputValue, MatchResult);
  finally
    RemoveTempRootIfNeeded(InputRoot);
  end;
end;

function TGocciaRegExpMatchAllIteratorValue.MatchNext(
  out AMatchValue: TGocciaValue): Boolean;
var
  MatchString: string;
begin
  if CanScan then
    Exit(ScanNext(AMatchValue));
  // A user exec (or exec getter) receives the matcher as this, so from here
  // on user code may hold it and observe its lastIndex: keep it in sync.
  FMatcherIsPrivate := False;

  if FLastIndexPending then
  begin
    FRegExp.SetProperty(PROP_LAST_INDEX,
      TGocciaNumberLiteralValue.Create(FLastIndex));
    FLastIndexPending := False;
  end;
  Result := MatchRegExpObjectOnce(FRegExp, FInput, AMatchValue);
  if Result and FGlobal then
  begin
    MatchString := TGocciaObjectValue(AMatchValue).GetProperty(
      MATCH_TEXT_PROPERTY).ToStringLiteral.Value;
    if MatchString = '' then
      AdvanceProtocolLastIndexAfterEmptyMatch(FRegExp, FInput, FUnicode);
  end;
end;

// ES2026 §22.2.9.1.1 %RegExpStringIteratorPrototype%.next()
function TGocciaRegExpMatchAllIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  MatchValue: TGocciaValue;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  if not FGlobal and FSingleMatchReturned then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  if not MatchNext(MatchValue) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  if not FGlobal then
    FSingleMatchReturned := True;

  Result := CreateIteratorResult(MatchValue, False);
end;

function TGocciaRegExpMatchAllIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  MatchValue: TGocciaValue;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if not FGlobal and FSingleMatchReturned then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if not MatchNext(MatchValue) then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if not FGlobal then
    FSingleMatchReturned := True;

  ADone := False;
  Result := MatchValue;
end;

function TGocciaRegExpMatchAllIteratorValue.ToStringTag: string;
begin
  Result := REGEXP_STRING_ITERATOR_TAG;
end;

procedure TGocciaRegExpMatchAllIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FRegExp) then
    FRegExp.MarkReferences;
  // FInputValue only spares creating, and charging the collector for, a
  // new string value of the whole subject for every result. The results
  // keep it alive while user code holds them; the iterator itself lets go
  // at every collection, so an unfinished iterator does not keep a copy of
  // the subject charged against the memory limit. The next result creates
  // a new one.
  FInputValue := nil;
end;

initialization
  GRegExpStringIteratorPrototypeSlot := RegisterRealmSlot(
    'RegExpStringIterator.prototype');

end.
