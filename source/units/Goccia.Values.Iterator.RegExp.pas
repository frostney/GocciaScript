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
    // Native bytes of FScanner (its decoded copy of the subject) charged to
    // the collector, so live iterators count against --max-memory and
    // garbage ones make the collector run.
    FScannerChargedBytes: Int64;
    FInputValue: TGocciaStringLiteralValue;
    FLastIndex: Integer;
    FLastIndexPending: Boolean;
    FFlagsChecked: Boolean;
    FSticky: Boolean;
    function TryCreateScanner: Boolean;
    procedure FreeScanner;
    function CanScan: Boolean;
    function ScanNext(out AMatchValue: TGocciaValue): Boolean;
    function MatchNext(out AMatchValue: TGocciaValue): Boolean;
  public
    constructor Create(const ARegExp: TGocciaObjectValue; const AInput: string;
      const AGlobal, AUnicode, AMatcherIsPrivate: Boolean);
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
  const AGlobal, AUnicode, AMatcherIsPrivate: Boolean);
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
end;

destructor TGocciaRegExpMatchAllIteratorValue.Destroy;
begin
  FreeScanner;
  inherited;
end;

function TGocciaRegExpMatchAllIteratorValue.TryCreateScanner: Boolean;
var
  Bytes: Int64;
  GC: TGarbageCollector;
begin
  Bytes := Int64(Length(FInput)) * SizeOf(Cardinal);
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    // Without room for the decoded subject, match through the protocol as
    // before, which reuses the per-thread decode instead.
    if not GC.TryReserveExternalBytes(Bytes, Self) then
      Exit(False);
    FScannerChargedBytes := Bytes;
  end;
  FScanner := CreateRegExpScanner(FRegExp, FInput);
  Result := True;
end;

procedure TGocciaRegExpMatchAllIteratorValue.FreeScanner;
var
  GC: TGarbageCollector;
begin
  FreeAndNil(FScanner);
  if FScannerChargedBytes > 0 then
  begin
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.ReleaseExternalBytes(FScannerChargedBytes);
    FScannerChargedBytes := 0;
  end;
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
  LastIndex: Double;
  MatchResult: TGocciaRegExpMatchResult;
  StartIndex: Integer;
begin
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
    Result := FScanner.Exec(StartIndex, FSticky);
  except
    on E: ERegExpRuntimeError do
      ThrowError(E.Message);
  end;
  if not Result then
  begin
    AMatchValue := nil;
    FLastIndex := 0;
    FLastIndexPending := True;
    // The iterator is done: release the decoded subject and match buffers
    // now rather than when the GC frees the iterator.
    FreeScanner;
    Exit;
  end;

  if FScanner.MatchEnd = FScanner.MatchIndex then
    FLastIndex := AdvanceUTF16StringIndex(FInput, FScanner.MatchEnd, FUnicode)
  else
    FLastIndex := FScanner.MatchEnd;
  FLastIndexPending := True;
  if not Assigned(FInputValue) then
    FInputValue := TGocciaStringLiteralValue.Create(FInput);
  FScanner.GetMatchResult(MatchResult);
  AMatchValue := BuildRegExpMatchArray(FInputValue, MatchResult);
end;

function TGocciaRegExpMatchAllIteratorValue.MatchNext(
  out AMatchValue: TGocciaValue): Boolean;
var
  MatchString: string;
begin
  if CanScan and (Assigned(FScanner) or TryCreateScanner) then
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
  if Assigned(FInputValue) then
    FInputValue.MarkReferences;
end;

initialization
  GRegExpStringIteratorPrototypeSlot := RegisterRealmSlot(
    'RegExpStringIterator.prototype');

end.
