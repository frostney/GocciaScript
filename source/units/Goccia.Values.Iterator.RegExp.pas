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
    // FRegExp came from %RegExp% itself, so no user code holds it and its
    // lastIndex can live in FLastIndex between scanner matches.
    FMatcherIsPrivate: Boolean;
    FScanner: TGocciaRegExpScanner;
    FInputValue: TGocciaStringLiteralValue;
    FLastIndex: Integer;
    FLastIndexPending: Boolean;
    FFlagsChecked: Boolean;
    FFlagsAgree: Boolean;
    FSticky: Boolean;
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
  FScanner.Free;
  inherited;
end;

// The flags the iterator took from Get(R, "flags") must agree with the
// matcher's internal flags, which RegExpBuiltinExec uses. A private
// matcher's internal flags cannot change, so they are checked once; whether
// exec is still the built-in is checked on every step.
function TGocciaRegExpMatchAllIteratorValue.CanScan: Boolean;
begin
  if not FMatcherIsPrivate or not FGlobal or
     not IsBuiltinExecRegExp(FRegExp) then
    Exit(False);
  if not FFlagsChecked then
  begin
    FFlagsAgree := HasInternalRegExpFlag(FRegExp, 'g') and
      (FUnicode = (HasInternalRegExpFlag(FRegExp, 'u') or
        HasInternalRegExpFlag(FRegExp, 'v')));
    FSticky := HasInternalRegExpFlag(FRegExp, 'y');
    FFlagsChecked := True;
  end;
  Result := FFlagsAgree;
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
  if CanScan then
    Exit(ScanNext(AMatchValue));

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
