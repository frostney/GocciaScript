unit Goccia.Values.Iterator.RegExp;

{$I Goccia.inc}

interface

uses
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
  public
    constructor Create(const ARegExp: TGocciaObjectValue; const AInput: string;
      const AGlobal, AUnicode: Boolean);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.RegExp.Runtime;

const
  REGEXP_STRING_ITERATOR_TAG = 'RegExp String Iterator';
  MATCH_TEXT_PROPERTY = '0';

{ TGocciaRegExpMatchAllIteratorValue }

constructor TGocciaRegExpMatchAllIteratorValue.Create(
  const ARegExp: TGocciaObjectValue; const AInput: string;
  const AGlobal, AUnicode: Boolean);
begin
  inherited Create;
  FRegExp := ARegExp;
  FInput := AInput;
  FGlobal := AGlobal;
  FUnicode := AUnicode;
  FSingleMatchReturned := False;
end;

// ES2026 §22.2.9.1.1 %RegExpStringIteratorPrototype%.next()
function TGocciaRegExpMatchAllIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  MatchValue: TGocciaValue;
  MatchString: string;
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

  if not MatchRegExpObjectOnce(FRegExp, FInput, MatchValue) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  if FGlobal then
  begin
    MatchString := TGocciaObjectValue(MatchValue).GetProperty(MATCH_TEXT_PROPERTY)
      .ToStringLiteral.Value;
    if MatchString = '' then
      AdvanceProtocolLastIndexAfterEmptyMatch(FRegExp, FInput, FUnicode);
  end
  else
    FSingleMatchReturned := True;

  Result := CreateIteratorResult(MatchValue, False);
end;

function TGocciaRegExpMatchAllIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  MatchValue: TGocciaValue;
  MatchString: string;
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

  if not MatchRegExpObjectOnce(FRegExp, FInput, MatchValue) then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if FGlobal then
  begin
    MatchString := TGocciaObjectValue(MatchValue).GetProperty(MATCH_TEXT_PROPERTY)
      .ToStringLiteral.Value;
    if MatchString = '' then
      AdvanceProtocolLastIndexAfterEmptyMatch(FRegExp, FInput, FUnicode);
  end
  else
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
end;

end.
