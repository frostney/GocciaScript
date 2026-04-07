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
    FSearchIndex: Integer;
    FGlobal: Boolean;
    FSingleMatchReturned: Boolean;
  public
    constructor Create(const ARegExp: TGocciaObjectValue; const AInput: string;
      const AGlobal: Boolean);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.RegExp.Runtime;

const
  REGEXP_STRING_ITERATOR_TAG = 'RegExp String Iterator';

{ TGocciaRegExpMatchAllIteratorValue }

constructor TGocciaRegExpMatchAllIteratorValue.Create(
  const ARegExp: TGocciaObjectValue; const AInput: string;
  const AGlobal: Boolean);
begin
  inherited Create;
  FRegExp := ARegExp;
  FInput := AInput;
  FSearchIndex := 0;
  FGlobal := AGlobal;
  FSingleMatchReturned := False;
end;

// ES2026 §22.2.9.1.1 %RegExpStringIteratorPrototype%.next()
function TGocciaRegExpMatchAllIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  MatchValue: TGocciaValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
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

  if not MatchRegExpObject(FRegExp, FInput, FSearchIndex, False, False,
    MatchValue, MatchIndex, MatchEnd, NextIndex) then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  FSearchIndex := NextIndex;
  if not FGlobal then
    FSingleMatchReturned := True;

  if FSearchIndex > Length(FInput) then
    FDone := True;

  Result := CreateIteratorResult(MatchValue, False);
end;

function TGocciaRegExpMatchAllIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  MatchValue: TGocciaValue;
  MatchIndex, MatchEnd, NextIndex: Integer;
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

  if not MatchRegExpObject(FRegExp, FInput, FSearchIndex, False, False,
    MatchValue, MatchIndex, MatchEnd, NextIndex) then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  FSearchIndex := NextIndex;
  if not FGlobal then
    FSingleMatchReturned := True;

  if FSearchIndex > Length(FInput) then
    FDone := True;

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
