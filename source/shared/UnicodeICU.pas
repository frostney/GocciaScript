unit UnicodeICU;

{$I Shared.inc}

interface

type
  TUnicodePropertyRange = record
    Lo: Cardinal;
    Hi: Cardinal;
  end;
  TUnicodePropertyRangeArray = array of TUnicodePropertyRange;

function TryICUGetUnicodePropertyRanges(const AProperty, AValue: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;

implementation

uses
  ICU;

const
  ICU_SUCCESS = 0;

type
  TICUErrorCode = LongInt;
  TUChar32 = LongInt;
  PUChar32 = ^TUChar32;

  TUsetOpenEmpty = function: Pointer; cdecl;
  TUsetApplyPropertyAlias = procedure(ASet: Pointer; const AProp: PWideChar;
    APropLength: LongInt; const AValue: PWideChar; AValueLength: LongInt;
    var AStatus: TICUErrorCode); cdecl;
  TUsetGetItemCount = function(ASet: Pointer): LongInt; cdecl;
  TUsetGetItem = function(ASet: Pointer; AItemIndex: LongInt;
    AStart, AEnd: PUChar32; AStr: PWideChar; AStrCapacity: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TUsetClose = procedure(ASet: Pointer); cdecl;

var
  FnOpenEmpty: TUsetOpenEmpty;
  FnApplyPropertyAlias: TUsetApplyPropertyAlias;
  FnGetItemCount: TUsetGetItemCount;
  FnGetItem: TUsetGetItem;
  FnClose: TUsetClose;
  LoadAttempted: Boolean;
  LoadSucceeded: Boolean;
  InitLock: TRTLCriticalSection;

function LoadSymbol(const AName: string; out APtr: Pointer): Boolean;
begin
  APtr := ICUGetProcAddress(AName);
  Result := Assigned(APtr);
end;

function TryLoadFunctions: Boolean;
var
  S: Pointer;
begin
  EnterCriticalSection(InitLock);
  try
    if LoadAttempted then
    begin
      Result := LoadSucceeded;
      Exit;
    end;

    LoadAttempted := True;
    Result := False;

    if not ICULibraryAvailable then
      Exit;

    if not LoadSymbol('uset_openEmpty', S) then Exit;
    FnOpenEmpty := TUsetOpenEmpty(S);

    if not LoadSymbol('uset_applyPropertyAlias', S) then Exit;
    FnApplyPropertyAlias := TUsetApplyPropertyAlias(S);

    if not LoadSymbol('uset_getItemCount', S) then Exit;
    FnGetItemCount := TUsetGetItemCount(S);

    if not LoadSymbol('uset_getItem', S) then Exit;
    FnGetItem := TUsetGetItem(S);

    if not LoadSymbol('uset_close', S) then Exit;
    FnClose := TUsetClose(S);

    LoadSucceeded := True;
    Result := True;
  finally
    LeaveCriticalSection(InitLock);
  end;
end;

function TryICUGetUnicodePropertyRanges(const AProperty, AValue: string;
  out ARanges: TUnicodePropertyRangeArray): Boolean;
var
  USet: Pointer;
  Status: TICUErrorCode;
  PropW, ValueW: UnicodeString;
  ItemCount, I, StringLen, RangeCount: LongInt;
  RangeStart, RangeEnd: TUChar32;
begin
  Result := False;
  SetLength(ARanges, 0);

  if not TryLoadFunctions then
    Exit;

  USet := FnOpenEmpty;
  if not Assigned(USet) then
    Exit;

  try
    Status := 0;
    PropW := UnicodeString(AProperty);
    ValueW := UnicodeString(AValue);

    if AValue = '' then
      FnApplyPropertyAlias(USet, PWideChar(PropW), Length(PropW), nil, 0, Status)
    else
      FnApplyPropertyAlias(USet, PWideChar(PropW), Length(PropW),
        PWideChar(ValueW), Length(ValueW), Status);

    if Status > ICU_SUCCESS then
      Exit;

    ItemCount := FnGetItemCount(USet);
    if ItemCount <= 0 then
      Exit;

    SetLength(ARanges, ItemCount);
    RangeCount := 0;

    for I := 0 to ItemCount - 1 do
    begin
      Status := 0;
      RangeStart := 0;
      RangeEnd := 0;
      StringLen := FnGetItem(USet, I, @RangeStart, @RangeEnd, nil, 0, Status);

      if StringLen > 0 then
        Continue;

      ARanges[RangeCount].Lo := Cardinal(RangeStart);
      ARanges[RangeCount].Hi := Cardinal(RangeEnd);
      Inc(RangeCount);
    end;

    SetLength(ARanges, RangeCount);
    Result := RangeCount > 0;
  finally
    FnClose(USet);
  end;
end;

initialization
  InitCriticalSection(InitLock);
  LoadAttempted := False;
  LoadSucceeded := False;

finalization
  DoneCriticalSection(InitLock);

end.
