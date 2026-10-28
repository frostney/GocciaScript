unit UnicodeStringList;

{$I Shared.inc}

interface

uses
  Generics.Collections,

  OrderedStringMap;

type
  { Strings in the order added, a string as often as it was added.
    HasString tells whether the list holds a string by exact UTF-16 code-unit
    equality, the comparison ECMAScript gives names. Once the list holds
    HASH_INDEX_MIN_COUNT strings, the first HasString builds a count of each
    string, which every later addition and removal keeps current, so a test
    costs constant time where a scan costs time linear in the list: callers
    that keep a list unique by testing each string before adding it stay
    linear in its length. FindString answers the same and tells how many
    strings it compared: 1 for a test through the count, the strings scanned
    otherwise. IndexOf still scans, with the RTL's comparison. }
  TUnicodeStringList = class(TList<UnicodeString>)
  private const
    HASH_INDEX_MIN_COUNT = 16;
  private
    FCounts: TOrderedStringMap<Integer>;
    procedure BuildCounts;
    procedure CountAdded(const AValue: UnicodeString);
    procedure CountRemoved(const AValue: UnicodeString);
  protected
    procedure Notify(
      {$IFDEF GOCCIA_FPC_CONSTREF_COLLECTION_NOTIFY}constref{$ELSE}const{$ENDIF}
      AValue: UnicodeString;
      ACollectionNotification: TCollectionNotification); override;
  public
    destructor Destroy; override;
    function HasString(const AValue: UnicodeString): Boolean;
    function FindString(const AValue: UnicodeString;
      out AProbes: Integer): Boolean;
  end;

implementation

destructor TUnicodeStringList.Destroy;
begin
  FCounts.Free;
  FCounts := nil;
  inherited;
end;

{ The empty string is never counted: growing Count adds empty strings without
  a notification, so HasString scans for it. }
procedure TUnicodeStringList.CountAdded(const AValue: UnicodeString);
var
  Count: Integer;
begin
  if AValue = '' then
    Exit;
  if FCounts.TryGetValue(AValue, Count) then
    FCounts.Add(AValue, Count + 1)
  else
    FCounts.Add(AValue, 1);
end;

procedure TUnicodeStringList.CountRemoved(const AValue: UnicodeString);
var
  Count: Integer;
begin
  if (AValue = '') or not FCounts.TryGetValue(AValue, Count) then
    Exit;
  if Count > 1 then
    FCounts.Add(AValue, Count - 1)
  else
    FCounts.Remove(AValue);
end;

procedure TUnicodeStringList.BuildCounts;
var
  I: Integer;
begin
  FCounts := TOrderedStringMap<Integer>.Create(Count * 2);
  for I := 0 to Count - 1 do
    CountAdded(Items[I]);
end;

procedure TUnicodeStringList.Notify(
  {$IFDEF GOCCIA_FPC_CONSTREF_COLLECTION_NOTIFY}constref{$ELSE}const{$ENDIF}
  AValue: UnicodeString;
  ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if not Assigned(FCounts) then
    Exit;
  // Only an addition, removal or extraction changes the counts; any other
  // notification a compiler's RTL sends leaves them.
  if ACollectionNotification = cnAdded then
    CountAdded(AValue)
  else if (ACollectionNotification = cnRemoved) or
          (ACollectionNotification = cnExtracted) then
    CountRemoved(AValue);
end;

function TUnicodeStringList.HasString(const AValue: UnicodeString): Boolean;
var
  Probes: Integer;
begin
  Result := FindString(AValue, Probes);
end;

function TUnicodeStringList.FindString(const AValue: UnicodeString;
  out AProbes: Integer): Boolean;
var
  I: Integer;
begin
  if (AValue <> '') and
     (Assigned(FCounts) or (Count >= HASH_INDEX_MIN_COUNT)) then
  begin
    if not Assigned(FCounts) then
      BuildCounts;
    AProbes := 1;
    Exit(FCounts.ContainsKey(AValue));
  end;
  for I := 0 to Count - 1 do
    if Items[I] = AValue then
    begin
      AProbes := I + 1;
      Exit(True);
    end;
  AProbes := Count;
  Result := False;
end;

end.
