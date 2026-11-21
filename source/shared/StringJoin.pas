unit StringJoin;

{$I Shared.inc}

// Joins a list of strings with a separator between each two, as
// Array.prototype.join does, in time linear in the result: the buffer grows
// by doubling, where appending to a string reallocates it on every append.
// A join whose only content is one string returns that string itself, as the
// appends did, and ToString shrinks the buffer to the result instead of
// copying it out, so a join needs about the memory the appends did.

interface

type
  TStringJoinBuffer = record
  private
    FSeparator: UnicodeString;
    // The first string, kept uncopied while nothing else has been written.
    FFirst: UnicodeString;
    FData: UnicodeString;
    FLength: SizeInt;
    FCapacity: SizeInt;
    FElementCount: SizeInt;
    procedure EnsureCapacity(const ANeeded: SizeInt);
    procedure WriteFirst;
    procedure Write(const AText: UnicodeString);
  public
    class function Create(const ASeparator: UnicodeString): TStringJoinBuffer; static;
    // Adds the next element's string; an empty one still takes its place
    // between two separators.
    procedure Add(const AText: UnicodeString);
    function ToString: UnicodeString;
    property ElementCount: SizeInt read FElementCount;
  end;

implementation

const
  MINIMUM_CAPACITY = 64;
  // Characters, so that the byte size of the buffer fits a SizeInt too.
  MAXIMUM_CAPACITY = High(SizeInt) div (2 * SizeOf(WideChar));

class function TStringJoinBuffer.Create(
  const ASeparator: UnicodeString): TStringJoinBuffer;
begin
  Result.FSeparator := ASeparator;
  Result.FFirst := '';
  Result.FData := '';
  Result.FLength := 0;
  Result.FCapacity := 0;
  Result.FElementCount := 0;
end;

procedure TStringJoinBuffer.EnsureCapacity(const ANeeded: SizeInt);
var
  NewCapacity: SizeInt;
begin
  if ANeeded <= FCapacity then
    Exit;
  if ANeeded > MAXIMUM_CAPACITY then
    System.Error(reOutOfMemory);
  if FCapacity < MINIMUM_CAPACITY then
    NewCapacity := MINIMUM_CAPACITY
  else if FCapacity > MAXIMUM_CAPACITY div 2 then
    NewCapacity := MAXIMUM_CAPACITY
  else
    NewCapacity := FCapacity * 2;
  if NewCapacity < ANeeded then
    NewCapacity := ANeeded;
  // SetLength gives FData a block of its own, so the writes below never reach
  // a string that ToString has handed out.
  SetLength(FData, NewCapacity);
  FCapacity := NewCapacity;
end;

// The kept first string goes into the buffer once something follows it.
procedure TStringJoinBuffer.WriteFirst;
var
  FirstLength: SizeInt;
begin
  FirstLength := System.Length(FFirst);
  EnsureCapacity(FirstLength);
  Move(PWideChar(FFirst)^, PWideChar(FData)^, FirstLength * SizeOf(WideChar));
  FLength := FirstLength;
  FFirst := '';
end;

procedure TStringJoinBuffer.Write(const AText: UnicodeString);
var
  TextLength: SizeInt;
begin
  TextLength := System.Length(AText);
  if TextLength = 0 then
    Exit;
  if FFirst <> '' then
    WriteFirst;
  if TextLength > MAXIMUM_CAPACITY - FLength then
    System.Error(reOutOfMemory);
  EnsureCapacity(FLength + TextLength);
  Move(PWideChar(AText)^, (PWideChar(FData) + FLength)^,
    TextLength * SizeOf(WideChar));
  Inc(FLength, TextLength);
end;

procedure TStringJoinBuffer.Add(const AText: UnicodeString);
begin
  if FElementCount > 0 then
    Write(FSeparator);
  if (FLength = 0) and (FFirst = '') then
    FFirst := AText
  else
    Write(AText);
  Inc(FElementCount);
end;

function TStringJoinBuffer.ToString: UnicodeString;
begin
  if FLength = 0 then
    Exit(FFirst);
  if FCapacity <> FLength then
  begin
    SetLength(FData, FLength);
    FCapacity := FLength;
  end;
  Result := FData;
end;

end.
