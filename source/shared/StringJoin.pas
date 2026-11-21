unit StringJoin;

{$I Shared.inc}

// Joins a list of strings with a separator between each two, as
// Array.prototype.join does. It keeps each string, not a copy, and Join
// writes the result once, at its exact length: the work is linear in the
// result, where appending to a string reallocates it on every append, and the
// memory is the result and the strings it is made of, as the appends needed
// at their last step. A join whose only content is one string returns that
// string itself, as the appends did.

interface

type
  TStringJoinBuffer = class
  private
    FSeparator: UnicodeString;
    // The non-empty strings, and the element each belongs to: the
    // separators before a string are its element's index.
    FTexts: array of UnicodeString;
    FElementIndexes: array of SizeInt;
    FTextCount: SizeInt;
    FElementCount: SizeInt;
  public
    constructor Create(const ASeparator: UnicodeString);
    // Adds the next element's string; an empty one still takes its place
    // between two separators.
    procedure Add(const AText: UnicodeString);
    // The joined string. It releases each string as it writes it, while that
    // string is still in the cache, so the buffer is empty afterwards.
    function Join: UnicodeString;
    property ElementCount: SizeInt read FElementCount;
  end;

implementation

const
  INITIAL_TEXT_CAPACITY = 16;
  // Characters, so that the byte size of the result fits a SizeInt too.
  MAXIMUM_LENGTH = High(SizeInt) div (2 * SizeOf(WideChar));

constructor TStringJoinBuffer.Create(const ASeparator: UnicodeString);
begin
  inherited Create;
  FSeparator := ASeparator;
end;

procedure TStringJoinBuffer.Add(const AText: UnicodeString);
var
  Capacity: SizeInt;
begin
  if AText <> '' then
  begin
    Capacity := System.Length(FTexts);
    if FTextCount = Capacity then
    begin
      if Capacity = 0 then
        Capacity := INITIAL_TEXT_CAPACITY
      else
        Capacity := Capacity * 2;
      SetLength(FTexts, Capacity);
      SetLength(FElementIndexes, Capacity);
    end;
    FTexts[FTextCount] := AText;
    FElementIndexes[FTextCount] := FElementCount;
    Inc(FTextCount);
  end;
  Inc(FElementCount);
end;

function TStringJoinBuffer.Join: UnicodeString;
var
  SeparatorLength, SeparatorCount, TotalLength, TextLength: SizeInt;
  SeparatorsWritten, I: SizeInt;
  Target: PWideChar;
begin
  SeparatorLength := System.Length(FSeparator);
  if (FElementCount > 1) and (SeparatorLength > 0) then
    SeparatorCount := FElementCount - 1
  else
    SeparatorCount := 0;
  if SeparatorCount = 0 then
  begin
    if FTextCount = 0 then
    begin
      FElementCount := 0;
      Exit('');
    end;
    if FTextCount = 1 then
    begin
      Result := FTexts[0];
      FTexts[0] := '';
      FTextCount := 0;
      FElementCount := 0;
      Exit;
    end;
  end;

  if (SeparatorCount > 0) and
     (SeparatorCount > MAXIMUM_LENGTH div SeparatorLength) then
    System.Error(reOutOfMemory);
  TotalLength := SeparatorCount * SeparatorLength;
  for I := 0 to FTextCount - 1 do
  begin
    TextLength := System.Length(FTexts[I]);
    if TextLength > MAXIMUM_LENGTH - TotalLength then
      System.Error(reOutOfMemory);
    Inc(TotalLength, TextLength);
  end;

  Result := '';
  SetLength(Result, TotalLength);
  Target := PWideChar(Result);
  SeparatorsWritten := 0;
  for I := 0 to FTextCount - 1 do
  begin
    if SeparatorCount > 0 then
      while SeparatorsWritten < FElementIndexes[I] do
      begin
        Move(PWideChar(FSeparator)^, Target^,
          SeparatorLength * SizeOf(WideChar));
        Inc(Target, SeparatorLength);
        Inc(SeparatorsWritten);
      end;
    TextLength := System.Length(FTexts[I]);
    Move(PWideChar(FTexts[I])^, Target^, TextLength * SizeOf(WideChar));
    Inc(Target, TextLength);
    FTexts[I] := '';
  end;
  while SeparatorsWritten < SeparatorCount do
  begin
    Move(PWideChar(FSeparator)^, Target^, SeparatorLength * SizeOf(WideChar));
    Inc(Target, SeparatorLength);
    Inc(SeparatorsWritten);
  end;
  FTextCount := 0;
  FElementCount := 0;
end;

end.
