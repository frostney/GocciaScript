unit Goccia.Token;

{$I Goccia.inc}

interface

uses
  Goccia.SourceSpan;

type
  TGocciaTokenType = (
    // Literals
    gttNumber, gttBigInt, gttString, gttTemplate, gttTemplateHead,
    gttTemplateMiddle, gttTemplateTail, gttRegex,
    gttTrue, gttFalse, gttNull,
    // Identifiers
    gttIdentifier,
    // Keywords
    gttConst, gttLet, gttClass, gttEnum, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
    gttReturn, gttIf, gttElse, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak, gttContinue,
    gttThrow, gttTry, gttCatch, gttFinally,
    gttImport, gttExport, gttFrom, gttAs, gttVar, gttWith, gttFunction,
    // Operators
    gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower,
    gttEqual, gttNotEqual, gttLooseEqual, gttLooseNotEqual, gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttAnd, gttOr, gttNullishCoalescing, gttNot, gttTypeof, gttVoid, gttInstanceof, gttIn, gttDelete, gttAssign, gttPlusAssign, gttMinusAssign,
    gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign, gttNullishCoalescingAssign, gttLogicalAndAssign, gttLogicalOrAssign,
    gttIncrement, gttDecrement,
    gttQuestion, gttColon, gttOptionalChaining,
    // Bitwise operators
    gttBitwiseAnd, gttBitwiseOr, gttBitwiseXor, gttBitwiseNot,
    gttLeftShift, gttRightShift, gttUnsignedRightShift,
    gttBitwiseAndAssign, gttBitwiseOrAssign, gttBitwiseXorAssign,
    gttLeftShiftAssign, gttRightShiftAssign, gttUnsignedRightShiftAssign,
    // Punctuation
    gttLeftParen, gttRightParen, gttLeftBrace, gttRightBrace,
    gttLeftBracket, gttRightBracket, gttComma, gttDot, gttSemicolon,
    gttArrow, gttSpread, gttHash, gttAt,
    // Special
    gttEOF
  );

  TGocciaTemplateTokenValue = record
    Cooked: string;
    Raw: string;
    CookedValid: Boolean;
  end;
  PGocciaTemplateTokenValue = ^TGocciaTemplateTokenValue;

  // A token is a 24-byte record in its list's chunked storage, and
  // TGocciaToken points at one. As class instances, a million tokens cost a
  // heap block each (header, VMT, constructor frame) and a list slot. Here a
  // token's address is stable until it is deleted, the list frees them all at
  // once, and two things a token needs are kept once per list instead of per
  // token: the source coordinates (FOwner is the list's slot in a registry)
  // and template values (a side table: few tokens have one). Delphi mode
  // dereferences the pointer implicitly, so Token.Lexeme reads as before.
  PGocciaTokenRec = ^TGocciaTokenRec;
  TGocciaToken = PGocciaTokenRec;
  TGocciaTokenRec = record
  private
    FLexeme: string;
    FStartOffset: Integer;
    FEndOffset: Integer;
    FType: TGocciaTokenType;
    FContainsEscape: Boolean;
    FOwner: Word;
    function GetSpan: TGocciaSourceSpan;
    function GetTemplateValue: TGocciaTemplateTokenValue;
    function GetLine: Integer;
    function GetStartLine: Integer;
    function GetColumn: Integer;
    function GetEndColumn: Integer;
  public
    property TokenType: TGocciaTokenType read FType;
    property Lexeme: string read FLexeme;
    // Line is the line the token ends on and StartLine the one it starts on.
    // They differ for a template literal or a string with a line continuation,
    // so a line terminator separates two tokens when the first one's Line is
    // before the second one's StartLine.
    property Line: Integer read GetLine;
    property StartLine: Integer read GetStartLine;
    property Column: Integer read GetColumn;
    property EndColumn: Integer read GetEndColumn;
    property Span: TGocciaSourceSpan read GetSpan;
    property ContainsEscape: Boolean read FContainsEscape;
    property TemplateValue: TGocciaTemplateTokenValue read GetTemplateValue;
  end;

  // The lexer's tokens in source order. Tokens are only ever removed from the
  // end (a lexer checkpoint rewinds), so token I lives in chunk I div
  // TOKEN_CHUNK at I mod TOKEN_CHUNK.
  TGocciaTokenList = class
  private const
    TOKEN_CHUNK_BITS = 12;
    TOKEN_CHUNK = 1 shl TOKEN_CHUNK_BITS;
  private
    FCount: Integer;
    FChunks: array of PGocciaTokenRec;
    FCoordinates: TGocciaSourceCoordinates;
    FOwner: Word;
    FTemplateTokens: array of TGocciaToken;
    FTemplateValues: array of PGocciaTemplateTokenValue;
    function GetItem(const AIndex: Integer): TGocciaToken;
    function NewSlot: TGocciaToken; inline;
    function FindTemplate(const AToken: TGocciaToken): PGocciaTemplateTokenValue;
    procedure ReleaseTemplate(const AToken: TGocciaToken);
  public
    // ACoordinates: the source every token of this list comes from. The
    // list does not own them.
    constructor Create(const ACoordinates: TGocciaSourceCoordinates);
    destructor Destroy; override;
    function Add(const AType: TGocciaTokenType; const ALexeme: string;
      const AStartOffset, AEndOffset: Integer;
      const AContainsEscape: Boolean = False): TGocciaToken;
    function AddTemplate(const AType: TGocciaTokenType;
      const ACooked, ARaw: string; const ACookedValid: Boolean;
      const AStartOffset, AEndOffset: Integer): TGocciaToken;
    // Only the last token can be deleted.
    procedure Delete(const AIndex: Integer);
    procedure Clear;
    // Unchecked: AIndex must be below Count.
    function At(const AIndex: Integer): TGocciaToken; inline;
    property Count: Integer read FCount;
    property Items[const AIndex: Integer]: TGocciaToken read GetItem; default;
  end;

implementation

uses
  Classes,
  SysUtils;

// Lists by FOwner slot. A slot is taken when a list is created and given back
// when it is destroyed; tokens only read their own list's entry, which does
// not change while the list lives.
var
  GTokenLists: array[1..65535] of TGocciaTokenList;
  GTokenListsLock: TRTLCriticalSection;
  GTokenListsNext: Integer = 1;

constructor TGocciaTokenList.Create(const ACoordinates: TGocciaSourceCoordinates);
var
  I, Slot: Integer;
begin
  inherited Create;
  FCoordinates := ACoordinates;
  Slot := 0;
  EnterCriticalSection(GTokenListsLock);
  try
    for I := 0 to High(GTokenLists) - 1 do
    begin
      if GTokenLists[GTokenListsNext] = nil then
      begin
        Slot := GTokenListsNext;
        GTokenLists[Slot] := Self;
      end;
      if GTokenListsNext = High(GTokenLists) then
        GTokenListsNext := 1
      else
        Inc(GTokenListsNext);
      if Slot <> 0 then
        Break;
    end;
  finally
    LeaveCriticalSection(GTokenListsLock);
  end;
  if Slot = 0 then
    raise EListError.Create('Too many token lists alive at once');
  FOwner := Slot;
end;

destructor TGocciaTokenList.Destroy;
begin
  Clear;
  if FOwner <> 0 then
  begin
    EnterCriticalSection(GTokenListsLock);
    GTokenLists[FOwner] := nil;
    LeaveCriticalSection(GTokenListsLock);
  end;
  inherited Destroy;
end;

function TGocciaTokenList.At(const AIndex: Integer): TGocciaToken;
begin
  Result := PGocciaTokenRec(PByte(FChunks[AIndex shr TOKEN_CHUNK_BITS]) +
    (AIndex and (TOKEN_CHUNK - 1)) * SizeOf(TGocciaTokenRec));
end;

function TGocciaTokenList.NewSlot: TGocciaToken;
var
  Chunk: Integer;
begin
  Chunk := FCount shr TOKEN_CHUNK_BITS;
  if Chunk >= Length(FChunks) then
  begin
    SetLength(FChunks, Chunk + 1);
    FChunks[Chunk] := AllocMem(TOKEN_CHUNK * SizeOf(TGocciaTokenRec));
  end;
  // AllocMem zero-filled the chunk and Delete leaves a slot zeroed: an empty
  // string is the record's empty state.
  Result := At(FCount);
  Result^.FOwner := FOwner;
  Inc(FCount);
end;

function TGocciaTokenList.Add(const AType: TGocciaTokenType;
  const ALexeme: string; const AStartOffset, AEndOffset: Integer;
  const AContainsEscape: Boolean): TGocciaToken;
begin
  Result := NewSlot;
  Result^.FType := AType;
  Result^.FLexeme := ALexeme;
  Result^.FStartOffset := AStartOffset;
  Result^.FEndOffset := AEndOffset;
  Result^.FContainsEscape := AContainsEscape;
end;

function TGocciaTokenList.AddTemplate(const AType: TGocciaTokenType;
  const ACooked, ARaw: string; const ACookedValid: Boolean;
  const AStartOffset, AEndOffset: Integer): TGocciaToken;
var
  V: PGocciaTemplateTokenValue;
  N: Integer;
begin
  Result := NewSlot;
  Result^.FType := AType;
  Result^.FStartOffset := AStartOffset;
  Result^.FEndOffset := AEndOffset;
  New(V);
  V^.Cooked := ACooked;
  V^.Raw := ARaw;
  V^.CookedValid := ACookedValid;
  N := Length(FTemplateTokens);
  SetLength(FTemplateTokens, N + 1);
  SetLength(FTemplateValues, N + 1);
  FTemplateTokens[N] := Result;
  FTemplateValues[N] := V;
end;

// Template tokens are added in source order, so the table is sorted by
// address within a chunk but not across chunks: search from the end, where
// the parser almost always looks.
function TGocciaTokenList.FindTemplate(const AToken: TGocciaToken): PGocciaTemplateTokenValue;
var
  I: Integer;
begin
  for I := High(FTemplateTokens) downto 0 do
    if FTemplateTokens[I] = AToken then
      Exit(FTemplateValues[I]);
  Result := nil;
end;

procedure TGocciaTokenList.ReleaseTemplate(const AToken: TGocciaToken);
var
  N: Integer;
begin
  N := High(FTemplateTokens);
  if (N >= 0) and (FTemplateTokens[N] = AToken) then
  begin
    Dispose(FTemplateValues[N]);
    SetLength(FTemplateTokens, N);
    SetLength(FTemplateValues, N);
  end;
end;

procedure TGocciaTokenList.Delete(const AIndex: Integer);
var
  T: TGocciaToken;
begin
  if AIndex <> FCount - 1 then
    raise EListError.CreateFmt('TGocciaTokenList.Delete: only the last token (%d) can be deleted, not %d',
      [FCount - 1, AIndex]);
  T := At(AIndex);
  ReleaseTemplate(T);
  T^.FLexeme := '';
  FillChar(T^, SizeOf(TGocciaTokenRec), 0);
  Dec(FCount);
end;

procedure TGocciaTokenList.Clear;
var
  I: Integer;
begin
  // The chunks go too, so a slot needs no zeroing: only its string is
  // released.
  for I := 0 to FCount - 1 do
    At(I)^.FLexeme := '';
  FCount := 0;
  for I := 0 to High(FTemplateValues) do
    Dispose(FTemplateValues[I]);
  FTemplateTokens := nil;
  FTemplateValues := nil;
  for I := 0 to High(FChunks) do
    FreeMem(FChunks[I]);
  FChunks := nil;
end;

function TGocciaTokenList.GetItem(const AIndex: Integer): TGocciaToken;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('Token index %d out of bounds (%d tokens)', [AIndex, FCount]);
  Result := At(AIndex);
end;

function TGocciaTokenRec.GetSpan: TGocciaSourceSpan;
begin
  Result := TGocciaSourceSpan.InSource(GTokenLists[FOwner].FCoordinates,
    FStartOffset, FEndOffset);
end;

function TGocciaTokenRec.GetTemplateValue: TGocciaTemplateTokenValue;
var
  V: PGocciaTemplateTokenValue;
begin
  V := GTokenLists[FOwner].FindTemplate(@Self);
  if Assigned(V) then
    Result := V^
  else
  begin
    Result.Cooked := '';
    Result.Raw := '';
    Result.CookedValid := False;
  end;
end;

function TGocciaTokenRec.GetLine: Integer;
begin
  Result := GetSpan.EndLine;
end;

function TGocciaTokenRec.GetStartLine: Integer;
begin
  Result := GetSpan.StartLine;
end;

function TGocciaTokenRec.GetColumn: Integer;
begin
  Result := GetSpan.StartColumn;
end;

function TGocciaTokenRec.GetEndColumn: Integer;
begin
  Result := GetSpan.EndColumn;
end;

initialization
  InitCriticalSection(GTokenListsLock);

finalization
  DoneCriticalSection(GTokenListsLock);

end.
