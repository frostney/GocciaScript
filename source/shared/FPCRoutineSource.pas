unit FPCRoutineSource;

{$I Shared.inc}

interface

uses
  SysUtils;

type
  EFPCRoutineSource = class(Exception);

  // The located implementation of one routine named by an FPC section.
  TFPCRoutineSpan = record
    Name: string;
    FirstLine: Integer;
    LastLine: Integer;
    Digest: string;
  end;

// Locates the one implementation in ASourceFile that an FPC `.text.n_`
// section names, and returns the SHA-256 of its text from the first token of
// its header through the semicolon after its final `end`, with CRLF and CR
// line endings normalised to LF. The section's parameter suffix is not
// decoded, so the routine must be the only implementation of its name in
// its scope. Raises EFPCRoutineSource, never a broader or empty span, when
// the section shape is unsupported (nested routines, nested or generic
// types), the source declares another unit or program, the routine is
// missing or ambiguous, or an include or conditional directive could change
// the routine's text without changing the digest. Include files are searched
// beside their includer, then in AIncludeDirectories.
function LocateFPCRoutine(const ASourceFile, ASection: string;
  const AIncludeDirectories: array of string): TFPCRoutineSpan;

// The unit-qualified routine an FPC section names, for diagnostics. Returns
// the section itself when its shape is unsupported.
function FPCSectionRoutineName(const ASection: string): string;

implementation

uses
  Classes,

  SHA256;

const
  TEXT_PREFIX = '.text.n_';
  UNIT_SEPARATOR = '_$$_';
  TYPE_SEPARATOR = '$_$';
  METHOD_SEPARATOR = '_$__$$_';
  PROGRAM_PREFIX = 'p$';
  MAX_INCLUDE_DEPTH = 8;
  // Delphi-mode reserved words. Any other word after a routine header is a
  // directive such as overload, inline or external.
  RESERVED_WORDS: array[0..63] of string = (
    'and', 'array', 'as', 'asm', 'begin', 'case', 'class', 'const',
    'constructor', 'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports', 'file', 'finalization', 'finally',
    'for', 'function', 'goto', 'if', 'implementation', 'in', 'inherited',
    'initialization', 'interface', 'is', 'label', 'library', 'mod', 'nil',
    'not', 'object', 'of', 'or', 'packed', 'procedure', 'program',
    'property', 'raise', 'record', 'repeat', 'resourcestring', 'set', 'shl',
    'shr', 'string', 'then', 'threadvar', 'to', 'try', 'type', 'unit',
    'until', 'uses', 'var', 'while', 'with', 'xor', 'bitpacked');
  CONDITIONAL_DIRECTIVES: array[0..7] of string = (
    'if', 'ifdef', 'ifndef', 'ifopt', 'else', 'elseif', 'endif', 'ifend');
  // Words an include file must not contain: each could open a routine or a
  // module part that the including file's parse cannot see.
  INCLUDE_REFUSED_WORDS: array[0..9] of string = (
    'procedure', 'function', 'constructor', 'destructor', 'operator',
    'implementation', 'initialization', 'finalization', 'unit', 'program');

type
  TTokenKind = (tkWord, tkSymbol, tkLiteral);
  TToken = record
    Kind: TTokenKind;
    Text: string;
    Escaped: Boolean;
    Start: Integer;
    Finish: Integer;
    Line: Integer;
  end;
  TTokens = array of TToken;
  TDirective = record
    Name: string;
    Argument: string;
    Start: Integer;
    Line: Integer;
  end;
  TDirectives = array of TDirective;
  TSourceText = record
    FileName: string;
    Bytes: TBytes;
    Tokens: TTokens;
    Directives: TDirectives;
  end;
  TRoutine = record
    Parts: array of string;
    Name: string;
    Level: Integer;
    HasBody: Boolean;
    Generic: Boolean;
    FirstToken: Integer;
    BodyToken: Integer;
    LastToken: Integer;
  end;
  TRoutines = array of TRoutine;
  TSectionName = record
    Module: string;
    TypeName: string;
    Routine: string;
  end;

  TRoutineParser = class
  private
    FSource: TSourceText;
    FPosition: Integer;
    FRoutines: TRoutines;
    FModule: string;
    procedure RefuseAt(const AIndex: Integer; const AMessage: string);
    function IsWord(const AIndex: Integer; const AText: string): Boolean;
    function IsSymbol(const AIndex: Integer; const AText: Char): Boolean;
    function IsRoutineKeyword(const AIndex: Integer): Boolean;
    function InDeclarationPosition(const AIndex: Integer): Boolean;
    function IsRoutineStart(const AIndex: Integer): Boolean;
    function IsTypeOpener(const AIndex: Integer): Boolean;
    function MatchingClose(const AIndex: Integer; const AOpen, AClose: Char): Integer;
    function SpelledText(const AIndex: Integer): string;
    procedure SkipPastSemicolon;
    procedure SkipCode(const AStartDepth: Integer);
    procedure ParseDeclarations(const ALevel: Integer);
    procedure ParseRoutine(const ALevel: Integer);
    function ParseModuleName: string;
  public
    constructor Create(const ASource: TSourceText);
    procedure Parse;
    property Module: string read FModule;
    property Routines: TRoutines read FRoutines;
  end;

procedure Refuse(const AFileName: string; const ALine: Integer; const AMessage: string);
begin
  if ALine > 0 then
    raise EFPCRoutineSource.Create(UTF8Encode(Format('%s:%d: %s', [AFileName, ALine, AMessage])));
  raise EFPCRoutineSource.Create(UTF8Encode(AFileName + ': ' + AMessage));
end;

function InList(const AText: string; const AList: array of string): Boolean;
var
  Item: string;
begin
  for Item in AList do
    if Item = AText then
      Exit(True);
  Result := False;
end;

function IsIdentifierStart(const ACharacter: Byte): Boolean;
begin
  Result := (ACharacter = Ord('_')) or ((ACharacter >= Ord('a')) and (ACharacter <= Ord('z'))) or
    ((ACharacter >= Ord('A')) and (ACharacter <= Ord('Z')));
end;

function IsIdentifierPart(const ACharacter: Byte): Boolean;
begin
  Result := IsIdentifierStart(ACharacter) or ((ACharacter >= Ord('0')) and (ACharacter <= Ord('9')));
end;

function IsHexDigit(const ACharacter: Byte): Boolean;
begin
  Result := ((ACharacter >= Ord('0')) and (ACharacter <= Ord('9'))) or
    ((ACharacter >= Ord('a')) and (ACharacter <= Ord('f'))) or
    ((ACharacter >= Ord('A')) and (ACharacter <= Ord('F')));
end;

function ByteText(const ABytes: TBytes; const AStart, AFinish: Integer): string;
var
  Index: Integer;
begin
  SetLength(Result, AFinish - AStart);
  for Index := AStart to AFinish - 1 do
    Result[Index - AStart + 1] := Char(ABytes[Index]);
end;

function ReadSourceBytes(const AFileName: string): TBytes;
var
  Stream: TFileStream;
begin
  Result := nil;
  if not FileExists(AFileName) then
    Refuse(AFileName, 0, 'source unavailable');
  Stream := TFileStream.Create(UTF8Encode(AFileName), fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size > High(Integer) then
      Refuse(AFileName, 0, 'source too large');
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then
      Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

// Delphi-mode lexing: comments do not nest, `{$` and `(*$` open directives,
// and `&` escapes a reserved word. Comments are dropped; directives are kept
// separately with their offsets.
procedure Tokenize(var ASource: TSourceText);
var
  Bytes: TBytes;
  Count, Position, Line, Start, StartLine, TokenCount, DirectiveCount, NameEnd: Integer;
  Character: Byte;
  Escaped, IsDirective: Boolean;
  Content: string;

  function At(const AOffset: Integer): Byte;
  begin
    if (AOffset >= 0) and (AOffset < Count) then
      Result := Bytes[AOffset]
    else
      Result := 0;
  end;

  procedure AddToken(const AKind: TTokenKind; const AText: string; const AEscaped: Boolean);
  begin
    if TokenCount = Length(ASource.Tokens) then
      SetLength(ASource.Tokens, TokenCount * 2 + 64);
    ASource.Tokens[TokenCount].Kind := AKind;
    ASource.Tokens[TokenCount].Text := AText;
    ASource.Tokens[TokenCount].Escaped := AEscaped;
    ASource.Tokens[TokenCount].Start := Start;
    ASource.Tokens[TokenCount].Finish := Position;
    ASource.Tokens[TokenCount].Line := StartLine;
    Inc(TokenCount);
  end;

  procedure AddDirective(const AText: string);
  begin
    NameEnd := 1;
    while (NameEnd <= Length(AText)) and IsIdentifierPart(Ord(AText[NameEnd])) do
      Inc(NameEnd);
    if DirectiveCount = Length(ASource.Directives) then
      SetLength(ASource.Directives, DirectiveCount * 2 + 16);
    ASource.Directives[DirectiveCount].Name := LowerCase(Copy(AText, 1, NameEnd - 1));
    ASource.Directives[DirectiveCount].Argument := Trim(Copy(AText, NameEnd, MaxInt));
    ASource.Directives[DirectiveCount].Start := Start;
    ASource.Directives[DirectiveCount].Line := StartLine;
    Inc(DirectiveCount);
  end;

  procedure AdvanceLine;
  begin
    if (At(Position) = 13) and (At(Position + 1) = 10) then
      Inc(Position);
    Inc(Position);
    Inc(Line);
  end;

begin
  Bytes := ASource.Bytes;
  Count := Length(Bytes);
  ASource.Tokens := nil;
  ASource.Directives := nil;
  TokenCount := 0;
  DirectiveCount := 0;
  Position := 0;
  Line := 1;
  while Position < Count do
  begin
    Character := Bytes[Position];
    Start := Position;
    StartLine := Line;
    if (Character = 13) or (Character = 10) then
      AdvanceLine
    else if Character <= Ord(' ') then
      Inc(Position)
    else if Character = Ord('{') then
    begin
      IsDirective := At(Position + 1) = Ord('$');
      Inc(Position);
      while (Position < Count) and (Bytes[Position] <> Ord('}')) do
        if (Bytes[Position] = 13) or (Bytes[Position] = 10) then
          AdvanceLine
        else
          Inc(Position);
      if Position >= Count then
        Refuse(ASource.FileName, StartLine, 'unterminated comment');
      if IsDirective then
      begin
        Content := ByteText(Bytes, Start + 2, Position);
        AddDirective(Content);
      end;
      Inc(Position);
    end
    else if (Character = Ord('(')) and (At(Position + 1) = Ord('*')) then
    begin
      IsDirective := At(Position + 2) = Ord('$');
      Inc(Position, 2);
      while (Position < Count) and not ((Bytes[Position] = Ord('*')) and (At(Position + 1) = Ord(')'))) do
        if (Bytes[Position] = 13) or (Bytes[Position] = 10) then
          AdvanceLine
        else
          Inc(Position);
      if Position >= Count then
        Refuse(ASource.FileName, StartLine, 'unterminated comment');
      if IsDirective then
        AddDirective(ByteText(Bytes, Start + 3, Position));
      Inc(Position, 2);
    end
    else if (Character = Ord('/')) and (At(Position + 1) = Ord('/')) then
    begin
      while (Position < Count) and (Bytes[Position] <> 13) and (Bytes[Position] <> 10) do
        Inc(Position);
    end
    else if Character = Ord('''') then
    begin
      Inc(Position);
      repeat
        if (Position >= Count) or (Bytes[Position] = 13) or (Bytes[Position] = 10) then
          Refuse(ASource.FileName, StartLine, 'unterminated string');
        if Bytes[Position] = Ord('''') then
        begin
          if At(Position + 1) <> Ord('''') then
            Break;
          Inc(Position);
        end;
        Inc(Position);
      until False;
      Inc(Position);
      AddToken(tkLiteral, '', False);
    end
    else if Character = Ord('#') then
    begin
      Inc(Position);
      if At(Position) = Ord('$') then
        Inc(Position);
      while IsHexDigit(At(Position)) do
        Inc(Position);
      AddToken(tkLiteral, '', False);
    end
    else if IsIdentifierStart(Character) or
      ((Character = Ord('&')) and IsIdentifierStart(At(Position + 1))) then
    begin
      Escaped := Character = Ord('&');
      if Escaped then
        Inc(Position);
      while IsIdentifierPart(At(Position)) do
        Inc(Position);
      if Escaped then
        AddToken(tkWord, LowerCase(ByteText(Bytes, Start + 1, Position)), True)
      else
        AddToken(tkWord, LowerCase(ByteText(Bytes, Start, Position)), False);
    end
    else if ((Character >= Ord('0')) and (Character <= Ord('9'))) or
      (((Character = Ord('$')) or (Character = Ord('&'))) and IsHexDigit(At(Position + 1))) or
      ((Character = Ord('%')) and ((At(Position + 1) = Ord('0')) or (At(Position + 1) = Ord('1')))) then
    begin
      Inc(Position);
      while IsIdentifierPart(At(Position)) do
        Inc(Position);
      AddToken(tkLiteral, '', False);
    end
    else
    begin
      Inc(Position);
      AddToken(tkSymbol, Char(Character), False);
    end;
  end;
  SetLength(ASource.Tokens, TokenCount);
  SetLength(ASource.Directives, DirectiveCount);
end;

function LoadSource(const AFileName: string): TSourceText;
begin
  Result := Default(TSourceText);
  Result.FileName := AFileName;
  Result.Bytes := ReadSourceBytes(AFileName);
  Tokenize(Result);
end;

{ TRoutineParser }

constructor TRoutineParser.Create(const ASource: TSourceText);
begin
  inherited Create;
  FSource := ASource;
end;

procedure TRoutineParser.RefuseAt(const AIndex: Integer; const AMessage: string);
begin
  if (AIndex >= 0) and (AIndex < Length(FSource.Tokens)) then
    Refuse(FSource.FileName, FSource.Tokens[AIndex].Line, AMessage)
  else
    Refuse(FSource.FileName, 0, AMessage + ' at end of file');
end;

function TRoutineParser.IsWord(const AIndex: Integer; const AText: string): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(FSource.Tokens)) and
    (FSource.Tokens[AIndex].Kind = tkWord) and not FSource.Tokens[AIndex].Escaped and
    (FSource.Tokens[AIndex].Text = AText);
end;

function TRoutineParser.IsSymbol(const AIndex: Integer; const AText: Char): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(FSource.Tokens)) and
    (FSource.Tokens[AIndex].Kind = tkSymbol) and (FSource.Tokens[AIndex].Text = AText);
end;

function TRoutineParser.IsRoutineKeyword(const AIndex: Integer): Boolean;
begin
  Result := IsWord(AIndex, 'procedure') or IsWord(AIndex, 'function') or
    IsWord(AIndex, 'constructor') or IsWord(AIndex, 'destructor');
end;

// A routine keyword after these tokens is part of a procedural type.
function TRoutineParser.InDeclarationPosition(const AIndex: Integer): Boolean;
var
  Previous: Integer;
begin
  Previous := AIndex - 1;
  if Previous < 0 then
    Exit(True);
  Result := not (IsSymbol(Previous, '=') or IsSymbol(Previous, ':') or
    IsSymbol(Previous, '(') or IsSymbol(Previous, ',') or IsSymbol(Previous, '<') or
    IsWord(Previous, 'of') or IsWord(Previous, 'to') or IsWord(Previous, 'class') or
    IsWord(Previous, 'generic') or (IsWord(Previous, 'type') and IsSymbol(Previous - 1, '=')));
end;

function TRoutineParser.IsRoutineStart(const AIndex: Integer): Boolean;
var
  Keyword: Integer;
begin
  if not InDeclarationPosition(AIndex) then
    Exit(False);
  Keyword := AIndex;
  if IsWord(Keyword, 'generic') then
    Inc(Keyword);
  if IsWord(Keyword, 'class') then
  begin
    Inc(Keyword);
    if IsWord(Keyword, 'operator') then
      Exit(True);
  end;
  Result := IsRoutineKeyword(Keyword);
end;

// Type constructors closed by `end`. A class or interface without a body
// (`class;`, `class of`, `class(TBase);`) and `of object` are not.
function TRoutineParser.IsTypeOpener(const AIndex: Integer): Boolean;
var
  Next: Integer;
begin
  if IsWord(AIndex, 'record') then
    Exit(not (IsSymbol(AIndex + 1, '>') or IsSymbol(AIndex + 1, ',')));
  if IsWord(AIndex, 'object') then
    Exit(not IsWord(AIndex - 1, 'of'));
  if not (IsWord(AIndex, 'class') or IsWord(AIndex, 'interface') or
    IsWord(AIndex, 'dispinterface')) then
    Exit(False);
  if not (IsSymbol(AIndex - 1, '=') or IsWord(AIndex - 1, 'packed')) then
    Exit(False);
  Next := AIndex + 1;
  if IsWord(Next, 'of') or IsSymbol(Next, ';') then
    Exit(False);
  if IsSymbol(Next, '(') then
    Exit(not IsSymbol(MatchingClose(Next, '(', ')') + 1, ';'));
  Result := True;
end;

function TRoutineParser.MatchingClose(const AIndex: Integer; const AOpen, AClose: Char): Integer;
var
  Depth: Integer;
begin
  Depth := 0;
  Result := AIndex;
  while Result < Length(FSource.Tokens) do
  begin
    if IsSymbol(Result, AOpen) then
      Inc(Depth)
    else if IsSymbol(Result, AClose) then
    begin
      Dec(Depth);
      if Depth = 0 then
        Exit;
    end;
    Inc(Result);
  end;
  RefuseAt(AIndex, 'unbalanced ' + AOpen);
end;

function TRoutineParser.SpelledText(const AIndex: Integer): string;
begin
  Result := ByteText(FSource.Bytes, FSource.Tokens[AIndex].Start, FSource.Tokens[AIndex].Finish);
end;

procedure TRoutineParser.SkipPastSemicolon;
begin
  while (FPosition < Length(FSource.Tokens)) and not IsSymbol(FPosition, ';') do
  begin
    if IsSymbol(FPosition, '(') then
      FPosition := MatchingClose(FPosition, '(', ')')
    else if IsSymbol(FPosition, '[') then
      FPosition := MatchingClose(FPosition, '[', ']');
    Inc(FPosition);
  end;
  if FPosition >= Length(FSource.Tokens) then
    RefuseAt(FPosition, 'expected ;');
  Inc(FPosition);
end;

// Leaves FPosition on the `end` that closes the block. Statements cannot
// declare routines, so a routine keyword here means the parse disagrees with
// the compiler's and is refused.
procedure TRoutineParser.SkipCode(const AStartDepth: Integer);
var
  Depth: Integer;
  InAssembler: Boolean;
begin
  Depth := AStartDepth;
  InAssembler := False;
  while FPosition < Length(FSource.Tokens) do
  begin
    if IsWord(FPosition, 'end') then
    begin
      InAssembler := False;
      Dec(Depth);
      if Depth = 0 then
        Exit;
    end
    else if not InAssembler then
    begin
      if IsWord(FPosition, 'begin') or IsWord(FPosition, 'case') or IsWord(FPosition, 'try') then
        Inc(Depth)
      else if IsWord(FPosition, 'asm') then
      begin
        Inc(Depth);
        InAssembler := True;
      end
      else if IsRoutineKeyword(FPosition) or IsWord(FPosition, 'implementation') or
        IsWord(FPosition, 'initialization') or
        (IsWord(FPosition, 'finalization') and (AStartDepth = 0)) then
        RefuseAt(FPosition, 'declaration keyword inside a statement block');
    end;
    Inc(FPosition);
  end;
  RefuseAt(FPosition, 'unterminated block');
end;

// Level 0 is the unit implementation or program declaration part; it ends on
// the module's final `end`. Deeper levels are a routine's local declarations
// and end on its body's `begin` or `asm`.
procedure TRoutineParser.ParseDeclarations(const ALevel: Integer);
var
  TypeDepth: Integer;
begin
  TypeDepth := 0;
  while FPosition < Length(FSource.Tokens) do
  begin
    if TypeDepth > 0 then
    begin
      if IsTypeOpener(FPosition) then
        Inc(TypeDepth)
      else if IsWord(FPosition, 'end') then
        Dec(TypeDepth);
      Inc(FPosition);
      Continue;
    end;
    if IsRoutineStart(FPosition) then
    begin
      ParseRoutine(ALevel);
      Continue;
    end;
    if IsWord(FPosition, 'begin') or IsWord(FPosition, 'asm') then
    begin
      if ALevel > 0 then
        Exit;
      SkipCode(0);
      Exit;
    end;
    if IsWord(FPosition, 'initialization') or IsWord(FPosition, 'finalization') then
    begin
      if ALevel > 0 then
        RefuseAt(FPosition, 'module section inside a routine');
      Inc(FPosition);
      SkipCode(1);
      Exit;
    end;
    if IsWord(FPosition, 'end') then
    begin
      if ALevel > 0 then
        RefuseAt(FPosition, 'unexpected end in routine declarations');
      Exit;
    end;
    if IsWord(FPosition, 'implementation') or IsWord(FPosition, 'unit') or
      IsWord(FPosition, 'program') then
      RefuseAt(FPosition, 'unexpected module keyword');
    if IsTypeOpener(FPosition) then
      Inc(TypeDepth);
    Inc(FPosition);
  end;
  RefuseAt(FPosition, 'unterminated declarations');
end;

procedure TRoutineParser.ParseRoutine(const ALevel: Integer);
var
  Routine: TRoutine;
  Index: Integer;
  Directive: string;
begin
  Routine := Default(TRoutine);
  Routine.FirstToken := FPosition;
  Routine.Level := ALevel;
  Routine.HasBody := True;
  if IsWord(FPosition, 'generic') then
  begin
    Routine.Generic := True;
    Inc(FPosition);
  end;
  if IsWord(FPosition, 'class') then
    Inc(FPosition);
  Inc(FPosition);
  repeat
    if (FPosition >= Length(FSource.Tokens)) or (FSource.Tokens[FPosition].Kind <> tkWord) or
      (InList(FSource.Tokens[FPosition].Text, RESERVED_WORDS) and not FSource.Tokens[FPosition].Escaped) then
      RefuseAt(FPosition, 'expected routine name');
    SetLength(Routine.Parts, Length(Routine.Parts) + 1);
    Routine.Parts[High(Routine.Parts)] := FSource.Tokens[FPosition].Text;
    Routine.Name := Routine.Name + SpelledText(FPosition);
    Inc(FPosition);
    if IsSymbol(FPosition, '<') then
    begin
      Routine.Generic := True;
      FPosition := MatchingClose(FPosition, '<', '>') + 1;
    end;
    if not IsSymbol(FPosition, '.') then
      Break;
    Routine.Name := Routine.Name + '.';
    Inc(FPosition);
  until False;
  if IsSymbol(FPosition, '(') then
    FPosition := MatchingClose(FPosition, '(', ')') + 1;
  if IsSymbol(FPosition, ':') then
    SkipPastSemicolon
  else if IsSymbol(FPosition, ';') then
    Inc(FPosition)
  else
    RefuseAt(FPosition, 'expected ; after routine header');
  while (FPosition < Length(FSource.Tokens)) and (FSource.Tokens[FPosition].Kind = tkWord) and
    not FSource.Tokens[FPosition].Escaped and
    not InList(FSource.Tokens[FPosition].Text, RESERVED_WORDS) do
  begin
    Directive := FSource.Tokens[FPosition].Text;
    if (Directive = 'forward') or (Directive = 'external') then
      Routine.HasBody := False;
    SkipPastSemicolon;
  end;
  Index := Length(FRoutines);
  SetLength(FRoutines, Index + 1);
  FRoutines[Index] := Routine;
  if not Routine.HasBody then
  begin
    FRoutines[Index].LastToken := FPosition - 1;
    Exit;
  end;
  ParseDeclarations(ALevel + 1);
  if not (IsWord(FPosition, 'begin') or IsWord(FPosition, 'asm')) then
    RefuseAt(FPosition, 'expected routine body');
  FRoutines[Index].BodyToken := FPosition;
  SkipCode(0);
  Inc(FPosition);
  if not IsSymbol(FPosition, ';') then
    RefuseAt(FPosition, 'expected ; after routine body');
  FRoutines[Index].LastToken := FPosition;
  Inc(FPosition);
end;

function TRoutineParser.ParseModuleName: string;
begin
  Result := '';
  repeat
    if (FPosition >= Length(FSource.Tokens)) or (FSource.Tokens[FPosition].Kind <> tkWord) then
      RefuseAt(FPosition, 'expected module name');
    Result := Result + FSource.Tokens[FPosition].Text;
    Inc(FPosition);
    if not IsSymbol(FPosition, '.') then
      Break;
    Result := Result + '.';
    Inc(FPosition);
  until False;
  SkipPastSemicolon;
end;

procedure TRoutineParser.Parse;
var
  Index: Integer;
begin
  FPosition := 0;
  if IsWord(0, 'unit') then
  begin
    Inc(FPosition);
    FModule := ParseModuleName;
    Index := FPosition;
    while (Index < Length(FSource.Tokens)) and not IsWord(Index, 'implementation') do
      Inc(Index);
    if Index >= Length(FSource.Tokens) then
      RefuseAt(Index, 'unit has no implementation section');
    FPosition := Index + 1;
  end
  else if IsWord(0, 'program') then
  begin
    Inc(FPosition);
    FModule := PROGRAM_PREFIX + ParseModuleName;
  end
  else
    RefuseAt(0, 'expected a unit or program');
  ParseDeclarations(0);
  // The whole module must parse, so no later implementation goes unseen.
  if not (IsWord(FPosition, 'end') and IsSymbol(FPosition + 1, '.') and
    (FPosition + 2 = Length(FSource.Tokens))) then
    RefuseAt(FPosition, 'expected the module''s final end.');
end;

function IsPlainIdentifier(const AText: string): Boolean;
var
  Index: Integer;
begin
  Result := (AText <> '') and IsIdentifierStart(Ord(AText[1])) and (Ord(AText[1]) < 128);
  for Index := 2 to Length(AText) do
    Result := Result and (Ord(AText[Index]) < 128) and IsIdentifierPart(Ord(AText[Index]));
end;

function IsModuleName(const AText: string): Boolean;
var
  Name: string;
  Position: Integer;
begin
  Name := AText;
  if Copy(Name, 1, Length(PROGRAM_PREFIX)) = PROGRAM_PREFIX then
    Delete(Name, 1, Length(PROGRAM_PREFIX));
  repeat
    Position := Pos('.', Name);
    if Position = 0 then
      Exit(IsPlainIdentifier(Name));
    if not IsPlainIdentifier(Copy(Name, 1, Position - 1)) then
      Exit(False);
    Delete(Name, 1, Position);
  until False;
end;

// Accepts `<module>_$$_<routine>[$suffix]` and
// `<module>$_$<type>_$__$$_<routine>[$suffix]` only. Nested routines and
// nested or generic types have other shapes and are refused.
function ParseSection(const ASection: string; out AName: TSectionName): string;
var
  Name, Head, Tail: string;
  Position: Integer;
begin
  AName := Default(TSectionName);
  Name := LowerCase(ASection);
  if Copy(Name, 1, Length(TEXT_PREFIX)) <> TEXT_PREFIX then
    Exit('not a project text section');
  Delete(Name, 1, Length(TEXT_PREFIX));
  Position := Pos(METHOD_SEPARATOR, Name);
  if Position > 0 then
  begin
    Head := Copy(Name, 1, Position - 1);
    Tail := Copy(Name, Position + Length(METHOD_SEPARATOR), MaxInt);
    Position := Pos(TYPE_SEPARATOR, Head);
    if Position = 0 then
      Exit('unsupported method section shape');
    AName.Module := Copy(Head, 1, Position - 1);
    AName.TypeName := Copy(Head, Position + Length(TYPE_SEPARATOR), MaxInt);
    if not IsPlainIdentifier(AName.TypeName) then
      Exit('methods of nested or generic types are not supported');
  end
  else
  begin
    Position := Pos(UNIT_SEPARATOR, Name);
    if Position = 0 then
      Exit('unsupported section shape');
    AName.Module := Copy(Name, 1, Position - 1);
    Tail := Copy(Name, Position + Length(UNIT_SEPARATOR), MaxInt);
  end;
  if (Pos(TYPE_SEPARATOR, AName.Module) > 0) or (Pos(UNIT_SEPARATOR, Tail) > 0) or
    (Pos(TYPE_SEPARATOR, Tail) > 0) then
    Exit('nested routines are not supported');
  if not IsModuleName(AName.Module) then
    Exit('unsupported module name');
  Position := Pos('$', Tail);
  if Position > 0 then
    AName.Routine := Copy(Tail, 1, Position - 1)
  else
    AName.Routine := Tail;
  if not IsPlainIdentifier(AName.Routine) then
    Exit('unsupported routine name');
  Result := '';
end;

function FPCSectionRoutineName(const ASection: string): string;
var
  Name: TSectionName;
begin
  if ParseSection(ASection, Name) <> '' then
    Exit(ASection);
  Result := Name.Module + '.';
  if Name.TypeName <> '' then
    Result := Result + Name.TypeName + '.';
  Result := Result + Name.Routine;
end;

function IsIncludeDirective(const ADirective: TDirective): Boolean;
begin
  Result := (ADirective.Name = 'include') or ((ADirective.Name = 'i') and
    (ADirective.Argument <> '') and not CharInSet(ADirective.Argument[1], ['+', '-']));
end;

procedure RefuseMacros(const ASource: TSourceText);
var
  Directive: TDirective;
begin
  for Directive in ASource.Directives do
    if (Directive.Name = 'macro') or ((Directive.Name = 'define') and (Pos(':=', Directive.Argument) > 0)) then
      Refuse(ASource.FileName, Directive.Line, 'macros can rename routines and are not supported');
end;

function ResolveInclude(const AIncluder: string; const ADirective: TDirective;
  const AIncludeDirectories: array of string): string;
const
  EXTENSIONS: array[0..3] of string = ('', '.inc', '.pp', '.pas');
var
  Name, Directory, Extension, Candidate: string;
  Directories: array of string;
  Index: Integer;
begin
  Name := Trim(ADirective.Argument);
  if (Length(Name) >= 2) and (Name[1] = '''') and (Name[Length(Name)] = '''') then
    Name := Copy(Name, 2, Length(Name) - 2);
  Name := StringReplace(Name, '\', PathDelim, [rfReplaceAll]);
  Name := StringReplace(Name, '/', PathDelim, [rfReplaceAll]);
  SetLength(Directories, Length(AIncludeDirectories) + 1);
  Directories[0] := ExtractFilePath(AIncluder);
  for Index := 0 to High(AIncludeDirectories) do
    Directories[Index + 1] := IncludeTrailingPathDelimiter(AIncludeDirectories[Index]);
  for Directory in Directories do
    for Extension in EXTENSIONS do
    begin
      if (Extension <> '') and (ExtractFileExt(Name) <> '') then
        Continue;
      Candidate := ExpandFileName(Directory + Name + Extension);
      if FileExists(Candidate) then
        Exit(Candidate);
    end;
  Refuse(AIncluder, ADirective.Line, 'include file not found: ' + Name);
  Result := '';
end;

// An include's text is outside every digest, so it must not be able to hold
// a routine or module part the including file's parse does not see.
procedure CheckIncludes(const ASource: TSourceText;
  const AIncludeDirectories: array of string; const ADepth: Integer);
var
  Directive: TDirective;
  Included: TSourceText;
  Token: TToken;
  Index: Integer;
begin
  for Directive in ASource.Directives do
  begin
    if not IsIncludeDirective(Directive) or (Copy(Directive.Argument, 1, 1) = '%') then
      Continue;
    if ADepth >= MAX_INCLUDE_DEPTH then
      Refuse(ASource.FileName, Directive.Line, 'includes nest too deeply');
    Included := LoadSource(ResolveInclude(ASource.FileName, Directive, AIncludeDirectories));
    for Index := 0 to High(Included.Tokens) do
    begin
      Token := Included.Tokens[Index];
      if (Token.Kind = tkWord) and not Token.Escaped and InList(Token.Text, INCLUDE_REFUSED_WORDS) then
        Refuse(Included.FileName, Token.Line,
          'included text can declare routines that no reviewed digest covers: ' + Token.Text);
    end;
    RefuseMacros(Included);
    CheckIncludes(Included, AIncludeDirectories, ADepth + 1);
  end;
end;

// The digest covers every branch of a conditional inside the routine, but the
// span must end at the same `end` whichever branch the compiler takes. A
// group must therefore open and close inside the statement part, and leave
// the block depth unchanged in each branch. Includes inside the routine are
// refused because their text is outside the digest.
procedure CheckRoutineDirectives(const ASource: TSourceText; const ARoutine: TRoutine);
var
  Directive: TDirective;
  Groups: array of Integer;
  First, Finish, TokenIndex, Depth: Integer;
  InAssembler: Boolean;
  Token: TToken;

  procedure RefuseDirective(const AReason: string);
  begin
    Refuse(ASource.FileName, Directive.Line, '{$' + Directive.Name + '} ' + AReason + ' in ' + ARoutine.Name);
  end;

begin
  First := ASource.Tokens[ARoutine.FirstToken].Start;
  Finish := ASource.Tokens[ARoutine.LastToken].Finish;
  Groups := nil;
  TokenIndex := ARoutine.BodyToken;
  Depth := 0;
  InAssembler := False;
  for Directive in ASource.Directives do
  begin
    if (Directive.Start < First) or (Directive.Start >= Finish) then
      Continue;
    if IsIncludeDirective(Directive) then
      RefuseDirective('includes text that no reviewed digest covers');
    if not InList(Directive.Name, CONDITIONAL_DIRECTIVES) then
      Continue;
    if Directive.Start < ASource.Tokens[ARoutine.BodyToken].Start then
      RefuseDirective('is only supported in the statement part');
    while (TokenIndex <= ARoutine.LastToken) and (ASource.Tokens[TokenIndex].Start < Directive.Start) do
    begin
      Token := ASource.Tokens[TokenIndex];
      if (Token.Kind = tkWord) and not Token.Escaped then
      begin
        if Token.Text = 'end' then
        begin
          InAssembler := False;
          Dec(Depth);
        end
        else if not InAssembler and ((Token.Text = 'begin') or (Token.Text = 'case') or
          (Token.Text = 'try') or (Token.Text = 'asm')) then
        begin
          Inc(Depth);
          InAssembler := Token.Text = 'asm';
        end;
      end;
      Inc(TokenIndex);
    end;
    if (Directive.Name = 'else') or (Directive.Name = 'elseif') or
      (Directive.Name = 'endif') or (Directive.Name = 'ifend') then
    begin
      if Length(Groups) = 0 then
        RefuseDirective('closes a conditional opened outside the routine');
      if Groups[High(Groups)] <> Depth then
        RefuseDirective('changes the block structure between conditional branches');
      if (Directive.Name = 'endif') or (Directive.Name = 'ifend') then
        SetLength(Groups, Length(Groups) - 1);
    end
    else
    begin
      SetLength(Groups, Length(Groups) + 1);
      Groups[High(Groups)] := Depth;
    end;
  end;
  if Length(Groups) > 0 then
    Refuse(ASource.FileName, ASource.Tokens[ARoutine.LastToken].Line,
      'a conditional opened in ' + ARoutine.Name + ' closes after it');
end;

function NormalisedDigest(const ABytes: TBytes; const AStart, AFinish: Integer): string;
var
  Normalised: TBytes;
  Index, Count: Integer;
begin
  SetLength(Normalised, AFinish - AStart);
  Count := 0;
  Index := AStart;
  while Index < AFinish do
  begin
    if ABytes[Index] = 13 then
    begin
      Normalised[Count] := 10;
      if (Index + 1 < AFinish) and (ABytes[Index + 1] = 10) then
        Inc(Index);
    end
    else
      Normalised[Count] := ABytes[Index];
    Inc(Count);
    Inc(Index);
  end;
  SetLength(Normalised, Count);
  Result := SHA256Hex(Normalised);
end;

function LocateFPCRoutine(const ASourceFile, ASection: string;
  const AIncludeDirectories: array of string): TFPCRoutineSpan;
var
  Section: TSectionName;
  Source: TSourceText;
  Parser: TRoutineParser;
  Routine, Found: TRoutine;
  Error: string;
  Matches, Last, First, Finish: Integer;
begin
  Result := Default(TFPCRoutineSpan);
  Error := ParseSection(ASection, Section);
  if Error <> '' then
    Refuse(ASourceFile, 0, Error + ': ' + ASection);
  Source := LoadSource(ASourceFile);
  Found := Default(TRoutine);
  Parser := TRoutineParser.Create(Source);
  try
    Parser.Parse;
    if Parser.Module <> Section.Module then
      Refuse(ASourceFile, 0, 'declares ' + Parser.Module + ', not ' + Section.Module);
    Matches := 0;
    for Routine in Parser.Routines do
    begin
      Last := High(Routine.Parts);
      if Routine.Parts[Last] <> Section.Routine then
        Continue;
      if Section.TypeName = '' then
      begin
        if Last <> 0 then
          Continue;
        // A nested namesake has another section, but would make a parse
        // disagreement on nesting select the wrong text.
        if Routine.Level > 0 then
          Refuse(ASourceFile, Source.Tokens[Routine.FirstToken].Line,
            'a nested routine shares the reviewed routine''s name');
      end
      else
      begin
        if (Last = 0) or (Routine.Parts[Last - 1] <> Section.TypeName) then
          Continue;
        if (Last <> 1) or (Routine.Level > 0) then
          Refuse(ASourceFile, Source.Tokens[Routine.FirstToken].Line,
            'methods of nested types are not supported: ' + Routine.Name);
      end;
      if Routine.Generic then
        Refuse(ASourceFile, Source.Tokens[Routine.FirstToken].Line,
          'generic routines are not supported: ' + Routine.Name);
      if not Routine.HasBody then
        Continue;
      Inc(Matches);
      Found := Routine;
    end;
    if Matches = 0 then
      Refuse(ASourceFile, 0, 'no implementation of ' + FPCSectionRoutineName(ASection));
    if Matches > 1 then
      Refuse(ASourceFile, 0, Format('%d implementations of %s; overloads cannot be told apart',
        [Matches, FPCSectionRoutineName(ASection)]));
  finally
    Parser.Free;
  end;
  First := Source.Tokens[Found.FirstToken].Start;
  Finish := Source.Tokens[Found.LastToken].Finish;
  CheckRoutineDirectives(Source, Found);
  RefuseMacros(Source);
  CheckIncludes(Source, AIncludeDirectories, 0);
  Result.Name := Found.Name;
  Result.FirstLine := Source.Tokens[Found.FirstToken].Line;
  Result.LastLine := Source.Tokens[Found.LastToken].Line;
  Result.Digest := NormalisedDigest(Source.Bytes, First, Finish);
end;

end.
