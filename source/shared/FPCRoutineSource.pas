unit FPCRoutineSource;

{$I Shared.inc}

interface

uses
  Classes,
  SysUtils;

type
  // A file refusal breaks every routine pinned in the file: its text cannot
  // be parsed, or could compile to other text than the parse sees. A routine
  // refusal concerns only the routine the section names.
  TFPCRoutineRefusal = (frFile, frRoutine);

  EFPCRoutineSource = class(Exception)
  private
    FSourceFile: string;
    FLine: Integer;
    FReason: string;
    FScope: TFPCRoutineRefusal;
  public
    constructor CreateRefusal(const ASourceFile: string; const ALine: Integer;
      const AReason: string; const AScope: TFPCRoutineRefusal);
    // The file whose text is refused, which can be an include file.
    property SourceFile: string read FSourceFile;
    // 0 when the refusal has no line.
    property Line: Integer read FLine;
    property Reason: string read FReason;
    property Scope: TFPCRoutineRefusal read FScope;
  end;

  // The located implementation of one routine named by an FPC section.
  TFPCRoutineSpan = record
    Name: string;
    FirstLine: Integer;
    LastLine: Integer;
    Digest: string;
  end;

  // Locates routines like LocateFPCRoutine, but parses and checks each source
  // file once, so a manifest run with several routines in one file reads it
  // once. Files must not change during the locator's lifetime.
  TFPCRoutineLocator = class
  private
    // The compiler working directories, then the -Fi directories.
    FIncludeDirectories: array of string;
    FFiles: TStringList;
  public
    // AIncludeDirectories are FPC's -Fi directories. ACompilerDirectories are
    // the working directories the compiler may have run in, which FPC
    // searches for an include between its includer's directory and them.
    constructor Create(const ACompilerDirectories, AIncludeDirectories: array of string);
    destructor Destroy; override;
    function Locate(const ASourceFile, ASection: string): TFPCRoutineSpan;
  end;

// Locates the one implementation in ASourceFile that an FPC `.text.n_`
// section names, and returns the SHA-256 of its text from the first token of
// its header through the semicolon after its final `end`, with CRLF and CR
// line endings normalised to LF. The section's parameter suffix is not
// decoded, so the routine must be the only implementation of its name in
// its scope. Raises EFPCRoutineSource, never a broader or empty span, when
// the section shape is unsupported (nested routines, nested or generic
// types), the source declares another unit or program, the routine is
// missing or ambiguous, or the file could compile to other text than the
// parse sees: unparseable text, another lexing mode, macros, includes that
// could hold routines or change the block structure, or conditionals that
// change the parse between branches (ADR 0180). Include files are searched
// as FPC 3.2.4 searches them: beside their includer, then in
// ACompilerDirectories, then in AIncludeDirectories. A name that matches
// files with different text in any of these is refused, since the file FPC
// takes then depends on its working directory and search order.
function LocateFPCRoutine(const ASourceFile, ASection: string;
  const ACompilerDirectories, AIncludeDirectories: array of string): TFPCRoutineSpan;

// The unit-qualified routine an FPC section names, for diagnostics. Returns
// the section itself when its shape is unsupported.
function FPCSectionRoutineName(const ASection: string): string;

implementation

uses
  SHA256;

const
  TEXT_PREFIX = '.text.n_';
  UNIT_SEPARATOR = '_$$_';
  TYPE_SEPARATOR = '$_$';
  METHOD_SEPARATOR = '_$__$$_';
  PROGRAM_PREFIX = 'p$';
  MAX_INCLUDE_DEPTH = 8;
  // Parser states recorded per token for the conditional and include checks.
  // Other tokens record their routine in the high half, and in the low half
  // their block depth plus ASSEMBLER_STATE inside an assembler block, or
  // TYPE_DEPTH plus their type depth inside a type declaration.
  STATE_UNSUPPORTED = -1;
  STATE_USES = -2;
  TYPE_DEPTH = $40000000;
  ASSEMBLER_STATE = $20000000;
  MODULE_CODE = -1;
  // Tokens after which the parse decides nothing from its neighbours, so a
  // conditional may start or end a branch next to one at declaration level.
  NEUTRAL_WORDS: array[0..10] of string = (
    'implementation', 'const', 'var', 'type', 'threadvar', 'resourcestring',
    'label', 'private', 'protected', 'public', 'published');
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
  // The statement-part block structure: begin, case, try and asm open a
  // block that end closes, and in an assembler block only end counts.
  TBlockScan = record
    Depth: Integer;
    InAssembler: Boolean;
  end;
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
    FStates: array of Int64;
    FCurrentRoutine: Integer;
    FImplementationStart: Integer;
    procedure RefuseAt(const AIndex: Integer; const AMessage: string);
    function StateOf(const ARoutine, ADepth: Integer; const AInType: Boolean): Int64;
    function StatementState(const AScan: TBlockScan): Int64;
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
    // The first token after `implementation`, or after a program's header.
    // It always follows at least one token.
    property ImplementationStart: Integer read FImplementationStart;
    function State(const AIndex: Integer): Int64;
    function IsNeutral(const AIndex: Integer): Boolean;
  end;

{ EFPCRoutineSource }

constructor EFPCRoutineSource.CreateRefusal(const ASourceFile: string; const ALine: Integer;
  const AReason: string; const AScope: TFPCRoutineRefusal);
begin
  if ALine > 0 then
    inherited Create(UTF8Encode(Format('%s:%d: %s', [ASourceFile, ALine, AReason])))
  else
    inherited Create(UTF8Encode(ASourceFile + ': ' + AReason));
  FSourceFile := ASourceFile;
  FLine := ALine;
  FReason := AReason;
  FScope := AScope;
end;

procedure Refuse(const AFileName: string; const ALine: Integer; const AMessage: string);
begin
  raise EFPCRoutineSource.CreateRefusal(AFileName, ALine, AMessage, frFile);
end;

procedure RefuseRoutine(const AFileName: string; const ALine: Integer; const AMessage: string);
begin
  raise EFPCRoutineSource.CreateRefusal(AFileName, ALine, AMessage, frRoutine);
end;

function IsBlockWord(const AToken: TToken; const AText: string): Boolean;
begin
  Result := (AToken.Kind = tkWord) and not AToken.Escaped and (AToken.Text = AText);
end;

// Applies one statement-part token to AScan. Returns True when the token is
// an `end` that closes a block.
function StepBlock(var AScan: TBlockScan; const AToken: TToken): Boolean;
begin
  Result := IsBlockWord(AToken, 'end');
  if Result then
  begin
    AScan.InAssembler := False;
    Dec(AScan.Depth);
  end
  else if not AScan.InAssembler then
  begin
    if IsBlockWord(AToken, 'begin') or IsBlockWord(AToken, 'case') or IsBlockWord(AToken, 'try') then
      Inc(AScan.Depth)
    else if IsBlockWord(AToken, 'asm') then
    begin
      Inc(AScan.Depth);
      AScan.InAssembler := True;
    end;
  end;
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
  Count, Position, Line, Start, StartLine, TokenCount, DirectiveCount: Integer;
  Character: Byte;
  Escaped, IsDirective: Boolean;

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
  var
    NameEnd: Integer;
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
        AddDirective(ByteText(Bytes, Start + 2, Position));
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
var
  Index: Integer;
begin
  inherited Create;
  FSource := ASource;
  SetLength(FStates, Length(FSource.Tokens));
  for Index := 0 to High(FStates) do
    FStates[Index] := STATE_UNSUPPORTED;
  FCurrentRoutine := MODULE_CODE;
end;

function TRoutineParser.State(const AIndex: Integer): Int64;
begin
  if (AIndex < 0) or (AIndex >= Length(FStates)) then
    Exit(STATE_UNSUPPORTED);
  Result := FStates[AIndex];
end;

function TRoutineParser.StateOf(const ARoutine, ADepth: Integer; const AInType: Boolean): Int64;
begin
  Result := (Int64(ARoutine) + 2) shl 32;
  if AInType and (ADepth > 0) then
    Result := Result + TYPE_DEPTH + ADepth
  else
    Result := Result + ADepth;
end;

// An assembler block's text is read differently, so its state differs from
// the same depth outside one.
function TRoutineParser.StatementState(const AScan: TBlockScan): Int64;
begin
  Result := StateOf(FCurrentRoutine, AScan.Depth, False);
  if AScan.InAssembler then
    Result := Result + ASSEMBLER_STATE;
end;

function TRoutineParser.IsNeutral(const AIndex: Integer): Boolean;
begin
  if IsSymbol(AIndex, ';') then
    Exit(True);
  if (AIndex < 0) or (AIndex >= Length(FSource.Tokens)) or (FSource.Tokens[AIndex].Kind <> tkWord) or
    FSource.Tokens[AIndex].Escaped then
    Exit(False);
  Result := InList(FSource.Tokens[AIndex].Text, NEUTRAL_WORDS) and
    not ((FSource.Tokens[AIndex].Text = 'type') and IsSymbol(AIndex - 1, '='));
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
  // `record` is also a generic constraint: `<T: record>`, `<T: record, U>`
  // or `<T: record; U: class>`.
  if IsWord(AIndex, 'record') then
    Exit(not (IsSymbol(AIndex + 1, '>') or IsSymbol(AIndex + 1, ',') or IsSymbol(AIndex + 1, ';')));
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
  Scan: TBlockScan;
begin
  Scan := Default(TBlockScan);
  Scan.Depth := AStartDepth;
  while FPosition < Length(FSource.Tokens) do
  begin
    FStates[FPosition] := StatementState(Scan);
    if not Scan.InAssembler and (IsRoutineKeyword(FPosition) or IsWord(FPosition, 'implementation') or
      IsWord(FPosition, 'initialization') or
      (IsWord(FPosition, 'finalization') and (AStartDepth = 0))) then
      RefuseAt(FPosition, 'declaration keyword inside a statement block');
    if StepBlock(Scan, FSource.Tokens[FPosition]) and (Scan.Depth = 0) then
      Exit;
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
    FStates[FPosition] := StateOf(FCurrentRoutine, TypeDepth, True);
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
    if IsWord(FPosition, 'uses') then
    begin
      // Only unit names, commas and `in` paths: a conditional may choose
      // units here without changing what the parse sees.
      Inc(FPosition);
      while (FPosition < Length(FSource.Tokens)) and not IsSymbol(FPosition, ';') do
      begin
        FStates[FPosition] := STATE_USES;
        Inc(FPosition);
      end;
      if FPosition < Length(FSource.Tokens) then
        FStates[FPosition] := STATE_USES;
      Inc(FPosition);
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
  Index, Outer: Integer;
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
  Index := Length(FRoutines);
  while (FPosition < Length(FSource.Tokens)) and (FSource.Tokens[FPosition].Kind = tkWord) and
    not FSource.Tokens[FPosition].Escaped and
    not InList(FSource.Tokens[FPosition].Text, RESERVED_WORDS) do
  begin
    // A directive such as `{$IFDEF FPC}inline;{$ENDIF}` leaves the routine's
    // declaration part where it was.
    FStates[FPosition] := StateOf(Index, 0, False);
    Directive := FSource.Tokens[FPosition].Text;
    if (Directive = 'forward') or (Directive = 'external') then
      Routine.HasBody := False;
    SkipPastSemicolon;
  end;
  SetLength(FRoutines, Index + 1);
  FRoutines[Index] := Routine;
  if not Routine.HasBody then
  begin
    FRoutines[Index].LastToken := FPosition - 1;
    Exit;
  end;
  Outer := FCurrentRoutine;
  FCurrentRoutine := Index;
  ParseDeclarations(ALevel + 1);
  if not (IsWord(FPosition, 'begin') or IsWord(FPosition, 'asm')) then
    RefuseAt(FPosition, 'expected routine body');
  SkipCode(0);
  FCurrentRoutine := Outer;
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
  FImplementationStart := FPosition;
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
  // The range check comes first: IsIdentifierStart takes a byte.
  Result := (AText <> '') and (Ord(AText[1]) < 128) and IsIdentifierStart(Ord(AText[1]));
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

// The lexer follows Delphi mode, where comments do not nest, and the parse
// takes names literally. Another mode or a macro could make the compiler read
// different text.
procedure RefuseLexingChanges(const ASource: TSourceText);
var
  Directive: TDirective;
  Mode: string;
begin
  for Directive in ASource.Directives do
  begin
    Mode := LowerCase(Directive.Argument);
    if (Directive.Name = 'macro') or ((Directive.Name = 'define') and (Pos(':=', Directive.Argument) > 0)) then
      Refuse(ASource.FileName, Directive.Line, 'macros can rename routines and are not supported');
    if Directive.Name = 'includepath' then
      Refuse(ASource.FileName, Directive.Line, '{$includepath} changes where FPC finds includes and is not supported');
    if ((Directive.Name = 'mode') and (Mode <> 'delphi') and (Mode <> 'delphiunicode')) or
      ((Directive.Name = 'modeswitch') and (Pos('nestedcomments', Mode) = 1)) then
      Refuse(ASource.FileName, Directive.Line, 'only Delphi-mode lexing is supported: {$' +
        Directive.Name + ' ' + Directive.Argument + '}');
  end;
end;

// cfileutl.pas path_absolute on the host the compiler runs on.
function IsAbsoluteIncludePath(const AName: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := ((Length(AName) > 0) and CharInSet(AName[1], ['\', '/'])) or
    ((Length(AName) > 2) and (AName[2] = ':') and CharInSet(AName[3], ['\', '/']));
{$ELSE}
  Result := (Length(AName) > 0) and (AName[1] = '/');
{$ENDIF}
end;

function SameBytes(const ALeft, ARight: TBytes): Boolean;
begin
  Result := (Length(ALeft) = Length(ARight)) and
    ((Length(ALeft) = 0) or CompareMem(@ALeft[0], @ARight[0], Length(ALeft)));
end;

// FPC 3.2.4 (scanner.pas dir_include and findincludefile, cfileutl.pas
// FindFile and FileExistsNonCase) tries the name as written, then, without
// an extension, with .inc, .pp and .pas. For each it searches the
// includer's directory, the compiler's working directory, any
// {$INCLUDEPATH}, then each -Fi directory, or only the named directory of an
// absolute name. On a case-sensitive file system it tries each path as
// written, lowercased, then uppercased. Every candidate is checked, and all
// that exist must hold the same bytes, so FPC's choice cannot change the text.
function ResolveInclude(const AIncluder: string; const ADirective: TDirective;
  const AIncludeDirectories: array of string): string;
const
  EXTENSIONS: array[0..3] of string = ('', '.inc', '.pp', '.pas');
var
  Name, Directory, Extension, Candidate, Variant: string;
  Directories, Variants: array of string;
  Found: TBytes;
  Index: Integer;
begin
  Name := Trim(ADirective.Argument);
  if (Length(Name) >= 2) and (Name[1] = '''') and (Name[Length(Name)] = '''') then
    Name := Copy(Name, 2, Length(Name) - 2);
  Name := StringReplace(Name, '\', string(PathDelim), [rfReplaceAll]);
  Name := StringReplace(Name, '/', string(PathDelim), [rfReplaceAll]);
  // `*` names the including source file; a final `.` is retried without it.
  // A drive-relative name such as C:name depends on the drive's directory.
  if (ExtractFileName(Name) = '') or (ExtractFileName(Name)[1] = '*') or (Name[Length(Name)] = '.') or
    ((Pos(':', Name) > 0) and not IsAbsoluteIncludePath(Name)) then
    Refuse(AIncluder, ADirective.Line, 'unsupported include name: ' + Name);
  if IsAbsoluteIncludePath(Name) then
  begin
    SetLength(Directories, 1);
    Directories[0] := '';
  end
  else
  begin
    SetLength(Directories, Length(AIncludeDirectories) + 1);
    Directories[0] := ExtractFilePath(AIncluder);
    for Index := 0 to High(AIncludeDirectories) do
      Directories[Index + 1] := IncludeTrailingPathDelimiter(AIncludeDirectories[Index]);
  end;
  Result := '';
  Found := nil;
  for Extension in EXTENSIONS do
  begin
    if (Extension <> '') and (ExtractFileExt(Name) <> '') then
      Continue;
    for Directory in Directories do
    begin
      SetLength(Variants, 1);
      Variants[0] := Name + Extension;
      if FileNameCaseSensitive then
      begin
        SetLength(Variants, 3);
        Variants[1] := LowerCase(Name + Extension);
        Variants[2] := UpperCase(Name + Extension);
      end;
      for Variant in Variants do
      begin
        Candidate := ExpandFileName(Directory + Variant);
        if not FileExists(Candidate) then
          Continue;
        if Result = '' then
        begin
          Result := Candidate;
          Found := ReadSourceBytes(Candidate);
        end
        else if (Candidate <> Result) and not SameBytes(Found, ReadSourceBytes(Candidate)) then
          Refuse(AIncluder, ADirective.Line, 'include ' + Name + ' names different files, ' + Result +
            ' and ' + Candidate + ', so the one FPC reads depends on its working directory and search order');
      end;
    end;
  end;
  if Result = '' then
    Refuse(AIncluder, ADirective.Line, 'include file not found: ' + Name);
end;

type
  TIncludeContext = (icDeclarations, icStatements);

// An include's text is outside every digest and outside the parse, so it
// must leave the parse's view of its includer true. It must not declare a
// routine or module part, change lexing, or leave a conditional open. In a
// statement block its blocks must close within it, and its conditionals must
// not change them between branches; anywhere else it may hold only
// directives and comments, which cannot open or close a type or routine.
procedure CheckIncludedText(const AIncluder: TSourceText; const ADirective: TDirective;
  const AContext: TIncludeContext; const AIncludeDirectories: array of string;
  const ADepth: Integer);
var
  Included: TSourceText;
  Scan: TBlockScan;
  Groups: array of TBlockScan;
  Token: TToken;
  Directive: TDirective;
  TokenIndex, DirectiveIndex: Integer;

  procedure RefuseDirective(const AReason: string);
  begin
    Refuse(Included.FileName, Directive.Line, '{$' + Directive.Name + '} ' + AReason);
  end;

begin
  if ADepth >= MAX_INCLUDE_DEPTH then
    Refuse(AIncluder.FileName, ADirective.Line, 'includes nest too deeply');
  Included := LoadSource(ResolveInclude(AIncluder.FileName, ADirective, AIncludeDirectories));
  for Token in Included.Tokens do
    if (Token.Kind = tkWord) and not Token.Escaped and InList(Token.Text, INCLUDE_REFUSED_WORDS) then
      Refuse(Included.FileName, Token.Line,
        'included text can declare routines that no reviewed digest covers: ' + Token.Text);
  RefuseLexingChanges(Included);
  Scan := Default(TBlockScan);
  Groups := nil;
  TokenIndex := 0;
  // Tokens and directives in text order; the last pass takes the tokens
  // after the last directive.
  for DirectiveIndex := 0 to Length(Included.Directives) do
  begin
    while (TokenIndex < Length(Included.Tokens)) and ((DirectiveIndex = Length(Included.Directives)) or
      (Included.Tokens[TokenIndex].Start < Included.Directives[DirectiveIndex].Start)) do
    begin
      Token := Included.Tokens[TokenIndex];
      if AContext <> icStatements then
        Refuse(Included.FileName, Token.Line,
          'included text outside a statement block must hold only directives and comments');
      if StepBlock(Scan, Token) and (Scan.Depth < 0) then
        Refuse(Included.FileName, Token.Line, 'included text closes a block it did not open');
      Inc(TokenIndex);
    end;
    if DirectiveIndex = Length(Included.Directives) then
      Break;
    Directive := Included.Directives[DirectiveIndex];
    if InList(Directive.Name, CONDITIONAL_DIRECTIVES) then
    begin
      if (Directive.Name = 'else') or (Directive.Name = 'elseif') or
        (Directive.Name = 'endif') or (Directive.Name = 'ifend') then
      begin
        if Length(Groups) = 0 then
          RefuseDirective('has no opening conditional in its include file');
        if (Groups[High(Groups)].Depth <> Scan.Depth) or
          (Groups[High(Groups)].InAssembler <> Scan.InAssembler) then
          RefuseDirective('changes the block structure between conditional branches');
        if (Directive.Name = 'endif') or (Directive.Name = 'ifend') then
          SetLength(Groups, Length(Groups) - 1);
      end
      else
      begin
        SetLength(Groups, Length(Groups) + 1);
        Groups[High(Groups)] := Scan;
      end;
    end
    else if IsIncludeDirective(Directive) and (Copy(Directive.Argument, 1, 1) <> '%') then
    begin
      if Scan.InAssembler then
        RefuseDirective('inside an assembler block is not supported');
      CheckIncludedText(Included, Directive, AContext, AIncludeDirectories, ADepth + 1);
    end;
  end;
  if Length(Groups) > 0 then
    Refuse(Included.FileName, 0, 'a conditional is not closed in its include file');
  if Scan.Depth <> 0 then
    Refuse(Included.FileName, 0, 'included text leaves a block open');
end;

// `{$I %NAME%}` inserts a string literal, not a file.
procedure CheckIncludes(const ASource: TSourceText; const AParser: TRoutineParser;
  const AIncludeDirectories: array of string);
var
  Directive: TDirective;
  NextToken: Integer;
  State, Low: Int64;
begin
  NextToken := 0;
  for Directive in ASource.Directives do
  begin
    if not IsIncludeDirective(Directive) or (Copy(Directive.Argument, 1, 1) = '%') then
      Continue;
    while (NextToken < Length(ASource.Tokens)) and (ASource.Tokens[NextToken].Start < Directive.Start) do
      Inc(NextToken);
    // The included text takes the place of the parse state at the next token.
    State := AParser.State(NextToken);
    Low := State and $FFFFFFFF;
    if (State < 0) or (Low = 0) or (Low >= TYPE_DEPTH) then
      CheckIncludedText(ASource, Directive, icDeclarations, AIncludeDirectories, 0)
    else if Low >= ASSEMBLER_STATE then
      Refuse(ASource.FileName, Directive.Line, '{$' + Directive.Name +
        '} inside an assembler block is not supported')
    else
      CheckIncludedText(ASource, Directive, icStatements, AIncludeDirectories, 0);
  end;
end;

// The parse reads every branch of a conditional, but the compiler reads only
// one, so branches must not change what the parse sees. In the implementation
// part each group must stay in one routine's statements, one declaration
// level or type body, or one uses clause, with the same depth at each of its
// directives. At declaration level a branch must also begin and end next to a
// token the parse decides nothing from (`;` or a section keyword). Then every
// branch choice gives the same routines and spans, and the digest covers
// every branch inside the reviewed routine.
procedure CheckConditionals(const ASource: TSourceText; const AParser: TRoutineParser);
type
  TGroup = record
    State: Int64;
    Enforced: Boolean;
  end;
var
  Directive: TDirective;
  Groups: array of TGroup;
  NextToken, ImplementationOffset: Integer;
  State, Depth: Int64;
  Enforced: Boolean;

  procedure RefuseDirective(const AReason: string);
  begin
    Refuse(ASource.FileName, Directive.Line, '{$' + Directive.Name + '} ' + AReason);
  end;

begin
  Groups := nil;
  NextToken := 0;
  // A directive between `implementation` and its first token belongs to it.
  ImplementationOffset := ASource.Tokens[AParser.ImplementationStart - 1].Finish;
  for Directive in ASource.Directives do
  begin
    if not InList(Directive.Name, CONDITIONAL_DIRECTIVES) then
      Continue;
    while (NextToken < Length(ASource.Tokens)) and (ASource.Tokens[NextToken].Start < Directive.Start) do
      Inc(NextToken);
    State := AParser.State(NextToken);
    Enforced := Directive.Start >= ImplementationOffset;
    if Enforced then
    begin
      if State = STATE_UNSUPPORTED then
        RefuseDirective('is not supported inside a routine header or directive in a file with a reviewed routine');
      Depth := State and $FFFFFFFF;
      if (State <> STATE_USES) and ((Depth = 0) or (Depth > TYPE_DEPTH)) and
        not AParser.IsNeutral(NextToken - 1) then
        RefuseDirective('must follow ; or a section keyword at declaration level in a file with a reviewed routine');
    end;
    if (Directive.Name = 'else') or (Directive.Name = 'elseif') or
      (Directive.Name = 'endif') or (Directive.Name = 'ifend') then
    begin
      if Length(Groups) = 0 then
        RefuseDirective('has no opening conditional');
      if Enforced and (not Groups[High(Groups)].Enforced or (Groups[High(Groups)].State <> State)) then
        RefuseDirective('changes the block structure between conditional branches');
      if (Directive.Name = 'endif') or (Directive.Name = 'ifend') then
        SetLength(Groups, Length(Groups) - 1);
    end
    else
    begin
      SetLength(Groups, Length(Groups) + 1);
      Groups[High(Groups)].State := State;
      Groups[High(Groups)].Enforced := Enforced;
    end;
  end;
  if Length(Groups) > 0 then
    Refuse(ASource.FileName, 0, 'a conditional is not closed');
end;

// An include inside the routine adds text that no digest covers.
procedure RefuseRoutineIncludes(const ASource: TSourceText; const ARoutine: TRoutine);
var
  Directive: TDirective;
  First, Finish: Integer;
begin
  First := ASource.Tokens[ARoutine.FirstToken].Start;
  Finish := ASource.Tokens[ARoutine.LastToken].Finish;
  for Directive in ASource.Directives do
    if (Directive.Start >= First) and (Directive.Start < Finish) and IsIncludeDirective(Directive) then
      RefuseRoutine(ASource.FileName, Directive.Line, '{$' + Directive.Name +
        '} includes text that no reviewed digest covers in ' + ARoutine.Name);
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

type
  TStoredRefusal = record
    Refused: Boolean;
    SourceFile: string;
    Line: Integer;
    Reason: string;
  end;

  // One source file's parse and whole-file checks.
  TParsedSource = class
  public
    Source: TSourceText;
    Module: string;
    Routines: TRoutines;
    // Raised before any routine is matched.
    ParseRefusal: TStoredRefusal;
    // Raised after the routine's own checks, as the checks ran before.
    CheckRefusal: TStoredRefusal;
  end;

function StoredRefusal(const AError: EFPCRoutineSource): TStoredRefusal;
begin
  Result.Refused := True;
  Result.SourceFile := AError.SourceFile;
  Result.Line := AError.Line;
  Result.Reason := AError.Reason;
end;

procedure RaiseStored(const ARefusal: TStoredRefusal);
begin
  if ARefusal.Refused then
    Refuse(ARefusal.SourceFile, ARefusal.Line, ARefusal.Reason);
end;

function ParseSource(const AFileName: string; const AIncludeDirectories: array of string): TParsedSource;
var
  Parser: TRoutineParser;
begin
  Result := TParsedSource.Create;
  try
    Result.Source := LoadSource(AFileName);
    RefuseLexingChanges(Result.Source);
    Parser := TRoutineParser.Create(Result.Source);
    try
      Parser.Parse;
      Result.Module := Parser.Module;
      Result.Routines := Parser.Routines;
      try
        CheckConditionals(Result.Source, Parser);
        CheckIncludes(Result.Source, Parser, AIncludeDirectories);
      except
        on E: EFPCRoutineSource do
          Result.CheckRefusal := StoredRefusal(E);
      end;
    finally
      Parser.Free;
    end;
  except
    on E: EFPCRoutineSource do
      Result.ParseRefusal := StoredRefusal(E);
    else
    begin
      Result.Free;
      raise;
    end;
  end;
end;

{ TFPCRoutineLocator }

constructor TFPCRoutineLocator.Create(const ACompilerDirectories, AIncludeDirectories: array of string);
var
  Index: Integer;
begin
  inherited Create;
  SetLength(FIncludeDirectories, Length(ACompilerDirectories) + Length(AIncludeDirectories));
  for Index := 0 to High(ACompilerDirectories) do
    FIncludeDirectories[Index] := ACompilerDirectories[Index];
  for Index := 0 to High(AIncludeDirectories) do
    FIncludeDirectories[Length(ACompilerDirectories) + Index] := AIncludeDirectories[Index];
  FFiles := TStringList.Create;
  FFiles.Sorted := True;
  FFiles.CaseSensitive := True;
  FFiles.OwnsObjects := True;
end;

destructor TFPCRoutineLocator.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TFPCRoutineLocator.Locate(const ASourceFile, ASection: string): TFPCRoutineSpan;
var
  Section: TSectionName;
  Parsed: TParsedSource;
  Routine, Found: TRoutine;
  Error, Key: string;
  Index, Matches, Last: Integer;
begin
  Result := Default(TFPCRoutineSpan);
  Error := ParseSection(ASection, Section);
  if Error <> '' then
    RefuseRoutine(ASourceFile, 0, Error + ': ' + ASection);
  Key := ExpandFileName(ASourceFile);
  Index := FFiles.IndexOf(UTF8Encode(Key));
  if Index >= 0 then
    Parsed := TParsedSource(FFiles.Objects[Index])
  else
  begin
    Parsed := ParseSource(ASourceFile, FIncludeDirectories);
    FFiles.AddObject(UTF8Encode(Key), Parsed);
  end;
  RaiseStored(Parsed.ParseRefusal);
  if Parsed.Module <> Section.Module then
    RefuseRoutine(ASourceFile, 0, 'declares ' + Parsed.Module + ', not ' + Section.Module);
  Found := Default(TRoutine);
  Matches := 0;
  for Routine in Parsed.Routines do
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
        RefuseRoutine(ASourceFile, Parsed.Source.Tokens[Routine.FirstToken].Line,
          'a nested routine shares the reviewed routine''s name');
    end
    else
    begin
      if (Last = 0) or (Routine.Parts[Last - 1] <> Section.TypeName) then
        Continue;
      if (Last <> 1) or (Routine.Level > 0) then
        RefuseRoutine(ASourceFile, Parsed.Source.Tokens[Routine.FirstToken].Line,
          'methods of nested types are not supported: ' + Routine.Name);
    end;
    if Routine.Generic then
      RefuseRoutine(ASourceFile, Parsed.Source.Tokens[Routine.FirstToken].Line,
        'generic routines are not supported: ' + Routine.Name);
    if not Routine.HasBody then
      Continue;
    Inc(Matches);
    Found := Routine;
  end;
  if Matches = 0 then
    RefuseRoutine(ASourceFile, 0, 'no implementation of ' + FPCSectionRoutineName(ASection));
  if Matches > 1 then
    RefuseRoutine(ASourceFile, 0, IntToStr(Matches) + ' implementations of ' +
      FPCSectionRoutineName(ASection) + '; overloads cannot be told apart');
  RefuseRoutineIncludes(Parsed.Source, Found);
  RaiseStored(Parsed.CheckRefusal);
  Result.Name := Found.Name;
  Result.FirstLine := Parsed.Source.Tokens[Found.FirstToken].Line;
  Result.LastLine := Parsed.Source.Tokens[Found.LastToken].Line;
  Result.Digest := NormalisedDigest(Parsed.Source.Bytes, Parsed.Source.Tokens[Found.FirstToken].Start,
    Parsed.Source.Tokens[Found.LastToken].Finish);
end;

function LocateFPCRoutine(const ASourceFile, ASection: string;
  const ACompilerDirectories, AIncludeDirectories: array of string): TFPCRoutineSpan;
var
  Locator: TFPCRoutineLocator;
begin
  Locator := TFPCRoutineLocator.Create(ACompilerDirectories, AIncludeDirectories);
  try
    Result := Locator.Locate(ASourceFile, ASection);
  finally
    Locator.Free;
  end;
end;

end.
