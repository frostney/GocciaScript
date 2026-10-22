unit SandboxGlob;

{ Glob patterns over the sandbox virtual filesystem: the name search behind
  the sandbox fs.globSync, and the file selection that content and fuzzy
  search share. docs/built-ins-sandbox-search.md is the specification and
  fixtures/sandbox-search/vectors.json pins it. Matching works on UTF-16 code
  units, as Node's fs.globSync does, and never changes a timestamp. }

{$I Shared.inc}

interface

uses
  SysUtils,

  SandboxVirtualFileSystem;

const
  // The most alternatives brace expansion may produce for one pattern:
  // "{a,b}" repeated n times expands to 2^n patterns.
  MAX_SANDBOX_GLOB_ALTERNATIVES = 1024;

type
  { A pattern the syntax refuses: a '..' segment, or too many brace
    alternatives. The script sees a TypeError. }
  ESandboxGlobError = class(Exception);

  TSandboxGlobTokenKind = (gtkLiteral, gtkStar, gtkAnyCharacter, gtkClass);

  TSandboxGlobRange = record
    Low: Char;
    High: Char;
  end;

  TSandboxGlobToken = record
    Kind: TSandboxGlobTokenKind;
    Literal: Char;
    Negated: Boolean;
    Ranges: array of TSandboxGlobRange;
  end;

  TSandboxGlobSegment = record
    Globstar: Boolean;
    { The segment starts with a literal '.', so it may match a hidden name. }
    DotLiteral: Boolean;
    Tokens: array of TSandboxGlobToken;
  end;

  TSandboxGlobPattern = record
    Absolute: Boolean;
    DirectoryOnly: Boolean;
    Segments: array of TSandboxGlobSegment;
  end;

  TSandboxGlobPatternArray = array of TSandboxGlobPattern;

  { Called once for every entry a walk visits, so the caller can enforce its
    time and instruction limits. }
  TSandboxGlobProgress = procedure of object;

  TSandboxSearchFile = record
    Path: string;
    Size: Int64;
  end;

  TSandboxSearchFileArray = array of TSandboxSearchFile;

{ Compiles every pattern, brace alternatives expanded. A pattern with no
  segments left (empty, or only '/' and '.') contributes nothing. }
function CompileSandboxGlobs(const APatterns: array of string):
  TSandboxGlobPatternArray;

{ fs.globSync: the entries below ACwd (a canonical sandbox path) that match a
  pattern and no exclude, relative to ACwd for a relative pattern and
  absolute for an absolute one, deduplicated and sorted. }
function GlobSandbox(const AFileSystem: TSandboxVirtualFileSystem;
  const APatterns, AExcludes: TSandboxGlobPatternArray; const ACwd: string;
  const AProgress: TSandboxGlobProgress): TStringArray;

{ The files content and fuzzy search look at: every file below ACwd, or
  with AHasGlobs only those matching one of AGlobs, minus the excluded ones;
  absolute paths, sorted. }
function SandboxSearchFiles(const AFileSystem: TSandboxVirtualFileSystem;
  const ACwd: string; const AHasGlobs: Boolean;
  const AGlobs, AExcludes: TSandboxGlobPatternArray;
  const AProgress: TSandboxGlobProgress): TSandboxSearchFileArray;

{ UTF-16 code unit order, JavaScript's default sort. }
function CompareSandboxPaths(const A, B: string): Integer;
procedure SortSandboxPaths(var APaths: TStringArray);

implementation

const
  PATH_SEPARATOR = '/';
  ALTERNATE_PATH_SEPARATOR = '\';
  HIDDEN_PREFIX = '.';
  CURRENT_DIRECTORY_SEGMENT = '.';
  PARENT_DIRECTORY_SEGMENT = '..';
  GLOBSTAR_SEGMENT = '**';

{ Brace expansion }

procedure ExpandBraces(const APattern: string; var AResults: TStringArray;
  var ACount: Integer);
var
  Closing: array of Integer;
  HasComma: array of Boolean;
  Stack: array of Integer;
  Bounds: array of Integer;
  StackCount, Index, OpenIndex, Part: Integer;
  Prefix, Suffix: string;
begin
  { Pair every brace in one pass: Closing[i] is the index of the brace that
    closes the one at i (0 when nothing does), HasComma[i] whether a comma
    sits directly inside it. }
  Closing := nil;
  HasComma := nil;
  Stack := nil;
  SetLength(Closing, Length(APattern) + 1);
  SetLength(HasComma, Length(APattern) + 1);
  SetLength(Stack, Length(APattern) + 1);
  StackCount := 0;
  for Index := 1 to Length(APattern) do
    case APattern[Index] of
      '{':
        begin
          Stack[StackCount] := Index;
          Inc(StackCount);
        end;
      ',':
        if StackCount > 0 then
          HasComma[Stack[StackCount - 1]] := True;
      '}':
        if StackCount > 0 then
        begin
          Dec(StackCount);
          Closing[Stack[StackCount]] := Index;
        end;
    end;

  // The first brace that has no closing brace makes the rest literal; the
  // first one with a comma expands; a brace with no comma is literal.
  for OpenIndex := 1 to Length(APattern) do
  begin
    if APattern[OpenIndex] <> '{' then
      Continue;
    if Closing[OpenIndex] = 0 then
      Break;
    if not HasComma[OpenIndex] then
      Continue;
    Bounds := nil;
    SetLength(Bounds, 1);
    Bounds[0] := OpenIndex;
    StackCount := 0;
    for Index := OpenIndex + 1 to Closing[OpenIndex] - 1 do
      case APattern[Index] of
        '{':
          Inc(StackCount);
        '}':
          Dec(StackCount);
        ',':
          if StackCount = 0 then
          begin
            SetLength(Bounds, Length(Bounds) + 1);
            Bounds[High(Bounds)] := Index;
          end;
      end;
    SetLength(Bounds, Length(Bounds) + 1);
    Bounds[High(Bounds)] := Closing[OpenIndex];
    Prefix := Copy(APattern, 1, OpenIndex - 1);
    Suffix := Copy(APattern, Closing[OpenIndex] + 1, MaxInt);
    for Part := 0 to Length(Bounds) - 2 do
      ExpandBraces(Prefix + Copy(APattern, Bounds[Part] + 1,
        Bounds[Part + 1] - Bounds[Part] - 1) + Suffix, AResults, ACount);
    Exit;
  end;
  if ACount >= MAX_SANDBOX_GLOB_ALTERNATIVES then
    raise ESandboxGlobError.CreateFmt(
      'glob pattern expands to more than %d alternatives',
      [MAX_SANDBOX_GLOB_ALTERNATIVES]);
  if ACount >= Length(AResults) then
    SetLength(AResults, ACount * 2 + 4);
  AResults[ACount] := APattern;
  Inc(ACount);
end;

{ Compilation }

procedure AddToken(var ASegment: TSandboxGlobSegment;
  const AToken: TSandboxGlobToken);
begin
  SetLength(ASegment.Tokens, Length(ASegment.Tokens) + 1);
  ASegment.Tokens[High(ASegment.Tokens)] := AToken;
end;

function LiteralToken(const ACharacter: Char): TSandboxGlobToken;
begin
  Result := Default(TSandboxGlobToken);
  Result.Kind := gtkLiteral;
  Result.Literal := ACharacter;
end;

function CompileSegment(const AText: string): TSandboxGlobSegment;
var
  Index, Cursor: Integer;
  Token: TSandboxGlobToken;
  First, Closed: Boolean;
begin
  Result := Default(TSandboxGlobSegment);
  if AText = GLOBSTAR_SEGMENT then
  begin
    Result.Globstar := True;
    Exit;
  end;
  Index := 1;
  while Index <= Length(AText) do
  begin
    case AText[Index] of
      '*':
        begin
          if (Length(Result.Tokens) = 0) or
             (Result.Tokens[High(Result.Tokens)].Kind <> gtkStar) then
          begin
            Token := Default(TSandboxGlobToken);
            Token.Kind := gtkStar;
            AddToken(Result, Token);
          end;
          Inc(Index);
        end;
      '?':
        begin
          Token := Default(TSandboxGlobToken);
          Token.Kind := gtkAnyCharacter;
          AddToken(Result, Token);
          Inc(Index);
        end;
      '[':
        begin
          Token := Default(TSandboxGlobToken);
          Token.Kind := gtkClass;
          Cursor := Index + 1;
          if (Cursor <= Length(AText)) and
             ((AText[Cursor] = '!') or (AText[Cursor] = '^')) then
          begin
            Token.Negated := True;
            Inc(Cursor);
          end;
          First := True;
          Closed := False;
          while Cursor <= Length(AText) do
          begin
            if (AText[Cursor] = ']') and not First then
            begin
              Closed := True;
              Break;
            end;
            First := False;
            SetLength(Token.Ranges, Length(Token.Ranges) + 1);
            Token.Ranges[High(Token.Ranges)].Low := AText[Cursor];
            if (Cursor + 2 <= Length(AText)) and (AText[Cursor + 1] = '-') and
               (AText[Cursor + 2] <> ']') then
            begin
              Token.Ranges[High(Token.Ranges)].High := AText[Cursor + 2];
              Inc(Cursor, 3);
            end
            else
            begin
              Token.Ranges[High(Token.Ranges)].High := AText[Cursor];
              Inc(Cursor);
            end;
          end;
          if Closed then
          begin
            AddToken(Result, Token);
            Index := Cursor + 1;
          end
          else
          begin
            { A '[' with no closing ']' is literal. }
            AddToken(Result, LiteralToken('['));
            Inc(Index);
          end;
        end;
    else
      AddToken(Result, LiteralToken(AText[Index]));
      Inc(Index);
    end;
  end;
  Result.DotLiteral := (Length(Result.Tokens) > 0) and
    (Result.Tokens[0].Kind = gtkLiteral) and
    (Result.Tokens[0].Literal = HIDDEN_PREFIX);
end;

function CompileAlternative(const AText: string;
  out APattern: TSandboxGlobPattern): Boolean;
var
  Start, Index: Integer;
  Part: string;
begin
  APattern := Default(TSandboxGlobPattern);
  APattern.Absolute := (Length(AText) > 0) and (AText[1] = PATH_SEPARATOR);
  APattern.DirectoryOnly := (Length(AText) > 0) and
    (AText[Length(AText)] = PATH_SEPARATOR);
  Start := 1;
  for Index := 1 to Length(AText) + 1 do
    if (Index > Length(AText)) or (AText[Index] = PATH_SEPARATOR) then
    begin
      Part := Copy(AText, Start, Index - Start);
      Start := Index + 1;
      if (Part = '') or (Part = CURRENT_DIRECTORY_SEGMENT) then
        Continue;
      if Part = PARENT_DIRECTORY_SEGMENT then
        raise ESandboxGlobError.Create(
          'glob patterns may not contain ''..'' segments');
      SetLength(APattern.Segments, Length(APattern.Segments) + 1);
      APattern.Segments[High(APattern.Segments)] := CompileSegment(Part);
    end;
  Result := Length(APattern.Segments) > 0;
end;

function CompileSandboxGlobs(const APatterns: array of string):
  TSandboxGlobPatternArray;
var
  Alternatives: TStringArray;
  AlternativeCount, PatternIndex, Index, Count: Integer;
  Compiled: TSandboxGlobPattern;
begin
  Result := nil;
  Count := 0;
  for PatternIndex := 0 to High(APatterns) do
  begin
    Alternatives := nil;
    AlternativeCount := 0;
    ExpandBraces(StringReplace(APatterns[PatternIndex],
      ALTERNATE_PATH_SEPARATOR, PATH_SEPARATOR, [rfReplaceAll]),
      Alternatives, AlternativeCount);
    for Index := 0 to AlternativeCount - 1 do
      if CompileAlternative(Alternatives[Index], Compiled) then
      begin
        if Count >= Length(Result) then
          SetLength(Result, Count * 2 + 4);
        Result[Count] := Compiled;
        Inc(Count);
      end;
  end;
  SetLength(Result, Count);
end;

{ Matching }

function TokenMatches(const AToken: TSandboxGlobToken;
  const ACharacter: Char): Boolean;
var
  Index: Integer;
  InClass: Boolean;
begin
  case AToken.Kind of
    gtkLiteral:
      Result := ACharacter = AToken.Literal;
    gtkAnyCharacter:
      Result := True;
    gtkClass:
      begin
        InClass := False;
        for Index := 0 to High(AToken.Ranges) do
          if (ACharacter >= AToken.Ranges[Index].Low) and
             (ACharacter <= AToken.Ranges[Index].High) then
          begin
            InClass := True;
            Break;
          end;
        Result := InClass <> AToken.Negated;
      end;
  else
    Result := False;
  end;
end;

{ One segment against one name. A '*' matches any run, so the last '*' seen
  is the only backtracking point needed: linear in the name per star. }
function SegmentMatches(const ASegment: TSandboxGlobSegment;
  const AName: string): Boolean;
var
  TokenIndex, NameIndex, StarToken, StarName, TokenCount: Integer;
begin
  if (Length(AName) > 0) and (AName[1] = HIDDEN_PREFIX) and
     not ASegment.DotLiteral then
    Exit(False);
  TokenCount := Length(ASegment.Tokens);
  TokenIndex := 0;
  NameIndex := 1;
  StarToken := -1;
  StarName := 0;
  while NameIndex <= Length(AName) do
  begin
    if (TokenIndex < TokenCount) and
       (ASegment.Tokens[TokenIndex].Kind = gtkStar) then
    begin
      StarToken := TokenIndex;
      StarName := NameIndex;
      Inc(TokenIndex);
    end
    else if (TokenIndex < TokenCount) and
       TokenMatches(ASegment.Tokens[TokenIndex], AName[NameIndex]) then
    begin
      Inc(TokenIndex);
      Inc(NameIndex);
    end
    else if StarToken >= 0 then
    begin
      TokenIndex := StarToken + 1;
      Inc(StarName);
      NameIndex := StarName;
    end
    else
      Exit(False);
  end;
  while (TokenIndex < TokenCount) and
        (ASegment.Tokens[TokenIndex].Kind = gtkStar) do
    Inc(TokenIndex);
  Result := TokenIndex = TokenCount;
end;

{ The pattern's segments against the names, by dynamic programming so that
  several '**' cost the product of the two lengths, not an exponential. }
function SegmentsMatch(const APattern: TSandboxGlobPattern;
  const ANames: TStringArray; const AFirstName: Integer): Boolean;
var
  SegmentCount, NameCount, SegmentIndex, NameIndex: Integer;
  Next, Current: array of Boolean;
  Segment: TSandboxGlobSegment;
begin
  SegmentCount := Length(APattern.Segments);
  NameCount := Length(ANames) - AFirstName;
  { Next[n]: segments after SegmentIndex match names n.. }
  SetLength(Next, NameCount + 1);
  SetLength(Current, NameCount + 1);
  for NameIndex := 0 to NameCount do
    Next[NameIndex] := NameIndex = NameCount;
  for SegmentIndex := SegmentCount - 1 downto 0 do
  begin
    Segment := APattern.Segments[SegmentIndex];
    if Segment.Globstar then
    begin
      Current[NameCount] := Next[NameCount];
      for NameIndex := NameCount - 1 downto 0 do
        Current[NameIndex] := Next[NameIndex] or
          ((ANames[AFirstName + NameIndex][1] <> HIDDEN_PREFIX) and
           Current[NameIndex + 1]);
    end
    else
    begin
      Current[NameCount] := False;
      for NameIndex := NameCount - 1 downto 0 do
        Current[NameIndex] := Next[NameIndex + 1] and
          SegmentMatches(Segment, ANames[AFirstName + NameIndex]);
    end;
    Next := Copy(Current);
  end;
  Result := Next[0];
end;

function PatternMatches(const APattern: TSandboxGlobPattern;
  const ANames: TStringArray; const ACwdDepth: Integer;
  const AUnderCwd, AIsDirectory: Boolean): Boolean;
begin
  if APattern.DirectoryOnly and not AIsDirectory then
    Exit(False);
  if APattern.Absolute then
    Result := SegmentsMatch(APattern, ANames, 0)
  else
    Result := AUnderCwd and SegmentsMatch(APattern, ANames, ACwdDepth);
end;

function AnyPatternMatches(const APatterns: TSandboxGlobPatternArray;
  const ANames: TStringArray; const ACwdDepth: Integer;
  const AUnderCwd, AIsDirectory: Boolean): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(APatterns) do
    if PatternMatches(APatterns[Index], ANames, ACwdDepth, AUnderCwd,
      AIsDirectory) then
      Exit(True);
  Result := False;
end;

{ Ordering }

function CompareSandboxPaths(const A, B: string): Integer;
var
  Index, Shorter: Integer;
begin
  Shorter := Length(A);
  if Length(B) < Shorter then
    Shorter := Length(B);
  for Index := 1 to Shorter do
    if A[Index] <> B[Index] then
    begin
      if Ord(A[Index]) < Ord(B[Index]) then
        Exit(-1);
      Exit(1);
    end;
  if Length(A) < Length(B) then
    Result := -1
  else if Length(A) > Length(B) then
    Result := 1
  else
    Result := 0;
end;

procedure MergeSortPaths(var APaths, AScratch: TStringArray;
  const ALow, AHigh: Integer);
var
  Middle, Left, Right, Target: Integer;
begin
  if AHigh <= ALow then
    Exit;
  Middle := (ALow + AHigh) div 2;
  MergeSortPaths(APaths, AScratch, ALow, Middle);
  MergeSortPaths(APaths, AScratch, Middle + 1, AHigh);
  Left := ALow;
  Right := Middle + 1;
  Target := ALow;
  while (Left <= Middle) and (Right <= AHigh) do
  begin
    if CompareSandboxPaths(APaths[Right], APaths[Left]) < 0 then
    begin
      AScratch[Target] := APaths[Right];
      Inc(Right);
    end
    else
    begin
      AScratch[Target] := APaths[Left];
      Inc(Left);
    end;
    Inc(Target);
  end;
  while Left <= Middle do
  begin
    AScratch[Target] := APaths[Left];
    Inc(Left);
    Inc(Target);
  end;
  while Right <= AHigh do
  begin
    AScratch[Target] := APaths[Right];
    Inc(Right);
    Inc(Target);
  end;
  for Target := ALow to AHigh do
    APaths[Target] := AScratch[Target];
end;

procedure SortSandboxPaths(var APaths: TStringArray);
var
  Scratch: TStringArray;
begin
  Scratch := nil;
  SetLength(Scratch, Length(APaths));
  MergeSortPaths(APaths, Scratch, 0, High(APaths));
end;

{ Walking }

type
  TSandboxGlobWalkMode = (gwmGlob, gwmFiles);

  TSandboxGlobWalker = class
  private
    FFileSystem: TSandboxVirtualFileSystem;
    FPatterns: TSandboxGlobPatternArray;
    FExcludes: TSandboxGlobPatternArray;
    FProgress: TSandboxGlobProgress;
    FMode: TSandboxGlobWalkMode;
    FHasPatterns: Boolean;
    FCwdNames: TStringArray;
    FAnyAbsolute: Boolean;
    FPaths: TStringArray;
    FPathCount: Integer;
    FFiles: TSandboxSearchFileArray;
    FFileCount: Integer;
    procedure AddPath(const APath: string);
    procedure AddFile(const APath: string; const ASize: Int64);
    function IsUnderCwd(const ANames: TStringArray): Boolean;
    function OnCwdPath(const ANames: TStringArray): Boolean;
    function RelativePath(const ANames: TStringArray): string;
    procedure Visit(const ADirectory: string; const ANames: TStringArray);
  public
    constructor Create(const AFileSystem: TSandboxVirtualFileSystem;
      const ACwd: string; const AMode: TSandboxGlobWalkMode;
      const AHasPatterns: Boolean;
      const APatterns, AExcludes: TSandboxGlobPatternArray;
      const AProgress: TSandboxGlobProgress);
    procedure Run;
  end;

function SplitCanonicalPath(const APath: string): TStringArray;
var
  Start, Index, Count: Integer;
begin
  Result := nil;
  Count := 0;
  Start := 1;
  for Index := 1 to Length(APath) + 1 do
    if (Index > Length(APath)) or (APath[Index] = PATH_SEPARATOR) then
    begin
      if Index > Start then
      begin
        SetLength(Result, Count + 1);
        Result[Count] := Copy(APath, Start, Index - Start);
        Inc(Count);
      end;
      Start := Index + 1;
    end;
end;

constructor TSandboxGlobWalker.Create(
  const AFileSystem: TSandboxVirtualFileSystem; const ACwd: string;
  const AMode: TSandboxGlobWalkMode; const AHasPatterns: Boolean;
  const APatterns, AExcludes: TSandboxGlobPatternArray;
  const AProgress: TSandboxGlobProgress);
var
  Index: Integer;
begin
  inherited Create;
  FFileSystem := AFileSystem;
  FMode := AMode;
  FHasPatterns := AHasPatterns;
  FPatterns := APatterns;
  FExcludes := AExcludes;
  FProgress := AProgress;
  FCwdNames := SplitCanonicalPath(ACwd);
  FAnyAbsolute := False;
  if AMode = gwmGlob then
    for Index := 0 to High(APatterns) do
      if APatterns[Index].Absolute then
        FAnyAbsolute := True;
end;

procedure TSandboxGlobWalker.AddPath(const APath: string);
begin
  if FPathCount >= Length(FPaths) then
    SetLength(FPaths, FPathCount * 2 + 16);
  FPaths[FPathCount] := APath;
  Inc(FPathCount);
end;

procedure TSandboxGlobWalker.AddFile(const APath: string; const ASize: Int64);
begin
  if FFileCount >= Length(FFiles) then
    SetLength(FFiles, FFileCount * 2 + 16);
  FFiles[FFileCount].Path := APath;
  FFiles[FFileCount].Size := ASize;
  Inc(FFileCount);
end;

function TSandboxGlobWalker.IsUnderCwd(const ANames: TStringArray): Boolean;
var
  Index: Integer;
begin
  if Length(ANames) <= Length(FCwdNames) then
    Exit(False);
  for Index := 0 to High(FCwdNames) do
    if ANames[Index] <> FCwdNames[Index] then
      Exit(False);
  Result := True;
end;

{ The directory is the cwd or one of its ancestors. }
function TSandboxGlobWalker.OnCwdPath(const ANames: TStringArray): Boolean;
var
  Index: Integer;
begin
  if Length(ANames) > Length(FCwdNames) then
    Exit(False);
  for Index := 0 to High(ANames) do
    if ANames[Index] <> FCwdNames[Index] then
      Exit(False);
  Result := True;
end;

function TSandboxGlobWalker.RelativePath(const ANames: TStringArray): string;
var
  Index: Integer;
begin
  Result := '';
  for Index := Length(FCwdNames) to High(ANames) do
  begin
    if Index > Length(FCwdNames) then
      Result := Result + PATH_SEPARATOR;
    Result := Result + ANames[Index];
  end;
end;

procedure TSandboxGlobWalker.Visit(const ADirectory: string;
  const ANames: TStringArray);
var
  Entries: TSandboxFsStatArray;
  Index, Depth: Integer;
  Names: TStringArray;
  UnderCwd, IsDirectory, Matched: Boolean;
  PatternIndex: Integer;
begin
  Entries := FFileSystem.SnapshotList(ADirectory);
  Depth := Length(ANames);
  Names := Copy(ANames);
  SetLength(Names, Depth + 1);
  for Index := 0 to High(Entries) do
  begin
    if Assigned(FProgress) then
      FProgress;
    Names[Depth] := Entries[Index].Name;
    IsDirectory := Entries[Index].Kind = nkDirectory;
    UnderCwd := IsUnderCwd(Names);
    { An excluded directory takes everything below it along. }
    if AnyPatternMatches(FExcludes, Names, Length(FCwdNames), UnderCwd,
      IsDirectory) then
      Continue;
    if FMode = gwmGlob then
    begin
      for PatternIndex := 0 to High(FPatterns) do
        if PatternMatches(FPatterns[PatternIndex], Names, Length(FCwdNames),
          UnderCwd, IsDirectory) then
        begin
          if FPatterns[PatternIndex].Absolute then
            AddPath(Entries[Index].Path)
          else
            AddPath(RelativePath(Names));
        end;
    end
    else if UnderCwd and not IsDirectory then
    begin
      Matched := not FHasPatterns or AnyPatternMatches(FPatterns, Names,
        Length(FCwdNames), True, False);
      if Matched then
        AddFile(Entries[Index].Path, Entries[Index].Size);
    end;
    if IsDirectory and (FAnyAbsolute or UnderCwd or OnCwdPath(Names)) then
      Visit(Entries[Index].Path, Names);
  end;
end;

procedure TSandboxGlobWalker.Run;
var
  Paths: TStringArray;
  Index, Count: Integer;
begin
  FPathCount := 0;
  FFileCount := 0;
  Visit(PATH_SEPARATOR, nil);
  SetLength(FPaths, FPathCount);
  SetLength(FFiles, FFileCount);
  if FMode = gwmGlob then
  begin
    SortSandboxPaths(FPaths);
    { Deduplicate: two patterns may match one entry. }
    Paths := nil;
    SetLength(Paths, Length(FPaths));
    Count := 0;
    for Index := 0 to High(FPaths) do
      if (Count = 0) or (Paths[Count - 1] <> FPaths[Index]) then
      begin
        Paths[Count] := FPaths[Index];
        Inc(Count);
      end;
    SetLength(Paths, Count);
    FPaths := Paths;
  end;
end;

function GlobSandbox(const AFileSystem: TSandboxVirtualFileSystem;
  const APatterns, AExcludes: TSandboxGlobPatternArray; const ACwd: string;
  const AProgress: TSandboxGlobProgress): TStringArray;
var
  Walker: TSandboxGlobWalker;
begin
  Walker := TSandboxGlobWalker.Create(AFileSystem, ACwd, gwmGlob, True,
    APatterns, AExcludes, AProgress);
  try
    Walker.Run;
    Result := Walker.FPaths;
  finally
    Walker.Free;
  end;
end;

procedure MergeSortFiles(var AFiles, AScratch: TSandboxSearchFileArray;
  const ALow, AHigh: Integer);
var
  Middle, Left, Right, Target: Integer;
begin
  if AHigh <= ALow then
    Exit;
  Middle := (ALow + AHigh) div 2;
  MergeSortFiles(AFiles, AScratch, ALow, Middle);
  MergeSortFiles(AFiles, AScratch, Middle + 1, AHigh);
  Left := ALow;
  Right := Middle + 1;
  Target := ALow;
  while (Left <= Middle) and (Right <= AHigh) do
  begin
    if CompareSandboxPaths(AFiles[Right].Path, AFiles[Left].Path) < 0 then
    begin
      AScratch[Target] := AFiles[Right];
      Inc(Right);
    end
    else
    begin
      AScratch[Target] := AFiles[Left];
      Inc(Left);
    end;
    Inc(Target);
  end;
  while Left <= Middle do
  begin
    AScratch[Target] := AFiles[Left];
    Inc(Left);
    Inc(Target);
  end;
  while Right <= AHigh do
  begin
    AScratch[Target] := AFiles[Right];
    Inc(Right);
    Inc(Target);
  end;
  for Target := ALow to AHigh do
    AFiles[Target] := AScratch[Target];
end;

function SandboxSearchFiles(const AFileSystem: TSandboxVirtualFileSystem;
  const ACwd: string; const AHasGlobs: Boolean;
  const AGlobs, AExcludes: TSandboxGlobPatternArray;
  const AProgress: TSandboxGlobProgress): TSandboxSearchFileArray;
var
  Walker: TSandboxGlobWalker;
  Scratch: TSandboxSearchFileArray;
begin
  Walker := TSandboxGlobWalker.Create(AFileSystem, ACwd, gwmFiles, AHasGlobs,
    AGlobs, AExcludes, AProgress);
  try
    Walker.Run;
    Result := Walker.FFiles;
  finally
    Walker.Free;
  end;
  { Each directory lists in the filesystem's own order; sort by path. }
  Scratch := nil;
  SetLength(Scratch, Length(Result));
  MergeSortFiles(Result, Scratch, 0, High(Result));
end;

end.
