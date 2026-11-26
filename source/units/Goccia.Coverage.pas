unit Goccia.Coverage;

{$I Goccia.inc}

{ Code coverage as Istanbul defines it (istanbul-lib-instrument's visitor, istanbul-lib-coverage's FileCoverage and
  the lcovonly reporter): the engine registers each statement, branch and function of a source file here as the
  compiler reaches it (a "site"), the VM counts the site each time execution reaches it, and the reports derive
  everything else (lines, totals, the ordering of Istanbul's maps) from those counts.

  Locations are Babel's: line 1-based, column 0-based, end column exclusive. }

interface

uses
  OrderedStringMap,

  Goccia.SourceMap,
  Goccia.SourceSpan;

{ Canonical coverage path form.

  Every report key and every emitted path — lcov `SF:`, the JSON object
  keys and their `"path"` field — uses one canonical spelling, so a file
  that is both the entry point and an imported module collapses into a
  single record instead of one record per spelling:

    * repo-relative when the file lives under a repository root (the
      nearest ancestor directory holding a `.git` entry, which is a
      directory in a normal clone and a file in a linked worktree or
      submodule), otherwise the absolute path;
    * directory separators are always '/', on every platform.

  Repo-relative is the form coverage consumers expect: Codecov matches
  report paths against repository paths and needs `fixes` mappings to
  cope with the absolute build-machine paths a CI run would otherwise
  emit, and genhtml resolves relative paths against its working
  directory.  Files outside any repository have no relative form to fall
  back to, so they stay absolute.

  Paths with no on-disk backing are virtual identities rather than
  files — `<stdin>`, and multifile section names such as `<stdin>[part1]`
  or `/abs/foo[part2].jsx`.  They keep their spelling verbatim and only
  have separators normalized. }
procedure ResolveCoveragePath(const APath: string;
  out ACanonical, AResolved: string);

{ The canonical half of ResolveCoveragePath. }
function CanonicalCoveragePath(const APath: string): string;

{ Rewrite directory separators to '/'.  On Windows both separators are
  accepted by the OS, so a backslash is always a separator and rewriting
  it is safe.  On POSIX the separator is already '/' and a backslash is a
  legal filename character, so the "every separator is '/'" guarantee
  holds without touching the string. }
function NormalizeCoveragePathSeparators(const APath: string): string;

{ Nearest ancestor of AStartDirectory holding a `.git` entry, or '' when
  there is none. }
function FindRepositoryRoot(const AStartDirectory: string): string;

type
  { A source range in Babel's terms: line 1-based, column 0-based (in UTF-16 code units), end column exclusive.
    StartLine 0: Istanbul has no location here (the empty alternate of an `if` without `else`). }
  TGocciaCoverageLoc = record
    StartLine: Integer;
    StartColumn: Integer;
    EndLine: Integer;
    EndColumn: Integer;
  end;

  TGocciaCoverageLocArray = array of TGocciaCoverageLoc;

  { One entry of Istanbul's statementMap and s. }
  TGocciaCoverageStatement = record
    Loc: TGocciaCoverageLoc;
    { where Istanbul numbers the statement, when that is not at its start: the visitor registers a declarator's
      initializer and a class property's value when it enters the declarator or the property, before the pattern
      or computed key that comes first in the source (OrderLine 0: at the start) }
    OrderLine: Integer;
    OrderColumn: Integer;
    HitCount: Int64;
  end;

  { One entry of Istanbul's branchMap and b. BranchType is Istanbul's: 'if', 'cond-expr', 'binary-expr', 'switch' or
    'default-arg'. }
  TGocciaCoverageBranch = record
    BranchType: string;
    Loc: TGocciaCoverageLoc;
    Locations: TGocciaCoverageLocArray;
    HitCounts: array of Int64;
  end;

  { One entry of Istanbul's fnMap and f. Name is '' for an anonymous function: the reports name it
    (anonymous_<index>) by its index in Istanbul's order. }
  TGocciaCoverageFunction = record
    Name: string;
    Decl: TGocciaCoverageLoc;
    Loc: TGocciaCoverageLoc;
    HitCount: Int64;
  end;

  TGocciaCoverageIndexArray = array of Integer;

  { A line of the lcov DA: records: a line some statement starts on, and the most hits among those statements. }
  TGocciaCoverageLine = record
    Line: Integer;
    HitCount: Int64;
  end;

  TGocciaCoverageLineArray = array of TGocciaCoverageLine;

  TGocciaFileCoverage = class
  private
    FFileName: string;
    FStatements: array of TGocciaCoverageStatement;
    FStatementCount: Integer;
    FStatementIndex: TOrderedStringMap<Integer>;
    FBranches: array of TGocciaCoverageBranch;
    FBranchCount: Integer;
    FBranchIndex: TOrderedStringMap<Integer>;
    FFunctions: array of TGocciaCoverageFunction;
    FFunctionCount: Integer;
    FFunctionIndex: TOrderedStringMap<Integer>;
    function GetStatement(const AIndex: Integer): TGocciaCoverageStatement;
    function GetBranch(const AIndex: Integer): TGocciaCoverageBranch;
    function GetFunction(const AIndex: Integer): TGocciaCoverageFunction;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    { Register a site, or find the one at the same location: compiling a site again (a `finally` block's code, which
      runs through two paths) returns the same index. The counts start at 0. }
    function RegisterStatement(const ALoc: TGocciaCoverageLoc;
      const AOrderLine: Integer = 0; const AOrderColumn: Integer = 0): Integer;
    function RegisterBranch(const ABranchType: string;
      const ALoc: TGocciaCoverageLoc;
      const ALocations: TGocciaCoverageLocArray): Integer;
    function RegisterFunction(const AName: string;
      const ADecl, ALoc: TGocciaCoverageLoc): Integer;

    procedure RecordStatementHit(const AIndex: Integer); {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordBranchHit(const ABranch, APath: Integer); {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordFunctionHit(const AIndex: Integer); {$IFDEF FPC}inline;{$ENDIF}

    { The sites in Istanbul's order, which is the order its visitor reaches them in: the source order of their
      starts, an enclosing site before the ones inside it. A site's key in Istanbul's maps ("0", "1", ...) is its
      position in this order. Each result lists indexes into the file's registration order. }
    function StatementOrder: TGocciaCoverageIndexArray;
    function BranchOrder: TGocciaCoverageIndexArray;
    function FunctionOrder: TGocciaCoverageIndexArray;

    { FileCoverage.getLineCoverage: for each line a statement starts on, the most hits among the statements that
      start on it, in line order. ASourceMap, when given, translates each statement's start first. }
    function LineCoverage(const ASourceMap: TGocciaSourceMap = nil): TGocciaCoverageLineArray;

    function StatementsFound: Integer;
    function StatementsHit: Integer;
    function LinesFound: Integer;
    function LinesHit: Integer;
    { One path of a branch is one thing found: a branch with n paths adds n. }
    function BranchesFound: Integer;
    function BranchesHit: Integer;
    function FunctionsFound: Integer;
    function FunctionsHit: Integer;

    property FileName: string read FFileName;
    property StatementCount: Integer read FStatementCount;
    property BranchCount: Integer read FBranchCount;
    property FunctionCount: Integer read FFunctionCount;
    property Statements[const AIndex: Integer]: TGocciaCoverageStatement read GetStatement;
    property Branches[const AIndex: Integer]: TGocciaCoverageBranch read GetBranch;
    property Functions[const AIndex: Integer]: TGocciaCoverageFunction read GetFunction;
  end;

  TGocciaCoverageFileMap = TOrderedStringMap<TGocciaFileCoverage>;

  TGocciaSourceMapMap = TOrderedStringMap<TGocciaSourceMap>;

  { Specialized here rather than reusing OrderedStringMap's TStringStringMap
    so the protected sequential-iteration API is reachable from this unit,
    exactly as it is for the two maps above. }
  TGocciaCoveragePathMap = TOrderedStringMap<string>;

  TGocciaCoverageTracker = class
  private
    FFiles: TGocciaCoverageFileMap;
    FSourceMaps: TGocciaSourceMapMap;
    { Raw spelling as it reaches the tracker -> canonical report key.
      Canonicalization stats the filesystem and walks up to the repository
      root, so it runs once per distinct spelling and is memoized here. }
    FPathCache: TGocciaCoveragePathMap;
    { Canonical report key -> native on-disk path it was derived from. }
    FResolvedPaths: TGocciaCoveragePathMap;
    FEnabled: Boolean;
    FSerial: Integer;
    { Single-entry memo over the canonicalize-then-look-up pair, which
      together dominate the per-hit cost of a coverage run. }
    FLastLookupPath: string;
    FLastLookupFile: TGocciaFileCoverage;
    { Raw spellings of files that are engine code, not user source (the
      bootstrap shims), and the record their hits go to instead, which is in
      no report. }
    FExcludedFiles: TGocciaCoveragePathMap;
    FDiscardedFile: TGocciaFileCoverage;
    { AFilePath must already be canonical. }
    function GetOrCreateFile(const AFilePath: string): TGocciaFileCoverage;
    function CanonicalizePath(const AFilePath: string): string;
  public
    class function Instance: TGocciaCoverageTracker;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    { Make sure the file has a record, so a report lists it even when it has no statements. }
    procedure RegisterSourceFile(const AFilePath: string);
    { Leave the file named AFilePath, exactly as the VM reports it, out of
      every report: its hits are recorded nowhere. For engine code that runs
      as bytecode, such as a bootstrap shim. }
    procedure ExcludeFile(const AFilePath: string);
    procedure RegisterSourceMap(const AFilePath: string;
      const ASourceMap: TGocciaSourceMap);

    { Canonicalize AFilePath, then resolve it to its coverage record, creating it. The record is the handle the
      hot path keeps: the VM looks a source up once and counts through the record. }
    function FileFor(const AFilePath: string): TGocciaFileCoverage;

    function RegisterStatement(const AFilePath: string;
      const ALoc: TGocciaCoverageLoc; const AOrderLine: Integer = 0;
      const AOrderColumn: Integer = 0): Integer;
    function RegisterBranch(const AFilePath, ABranchType: string;
      const ALoc: TGocciaCoverageLoc;
      const ALocations: TGocciaCoverageLocArray): Integer;
    function RegisterFunction(const AFilePath, AName: string;
      const ADecl, ALoc: TGocciaCoverageLoc): Integer;
    procedure RecordStatementHit(const AFilePath: string; const AIndex: Integer);
    procedure RecordBranchHit(const AFilePath: string;
      const ABranch, APath: Integer);
    procedure RecordFunctionHit(const AFilePath: string; const AIndex: Integer);

    function GetFileCoverage(const AFilePath: string): TGocciaFileCoverage;
    function GetSourceMap(const AFilePath: string): TGocciaSourceMap;

    { Native on-disk path behind a canonical report key.  Reporters must
      read source text through this and never through the key itself: a
      canonical key is repo-relative and so need not resolve against the
      process's working directory.  Falls back to ACanonicalPath when the
      key was never registered (virtual identities, or a record created by
      MergeFrom from a tracker that had no resolved path either). }
    function ResolvedSourcePath(const ACanonicalPath: string): string;

    { Merge all coverage data from ASource into this tracker.
      Sites are matched by their location (the registration order differs
      between threads) and their hit *counts* are added together, so merging
      N workers yields the same totals a single-threaded run would produce. }
    procedure MergeFrom(const ASource: TGocciaCoverageTracker);

    property Files: TGocciaCoverageFileMap read FFiles;
    property SourceMaps: TGocciaSourceMapMap read FSourceMaps;
    property Enabled: Boolean read FEnabled write FEnabled;
    { unique among the trackers of the process: a cache of a tracker's records tells it from a later one at its address }
    property Serial: Integer read FSerial;
  end;

{ A site's location as a string key, as the registration and merge match locations by. }
function CoverageLocKey(const ALoc: TGocciaCoverageLoc): string;

{ The Babel location of a span, or of ASpan's source between two other offsets (a part of the node the AST has no
  node for): StartLine 0 for a span with no coordinates (a node the parser did not read from source). Columns are
  counted in UTF-16 code units, as Babel counts them. }
function CoverageLocOfSpan(const ASpan: TGocciaSourceSpan): TGocciaCoverageLoc;
function CoverageLocOfOffsets(const ASpan: TGocciaSourceSpan;
  const AStartOffset, AEndOffset: Integer): TGocciaCoverageLoc;

{ The offset where the label ahead of the statement at AOffset starts (`outer:` in `outer: for (...)`), or -1 when
  no label precedes it. The AST keeps a statement's labels as names, not positions. }
function CoverageLabelStart(const ASpan: TGocciaSourceSpan; const AOffset: Integer): Integer;

{ The offset of the `get` or `set` keyword that ends just before AOffset (an accessor's span starts at its name), or
  AOffset when no such keyword precedes it. }
function CoverageAccessorStart(const ASpan: TGocciaSourceSpan; const AOffset: Integer;
  const AKeyword: string): Integer;

{ The offset where the identifier of the function whose node starts at AStartOffset begins (after `export`,
  `default`, `async`, `function` and `*`), or -1 when the name is not there. }
function CoverageFunctionNameStart(const ASpan: TGocciaSourceSpan;
  const AStartOffset: Integer; const AName: string): Integer;

implementation

uses
  SysUtils;


{ Coverage Path Canonicalization }

function NormalizeCoveragePathSeparators(const APath: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := StringReplace(APath, '\', '/', [rfReplaceAll]);
  {$ELSE}
  Result := APath;
  {$ENDIF}
end;

function FindRepositoryRoot(const AStartDirectory: string): string;
var
  Dir, Parent, Marker: string;
begin
  Result := '';
  Dir := ExcludeTrailingPathDelimiter(AStartDirectory);
  while Dir <> '' do
  begin
    Marker := IncludeTrailingPathDelimiter(Dir) + '.git';
    if DirectoryExists(Marker) or FileExists(Marker) then
      Exit(Dir);
    Parent := ExcludeTrailingPathDelimiter(ExtractFileDir(Dir));
    { ExtractFileDir is a fixed point at the filesystem root ('C:' on
      Windows) and returns '' once it runs past a POSIX root. }
    if (Parent = '') or (Parent = Dir) then
      Break;
    Dir := Parent;
  end;
end;

function SamePathPrefix(const A, B: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := CompareText(A, B) = 0;
  {$ELSE}
  Result := A = B;
  {$ENDIF}
end;

procedure ResolveCoveragePath(const APath: string;
  out ACanonical, AResolved: string);
var
  Absolute, Root, RootPrefix: string;
begin
  if APath = '' then
  begin
    ACanonical := '';
    AResolved := '';
    Exit;
  end;

  if not FileExists(APath) then
  begin
    { Virtual identity — nothing on disk to canonicalize against, and
      expanding it would invent a bogus absolute path. }
    AResolved := APath;
    ACanonical := NormalizeCoveragePathSeparators(APath);
    Exit;
  end;

  Absolute := ExpandFileName(APath);
  AResolved := Absolute;

  Root := FindRepositoryRoot(ExtractFileDir(Absolute));
  if Root <> '' then
  begin
    RootPrefix := IncludeTrailingPathDelimiter(Root);
    if (Length(Absolute) > Length(RootPrefix)) and
       SamePathPrefix(Copy(Absolute, 1, Length(RootPrefix)), RootPrefix) then
    begin
      ACanonical := NormalizeCoveragePathSeparators(
        Copy(Absolute, Length(RootPrefix) + 1, MaxInt));
      Exit;
    end;
  end;

  ACanonical := NormalizeCoveragePathSeparators(Absolute);
end;

function CanonicalCoveragePath(const APath: string): string;
var
  Resolved: string;
begin
  ResolveCoveragePath(APath, Result, Resolved);
end;

{ Locations }

{ A span's source is UTF-16 text and its offsets index it, so a column is already counted in UTF-16 code units,
  as Babel counts them. }
procedure SourcePosition(const ASpan: TGocciaSourceSpan; const AOffset: Integer;
  out ALine, AColumn: Integer);
var
  FirstColumn: Integer;
begin
  ASpan.Coordinates.PositionAtOffset(AOffset, ALine, FirstColumn);
  AColumn := FirstColumn - 1;
end;

function CoverageLocOfOffsets(const ASpan: TGocciaSourceSpan;
  const AStartOffset, AEndOffset: Integer): TGocciaCoverageLoc;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ASpan.Coordinates = nil then Exit;
  SourcePosition(ASpan, AStartOffset, Result.StartLine, Result.StartColumn);
  SourcePosition(ASpan, AEndOffset, Result.EndLine, Result.EndColumn);
end;

function CoverageLocOfSpan(const ASpan: TGocciaSourceSpan): TGocciaCoverageLoc;
begin
  Result := CoverageLocOfOffsets(ASpan, ASpan.StartOffset, ASpan.EndOffset);
end;

function IsIdentifierChar(const AChar: Char): Boolean;
begin
  Result := ((AChar >= '0') and (AChar <= '9')) or ((AChar >= 'A') and (AChar <= 'Z')) or
    ((AChar >= 'a') and (AChar <= 'z')) or (AChar = '_') or (AChar = '$') or (Ord(AChar) >= $80);
end;

function IsSpaceChar(const AChar: Char): Boolean;
begin
  Result := (AChar = ' ') or (AChar = #9) or (AChar = #10) or (AChar = #13);
end;

{ the offset (0-based) after the whitespace and comments that start at AOffset }
function SkipTrivia(const ASource: string; const AOffset: Integer): Integer;
var
  Length_: Integer;
begin
  Result := AOffset;
  Length_ := Length(ASource);
  while Result < Length_ do
  begin
    case ASource[Result + 1] of
      ' ', #9, #10, #13: Inc(Result);
      '/':
        if (Result + 1 < Length_) and (ASource[Result + 2] = '/') then
        begin
          while (Result < Length_) and not ((ASource[Result + 1] = #10) or (ASource[Result + 1] = #13)) do Inc(Result);
        end
        else if (Result + 1 < Length_) and (ASource[Result + 2] = '*') then
        begin
          Inc(Result, 2);
          while (Result + 1 < Length_) and not ((ASource[Result + 1] = '*') and (ASource[Result + 2] = '/')) do
            Inc(Result);
          Inc(Result, 2);
        end
        else Exit;
    else
      Exit;
    end;
  end;
end;

function CoverageLabelStart(const ASpan: TGocciaSourceSpan; const AOffset: Integer): Integer;
var
  Source: string;
  I, Stop: Integer;
begin
  Result := -1;
  if ASpan.Coordinates = nil then Exit;
  Source := ASpan.Source;
  I := AOffset;
  if I > Length(Source) then Exit;
  { back over whitespace to the colon }
  while (I > 0) and IsSpaceChar(Source[I]) do Dec(I);
  if (I <= 0) or (Source[I] <> ':') then Exit;
  Dec(I);
  while (I > 0) and IsSpaceChar(Source[I]) do Dec(I);
  Stop := I;
  while (I > 0) and IsIdentifierChar(Source[I]) do Dec(I);
  if I = Stop then Exit;
  Result := I;
end;

function CoverageAccessorStart(const ASpan: TGocciaSourceSpan; const AOffset: Integer;
  const AKeyword: string): Integer;
var
  Source: string;
  I, Depth: Integer;
begin
  Result := AOffset;
  if ASpan.Coordinates = nil then Exit;
  Source := ASpan.Source;
  I := AOffset;
  if I > Length(Source) then Exit;
  { a computed key's span starts at its closing bracket }
  if (I < Length(Source)) and (Source[I + 1] = ']') then Inc(I);
  while (I > 0) and IsSpaceChar(Source[I]) do Dec(I);
  { a computed key: back over its brackets }
  if (I > 0) and (Source[I] = ']') then
  begin
    Depth := 0;
    while I > 0 do
    begin
      if Source[I] = ']' then Inc(Depth)
      else if Source[I] = '[' then Dec(Depth);
      Dec(I);
      if Depth = 0 then Break;
    end;
    while (I > 0) and IsSpaceChar(Source[I]) do Dec(I);
  end;
  if I < Length(AKeyword) then Exit;
  if Copy(Source, I - Length(AKeyword) + 1, Length(AKeyword)) <> AKeyword then Exit;
  if (I - Length(AKeyword) > 0) and IsIdentifierChar(Source[I - Length(AKeyword)]) then Exit;
  Result := I - Length(AKeyword);
end;

function CoverageFunctionNameStart(const ASpan: TGocciaSourceSpan;
  const AStartOffset: Integer; const AName: string): Integer;
var
  Source: string;
  Offset, Word_, Limit: Integer;
  Text: string;
begin
  Result := -1;
  if (ASpan.Coordinates = nil) or (AName = '') then Exit;
  Source := ASpan.Source;
  Offset := AStartOffset;
  Limit := 0;
  while Limit < 8 do
  begin
    Inc(Limit);
    Offset := SkipTrivia(Source, Offset);
    if Offset >= Length(Source) then Exit;
    if Source[Offset + 1] = '*' then
    begin
      Inc(Offset);
      Continue;
    end;
    if Copy(Source, Offset + 1, Length(AName)) = AName then
    begin
      Word_ := Offset + Length(AName);
      if (Word_ >= Length(Source)) or not IsIdentifierChar(Source[Word_ + 1]) then
        Exit(Offset);
    end;
    { a keyword before the name: export, default, async, function }
    Word_ := Offset;
    while (Word_ < Length(Source)) and IsIdentifierChar(Source[Word_ + 1]) do Inc(Word_);
    if Word_ = Offset then Exit;
    Text := Copy(Source, Offset + 1, Word_ - Offset);
    if (Text <> 'export') and (Text <> 'default') and (Text <> 'async') and (Text <> 'function') then Exit;
    Offset := Word_;
  end;
end;


function CoverageLocKey(const ALoc: TGocciaCoverageLoc): string;
begin
  Result := IntToStr(ALoc.StartLine) + ':' + IntToStr(ALoc.StartColumn) + '-' +
    IntToStr(ALoc.EndLine) + ':' + IntToStr(ALoc.EndColumn);
end;

{ Orders sites the way Istanbul's visitor reaches them: by where they start, then an enclosing site (the one that
  ends later) before the one inside it, then by registration. }
type
  TSiteKey = record
    StartLine, StartColumn, EndLine, EndColumn, Index: Integer;
  end;

  TSiteKeyArray = array of TSiteKey;

function SiteKeyLess(const A, B: TSiteKey): Boolean;
begin
  if A.StartLine <> B.StartLine then Exit(A.StartLine < B.StartLine);
  if A.StartColumn <> B.StartColumn then Exit(A.StartColumn < B.StartColumn);
  if A.EndLine <> B.EndLine then Exit(A.EndLine > B.EndLine);
  if A.EndColumn <> B.EndColumn then Exit(A.EndColumn > B.EndColumn);
  Result := A.Index < B.Index;
end;

procedure SortSiteKeys(var AKeys: TSiteKeyArray);
var
  Buffer: TSiteKeyArray;

  procedure MergeSort(ALow, AHigh: Integer);
  var
    Middle, I, J, K: Integer;
  begin
    if AHigh - ALow < 1 then Exit;
    Middle := (ALow + AHigh) div 2;
    MergeSort(ALow, Middle);
    MergeSort(Middle + 1, AHigh);
    I := ALow;
    J := Middle + 1;
    K := ALow;
    while (I <= Middle) and (J <= AHigh) do
    begin
      if SiteKeyLess(AKeys[J], AKeys[I]) then
      begin
        Buffer[K] := AKeys[J];
        Inc(J);
      end
      else
      begin
        Buffer[K] := AKeys[I];
        Inc(I);
      end;
      Inc(K);
    end;
    while I <= Middle do
    begin
      Buffer[K] := AKeys[I];
      Inc(I);
      Inc(K);
    end;
    while J <= AHigh do
    begin
      Buffer[K] := AKeys[J];
      Inc(J);
      Inc(K);
    end;
    for K := ALow to AHigh do
      AKeys[K] := Buffer[K];
  end;

begin
  SetLength(Buffer, Length(AKeys));
  MergeSort(0, High(AKeys));
end;

function OrderOf(const AKeys: TSiteKeyArray): TGocciaCoverageIndexArray;
var
  Sorted: TSiteKeyArray;
  I: Integer;
begin
  Sorted := Copy(AKeys, 0, Length(AKeys));
  SortSiteKeys(Sorted);
  SetLength(Result, Length(Sorted));
  for I := 0 to High(Sorted) do
    Result[I] := Sorted[I].Index;
end;

function SiteKey(const ALoc: TGocciaCoverageLoc; const AIndex: Integer): TSiteKey;
begin
  Result.StartLine := ALoc.StartLine;
  Result.StartColumn := ALoc.StartColumn;
  Result.EndLine := ALoc.EndLine;
  Result.EndColumn := ALoc.EndColumn;
  Result.Index := AIndex;
end;

function BranchKey(const ABranchType: string; const ALoc: TGocciaCoverageLoc;
  const ALocations: TGocciaCoverageLocArray): string;
var
  I: Integer;
begin
  Result := ABranchType + '@' + CoverageLocKey(ALoc);
  for I := 0 to High(ALocations) do
    Result := Result + '|' + CoverageLocKey(ALocations[I]);
end;

function FunctionKey(const ADecl, ALoc: TGocciaCoverageLoc): string;
begin
  Result := CoverageLocKey(ADecl) + '/' + CoverageLocKey(ALoc);
end;

{ TGocciaFileCoverage }

constructor TGocciaFileCoverage.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FStatementIndex := TOrderedStringMap<Integer>.Create;
  FBranchIndex := TOrderedStringMap<Integer>.Create;
  FFunctionIndex := TOrderedStringMap<Integer>.Create;
end;

destructor TGocciaFileCoverage.Destroy;
begin
  FFunctionIndex.Free;
  FBranchIndex.Free;
  FStatementIndex.Free;
  inherited;
end;

function TGocciaFileCoverage.GetStatement(const AIndex: Integer): TGocciaCoverageStatement;
begin
  Result := FStatements[AIndex];
end;

function TGocciaFileCoverage.GetBranch(const AIndex: Integer): TGocciaCoverageBranch;
begin
  Result := FBranches[AIndex];
end;

function TGocciaFileCoverage.GetFunction(const AIndex: Integer): TGocciaCoverageFunction;
begin
  Result := FFunctions[AIndex];
end;

function TGocciaFileCoverage.RegisterStatement(const ALoc: TGocciaCoverageLoc;
  const AOrderLine: Integer; const AOrderColumn: Integer): Integer;
var
  Key: string;
begin
  Key := CoverageLocKey(ALoc);
  if FStatementIndex.TryGetValue(Key, Result) then
    Exit;
  if FStatementCount = Length(FStatements) then
    SetLength(FStatements, FStatementCount * 2 + 16);
  FStatements[FStatementCount].Loc := ALoc;
  FStatements[FStatementCount].OrderLine := AOrderLine;
  FStatements[FStatementCount].OrderColumn := AOrderColumn;
  FStatements[FStatementCount].HitCount := 0;
  Result := FStatementCount;
  FStatementIndex.Add(Key, Result);
  Inc(FStatementCount);
end;

function TGocciaFileCoverage.RegisterBranch(const ABranchType: string;
  const ALoc: TGocciaCoverageLoc; const ALocations: TGocciaCoverageLocArray): Integer;
var
  Key: string;
  I: Integer;
begin
  Key := BranchKey(ABranchType, ALoc, ALocations);
  if FBranchIndex.TryGetValue(Key, Result) then
    Exit;
  if FBranchCount = Length(FBranches) then
    SetLength(FBranches, FBranchCount * 2 + 16);
  FBranches[FBranchCount].BranchType := ABranchType;
  FBranches[FBranchCount].Loc := ALoc;
  FBranches[FBranchCount].Locations := Copy(ALocations, 0, Length(ALocations));
  SetLength(FBranches[FBranchCount].HitCounts, Length(ALocations));
  for I := 0 to High(ALocations) do
    FBranches[FBranchCount].HitCounts[I] := 0;
  Result := FBranchCount;
  FBranchIndex.Add(Key, Result);
  Inc(FBranchCount);
end;

function TGocciaFileCoverage.RegisterFunction(const AName: string;
  const ADecl, ALoc: TGocciaCoverageLoc): Integer;
var
  Key: string;
begin
  Key := FunctionKey(ADecl, ALoc);
  if FFunctionIndex.TryGetValue(Key, Result) then
    Exit;
  if FFunctionCount = Length(FFunctions) then
    SetLength(FFunctions, FFunctionCount * 2 + 16);
  FFunctions[FFunctionCount].Name := AName;
  FFunctions[FFunctionCount].Decl := ADecl;
  FFunctions[FFunctionCount].Loc := ALoc;
  FFunctions[FFunctionCount].HitCount := 0;
  Result := FFunctionCount;
  FFunctionIndex.Add(Key, Result);
  Inc(FFunctionCount);
end;

procedure TGocciaFileCoverage.RecordStatementHit(const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FStatementCount) then
    Inc(FStatements[AIndex].HitCount);
end;

procedure TGocciaFileCoverage.RecordBranchHit(const ABranch, APath: Integer);
begin
  if (ABranch >= 0) and (ABranch < FBranchCount) and (APath >= 0) and
     (APath < Length(FBranches[ABranch].HitCounts)) then
    Inc(FBranches[ABranch].HitCounts[APath]);
end;

procedure TGocciaFileCoverage.RecordFunctionHit(const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FFunctionCount) then
    Inc(FFunctions[AIndex].HitCount);
end;

function TGocciaFileCoverage.StatementOrder: TGocciaCoverageIndexArray;
var
  Keys: TSiteKeyArray;
  I: Integer;
begin
  SetLength(Keys, FStatementCount);
  for I := 0 to FStatementCount - 1 do
  begin
    Keys[I] := SiteKey(FStatements[I].Loc, I);
    if FStatements[I].OrderLine > 0 then
    begin
      Keys[I].StartLine := FStatements[I].OrderLine;
      Keys[I].StartColumn := FStatements[I].OrderColumn;
    end;
  end;
  Result := OrderOf(Keys);
end;

function TGocciaFileCoverage.BranchOrder: TGocciaCoverageIndexArray;
var
  Keys: TSiteKeyArray;
  I: Integer;
begin
  SetLength(Keys, FBranchCount);
  for I := 0 to FBranchCount - 1 do
    Keys[I] := SiteKey(FBranches[I].Loc, I);
  Result := OrderOf(Keys);
end;

function TGocciaFileCoverage.FunctionOrder: TGocciaCoverageIndexArray;
var
  Keys: TSiteKeyArray;
  I: Integer;
begin
  { a function is ordered by its declaration: its identifier's start, else its node's }
  SetLength(Keys, FFunctionCount);
  for I := 0 to FFunctionCount - 1 do
    Keys[I] := SiteKey(FFunctions[I].Decl, I);
  Result := OrderOf(Keys);
end;

function TGocciaFileCoverage.LineCoverage(const ASourceMap: TGocciaSourceMap): TGocciaCoverageLineArray;
var
  HitCounts: array of Int64;
  Sorted: TSiteKeyArray;
  Count, I, J, Line, OrigLine, OrigColumn, Last: Integer;
begin
  { each statement at its start line (through the source map, when there is one) }
  SetLength(Sorted, FStatementCount);
  SetLength(HitCounts, FStatementCount);
  Count := 0;
  for I := 0 to FStatementCount - 1 do
  begin
    Line := FStatements[I].Loc.StartLine;
    if Line <= 0 then Continue;
    if Assigned(ASourceMap) and
       ASourceMap.Translate(Line, FStatements[I].Loc.StartColumn + 1, OrigLine, OrigColumn) then
      Line := OrigLine;
    Sorted[Count].StartLine := Line;
    Sorted[Count].StartColumn := 0;
    Sorted[Count].EndLine := 0;
    Sorted[Count].EndColumn := 0;
    Sorted[Count].Index := Count;
    HitCounts[Count] := FStatements[I].HitCount;
    Inc(Count);
  end;
  SetLength(Sorted, Count);
  SortSiteKeys(Sorted);
  { one entry per distinct line: the greatest count among the statements starting on it }
  SetLength(Result, Count);
  J := 0;
  Last := -1;
  for I := 0 to Count - 1 do
    if Sorted[I].StartLine = Last then
    begin
      if HitCounts[Sorted[I].Index] > Result[J - 1].HitCount then
        Result[J - 1].HitCount := HitCounts[Sorted[I].Index];
    end
    else
    begin
      Result[J].Line := Sorted[I].StartLine;
      Result[J].HitCount := HitCounts[Sorted[I].Index];
      Last := Sorted[I].StartLine;
      Inc(J);
    end;
  SetLength(Result, J);
end;

function TGocciaFileCoverage.StatementsFound: Integer;
begin
  Result := FStatementCount;
end;

function TGocciaFileCoverage.StatementsHit: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FStatementCount - 1 do
    if FStatements[I].HitCount > 0 then
      Inc(Result);
end;

function TGocciaFileCoverage.LinesFound: Integer;
begin
  Result := Length(LineCoverage);
end;

function TGocciaFileCoverage.LinesHit: Integer;
var
  Lines: TGocciaCoverageLineArray;
  I: Integer;
begin
  Result := 0;
  Lines := LineCoverage;
  for I := 0 to High(Lines) do
    if Lines[I].HitCount > 0 then
      Inc(Result);
end;

function TGocciaFileCoverage.BranchesFound: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBranchCount - 1 do
    Inc(Result, Length(FBranches[I].HitCounts));
end;

function TGocciaFileCoverage.BranchesHit: Integer;
var
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to FBranchCount - 1 do
    for J := 0 to High(FBranches[I].HitCounts) do
      if FBranches[I].HitCounts[J] > 0 then
        Inc(Result);
end;

function TGocciaFileCoverage.FunctionsFound: Integer;
begin
  Result := FFunctionCount;
end;

function TGocciaFileCoverage.FunctionsHit: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FFunctionCount - 1 do
    if FFunctions[I].HitCount > 0 then
      Inc(Result);
end;

{ TGocciaCoverageTracker }

var
  TrackerSerialCounter: Integer;

threadvar
  CoverageTrackerThreadInstance: TGocciaCoverageTracker;

class function TGocciaCoverageTracker.Instance: TGocciaCoverageTracker;
begin
  Result := CoverageTrackerThreadInstance;
end;

class procedure TGocciaCoverageTracker.Initialize;
begin
  if not Assigned(CoverageTrackerThreadInstance) then
    CoverageTrackerThreadInstance := TGocciaCoverageTracker.Create;
end;

class procedure TGocciaCoverageTracker.Shutdown;
begin
  FreeAndNil(CoverageTrackerThreadInstance);
end;

constructor TGocciaCoverageTracker.Create;
begin
  inherited Create;
  FFiles := TGocciaCoverageFileMap.Create;
  FSourceMaps := TGocciaSourceMapMap.Create;
  FPathCache := TGocciaCoveragePathMap.Create;
  FResolvedPaths := TGocciaCoveragePathMap.Create;
  FExcludedFiles := TGocciaCoveragePathMap.Create;
  FEnabled := False;
  FLastLookupFile := nil;
  FSerial := InterlockedIncrement(TrackerSerialCounter);
end;

destructor TGocciaCoverageTracker.Destroy;
var
  IterState: Integer;
  Key: string;
  FileCov: TGocciaFileCoverage;
  SrcMap: TGocciaSourceMap;
begin
  if Assigned(FSourceMaps) then
  begin
    IterState := 0;
    while FSourceMaps.GetNextEntry(IterState, Key, SrcMap) do
      SrcMap.Free;
    FSourceMaps.Free;
  end;
  if Assigned(FFiles) then
  begin
    IterState := 0;
    while FFiles.GetNextEntry(IterState, Key, FileCov) do
      FileCov.Free;
    FFiles.Free;
  end;
  FResolvedPaths.Free;
  FPathCache.Free;
  FExcludedFiles.Free;
  FDiscardedFile.Free;
  inherited;
end;

function TGocciaCoverageTracker.CanonicalizePath(
  const AFilePath: string): string;
var
  Resolved: string;
begin
  if FPathCache.TryGetValue(AFilePath, Result) then
    Exit;
  ResolveCoveragePath(AFilePath, Result, Resolved);
  FPathCache.Add(AFilePath, Result);
  if not FResolvedPaths.ContainsKey(Result) then
    FResolvedPaths.Add(Result, Resolved);
end;

function TGocciaCoverageTracker.GetOrCreateFile(
  const AFilePath: string): TGocciaFileCoverage;
begin
  if not FFiles.TryGetValue(AFilePath, Result) then
  begin
    Result := TGocciaFileCoverage.Create(AFilePath);
    FFiles.Add(AFilePath, Result);
  end;
end;

function TGocciaCoverageTracker.FileFor(
  const AFilePath: string): TGocciaFileCoverage;
begin
  if Assigned(FLastLookupFile) and (AFilePath = FLastLookupPath) then
    Exit(FLastLookupFile);
  if (FExcludedFiles.Count > 0) and FExcludedFiles.ContainsKey(AFilePath) then
  begin
    if not Assigned(FDiscardedFile) then
      FDiscardedFile := TGocciaFileCoverage.Create(AFilePath);
    Result := FDiscardedFile;
  end
  else
    Result := GetOrCreateFile(CanonicalizePath(AFilePath));
  FLastLookupPath := AFilePath;
  FLastLookupFile := Result;
end;

procedure TGocciaCoverageTracker.ExcludeFile(const AFilePath: string);
begin
  if FExcludedFiles.ContainsKey(AFilePath) then
    Exit;
  FExcludedFiles.Add(AFilePath, AFilePath);
  if AFilePath = FLastLookupPath then
    FLastLookupFile := nil;
end;

function TGocciaCoverageTracker.ResolvedSourcePath(
  const ACanonicalPath: string): string;
begin
  if not FResolvedPaths.TryGetValue(ACanonicalPath, Result) then
    Result := ACanonicalPath;
end;


procedure TGocciaCoverageTracker.RegisterSourceFile(const AFilePath: string);
begin
  { a file registered once as an import and again as the entry point is one record, not two }
  FileFor(AFilePath);
end;

procedure TGocciaCoverageTracker.RegisterSourceMap(const AFilePath: string;
  const ASourceMap: TGocciaSourceMap);
var
  OldMap: TGocciaSourceMap;
  Key: string;
begin
  Key := CanonicalizePath(AFilePath);
  if FSourceMaps.TryGetValue(Key, OldMap) then
  begin
    OldMap.Free;
    FSourceMaps.Remove(Key);
  end;
  FSourceMaps.Add(Key, ASourceMap);
end;

function TGocciaCoverageTracker.GetSourceMap(
  const AFilePath: string): TGocciaSourceMap;
begin
  if not FSourceMaps.TryGetValue(AFilePath, Result) then
    if not FSourceMaps.TryGetValue(CanonicalizePath(AFilePath), Result) then
      Result := nil;
end;

function TGocciaCoverageTracker.RegisterStatement(const AFilePath: string;
  const ALoc: TGocciaCoverageLoc; const AOrderLine: Integer;
  const AOrderColumn: Integer): Integer;
begin
  Result := FileFor(AFilePath).RegisterStatement(ALoc, AOrderLine, AOrderColumn);
end;

function TGocciaCoverageTracker.RegisterBranch(const AFilePath, ABranchType: string;
  const ALoc: TGocciaCoverageLoc; const ALocations: TGocciaCoverageLocArray): Integer;
begin
  Result := FileFor(AFilePath).RegisterBranch(ABranchType, ALoc, ALocations);
end;

function TGocciaCoverageTracker.RegisterFunction(const AFilePath, AName: string;
  const ADecl, ALoc: TGocciaCoverageLoc): Integer;
begin
  Result := FileFor(AFilePath).RegisterFunction(AName, ADecl, ALoc);
end;

procedure TGocciaCoverageTracker.RecordStatementHit(const AFilePath: string;
  const AIndex: Integer);
begin
  FileFor(AFilePath).RecordStatementHit(AIndex);
end;

procedure TGocciaCoverageTracker.RecordBranchHit(const AFilePath: string;
  const ABranch, APath: Integer);
begin
  FileFor(AFilePath).RecordBranchHit(ABranch, APath);
end;

procedure TGocciaCoverageTracker.RecordFunctionHit(const AFilePath: string;
  const AIndex: Integer);
begin
  FileFor(AFilePath).RecordFunctionHit(AIndex);
end;

function TGocciaCoverageTracker.GetFileCoverage(
  const AFilePath: string): TGocciaFileCoverage;
begin
  if not FFiles.TryGetValue(AFilePath, Result) then
    if not FFiles.TryGetValue(CanonicalizePath(AFilePath), Result) then
      Result := nil;
end;

procedure TGocciaCoverageTracker.MergeFrom(const ASource: TGocciaCoverageTracker);
var
  IterState, I, J, Index: Integer;
  Key: string;
  SrcFile, DstFile: TGocciaFileCoverage;
  Stmt: TGocciaCoverageStatement;
  Branch: TGocciaCoverageBranch;
  Func: TGocciaCoverageFunction;
  SrcMap: TGocciaSourceMap;
  ResolvedPath: string;
begin
  if (ASource = nil) or (ASource.Files = nil) then Exit;

  IterState := 0;
  while ASource.Files.GetNextEntry(IterState, Key, SrcFile) do
  begin
    DstFile := GetOrCreateFile(Key);

    { sites are matched by location: each thread registered them in its own order, so an index means
      nothing across trackers }
    for I := 0 to SrcFile.StatementCount - 1 do
    begin
      Stmt := SrcFile.Statements[I];
      Index := DstFile.RegisterStatement(Stmt.Loc, Stmt.OrderLine, Stmt.OrderColumn);
      Inc(DstFile.FStatements[Index].HitCount, Stmt.HitCount);
    end;

    { a site no worker reached still keeps its place, with zero hits }
    for I := 0 to SrcFile.BranchCount - 1 do
    begin
      Branch := SrcFile.Branches[I];
      Index := DstFile.RegisterBranch(Branch.BranchType, Branch.Loc, Branch.Locations);
      for J := 0 to High(Branch.HitCounts) do
        Inc(DstFile.FBranches[Index].HitCounts[J], Branch.HitCounts[J]);
    end;

    for I := 0 to SrcFile.FunctionCount - 1 do
    begin
      Func := SrcFile.Functions[I];
      Index := DstFile.RegisterFunction(Func.Name, Func.Decl, Func.Loc);
      Inc(DstFile.FFunctions[Index].HitCount, Func.HitCount);
    end;
  end;

  { Merge source maps — adopt from source if not already present. }
  if Assigned(ASource.SourceMaps) then
  begin
    IterState := 0;
    while ASource.SourceMaps.GetNextEntry(IterState, Key, SrcMap) do
    begin
      if not FSourceMaps.ContainsKey(Key) then
        FSourceMaps.Add(Key, SrcMap.Clone);
    end;
  end;

  { Adopt the source's canonical-key -> on-disk-path mappings.  A worker
    thread may be the only tracker that ever saw a given file, and without
    its resolved path the reporters could not read that file's source. }
  if Assigned(ASource.FResolvedPaths) then
  begin
    IterState := 0;
    while ASource.FResolvedPaths.GetNextEntry(IterState, Key, ResolvedPath) do
      if not FResolvedPaths.ContainsKey(Key) then
        FResolvedPaths.Add(Key, ResolvedPath);
  end;
end;

end.
