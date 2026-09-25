unit Goccia.Coverage;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap,

  Goccia.SourceMap;

function CountExecutableLines(const ASourceLines: TStrings): Integer;
procedure BuildExecutableLineFlags(const ASourceLines: TStrings;
  var AFlags: array of Boolean);
function IsStructuralOnly(const ATrimmed: string): Boolean;
function StartsWithWord(const ALine, AWord: string): Boolean;

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
  TGocciaCoverageBranch = record
    Line: Integer;
    Column: Integer;
    BranchIndex: Integer;
    HitCount: Integer;
  end;

  TGocciaCoverageBranchList = TList<TGocciaCoverageBranch>;

  TGocciaCoverageFunction = record
    Name: string;
    Line: Integer;
    Column: Integer;
    HitCount: Integer;
  end;

  TGocciaCoverageFunctionList = TList<TGocciaCoverageFunction>;

  TGocciaFileCoverage = class
  private
    FFileName: string;
    FLineHits: array of Integer;
    FExecutableLines: Integer;
    FBranches: TGocciaCoverageBranchList;
    FFunctions: TGocciaCoverageFunctionList;
  public
    constructor Create(const AFileName: string; const AExecutableLines: Integer);
    destructor Destroy; override;

    procedure AddLineHits(const ALine, ACount: Integer); {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordLineHit(const ALine: Integer); {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordBranchHit(const ALine, AColumn, ABranchIndex: Integer);
    procedure AddBranchHits(const ALine, AColumn, ABranchIndex,
      ACount: Integer);
    procedure EnsureBranchExists(const ALine, AColumn, ABranchIndex: Integer);
    procedure RegisterFunction(const AName: string;
      const ALine, AColumn: Integer);
    procedure RecordFunctionHit(const AName: string;
      const ALine, AColumn: Integer);

    function LinesHit: Integer;
    function BranchesFound: Integer;
    function BranchesHit: Integer;
    function FunctionsFound: Integer;
    function FunctionsHit: Integer;

    function LineHitCount: Integer;

    property FileName: string read FFileName;
    property ExecutableLines: Integer read FExecutableLines;
    property Branches: TGocciaCoverageBranchList read FBranches;
    property Functions: TGocciaCoverageFunctionList read FFunctions;
    function GetLineHitCount(const ALine: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}
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
    FLastHitFile: string;
    FLastHitLine: Integer;
    { Single-entry memo over the canonicalize-then-look-up pair, which
      together dominate the per-line-hit cost of a coverage run. }
    FLastLookupPath: string;
    FLastLookupFile: TGocciaFileCoverage;
    { AFilePath must already be canonical. }
    function GetOrCreateFile(const AFilePath: string): TGocciaFileCoverage;
    function CanonicalizePath(const AFilePath: string): string;
    { Canonicalize AFilePath, then resolve it to its coverage record. }
    function FileFor(const AFilePath: string): TGocciaFileCoverage;
  public
    class function Instance: TGocciaCoverageTracker;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterSourceFile(const AFilePath: string;
      const AExecutableLines: Integer);
    procedure RegisterSourceMap(const AFilePath: string;
      const ASourceMap: TGocciaSourceMap);
    procedure RecordLineHit(const AFilePath: string;
      const ALine: Integer); {$IFDEF FPC}inline;{$ENDIF}
    procedure RecordBranchHit(const AFilePath: string;
      const ALine, AColumn, ABranchIndex: Integer);
    procedure RegisterFunction(const AFilePath, AName: string;
      const ALine, AColumn: Integer);
    procedure RecordFunctionHit(const AFilePath, AName: string;
      const ALine, AColumn: Integer);

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
      Line, branch, and function hit *counts* are added together, so merging
      N workers yields the same totals a single-threaded run would produce.
      Files present in ASource but not in this tracker are created with
      0 executable lines. }
    procedure MergeFrom(const ASource: TGocciaCoverageTracker);

    property Files: TGocciaCoverageFileMap read FFiles;
    property SourceMaps: TGocciaSourceMapMap read FSourceMaps;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

uses
  SysUtils;

const
  DEFAULT_LINE_CAPACITY = 256;

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

function IsStructuralOnly(const ATrimmed: string): Boolean;
var
  I: Integer;
begin
  // Returns True if the line contains only braces, brackets, parens,
  // semicolons, commas, and whitespace — e.g., });  }  );  ],  ]);
  for I := 1 to Length(ATrimmed) do
    if not (ATrimmed[I] in ['}', ')', ']', ';', ',', '{', '(', '[', ' ', #9]) then
      Exit(False);
  Result := True;
end;

function StartsWithWord(const ALine, AWord: string): Boolean;
var
  WordLen: Integer;
begin
  WordLen := Length(AWord);
  Result := (Length(ALine) > WordLen) and
            (Copy(ALine, 1, WordLen) = AWord) and
            (ALine[WordLen + 1] in [' ', '(', '<', '{', #9]);
end;

function CountUnescapedBackticks(const ALine: string): Integer;
var
  J, Len: Integer;
begin
  Result := 0;
  Len := Length(ALine);
  J := 1;
  while J <= Len do
  begin
    if (ALine[J] = '\') then
    begin
      Inc(J, 2); // skip escaped character
      Continue;
    end;
    if ALine[J] = '`' then
      Inc(Result);
    Inc(J);
  end;
end;

function IsSkippedTestLine(const ATrimmed: string): Boolean;
begin
  Result := (Pos('test.skip(', ATrimmed) > 0) or
            (Pos('describe.skip(', ATrimmed) > 0);
end;

procedure BuildExecutableLineFlags(const ASourceLines: TStrings;
  var AFlags: array of Boolean);
var
  I, J, Len: Integer;
  Trimmed: string;
  InBlockComment: Boolean;
  InTemplateLiteral: Boolean;
  TypeBodyDepth: Integer;
  SkipBodyDepth: Integer;
begin
  InBlockComment := False;
  InTemplateLiteral := False;
  TypeBodyDepth := 0;
  SkipBodyDepth := 0;

  for I := 0 to ASourceLines.Count - 1 do
  begin
    AFlags[I] := False;
    Trimmed := Trim(ASourceLines[I]);
    Len := Length(Trimmed);

    // Empty line
    if Len = 0 then Continue;

    // Template literal continuation — lines inside backtick strings are data
    if InTemplateLiteral then
    begin
      if Odd(CountUnescapedBackticks(Trimmed)) then
        InTemplateLiteral := False;
      Continue;
    end;

    // Track block comment state
    if InBlockComment then
    begin
      if Pos('*/', Trimmed) > 0 then
        InBlockComment := False;
      Continue;
    end;

    // Single-line comment
    if (Len >= 2) and (Trimmed[1] = '/') and (Trimmed[2] = '/') then Continue;

    // Block comment opening — check if it also closes on same line
    if (Len >= 2) and (Trimmed[1] = '/') and (Trimmed[2] = '*') then
    begin
      if Pos('*/', Trimmed) > 2 then
        { Single-line block comment like /* ... */ — skip the whole line }
      else
        InBlockComment := True;
      Continue;
    end;

    // Check for template literal opening on this line (odd number of backticks
    // means the line ends inside a template)
    if Odd(CountUnescapedBackticks(Trimmed)) then
      InTemplateLiteral := True;

    // Track skipped test/describe body depth — lines inside are intentionally unexecuted
    if SkipBodyDepth > 0 then
    begin
      for J := 1 to Len do
      begin
        if Trimmed[J] = '(' then Inc(SkipBodyDepth)
        else if Trimmed[J] = ')' then Dec(SkipBodyDepth);
      end;
      Continue;
    end;

    // Detect test.skip/describe.skip — body is unconditionally skipped
    if IsSkippedTestLine(Trimmed) then
    begin
      SkipBodyDepth := 0;
      for J := 1 to Len do
      begin
        if Trimmed[J] = '(' then Inc(SkipBodyDepth)
        else if Trimmed[J] = ')' then Dec(SkipBodyDepth);
      end;
      AFlags[I] := True; // The skip line itself is executable (it gets evaluated)
      Continue;
    end;

    // Track interface body depth — lines inside are type-only
    if TypeBodyDepth > 0 then
    begin
      for J := 1 to Len do
      begin
        if Trimmed[J] = '{' then Inc(TypeBodyDepth)
        else if Trimmed[J] = '}' then Dec(TypeBodyDepth);
      end;
      Continue;
    end;

    // Detect interface block openings:
    //   interface Foo {         → opening line is executable, body is not
    //   interface Foo extends X {
    if StartsWithWord(Trimmed, 'interface') and (Pos('{', Trimmed) > 0) then
    begin
      TypeBodyDepth := 1;
      for J := Pos('{', Trimmed) + 1 to Len do
      begin
        if Trimmed[J] = '{' then Inc(TypeBodyDepth)
        else if Trimmed[J] = '}' then Dec(TypeBodyDepth);
      end;
      AFlags[I] := True; // The opening line itself is executable
      Continue;
    end;

    // Pure structural lines: only closing braces/parens/semicolons
    if IsStructuralOnly(Trimmed) then Continue;

    AFlags[I] := True;
  end;
end;

function CountExecutableLines(const ASourceLines: TStrings): Integer;
var
  Flags: array of Boolean;
  I: Integer;
begin
  SetLength(Flags, ASourceLines.Count);
  BuildExecutableLineFlags(ASourceLines, Flags);
  Result := 0;
  for I := 0 to High(Flags) do
    if Flags[I] then
      Inc(Result);
end;

{ TGocciaFileCoverage }

constructor TGocciaFileCoverage.Create(const AFileName: string;
  const AExecutableLines: Integer);
var
  I, Capacity: Integer;
begin
  inherited Create;
  FFileName := AFileName;
  FExecutableLines := AExecutableLines;
  if AExecutableLines > 0 then
    Capacity := AExecutableLines + 1
  else
    Capacity := DEFAULT_LINE_CAPACITY;
  SetLength(FLineHits, Capacity);
  for I := 0 to High(FLineHits) do
    FLineHits[I] := 0;
  FBranches := TGocciaCoverageBranchList.Create;
  FFunctions := TGocciaCoverageFunctionList.Create;
end;

destructor TGocciaFileCoverage.Destroy;
begin
  FFunctions.Free;
  FBranches.Free;
  inherited;
end;

procedure TGocciaFileCoverage.RegisterFunction(const AName: string;
  const ALine, AColumn: Integer);
var
  I: Integer;
  Func: TGocciaCoverageFunction;
begin
  if ALine <= 0 then Exit;
  for I := 0 to FFunctions.Count - 1 do
    if (FFunctions[I].Line = ALine) and
       (FFunctions[I].Column = AColumn) then
    begin
      if (FFunctions[I].Name = '') and (AName <> '') then
      begin
        Func := FFunctions[I];
        Func.Name := AName;
        FFunctions[I] := Func;
      end;
      Exit;
    end;

  Func.Name := AName;
  Func.Line := ALine;
  Func.Column := AColumn;
  Func.HitCount := 0;
  FFunctions.Add(Func);
end;

procedure TGocciaFileCoverage.RecordFunctionHit(const AName: string;
  const ALine, AColumn: Integer);
var
  I: Integer;
  Func: TGocciaCoverageFunction;
begin
  RegisterFunction(AName, ALine, AColumn);
  for I := 0 to FFunctions.Count - 1 do
    if (FFunctions[I].Line = ALine) and
       (FFunctions[I].Column = AColumn) then
    begin
      Func := FFunctions[I];
      if (Func.Name = '') and (AName <> '') then
        Func.Name := AName;
      Inc(Func.HitCount);
      FFunctions[I] := Func;
      Exit;
    end;
end;

procedure TGocciaFileCoverage.AddLineHits(const ALine, ACount: Integer);
var
  NewLength, I: Integer;
begin
  if (ALine <= 0) or (ACount <= 0) then Exit;
  if ALine >= Length(FLineHits) then
  begin
    NewLength := Length(FLineHits);
    if NewLength < 16 then
      NewLength := 16;
    while NewLength <= ALine do
      NewLength := NewLength * 2;
    I := Length(FLineHits);
    SetLength(FLineHits, NewLength);
    while I < NewLength do
    begin
      FLineHits[I] := 0;
      Inc(I);
    end;
  end;
  Inc(FLineHits[ALine], ACount);
end;

procedure TGocciaFileCoverage.RecordLineHit(const ALine: Integer);
begin
  AddLineHits(ALine, 1);
end;

procedure TGocciaFileCoverage.RecordBranchHit(const ALine, AColumn,
  ABranchIndex: Integer);
begin
  AddBranchHits(ALine, AColumn, ABranchIndex, 1);
end;

procedure TGocciaFileCoverage.AddBranchHits(const ALine, AColumn,
  ABranchIndex, ACount: Integer);
var
  I: Integer;
  Branch: TGocciaCoverageBranch;
begin
  if ACount <= 0 then
  begin
    EnsureBranchExists(ALine, AColumn, ABranchIndex);
    Exit;
  end;

  for I := 0 to FBranches.Count - 1 do
    if (FBranches[I].Line = ALine) and (FBranches[I].Column = AColumn) and
       (FBranches[I].BranchIndex = ABranchIndex) then
    begin
      Branch := FBranches[I];
      Inc(Branch.HitCount, ACount);
      FBranches[I] := Branch;
      // Ensure the opposite arm exists for binary branches (if/ternary/short-circuit)
      if ABranchIndex <= 1 then
        EnsureBranchExists(ALine, AColumn, 1 - ABranchIndex);
      Exit;
    end;

  Branch.Line := ALine;
  Branch.Column := AColumn;
  Branch.BranchIndex := ABranchIndex;
  Branch.HitCount := ACount;
  FBranches.Add(Branch);

  // Ensure the opposite arm exists for binary branches
  if ABranchIndex <= 1 then
    EnsureBranchExists(ALine, AColumn, 1 - ABranchIndex);
end;

procedure TGocciaFileCoverage.EnsureBranchExists(const ALine, AColumn,
  ABranchIndex: Integer);
var
  I: Integer;
  Branch: TGocciaCoverageBranch;
begin
  for I := 0 to FBranches.Count - 1 do
    if (FBranches[I].Line = ALine) and (FBranches[I].Column = AColumn) and
       (FBranches[I].BranchIndex = ABranchIndex) then
      Exit;

  Branch.Line := ALine;
  Branch.Column := AColumn;
  Branch.BranchIndex := ABranchIndex;
  Branch.HitCount := 0;
  FBranches.Add(Branch);
end;

function TGocciaFileCoverage.LinesHit: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to High(FLineHits) do
    if FLineHits[I] > 0 then
      Inc(Result);
end;

function TGocciaFileCoverage.BranchesFound: Integer;
begin
  Result := FBranches.Count;
end;

function TGocciaFileCoverage.BranchesHit: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBranches.Count - 1 do
    if FBranches[I].HitCount > 0 then
      Inc(Result);
end;

function TGocciaFileCoverage.FunctionsFound: Integer;
begin
  Result := FFunctions.Count;
end;

function TGocciaFileCoverage.FunctionsHit: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FFunctions.Count - 1 do
    if FFunctions[I].HitCount > 0 then
      Inc(Result);
end;

function TGocciaFileCoverage.LineHitCount: Integer;
begin
  Result := Length(FLineHits);
end;

function TGocciaFileCoverage.GetLineHitCount(const ALine: Integer): Integer;
begin
  if (ALine >= 0) and (ALine < Length(FLineHits)) then
    Result := FLineHits[ALine]
  else
    Result := 0;
end;

{ TGocciaCoverageTracker }

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
  FEnabled := False;
  FLastLookupFile := nil;
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
    Result := TGocciaFileCoverage.Create(AFilePath, 0);
    FFiles.Add(AFilePath, Result);
  end;
end;

function TGocciaCoverageTracker.FileFor(
  const AFilePath: string): TGocciaFileCoverage;
begin
  if Assigned(FLastLookupFile) and (AFilePath = FLastLookupPath) then
    Exit(FLastLookupFile);
  Result := GetOrCreateFile(CanonicalizePath(AFilePath));
  FLastLookupPath := AFilePath;
  FLastLookupFile := Result;
end;

function TGocciaCoverageTracker.ResolvedSourcePath(
  const ACanonicalPath: string): string;
begin
  if not FResolvedPaths.TryGetValue(ACanonicalPath, Result) then
    Result := ACanonicalPath;
end;

procedure TGocciaCoverageTracker.RegisterSourceFile(const AFilePath: string;
  const AExecutableLines: Integer);
var
  FileCov: TGocciaFileCoverage;
  Key: string;
begin
  Key := CanonicalizePath(AFilePath);
  if not FFiles.TryGetValue(Key, FileCov) then
  begin
    FileCov := TGocciaFileCoverage.Create(Key, AExecutableLines);
    FFiles.Add(Key, FileCov);
  end
  { A file registered once as an import (auto-created with no executable
    line count) and again as the entry point is one record, not two. }
  else if (FileCov.ExecutableLines = 0) and (AExecutableLines > 0) then
    FileCov.FExecutableLines := AExecutableLines;
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

procedure TGocciaCoverageTracker.RecordLineHit(const AFilePath: string;
  const ALine: Integer);
begin
  if (ALine = FLastHitLine) and (AFilePath = FLastHitFile) then Exit;
  FLastHitLine := ALine;
  FLastHitFile := AFilePath;
  FileFor(AFilePath).RecordLineHit(ALine);
end;

procedure TGocciaCoverageTracker.RecordBranchHit(const AFilePath: string;
  const ALine, AColumn, ABranchIndex: Integer);
begin
  FileFor(AFilePath).RecordBranchHit(ALine, AColumn, ABranchIndex);
end;

procedure TGocciaCoverageTracker.RegisterFunction(const AFilePath,
  AName: string; const ALine, AColumn: Integer);
begin
  FileFor(AFilePath).RegisterFunction(AName, ALine, AColumn);
end;

procedure TGocciaCoverageTracker.RecordFunctionHit(const AFilePath,
  AName: string; const ALine, AColumn: Integer);
begin
  FileFor(AFilePath).RecordFunctionHit(AName, ALine, AColumn);
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
  IterState, I: Integer;
  Key: string;
  SrcFile, DstFile: TGocciaFileCoverage;
  Branch: TGocciaCoverageBranch;
  Func, DstFunc: TGocciaCoverageFunction;
  J: Integer;
  SrcMap: TGocciaSourceMap;
  ResolvedPath: string;
begin
  if (ASource = nil) or (ASource.Files = nil) then Exit;

  IterState := 0;
  while ASource.Files.GetNextEntry(IterState, Key, SrcFile) do
  begin
    DstFile := GetOrCreateFile(Key);
    { If the destination was auto-created with 0 executable lines but the
      source has a proper count, adopt it. }
    if (DstFile.ExecutableLines = 0) and (SrcFile.ExecutableLines > 0) then
      DstFile.FExecutableLines := SrcFile.ExecutableLines;

    { Merge line hits — sum the source counts, not one hit per covered line. }
    for I := 1 to SrcFile.LineHitCount - 1 do
      DstFile.AddLineHits(I, SrcFile.GetLineHitCount(I));

    { Merge branch hits — sum the source counts; zero-hit arms are still
      registered so the branch map keeps its shape. }
    for I := 0 to SrcFile.Branches.Count - 1 do
    begin
      Branch := SrcFile.Branches[I];
      DstFile.AddBranchHits(Branch.Line, Branch.Column, Branch.BranchIndex,
        Branch.HitCount);
    end;

    { Merge function definitions and hit counts by source position. }
    for I := 0 to SrcFile.Functions.Count - 1 do
    begin
      Func := SrcFile.Functions[I];
      DstFile.RegisterFunction(Func.Name, Func.Line, Func.Column);
      if Func.HitCount > 0 then
        for J := 0 to DstFile.Functions.Count - 1 do
          if (DstFile.Functions[J].Line = Func.Line) and
             (DstFile.Functions[J].Column = Func.Column) then
          begin
            DstFunc := DstFile.Functions[J];
            Inc(DstFunc.HitCount, Func.HitCount);
            DstFile.Functions[J] := DstFunc;
            Break;
          end;
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
