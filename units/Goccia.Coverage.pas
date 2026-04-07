unit Goccia.Coverage;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap;

function CountExecutableLines(const ASourceLines: TStrings): Integer;
procedure BuildExecutableLineFlags(const ASourceLines: TStrings;
  var AFlags: array of Boolean);
function IsStructuralOnly(const ATrimmed: string): Boolean;
function StartsWithWord(const ALine, AWord: string): Boolean;

type
  TGocciaCoverageBranch = record
    Line: Integer;
    Column: Integer;
    BranchIndex: Integer;
    HitCount: Integer;
  end;

  TGocciaCoverageBranchList = TList<TGocciaCoverageBranch>;

  TGocciaFileCoverage = class
  private
    FFileName: string;
    FLineHits: array of Integer;
    FExecutableLines: Integer;
    FBranches: TGocciaCoverageBranchList;
  public
    constructor Create(const AFileName: string; const AExecutableLines: Integer);
    destructor Destroy; override;

    procedure RecordLineHit(const ALine: Integer); inline;
    procedure RecordBranchHit(const ALine, AColumn, ABranchIndex: Integer);
    procedure EnsureBranchExists(const ALine, AColumn, ABranchIndex: Integer);

    function LinesHit: Integer;
    function BranchesFound: Integer;
    function BranchesHit: Integer;

    function LineHitCount: Integer;

    property FileName: string read FFileName;
    property ExecutableLines: Integer read FExecutableLines;
    property Branches: TGocciaCoverageBranchList read FBranches;
    function GetLineHitCount(const ALine: Integer): Integer; inline;
  end;

  TGocciaCoverageFileMap = TOrderedStringMap<TGocciaFileCoverage>;

  TGocciaCoverageTracker = class
  private class var
    FInstance: TGocciaCoverageTracker;
  private
    FFiles: TGocciaCoverageFileMap;
    FEnabled: Boolean;
    FLastHitFile: string;
    FLastHitLine: Integer;
    function GetOrCreateFile(const AFilePath: string): TGocciaFileCoverage;
  public
    class function Instance: TGocciaCoverageTracker;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterSourceFile(const AFilePath: string;
      const AExecutableLines: Integer);
    procedure RecordLineHit(const AFilePath: string;
      const ALine: Integer); inline;
    procedure RecordBranchHit(const AFilePath: string;
      const ALine, AColumn, ABranchIndex: Integer);

    function GetFileCoverage(const AFilePath: string): TGocciaFileCoverage;

    property Files: TGocciaCoverageFileMap read FFiles;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

uses
  SysUtils;

const
  DEFAULT_LINE_CAPACITY = 256;

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
end;

destructor TGocciaFileCoverage.Destroy;
begin
  FBranches.Free;
  inherited;
end;

procedure TGocciaFileCoverage.RecordLineHit(const ALine: Integer);
var
  NewLength, I: Integer;
begin
  if ALine <= 0 then Exit;
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
  Inc(FLineHits[ALine]);
end;

procedure TGocciaFileCoverage.RecordBranchHit(const ALine, AColumn,
  ABranchIndex: Integer);
var
  I: Integer;
  Branch: TGocciaCoverageBranch;
begin
  for I := 0 to FBranches.Count - 1 do
    if (FBranches[I].Line = ALine) and (FBranches[I].Column = AColumn) and
       (FBranches[I].BranchIndex = ABranchIndex) then
    begin
      Branch := FBranches[I];
      Inc(Branch.HitCount);
      FBranches[I] := Branch;
      // Ensure the opposite arm exists for binary branches (if/ternary/short-circuit)
      if ABranchIndex <= 1 then
        EnsureBranchExists(ALine, AColumn, 1 - ABranchIndex);
      Exit;
    end;

  Branch.Line := ALine;
  Branch.Column := AColumn;
  Branch.BranchIndex := ABranchIndex;
  Branch.HitCount := 1;
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

class function TGocciaCoverageTracker.Instance: TGocciaCoverageTracker;
begin
  Result := FInstance;
end;

class procedure TGocciaCoverageTracker.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaCoverageTracker.Create;
end;

class procedure TGocciaCoverageTracker.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaCoverageTracker.Create;
begin
  inherited Create;
  FFiles := TGocciaCoverageFileMap.Create;
  FEnabled := False;
end;

destructor TGocciaCoverageTracker.Destroy;
var
  IterState: Integer;
  Key: string;
  Value: TGocciaFileCoverage;
begin
  if Assigned(FFiles) then
  begin
    IterState := 0;
    while FFiles.GetNextEntry(IterState, Key, Value) do
      Value.Free;
    FFiles.Free;
  end;
  inherited;
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

procedure TGocciaCoverageTracker.RegisterSourceFile(const AFilePath: string;
  const AExecutableLines: Integer);
var
  FileCov: TGocciaFileCoverage;
begin
  if not FFiles.TryGetValue(AFilePath, FileCov) then
  begin
    FileCov := TGocciaFileCoverage.Create(AFilePath, AExecutableLines);
    FFiles.Add(AFilePath, FileCov);
  end;
end;

procedure TGocciaCoverageTracker.RecordLineHit(const AFilePath: string;
  const ALine: Integer);
begin
  if (ALine = FLastHitLine) and (AFilePath = FLastHitFile) then Exit;
  FLastHitLine := ALine;
  FLastHitFile := AFilePath;
  GetOrCreateFile(AFilePath).RecordLineHit(ALine);
end;

procedure TGocciaCoverageTracker.RecordBranchHit(const AFilePath: string;
  const ALine, AColumn, ABranchIndex: Integer);
begin
  GetOrCreateFile(AFilePath).RecordBranchHit(ALine, AColumn, ABranchIndex);
end;

function TGocciaCoverageTracker.GetFileCoverage(
  const AFilePath: string): TGocciaFileCoverage;
begin
  if not FFiles.TryGetValue(AFilePath, Result) then
    Result := nil;
end;

end.
