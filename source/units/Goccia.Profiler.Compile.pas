unit Goccia.Profiler.Compile;

{$I Goccia.inc}

{ The compile-phase profile (--profile=compile): lex, parse and compile self
  time per source function and per file, and counts of the parser's and the
  compiler's expensive operations, so a cost that grows faster than the
  source shows where it grows.

  The parser and the compiler take the per-thread instance once, when they
  are created or start a compile, and keep it in a field (a scope inherits
  its parent's), so with profiling off every counting site costs one nil
  test of a field it already holds and no thread-variable lookup.

  Attribution is by switch: the operations counted and the time spent since
  the last switch belong to the function being parsed or compiled at that
  moment. The parser switches as it enters and leaves a function; the
  compiler switches as it enters and leaves a function template. A function
  is identified by its file and the line and column of its node, which both
  phases see the same way, so its parse and its compile meet in one record.
  The top level of a file is line 0, column 0. }

interface

uses
  HashMap,
  OrderedStringMap,
  StringBuffer;

type
  { Each counter counts work whose amount a linear compile keeps
    proportional to the function's nodes or tokens. }
  TGocciaCompileCounter = (
    // Expressions and statements compiled (CompileExpressionNode,
    // CompileStatementNode): the size the other counters are compared with.
    ccNodesCompiled,
    // Constant-folding attempts: the one made for every expression node
    // (CompileExpressionNode), and those of TryEmitConstantExpression.
    ccConstantEmitAttempts,
    // TryEvaluateConstantExpression, counting each recursive step: the walk
    // a constant-folding attempt makes over an operand subtree.
    ccConstantEvaluationVisits,
    // ExpressionType, counting each recursive step: type inference over an
    // expression subtree.
    ccExpressionTypeVisits,
    // HasExactNumberProof: the Number proof of an operand.
    ccNumberProofVisits,
    // Name lookups among one function scope's locals, and the locals each
    // examined (actual buckets for the hashed index, entries without it, plus
    // each shadowed local of the name it steps over).
    ccLocalLookups,
    ccLocalLookupProbes,
    // ResolveUpvalue, one per function scope it climbs, and the upvalue
    // entries scanned for an existing capture.
    ccUpvalueResolutions,
    ccUpvalueListProbes,
    // Locals visited by walks over a function scope's list of locals: the
    // reset of what earlier case clauses settled (ForgetCaseClauseFacts),
    // relinking the names after a rename (RelinkLocalNames), and suspending
    // the bindings a finally block must not see (SuspendBindingsDeeperThan).
    ccLocalListScans,
    // Lookups in a function's hashed set of parameter names, one per name
    // it records and one per test of a name.
    ccParameterNameProbes,
    // Lookups in a list of unique binding names (collecting a pattern's or a
    // declaration's names, the duplicate-parameter check), and the names
    // each compared: 1 once the list is hashed (16 names or more), the names
    // scanned below that.
    ccBindingNameLookups,
    ccBindingNameProbes,
    // Parameters visited by the accumulating default-initializer proof.
    ccParameterInitializerVisits,
    // Closure source checks for a simple parameter default destination.
    ccParameterInitializerSourceChecks);

  TGocciaCompileCounters = array[TGocciaCompileCounter] of Int64;

  TGocciaCompileFunctionProfile = record
    Name: string;
    SourceFile: string;
    Line: Integer;
    Column: Integer;
    LexNanoseconds: Int64;
    ParseNanoseconds: Int64;
    CompileNanoseconds: Int64;
    Tokens: Int64;
    Instructions: Int64;
    Counters: TGocciaCompileCounters;
    // Nodes compiled by kind, indexed like the profiler's node kinds.
    NodeKinds: array of Int64;
    // The compile that last named the function (NameFunction).
    Generation: Integer;
  end;

  TGocciaCompilePhase = (cphNone, cphParse, cphCompile);

  TGocciaCompileProfileFrame = record
    Target: Integer;
    Phase: TGocciaCompilePhase;
    Source: Pointer;
    LastLexNanoseconds: Int64;
    LastTokens: Int64;
  end;

  TGocciaCompileProfiler = class
  private
    FFunctions: array of TGocciaCompileFunctionProfile;
    FFunctionCount: Integer;
    FDeterministic: Boolean;
    FFunctionIndex: TOrderedStringMap<Integer>;
    FExcludedFiles: TOrderedStringMap<Boolean>;
    FNodeKindIndex: THashMap<TClass, Integer>;
    FNodeKindNames: array of string;
    FNodeKindCounts: array of Int64;
    FNodeKindCount: Integer;
    // The work counted since the last switch, and whom it is charged to.
    FActive: TGocciaCompileCounters;
    FTarget: Integer;
    FPhase: TGocciaCompilePhase;
    FLastNanoseconds: Int64;
    FLastLexNanoseconds: Int64;
    FLastTokens: Int64;
    // The token list of the parse whose lexer FLast* were read from.
    FSource: Pointer;
    // What each open parse function or compile interrupted.
    FSaved: array of TGocciaCompileProfileFrame;
    FSavedCount: Integer;
    // Names that stand in for the file while code compiled at run time is
    // parsed and compiled (BeginUnit).
    FUnitNames: array of string;
    FUnitCount: Integer;
    // Each distinct run-time source (kind and text), by its ordinal.
    FUnitIdentities: TOrderedStringMap<Integer>;
    FGeneration: Integer;
    function FunctionFor(const AFile: string; const ALine,
      AColumn: Integer): Integer;
    procedure Switch(const ALexNanoseconds, ATokens: Int64);
    procedure Save;
    procedure Restore;
  public
    class function Instance: TGocciaCompileProfiler;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;
    destructor Destroy; override;

    procedure Count(const ACounter: TGocciaCompileCounter); overload; {$IFDEF FPC}inline;{$ENDIF}
    procedure Count(const ACounter: TGocciaCompileCounter;
      const AAmount: Int64); overload; {$IFDEF FPC}inline;{$ENDIF}
    procedure CountLookup(const ALookups, AProbes: TGocciaCompileCounter;
      const AProbeCount: Int64); {$IFDEF FPC}inline;{$ENDIF}
    procedure CountNode(const AKind: TClass);

    // The parser: around the top level of a file and around each function.
    // ASource identifies the parse's token stream; ALexNanoseconds and
    // ATokens are its lexer's running scan time and the tokens scanned so
    // far, read at the call.
    procedure BeginParse(const AFile: string; const ALine, AColumn: Integer;
      const ASource: Pointer; const ALexNanoseconds, ATokens: Int64);
    procedure EndParse(const ALexNanoseconds, ATokens: Int64);

    // The compiler: around one compile, and at each switch of template.
    // ALeaving* describe the template being left, if any.
    procedure BeginCompile;
    procedure EnterTemplate(const ALeavingName: string;
      const ALeavingInstructions: Integer; const AName, AFile: string;
      const ALine, AColumn: Integer);
    procedure EndCompile(const ALeavingName: string;
      const ALeavingInstructions: Integer);
    // A function's final name and instruction count, once its compile ended.
    procedure NameFunction(const AFile: string; const ALine, AColumn: Integer;
      const AName: string; const AInstructions: Integer);

    // Code compiled at run time (eval, the Function constructor) is parsed
    // and compiled as a file of its own, AKind followed by the ordinal of
    // ASource among the distinct sources seen (of every kind), instead of
    // the file of its caller: its functions do not merge with that file's,
    // nor with another source's, and the same source compiled again adds to
    // its own records.
    procedure BeginUnit(const AKind, ASource: string);
    // Ends the units begun since UnitDepth read ADepth; a second call ends
    // none.
    procedure EndUnitTo(const ADepth: Integer);
    property UnitDepth: Integer read FUnitCount;

    // Engine code (bootstrap shims) is left out of the report.
    procedure ExcludeFile(const AFile: string);
    function IsExcluded(const AFile: string): Boolean;

    function GetFunction(const AIndex: Integer): TGocciaCompileFunctionProfile;
    property Deterministic: Boolean read FDeterministic write FDeterministic;
    property FunctionCount: Integer read FFunctionCount;
    property NodeKindCount: Integer read FNodeKindCount;
    function NodeKindName(const AIndex: Integer): string;
    function NodeKindTotal(const AIndex: Integer): Int64;
  end;

const
  COMPILE_COUNTER_JSON_NAMES: array[TGocciaCompileCounter] of string = (
    'nodesCompiled',
    'constantEmitAttempts',
    'constantEvaluationVisits',
    'expressionTypeVisits',
    'numberProofVisits',
    'localLookups',
    'localLookupProbes',
    'upvalueResolutions',
    'upvalueListProbes',
    'localListScans',
    'parameterNameProbes',
    'bindingNameLookups',
    'bindingNameProbes',
    'parameterInitializerVisits',
    'parameterInitializerSourceChecks');

  COMPILE_COUNTER_LABELS: array[TGocciaCompileCounter] of string = (
    'Nodes compiled',
    'Constant-emit attempts',
    'Constant-evaluation visits',
    'Expression-type visits',
    'Number-proof visits',
    'Local lookups',
    'Local lookup probes',
    'Upvalue resolutions',
    'Upvalue list probes',
    'Local-list scans',
    'Parameter-name probes',
    'Binding-name lookups',
    'Binding-name probes',
    'Parameter-initializer visits',
    'Parameter-initializer source checks');

procedure PrintCompileProfile(const AProfiler: TGocciaCompileProfiler);
{ The "compile" section of the profile JSON, without a trailing comma: empty
  arrays and zero counters when AProfiler is nil. }
procedure AppendCompileProfileJSON(var ABuffer: TStringBuffer;
  const AProfiler: TGocciaCompileProfiler);

implementation

uses
  SysUtils,

  NumericText,
  TimingUtils,

  Goccia.JSON.Utils;

const
  MAX_COMPILE_FUNCTIONS_DISPLAYED = 20;
  MAX_NODE_KINDS_DISPLAYED = 20;
  NODE_KIND_PREFIX = 'TGoccia';

threadvar
  CompileProfilerThreadInstance: TGocciaCompileProfiler;

{ TGocciaCompileProfiler }

class function TGocciaCompileProfiler.Instance: TGocciaCompileProfiler;
begin
  Result := CompileProfilerThreadInstance;
end;

class procedure TGocciaCompileProfiler.Initialize;
begin
  if not Assigned(CompileProfilerThreadInstance) then
    CompileProfilerThreadInstance := TGocciaCompileProfiler.Create;
end;

class procedure TGocciaCompileProfiler.Shutdown;
begin
  FreeAndNil(CompileProfilerThreadInstance);
end;

constructor TGocciaCompileProfiler.Create;
begin
  inherited Create;
  FFunctionIndex := TOrderedStringMap<Integer>.Create;
  FExcludedFiles := TOrderedStringMap<Boolean>.Create;
  FUnitIdentities := TOrderedStringMap<Integer>.Create;
  FNodeKindIndex := THashMap<TClass, Integer>.Create;
  FFunctionCount := 0;
  FNodeKindCount := 0;
  FTarget := -1;
  FPhase := cphNone;
  FSavedCount := 0;
  FSource := nil;
  FillChar(FActive, SizeOf(FActive), 0);
end;

destructor TGocciaCompileProfiler.Destroy;
begin
  FNodeKindIndex.Free;
  FUnitIdentities.Free;
  FExcludedFiles.Free;
  FFunctionIndex.Free;
  inherited;
end;

procedure TGocciaCompileProfiler.Count(const ACounter: TGocciaCompileCounter);
begin
  Inc(FActive[ACounter]);
end;

procedure TGocciaCompileProfiler.Count(const ACounter: TGocciaCompileCounter;
  const AAmount: Int64);
begin
  Inc(FActive[ACounter], AAmount);
end;

procedure TGocciaCompileProfiler.CountLookup(const ALookups,
  AProbes: TGocciaCompileCounter; const AProbeCount: Int64);
begin
  Inc(FActive[ALookups]);
  Inc(FActive[AProbes], AProbeCount);
end;

procedure TGocciaCompileProfiler.CountNode(const AKind: TClass);
var
  Index: Integer;
  Name: string;
begin
  Inc(FActive[ccNodesCompiled]);
  if not FNodeKindIndex.TryGetValue(AKind, Index) then
  begin
    Name := AKind.ClassName;
    if Copy(Name, 1, Length(NODE_KIND_PREFIX)) = NODE_KIND_PREFIX then
      Delete(Name, 1, Length(NODE_KIND_PREFIX));
    if FNodeKindCount >= Length(FNodeKindNames) then
    begin
      SetLength(FNodeKindNames, FNodeKindCount * 2 + 16);
      SetLength(FNodeKindCounts, FNodeKindCount * 2 + 16);
    end;
    Index := FNodeKindCount;
    FNodeKindNames[Index] := Name;
    FNodeKindCounts[Index] := 0;
    Inc(FNodeKindCount);
    FNodeKindIndex.Add(AKind, Index);
  end;
  Inc(FNodeKindCounts[Index]);
  // Nodes are counted only while a template is open, so the target is the
  // function the node belongs to.
  if FTarget >= 0 then
  begin
    if Length(FFunctions[FTarget].NodeKinds) <= Index then
      SetLength(FFunctions[FTarget].NodeKinds, FNodeKindCount);
    Inc(FFunctions[FTarget].NodeKinds[Index]);
  end;
end;

// The compiler keeps a function's column in 16 bits (its debug information),
// the parser in full: the key wraps it as the compiler does, and the record
// keeps the full column the parser saw.
function TGocciaCompileProfiler.FunctionFor(const AFile: string;
  const ALine, AColumn: Integer): Integer;
var
  Key, FileName: string;
begin
  if FUnitCount > 0 then
    FileName := FUnitNames[FUnitCount - 1]
  else
    FileName := AFile;
  Key := IntToStr(ALine) + ':' + IntToStr(AColumn and $FFFF) + ':' + FileName;
  if FFunctionIndex.TryGetValue(Key, Result) then
  begin
    if AColumn > FFunctions[Result].Column then
      FFunctions[Result].Column := AColumn;
    Exit;
  end;
  if FFunctionCount >= Length(FFunctions) then
    SetLength(FFunctions, FFunctionCount * 2 + 16);
  Result := FFunctionCount;
  FFunctions[Result] := Default(TGocciaCompileFunctionProfile);
  FFunctions[Result].SourceFile := FileName;
  FFunctions[Result].Line := ALine;
  FFunctions[Result].Column := AColumn;
  if (ALine = 0) and (AColumn = 0) then
    FFunctions[Result].Name := '<module>';
  Inc(FFunctionCount);
  FFunctionIndex.Add(Key, Result);
end;

// Charges the time and work since the last switch to the current target:
// in a parse, the lexer's scan time to lexing, the rest to parsing.
procedure TGocciaCompileProfiler.Switch(const ALexNanoseconds, ATokens: Int64);
var
  Now, Elapsed, LexElapsed: Int64;
  Counter: TGocciaCompileCounter;
begin
  Now := GetNanoseconds;
  Elapsed := Now - FLastNanoseconds;
  if FTarget >= 0 then
  begin
    for Counter := Low(TGocciaCompileCounter) to High(TGocciaCompileCounter) do
      Inc(FFunctions[FTarget].Counters[Counter], FActive[Counter]);
    case FPhase of
      cphParse:
        begin
          LexElapsed := ALexNanoseconds - FLastLexNanoseconds;
          if LexElapsed < 0 then
            LexElapsed := 0;
          if LexElapsed > Elapsed then
            LexElapsed := Elapsed;
          Inc(FFunctions[FTarget].LexNanoseconds, LexElapsed);
          Inc(FFunctions[FTarget].ParseNanoseconds, Elapsed - LexElapsed);
          if ATokens > FLastTokens then
            Inc(FFunctions[FTarget].Tokens, ATokens - FLastTokens);
        end;
      cphCompile:
        Inc(FFunctions[FTarget].CompileNanoseconds, Elapsed);
    end;
  end;
  FillChar(FActive, SizeOf(FActive), 0);
  FLastNanoseconds := Now;
  FLastLexNanoseconds := ALexNanoseconds;
  FLastTokens := ATokens;
end;

procedure TGocciaCompileProfiler.Save;
begin
  if FSavedCount >= Length(FSaved) then
    SetLength(FSaved, FSavedCount * 2 + 16);
  FSaved[FSavedCount].Target := FTarget;
  FSaved[FSavedCount].Phase := FPhase;
  FSaved[FSavedCount].Source := FSource;
  FSaved[FSavedCount].LastLexNanoseconds := FLastLexNanoseconds;
  FSaved[FSavedCount].LastTokens := FLastTokens;
  Inc(FSavedCount);
end;

// Back to what the last Save interrupted. Its lexer has not scanned since
// (the interrupting work had a lexer of its own, or none), so its readings
// are the ones it was interrupted at.
procedure TGocciaCompileProfiler.Restore;
begin
  if FSavedCount > 0 then
  begin
    Dec(FSavedCount);
    FTarget := FSaved[FSavedCount].Target;
    FPhase := FSaved[FSavedCount].Phase;
    if FSaved[FSavedCount].Source <> FSource then
    begin
      FSource := FSaved[FSavedCount].Source;
      FLastLexNanoseconds := FSaved[FSavedCount].LastLexNanoseconds;
      FLastTokens := FSaved[FSavedCount].LastTokens;
    end;
  end
  else
  begin
    FTarget := -1;
    FPhase := cphNone;
  end;
end;

procedure TGocciaCompileProfiler.BeginParse(const AFile: string;
  const ALine, AColumn: Integer; const ASource: Pointer;
  const ALexNanoseconds, ATokens: Int64);
begin
  if ASource = FSource then
    Switch(ALexNanoseconds, ATokens)
  else
  begin
    // Another parse's lexer: what this one scanned since the last switch
    // cannot be told apart, so the interrupted work keeps only its time.
    Switch(FLastLexNanoseconds, FLastTokens);
    Save;
    FSource := ASource;
    FLastLexNanoseconds := ALexNanoseconds;
    FLastTokens := ATokens;
    FTarget := FunctionFor(AFile, ALine, AColumn);
    FPhase := cphParse;
    Exit;
  end;
  Save;
  FTarget := FunctionFor(AFile, ALine, AColumn);
  FPhase := cphParse;
end;

procedure TGocciaCompileProfiler.EndParse(const ALexNanoseconds,
  ATokens: Int64);
begin
  Switch(ALexNanoseconds, ATokens);
  Restore;
end;

procedure TGocciaCompileProfiler.BeginUnit(const AKind, ASource: string);
var
  Identity: string;
  Ordinal: Integer;
begin
  // The whole text is the identity, so two sources never share a unit; the
  // units of one source (a Function constructor's validation parses and
  // its wrapper) share its ordinal.
  Identity := ASource;
  if not FUnitIdentities.TryGetValue(Identity, Ordinal) then
  begin
    Ordinal := FUnitIdentities.Count + 1;
    FUnitIdentities.Add(Identity, Ordinal);
  end;
  if FUnitCount >= Length(FUnitNames) then
    SetLength(FUnitNames, FUnitCount * 2 + 4);
  FUnitNames[FUnitCount] := AKind + '#' + IntToStr(Ordinal);
  Inc(FUnitCount);
end;

procedure TGocciaCompileProfiler.EndUnitTo(const ADepth: Integer);
begin
  if (ADepth >= 0) and (ADepth < FUnitCount) then
    FUnitCount := ADepth;
end;

procedure TGocciaCompileProfiler.BeginCompile;
begin
  Inc(FGeneration);
  Switch(FLastLexNanoseconds, FLastTokens);
  Save;
  FTarget := -1;
  FPhase := cphCompile;
end;

procedure TGocciaCompileProfiler.EnterTemplate(const ALeavingName: string;
  const ALeavingInstructions: Integer; const AName, AFile: string;
  const ALine, AColumn: Integer);
begin
  Switch(FLastLexNanoseconds, FLastTokens);
  if FTarget >= 0 then
  begin
    if ALeavingName <> '' then
      FFunctions[FTarget].Name := ALeavingName;
    if ALeavingInstructions >= 0 then
      FFunctions[FTarget].Instructions := ALeavingInstructions;
  end;
  FTarget := FunctionFor(AFile, ALine, AColumn);
  if AName <> '' then
    FFunctions[FTarget].Name := AName;
  FPhase := cphCompile;
end;

procedure TGocciaCompileProfiler.EndCompile(const ALeavingName: string;
  const ALeavingInstructions: Integer);
begin
  Switch(FLastLexNanoseconds, FLastTokens);
  if FTarget >= 0 then
  begin
    if ALeavingName <> '' then
      FFunctions[FTarget].Name := ALeavingName;
    if ALeavingInstructions >= 0 then
      FFunctions[FTarget].Instructions := ALeavingInstructions;
  end;
  Restore;
end;

procedure TGocciaCompileProfiler.NameFunction(const AFile: string;
  const ALine, AColumn: Integer; const AName: string;
  const AInstructions: Integer);
var
  Index: Integer;
begin
  Index := FunctionFor(AFile, ALine, AColumn);
  // Templates that share a position (a class's field initializers and
  // static blocks) add up; the first one names the record.
  if FFunctions[Index].Generation = FGeneration then
  begin
    Inc(FFunctions[Index].Instructions, AInstructions);
    Exit;
  end;
  FFunctions[Index].Generation := FGeneration;
  if AName <> '' then
    FFunctions[Index].Name := AName;
  FFunctions[Index].Instructions := AInstructions;
end;

procedure TGocciaCompileProfiler.ExcludeFile(const AFile: string);
begin
  FExcludedFiles.AddOrSetValue(AFile, True);
end;

function TGocciaCompileProfiler.IsExcluded(const AFile: string): Boolean;
begin
  Result := FExcludedFiles.ContainsKey(AFile);
end;

function TGocciaCompileProfiler.GetFunction(
  const AIndex: Integer): TGocciaCompileFunctionProfile;
begin
  Result := FFunctions[AIndex];
  if FDeterministic then
  begin
    Result.LexNanoseconds := 0;
    Result.ParseNanoseconds := 0;
    Result.CompileNanoseconds := 0;
  end;
end;

function TGocciaCompileProfiler.NodeKindName(const AIndex: Integer): string;
begin
  Result := FNodeKindNames[AIndex];
end;

function TGocciaCompileProfiler.NodeKindTotal(const AIndex: Integer): Int64;
begin
  Result := FNodeKindCounts[AIndex];
end;

{ Report }

type
  TNodeKindCounts = array of Int64;

  TCompileFileSummary = record
    SourceFile: string;
    LexNanoseconds: Int64;
    ParseNanoseconds: Int64;
    CompileNanoseconds: Int64;
    Functions: Integer;
    Tokens: Int64;
    Instructions: Int64;
    Counters: TGocciaCompileCounters;
    NodeKinds: TNodeKindCounts;
  end;

  TCompileReport = record
    Functions: array of TGocciaCompileFunctionProfile;
    Files: array of TCompileFileSummary;
    Totals: TGocciaCompileCounters;
  end;

function SelfNanoseconds(const AProfile: TGocciaCompileFunctionProfile): Int64;
begin
  Result := AProfile.LexNanoseconds + AProfile.ParseNanoseconds +
    AProfile.CompileNanoseconds;
end;

// A stable merge sort of AOrder by descending AKeys: O(n log n), so a file of
// many thousands of functions reports as fast as it compiles.
procedure SortByDescendingKey(var AOrder: array of Integer;
  const AKeys: array of Int64);
var
  Scratch: array of Integer;
  Width, Low, Middle, High, Left, Right, Index: Integer;
begin
  SetLength(Scratch, Length(AOrder));
  Width := 1;
  while Width < Length(AOrder) do
  begin
    Low := 0;
    while Low < Length(AOrder) do
    begin
      Middle := Low + Width;
      if Middle > Length(AOrder) then
        Middle := Length(AOrder);
      High := Low + 2 * Width;
      if High > Length(AOrder) then
        High := Length(AOrder);
      Left := Low;
      Right := Middle;
      for Index := Low to High - 1 do
        if (Left < Middle) and ((Right >= High) or
           (AKeys[AOrder[Left]] >= AKeys[AOrder[Right]])) then
        begin
          Scratch[Index] := AOrder[Left];
          Inc(Left);
        end
        else
        begin
          Scratch[Index] := AOrder[Right];
          Inc(Right);
        end;
      Low := High;
    end;
    for Index := 0 to System.High(AOrder) do
      AOrder[Index] := Scratch[Index];
    Width := Width * 2;
  end;
end;

procedure AddNodeKinds(var ATotal: TNodeKindCounts;
  const AKinds: array of Int64);
var
  I: Integer;
begin
  if Length(ATotal) < Length(AKinds) then
    SetLength(ATotal, Length(AKinds));
  for I := 0 to System.High(AKinds) do
    Inc(ATotal[I], AKinds[I]);
end;

// The functions of the files not excluded, by descending self time, and the
// per-file sums in the order the files were first seen.
function BuildCompileReport(
  const AProfiler: TGocciaCompileProfiler): TCompileReport;
var
  I, J, FileCount, FunctionCount: Integer;
  Profile: TGocciaCompileFunctionProfile;
  Included: array of TGocciaCompileFunctionProfile;
  Order: array of Integer;
  Keys: array of Int64;
  FileIndex: TOrderedStringMap<Integer>;
  Counter: TGocciaCompileCounter;
begin
  SetLength(Result.Functions, 0);
  SetLength(Result.Files, 0);
  FillChar(Result.Totals, SizeOf(Result.Totals), 0);
  if not Assigned(AProfiler) then
    Exit;

  FileIndex := TOrderedStringMap<Integer>.Create;
  try
    SetLength(Included, AProfiler.FunctionCount);
    FunctionCount := 0;
    FileCount := 0;
    for I := 0 to AProfiler.FunctionCount - 1 do
    begin
      Profile := AProfiler.GetFunction(I);
      if AProfiler.IsExcluded(Profile.SourceFile) then
        Continue;
      Included[FunctionCount] := Profile;
      Inc(FunctionCount);
      if not FileIndex.TryGetValue(Profile.SourceFile, J) then
      begin
        J := FileCount;
        Inc(FileCount);
        SetLength(Result.Files, FileCount);
        Result.Files[J] := Default(TCompileFileSummary);
        Result.Files[J].SourceFile := Profile.SourceFile;
        FileIndex.Add(Profile.SourceFile, J);
      end;
      Inc(Result.Files[J].LexNanoseconds, Profile.LexNanoseconds);
      Inc(Result.Files[J].ParseNanoseconds, Profile.ParseNanoseconds);
      Inc(Result.Files[J].CompileNanoseconds, Profile.CompileNanoseconds);
      Inc(Result.Files[J].Functions);
      Inc(Result.Files[J].Tokens, Profile.Tokens);
      Inc(Result.Files[J].Instructions, Profile.Instructions);
      AddNodeKinds(Result.Files[J].NodeKinds, Profile.NodeKinds);
      for Counter := Low(TGocciaCompileCounter) to High(TGocciaCompileCounter) do
      begin
        Inc(Result.Files[J].Counters[Counter], Profile.Counters[Counter]);
        Inc(Result.Totals[Counter], Profile.Counters[Counter]);
      end;
    end;
  finally
    FileIndex.Free;
  end;

  SetLength(Order, FunctionCount);
  SetLength(Keys, FunctionCount);
  for I := 0 to FunctionCount - 1 do
  begin
    Order[I] := I;
    Keys[I] := SelfNanoseconds(Included[I]);
  end;
  SortByDescendingKey(Order, Keys);
  SetLength(Result.Functions, FunctionCount);
  for I := 0 to FunctionCount - 1 do
    Result.Functions[I] := Included[Order[I]];
end;

function FunctionLocation(const AProfile: TGocciaCompileFunctionProfile): string;
begin
  if AProfile.Line > 0 then
    Result := Format('%s:%d:%d', [AProfile.SourceFile, AProfile.Line,
      AProfile.Column])
  else
    Result := AProfile.SourceFile;
end;

function FunctionDisplayName(
  const AProfile: TGocciaCompileFunctionProfile): string;
begin
  if AProfile.Name <> '' then
    Result := AProfile.Name
  else
    Result := '<anonymous>';
end;

function PerNode(const ACount, ANodes: Int64): string;
begin
  if ANodes > 0 then
    Result := Format('%.1f', [ACount / ANodes], CreateInvariantFormatSettings)
  else
    Result := '-';
end;

procedure PrintCompileProfile(const AProfiler: TGocciaCompileProfiler);
var
  Report: TCompileReport;
  I, Shown, Top: Integer;
  Counter: TGocciaCompileCounter;
  KindOrder: array of Integer;
  J, Temp: Integer;
begin
  Report := BuildCompileReport(AProfiler);
  WriteLn;
  if Length(Report.Functions) = 0 then
  begin
    WriteLn('Compile Profile: (nothing parsed or compiled)');
    Exit;
  end;

  WriteLn('Compile Profile:');
  WriteLn(Format('  %-12s %-12s %-12s %9s %10s %10s  %s', [
    'Lex', 'Parse', 'Compile', 'Functions', 'Tokens', 'Nodes', 'File']));
  for I := 0 to High(Report.Files) do
    WriteLn(Format('  %-12s %-12s %-12s %9d %10d %10d  %s', [
      FormatDuration(Report.Files[I].LexNanoseconds),
      FormatDuration(Report.Files[I].ParseNanoseconds),
      FormatDuration(Report.Files[I].CompileNanoseconds),
      Report.Files[I].Functions,
      Report.Files[I].Tokens,
      Report.Files[I].Counters[ccNodesCompiled],
      Report.Files[I].SourceFile]));

  Shown := Length(Report.Functions);
  if Shown > MAX_COMPILE_FUNCTIONS_DISPLAYED then
    Shown := MAX_COMPILE_FUNCTIONS_DISPLAYED;
  WriteLn;
  WriteLn(Format('Compile Profile Functions (self time, top %d of %d):', [
    Shown, Length(Report.Functions)]));
  WriteLn(Format('  %-12s %-12s %-12s %9s %9s %9s  %-24s %s', [
    'Lex', 'Parse', 'Compile', 'Tokens', 'Nodes', 'Instrs', 'Function',
    'Location']));
  for I := 0 to Shown - 1 do
    WriteLn(Format('  %-12s %-12s %-12s %9d %9d %9d  %-24s %s', [
      FormatDuration(Report.Functions[I].LexNanoseconds),
      FormatDuration(Report.Functions[I].ParseNanoseconds),
      FormatDuration(Report.Functions[I].CompileNanoseconds),
      Report.Functions[I].Tokens,
      Report.Functions[I].Counters[ccNodesCompiled],
      Report.Functions[I].Instructions,
      FunctionDisplayName(Report.Functions[I]),
      FunctionLocation(Report.Functions[I])]));

  WriteLn;
  WriteLn('Compile Profile Work (per node: count / nodes compiled; ' +
    'function: the one with the highest count):');
  WriteLn(Format('  %-28s %14s %10s  %14s %10s  %s', [
    'Counter', 'Total', 'Per node', 'In function', 'Per node', 'Function']));
  for Counter := Low(TGocciaCompileCounter) to High(TGocciaCompileCounter) do
  begin
    if Counter = ccNodesCompiled then
      Continue;
    Top := -1;
    for I := 0 to High(Report.Functions) do
      if (Report.Functions[I].Counters[Counter] > 0) and ((Top < 0) or
         (Report.Functions[I].Counters[Counter] >
          Report.Functions[Top].Counters[Counter])) then
        Top := I;
    if Top < 0 then
      WriteLn(Format('  %-28s %14d %10s', [COMPILE_COUNTER_LABELS[Counter],
        Report.Totals[Counter],
        PerNode(Report.Totals[Counter], Report.Totals[ccNodesCompiled])]))
    else
      WriteLn(Format('  %-28s %14d %10s  %14d %10s  %s %s', [
        COMPILE_COUNTER_LABELS[Counter],
        Report.Totals[Counter],
        PerNode(Report.Totals[Counter], Report.Totals[ccNodesCompiled]),
        Report.Functions[Top].Counters[Counter],
        PerNode(Report.Functions[Top].Counters[Counter],
          Report.Functions[Top].Counters[ccNodesCompiled]),
        FunctionDisplayName(Report.Functions[Top]),
        FunctionLocation(Report.Functions[Top])]));
  end;

  if AProfiler.NodeKindCount = 0 then
    Exit;
  SetLength(KindOrder, AProfiler.NodeKindCount);
  for I := 0 to High(KindOrder) do
    KindOrder[I] := I;
  for I := 1 to High(KindOrder) do
  begin
    Temp := KindOrder[I];
    J := I - 1;
    while (J >= 0) and (AProfiler.NodeKindTotal(KindOrder[J]) <
           AProfiler.NodeKindTotal(Temp)) do
    begin
      KindOrder[J + 1] := KindOrder[J];
      Dec(J);
    end;
    KindOrder[J + 1] := Temp;
  end;
  Shown := Length(KindOrder);
  if Shown > MAX_NODE_KINDS_DISPLAYED then
    Shown := MAX_NODE_KINDS_DISPLAYED;
  WriteLn;
  WriteLn(Format('Compile Profile Nodes by Kind (top %d of %d):', [Shown,
    Length(KindOrder)]));
  for I := 0 to Shown - 1 do
    WriteLn(Format('  %-40s %14d', [AProfiler.NodeKindName(KindOrder[I]),
      AProfiler.NodeKindTotal(KindOrder[I])]));
end;

procedure AppendCounters(var ABuffer: TStringBuffer;
  const ACounters: TGocciaCompileCounters);
var
  Counter: TGocciaCompileCounter;
begin
  ABuffer.AppendChar('{');
  for Counter := Low(TGocciaCompileCounter) to High(TGocciaCompileCounter) do
  begin
    if Counter <> Low(TGocciaCompileCounter) then
      ABuffer.Append(', ');
    ABuffer.Append(Format('"%s": %d', [COMPILE_COUNTER_JSON_NAMES[Counter],
      ACounters[Counter]]));
  end;
  ABuffer.AppendChar('}');
end;

// The kinds counted, as {"Kind": count}, leaving out kinds never counted.
procedure AppendNodeKinds(var ABuffer: TStringBuffer;
  const AProfiler: TGocciaCompileProfiler; const AKinds: array of Int64);
var
  I: Integer;
  First: Boolean;
begin
  ABuffer.AppendChar('{');
  First := True;
  for I := 0 to System.High(AKinds) do
    if AKinds[I] > 0 then
    begin
      if not First then
        ABuffer.Append(', ');
      First := False;
      ABuffer.Append(Format('"%s": %d', [
        EscapeJSONString(AProfiler.NodeKindName(I)), AKinds[I]]));
    end;
  ABuffer.AppendChar('}');
end;

procedure AppendCompileProfileJSON(var ABuffer: TStringBuffer;
  const AProfiler: TGocciaCompileProfiler);
var
  Report: TCompileReport;
  I: Integer;
begin
  Report := BuildCompileReport(AProfiler);
  ABuffer.Append(#10'  "compile": {');

  ABuffer.Append(#10'    "files": [');
  for I := 0 to High(Report.Files) do
  begin
    if I > 0 then
      ABuffer.AppendChar(',');
    ABuffer.Append(Format(#10'      {"sourceFile": "%s", "lexTimeNs": %d, ' +
      '"parseTimeNs": %d, "compileTimeNs": %d, "functions": %d, ' +
      '"tokens": %d, "instructions": %d, "counters": ', [
      EscapeJSONString(Report.Files[I].SourceFile),
      Report.Files[I].LexNanoseconds,
      Report.Files[I].ParseNanoseconds,
      Report.Files[I].CompileNanoseconds,
      Report.Files[I].Functions,
      Report.Files[I].Tokens,
      Report.Files[I].Instructions]));
    AppendCounters(ABuffer, Report.Files[I].Counters);
    ABuffer.Append(', "nodeKinds": ');
    AppendNodeKinds(ABuffer, AProfiler, Report.Files[I].NodeKinds);
    ABuffer.AppendChar('}');
  end;
  ABuffer.Append(#10'    ],');

  ABuffer.Append(#10'    "functions": [');
  for I := 0 to High(Report.Functions) do
  begin
    if I > 0 then
      ABuffer.AppendChar(',');
    ABuffer.Append(Format(#10'      {"name": "%s", "sourceFile": "%s", ' +
      '"line": %d, "column": %d, "lexTimeNs": %d, "parseTimeNs": %d, ' +
      '"compileTimeNs": %d, "tokens": %d, "instructions": %d, "counters": ', [
      EscapeJSONString(FunctionDisplayName(Report.Functions[I])),
      EscapeJSONString(Report.Functions[I].SourceFile),
      Report.Functions[I].Line,
      Report.Functions[I].Column,
      Report.Functions[I].LexNanoseconds,
      Report.Functions[I].ParseNanoseconds,
      Report.Functions[I].CompileNanoseconds,
      Report.Functions[I].Tokens,
      Report.Functions[I].Instructions]));
    AppendCounters(ABuffer, Report.Functions[I].Counters);
    ABuffer.Append(', "nodeKinds": ');
    AppendNodeKinds(ABuffer, AProfiler, Report.Functions[I].NodeKinds);
    ABuffer.AppendChar('}');
  end;
  ABuffer.Append(#10'    ],');

  ABuffer.Append(#10'    "nodeKinds": [');
  if Assigned(AProfiler) then
    for I := 0 to AProfiler.NodeKindCount - 1 do
    begin
      if I > 0 then
        ABuffer.AppendChar(',');
      ABuffer.Append(Format(#10'      {"kind": "%s", "count": %d}', [
        EscapeJSONString(AProfiler.NodeKindName(I)),
        AProfiler.NodeKindTotal(I)]));
    end;
  ABuffer.Append(#10'    ],');

  ABuffer.Append(#10'    "counters": ');
  AppendCounters(ABuffer, Report.Totals);
  ABuffer.Append(#10'  }');
end;

end.
