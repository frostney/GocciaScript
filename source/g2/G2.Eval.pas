unit G2.Eval;
{ Parsing and compiling at run time: the script entry and the Function constructor. }
{$mode delphiunicode}{$H+}
{$I G2.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils, Goccia.SourcePipeline, G2.Core, G2.Bytecode, G2.VM, G2.Compiler;

{ Goal: see CompileProgram (0 isolated, 1 a Script, 2 global eval code) }
{ 16.2.1.7.1 ParseModule + compile (goal 3): the template, and the module info (the caller owns it); Err /
  ErrName on failure }
function ParseAndCompileModule(const Source, FileName: UnicodeString; Id: Integer; var T: PFuncTemplate;
  var Info: TModuleInfo; var Err, ErrName: UnicodeString): Boolean;
function ParseAndCompile(const Source, FileName: UnicodeString; AsExpression: Boolean; var T: PFuncTemplate;
  var Err: UnicodeString; Goal: Integer = 0): Boolean;
procedure InitEval;
{ every function compiled with its code now (On), as for a bytecode file, or lazily again as G2_LAZY says }
procedure SetEagerCompile(On: Boolean); noinline;
{ sets a TGocciaParser's options from the active language profile (G2.Core) }
procedure ApplyProfileOptions(P: TObject; Module: Boolean);
{ the CLIs' front-end options (Goccia.SourcePipeline) for the active profile and the goal }
function ActivePipelineOptions(Module: Boolean): TGocciaSourcePipelineOptions;
{ an entry file under a restricted profile, parsed as the CLIs parse it (SourceType 0 detect, 1 Script, 2
  Module): a Script compiled (goal 1) into T; a Module only reported (IsModule), for G2.Modules to load }
function HostCompileEntry(const Source, FileName: UnicodeString; SourceType: Integer; var T: PFuncTemplate;
  out IsModule: Boolean; var Err: UnicodeString; ReturnLast: Boolean = False): Boolean;

threadvar
  { why the last ParseAndCompile failed: 1 the parser rejected the source (an early error; ParseErrorName is
    the ECMAScript error it stands for), 2 G2's compiler does not support a construct }
  CompileFailKind: Integer;
  ParseErrorName: UnicodeString;
  { where HostCompileEntry's source was rejected (0 when the error has no position) and the fix the parser
    suggests ('' for none) }
  ParseErrorLine, ParseErrorColumn: Integer;
  ParseErrorSuggestion: UnicodeString;
  { the next module compiled keeps its completion value as its function's result (a host that reports an entry
    module's last value); cleared by the compile }
  ModuleReturnLast: Boolean;
  { the warnings of the last HostCompileEntry's parse (warning-unsupported-features: what the parser recovered
    from), in source order }
  EntryWarningCount: Integer;
  EntryWarningMessages, EntryWarningSuggestions: array of UnicodeString;
  EntryWarningLines, EntryWarningColumns: array of Integer;
  { the import declarations and re-exports of the last HostCompileEntry's explicit Script (GocciaScript accepts
    them there, docs/language.md, Entry source type), as the source of a module of only them, each at its own
    line and column; '' when it has none }
  ScriptImportSource: UnicodeString;

implementation

uses
  Classes, Linux, UnixType, TextSemantics, OrderedStringMap, Goccia.Modules.Phases, Goccia.Lexer, Goccia.Parser, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Error, Goccia.SourceSpan, G2.Scan,
  G2.PreParse, G2.Lazy, G2.Snapshot, G2.Artifact, Goccia.Profiler.Compile;

{ the compile profile leaves the host's own code out (a prelude's, named like <goccia:console>) }
procedure ExcludeHostSource(const FileName: UnicodeString);
var P: TGocciaCompileProfiler;
begin
  P := TGocciaCompileProfiler.Instance;
  if (P <> nil) and (Length(FileName) > 2) and (FileName[1] = '<') and (FileName[Length(FileName)] = '>') and
     (Pos(':', FileName) > 0) then
    P.ExcludeFile(FileName);
end;

threadvar
  LazyMode: Integer;   { G2_LAZY: 0 not read yet, 1 off (G2_LAZY=0), 2 on (the default) }

threadvar
  RoundTripMode: Integer;   { G2_ARTIFACT_ROUNDTRIP: 0 not read yet, 1 off, 2 on }

{ G2_ARTIFACT_ROUNDTRIP=1 (a test mode): every Script and module compiled goes through a bytecode file (written,
  read back and verified, G2.Artifact) before it runs, and nothing compiles lazily }
function RoundTripOn: Boolean;
begin
  if RoundTripMode = 0 then
    if GetEnvironmentVariable('G2_ARTIFACT_ROUNDTRIP') = '1' then RoundTripMode := 2 else RoundTripMode := 1;
  Result := RoundTripMode = 2;
end;

{ T (and a module's Info) replaced by what a bytecode file of them reads back as; a failure is fatal: the test
  mode exists to find what the format or the verifier gets wrong }
procedure RoundTrip(var T: PFuncTemplate; var Info: TModuleInfo; const FileName: UnicodeString; Id: Integer);
var A, B: TG2Artifact; Bytes: TBytes; Err: UnicodeString; Unr: Boolean;
begin
  A := Default(TG2Artifact);
  A.SourcePath := FileName;
  A.Profile := CurrentArtifactProfile(Info <> nil, False, Unr);
  A.Unrestricted := Unr;
  A.Top := T;
  A.Info := Info;
  A.ScriptImports := '';
  try
    Bytes := SaveArtifact(A);
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'artifact round trip: ', FileName, ': cannot write: ', E.Message);
      Halt(70);
    end;
  end;
  Err := '';
  if not LoadArtifact(PByte(Bytes), Length(Bytes), FileName, Id, B, Err) then
  begin
    WriteLn(ErrOutput, 'artifact round trip: ', FileName, ': rejected: ', Err);
    Halt(70);
  end;
  if Info <> nil then Info.Free;
  T := B.Top;
  Info := B.Info;
end;

procedure SetEagerCompile(On: Boolean);
begin
  if On then LazyMode := 1 else LazyMode := 0;
end;

function LazyOn: Boolean;
begin
  if LazyMode = 0 then
  begin
    if (GetEnvironmentVariable('G2_LAZY') = '0') or RoundTripOn then LazyMode := 1 else LazyMode := 2;
    { the round trip compiles everything as for a bytecode file }
    if RoundTripOn then SetArtifactCompile(True);
  end;
  { the pre-parser does not apply a restricted profile's early errors to the bodies it skips }
  Result := (LazyMode = 2) and not ActiveProfile.Restricted;
end;

{ the active profile's parser options (G2.Core TLangProfile): an unrestricted profile turns every form on }
procedure ProfileOptions(var Opts: TGocciaParserOptions; Module: Boolean);
var Pr: TLangProfile;
begin
  Pr := ActiveProfile;
  Opts.AutomaticSemicolonInsertion := not Pr.Restricted or Pr.Asi;
  Opts.VarDeclarationsEnabled := not Pr.Restricted or Pr.VarDecl;
  Opts.FunctionDeclarationsEnabled := not Pr.Restricted or Pr.FuncDecl;
  Opts.TraditionalForLoopsEnabled := not Pr.Restricted or Pr.ForLoop;
  Opts.WhileLoopsEnabled := not Pr.Restricted or Pr.WhileLoop;
  Opts.LooseEqualityEnabled := not Pr.Restricted or Pr.LooseEq;
  Opts.NonStrictModeEnabled := not Module and (not Pr.Restricted or Pr.NonStrict);
  Opts.LabelStatementsEnabled := not Pr.Restricted or Pr.Labels;
  Opts.ForInLoopsEnabled := not Pr.Restricted or Pr.ForIn;
  Opts.WarningUnsupportedFeatures := Pr.Restricted and Pr.WarningUnsupported;
  if Module then
  begin
    Opts.ModuleSource := True;
    Opts.ImportMetaAllowed := True;
  end;
end;

procedure ApplyProfileOptions(P: TObject; Module: Boolean);
var Opts: TGocciaParserOptions;
begin
  Opts := TGocciaParser(P).Options;
  ProfileOptions(Opts, Module);
  TGocciaParser(P).ApplyOptions(Opts);
end;

procedure ScriptOptions(P: TGocciaParser);
begin
  ApplyProfileOptions(P, False);
end;

{ LastModuleInfo, taken: through a noinline routine (an inline threadvar load here got FPC 3.3.1's absolute
  relocation, tlsscan.py) }
function TakeLastModuleInfo: TModuleInfo; noinline;
begin
  Result := LastModuleInfo;
  LastModuleInfo := nil;
end;

procedure ModuleOptions(P: TGocciaParser);
begin
  ApplyProfileOptions(P, True);
end;

{ a restricted profile's front end is the CLIs' (Goccia.SourcePipeline): its compatibility flags, the JSX
  preprocessor, and the goal, which stDetect leaves to the source (ADR 0140) }
function HostPipelineOptions(SourceType: TGocciaSourceType): TGocciaSourcePipelineOptions;
var Pr: TLangProfile;
begin
  Pr := ActiveProfile;
  Result := TGocciaSourcePipeline.DefaultOptions;
  Result.Preprocessors := [ppJSX];
  Result.Compatibility := [];
  if Pr.Asi then Include(Result.Compatibility, cfASI);
  if Pr.VarDecl then Include(Result.Compatibility, cfVar);
  if Pr.FuncDecl then Include(Result.Compatibility, cfFunction);
  if Pr.ForLoop then Include(Result.Compatibility, cfTraditionalFor);
  if Pr.WhileLoop then Include(Result.Compatibility, cfWhileLoops);
  if Pr.LooseEq then Include(Result.Compatibility, cfLooseEquality);
  if Pr.NonStrict then Include(Result.Compatibility, cfNonStrictMode);
  if Pr.ArgumentsObject then Include(Result.Compatibility, cfArgumentsObject);
  if Pr.Labels then Include(Result.Compatibility, cfLabel);
  if Pr.ForIn then Include(Result.Compatibility, cfForIn);
  Result.WarningUnsupportedFeatures := Pr.WarningUnsupported;
  Result.SourceType := SourceType;
end;

function ActivePipelineOptions(Module: Boolean): TGocciaSourcePipelineOptions;
begin
  if Module then Result := HostPipelineOptions(stModule) else Result := HostPipelineOptions(stScript);
end;

{ Source parsed by the CLIs' front end; Settled the goal it was parsed with }
function HostParse(const Source, FileName: UnicodeString; SourceType: TGocciaSourceType;
  out Settled: TGocciaSourceType): TGocciaProgram;
var Lines: TStringList; R: TGocciaSourcePipelineResult; I: Integer;
begin
  EntryWarningCount := 0;
  Lines := CreateFileTextLines(NormalizeNewlinesToLF(UTF8Encode(Source)));
  try
    R := TGocciaSourcePipeline.Parse(Lines, UTF8Encode(FileName), HostPipelineOptions(SourceType));
    try
      EntryWarningCount := R.WarningCount;
      SetLength(EntryWarningMessages, R.WarningCount);
      SetLength(EntryWarningSuggestions, R.WarningCount);
      SetLength(EntryWarningLines, R.WarningCount);
      SetLength(EntryWarningColumns, R.WarningCount);
      for I := 0 to R.WarningCount - 1 do
      begin
        EntryWarningMessages[I] := UTF8Decode(R.Warnings[I].Message);
        EntryWarningSuggestions[I] := UTF8Decode(R.Warnings[I].Suggestion);
        EntryWarningLines[I] := R.Warnings[I].Line;
        EntryWarningColumns[I] := R.Warnings[I].Column;
      end;
      Settled := R.SourceType;
      Result := R.TakeProgramNode;
    finally
      R.Free;
    end;
  finally
    Lines.Free;
  end;
end;

type
  { a for await statement outside every function (a function, method, accessor or class body is not entered) }
  TTopForAwaitScan = class(TGocciaASTChildVisitor)
  public
    Found: Boolean;
    procedure VisitChild(const ANode: TGocciaASTNode); override;
    procedure EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); override;
    procedure LeavePart; override;
  end;

procedure TTopForAwaitScan.VisitChild(const ANode: TGocciaASTNode);
begin
  if (ANode = nil) or Found then Exit;
  if (ANode is TGocciaFunctionExpression) or (ANode is TGocciaArrowFunctionExpression) or
     (ANode is TGocciaObjectMethodDefinition) or (ANode is TGocciaClassExpression) or
     (ANode is TGocciaClassDeclaration) or (ANode is TGocciaFunctionDeclaration) or
     (ANode is TGocciaGetterExpression) or (ANode is TGocciaSetterExpression) then Exit;
  if ANode is TGocciaForAwaitOfStatement then
  begin
    Found := True;
    Exit;
  end;
  ANode.ForEachChild(Self);
end;

procedure TTopForAwaitScan.EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
  const AFlags: TGocciaASTPartFlags);
begin
end;

procedure TTopForAwaitScan.LeavePart;
begin
end;

{ a top-level for await statement in Prog: top-level await, which selects Module for a detected entry
  (docs/language.md, Entry source type) }
function HasTopLevelForAwait(Prog: TGocciaProgram): Boolean;
var Sc: TTopForAwaitScan; I: Integer;
begin
  Sc := TTopForAwaitScan.Create;
  try
    for I := 0 to Prog.Body.Count - 1 do Sc.VisitChild(Prog.Body[I]);
    Result := Sc.Found;
  finally
    Sc.Free;
  end;
end;

{ the export declarations of a Script's top level replaced by the declarations they export, and export lists
  dropped. The program's list owns its statements: the export nodes taken out are not freed, since each owns the
  declaration that now stands in its place (one entry's few nodes, once a run) }
procedure ScriptExportsAsDeclarations(Prog: TGocciaProgram);
var I: Integer; N: TGocciaStatement; D: TGocciaStatement;
begin
  Prog.Body.OwnsObjects := False;
  try
    for I := Prog.Body.Count - 1 downto 0 do
    begin
      N := Prog.Body[I];
      D := nil;
      if N is TGocciaExportVariableDeclaration then D := TGocciaExportVariableDeclaration(N).Declaration
      else if N is TGocciaExportDestructuringDeclaration then D := TGocciaExportDestructuringDeclaration(N).Declaration
      else if N is TGocciaExportFunctionDeclaration then D := TGocciaExportFunctionDeclaration(N).Declaration
      else if N is TGocciaExportClassDeclaration then D := TGocciaExportClassDeclaration(N).Declaration
      else if N is TGocciaExportEnumDeclaration then D := TGocciaExportEnumDeclaration(N).Declaration
      else if N is TGocciaExportDeclaration then
      begin
        Prog.Body.Delete(I);
        Continue;
      end;
      if D <> nil then Prog.Body[I] := D;
    end;
  finally
    Prog.Body.OwnsObjects := True;
  end;
end;

{ S as a JavaScript string literal }
function QuoteJS(const S: UnicodeString): UnicodeString;
var I: Integer;
begin
  Result := '"';
  for I := 1 to Length(S) do
    case S[I] of
      '"', '\': Result := Result + '\' + S[I];
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #$2028: Result := Result + ' ';
      #$2029: Result := Result + ' ';
    else
      Result := Result + S[I];
    end;
  Result := Result + '"';
end;

{ the import declarations and re-exports of a Script's top level taken out of it, and written as the source of a
  module of only them (ScriptImportSource), each where it stood (a link error is located at its declaration); the
  nodes taken out are freed }
function ScriptImportsAsModule(Prog: TGocciaProgram): UnicodeString;
var I, Line, Col, AtLine: Integer; N: TGocciaStatement; Decl, From: UnicodeString; Pair: TStringStringMap.TKeyValuePair;
  Found: Boolean;
begin
  Result := '';
  AtLine := 1;
  Found := False;
  for I := 0 to Prog.Body.Count - 1 do
  begin
    N := Prog.Body[I];
    if N is TGocciaImportDeclaration then
      with TGocciaImportDeclaration(N) do
      begin
        From := QuoteJS(ModulePath);
        if AttributeType <> '' then From := From + ' with { type: ' + QuoteJS(AttributeType) + ' }';
        Decl := '';
        if Phase = icpSource then
        begin
          for Pair in Imports do Decl := Decl + 'import source ' + UnicodeString(Pair.Key) + ' from ' + From + ';';
        end
        else if Phase = icpDefer then
          Decl := 'import defer * as ' + NamespaceName + ' from ' + From + ';'
        else
        begin
          if NamespaceName <> '' then Decl := 'import * as ' + NamespaceName + ' from ' + From + ';';
          if Imports.Count > 0 then
          begin
            Decl := Decl + 'import { ';
            for Pair in Imports do Decl := Decl + QuoteJS(Pair.Value) + ' as ' + UnicodeString(Pair.Key) + ', ';
            Decl := Decl + '} from ' + From + ';';
          end;
          if Decl = '' then Decl := 'import ' + From + ';';
        end;
      end
    else if N is TGocciaReExportDeclaration then
      with TGocciaReExportDeclaration(N) do
      begin
        From := QuoteJS(ModulePath);
        if AttributeType <> '' then From := From + ' with { type: ' + QuoteJS(AttributeType) + ' }';
        if IsStarExport and (NamespaceName = '') then Decl := 'export * from ' + From + ';'
        else if IsStarExport then Decl := 'export * as ' + QuoteJS(NamespaceName) + ' from ' + From + ';'
        else
        begin
          Decl := 'export { ';
          for Pair in ExportsTable do Decl := Decl + QuoteJS(Pair.Value) + ' as ' + QuoteJS(Pair.Key) + ', ';
          Decl := Decl + '} from ' + From + ';';
        end;
      end
    else Continue;
    Line := N.Line;
    Col := N.Column;
    if Line < AtLine then Line := AtLine;
    while AtLine < Line do
    begin
      Result := Result + #10;
      Inc(AtLine);
    end;
    if Col > 1 then Result := Result + StringOfChar(' ', Col - 1);
    Result := Result + Decl + #10;
    Inc(AtLine);
    Found := True;
  end;
  if not Found then Exit;
  for I := Prog.Body.Count - 1 downto 0 do
    if (Prog.Body[I] is TGocciaImportDeclaration) or (Prog.Body[I] is TGocciaReExportDeclaration) then
      Prog.Body.Delete(I);
end;

function HostCompileEntry(const Source, FileName: UnicodeString; SourceType: Integer; var T: PFuncTemplate;
  out IsModule: Boolean; var Err: UnicodeString; ReturnLast: Boolean): Boolean;
var Prog: TGocciaProgram; Goal: TGocciaSourceType; NoInfo: TModuleInfo;
begin
  Result := False;
  IsModule := False;
  ScriptImportSource := '';
  Prog := nil;
  try
    try
      case SourceType of
        1: Goal := stScript;
        2: Goal := stModule;
      else
        Goal := stDetect;
      end;
      Prog := HostParse(Source, FileName, Goal, Goal);
      if Goal = stModule then
      begin
        IsModule := True;
        Exit(True);
      end;
      { an explicit Script keeps GocciaScript's acceptance of export declarations (docs/language.md, Entry source
        type): each declares its binding as it would without export, and an export list declares nothing }
      if SourceType = 1 then
      begin
        ScriptImportSource := ScriptImportsAsModule(Prog);
        ScriptExportsAsDeclarations(Prog);
      end;
      { detection reads a top-level for await as Script; it is top-level await, so the entry is a Module }
      if (SourceType = 0) and HasTopLevelForAwait(Prog) then
      begin
        IsModule := True;
        Exit(True);
      end;
      T := CompileProgram(Prog, FileName, ReturnLast, 1);
      if RoundTripOn then
      begin
        NoInfo := nil;
        RoundTrip(T, NoInfo, FileName, 0);
      end;
      Result := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is TGocciaReferenceError then ParseErrorName := 'ReferenceError' else ParseErrorName := 'SyntaxError';
        ParseErrorLine := 0;
        ParseErrorColumn := 0;
        ParseErrorSuggestion := '';
        if E is TGocciaError then
        begin
          ParseErrorLine := TGocciaError(E).Line;
          ParseErrorColumn := TGocciaError(E).Column;
          ParseErrorSuggestion := UnicodeString(TGocciaError(E).Suggestion);
        end;
      end;
    end;
  finally
    Prog.Free;
  end;
end;

{ a module with lazy functions: as LazyParseAndCompile, goal 3 }
function LazyParseAndCompileModule(const Source, FileName: UnicodeString; Id: Integer; var T: PFuncTemplate;
  var Info: TModuleInfo; var Err, ErrName: UnicodeString; out Ok: Boolean): Boolean;
var PP: TPreParser; LS: TLazySource; L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram;
begin
  Result := False;
  Ok := False;
  LS := nil;
  PP := TPreParser.Create(PWideChar(Source), Length(Source), pgModule, True);
  try
    try
      PP.ParseProgram;
      LS := BuildLazySource(Source, FileName, PP);
    except
      on EScanError do Exit;
    end;
  finally
    PP.Free;
  end;
  if LS = nil then Exit;
  Result := True;
  LS.IsModule := True;
  LS.ModuleId := Id;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(LS.Text, FileName);
      LS.Coords := L.SourceCoordinates;
      LS.CoordsRef := LS.Coords;
      L.StandardIdentifiers := True;
      P := TGocciaParser.CreateFromLexer(L, FileName, nil);
      ModuleOptions(P);
      P.SetFunctionBodySkipper(@LazyBodySkipper, LS);
      P.SetEarlyErrorsChecked(True);
      Prog := P.Parse;
      L.Tokens.Clear;
      CompilingModuleId := Id;
      CurLazySource := LS;
      try
        T := CompileProgram(Prog, FileName, ModuleReturnLast, 3);
      finally
        CurLazySource := nil;
      end;
      Info := TakeLastModuleInfo;
      Ok := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is TGocciaReferenceError then ErrName := 'ReferenceError' else ErrName := 'SyntaxError';
        { where in the module, for the host's report of an import whose module did not parse }
        ParseErrorLine := 0;
        ParseErrorColumn := 0;
        ParseErrorSuggestion := '';
        if E is TGocciaError then
        begin
          ParseErrorLine := TGocciaError(E).Line;
          ParseErrorColumn := TGocciaError(E).Column;
          ParseErrorSuggestion := UnicodeString(TGocciaError(E).Suggestion);
        end;
      end;
    end;
  finally
    CompilingModuleId := 0;
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

{ a Script with lazy functions (G2.Lazy): pre-parsed, then parsed with the eligible bodies skipped and compiled
  with stubs for them. False when the source goes the eager way (a syntax error, whose message the eager parse
  gives, or nothing to defer); otherwise Ok and Err are the result }
function LazyParseAndCompile(const Source, FileName: UnicodeString; var T: PFuncTemplate; var Err: UnicodeString;
  out Ok: Boolean): Boolean;
var PP: TPreParser; LS: TLazySource; L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram;
begin
  Result := False;
  Ok := False;
  LS := nil;
  PP := TPreParser.Create(PWideChar(Source), Length(Source), pgScript, False);
  try
    try
      PP.ParseProgram;
      LS := BuildLazySource(Source, FileName, PP);
    except
      on EScanError do Exit;
    end;
  finally
    PP.Free;
  end;
  if LS = nil then Exit;
  Result := True;
  CompileFailKind := 1;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(LS.Text, FileName);
      LS.Coords := L.SourceCoordinates;
      LS.CoordsRef := LS.Coords;
      L.HTMLComments := True;
      L.StandardIdentifiers := True;
      P := TGocciaParser.CreateFromLexer(L, FileName, nil);
      ScriptOptions(P);
      P.SetFunctionBodySkipper(@LazyBodySkipper, LS);
      P.SetEarlyErrorsChecked(True);
      Prog := P.Parse;
      L.Tokens.Clear;
      CompileFailKind := 2;
      CurLazySource := LS;
      try
        T := CompileProgram(Prog, FileName, False, 1);
      finally
        CurLazySource := nil;
      end;
      CompileFailKind := 0;
      Ok := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is EEarlySyntaxError then CompileFailKind := 1;
        if E is TGocciaReferenceError then ParseErrorName := 'ReferenceError' else ParseErrorName := 'SyntaxError';
      end;
    end;
  finally
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

type
  { the function node whose body block starts at Target (a lazily compiled method, in its container's parse) }
  TBodyFinder = class(TGocciaASTChildVisitor)
  public
    Target: Integer;
    Found: Boolean;
    Params: TGocciaParameterArray;
    Body: TGocciaASTNode;
    procedure VisitChild(const ANode: TGocciaASTNode); override;
    procedure EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); override;
    procedure LeavePart; override;
  end;

procedure TBodyFinder.VisitChild(const ANode: TGocciaASTNode);
var B: TGocciaASTNode;
begin
  if (ANode = nil) or Found then Exit;
  B := nil;
  if ANode is TGocciaFunctionExpression then B := TGocciaFunctionExpression(ANode).Body
  else if ANode is TGocciaArrowFunctionExpression then B := TGocciaArrowFunctionExpression(ANode).Body
  else if ANode is TGocciaGetterExpression then B := TGocciaGetterExpression(ANode).Body
  else if ANode is TGocciaSetterExpression then B := TGocciaSetterExpression(ANode).Body
  else if ANode is TGocciaClassMethod then B := TGocciaClassMethod(ANode).Body;
  if (B <> nil) and (B is TGocciaBlockStatement) and (B.Span.StartOffset = Target) then
  begin
    Found := True;
    Body := B;
    if ANode is TGocciaFunctionExpression then Params := TGocciaFunctionExpression(ANode).Parameters
    else if ANode is TGocciaArrowFunctionExpression then Params := TGocciaArrowFunctionExpression(ANode).Parameters
    else if ANode is TGocciaSetterExpression then Params := TGocciaSetterExpression(ANode).Parameters
    else if ANode is TGocciaClassMethod then Params := TGocciaClassMethod(ANode).Parameters
    else Params := nil;
    Exit;
  end;
  ANode.ForEachChild(Self);
end;

procedure TBodyFinder.EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
  const AFlags: TGocciaASTPartFlags);
begin
end;

procedure TBodyFinder.LeavePart;
begin
end;

{ LazyCompileHook: a lazy template's function, parsed from its own source range and compiled, moved into the
  template (its closures keep the stub's upvalues, name and source text) }
var
  { G2_LAZY_DEBUG set: 1, not: 0, not read yet: -1 }
  LazyDebug: Integer = -1;

function LazyCompileIn(T: PFuncTemplate): Boolean; forward;

{ lazy sources exist only under the unrestricted profile (LazyOn): their functions compile under it, whatever
  profile is active when they are first called }
function LazyCompile(T: PFuncTemplate): Boolean;
var Saved: TLangProfile;
begin
  Saved := ActiveProfile;
  SetActiveProfile(Default(TLangProfile));
  try
    Result := LazyCompileIn(T);
  finally
    SetActiveProfile(Saved);
  end;
end;

function LazyCompileIn(T: PFuncTemplate): Boolean;
var St: PLazyStub; LS, PrevSrc: TLazySource; R: ^TLazyRec; L: TGocciaLexer; P: TGocciaParser; E: TGocciaExpression;
  NT: PFuncTemplate; SaveUp: array of TUpvalDesc; SaveName: PStr; SaveRange: TGocciaLazySourceText;
  Finder: TBodyFinder; IsMethod: Boolean; PrevModule: Integer;
  Arena, PrevArena: TGocciaASTArena; Msg: UnicodeString; TS0, TS1, TSa, TSb: TTimeSpec; RStart, RStop: Integer;
begin
  clock_gettime(CLOCK_MONOTONIC, @TS0);
  St := PLazyStub(T^.Lazy);
  if St^.Src = nil then Exit(MaterializeSnapshotTemplate(T));
  LS := St^.Src;
  R := @LS.Recs[St^.Id];
  if LazyDebug < 0 then
    if GetEnvironmentVariable('G2_LAZY_DEBUG') <> '' then LazyDebug := 1 else LazyDebug := 0;
  if LazyDebug = 1 then
    WriteLn(ErrOutput, 'lazy compile ', St^.CompileName, ' (', LS.FileName, ' ', R^.Start, '..', R^.Stop, ', module ',
      LS.IsModule, ', method ', R^.CStart >= 0, ')');
  L := nil; P := nil; Finder := nil;
  Msg := '';
  IsMethod := R^.CStart >= 0;
  Arena := TGocciaASTArena.Create;
  try
    try
      { a method is parsed with its object literal or class, the other bodies there skipped }
      if IsMethod then begin RStart := R^.CStart; RStop := R^.CStop; end
      else begin RStart := R^.Start; RStop := R^.Stop; end;
      if (LS.Lexer <> nil) and not LS.LexerBusy then
      begin
        L := TGocciaLexer(LS.Lexer);
        L.Restart(RStart, RStop);
      end
      else
      begin
        L := TGocciaLexer.Create(LS.Text, LS.FileName, LS.Coords);
        L.HTMLComments := not LS.IsModule;
        L.StandardIdentifiers := True;
        L.SetRange(RStart, RStop);
        if LS.Lexer = nil then LS.Lexer := L;
      end;
      if L = LS.Lexer then LS.LexerBusy := True;
      P := TGocciaParser.CreateFromLexer(L, LS.FileName, nil);
      if LS.IsModule then ModuleOptions(P) else ScriptOptions(P);
      if IsMethod then
      begin
        { the container's own context }
        P.SetStrictMode(ffCtxStrict in R^.Flags);
        P.SetFunctionContext(ffCtxAwait in R^.Flags, ffCtxYield in R^.Flags);
      end
      else P.SetStrictMode(St^.IsStrict);
      P.SetFunctionBodySkipper(@LazyBodySkipper, LS);
      P.SetEarlyErrorsChecked(True);
      LS.Compiling := St^.Id;
      LS.SkipOutside := IsMethod;
      LS.TargetStart := R^.Start;
      LS.TargetStop := R^.Stop;
      PrevArena := ActivateASTArena(Arena);
      clock_gettime(CLOCK_MONOTONIC, @TSa);
      try
        E := P.ParseExpressionUnchecked;
      finally
        RestoreASTArena(PrevArena);
        LS.Compiling := -1;
        LS.SkipOutside := False;
      end;
      clock_gettime(CLOCK_MONOTONIC, @TSb);
      Inc(LazySetupNs, (Int64(TSa.tv_sec) - TS0.tv_sec) * 1000000000 + (TSa.tv_nsec - TS0.tv_nsec));
      Inc(LazyParseNs, (Int64(TSb.tv_sec) - TSa.tv_sec) * 1000000000 + (TSb.tv_nsec - TSa.tv_nsec));
      Inc(LazyTokens, L.Tokens.Count);
      Inc(LazyChars, R^.Stop - R^.Start);
      Finder := TBodyFinder.Create;
      Finder.Target := R^.Body;
      Finder.VisitChild(E);
      if not Finder.Found or not P.AtEnd then
        raise Exception.Create('lazy compile: the function is not in its source range (' + E.ClassName + ' found ' +
          BoolToStr(Finder.Found, True) + ' at end ' + BoolToStr(P.AtEnd, True) + ')');
      PrevSrc := CurLazySource;
      PrevModule := CompilingModuleId;
      CurLazySource := LS;
      if LS.IsModule then CompilingModuleId := LS.ModuleId else CompilingModuleId := 0;
      try
        NT := CompileLazyFunction(Finder.Params, Finder.Body, St, LS.FileName);
      finally
        CurLazySource := PrevSrc;
        CompilingModuleId := PrevModule;
      end;
      SaveUp := T^.Upvals;
      SaveName := T^.Name;
      SaveRange := T^.SourceRange;
      T^ := NT^;
      T^.Upvals := SaveUp;
      T^.Name := SaveName;
      T^.SourceRange := SaveRange;
      T^.Lazy := nil;
      FinishTemplate(T);
      { NT stays registered, emptied: T owns its arrays and IC memory now }
      Finalize(NT^);
      FillChar(NT^, SizeOf(TFuncTemplate), 0);
      Dispose(St);
    except
      on Ex: Exception do Msg := UnicodeString(Ex.Message);
    end;
  finally
    Finder.Free;
    P.Free;
    if L = LS.Lexer then
    begin
      LS.LexerBusy := False;
      L.Tokens.Reset;
    end
    else L.Free;
    Arena.Free;
  end;
  clock_gettime(CLOCK_MONOTONIC, @TS1);
  Inc(LazyCompileCount);
  Inc(LazyCompileNs, (Int64(TS1.tv_sec) - TS0.tv_sec) * 1000000000 + (TS1.tv_nsec - TS0.tv_nsec));
  if Msg <> '' then Exit(ThrowError('SyntaxError', Msg));
  Result := True;
end;

function ParseAndCompileModule(const Source, FileName: UnicodeString; Id: Integer; var T: PFuncTemplate;
  var Info: TModuleInfo; var Err, ErrName: UnicodeString): Boolean;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Settled: TGocciaSourceType;
begin
  Info := nil;
  ExcludeHostSource(FileName);
  if LazyOn and LazyParseAndCompileModule(Source, FileName, Id, T, Info, Err, ErrName, Result) then Exit;
  Result := False;
  L := nil; P := nil; Prog := nil;
  try
    try
      if ActiveProfile.Restricted then Prog := HostParse(Source, FileName, stModule, Settled)
      else
      begin
        L := TGocciaLexer.Create(Source, FileName);
        L.StandardIdentifiers := True;
        P := TGocciaParser.CreateFromLexer(L, FileName, nil);
        ModuleOptions(P);
        Prog := P.Parse;
        L.Tokens.Clear;
      end;
      CompilingModuleId := Id;
      T := CompileProgram(Prog, FileName, ModuleReturnLast, 3);
      Info := LastModuleInfo;
      LastModuleInfo := nil;
      if RoundTripOn then RoundTrip(T, Info, FileName, Id);
      Result := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is TGocciaReferenceError then ErrName := 'ReferenceError' else ErrName := 'SyntaxError';
        { where in the module, for the host's report of an import whose module did not parse }
        ParseErrorLine := 0;
        ParseErrorColumn := 0;
        ParseErrorSuggestion := '';
        if E is TGocciaError then
        begin
          ParseErrorLine := TGocciaError(E).Line;
          ParseErrorColumn := TGocciaError(E).Column;
          ParseErrorSuggestion := UnicodeString(TGocciaError(E).Suggestion);
        end;
      end;
    end;
  finally
    CompilingModuleId := 0;
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

function ParseAndCompile(const Source, FileName: UnicodeString; AsExpression: Boolean; var T: PFuncTemplate;
  var Err: UnicodeString; Goal: Integer): Boolean;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Settled: TGocciaSourceType; NoInfo: TModuleInfo;
begin
  ExcludeHostSource(FileName);
  if (Goal = 1) and not AsExpression and LazyOn and LazyParseAndCompile(Source, FileName, T, Err, Result) then Exit;
  Result := False;
  CompileFailKind := 1;
  L := nil; P := nil; Prog := nil;
  try
    try
      if (Goal = 1) and not AsExpression and ActiveProfile.Restricted then
        Prog := HostParse(Source, FileName, stScript, Settled)
      else
      begin
        L := TGocciaLexer.Create(Source, FileName);
        L.HTMLComments := True;   { B.1.1: Script, eval and Function code }
        L.StandardIdentifiers := True;
        P := TGocciaParser.CreateFromLexer(L, FileName, nil);   { source lines: built only if an error needs them }
        ScriptOptions(P);
        CompileFailKind := 1;
        { goal 4 (a direct eval in function code): the private names of its caller's classes are visible; the
          compiler checks each against the caller's bindings (AllPrivateIdentifiersValid) }
        if Goal = 4 then Prog := P.ParseUnchecked else Prog := P.Parse;
        L.Tokens.Clear;
      end;
      CompileFailKind := 2;
      T := CompileProgram(Prog, FileName, AsExpression, Goal);
      if (Goal = 1) and not AsExpression and RoundTripOn then
      begin
        NoInfo := nil;
        RoundTrip(T, NoInfo, FileName, 0);
      end;
      CompileFailKind := 0;
      Result := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is EEarlySyntaxError then CompileFailKind := 1;
        if E is TGocciaReferenceError then ParseErrorName := 'ReferenceError' else ParseErrorName := 'SyntaxError';
      end;
    end;
  finally
    { a template keeps nothing of these: the AST, then the parser, then the lexer, which owns the source
      coordinates the AST's spans point at. Before, every eval and Function call leaked all three. }
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

{ whether parameter text could open something (a comment, string, template, regular expression, bracket or
  HTML-like comment) that the body text closes: only then do the parameters need a parse of their own }
function ParamsMayEscape(const P: UnicodeString): Boolean;
var I: Integer;
begin
  for I := 1 to Length(P) do
    case P[I] of
      '/', '`', '''', '"', '(', ')', '[', ']', '{', '}', '<', '-', '\': Exit(True);
    end;
  Result := False;
end;

{ CreateDynamicFunction's source, the function expression it wraps the parameters and body in, compiled; the
  wrapper's function keyword is the engine's, so a restricted profile parses it with compat-function on. As in
  GocciaScript, the function is non-strict unless its body says "use strict" (this is then the global object
  for a call without a receiver), whatever the profile makes other code. }
function CompileDynamic(const Src: UnicodeString; var T: PFuncTemplate; var Err: UnicodeString): Boolean;
var Saved, Pr: TLangProfile;
begin
  Saved := ActiveProfile;
  Pr := Saved;
  Pr.FuncDecl := True;
  Pr.NonStrict := True;
  SetActiveProfile(Pr);
  DynFuncAnonymous := True;
  try
    Result := ParseAndCompile(Src, '<Function>', True, T, Err);
  finally
    DynFuncAnonymous := False;
    SetActiveProfile(Saved);
  end;
end;

{ S with each lone surrogate as U+FFFD, as the source text a parse keeps (UTF-8) holds it }
function LoneSurrogatesReplaced(const S: UnicodeString): UnicodeString;
var I: Integer; C: Word;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do
  begin
    C := Ord(Result[I]);
    if (C >= $D800) and (C <= $DBFF) and (I < Length(Result)) and (Ord(Result[I + 1]) >= $DC00) and
       (Ord(Result[I + 1]) <= $DFFF) then
      Inc(I, 2)
    else
    begin
      if (C >= $D800) and (C <= $DFFF) then Result[I] := WideChar($FFFD);
      Inc(I);
    end;
  end;
end;

{ 20.2.1.1.1 CreateDynamicFunction: Function, GeneratorFunction, AsyncFunction and AsyncGeneratorFunction
  (NativeData 0-3) }
function Function_Ctor(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
const
  Heads: array[0..3] of UnicodeString = ('(function anonymous(', '(function* anonymous(', '(async function anonymous(',
    '(async function* anonymous(');
  CtorNames: array[0..3] of UnicodeString = ('Function', 'GeneratorFunction', 'AsyncFunction',
    'AsyncGeneratorFunction');
var I: Integer; Params, Body, Src, Err: UnicodeString; S: TValue; T: PFuncTemplate; Clo: TValue; Kind: Integer;
  Prof: TGocciaCompileProfiler; Ident, Err2: UnicodeString; Depth, Depth2: Integer; T2: PFuncTemplate;
begin
  Kind := NativeData;
  { a restricted profile creates functions from strings only with unsafe-function-constructor; the host audits
    the decision either way }
  if not ProfileFunctionCtor then
  begin
    if not AuditDecision(akFunctionConstructor, False, CtorNames[Kind], 'dynamic code generation is disabled') then
      Exit(False);
    Exit(ThrowError('TypeError', 'Dynamic code generation is disabled. Pass --unsafe-function-constructor to enable the Function constructor'));
  end;
  if not AuditDecision(akFunctionConstructor, True, CtorNames[Kind], 'dynamic code generation is enabled') then
    Exit(False);
  Params := '';
  Body := '';
  for I := 1 to Argc do
  begin
    if not ToStringV(Args[I], S) then Exit(False);
    if I = Argc then Body := StrToUnicode(StrOf(S))
    else
    begin
      if Params <> '' then Params := Params + ',';
      Params := Params + StrToUnicode(StrOf(S));
    end;
  end;
  { the compile profile: the source is a unit of its own (its kind, its parameters length-prefixed, its body: no
    split of the same text between them makes another), and so are its parameters and its body parsed on their
    own, as GocciaScript validates them (their results are not used: the whole source decides) }
  Prof := TGocciaCompileProfiler.Instance;
  Depth := -1;
  if Prof <> nil then
  begin
    Ident := IntToStr(Kind) + ':' + IntToStr(Length(Params)) + ':' + Params + Body;
    Depth := Prof.UnitDepth;
    Prof.BeginUnit('<Function>', Ident);
    Depth2 := Prof.UnitDepth;
    Prof.BeginUnit('<Function:params>', Ident);
    CompileDynamic(Heads[Kind] + Params + #10') {'#10'})', T2, Err2);
    Prof.EndUnitTo(Depth2);
    Prof.BeginUnit('<Function:body>', Ident);
    CompileDynamic(Heads[Kind] + #10') {'#10 + Body + #10'})', T2, Err2);
    Prof.EndUnitTo(Depth2);
    Pending := VUndefined;
  end;
  try
    { 20.2.1.1.1 steps 15-18: the parameters parse as FormalParameters on their own (a comment, template or
      function they open does not end in the body) }
    if ParamsMayEscape(Params) then
    begin
      if not CompileDynamic(Heads[Kind] + Params + #10') {'#10'})', T, Err) then Exit(ThrowError('SyntaxError', Err));
    end;
    Src := Heads[Kind] + Params + #10') {'#10 + Body + #10'})';
    if not CompileDynamic(Src, T, Err) then Exit(ThrowError('SyntaxError', Err));
  finally
    if Prof <> nil then Prof.EndUnitTo(Depth);
  end;
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, VUndefined, nil, 0, Res);
  { and the body as a FunctionBody: the function the source makes ends at the brace added after it (the source
  range is kept as UTF-8, where a lone surrogate became U+FFFD) }
  if Result and IsObject(Res) and (ObjOf(Res)^.OKind = okFunction) and
     (UTF8Decode(PFuncTemplate(ObjOf(Res)^.Native)^.SourceRange.Get) <>
      LoneSurrogatesReplaced(Copy(Src, 2, Length(Src) - 2))) then
    Result := ThrowError('SyntaxError', 'Function body is not a FunctionBody');
end;

function AsyncIter_Self(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Args[0];
  Result := True;
end;

procedure InitEval;
var F: TValue;
  function Proto(Parent: TValue; var Slot: TValue): TValue;
  begin
    AddRoot(@Slot);
    Slot := MkPtr(NewPlainObject(Parent, 8));
    Result := Slot;
  end;
  procedure Ctor(const Name: UnicodeString; Kind: Integer; ProtoV: TValue);
  var C: TValue;
  begin
    PushRoot(ProtoV);
    C := NewNativeFunctionData(Function_Ctor, Name, 1, Kind);
    PushRoot(C);
    SetProto(ObjOf(C), RootAt(TempTop - 3));   { [[Prototype]]: %Function% }
    DefineOwn(RootAt(TempTop - 1), AtomPrototype, RootAt(TempTop - 2), 0);
    DefineOwn(RootAt(TempTop - 2), AtomConstructor, RootAt(TempTop - 1), pfConfigurable);
    PopRoots(2);
  end;
  procedure Tag(O: TValue; const T: UnicodeString);
  begin
    DefineOwn(O, SymKey(SymToStringTagW), MkPtr(AtomOf(T)), pfConfigurable);
  end;
begin
  { the round trip (G2_ARTIFACT_ROUNDTRIP) compiles everything as for a bytecode file }
  SetArtifactCompile(RoundTripOn);
  LazyCompileHook := @LazyCompile;
  PreParseRegExpCheck := RegExpValidateHook;
  F := NewNativeFunction(Function_Ctor, 'Function', 1);
  PushRoot(F);
  DefineOwn(RootAt(TempTop - 1), AtomPrototype, FunctionProto, 0);
  DefineOwn(FunctionProto, AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf('Function'), RootAt(TempTop - 1), pfHidden);
  { 27.3 - 27.7: the function and iterator intrinsics of generators and async code }
  Proto(FunctionProto, GenFunctionProto);
  Proto(FunctionProto, AsyncFunctionProto);
  Proto(FunctionProto, AsyncGenFunctionProto);
  Proto(ObjectProto, AsyncIteratorProto);
  Proto(AsyncIteratorProto, AsyncGeneratorProto);
  Proto(AsyncIteratorProto, AsyncFromSyncIterProto);
  Ctor('GeneratorFunction', 1, GenFunctionProto);
  Ctor('AsyncFunction', 2, AsyncFunctionProto);
  Ctor('AsyncGeneratorFunction', 3, AsyncGenFunctionProto);
  DefineOwn(GenFunctionProto, AtomPrototype, GeneratorProto, pfConfigurable);
  DefineOwn(GeneratorProto, AtomConstructor, GenFunctionProto, pfConfigurable);
  DefineOwn(AsyncGenFunctionProto, AtomPrototype, AsyncGeneratorProto, pfConfigurable);
  DefineOwn(AsyncGeneratorProto, AtomConstructor, AsyncGenFunctionProto, pfConfigurable);
  Tag(GenFunctionProto, 'GeneratorFunction');
  Tag(AsyncFunctionProto, 'AsyncFunction');
  Tag(AsyncGenFunctionProto, 'AsyncGeneratorFunction');
  Tag(AsyncGeneratorProto, 'AsyncGenerator');
  F := NewNativeFunction(AsyncIter_Self, '[Symbol.asyncIterator]', 0);
  DefineOwn(AsyncIteratorProto, SymKey(SymAsyncIterator), F, pfHidden);
  GenFunctionShape := FnShapeFor(GenFunctionProto);
  AsyncFunctionShape := FnShapeFor(AsyncFunctionProto);
  AsyncGenFunctionShape := FnShapeFor(AsyncGenFunctionProto);
  PopRoots(1);
end;

end.
