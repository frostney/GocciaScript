unit Goccia.Compiler.Scope;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.Compiler.ConstantValue,
  Goccia.Modules,
  Goccia.Profiler.Compile;

type
  TGocciaCompilerVariableKind = (cvkLocal, cvkUpvalue, cvkGlobal);
  TGocciaExpressionTypeMemo = TDictionary<TGocciaExpression, TGocciaLocalType>;
  TGocciaCompilerExportNameArray = array of string;

  TGocciaCompilerLocal = record
    Name: string;
    Slot: UInt16;
    Depth: Integer;
    IsCaptured: Boolean;
    IsConst: Boolean;
    // True once the code that initializes this binding has been compiled: its
    // declaration, the for...of or counted-loop step that fills it, or the
    // parameter preamble. A read compiled after that point cannot observe the
    // TDZ hole. Whether the read may also take the register as an operand is
    // decided where the operand is compiled; see TryResolveSettledLocalName.
    IsInitialized: Boolean;
    // True when the binding's declaration need not set its TDZ hole: the
    // block that declares it set the hole on entry, and until the declaration
    // runs nothing can store to it, or, for a leading binding of a function
    // body, nothing can observe the binding before its declaration completes
    // (LeadingLexicalsWithUnobservableHole). Coverage ignores it, so that the
    // declaration keeps an instruction on its own line.
    HoleAtBlockEntry: Boolean;
    // Above zero while the code being compiled is outside this local's scope
    // although the local is still declared: ResolveLocal skips it then. See
    // TGocciaCompilerScope.SuspendBindingsDeeperThan.
    SuspendCount: Integer;
    IsNonStrictImmutable: Boolean;
    IsVar: Boolean;
    // A binding of a catch clause's parameter.
    IsCatchParameter: Boolean;
    // A binding of the calling function outside the variable environment a
    // sloppy direct eval in its body declares vars in: a parameter, or
    // `arguments`, of a function whose parameter list has expressions
    // (ES2026 §10.2.11 steps 20 and 30). Set on the locals rebuilt from an
    // eval site record; TGocciaCompilerScope.LocalOutsideVarEnvironment
    // works it out for the function being compiled.
    IsOutsideVarEnvironment: Boolean;
    // Rebuilt from an eval site record: a binding of the function whose
    // variable environment eval code declares its vars in
    // (TGocciaDirectEvalBindingInfo.IsCallerFunctionBinding).
    IsCallerFunctionBinding: Boolean;
    // The local of the function's arguments object, which
    // FunctionDeclarationInstantiation binds with the parameters (ES2026
    // §10.2.11 step 22.f).
    IsArgumentsObject: Boolean;
    IsGlobalBacked: Boolean;
    IsArrayTyped: Boolean;
    IsCallProvenNumeric: Boolean;
    // True when every value this binding can hold is a Number, so its Number
    // hint stays valid across assignments (Goccia.Compiler.NumericBindings).
    HoldsOnlyNumbers: Boolean;
    TypeHint: TGocciaLocalType;
    IsStrictlyTyped: Boolean;
    // A strictly typed binding that can hold a value no type check has seen:
    // the undefined of an annotated let or var declared without an
    // initializer, or the array of an annotated rest parameter. Its TypeHint
    // is still enforced on every store, but it does not describe what a read
    // yields, so ExpressionType does not report it.
    HoldsUncheckedValue: Boolean;
    ParamTypeSignature: string;
    // The type every call of the function this const binding holds returns,
    // or sltUntyped (CalleeReturnType in Goccia.Compiler.Statements).
    ReturnTypeHint: TGocciaLocalType;
    TypeAnnotation: string;
    ElementTypeAnnotation: string;
    HasConstantValue: Boolean;
    ConstantValue: TGocciaCompileTimeValue;
    IsImportBinding: Boolean;
    ImportPhase: TGocciaImportCallPhase;
    ImportModulePath: string;
    ImportExportName: string;
    ExportNames: TGocciaCompilerExportNameArray;
    ExportNameCount: Integer;
    // This local's entry in the template's debug locals, which name the
    // binding in a temporal-dead-zone error; -1 before a template is attached.
    DebugLocalIndex: Integer;
    // The newest older local of the same name, or -1: the binding this one
    // hides. Following these links visits only the locals of one name, so
    // dropping a block's binding or finding a name's var stays constant time
    // however many other locals the function has.
    ShadowedLocal: Integer;
  end;

  PGocciaCompilerLocal = ^TGocciaCompilerLocal;

  TGocciaCompilerUpvalue = record
    Name: string;
    Index: UInt16;
    IsLocal: Boolean;
    IsConst: Boolean;
    IsNonStrictImmutable: Boolean;
    IsVar: Boolean;
    IsGlobalBacked: Boolean;
    TypeHint: TGocciaLocalType;
    IsStrictlyTyped: Boolean;
    HoldsUncheckedValue: Boolean;
    ParamTypeSignature: string;
    // Copied from the captured binding: the return type of a const function
    // and whether a let holds only Numbers.
    ReturnTypeHint: TGocciaLocalType;
    HoldsOnlyNumbers: Boolean;
    IsImportBinding: Boolean;
    ImportPhase: TGocciaImportCallPhase;
    ImportModulePath: string;
    ImportExportName: string;
    ExportNames: TGocciaCompilerExportNameArray;
    ExportNameCount: Integer;
  end;

  // What TGocciaCompilerScope.SuspendBindingsDeeperThan took out of name
  // resolution, for ResumeBindings to put back.
  TGocciaCompilerSuspendedBindings = record
    Depth: Integer;
    LocalCount: Integer;
    WithBindingNames: array of string;
    WithBindingDepths: array of Integer;
    WithBindingCount: Integer;
  end;

  TGocciaCompilerScope = class
  private
    FParent: TGocciaCompilerScope;
    FLocals: array of TGocciaCompilerLocal;
    FLocalIndex: TOrderedStringMap<Integer>;
    FLocalCount: Integer;
    // How many of the locals hold each register, indexed by slot, so that
    // SlotHoldsLocal answers without visiting the locals.
    FSlotLocalCounts: array of Integer;
    FUpvalues: array of TGocciaCompilerUpvalue;
    FUpvalueCount: Integer;
    FDepth: Integer;
    FNextSlot: Integer;
    FMaxSlot: UInt16;
    FPrivatePrefix: string;
    // Register of the class whose body this function's code is evaluating,
    // or -1 outside a class body.
    FPrivateClassReg: Integer;
    FPrivateNames: array of string;
    FPrivatePrefixes: array of string;
    FPrivateNameCount: Integer;
    FIsArrow: Boolean;
    FDirectEvalSyntheticArgumentsSlot: Integer;
    FGlobalBackedBindingsByName: Boolean;
    FImportUpvaluesByName: Boolean;
    FWithBindingNames: array of string;
    FWithBindingDepths: array of Integer;
    FWithBindingCount: Integer;
    FLoopDepth: Integer;
    FLoopMayCreateClosure: Boolean;
    // FLoopMayCreateClosure as it was when each open loop was entered.
    FOuterLoopMayCreateClosure: array of Boolean;
    FDirectEvalSeen: Boolean;
    FDirectEvalUsage: TGocciaDirectEvalUsage;
    FEvalVarObjectSlot: Integer;
    FParameterEvalVarObjectSlot: Integer;
    FEvalVarObjectName: string;
    FParameterEvalVarObjectName: string;
    FCompilingParameters: Boolean;
    FIsSloppyEvalCode: Boolean;
    FEvalCodeVarEnvironment: TGocciaDirectEvalVarEnvironment;
    FParametersInVarEnvironment: Boolean;
    FParameterNames: array of string;
    FTemplate: TGocciaFunctionTemplate;
    // The compile profile when --profile=compile is on, else nil; a scope
    // takes its parent's (Goccia.Profiler.Compile).
    FCompileProfile: TGocciaCompileProfiler;
    FUpvaluesSeeded: Boolean;
    // The join of the types this function returns (zeroed by InitInstance:
    // nothing seen yet), and the result for the last function compiled as a
    // child of this scope.
    FReturnType: TGocciaLocalType;
    FReturnSeen: Boolean;
    FReturnConflict: Boolean;
    FLastChildReturnType: TGocciaLocalType;
    // The binary expression this function's code compiled last, and the type
    // of the value its instruction produces (see NoteBinaryResultType).
    FLastBinaryExpression: TGocciaExpression;
    FLastBinaryResultType: TGocciaLocalType;
    // ExpressionType's answers for binary expressions (ExpressionTypeMemo),
    // and the template and code count they hold for.
    FTypeMemo: TGocciaExpressionTypeMemo;
    FTypeMemoTemplate: TGocciaFunctionTemplate;
    FTypeMemoCodeCount: Integer;
    // While the case clauses of a switch statement are compiled, the locals
    // marked initialized or given a constant value, in that order, and where
    // each open switch's next clause starts reading them (BeginCaseClauses).
    FSettledLocals: array of Integer;
    FSettledLocalCount: Integer;
    FCaseClauseMarks: array of Integer;
    FCaseClauseMarkCount: Integer;
    procedure EnsureLocalIndex;
    procedure NoteSettledLocal(const AIndex: Integer);
    procedure CountLocalSlot(const ASlot: UInt16);
    function NewestLocalNamed(const AName: string): Integer;
    procedure RecordDebugLocal(const AIndex: Integer);
    procedure ForgetTopLocalName;
    procedure RelinkLocalNames;
  public
    constructor Create(const AParent: TGocciaCompilerScope;
      const ADepth: Integer);
    destructor Destroy; override;

    { Sizes the locals table for ACount locals when nothing is declared
      yet. Every slot of the table is initialized and finalized whether a
      local fills it or not, so a scope that knows it holds fewer locals
      than the first growth step allocates skips that work. }
    procedure ReserveLocals(const ACount: Integer);
    function DeclareLocal(const AName: string;
      const AIsConst: Boolean): UInt16;
    function DeclareVarLocal(const AName: string): UInt16;
    function ResolveLocal(const AName: string): Integer;
    { The oldest local named AName in register ASlot, suspended or not, or
      -1. }
    function FindLocalBySlot(const AName: string;
      const ASlot: UInt16): Integer;
    { The oldest local named AName at the function's top level (depth 0),
      suspended or not, or -1. }
    function FindVarLocalIndex(const AName: string): Integer;
    function ResolveUpvalue(const AName: string): Integer;
    function AddUpvalue(const AName: string; const AIndex: UInt16;
      const AIsLocal: Boolean; const AIsConst: Boolean = False;
      const AIsVar: Boolean = False): Integer;
    { The calling function of a direct eval, rebuilt from its eval site
      record (TGocciaCompiler.CompileDirectEval): declares AName at the
      caller's register ASlot, and returns the local's index. }
    function DeclareCallerLocal(const AName: string; const ASlot: UInt16;
      const AIsConst, AIsVar: Boolean): Integer;
    { Makes AName this scope's upvalue AIndex, the index the calling
      function's closure holds it at. Once a scope has a seeded upvalue,
      ResolveUpvalue answers from the seeded ones alone. }
    procedure SeedUpvalue(const AIndex: UInt16; const AName: string;
      const AIsConst, AIsVar, AIsNonStrictImmutable: Boolean);

    { Makes ATemplate the function whose debug info records this scope's
      locals, with the code range each one occupies its slot over. Locals
      declared before the template was attached start at its current PC.
      Only the first call attaches; later ones are no-ops. }
    procedure AttachTemplate(const ATemplate: TGocciaFunctionTemplate);

    function AllocateRegister: UInt16;
    procedure FreeRegister;
    procedure BeginScope;
    procedure EndScope(out AClosedLocals: TArray<UInt16>;
      out AClosedCount: Integer);

    property Parent: TGocciaCompilerScope read FParent;
    property CompileProfile: TGocciaCompileProfiler read FCompileProfile
      write FCompileProfile;
    property LocalCount: Integer read FLocalCount;
    property UpvalueCount: Integer read FUpvalueCount;
    property Depth: Integer read FDepth;
    property MaxSlot: UInt16 read FMaxSlot;
    property NextSlot: Integer read FNextSlot;

    function GetLocal(const AIndex: Integer): TGocciaCompilerLocal;
    // True when one of the locals, suspended or not, holds register ASlot.
    // Constant time: a call is compiled with this check, and a function's top
    // level can hold thousands of locals.
    function SlotHoldsLocal(const ASlot: UInt16): Boolean;
    // The local in place, without copying the record and its managed fields.
    // The pointer is valid only until the next DeclareLocal or DeclareVarLocal,
    // which can reallocate the local array.
    function LocalAt(const AIndex: Integer): PGocciaCompilerLocal; {$IFDEF FPC}inline;{$ENDIF}
    function GetUpvalue(const AIndex: Integer): TGocciaCompilerUpvalue;
    procedure MarkCaptured(const AIndex: Integer);
    // Takes the local out of name resolution under its current name. Its slot
    // stays allocated and the closures already compiled keep capturing it; a
    // name lookup from here on, including DeclareVarLocal, no longer finds it.
    procedure RenameLocal(const AIndex: Integer; const ANewName: string);
    procedure MarkLocalInitialized(const AIndex: Integer);
    // ES2026 §10.2.11 FunctionDeclarationInstantiation(func, argumentsList)
    // step 5: parameterNames, the BoundNames of the function's formal
    // parameters. Recorded while the parameter list is compiled; the
    // function body reads them before it is compiled.
    procedure AddParameterName(const AName: string);
    function HasParameterName(const AName: string): Boolean;
    { The case clauses of a switch statement share one block scope, and a
      clause can be entered without running the declarations of the clauses
      before it. So a read in a clause must neither use the register of an
      earlier clause's binding directly nor be replaced by a constant tracked
      for it. BeginCaseClauses is called after the switch's BeginScope and
      before its lexical prelude; from then on the scope notes every local
      marked initialized or given a constant value. Before each clause,
      ForgetCaseClauseFacts(Depth) clears both for the switch's own locals
      (those at ADepth) noted since the previous clause, and keeps the notes
      on other live locals for an enclosing switch. Every local at ADepth
      that is initialized or holds a constant was noted, so this visits only
      what the previous clause settled, not every local of the function.
      EndCaseClauses closes the innermost switch. }
    procedure BeginCaseClauses;
    procedure ForgetCaseClauseFacts(const ADepth: Integer);
    procedure EndCaseClauses;
    procedure EnterLoop(const AMayCreateClosure: Boolean);
    procedure LeaveLoop;
    procedure MarkDirectEvalSeen;
    function MutableLocalsMayLeaveRegisters: Boolean;
    procedure MarkNonStrictImmutable(const AIndex: Integer);
    procedure MarkGlobalBacked(const AIndex: Integer);
    procedure SetLocalTypeHint(const AIndex: Integer;
      const ATypeHint: TGocciaLocalType);
    procedure SetLocalStrictlyTyped(const AIndex: Integer;
      const AStrictlyTyped: Boolean);
    procedure SetLocalHoldsUncheckedValue(const AIndex: Integer;
      const AHoldsUncheckedValue: Boolean);
    procedure SetLocalArrayTyped(const AIndex: Integer;
      const AArrayTyped: Boolean);
    procedure SetLocalCallProvenNumeric(const AIndex: Integer;
      const AProvenNumeric: Boolean);
    procedure SetLocalHoldsOnlyNumbers(const AIndex: Integer;
      const AHoldsOnlyNumbers: Boolean);
    procedure SetLocalParamTypeSignature(const AIndex: Integer;
      const ASignature: string);
    procedure SetLocalReturnTypeHint(const AIndex: Integer;
      const AType: TGocciaLocalType);
    // Records that one return of this function yields a value of AType
    // (sltUntyped: unknown, or undefined).
    procedure NoteReturnType(const AType: TGocciaLocalType);
    // The type every return of this function yields, or sltUntyped.
    function InferredReturnType: TGocciaLocalType;
    property LastChildReturnType: TGocciaLocalType read FLastChildReturnType
      write FLastChildReturnType;
    { A binary operator hands the type of the value it computed to the
      operator whose operand it is, which would otherwise work the type out
      again from the operand's whole subtree: in a chain of k operators that
      is k walks, each as long as the chain so far. The operand's value is
      computed into a register before anything after it runs, so its type is
      the one it had when its instruction was chosen.
      NoteBinaryResultType records it as the last act of compiling AExpr;
      TakeBinaryResultType, called right after the operand is compiled, gives
      it back once, and only for that operand: a nested operator recorded
      later, or an operand compiled another way (folded to a constant, or
      simplified), leaves no record of AExpr. }
    { ExpressionType's answers for the binary expressions a walk has typed,
      for a caller that would otherwise walk them again. They hold while
      ATemplate emits nothing but folded constants (NoteConstantEmitted),
      which change nothing a type depends on; once anything else is emitted
      the record is emptied when next asked for. }
    function ExpressionTypeMemo(
      const ATemplate: TGocciaFunctionTemplate): TGocciaExpressionTypeMemo;
    { ATemplate's code grew from ABefore to AAfter instructions by a folded
      constant alone. }
    procedure NoteConstantEmitted(const ATemplate: TGocciaFunctionTemplate;
      const ABefore, AAfter: Integer);
    procedure ForgetBinaryResultType;
    procedure NoteBinaryResultType(const AExpr: TGocciaExpression;
      const AType: TGocciaLocalType);
    function TakeBinaryResultType(const AExpr: TGocciaExpression;
      out AType: TGocciaLocalType): Boolean;
    procedure SetLocalTypeAnnotation(const AIndex: Integer;
      const AAnnotation: string);
    procedure SetLocalElementTypeAnnotation(const AIndex: Integer;
      const AAnnotation: string);
    procedure SetLocalConstantValue(const AIndex: Integer;
      const AValue: TGocciaCompileTimeValue);
    procedure ClearLocalConstantValue(const AIndex: Integer);
    procedure MarkImportBinding(const AIndex: Integer;
      const APhase: TGocciaImportCallPhase; const AModulePath,
      AExportName: string);
    procedure MarkExportBinding(const AIndex: Integer;
      const AExportName: string);
    function TryGetVisibleConstantValue(const AName: string;
      out AValue: TGocciaCompileTimeValue): Boolean;
    function HasVisibleLocal(const AName: string): Boolean;
    function DirectEvalMayShadow(const AName: string): Boolean;
    function ResolvePrivatePrefix: string;
    function ResolvePrivatePrefixForName(const AName: string): string;
    procedure DeclarePrivateNamePrefix(const AName, APrefix: string);
    function PrivateNameMark: Integer;
    // This scope's own private names, in declaration order.
    function GetPrivateName(const AIndex: Integer): string;
    function GetPrivateNamePrefix(const AIndex: Integer): string;
    procedure RestorePrivateNameMark(const AMark: Integer);
    procedure PushWithBinding(const AHiddenName: string);
    procedure PopWithBinding;
    function SuspendBindingsDeeperThan(
      const ADepth: Integer): TGocciaCompilerSuspendedBindings;
    procedure ResumeBindings(
      const ASuspended: TGocciaCompilerSuspendedBindings);
    function GetWithBindingName(const AIndex: Integer): string;
    { The with bindings a reference to AName compiled in this scope probes
      before its static binding, nearest first: the `with` objects and the
      hidden variable objects (DeclareEvalVarObjects) between the reference
      and the binding the name resolves to, or all of them when it resolves
      to none (ES2026 §9.1.2.1 GetIdentifierReference). A `with` object of
      the binding's own function is between when the binding is not declared
      inside its statement. A hidden variable object of that function is
      between only for a binding outside the variable environment it stands
      for: a parameter of a parameter list with expressions, for the body's
      object, or a binding listed before it in an eval site record that the
      scope was rebuilt from. The body's object is not in effect while the
      parameter list compiles. }
    function NameProbes(const AName: string): TArray<string>;
    function GetWithBindingDepth(const AIndex: Integer): Integer;
    property PrivatePrefix: string read FPrivatePrefix write FPrivatePrefix;
    property PrivateClassReg: Integer read FPrivateClassReg
      write FPrivateClassReg;
    property IsArrow: Boolean read FIsArrow write FIsArrow;
    property DirectEvalSyntheticArgumentsSlot: Integer read FDirectEvalSyntheticArgumentsSlot write FDirectEvalSyntheticArgumentsSlot;
    { The top level of a non-strict global eval: its var and function
      bindings are deletable properties of the global object (ES2026
      §19.2.1.3 steps 17-18), so a reference to one, in the eval or in a
      function it creates, resolves by name at run time as any reference to
      a global does, and is a ReferenceError once the binding is deleted.
      ResolveLocal does not return such a local. }
    property GlobalBackedBindingsByName: Boolean
      read FGlobalBackedBindingsByName write FGlobalBackedBindingsByName;
    { The scope of a function created while its module is linked (a
      top-level function declaration, ES2026 §16.2.1.7.3.1
      InitializeEnvironment). It exists before the module body has run, so
      it cannot capture the body's registers: an import binding of the
      module, which the body keeps in a register, is captured as a
      global-backed upvalue and read by name from the module environment,
      as every other top-level binding of a linked module is. }
    property ImportUpvaluesByName: Boolean
      read FImportUpvaluesByName write FImportUpvaluesByName;
    property WithBindingCount: Integer read FWithBindingCount;
    property LoopDepth: Integer read FLoopDepth;
    property LoopMayCreateClosure: Boolean read FLoopMayCreateClosure;
    property DirectEvalSeen: Boolean read FDirectEvalSeen;
    // ADR 0134 static detection: what the parser found in this function's own
    // code (TGocciaDirectEvalUsage), set before any of it is compiled. Empty
    // for the compiler's own functions around class field initializers and
    // static blocks, whose code is strict.
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
    { Declares the hidden locals that stand for the variable environments a
      sloppy direct eval in this function declares its vars in (ADR 0134):
      the function's own, and, when the parameter list is not simple, the
      parameters' (ES2026 §10.2.11 step 20). Called with the parameters
      declared and before any code of the function is compiled, so the
      registers are this function's for its whole extent. Does nothing
      unless DirectEvalUsage has deuSloppyDirectEval. }
    procedure DeclareEvalVarObjects(const AParameterListIsSimple: Boolean);
    // The registers DeclareEvalVarObjects reserved, or -1.
    property EvalVarObjectSlot: Integer read FEvalVarObjectSlot;
    property ParameterEvalVarObjectSlot: Integer
      read FParameterEvalVarObjectSlot;
    // The names of those hidden locals, or ''. Each local holds its hidden
    // variable object once an eval needs it, and is pushed as a with binding:
    // references such a var can shadow probe it as they probe a `with`
    // object, the parameters' after the body's.
    property EvalVarObjectName: string read FEvalVarObjectName;
    property ParameterEvalVarObjectName: string
      read FParameterEvalVarObjectName;
    // Whether local AIndex is outside the variable environment a sloppy
    // direct eval in the function body declares its vars in, so that such a
    // var shadows it (TGocciaCompilerLocal.IsOutsideVarEnvironment).
    function LocalOutsideVarEnvironment(const AIndex: Integer): Boolean;
    procedure MarkOutsideVarEnvironment(const AIndex: Integer);
    // Marks the depth-0 local at ASlot as the arguments object's
    // (TGocciaCompilerLocal.IsArgumentsObject).
    procedure MarkArgumentsObjectSlot(const ASlot: UInt16);
    // Marks local AIndex, rebuilt from an eval site record, as a binding of
    // the caller function (IsCallerFunctionBinding), a var of its variable
    // environment when AIsVar.
    procedure MarkCallerFunctionBinding(const AIndex: Integer;
      const AIsVar, AIsCatchParameter: Boolean);
    // The number of `with` objects among the with bindings, not counting the
    // hidden variable objects.
    function WithObjectCount: Integer;
    // True while the parameter list is compiled, where a direct eval's vars
    // go to the parameters' environment.
    property CompilingParameters: Boolean read FCompilingParameters
      write FCompilingParameters;
    { The top level of non-strict direct eval code, whose variable
      environment is its caller's (ES2026 §19.2.1.1 PerformEval step 16):
      EvalCodeVarEnvironment says which, devGlobal or devFunction, and a
      direct eval in this code declares its vars there too. }
    property IsSloppyEvalCode: Boolean read FIsSloppyEvalCode
      write FIsSloppyEvalCode;
    property EvalCodeVarEnvironment: TGocciaDirectEvalVarEnvironment
      read FEvalCodeVarEnvironment write FEvalCodeVarEnvironment;
    // ES2026 §10.2.11 step 28: the parameter list has no expressions, so the
    // parameters are bindings of the function's variable environment, which a
    // sloppy direct eval in the body declares its vars in.
    property ParametersInVarEnvironment: Boolean
      read FParametersInVarEnvironment write FParametersInVarEnvironment;
  end;

function NextClassPrivatePrefix: string;
// Keeps the hidden variable object names DeclareEvalVarObjects makes from
// now on different from AName, one an eval site record already names.
procedure ReserveEvalVarObjectName(const AName: string);


implementation

uses
  SysUtils,

  Goccia.Error.Messages;

const
  COMPILER_LOCAL_INDEX_THRESHOLD = 64;
  COMPILER_SUPER_LOCAL = '__super__';

{ TGocciaCompilerScope }

procedure TGocciaCompilerScope.EnsureLocalIndex;
var
  I: Integer;
begin
  if Assigned(FLocalIndex) then
    Exit;
  if FLocalCount < COMPILER_LOCAL_INDEX_THRESHOLD then
    Exit;

  FLocalIndex := TOrderedStringMap<Integer>.Create(FLocalCount * 2);
  for I := 0 to FLocalCount - 1 do
    FLocalIndex.Add(FLocals[I].Name, I);
end;

procedure TGocciaCompilerScope.CountLocalSlot(const ASlot: UInt16);
var
  NewLength: Integer;
begin
  if ASlot >= Length(FSlotLocalCounts) then
  begin
    NewLength := Length(FSlotLocalCounts) * 2 + 16;
    if NewLength <= ASlot then
      NewLength := ASlot + 1;
    SetLength(FSlotLocalCounts, NewLength);
  end;
  Inc(FSlotLocalCounts[ASlot]);
end;

function TGocciaCompilerScope.NewestLocalNamed(const AName: string): Integer;
var
  I: Integer;
begin
  if Assigned(FLocalIndex) then
  begin
    if Assigned(FCompileProfile) then
      FCompileProfile.CountLookup(ccLocalLookups, ccLocalLookupProbes, 1);
    if not FLocalIndex.TryGetValue(AName, Result) then
      Result := -1;
    Exit;
  end;
  // Without the index there are fewer than COMPILER_LOCAL_INDEX_THRESHOLD
  // locals to scan.
  for I := FLocalCount - 1 downto 0 do
    if FLocals[I].Name = AName then
    begin
      if Assigned(FCompileProfile) then
        FCompileProfile.CountLookup(ccLocalLookups, ccLocalLookupProbes,
          FLocalCount - I);
      Exit(I);
    end;
  if Assigned(FCompileProfile) then
    FCompileProfile.CountLookup(ccLocalLookups, ccLocalLookupProbes,
      FLocalCount);
  Result := -1;
end;

// The top local has just been popped (it is at FLocalCount): its name now
// reaches the local it hid, if any.
procedure TGocciaCompilerScope.ForgetTopLocalName;
var
  Shadowed: Integer;
begin
  if not Assigned(FLocalIndex) then
    Exit;

  Shadowed := FLocals[FLocalCount].ShadowedLocal;
  if Shadowed >= 0 then
    FLocalIndex.Add(FLocals[FLocalCount].Name, Shadowed)
  else
    FLocalIndex.Remove(FLocals[FLocalCount].Name);

  if FLocalCount < COMPILER_LOCAL_INDEX_THRESHOLD then
    FreeAndNil(FLocalIndex);
end;

// Recomputes every local's ShadowedLocal and the index from the names, after
// a local below the top changed its name.
procedure TGocciaCompilerScope.RelinkLocalNames;
var
  I, Previous: Integer;
  Newest: TOrderedStringMap<Integer>;
begin
  if Assigned(FCompileProfile) then
    FCompileProfile.Count(ccLocalListScans, FLocalCount);
  Newest := TOrderedStringMap<Integer>.Create(FLocalCount * 2);
  try
    for I := 0 to FLocalCount - 1 do
    begin
      if Newest.TryGetValue(FLocals[I].Name, Previous) then
        FLocals[I].ShadowedLocal := Previous
      else
        FLocals[I].ShadowedLocal := -1;
      Newest.Add(FLocals[I].Name, I);
    end;
    // Newest now maps each name to its newest local, which is the index.
    if Assigned(FLocalIndex) then
    begin
      FLocalIndex.Free;
      FLocalIndex := Newest;
      Newest := nil;
    end;
  finally
    Newest.Free;
  end;
end;

constructor TGocciaCompilerScope.Create(const AParent: TGocciaCompilerScope;
  const ADepth: Integer);
var
  I: Integer;
begin
  inherited Create;
  FParent := AParent;
  FLocalIndex := nil;
  FLocalCount := 0;
  FUpvalueCount := 0;
  FDepth := ADepth;
  FNextSlot := 0;
  FMaxSlot := 0;
  FPrivateNameCount := 0;
  FPrivateClassReg := -1;
  FDirectEvalSyntheticArgumentsSlot := -1;
  FWithBindingCount := 0;
  FLoopDepth := 0;
  FLoopMayCreateClosure := False;
  FDirectEvalSeen := False;
  FDirectEvalUsage := [];
  FEvalVarObjectSlot := -1;
  FParameterEvalVarObjectSlot := -1;
  FCompilingParameters := False;
  FParametersInVarEnvironment := False;
  FUpvaluesSeeded := False;
  FTemplate := nil;
  if Assigned(AParent) then
    FCompileProfile := AParent.FCompileProfile
  else
    FCompileProfile := nil;
  FLastBinaryExpression := nil;
  FTypeMemo := nil;
  FTypeMemoTemplate := nil;
  FSettledLocalCount := 0;
  FCaseClauseMarkCount := 0;
  if Assigned(AParent) and (AParent.FWithBindingCount > 0) then
  begin
    FWithBindingCount := AParent.FWithBindingCount;
    SetLength(FWithBindingNames, FWithBindingCount);
    SetLength(FWithBindingDepths, FWithBindingCount);
    for I := 0 to FWithBindingCount - 1 do
    begin
      FWithBindingNames[I] := AParent.FWithBindingNames[I];
      FWithBindingDepths[I] := -1;
    end;
  end;
end;

destructor TGocciaCompilerScope.Destroy;
begin
  FLocalIndex.Free;
  FTypeMemo.Free;
  inherited;
end;

procedure TGocciaCompilerScope.ReserveLocals(const ACount: Integer);
begin
  if (FLocalCount = 0) and (Length(FLocals) = 0) and (ACount > 0) then
    SetLength(FLocals, ACount);
end;

function TGocciaCompilerScope.DeclareLocal(const AName: string;
  const AIsConst: Boolean): UInt16;
begin
  if FNextSlot >= High(UInt16) then
    raise EGocciaBytecodeLimit.CreateFmt(SErrorBytecodeLocalLimit,
      [High(UInt16)]);
  if FLocalCount >= Length(FLocals) then
    SetLength(FLocals, FLocalCount * 2 + 8);
  FLocals[FLocalCount].Name := AName;
  FLocals[FLocalCount].Slot := UInt16(FNextSlot);
  FLocals[FLocalCount].Depth := FDepth;
  FLocals[FLocalCount].IsCaptured := False;
  FLocals[FLocalCount].IsInitialized := False;
  FLocals[FLocalCount].HoleAtBlockEntry := False;
  FLocals[FLocalCount].SuspendCount := 0;
  FLocals[FLocalCount].IsConst := AIsConst;
  FLocals[FLocalCount].IsNonStrictImmutable := False;
  FLocals[FLocalCount].IsVar := False;
  FLocals[FLocalCount].IsCatchParameter := False;
  FLocals[FLocalCount].IsOutsideVarEnvironment := False;
  FLocals[FLocalCount].IsCallerFunctionBinding := False;
  FLocals[FLocalCount].IsArgumentsObject := False;
  FLocals[FLocalCount].IsGlobalBacked := False;
  FLocals[FLocalCount].IsArrayTyped := False;
  FLocals[FLocalCount].IsCallProvenNumeric := False;
  FLocals[FLocalCount].HoldsOnlyNumbers := False;
  FLocals[FLocalCount].TypeHint := sltUntyped;
  FLocals[FLocalCount].IsStrictlyTyped := False;
  FLocals[FLocalCount].HoldsUncheckedValue := False;
  FLocals[FLocalCount].ParamTypeSignature := '';
  FLocals[FLocalCount].ReturnTypeHint := sltUntyped;
  FLocals[FLocalCount].TypeAnnotation := '';
  FLocals[FLocalCount].ElementTypeAnnotation := '';
  FLocals[FLocalCount].HasConstantValue := False;
  ResetCompileTimeValue(FLocals[FLocalCount].ConstantValue, ctvkUnknown);
  FLocals[FLocalCount].IsImportBinding := False;
  FLocals[FLocalCount].ImportPhase := icpEvaluation;
  FLocals[FLocalCount].ImportModulePath := '';
  FLocals[FLocalCount].ImportExportName := '';
  FLocals[FLocalCount].ExportNameCount := 0;
  SetLength(FLocals[FLocalCount].ExportNames, 0);
  FLocals[FLocalCount].ShadowedLocal := NewestLocalNamed(AName);
  RecordDebugLocal(FLocalCount);
  CountLocalSlot(FLocals[FLocalCount].Slot);
  Result := UInt16(FNextSlot);
  EnsureLocalIndex;
  if Assigned(FLocalIndex) then
    FLocalIndex.Add(AName, FLocalCount);
  Inc(FLocalCount);
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := UInt16(FNextSlot);
end;

function TGocciaCompilerScope.DeclareVarLocal(const AName: string): UInt16;
var
  I, Oldest: Integer;
begin
  // Check if already declared at var/function scope (var redeclaration is allowed).
  // Block-scoped declarations with the same name must not capture var writes.
  // The oldest such local is the var's.
  Oldest := -1;
  I := NewestLocalNamed(AName);
  while I >= 0 do
  begin
    if (FLocals[I].Depth = 0) and
       (FLocals[I].Slot <> FDirectEvalSyntheticArgumentsSlot) then
      Oldest := I;
    I := FLocals[I].ShadowedLocal;
  end;
  if Oldest >= 0 then
    Exit(FLocals[Oldest].Slot);

  // Declare at depth 0 so EndScope never pops it
  if FNextSlot >= High(UInt16) then
    raise EGocciaBytecodeLimit.CreateFmt(SErrorBytecodeLocalLimit,
      [High(UInt16)]);
  if FLocalCount >= Length(FLocals) then
    SetLength(FLocals, FLocalCount * 2 + 8);
  FLocals[FLocalCount].Name := AName;
  FLocals[FLocalCount].Slot := UInt16(FNextSlot);
  FLocals[FLocalCount].Depth := 0;
  FLocals[FLocalCount].IsCaptured := False;
  FLocals[FLocalCount].IsInitialized := False;
  FLocals[FLocalCount].HoleAtBlockEntry := False;
  FLocals[FLocalCount].SuspendCount := 0;
  FLocals[FLocalCount].IsConst := False;
  FLocals[FLocalCount].IsNonStrictImmutable := False;
  FLocals[FLocalCount].IsVar := True;
  FLocals[FLocalCount].IsCatchParameter := False;
  FLocals[FLocalCount].IsOutsideVarEnvironment := False;
  FLocals[FLocalCount].IsCallerFunctionBinding := False;
  FLocals[FLocalCount].IsArgumentsObject := False;
  FLocals[FLocalCount].IsGlobalBacked := False;
  FLocals[FLocalCount].IsArrayTyped := False;
  FLocals[FLocalCount].IsCallProvenNumeric := False;
  FLocals[FLocalCount].HoldsOnlyNumbers := False;
  FLocals[FLocalCount].TypeHint := sltUntyped;
  FLocals[FLocalCount].IsStrictlyTyped := False;
  FLocals[FLocalCount].HoldsUncheckedValue := False;
  FLocals[FLocalCount].ParamTypeSignature := '';
  FLocals[FLocalCount].ReturnTypeHint := sltUntyped;
  FLocals[FLocalCount].TypeAnnotation := '';
  FLocals[FLocalCount].ElementTypeAnnotation := '';
  FLocals[FLocalCount].HasConstantValue := False;
  ResetCompileTimeValue(FLocals[FLocalCount].ConstantValue, ctvkUnknown);
  FLocals[FLocalCount].IsImportBinding := False;
  FLocals[FLocalCount].ImportPhase := icpEvaluation;
  FLocals[FLocalCount].ImportModulePath := '';
  FLocals[FLocalCount].ImportExportName := '';
  FLocals[FLocalCount].ExportNameCount := 0;
  SetLength(FLocals[FLocalCount].ExportNames, 0);
  FLocals[FLocalCount].ShadowedLocal := NewestLocalNamed(AName);
  RecordDebugLocal(FLocalCount);
  CountLocalSlot(FLocals[FLocalCount].Slot);
  Result := UInt16(FNextSlot);
  EnsureLocalIndex;
  if Assigned(FLocalIndex) then
    FLocalIndex.Add(AName, FLocalCount);
  Inc(FLocalCount);
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := UInt16(FNextSlot);
end;

function TGocciaCompilerScope.ResolveLocal(const AName: string): Integer;
var
  I: Integer;
begin
  // The newest local of the name, or, when that one is suspended or resolves
  // by name, the next older one: the links visit only locals of this name.
  I := NewestLocalNamed(AName);
  while I >= 0 do
  begin
    if (FLocals[I].SuspendCount = 0) and
       not (FGlobalBackedBindingsByName and FLocals[I].IsGlobalBacked) then
      Exit(I);
    I := FLocals[I].ShadowedLocal;
    if Assigned(FCompileProfile) then
      FCompileProfile.Count(ccLocalLookupProbes);
  end;
  Result := -1;
end;

function TGocciaCompilerScope.FindLocalBySlot(const AName: string;
  const ASlot: UInt16): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := NewestLocalNamed(AName);
  while I >= 0 do
  begin
    if FLocals[I].Slot = ASlot then
      Result := I;
    I := FLocals[I].ShadowedLocal;
  end;
end;

function TGocciaCompilerScope.FindVarLocalIndex(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := NewestLocalNamed(AName);
  while I >= 0 do
  begin
    if FLocals[I].Depth = 0 then
      Result := I;
    I := FLocals[I].ShadowedLocal;
  end;
end;

function TGocciaCompilerScope.ResolveUpvalue(const AName: string): Integer;
var
  LocalIdx: Integer;
  UpvalueIdx: Integer;
  Idx: Integer;
begin
  if Assigned(FCompileProfile) then
    FCompileProfile.Count(ccUpvalueResolutions);
  if FUpvaluesSeeded then
  begin
    for Idx := 0 to FUpvalueCount - 1 do
      if FUpvalues[Idx].Name = AName then
      begin
        if Assigned(FCompileProfile) then
          FCompileProfile.Count(ccUpvalueListProbes, Idx + 1);
        Exit(Idx);
      end;
    if Assigned(FCompileProfile) then
      FCompileProfile.Count(ccUpvalueListProbes, FUpvalueCount);
    Exit(-1);
  end;
  if not Assigned(FParent) then
    Exit(-1);

  LocalIdx := FParent.ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    FParent.MarkCaptured(LocalIdx);
    Idx := AddUpvalue(AName, FParent.FLocals[LocalIdx].Slot, True,
      FParent.FLocals[LocalIdx].IsConst, FParent.FLocals[LocalIdx].IsVar);
    FUpvalues[Idx].IsNonStrictImmutable :=
      FParent.FLocals[LocalIdx].IsNonStrictImmutable;
    FUpvalues[Idx].IsGlobalBacked := FParent.FLocals[LocalIdx].IsGlobalBacked;
    FUpvalues[Idx].TypeHint := FParent.FLocals[LocalIdx].TypeHint;
    FUpvalues[Idx].IsStrictlyTyped := FParent.FLocals[LocalIdx].IsStrictlyTyped;
    FUpvalues[Idx].HoldsUncheckedValue :=
      FParent.FLocals[LocalIdx].HoldsUncheckedValue;
    FUpvalues[Idx].ParamTypeSignature := FParent.FLocals[LocalIdx].ParamTypeSignature;
    FUpvalues[Idx].ReturnTypeHint := FParent.FLocals[LocalIdx].ReturnTypeHint;
    FUpvalues[Idx].HoldsOnlyNumbers := FParent.FLocals[LocalIdx].HoldsOnlyNumbers and
      not FParent.FLocals[LocalIdx].IsGlobalBacked;
    FUpvalues[Idx].IsImportBinding := FParent.FLocals[LocalIdx].IsImportBinding;
    FUpvalues[Idx].ImportPhase := FParent.FLocals[LocalIdx].ImportPhase;
    FUpvalues[Idx].ImportModulePath := FParent.FLocals[LocalIdx].ImportModulePath;
    FUpvalues[Idx].ImportExportName := FParent.FLocals[LocalIdx].ImportExportName;
    FUpvalues[Idx].ExportNames := Copy(FParent.FLocals[LocalIdx].ExportNames);
    FUpvalues[Idx].ExportNameCount := FParent.FLocals[LocalIdx].ExportNameCount;
    if FImportUpvaluesByName and FUpvalues[Idx].IsImportBinding then
    begin
      FUpvalues[Idx].IsImportBinding := False;
      FUpvalues[Idx].IsGlobalBacked := True;
    end;
    Exit(Idx);
  end;

  if not FIsArrow and (AName = COMPILER_SUPER_LOCAL) then
    Exit(-1);

  UpvalueIdx := FParent.ResolveUpvalue(AName);
  if UpvalueIdx >= 0 then
  begin
    if UpvalueIdx > High(UInt16) then
      raise EGocciaBytecodeLimit.CreateFmt(SErrorBytecodeUpvalueLimit,
        [High(UInt16)]);
    Idx := AddUpvalue(AName, UInt16(UpvalueIdx), False,
      FParent.FUpvalues[UpvalueIdx].IsConst,
      FParent.FUpvalues[UpvalueIdx].IsVar);
    FUpvalues[Idx].IsNonStrictImmutable :=
      FParent.FUpvalues[UpvalueIdx].IsNonStrictImmutable;
    FUpvalues[Idx].IsGlobalBacked := FParent.FUpvalues[UpvalueIdx].IsGlobalBacked;
    FUpvalues[Idx].TypeHint := FParent.FUpvalues[UpvalueIdx].TypeHint;
    FUpvalues[Idx].IsStrictlyTyped := FParent.FUpvalues[UpvalueIdx].IsStrictlyTyped;
    FUpvalues[Idx].HoldsUncheckedValue :=
      FParent.FUpvalues[UpvalueIdx].HoldsUncheckedValue;
    FUpvalues[Idx].ParamTypeSignature := FParent.FUpvalues[UpvalueIdx].ParamTypeSignature;
    FUpvalues[Idx].ReturnTypeHint := FParent.FUpvalues[UpvalueIdx].ReturnTypeHint;
    FUpvalues[Idx].HoldsOnlyNumbers := FParent.FUpvalues[UpvalueIdx].HoldsOnlyNumbers;
    FUpvalues[Idx].IsImportBinding := FParent.FUpvalues[UpvalueIdx].IsImportBinding;
    FUpvalues[Idx].ImportPhase := FParent.FUpvalues[UpvalueIdx].ImportPhase;
    FUpvalues[Idx].ImportModulePath := FParent.FUpvalues[UpvalueIdx].ImportModulePath;
    FUpvalues[Idx].ImportExportName := FParent.FUpvalues[UpvalueIdx].ImportExportName;
    FUpvalues[Idx].ExportNames := Copy(FParent.FUpvalues[UpvalueIdx].ExportNames);
    FUpvalues[Idx].ExportNameCount := FParent.FUpvalues[UpvalueIdx].ExportNameCount;
    Exit(Idx);
  end;

  Result := -1;
end;

function TGocciaCompilerScope.AddUpvalue(const AName: string; const AIndex: UInt16;
  const AIsLocal: Boolean; const AIsConst: Boolean;
  const AIsVar: Boolean): Integer;
var
  I: Integer;
begin
  for I := 0 to FUpvalueCount - 1 do
    if (FUpvalues[I].Index = AIndex) and (FUpvalues[I].IsLocal = AIsLocal) then
    begin
      if Assigned(FCompileProfile) then
        FCompileProfile.Count(ccUpvalueListProbes, I + 1);
      Exit(I);
    end;
  if Assigned(FCompileProfile) then
    FCompileProfile.Count(ccUpvalueListProbes, FUpvalueCount);

  if FUpvalueCount >= High(UInt16) then
    raise EGocciaBytecodeLimit.CreateFmt(SErrorBytecodeUpvalueLimit,
      [High(UInt16)]);
  if FUpvalueCount >= Length(FUpvalues) then
    SetLength(FUpvalues, FUpvalueCount * 2 + 4);
  FUpvalues[FUpvalueCount].Name := AName;
  FUpvalues[FUpvalueCount].Index := AIndex;
  FUpvalues[FUpvalueCount].IsLocal := AIsLocal;
  FUpvalues[FUpvalueCount].IsConst := AIsConst;
  FUpvalues[FUpvalueCount].IsNonStrictImmutable := False;
  FUpvalues[FUpvalueCount].IsVar := AIsVar;
  FUpvalues[FUpvalueCount].IsGlobalBacked := False;
  FUpvalues[FUpvalueCount].TypeHint := sltUntyped;
  FUpvalues[FUpvalueCount].IsStrictlyTyped := False;
  FUpvalues[FUpvalueCount].HoldsUncheckedValue := False;
  FUpvalues[FUpvalueCount].ParamTypeSignature := '';
  FUpvalues[FUpvalueCount].ReturnTypeHint := sltUntyped;
  FUpvalues[FUpvalueCount].HoldsOnlyNumbers := False;
  FUpvalues[FUpvalueCount].IsImportBinding := False;
  FUpvalues[FUpvalueCount].ImportPhase := icpEvaluation;
  FUpvalues[FUpvalueCount].ImportModulePath := '';
  FUpvalues[FUpvalueCount].ImportExportName := '';
  FUpvalues[FUpvalueCount].ExportNameCount := 0;
  SetLength(FUpvalues[FUpvalueCount].ExportNames, 0);
  Result := FUpvalueCount;
  Inc(FUpvalueCount);
end;

function TGocciaCompilerScope.DeclareCallerLocal(const AName: string;
  const ASlot: UInt16; const AIsConst, AIsVar: Boolean): Integer;
var
  NextSlot: Integer;
begin
  NextSlot := FNextSlot;
  if ASlot + 1 > NextSlot then
    NextSlot := ASlot + 1;
  FNextSlot := ASlot;
  DeclareLocal(AName, AIsConst);
  FNextSlot := NextSlot;
  if FNextSlot > FMaxSlot then
    FMaxSlot := UInt16(FNextSlot);
  Result := FLocalCount - 1;
  FLocals[Result].IsVar := AIsVar;
end;

procedure TGocciaCompilerScope.SeedUpvalue(const AIndex: UInt16;
  const AName: string; const AIsConst, AIsVar,
  AIsNonStrictImmutable: Boolean);
begin
  FUpvaluesSeeded := True;
  while FUpvalueCount <= AIndex do
  begin
    if FUpvalueCount >= Length(FUpvalues) then
      SetLength(FUpvalues, FUpvalueCount * 2 + 4);
    // A gap holds an upvalue of the caller that no name reaches.
    FUpvalues[FUpvalueCount] := Default(TGocciaCompilerUpvalue);
    Inc(FUpvalueCount);
  end;
  FUpvalues[AIndex] := Default(TGocciaCompilerUpvalue);
  FUpvalues[AIndex].Name := AName;
  FUpvalues[AIndex].Index := AIndex;
  FUpvalues[AIndex].IsConst := AIsConst;
  FUpvalues[AIndex].IsVar := AIsVar;
  FUpvalues[AIndex].IsNonStrictImmutable := AIsNonStrictImmutable;
end;

procedure TGocciaCompilerScope.RecordDebugLocal(const AIndex: Integer);
begin
  FLocals[AIndex].DebugLocalIndex := -1;
  if not (Assigned(FTemplate) and Assigned(FTemplate.DebugInfo)) then
    Exit;
  FLocals[AIndex].DebugLocalIndex := FTemplate.DebugInfo.LocalCount;
  FTemplate.DebugInfo.AddLocal(FLocals[AIndex].Name, FLocals[AIndex].Slot,
    UInt32(FTemplate.CodeCount), High(UInt32));
end;

procedure TGocciaCompilerScope.AttachTemplate(
  const ATemplate: TGocciaFunctionTemplate);
var
  I: Integer;
begin
  if Assigned(FTemplate) then
    Exit;
  FTemplate := ATemplate;
  for I := 0 to FLocalCount - 1 do
    RecordDebugLocal(I);
end;

function TGocciaCompilerScope.AllocateRegister: UInt16;
begin
  if FNextSlot >= High(UInt16) then
    raise EGocciaBytecodeLimit.CreateFmt(SErrorBytecodeRegisterLimit,
      [High(UInt16)]);
  Result := UInt16(FNextSlot);
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := UInt16(FNextSlot);
end;

procedure TGocciaCompilerScope.FreeRegister;
begin
  if FNextSlot > 0 then
    Dec(FNextSlot);
end;

procedure TGocciaCompilerScope.BeginScope;
begin
  Inc(FDepth);
end;

procedure TGocciaCompilerScope.EndScope(out AClosedLocals: TArray<UInt16>;
  out AClosedCount: Integer);
begin
  SetLength(AClosedLocals, 0);
  AClosedCount := 0;
  while (FLocalCount > 0) and (FLocals[FLocalCount - 1].Depth = FDepth) do
  begin
    Dec(FLocalCount);
    Dec(FSlotLocalCounts[FLocals[FLocalCount].Slot]);
    if FLocals[FLocalCount].DebugLocalIndex >= 0 then
      FTemplate.DebugInfo.SetLocalEndPC(FLocals[FLocalCount].DebugLocalIndex,
        UInt32(FTemplate.CodeCount));
    if FLocals[FLocalCount].IsCaptured then
    begin
      if AClosedCount >= Length(AClosedLocals) then
        SetLength(AClosedLocals, AClosedCount * 2 + 4);
      AClosedLocals[AClosedCount] := FLocals[FLocalCount].Slot;
      Inc(AClosedCount);
    end;
    Dec(FNextSlot);
    ForgetTopLocalName;
  end;
  SetLength(AClosedLocals, AClosedCount);
  Dec(FDepth);
end;

function TGocciaCompilerScope.GetLocal(
  const AIndex: Integer): TGocciaCompilerLocal;
begin
  Result := FLocals[AIndex];
end;

function TGocciaCompilerScope.SlotHoldsLocal(const ASlot: UInt16): Boolean;
begin
  Result := (ASlot < Length(FSlotLocalCounts)) and
    (FSlotLocalCounts[ASlot] > 0);
end;

function TGocciaCompilerScope.LocalAt(
  const AIndex: Integer): PGocciaCompilerLocal;
begin
  Result := @FLocals[AIndex];
end;

function TGocciaCompilerScope.GetUpvalue(
  const AIndex: Integer): TGocciaCompilerUpvalue;
begin
  Result := FUpvalues[AIndex];
end;

procedure TGocciaCompilerScope.MarkCaptured(const AIndex: Integer);
begin
  FLocals[AIndex].IsCaptured := True;
end;

procedure TGocciaCompilerScope.RenameLocal(const AIndex: Integer;
  const ANewName: string);
begin
  FLocals[AIndex].Name := ANewName;
  RelinkLocalNames;
end;

procedure TGocciaCompilerScope.MarkLocalInitialized(const AIndex: Integer);
begin
  FLocals[AIndex].IsInitialized := True;
  NoteSettledLocal(AIndex);
end;

procedure TGocciaCompilerScope.AddParameterName(const AName: string);
begin
  if (AName = '') or HasParameterName(AName) then
    Exit;
  SetLength(FParameterNames, Length(FParameterNames) + 1);
  FParameterNames[High(FParameterNames)] := AName;
end;

function TGocciaCompilerScope.HasParameterName(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FParameterNames) do
    if FParameterNames[I] = AName then
    begin
      if Assigned(FCompileProfile) then
        FCompileProfile.Count(ccParameterNameProbes, I + 1);
      Exit(True);
    end;
  if Assigned(FCompileProfile) then
    FCompileProfile.Count(ccParameterNameProbes, Length(FParameterNames));
  Result := False;
end;

procedure TGocciaCompilerScope.NoteSettledLocal(const AIndex: Integer);
begin
  if FCaseClauseMarkCount = 0 then
    Exit;
  if FSettledLocalCount >= Length(FSettledLocals) then
    SetLength(FSettledLocals, FSettledLocalCount * 2 + 16);
  FSettledLocals[FSettledLocalCount] := AIndex;
  Inc(FSettledLocalCount);
end;

procedure TGocciaCompilerScope.BeginCaseClauses;
begin
  if FCaseClauseMarkCount >= Length(FCaseClauseMarks) then
    SetLength(FCaseClauseMarks, FCaseClauseMarkCount * 2 + 4);
  FCaseClauseMarks[FCaseClauseMarkCount] := FSettledLocalCount;
  Inc(FCaseClauseMarkCount);
end;

procedure TGocciaCompilerScope.ForgetCaseClauseFacts(const ADepth: Integer);
var
  I, Kept, Index: Integer;
begin
  // Only the innermost open switch reaches a clause boundary: a switch
  // nested in a clause has ended before the next clause of its own begins.
  Kept := FCaseClauseMarks[FCaseClauseMarkCount - 1];
  if Assigned(FCompileProfile) then
    FCompileProfile.Count(ccLocalListScans, FSettledLocalCount - Kept);
  for I := Kept to FSettledLocalCount - 1 do
  begin
    Index := FSettledLocals[I];
    // A local of a block that has ended is dropped: its index may hold a
    // newer local by now, which is noted again when it is settled.
    if Index >= FLocalCount then
      Continue;
    if FLocals[Index].Depth = ADepth then
    begin
      FLocals[Index].IsInitialized := False;
      if FLocals[Index].HasConstantValue then
        ClearLocalConstantValue(Index);
      Continue;
    end;
    FSettledLocals[Kept] := Index;
    Inc(Kept);
  end;
  FSettledLocalCount := Kept;
  FCaseClauseMarks[FCaseClauseMarkCount - 1] := Kept;
end;

procedure TGocciaCompilerScope.EndCaseClauses;
begin
  Dec(FCaseClauseMarkCount);
  if FCaseClauseMarkCount = 0 then
    FSettledLocalCount := 0;
end;

// AMayCreateClosure describes the subtree of the loop being entered. A loop
// nested in one that may create a closure inherits that answer, and every loop
// is asked for its own: a loop compiled while another is open is not always
// part of that loop's subtree. A finally block is compiled again at each
// return, break or continue that leaves its try statement, so a loop inside a
// finally block can be compiled in the middle of a loop it does not belong to.
procedure TGocciaCompilerScope.EnterLoop(const AMayCreateClosure: Boolean);
begin
  if FLoopDepth >= Length(FOuterLoopMayCreateClosure) then
    SetLength(FOuterLoopMayCreateClosure, FLoopDepth * 2 + 4);
  FOuterLoopMayCreateClosure[FLoopDepth] := FLoopMayCreateClosure;
  FLoopMayCreateClosure := FLoopMayCreateClosure or AMayCreateClosure;
  Inc(FLoopDepth);
end;

procedure TGocciaCompilerScope.LeaveLoop;
begin
  Dec(FLoopDepth);
  FLoopMayCreateClosure := FOuterLoopMayCreateClosure[FLoopDepth];
end;

procedure TGocciaCompilerScope.MarkDirectEvalSeen;
begin
  FDirectEvalSeen := True;
end;

threadvar
  GEvalVarObjectCounter: Integer;

procedure ReserveEvalVarObjectName(const AName: string);
var
  Suffix: Integer;
begin
  if TryStrToInt(Copy(AName, LastDelimiter(':', AName) + 1, MaxInt),
     Suffix) and (Suffix >= GEvalVarObjectCounter) then
    GEvalVarObjectCounter := Suffix + 1;
end;

procedure TGocciaCompilerScope.DeclareEvalVarObjects(
  const AParameterListIsSimple: Boolean);
var
  Suffix: string;
begin
  if not (deuSloppyDirectEval in FDirectEvalUsage) then
    Exit;
  if FEvalVarObjectSlot >= 0 then
    Exit;
  Suffix := IntToStr(GEvalVarObjectCounter);
  Inc(GEvalVarObjectCounter);
  // The parameters' environment encloses the body's, so its object is
  // declared and probed first, and the body's is the nearer with binding.
  if not AParameterListIsSimple then
  begin
    FParameterEvalVarObjectName := PARAMETER_EVAL_VAR_OBJECT_LOCAL + Suffix;
    FParameterEvalVarObjectSlot := DeclareLocal(FParameterEvalVarObjectName,
      False);
    PushWithBinding(FParameterEvalVarObjectName);
  end;
  FEvalVarObjectName := EVAL_VAR_OBJECT_LOCAL + Suffix;
  FEvalVarObjectSlot := DeclareLocal(FEvalVarObjectName, False);
  PushWithBinding(FEvalVarObjectName);
end;

// With parameter expressions, the parameters and the arguments object are
// bound in an environment outside the body's variable environment (ES2026
// §10.2.11 steps 20, 22.f and 30). The arguments object's local is declared
// as a var, so it is told apart by its flag; a body var named like a
// parameter is a var of the body's environment.
function TGocciaCompilerScope.LocalOutsideVarEnvironment(
  const AIndex: Integer): Boolean;
begin
  Result := FLocals[AIndex].IsOutsideVarEnvironment or
    ((FParameterEvalVarObjectName <> '') and
     (FLocals[AIndex].Depth = 0) and
     (FLocals[AIndex].IsArgumentsObject or
      (not FLocals[AIndex].IsVar and
       ((FLocals[AIndex].Name = 'arguments') or
        HasParameterName(FLocals[AIndex].Name)))));
end;

procedure TGocciaCompilerScope.MarkArgumentsObjectSlot(const ASlot: UInt16);
var
  I: Integer;
begin
  for I := FLocalCount - 1 downto 0 do
    if (FLocals[I].Depth = 0) and (FLocals[I].Slot = ASlot) then
    begin
      FLocals[I].IsArgumentsObject := True;
      Exit;
    end;
end;

procedure TGocciaCompilerScope.MarkOutsideVarEnvironment(
  const AIndex: Integer);
begin
  FLocals[AIndex].IsOutsideVarEnvironment := True;
end;

procedure TGocciaCompilerScope.MarkCallerFunctionBinding(
  const AIndex: Integer; const AIsVar, AIsCatchParameter: Boolean);
begin
  FLocals[AIndex].IsCallerFunctionBinding := True;
  FLocals[AIndex].IsVar := AIsVar;
  FLocals[AIndex].IsCatchParameter := AIsCatchParameter;
end;

function TGocciaCompilerScope.WithObjectCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FWithBindingCount - 1 do
    if not IsEvalVarObjectName(FWithBindingNames[I]) then
      Inc(Result);
end;

// True where a let binding or a parameter of this function may hold its
// current value somewhere other than its register, although no closure
// compiled so far has captured it. Two things cause that:
//
// - A closure created later in an enclosing loop. It is compiled after the
//   read, so the read cannot see IsCaptured, yet on the next iteration the
//   read runs after the closure has written the binding's cell.
// - Direct eval. Code it creates is a closure of the calling frame and writes
//   the caller's bindings through their cells, also after the eval call has
//   returned (ADR 0134). The parser marks the function that contains the call
//   before any of it is compiled, so the whole function keeps the copy; a
//   function without one is unaffected.
function TGocciaCompilerScope.MutableLocalsMayLeaveRegisters: Boolean;
begin
  Result := FLoopMayCreateClosure or FDirectEvalSeen or
    (deuDirectEval in FDirectEvalUsage);
end;

procedure TGocciaCompilerScope.MarkNonStrictImmutable(const AIndex: Integer);
begin
  FLocals[AIndex].IsNonStrictImmutable := True;
end;

procedure TGocciaCompilerScope.MarkGlobalBacked(const AIndex: Integer);
begin
  FLocals[AIndex].IsGlobalBacked := True;
end;

procedure TGocciaCompilerScope.SetLocalTypeHint(const AIndex: Integer;
  const ATypeHint: TGocciaLocalType);
begin
  FLocals[AIndex].TypeHint := ATypeHint;
end;

procedure TGocciaCompilerScope.SetLocalParamTypeSignature(const AIndex: Integer;
  const ASignature: string);
begin
  FLocals[AIndex].ParamTypeSignature := ASignature;
end;

procedure TGocciaCompilerScope.SetLocalReturnTypeHint(const AIndex: Integer;
  const AType: TGocciaLocalType);
begin
  FLocals[AIndex].ReturnTypeHint := AType;
end;

procedure TGocciaCompilerScope.NoteReturnType(const AType: TGocciaLocalType);
begin
  if FReturnConflict then
    Exit;
  if (AType = sltUntyped) or (AType = sltReference) then
  begin
    FReturnConflict := True;
    Exit;
  end;
  if not FReturnSeen then
  begin
    FReturnSeen := True;
    FReturnType := AType;
  end
  else if FReturnType <> AType then
  begin
    if (FReturnType in [sltInteger, sltFloat]) and
       (AType in [sltInteger, sltFloat]) then
      FReturnType := sltFloat
    else
      FReturnConflict := True;
  end;
end;

function TGocciaCompilerScope.ExpressionTypeMemo(
  const ATemplate: TGocciaFunctionTemplate): TGocciaExpressionTypeMemo;
begin
  if not Assigned(FTypeMemo) then
    FTypeMemo := TGocciaExpressionTypeMemo.Create
  else if (FTypeMemoTemplate <> ATemplate) or
          (FTypeMemoCodeCount <> ATemplate.CodeCount) then
    FTypeMemo.Clear;
  FTypeMemoTemplate := ATemplate;
  FTypeMemoCodeCount := ATemplate.CodeCount;
  Result := FTypeMemo;
end;

procedure TGocciaCompilerScope.NoteConstantEmitted(
  const ATemplate: TGocciaFunctionTemplate; const ABefore, AAfter: Integer);
begin
  if Assigned(FTypeMemo) and (FTypeMemoTemplate = ATemplate) and
     (FTypeMemoCodeCount = ABefore) then
    FTypeMemoCodeCount := AAfter;
end;

procedure TGocciaCompilerScope.ForgetBinaryResultType;
begin
  FLastBinaryExpression := nil;
end;

procedure TGocciaCompilerScope.NoteBinaryResultType(
  const AExpr: TGocciaExpression; const AType: TGocciaLocalType);
begin
  FLastBinaryExpression := AExpr;
  FLastBinaryResultType := AType;
end;

function TGocciaCompilerScope.TakeBinaryResultType(
  const AExpr: TGocciaExpression; out AType: TGocciaLocalType): Boolean;
begin
  Result := Assigned(AExpr) and (FLastBinaryExpression = AExpr);
  if Result then
    AType := FLastBinaryResultType
  else
    AType := sltUntyped;
  FLastBinaryExpression := nil;
end;

function TGocciaCompilerScope.InferredReturnType: TGocciaLocalType;
begin
  if FReturnSeen and not FReturnConflict then
    Result := FReturnType
  else
    Result := sltUntyped;
end;

procedure TGocciaCompilerScope.SetLocalStrictlyTyped(const AIndex: Integer;
  const AStrictlyTyped: Boolean);
begin
  FLocals[AIndex].IsStrictlyTyped := AStrictlyTyped;
end;

procedure TGocciaCompilerScope.SetLocalHoldsUncheckedValue(
  const AIndex: Integer; const AHoldsUncheckedValue: Boolean);
begin
  FLocals[AIndex].HoldsUncheckedValue := AHoldsUncheckedValue;
end;

procedure TGocciaCompilerScope.SetLocalArrayTyped(const AIndex: Integer;
  const AArrayTyped: Boolean);
begin
  FLocals[AIndex].IsArrayTyped := AArrayTyped;
end;

procedure TGocciaCompilerScope.SetLocalCallProvenNumeric(
  const AIndex: Integer; const AProvenNumeric: Boolean);
begin
  FLocals[AIndex].IsCallProvenNumeric := AProvenNumeric;
end;

procedure TGocciaCompilerScope.SetLocalHoldsOnlyNumbers(
  const AIndex: Integer; const AHoldsOnlyNumbers: Boolean);
begin
  FLocals[AIndex].HoldsOnlyNumbers := AHoldsOnlyNumbers;
end;

procedure TGocciaCompilerScope.SetLocalTypeAnnotation(const AIndex: Integer;
  const AAnnotation: string);
begin
  FLocals[AIndex].TypeAnnotation := AAnnotation;
end;

procedure TGocciaCompilerScope.SetLocalElementTypeAnnotation(const AIndex: Integer;
  const AAnnotation: string);
begin
  FLocals[AIndex].ElementTypeAnnotation := AAnnotation;
end;

procedure TGocciaCompilerScope.SetLocalConstantValue(const AIndex: Integer;
  const AValue: TGocciaCompileTimeValue);
begin
  FLocals[AIndex].HasConstantValue := True;
  FLocals[AIndex].ConstantValue := AValue;
  NoteSettledLocal(AIndex);
end;

procedure TGocciaCompilerScope.ClearLocalConstantValue(const AIndex: Integer);
begin
  FLocals[AIndex].HasConstantValue := False;
  ResetCompileTimeValue(FLocals[AIndex].ConstantValue, ctvkUnknown);
end;

procedure TGocciaCompilerScope.MarkImportBinding(const AIndex: Integer;
  const APhase: TGocciaImportCallPhase; const AModulePath,
  AExportName: string);
begin
  FLocals[AIndex].IsImportBinding := True;
  FLocals[AIndex].ImportPhase := APhase;
  FLocals[AIndex].ImportModulePath := AModulePath;
  FLocals[AIndex].ImportExportName := AExportName;
end;

procedure TGocciaCompilerScope.MarkExportBinding(const AIndex: Integer;
  const AExportName: string);
var
  I: Integer;
begin
  for I := 0 to FLocals[AIndex].ExportNameCount - 1 do
    if FLocals[AIndex].ExportNames[I] = AExportName then
      Exit;

  if FLocals[AIndex].ExportNameCount >= Length(FLocals[AIndex].ExportNames) then
    SetLength(FLocals[AIndex].ExportNames,
      FLocals[AIndex].ExportNameCount * 2 + 2);
  FLocals[AIndex].ExportNames[FLocals[AIndex].ExportNameCount] :=
    AExportName;
  Inc(FLocals[AIndex].ExportNameCount);
end;

function TGocciaCompilerScope.TryGetVisibleConstantValue(const AName: string;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  LocalIdx: Integer;
begin
  LocalIdx := ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    Result := FLocals[LocalIdx].HasConstantValue;
    if Result then
      AValue := FLocals[LocalIdx].ConstantValue
    else
      ResetCompileTimeValue(AValue, ctvkUnknown);
    Exit;
  end;

  if Assigned(FParent) then
    Exit(FParent.TryGetVisibleConstantValue(AName, AValue));

  ResetCompileTimeValue(AValue, ctvkUnknown);
  Result := False;
end;

function TGocciaCompilerScope.HasVisibleLocal(const AName: string): Boolean;
begin
  if ResolveLocal(AName) >= 0 then
    Exit(True);

  Result := Assigned(FParent) and FParent.HasVisibleLocal(AName);
end;

// ES2026 §19.2.1.3 EvalDeclarationInstantiation: a sloppy direct eval declares
// its vars in the variable environment of the function that calls it, where
// they shadow a same-named binding of any enclosing function. What the
// compiler knows about such a binding, its constant value or its type, holds
// for a reference only when no function containing a sloppy direct eval lies
// between the reference and the declaration, the reference's own function
// included. The parser marks those functions (deuSloppyDirectEval) before any
// of their code is compiled, so the answer covers code compiled ahead of the
// eval call too.
function TGocciaCompilerScope.DirectEvalMayShadow(const AName: string): Boolean;
var
  Scope: TGocciaCompilerScope;
begin
  Scope := Self;
  while Assigned(Scope) and (Scope.ResolveLocal(AName) < 0) do
  begin
    if deuSloppyDirectEval in Scope.FDirectEvalUsage then
      Exit(True);
    Scope := Scope.FParent;
  end;
  Result := False;
end;

function TGocciaCompilerScope.ResolvePrivatePrefix: string;
var
  S: TGocciaCompilerScope;
begin
  S := Self;
  while Assigned(S) do
  begin
    if S.FPrivatePrefix <> '' then
      Exit(S.FPrivatePrefix);
    S := S.FParent;
  end;
  Result := '';
end;

function TGocciaCompilerScope.ResolvePrivatePrefixForName(
  const AName: string): string;
var
  I: Integer;
begin
  for I := FPrivateNameCount - 1 downto 0 do
    if FPrivateNames[I] = AName then
      Exit(FPrivatePrefixes[I]);

  if Assigned(FParent) then
    Exit(FParent.ResolvePrivatePrefixForName(AName));

  Result := ResolvePrivatePrefix;
end;

procedure TGocciaCompilerScope.DeclarePrivateNamePrefix(const AName,
  APrefix: string);
begin
  if FPrivateNameCount >= Length(FPrivateNames) then
  begin
    SetLength(FPrivateNames, FPrivateNameCount * 2 + 8);
    SetLength(FPrivatePrefixes, FPrivateNameCount * 2 + 8);
  end;
  FPrivateNames[FPrivateNameCount] := AName;
  FPrivatePrefixes[FPrivateNameCount] := APrefix;
  Inc(FPrivateNameCount);
end;

function TGocciaCompilerScope.PrivateNameMark: Integer;
begin
  Result := FPrivateNameCount;
end;

function TGocciaCompilerScope.GetPrivateName(const AIndex: Integer): string;
begin
  Result := FPrivateNames[AIndex];
end;

function TGocciaCompilerScope.GetPrivateNamePrefix(
  const AIndex: Integer): string;
begin
  Result := FPrivatePrefixes[AIndex];
end;

procedure TGocciaCompilerScope.RestorePrivateNameMark(const AMark: Integer);
begin
  if AMark < 0 then
    Exit;
  if AMark < FPrivateNameCount then
    FPrivateNameCount := AMark;
end;

procedure TGocciaCompilerScope.PushWithBinding(const AHiddenName: string);
begin
  if FWithBindingCount >= Length(FWithBindingNames) then
  begin
    SetLength(FWithBindingNames, FWithBindingCount * 2 + 4);
    SetLength(FWithBindingDepths, FWithBindingCount * 2 + 4);
  end;
  FWithBindingNames[FWithBindingCount] := AHiddenName;
  FWithBindingDepths[FWithBindingCount] := FDepth;
  Inc(FWithBindingCount);
end;

procedure TGocciaCompilerScope.PopWithBinding;
begin
  if FWithBindingCount > 0 then
    Dec(FWithBindingCount);
end;

// Code compiled out of place, such as a finally block inlined at a `return`
// inside its try block, runs in an outer scope while the locals and with
// objects of the scopes it was inlined into are still declared. Suspending
// the ones deeper than ADepth lets names in that code resolve as they do at
// ADepth. The slots stay allocated, and suspensions may nest.
function TGocciaCompilerScope.SuspendBindingsDeeperThan(
  const ADepth: Integer): TGocciaCompilerSuspendedBindings;
var
  I, VisibleCount: Integer;
begin
  Result.Depth := ADepth;
  Result.LocalCount := FLocalCount;
  if Assigned(FCompileProfile) then
    FCompileProfile.Count(ccLocalListScans, FLocalCount);
  for I := 0 to FLocalCount - 1 do
    if FLocals[I].Depth > ADepth then
      Inc(FLocals[I].SuspendCount);

  // The suspended with objects come off the stack. The code compiled
  // meanwhile works on copies, so that a `with` in it cannot overwrite them.
  Result.WithBindingNames := FWithBindingNames;
  Result.WithBindingDepths := FWithBindingDepths;
  Result.WithBindingCount := FWithBindingCount;
  VisibleCount := FWithBindingCount;
  while (VisibleCount > 0) and
        (FWithBindingDepths[VisibleCount - 1] > ADepth) do
    Dec(VisibleCount);
  FWithBindingNames := Copy(FWithBindingNames, 0, VisibleCount);
  FWithBindingDepths := Copy(FWithBindingDepths, 0, VisibleCount);
  FWithBindingCount := VisibleCount;
end;

procedure TGocciaCompilerScope.ResumeBindings(
  const ASuspended: TGocciaCompilerSuspendedBindings);
var
  I: Integer;
begin
  // Locals declared since then follow the first ASuspended.LocalCount ones
  // and were never suspended.
  for I := 0 to ASuspended.LocalCount - 1 do
    if FLocals[I].Depth > ASuspended.Depth then
      Dec(FLocals[I].SuspendCount);

  FWithBindingNames := ASuspended.WithBindingNames;
  FWithBindingDepths := ASuspended.WithBindingDepths;
  FWithBindingCount := ASuspended.WithBindingCount;
end;

function TGocciaCompilerScope.GetWithBindingName(
  const AIndex: Integer): string;
begin
  Result := FWithBindingNames[AIndex];
end;

function TGocciaCompilerScope.NameProbes(
  const AName: string): TArray<string>;
var
  Scope: TGocciaCompilerScope;
  LocalIdx, I, Count: Integer;
  Found: Boolean;
  Local: PGocciaCompilerLocal;
  EntryName: string;
  EntryDepth: Integer;

  procedure Add(const AHiddenName: string);
  begin
    if Count >= Length(Result) then
      SetLength(Result, Count * 2 + 4);
    Result[Count] := AHiddenName;
    Inc(Count);
  end;

begin
  Result := nil;
  Count := 0;
  if (FWithBindingCount = 0) or (AName = 'this') or
     (Pos('#with:', AName) = 1) or IsEvalVarObjectName(AName) then
    Exit;
  Scope := Self;
  while Assigned(Scope) do
  begin
    LocalIdx := Scope.ResolveLocal(AName);
    Found := LocalIdx >= 0;
    if Found then
      Local := @Scope.FLocals[LocalIdx]
    else
      Local := nil;
    // A scope's own with bindings follow the ones it copied from its parent,
    // whose depth is -1; those are visited as the parent's own.
    for I := Scope.FWithBindingCount - 1 downto 0 do
    begin
      EntryDepth := Scope.FWithBindingDepths[I];
      if EntryDepth < 0 then
        Break;
      EntryName := Scope.FWithBindingNames[I];
      if IsEvalVarObjectName(EntryName) then
      begin
        if IsBodyEvalVarObjectName(EntryName) and Scope.FCompilingParameters
        then
          Continue;
        if Found and not (Local^.Depth < EntryDepth) and
           not (IsBodyEvalVarObjectName(EntryName) and
                Scope.LocalOutsideVarEnvironment(LocalIdx)) then
          Continue;
      end
      else if Found and (Local^.Depth > EntryDepth) then
        Continue;
      Add(EntryName);
    end;
    if Found then
      Break;
    Scope := Scope.FParent;
  end;
  SetLength(Result, Count);
end;

function TGocciaCompilerScope.GetWithBindingDepth(
  const AIndex: Integer): Integer;
begin
  Result := FWithBindingDepths[AIndex];
end;

threadvar
  GClassPrivateCounter: Integer;

function NextClassPrivatePrefix: string;
begin
  Result := IntToStr(GClassPrivateCounter) + '$';
  Inc(GClassPrivateCounter);
end;

end.
