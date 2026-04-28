unit Goccia.Compiler.PatternMatching;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Bytecode,
  Goccia.Compiler.Context;

type
  TGocciaJumpArray = array of Integer;

procedure CompileIsExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIsExpression; const ADest: UInt8);
procedure CompileMatchExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMatchExpression; const ADest: UInt8);
procedure CompilePatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaMatchPattern;
  const ADest: UInt8);
procedure DeclarePatternBindings(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaMatchPattern);
function CompileConditionWithPatternBindings(const ACtx: TGocciaCompilationContext;
  const ACondition: TGocciaExpression; const ADest: UInt8;
  out ASubjectReg: UInt8; out AFailJumps: TGocciaJumpArray): Boolean;

implementation

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.Compiler.Scope,
  Goccia.Constants.PropertyNames,
  Goccia.Scope.BindingMap,
  Goccia.Token;

procedure EmitBoolean(const ACtx: TGocciaCompilationContext; const ADest: UInt8;
  const AValue: Boolean);
begin
  if AValue then
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
end;

procedure EmitMoveToBinding(const ACtx: TGocciaCompilationContext;
  const AName: string; const AIsConst: Boolean; const ASubjectReg: UInt8);
var
  LocalIdx: Integer;
  Slot: UInt8;
begin
  LocalIdx := ACtx.Scope.ResolveLocal(AName);
  if (LocalIdx >= 0) and
     (ACtx.Scope.GetLocal(LocalIdx).Depth = ACtx.Scope.Depth) then
    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot
  else
  begin
    Slot := ACtx.Scope.DeclareLocal(AName, AIsConst);
    LocalIdx := ACtx.Scope.ResolveLocal(AName);
  end;
  EmitInstruction(ACtx, EncodeABC(OP_MOVE, Slot, ASubjectReg, 0));
  if (LocalIdx >= 0) and ACtx.Scope.GetLocal(LocalIdx).IsCaptured then
    EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, Slot, UInt16(Slot)));
end;

procedure AddPatternBindingName(const ANames: TStringList; const AName: string);
begin
  if ANames.IndexOf(AName) < 0 then
    ANames.Add(AName);
end;

procedure CollectPatternBindingNames(const APattern: TGocciaMatchPattern;
  const ANames: TStringList);
var
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaBindingMatchPattern then
    AddPatternBindingName(ANames, TGocciaBindingMatchPattern(APattern).Name)
  else if APattern is TGocciaAsMatchPattern then
  begin
    CollectPatternBindingNames(TGocciaAsMatchPattern(APattern).Pattern, ANames);
    AddPatternBindingName(ANames, TGocciaAsMatchPattern(APattern).Name);
  end
  else if APattern is TGocciaArrayMatchPattern then
  begin
    for I := 0 to TGocciaArrayMatchPattern(APattern).Elements.Count - 1 do
      CollectPatternBindingNames(TGocciaArrayMatchPattern(APattern).Elements[I],
        ANames);
    CollectPatternBindingNames(TGocciaArrayMatchPattern(APattern).RestPattern,
      ANames);
  end
  else if APattern is TGocciaObjectMatchPattern then
  begin
    for I := 0 to TGocciaObjectMatchPattern(APattern).Properties.Count - 1 do
      CollectPatternBindingNames(
        TGocciaObjectMatchPattern(APattern).Properties[I].Pattern, ANames);
    CollectPatternBindingNames(TGocciaObjectMatchPattern(APattern).RestPattern,
      ANames);
  end
  else if APattern is TGocciaExtractorMatchPattern then
  begin
    for I := 0 to TGocciaExtractorMatchPattern(APattern).Arguments.Count - 1 do
      CollectPatternBindingNames(TGocciaExtractorMatchPattern(APattern).Arguments[I],
        ANames);
    CollectPatternBindingNames(TGocciaExtractorMatchPattern(APattern).RestPattern,
      ANames);
  end
  else if APattern is TGocciaAndMatchPattern then
  begin
    for I := 0 to TGocciaAndMatchPattern(APattern).Patterns.Count - 1 do
      CollectPatternBindingNames(TGocciaAndMatchPattern(APattern).Patterns[I],
        ANames);
  end
  else if APattern is TGocciaOrMatchPattern then
  begin
    for I := 0 to TGocciaOrMatchPattern(APattern).Patterns.Count - 1 do
      CollectPatternBindingNames(TGocciaOrMatchPattern(APattern).Patterns[I],
        ANames);
  end;
end;

procedure BeginTentativeBindings(const ACtx: TGocciaCompilationContext;
  const ANames: TStringList; const ASnapshotRegs, ABindingSlots: TList<UInt8>;
  const ACapturedSlots: TList<Boolean>);
var
  I, LocalIdx: Integer;
  Slot, SnapshotReg: UInt8;
begin
  for I := 0 to ANames.Count - 1 do
  begin
    LocalIdx := ACtx.Scope.ResolveLocal(ANames[I]);
    if LocalIdx < 0 then
      Continue;

    Slot := ACtx.Scope.GetLocal(LocalIdx).Slot;
    SnapshotReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, SnapshotReg, Slot, 0));
    ASnapshotRegs.Add(SnapshotReg);
    ABindingSlots.Add(Slot);
    ACapturedSlots.Add(ACtx.Scope.GetLocal(LocalIdx).IsCaptured);
  end;
end;

procedure RestoreTentativeBindings(const ACtx: TGocciaCompilationContext;
  const ASnapshotRegs, ABindingSlots: TList<UInt8>;
  const ACapturedSlots: TList<Boolean>);
var
  I: Integer;
begin
  for I := 0 to ASnapshotRegs.Count - 1 do
  begin
    EmitInstruction(ACtx, EncodeABC(OP_MOVE, ABindingSlots[I], ASnapshotRegs[I],
      0));
    if ACapturedSlots[I] then
      EmitInstruction(ACtx, EncodeABx(OP_SET_LOCAL, ABindingSlots[I],
        UInt16(ABindingSlots[I])));
  end;
end;

procedure FreeTentativeBindings(const ACtx: TGocciaCompilationContext;
  const ASnapshotRegs: TList<UInt8>);
var
  I: Integer;
begin
  for I := ASnapshotRegs.Count - 1 downto 0 do
    ACtx.Scope.FreeRegister;
end;

procedure DeclarePatternBindings(const ACtx: TGocciaCompilationContext;
  const APattern: TGocciaMatchPattern);
var
  I, LocalIdx: Integer;
  Name: string;
  IsConst: Boolean;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaBindingMatchPattern then
  begin
    Name := TGocciaBindingMatchPattern(APattern).Name;
    IsConst := TGocciaBindingMatchPattern(APattern).DeclarationType = dtConst;
    LocalIdx := ACtx.Scope.ResolveLocal(Name);
    if (LocalIdx < 0) or
       (ACtx.Scope.GetLocal(LocalIdx).Depth <> ACtx.Scope.Depth) then
      ACtx.Scope.DeclareLocal(Name, IsConst);
  end
  else if APattern is TGocciaAsMatchPattern then
  begin
    DeclarePatternBindings(ACtx, TGocciaAsMatchPattern(APattern).Pattern);
    Name := TGocciaAsMatchPattern(APattern).Name;
    IsConst := TGocciaAsMatchPattern(APattern).DeclarationType = dtConst;
    LocalIdx := ACtx.Scope.ResolveLocal(Name);
    if (LocalIdx < 0) or
       (ACtx.Scope.GetLocal(LocalIdx).Depth <> ACtx.Scope.Depth) then
      ACtx.Scope.DeclareLocal(Name, IsConst);
  end
  else if APattern is TGocciaArrayMatchPattern then
  begin
    for I := 0 to TGocciaArrayMatchPattern(APattern).Elements.Count - 1 do
      DeclarePatternBindings(ACtx, TGocciaArrayMatchPattern(APattern).Elements[I]);
    DeclarePatternBindings(ACtx, TGocciaArrayMatchPattern(APattern).RestPattern);
  end
  else if APattern is TGocciaObjectMatchPattern then
  begin
    for I := 0 to TGocciaObjectMatchPattern(APattern).Properties.Count - 1 do
      DeclarePatternBindings(ACtx,
        TGocciaObjectMatchPattern(APattern).Properties[I].Pattern);
    DeclarePatternBindings(ACtx, TGocciaObjectMatchPattern(APattern).RestPattern);
  end
  else if APattern is TGocciaExtractorMatchPattern then
  begin
    for I := 0 to TGocciaExtractorMatchPattern(APattern).Arguments.Count - 1 do
      DeclarePatternBindings(ACtx,
        TGocciaExtractorMatchPattern(APattern).Arguments[I]);
    DeclarePatternBindings(ACtx, TGocciaExtractorMatchPattern(APattern).RestPattern);
  end
  else if APattern is TGocciaAndMatchPattern then
  begin
    for I := 0 to TGocciaAndMatchPattern(APattern).Patterns.Count - 1 do
      DeclarePatternBindings(ACtx, TGocciaAndMatchPattern(APattern).Patterns[I]);
  end
  else if APattern is TGocciaOrMatchPattern then
  begin
    if TGocciaOrMatchPattern(APattern).Patterns.Count > 0 then
      DeclarePatternBindings(ACtx, TGocciaOrMatchPattern(APattern).Patterns[0]);
  end;
end;

procedure CompileObjectPatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaObjectMatchPattern;
  const ADest: UInt8);
var
  I, EndJump: Integer;
  FailJumps: TList<Integer>;
  Prop: TGocciaObjectMatchProperty;
  KeyReg, ValueReg, ExclusionReg, RemainderReg, AdjacentReg: UInt8;
  KeyIdx: UInt16;
  HasRest: Boolean;
begin
  FailJumps := TList<Integer>.Create;
  HasRest := Assigned(APattern.RestPattern);
  if HasRest then
  begin
    ExclusionReg := ACtx.Scope.AllocateRegister;
    EmitInstruction(ACtx, EncodeABC(OP_NEW_ARRAY, ExclusionReg, 0, 0));
  end;
  try
    for I := 0 to APattern.Properties.Count - 1 do
    begin
      Prop := APattern.Properties[I];
      KeyReg := ACtx.Scope.AllocateRegister;
      ValueReg := ACtx.Scope.AllocateRegister;
      if Prop.Computed then
      begin
        ACtx.CompileExpression(Prop.KeyExpression, KeyReg);
        EmitInstruction(ACtx, EncodeABC(OP_MATCH_HAS_PROPERTY, ADest, ASubjectReg,
          KeyReg));
        FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
        EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, ValueReg, ASubjectReg, KeyReg));
      end
      else
      begin
        KeyIdx := ACtx.Template.AddConstantString(Prop.Key);
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, KeyReg, KeyIdx));
        EmitInstruction(ACtx, EncodeABC(OP_MATCH_HAS_PROPERTY, ADest, ASubjectReg,
          KeyReg));
        FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
        if KeyIdx > High(UInt8) then
          EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, ValueReg, ASubjectReg, KeyReg))
        else
          EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, ValueReg,
            ASubjectReg, UInt8(KeyIdx)));
      end;
      CompilePatternTest(ACtx, ValueReg, Prop.Pattern, ADest);
      FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
      if HasRest then
        EmitInstruction(ACtx, EncodeABC(OP_ARRAY_PUSH, ExclusionReg, KeyReg, 0));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;

    if HasRest then
    begin
      RemainderReg := ACtx.Scope.AllocateRegister;
      AdjacentReg := ACtx.Scope.AllocateRegister;
      if ExclusionReg <> AdjacentReg then
        EmitInstruction(ACtx, EncodeABC(OP_MOVE, AdjacentReg, ExclusionReg, 0));
      EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, RemainderReg,
        COLLECTION_OP_OBJECT_REST, ASubjectReg));
      CompilePatternTest(ACtx, RemainderReg, APattern.RestPattern, ADest);
      FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end
    else
      EmitBoolean(ACtx, ADest, True);
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

    for I := 0 to FailJumps.Count - 1 do
      PatchJumpTarget(ACtx, FailJumps[I]);
    EmitBoolean(ACtx, ADest, False);
    PatchJumpTarget(ACtx, EndJump);
  finally
    if HasRest then
      ACtx.Scope.FreeRegister;
    FailJumps.Free;
  end;
end;

procedure CompileArrayPatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaArrayMatchPattern;
  const ADest: UInt8);
var
  I, EndJump: Integer;
  FailJumps: TList<Integer>;
  ItemsReg, LenReg, ExpectedReg, IndexReg, ValueReg: UInt8;
  TailThisReg, TailMethodReg, TailStartReg: UInt8;
  SliceIdx: UInt16;
begin
  FailJumps := TList<Integer>.Create;
  ItemsReg := ACtx.Scope.AllocateRegister;
  LenReg := ACtx.Scope.AllocateRegister;
  ExpectedReg := ACtx.Scope.AllocateRegister;
  IndexReg := ACtx.Scope.AllocateRegister;
  ValueReg := ACtx.Scope.AllocateRegister;
  try
    EmitInstruction(ACtx, EncodeABC(OP_COLLECTION_OP, ItemsReg,
      COLLECTION_OP_TRY_ITERABLE_TO_ARRAY, ASubjectReg));
    FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_NULLISH, ItemsReg,
      GOCCIA_NULLISH_MATCH_UNDEFINED));

    EmitInstruction(ACtx, EncodeABC(OP_GET_LENGTH, LenReg, ItemsReg, 0));
    EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, ExpectedReg,
      APattern.Elements.Count));
    if Assigned(APattern.RestPattern) or APattern.HasRestWildcard then
      EmitInstruction(ACtx, EncodeABC(OP_GTE, ADest, LenReg, ExpectedReg))
    else
      EmitInstruction(ACtx, EncodeABC(OP_EQ, ADest, LenReg, ExpectedReg));
    FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));

    for I := 0 to APattern.Elements.Count - 1 do
    begin
      if not Assigned(APattern.Elements[I]) then
        Continue;
      EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, IndexReg, I));
      EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, ValueReg, ItemsReg, IndexReg));
      CompilePatternTest(ACtx, ValueReg, APattern.Elements[I], ADest);
      FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
    end;

    if Assigned(APattern.RestPattern) then
    begin
      TailThisReg := ACtx.Scope.AllocateRegister;
      TailMethodReg := ACtx.Scope.AllocateRegister;
      TailStartReg := ACtx.Scope.AllocateRegister;
      SliceIdx := ACtx.Template.AddConstantString(PROP_SLICE);
      EmitInstruction(ACtx, EncodeABC(OP_MOVE, TailThisReg, ItemsReg, 0));
      if SliceIdx > High(UInt8) then
      begin
        EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, TailStartReg, SliceIdx));
        EmitInstruction(ACtx, EncodeABC(OP_GET_INDEX, TailMethodReg,
          ItemsReg, TailStartReg));
      end
      else
        EmitInstruction(ACtx, EncodeABC(OP_GET_PROP_CONST, TailMethodReg,
          ItemsReg, UInt8(SliceIdx)));
      EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, TailStartReg,
        APattern.Elements.Count));
      EmitInstruction(ACtx, EncodeABC(OP_CALL_METHOD, TailMethodReg, 1, 0));
      CompilePatternTest(ACtx, TailMethodReg, APattern.RestPattern, ADest);
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end
    else
      EmitBoolean(ACtx, ADest, True);
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);

    for I := 0 to FailJumps.Count - 1 do
      PatchJumpTarget(ACtx, FailJumps[I]);
    EmitBoolean(ACtx, ADest, False);
    PatchJumpTarget(ACtx, EndJump);
  finally
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
    FailJumps.Free;
  end;
end;

procedure CompileRelationalPatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaRelationalMatchPattern;
  const ADest: UInt8);
var
  CandidateReg: UInt8;
  Op: TGocciaOpCode;
begin
  CandidateReg := ACtx.Scope.AllocateRegister;
  try
    ACtx.CompileExpression(APattern.Expression, CandidateReg);
    case APattern.Operator of
      gttLess:         Op := OP_LT;
      gttLessEqual:    Op := OP_LTE;
      gttGreater:      Op := OP_GT;
      gttGreaterEqual: Op := OP_GTE;
    else
      Op := OP_EQ;
    end;
    EmitInstruction(ACtx, EncodeABC(Op, ADest, ASubjectReg, CandidateReg));
  finally
    ACtx.Scope.FreeRegister;
  end;
end;

procedure CompileAndPatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaAndMatchPattern;
  const ADest: UInt8);
var
  I, EndJump: Integer;
  FailJumps: TList<Integer>;
begin
  FailJumps := TList<Integer>.Create;
  try
    for I := 0 to APattern.Patterns.Count - 1 do
    begin
      CompilePatternTest(ACtx, ASubjectReg, APattern.Patterns[I], ADest);
      FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
    end;
    EmitBoolean(ACtx, ADest, True);
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    for I := 0 to FailJumps.Count - 1 do
      PatchJumpTarget(ACtx, FailJumps[I]);
    EmitBoolean(ACtx, ADest, False);
    PatchJumpTarget(ACtx, EndJump);
  finally
    FailJumps.Free;
  end;
end;

procedure CompileOrPatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaOrMatchPattern;
  const ADest: UInt8);
var
  I, EndJump: Integer;
  SuccessJumps: TList<Integer>;
  BindingNames: TStringList;
  SnapshotRegs, BindingSlots: TList<UInt8>;
  CapturedSlots: TList<Boolean>;
begin
  SuccessJumps := TList<Integer>.Create;
  BindingNames := TStringList.Create;
  try
    BindingNames.CaseSensitive := True;
    CollectPatternBindingNames(APattern, BindingNames);
    for I := 0 to APattern.Patterns.Count - 1 do
    begin
      SnapshotRegs := TList<UInt8>.Create;
      BindingSlots := TList<UInt8>.Create;
      CapturedSlots := TList<Boolean>.Create;
      try
        BeginTentativeBindings(ACtx, BindingNames, SnapshotRegs, BindingSlots,
          CapturedSlots);
        CompilePatternTest(ACtx, ASubjectReg, APattern.Patterns[I], ADest);
        SuccessJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_TRUE, ADest));
        RestoreTentativeBindings(ACtx, SnapshotRegs, BindingSlots, CapturedSlots);
      finally
        FreeTentativeBindings(ACtx, SnapshotRegs);
        CapturedSlots.Free;
        BindingSlots.Free;
        SnapshotRegs.Free;
      end;
    end;
    EmitBoolean(ACtx, ADest, False);
    EndJump := EmitJumpInstruction(ACtx, OP_JUMP, 0);
    for I := 0 to SuccessJumps.Count - 1 do
      PatchJumpTarget(ACtx, SuccessJumps[I]);
    EmitBoolean(ACtx, ADest, True);
    PatchJumpTarget(ACtx, EndJump);
  finally
    BindingNames.Free;
    SuccessJumps.Free;
  end;
end;

procedure CompileAsPatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaAsMatchPattern;
  const ADest: UInt8);
var
  EndJump: Integer;
begin
  CompilePatternTest(ACtx, ASubjectReg, APattern.Pattern, ADest);
  EndJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest);
  EmitMoveToBinding(ACtx, APattern.Name, APattern.DeclarationType = dtConst,
    ASubjectReg);
  PatchJumpTarget(ACtx, EndJump);
end;

procedure CompilePatternTest(const ACtx: TGocciaCompilationContext;
  const ASubjectReg: UInt8; const APattern: TGocciaMatchPattern;
  const ADest: UInt8);
var
  CandidateReg: UInt8;
begin
  // TC39 Pattern Matching §30.1: compile each pattern to a boolean test,
  // moving bindings into lexical slots only after the local test succeeds.
  if APattern is TGocciaWildcardMatchPattern then
    EmitBoolean(ACtx, ADest, True)
  else if APattern is TGocciaBindingMatchPattern then
  begin
    EmitMoveToBinding(ACtx, TGocciaBindingMatchPattern(APattern).Name,
      TGocciaBindingMatchPattern(APattern).DeclarationType = dtConst,
      ASubjectReg);
    EmitBoolean(ACtx, ADest, True);
  end
  else if APattern is TGocciaValueMatchPattern then
  begin
    CandidateReg := ACtx.Scope.AllocateRegister;
    try
      ACtx.CompileExpression(TGocciaValueMatchPattern(APattern).Expression,
        CandidateReg);
      EmitInstruction(ACtx, EncodeABC(OP_MATCH_VALUE, ADest, ASubjectReg,
        CandidateReg));
    finally
      ACtx.Scope.FreeRegister;
    end;
  end
  else if APattern is TGocciaObjectMatchPattern then
    CompileObjectPatternTest(ACtx, ASubjectReg,
      TGocciaObjectMatchPattern(APattern), ADest)
  else if APattern is TGocciaArrayMatchPattern then
    CompileArrayPatternTest(ACtx, ASubjectReg,
      TGocciaArrayMatchPattern(APattern), ADest)
  else if APattern is TGocciaRelationalMatchPattern then
    CompileRelationalPatternTest(ACtx, ASubjectReg,
      TGocciaRelationalMatchPattern(APattern), ADest)
  else if APattern is TGocciaGuardMatchPattern then
  begin
    ACtx.CompileExpression(TGocciaGuardMatchPattern(APattern).Condition, ADest);
    EmitInstruction(ACtx, EncodeABC(OP_TO_BOOL, ADest, ADest, 0));
  end
  else if APattern is TGocciaAsMatchPattern then
    CompileAsPatternTest(ACtx, ASubjectReg, TGocciaAsMatchPattern(APattern),
      ADest)
  else if APattern is TGocciaAndMatchPattern then
    CompileAndPatternTest(ACtx, ASubjectReg, TGocciaAndMatchPattern(APattern),
      ADest)
  else if APattern is TGocciaOrMatchPattern then
    CompileOrPatternTest(ACtx, ASubjectReg, TGocciaOrMatchPattern(APattern),
      ADest)
  else if APattern is TGocciaNotMatchPattern then
  begin
    CompilePatternTest(ACtx, ASubjectReg,
      TGocciaNotMatchPattern(APattern).Pattern, ADest);
    EmitInstruction(ACtx, EncodeABC(OP_NOT, ADest, ADest, 0));
  end
  else
    EmitBoolean(ACtx, ADest, False);
end;

procedure CompileIsExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIsExpression; const ADest: UInt8);
var
  SubjectReg: UInt8;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, I: Integer;
begin
  SubjectReg := ACtx.Scope.AllocateRegister;
  ACtx.CompileExpression(AExpr.Subject, SubjectReg);
  ACtx.Scope.BeginScope;
  DeclarePatternBindings(ACtx, AExpr.Pattern);
  CompilePatternTest(ACtx, SubjectReg, AExpr.Pattern, ADest);
  ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
  for I := 0 to ClosedCount - 1 do
    EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[I], 0, 0));
  ACtx.Scope.FreeRegister;
end;

procedure CompileMatchExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaMatchExpression; const ADest: UInt8);
var
  SubjectReg, TestReg: UInt8;
  I, FalseJump, EndJump: Integer;
  EndJumps: TList<Integer>;
  ClosedLocals: array[0..255] of UInt8;
  ClosedCount, ClosedIndex: Integer;
  TypeErrorReg, MessageReg: UInt8;
begin
  SubjectReg := ACtx.Scope.AllocateRegister;
  TestReg := ACtx.Scope.AllocateRegister;
  EndJumps := TList<Integer>.Create;
  try
    ACtx.CompileExpression(AExpr.Subject, SubjectReg);
    for I := 0 to AExpr.Clauses.Count - 1 do
    begin
      ACtx.Scope.BeginScope;
      DeclarePatternBindings(ACtx, AExpr.Clauses[I].Pattern);
      CompilePatternTest(ACtx, SubjectReg, AExpr.Clauses[I].Pattern, TestReg);
      FalseJump := EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, TestReg);
      ACtx.CompileExpression(AExpr.Clauses[I].Expression, ADest);
      ACtx.Scope.EndScope(ClosedLocals, ClosedCount);
      for ClosedIndex := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[ClosedIndex], 0, 0));
      EndJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP, 0));
      PatchJumpTarget(ACtx, FalseJump);
      for ClosedIndex := 0 to ClosedCount - 1 do
        EmitInstruction(ACtx, EncodeABC(OP_CLOSE_UPVALUE, ClosedLocals[ClosedIndex], 0, 0));
    end;

    if Assigned(AExpr.DefaultExpression) then
      ACtx.CompileExpression(AExpr.DefaultExpression, ADest)
    else
    begin
      TypeErrorReg := ACtx.Scope.AllocateRegister;
      MessageReg := ACtx.Scope.AllocateRegister;
      EmitInstruction(ACtx, EncodeABx(OP_GET_GLOBAL, TypeErrorReg,
        ACtx.Template.AddConstantString('TypeError')));
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, MessageReg,
        ACtx.Template.AddConstantString('No pattern matched')));
      EmitInstruction(ACtx, EncodeABC(OP_CONSTRUCT, TypeErrorReg, TypeErrorReg, 1));
      EmitInstruction(ACtx, EncodeABC(OP_THROW, TypeErrorReg, 0, 0));
      ACtx.Scope.FreeRegister;
      ACtx.Scope.FreeRegister;
    end;

    EndJump := CurrentCodePosition(ACtx);
    for I := 0 to EndJumps.Count - 1 do
      PatchJumpTarget(ACtx, EndJumps[I]);
  finally
    EndJumps.Free;
    ACtx.Scope.FreeRegister;
    ACtx.Scope.FreeRegister;
  end;
end;

function CompileConditionWithPatternBindings(const ACtx: TGocciaCompilationContext;
  const ACondition: TGocciaExpression; const ADest: UInt8;
  out ASubjectReg: UInt8; out AFailJumps: TGocciaJumpArray): Boolean;
var
  FailJumps: TList<Integer>;
  I: Integer;

  function ContainsSafePatternCondition(const AExpression: TGocciaExpression): Boolean;
  var
    BinaryExpr: TGocciaBinaryExpression;
  begin
    if AExpression is TGocciaIsExpression then
      Exit(True);

    if AExpression is TGocciaBinaryExpression then
    begin
      BinaryExpr := TGocciaBinaryExpression(AExpression);
      if BinaryExpr.Operator = gttAnd then
        Exit(ContainsSafePatternCondition(BinaryExpr.Left) or
          ContainsSafePatternCondition(BinaryExpr.Right));
    end;

    Result := False;
  end;

  procedure CompileConditionPart(const AExpression: TGocciaExpression);
  var
    BinaryExpr: TGocciaBinaryExpression;
    IsExpr: TGocciaIsExpression;
  begin
    if AExpression is TGocciaBinaryExpression then
    begin
      BinaryExpr := TGocciaBinaryExpression(AExpression);
      if BinaryExpr.Operator = gttAnd then
      begin
        CompileConditionPart(BinaryExpr.Left);
        FailJumps.Add(EmitJumpInstruction(ACtx, OP_JUMP_IF_FALSE, ADest));
        CompileConditionPart(BinaryExpr.Right);
        Exit;
      end;
    end;

    if AExpression is TGocciaIsExpression then
    begin
      IsExpr := TGocciaIsExpression(AExpression);
      ACtx.CompileExpression(IsExpr.Subject, ASubjectReg);
      DeclarePatternBindings(ACtx, IsExpr.Pattern);
      CompilePatternTest(ACtx, ASubjectReg, IsExpr.Pattern, ADest);
    end
    else
      ACtx.CompileExpression(AExpression, ADest);
  end;
begin
  Result := False;
  ASubjectReg := 0;
  SetLength(AFailJumps, 0);

  if not ContainsSafePatternCondition(ACondition) then
    Exit;

  ASubjectReg := ACtx.Scope.AllocateRegister;
  ACtx.Scope.BeginScope;
  FailJumps := TList<Integer>.Create;
  try
    CompileConditionPart(ACondition);
    SetLength(AFailJumps, FailJumps.Count);
    for I := 0 to FailJumps.Count - 1 do
      AFailJumps[I] := FailJumps[I];
    Result := True;
  finally
    FailJumps.Free;
  end;
end;

end.
