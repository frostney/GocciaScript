unit Souffle.VM;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Module,
  Souffle.GarbageCollector,
  Souffle.Heap,
  Souffle.Value,
  Souffle.VM.CallFrame,
  Souffle.VM.Closure,
  Souffle.VM.Exception,
  Souffle.VM.RuntimeOperations,
  Souffle.VM.Upvalue;

type
  TSouffleVM = class
  private const
    MAX_REGISTERS  = 65536;
    INITIAL_FRAMES = 64;
  private
    FRegisters: array of TSouffleValue;
    FCallStack: TSouffleCallStack;
    FHandlerStack: TSouffleHandlerStack;
    FOpenUpvalues: TSouffleUpvalue;
    FRuntimeOps: TSouffleRuntimeOperations;
    FGC: TSouffleGarbageCollector;
    FBaseFrameCount: Integer;

    function CaptureUpvalue(const ARegisterIndex: Integer): TSouffleUpvalue;
    procedure CloseUpvalues(const ALastRegister: Integer);
    procedure MarkVMRoots;
    procedure CallClosure(const AClosure: TSouffleClosure;
      const AArgBase, AArgCount, AReturnAbsolute: Integer;
      const AReceiver: TSouffleValue);

    procedure ExecuteLoop;
    procedure ExecuteTier1(const AFrame: PSouffleVMCallFrame;
      const AInstruction: UInt32; const AOp: UInt8);
    procedure ExecuteTier2(const AFrame: PSouffleVMCallFrame;
      const AInstruction: UInt32; const AOp: UInt8);

    function MaterializeConstant(
      const AConstant: TSouffleBytecodeConstant): TSouffleValue;
  public
    constructor Create(const ARuntimeOps: TSouffleRuntimeOperations);
    destructor Destroy; override;

    function Execute(const AModule: TSouffleBytecodeModule): TSouffleValue;
    function ExecuteFunction(const AClosure: TSouffleClosure;
      const AArgs: array of TSouffleValue): TSouffleValue;
  end;

implementation

uses
  SysUtils;

{ TSouffleVM }

constructor TSouffleVM.Create(const ARuntimeOps: TSouffleRuntimeOperations);
begin
  inherited Create;
  SetLength(FRegisters, MAX_REGISTERS);
  FCallStack := TSouffleCallStack.Create(INITIAL_FRAMES);
  FHandlerStack := TSouffleHandlerStack.Create;
  FOpenUpvalues := nil;
  FRuntimeOps := ARuntimeOps;

  FGC := TSouffleGarbageCollector.Instance;
  if Assigned(FGC) then
    FGC.SetExternalRootMarker(MarkVMRoots);
end;

destructor TSouffleVM.Destroy;
begin
  if Assigned(FGC) then
    FGC.SetExternalRootMarker(nil);
  FCallStack.Free;
  FHandlerStack.Free;
  inherited;
end;

function TSouffleVM.Execute(const AModule: TSouffleBytecodeModule): TSouffleValue;
var
  TopClosure: TSouffleClosure;
begin
  if not Assigned(AModule.TopLevel) then
    Exit(SouffleNil);

  TopClosure := TSouffleClosure.Create(AModule.TopLevel);
  if Assigned(FGC) then
    FGC.AllocateObject(TopClosure);

  Result := ExecuteFunction(TopClosure, []);
end;

function TSouffleVM.ExecuteFunction(const AClosure: TSouffleClosure;
  const AArgs: array of TSouffleValue): TSouffleValue;
var
  Frame: PSouffleVMCallFrame;
  I, Base, SavedBaseFrameCount: Integer;
begin
  Base := 0;
  if not FCallStack.IsEmpty then
    Base := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Prototype.MaxRegisters;

  if Base + AClosure.Prototype.MaxRegisters > Length(FRegisters) then
    raise Exception.Create('Stack overflow: register window exceeds capacity');

  Frame := FCallStack.Push(AClosure.Prototype, AClosure, Base, Base, FHandlerStack.Count);

  for I := 0 to High(AArgs) do
    FRegisters[Base + I] := AArgs[I];
  for I := Length(AArgs) to AClosure.Prototype.MaxRegisters - 1 do
    FRegisters[Base + I] := SouffleNil;

  SavedBaseFrameCount := FBaseFrameCount;
  FBaseFrameCount := FCallStack.Count;

  try
    ExecuteLoop;
  except
    on E: ESouffleThrow do
    begin
      while FCallStack.Count >= FBaseFrameCount do
        FCallStack.Pop;
      FBaseFrameCount := SavedBaseFrameCount;
      Result := SouffleNil;
      Exit;
    end;
  end;

  FBaseFrameCount := SavedBaseFrameCount;
  Result := FRegisters[Base];
end;

procedure TSouffleVM.ExecuteLoop;
var
  Frame: PSouffleVMCallFrame;
  Instruction: UInt32;
  Op: UInt8;
begin
  while FCallStack.Count >= FBaseFrameCount do
  begin
    Frame := FCallStack.Peek;

    if Frame^.IP >= Frame^.Prototype.CodeCount then
    begin
      CloseUpvalues(Frame^.BaseRegister);
      FRegisters[Frame^.ReturnRegister] := SouffleNil;
      FCallStack.Pop;
      Continue;
    end;

    Instruction := Frame^.Prototype.GetInstruction(Frame^.IP);
    Inc(Frame^.IP);
    Op := DecodeOp(Instruction);

    if Op < OP_RT_FIRST then
      ExecuteTier1(Frame, Instruction, Op)
    else
      ExecuteTier2(Frame, Instruction, Op);
  end;
end;

procedure TSouffleVM.CallClosure(const AClosure: TSouffleClosure;
  const AArgBase, AArgCount, AReturnAbsolute: Integer;
  const AReceiver: TSouffleValue);
var
  NewBase, I: Integer;
  Frame: PSouffleVMCallFrame;
begin
  NewBase := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Prototype.MaxRegisters;
  if NewBase + AClosure.Prototype.MaxRegisters > MAX_REGISTERS then
    raise Exception.Create('Stack overflow');

  for I := 0 to AArgCount - 1 do
    FRegisters[NewBase + I] := FRegisters[AArgBase + I];
  for I := AArgCount to AClosure.Prototype.MaxRegisters - 1 do
    FRegisters[NewBase + I] := SouffleNil;

  Frame := FCallStack.Push(AClosure.Prototype, AClosure, NewBase,
    AReturnAbsolute, FHandlerStack.Count);
end;

procedure TSouffleVM.ExecuteTier1(const AFrame: PSouffleVMCallFrame;
  const AInstruction: UInt32; const AOp: UInt8);
var
  A, B: UInt8;
  Bx: UInt16;
  sBx: Int16;
  Ax: Int32;
  Base: Integer;
  Closure: TSouffleClosure;
  Upval: TSouffleUpvalue;
  Proto: TSouffleFunctionPrototype;
  Desc: TSouffleUpvalueDescriptor;
  I: Integer;
  Handler: TSouffleHandlerEntry;
begin
  Base := AFrame^.BaseRegister;

  case TSouffleOpCode(AOp) of
    OP_LOAD_CONST:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := MaterializeConstant(
        AFrame^.Prototype.GetConstant(Bx));
    end;

    OP_LOAD_NIL:
    begin
      A := DecodeA(AInstruction);
      FRegisters[Base + A] := SouffleNil;
    end;

    OP_LOAD_TRUE:
    begin
      A := DecodeA(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(True);
    end;

    OP_LOAD_FALSE:
    begin
      A := DecodeA(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(False);
    end;

    OP_LOAD_INT:
    begin
      A := DecodeA(AInstruction);
      sBx := DecodesBx(AInstruction);
      FRegisters[Base + A] := SouffleInteger(sBx);
    end;

    OP_MOVE:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRegisters[Base + B];
    end;

    OP_GET_LOCAL:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := FRegisters[Base + Bx];
    end;

    OP_SET_LOCAL:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FRegisters[Base + Bx] := FRegisters[Base + A];
    end;

    OP_GET_UPVALUE:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      if Assigned(AFrame^.Closure) then
      begin
        Upval := AFrame^.Closure.GetUpvalue(Bx);
        if Assigned(Upval) then
        begin
          if Upval.IsOpen then
            FRegisters[Base + A] := FRegisters[Upval.RegisterIndex]
          else
            FRegisters[Base + A] := Upval.Closed;
        end
        else
          FRegisters[Base + A] := SouffleNil;
      end
      else
        FRegisters[Base + A] := SouffleNil;
    end;

    OP_SET_UPVALUE:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      if Assigned(AFrame^.Closure) then
      begin
        Upval := AFrame^.Closure.GetUpvalue(Bx);
        if Assigned(Upval) then
        begin
          if Upval.IsOpen then
            FRegisters[Upval.RegisterIndex] := FRegisters[Base + A]
          else
            Upval.Closed := FRegisters[Base + A];
        end;
      end;
    end;

    OP_CLOSE_UPVALUE:
    begin
      A := DecodeA(AInstruction);
      CloseUpvalues(Base + A);
    end;

    OP_JUMP:
    begin
      Ax := DecodeAx(AInstruction);
      AFrame^.IP := AFrame^.IP + Ax;
      if Assigned(FGC) and (Ax < 0) then
        FGC.CollectIfNeeded;
    end;

    OP_JUMP_IF_TRUE:
    begin
      A := DecodeA(AInstruction);
      sBx := DecodesBx(AInstruction);
      if SouffleIsTrue(FRegisters[Base + A]) then
        AFrame^.IP := AFrame^.IP + sBx;
    end;

    OP_JUMP_IF_FALSE:
    begin
      A := DecodeA(AInstruction);
      sBx := DecodesBx(AInstruction);
      if not SouffleIsTrue(FRegisters[Base + A]) then
        AFrame^.IP := AFrame^.IP + sBx;
    end;

    OP_JUMP_IF_NIL:
    begin
      A := DecodeA(AInstruction);
      sBx := DecodesBx(AInstruction);
      if SouffleIsNil(FRegisters[Base + A]) then
        AFrame^.IP := AFrame^.IP + sBx;
    end;

    OP_JUMP_IF_NOT_NIL:
    begin
      A := DecodeA(AInstruction);
      sBx := DecodesBx(AInstruction);
      if not SouffleIsNil(FRegisters[Base + A]) then
        AFrame^.IP := AFrame^.IP + sBx;
    end;

    OP_CLOSURE:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      Proto := AFrame^.Prototype.GetFunction(Bx);
      Closure := TSouffleClosure.Create(Proto);
      if Assigned(FGC) then
        FGC.AllocateObject(Closure);

      for I := 0 to Proto.UpvalueCount - 1 do
      begin
        Desc := Proto.GetUpvalueDescriptor(I);
        if Desc.IsLocal then
          Upval := CaptureUpvalue(Base + Desc.Index)
        else if Assigned(AFrame^.Closure) then
          Upval := AFrame^.Closure.GetUpvalue(Desc.Index)
        else
          Upval := nil;
        Closure.SetUpvalue(I, Upval);
      end;

      FRegisters[Base + A] := SouffleReference(Closure);
    end;

    OP_PUSH_HANDLER:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FHandlerStack.Push(
        AFrame^.IP + Bx,
        -1,
        A,
        FCallStack.Count - 1,
        Base);
    end;

    OP_POP_HANDLER:
    begin
      if not FHandlerStack.IsEmpty then
        FHandlerStack.Pop;
    end;

    OP_THROW:
    begin
      A := DecodeA(AInstruction);
      if FHandlerStack.IsEmpty then
        raise ESouffleThrow.Create(FRegisters[Base + A]);

      Handler := FHandlerStack.Peek;
      FHandlerStack.Pop;

      while FCallStack.Count - 1 > Handler.FrameIndex do
      begin
        CloseUpvalues(FCallStack.Peek^.BaseRegister);
        FCallStack.Pop;
      end;

      FCallStack.Peek^.IP := Handler.CatchIP;
      FRegisters[Handler.BaseRegister + Handler.CatchRegister] :=
        FRegisters[Base + A];
    end;

    OP_RETURN:
    begin
      A := DecodeA(AInstruction);
      CloseUpvalues(Base);
      FRegisters[AFrame^.ReturnRegister] := FRegisters[Base + A];
      FCallStack.Pop;
    end;

    OP_RETURN_NIL:
    begin
      CloseUpvalues(Base);
      FRegisters[AFrame^.ReturnRegister] := SouffleNil;
      FCallStack.Pop;
    end;

    OP_NOP:;

    OP_LINE:;
  end;
end;

procedure TSouffleVM.ExecuteTier2(const AFrame: PSouffleVMCallFrame;
  const AInstruction: UInt32; const AOp: UInt8);
var
  A, B, C: UInt8;
  Bx: UInt16;
  Base: Integer;
  Done: Boolean;
begin
  Base := AFrame^.BaseRegister;

  case TSouffleOpCode(AOp) of
    // Arithmetic
    OP_RT_ADD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Add(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_SUB:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Subtract(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_MUL:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Multiply(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_DIV:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Divide(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_MOD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Modulo(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_POW:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Power(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_NEG:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Negate(FRegisters[Base + B]);
    end;

    // Bitwise
    OP_RT_BAND:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.BitwiseAnd(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_BOR:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.BitwiseOr(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_BXOR:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.BitwiseXor(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_SHL:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.ShiftLeft(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_SHR:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.ShiftRight(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_USHR:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.UnsignedShiftRight(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_BNOT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.BitwiseNot(FRegisters[Base + B]);
    end;

    // Comparison
    OP_RT_EQ:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Equal(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_NEQ:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.NotEqual(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_LT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.LessThan(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_GT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GreaterThan(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_LTE:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.LessThanOrEqual(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_GTE:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GreaterThanOrEqual(FRegisters[Base + B], FRegisters[Base + C]);
    end;

    // Logical / Type
    OP_RT_NOT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.LogicalNot(FRegisters[Base + B]);
    end;
    OP_RT_TYPEOF:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.TypeOf(FRegisters[Base + B]);
    end;
    OP_RT_IS_INSTANCE:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.IsInstance(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_HAS_PROPERTY:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.HasProperty(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_TO_BOOLEAN:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.ToBoolean(FRegisters[Base + B]);
    end;

    // Compound creation
    OP_RT_NEW_COMPOUND:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.CreateCompound(B);
    end;
    OP_RT_INIT_FIELD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.InitField(FRegisters[Base + A],
        AFrame^.Prototype.GetConstant(B).StringValue,
        FRegisters[Base + C]);
    end;
    OP_RT_INIT_INDEX:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.InitIndex(FRegisters[Base + A],
        FRegisters[Base + B], FRegisters[Base + C]);
    end;

    // Property access
    OP_RT_GET_PROP:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GetProperty(FRegisters[Base + B],
        AFrame^.Prototype.GetConstant(C).StringValue);
    end;
    OP_RT_SET_PROP:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.SetProperty(FRegisters[Base + A],
        AFrame^.Prototype.GetConstant(B).StringValue,
        FRegisters[Base + C]);
    end;
    OP_RT_GET_INDEX:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GetIndex(FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_SET_INDEX:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.SetIndex(FRegisters[Base + A], FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_DEL_PROP:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.DeleteProperty(FRegisters[Base + B],
        AFrame^.Prototype.GetConstant(C).StringValue);
    end;

    // Invocation
    OP_RT_CALL:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleClosure) then
        CallClosure(TSouffleClosure(FRegisters[Base + A].AsReference),
          Base + A + 1, B, Base + A, SouffleNil)
      else
      begin
        FRegisters[Base + A] := FRuntimeOps.Invoke(
          FRegisters[Base + A],
          @FRegisters[Base + A + 1],
          B,
          SouffleNil);
      end;
      if Assigned(FGC) then
        FGC.CollectIfNeeded;
    end;
    OP_RT_CALL_METHOD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleClosure) then
        CallClosure(TSouffleClosure(FRegisters[Base + A].AsReference),
          Base + A + 1, B, Base + A, FRegisters[Base + A - 1])
      else
      begin
        FRegisters[Base + A] := FRuntimeOps.Invoke(
          FRegisters[Base + A],
          @FRegisters[Base + A + 1],
          B,
          FRegisters[Base + A - 1]);
      end;
      if Assigned(FGC) then
        FGC.CollectIfNeeded;
    end;
    OP_RT_CONSTRUCT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.Construct(
        FRegisters[Base + B],
        @FRegisters[Base + B + 1],
        C);
      if Assigned(FGC) then
        FGC.CollectIfNeeded;
    end;

    // Iteration
    OP_RT_GET_ITER:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GetIterator(FRegisters[Base + B]);
    end;
    OP_RT_ITER_NEXT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.IteratorNext(FRegisters[Base + C], Done);
      FRegisters[Base + B] := SouffleBoolean(Done);
    end;
    OP_RT_SPREAD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRuntimeOps.SpreadInto(FRegisters[Base + A], FRegisters[Base + B]);
    end;

    // Modules
    OP_RT_IMPORT:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.ImportModule(
        AFrame^.Prototype.GetConstant(Bx).StringValue);
    end;
    OP_RT_EXPORT:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRuntimeOps.ExportBinding(FRegisters[Base + A],
        AFrame^.Prototype.GetConstant(Bx).StringValue);
    end;

    // Async
    OP_RT_AWAIT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.AwaitValue(FRegisters[Base + B]);
    end;

    // Globals
    OP_RT_GET_GLOBAL:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GetGlobal(
        AFrame^.Prototype.GetConstant(Bx).StringValue);
    end;
    OP_RT_SET_GLOBAL:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRuntimeOps.SetGlobal(
        AFrame^.Prototype.GetConstant(Bx).StringValue,
        FRegisters[Base + A]);
    end;

  end;
end;

function TSouffleVM.CaptureUpvalue(const ARegisterIndex: Integer): TSouffleUpvalue;
var
  Current, Prev: TSouffleUpvalue;
begin
  Prev := nil;
  Current := FOpenUpvalues;

  while Assigned(Current) and (Current.RegisterIndex > ARegisterIndex) do
  begin
    Prev := Current;
    Current := Current.Next;
  end;

  if Assigned(Current) and (Current.RegisterIndex = ARegisterIndex) then
    Exit(Current);

  Result := TSouffleUpvalue.Create(ARegisterIndex);
  if Assigned(FGC) then
    FGC.AllocateObject(Result);
  Result.Next := Current;

  if Assigned(Prev) then
    Prev.Next := Result
  else
    FOpenUpvalues := Result;
end;

procedure TSouffleVM.CloseUpvalues(const ALastRegister: Integer);
var
  Upval: TSouffleUpvalue;
begin
  while Assigned(FOpenUpvalues) and (FOpenUpvalues.RegisterIndex >= ALastRegister) do
  begin
    Upval := FOpenUpvalues;
    Upval.Close(FRegisters[Upval.RegisterIndex]);
    FOpenUpvalues := Upval.Next;
  end;
end;

procedure TSouffleVM.MarkVMRoots;
var
  I: Integer;
  HighWater: Integer;
begin
  if FCallStack.IsEmpty then
    Exit;

  HighWater := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Prototype.MaxRegisters;
  for I := 0 to HighWater - 1 do
    if SouffleIsReference(FRegisters[I]) and Assigned(FRegisters[I].AsReference) then
      FRegisters[I].AsReference.MarkReferences;

  if Assigned(FOpenUpvalues) then
    FOpenUpvalues.MarkReferences;
end;

function TSouffleVM.MaterializeConstant(
  const AConstant: TSouffleBytecodeConstant): TSouffleValue;
var
  Str: TSouffleString;
begin
  case AConstant.Kind of
    bckNil:     Result := SouffleNil;
    bckTrue:    Result := SouffleBoolean(True);
    bckFalse:   Result := SouffleBoolean(False);
    bckInteger: Result := SouffleInteger(AConstant.IntValue);
    bckFloat:   Result := SouffleFloat(AConstant.FloatValue);
    bckString:
    begin
      Str := TSouffleString.Create(AConstant.StringValue);
      if Assigned(FGC) then
        FGC.AllocateObject(Str);
      Result := SouffleReference(Str);
    end;
  else
    Result := SouffleNil;
  end;
end;

end.
