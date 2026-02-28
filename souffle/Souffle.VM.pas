unit Souffle.VM;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Module,
  Souffle.Compound,
  Souffle.GarbageCollector,
  Souffle.Heap,
  Souffle.Value,
  Souffle.VM.CallFrame,
  Souffle.VM.Closure,
  Souffle.VM.Exception,
  Souffle.VM.NativeFunction,
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

    FArrayMetatable: TSouffleHeapObject;
    FTableMetatable: TSouffleHeapObject;

    function CaptureUpvalue(const ARegisterIndex: Integer): TSouffleUpvalue;
    procedure CloseUpvalues(const ALastRegister: Integer);
    procedure MarkVMRoots;
    procedure CallClosure(const AClosure: TSouffleClosure;
      const AArgBase, AArgCount, AReturnAbsolute: Integer;
      const AReceiver: TSouffleValue);

    procedure ExecuteLoop;
    procedure ExecuteCoreOp(const AFrame: PSouffleVMCallFrame;
      const AInstruction: UInt32; const AOp: UInt8);
    procedure ExecuteRuntimeOp(const AFrame: PSouffleVMCallFrame;
      const AInstruction: UInt32; const AOp: UInt8);

    function MaterializeConstant(
      const AConstant: TSouffleBytecodeConstant): TSouffleValue;

    function MetatableGet(const AObject: TSouffleHeapObject;
      const AKey: string; out AValue: TSouffleValue): Boolean;
  public
    constructor Create(const ARuntimeOps: TSouffleRuntimeOperations);
    destructor Destroy; override;

    function Execute(const AModule: TSouffleBytecodeModule): TSouffleValue;
    function ExecuteFunction(const AClosure: TSouffleClosure;
      const AArgs: array of TSouffleValue): TSouffleValue;

    property ArrayMetatable: TSouffleHeapObject read FArrayMetatable write FArrayMetatable;
    property TableMetatable: TSouffleHeapObject read FTableMetatable write FTableMetatable;
  end;

implementation

uses
  Math,
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
  FArrayMetatable := nil;
  FTableMetatable := nil;

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
  I, Base, SavedBaseFrameCount, ArgsToCopy, ArgOffset, RequiredSpace: Integer;
begin
  Base := 0;
  if not FCallStack.IsEmpty then
    Base := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Template.MaxRegisters;

  RequiredSpace := AClosure.Template.MaxRegisters;
  if Length(AArgs) > RequiredSpace then
    RequiredSpace := Length(AArgs);
  if Base + RequiredSpace > Length(FRegisters) then
    raise Exception.Create('Stack overflow: register window exceeds capacity');

  Frame := FCallStack.Push(AClosure.Template, AClosure, Base, Base, FHandlerStack.Count);

  if AClosure.Template.HasReceiver then
    ArgOffset := 1
  else
    ArgOffset := 0;

  ArgsToCopy := Length(AArgs);
  if ArgsToCopy > AClosure.Template.MaxRegisters then
    ArgsToCopy := AClosure.Template.MaxRegisters;

  for I := 0 to ArgsToCopy - 1 do
    FRegisters[Base + I] := AArgs[I];
  for I := ArgsToCopy to AClosure.Template.MaxRegisters - 1 do
    FRegisters[Base + I] := SouffleNil;

  for I := AClosure.Template.MaxRegisters to Length(AArgs) - 1 do
    FRegisters[Base + I] := AArgs[I];

  Frame^.ArgCount := Length(AArgs) - ArgOffset;
  Frame^.ArgSourceBase := Base + ArgOffset;

  SavedBaseFrameCount := FBaseFrameCount;
  FBaseFrameCount := FCallStack.Count;

  try
    ExecuteLoop;
  except
    on E: ESouffleThrow do
    begin
      while FCallStack.Count >= FBaseFrameCount do
      begin
        CloseUpvalues(FCallStack.Peek^.BaseRegister);
        FCallStack.Pop;
      end;
      FBaseFrameCount := SavedBaseFrameCount;
      raise;
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

    if Frame^.IP >= Frame^.Template.CodeCount then
    begin
      CloseUpvalues(Frame^.BaseRegister);
      FRegisters[Frame^.ReturnRegister] := SouffleNil;
      FCallStack.Pop;
      Continue;
    end;

    Instruction := Frame^.Template.GetInstruction(Frame^.IP);
    Inc(Frame^.IP);
    Op := DecodeOp(Instruction);

    if Op < OP_RT_FIRST then
      ExecuteCoreOp(Frame, Instruction, Op)
    else
      ExecuteRuntimeOp(Frame, Instruction, Op);
  end;
end;

procedure TSouffleVM.CallClosure(const AClosure: TSouffleClosure;
  const AArgBase, AArgCount, AReturnAbsolute: Integer;
  const AReceiver: TSouffleValue);
var
  NewBase, I, ArgsToCopy, ArgOffset: Integer;
  Frame: PSouffleVMCallFrame;
begin
  NewBase := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Template.MaxRegisters;
  if NewBase + AClosure.Template.MaxRegisters > MAX_REGISTERS then
    raise Exception.Create('Stack overflow');

  if AClosure.Template.HasReceiver then
    ArgOffset := 1
  else
    ArgOffset := 0;

  if AClosure.Template.HasReceiver then
    FRegisters[NewBase] := AReceiver;

  ArgsToCopy := AArgCount;
  if ArgsToCopy + ArgOffset > AClosure.Template.MaxRegisters then
    ArgsToCopy := AClosure.Template.MaxRegisters - ArgOffset;

  for I := 0 to ArgsToCopy - 1 do
    FRegisters[NewBase + ArgOffset + I] := FRegisters[AArgBase + I];
  for I := ArgsToCopy + ArgOffset to AClosure.Template.MaxRegisters - 1 do
    FRegisters[NewBase + I] := SouffleNil;

  Frame := FCallStack.Push(AClosure.Template, AClosure, NewBase,
    AReturnAbsolute, FHandlerStack.Count);
  Frame^.ArgCount := AArgCount;
  Frame^.ArgSourceBase := AArgBase;
end;

procedure TSouffleVM.ExecuteCoreOp(const AFrame: PSouffleVMCallFrame;
  const AInstruction: UInt32; const AOp: UInt8);
var
  A, B, C: UInt8;
  Bx: UInt16;
  sBx: Int16;
  Ax: Int32;
  Base: Integer;
  Closure: TSouffleClosure;
  Upval: TSouffleUpvalue;
  Tmpl: TSouffleFunctionTemplate;
  Desc: TSouffleUpvalueDescriptor;
  I: Integer;
  Handler: TSouffleHandlerEntry;
  Arr, RestArr: TSouffleArray;
  RestCount: Integer;
  Tbl: TSouffleTable;
  TblVal: TSouffleValue;
begin
  Base := AFrame^.BaseRegister;

  case TSouffleOpCode(AOp) of
    OP_LOAD_CONST:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := MaterializeConstant(
        AFrame^.Template.GetConstant(Bx));
    end;

    OP_LOAD_NIL:
    begin
      A := DecodeA(AInstruction);
      FRegisters[Base + A] := SouffleNil;
    end;

    OP_LOAD_NULL:
    begin
      A := DecodeA(AInstruction);
      FRegisters[Base + A] := SouffleNull;
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
      if SouffleIsNil(FRegisters[Base + A]) or
         SouffleIsNull(FRegisters[Base + A]) then
        AFrame^.IP := AFrame^.IP + sBx;
    end;

    OP_JUMP_IF_NOT_NIL:
    begin
      A := DecodeA(AInstruction);
      sBx := DecodesBx(AInstruction);
      if not SouffleIsNil(FRegisters[Base + A]) and
         not SouffleIsNull(FRegisters[Base + A]) then
        AFrame^.IP := AFrame^.IP + sBx;
    end;

    OP_CLOSURE:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      Tmpl := AFrame^.Template.GetFunction(Bx);
      Closure := TSouffleClosure.Create(Tmpl);
      if Assigned(FGC) then
        FGC.AllocateObject(Closure);

      for I := 0 to Tmpl.UpvalueCount - 1 do
      begin
        Desc := Tmpl.GetUpvalueDescriptor(I);
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

    OP_NEW_ARRAY:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      Arr := TSouffleArray.Create(B);
      Arr.Metatable := FArrayMetatable;
      if Assigned(FGC) then
        FGC.AllocateObject(Arr);
      FRegisters[Base + A] := SouffleReference(Arr);
    end;

    OP_ARRAY_PUSH:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleArray) then
        TSouffleArray(FRegisters[Base + A].AsReference).Push(FRegisters[Base + B]);
    end;

    OP_ARRAY_GET:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         (FRegisters[Base + B].AsReference is TSouffleArray) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := TSouffleArray(
          FRegisters[Base + B].AsReference).Get(
            Integer(FRegisters[Base + C].AsInteger))
      else
        FRegisters[Base + A] := FRuntimeOps.GetIndex(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;

    OP_ARRAY_SET:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleArray) and
         (FRegisters[Base + B].Kind = svkInteger) then
        TSouffleArray(FRegisters[Base + A].AsReference).Put(
          Integer(FRegisters[Base + B].AsInteger), FRegisters[Base + C])
      else
        FRuntimeOps.SetIndex(
          FRegisters[Base + A], FRegisters[Base + B], FRegisters[Base + C]);
    end;

    OP_NEW_TABLE:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      Tbl := TSouffleTable.Create(B);
      Tbl.Metatable := FTableMetatable;
      if Assigned(FGC) then
        FGC.AllocateObject(Tbl);
      FRegisters[Base + A] := SouffleReference(Tbl);
    end;

    OP_TABLE_GET:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         Assigned(FRegisters[Base + B].AsReference) then
      begin
        if FRegisters[Base + B].AsReference is TSouffleTable then
        begin
          Tbl := TSouffleTable(FRegisters[Base + B].AsReference);
          if Tbl.Get(AFrame^.Template.GetConstant(C).StringValue, TblVal) then
          begin
            FRegisters[Base + A] := TblVal;
            Exit;
          end;
        end;
        if MetatableGet(FRegisters[Base + B].AsReference,
             AFrame^.Template.GetConstant(C).StringValue, TblVal) then
        begin
          FRegisters[Base + A] := TblVal;
          Exit;
        end;
      end;
      FRegisters[Base + A] := FRuntimeOps.GetProperty(
        FRegisters[Base + B], AFrame^.Template.GetConstant(C).StringValue);
    end;

    OP_TABLE_SET:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleTable) then
        TSouffleTable(FRegisters[Base + A].AsReference).Put(
          AFrame^.Template.GetConstant(B).StringValue, FRegisters[Base + C])
      else
        FRuntimeOps.SetProperty(
          FRegisters[Base + A], AFrame^.Template.GetConstant(B).StringValue,
          FRegisters[Base + C]);
    end;

    OP_TABLE_DELETE:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleTable) then
        TSouffleTable(FRegisters[Base + A].AsReference).Delete(
          AFrame^.Template.GetConstant(Bx).StringValue)
      else
        FRuntimeOps.DeleteProperty(
          FRegisters[Base + A], AFrame^.Template.GetConstant(Bx).StringValue);
    end;

    OP_GET_LENGTH:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         Assigned(FRegisters[Base + B].AsReference) then
      begin
        if FRegisters[Base + B].AsReference is TSouffleArray then
          FRegisters[Base + A] := SouffleInteger(
            TSouffleArray(FRegisters[Base + B].AsReference).Count)
        else if FRegisters[Base + B].AsReference is TSouffleTable then
          FRegisters[Base + A] := SouffleInteger(
            TSouffleTable(FRegisters[Base + B].AsReference).Count)
        else if FRegisters[Base + B].AsReference is TSouffleString then
          FRegisters[Base + A] := SouffleInteger(
            Length(TSouffleString(FRegisters[Base + B].AsReference).Value))
        else
          FRegisters[Base + A] := FRuntimeOps.GetProperty(
            FRegisters[Base + B], 'length');
      end
      else
        FRegisters[Base + A] := SouffleInteger(0);
    end;

    OP_ARG_COUNT:
    begin
      A := DecodeA(AInstruction);
      FRegisters[Base + A] := SouffleInteger(AFrame^.ArgCount);
    end;

    OP_COLLECT_REST:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      RestCount := AFrame^.ArgCount - Integer(B);
      if RestCount < 0 then
        RestCount := 0;
      RestArr := TSouffleArray.Create(RestCount);
      RestArr.Metatable := FArrayMetatable;
      if Assigned(FGC) then
        FGC.AllocateObject(RestArr);
      for I := 0 to RestCount - 1 do
        RestArr.Push(FRegisters[AFrame^.ArgSourceBase + Integer(B) + I]);
      FRegisters[Base + A] := SouffleReference(RestArr);
    end;

    OP_NOP:;

    OP_LINE:;
  end;
end;

procedure TSouffleVM.ExecuteRuntimeOp(const AFrame: PSouffleVMCallFrame;
  const AInstruction: UInt32; const AOp: UInt8);
var
  A, B, C: UInt8;
  Bx: UInt16;
  Base: Integer;
  Done: Boolean;
  FloatB, FloatC: Double;
begin
  Base := AFrame^.BaseRegister;

  case TSouffleOpCode(AOp) of
    // Arithmetic — inline integer/float fast paths, fallback to runtime for strings/objects
    OP_RT_ADD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleInteger(
          FRegisters[Base + B].AsInteger + FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleFloat(
          SouffleAsNumber(FRegisters[Base + B]) + SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.Add(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_SUB:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleInteger(
          FRegisters[Base + B].AsInteger - FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleFloat(
          SouffleAsNumber(FRegisters[Base + B]) - SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.Subtract(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_MUL:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleInteger(
          FRegisters[Base + B].AsInteger * FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleFloat(
          SouffleAsNumber(FRegisters[Base + B]) * SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.Multiply(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_DIV:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if SouffleIsNumeric(FRegisters[Base + B]) and
         SouffleIsNumeric(FRegisters[Base + C]) then
      begin
        FloatB := SouffleAsNumber(FRegisters[Base + B]);
        FloatC := SouffleAsNumber(FRegisters[Base + C]);
        if FloatC = 0.0 then
        begin
          if FloatB = 0.0 then
            FRegisters[Base + A] := SouffleFloat(NaN)
          else if FloatB > 0 then
            FRegisters[Base + A] := SouffleFloat(Infinity)
          else
            FRegisters[Base + A] := SouffleFloat(NegInfinity);
        end
        else
          FRegisters[Base + A] := SouffleFloat(FloatB / FloatC);
      end
      else
        FRegisters[Base + A] := FRuntimeOps.Divide(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_MOD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) and
         (FRegisters[Base + C].AsInteger <> 0) then
        FRegisters[Base + A] := SouffleInteger(
          FRegisters[Base + B].AsInteger mod FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
      begin
        FloatC := SouffleAsNumber(FRegisters[Base + C]);
        if FloatC = 0.0 then
          FRegisters[Base + A] := SouffleFloat(NaN)
        else
          FRegisters[Base + A] := SouffleFloat(
            FMod(SouffleAsNumber(FRegisters[Base + B]), FloatC));
      end
      else
        FRegisters[Base + A] := FRuntimeOps.Modulo(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_POW:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if SouffleIsNumeric(FRegisters[Base + B]) and
         SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleFloat(
          Math.Power(SouffleAsNumber(FRegisters[Base + B]),
                     SouffleAsNumber(FRegisters[Base + C])))
      else
        FRegisters[Base + A] := FRuntimeOps.Power(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_NEG:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      if FRegisters[Base + B].Kind = svkInteger then
        FRegisters[Base + A] := SouffleInteger(-FRegisters[Base + B].AsInteger)
      else if FRegisters[Base + B].Kind = svkFloat then
        FRegisters[Base + A] := SouffleFloat(-FRegisters[Base + B].AsFloat)
      else
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

    // Comparison — inline primitive fast paths, fallback to runtime for references
    OP_RT_EQ:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if FRegisters[Base + B].Kind <> FRegisters[Base + C].Kind then
      begin
        if SouffleIsNumeric(FRegisters[Base + B]) and
           SouffleIsNumeric(FRegisters[Base + C]) then
          FRegisters[Base + A] := SouffleBoolean(
            SouffleAsNumber(FRegisters[Base + B]) = SouffleAsNumber(FRegisters[Base + C]))
        else
          FRegisters[Base + A] := FRuntimeOps.Equal(
            FRegisters[Base + B], FRegisters[Base + C]);
      end
      else
        case FRegisters[Base + B].Kind of
          svkNil, svkNull:
            FRegisters[Base + A] := SouffleBoolean(True);
          svkBoolean:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsBoolean = FRegisters[Base + C].AsBoolean);
          svkInteger:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsInteger = FRegisters[Base + C].AsInteger);
          svkFloat:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsFloat = FRegisters[Base + C].AsFloat);
        else
          FRegisters[Base + A] := FRuntimeOps.Equal(
            FRegisters[Base + B], FRegisters[Base + C]);
        end;
    end;
    OP_RT_NEQ:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if FRegisters[Base + B].Kind <> FRegisters[Base + C].Kind then
      begin
        if SouffleIsNumeric(FRegisters[Base + B]) and
           SouffleIsNumeric(FRegisters[Base + C]) then
          FRegisters[Base + A] := SouffleBoolean(
            SouffleAsNumber(FRegisters[Base + B]) <> SouffleAsNumber(FRegisters[Base + C]))
        else
          FRegisters[Base + A] := FRuntimeOps.NotEqual(
            FRegisters[Base + B], FRegisters[Base + C]);
      end
      else
        case FRegisters[Base + B].Kind of
          svkNil, svkNull:
            FRegisters[Base + A] := SouffleBoolean(False);
          svkBoolean:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsBoolean <> FRegisters[Base + C].AsBoolean);
          svkInteger:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsInteger <> FRegisters[Base + C].AsInteger);
          svkFloat:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsFloat <> FRegisters[Base + C].AsFloat);
        else
          FRegisters[Base + A] := FRuntimeOps.NotEqual(
            FRegisters[Base + B], FRegisters[Base + C]);
        end;
    end;
    OP_RT_LT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleBoolean(
          FRegisters[Base + B].AsInteger < FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleBoolean(
          SouffleAsNumber(FRegisters[Base + B]) < SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.LessThan(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_GT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleBoolean(
          FRegisters[Base + B].AsInteger > FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleBoolean(
          SouffleAsNumber(FRegisters[Base + B]) > SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.GreaterThan(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_LTE:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleBoolean(
          FRegisters[Base + B].AsInteger <= FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleBoolean(
          SouffleAsNumber(FRegisters[Base + B]) <= SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.LessThanOrEqual(
          FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_GTE:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      if (FRegisters[Base + B].Kind = svkInteger) and
         (FRegisters[Base + C].Kind = svkInteger) then
        FRegisters[Base + A] := SouffleBoolean(
          FRegisters[Base + B].AsInteger >= FRegisters[Base + C].AsInteger)
      else if SouffleIsNumeric(FRegisters[Base + B]) and
              SouffleIsNumeric(FRegisters[Base + C]) then
        FRegisters[Base + A] := SouffleBoolean(
          SouffleAsNumber(FRegisters[Base + B]) >= SouffleAsNumber(FRegisters[Base + C]))
      else
        FRegisters[Base + A] := FRuntimeOps.GreaterThanOrEqual(
          FRegisters[Base + B], FRegisters[Base + C]);
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

    // Property access
    OP_RT_GET_PROP:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GetProperty(FRegisters[Base + B],
        AFrame^.Template.GetConstant(C).StringValue);
    end;
    OP_RT_SET_PROP:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.SetProperty(FRegisters[Base + A],
        AFrame^.Template.GetConstant(B).StringValue,
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
        AFrame^.Template.GetConstant(C).StringValue);
    end;

    // Invocation
    OP_RT_CALL:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         Assigned(FRegisters[Base + A].AsReference) then
      begin
        if FRegisters[Base + A].AsReference is TSouffleClosure then
        begin
          CallClosure(TSouffleClosure(FRegisters[Base + A].AsReference),
            Base + A + 1, B, Base + A, SouffleNil);
        end
        else if FRegisters[Base + A].AsReference is TSouffleNativeFunction then
        begin
          FRegisters[Base + A] := TSouffleNativeFunction(
            FRegisters[Base + A].AsReference).Invoke(
              SouffleNil, @FRegisters[Base + A + 1], B);
        end
        else
          FRegisters[Base + A] := FRuntimeOps.Invoke(
            FRegisters[Base + A], @FRegisters[Base + A + 1], B, SouffleNil);
      end
      else
        FRegisters[Base + A] := FRuntimeOps.Invoke(
          FRegisters[Base + A], @FRegisters[Base + A + 1], B, SouffleNil);
      if Assigned(FGC) then
        FGC.CollectIfNeeded;
    end;
    OP_RT_CALL_METHOD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         Assigned(FRegisters[Base + A].AsReference) then
      begin
        if FRegisters[Base + A].AsReference is TSouffleClosure then
        begin
          CallClosure(TSouffleClosure(FRegisters[Base + A].AsReference),
            Base + A + 1, B, Base + A, FRegisters[Base + A - 1]);
        end
        else if FRegisters[Base + A].AsReference is TSouffleNativeFunction then
        begin
          FRegisters[Base + A] := TSouffleNativeFunction(
            FRegisters[Base + A].AsReference).Invoke(
              FRegisters[Base + A - 1], @FRegisters[Base + A + 1], B);
        end
        else
          FRegisters[Base + A] := FRuntimeOps.Invoke(
            FRegisters[Base + A], @FRegisters[Base + A + 1], B,
            FRegisters[Base + A - 1]);
      end
      else
        FRegisters[Base + A] := FRuntimeOps.Invoke(
          FRegisters[Base + A], @FRegisters[Base + A + 1], B,
          FRegisters[Base + A - 1]);
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
        AFrame^.Template.GetConstant(Bx).StringValue);
    end;
    OP_RT_EXPORT:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRuntimeOps.ExportBinding(FRegisters[Base + A],
        AFrame^.Template.GetConstant(Bx).StringValue);
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
        AFrame^.Template.GetConstant(Bx).StringValue);
    end;
    OP_RT_SET_GLOBAL:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRuntimeOps.SetGlobal(
        AFrame^.Template.GetConstant(Bx).StringValue,
        FRegisters[Base + A]);
    end;
    OP_RT_HAS_GLOBAL:
    begin
      A := DecodeA(AInstruction); Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(FRuntimeOps.HasGlobal(
        AFrame^.Template.GetConstant(Bx).StringValue));
    end;

    OP_RT_DEF_GETTER:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.DefineGetter(FRegisters[Base + A],
        AFrame^.Template.GetConstant(B).StringValue, FRegisters[Base + C]);
    end;

    OP_RT_DEF_SETTER:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRuntimeOps.DefineSetter(FRegisters[Base + A],
        AFrame^.Template.GetConstant(B).StringValue, FRegisters[Base + C]);
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
  Frame: PSouffleVMCallFrame;
begin
  if FCallStack.IsEmpty then
    Exit;

  HighWater := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Template.MaxRegisters;
  for I := 0 to HighWater - 1 do
    if SouffleIsReference(FRegisters[I]) and Assigned(FRegisters[I].AsReference) then
      FRegisters[I].AsReference.MarkReferences;

  for I := 0 to FCallStack.Count - 1 do
  begin
    Frame := FCallStack.GetFrame(I);
    if Assigned(Frame^.Closure) then
      Frame^.Closure.MarkReferences;
  end;

  if Assigned(FOpenUpvalues) then
    FOpenUpvalues.MarkReferences;

  if Assigned(FArrayMetatable) and not FArrayMetatable.GCMarked then
    FArrayMetatable.MarkReferences;
  if Assigned(FTableMetatable) and not FTableMetatable.GCMarked then
    FTableMetatable.MarkReferences;
end;

function TSouffleVM.MetatableGet(const AObject: TSouffleHeapObject;
  const AKey: string; out AValue: TSouffleValue): Boolean;
var
  Meta: TSouffleHeapObject;
begin
  Meta := AObject.Metatable;
  if Assigned(Meta) and (Meta is TSouffleTable) then
    Exit(TSouffleTable(Meta).Get(AKey, AValue));
  Result := False;
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
