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

    FArrayDelegate: TSouffleHeapObject;
    FRecordDelegate: TSouffleHeapObject;

    function CaptureUpvalue(const ARegisterIndex: Integer): TSouffleUpvalue;
    procedure CloseUpvalues(const ALastRegister: Integer);
    procedure MarkVMRoots;
    procedure CallClosure(const AClosure: TSouffleClosure;
      const AArgBase, AArgCount, AReturnAbsolute: Integer;
      const AReceiver: TSouffleValue);
    function InvokeWithSpread(const ACallee, AArgsArray,
      AReceiver: TSouffleValue): TSouffleValue;

    procedure ExecuteLoop;
    procedure ExecuteCoreOp(const AFrame: PSouffleVMCallFrame;
      const AInstruction: UInt32; const AOp: UInt8);
    procedure ExecuteRuntimeOp(const AFrame: PSouffleVMCallFrame;
      const AInstruction: UInt32; const AOp: UInt8);

    function MaterializeConstant(
      const AConstant: TSouffleBytecodeConstant): TSouffleValue;

    function DelegateGet(const AObject: TSouffleHeapObject;
      const AKey: string; out AValue: TSouffleValue): Boolean;
  public
    constructor Create(const ARuntimeOps: TSouffleRuntimeOperations);
    destructor Destroy; override;

    function Execute(const AModule: TSouffleBytecodeModule): TSouffleValue;
    function ExecuteFunction(const AClosure: TSouffleClosure;
      const AArgs: array of TSouffleValue): TSouffleValue;

    property ArrayDelegate: TSouffleHeapObject read FArrayDelegate write FArrayDelegate;
    property RecordDelegate: TSouffleHeapObject read FRecordDelegate write FRecordDelegate;
  end;

implementation

uses
  Math,
  SysUtils;

{ TSouffleVM }

constructor TSouffleVM.Create(const ARuntimeOps: TSouffleRuntimeOperations);
begin
  inherited Create;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  SetLength(FRegisters, MAX_REGISTERS);
  FCallStack := TSouffleCallStack.Create(INITIAL_FRAMES);
  FHandlerStack := TSouffleHandlerStack.Create;
  FOpenUpvalues := nil;
  FRuntimeOps := ARuntimeOps;
  FArrayDelegate := nil;
  FRecordDelegate := nil;

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

  Result := ExecuteFunction(TopClosure, [SouffleNil]);
end;

function TSouffleVM.ExecuteFunction(const AClosure: TSouffleClosure;
  const AArgs: array of TSouffleValue): TSouffleValue;
var
  Frame: PSouffleVMCallFrame;
  I, Base, SavedBaseFrameCount, ArgsToCopy, RequiredSpace: Integer;
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

  ArgsToCopy := Length(AArgs);
  if ArgsToCopy > AClosure.Template.MaxRegisters then
    ArgsToCopy := AClosure.Template.MaxRegisters;

  for I := 0 to ArgsToCopy - 1 do
    FRegisters[Base + I] := AArgs[I];
  for I := ArgsToCopy to AClosure.Template.MaxRegisters - 1 do
    FRegisters[Base + I] := SouffleNil;

  for I := AClosure.Template.MaxRegisters to Length(AArgs) - 1 do
    FRegisters[Base + I] := AArgs[I];

  if Length(AArgs) > 1 then
  begin
    Frame^.ArgCount := Length(AArgs) - 1;
    Frame^.ArgSourceBase := Base + 1;
  end
  else
  begin
    Frame^.ArgCount := 0;
    Frame^.ArgSourceBase := Base + 1;
  end;

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
  HandlerEntry: TSouffleHandlerEntry;
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

    try
      if Op < OP_RT_FIRST then
        ExecuteCoreOp(Frame, Instruction, Op)
      else
        ExecuteRuntimeOp(Frame, Instruction, Op);
    except
      on E: ESouffleThrow do
      begin
        if FHandlerStack.IsEmpty then
          raise;

        HandlerEntry := FHandlerStack.Peek;
        FHandlerStack.Pop;

        while FCallStack.Count - 1 > HandlerEntry.FrameIndex do
        begin
          CloseUpvalues(FCallStack.Peek^.BaseRegister);
          FCallStack.Pop;
        end;

        FCallStack.Peek^.IP := HandlerEntry.CatchIP;
        FRegisters[HandlerEntry.BaseRegister + HandlerEntry.CatchRegister] :=
          E.ThrownValue;
      end;
    end;
  end;
end;

procedure TSouffleVM.CallClosure(const AClosure: TSouffleClosure;
  const AArgBase, AArgCount, AReturnAbsolute: Integer;
  const AReceiver: TSouffleValue);
var
  NewBase, I, ArgsToCopy: Integer;
  Frame: PSouffleVMCallFrame;
begin
  NewBase := FCallStack.Peek^.BaseRegister + FCallStack.Peek^.Template.MaxRegisters;
  if NewBase + AClosure.Template.MaxRegisters > MAX_REGISTERS then
    raise Exception.Create('Stack overflow');

  FRegisters[NewBase] := AReceiver;

  ArgsToCopy := AArgCount;
  if ArgsToCopy + 1 > AClosure.Template.MaxRegisters then
    ArgsToCopy := AClosure.Template.MaxRegisters - 1;

  for I := 0 to ArgsToCopy - 1 do
    FRegisters[NewBase + 1 + I] := FRegisters[AArgBase + I];
  for I := ArgsToCopy + 1 to AClosure.Template.MaxRegisters - 1 do
    FRegisters[NewBase + I] := SouffleNil;

  Frame := FCallStack.Push(AClosure.Template, AClosure, NewBase,
    AReturnAbsolute, FHandlerStack.Count);
  Frame^.ArgCount := AArgCount;
  Frame^.ArgSourceBase := NewBase + 1;
end;

function TSouffleVM.InvokeWithSpread(const ACallee, AArgsArray,
  AReceiver: TSouffleValue): TSouffleValue;
var
  Arr: TSouffleArray;
  Args: array of TSouffleValue;
  I, Count: Integer;
begin
  if SouffleIsReference(AArgsArray) and (AArgsArray.AsReference is TSouffleArray) then
    Arr := TSouffleArray(AArgsArray.AsReference)
  else
    Arr := nil;

  if Assigned(Arr) then
    Count := Arr.Count
  else
    Count := 0;

  SetLength(Args, Count);
  for I := 0 to Count - 1 do
    Args[I] := Arr.Get(I);

  if SouffleIsReference(ACallee) and Assigned(ACallee.AsReference) then
  begin
    if ACallee.AsReference is TSouffleNativeFunction then
    begin
      if Count > 0 then
        Result := TSouffleNativeFunction(ACallee.AsReference).Invoke(
          AReceiver, @Args[0], Count)
      else
        Result := TSouffleNativeFunction(ACallee.AsReference).Invoke(
          AReceiver, nil, 0);
      Exit;
    end;
  end;

  if Count > 0 then
    Result := FRuntimeOps.Invoke(ACallee, @Args[0], Count, AReceiver)
  else
    Result := FRuntimeOps.Invoke(ACallee, nil, 0, AReceiver);
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
  Rec: TSouffleRecord;
  RecVal: TSouffleValue;
  Bp: TSouffleBlueprint;
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
      B := DecodeB(AInstruction);
      FRegisters[Base + A] := SouffleNilWithFlags(B);
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
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsNil(FRegisters[Base + A]) and
         ((B = SOUFFLE_NIL_MATCH_ANY) or (FRegisters[Base + A].Flags = B)) then
        AFrame^.IP := AFrame^.IP + C;
    end;

    OP_JUMP_IF_NOT_NIL:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if not (SouffleIsNil(FRegisters[Base + A]) and
              ((B = SOUFFLE_NIL_MATCH_ANY) or (FRegisters[Base + A].Flags = B))) then
        AFrame^.IP := AFrame^.IP + C;
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
      Arr.Delegate := FArrayDelegate;
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

    OP_NEW_RECORD:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      Rec := TSouffleRecord.Create(B);
      Rec.Delegate := FRecordDelegate;
      if Assigned(FGC) then
        FGC.AllocateObject(Rec);
      FRegisters[Base + A] := SouffleReference(Rec);
    end;

    OP_RECORD_GET:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         Assigned(FRegisters[Base + B].AsReference) then
      begin
        if FRegisters[Base + B].AsReference is TSouffleRecord then
        begin
          Rec := TSouffleRecord(FRegisters[Base + B].AsReference);
          if Rec.Get(AFrame^.Template.GetConstant(C).StringValue, RecVal) then
          begin
            FRegisters[Base + A] := RecVal;
            Exit;
          end;
        end;
        if DelegateGet(FRegisters[Base + B].AsReference,
             AFrame^.Template.GetConstant(C).StringValue, RecVal) then
        begin
          FRegisters[Base + A] := RecVal;
          Exit;
        end;
      end;
      FRegisters[Base + A] := FRuntimeOps.GetProperty(
        FRegisters[Base + B], AFrame^.Template.GetConstant(C).StringValue);
    end;

    OP_RECORD_SET:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) then
      begin
        if FRegisters[Base + A].AsReference is TSouffleRecord then
          TSouffleRecord(FRegisters[Base + A].AsReference).Put(
            AFrame^.Template.GetConstant(B).StringValue, FRegisters[Base + C])
        else if FRegisters[Base + A].AsReference is TSouffleBlueprint then
          TSouffleBlueprint(FRegisters[Base + A].AsReference).Methods.Put(
            AFrame^.Template.GetConstant(B).StringValue, FRegisters[Base + C])
        else
          FRuntimeOps.SetProperty(
            FRegisters[Base + A], AFrame^.Template.GetConstant(B).StringValue,
            FRegisters[Base + C]);
      end;
    end;

    OP_RECORD_DELETE:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleRecord) then
        TSouffleRecord(FRegisters[Base + A].AsReference).Delete(
          AFrame^.Template.GetConstant(Bx).StringValue)
      else
        FRuntimeOps.DeleteProperty(
          FRegisters[Base + A], AFrame^.Template.GetConstant(Bx).StringValue);
    end;

    OP_GET_LENGTH:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      if SouffleIsStringValue(FRegisters[Base + B]) then
        FRegisters[Base + A] := SouffleInteger(
          Length(SouffleGetString(FRegisters[Base + B])))
      else if SouffleIsReference(FRegisters[Base + B]) and
         Assigned(FRegisters[Base + B].AsReference) then
      begin
        if FRegisters[Base + B].AsReference is TSouffleArray then
          FRegisters[Base + A] := SouffleInteger(
            TSouffleArray(FRegisters[Base + B].AsReference).Count)
        else if FRegisters[Base + B].AsReference is TSouffleRecord then
          FRegisters[Base + A] := SouffleInteger(
            TSouffleRecord(FRegisters[Base + B].AsReference).Count)
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

    OP_PACK_ARGS:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      RestCount := AFrame^.ArgCount - Integer(B);
      if RestCount < 0 then
        RestCount := 0;
      RestArr := TSouffleArray.Create(RestCount);
      RestArr.Delegate := FArrayDelegate;
      if Assigned(FGC) then
        FGC.AllocateObject(RestArr);
      for I := 0 to RestCount - 1 do
        RestArr.Push(FRegisters[AFrame^.ArgSourceBase + Integer(B) + I]);
      FRegisters[Base + A] := SouffleReference(RestArr);
    end;

    OP_ADD_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleInteger(
        FRegisters[Base + B].AsInteger + FRegisters[Base + C].AsInteger);
    end;

    OP_SUB_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleInteger(
        FRegisters[Base + B].AsInteger - FRegisters[Base + C].AsInteger);
    end;

    OP_MUL_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleInteger(
        FRegisters[Base + B].AsInteger * FRegisters[Base + C].AsInteger);
    end;

    OP_DIV_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        (FRegisters[Base + B].AsInteger * 1.0) /
        (FRegisters[Base + C].AsInteger * 1.0));
    end;

    OP_MOD_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleInteger(
        FRegisters[Base + B].AsInteger mod FRegisters[Base + C].AsInteger);
    end;

    OP_NEG_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      FRegisters[Base + A] := SouffleInteger(
        -FRegisters[Base + B].AsInteger);
    end;

    OP_ADD_FLOAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        FRegisters[Base + B].AsFloat + FRegisters[Base + C].AsFloat);
    end;

    OP_SUB_FLOAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        FRegisters[Base + B].AsFloat - FRegisters[Base + C].AsFloat);
    end;

    OP_MUL_FLOAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        FRegisters[Base + B].AsFloat * FRegisters[Base + C].AsFloat);
    end;

    OP_DIV_FLOAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        FRegisters[Base + B].AsFloat / FRegisters[Base + C].AsFloat);
    end;

    OP_MOD_FLOAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        FMod(FRegisters[Base + B].AsFloat, FRegisters[Base + C].AsFloat));
    end;

    OP_NEG_FLOAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      FRegisters[Base + A] := SouffleFloat(
        -FRegisters[Base + B].AsFloat);
    end;

    OP_EQ_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(
        FRegisters[Base + B].AsInteger = FRegisters[Base + C].AsInteger);
    end;

    OP_NEQ_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(
        FRegisters[Base + B].AsInteger <> FRegisters[Base + C].AsInteger);
    end;

    OP_LT_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(
        FRegisters[Base + B].AsInteger < FRegisters[Base + C].AsInteger);
    end;

    OP_GT_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(
        FRegisters[Base + B].AsInteger > FRegisters[Base + C].AsInteger);
    end;

    OP_LTE_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(
        FRegisters[Base + B].AsInteger <= FRegisters[Base + C].AsInteger);
    end;

    OP_GTE_INT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleBoolean(
        FRegisters[Base + B].AsInteger >= FRegisters[Base + C].AsInteger);
    end;

    OP_CONCAT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := SouffleString(
        SouffleValueToString(FRegisters[Base + B]) +
        SouffleValueToString(FRegisters[Base + C]));
    end;

    OP_GET_LOCAL_INT, OP_GET_LOCAL_FLOAT, OP_GET_LOCAL_BOOL,
    OP_GET_LOCAL_STRING, OP_GET_LOCAL_REF:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FRegisters[Base + A] := FRegisters[Base + Bx];
    end;

    OP_SET_LOCAL_INT, OP_SET_LOCAL_FLOAT, OP_SET_LOCAL_BOOL,
    OP_SET_LOCAL_STRING, OP_SET_LOCAL_REF:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      FRegisters[Base + Bx] := FRegisters[Base + A];
    end;

    OP_ARRAY_POP:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         (FRegisters[Base + B].AsReference is TSouffleArray) then
        FRegisters[Base + A] := TSouffleArray(
          FRegisters[Base + B].AsReference).Pop
      else
        FRegisters[Base + A] := SouffleNil;
    end;

    OP_NEW_BLUEPRINT:
    begin
      A := DecodeA(AInstruction);
      Bx := DecodeBx(AInstruction);
      Bp := TSouffleBlueprint.Create(
        AFrame^.Template.GetConstant(Bx).StringValue, 0);
      if Assigned(FGC) then
        FGC.AllocateObject(Bp);
      FRegisters[Base + A] := SouffleReference(Bp);
    end;

    OP_INHERIT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleBlueprint) and
         SouffleIsReference(FRegisters[Base + B]) and
         (FRegisters[Base + B].AsReference is TSouffleBlueprint) then
        TSouffleBlueprint(FRegisters[Base + A].AsReference).SuperBlueprint :=
          TSouffleBlueprint(FRegisters[Base + B].AsReference);
    end;

    OP_INSTANTIATE:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         (FRegisters[Base + B].AsReference is TSouffleBlueprint) then
      begin
        Bp := TSouffleBlueprint(FRegisters[Base + B].AsReference);
        Rec := TSouffleRecord.CreateFromBlueprint(Bp);
        Rec.Delegate := Bp.Methods;
        if Assigned(FGC) then
          FGC.AllocateObject(Rec);
        FRegisters[Base + A] := SouffleReference(Rec);
      end
      else
        FRegisters[Base + A] := SouffleNil;
    end;

    OP_GET_SLOT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + B]) and
         (FRegisters[Base + B].AsReference is TSouffleRecord) then
        FRegisters[Base + A] :=
          TSouffleRecord(FRegisters[Base + B].AsReference).GetSlot(C)
      else
        FRegisters[Base + A] := SouffleNil;
    end;

    OP_SET_SLOT:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      if SouffleIsReference(FRegisters[Base + A]) and
         (FRegisters[Base + A].AsReference is TSouffleRecord) then
        TSouffleRecord(FRegisters[Base + A].AsReference).SetSlot(
          B, FRegisters[Base + C]);
    end;

    OP_TO_PRIMITIVE:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      case FRegisters[Base + B].Kind of
        svkNil, svkBoolean, svkInteger, svkFloat:
          FRegisters[Base + A] := FRegisters[Base + B];
      else
        if SouffleIsStringValue(FRegisters[Base + B]) then
          FRegisters[Base + A] := FRegisters[Base + B]
        else
          FRegisters[Base + A] := FRuntimeOps.ToPrimitive(FRegisters[Base + B]);
      end;
    end;

    OP_UNPACK:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.ArrayRest(FRegisters[Base + B], C);
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
        FRegisters[Base + A] := SouffleFloat(
          SouffleAsNumber(FRegisters[Base + B]) /
          SouffleAsNumber(FRegisters[Base + C]))
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
          svkNil:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].Flags = FRegisters[Base + C].Flags);
          svkBoolean:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsBoolean = FRegisters[Base + C].AsBoolean);
          svkInteger:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsInteger = FRegisters[Base + C].AsInteger);
          svkFloat:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsFloat = FRegisters[Base + C].AsFloat);
          svkString:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsInlineString = FRegisters[Base + C].AsInlineString);
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
          svkNil:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].Flags <> FRegisters[Base + C].Flags);
          svkBoolean:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsBoolean <> FRegisters[Base + C].AsBoolean);
          svkInteger:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsInteger <> FRegisters[Base + C].AsInteger);
          svkFloat:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsFloat <> FRegisters[Base + C].AsFloat);
          svkString:
            FRegisters[Base + A] := SouffleBoolean(
              FRegisters[Base + B].AsInlineString <> FRegisters[Base + C].AsInlineString);
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
    OP_RT_CALL_SPREAD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := InvokeWithSpread(
        FRegisters[Base + A], FRegisters[Base + B], SouffleNil);
      if Assigned(FGC) then
        FGC.CollectIfNeeded;
    end;
    OP_RT_CALL_METHOD_SPREAD:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := InvokeWithSpread(
        FRegisters[Base + A], FRegisters[Base + B], FRegisters[Base + A - 1]);
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

    OP_RT_AWAIT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.AwaitValue(FRegisters[Base + B]);
    end;

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

    OP_RT_DEL_INDEX:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.DeleteIndex(
        FRegisters[Base + B], FRegisters[Base + C]);
    end;
    OP_RT_SPREAD_OBJ:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRuntimeOps.SpreadObjectInto(FRegisters[Base + A], FRegisters[Base + B]);
    end;

    OP_RT_OBJ_REST:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.ObjectRest(
        FRegisters[Base + B], FRegisters[Base + C]);
    end;

    OP_RT_REQUIRE_OBJECT:
    begin
      A := DecodeA(AInstruction);
      FRuntimeOps.RequireObjectCoercible(FRegisters[Base + A]);
    end;

    OP_RT_REQUIRE_ITERABLE:
    begin
      A := DecodeA(AInstruction);
      FRuntimeOps.RequireIterable(FRegisters[Base + A]);
    end;

    OP_RT_TO_STRING:
    begin
      A := DecodeA(AInstruction);
      B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.CoerceValueToString(FRegisters[Base + B]);
    end;

    OP_RT_GET_ASYNC_ITER:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.GetAsyncIterator(FRegisters[Base + B]);
    end;
    OP_RT_ASYNC_ITER_NEXT:
    begin
      A := DecodeA(AInstruction); B := DecodeB(AInstruction); C := DecodeC(AInstruction);
      FRegisters[Base + A] := FRuntimeOps.AsyncIteratorNext(FRegisters[Base + C], Done);
      FRegisters[Base + B] := SouffleBoolean(Done);
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

  if Assigned(FArrayDelegate) and not FArrayDelegate.GCMarked then
    FArrayDelegate.MarkReferences;
  if Assigned(FRecordDelegate) and not FRecordDelegate.GCMarked then
    FRecordDelegate.MarkReferences;
end;

function TSouffleVM.DelegateGet(const AObject: TSouffleHeapObject;
  const AKey: string; out AValue: TSouffleValue): Boolean;
var
  Current: TSouffleHeapObject;
begin
  Current := AObject.Delegate;
  while Assigned(Current) do
  begin
    if (Current is TSouffleRecord) and
       TSouffleRecord(Current).Get(AKey, AValue) then
      Exit(True);
    Current := Current.Delegate;
  end;
  Result := False;
end;

function TSouffleVM.MaterializeConstant(
  const AConstant: TSouffleBytecodeConstant): TSouffleValue;
begin
  case AConstant.Kind of
    bckNil:     Result := SouffleNil;
    bckTrue:    Result := SouffleBoolean(True);
    bckFalse:   Result := SouffleBoolean(False);
    bckInteger: Result := SouffleInteger(AConstant.IntValue);
    bckFloat:   Result := SouffleFloat(AConstant.FloatValue);
    bckString:  Result := SouffleString(AConstant.StringValue);
  else
    Result := SouffleNil;
  end;
end;

end.
