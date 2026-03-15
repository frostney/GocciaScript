unit Souffle.Wasm.Translator;

{$I Souffle.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Souffle.Bytecode,
  Souffle.Bytecode.Chunk,
  Souffle.Bytecode.Module,
  Souffle.Wasm.Emitter;

type
  TOpcodeSet = set of Byte;

  TSouffleWasmTranslator = class
  private type
    TLabelEntry = record
      TargetPC: Integer;
      IsLoop: Boolean;
      IsTry: Boolean;
    end;

    TBlockEvent = record
      PC: Integer;
      IsOpen: Boolean;
      IsLoop: Boolean;
    end;

  private
    FWasmModule: TWasmModule;
    FFuncTemplates: TList<TSouffleFunctionTemplate>;
    FFuncWasmIndices: TDictionary<TSouffleFunctionTemplate, UInt32>;
    FImportCount: UInt32;
    FConstBase: TDictionary<TSouffleFunctionTemplate, Integer>;
    FGlobalConstants: array of TSouffleBytecodeConstant;
    FGlobalConstantCount: Integer;

    FRtNil: UInt32;
    FRtTrue: UInt32;
    FRtFalse: UInt32;
    FRtBoxInt: UInt32;
    FRtLoadConst: UInt32;
    FRtToBool: UInt32;

    FRtAdd: UInt32;
    FRtSub: UInt32;
    FRtMul: UInt32;
    FRtDiv: UInt32;
    FRtMod: UInt32;
    FRtPow: UInt32;
    FRtNeg: UInt32;
    FRtBand: UInt32;
    FRtBor: UInt32;
    FRtBxor: UInt32;
    FRtShl: UInt32;
    FRtShr: UInt32;
    FRtUshr: UInt32;
    FRtBnot: UInt32;

    FRtEq: UInt32;
    FRtNeq: UInt32;
    FRtLt: UInt32;
    FRtGt: UInt32;
    FRtLte: UInt32;
    FRtGte: UInt32;
    FRtNot: UInt32;
    FRtTypeOf: UInt32;
    FRtIsInstance: UInt32;
    FRtToNumber: UInt32;
    FRtHasProperty: UInt32;
    FRtToBooleanVal: UInt32;

    FRtGetProp: UInt32;
    FRtSetProp: UInt32;
    FRtGetIndex: UInt32;
    FRtSetIndex: UInt32;
    FRtDelProp: UInt32;
    FRtDelIndex: UInt32;

    FRtCall: UInt32;
    FRtCallMethod: UInt32;
    FRtConstruct: UInt32;

    FRtGetIter: UInt32;
    FRtIterNext: UInt32;

    FRtImport: UInt32;
    FRtExport: UInt32;
    FRtAwait: UInt32;

    FRtGetGlobal: UInt32;
    FRtSetGlobal: UInt32;
    FRtHasGlobal: UInt32;

    FRtToString: UInt32;
    FRtExtOp: UInt32;

    FRtConcat: UInt32;
    FRtToPrimitive: UInt32;

    FRtNewArray: UInt32;
    FRtArrayPush: UInt32;
    FRtArrayPop: UInt32;
    FRtArrayGet: UInt32;
    FRtArraySet: UInt32;
    FRtNewRecord: UInt32;
    FRtRecordGet: UInt32;
    FRtRecordSet: UInt32;
    FRtRecordDelete: UInt32;
    FRtGetLength: UInt32;
    FRtNewBlueprint: UInt32;
    FRtInherit: UInt32;
    FRtInstantiate: UInt32;
    FRtGetSlot: UInt32;
    FRtSetSlot: UInt32;
    FRtUnpack: UInt32;
    FRtArgCount: UInt32;
    FRtPackArgs: UInt32;
    FRtCheckType: UInt32;

    FRtClosure: UInt32;
    FRtGetUpvalue: UInt32;
    FRtSetUpvalue: UInt32;
    FRtCloseUpvalue: UInt32;

    FExnTagIdx: UInt32;

    procedure CollectFunctions(const ATemplate: TSouffleFunctionTemplate);
    procedure FlattenConstantPools;
    function GlobalConstIdx(const ATemplate: TSouffleFunctionTemplate;
      ALocalIdx: Integer): Int32;
    procedure SerializeConstants;
    procedure ScanUsedOpcodes(out AUsed: TOpcodeSet);
    procedure RegisterRuntimeImports(const AUsed: TOpcodeSet);
    function ImportRt1(const AName: string): UInt32;
    function ImportRt2(const AName: string): UInt32;
    function ImportRt3(const AName: string): UInt32;
    function ImportRtTyped(const AName: string;
      const AParams, AResults: array of TWasmValType): UInt32;
    procedure TranslateFunction(const ATemplate: TSouffleFunctionTemplate;
      const ABuilder: TWasmCodeBuilder);
    procedure AnalyzeJumps(const ATemplate: TSouffleFunctionTemplate;
      out AEvents: TList<TBlockEvent>);
    function FindLabelDepth(const AStack: TList<TLabelEntry>;
      ATargetPC: Integer; AIsBackward: Boolean): Integer;
    procedure EmitInstruction(const ABuilder: TWasmCodeBuilder;
      const ATemplate: TSouffleFunctionTemplate;
      const AInstruction: UInt32; APC: Integer;
      AScratchLocal: UInt32);

  public
    function Translate(const AModule: TSouffleBytecodeModule): TWasmModule;
    procedure TranslateToFile(const AModule: TSouffleBytecodeModule;
      const AFileName: string);
    function TranslateToBytes(const AModule: TSouffleBytecodeModule): TBytes;
  end;

implementation

{ TSouffleWasmTranslator }

function TSouffleWasmTranslator.ImportRt1(const AName: string): UInt32;
begin
  Result := FWasmModule.ImportFunction('souffle', AName,
    [WT_EXTERNREF], [WT_EXTERNREF]);
end;

function TSouffleWasmTranslator.ImportRt2(const AName: string): UInt32;
begin
  Result := FWasmModule.ImportFunction('souffle', AName,
    [WT_EXTERNREF, WT_EXTERNREF], [WT_EXTERNREF]);
end;

function TSouffleWasmTranslator.ImportRt3(const AName: string): UInt32;
begin
  Result := FWasmModule.ImportFunction('souffle', AName,
    [WT_EXTERNREF, WT_EXTERNREF, WT_EXTERNREF], [WT_EXTERNREF]);
end;

function TSouffleWasmTranslator.ImportRtTyped(const AName: string;
  const AParams, AResults: array of TWasmValType): UInt32;
begin
  Result := FWasmModule.ImportFunction('souffle', AName, AParams, AResults);
end;

procedure TSouffleWasmTranslator.CollectFunctions(
  const ATemplate: TSouffleFunctionTemplate);
var
  I: Integer;
begin
  FFuncTemplates.Add(ATemplate);
  for I := 0 to ATemplate.FunctionCount - 1 do
    CollectFunctions(ATemplate.GetFunction(I));
end;

procedure TSouffleWasmTranslator.FlattenConstantPools;
var
  I, J: Integer;
  Template: TSouffleFunctionTemplate;
begin
  FConstBase := TDictionary<TSouffleFunctionTemplate, Integer>.Create;
  FGlobalConstantCount := 0;
  for I := 0 to FFuncTemplates.Count - 1 do
  begin
    Template := FFuncTemplates[I];
    FConstBase.Add(Template, FGlobalConstantCount);
    if Template.ConstantCount > 0 then
    begin
      if FGlobalConstantCount + Template.ConstantCount > Length(FGlobalConstants) then
        SetLength(FGlobalConstants, (FGlobalConstantCount + Template.ConstantCount) * 2 + 8);
      for J := 0 to Template.ConstantCount - 1 do
      begin
        FGlobalConstants[FGlobalConstantCount] := Template.GetConstant(J);
        Inc(FGlobalConstantCount);
      end;
    end;
  end;
end;

function TSouffleWasmTranslator.GlobalConstIdx(
  const ATemplate: TSouffleFunctionTemplate;
  ALocalIdx: Integer): Int32;
begin
  Result := Int32(FConstBase[ATemplate] + ALocalIdx);
end;

procedure TSouffleWasmTranslator.SerializeConstants;
var
  MS: TMemoryStream;
  Data: TBytes;
  I: Integer;
  CountLE: UInt32;
  Kind: Byte;
  IntVal: Int64;
  FloatVal: Double;
  StrBytes: TBytes;
  StrLen: UInt32;
begin
  MS := TMemoryStream.Create;
  try
    CountLE := UInt32(FGlobalConstantCount);
    MS.Write(CountLE, 4);
    for I := 0 to FGlobalConstantCount - 1 do
    begin
      case FGlobalConstants[I].Kind of
        bckNil:
        begin
          Kind := 0;
          MS.Write(Kind, 1);
        end;
        bckTrue:
        begin
          Kind := 1;
          MS.Write(Kind, 1);
        end;
        bckFalse:
        begin
          Kind := 2;
          MS.Write(Kind, 1);
        end;
        bckInteger:
        begin
          Kind := 3;
          MS.Write(Kind, 1);
          IntVal := FGlobalConstants[I].IntValue;
          MS.Write(IntVal, 8);
        end;
        bckFloat:
        begin
          Kind := 4;
          MS.Write(Kind, 1);
          FloatVal := FGlobalConstants[I].FloatValue;
          MS.Write(FloatVal, 8);
        end;
        bckString:
        begin
          Kind := 5;
          MS.Write(Kind, 1);
          StrBytes := TEncoding.UTF8.GetBytes(FGlobalConstants[I].StringValue);
          StrLen := UInt32(Length(StrBytes));
          MS.Write(StrLen, 4);
          if StrLen > 0 then
            MS.Write(StrBytes[0], StrLen);
        end;
      end;
    end;
    SetLength(Data, MS.Size);
    MS.Position := 0;
    MS.Read(Data[0], MS.Size);
    FWasmModule.AddCustomSection('souffle:constants', Data);
  finally
    MS.Free;
  end;
end;

procedure TSouffleWasmTranslator.ScanUsedOpcodes(out AUsed: TOpcodeSet);
var
  I, PC: Integer;
  Template: TSouffleFunctionTemplate;
begin
  AUsed := [];
  for I := 0 to FFuncTemplates.Count - 1 do
  begin
    Template := FFuncTemplates[I];
    for PC := 0 to Template.CodeCount - 1 do
      Include(AUsed, DecodeOp(Template.GetInstruction(PC)));
  end;
end;

procedure TSouffleWasmTranslator.RegisterRuntimeImports(
  const AUsed: TOpcodeSet);
const
  NONE = UInt32($FFFFFFFF);

  function Need(const AOps: array of TSouffleOpCode): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to High(AOps) do
      if Byte(AOps[I]) in AUsed then
        Exit(True);
    Result := False;
  end;

  function Cond1(const AName: string;
    const AOps: array of TSouffleOpCode): UInt32;
  begin
    if Need(AOps) then Result := ImportRt1(AName)
    else Result := NONE;
  end;

  function Cond2(const AName: string;
    const AOps: array of TSouffleOpCode): UInt32;
  begin
    if Need(AOps) then Result := ImportRt2(AName)
    else Result := NONE;
  end;

  function Cond3(const AName: string;
    const AOps: array of TSouffleOpCode): UInt32;
  begin
    if Need(AOps) then Result := ImportRt3(AName)
    else Result := NONE;
  end;

  function CondT(const AName: string;
    const AParams, AResults: array of TWasmValType;
    const AOps: array of TSouffleOpCode): UInt32;
  begin
    if Need(AOps) then Result := ImportRtTyped(AName, AParams, AResults)
    else Result := NONE;
  end;

begin
  FRtNil := CondT('rt_nil', [WT_I32], [WT_EXTERNREF],
    [OP_LOAD_NIL, OP_RETURN_NIL]);
  FRtTrue := CondT('rt_true', [], [WT_EXTERNREF],
    [OP_LOAD_TRUE]);
  FRtFalse := CondT('rt_false', [], [WT_EXTERNREF],
    [OP_LOAD_FALSE]);
  FRtBoxInt := CondT('rt_box_int', [WT_I64], [WT_EXTERNREF],
    [OP_LOAD_INT]);
  FRtLoadConst := CondT('rt_load_const', [WT_I32], [WT_EXTERNREF],
    [OP_LOAD_CONST]);
  FRtToBool := CondT('rt_to_bool', [WT_EXTERNREF], [WT_I32],
    [OP_JUMP_IF_TRUE, OP_JUMP_IF_FALSE]);

  FRtAdd := Cond2('rt_add', [OP_RT_ADD, OP_ADD_INT, OP_ADD_FLOAT]);
  FRtSub := Cond2('rt_sub', [OP_RT_SUB, OP_SUB_INT, OP_SUB_FLOAT]);
  FRtMul := Cond2('rt_mul', [OP_RT_MUL, OP_MUL_INT, OP_MUL_FLOAT]);
  FRtDiv := Cond2('rt_div', [OP_RT_DIV, OP_DIV_INT, OP_DIV_FLOAT]);
  FRtMod := Cond2('rt_mod', [OP_RT_MOD, OP_MOD_INT, OP_MOD_FLOAT]);
  FRtPow := Cond2('rt_pow', [OP_RT_POW]);
  FRtNeg := Cond1('rt_neg', [OP_RT_NEG, OP_NEG_INT, OP_NEG_FLOAT]);
  FRtBand := Cond2('rt_band', [OP_RT_BAND]);
  FRtBor := Cond2('rt_bor', [OP_RT_BOR]);
  FRtBxor := Cond2('rt_bxor', [OP_RT_BXOR]);
  FRtShl := Cond2('rt_shl', [OP_RT_SHL]);
  FRtShr := Cond2('rt_shr', [OP_RT_SHR]);
  FRtUshr := Cond2('rt_ushr', [OP_RT_USHR]);
  FRtBnot := Cond1('rt_bnot', [OP_RT_BNOT]);

  FRtEq := Cond2('rt_eq', [OP_RT_EQ, OP_EQ_INT, OP_EQ_FLOAT]);
  FRtNeq := Cond2('rt_neq', [OP_RT_NEQ, OP_NEQ_INT, OP_NEQ_FLOAT]);
  FRtLt := Cond2('rt_lt', [OP_RT_LT, OP_LT_INT, OP_LT_FLOAT]);
  FRtGt := Cond2('rt_gt', [OP_RT_GT, OP_GT_INT, OP_GT_FLOAT]);
  FRtLte := Cond2('rt_lte', [OP_RT_LTE, OP_LTE_INT, OP_LTE_FLOAT]);
  FRtGte := Cond2('rt_gte', [OP_RT_GTE, OP_GTE_INT, OP_GTE_FLOAT]);
  FRtNot := Cond1('rt_not', [OP_NOT]);
  FRtTypeOf := Cond1('rt_typeof', [OP_RT_TYPEOF]);
  FRtIsInstance := Cond2('rt_is_instance', [OP_RT_IS_INSTANCE]);
  FRtHasProperty := Cond2('rt_has_property', [OP_RT_HAS_PROPERTY]);
  FRtToBooleanVal := Cond1('rt_to_boolean_val', [OP_TO_BOOL]);
  FRtToNumber := Cond1('rt_to_number', [OP_RT_TO_NUMBER]);

  FRtGetProp := CondT('rt_get_prop',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_RT_GET_PROP]);
  FRtSetProp := CondT('rt_set_prop',
    [WT_EXTERNREF, WT_I32, WT_EXTERNREF], [], [OP_RT_SET_PROP]);
  FRtGetIndex := Cond2('rt_get_index', [OP_RT_GET_INDEX]);
  FRtSetIndex := CondT('rt_set_index',
    [WT_EXTERNREF, WT_EXTERNREF, WT_EXTERNREF], [], [OP_RT_SET_INDEX]);
  FRtDelProp := CondT('rt_del_prop',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_RT_DEL_PROP]);
  FRtDelIndex := Cond2('rt_del_index', [OP_RT_DEL_INDEX]);

  FRtCall := CondT('rt_call',
    [WT_EXTERNREF, WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_RT_CALL]);
  FRtCallMethod := CondT('rt_call_method',
    [WT_EXTERNREF, WT_EXTERNREF, WT_EXTERNREF, WT_I32], [WT_EXTERNREF],
    [OP_RT_CALL_METHOD]);
  FRtConstruct := CondT('rt_construct',
    [WT_EXTERNREF, WT_EXTERNREF, WT_I32], [WT_EXTERNREF],
    [OP_RT_CONSTRUCT]);

  FRtGetIter := CondT('rt_get_iter',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_RT_GET_ITER]);
  FRtIterNext := Cond1('rt_iter_next', [OP_RT_ITER_NEXT]);

  FRtImport := CondT('rt_import', [WT_I32], [WT_EXTERNREF],
    [OP_RT_IMPORT]);
  FRtExport := CondT('rt_export', [WT_EXTERNREF, WT_I32], [],
    [OP_RT_EXPORT]);
  FRtAwait := Cond1('rt_await', [OP_RT_AWAIT]);

  FRtGetGlobal := CondT('rt_get_global', [WT_I32], [WT_EXTERNREF],
    [OP_RT_GET_GLOBAL]);
  FRtSetGlobal := CondT('rt_set_global', [WT_EXTERNREF, WT_I32], [],
    [OP_RT_SET_GLOBAL]);
  FRtHasGlobal := CondT('rt_has_global', [WT_I32], [WT_EXTERNREF],
    [OP_RT_HAS_GLOBAL]);

  FRtToString := Cond1('rt_to_string', [OP_RT_TO_STRING]);
  FRtExtOp := CondT('rt_ext_op',
    [WT_I32, WT_EXTERNREF, WT_EXTERNREF, WT_EXTERNREF], [WT_EXTERNREF],
    [OP_RT_EXT]);

  FRtConcat := Cond2('rt_concat', [OP_CONCAT]);
  FRtToPrimitive := Cond1('rt_to_primitive', [OP_TO_PRIMITIVE]);

  FRtNewArray := CondT('rt_new_array', [WT_I32], [WT_EXTERNREF],
    [OP_NEW_ARRAY, OP_RT_CALL, OP_RT_CALL_METHOD, OP_RT_CONSTRUCT]);
  FRtArrayPush := CondT('rt_array_push',
    [WT_EXTERNREF, WT_EXTERNREF], [],
    [OP_ARRAY_PUSH, OP_RT_CALL, OP_RT_CALL_METHOD, OP_RT_CONSTRUCT]);
  FRtArrayPop := Cond1('rt_array_pop', [OP_ARRAY_POP]);
  FRtArrayGet := Cond2('rt_array_get', [OP_ARRAY_GET]);
  FRtArraySet := Cond3('rt_array_set', [OP_ARRAY_SET]);
  FRtNewRecord := CondT('rt_new_record', [WT_I32], [WT_EXTERNREF],
    [OP_NEW_RECORD]);
  FRtRecordGet := CondT('rt_record_get',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_RECORD_GET]);
  FRtRecordSet := CondT('rt_record_set',
    [WT_EXTERNREF, WT_I32, WT_EXTERNREF], [], [OP_RECORD_SET]);
  FRtRecordDelete := CondT('rt_record_delete',
    [WT_EXTERNREF, WT_I32], [], [OP_RECORD_DELETE]);
  FRtGetLength := Cond1('rt_get_length', [OP_GET_LENGTH]);
  FRtNewBlueprint := CondT('rt_new_blueprint', [WT_I32], [WT_EXTERNREF],
    [OP_NEW_BLUEPRINT]);
  FRtInherit := Cond2('rt_inherit', [OP_INHERIT]);
  FRtInstantiate := Cond1('rt_instantiate', [OP_INSTANTIATE]);
  FRtGetSlot := CondT('rt_get_slot',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_GET_SLOT]);
  FRtSetSlot := CondT('rt_set_slot',
    [WT_EXTERNREF, WT_I32, WT_EXTERNREF], [], [OP_SET_SLOT]);
  FRtUnpack := CondT('rt_unpack',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_UNPACK]);
  FRtArgCount := CondT('rt_arg_count', [], [WT_EXTERNREF],
    [OP_ARG_COUNT]);
  FRtPackArgs := CondT('rt_pack_args', [WT_I32], [WT_EXTERNREF],
    [OP_PACK_ARGS]);
  FRtCheckType := CondT('rt_check_type',
    [WT_EXTERNREF, WT_I32], [], [OP_CHECK_TYPE]);

  FRtClosure := CondT('rt_closure', [WT_I32], [WT_EXTERNREF],
    [OP_CLOSURE]);
  FRtGetUpvalue := CondT('rt_get_upvalue',
    [WT_EXTERNREF, WT_I32], [WT_EXTERNREF], [OP_GET_UPVALUE, OP_CLOSURE]);
  FRtSetUpvalue := CondT('rt_set_upvalue',
    [WT_EXTERNREF, WT_I32, WT_EXTERNREF], [], [OP_SET_UPVALUE, OP_CLOSURE]);
  FRtCloseUpvalue := CondT('rt_close_upvalue',
    [WT_EXTERNREF, WT_I32], [], [OP_CLOSE_UPVALUE]);

  FImportCount := FWasmModule.ImportFuncCount;

  if Need([OP_THROW, OP_PUSH_HANDLER]) then
    FExnTagIdx := FWasmModule.ImportTag('souffle', '__exn_tag', [WT_EXTERNREF])
  else
    FExnTagIdx := UInt32($FFFFFFFF);
end;

procedure TSouffleWasmTranslator.AnalyzeJumps(
  const ATemplate: TSouffleFunctionTemplate;
  out AEvents: TList<TBlockEvent>);
var
  PC, TargetPC: Integer;
  Instr: UInt32;
  Op: Byte;
  Offset: Int32;
  Ev: TBlockEvent;
begin
  AEvents := TList<TBlockEvent>.Create;

  for PC := 0 to ATemplate.CodeCount - 1 do
  begin
    Instr := ATemplate.GetInstruction(PC);
    Op := DecodeOp(Instr);

    case TSouffleOpCode(Op) of
      OP_JUMP:
      begin
        Offset := DecodeAx(Instr);
        TargetPC := PC + 1 + Offset;
        if Offset > 0 then
        begin
          Ev.PC := PC;
          Ev.IsOpen := True;
          Ev.IsLoop := False;
          AEvents.Add(Ev);
          Ev.PC := TargetPC;
          Ev.IsOpen := False;
          Ev.IsLoop := False;
          AEvents.Add(Ev);
        end
        else if Offset < 0 then
        begin
          Ev.PC := TargetPC;
          Ev.IsOpen := True;
          Ev.IsLoop := True;
          AEvents.Add(Ev);
          Ev.PC := PC + 1;
          Ev.IsOpen := False;
          Ev.IsLoop := True;
          AEvents.Add(Ev);
        end;
      end;
      OP_JUMP_IF_TRUE, OP_JUMP_IF_FALSE:
      begin
        Offset := DecodesBx(Instr);
        TargetPC := PC + 1 + Offset;
        if Offset > 0 then
        begin
          Ev.PC := PC;
          Ev.IsOpen := True;
          Ev.IsLoop := False;
          AEvents.Add(Ev);
          Ev.PC := TargetPC;
          Ev.IsOpen := False;
          Ev.IsLoop := False;
          AEvents.Add(Ev);
        end;
      end;
    end;
  end;
end;

function TSouffleWasmTranslator.FindLabelDepth(
  const AStack: TList<TLabelEntry>;
  ATargetPC: Integer; AIsBackward: Boolean): Integer;
var
  I: Integer;
begin
  for I := AStack.Count - 1 downto 0 do
  begin
    if AIsBackward and AStack[I].IsLoop and (AStack[I].TargetPC = ATargetPC) then
      Exit(AStack.Count - 1 - I);
    if (not AIsBackward) and (not AStack[I].IsLoop) and (AStack[I].TargetPC = ATargetPC) then
      Exit(AStack.Count - 1 - I);
  end;
  Result := 0;
end;

procedure TSouffleWasmTranslator.EmitInstruction(
  const ABuilder: TWasmCodeBuilder;
  const ATemplate: TSouffleFunctionTemplate;
  const AInstruction: UInt32; APC: Integer;
  AScratchLocal: UInt32);
var
  Op: Byte;
  A, B, C: UInt8;
  Bx: UInt16;
  sBx: Int16;
  Const_: TSouffleBytecodeConstant;
  ChildTemplate: TSouffleFunctionTemplate;
  ChildWasmIdx: UInt32;
  UpIdx, ArgIdx: Integer;
  UpDesc: TSouffleUpvalueDescriptor;
  IsSpread: Boolean;
begin
  Op := DecodeOp(AInstruction);
  A := DecodeA(AInstruction);
  B := DecodeB(AInstruction);
  C := DecodeC(AInstruction);
  Bx := DecodeBx(AInstruction);
  sBx := DecodesBx(AInstruction);

  case TSouffleOpCode(Op) of

    // Load / Store / Move
    OP_LOAD_CONST:
    begin
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtLoadConst);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LOAD_NIL:
    begin
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtNil);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LOAD_TRUE:
    begin
      ABuilder.EmitCall(FRtTrue);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LOAD_FALSE:
    begin
      ABuilder.EmitCall(FRtFalse);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LOAD_INT:
    begin
      ABuilder.EmitI64Const(Int64(sBx));
      ABuilder.EmitCall(FRtBoxInt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_MOVE:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalSet(A);
    end;

    // Variables
    OP_GET_LOCAL:
    begin
      ABuilder.EmitLocalGet(Bx);
      ABuilder.EmitLocalSet(A);
    end;
    OP_SET_LOCAL:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitLocalSet(Bx);
    end;
    OP_GET_UPVALUE:
    begin
      ABuilder.EmitLocalGet(0);
      ABuilder.EmitI32Const(Int32(Bx));
      ABuilder.EmitCall(FRtGetUpvalue);
      ABuilder.EmitLocalSet(A);
    end;
    OP_SET_UPVALUE:
    begin
      ABuilder.EmitLocalGet(0);
      ABuilder.EmitI32Const(Int32(Bx));
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitCall(FRtSetUpvalue);
    end;
    OP_CLOSE_UPVALUE:
    begin
      ABuilder.EmitLocalGet(0);
      ABuilder.EmitI32Const(Int32(A));
      ABuilder.EmitCall(FRtCloseUpvalue);
    end;

    // Control flow: jumps are handled by the block/loop structure
    OP_JUMP:
    begin
      // Handled by AnalyzeJumps — emit br to the appropriate label
      // The actual br is emitted by TranslateFunction's control flow logic
    end;
    OP_JUMP_IF_TRUE:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitCall(FRtToBool);
      // br_if is emitted by TranslateFunction's control flow logic
    end;
    OP_JUMP_IF_FALSE:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitCall(FRtToBool);
      ABuilder.EmitI32Eqz;
      // br_if is emitted by TranslateFunction's control flow logic
    end;
    OP_JUMP_IF_NIL:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitRefIsNull;
    end;
    OP_JUMP_IF_NOT_NIL:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitRefIsNull;
      ABuilder.EmitI32Eqz;
    end;

    // Closures
    OP_CLOSURE:
    begin
      ChildTemplate := ATemplate.GetFunction(Bx);
      ChildWasmIdx := FImportCount + UInt32(FFuncTemplates.IndexOf(ChildTemplate));
      ABuilder.EmitI32Const(Int32(ChildWasmIdx));
      ABuilder.EmitCall(FRtClosure);
      ABuilder.EmitLocalSet(A);
      for UpIdx := 0 to ChildTemplate.UpvalueCount - 1 do
      begin
        UpDesc := ChildTemplate.GetUpvalueDescriptor(UpIdx);
        ABuilder.EmitLocalGet(A);
        ABuilder.EmitI32Const(Int32(UpIdx));
        if UpDesc.IsLocal then
          ABuilder.EmitLocalGet(UpDesc.Index)
        else
        begin
          ABuilder.EmitLocalGet(0);
          ABuilder.EmitI32Const(Int32(UpDesc.Index));
          ABuilder.EmitCall(FRtGetUpvalue);
        end;
        ABuilder.EmitCall(FRtSetUpvalue);
      end;
    end;

    // Exception handling (structural try/catch replaces push/pop)
    OP_PUSH_HANDLER:;
    OP_POP_HANDLER:;
    OP_THROW:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitThrow(FExnTagIdx);
    end;

    // Return
    OP_RETURN:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitReturn;
    end;
    OP_RETURN_NIL:
    begin
      ABuilder.EmitI32Const(0);
      ABuilder.EmitCall(FRtNil);
      ABuilder.EmitReturn;
    end;

    // Compound types
    OP_NEW_ARRAY:
    begin
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtNewArray);
      ABuilder.EmitLocalSet(A);
    end;
    OP_ARRAY_PUSH:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtArrayPush);
    end;
    OP_ARRAY_POP:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtArrayPop);
      ABuilder.EmitLocalSet(A);
    end;
    OP_ARRAY_GET:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtArrayGet);
      ABuilder.EmitLocalSet(A);
    end;
    OP_ARRAY_SET:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtArraySet);
    end;
    OP_NEW_RECORD:
    begin
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtNewRecord);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RECORD_GET:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, C));
      ABuilder.EmitCall(FRtRecordGet);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RECORD_SET:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, B));
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtRecordSet);
    end;
    OP_RECORD_DELETE:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtRecordDelete);
    end;
    OP_GET_LENGTH:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtGetLength);
      ABuilder.EmitLocalSet(A);
    end;

    // Arguments
    OP_ARG_COUNT:
    begin
      ABuilder.EmitCall(FRtArgCount);
      ABuilder.EmitLocalSet(A);
    end;
    OP_PACK_ARGS:
    begin
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtPackArgs);
      ABuilder.EmitLocalSet(A);
    end;

    // Debug
    OP_NOP:; // no-op
    OP_LINE:; // debug annotation, skip

    // Integer arithmetic
    OP_ADD_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtAdd);
      ABuilder.EmitLocalSet(A);
    end;
    OP_SUB_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtSub);
      ABuilder.EmitLocalSet(A);
    end;
    OP_MUL_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtMul);
      ABuilder.EmitLocalSet(A);
    end;
    OP_DIV_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtDiv);
      ABuilder.EmitLocalSet(A);
    end;
    OP_MOD_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtMod);
      ABuilder.EmitLocalSet(A);
    end;
    OP_NEG_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtNeg);
      ABuilder.EmitLocalSet(A);
    end;

    // Float arithmetic
    OP_ADD_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtAdd);
      ABuilder.EmitLocalSet(A);
    end;
    OP_SUB_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtSub);
      ABuilder.EmitLocalSet(A);
    end;
    OP_MUL_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtMul);
      ABuilder.EmitLocalSet(A);
    end;
    OP_DIV_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtDiv);
      ABuilder.EmitLocalSet(A);
    end;
    OP_MOD_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtMod);
      ABuilder.EmitLocalSet(A);
    end;
    OP_NEG_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtNeg);
      ABuilder.EmitLocalSet(A);
    end;

    // Integer comparison
    OP_EQ_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtEq);
      ABuilder.EmitLocalSet(A);
    end;
    OP_NEQ_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtNeq);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LT_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtLt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_GT_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LTE_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtLte);
      ABuilder.EmitLocalSet(A);
    end;
    OP_GTE_INT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGte);
      ABuilder.EmitLocalSet(A);
    end;

    // String
    OP_CONCAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtConcat);
      ABuilder.EmitLocalSet(A);
    end;

    // Type checking
    OP_CHECK_TYPE:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtCheckType);
    end;

    // Float comparison
    OP_EQ_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtEq);
      ABuilder.EmitLocalSet(A);
    end;
    OP_NEQ_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtNeq);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LT_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtLt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_GT_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_LTE_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtLte);
      ABuilder.EmitLocalSet(A);
    end;
    OP_GTE_FLOAT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGte);
      ABuilder.EmitLocalSet(A);
    end;

    // Blueprint
    OP_NEW_BLUEPRINT:
    begin
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtNewBlueprint);
      ABuilder.EmitLocalSet(A);
    end;
    OP_INHERIT:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtInherit);
      ABuilder.EmitDrop;
    end;
    OP_INSTANTIATE:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtInstantiate);
      ABuilder.EmitLocalSet(A);
    end;
    OP_GET_SLOT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(Int32(C));
      ABuilder.EmitCall(FRtGetSlot);
      ABuilder.EmitLocalSet(A);
    end;
    OP_SET_SLOT:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtSetSlot);
    end;

    // Type coercion
    OP_TO_PRIMITIVE:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtToPrimitive);
      ABuilder.EmitLocalSet(A);
    end;

    // Destructuring
    OP_UNPACK:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(Int32(C));
      ABuilder.EmitCall(FRtUnpack);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Arithmetic
    OP_RT_ADD:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtAdd);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_SUB:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtSub);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_MUL:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtMul);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_DIV:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtDiv);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_MOD:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtMod);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_POW:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtPow);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_NEG:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtNeg);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Bitwise
    OP_RT_BAND:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtBand);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_BOR:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtBor);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_BXOR:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtBxor);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_SHL:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtShl);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_SHR:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtShr);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_USHR:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtUshr);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_BNOT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtBnot);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Comparison
    OP_RT_EQ:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtEq);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_NEQ:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtNeq);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_LT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtLt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_GT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGt);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_LTE:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtLte);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_GTE:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGte);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Logical / Type
    OP_NOT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtNot);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_TYPEOF:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtTypeOf);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_IS_INSTANCE:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtIsInstance);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_HAS_PROPERTY:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtHasProperty);
      ABuilder.EmitLocalSet(A);
    end;
    OP_TO_BOOL:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtToBooleanVal);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_TO_NUMBER:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtToNumber);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Property access
    OP_RT_GET_PROP:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, C));
      ABuilder.EmitCall(FRtGetProp);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_SET_PROP:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, B));
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtSetProp);
    end;
    OP_RT_GET_INDEX:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtGetIndex);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_SET_INDEX:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtSetIndex);
    end;
    OP_RT_DEL_PROP:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, C));
      ABuilder.EmitCall(FRtDelProp);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_DEL_INDEX:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtDelIndex);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Invocation
    OP_RT_CALL:
    begin
      IsSpread := (C and 1) = 1;
      ABuilder.EmitLocalGet(A);
      if IsSpread then
        ABuilder.EmitLocalGet(B)
      else
      begin
        ABuilder.EmitI32Const(Int32(B));
        ABuilder.EmitCall(FRtNewArray);
        ABuilder.EmitLocalSet(AScratchLocal);
        for ArgIdx := 0 to B - 1 do
        begin
          ABuilder.EmitLocalGet(AScratchLocal);
          ABuilder.EmitLocalGet(A + 1 + ArgIdx);
          ABuilder.EmitCall(FRtArrayPush);
        end;
        ABuilder.EmitLocalGet(AScratchLocal);
      end;
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtCall);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_CALL_METHOD:
    begin
      IsSpread := (C and 1) = 1;
      ABuilder.EmitLocalGet(A - 1);
      ABuilder.EmitLocalGet(A);
      if IsSpread then
        ABuilder.EmitLocalGet(B)
      else
      begin
        ABuilder.EmitI32Const(Int32(B));
        ABuilder.EmitCall(FRtNewArray);
        ABuilder.EmitLocalSet(AScratchLocal);
        for ArgIdx := 0 to B - 1 do
        begin
          ABuilder.EmitLocalGet(AScratchLocal);
          ABuilder.EmitLocalGet(A + 1 + ArgIdx);
          ABuilder.EmitCall(FRtArrayPush);
        end;
        ABuilder.EmitLocalGet(AScratchLocal);
      end;
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitCall(FRtCallMethod);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_CONSTRUCT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(Int32(C));
      ABuilder.EmitCall(FRtNewArray);
      ABuilder.EmitLocalSet(AScratchLocal);
      for ArgIdx := 0 to C - 1 do
      begin
        ABuilder.EmitLocalGet(AScratchLocal);
        ABuilder.EmitLocalGet(B + 1 + ArgIdx);
        ABuilder.EmitCall(FRtArrayPush);
      end;
      ABuilder.EmitLocalGet(AScratchLocal);
      ABuilder.EmitI32Const(Int32(C));
      ABuilder.EmitCall(FRtConstruct);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Iteration
    OP_RT_GET_ITER:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitI32Const(Int32(C));
      ABuilder.EmitCall(FRtGetIter);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_ITER_NEXT:
    begin
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitCall(FRtIterNext);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Modules
    OP_RT_IMPORT:
    begin
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtImport);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_EXPORT:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtExport);
    end;

    // Runtime: Async
    OP_RT_AWAIT:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtAwait);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Globals
    OP_RT_GET_GLOBAL:
    begin
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtGetGlobal);
      ABuilder.EmitLocalSet(A);
    end;
    OP_RT_SET_GLOBAL:
    begin
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtSetGlobal);
    end;
    OP_RT_HAS_GLOBAL:
    begin
      ABuilder.EmitI32Const(GlobalConstIdx(ATemplate, Bx));
      ABuilder.EmitCall(FRtHasGlobal);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Coercion
    OP_RT_TO_STRING:
    begin
      ABuilder.EmitLocalGet(B);
      ABuilder.EmitCall(FRtToString);
      ABuilder.EmitLocalSet(A);
    end;

    // Runtime: Extension
    OP_RT_EXT:
    begin
      ABuilder.EmitI32Const(Int32(B));
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitLocalGet(C);
      ABuilder.EmitLocalGet(A);
      ABuilder.EmitCall(FRtExtOp);
      ABuilder.EmitLocalSet(A);
    end;

  else
    ABuilder.EmitUnreachable;
  end;
end;

procedure TSouffleWasmTranslator.TranslateFunction(
  const ATemplate: TSouffleFunctionTemplate;
  const ABuilder: TWasmCodeBuilder);
var
  ExtraLocals, I, K, PC, HIdx: Integer;
  ScratchLocal: UInt32;
  LabelStack: TList<TLabelEntry>;
  ForwardTargets: TList<Integer>;
  LoopHeaders: TDictionary<Integer, Integer>;
  InteriorSet: TDictionary<Integer, Boolean>;
  HandlerByStart: TDictionary<Integer, Integer>;
  HandlerByEnd: TDictionary<Integer, Integer>;
  HandlerStarts: array of Integer;
  HandlerEnds: array of Integer;
  HandlerCatchTargets: array of Integer;
  HandlerCatchRegs: array of UInt8;
  HandlerCount: Integer;
  PushStack: TList<Integer>;
  Instr: UInt32;
  Op: Byte;
  Lbl: TLabelEntry;
  Offset: Int32;
  TargetPC, LoopEndPC, Depth: Integer;
  Dummy: Boolean;

  procedure ScanHandlers;
  var
    ScanPC: Integer;
    ScanInstr: UInt32;
    ScanOp: Byte;
    Idx: Integer;
  begin
    HandlerCount := 0;
    PushStack := TList<Integer>.Create;
    for ScanPC := 0 to ATemplate.CodeCount - 1 do
    begin
      ScanInstr := ATemplate.GetInstruction(ScanPC);
      ScanOp := DecodeOp(ScanInstr);
      if TSouffleOpCode(ScanOp) = OP_PUSH_HANDLER then
      begin
        Idx := HandlerCount;
        Inc(HandlerCount);
        SetLength(HandlerStarts, HandlerCount);
        SetLength(HandlerEnds, HandlerCount);
        SetLength(HandlerCatchTargets, HandlerCount);
        SetLength(HandlerCatchRegs, HandlerCount);
        HandlerStarts[Idx] := ScanPC;
        HandlerCatchRegs[Idx] := DecodeA(ScanInstr);
        HandlerCatchTargets[Idx] := ScanPC + 1 + Integer(DecodeBx(ScanInstr));
        HandlerEnds[Idx] := ATemplate.CodeCount;
        PushStack.Add(Idx);
      end
      else if TSouffleOpCode(ScanOp) = OP_POP_HANDLER then
      begin
        if PushStack.Count > 0 then
        begin
          Idx := PushStack[PushStack.Count - 1];
          PushStack.Delete(PushStack.Count - 1);
          HandlerEnds[Idx] := ScanPC + 1;
        end;
      end;
    end;
    PushStack.Free;
  end;

  procedure CollectTargets;
  var
    ScanPC, HI: Integer;
    ScanInstr: UInt32;
    ScanOp: Byte;
    ScanOffset: Int32;
    ScanTarget, Existing: Integer;
  begin
    ForwardTargets := TList<Integer>.Create;
    LoopHeaders := TDictionary<Integer, Integer>.Create;
    for ScanPC := 0 to ATemplate.CodeCount - 1 do
    begin
      ScanInstr := ATemplate.GetInstruction(ScanPC);
      ScanOp := DecodeOp(ScanInstr);
      case TSouffleOpCode(ScanOp) of
        OP_JUMP:
        begin
          ScanOffset := DecodeAx(ScanInstr);
          ScanTarget := ScanPC + 1 + ScanOffset;
          if ScanOffset > 0 then
          begin
            if not ForwardTargets.Contains(ScanTarget) then
              ForwardTargets.Add(ScanTarget);
          end
          else if ScanOffset < 0 then
          begin
            if LoopHeaders.TryGetValue(ScanTarget, Existing) then
            begin
              if ScanPC + 1 > Existing then
                LoopHeaders[ScanTarget] := ScanPC + 1;
            end
            else
              LoopHeaders.Add(ScanTarget, ScanPC + 1);
          end;
        end;
        OP_JUMP_IF_TRUE, OP_JUMP_IF_FALSE:
        begin
          ScanOffset := DecodesBx(ScanInstr);
          ScanTarget := ScanPC + 1 + ScanOffset;
          if ScanOffset > 0 then
          begin
            if not ForwardTargets.Contains(ScanTarget) then
              ForwardTargets.Add(ScanTarget);
          end;
        end;
        OP_JUMP_IF_NIL, OP_JUMP_IF_NOT_NIL:
        begin
          ScanTarget := ScanPC + 1 + Integer(DecodeC(ScanInstr));
          if not ForwardTargets.Contains(ScanTarget) then
            ForwardTargets.Add(ScanTarget);
        end;
      end;
    end;

    for HI := 0 to HandlerCount - 1 do
      if not ForwardTargets.Contains(HandlerCatchTargets[HI]) then
        ForwardTargets.Add(HandlerCatchTargets[HI]);

    ForwardTargets.Sort;
  end;

begin
  ScanHandlers;

  ExtraLocals := Integer(ATemplate.MaxRegisters) -
    Integer(ATemplate.ParameterCount) - 1;
  if ExtraLocals > 0 then
    ABuilder.AddLocal(WT_EXTERNREF, UInt32(ExtraLocals));
  ScratchLocal := ATemplate.MaxRegisters;
  ABuilder.AddLocal(WT_EXTERNREF, 1);

  CollectTargets;

  InteriorSet := TDictionary<Integer, Boolean>.Create;
  HandlerByStart := TDictionary<Integer, Integer>.Create;
  HandlerByEnd := TDictionary<Integer, Integer>.Create;
  LabelStack := TList<TLabelEntry>.Create;
  try
    for I := 0 to HandlerCount - 1 do
    begin
      HandlerByStart.AddOrSetValue(HandlerStarts[I], I);
      HandlerByEnd.AddOrSetValue(HandlerEnds[I], I);
      for K := 0 to ForwardTargets.Count - 1 do
        if (ForwardTargets[K] > HandlerStarts[I]) and
           (ForwardTargets[K] < HandlerEnds[I]) then
          InteriorSet.AddOrSetValue(ForwardTargets[K], True);
    end;

    // Pre-open exterior forward-target blocks (outermost = latest target)
    for I := ForwardTargets.Count - 1 downto 0 do
    begin
      if not InteriorSet.TryGetValue(ForwardTargets[I], Dummy) then
      begin
        Lbl.TargetPC := ForwardTargets[I];
        Lbl.IsLoop := False;
        Lbl.IsTry := False;
        LabelStack.Add(Lbl);
        ABuilder.EmitBlock(WT_VOID);
      end;
    end;

    for PC := 0 to ATemplate.CodeCount - 1 do
    begin
      // Close try blocks at TryEnd (before closing forward blocks)
      if HandlerByEnd.TryGetValue(PC, HIdx) then
      begin
        ABuilder.EmitCatch(FExnTagIdx);
        ABuilder.EmitLocalSet(HandlerCatchRegs[HIdx]);
        Depth := FindLabelDepth(LabelStack,
          HandlerCatchTargets[HIdx], False);
        ABuilder.EmitBr(UInt32(Depth));
        ABuilder.EmitEnd;
        if (LabelStack.Count > 0) and
           LabelStack[LabelStack.Count - 1].IsTry then
          LabelStack.Delete(LabelStack.Count - 1);
      end;

      // Close forward-target blocks at their target PC
      while (LabelStack.Count > 0) and
            (not LabelStack[LabelStack.Count - 1].IsLoop) and
            (not LabelStack[LabelStack.Count - 1].IsTry) and
            (LabelStack[LabelStack.Count - 1].TargetPC = PC) do
      begin
        ABuilder.EmitEnd;
        LabelStack.Delete(LabelStack.Count - 1);
      end;

      // Open loop blocks at their header PC
      if LoopHeaders.TryGetValue(PC, LoopEndPC) then
      begin
        Lbl.TargetPC := PC;
        Lbl.IsLoop := True;
        Lbl.IsTry := False;
        LabelStack.Add(Lbl);
        ABuilder.EmitLoop(WT_VOID);
      end;

      // Open try blocks at TryStart, then interior forward blocks
      if HandlerByStart.TryGetValue(PC, HIdx) then
      begin
        Lbl.TargetPC := -1;
        Lbl.IsLoop := False;
        Lbl.IsTry := True;
        LabelStack.Add(Lbl);
        ABuilder.EmitTry(WT_VOID);

        for K := ForwardTargets.Count - 1 downto 0 do
          if (ForwardTargets[K] > HandlerStarts[HIdx]) and
             (ForwardTargets[K] < HandlerEnds[HIdx]) then
          begin
            Lbl.TargetPC := ForwardTargets[K];
            Lbl.IsLoop := False;
            Lbl.IsTry := False;
            LabelStack.Add(Lbl);
            ABuilder.EmitBlock(WT_VOID);
          end;
      end;

      Instr := ATemplate.GetInstruction(PC);
      Op := DecodeOp(Instr);

      case TSouffleOpCode(Op) of
        OP_JUMP:
        begin
          Offset := DecodeAx(Instr);
          TargetPC := PC + 1 + Offset;
          if Offset < 0 then
            ABuilder.EmitBr(FindLabelDepth(LabelStack, TargetPC, True))
          else if Offset > 0 then
            ABuilder.EmitBr(FindLabelDepth(LabelStack, TargetPC, False))
          else
            ; // jump to next instruction = nop
        end;
        OP_JUMP_IF_TRUE:
        begin
          EmitInstruction(ABuilder, ATemplate, Instr, PC, ScratchLocal);
          Offset := DecodesBx(Instr);
          TargetPC := PC + 1 + Offset;
          if Offset > 0 then
            ABuilder.EmitBrIf(FindLabelDepth(LabelStack, TargetPC, False));
        end;
        OP_JUMP_IF_FALSE:
        begin
          EmitInstruction(ABuilder, ATemplate, Instr, PC, ScratchLocal);
          Offset := DecodesBx(Instr);
          TargetPC := PC + 1 + Offset;
          if Offset > 0 then
            ABuilder.EmitBrIf(FindLabelDepth(LabelStack, TargetPC, False));
        end;
        OP_JUMP_IF_NIL, OP_JUMP_IF_NOT_NIL:
        begin
          EmitInstruction(ABuilder, ATemplate, Instr, PC, ScratchLocal);
          TargetPC := PC + 1 + Integer(DecodeC(Instr));
          ABuilder.EmitBrIf(FindLabelDepth(LabelStack, TargetPC, False));
        end;
      else
        EmitInstruction(ABuilder, ATemplate, Instr, PC, ScratchLocal);
      end;

      // Close loop blocks at their end PC
      if (LabelStack.Count > 0) and
         LabelStack[LabelStack.Count - 1].IsLoop and
         LoopHeaders.TryGetValue(LabelStack[LabelStack.Count - 1].TargetPC, LoopEndPC) and
         (PC + 1 >= LoopEndPC) then
      begin
        ABuilder.EmitEnd;
        LabelStack.Delete(LabelStack.Count - 1);
      end;
    end;

    // Close any remaining open blocks
    while LabelStack.Count > 0 do
    begin
      ABuilder.EmitEnd;
      LabelStack.Delete(LabelStack.Count - 1);
    end;

    ABuilder.EmitEnd;
  finally
    InteriorSet.Free;
    HandlerByStart.Free;
    HandlerByEnd.Free;
    ForwardTargets.Free;
    LoopHeaders.Free;
    LabelStack.Free;
  end;
end;

function TSouffleWasmTranslator.Translate(
  const AModule: TSouffleBytecodeModule): TWasmModule;
var
  I: Integer;
  Template: TSouffleFunctionTemplate;
  ParamTypes: TWasmValTypeArray;
  TypeIdx, WasmFuncIdx: UInt32;
  Builder: TWasmCodeBuilder;
  UsedOps: TOpcodeSet;
begin
  FWasmModule := TWasmModule.Create;
  FFuncTemplates := TList<TSouffleFunctionTemplate>.Create;
  FFuncWasmIndices := TDictionary<TSouffleFunctionTemplate, UInt32>.Create;
  try
    CollectFunctions(AModule.TopLevel);
    FlattenConstantPools;
    ScanUsedOpcodes(UsedOps);
    RegisterRuntimeImports(UsedOps);

    for I := 0 to FFuncTemplates.Count - 1 do
    begin
      Template := FFuncTemplates[I];
      SetLength(ParamTypes, Template.ParameterCount + 1);
      ParamTypes[0] := WT_EXTERNREF;
      for TypeIdx := 1 to Template.ParameterCount do
        ParamTypes[TypeIdx] := WT_EXTERNREF;
      TypeIdx := FWasmModule.FindOrAddFuncType(ParamTypes, [WT_EXTERNREF]);
      Builder := FWasmModule.AddFunction(TypeIdx);
      WasmFuncIdx := FImportCount + UInt32(I);
      FFuncWasmIndices.Add(Template, WasmFuncIdx);

      TranslateFunction(Template, Builder);
    end;

    // Export all functions so the host can call them for closures
    for I := 0 to FFuncTemplates.Count - 1 do
      FWasmModule.ExportFunction('__fn_' + IntToStr(FImportCount + UInt32(I)),
        FImportCount + UInt32(I));

    // Export the top-level function as _start
    if FFuncWasmIndices.ContainsKey(AModule.TopLevel) then
      FWasmModule.ExportFunction('_start',
        FFuncWasmIndices[AModule.TopLevel]);

    SerializeConstants;

    Result := FWasmModule;
    FWasmModule := nil;
  finally
    FConstBase.Free;
    FFuncWasmIndices.Free;
    FFuncTemplates.Free;
    FreeAndNil(FWasmModule);
  end;
end;

procedure TSouffleWasmTranslator.TranslateToFile(
  const AModule: TSouffleBytecodeModule; const AFileName: string);
var
  WasmMod: TWasmModule;
begin
  WasmMod := Translate(AModule);
  try
    WasmMod.SaveToFile(AFileName);
  finally
    WasmMod.Free;
  end;
end;

function TSouffleWasmTranslator.TranslateToBytes(
  const AModule: TSouffleBytecodeModule): TBytes;
var
  WasmMod: TWasmModule;
begin
  WasmMod := Translate(AModule);
  try
    Result := WasmMod.SaveToBytes;
  finally
    WasmMod.Free;
  end;
end;

end.
