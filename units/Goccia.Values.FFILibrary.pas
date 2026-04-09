unit Goccia.Values.FFILibrary;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.FFI.DynamicLibrary,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaFFILibraryValue = class(TGocciaObjectValue)
  private
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
  private
    FHandle: TGocciaFFILibraryHandle;

    procedure InitializePrototype;
  published
    function Bind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Symbol(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function Close(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function PathGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ClosedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AHandle: TGocciaFFILibraryHandle);
    destructor Destroy; override;

    function GetProperty(const AName: string): TGocciaValue; override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const ATarget: TGocciaObjectValue);

    property Handle: TGocciaFFILibraryHandle read FHandle;
  end;

implementation

uses
  SysUtils,

  GarbageCollector.Generic,

  Goccia.FFI.Call,
  Goccia.FFI.Types,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFIPointer,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue;

const
  FFI_LIBRARY_TAG = 'FFILibrary';

  PROP_FFI_PATH   = 'path';
  PROP_FFI_CLOSED = 'closed';

// ==========================================================================
// TGocciaFFIBoundFunction — captures a symbol + signature for callbacks
// ==========================================================================

type
  TGocciaFFIBoundFunction = class
  private
    FSymbol: CodePointer;
    FSignature: TGocciaFFISignature;
    FName: string;
  published
    function Call(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ASymbol: CodePointer; const ASignature: TGocciaFFISignature; const AName: string);
  end;

constructor TGocciaFFIBoundFunction.Create(const ASymbol: CodePointer; const ASignature: TGocciaFFISignature; const AName: string);
begin
  FSymbol := ASymbol;
  FSignature := ASignature;
  FName := AName;
end;

function TGocciaFFIBoundFunction.Call(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  IntArgs: array of PtrInt;
  SingleArgs: array of Single;
  DoubleArgs: array of Double;
  TempStrings: array of AnsiString;
  I, TempCount: Integer;
  ArgValue: TGocciaValue;
  NumValue: TGocciaNumberLiteralValue;
  CallResult: TGocciaFFIResult;
  State: TGocciaFFICallState;
  {$IFDEF CPU64}
  GprIdx, FprIdx: Integer;
  {$ENDIF}
  {$IFDEF CPUI386}
  StackOffset: Integer;
  IntVal: LongInt;
  DoubleVal: Double;
  {$ENDIF}
begin
  // Validate arg count
  if AArgs.Length < FSignature.ArgCount then
    ThrowTypeError('FFI function ' + FName + ' expects ' +
      IntToStr(FSignature.ArgCount) + ' arguments, got ' +
      IntToStr(AArgs.Length));

  // Marshal arguments into native arrays
  SetLength(IntArgs, 0);
  SetLength(SingleArgs, 0);
  SetLength(DoubleArgs, 0);
  TempCount := 0;

  case FSignature.ArgClass of
    facInteger:
    begin
      SetLength(IntArgs, FSignature.ArgCount);
      SetLength(TempStrings, FSignature.ArgCount);
      for I := 0 to FSignature.ArgCount - 1 do
      begin
        ArgValue := AArgs.GetElement(I);
        case FSignature.ArgTypes[I] of
          fftBool:
            if ArgValue.ToBooleanLiteral.Value then
              IntArgs[I] := 1
            else
              IntArgs[I] := 0;
          fftI8, fftI16, fftI32:
          begin
            NumValue := ArgValue.ToNumberLiteral;
            IntArgs[I] := PtrInt(Trunc(NumValue.Value));
          end;
          fftI64:
          begin
            NumValue := ArgValue.ToNumberLiteral;
            IntArgs[I] := PtrInt(Int64(Trunc(NumValue.Value)));
          end;
          fftU8, fftU16, fftU32:
          begin
            NumValue := ArgValue.ToNumberLiteral;
            IntArgs[I] := PtrInt(PtrUInt(Trunc(NumValue.Value)));
          end;
          fftU64:
          begin
            NumValue := ArgValue.ToNumberLiteral;
            IntArgs[I] := PtrInt(QWord(Trunc(NumValue.Value)));
          end;
          fftPointer:
          begin
            if ArgValue is TGocciaFFIPointerValue then
              IntArgs[I] := PtrInt(TGocciaFFIPointerValue(ArgValue).Address)
            else if ArgValue is TGocciaArrayBufferValue then
            begin
              if Length(TGocciaArrayBufferValue(ArgValue).Data) = 0 then
                IntArgs[I] := 0
              else
                IntArgs[I] := PtrInt(@TGocciaArrayBufferValue(ArgValue).Data[0]);
            end
            else if ArgValue is TGocciaSharedArrayBufferValue then
            begin
              if Length(TGocciaSharedArrayBufferValue(ArgValue).Data) = 0 then
                IntArgs[I] := 0
              else
                IntArgs[I] := PtrInt(@TGocciaSharedArrayBufferValue(ArgValue).Data[0]);
            end
            else if ArgValue is TGocciaTypedArrayValue then
            begin
              if Length(TGocciaTypedArrayValue(ArgValue).BufferData) = 0 then
                IntArgs[I] := 0
              else
                IntArgs[I] := PtrInt(@TGocciaTypedArrayValue(ArgValue).BufferData[
                  TGocciaTypedArrayValue(ArgValue).ByteOffset]);
            end
            else if ArgValue is TGocciaNullLiteralValue then
              IntArgs[I] := 0
            else
              ThrowTypeError('FFI argument ' + IntToStr(I) +
                ' must be an ArrayBuffer, TypedArray, FFIPointer, or null');
          end;
          fftCString:
          begin
            TempStrings[TempCount] := AnsiString(ArgValue.ToStringLiteral.Value);
            IntArgs[I] := PtrInt(PAnsiChar(TempStrings[TempCount]));
            Inc(TempCount);
          end;
        end;
      end;
    end;
    facSingle:
    begin
      SetLength(SingleArgs, FSignature.ArgCount);
      for I := 0 to FSignature.ArgCount - 1 do
      begin
        NumValue := AArgs.GetElement(I).ToNumberLiteral;
        SingleArgs[I] := NumValue.Value;
      end;
    end;
    facDouble:
    begin
      SetLength(DoubleArgs, FSignature.ArgCount);
      for I := 0 to FSignature.ArgCount - 1 do
      begin
        NumValue := AArgs.GetElement(I).ToNumberLiteral;
        DoubleArgs[I] := NumValue.Value;
      end;
    end;
    facMixed:
    begin
      {$IFDEF CPU64}
      // Fill TGocciaFFICallState with separate GPR/FPR counters and
      // call the assembly trampoline directly
      FillChar(State, SizeOf(State), 0);
      State.FuncPtr := FSymbol;
      GprIdx := 0;
      FprIdx := 0;
      SetLength(TempStrings, FSignature.ArgCount);
      for I := 0 to FSignature.ArgCount - 1 do
      begin
        ArgValue := AArgs.GetElement(I);
        if FFITypeToArgClass(FSignature.ArgTypes[I]) = facDouble then
        begin
          NumValue := ArgValue.ToNumberLiteral;
          {$IFDEF MSWINDOWS}
          // Win64: positional — double goes in both Gpr[I] and Fpr[I]
          State.Fpr[I] := NumValue.Value;
          Move(State.Fpr[I], State.Gpr[I], SizeOf(Double));
          {$ELSE}
          // System V / AArch64: separate counter
          State.Fpr[FprIdx] := NumValue.Value;
          Inc(FprIdx);
          {$ENDIF}
        end
        else
        begin
          // Integer-class argument
          case FSignature.ArgTypes[I] of
            fftBool:
              if ArgValue.ToBooleanLiteral.Value then
                {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := 1
              else
                {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := 0;
            fftI8, fftI16, fftI32:
            begin
              NumValue := ArgValue.ToNumberLiteral;
              {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(Trunc(NumValue.Value));
            end;
            fftI64:
            begin
              NumValue := ArgValue.ToNumberLiteral;
              {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(Int64(Trunc(NumValue.Value)));
            end;
            fftU8, fftU16, fftU32:
            begin
              NumValue := ArgValue.ToNumberLiteral;
              {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(PtrUInt(Trunc(NumValue.Value)));
            end;
            fftU64:
            begin
              NumValue := ArgValue.ToNumberLiteral;
              {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(QWord(Trunc(NumValue.Value)));
            end;
            fftPointer:
            begin
              if ArgValue is TGocciaFFIPointerValue then
                {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(TGocciaFFIPointerValue(ArgValue).Address)
              else if ArgValue is TGocciaArrayBufferValue then
              begin
                if Length(TGocciaArrayBufferValue(ArgValue).Data) = 0 then
                  {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := 0
                else
                  {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(@TGocciaArrayBufferValue(ArgValue).Data[0]);
              end
              else if ArgValue is TGocciaSharedArrayBufferValue then
              begin
                if Length(TGocciaSharedArrayBufferValue(ArgValue).Data) = 0 then
                  {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := 0
                else
                  {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(@TGocciaSharedArrayBufferValue(ArgValue).Data[0]);
              end
              else if ArgValue is TGocciaTypedArrayValue then
              begin
                if Length(TGocciaTypedArrayValue(ArgValue).BufferData) = 0 then
                  {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := 0
                else
                  {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(@TGocciaTypedArrayValue(ArgValue).BufferData[
                    TGocciaTypedArrayValue(ArgValue).ByteOffset]);
              end
              else if ArgValue is TGocciaNullLiteralValue then
                {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := 0
              else
                ThrowTypeError('FFI argument ' + IntToStr(I) +
                  ' must be an ArrayBuffer, TypedArray, FFIPointer, or null');
            end;
            fftCString:
            begin
              TempStrings[TempCount] := AnsiString(ArgValue.ToStringLiteral.Value);
              {$IFDEF MSWINDOWS}State.Gpr[I]{$ELSE}State.Gpr[GprIdx]{$ENDIF} := PtrInt(PAnsiChar(TempStrings[TempCount]));
              Inc(TempCount);
            end;
          end;
          {$IFNDEF MSWINDOWS}
          Inc(GprIdx);
          {$ENDIF}
        end;
      end;
      State.GprCount := GprIdx;
      State.FprCount := FprIdx;
      FFITrampolineCall(State);
      CallResult.AsInt := State.RetInt;
      CallResult.AsDouble := State.RetFloat;
      {$ENDIF}
      {$IFDEF CPUI386}
      // i386 cdecl: pre-pack all args into StackBuf in left-to-right order.
      // The trampoline copies this buffer to ESP and calls.
      FillChar(State, SizeOf(State), 0);
      State.FuncPtr := FSymbol;
      State.ReturnIsFloat := FSignature.ReturnClass in [frcSingle, frcDouble];
      StackOffset := 0;
      SetLength(TempStrings, FSignature.ArgCount);
      for I := 0 to FSignature.ArgCount - 1 do
      begin
        ArgValue := AArgs.GetElement(I);
        if FFITypeToArgClass(FSignature.ArgTypes[I]) = facDouble then
        begin
          DoubleVal := ArgValue.ToNumberLiteral.Value;
          Move(DoubleVal, State.StackBuf[StackOffset], 8);
          Inc(StackOffset, 8);
        end
        else
        begin
          case FSignature.ArgTypes[I] of
            fftBool:
              if ArgValue.ToBooleanLiteral.Value then
                IntVal := 1
              else
                IntVal := 0;
            fftI8, fftI16, fftI32:
              IntVal := LongInt(Trunc(ArgValue.ToNumberLiteral.Value));
            fftU8, fftU16, fftU32:
              IntVal := LongInt(LongWord(Trunc(ArgValue.ToNumberLiteral.Value)));
            fftPointer:
            begin
              if ArgValue is TGocciaFFIPointerValue then
                IntVal := LongInt(TGocciaFFIPointerValue(ArgValue).Address)
              else if ArgValue is TGocciaArrayBufferValue then
              begin
                if Length(TGocciaArrayBufferValue(ArgValue).Data) = 0 then
                  IntVal := 0
                else
                  IntVal := LongInt(@TGocciaArrayBufferValue(ArgValue).Data[0]);
              end
              else if ArgValue is TGocciaSharedArrayBufferValue then
              begin
                if Length(TGocciaSharedArrayBufferValue(ArgValue).Data) = 0 then
                  IntVal := 0
                else
                  IntVal := LongInt(@TGocciaSharedArrayBufferValue(ArgValue).Data[0]);
              end
              else if ArgValue is TGocciaTypedArrayValue then
              begin
                if Length(TGocciaTypedArrayValue(ArgValue).BufferData) = 0 then
                  IntVal := 0
                else
                  IntVal := LongInt(@TGocciaTypedArrayValue(ArgValue).BufferData[
                    TGocciaTypedArrayValue(ArgValue).ByteOffset]);
              end
              else if ArgValue is TGocciaNullLiteralValue then
                IntVal := 0
              else
                ThrowTypeError('FFI argument ' + IntToStr(I) +
                  ' must be an ArrayBuffer, TypedArray, FFIPointer, or null');
            end;
            fftCString:
            begin
              TempStrings[TempCount] := AnsiString(ArgValue.ToStringLiteral.Value);
              IntVal := LongInt(PAnsiChar(TempStrings[TempCount]));
              Inc(TempCount);
            end;
          else
            IntVal := LongInt(Trunc(ArgValue.ToNumberLiteral.Value));
          end;
          Move(IntVal, State.StackBuf[StackOffset], 4);
          Inc(StackOffset, 4);
        end;
      end;
      State.StackSize := StackOffset;
      FFITrampolineCall(State);
      CallResult.AsInt := State.RetInt;
      CallResult.AsDouble := State.RetFloat;
      {$ENDIF}
    end;
  end;

  // Dispatch the homogeneous call (mixed already dispatched above via trampoline)
  if FSignature.ArgClass <> facMixed then
    FFIDispatchCall(FSymbol, FSignature.ArgCount, FSignature.ArgClass,
      FSignature.ReturnClass, IntArgs, SingleArgs, DoubleArgs, CallResult);

  // Unmarshal return value
  case FSignature.ReturnType of
    fftVoid:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    fftBool:
      if CallResult.AsInt <> 0 then
        Result := TGocciaBooleanLiteralValue.TrueValue
      else
        Result := TGocciaBooleanLiteralValue.FalseValue;
    fftI8:
      Result := TGocciaNumberLiteralValue.Create(ShortInt(CallResult.AsInt));
    fftI16:
      Result := TGocciaNumberLiteralValue.Create(SmallInt(CallResult.AsInt));
    fftI32:
      Result := TGocciaNumberLiteralValue.Create(LongInt(CallResult.AsInt));
    fftI64:
      Result := TGocciaNumberLiteralValue.Create(Int64(CallResult.AsInt));
    fftU8:
      Result := TGocciaNumberLiteralValue.Create(Byte(CallResult.AsInt));
    fftU16:
      Result := TGocciaNumberLiteralValue.Create(Word(CallResult.AsInt));
    fftU32:
      Result := TGocciaNumberLiteralValue.Create(LongWord(CallResult.AsInt));
    fftU64:
      Result := TGocciaNumberLiteralValue.Create(QWord(CallResult.AsInt));
    fftF32:
      Result := TGocciaNumberLiteralValue.Create(CallResult.AsSingle);
    fftF64:
      Result := TGocciaNumberLiteralValue.Create(CallResult.AsDouble);
    fftPointer:
      Result := TGocciaFFIPointerValue.Create(Pointer(CallResult.AsInt));
    fftCString:
    begin
      if CallResult.AsInt = 0 then
        Result := TGocciaNullLiteralValue.NullValue
      else
        Result := TGocciaStringLiteralValue.Create(string(PAnsiChar(CallResult.AsInt)));
    end;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ==========================================================================
// TGocciaFFILibraryValue
// ==========================================================================

constructor TGocciaFFILibraryValue.Create(const AHandle: TGocciaFFILibraryHandle);
begin
  inherited Create;
  FHandle := AHandle;
  InitializePrototype;
  if Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

destructor TGocciaFFILibraryValue.Destroy;
begin
  FHandle.Free;
  inherited;
end;

procedure TGocciaFFILibraryValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod('bind', Bind, 2, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('symbol', Symbol, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('close', Close, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddAccessor(PROP_FFI_PATH, PathGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_FFI_CLOSED, ClosedGetter, nil, [pfConfigurable]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create(FFI_LIBRARY_TAG),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaFFILibraryValue.ExposePrototype(const ATarget: TGocciaObjectValue);
begin
  // Prototype is initialized lazily on first Create; nothing to expose on a constructor
end;

function TGocciaFFILibraryValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_FFI_PATH then
    Result := TGocciaStringLiteralValue.Create(FHandle.Path)
  else if AName = PROP_FFI_CLOSED then
  begin
    if FHandle.IsClosed then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
    Result := inherited GetProperty(AName);
end;

function TGocciaFFILibraryValue.ToStringTag: string;
begin
  Result := FFI_LIBRARY_TAG;
end;

procedure TGocciaFFILibraryValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

// -- Prototype methods ------------------------------------------------------

function ParseSignatureFromArgs(const AArgs: TGocciaArgumentsCollection; const AFuncName: string): TGocciaFFISignature;
var
  SigObj: TGocciaObjectValue;
  ArgsField, ReturnsField: TGocciaValue;
  ArgsArray: TGocciaArrayValue;
  ReturnStr: string;
  I: Integer;
  TypeName: string;
  ValidationError: string;
begin
  if AArgs.Length < 2 then
    ThrowTypeError('bind requires a function name and a signature object');

  if not (AArgs.GetElement(1) is TGocciaObjectValue) then
    ThrowTypeError('bind second argument must be a signature object { args: [...], returns: "..." }');

  SigObj := TGocciaObjectValue(AArgs.GetElement(1));

  // Parse args array
  ArgsField := SigObj.GetProperty('args');
  if ArgsField is TGocciaArrayValue then
  begin
    ArgsArray := TGocciaArrayValue(ArgsField);
    Result.ArgCount := ArgsArray.Elements.Count;
    SetLength(Result.ArgTypes, Result.ArgCount);
    for I := 0 to Result.ArgCount - 1 do
    begin
      TypeName := ArgsArray.Elements[I].ToStringLiteral.Value;
      Result.ArgTypes[I] := ParseFFIType(TypeName);
      if (Result.ArgTypes[I] = fftVoid) and (TypeName <> FFI_TYPE_VOID) then
        ThrowTypeError('Unknown FFI type: ' + TypeName);
      if Result.ArgTypes[I] = fftVoid then
        ThrowTypeError('void is not a valid argument type');
    end;
  end
  else if (ArgsField = nil) or (ArgsField is TGocciaUndefinedLiteralValue) then
  begin
    Result.ArgCount := 0;
    SetLength(Result.ArgTypes, 0);
  end
  else
    ThrowTypeError('signature args must be an array of type strings');

  // Parse return type
  ReturnsField := SigObj.GetProperty('returns');
  if ReturnsField is TGocciaStringLiteralValue then
  begin
    ReturnStr := TGocciaStringLiteralValue(ReturnsField).Value;
    Result.ReturnType := ParseFFIType(ReturnStr);
    if (Result.ReturnType = fftVoid) and (ReturnStr <> FFI_TYPE_VOID) then
      ThrowTypeError('Unknown FFI return type: ' + ReturnStr);
  end
  else if (ReturnsField = nil) or (ReturnsField is TGocciaUndefinedLiteralValue) then
    Result.ReturnType := fftVoid
  else
    ThrowTypeError('signature returns must be a type string');

  // Validate
  ValidationError := ValidateSignature(Result);
  if ValidationError <> '' then
    ThrowTypeError(ValidationError);
end;

function TGocciaFFILibraryValue.Bind(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Lib: TGocciaFFILibraryValue;
  FuncName: string;
  Sig: TGocciaFFISignature;
  SymbolPtr: CodePointer;
  BoundFunc: TGocciaFFIBoundFunction;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError('bind requires an FFILibrary');
  Lib := TGocciaFFILibraryValue(AThisValue);

  if Lib.FHandle.IsClosed then
    ThrowTypeError('Cannot bind from a closed library');

  FuncName := AArgs.GetElement(0).ToStringLiteral.Value;
  Sig := ParseSignatureFromArgs(AArgs, FuncName);

  try
    SymbolPtr := Lib.FHandle.FindSymbol(FuncName);
  except
    on E: Exception do
      ThrowTypeError(E.Message);
  end;
  BoundFunc := TGocciaFFIBoundFunction.Create(SymbolPtr, Sig, FuncName);
  Result := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    BoundFunc.Call, FuncName, Sig.ArgCount);
end;

function TGocciaFFILibraryValue.Symbol(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Lib: TGocciaFFILibraryValue;
  SymName: string;
  SymPtr: CodePointer;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError('symbol requires an FFILibrary');
  Lib := TGocciaFFILibraryValue(AThisValue);

  if Lib.FHandle.IsClosed then
    ThrowTypeError('Cannot look up symbol in a closed library');

  if AArgs.Length < 1 then
    ThrowTypeError('symbol requires a symbol name');

  SymName := AArgs.GetElement(0).ToStringLiteral.Value;
  try
    SymPtr := Lib.FHandle.FindSymbol(SymName);
  except
    on E: Exception do
      ThrowTypeError(E.Message);
  end;
  Result := TGocciaFFIPointerValue.Create(Pointer(SymPtr));
end;

function TGocciaFFILibraryValue.Close(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Lib: TGocciaFFILibraryValue;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError('close requires an FFILibrary');
  Lib := TGocciaFFILibraryValue(AThisValue);
  Lib.FHandle.Close;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaFFILibraryValue.PathGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError('FFILibrary.path requires an FFILibrary');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaFFILibraryValue(AThisValue).FHandle.Path);
end;

function TGocciaFFILibraryValue.ClosedGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaFFILibraryValue) then
    ThrowTypeError('FFILibrary.closed requires an FFILibrary');
  if TGocciaFFILibraryValue(AThisValue).FHandle.IsClosed then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

end.
