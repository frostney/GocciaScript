unit Goccia.FFI.ABI;

{$I Goccia.inc}

interface

uses
  Goccia.FFI.Types;

type
  TGocciaFFIABI = (
    fabiSysVX64,
    fabiWin64,
    fabiAAPCS64,
    fabiDarwinARM64,
    fabiI386Win
  );

  TGocciaFFIPlacementKind = (fpkGPR, fpkFPR, fpkStack);

  TGocciaFFIPlacement = record
    Kind: TGocciaFFIPlacementKind;
    RegisterIndex: Integer;
    StackOffset: Integer;
    ValueOffset: Integer;
    Size: Integer;
  end;

  TGocciaFFIPlacementArray = array of TGocciaFFIPlacement;

  TGocciaFFIArgumentPlan = record
    TypeDescriptor: TGocciaFFITypeDescriptor;
    Placements: TGocciaFFIPlacementArray;
    Indirect: Boolean;
    IndirectCopyOffset: Integer;
  end;

  TGocciaFFIReturnPlan = record
    TypeDescriptor: TGocciaFFITypeDescriptor;
    Placements: TGocciaFFIPlacementArray;
    UsesHiddenPointer: Boolean;
  end;

  TGocciaFFICompiledSignature = class
  private
    FABI: TGocciaFFIABI;
    FArguments: array of TGocciaFFIArgumentPlan;
    FReturnPlan: TGocciaFFIReturnPlan;
    FStackSize: Integer;
    FScratchSize: Integer;
    FGPRCount: Integer;
    FFPRCount: Integer;
    FVariadicStartIndex: Integer;
    function GetArgumentCount: Integer;
    function GetArgument(const AIndex: Integer): TGocciaFFIArgumentPlan;
  public
    constructor Create(const AABI: TGocciaFFIABI;
      const AArguments: array of TGocciaFFITypeDescriptor;
      const AReturnType: TGocciaFFITypeDescriptor;
      const AVariadicStartIndex: Integer = -1);
    destructor Destroy; override;
    property ABI: TGocciaFFIABI read FABI;
    property ArgumentCount: Integer read GetArgumentCount;
    property Arguments[const AIndex: Integer]: TGocciaFFIArgumentPlan
      read GetArgument;
    property ReturnPlan: TGocciaFFIReturnPlan read FReturnPlan;
    property StackSize: Integer read FStackSize;
    property ScratchSize: Integer read FScratchSize;
    property GPRCount: Integer read FGPRCount;
    property FPRCount: Integer read FFPRCount;
  end;

function CurrentFFIABI: TGocciaFFIABI;

implementation

uses
  SysUtils;

type
  TSysVEightbyteClass = (secNone, secInteger, secSSE, secMemory);
  TSysVEightbyteClasses = array[0..1] of TSysVEightbyteClass;
  THFAOffsets = array of Integer;

function CurrentFFIABI: TGocciaFFIABI;
begin
  {$IF (defined(GOCCIA_CPU_X86_64)) and defined(MSWINDOWS)}
  Result := fabiWin64;
  {$ELSEIF (defined(GOCCIA_CPU_X86_64))}
  Result := fabiSysVX64;
  {$ELSEIF (defined(GOCCIA_CPU_AARCH64)) and defined(DARWIN)}
  Result := fabiDarwinARM64;
  {$ELSEIF (defined(GOCCIA_CPU_AARCH64))}
  Result := fabiAAPCS64;
  {$ELSEIF (defined(GOCCIA_CPU_X86)) and defined(MSWINDOWS)}
  Result := fabiI386Win;
  {$ELSE}
  {$ERROR Unsupported FFI ABI target}
  {$ENDIF}
end;

function TypeIsFloatScalar(const AType: TGocciaFFITypeDescriptor): Boolean;
begin
  Result := (AType.Kind = ftkScalar) and
    (AType.ScalarType in [fftF32, fftF64]);
end;

function TypeIsIntegerScalar(const AType: TGocciaFFITypeDescriptor): Boolean;
begin
  Result := (AType.Kind in [ftkCallback, ftkNullable]) or
    ((AType.Kind = ftkScalar) and
     not (AType.ScalarType in [fftVoid, fftF32, fftF64]));
end;

function HFAElementSize(const AType: TGocciaFFIType): Integer;
begin
  if AType = fftF32 then
    Result := 4
  else
    Result := 8;
end;

procedure AppendPlacement(var APlacements: TGocciaFFIPlacementArray;
  const AKind: TGocciaFFIPlacementKind; const ARegisterIndex,
  AStackOffset, AValueOffset, ASize: Integer);
var
  Index: Integer;
begin
  Index := Length(APlacements);
  SetLength(APlacements, Index + 1);
  APlacements[Index].Kind := AKind;
  APlacements[Index].RegisterIndex := ARegisterIndex;
  APlacements[Index].StackOffset := AStackOffset;
  APlacements[Index].ValueOffset := AValueOffset;
  APlacements[Index].Size := ASize;
end;

function StackAppend(var AStackOffset: Integer; const ASize,
  AAlignment, ASlotAlignment: Integer): Integer;
var
  EffectiveAlignment, SlotSize: Integer;
begin
  EffectiveAlignment := AAlignment;
  if EffectiveAlignment > ASlotAlignment then
    EffectiveAlignment := ASlotAlignment;
  if EffectiveAlignment < 1 then
    EffectiveAlignment := 1;
  AStackOffset := AlignFFIOffset(AStackOffset, EffectiveAlignment);
  Result := AStackOffset;
  SlotSize := AlignFFIOffset(ASize, ASlotAlignment);
  if SlotSize > MAX_FFI_AGGREGATE_SIZE - AStackOffset then
    raise EArgumentOutOfRangeException.Create('FFI call stack image exceeds limit');
  Inc(AStackOffset, SlotSize);
end;

function ScratchAppend(var AScratchOffset: Integer; const ASize,
  AAlignment: Integer): Integer;
begin
  AScratchOffset := AlignFFIOffset(AScratchOffset, AAlignment);
  Result := AScratchOffset;
  if ASize > MAX_FFI_AGGREGATE_SIZE - AScratchOffset then
    raise EArgumentOutOfRangeException.Create('FFI call scratch image exceeds limit');
  Inc(AScratchOffset, ASize);
end;

function MergeSysVClass(const ALeft,
  ARight: TSysVEightbyteClass): TSysVEightbyteClass;
begin
  if ALeft = secNone then Exit(ARight);
  if ARight = secNone then Exit(ALeft);
  if ALeft = ARight then Exit(ALeft);
  if (ALeft = secMemory) or (ARight = secMemory) then Exit(secMemory);
  if (ALeft = secInteger) or (ARight = secInteger) then Exit(secInteger);
  Result := secSSE;
end;

procedure MergeSysVAtOffset(const AClass: TSysVEightbyteClass;
  const AOffset, ASize: Integer; var AClasses: TSysVEightbyteClasses);
var
  FirstChunk, LastChunk, I: Integer;
begin
  if ASize = 0 then Exit;
  FirstChunk := AOffset div 8;
  LastChunk := (AOffset + ASize - 1) div 8;
  if LastChunk > 1 then
  begin
    AClasses[0] := secMemory;
    AClasses[1] := secMemory;
    Exit;
  end;
  for I := FirstChunk to LastChunk do
    AClasses[I] := MergeSysVClass(AClasses[I], AClass);
end;

procedure ClassifySysV(const AType: TGocciaFFITypeDescriptor;
  const ABaseOffset: Integer; var AClasses: TSysVEightbyteClasses);
var
  I: Integer;
  Field: TGocciaFFIFieldDescriptor;
begin
  if AType.Size > 16 then
  begin
    AClasses[0] := secMemory;
    AClasses[1] := secMemory;
    Exit;
  end;
  if TypeIsFloatScalar(AType) then
  begin
    MergeSysVAtOffset(secSSE, ABaseOffset, AType.Size, AClasses);
    Exit;
  end;
  if TypeIsIntegerScalar(AType) then
  begin
    MergeSysVAtOffset(secInteger, ABaseOffset, AType.Size, AClasses);
    Exit;
  end;
  case AType.Kind of
    ftkStruct:
      for I := 0 to AType.FieldCount - 1 do
      begin
        Field := AType.FieldAt(I);
        ClassifySysV(Field.TypeDescriptor, ABaseOffset + Field.Offset,
          AClasses);
      end;
    ftkUnion:
      for I := 0 to AType.FieldCount - 1 do
        ClassifySysV(AType.FieldAt(I).TypeDescriptor, ABaseOffset, AClasses);
    ftkArray:
      for I := 0 to AType.ElementCount - 1 do
        ClassifySysV(AType.ElementType,
          ABaseOffset + (I * AType.ElementType.Size), AClasses);
  else
    MergeSysVAtOffset(secMemory, ABaseOffset, AType.Size, AClasses);
  end;
end;

function CollectHFA(const AType: TGocciaFFITypeDescriptor;
  const ABaseOffset: Integer; var AScalarType: TGocciaFFIType;
  var AOffsets: THFAOffsets): Boolean;
var
  I, J, Index: Integer;
  Field: TGocciaFFIFieldDescriptor;
  MemberScalarType, UnionScalarType: TGocciaFFIType;
  MemberOffsets, UnionOffsets: THFAOffsets;
begin
  if TypeIsFloatScalar(AType) then
  begin
    if (Length(AOffsets) > 0) and (AScalarType <> AType.ScalarType) then
      Exit(False);
    AScalarType := AType.ScalarType;
    Index := Length(AOffsets);
    SetLength(AOffsets, Index + 1);
    AOffsets[Index] := ABaseOffset;
    Exit(Length(AOffsets) <= 4);
  end;
  case AType.Kind of
    ftkStruct:
      for I := 0 to AType.FieldCount - 1 do
      begin
        Field := AType.FieldAt(I);
        if not CollectHFA(Field.TypeDescriptor, ABaseOffset + Field.Offset,
          AScalarType, AOffsets) then
          Exit(False);
      end;
    ftkUnion:
    begin
      SetLength(UnionOffsets, 0);
      UnionScalarType := fftVoid;
      for I := 0 to AType.FieldCount - 1 do
      begin
        SetLength(MemberOffsets, 0);
        MemberScalarType := fftVoid;
        if not CollectHFA(AType.FieldAt(I).TypeDescriptor, ABaseOffset,
          MemberScalarType, MemberOffsets) then
          Exit(False);
        if (I > 0) and (MemberScalarType <> UnionScalarType) then
          Exit(False);
        if I = 0 then UnionScalarType := MemberScalarType;
        if Length(MemberOffsets) > Length(UnionOffsets) then
        begin
          SetLength(UnionOffsets, Length(MemberOffsets));
          for J := 0 to High(MemberOffsets) do
            UnionOffsets[J] := MemberOffsets[J];
        end;
      end;
      if (Length(AOffsets) > 0) and (AScalarType <> UnionScalarType) then
        Exit(False);
      AScalarType := UnionScalarType;
      Index := Length(AOffsets);
      SetLength(AOffsets, Index + Length(UnionOffsets));
      for I := 0 to High(UnionOffsets) do
        AOffsets[Index + I] := UnionOffsets[I];
      if Length(AOffsets) > 4 then Exit(False);
    end;
    ftkArray:
      for I := 0 to AType.ElementCount - 1 do
        if not CollectHFA(AType.ElementType,
          ABaseOffset + (I * AType.ElementType.Size), AScalarType,
          AOffsets) then
          Exit(False);
  else
    Exit(False);
  end;
  Result := (Length(AOffsets) > 0) and (Length(AOffsets) <= 4);
end;

procedure CompileSysVReturn(var APlan: TGocciaFFIReturnPlan;
  var AGPRCount: Integer);
var
  Classes: TSysVEightbyteClasses;
  I, ChunkCount, GPRIndex, FPRIndex, ChunkSize: Integer;
begin
  if APlan.TypeDescriptor.Kind = ftkScalar then
  begin
    if APlan.TypeDescriptor.ScalarType = fftVoid then Exit;
    if TypeIsFloatScalar(APlan.TypeDescriptor) then
      AppendPlacement(APlan.Placements, fpkFPR, 0, 0, 0,
        APlan.TypeDescriptor.Size)
    else
      AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
        APlan.TypeDescriptor.Size);
    Exit;
  end;
  if APlan.TypeDescriptor.Kind = ftkCallback then
  begin
    AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
      SizeOf(Pointer));
    Exit;
  end;
  Classes[0] := secNone;
  Classes[1] := secNone;
  ClassifySysV(APlan.TypeDescriptor, 0, Classes);
  if Classes[0] = secMemory then
  begin
    APlan.UsesHiddenPointer := True;
    AGPRCount := 1;
    Exit;
  end;
  ChunkCount := (APlan.TypeDescriptor.Size + 7) div 8;
  GPRIndex := 0;
  FPRIndex := 0;
  for I := 0 to ChunkCount - 1 do
  begin
    ChunkSize := APlan.TypeDescriptor.Size - (I * 8);
    if ChunkSize > 8 then ChunkSize := 8;
    if Classes[I] = secSSE then
    begin
      AppendPlacement(APlan.Placements, fpkFPR, FPRIndex, 0, I * 8,
        ChunkSize);
      Inc(FPRIndex);
    end
    else
    begin
      AppendPlacement(APlan.Placements, fpkGPR, GPRIndex, 0, I * 8,
        ChunkSize);
      Inc(GPRIndex);
    end;
  end;
end;

procedure CompileSysVArgument(var APlan: TGocciaFFIArgumentPlan;
  var AGPRCount, AFPRCount, AStackOffset: Integer);
var
  Classes: TSysVEightbyteClasses;
  I, NeededGPR, NeededFPR, ChunkCount, ChunkSize, StackOffset: Integer;
begin
  if TypeIsFloatScalar(APlan.TypeDescriptor) then
  begin
    if AFPRCount < 8 then
    begin
      AppendPlacement(APlan.Placements, fpkFPR, AFPRCount, 0, 0,
        APlan.TypeDescriptor.Size);
      Inc(AFPRCount);
    end
    else
    begin
      StackOffset := StackAppend(AStackOffset,
        APlan.TypeDescriptor.Size, APlan.TypeDescriptor.Alignment, 8);
      AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
        APlan.TypeDescriptor.Size);
    end;
    Exit;
  end;
  if TypeIsIntegerScalar(APlan.TypeDescriptor) then
  begin
    if AGPRCount < 6 then
    begin
      AppendPlacement(APlan.Placements, fpkGPR, AGPRCount, 0, 0,
        APlan.TypeDescriptor.Size);
      Inc(AGPRCount);
    end
    else
    begin
      StackOffset := StackAppend(AStackOffset,
        APlan.TypeDescriptor.Size, APlan.TypeDescriptor.Alignment, 8);
      AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
        APlan.TypeDescriptor.Size);
    end;
    Exit;
  end;

  Classes[0] := secNone;
  Classes[1] := secNone;
  ClassifySysV(APlan.TypeDescriptor, 0, Classes);
  if Classes[0] = secMemory then
  begin
    StackOffset := StackAppend(AStackOffset, APlan.TypeDescriptor.Size,
      APlan.TypeDescriptor.Alignment, 8);
    AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
      APlan.TypeDescriptor.Size);
    Exit;
  end;
  ChunkCount := (APlan.TypeDescriptor.Size + 7) div 8;
  NeededGPR := 0;
  NeededFPR := 0;
  for I := 0 to ChunkCount - 1 do
    if Classes[I] = secSSE then Inc(NeededFPR) else Inc(NeededGPR);
  if (AGPRCount + NeededGPR > 6) or (AFPRCount + NeededFPR > 8) then
  begin
    StackOffset := StackAppend(AStackOffset, APlan.TypeDescriptor.Size,
      APlan.TypeDescriptor.Alignment, 8);
    AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
      APlan.TypeDescriptor.Size);
    Exit;
  end;
  for I := 0 to ChunkCount - 1 do
  begin
    ChunkSize := APlan.TypeDescriptor.Size - (I * 8);
    if ChunkSize > 8 then ChunkSize := 8;
    if Classes[I] = secSSE then
    begin
      AppendPlacement(APlan.Placements, fpkFPR, AFPRCount, 0, I * 8,
        ChunkSize);
      Inc(AFPRCount);
    end
    else
    begin
      AppendPlacement(APlan.Placements, fpkGPR, AGPRCount, 0, I * 8,
        ChunkSize);
      Inc(AGPRCount);
    end;
  end;
end;

procedure CompileARM64Return(var APlan: TGocciaFFIReturnPlan);
var
  HFAType: TGocciaFFIType;
  HFAOffsets: THFAOffsets;
  I, ChunkCount, ChunkSize: Integer;
begin
  if APlan.TypeDescriptor.Kind = ftkScalar then
  begin
    if APlan.TypeDescriptor.ScalarType = fftVoid then Exit;
    if TypeIsFloatScalar(APlan.TypeDescriptor) then
      AppendPlacement(APlan.Placements, fpkFPR, 0, 0, 0,
        APlan.TypeDescriptor.Size)
    else
      AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
        APlan.TypeDescriptor.Size);
    Exit;
  end;
  if APlan.TypeDescriptor.Kind = ftkCallback then
  begin
    AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
      SizeOf(Pointer));
    Exit;
  end;
  SetLength(HFAOffsets, 0);
  if CollectHFA(APlan.TypeDescriptor, 0, HFAType, HFAOffsets) then
  begin
    for I := 0 to High(HFAOffsets) do
      AppendPlacement(APlan.Placements, fpkFPR, I, 0, HFAOffsets[I],
        HFAElementSize(HFAType));
    Exit;
  end;
  if APlan.TypeDescriptor.Size > 16 then
  begin
    APlan.UsesHiddenPointer := True;
    Exit;
  end;
  ChunkCount := (APlan.TypeDescriptor.Size + 7) div 8;
  for I := 0 to ChunkCount - 1 do
  begin
    ChunkSize := APlan.TypeDescriptor.Size - (I * 8);
    if ChunkSize > 8 then ChunkSize := 8;
    AppendPlacement(APlan.Placements, fpkGPR, I, 0, I * 8, ChunkSize);
  end;
end;

procedure CompileARM64Argument(var APlan: TGocciaFFIArgumentPlan;
  var AGPRCount, AFPRCount, AStackOffset, AScratchOffset: Integer;
  const ADarwin: Boolean);
var
  HFAType: TGocciaFFIType;
  HFAOffsets: THFAOffsets;
  I, ChunkCount, ChunkSize, StackOffset, SlotAlignment: Integer;
  PointerPlacement: TGocciaFFIPlacement;
  IsHFA: Boolean;
begin
  SlotAlignment := 8;
  if TypeIsFloatScalar(APlan.TypeDescriptor) then
  begin
    if AFPRCount < 8 then
    begin
      AppendPlacement(APlan.Placements, fpkFPR, AFPRCount, 0, 0,
        APlan.TypeDescriptor.Size);
      Inc(AFPRCount);
    end
    else
    begin
      if ADarwin then SlotAlignment := APlan.TypeDescriptor.Size;
      StackOffset := StackAppend(AStackOffset,
        APlan.TypeDescriptor.Size, APlan.TypeDescriptor.Alignment,
        SlotAlignment);
      AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
        APlan.TypeDescriptor.Size);
    end;
    Exit;
  end;
  if TypeIsIntegerScalar(APlan.TypeDescriptor) then
  begin
    if AGPRCount < 8 then
    begin
      AppendPlacement(APlan.Placements, fpkGPR, AGPRCount, 0, 0,
        APlan.TypeDescriptor.Size);
      Inc(AGPRCount);
    end
    else
    begin
      if ADarwin and (APlan.TypeDescriptor.Size < 8) then
        SlotAlignment := APlan.TypeDescriptor.Size;
      StackOffset := StackAppend(AStackOffset,
        APlan.TypeDescriptor.Size, APlan.TypeDescriptor.Alignment,
        SlotAlignment);
      AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
        APlan.TypeDescriptor.Size);
    end;
    Exit;
  end;

  SetLength(HFAOffsets, 0);
  IsHFA := CollectHFA(APlan.TypeDescriptor, 0, HFAType, HFAOffsets);
  if IsHFA and (AFPRCount + Length(HFAOffsets) <= 8) then
  begin
    for I := 0 to High(HFAOffsets) do
    begin
      AppendPlacement(APlan.Placements, fpkFPR, AFPRCount, 0,
        HFAOffsets[I], HFAElementSize(HFAType));
      Inc(AFPRCount);
    end;
    Exit;
  end;
  if IsHFA then
  begin
    AFPRCount := 8;
    SlotAlignment := 8;
    if ADarwin then
      SlotAlignment := APlan.TypeDescriptor.Alignment;
    StackOffset := StackAppend(AStackOffset, APlan.TypeDescriptor.Size,
      APlan.TypeDescriptor.Alignment, SlotAlignment);
    AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
      APlan.TypeDescriptor.Size);
    Exit;
  end;
  if APlan.TypeDescriptor.Size > 16 then
  begin
    APlan.Indirect := True;
    APlan.IndirectCopyOffset := ScratchAppend(AScratchOffset,
      APlan.TypeDescriptor.Size, 16);
    PointerPlacement.Kind := fpkGPR;
    PointerPlacement.RegisterIndex := -1;
    PointerPlacement.StackOffset := -1;
    PointerPlacement.ValueOffset := 0;
    PointerPlacement.Size := SizeOf(Pointer);
    if AGPRCount < 8 then
    begin
      PointerPlacement.RegisterIndex := AGPRCount;
      Inc(AGPRCount);
    end
    else
    begin
      PointerPlacement.Kind := fpkStack;
      PointerPlacement.StackOffset := StackAppend(AStackOffset,
        SizeOf(Pointer), SizeOf(Pointer), 8);
    end;
    SetLength(APlan.Placements, 1);
    APlan.Placements[0] := PointerPlacement;
    Exit;
  end;
  ChunkCount := (APlan.TypeDescriptor.Size + 7) div 8;
  if AGPRCount + ChunkCount <= 8 then
    for I := 0 to ChunkCount - 1 do
    begin
      ChunkSize := APlan.TypeDescriptor.Size - (I * 8);
      if ChunkSize > 8 then ChunkSize := 8;
      AppendPlacement(APlan.Placements, fpkGPR, AGPRCount, 0, I * 8,
        ChunkSize);
      Inc(AGPRCount);
    end
  else
  begin
    AGPRCount := 8;
    StackOffset := StackAppend(AStackOffset, APlan.TypeDescriptor.Size,
      APlan.TypeDescriptor.Alignment, 8);
    AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
      APlan.TypeDescriptor.Size);
  end;
end;

procedure CompileWin64Return(var APlan: TGocciaFFIReturnPlan;
  var APosition: Integer);
begin
  if (APlan.TypeDescriptor.Kind = ftkScalar) and
     (APlan.TypeDescriptor.ScalarType = fftVoid) then Exit;
  if TypeIsFloatScalar(APlan.TypeDescriptor) then
    AppendPlacement(APlan.Placements, fpkFPR, 0, 0, 0,
      APlan.TypeDescriptor.Size)
  else if TypeIsIntegerScalar(APlan.TypeDescriptor) or
          (APlan.TypeDescriptor.Size in [1, 2, 4, 8]) then
    AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
      APlan.TypeDescriptor.Size)
  else
  begin
    APlan.UsesHiddenPointer := True;
    APosition := 1;
  end;
end;

procedure CompileWin64Argument(var APlan: TGocciaFFIArgumentPlan;
  var APosition, AStackOffset, AScratchOffset: Integer;
  const AVariadicFunction: Boolean);
var
  Placement, IntegerMirrorPlacement: TGocciaFFIPlacement;
begin
  Placement.RegisterIndex := -1;
  Placement.StackOffset := -1;
  Placement.ValueOffset := 0;
  Placement.Size := APlan.TypeDescriptor.Size;
  if APlan.TypeDescriptor.IsAggregate and
     not (APlan.TypeDescriptor.Size in [1, 2, 4, 8]) then
  begin
    APlan.Indirect := True;
    APlan.IndirectCopyOffset := ScratchAppend(AScratchOffset,
      APlan.TypeDescriptor.Size, 16);
    Placement.Size := SizeOf(Pointer);
    Placement.Kind := fpkGPR;
  end
  else if TypeIsFloatScalar(APlan.TypeDescriptor) then
    Placement.Kind := fpkFPR
  else
    Placement.Kind := fpkGPR;
  if APosition < 4 then
  begin
    Placement.RegisterIndex := APosition
  end
  else
  begin
    Placement.Kind := fpkStack;
    Placement.StackOffset := StackAppend(AStackOffset, 8, 8, 8);
  end;
  SetLength(APlan.Placements, 1);
  APlan.Placements[0] := Placement;
  if AVariadicFunction and TypeIsFloatScalar(APlan.TypeDescriptor) and
     (APosition < 4) then
  begin
    IntegerMirrorPlacement := Placement;
    IntegerMirrorPlacement.Kind := fpkGPR;
    SetLength(APlan.Placements, 2);
    APlan.Placements[1] := IntegerMirrorPlacement;
  end;
  Inc(APosition);
end;

procedure CompileDarwinARM64VariadicArgument(
  var APlan: TGocciaFFIArgumentPlan;
  var AStackOffset, AScratchOffset: Integer);
var
  Placement: TGocciaFFIPlacement;
begin
  Placement.Kind := fpkStack;
  Placement.RegisterIndex := -1;
  Placement.ValueOffset := 0;
  if APlan.TypeDescriptor.IsAggregate and
     (APlan.TypeDescriptor.Size > 16) then
  begin
    APlan.Indirect := True;
    APlan.IndirectCopyOffset := ScratchAppend(AScratchOffset,
      APlan.TypeDescriptor.Size, 16);
    Placement.Size := SizeOf(Pointer);
    Placement.StackOffset := StackAppend(AStackOffset, SizeOf(Pointer),
      SizeOf(Pointer), 8);
  end
  else
  begin
    Placement.Size := APlan.TypeDescriptor.Size;
    Placement.StackOffset := StackAppend(AStackOffset,
      APlan.TypeDescriptor.Size, APlan.TypeDescriptor.Alignment, 8);
  end;
  SetLength(APlan.Placements, 1);
  APlan.Placements[0] := Placement;
end;

procedure CompileI386Return(var APlan: TGocciaFFIReturnPlan;
  var AStackOffset: Integer);
begin
  if (APlan.TypeDescriptor.Kind = ftkScalar) and
     (APlan.TypeDescriptor.ScalarType = fftVoid) then Exit;
  if TypeIsFloatScalar(APlan.TypeDescriptor) then
    AppendPlacement(APlan.Placements, fpkFPR, 0, 0, 0,
      APlan.TypeDescriptor.Size)
  else if TypeIsIntegerScalar(APlan.TypeDescriptor) then
    AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
      APlan.TypeDescriptor.Size)
  else if APlan.TypeDescriptor.Size in [1, 2, 4] then
    AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0,
      APlan.TypeDescriptor.Size)
  else if APlan.TypeDescriptor.Size = 8 then
  begin
    AppendPlacement(APlan.Placements, fpkGPR, 0, 0, 0, 4);
    AppendPlacement(APlan.Placements, fpkGPR, 1, 0, 4, 4);
  end
  else
  begin
    APlan.UsesHiddenPointer := True;
    AStackOffset := 4;
  end;
end;

procedure CompileI386Argument(var APlan: TGocciaFFIArgumentPlan;
  var AStackOffset: Integer);
var
  StackOffset, SlotSize: Integer;
begin
  if APlan.TypeDescriptor.IsAggregate then
    SlotSize := AlignFFIOffset(APlan.TypeDescriptor.Size, 4)
  else if APlan.TypeDescriptor.Size > 4 then
    SlotSize := APlan.TypeDescriptor.Size
  else
    SlotSize := 4;
  StackOffset := StackAppend(AStackOffset, SlotSize, 4, 4);
  AppendPlacement(APlan.Placements, fpkStack, -1, StackOffset, 0,
    APlan.TypeDescriptor.Size);
end;

constructor TGocciaFFICompiledSignature.Create(const AABI: TGocciaFFIABI;
  const AArguments: array of TGocciaFFITypeDescriptor;
  const AReturnType: TGocciaFFITypeDescriptor;
  const AVariadicStartIndex: Integer);
var
  I, GPRCount, FPRCount, Position, StackOffset, ScratchOffset: Integer;
begin
  inherited Create;
  if Length(AArguments) > MAX_FFI_ARGS then
    raise EArgumentException.Create('FFI signature has too many arguments');
  if (AVariadicStartIndex < -1) or
     (AVariadicStartIndex > Length(AArguments)) then
    raise EArgumentException.Create('FFI variadic argument index is invalid');
  if not Assigned(AReturnType) then
    raise EArgumentException.Create('FFI signature return type is missing');
  FABI := AABI;
  FVariadicStartIndex := AVariadicStartIndex;
  FReturnPlan.TypeDescriptor := AReturnType;
  AReturnType.AddReference;
  SetLength(FArguments, Length(AArguments));
  GPRCount := 0;
  FPRCount := 0;
  Position := 0;
  StackOffset := 0;
  ScratchOffset := 0;

  case FABI of
    fabiSysVX64: CompileSysVReturn(FReturnPlan, GPRCount);
    fabiWin64: CompileWin64Return(FReturnPlan, Position);
    fabiAAPCS64, fabiDarwinARM64: CompileARM64Return(FReturnPlan);
    fabiI386Win: CompileI386Return(FReturnPlan, StackOffset);
  end;

  for I := 0 to High(AArguments) do
  begin
    if not Assigned(AArguments[I]) then
      raise EArgumentException.Create('FFI signature argument type is missing');
    if (AArguments[I].Kind = ftkScalar) and
       (AArguments[I].ScalarType = fftVoid) then
      raise EArgumentException.Create('FFI signature argument cannot be void');
    FArguments[I].TypeDescriptor := AArguments[I];
    AArguments[I].AddReference;
    case FABI of
      fabiSysVX64:
        CompileSysVArgument(FArguments[I], GPRCount, FPRCount, StackOffset);
      fabiWin64:
        CompileWin64Argument(FArguments[I], Position, StackOffset,
          ScratchOffset, FVariadicStartIndex >= 0);
      fabiAAPCS64:
        CompileARM64Argument(FArguments[I], GPRCount, FPRCount, StackOffset,
          ScratchOffset, False);
      fabiDarwinARM64:
        if (FVariadicStartIndex >= 0) and
           (I >= FVariadicStartIndex) then
          CompileDarwinARM64VariadicArgument(FArguments[I], StackOffset,
            ScratchOffset)
        else
          CompileARM64Argument(FArguments[I], GPRCount, FPRCount, StackOffset,
            ScratchOffset, True);
      fabiI386Win:
        CompileI386Argument(FArguments[I], StackOffset);
    end;
  end;
  if FABI in [fabiSysVX64, fabiWin64, fabiAAPCS64, fabiDarwinARM64] then
    FStackSize := AlignFFIOffset(StackOffset, 16)
  else
    FStackSize := AlignFFIOffset(StackOffset, 4);
  FScratchSize := AlignFFIOffset(ScratchOffset, 16);
  FGPRCount := GPRCount;
  FFPRCount := FPRCount;
end;

destructor TGocciaFFICompiledSignature.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FArguments) do
    if Assigned(FArguments[I].TypeDescriptor) then
      FArguments[I].TypeDescriptor.ReleaseReference;
  if Assigned(FReturnPlan.TypeDescriptor) then
    FReturnPlan.TypeDescriptor.ReleaseReference;
  inherited;
end;

function TGocciaFFICompiledSignature.GetArgumentCount: Integer;
begin
  Result := Length(FArguments);
end;

function TGocciaFFICompiledSignature.GetArgument(
  const AIndex: Integer): TGocciaFFIArgumentPlan;
begin
  Result := FArguments[AIndex];
end;

end.
