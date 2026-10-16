program Goccia.FFI.ABI.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.FFI.ABI,
  Goccia.FFI.Types;

type
  TFFIABITests = class(TTestSuite)
  private
    procedure ExpectPlacement(const APlan: TGocciaFFICompiledSignature;
      const AArgumentIndex, APlacementIndex: Integer;
      const AKind: TGocciaFFIPlacementKind;
      const ARegisterIndex, AStackOffset: Integer);
    procedure TestMixedScalarClassification;
    procedure TestHighArityPlacement;
    procedure TestWin64VariadicFloatMirroring;
    procedure TestDarwinARM64VariadicTailPlacement;
    procedure TestWinARM64VariadicUsesIntegerRegisters;
    procedure TestWinARM64VariadicSplitsAcrossX7;
    procedure TestWinARM64VariadicLargeAggregateByPointer;
  public
    procedure SetupTests; override;
  end;

procedure TFFIABITests.SetupTests;
begin
  Test('mixed scalar arguments are classified independently',
    TestMixedScalarClassification);
  Test('high arity arguments spill through the ABI planner',
    TestHighArityPlacement);
  Test('Win64 variadic floats are mirrored into integer registers',
    TestWin64VariadicFloatMirroring);
  Test('Darwin ARM64 variadic tails use stack slots',
    TestDarwinARM64VariadicTailPlacement);
  Test('Windows ARM64 variadic calls pass every argument in integer registers',
    TestWinARM64VariadicUsesIntegerRegisters);
  Test('Windows ARM64 variadic aggregates split between x7 and the stack',
    TestWinARM64VariadicSplitsAcrossX7);
  Test('Windows ARM64 variadic aggregates over 16 bytes pass by pointer',
    TestWinARM64VariadicLargeAggregateByPointer);
end;

procedure TFFIABITests.ExpectPlacement(
  const APlan: TGocciaFFICompiledSignature;
  const AArgumentIndex, APlacementIndex: Integer;
  const AKind: TGocciaFFIPlacementKind;
  const ARegisterIndex, AStackOffset: Integer);
var
  Placement: TGocciaFFIPlacement;
begin
  Placement := APlan.Arguments[AArgumentIndex].Placements[APlacementIndex];
  Expect<Integer>(Ord(Placement.Kind)).ToBe(Ord(AKind));
  Expect<Integer>(Placement.RegisterIndex).ToBe(ARegisterIndex);
  Expect<Integer>(Placement.StackOffset).ToBe(AStackOffset);
end;

procedure TFFIABITests.TestMixedScalarClassification;
var
  Arguments: array[0..3] of TGocciaFFITypeDescriptor;
  ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  ABI: TGocciaFFIABI;
  I: Integer;
begin
  Arguments[0] := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
  Arguments[1] := TGocciaFFITypeDescriptor.CreateScalar(fftF32);
  Arguments[2] := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
  Arguments[3] := TGocciaFFITypeDescriptor.CreateScalar(fftF32);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid);
  try
    for ABI := Low(TGocciaFFIABI) to High(TGocciaFFIABI) do
    begin
      Plan := TGocciaFFICompiledSignature.Create(ABI, Arguments, ReturnType);
      try
        case ABI of
          fabiSysVX64:
          begin
            ExpectPlacement(Plan, 0, 0, fpkGPR, 0, 0);
            ExpectPlacement(Plan, 1, 0, fpkFPR, 0, 0);
            ExpectPlacement(Plan, 2, 0, fpkGPR, 1, 0);
            ExpectPlacement(Plan, 3, 0, fpkFPR, 1, 0);
          end;
          fabiWin64:
          begin
            ExpectPlacement(Plan, 0, 0, fpkGPR, 0, -1);
            ExpectPlacement(Plan, 1, 0, fpkFPR, 1, -1);
            ExpectPlacement(Plan, 2, 0, fpkGPR, 2, -1);
            ExpectPlacement(Plan, 3, 0, fpkFPR, 3, -1);
          end;
          fabiAAPCS64, fabiDarwinARM64, fabiWinARM64:
          begin
            ExpectPlacement(Plan, 0, 0, fpkGPR, 0, 0);
            ExpectPlacement(Plan, 1, 0, fpkFPR, 0, 0);
            ExpectPlacement(Plan, 2, 0, fpkGPR, 1, 0);
            ExpectPlacement(Plan, 3, 0, fpkFPR, 1, 0);
          end;
          fabiI386Win:
          begin
            ExpectPlacement(Plan, 0, 0, fpkStack, -1, 0);
            ExpectPlacement(Plan, 1, 0, fpkStack, -1, 4);
            ExpectPlacement(Plan, 2, 0, fpkStack, -1, 8);
            ExpectPlacement(Plan, 3, 0, fpkStack, -1, 12);
          end;
        end;
      finally
        Plan.Free;
      end;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
  end;
end;

procedure TFFIABITests.TestHighArityPlacement;
var
  Arguments: array[0..8] of TGocciaFFITypeDescriptor;
  ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  ABI: TGocciaFFIABI;
  I: Integer;
begin
  for I := 0 to High(Arguments) do
    Arguments[I] := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid);
  try
    for ABI := Low(TGocciaFFIABI) to High(TGocciaFFIABI) do
    begin
      Plan := TGocciaFFICompiledSignature.Create(ABI, Arguments, ReturnType);
      try
        case ABI of
          fabiSysVX64:
            ExpectPlacement(Plan, 8, 0, fpkStack, -1, 16);
          fabiWin64:
            ExpectPlacement(Plan, 8, 0, fpkStack, -1, 32);
          fabiAAPCS64, fabiDarwinARM64, fabiWinARM64:
            ExpectPlacement(Plan, 8, 0, fpkStack, -1, 0);
          fabiI386Win:
            ExpectPlacement(Plan, 8, 0, fpkStack, -1, 32);
        end;
      finally
        Plan.Free;
      end;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
  end;
end;

procedure TFFIABITests.TestWin64VariadicFloatMirroring;
var
  Arguments: array[0..1] of TGocciaFFITypeDescriptor;
  ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  I: Integer;
begin
  Arguments[0] := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  Arguments[1] := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid);
  try
    Plan := TGocciaFFICompiledSignature.Create(fabiWin64, Arguments,
      ReturnType, 1);
    try
      Expect<Integer>(Length(Plan.Arguments[0].Placements)).ToBe(2);
      ExpectPlacement(Plan, 0, 0, fpkFPR, 0, -1);
      ExpectPlacement(Plan, 0, 1, fpkGPR, 0, -1);
      Expect<Integer>(Length(Plan.Arguments[1].Placements)).ToBe(2);
      ExpectPlacement(Plan, 1, 0, fpkFPR, 1, -1);
      ExpectPlacement(Plan, 1, 1, fpkGPR, 1, -1);
    finally
      Plan.Free;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
  end;
end;

procedure TFFIABITests.TestDarwinARM64VariadicTailPlacement;
var
  Arguments: array[0..2] of TGocciaFFITypeDescriptor;
  ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  I: Integer;
begin
  Arguments[0] := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  Arguments[1] := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  Arguments[2] := TGocciaFFITypeDescriptor.CreateScalar(fftI64);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid);
  try
    Plan := TGocciaFFICompiledSignature.Create(fabiDarwinARM64, Arguments,
      ReturnType, 1);
    try
      ExpectPlacement(Plan, 0, 0, fpkFPR, 0, 0);
      ExpectPlacement(Plan, 1, 0, fpkStack, -1, 0);
      ExpectPlacement(Plan, 2, 0, fpkStack, -1, 8);
      Expect<Integer>(Plan.StackSize).ToBe(16);
    finally
      Plan.Free;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
  end;
end;

procedure TFFIABITests.TestWinARM64VariadicUsesIntegerRegisters;
var
  Arguments: array[0..3] of TGocciaFFITypeDescriptor;
  F64, ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  I: Integer;
begin
  F64 := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  // A fixed f64, a fixed i32, then a tail of an f64 and a two-double
  // aggregate, which AAPCS64 would treat as an HFA.
  Arguments[0] := F64;
  F64.AddReference;
  Arguments[1] := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
  Arguments[2] := F64;
  F64.AddReference;
  Arguments[3] := TGocciaFFITypeDescriptor.CreateArray(F64, 2);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  try
    Plan := TGocciaFFICompiledSignature.Create(fabiWinARM64, Arguments,
      ReturnType, 2);
    try
      ExpectPlacement(Plan, 0, 0, fpkGPR, 0, -1);
      ExpectPlacement(Plan, 1, 0, fpkGPR, 1, -1);
      ExpectPlacement(Plan, 2, 0, fpkGPR, 2, -1);
      Expect<Integer>(Length(Plan.Arguments[3].Placements)).ToBe(2);
      ExpectPlacement(Plan, 3, 0, fpkGPR, 3, -1);
      ExpectPlacement(Plan, 3, 1, fpkGPR, 4, -1);
      Expect<Integer>(Plan.Arguments[3].Placements[1].ValueOffset).ToBe(8);
      Expect<Integer>(Plan.GPRCount).ToBe(5);
      Expect<Integer>(Plan.FPRCount).ToBe(0);
      Expect<Integer>(Plan.StackSize).ToBe(0);
      // The return value still comes back in d0.
      Expect<Integer>(Ord(Plan.ReturnPlan.Placements[0].Kind))
        .ToBe(Ord(fpkFPR));
    finally
      Plan.Free;
    end;
    // The same signature without a variadic tail keeps AAPCS64 placement.
    Plan := TGocciaFFICompiledSignature.Create(fabiWinARM64, Arguments,
      ReturnType);
    try
      ExpectPlacement(Plan, 0, 0, fpkFPR, 0, 0);
      ExpectPlacement(Plan, 1, 0, fpkGPR, 0, 0);
      ExpectPlacement(Plan, 2, 0, fpkFPR, 1, 0);
      ExpectPlacement(Plan, 3, 0, fpkFPR, 2, 0);
      ExpectPlacement(Plan, 3, 1, fpkFPR, 3, 0);
    finally
      Plan.Free;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
    F64.ReleaseReference;
  end;
end;

procedure TFFIABITests.TestWinARM64VariadicSplitsAcrossX7;
var
  Arguments: array[0..8] of TGocciaFFITypeDescriptor;
  F64, ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  I: Integer;
begin
  F64 := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  for I := 0 to 6 do
    Arguments[I] := TGocciaFFITypeDescriptor.CreateScalar(fftI64);
  Arguments[7] := TGocciaFFITypeDescriptor.CreateArray(F64, 2);
  Arguments[8] := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid);
  try
    Plan := TGocciaFFICompiledSignature.Create(fabiWinARM64, Arguments,
      ReturnType, 1);
    try
      ExpectPlacement(Plan, 6, 0, fpkGPR, 6, -1);
      Expect<Integer>(Length(Plan.Arguments[7].Placements)).ToBe(2);
      ExpectPlacement(Plan, 7, 0, fpkGPR, 7, -1);
      Expect<Integer>(Plan.Arguments[7].Placements[0].ValueOffset).ToBe(0);
      Expect<Integer>(Plan.Arguments[7].Placements[0].Size).ToBe(8);
      ExpectPlacement(Plan, 7, 1, fpkStack, -1, 0);
      Expect<Integer>(Plan.Arguments[7].Placements[1].ValueOffset).ToBe(8);
      Expect<Integer>(Plan.Arguments[7].Placements[1].Size).ToBe(8);
      ExpectPlacement(Plan, 8, 0, fpkStack, -1, 8);
      Expect<Integer>(Plan.StackSize).ToBe(16);
    finally
      Plan.Free;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
    F64.ReleaseReference;
  end;
end;

procedure TFFIABITests.TestWinARM64VariadicLargeAggregateByPointer;
var
  Arguments: array[0..1] of TGocciaFFITypeDescriptor;
  F64, ReturnType: TGocciaFFITypeDescriptor;
  Plan: TGocciaFFICompiledSignature;
  I: Integer;
begin
  F64 := TGocciaFFITypeDescriptor.CreateScalar(fftF64);
  Arguments[0] := TGocciaFFITypeDescriptor.CreateScalar(fftI32);
  Arguments[1] := TGocciaFFITypeDescriptor.CreateArray(F64, 3);
  ReturnType := TGocciaFFITypeDescriptor.CreateScalar(fftVoid);
  try
    Plan := TGocciaFFICompiledSignature.Create(fabiWinARM64, Arguments,
      ReturnType, 1);
    try
      Expect<Boolean>(Plan.Arguments[1].Indirect).ToBe(True);
      Expect<Integer>(Length(Plan.Arguments[1].Placements)).ToBe(1);
      ExpectPlacement(Plan, 1, 0, fpkGPR, 1, -1);
      Expect<Integer>(Plan.Arguments[1].Placements[0].Size)
        .ToBe(SizeOf(Pointer));
      Expect<Integer>(Plan.ScratchSize).ToBe(32);
    finally
      Plan.Free;
    end;
  finally
    ReturnType.ReleaseReference;
    for I := 0 to High(Arguments) do
      Arguments[I].ReleaseReference;
    F64.ReleaseReference;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TFFIABITests.Create('Goccia.FFI.ABI'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
