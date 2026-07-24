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
          fabiAAPCS64, fabiDarwinARM64:
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
          fabiAAPCS64, fabiDarwinARM64:
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

begin
  TestRunnerProgram.AddSuite(TFFIABITests.Create('Goccia.FFI.ABI'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
