program Goccia.RegExp.Compiler.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.RegExp.&Program,
  Goccia.RegExp.Compiler;

type
  TRegExpCompilerTests = class(TTestSuite)
  private
    procedure TestLargestOperandRoundTrips;
    procedure TestOperandPast24BitsRaises;
    procedure TestNegativeOperandRaises;
    procedure TestBackReferencePastIndexBitsRaises;
  public
    procedure SetupTests; override;
  end;

function RaisesConvertError(const AOp: TRegExpOpCode;
  const AOperand: Integer): Boolean;
begin
  Result := False;
  try
    EncodeRegExpInstruction(AOp, AOperand);
  except
    on EConvertError do
      Result := True;
  end;
end;

procedure TRegExpCompilerTests.SetupTests;
begin
  Test('the largest 24-bit operand encodes and decodes unchanged',
    TestLargestOperandRoundTrips);
  Test('an operand of 2^24 is an error, not a truncated operand',
    TestOperandPast24BitsRaises);
  Test('a negative operand is an error', TestNegativeOperandRaises);
  Test('a back reference index past its operand bits is an error',
    TestBackReferencePastIndexBitsRaises);
end;

procedure TRegExpCompilerTests.TestLargestOperandRoundTrips;
var
  Instr: UInt32;
begin
  Instr := EncodeRegExpInstruction(RX_JUMP, $FFFFFF);
  Expect<Integer>(Integer(Instr and $FF)).ToBe(Ord(RX_JUMP));
  Expect<Integer>(Integer(Instr shr 8)).ToBe($FFFFFF);
end;

procedure TRegExpCompilerTests.TestOperandPast24BitsRaises;
begin
  Expect<Boolean>(RaisesConvertError(RX_JUMP, $1000000)).ToBe(True);
  Expect<Boolean>(RaisesConvertError(RX_SPLIT, $1000001)).ToBe(True);
end;

procedure TRegExpCompilerTests.TestNegativeOperandRaises;
begin
  Expect<Boolean>(RaisesConvertError(RX_JUMP, -1)).ToBe(True);
end;

function CompileRaisesConvertError(const APattern, AFlags: string): Boolean;
begin
  Result := False;
  try
    CompileRegExp(APattern, AFlags);
  except
    on EConvertError do
      Result := True;
  end;
end;

procedure TRegExpCompilerTests.TestBackReferencePastIndexBitsRaises;
begin
  Expect<Boolean>(Length(CompileRegExp('(a)\1', 'u').Code) > 0).ToBe(True);
  // With the u flag a decimal escape is a back reference and must name a
  // group: 2097152 would set a flag bit, and 2^32 + 1 would wrap to group 1.
  Expect<Boolean>(CompileRaisesConvertError('(a)\2097152', 'u')).ToBe(True);
  Expect<Boolean>(CompileRaisesConvertError('(a)\4294967297', 'u')).ToBe(True);
  Expect<Boolean>(CompileRaisesConvertError('(a)\99999999999', 'u')).ToBe(True);
  // Without it, past the number of groups it is an Annex B escape
  // (B.1.2), not a back reference, so nothing wraps either.
  Expect<Boolean>(CompileRaisesConvertError('(a)\4294967297', '')).ToBe(False);
end;

begin
  TestRunnerProgram.AddSuite(
    TRegExpCompilerTests.Create('Goccia.RegExp.Compiler'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
