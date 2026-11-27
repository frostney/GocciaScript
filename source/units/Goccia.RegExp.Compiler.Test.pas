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
    procedure TestWorkspaceLimitCountsNamedGroupStorage;
  public
    procedure SetupTests; override;
  end;

  ETestWorkspaceLimit = class(Exception);

procedure CheckTestWorkspace(const ABytes: Int64);
begin
  if ABytes > 3 * 1024 then
    raise ETestWorkspaceLimit.Create('test workspace limit');
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
  Test('the workspace limit includes named-group bookkeeping',
    TestWorkspaceLimitCountsNamedGroupStorage);
end;

procedure TRegExpCompilerTests.TestWorkspaceLimitCountsNamedGroupStorage;
var
  I: Integer;
  NamedPattern, UnnamedPattern: string;
  Refused: Boolean;
begin
  NamedPattern := '';
  UnnamedPattern := '';
  for I := 1 to 40 do
  begin
    NamedPattern := NamedPattern + '(?<group' + IntToStr(I) + '>a)';
    UnnamedPattern := UnnamedPattern + '(a)';
  end;
  Expect<Integer>(CompileRegExp(UnnamedPattern, '', @CheckTestWorkspace).CaptureCount).ToBe(40);
  Refused := False;
  try
    CompileRegExp(NamedPattern, '', @CheckTestWorkspace);
  except
    on ETestWorkspaceLimit do
      Refused := True;
  end;
  Expect<Boolean>(Refused).ToBe(True);
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
