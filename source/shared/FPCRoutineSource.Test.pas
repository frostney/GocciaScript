program FPCRoutineSource.Test;

{$I Shared.inc}

uses
  Classes,
  SysUtils,

  FPCRoutineSource,
  SHA256,
  TestingPascalLibrary;

const
  ROUTINE_SECTION = '.text.n_fixture_$$_reviewedroutine';
  METHOD_SECTION = '.text.n_fixture$_$tfirst_$__$$_reviewedmethod$crc12345678';
  OTHER_METHOD_SECTION = '.text.n_fixture$_$tsecond_$__$$_reviewedmethod$crc12345678';
  ROUTINE_TEXT = 'procedure ReviewedRoutine(const AValue: Integer);' + LineEnding +
    'var' + LineEnding +
    '  Text: string;' + LineEnding +
    'begin' + LineEnding +
    '  Text := ''end; procedure Hidden;''; { end } // begin' + LineEnding +
    '  case AValue of' + LineEnding +
    '    0: (* end *) Text := '''';' + LineEnding +
    '  end;' + LineEnding +
    '  try' + LineEnding +
    '    Text := Text + #39;' + LineEnding +
    '  finally' + LineEnding +
    '  end;' + LineEnding +
    'end;';
  OTHER_TEXT = 'procedure Unrelated;' + LineEnding + 'begin' + LineEnding + 'end;';

type
  TFPCRoutineSourceTests = class(TTestSuite)
  private
    FDirectory: string;
    FSourceFile: string;
    procedure WriteText(const AFileName, AText: string);
    function UnitText(const AImplementation: string; const AInterface: string = ''): string;
    function Locate(const AText, ASection: string): TFPCRoutineSpan;
    procedure Refuse(const AText, ASection, AReason: string);
    function Digest(const AText: string): string;
    procedure TestRoutineSpan;
    procedure TestEditsOutsideRoutine;
    procedure TestEditsInsideRoutine;
    procedure TestMethodOwner;
    procedure TestMissingRoutine;
    procedure TestOverloads;
    procedure TestNestedRoutines;
    procedure TestNestedAndGenericTypes;
    procedure TestIncludes;
    procedure TestConditionals;
    procedure TestModuleName;
    procedure TestIncompleteParse;
    procedure TestDeclarations;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetupTests; override;
  end;

constructor TFPCRoutineSourceTests.Create;
var
  Temporary: string;
begin
  inherited Create('FPCRoutineSource');
  Temporary := string(GetTempFileName('', 'routine'));
  DeleteFile(Temporary);
  FDirectory := Temporary;
  if not CreateDir(FDirectory) then
    raise Exception.Create('Cannot create routine source test directory');
  ForceDirectories(FDirectory + PathDelim + 'includes');
  FSourceFile := FDirectory + PathDelim + 'fixture.pas';
  WriteText(FDirectory + PathDelim + 'includes' + PathDelim + 'Shared.inc',
    '{$IFDEF FPC} {$mode delphiunicode} {$ENDIF}' + LineEnding);
end;

destructor TFPCRoutineSourceTests.Destroy;
begin
  DeleteFile(FSourceFile);
  DeleteFile(FDirectory + PathDelim + 'local.inc');
  DeleteFile(FDirectory + PathDelim + 'includes' + PathDelim + 'Shared.inc');
  RemoveDir(FDirectory + PathDelim + 'includes');
  RemoveDir(FDirectory);
  inherited Destroy;
end;

procedure TFPCRoutineSourceTests.WriteText(const AFileName, AText: string);
var
  Stream: TFileStream;
  Bytes: UTF8String;
begin
  Bytes := UTF8Encode(AText);
  Stream := TFileStream.Create(UTF8Encode(AFileName), fmCreate);
  try
    if Length(Bytes) > 0 then
      Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function TFPCRoutineSourceTests.UnitText(const AImplementation: string;
  const AInterface: string): string;
begin
  Result := 'unit Fixture;' + LineEnding + LineEnding + '{$I Shared.inc}' + LineEnding +
    LineEnding + 'interface' + LineEnding + LineEnding + AInterface + LineEnding +
    'implementation' + LineEnding + LineEnding + AImplementation + LineEnding + LineEnding +
    'end.' + LineEnding;
end;

function TFPCRoutineSourceTests.Locate(const AText, ASection: string): TFPCRoutineSpan;
begin
  WriteText(FSourceFile, AText);
  Result := LocateFPCRoutine(FSourceFile, ASection, [FDirectory + PathDelim + 'includes']);
end;

procedure TFPCRoutineSourceTests.Refuse(const AText, ASection, AReason: string);
var
  Refused: Boolean;
begin
  Refused := False;
  try
    Locate(AText, ASection);
  except
    on E: EFPCRoutineSource do
    begin
      Refused := True;
      if Pos(AReason, UTF8Decode(E.Message)) = 0 then
        Fail('Expected ' + AReason + ', got ' + UTF8Decode(E.Message));
    end;
  end;
  Expect<Boolean>(Refused).ToBe(True);
end;

function TFPCRoutineSourceTests.Digest(const AText: string): string;
var
  Text: UTF8String;
  Bytes: TBytes;
begin
  Text := UTF8Encode(StringReplace(AText, #13#10, #10, [rfReplaceAll]));
  SetLength(Bytes, Length(Text));
  if Length(Text) > 0 then
    Move(Text[1], Bytes[0], Length(Text));
  Result := SHA256Hex(Bytes);
end;

procedure TFPCRoutineSourceTests.SetupTests;
begin
  Test('hashes a routine from its header through its final semicolon', TestRoutineSpan);
  Test('keeps the digest across edits outside the routine', TestEditsOutsideRoutine);
  Test('changes the digest for every edit inside the routine', TestEditsInsideRoutine);
  Test('selects a method by its owning type', TestMethodOwner);
  Test('refuses a missing or renamed routine', TestMissingRoutine);
  Test('refuses overloads it cannot tell apart', TestOverloads);
  Test('refuses nested routines and nested namesakes', TestNestedRoutines);
  Test('refuses methods of nested and generic types', TestNestedAndGenericTypes);
  Test('refuses include files that could hide text', TestIncludes);
  Test('accepts only block-neutral conditionals inside the routine', TestConditionals);
  Test('requires the section''s unit or program', TestModuleName);
  Test('refuses source it cannot parse completely', TestIncompleteParse);
  Test('skips type declarations and procedural types', TestDeclarations);
end;

procedure TFPCRoutineSourceTests.TestRoutineSpan;
var
  Span: TFPCRoutineSpan;
begin
  Span := Locate(UnitText('{ Reviewed. }' + LineEnding + ROUTINE_TEXT + LineEnding + LineEnding +
    OTHER_TEXT), ROUTINE_SECTION);
  Expect<string>(Span.Name).ToBe('ReviewedRoutine');
  Expect<string>(Span.Digest).ToBe(Digest(ROUTINE_TEXT));
  Expect<Integer>(Span.FirstLine).ToBe(11);
  Expect<Integer>(Span.LastLine).ToBe(23);
end;

procedure TFPCRoutineSourceTests.TestEditsOutsideRoutine;
var
  Expected: string;
begin
  Expected := Digest(ROUTINE_TEXT);
  Expect<string>(Locate(UnitText(ROUTINE_TEXT + LineEnding + OTHER_TEXT), ROUTINE_SECTION).Digest).ToBe(Expected);
  Expect<string>(Locate(UnitText('{ Moved and commented. }' + LineEnding + OTHER_TEXT + LineEnding +
    StringReplace(OTHER_TEXT, 'Unrelated', 'Added', []) + LineEnding + LineEnding + ROUTINE_TEXT,
    'procedure ReviewedRoutine(const AValue: Integer);'), ROUTINE_SECTION).Digest).ToBe(Expected);
  Expect<string>(Locate(StringReplace(UnitText(ROUTINE_TEXT + LineEnding + OTHER_TEXT), LineEnding,
    #13#10, [rfReplaceAll]), ROUTINE_SECTION).Digest).ToBe(Expected);
  Expect<string>(Locate(StringReplace(UnitText(ROUTINE_TEXT + LineEnding + OTHER_TEXT), LineEnding,
    #13, [rfReplaceAll]), ROUTINE_SECTION).Digest).ToBe(Expected);
end;

procedure TFPCRoutineSourceTests.TestEditsInsideRoutine;
const
  EDITS: array[0..5] of array[0..1] of string = (
    ('Text := Text + #39;', 'Text := Text + #40;'),
    ('{ end }', '{ edited }'),
    ('(const AValue: Integer)', '(const AValue: Cardinal)'),
    ('  finally', '  except'),
    ('procedure ReviewedRoutine(', 'procedure  ReviewedRoutine('),
    ('end;' + LineEnding + 'end;', 'end;' + LineEnding + '  Exit;' + LineEnding + 'end;'));
var
  Expected: string;
  Index: Integer;
  Span: TFPCRoutineSpan;
begin
  Expected := Digest(ROUTINE_TEXT);
  for Index := Low(EDITS) to High(EDITS) do
  begin
    if Pos(EDITS[Index][0], ROUTINE_TEXT) = 0 then
      Fail('Fixture edit does not apply: ' + EDITS[Index][0]);
    Span := Locate(UnitText(StringReplace(ROUTINE_TEXT, EDITS[Index][0], EDITS[Index][1], [])),
      ROUTINE_SECTION);
    if Span.Digest = Expected then
      Fail('Edit kept the digest: ' + EDITS[Index][1]);
  end;
  // A nested routine is part of its owner's text.
  Expected := Locate(UnitText('procedure ReviewedRoutine;' + LineEnding +
    '  procedure Inner;' + LineEnding + '  begin' + LineEnding + '  end;' + LineEnding +
    'begin' + LineEnding + '  Inner;' + LineEnding + 'end;'), ROUTINE_SECTION).Digest;
  Span := Locate(UnitText('procedure ReviewedRoutine;' + LineEnding +
    '  procedure Inner;' + LineEnding + '  begin' + LineEnding + '    Exit;' + LineEnding + '  end;' + LineEnding +
    'begin' + LineEnding + '  Inner;' + LineEnding + 'end;'), ROUTINE_SECTION);
  if Span.Digest = Expected then
    Fail('Nested routine edit kept the digest');
  Expect<Integer>(Span.LastLine).ToBe(17);
end;

procedure TFPCRoutineSourceTests.TestMethodOwner;
const
  TYPES = 'type' + LineEnding +
    '  TFirst = class' + LineEnding + '    procedure ReviewedMethod;' + LineEnding + '  end;' + LineEnding +
    '  TSecond = class' + LineEnding + '    procedure ReviewedMethod;' + LineEnding + '  end;' + LineEnding;
  FIRST = 'procedure TFirst.ReviewedMethod;' + LineEnding + 'begin' + LineEnding + '  Exit;' + LineEnding + 'end;';
  SECOND = 'procedure TSecond.ReviewedMethod;' + LineEnding + 'begin' + LineEnding + 'end;';
var
  Text: string;
  Span: TFPCRoutineSpan;
begin
  Text := UnitText('procedure ReviewedMethod;' + LineEnding + 'begin' + LineEnding + 'end;' +
    LineEnding + FIRST + LineEnding + SECOND, TYPES);
  Span := Locate(Text, METHOD_SECTION);
  Expect<string>(Span.Name).ToBe('TFirst.ReviewedMethod');
  Expect<string>(Span.Digest).ToBe(Digest(FIRST));
  Span := Locate(Text, OTHER_METHOD_SECTION);
  Expect<string>(Span.Name).ToBe('TSecond.ReviewedMethod');
  Expect<string>(Span.Digest).ToBe(Digest(SECOND));
  Span := Locate(UnitText('class procedure TFirst.ReviewedMethod;' + LineEnding + 'begin' +
    LineEnding + 'end;'), METHOD_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest('class procedure TFirst.ReviewedMethod;' + LineEnding +
    'begin' + LineEnding + 'end;'));
end;

procedure TFPCRoutineSourceTests.TestMissingRoutine;
begin
  Refuse(UnitText(StringReplace(ROUTINE_TEXT, 'ReviewedRoutine', 'RenamedRoutine', [])),
    ROUTINE_SECTION, 'no implementation of fixture.reviewedroutine');
  Refuse(UnitText(OTHER_TEXT), ROUTINE_SECTION, 'no implementation');
  // A declaration without a body is not the reviewed text.
  Refuse(UnitText('procedure ReviewedRoutine; external ''library'';'), ROUTINE_SECTION,
    'no implementation');
  Refuse(UnitText('procedure TSecond.ReviewedMethod;' + LineEnding + 'begin' + LineEnding + 'end;'),
    METHOD_SECTION, 'no implementation of fixture.tfirst.reviewedmethod');
  // The interface declaration is not an implementation.
  Refuse(UnitText(OTHER_TEXT, 'procedure ReviewedRoutine;' + LineEnding), ROUTINE_SECTION,
    'no implementation');
  DeleteFile(FSourceFile);
  try
    LocateFPCRoutine(FSourceFile, ROUTINE_SECTION, []);
    Fail('Expected a missing source to be refused');
  except
    on E: EFPCRoutineSource do
      if Pos('source unavailable', UTF8Decode(E.Message)) = 0 then
        Fail('Expected source unavailable, got ' + UTF8Decode(E.Message));
  end;
end;

procedure TFPCRoutineSourceTests.TestOverloads;
var
  Span: TFPCRoutineSpan;
begin
  Refuse(UnitText(ROUTINE_TEXT + LineEnding + StringReplace(ROUTINE_TEXT, 'Integer)', 'string)', [])),
    ROUTINE_SECTION, '2 implementations of fixture.reviewedroutine; overloads cannot be told apart');
  Refuse(UnitText('procedure TFirst.ReviewedMethod(const AValue: Integer);' + LineEnding + 'begin' +
    LineEnding + 'end;' + LineEnding + 'procedure TFirst.ReviewedMethod(const AValue: string);' +
    LineEnding + 'begin' + LineEnding + 'end;'), METHOD_SECTION, '2 implementations');
  // A forward declaration and its implementation are one routine.
  Span := Locate(UnitText('procedure ReviewedRoutine(const AValue: Integer); forward;' + LineEnding +
    OTHER_TEXT + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest(ROUTINE_TEXT));
end;

procedure TFPCRoutineSourceTests.TestNestedRoutines;
begin
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_fixture$_$outer$longint_$$_reviewedroutine',
    'nested routines are not supported');
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_fixture_$$_outer_$$_reviewedroutine',
    'nested routines are not supported');
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_fixture$_$tfirst_$__$$_outer$_$reviewedmethod',
    'nested routines are not supported');
  Refuse(UnitText('procedure Outer;' + LineEnding + '  procedure ReviewedRoutine;' + LineEnding +
    '  begin' + LineEnding + '  end;' + LineEnding + 'begin' + LineEnding + 'end;' + LineEnding +
    ROUTINE_TEXT), ROUTINE_SECTION, 'a nested routine shares the reviewed routine''s name');
end;

procedure TFPCRoutineSourceTests.TestNestedAndGenericTypes;
begin
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_fixture$_$touter.tfirst_$__$$_reviewedmethod',
    'methods of nested or generic types are not supported');
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_fixture$_$tfirst$1$crc0000_$__$$_reviewedmethod',
    'methods of nested or generic types are not supported');
  Refuse(UnitText('procedure TOuter.TFirst.ReviewedMethod;' + LineEnding + 'begin' + LineEnding +
    'end;'), METHOD_SECTION, 'methods of nested types are not supported');
  Refuse(UnitText('procedure TFirst<T>.ReviewedMethod;' + LineEnding + 'begin' + LineEnding +
    'end;'), METHOD_SECTION, 'generic routines are not supported');
end;

procedure TFPCRoutineSourceTests.TestIncludes;
var
  Statements: string;
begin
  // The prologue include is found through the include directories.
  Locate(UnitText(ROUTINE_TEXT), ROUTINE_SECTION);
  Refuse(UnitText(StringReplace(ROUTINE_TEXT, '  try', '  {$I local.inc}' + LineEnding + '  try', [])),
    ROUTINE_SECTION, '{$i} includes text that no reviewed digest covers in ReviewedRoutine');
  Refuse(UnitText(StringReplace(ROUTINE_TEXT, '  try', '  {$INCLUDE local.inc}' + LineEnding + '  try', [])),
    ROUTINE_SECTION, '{$include} includes text');
  // {$I+} and {$I-} switch I/O checking; they include nothing.
  Locate(UnitText(StringReplace(ROUTINE_TEXT, '  try', '  {$I-}' + LineEnding + '  try', [])),
    ROUTINE_SECTION);
  Statements := 'procedure Other;' + LineEnding + 'begin' + LineEnding + '  {$I local.inc}' +
    LineEnding + 'end;' + LineEnding + ROUTINE_TEXT;
  WriteText(FDirectory + PathDelim + 'local.inc', '  case 1 of' + LineEnding + '    1: Exit;' +
    LineEnding + '  end;' + LineEnding);
  Expect<string>(Locate(UnitText(Statements), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
  // An included overload would make the visible implementation ambiguous.
  WriteText(FDirectory + PathDelim + 'local.inc', '  end;' + LineEnding +
    'procedure ReviewedRoutine(const AValue: string);' + LineEnding + 'begin' + LineEnding);
  Refuse(UnitText(Statements), ROUTINE_SECTION, 'included text can declare routines');
  Refuse(UnitText('{$I local.inc}' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION,
    'included text can declare routines');
  DeleteFile(FDirectory + PathDelim + 'local.inc');
  Refuse(UnitText(Statements), ROUTINE_SECTION, 'include file not found: local.inc');
end;

procedure TFPCRoutineSourceTests.TestConditionals;
const
  BALANCED = 'procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding +
    '{$IFDEF WINDOWS}' + LineEnding + '  if True then' + LineEnding + '  begin' + LineEnding +
    '  end;' + LineEnding + '{$ELSE}' + LineEnding + '  Exit;' + LineEnding + '{$ENDIF}' +
    LineEnding + 'end;';
var
  Expected: string;
begin
  Expected := Locate(UnitText(BALANCED), ROUTINE_SECTION).Digest;
  Expect<string>(Expected).ToBe(Digest(BALANCED));
  // Every branch is in the digest, including the one this host skips.
  if Locate(UnitText(StringReplace(BALANCED, '  Exit;', '  Halt;', [])), ROUTINE_SECTION).Digest = Expected then
    Fail('Edit in an inactive branch kept the digest');
  // On Windows the routine would continue past the `end` the parse stops at.
  // Block-neutral branches end the routine at the same `end` either way.
  Locate(UnitText('procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding + '  begin' +
    LineEnding + '{$IFDEF WINDOWS}' + LineEnding + '  end;' + LineEnding + '  begin' + LineEnding +
    '{$ENDIF}' + LineEnding + '  end;' + LineEnding + 'end;'), ROUTINE_SECTION);
  // The union parses, but on Windows the routine would continue past the
  // `end` the parse stops at.
  Refuse(UnitText('procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding + '  begin' +
    LineEnding + '{$IFDEF WINDOWS}' + LineEnding + '  begin' + LineEnding + '{$ELSE}' + LineEnding +
    '  end;' + LineEnding + '{$ENDIF}' + LineEnding + '  end;' + LineEnding + 'end;'), ROUTINE_SECTION,
    '{$else} changes the block structure between conditional branches in ReviewedRoutine');
  Refuse(UnitText('procedure ReviewedRoutine;' + LineEnding + '{$IFNDEF WINDOWS}' + LineEnding +
    'begin' + LineEnding + 'end;' + LineEnding + 'procedure Unused;' + LineEnding + '{$ENDIF}' +
    LineEnding + 'begin' + LineEnding + '  Exit;' + LineEnding + 'end;'), ROUTINE_SECTION,
    '{$ifndef} is only supported in the statement part');
  Refuse(UnitText('{$IFDEF WINDOWS}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding +
    'begin' + LineEnding + '{$ELSE}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding +
    'begin' + LineEnding + '  Exit;' + LineEnding + '{$ENDIF}' + LineEnding + 'end;'),
    ROUTINE_SECTION, 'declaration keyword inside a statement block');
  Refuse(UnitText('procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding +
    '{$IFDEF WINDOWS}' + LineEnding + '  Exit;' + LineEnding + 'end;' + LineEnding + '{$ENDIF}'),
    ROUTINE_SECTION, 'a conditional opened in ReviewedRoutine closes after it');
  Refuse(UnitText('{$IFDEF WINDOWS}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding +
    'begin' + LineEnding + '{$ENDIF}' + LineEnding + 'end;'), ROUTINE_SECTION,
    '{$endif} closes a conditional opened outside the routine');
  // A conditional around the whole routine stays outside its text.
  Expect<string>(Locate(UnitText('{$IFDEF WINDOWS}' + LineEnding + ROUTINE_TEXT + LineEnding +
    '{$ENDIF}'), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
end;

procedure TFPCRoutineSourceTests.TestModuleName;
var
  Span: TFPCRoutineSpan;
begin
  Refuse(StringReplace(UnitText(ROUTINE_TEXT), 'unit Fixture;', 'unit Other.Fixture;', []),
    ROUTINE_SECTION, 'declares other.fixture, not fixture');
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_p$fixture_$$_reviewedroutine', 'declares fixture, not p$fixture');
  Span := Locate('program Fixture.Test;' + LineEnding + LineEnding + 'uses' + LineEnding +
    '  SysUtils;' + LineEnding + LineEnding + ROUTINE_TEXT + LineEnding + LineEnding + 'begin' +
    LineEnding + '  ReviewedRoutine(0);' + LineEnding + 'end.' + LineEnding,
    '.text.n_p$fixture.test_$$_reviewedroutine$longint');
  Expect<string>(Span.Digest).ToBe(Digest(ROUTINE_TEXT));
  Refuse(UnitText(ROUTINE_TEXT), '.data.n_fixture_$$_reviewedroutine', 'not a project text section');
end;

procedure TFPCRoutineSourceTests.TestIncompleteParse;
begin
  // Text after the final end. could hold an unseen overload.
  Refuse(UnitText(ROUTINE_TEXT) + OTHER_TEXT, ROUTINE_SECTION, 'expected the module''s final end.');
  Refuse(UnitText('procedure Other;' + LineEnding + 'begin' + LineEnding + OTHER_TEXT + LineEnding +
    'end;' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION, 'declaration keyword inside a statement block');
  Refuse(UnitText(ROUTINE_TEXT + LineEnding + '{ unterminated'), ROUTINE_SECTION, 'unterminated comment');
  Refuse(UnitText(ROUTINE_TEXT + LineEnding + 'const Text = ''unterminated;'), ROUTINE_SECTION,
    'unterminated string');
  Refuse(UnitText(StringReplace(ROUTINE_TEXT, 'begin', 'begin' + LineEnding + '  begin', [])),
    ROUTINE_SECTION, 'expected ; after routine body');
  Refuse('{$MACRO ON}' + LineEnding + UnitText(ROUTINE_TEXT), ROUTINE_SECTION, 'macros');
  Refuse(UnitText('{$DEFINE ReviewedRoutine:=Other}' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION, 'macros');
end;

procedure TFPCRoutineSourceTests.TestDeclarations;
const
  DECLARATIONS = 'type' + LineEnding +
    '  TError = class(Exception);' + LineEnding +
    '  TForward = class;' + LineEnding +
    '  TForwardClass = class of TForward;' + LineEnding +
    '  TCallback = procedure(const AValue: Integer) of object;' + LineEnding +
    '  TFunction = function: Integer;' + LineEnding +
    '  TForward = class(TObject)' + LineEnding +
    '    class procedure ReviewedRoutine;' + LineEnding +
    '    procedure Inner; virtual;' + LineEnding +
    '  end;' + LineEnding +
    '  TVariant = record' + LineEnding +
    '    case Integer of' + LineEnding +
    '      0: (Value: Integer);' + LineEnding +
    '      1: (Nested: record Inner: Byte; end);' + LineEnding +
    '  end;' + LineEnding +
    '  IReviewed = interface' + LineEnding +
    '    procedure ReviewedRoutine;' + LineEnding +
    '  end;' + LineEnding +
    'var' + LineEnding +
    '  Callback: procedure; cdecl;' + LineEnding +
    'const' + LineEnding +
    '  Values: array[0..1] of Integer = (0, 1);' + LineEnding;
  LOCAL_TYPES = 'procedure ReviewedRoutine(const AValue: Integer);' + LineEnding +
    'type' + LineEnding + '  TLocal = record' + LineEnding + '    Value: Integer;' + LineEnding +
    '  end;' + LineEnding + 'var' + LineEnding + '  Local: TLocal;' + LineEnding + 'begin' +
    LineEnding + '  Local.Value := AValue;' + LineEnding + 'end;';
var
  Span: TFPCRoutineSpan;
begin
  Span := Locate(UnitText(DECLARATIONS + LineEnding + ROUTINE_TEXT + LineEnding + OTHER_TEXT,
    DECLARATIONS), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest(ROUTINE_TEXT));
  Span := Locate(UnitText(LOCAL_TYPES + LineEnding + OTHER_TEXT), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest(LOCAL_TYPES));
  Span := Locate(UnitText(ROUTINE_TEXT + LineEnding + LineEnding + 'initialization' + LineEnding +
    '  ReviewedRoutine(0);' + LineEnding + 'finalization' + LineEnding + '  ReviewedRoutine(1);'),
    ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest(ROUTINE_TEXT));
end;

begin
  TestRunnerProgram.AddSuite(TFPCRoutineSourceTests.Create);
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
