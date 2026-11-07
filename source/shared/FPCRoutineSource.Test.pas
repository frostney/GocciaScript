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
    FCompilerDirectories: array of string;
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
    procedure TestHeaderDirectives;
    procedure TestAssembler;
    procedure TestIncludedBlocks;
    procedure TestLocatorCache;
    procedure TestIncludeSearch;
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
  ForceDirectories(FDirectory + PathDelim + 'cwd');
  ForceDirectories(FDirectory + PathDelim + 'absolute');
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
  RemoveDir(FDirectory + PathDelim + 'cwd');
  RemoveDir(FDirectory + PathDelim + 'absolute');
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
  Result := LocateFPCRoutine(FSourceFile, ASection, FCompilerDirectories, [FDirectory + PathDelim + 'includes']);
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
  Test('accepts conditional directives in the routine header only when the parse is the same',
    TestHeaderDirectives);
  Test('reads assembler blocks and escaped words as the compiler does', TestAssembler);
  Test('refuses includes that change the block structure', TestIncludedBlocks);
  Test('parses each file once and keeps file refusals apart from routine refusals', TestLocatorCache);
  Test('finds includes where FPC does and refuses names that match different files', TestIncludeSearch);
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
  Span: TFPCRoutineSpan;
begin
  Expected := Digest(ROUTINE_TEXT);
  Expect<string>(Locate(UnitText(ROUTINE_TEXT + LineEnding + OTHER_TEXT), ROUTINE_SECTION).Digest).ToBe(Expected);
  Expect<string>(Locate(UnitText('{ Moved and commented. }' + LineEnding + OTHER_TEXT + LineEnding +
    StringReplace(OTHER_TEXT, 'Unrelated', 'Added', []) + LineEnding + LineEnding + ROUTINE_TEXT,
    'procedure ReviewedRoutine(const AValue: Integer);'), ROUTINE_SECTION).Digest).ToBe(Expected);
  Span := Locate(StringReplace(UnitText(ROUTINE_TEXT + LineEnding + OTHER_TEXT), LineEnding,
    #13#10, [rfReplaceAll]), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Expected);
  Expect<Integer>(Span.FirstLine).ToBe(10);
  Expect<Integer>(Span.LastLine).ToBe(22);
  Span := Locate(StringReplace(UnitText(ROUTINE_TEXT + LineEnding + OTHER_TEXT), LineEnding,
    #13, [rfReplaceAll]), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Expected);
  Expect<Integer>(Span.FirstLine).ToBe(10);
  Expect<Integer>(Span.LastLine).ToBe(22);
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
    LocateFPCRoutine(FSourceFile, ROUTINE_SECTION, [], []);
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
  Refuse(UnitText(StringReplace(ROUTINE_TEXT, '  try', '  (*$I local.inc*)' + LineEnding + '  try', [])),
    ROUTINE_SECTION, '{$i} includes text that no reviewed digest covers in ReviewedRoutine');
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
  WriteText(FDirectory + PathDelim + 'local.inc', '{$mode objfpc}' + LineEnding);
  Refuse(UnitText('{$I local.inc}' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION,
    'only Delphi-mode lexing is supported');
  WriteText(FDirectory + PathDelim + 'local.inc', '(*$I local.inc*)' + LineEnding);
  Refuse(UnitText('{$I local.inc}' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION,
    'includes nest too deeply');
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
    '{$else} changes the block structure between conditional branches');
  // Without WINDOWS the compiled body is the one the parse gives to Unused.
  Refuse(UnitText('procedure ReviewedRoutine;' + LineEnding + '{$IFNDEF WINDOWS}' + LineEnding +
    'begin' + LineEnding + 'end;' + LineEnding + 'procedure Unused;' + LineEnding + '{$ENDIF}' +
    LineEnding + 'begin' + LineEnding + '  Exit;' + LineEnding + 'end;'), ROUTINE_SECTION,
    '{$endif} changes the block structure between conditional branches');
  Refuse(UnitText('{$IFDEF WINDOWS}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding +
    'begin' + LineEnding + '{$ELSE}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding +
    'begin' + LineEnding + '  Exit;' + LineEnding + '{$ENDIF}' + LineEnding + 'end;'),
    ROUTINE_SECTION, 'declaration keyword inside a statement block');
  Refuse(UnitText('procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding +
    '{$IFDEF WINDOWS}' + LineEnding + '  Exit;' + LineEnding + 'end;' + LineEnding + '{$ENDIF}'),
    ROUTINE_SECTION, '{$endif} changes the block structure between conditional branches');
  Refuse(UnitText('{$IFDEF WINDOWS}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding +
    'begin' + LineEnding + '{$ENDIF}' + LineEnding + 'end;'), ROUTINE_SECTION,
    '{$endif} changes the block structure between conditional branches');
  // A dead branch that opens a type hides the compiled routine inside it,
  // so the parse would hash the reviewed copy in the other dead branch.
  Refuse(UnitText('{$IFDEF NEVER}' + LineEnding + 'type' + LineEnding + '  TNever = record' +
    LineEnding + '{$ENDIF}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding + 'begin' +
    LineEnding + '  Halt;' + LineEnding + 'end;' + LineEnding + '{$IFDEF NEVER}' + LineEnding +
    'procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding + 'end;' + LineEnding + '{$ENDIF}'),
    ROUTINE_SECTION, '{$endif} must follow ; or a section keyword at declaration level');
  // Every branch's implementation counts, so a dead reviewed copy is ambiguous.
  Refuse(UnitText('{$IFDEF NEVER}' + LineEnding + ROUTINE_TEXT + LineEnding + '{$ELSE}' + LineEnding +
    StringReplace(ROUTINE_TEXT, '#39', '#40', []) + LineEnding + '{$ENDIF}'), ROUTINE_SECTION,
    '2 implementations');
  Refuse(UnitText('procedure ReviewedRoutine; {$IFDEF FPC}external ''library''{$ENDIF};' + LineEnding +
    ROUTINE_TEXT), ROUTINE_SECTION, '{$endif} is not supported inside a routine header');
  // Common declaration-level choices keep the parse the same.
  Expected := Digest('procedure ReviewedRoutine; {$IFDEF FPC}inline;{$ENDIF}' + LineEnding + 'begin' +
    LineEnding + 'end;');
  Expect<string>(Locate(UnitText('uses' + LineEnding + '  SysUtils{$IFDEF WINDOWS}, Windows{$ENDIF};' +
    LineEnding + LineEnding + 'const' + LineEnding + '{$IFDEF CPU64}' + LineEnding + '  Size = 8;' +
    LineEnding + '{$ELSE}' + LineEnding + '  Size = 4;' + LineEnding + '{$ENDIF}' + LineEnding +
    'type' + LineEnding + '  TRecord = record' + LineEnding + '    Value: Integer;' + LineEnding +
    '{$IFDEF DEBUG}' + LineEnding + '    Name: string;' + LineEnding + '{$ENDIF}' + LineEnding +
    '  end;' + LineEnding + LineEnding + 'function Other: Boolean; {$IFDEF FPC}inline;{$ENDIF}' +
    LineEnding + 'begin' + LineEnding + '  Result := True;' + LineEnding + 'end;' + LineEnding +
    'procedure ReviewedRoutine; {$IFDEF FPC}inline;{$ENDIF}' + LineEnding + 'begin' + LineEnding +
    'end;'), ROUTINE_SECTION).Digest).ToBe(Expected);
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
  // A name outside Latin-1 is unsupported, not a range error.
  Expect<string>(FPCSectionRoutineName('.text.n_fixture_$$_' + #$0100 + 'routine')).ToBe(
    '.text.n_fixture_$$_' + #$0100 + 'routine');
  Expect<string>(FPCSectionRoutineName('.text.n_' + #$0100 + '_$$_reviewedroutine')).ToBe(
    '.text.n_' + #$0100 + '_$$_reviewedroutine');
  Refuse(UnitText(ROUTINE_TEXT), '.text.n_fixture_$$_' + #$0100 + 'routine', 'unsupported routine name');
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
  // Object Pascal mode nests comments, so the lexer would end one too early.
  Refuse('{$mode objfpc}' + LineEnding + UnitText(ROUTINE_TEXT), ROUTINE_SECTION,
    'only Delphi-mode lexing is supported: {$mode objfpc}');
  Refuse('{$modeswitch nestedcomments}' + LineEnding + UnitText(ROUTINE_TEXT), ROUTINE_SECTION,
    'only Delphi-mode lexing is supported');
  Locate('{$mode delphiunicode}' + LineEnding + UnitText(ROUTINE_TEXT), ROUTINE_SECTION);
  // A conditional opened in the interface must not close in the implementation.
  Refuse(UnitText(ROUTINE_TEXT + LineEnding + '{$ENDIF}', '{$IFDEF NEVER}' + LineEnding),
    ROUTINE_SECTION, '{$endif} changes the block structure between conditional branches');
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
    '  TBox<T: record; U: class> = class' + LineEnding +
    '    Value: T;' + LineEnding +
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


procedure TFPCRoutineSourceTests.TestHeaderDirectives;
const
  INLINE_ROUTINE = 'function ReviewedRoutine(const AValue: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}' +
    LineEnding + 'var' + LineEnding + '  Local: Integer;' + LineEnding + 'begin' + LineEnding +
    '  Local := AValue;' + LineEnding + '  Result := Local;' + LineEnding + 'end;';
  METHOD = 'procedure TFirst.ReviewedMethod; {$IFDEF FPC}inline;{$ENDIF}' + LineEnding + 'begin' +
    LineEnding + 'end;';
var
  Expected: string;
begin
  // The house inline idiom keeps a pin: the digest is the routine's text.
  Expected := Locate(UnitText(OTHER_TEXT + LineEnding + INLINE_ROUTINE), ROUTINE_SECTION).Digest;
  Expect<string>(Expected).ToBe(Digest(INLINE_ROUTINE));
  // The idiom cannot hide an edit: the directive and body stay in the text.
  if Locate(UnitText(StringReplace(INLINE_ROUTINE, 'Result := Local;', 'Result := 0;', [])),
    ROUTINE_SECTION).Digest = Expected then
    Fail('Body edit under an inline idiom kept the digest');
  if Locate(UnitText(StringReplace(INLINE_ROUTINE, '{$IFDEF FPC}inline;{$ENDIF}', '', [])),
    ROUTINE_SECTION).Digest = Expected then
    Fail('Removing the inline idiom kept the digest');
  Expect<string>(Locate(UnitText('type' + LineEnding + '  TFirst = class' + LineEnding +
    '    procedure ReviewedMethod; {$IFDEF FPC}inline;{$ENDIF}' + LineEnding + '  end;' + LineEnding +
    LineEnding + METHOD), METHOD_SECTION).Digest).ToBe(Digest(METHOD));
  // A branch that drops the body moves the compiled body out of the routine
  // the parse sees, so it is refused, never hashed.
  Refuse(UnitText('procedure ReviewedRoutine; {$IFDEF NEVER}forward;{$ENDIF}' + LineEnding + 'begin' +
    LineEnding + 'end;'), ROUTINE_SECTION, 'expected the module''s final end.');
  Refuse(UnitText('procedure ReviewedRoutine; {$IFDEF FPC}inline;{$ELSE}external ''x'';{$ENDIF}' +
    LineEnding + 'begin' + LineEnding + 'end;'), ROUTINE_SECTION, 'expected the module''s final end.');
  // With the dead routine's header and body in the parse's view, ReviewedRoutine
  // would end at the parse's first end; X decides which body is compiled.
  Refuse(UnitText('procedure ReviewedRoutine; {$IFDEF X}inline;' + LineEnding + 'begin' + LineEnding +
    'end;' + LineEnding + 'procedure Other; {$ENDIF}' + LineEnding + 'begin' + LineEnding + '  Halt;' +
    LineEnding + 'end;'), ROUTINE_SECTION,
    '{$endif} changes the block structure between conditional branches');
end;

procedure TFPCRoutineSourceTests.TestAssembler;
const
  ASSEMBLER_ROUTINE = 'procedure ReviewedRoutine; assembler;' + LineEnding + 'asm' + LineEnding +
    '@try:' + LineEnding + '  jmp @case' + LineEnding + '@case:' + LineEnding + 'end;';
  ESCAPED_ROUTINE = 'procedure ReviewedRoutine;' + LineEnding + 'var' + LineEnding +
    '  &end: Integer;' + LineEnding + 'begin' + LineEnding + '  &end := 1;' + LineEnding +
    '  &begin(&end);' + LineEnding + 'end;';
var
  Span: TFPCRoutineSpan;
begin
  // In an assembler block only `end` closes; labels named try or case open nothing.
  Span := Locate(UnitText(ASSEMBLER_ROUTINE + LineEnding + OTHER_TEXT), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest(ASSEMBLER_ROUTINE));
  Expect<Integer>(Span.LastLine).ToBe(15);
  Span := Locate(UnitText('procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding + '  asm' +
    LineEnding + '@begin:' + LineEnding + '  end;' + LineEnding + 'end;' + LineEnding + OTHER_TEXT),
    ROUTINE_SECTION);
  Expect<Integer>(Span.LastLine).ToBe(15);
  // `&end` is an identifier, not the end of a block.
  Span := Locate(UnitText(ESCAPED_ROUTINE + LineEnding + OTHER_TEXT), ROUTINE_SECTION);
  Expect<string>(Span.Digest).ToBe(Digest(ESCAPED_ROUTINE));
  Expect<Integer>(Span.LastLine).ToBe(16);
  // A branch that leaves the assembler block at the same depth changes how
  // the compiler reads the text after it.
  Refuse(UnitText('procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding + '  asm' +
    LineEnding + '    nop' + LineEnding + '{$IFDEF X}' + LineEnding + '  end;' + LineEnding + '  begin' +
    LineEnding + '{$ENDIF}' + LineEnding + '  end;' + LineEnding + 'end;'), ROUTINE_SECTION,
    '{$endif} changes the block structure between conditional branches');
end;

procedure TFPCRoutineSourceTests.TestIncludedBlocks;
const
  // The reviewed copy is dead; the compiled routine sits after the include.
  HIDDEN = 'type' + LineEnding + '  TBox = record' + LineEnding + '    Value: Integer;' + LineEnding +
    '{$I local.inc}' + LineEnding + 'procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding +
    '  Halt;' + LineEnding + 'end;' + LineEnding + '{$IFDEF NEVER}' + LineEnding +
    'procedure ReviewedRoutine;' + LineEnding + 'begin' + LineEnding + 'end;' + LineEnding + '{$ENDIF}';
  STATEMENTS = 'procedure Other;' + LineEnding + 'begin' + LineEnding + '  {$I local.inc}' + LineEnding +
    'end;' + LineEnding + ROUTINE_TEXT;

  procedure IncludeText(const AText: string);
  begin
    WriteText(FDirectory + PathDelim + 'local.inc', AText + LineEnding);
  end;

begin
  // A record closed by an include would hide the compiled routine from the
  // parse, which would hash the dead copy.
  IncludeText('  end;');
  Refuse(UnitText(HIDDEN), ROUTINE_SECTION,
    'included text outside a statement block must hold only directives and comments');
  IncludeText('  Extra: Integer;');
  Refuse(UnitText(HIDDEN), ROUTINE_SECTION, 'outside a statement block');
  // A statement include must close every block it opens, and open every
  // block it closes.
  IncludeText('  begin');
  Refuse(UnitText(STATEMENTS), ROUTINE_SECTION, 'included text leaves a block open');
  IncludeText('  end;' + LineEnding + '  begin');
  Refuse(UnitText(STATEMENTS), ROUTINE_SECTION, 'included text closes a block it did not open');
  IncludeText('  asm' + LineEnding + '    nop');
  Refuse(UnitText(STATEMENTS), ROUTINE_SECTION, 'included text leaves a block open');
  IncludeText('{$IFDEF X}' + LineEnding + '  begin' + LineEnding + '{$ENDIF}' + LineEnding + '  end;' +
    LineEnding + '{$IFDEF X}' + LineEnding + '  begin' + LineEnding + '{$ENDIF}');
  Refuse(UnitText(STATEMENTS), ROUTINE_SECTION,
    '{$endif} changes the block structure between conditional branches');
  IncludeText('{$IFDEF X}');
  Refuse(UnitText(STATEMENTS), ROUTINE_SECTION, 'a conditional is not closed in its include file');
  IncludeText('{$ELSE}');
  Refuse(UnitText('{$IFDEF X}' + LineEnding + '{$I local.inc}' + LineEnding + '{$ENDIF}' + LineEnding +
    ROUTINE_TEXT), ROUTINE_SECTION, '{$else} has no opening conditional in its include file');
  IncludeText('  nop');
  Refuse(UnitText('procedure Other; assembler;' + LineEnding + 'asm' + LineEnding + '  {$I local.inc}' +
    LineEnding + 'end;' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION,
    '{$i} inside an assembler block is not supported');
  // Balanced statements, including blocks inside branches, are accepted.
  IncludeText('  try' + LineEnding + '{$IFDEF X}' + LineEnding + '    begin' + LineEnding + '    end;' +
    LineEnding + '{$ENDIF}' + LineEnding + '  finally' + LineEnding + '  end;');
  Expect<string>(Locate(UnitText(STATEMENTS), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
  // Directives and comments are accepted anywhere.
  IncludeText('{ Settings. }' + LineEnding + '{$IFDEF X}' + LineEnding + '  {$DEFINE Y}' + LineEnding +
    '{$ENDIF}');
  Expect<string>(Locate(UnitText('{$I local.inc}' + LineEnding + STATEMENTS),
    ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
end;

procedure TFPCRoutineSourceTests.TestLocatorCache;
const
  TYPES = 'type' + LineEnding + '  TFirst = class' + LineEnding + '    procedure ReviewedMethod;' +
    LineEnding + '  end;' + LineEnding;
  FIRST = 'procedure TFirst.ReviewedMethod;' + LineEnding + 'begin' + LineEnding + 'end;';
var
  Locator: TFPCRoutineLocator;
  Scope: TFPCRoutineRefusal;
  RefusedFile: string;

  function RefusalScope(const ASection: string): TFPCRoutineRefusal;
  begin
    Result := frRoutine;
    try
      Locator.Locate(FSourceFile, ASection);
      Fail('Expected a refusal for ' + ASection);
    except
      on E: EFPCRoutineSource do
      begin
        Result := E.Scope;
        RefusedFile := E.SourceFile;
      end;
    end;
  end;

begin
  WriteText(FSourceFile, UnitText(ROUTINE_TEXT + LineEnding + FIRST, TYPES));
  Locator := TFPCRoutineLocator.Create([], [FDirectory + PathDelim + 'includes']);
  try
    Expect<string>(Locator.Locate(FSourceFile, ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
    // The file is read once: later edits are not seen within one run.
    WriteText(FSourceFile, UnitText(OTHER_TEXT));
    Expect<string>(Locator.Locate(FSourceFile, METHOD_SECTION).Digest).ToBe(Digest(FIRST));
    Scope := RefusalScope(OTHER_METHOD_SECTION);
    Expect<Boolean>(Scope = frRoutine).ToBe(True);
    Expect<string>(RefusedFile).ToBe(FSourceFile);
  finally
    Locator.Free;
  end;
  // A file refusal repeats for every section and is not about the routine.
  WriteText(FSourceFile, UnitText(ROUTINE_TEXT) + OTHER_TEXT);
  Locator := TFPCRoutineLocator.Create([], [FDirectory + PathDelim + 'includes']);
  try
    Scope := RefusalScope(ROUTINE_SECTION);
    Expect<Boolean>(Scope = frFile).ToBe(True);
    Expect<string>(RefusedFile).ToBe(FSourceFile);
    Scope := RefusalScope(METHOD_SECTION);
    Expect<Boolean>(Scope = frFile).ToBe(True);
  finally
    Locator.Free;
  end;
end;

// FPC 3.2.4 tries each extension in turn and, for each, the includer's
// directory, its working directory, then each -Fi directory.
procedure TFPCRoutineSourceTests.TestIncludeSearch;
const
  CLOSES_BLOCK = '  end;';
  NEUTRAL = '{ Settings. }';
var
  Local, Includes, WorkingDirectory, Absolute: string;
  Created: TStringList;
  Item: string;

  procedure Put(const AFileName, AText: string);
  begin
    WriteText(AFileName, AText + LineEnding);
    Created.Add(UTF8Encode(AFileName));
  end;

  procedure Remove;
  var
    Index: Integer;
  begin
    for Index := 0 to Created.Count - 1 do
      DeleteFile(string(Created[Index]));
    Created.Clear;
  end;

  function Including(const AName: string): string;
  begin
    Result := UnitText('{$I ' + AName + '}' + LineEnding + ROUTINE_TEXT);
  end;

begin
  Local := FDirectory + PathDelim;
  Includes := FDirectory + PathDelim + 'includes' + PathDelim;
  WorkingDirectory := FDirectory + PathDelim + 'cwd';
  Absolute := FDirectory + PathDelim + 'absolute' + PathDelim;
  Created := TStringList.Create;
  try
    // The working directory comes before the -Fi directories, so a file
    // there with other text could be the one FPC reads.
    Put(Includes + 'search.inc', NEUTRAL);
    Put(WorkingDirectory + PathDelim + 'search.inc', CLOSES_BLOCK);
    Expect<string>(Locate(Including('search.inc'), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
    SetLength(FCompilerDirectories, 1);
    FCompilerDirectories[0] := WorkingDirectory;
    Refuse(Including('search.inc'), ROUTINE_SECTION, 'include search.inc names different files');
    // The same text in both places compiles the same either way.
    Put(WorkingDirectory + PathDelim + 'search.inc', NEUTRAL);
    Expect<string>(Locate(Including('search.inc'), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
    // Only the working directory has it: that is the file checked.
    Remove;
    Put(WorkingDirectory + PathDelim + 'search.inc', CLOSES_BLOCK);
    Refuse(Including('search.inc'), ROUTINE_SECTION, 'cwd' + PathDelim + 'search.inc:1: included text outside');
    Remove;
    FCompilerDirectories := nil;
    // FPC tries the exact name in every directory before adding `.inc`, so
    // -Fi's `search` competes with the includer's `search.inc`.
    Put(Local + 'search.inc', NEUTRAL);
    Put(Includes + 'search', CLOSES_BLOCK);
    Refuse(Including('search'), ROUTINE_SECTION, 'include search names different files');
    Remove;
    Put(Includes + 'search.pp', CLOSES_BLOCK);
    Refuse(Including('search'), ROUTINE_SECTION, 'search.pp:1: included text outside');
    Remove;
    // An absolute name is read from its own directory only.
    Put(Local + 'search.inc', NEUTRAL);
    Put(Absolute + 'search.inc', CLOSES_BLOCK);
    Refuse(Including(Absolute + 'search.inc'), ROUTINE_SECTION,
      'absolute' + PathDelim + 'search.inc:1: included text outside');
    Refuse(Including(Absolute + 'missing.inc'), ROUTINE_SECTION, 'include file not found');
    Remove;
    // FPC also tries the lowercased and uppercased name. macOS and Windows
    // volumes usually ignore case, so the spellings name one file there.
    Put(Local + 'search.inc', CLOSES_BLOCK);
    if FileExists(Local + 'SEARCH.INC') then
    begin
      // The spelling as written is tried first and opens the one file.
      Refuse(Including('Search.inc'), ROUTINE_SECTION, 'Search.inc:1: included text outside');
      Put(Local + 'Search.inc', NEUTRAL);
      Expect<string>(Locate(Including('Search.inc'), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
    end
    else
    begin
      Refuse(Including('Search.inc'), ROUTINE_SECTION, 'search.inc:1: included text outside');
      Put(Local + 'Search.inc', NEUTRAL);
      Refuse(Including('Search.inc'), ROUTINE_SECTION, 'include Search.inc names different files');
    end;
    Remove;
    // FPC searches a nested include beside the parent it read. Identical
    // parents in two directories could each find another child, so a
    // parent that includes more must be found in one directory only.
    SetLength(FCompilerDirectories, 2);
    FCompilerDirectories[0] := WorkingDirectory;
    FCompilerDirectories[1] := Absolute;
    ForceDirectories(WorkingDirectory + PathDelim + 'nested');
    ForceDirectories(Absolute + 'nested');
    Put(WorkingDirectory + PathDelim + 'nested' + PathDelim + 'parent.inc', '{$I child.inc}');
    Put(Absolute + 'nested' + PathDelim + 'parent.inc', '{$I child.inc}');
    Put(WorkingDirectory + PathDelim + 'nested' + PathDelim + 'child.inc', NEUTRAL);
    Put(Absolute + 'nested' + PathDelim + 'child.inc', CLOSES_BLOCK);
    Refuse(Including('nested/parent.inc'), ROUTINE_SECTION,
      'include nested/parent.inc is found in more than one directory');
    // A parent that includes nothing more reads the same text from either.
    Put(Absolute + 'nested' + PathDelim + 'parent.inc', NEUTRAL);
    Put(WorkingDirectory + PathDelim + 'nested' + PathDelim + 'parent.inc', NEUTRAL);
    Expect<string>(Locate(Including('nested/parent.inc'), ROUTINE_SECTION).Digest).ToBe(Digest(ROUTINE_TEXT));
    Remove;
    RemoveDir(WorkingDirectory + PathDelim + 'nested');
    RemoveDir(Absolute + 'nested');
    FCompilerDirectories := nil;
    // Names whose file FPC derives from elsewhere are refused.
    Refuse(Including('*.inc'), ROUTINE_SECTION, 'unsupported include name');
    Refuse(Including('search.'), ROUTINE_SECTION, 'unsupported include name');
    Refuse(UnitText('{$INCLUDEPATH cwd}' + LineEnding + ROUTINE_TEXT), ROUTINE_SECTION,
      '{$includepath} changes where FPC finds includes');
  finally
    FCompilerDirectories := nil;
    Remove;
    Created.Free;
  end;
  for Item in [Local + 'search.inc'] do
    DeleteFile(Item);
end;
begin
  TestRunnerProgram.AddSuite(TFPCRoutineSourceTests.Create);
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
