{ Which strict type an annotation gets under --strict-types: the outermost
  form of the type, enforced only where every value the type admits passes the
  check, and the annotation text the parser collects for it. }

program Goccia.Types.Enforcement.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Bytecode.Chunk,
  Goccia.SourcePipeline,
  Goccia.TestSetup,
  Goccia.Types.Enforcement;

type
  TTypeEnforcementTests = class(TTestSuite)
  private
    procedure ExpectType(const AAnnotation: string;
      const AExpected: TGocciaLocalType);
    function CollectedAnnotation(const ASource: string): string;
  public
    procedure SetupTests; override;

    procedure TestKeywords;
    procedure TestObjectForms;
    procedure TestPrecedence;
    procedure TestUnenforcedForms;
    procedure TestNamesAreNotEnforced;
    procedure TestLiteralTokens;
    procedure TestParserKeepsLiteralTypesWhole;
  end;

procedure TTypeEnforcementTests.SetupTests;
begin
  Test('number, string, boolean and object get their strict type',
    TestKeywords);
  Test('Arrays, tuples and function and constructor types are objects',
    TestObjectForms);
  Test('The outermost form decides, in TypeScript precedence',
    TestPrecedence);
  Test('Forms that admit values outside a strict type stay unenforced',
    TestUnenforcedForms);
  Test('A name is not enforced, since a program can rebind it',
    TestNamesAreNotEnforced);
  Test('A literal type is one token whatever its text holds',
    TestLiteralTokens);
  Test('The parser collects string and template literal types whole',
    TestParserKeepsLiteralTypesWhole);
end;

procedure TTypeEnforcementTests.ExpectType(const AAnnotation: string;
  const AExpected: TGocciaLocalType);
begin
  Expect<Integer>(Ord(TypeAnnotationToLocalType(AAnnotation)))
    .ToBe(Ord(AExpected));
end;

{ The annotation text of the first parameter of `const f = (<p>) => p;`. }
function TTypeEnforcementTests.CollectedAnnotation(
  const ASource: string): string;
var
  Lines: TStringList;
  PipelineResult: TGocciaSourcePipelineResult;
  ProgramNode: TGocciaProgram;
  Declaration: TGocciaVariableDeclaration;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := ASource;
    PipelineResult := TGocciaSourcePipeline.Parse(Lines, 'annotation.ts',
      TGocciaSourcePipeline.DefaultOptions);
    try
      ProgramNode := PipelineResult.TakeProgramNode;
    finally
      PipelineResult.Free;
    end;
  finally
    Lines.Free;
  end;
  try
    Declaration := ProgramNode.Body[0] as TGocciaVariableDeclaration;
    Result := (Declaration.Variables[0].Initializer as
      TGocciaArrowFunctionExpression).Parameters[0].TypeAnnotation;
  finally
    ProgramNode.Free;
  end;
end;

procedure TTypeEnforcementTests.TestKeywords;
begin
  ExpectType('number', sltFloat);
  ExpectType('string', sltString);
  ExpectType('boolean', sltBoolean);
  ExpectType('object', sltReference);
  ExpectType('( number )', sltFloat);
  ExpectType('', sltUntyped);
end;

procedure TTypeEnforcementTests.TestObjectForms;
begin
  ExpectType('number [ ]', sltReference);
  ExpectType('number[]', sltReference);
  ExpectType('( string | number ) [ ]', sltReference);
  ExpectType('readonly number [ ]', sltReference);
  ExpectType('[ string , number ]', sltReference);
  ExpectType('readonly [ string ]', sltReference);
  ExpectType('( ) => void', sltReference);
  ExpectType('< T > ( a : T ) => T', sltReference);
  ExpectType('new ( ) => object', sltReference);
  ExpectType('abstract new ( ) => object', sltReference);
  ExpectType('( ( ) => void ) [ ]', sltReference);
  ExpectType('typeof y [ ]', sltReference);
end;

procedure TTypeEnforcementTests.TestPrecedence;
begin
  // A function type's return type runs to the end.
  ExpectType('( ) => { } & { }', sltReference);
  ExpectType('( a : number ) => string | number', sltReference);
  ExpectType('new ( ) => { } & { }', sltReference);
  // keyof applies to the array type after it.
  ExpectType('keyof string [ ]', sltUntyped);
  // A parenthesized function type in a union is a member of the union.
  ExpectType('( ( ) => void ) | number', sltUntyped);
  ExpectType('string extends string ? number : object', sltUntyped);
end;

procedure TTypeEnforcementTests.TestUnenforcedForms;
begin
  ExpectType('N [ "kind" ]', sltUntyped);
  ExpectType('( typeof y ) [ number ]', sltUntyped);
  ExpectType('string | number', sltUntyped);
  ExpectType('| number', sltUntyped);
  ExpectType('& number', sltUntyped);
  ExpectType('keyof { a : 1 }', sltUntyped);
  ExpectType('typeof y', sltUntyped);
  ExpectType('unique symbol', sltUntyped);
  ExpectType('{ length : number }', sltUntyped);
  ExpectType('{ }', sltUntyped);
  ExpectType('any', sltUntyped);
  ExpectType('unknown', sltUntyped);
end;

procedure TTypeEnforcementTests.TestNamesAreNotEnforced;
begin
  ExpectType('Array < number >', sltUntyped);
  ExpectType('Map < string , number >', sltUntyped);
  ExpectType('Promise < number >', sltUntyped);
  ExpectType('Readonly < string >', sltUntyped);
  ExpectType('Function', sltUntyped);
  ExpectType('Object', sltUntyped);
  ExpectType('N', sltUntyped);
end;

procedure TTypeEnforcementTests.TestLiteralTokens;
begin
  ExpectType('"number"', sltUntyped);
  ExpectType('''object''', sltUntyped);
  ExpectType('"[]"', sltUntyped);
  ExpectType('"a\"]"', sltUntyped);
  ExpectType('`\`a${string}\``', sltUntyped);
  // A quote, bracket or `${` inside the template is still inside it.
  ExpectType('`\`a${"}" | \`]\`}\`` | string [ ]', sltUntyped);
  ExpectType('`\`a${string /* ${ */}b\`` [ ]', sltReference);
end;

procedure TTypeEnforcementTests.TestParserKeepsLiteralTypesWhole;
begin
  Expect<string>(CollectedAnnotation('const f = (p: "number") => p;'))
    .ToBe('"number"');
  Expect<string>(CollectedAnnotation('const f = (p: N["kind"]) => p;'))
    .ToBe('N [ "kind" ]');
  Expect<string>(CollectedAnnotation(
    'const f = (p: `a${"}"}`[]) => p;'))
    .ToBe('`\`a${"}"}\`` [ ]');
  Expect<string>(CollectedAnnotation(
    'const f = (p: `a\`b`) => p;'))
    .ToBe('`\`a\\\`b\``');
end;

begin
  TestRunnerProgram.AddSuite(TTypeEnforcementTests.Create(
    'Strict type annotations'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.
