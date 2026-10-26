program Goccia.ErasedTypeDeclarations.Test;

{$I Goccia.inc}

{ A type-level declaration is erased into an empty statement, and the compiler
  must keep receiving exactly that: the same class, the same span and no
  children. It compares exact class types in places, so a subclass would not
  be the same node to it. What the statement was erased from rides along as a
  tag that only source-reporting tools read. }

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,
  TextSemantics,

  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.GarbageCollector,
  Goccia.SourcePipeline,
  Goccia.TestSetup;

type
  TErasedTypeDeclarationTests = class(TTestSuite)
  private
    procedure ExpectErased(const ASource: string;
      const AExpected: TGocciaErasedTypeKind);

    procedure TestTypeAlias;
    procedure TestInterface;
    procedure TestExportTypeAlias;
    procedure TestExportInterface;
    procedure TestImportType;
    procedure TestImportTypeNamespace;
    procedure TestExportTypeList;
    procedure TestExportTypeListFrom;
    procedure TestExportTypeStar;
    procedure TestSemicolon;
  public
    procedure SetupTests; override;
  end;

procedure TErasedTypeDeclarationTests.SetupTests;
begin
  Test('type is erased as a type alias', TestTypeAlias);
  Test('interface is erased as an interface', TestInterface);
  Test('export type with a name is erased as an exported alias',
    TestExportTypeAlias);
  Test('export interface is erased as an exported interface',
    TestExportInterface);
  Test('import type is erased as an import', TestImportType);
  Test('import type of a namespace is erased as an import',
    TestImportTypeNamespace);
  Test('export type with a list is erased as an export', TestExportTypeList);
  Test('export type with a list and from is erased as a re-export',
    TestExportTypeListFrom);
  Test('export type * is erased as a re-export', TestExportTypeStar);
  Test('a semicolon is an empty statement with no erased type',
    TestSemicolon);
end;

function ParseSource(const ASource: string): TGocciaSourcePipelineResult;
var
  Lines: TStringList;
  Options: TGocciaSourcePipelineOptions;
begin
  Options := TGocciaSourcePipeline.DefaultOptions;
  Options.SourceType := stModule;
  Lines := CreateTextLines(ASource);
  try
    Result := TGocciaSourcePipeline.Parse(Lines, 'erased.ts', Options);
  finally
    Lines.Free;
  end;
end;

procedure TErasedTypeDeclarationTests.ExpectErased(const ASource: string;
  const AExpected: TGocciaErasedTypeKind);
var
  Parsed: TGocciaSourcePipelineResult;
  Statement: TGocciaStatement;
begin
  Parsed := ParseSource(ASource);
  try
    Expect<Integer>(Parsed.ProgramNode.Body.Count).ToBe(1);
    Statement := Parsed.ProgramNode.Body[0];
    Expect<Boolean>(Statement.ClassType = TGocciaEmptyStatement).ToBe(True);
    if Statement.ClassType = TGocciaEmptyStatement then
    begin
      Expect<Integer>(Ord(TGocciaEmptyStatement(Statement).ErasedTypeKind))
        .ToBe(Ord(AExpected));
      Expect<Integer>(Statement.Span.StartOffset).ToBe(0);
      Expect<Integer>(Statement.Span.EndOffset).ToBe(Length(ASource));
    end;
  finally
    Parsed.Free;
  end;
end;

procedure TErasedTypeDeclarationTests.TestTypeAlias;
begin
  ExpectErased('type T = string | number;', etkTypeAlias);
end;

procedure TErasedTypeDeclarationTests.TestInterface;
begin
  ExpectErased('interface I { x: number }', etkInterface);
end;

procedure TErasedTypeDeclarationTests.TestExportTypeAlias;
begin
  ExpectErased('export type T = { a: 1 };', etkExportTypeAlias);
end;

procedure TErasedTypeDeclarationTests.TestExportInterface;
begin
  ExpectErased('export interface I {}', etkExportInterface);
end;

procedure TErasedTypeDeclarationTests.TestImportType;
begin
  ExpectErased('import type { A } from "./a.ts";', etkImport);
end;

procedure TErasedTypeDeclarationTests.TestImportTypeNamespace;
begin
  ExpectErased('import type * as NS from "./ns.ts";', etkImport);
end;

procedure TErasedTypeDeclarationTests.TestExportTypeList;
begin
  ExpectErased('export type { A, B as C };', etkExport);
end;

procedure TErasedTypeDeclarationTests.TestExportTypeListFrom;
begin
  ExpectErased('export type { A } from "./a.ts";', etkReExport);
end;

procedure TErasedTypeDeclarationTests.TestExportTypeStar;
begin
  ExpectErased('export type * as S from "./s.ts";', etkReExport);
end;

procedure TErasedTypeDeclarationTests.TestSemicolon;
begin
  ExpectErased(';', etkNone);
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(
      TErasedTypeDeclarationTests.Create('ErasedTypeDeclarations'));
    RunGocciaTests;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
