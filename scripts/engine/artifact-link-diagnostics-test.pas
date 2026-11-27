program ArtifactLinkDiagnosticsTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  Goccia.Artifact,
  Goccia.Embed;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckError(const AMode, APath: UnicodeString; const AResult: TGocciaRunResult;
  ALine, AColumn: Integer; const AExport: UnicodeString = 'missing');
begin
  Check(not AResult.Ok and (AResult.ErrorName = 'SyntaxError') and
    (Pos('has no export named "' + AExport + '"', AResult.ErrorText) > 0), AMode + ': ' + AResult.ErrorMessage);
  Check((AResult.ErrorFile = APath) and (AResult.ErrorLine = ALine) and (AResult.ErrorColumn = AColumn),
    AMode + ': expected ' + APath + ':' + IntToStr(ALine) + ':' + IntToStr(AColumn) + ', got ' +
    AResult.ErrorFile + ':' + IntToStr(AResult.ErrorLine) + ':' + IntToStr(AResult.ErrorColumn));
end;

procedure CheckCase(const APath, ASource: UnicodeString; ALine, AColumn: Integer;
  AGoal: TGocciaSourceKind = gskModule; const AExport: UnicodeString = 'missing');
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Bytes: TBytes;
  Bytecode: RawByteString;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.SourceKind := AGoal;
    RunResult := Engine.Run(APath, ASource);
    CheckError('source', APath, RunResult, ALine, AColumn, AExport);
  finally
    Engine.Free;
  end;

  Engine := TGocciaEngine.Create;
  try
    Engine.SourceKind := AGoal;
    Check(Engine.CompileArtifact(APath, ASource, Bytes, RunResult), 'could not compile: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
  end;
  SetString(Bytecode, PAnsiChar(@Bytes[0]), Length(Bytes));

  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.RunArtifact(ChangeFileExt(APath, '.gbc'), Bytecode);
    CheckError('artifact', APath, RunResult, ALine, AColumn, AExport);
  finally
    Engine.Free;
  end;
end;

procedure CheckInvalidLocation(const APath: UnicodeString; ALine, AColumn: Integer);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Artifact, InvalidArtifact: TGocciaArtifact;
  Bytes, InvalidBytes: TBytes;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Check(Engine.CompileArtifact(APath, 'import { present } from "./dep.mjs";', Bytes, RunResult),
      'could not compile malformed-location fixture: ' + RunResult.ErrorMessage);
    Check(LoadArtifact(@Bytes[0], Length(Bytes), APath, 0, Artifact, ErrorText), ErrorText);
    try
      Artifact.Info.Imports[0].SourceLine := ALine;
      Artifact.Info.Imports[0].SourceColumn := AColumn;
      InvalidBytes := SaveArtifact(Artifact);
    finally
      Artifact.Info.Free;
    end;
    if LoadArtifact(@InvalidBytes[0], Length(InvalidBytes), APath, 0, InvalidArtifact, ErrorText) then
    begin
      InvalidArtifact.Info.Free;
      raise Exception.Create('an invalid declaration location was accepted');
    end;
    Check(Pos('invalid module declaration location', ErrorText) > 0, 'wrong verifier error: ' + ErrorText);
  finally
    Engine.Free;
  end;
end;

procedure CheckDynamic(const AEntry, AImportPath: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    RunResult := Engine.Run(AEntry, 'await import("./dynamic.mjs");');
    CheckError('dynamic rejection', AImportPath, RunResult, 2, 1);
    RunResult := Engine.Run(ChangeFileExt(AEntry, '.second.mjs'),
      'await import("./dynamic.mjs").then(() => false, error => error.stack.includes("dynamic.mjs:2:1)"));');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'), 'dynamic rejection lost the declaration stack frame');
  finally
    Engine.Free;
  end;
end;

const
  PresentImport = 'import { present } from "./dep.mjs";';
  MissingImport = 'import { missing } from "./dep.mjs";';
var
  Directory, Entry: UnicodeString;
  Dependency: TStringList;
begin
  Directory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
  Dependency := TStringList.Create;
  try
    Dependency.Add('export const present = 1;');
    Dependency.SaveToFile(Directory + 'dep.mjs');
    Dependency.Text := PresentImport + LineEnding + MissingImport + LineEnding;
    Dependency.SaveToFile(Directory + 'dynamic.mjs');
  finally
    Dependency.Free;
  end;

  Entry := Directory + 'entry.mjs';
  CheckCase(Entry, '  import { present } from "./dep.mjs";'#10'import { missing } from "./dep.mjs";', 2, 1);
  CheckCase(Entry, '  import { missing } from "./dep.mjs";'#10'import { present } from "./dep.mjs";', 1, 3);
  CheckCase(Entry, ' export { present } from "./dep.mjs";'#10'   export { missing } from "./dep.mjs";', 2, 4);
  CheckCase(Entry, ' export { missing } from "./dep.mjs";'#10'   export { present } from "./dep.mjs";', 1, 2);
  CheckCase(Directory + 'script(#script-imports).mjs', PresentImport + '  ' + MissingImport,
    1, Length(PresentImport) + 3, gskScript);
  CheckCase(Directory + 'script-combined.mjs', '  import missing, * as ns from "./dep.mjs";',
    1, 3, gskScript, 'default');
  CheckDynamic(Directory + 'dynamic-entry.mjs', Directory + 'dynamic.mjs');
  CheckInvalidLocation(Entry, 1, -1);
  CheckInvalidLocation(Entry, 0, 0);
  WriteLn('Artifact and source link locations: passed');
end.
