program ArtifactLinkDiagnosticsTest;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  Goccia.Artifact,
  Goccia.Core,
  Goccia.Embed,
  Goccia.HeapSnapshot,
  Goccia.HostAPI,
  Goccia.VM;

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
    RunResult := Engine.Run(ChangeFileExt(AEntry, '.third.mjs'),
      'const load = async () => { await import("./dynamic.mjs"); };' +
      'await load().then(() => false, error => error.stack.includes("dynamic.mjs:2:1)") && ' +
      'error.stack.includes("at load ("));');
    Check(RunResult.Ok and (RunResult.ResultText = 'true'), 'dynamic rejection lost the import() call''s frames');
  finally
    Engine.Free;
  end;
end;

procedure CheckLinkFailurePrecedence(const AEntry, AImportPath: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.Run(AEntry,
      'const failed = await import("./load-failure.mjs").then(() => false, error => error.message.includes("absent.mjs"));'#10 +
      'if (!failed) throw new Error("expected the first load to fail");'#10 +
      'await import("./dynamic.mjs");');
    CheckError('link rejection after a caught load failure', AImportPath, RunResult, 2, 1);
  finally
    Engine.Free;
  end;
end;

procedure CheckSnapshot(const AEntry, AImportPath: UnicodeString);
const
  Source = 'try { await import("./dynamic.mjs"); } catch (error) { expect(error).toBeDefined(); }';
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Roots: TArray<TGocciaHeapRoot>;
  ErrorText, SourcePath: UnicodeString;
  Observations, SourceLine, SourceColumn: Integer;
  Restored: TValue;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.TestMode := True;
    Engine.HeapSnapshots := True;
    Check(Engine.PrepareHost(ErrorText), 'could not prepare snapshot host: ' + ErrorText);
    Check(InstallTestingAPI(ErrorText), 'could not prepare snapshot observer: ' + ErrorText);
    RunResult := Engine.Run(AEntry, Source);
    Engine.EndTests;
    Check(RunResult.Ok, 'catching the link error failed: ' + RunResult.ErrorMessage);
    Roots := Engine.HeapSnapshotObserved(Observations);
    Check((Observations = 1) and (Length(Roots) = 1), 'expected to observe exactly one caught error');
    Check(Length(Roots[0].Data) > 0, 'caught error refused a snapshot: ' + Roots[0].Refusal);
  finally
    Engine.Free;
  end;

  Engine := TGocciaEngine.Create;
  try
    Engine.TestMode := True;
    Engine.HeapSnapshots := True;
    Check(Engine.PrepareHost(ErrorText), 'could not prepare restore host: ' + ErrorText);
    Check(InstallTestingAPI(ErrorText), 'could not prepare restore observer: ' + ErrorText);
    Check(Engine.HeapSnapshotVerify(Roots[0], AEntry, Source, ErrorText) = ghvPassed,
      'caught error did not round-trip: ' + ErrorText);
    Check(SnapshotRestore(Roots[0].Data, Restored, ErrorText), 'could not restore the caught error: ' + ErrorText);
    PushRoot(Restored);
    try
      Check(ErrorSourceLocation(RootAt(TempTop - 1), SourcePath, SourceLine, SourceColumn),
        'restored error did not retain its declaration location');
      Check((SourcePath = AImportPath) and (SourceLine = 2) and (SourceColumn = 1),
        'restored error has the wrong declaration location');
    finally
      PopRoots(1);
    end;
  finally
    Engine.Free;
  end;
end;

procedure CheckCompanionIsolation(const AEntry, ACompanion: UnicodeString);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.SourceKind := gskModule;
    RunResult := Engine.Run(ACompanion, 'export const v = 23;');
    Check(RunResult.Ok, 'regular module did not load: ' + RunResult.ErrorMessage);
    Engine.SourceKind := gskScript;
    RunResult := Engine.Run(AEntry, '  import "./absent.mjs";');
    Check(not RunResult.Ok and (RunResult.ErrorFile = AEntry) and (RunResult.ErrorLine = 1) and
      (RunResult.ErrorColumn = 3), 'earlier module changed the Script diagnostic: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
  end;

  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskScript;
    RunResult := Engine.Run(AEntry, 'import { present } from "./dep.mjs"; present;');
    Check(RunResult.Ok and (RunResult.ResultText = '1'), 'Script imports did not run: ' + RunResult.ErrorMessage);
    Engine.SourceKind := gskModule;
    RunResult := Engine.Run(ACompanion, 'export const v = 23; v;');
    Check(RunResult.Ok and (RunResult.ResultText = '23'), 'Script companion displaced a later regular module');
  finally
    Engine.Free;
  end;
end;

procedure CheckDuplicateScriptBinding(const AEntry: UnicodeString);
const
  Source = 'import { present, present } from "./dep.mjs"; present;';
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Bytes: TBytes;
  Artifact: RawByteString;
begin
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    Engine.SourceKind := gskScript;
    RunResult := Engine.Run(AEntry, Source);
    Check(RunResult.Ok and (RunResult.ResultText = '1'), 'duplicate Script import changed: ' + RunResult.ErrorMessage);
    Check(Engine.CompileArtifact(AEntry, Source, Bytes, RunResult), 'duplicate Script import did not compile');
  finally
    Engine.Free;
  end;
  SetString(Artifact, PAnsiChar(@Bytes[0]), Length(Bytes));
  Engine := TGocciaEngine.Create;
  try
    Engine.ResultForm := grfString;
    RunResult := Engine.RunArtifact(ChangeFileExt(AEntry, '.gbc'), Artifact);
    Check(RunResult.Ok and (RunResult.ResultText = '1'), 'artifact duplicate Script import changed');
    Engine.SourceKind := gskModule;
    RunResult := Engine.Run(ChangeFileExt(AEntry, '.module.mjs'), Source);
    Check(not RunResult.Ok and (RunResult.ErrorName = 'SyntaxError'), 'ordinary Module accepted a duplicate binding');
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
    Dependency.Text := 'import "./absent.mjs";' + LineEnding;
    Dependency.SaveToFile(Directory + 'load-failure.mjs');
    Dependency.Text := 'export const v = 23;' + LineEnding;
    Dependency.SaveToFile(Directory + 'collides.mjs#script-imports');
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
  CheckCase(Directory + 'script-combined.mjs#script-imports', MissingImport, 1, 1);
  CheckCase(Directory + 'module.jsx', 'const x = <x/>; ' + MissingImport, 1, 17);
  CheckCase(Directory + 'script.jsx', 'const x = <x/>; ' + MissingImport, 1, 17, gskScript);
  CheckDynamic(Directory + 'dynamic-entry.mjs', Directory + 'dynamic.mjs');
  CheckLinkFailurePrecedence(Directory + 'precedence.mjs', Directory + 'dynamic.mjs');
  CheckSnapshot(Directory + 'snapshot.mjs', Directory + 'dynamic.mjs');
  CheckCompanionIsolation(Directory + 'collides.mjs', Directory + 'collides.mjs#script-imports');
  CheckDuplicateScriptBinding(Directory + 'repeated.mjs');
  CheckCase(Directory + 'bom.mjs', #$FEFF + MissingImport, 1, 1);
  CheckInvalidLocation(Entry, 1, -1);
  CheckInvalidLocation(Entry, 0, 0);
  WriteLn('Artifact and source link locations: passed');
end.
