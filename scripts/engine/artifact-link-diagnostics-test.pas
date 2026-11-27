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
  ALine, AColumn: Integer);
begin
  Check(not AResult.Ok and (AResult.ErrorName = 'SyntaxError') and
    (Pos('has no export named "missing"', AResult.ErrorText) > 0), AMode + ': ' + AResult.ErrorMessage);
  Check((AResult.ErrorFile = APath) and (AResult.ErrorLine = ALine) and (AResult.ErrorColumn = AColumn),
    AMode + ': expected ' + APath + ':' + IntToStr(ALine) + ':' + IntToStr(AColumn) + ', got ' +
    AResult.ErrorFile + ':' + IntToStr(AResult.ErrorLine) + ':' + IntToStr(AResult.ErrorColumn));
end;

procedure CheckCase(const APath, ASource: UnicodeString; ALine, AColumn: Integer);
var
  Engine: TGocciaEngine;
  RunResult: TGocciaRunResult;
  Bytes: TBytes;
  Bytecode: RawByteString;
begin
  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.Run(APath, ASource);
    CheckError('source', APath, RunResult, ALine, AColumn);
  finally
    Engine.Free;
  end;

  Engine := TGocciaEngine.Create;
  try
    Check(Engine.CompileArtifact(APath, ASource, Bytes, RunResult), 'could not compile: ' + RunResult.ErrorMessage);
  finally
    Engine.Free;
  end;
  SetString(Bytecode, PAnsiChar(@Bytes[0]), Length(Bytes));

  Engine := TGocciaEngine.Create;
  try
    RunResult := Engine.RunArtifact(ChangeFileExt(APath, '.gbc'), Bytecode);
    CheckError('artifact', APath, RunResult, ALine, AColumn);
  finally
    Engine.Free;
  end;
end;

procedure CheckInvalidLocation(const APath: UnicodeString);
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
      Artifact.Info.Imports[0].SourceColumn := -1;
      InvalidBytes := SaveArtifact(Artifact);
    finally
      Artifact.Info.Free;
    end;
    if LoadArtifact(@InvalidBytes[0], Length(InvalidBytes), APath, 0, InvalidArtifact, ErrorText) then
    begin
      InvalidArtifact.Info.Free;
      raise Exception.Create('a negative declaration column was accepted');
    end;
    Check(Pos('invalid module declaration location', ErrorText) > 0, 'wrong verifier error: ' + ErrorText);
  finally
    Engine.Free;
  end;
end;

var
  Directory, Entry: UnicodeString;
  Dependency: TStringList;
begin
  Directory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
  Dependency := TStringList.Create;
  try
    Dependency.Add('export const present = 1;');
    Dependency.SaveToFile(Directory + 'dep.mjs');
  finally
    Dependency.Free;
  end;

  Entry := Directory + 'entry.mjs';
  CheckCase(Entry, '  import { present } from "./dep.mjs";'#10'import { missing } from "./dep.mjs";', 2, 1);
  CheckCase(Entry, '  import { missing } from "./dep.mjs";'#10'import { present } from "./dep.mjs";', 1, 3);
  CheckCase(Entry, ' export { present } from "./dep.mjs";'#10'   export { missing } from "./dep.mjs";', 2, 4);
  CheckCase(Entry, ' export { missing } from "./dep.mjs";'#10'   export { present } from "./dep.mjs";', 1, 2);
  CheckInvalidLocation(Entry);
  WriteLn('Artifact and source link locations: passed');
end.
