program ScriptArtifactImportsTest;

{ A Script run from a bytecode file reached through a symlink, with no source beside it, resolves import() from
  the real file's directory: during its run and from a closure a later Run on the same engine calls (ADR 0196).
  The fixture directory (script-artifact-imports-test.sh) holds real/sib.mjs, link/sib.mjs and
  link/standalone.gbc, a symlink to real/standalone.gbc, which this program writes. }

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,
  Goccia.Embed;

procedure Check(ACondition: Boolean; const AMessage: UnicodeString);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function Failure(const R: TGocciaRunResult): UnicodeString;
begin
  Result := R.ErrorMessage;
end;

procedure WriteArtifact(const APath: string; const ABytes: TBytes);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(ABytes) > 0 then Stream.WriteBuffer(ABytes[0], Length(ABytes));
  finally
    Stream.Free;
  end;
end;

function ArtifactText(const ABytes: TBytes): RawByteString;
begin
  SetLength(Result, Length(ABytes));
  if Length(ABytes) > 0 then Move(ABytes[0], Result[1], Length(ABytes));
end;

procedure CheckImportDirectory(const ADir: string);
var
  Engine: TGocciaEngine;
  R: TGocciaRunResult;
  Bytes: TBytes;
  ErrorText: UnicodeString;
begin
  Engine := TGocciaEngine.Create;
  try
    Check(Engine.PrepareHost(ErrorText), 'host setup failed: ' + ErrorText);
    Engine.SourceKind := gskScript;
    R := Default(TGocciaRunResult);
    Check(Engine.CompileArtifact(ADir + '/real/standalone.js',
      'globalThis.load = () => import("./sib.mjs"); load().then(m => { globalThis.during = m.who; });',
      Bytes, R), 'the Script did not compile: ' + Failure(R));
    WriteArtifact(ADir + '/real/standalone.gbc', Bytes);
    R := Engine.RunArtifact(ADir + '/link/standalone.gbc', ArtifactText(Bytes));
    Check(R.Ok, 'the bytecode Script failed: ' + Failure(R));
    R := Engine.Run(ADir + '/later.js', 'load().then(m => { globalThis.later = m.who; });');
    Check(R.Ok, 'the later run failed: ' + Failure(R));
    Engine.ResultForm := grfString;
    R := Engine.Run(ADir + '/check.js', 'during + " " + later;');
    Check(R.Ok and R.HasResult, 'the check failed: ' + Failure(R));
    Check(R.ResultText = 'real real', 'import() resolved from the link directory: ' + R.ResultText);
  finally
    Engine.Free;
  end;
end;

begin
  try
    Check(ParamCount = 1, 'usage: script-artifact-imports-test <fixture directory>');
    CheckImportDirectory(ExcludeTrailingPathDelimiter(ExpandFileName(ParamStr(1))));
    WriteLn('Script artifact imports resolve from the real directory across runs');
  except
    on E: Exception do
    begin
      WriteLn(ErrOutput, 'FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
