program g2threads;
{ Runs one script on N threads at once, each in an engine of its own, for R rounds, and checks that every
  thread printed what the first one did. Usage: g2threads THREADS ROUNDS FILE }
{$mode delphiunicode}{$H+}

uses
  Classes,
  SysUtils,

  cthreads,
  G2.Builtins,
  G2.Engine;

type
  TRunner = class(TThread)
  public
    Source, FileName: UnicodeString;
    Lines: TStringList;
    Err: UnicodeString;
    Ok: Boolean;
    constructor Create(const ASource, AFileName: UnicodeString);
    destructor Destroy; override;
  protected
    procedure Execute; override;
  end;

threadvar
  CurrentLines: TStringList;

{ benchmark timings differ between threads: only the other lines are compared }
procedure CollectLine(const ALine: UnicodeString);
begin
  if (Pos('runs/s', ALine) = 0) and (Pos('Geometric mean', ALine) = 0) then CurrentLines.Add(UTF8Encode(ALine));
end;

constructor TRunner.Create(const ASource, AFileName: UnicodeString);
begin
  Source := ASource;
  FileName := AFileName;
  Lines := TStringList.Create;
  inherited Create(True);
end;

destructor TRunner.Destroy;
begin
  Lines.Free;
  inherited;
end;

procedure TRunner.Execute;
begin
  CurrentLines := Lines;
  ConsoleSink := @CollectLine;
  try
    EngineInit;
    try
      Ok := EngineRunScript(Source, FileName, Err);
    finally
      EngineDone;
    end;
  except
    on E: Exception do begin Ok := False; Err := UnicodeString(E.ClassName + ': ' + E.Message); end;
  end;
end;

function ReadSource(const APath: string): UnicodeString;
var F: TFileStream; Raw: RawByteString;
begin
  F := TFileStream.Create(APath, fmOpenRead);
  try
    SetLength(Raw, F.Size);
    if F.Size > 0 then F.ReadBuffer(Raw[1], F.Size);
  finally
    F.Free;
  end;
  Result := UTF8Decode(Raw);
end;

function RSSKB: Int64;
var S: TStringList; I: Integer;
begin
  Result := -1;
  S := TStringList.Create;
  try
    S.LoadFromFile('/proc/self/status');
    for I := 0 to S.Count - 1 do
      if S[I].StartsWith('VmRSS:') then Exit(StrToInt64(Trim(Copy(S[I], 7, Length(S[I]) - 9))));
  finally
    S.Free;
  end;
end;

var
  N, R, I, Round, Bad: Integer;
  Src: UnicodeString;
  Runners: array of TRunner;
  T0: QWord;
begin
  N := StrToInt(ParamStr(1));
  R := StrToInt(ParamStr(2));
  Src := ReadSource(ParamStr(3));
  Bad := 0;
  for Round := 1 to R do
  begin
    T0 := GetTickCount64;
    SetLength(Runners, N);
    for I := 0 to N - 1 do Runners[I] := TRunner.Create(Src, UnicodeString(ParamStr(3)));
    for I := 0 to N - 1 do Runners[I].Start;
    for I := 0 to N - 1 do Runners[I].WaitFor;
    for I := 0 to N - 1 do
    begin
      if not Runners[I].Ok then begin WriteLn('round ', Round, ' thread ', I, ' failed: ', UTF8Encode(Runners[I].Err)); Inc(Bad); end
      else if Runners[I].Lines.Text <> Runners[0].Lines.Text then
      begin
        WriteLn('round ', Round, ' thread ', I, ' printed something else than thread 0');
        Inc(Bad);
      end;
    end;
    WriteLn('round ', Round, ': ', N, ' threads in ', GetTickCount64 - T0, ' ms, RSS ', RSSKB, ' KB; thread 0 printed ',
      Runners[0].Lines.Count, ' lines');
    for I := 0 to N - 1 do
      if Runners[I].Lines.Count > 0 then
        WriteLn('  thread ', I, ': ', Copy(Runners[I].Lines.Text, Pos('DIGEST', Runners[I].Lines.Text), 30).Split([#10])[0]);
    for I := 0 to N - 1 do Runners[I].Free;
  end;
  if Bad = 0 then WriteLn('all threads agree') else begin WriteLn(Bad, ' failures'); Halt(1); end;
end.
