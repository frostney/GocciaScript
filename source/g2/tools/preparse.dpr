program preparse;

{ Checks G2.PreParse (a development tool, not part of the G2 build).
    preparse --t262 DIR [PREFIX...]   every test262 test under DIR/test: rejected exactly when its front matter
                                      says negative phase parse; modes as scripts/g2/run262.py (a strict run
                                      puts "use strict"; and a newline in front)
    preparse [--module] FILE...       pre-parses each file and prints its error, if any
    preparse --bench [--module] FILE  times the pre-parser over the file (best of 5)
    preparse --list LISTFILE          per listed file "SM path": S / M the file pre-parses as a Script / Module,
                                      lowercase if not (as scripts' v8check.js prints for V8) }

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, G2.Core, G2.VM, G2.Engine, G2.Scan, G2.PreParse;

function ReadFileText(const FN: string): UnicodeString;
var FS: TFileStream; Raw: RawByteString;
begin
  FS := TFileStream.Create(FN, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Raw, FS.Size);
    if FS.Size > 0 then FS.ReadBuffer(Raw[1], FS.Size);
  finally
    FS.Free;
  end;
  if (Length(Raw) >= 3) and (Raw[1] = #$EF) and (Raw[2] = #$BB) and (Raw[3] = #$BF) then Delete(Raw, 1, 3);
  Result := UTF8Decode(Raw);
end;

var
  Files, Runs, FalseRejects, FalseAccepts: Integer;

procedure FindFiles(const Dir: string; L: TStringList);
var SR: TSearchRec;
begin
  if FindFirst(Dir + '/*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then FindFiles(Dir + '/' + SR.Name, L)
      else if (ExtractFileExt(SR.Name) = '.js') and (Pos('_FIXTURE', SR.Name) = 0) then L.Add(Dir + '/' + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure CheckOne(const Rel: string; const Src: UnicodeString; Module, Strict, ExpectReject: Boolean);
var Msg: string; Line, Col: Integer; Ok: Boolean; Text: UnicodeString; Mode: string;
begin
  Inc(Runs);
  if Strict and not Module then Text := '"use strict";'#10 + Src else Text := Src;
  if Module then Ok := PreParseSource(Text, pgModule, True, Msg, Line, Col)
  else Ok := PreParseSource(Text, pgScript, False, Msg, Line, Col);
  if Module then Mode := 'module' else if Strict then Mode := 'strict' else Mode := 'sloppy';
  if Ok and ExpectReject then
  begin
    Inc(FalseAccepts);
    WriteLn('ACCEPT ', Mode, ' ', Rel);
  end
  else if not Ok and not ExpectReject then
  begin
    Inc(FalseRejects);
    WriteLn('REJECT ', Mode, ' ', Rel, ' ', Line, ':', Col, ' ', Msg);
  end;
end;

procedure Test262(const Dir: string; Prefixes: TStringList);
var L: TStringList; I, J, A, B: Integer; Src: UnicodeString; Meta, Rel: string; Neg, Module, Raw, OnlyStrict,
  NoStrict, Keep: Boolean; Flags: string;
begin
  L := TStringList.Create;
  FindFiles(Dir + '/test', L);
  L.Sort;
  for I := 0 to L.Count - 1 do
  begin
    Rel := Copy(L[I], Length(Dir) + 7, MaxInt);
    if Prefixes.Count > 0 then
    begin
      Keep := False;
      for J := 0 to Prefixes.Count - 1 do
        if Copy(Rel, 1, Length(Prefixes[J])) = Prefixes[J] then Keep := True;
      if not Keep then Continue;
    end;
    Src := ReadFileText(L[I]);
    A := Pos('/*---', Src);
    B := Pos('---*/', Src);
    if (A > 0) and (B > A) then Meta := UTF8Encode(Copy(Src, A, B - A)) else Meta := '';
    Neg := (Pos('negative:', Meta) > 0) and (Pos('phase: parse', Meta) > 0);
    J := Pos('flags:', Meta);
    Flags := '';
    if J > 0 then
    begin
      Flags := Copy(Meta, J, MaxInt);
      if Pos('[', Copy(Flags, 1, Pos(#10, Flags))) > 0 then Flags := Copy(Flags, 1, Pos(#10, Flags))
      else
      begin
        { a YAML block list: the following lines that start with spaces and a dash }
        Flags := Copy(Flags, Pos(#10, Flags) + 1, MaxInt);
        A := 1;
        while (A < Length(Flags)) and ((Flags[A] = ' ') or (Flags[A] = '-')) do
        begin
          while (A <= Length(Flags)) and (Flags[A] <> #10) do Inc(A);
          Inc(A);
        end;
        Flags := Copy(Flags, 1, A);
      end;
    end;
    Module := Pos('module', Flags) > 0;
    Raw := Pos('raw', Flags) > 0;
    OnlyStrict := Pos('onlyStrict', Flags) > 0;
    NoStrict := Pos('noStrict', Flags) > 0;
    Inc(Files);
    if Module then CheckOne(Rel, Src, True, True, Neg)
    else if Raw or NoStrict then CheckOne(Rel, Src, False, False, Neg)
    else if OnlyStrict then CheckOne(Rel, Src, False, True, Neg)
    else
    begin
      CheckOne(Rel, Src, False, False, Neg);
      CheckOne(Rel, Src, False, True, Neg);
    end;
  end;
  L.Free;
  WriteLn(Format('files %d, runs %d, false rejects %d, false accepts %d', [Files, Runs, FalseRejects, FalseAccepts]));
end;

procedure Bench(const FN: string; Module: Boolean);
var Src: UnicodeString; R: Integer; T0, Best: QWord; P: TPreParser; G: TParseGoal;
begin
  Src := ReadFileText(FN);
  if Module then G := pgModule else G := pgScript;
  Best := High(QWord);
  for R := 1 to 5 do
  begin
    T0 := GetTickCount64;
    P := TPreParser.Create(PWideChar(Src), Length(Src), G, False);
    try
      P.ParseProgram;
    finally
      if R = 5 then WriteLn(Format('%s: %d functions, %d free names', [ExtractFileName(FN), P.FuncCount, P.FreeCount]));
      P.Free;
    end;
    Best := Min(Best, GetTickCount64 - T0);
  end;
  WriteLn(Format('%s: %d chars, pre-parse %d ms (best of 5)', [ExtractFileName(FN), Length(Src), Best]));
end;

var I: Integer; Module, DoBench: Boolean; Msg, Msg2, Tag: string; Src: UnicodeString; Line, Col: Integer; Prefixes: TStringList; G: TParseGoal;
begin
  EngineInit;
  PreParseRegExpCheck := RegExpValidateHook;
  Module := False;
  DoBench := False;
  if (ParamCount >= 2) and (ParamStr(1) = '--list') then
  begin
    Prefixes := TStringList.Create;
    Prefixes.LoadFromFile(ParamStr(2));
    for I := 0 to Prefixes.Count - 1 do
    begin
      if Prefixes[I] = '' then Continue;
      try
        Src := ReadFileText(Prefixes[I]);
      except
        Continue;
      end;
      if PreParseSource(Src, pgScript, False, Msg, Line, Col) then Tag := 'S' else Tag := 's';
      if PreParseSource(Src, pgModule, True, Msg2, Line, Col) then Tag := Tag + 'M' else Tag := Tag + 'm';
      WriteLn(Tag, ' ', Prefixes[I], ' | ', Msg, ' | ', Msg2);
    end;
    Exit;
  end;
  if (ParamCount >= 2) and (ParamStr(1) = '--t262') then
  begin
    Prefixes := TStringList.Create;
    for I := 3 to ParamCount do Prefixes.Add(ParamStr(I));
    Test262(ParamStr(2), Prefixes);
    Exit;
  end;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--module' then Module := True
    else if ParamStr(I) = '--bench' then DoBench := True
    else if DoBench then Bench(ParamStr(I), Module)
    else
    begin
      if Module then G := pgModule else G := pgScript;
      if PreParseSource(ReadFileText(ParamStr(I)), G, False, Msg, Line, Col) then WriteLn(ParamStr(I), ': ok')
      else WriteLn(ParamStr(I), ':', Line, ':', Col, ': ', Msg);
    end;
end.
