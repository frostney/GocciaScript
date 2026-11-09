unit G2.Snapshot;
{ Compiled JS preludes embedded in the binary.

  Parsing and compiling the builtins written in JS (G2.Builtins4, G2.Promise)
  was half of starting an empty script. build.sh compiles them once with the
  freshly built engine (G2_SNAPSHOT_OUT), writes the templates to
  G2.PreludeSnapshot.inc and rebuilds; at startup CompilePrelude loads a
  template from there instead of parsing.

  A snapshot entry is used only if the prelude's source still hashes to the
  value stored with it; otherwise the prelude is parsed as before. The format
  holds what a template's code depends on: constants are numbers and the
  like (stored as their bits) or strings (stored as text and interned again),
  and IC keys are atoms. Anything else makes the writer give up on that
  prelude, which is then parsed at startup. }
{$mode delphiunicode}{$H+}
{$POINTERMATH ON}

interface

uses
  SysUtils, Classes, G2.Core, G2.Bytecode;

function CompilePrelude(const Name, Source: UnicodeString; var T: PFuncTemplate; var Err: UnicodeString): Boolean;

implementation

uses
  BigInteger, G2.BigIntV, G2.Eval;

{$I G2.PreludeSnapshot.inc}

const
  SNAP_MAGIC = $31533247;   { 'G2S1' }

{ identifies the prelude source a snapshot entry was made from: its length and about 4096 of its characters,
  evenly spaced (the build makes the snapshot from the same sources; startup hashes every prelude) }
function SourceHash(const S: UnicodeString): QWord;
var I, Stride: Integer;
begin
  {$PUSH}{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}
  Result := 1469598103934665603 xor QWord(Length(S));
  Stride := Length(S) div 4096 + 1;
  I := 1;
  while I <= Length(S) do
  begin
    Result := (Result xor Ord(S[I])) * 1099511628211;
    Inc(I, Stride);
  end;
  {$POP}
end;

{ ---------------- writing ---------------- }

type
  ESnapshotUnsupported = class(Exception);

procedure WriteI32(S: TStream; V: Integer); begin S.WriteBuffer(V, 4); end;
procedure WriteU64(S: TStream; V: QWord); begin S.WriteBuffer(V, 8); end;
procedure WriteU8(S: TStream; V: Byte); begin S.WriteBuffer(V, 1); end;

procedure WriteText(S: TStream; const U: UnicodeString);
begin
  WriteI32(S, Length(U));
  if Length(U) > 0 then S.WriteBuffer(U[1], Length(U) * 2);
end;

procedure WriteStr(S: TStream; P: PStr);
begin
  if P = nil then WriteI32(S, -1) else WriteText(S, StrToUnicode(P));
end;

procedure WriteTemplate(S: TStream; T: PFuncTemplate);
var I: Integer; V: TValue; Flags: Byte;
begin
  WriteStr(S, T^.Name);
  WriteI32(S, T^.CodeLen); WriteI32(S, T^.EntryPC); WriteI32(S, T^.NumParams); WriteI32(S, T^.MaxRegs);
  WriteI32(S, T^.RestReg); WriteI32(S, T^.ArgsReg); WriteI32(S, T^.HomeReg); WriteI32(S, T^.NewTargetReg);
  WriteI32(S, T^.ExpectedProps); WriteI32(S, T^.Length); WriteI32(S, T^.AsyncReg);
  Flags := Ord(T^.IsArrow) or Ord(T^.IsClassCtor) shl 1 or Ord(T^.IsDerived) shl 2 or Ord(T^.IsMethod) shl 3 or
    Ord(T^.IsGenerator) shl 4 or Ord(T^.IsAsync) shl 5 or Ord(T^.IsStrict) shl 6 or Ord(T^.SimpleParams) shl 7;
  WriteU8(S, Flags);
  for I := 0 to T^.CodeLen - 1 do WriteU64(S, T^.Code[I]);
  WriteI32(S, Length(T^.Lines));
  for I := 0 to High(T^.Lines) do WriteI32(S, T^.Lines[I]);
  WriteI32(S, Length(T^.Consts));
  for I := 0 to High(T^.Consts) do
  begin
    V := T^.Consts[I];
    if not IsPtr(V) then begin WriteU8(S, 0); WriteU64(S, V); end
    else if IsString(V) then begin WriteU8(S, 1); WriteStr(S, PStr(PtrOf(V))); end
    else if IsBigInt(V) then begin WriteU8(S, 2); WriteText(S, BigToString(V)); end
    else raise ESnapshotUnsupported.Create('constant of a kind the snapshot cannot hold');
  end;
  WriteI32(S, Length(T^.Upvals));
  for I := 0 to High(T^.Upvals) do begin WriteU8(S, Ord(T^.Upvals[I].FromParentLocal)); WriteI32(S, T^.Upvals[I].Index); end;
  WriteI32(S, Length(T^.ICs));
  for I := 0 to High(T^.ICs) do
  begin
    { a fresh IC holds its key and Slot -1 only }
    if (T^.ICs[I].Shape <> nil) or (T^.ICs[I].NewShape <> nil) or (T^.ICs[I].PolyCount <> 0) or (T^.ICs[I].Depth <> 0) then
      raise ESnapshotUnsupported.Create('IC filled at compile time');
    WriteStr(S, T^.ICs[I].Key);
    WriteI32(S, T^.ICs[I].Slot);
  end;
  WriteI32(S, Length(T^.Children));
  for I := 0 to High(T^.Children) do WriteTemplate(S, T^.Children[I]);
  WriteU8(S, Ord(T^.FieldInit <> nil));
  if T^.FieldInit <> nil then WriteTemplate(S, T^.FieldInit);
  WriteText(S, T^.SourceName);
  WriteText(S, T^.SourceRange.Get);
end;

var SnapshotStarted: Boolean = False;

{ G2_SNAPSHOT_OUT names the file the entries are appended to (build.sh turns it into the include) }
procedure AppendEntry(const Name, Source: UnicodeString; T: PFuncTemplate);
var Path: string; F: TFileStream; M: TMemoryStream;
begin
  Path := GetEnvironmentVariable('G2_SNAPSHOT_OUT');
  if Path = '' then Exit;
  M := TMemoryStream.Create;
  try
    try
      WriteTemplate(M, T);
    except
      on E: ESnapshotUnsupported do
      begin
        WriteLn(ErrOutput, 'snapshot: ', Name, ' left out: ', E.Message);
        Exit;
      end;
    end;
    { the first entry of a run starts the file afresh }
    if SnapshotStarted then F := TFileStream.Create(Path, fmOpenReadWrite) else
    begin
      SnapshotStarted := True;
      F := TFileStream.Create(Path, fmCreate);
      WriteI32(F, SNAP_MAGIC);
    end;
    try
      F.Seek(0, soEnd);
      WriteText(F, Name);
      WriteU64(F, SourceHash(Source));
      WriteI32(F, M.Size);
      F.CopyFrom(M, 0);
    finally
      F.Free;
    end;
  finally
    M.Free;
  end;
end;

{ ---------------- reading ---------------- }

type
  TReader = record
    P, Limit: PByte;
    function I32: Integer;
    function U64: QWord;
    function U8: Byte;
    function Text: UnicodeString;
    function Str: PStr;
  end;

function TReader.I32: Integer; begin Move(P^, Result, 4); Inc(P, 4); end;
function TReader.U64: QWord; begin Move(P^, Result, 8); Inc(P, 8); end;
function TReader.U8: Byte; begin Result := P^; Inc(P); end;

function TReader.Text: UnicodeString;
var N: Integer;
begin
  N := I32;
  if N <= 0 then Exit('');
  SetLength(Result, N);
  Move(P^, Result[1], N * 2);
  Inc(P, N * 2);
end;

function TReader.Str: PStr;
var N: Integer; U: UnicodeString;
begin
  N := I32;
  if N < 0 then Exit(nil);
  SetLength(U, N);
  if N > 0 then Move(P^, U[1], N * 2);
  Inc(P, N * 2);
  Result := AtomOf(U);
end;

function ReadTemplate(var R: TReader): PFuncTemplate;
var T: PFuncTemplate; I, N: Integer; Flags: Byte;
begin
  New(T);
  RegisterTemplate(T);
  T^.Name := R.Str;
  T^.CodeLen := R.I32; T^.EntryPC := R.I32; T^.NumParams := R.I32; T^.MaxRegs := R.I32;
  T^.RestReg := R.I32; T^.ArgsReg := R.I32; T^.HomeReg := R.I32; T^.NewTargetReg := R.I32;
  T^.ExpectedProps := R.I32; T^.Length := R.I32; T^.AsyncReg := R.I32;
  T^.GdiKind := 0; T^.GdiLexBase := 0;   { prelude templates are never Script tops }
  Flags := R.U8;
  T^.IsArrow := Flags and 1 <> 0; T^.IsClassCtor := Flags and 2 <> 0; T^.IsDerived := Flags and 4 <> 0;
  T^.IsMethod := Flags and 8 <> 0; T^.IsGenerator := Flags and 16 <> 0; T^.IsAsync := Flags and 32 <> 0;
  T^.IsStrict := Flags and 64 <> 0; T^.SimpleParams := Flags and 128 <> 0;
  SetLength(T^.Code, T^.CodeLen);
  for I := 0 to T^.CodeLen - 1 do T^.Code[I] := R.U64;
  SetLength(T^.Lines, R.I32);
  for I := 0 to High(T^.Lines) do T^.Lines[I] := R.I32;
  SetLength(T^.Consts, R.I32);
  for I := 0 to High(T^.Consts) do
    case R.U8 of
      0: T^.Consts[I] := R.U64;
      1: T^.Consts[I] := MkPtr(R.Str);
    else T^.Consts[I] := BigToValue(TBigInteger.FromDecimalString(string(StrToUnicode(R.Str))), True);   { a BigInt literal }
    end;
  SetLength(T^.Upvals, R.I32);
  for I := 0 to High(T^.Upvals) do begin T^.Upvals[I].FromParentLocal := R.U8 <> 0; T^.Upvals[I].Index := R.I32; end;
  N := R.I32;
  SetLength(T^.ICs, N);
  for I := 0 to N - 1 do
  begin
    FillChar(T^.ICs[I], SizeOf(TPropIC), 0);
    T^.ICs[I].Key := R.Str;
    T^.ICs[I].Slot := R.I32;
  end;
  SetLength(T^.Children, R.I32);
  for I := 0 to High(T^.Children) do T^.Children[I] := ReadTemplate(R);
  if R.U8 <> 0 then T^.FieldInit := ReadTemplate(R) else T^.FieldInit := nil;
  T^.SourceName := R.Text;
  T^.SourceRange.SetText(R.Text);
  Result := T;
end;

function FromSnapshot(const Name, Source: UnicodeString; var T: PFuncTemplate): Boolean;
var R: TReader; EntryName: UnicodeString; Hash: QWord; Len: Integer;
begin
  Result := False;
  if (G2PreludeSnapshotSize < 4) or (GetEnvironmentVariable('G2_NO_SNAPSHOT') <> '') then Exit;
  R.P := @G2PreludeSnapshot[0];
  R.Limit := R.P + G2PreludeSnapshotSize;
  if R.I32 <> SNAP_MAGIC then Exit;
  while R.P < R.Limit do
  begin
    EntryName := R.Text;
    Hash := R.U64;
    Len := R.I32;
    if EntryName = Name then
    begin
      if Hash <> SourceHash(Source) then Exit;   { the prelude changed since the snapshot: parse it }
      T := ReadTemplate(R);
      Exit(True);
    end;
    Inc(R.P, Len);
  end;
end;

function CompilePrelude(const Name, Source: UnicodeString; var T: PFuncTemplate; var Err: UnicodeString): Boolean;
begin
  if FromSnapshot(Name, Source, T) then Exit(True);
  Result := ParseAndCompile(Source, Name, True, T, Err);
  if Result then AppendEntry(Name, Source, T);
end;

end.
