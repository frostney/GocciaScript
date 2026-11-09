unit G2.Temporal;
{ Temporal: the spec's operations and objects are written in JS (prelude/temporal.js -> G2.Temporal.inc) and
  snapshotted; this unit gives the prelude what JS cannot reach: the clock, the time zone database and the system
  time zone. Zones come as TZif files from GOCCIA_TZDIR or /usr/share/zoneinfo, the zone and link names from that
  directory's tzdata.zi, as in Goccia (ADR 0034); the prelude parses them and caches each zone. }
{$mode delphiunicode}{$H+}

interface

procedure InitTemporal;

implementation

uses
  {$IFDEF UNIX}BaseUnix, Unix, Linux, UnixType, {$ENDIF}SysUtils, Classes, BigInteger, G2.Core, G2.Bytecode, G2.VM, G2.BigIntV,
  G2.Snapshot;

{$I G2.Temporal.inc}

function ZoneDir: string;
begin
  Result := GetEnvironmentVariable('GOCCIA_TZDIR');
  if Result = '' then Result := '/usr/share/zoneinfo';
  Result := IncludeTrailingPathDelimiter(Result);
end;

{ the file's bytes as a string of code units 0..255, or undefined when it cannot be read }
function FileBytes(const Path: string): TValue;
var F: TFileStream; B: TBytes; S: UnicodeString; I: Integer;
begin
  Result := VUndefined;
  if not FileExists(Path) then Exit;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    try
      SetLength(B, F.Size);
      if Length(B) > 0 then F.ReadBuffer(B[0], Length(B));
    finally
      F.Free;
    end;
  except
    Exit;
  end;
  SetLength(S, Length(B));
  for I := 0 to High(B) do S[I + 1] := WideChar(B[I]);
  Result := StrFromUnicode(S);
end;

{ nowNs(): the current time, nanoseconds since the epoch, as a BigInt }
function Temporal_NowNs(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var TS: TTimeSpec; B: TBigInteger;
begin
  clock_gettime(CLOCK_REALTIME, @TS);
  B := TBigInteger.FromInt64(Int64(TS.tv_sec)).Multiply(TBigInteger.FromInt64(1000000000)).Add(
    TBigInteger.FromInt64(Int64(TS.tv_nsec)));
  Res := BigToValue(B);
  Result := True;
end;

{ tzData(name): the zone's TZif file; only names made of the characters of IANA zone names, so no path can
  leave the zone directory }
function Temporal_TzData(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Name: UnicodeString; I: Integer; C: WideChar;
begin
  Res := VUndefined;
  Result := True;
  if (Argc < 1) or not IsString(Args[1]) then Exit;
  Name := StrToUnicode(StrOf(Args[1]));
  if (Name = '') or (Length(Name) > 256) or (Name[1] = '/') or (Pos('..', Name) > 0) then Exit;
  for I := 1 to Length(Name) do
  begin
    C := Name[I];
    if not (CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '/', '_', '-', '+', '.'])) then Exit;
  end;
  Res := FileBytes(ZoneDir + string(Name));
end;

{ tzIndex(): the zone directory's tzdata.zi (its Z and L lines name every zone and link) }
function Temporal_TzIndex(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := FileBytes(ZoneDir + 'tzdata.zi');
  Result := True;
end;

{ systemTZ(): TZ, else the zone /etc/localtime links to, else undefined }
function Temporal_SystemTZ(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: string; P: Integer;
begin
  Res := VUndefined;
  Result := True;
  S := GetEnvironmentVariable('TZ');
  if (S <> '') and (S[1] = ':') then Delete(S, 1, 1);
  if S = '' then
  begin
    {$IFDEF UNIX}
    S := fpReadLink('/etc/localtime');
    {$ENDIF}
    P := Pos('zoneinfo/', S);
    if P > 0 then S := Copy(S, P + Length('zoneinfo/'), MaxInt) else S := '';
  end;
  if S <> '' then Res := StrFromUnicode(UnicodeString(S));
end;

procedure InitTemporal;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R: TValue; CA: array[0..5] of TValue; I: Integer; Ok: Boolean;
begin
  if not CompilePrelude('<temporal>', TemporalSrc, T, Err) then raise Exception.Create('temporal prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('temporal prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('temporal prelude run');
  PushRoot(Fn);
  { each native stays rooted while the next one is allocated }
  PushRoot(NewNativeFunction(Temporal_NowNs, 'nowNs', 0));
  PushRoot(NewNativeFunction(Temporal_TzData, 'tzData', 1));
  PushRoot(NewNativeFunction(Temporal_TzIndex, 'tzIndex', 0));
  PushRoot(NewNativeFunction(Temporal_SystemTZ, 'systemTZ', 0));
  PushRoot(NewNativeFunction(MarkIntrinsicClass, 'markIntrinsic', 1));
  CA[0] := GlobalObj;
  for I := 1 to 5 do CA[I] := RootAt(TempTop - 6 + I);
  Ok := CallValue(RootAt(TempTop - 6), VUndefined, @CA[0], 6, R);
  PopRoots(5);
  if not Ok then
  begin
    PopRoots(1);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('temporal prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('temporal prelude call');
  end;
  PopRoots(1);
end;

end.
