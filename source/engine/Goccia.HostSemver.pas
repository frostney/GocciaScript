unit Goccia.HostSemver;
{ goccia:semver's native half for the CLI host: one dispatcher, semver(op, ...), over Goccia.Semver (the
  engine-independent SemVer 2.0.0 core). Its answer is JSON text, which prelude/semver.js parses; a version or
  range the core refuses throws its TypeError or Error. Options travel as a bit set: 1 loose, 2 includePrerelease,
  4 rtl. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}

interface

uses
  Goccia.Core;

{ semver(op, ...): op names the core operation (see the case in the implementation) }
function Host_Semver(Args: PValue; Argc: Integer; var Res: TValue): Boolean;

implementation

uses
  SysUtils, Classes, Goccia.VM, Goccia.Semver;

function Arg(Args: PValue; Argc, I: Integer): UnicodeString;
var S: TValue;
begin
  Result := '';
  if (I <= Argc) and IsString(Args[I]) then Result := StrToUnicode(StrOf(Args[I]))
  else if (I <= Argc) and ToStringV(Args[I], S) and IsString(S) then Result := StrToUnicode(StrOf(S));
end;

function U8(const S: UnicodeString): string;
begin
  Result := string(UTF8Encode(S));
end;

function OptionsAt(Args: PValue; Argc, I: Integer): TGocciaSemverOptions;
var B: Integer;
begin
  Result := DefaultSemverOptions;
  if I > Argc then Exit;
  B := Round(PrimitiveToNumber(Args[I]));
  Result.Loose := (B and 1) <> 0;
  Result.IncludePrerelease := (B and 2) <> 0;
  Result.RTL := (B and 4) <> 0;
end;

function J(const S: string): string;
var I: Integer; C: Char;
begin
  Result := '"';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #9: Result := Result + '\t';
    else
      if Ord(C) < 32 then Result := Result + '\u' + IntToHex(Ord(C), 4) else Result := Result + C;
    end;
  end;
  Result := Result + '"';
end;

function JBool(B: Boolean): string;
begin
  if B then Result := 'true' else Result := 'false';
end;

function OptionsJSON(const O: TGocciaSemverOptions): string;
begin
  Result := '{"loose":' + JBool(O.Loose) + ',"includePrerelease":' + JBool(O.IncludePrerelease) + ',"rtl":' +
    JBool(O.RTL) + '}';
end;

function IdentifiersJSON(const A: TGocciaSemverIdentifierArray): string;
var I: Integer;
begin
  Result := '[';
  for I := 0 to High(A) do
  begin
    if I > 0 then Result := Result + ',';
    if A[I].Kind = sikNumeric then Result := Result + IntToStr(A[I].NumericValue) else Result := Result + J(A[I].TextValue);
  end;
  Result := Result + ']';
end;

function StringsJSON(const A: array of string): string;
var I: Integer;
begin
  Result := '[';
  for I := 0 to High(A) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + J(A[I]);
  end;
  Result := Result + ']';
end;

function SemverJSON(const V: TGocciaSemver): string;
begin
  Result := '{"options":' + OptionsJSON(V.Options) + ',"raw":' + J(V.Raw) + ',"version":' + J(V.Version) +
    ',"major":' + IntToStr(V.Major) + ',"minor":' + IntToStr(V.Minor) + ',"patch":' + IntToStr(V.Patch) +
    ',"prerelease":' + IdentifiersJSON(V.Prerelease) + ',"build":' + StringsJSON(V.Build) + '}';
end;

function ComparatorJSON(const C: TGocciaSemverComparator): string;
begin
  Result := '{"options":' + OptionsJSON(C.Semver.Options) + ',"operator":' + J(C.Operator) + ',"any":' +
    JBool(C.IsAny) + ',"semver":' + SemverJSON(C.Semver) + ',"value":' + J(C.Value) + '}';
end;

function RangeJSON(const R: TGocciaSemverRange): string;
var I, K: Integer;
begin
  Result := '{"options":' + OptionsJSON(R.Options) + ',"raw":' + J(R.Raw) + ',"range":' + J(RangeToString(R)) +
    ',"set":[';
  for I := 0 to High(R.SetOfComparators) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + '[';
    for K := 0 to High(R.SetOfComparators[I]) do
    begin
      if K > 0 then Result := Result + ',';
      Result := Result + ComparatorJSON(R.SetOfComparators[I][K]);
    end;
    Result := Result + ']';
  end;
  Result := Result + ']}';
end;

{ the JSON array of strings at argument I }
function ListAt(Args: PValue; Argc, I: Integer): TGocciaSemverStringArray;
var L: TStringList; S: string; P, Q: Integer;
begin
  SetLength(Result, 0);
  S := U8(Arg(Args, Argc, I));
  L := TStringList.Create;
  try
    { the prelude sends JSON.stringify of strings: split on the quoted items }
    P := 1;
    while P <= Length(S) do
    begin
      if S[P] = '"' then
      begin
        Q := P + 1;
        while (Q <= Length(S)) and (S[Q] <> '"') do
        begin
          if S[Q] = '\' then Inc(Q);
          Inc(Q);
        end;
        L.Add(StringReplace(StringReplace(Copy(S, P + 1, Q - P - 1), '\"', '"', [rfReplaceAll]), '\\', '\',
          [rfReplaceAll]));
        P := Q + 1;
      end
      else Inc(P);
    end;
    SetLength(Result, L.Count);
    for P := 0 to L.Count - 1 do Result[P] := L[P];
  finally
    L.Free;
  end;
end;

function Host_Semver(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Op, Out_: string; V: TGocciaSemver; C, D: TGocciaSemverComparator; R: TGocciaSemverRange;
  Ids: TGocciaSemverIdentifierArray; L: TGocciaSemverStringArray; M: TGocciaSemverStringMatrix; I, K: Integer;
  T: string; Dir: Integer;
begin
  Op := U8(Arg(Args, Argc, 1));
  try
    if Op = 'semver' then Out_ := SemverJSON(MustParseSemver(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'comparator' then Out_ := ComparatorJSON(ParseComparator(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'range' then Out_ := RangeJSON(MustParseRange(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'valid' then Out_ := J(Valid(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'clean' then Out_ := J(Clean(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'validRange' then Out_ := J(ValidRange(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'parse' then
    begin
      if ParseSemver(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3), V) then Out_ := SemverJSON(V) else Out_ := 'null';
    end
    else if Op = 'coerce' then
    begin
      if Coerce(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3), V) then Out_ := SemverJSON(V) else Out_ := 'null';
    end
    else if Op = 'minVersion' then
    begin
      T := MinVersion(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3));
      if (T <> '') and ParseSemver(T, OptionsAt(Args, Argc, 3), V) then Out_ := SemverJSON(V) else Out_ := 'null';
    end
    else if (Op = 'inc') or (Op = 'incStrict') then
    begin
      if TryIncrement(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 5), U8(Arg(Args, Argc, 4)),
        False, False, T) then Out_ := J(T)
      else if Op = 'incStrict' then Exit(ThrowError('Error', 'invalid increment'))
      else Out_ := 'null';
    end
    else if Op = 'prerelease' then
    begin
      if PrereleaseOf(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3), Ids) then Out_ := IdentifiersJSON(Ids)
      else Out_ := 'null';
    end
    else if Op = 'major' then Out_ := IntToStr(MajorOf(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'minor' then Out_ := IntToStr(MinorOf(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'patch' then Out_ := IntToStr(PatchOf(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3)))
    else if Op = 'compare' then
      Out_ := IntToStr(Compare(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'compareBuild' then
      Out_ := IntToStr(CompareBuild(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'compareLoose' then Out_ := IntToStr(CompareLoose(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3))))
    else if Op = 'diff' then Out_ := J(Diff(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'cmp' then
      Out_ := JBool(Cmp(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), U8(Arg(Args, Argc, 4)), OptionsAt(Args, Argc, 5)))
    else if Op = 'satisfies' then
      Out_ := JBool(Satisfies(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'intersects' then
      Out_ := JBool(RangeIntersects(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'gtr' then
      Out_ := JBool(GreaterThanRange(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'ltr' then
      Out_ := JBool(LessThanRange(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'outside' then
      Out_ := JBool(Outside(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), U8(Arg(Args, Argc, 4)),
        OptionsAt(Args, Argc, 5)))
    else if Op = 'subset' then
      Out_ := JBool(IsSubset(U8(Arg(Args, Argc, 2)), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'toComparators' then
    begin
      M := ToComparators(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3));
      Out_ := '[';
      for I := 0 to High(M) do
      begin
        if I > 0 then Out_ := Out_ + ',';
        Out_ := Out_ + StringsJSON(M[I]);
      end;
      Out_ := Out_ + ']';
    end
    else if Op = 'maxSatisfying' then
      Out_ := J(MaxSatisfying(ListAt(Args, Argc, 2), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'minSatisfying' then
      Out_ := J(MinSatisfying(ListAt(Args, Argc, 2), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'simplifyRange' then
      Out_ := J(SimplifyRange(ListAt(Args, Argc, 2), U8(Arg(Args, Argc, 3)), OptionsAt(Args, Argc, 4)))
    else if Op = 'sort' then
    begin
      L := ListAt(Args, Argc, 2);
      Dir := Round(PrimitiveToNumber(Args[4]));
      for I := 0 to High(L) do
        for K := I + 1 to High(L) do
          if CompareBuild(L[I], L[K], OptionsAt(Args, Argc, 3)) * Dir > 0 then
          begin
            T := L[I]; L[I] := L[K]; L[K] := T;
          end;
      Out_ := StringsJSON(L);
    end
    else if Op = 'comparatorTest' then
    begin
      C := ParseComparator(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3));
      V := MustParseSemver(U8(Arg(Args, Argc, 4)), C.Semver.Options);
      Out_ := JBool(Cmp(V.Version, C.Operator, C.Semver.Version, C.Semver.Options));
    end
    else if Op = 'comparatorIntersects' then
    begin
      C := ParseComparator(U8(Arg(Args, Argc, 2)), OptionsAt(Args, Argc, 3));
      D := ParseComparator(U8(Arg(Args, Argc, 4)), OptionsAt(Args, Argc, 5));
      Out_ := JBool(ComparatorIntersects(C, D, OptionsAt(Args, Argc, 6)));
    end
    else Exit(ThrowTypeError('semver: unknown operation ' + UnicodeString(Op)));
  except
    on E: EGocciaSemverTypeError do Exit(ThrowTypeError(UTF8Decode(E.Message)));
    on E: EGocciaSemverError do Exit(ThrowError('Error', UTF8Decode(E.Message)));
  end;
  Res := StrFromUnicode(UTF8Decode(Out_));
  Result := True;
end;

end.
