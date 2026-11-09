unit G2.RegExpV;
{ RegExp objects: patterns compile with Goccia's RegExp compiler (Goccia.RegExp.Compiler) and run on G2's matcher
  (G2.Regex), which reads G2 strings in place. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  SysUtils, Math, Generics.Collections, G2.Core, G2.VM;

const
  okRegExp = 17;

procedure InitRegExp;
{ the spec layer in prelude/regexp.js: after every other built-in }
procedure InitRegExpPrelude;
function CreateRegExp(Pattern, Flags: PStr; var Res: TValue): Boolean;

implementation

uses
  Goccia.RegExp.&Program, Goccia.RegExp.Compiler, Goccia.RegExp.Engine, G2.Regex, G2.Bytecode, G2.Snapshot;

{$I G2.RegExpPrelude.inc}

type
  PCompiled = ^TCompiled;
  TCompiled = record
    Prog: TRegExpProgram;
    Pattern, Flags: UnicodeString;
    Global, Sticky, IgnoreCase, Multiline, Unicode, DotAll, HasIndices: Boolean;
    M: TRegexMatcher;   { one per compiled pattern: a match leaves its captures here until the next one }
  end;

threadvar
  Compiled: TDictionary<UnicodeString, PCompiled>;
  RegExpProto, StrReplaceOrig, StrSplitOrig, StrReplaceAllOrig, Scratch: TValue;
  AtomLastIndex, AtomIndex, AtomInput, AtomGroups: PStr;
  RegExpShape: PShape;    { a RegExp object's shape while lastIndex is its only own property: it is slot 0 }
  MatchShape: PShape;     { an exec() result's shape with index, input and groups, once one has been built }

function Arg(Args: PValue; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

{ the compiled pattern (cached), or False with the SyntaxError's message in Msg }
function TryCompile(const Pattern, Flags: UnicodeString; var C: PCompiled; var Msg: UnicodeString): Boolean;
var Key: UnicodeString; P: TRegExpProgram;
begin
  Key := Flags + '/' + Pattern;
  if Compiled.TryGetValue(Key, C) then Exit(True);
  try
    ValidateRegExpFlags(Flags);
    P := CompileRegExp(Pattern, Flags);
  except
    on E: Exception do
    begin
      Msg := 'Invalid regular expression: /' + Pattern + '/' + Flags + ': ' + UnicodeString(E.Message);
      Exit(False);
    end;
  end;
  New(C);
  C^.Prog := P;
  C^.Pattern := Pattern;
  C^.Flags := Flags;
  C^.Global := Pos('g', Flags) > 0;
  C^.Sticky := Pos('y', Flags) > 0;
  C^.IgnoreCase := Pos('i', Flags) > 0;
  C^.Multiline := Pos('m', Flags) > 0;
  C^.Unicode := (Pos('u', Flags) > 0) or (Pos('v', Flags) > 0);
  C^.DotAll := Pos('s', Flags) > 0;
  C^.HasIndices := Pos('d', Flags) > 0;
  C^.M := TRegexMatcher.Create(P, Pattern);
  Compiled.Add(Key, C);
  Result := True;
end;

function GetCompiled(const Pattern, Flags: UnicodeString; var C: PCompiled): Boolean;
var Msg: UnicodeString;
begin
  Result := TryCompile(Pattern, Flags, C, Msg);
  if not Result then ThrowError('SyntaxError', Msg);
end;

{ RegExpAlloc: an object with the RegExp slots (Native nil until RegExpInitialize) and its lastIndex }
const
  ofLegacyOff = 32;   { a RegExp made with a new.target other than %RegExp%: no legacy static properties }

function AllocRegExp(Proto: TValue; Legacy: Boolean = True): TValue;
var O: PObj;
begin
  O := NewObject(RootShapeFor(Proto), 2, okRegExp);
  O^.Native := nil;
  if not Legacy then O^.OFlags := O^.OFlags or ofLegacyOff;
  AddProperty(O, AtomLastIndex, VUndefined, pfWritable);
  if Proto = RegExpProto then RegExpShape := O^.Shape;
  Result := MkPtr(O);
end;

{ 13.2.7.2 a literal's early errors: the pattern compiled now (and cached), or the error message }
function ValidateLiteral(const Pattern, Flags: UnicodeString): UnicodeString;
var C: PCompiled;
begin
  Result := '';
  if not TryCompile(Pattern, Flags, C, Result) then
  begin
    { an implementation limit (a repetition count Goccia's RegExp compiler cannot copy) is not a syntax error of
      the literal: it throws when the literal is evaluated }
    if Pos('quantifier bound too large', Result) > 0 then Result := '';
    Exit;
  end;
  Result := '';
end;

function NewRegExpObject(C: PCompiled): TValue;
begin
  Result := AllocRegExp(RegExpProto);
  ObjOf(Result)^.Native := C;
  ObjOf(Result)^.Slots[0] := MkInt(0);
end;

function CreateRegExp(Pattern, Flags: PStr; var Res: TValue): Boolean;
var C: PCompiled;
begin
  if not GetCompiled(StrToUnicode(Pattern), StrToUnicode(Flags), C) then Exit(False);
  Res := NewRegExpObject(C);
  Result := True;
end;

{ an initialized RegExp: [[RegExpMatcher]] set }
function IsRegExpV(V: TValue): Boolean; inline;
begin
  Result := IsObject(V) and (ObjOf(V)^.OKind = okRegExp) and (ObjOf(V)^.Native <> nil);
end;

function CompiledOf(V: TValue): PCompiled; inline;
begin
  Result := PCompiled(ObjOf(V)^.Native);
end;

{ ---- the legacy static properties (RegExp.$1-$9, input, lastMatch, lastParen, leftContext, rightContext) ---- }

type
  { on the heap, behind one threadvar pointer (FPC 3.3.1 native TLS: no constant offsets into a threadvar) }
  PLegacy = ^TLegacy;
  TLegacy = record
    MatchStr: TValue;   { the subject of the last match; undefined before any }
    InputStr: TValue;   { [[RegExpInput]]: the subject, or what was assigned to RegExp.input }
    Pos: array[0..21] of Integer;   { the match, $1-$9 and the last group as start/end pairs (-1: none) }
    State: Integer;     { 0 nothing matched yet, 1 valid, 2 invalidated by a RegExp without legacy features }
    N: Integer;         { the capture count: Pos holds groups 0..min(N, 9), and the last one at 20 when N > 9 }
  end;

threadvar
  RegExpCtorV: TValue;
  Legacy: PLegacy;

{ UpdateLegacyRegExpStaticProperties / InvalidateLegacyRegExpStaticProperties after a successful match }
procedure RecordLegacy(Re: TValue; Sc: TRegexMatcher; S: PStr);
var I, St, En, N, M: Integer; L: PLegacy;
begin
  L := Legacy;
  if (ObjOf(Re)^.OFlags and ofLegacyOff) <> 0 then
  begin
    L^.State := 2;
    L^.MatchStr := VUndefined;
    L^.InputStr := VUndefined;
    Exit;
  end;
  L^.State := 1;
  L^.MatchStr := MkPtr(S);
  L^.InputStr := MkPtr(S);
  N := Sc.CaptureCount;
  L^.N := N;
  if N > 9 then M := 9 else M := N;
  for I := 0 to M do
  begin
    if not Sc.TryGetGroup(I, St, En) then begin St := -1; En := -1; end;
    L^.Pos[I * 2] := St;
    L^.Pos[I * 2 + 1] := En;
  end;
  if N > 9 then
  begin
    if not Sc.TryGetGroup(N, St, En) then begin St := -1; En := -1; end;
    L^.Pos[20] := St;
    L^.Pos[21] := En;
  end;
end;

{ the getters, by NativeData: 0 input, 1 lastMatch, 2 lastParen, 3 leftContext, 4 rightContext, 5-13 $1-$9 }
function Legacy_Get(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var K, St, En: Integer; S: PStr;
begin
  if Args[0] <> RegExpCtorV then Exit(ThrowTypeError('RegExp legacy static property read through a receiver other than RegExp'));
  if Legacy^.State = 2 then Exit(ThrowTypeError('RegExp legacy static properties are not available after a subclass match'));
  K := NativeData;
  if K = 0 then
  begin
    if IsString(Legacy^.InputStr) then Res := Legacy^.InputStr else Res := MkPtr(AtomOf(''));
    Exit(True);
  end;
  if Legacy^.State = 0 then begin Res := MkPtr(AtomOf('')); Exit(True); end;
  S := StrOf(Legacy^.MatchStr);
  case K of
    1: begin St := Legacy^.Pos[0]; En := Legacy^.Pos[1]; end;
    2:
      if Legacy^.N = 0 then begin St := 0; En := 0; end
      else if Legacy^.N > 9 then begin St := Legacy^.Pos[20]; En := Legacy^.Pos[21]; end
      else begin St := Legacy^.Pos[Legacy^.N * 2]; En := Legacy^.Pos[Legacy^.N * 2 + 1]; end;
    3: begin St := 0; En := Legacy^.Pos[0]; end;
    4: begin St := Legacy^.Pos[1]; En := S^.Len; end;
  else
    if K - 4 > Legacy^.N then St := -1
    else begin St := Legacy^.Pos[(K - 4) * 2]; En := Legacy^.Pos[(K - 4) * 2 + 1]; end;
  end;
  if St < 0 then begin Res := MkPtr(AtomOf('')); Exit(True); end;
  Res := SubString(S, St, En - St);
  Result := True;
end;

{ set RegExp.input / RegExp.$_ }
function Legacy_SetInput(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  if Args[0] <> RegExpCtorV then Exit(ThrowTypeError('RegExp.input set through a receiver other than RegExp'));
  if not ToStringV(Arg(Args, Argc, 1), V) then Exit(False);
  Legacy^.InputStr := V;
  Res := VUndefined;
  Result := True;
end;

{ ---- the unmodified-RegExp check of the native fast paths ---- }

const
  IntactCount = 16;

threadvar
  IntactKeys: array of PStr;
  IntactSlots: array of Integer;
  IntactShape: PShape;
  IntactVersion: Cardinal;
  IntactOrig: TValue;   { array: the original value of each key's slot on RegExp.prototype }
  SpeciesOrig: TValue;  { the RegExp constructor's original @@species accessor }
  JSFns: TValue;   { the prelude's functions, by JSF_* index }

const
  JSF_Construct = 0; JSF_Test = 1; JSF_Match = 2; JSF_MatchAll = 3; JSF_Replace = 4; JSF_ReplaceAll = 5;
  JSF_Search = 6; JSF_Split = 7;

{ RegExp.prototype still holds the original value of every slot in Mask (bit I: IntactKeys[I]) }
function ProtoIntact(Mask: Cardinal): Boolean;
var P: PObj; I: Integer; E: PValueArray;
begin
  if not IsObject(IntactOrig) then Exit(False);
  P := ObjOf(RegExpProto);
  if (P^.Shape <> IntactShape) or (P^.Shape^.Version <> IntactVersion) then
  begin
    IntactShape := P^.Shape;
    IntactVersion := P^.Shape^.Version;
    for I := 0 to IntactCount - 1 do IntactSlots[I] := ShapeLookup(P^.Shape, IntactKeys[I]);
  end;
  E := ObjOf(IntactOrig)^.Elements;
  I := 0;
  while Mask <> 0 do
  begin
    if (Mask and 1) <> 0 then
      if (IntactSlots[I] < 0) or (GetSlot(P, IntactSlots[I]) <> E^.Data[I]) then Exit(False);
    Mask := Mask shr 1;
    Inc(I);
  end;
  Result := True;
end;

const
  { the slots each operation's spec steps read: exec, the flags getter and the flag getters it reads, the
    Symbol method that String.prototype dispatches to, constructor (SpeciesConstructor) }
  MK_Exec = 1; MK_Flags = $3FE; MK_Match = 1 shl 10; MK_Replace = 1 shl 12; MK_Search = 1 shl 13;
  MK_Split = 1 shl 14; MK_Ctor = 1 shl 15;
  MaskTest = MK_Exec;
  MaskMatch = MK_Exec or MK_Flags or MK_Match;
  MaskSearch = MK_Exec or MK_Search;
  MaskReplace = MK_Exec or MK_Flags or MK_Replace;
  MaskReplaceAll = MK_Exec or MK_Flags or MK_Match or MK_Replace;
  MaskSplit = MK_Exec or MK_Flags or MK_Split or MK_Ctor;

{ R is an unmodified RegExp: its own lastIndex an integer, its prototype RegExp.prototype as built }
function FastRe(R: TValue; Mask: Cardinal): Boolean; inline;
begin
  Result := IsRegExpV(R) and (ObjOf(R)^.Shape = RegExpShape) and IsInt(ObjOf(R)^.Slots[0]) and ProtoIntact(Mask);
end;

function SpeciesIntact: Boolean;
var S: Integer;
begin
  S := ShapeLookup(ObjOf(RegExpCtorV)^.Shape, SymKey(SymSpecies));
  Result := (S >= 0) and (GetSlot(ObjOf(RegExpCtorV), S) = SpeciesOrig);
end;

function CallJS(Idx: Integer; This: TValue; Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := CallValue(ObjOf(JSFns)^.Elements^.Data[Idx], This, Args, Argc, Res);
end;

{ ---- RegExpBuiltinExec ---- }

{ ToLength(Get(R, "lastIndex")) }
function GetLastIndex(Re: TValue; var Idx: Int64): Boolean;
var V: TValue; D: Double;
begin
  if (ObjOf(Re)^.Shape = RegExpShape) and IsInt(ObjOf(Re)^.Slots[0]) then
  begin
    Idx := IntOf(ObjOf(Re)^.Slots[0]);
    if Idx < 0 then Idx := 0;
    Exit(True);
  end;
  if not GetV(Re, AtomLastIndex, V) then Exit(False);
  if not ToNumberV(V, D) then Exit(False);
  if IsNaN(D) or (D <= 0) then Idx := 0 else if D > 9007199254740991.0 then Idx := 9007199254740991 else Idx := Trunc(D);
  Result := True;
end;

{ Set(R, "lastIndex", I, true): R is a RegExp, whose lastIndex is a non-configurable own data property }
function SetLastIndex(Re: TValue; I: Int64): Boolean; inline;
begin
  if ObjOf(Re)^.Shape = RegExpShape then
  begin
    ObjOf(Re)^.Slots[0] := MkNumber(I);
    Exit(True);
  end;
  Result := SetVT(Re, AtomLastIndex, MkNumber(I));
end;

{ the [start, end] pair array of capture I, or undefined }
function IndexPair(Sc: TRegexMatcher; I: Integer): TValue;
var A: PObj; St, En: Integer;
begin
  if not Sc.TryGetGroup(I, St, En) then Exit(VUndefined);
  A := NewArray(2, 2);
  A^.Elements^.Data[0] := MkInt(St);
  A^.Elements^.Data[1] := MkInt(En);
  Result := MkPtr(A);
end;

{ the named groups in capture order (a name listed once per group that has it) }
procedure GroupsInOrder(Sc: TRegexMatcher; var NG: TGocciaRegExpNamedGroups);
var I, J: Integer; T: TGocciaRegExpNamedGroup;
begin
  NG := Copy(Sc.NamedGroups);
  for I := 1 to High(NG) do
  begin
    T := NG[I];
    J := I - 1;
    while (J >= 0) and (NG[J].Index > T.Index) do begin NG[J + 1] := NG[J]; Dec(J); end;
    NG[J + 1] := T;
  end;
end;

{ the groups object of the current match (22.2.7.2 steps 33-34): a name shared by several groups gets the
  value of the one that participated }
function GroupsObject(Sc: TRegexMatcher; S: PStr; Indices: TValue): TValue;
var G: PObj; NG: TGocciaRegExpNamedGroups; I, St, En, K: Integer; V: TValue; Seen: array of PStr; Key: PStr;
  Dup: Boolean;
begin
  GroupsInOrder(Sc, NG);
  PushRoot(MkPtr(S)); PushRoot(Indices);
  G := NewPlainObject(VNull, 4);
  PushRoot(MkPtr(G));
  SetLength(Seen, 0);
  for I := 0 to High(NG) do
  begin
    Key := AtomOf(UnicodeString(NG[I].Name));
    Dup := False;
    for K := 0 to High(Seen) do if Seen[K] = Key then Dup := True;
    if Dup then Continue;
    if IsObject(RootAt(TempTop - 2)) then
      V := ObjOf(RootAt(TempTop - 2))^.Elements^.Data[NG[I].Index]
    else if Sc.TryGetGroup(NG[I].Index, St, En) then V := SubString(StrOf(RootAt(TempTop - 3)), St, En - St)
    else V := VUndefined;
    DefineOwn(RootAt(TempTop - 1), Key, V, pfDefault);
    if V <> VUndefined then
    begin
      SetLength(Seen, Length(Seen) + 1);
      Seen[High(Seen)] := Key;
    end;
  end;
  Result := RootAt(TempTop - 1);
  PopRoots(3);
end;

{ the exec() result array of the scanner's current match }
function MatchArray(Sc: TRegexMatcher; C: PCompiled; S: PStr): TValue;
var A: PObj; I, St, En: Integer; V: TValue; N: Integer; VA: PValueArray;
begin
  N := Sc.CaptureCount;
  PushRoot(MkPtr(S));
  if (MatchShape <> nil) and not Sc.HasNamedGroups and not C^.HasIndices then
  begin
    { the shape every exec() result without named groups has, with index, input and groups inline }
    VA := NewValueArray(N + 1);
    VA^.Len := N + 1;
    for I := 0 to N do VA^.Data[I] := VHole;
    PushRoot(MkPtr(VA));
    A := NewObject(MatchShape, 3, okArray);
    VA := PValueArray(PtrOf(RootAt(TempTop - 1)));
    PopRoots(1);
    A^.Elements := VA;
    WriteBarrier(@A^.Hdr, MkPtr(VA));
    A^.Slots[0] := MkInt(Sc.MatchIndex);
    A^.Slots[1] := RootAt(TempTop - 1);
    A^.Slots[2] := VUndefined;
    PushRoot(MkPtr(A));
    for I := 0 to N do
    begin
      if Sc.TryGetGroup(I, St, En) then V := SubString(StrOf(RootAt(TempTop - 2)), St, En - St) else V := VUndefined;
      ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, V);
    end;
    Result := RootAt(TempTop - 1);
    PopRoots(2);
    Exit;
  end;
  A := NewArray(N + 1, N + 1);
  PushRoot(MkPtr(A));
  for I := 0 to N do
  begin
    if Sc.TryGetGroup(I, St, En) then V := SubString(StrOf(RootAt(TempTop - 2)), St, En - St) else V := VUndefined;
    ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, V);
  end;
  AddProperty(ObjOf(RootAt(TempTop - 1)), AtomIndex, MkInt(Sc.MatchIndex), pfDefault);
  AddProperty(ObjOf(RootAt(TempTop - 1)), AtomInput, RootAt(TempTop - 2), pfDefault);
  if Sc.HasNamedGroups then
  begin
    V := GroupsObject(Sc, StrOf(RootAt(TempTop - 2)), VUndefined);
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomGroups, V, pfDefault);
  end
  else
  begin
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomGroups, VUndefined, pfDefault);
    A := ObjOf(RootAt(TempTop - 1));
    if (MatchShape = nil) and (A^.InlineCap = 0) and (A^.Overflow <> nil) and (A^.Overflow^.Cap >= 3) then
      MatchShape := A^.Shape;
  end;
  if C^.HasIndices then
  begin
    { 22.2.7.8 MakeMatchIndicesIndexPairArray }
    A := NewArray(N + 1, N + 1);
    PushRoot(MkPtr(A));
    for I := 0 to N do
    begin
      V := IndexPair(Sc, I);
      ValueArrayStore(ObjOf(RootAt(TempTop - 1))^.Elements, I, V);
    end;
    if Sc.HasNamedGroups then V := GroupsObject(Sc, StrOf(RootAt(TempTop - 3)), RootAt(TempTop - 1))
    else V := VUndefined;
    AddProperty(ObjOf(RootAt(TempTop - 1)), AtomGroups, V, pfDefault);
    AddProperty(ObjOf(RootAt(TempTop - 2)), AtomOf('indices'), RootAt(TempTop - 1), pfDefault);
    PopRoots(1);
  end;
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ 22.2.7.2 RegExpBuiltinExec: Res the match array (WantArray) or null; Matched whether it matched }
function BuiltinExec(Re: TValue; S: PStr; WantArray: Boolean; var Res: TValue; var Matched: Boolean): Boolean;
var C: PCompiled; LI: Int64; Sc: TRegexMatcher; GS: Boolean;
begin
  Matched := False;
  Res := VNull;
  C := CompiledOf(Re);
  if (ObjOf(Re)^.Shape = RegExpShape) and IsInt(ObjOf(Re)^.Slots[0]) then
  begin
    { an integer own lastIndex: read and written in place, no code runs, so nothing needs a root }
    GS := C^.Global or C^.Sticky;
    if GS then
    begin
      LI := IntOf(ObjOf(Re)^.Slots[0]);
      if LI < 0 then LI := 0;
      if LI > S^.Len then begin ObjOf(Re)^.Slots[0] := MkInt(0); Exit(True); end;
    end
    else LI := 0;
    Sc := C^.M;
    Matched := Sc.Exec(StrChars(S), S^.Len, LI, C^.Sticky);
    if Sc.Failed then Exit(ThrowError('SyntaxError', Sc.ErrorMessage));
    if not Matched then
    begin
      if GS then ObjOf(Re)^.Slots[0] := MkInt(0);
      Exit(True);
    end;
    RecordLegacy(Re, Sc, S);
    if GS then ObjOf(Re)^.Slots[0] := MkInt(Sc.MatchEnd);
    if WantArray then Res := MatchArray(Sc, C, S);
    Exit(True);
  end;
  PushRoot(Re); PushRoot(MkPtr(S));
  if not GetLastIndex(Re, LI) then begin PopRoots(2); Exit(False); end;
  Re := RootAt(TempTop - 2); S := StrOf(RootAt(TempTop - 1));
  C := CompiledOf(Re);
  GS := C^.Global or C^.Sticky;
  if not GS then LI := 0;
  if LI > S^.Len then
  begin
    PopRoots(2);
    if GS then Exit(SetLastIndex(Re, 0));
    Exit(True);
  end;
  Sc := C^.M;
  Matched := Sc.Exec(StrChars(S), S^.Len, LI, C^.Sticky);
  if Sc.Failed then begin PopRoots(2); Exit(ThrowError('SyntaxError', Sc.ErrorMessage)); end;
  if not Matched then
  begin
    PopRoots(2);
    if GS then Exit(SetLastIndex(Re, 0));
    Exit(True);
  end;
  RecordLegacy(RootAt(TempTop - 2), Sc, StrOf(RootAt(TempTop - 1)));
  if GS and not SetLastIndex(Re, Sc.MatchEnd) then begin PopRoots(2); Exit(False); end;
  if WantArray then Res := MatchArray(Sc, C, StrOf(RootAt(TempTop - 1)));
  PopRoots(2);
  Result := True;
end;

{ AdvanceStringIndex: past an empty match by one code point (a surrogate pair is one under the u flag) }
function NextIndex(S: PStr; I: Integer; Uni: Boolean): Integer;
var P: PWideChar;
begin
  Result := I + 1;
  if not Uni or (I + 1 >= S^.Len) then Exit;
  P := StrChars(S);
  if (Ord(P[I]) >= $D800) and (Ord(P[I]) <= $DBFF) and (Ord(P[I + 1]) >= $DC00) and (Ord(P[I + 1]) <= $DFFF) then
    Result := I + 2;
end;

function StrArg(Args: PValue; Argc, I: Integer; var S: PStr): Boolean;
var V: TValue;
begin
  if not ToStringV(Arg(Args, Argc, I), V) then Exit(False);
  S := StrOf(V);
  Result := True;
end;

{ 22.2.6.2 RegExp.prototype.exec ( string ) }
function Re_Exec(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; M: Boolean;
begin
  if not IsRegExpV(Args[0]) then Exit(ThrowTypeError('RegExp.prototype.exec called on an incompatible receiver'));
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  Result := BuiltinExec(Args[0], S, True, Res, M);
end;

{ 22.2.6.16 RegExp.prototype.test ( S ): the builtin exec for an unmodified RegExp, else the prelude's }
function Re_Test(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; M: Boolean; V: TValue;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('RegExp.prototype.test called on a non-object'));
  if not StrArg(Args, Argc, 1, S) then Exit(False);
  if FastRe(Args[0], MaskTest) then
  begin
    if not BuiltinExec(Args[0], S, False, V, M) then Exit(False);
    Res := MkBool(M);
    Exit(True);
  end;
  V := MkPtr(S);
  Result := CallJS(JSF_Test, Args[0], @V, 1, Res);
end;

{ 22.2.6.13.1 EscapeRegExpPattern }
function EscapePattern(const P: UnicodeString): UnicodeString;
var I: Integer; InClass: Boolean; C: WideChar;
begin
  if P = '' then Exit('(?:)');
  Result := '';
  InClass := False;
  I := 1;
  while I <= Length(P) do
  begin
    C := P[I];
    if (C = '\') and (I < Length(P)) then
    begin
      Result := Result + C;
      Inc(I);
      C := P[I];
      case Ord(C) of
        10: Result := Result + 'n';
        13: Result := Result + 'r';
        $2028: Result := Result + 'u2028';
        $2029: Result := Result + 'u2029';
      else Result := Result + C;
      end;
      Inc(I);
      Continue;
    end;
    case Ord(C) of
      10: Result := Result + '\n';
      13: Result := Result + '\r';
      $2028: Result := Result + ' ';
      $2029: Result := Result + ' ';
    else
      begin
        if C = '[' then InClass := True
        else if C = ']' then InClass := False;
        if (C = '/') and not InClass then Result := Result + '\/' else Result := Result + C;
      end;
    end;
    Inc(I);
  end;
end;

{ 22.2.6.13 get RegExp.prototype.source }
function Re_Source(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('RegExp.prototype.source getter called on a non-object'));
  if not IsRegExpV(Args[0]) then
  begin
    if Args[0] = RegExpProto then begin Res := StrFromUnicode('(?:)'); Exit(True); end;
    Exit(ThrowTypeError('RegExp.prototype.source getter called on an incompatible receiver'));
  end;
  Res := StrFromUnicode(EscapePattern(CompiledOf(Args[0])^.Pattern));
  Result := True;
end;

{ 22.2.6.4.1 RegExpHasFlag }
function FlagGetter(Args: PValue; var Res: TValue; F: Char): Boolean;
begin
  if not IsObject(Args[0]) then Exit(ThrowTypeError('RegExp flag getter called on a non-object'));
  if not IsRegExpV(Args[0]) then
  begin
    if Args[0] = RegExpProto then begin Res := VUndefined; Exit(True); end;
    Exit(ThrowTypeError('RegExp flag getter called on an incompatible receiver'));
  end;
  Res := MkBool(Pos(F, CompiledOf(Args[0])^.Flags) > 0);
  Result := True;
end;
function Re_Global(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'g'); end;
function Re_IgnoreCase(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'i'); end;
function Re_Multiline(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'm'); end;
function Re_Sticky(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'y'); end;
function Re_Unicode(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'u'); end;
function Re_UnicodeSets(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'v'); end;
function Re_DotAll(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 's'); end;
function Re_HasIndices(Args: PValue; Argc: Integer; var Res: TValue): Boolean; begin Result := FlagGetter(Args, Res, 'd'); end;

{ 22.2.4.1 RegExp ( pattern, flags ): string arguments under RegExp itself build directly; the rest goes
  through the prelude's steps }
function Re_Ctor(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var P, F: TValue; PS, FS: UnicodeString; C: PCompiled; CA: array[0..2] of TValue;
begin
  P := Arg(Args, Argc, 1);
  F := Arg(Args, Argc, 2);
  if ((P = VUndefined) or IsString(P)) and ((F = VUndefined) or IsString(F)) and
     ((CurNewTarget = VUndefined) or (CurNewTarget = RegExpCtorV)) then
  begin
    if P = VUndefined then PS := '' else PS := StrToUnicode(StrOf(P));
    if F = VUndefined then FS := '' else FS := StrToUnicode(StrOf(F));
    if not GetCompiled(PS, FS, C) then Exit(False);
    Res := NewRegExpObject(C);
    Exit(True);
  end;
  CA[0] := P; CA[1] := F; CA[2] := CurNewTarget;
  Result := CallJS(JSF_Construct, VUndefined, @CA[0], 3, Res);
end;

{ ---- the prelude's natives ---- }

function N_Exec(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var M: Boolean;
begin
  if not IsRegExpV(Arg(Args, Argc, 1)) then Exit(ThrowTypeError('RegExp exec called on an incompatible receiver'));
  Result := BuiltinExec(Args[1], StrOf(Args[2]), True, Res, M);
end;

function N_IsRE(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool(IsRegExpV(Arg(Args, Argc, 1)));
  Result := True;
end;

function N_Alloc(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := AllocRegExp(Args[1], ToBool(Arg(Args, Argc, 2)));
  Result := True;
end;

{ [[LegacyFeaturesEnabled]] }
function N_Legacy(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := MkBool((ObjOf(Args[1])^.OFlags and ofLegacyOff) = 0);
  Result := True;
end;

{ RegExpInitialize's parse: N.init(O, P, F) with P and F strings }
function N_Init(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var C: PCompiled;
begin
  if not GetCompiled(StrToUnicode(StrOf(Args[2])), StrToUnicode(StrOf(Args[3])), C) then Exit(False);
  ObjOf(Args[1])^.Native := C;
  Res := VUndefined;
  Result := True;
end;

function N_Source(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode(CompiledOf(Args[1])^.Pattern);
  Result := True;
end;

function N_Flags(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode(CompiledOf(Args[1])^.Flags);
  Result := True;
end;

{ ---- String methods with a RegExp argument ---- }

{ this a String value and the argument an unmodified RegExp: the native path stands in for the spec's }
function FastStrRe(Args: PValue; Argc: Integer; Mask: Cardinal): Boolean; inline;
begin
  Result := IsString(Args[0]) and (Argc >= 1) and FastRe(Args[1], Mask);
end;

{ a primitive pattern and an unmodified RegExp.prototype: RegExpCreate(pattern) is unobservable }
function FastStrPattern(Args: PValue; Argc: Integer; Mask: Cardinal): Boolean; inline;
begin
  Result := IsString(Args[0]) and not IsObject(Arg(Args, Argc, 1)) and ProtoIntact(Mask);
end;

function CallJSMethod(Idx: Integer; Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := CallJS(Idx, Args[0], @Args[1], Argc, Res);
end;

function MatchGlobal(Args: PValue; C: PCompiled; var Res: TValue): Boolean;
var A: PObj; S: PStr; Sc: TRegexMatcher; Ok: Boolean; St, En: Integer; M: TValue;
begin
  if not SetLastIndex(Args[1], 0) then Exit(False);
  A := NewArray(0, 4);
  PushRoot(MkPtr(A));
  S := StrOf(Args[0]);
  St := 0;
  while St <= S^.Len do
  begin
    Sc := C^.M;
    Ok := Sc.Exec(StrChars(S), S^.Len, St, C^.Sticky);
    if Sc.Failed then begin PopRoots(1); Exit(ThrowError('SyntaxError', Sc.ErrorMessage)); end;
    if not Ok then Break;
    RecordLegacy(Args[1], Sc, StrOf(Args[0]));
    Sc.TryGetGroup(0, St, En);
    M := SubString(StrOf(Args[0]), St, En - St);
    ArrayPush(ObjOf(RootAt(TempTop - 1)), M);
    if En = St then St := NextIndex(StrOf(Args[0]), En, C^.Unicode) else St := En;
    S := StrOf(Args[0]);
  end;
  if ArrayLen(ObjOf(RootAt(TempTop - 1))) = 0 then Res := VNull else Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ 22.1.3.13 String.prototype.match }
function Str_Match(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var C: PCompiled; M: Boolean; PS: UnicodeString; V: TValue;
begin
  if FastStrPattern(Args, Argc, MaskMatch) then
  begin
    { RegExpCreate(pattern, undefined), then its Symbol.match }
    if Arg(Args, Argc, 1) = VUndefined then PS := ''
    else begin if not ToStringV(Args[1], V) then Exit(False); PS := StrToUnicode(StrOf(V)); end;
    if not GetCompiled(PS, '', C) then Exit(False);
    V := NewRegExpObject(C);
    if Argc < 1 then Exit(BuiltinExec(V, StrOf(Args[0]), True, Res, M));
    Args[1] := V;   { a stack slot: it keeps the new RegExp rooted }
  end
  else if not FastStrRe(Args, Argc, MaskMatch) then Exit(CallJSMethod(JSF_Match, Args, Argc, Res));
  C := CompiledOf(Args[1]);
  if not C^.Global then Exit(BuiltinExec(Args[1], StrOf(Args[0]), True, Res, M));
  Result := MatchGlobal(Args, C, Res);
end;

{ 22.1.3.14 String.prototype.matchAll: the RegExp String Iterator is the prelude's }
function Str_MatchAll(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Result := CallJSMethod(JSF_MatchAll, Args, Argc, Res);
end;

{ 22.1.3.22 String.prototype.search }
function Str_Search(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; Sc: TRegexMatcher; Ok: Boolean; C: PCompiled; PS: UnicodeString; V: TValue;
begin
  if FastStrPattern(Args, Argc, MaskSearch) then
  begin
    if Arg(Args, Argc, 1) = VUndefined then PS := ''
    else begin if not ToStringV(Args[1], V) then Exit(False); PS := StrToUnicode(StrOf(V)); end;
    if not GetCompiled(PS, '', C) then Exit(False);
  end
  else if FastStrRe(Args, Argc, MaskSearch) then C := CompiledOf(Args[1])
  else Exit(CallJSMethod(JSF_Search, Args, Argc, Res));
  { lastIndex is set to 0 for the search and back afterwards: unchanged }
  S := StrOf(Args[0]);
  Sc := C^.M;
  Ok := Sc.Exec(StrChars(S), S^.Len, 0, C^.Sticky);
  if Sc.Failed then Exit(ThrowError('SyntaxError', Sc.ErrorMessage));
  if Ok then
  begin
    if IsObject(Arg(Args, Argc, 1)) then RecordLegacy(Args[1], Sc, StrOf(Args[0]))
    else RecordLegacy(RegExpProto, Sc, StrOf(Args[0]));
    Res := MkInt(Sc.MatchIndex);
  end
  else Res := MkInt(-1);
  Result := True;
end;

{ $-substitutions of a replacement string (22.1.3.19.1 GetSubstitution) from a recorded match: Caps holds
  start/end pairs (-1 when a group did not participate) }
function Substitute(const Rep, Subject: UnicodeString; Caps: PInteger; Base, N: Integer;
  const NG: TGocciaRegExpNamedGroups): UnicodeString;
var I, G, K, J, MS, ME: Integer; C: WideChar; Name: UnicodeString; Found: Boolean;
begin
  Result := '';
  MS := Caps[Base]; ME := Caps[Base + 1];
  I := 1;
  while I <= Length(Rep) do
  begin
    C := Rep[I];
    if (C = '$') and (I < Length(Rep)) then
    begin
      case Rep[I + 1] of
        '$': begin Result := Result + '$'; Inc(I, 2); Continue; end;
        '&': begin Result := Result + Copy(Subject, MS + 1, ME - MS); Inc(I, 2); Continue; end;
        '`': begin Result := Result + Copy(Subject, 1, MS); Inc(I, 2); Continue; end;
        '''': begin Result := Result + Copy(Subject, ME + 1, MaxInt); Inc(I, 2); Continue; end;
        '0'..'9':
          begin
            G := Ord(Rep[I + 1]) - 48;
            K := 2;
            if (I + 2 <= Length(Rep)) and CharInSet(Rep[I + 2], ['0'..'9']) and (G * 10 + Ord(Rep[I + 2]) - 48 <= N) then
            begin
              G := G * 10 + Ord(Rep[I + 2]) - 48;
              K := 3;
            end;
            if (G >= 1) and (G <= N) then
            begin
              if Caps[Base + G * 2] >= 0 then
                Result := Result + Copy(Subject, Caps[Base + G * 2] + 1, Caps[Base + G * 2 + 1] - Caps[Base + G * 2]);
              Inc(I, K);
              Continue;
            end;
          end;
        '<':
          if Length(NG) > 0 then
          begin
            K := Pos('>', Copy(Rep, I + 2, MaxInt));
            if K > 0 then
            begin
              Name := Copy(Rep, I + 2, K - 1);
              Found := False;
              for J := 0 to High(NG) do
                if not Found and (UnicodeString(NG[J].Name) = Name) and (Caps[Base + NG[J].Index * 2] >= 0) then
                begin
                  G := NG[J].Index;
                  Result := Result + Copy(Subject, Caps[Base + G * 2] + 1, Caps[Base + G * 2 + 1] - Caps[Base + G * 2]);
                  Found := True;
                end;
              Inc(I, K + 2);
              Continue;
            end;
          end;
      end;
    end;
    Result := Result + C;
    Inc(I);
  end;
end;

{ 22.2.6.11 for an unmodified RegExp: every match is recorded first, then the replacements are made (a
  replacer function runs after the matching, as in the spec) }
function ReplaceWithRegExp(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; C: PCompiled; Subject, Out_, Rep: UnicodeString; Sc: TRegexMatcher; Ok, Global, IsFn: Boolean;
  St, En, G, K, N, NM, Pos_, I: Integer; RV, SV, GV: TValue; CallArgs: array of TValue; Start: Int64;
  CapsDyn: array of Integer; CapsBuf: array[0..63] of Integer; Caps: PInteger; CapsLen: Integer;
  NG: TGocciaRegExpNamedGroups; Rooted: Integer; GObj: PObj; Key: PStr;
begin
  C := CompiledOf(Args[1]);
  Global := C^.Global;
  IsFn := IsCallable(Arg(Args, Argc, 2));
  if not IsFn then Rep := StrToUnicode(StrOf(Args[2]));
  Subject := StrToUnicode(StrOf(Args[0]));
  Start := 0;
  if Global then
  begin
    if not SetLastIndex(Args[1], 0) then Exit(False);
  end
  else if not GetLastIndex(Args[1], Start) then Exit(False)
  else if not C^.Sticky then Start := 0;
  { the matches }
  Sc := C^.M;
  N := Sc.CaptureCount;
  NG := Sc.NamedGroups;
  NM := 0;
  Caps := @CapsBuf[0];
  CapsLen := Length(CapsBuf);
  St := Start;
  while St <= Length(Subject) do
  begin
    S := StrOf(Args[0]);
    Sc := C^.M;
    Ok := Sc.Exec(StrChars(S), S^.Len, St, C^.Sticky);
    if Sc.Failed then Exit(ThrowError('SyntaxError', Sc.ErrorMessage));
    if not Ok then Break;
    RecordLegacy(Args[1], Sc, StrOf(Args[0]));
    if CapsLen < (NM + 1) * (N + 1) * 2 then
    begin
      { the matches outgrow the stack buffer: on to the heap }
      SetLength(CapsDyn, (NM + 1) * (N + 1) * 4 + 16);
      if Caps = @CapsBuf[0] then Move(CapsBuf[0], CapsDyn[0], SizeOf(CapsBuf));
      Caps := @CapsDyn[0];
      CapsLen := Length(CapsDyn);
    end;
    for G := 0 to N do
      if Sc.TryGetGroup(G, K, Pos_) then
      begin
        Caps[NM * (N + 1) * 2 + G * 2] := K;
        Caps[NM * (N + 1) * 2 + G * 2 + 1] := Pos_;
      end
      else
      begin
        Caps[NM * (N + 1) * 2 + G * 2] := -1;
        Caps[NM * (N + 1) * 2 + G * 2 + 1] := -1;
      end;
    Inc(NM);
    Sc.TryGetGroup(0, St, En);
    if not Global then Break;
    if En = St then St := NextIndex(StrOf(Args[0]), En, C^.Unicode) else St := En;
  end;
  { lastIndex as the spec's last RegExpBuiltinExec leaves it }
  if Global then
  begin
    if not SetLastIndex(Args[1], 0) then Exit(False);
  end
  else if C^.Sticky then
  begin
    if NM = 0 then Ok := SetLastIndex(Args[1], 0) else Ok := SetLastIndex(Args[1], Caps[1]);
    if not Ok then Exit(False);
  end;
  if NM = 0 then begin Res := Args[0]; Exit(True); end;
  { the replacements }
  Out_ := '';
  Pos_ := 0;
  for I := 0 to NM - 1 do
  begin
    St := Caps[I * (N + 1) * 2]; En := Caps[I * (N + 1) * 2 + 1];
    Out_ := Out_ + Copy(Subject, Pos_ + 1, St - Pos_);
    if IsFn then
    begin
      if Length(NG) > 0 then SetLength(CallArgs, N + 4) else SetLength(CallArgs, N + 3);
      Rooted := 0;
      for G := 0 to N do
      begin
        K := Caps[I * (N + 1) * 2 + G * 2];
        if K >= 0 then SV := StrFromUnicode(Copy(Subject, K + 1, Caps[I * (N + 1) * 2 + G * 2 + 1] - K))
        else SV := VUndefined;
        PushRoot(SV);
        Inc(Rooted);
      end;
      if Length(NG) > 0 then
      begin
        { the groups object: a name's value from the group of that name that participated }
        GObj := NewPlainObject(VNull, 4);
        PushRoot(MkPtr(GObj));
        Inc(Rooted);
        for G := 0 to High(NG) do
        begin
          Key := AtomOf(UnicodeString(NG[G].Name));
          if ShapeLookup(ObjOf(RootAt(TempTop - 1))^.Shape, Key) >= 0 then
          begin
            if Caps[I * (N + 1) * 2 + NG[G].Index * 2] < 0 then Continue;
          end;
          GV := RootAt(TempTop - 1 - (N + 1) + NG[G].Index);
          DefineOwn(RootAt(TempTop - 1), Key, GV, pfDefault);
        end;
      end;
      for G := 0 to N do CallArgs[G] := RootAt(TempTop - Rooted + G);
      CallArgs[N + 1] := MkInt(St);
      CallArgs[N + 2] := Args[0];
      if Length(NG) > 0 then CallArgs[N + 3] := RootAt(TempTop - 1);
      if not CallValue(Args[2], VUndefined, @CallArgs[0], Length(CallArgs), RV) then begin PopRoots(Rooted); Exit(False); end;
      PopRoots(Rooted);
      if not ToStringV(RV, SV) then Exit(False);
      Out_ := Out_ + StrToUnicode(StrOf(SV));
    end
    else
      Out_ := Out_ + Substitute(Rep, Subject, Caps, I * (N + 1) * 2, N, NG);
    Pos_ := En;
  end;
  Out_ := Out_ + Copy(Subject, Pos_ + 1, MaxInt);
  Res := StrFromUnicode(Out_);
  Result := True;
end;

{ the replace value is a string or a function: converting it runs no code }
function FastReplaceValue(Args: PValue; Argc: Integer): Boolean; inline;
begin
  Result := (Argc >= 2) and (IsString(Args[2]) or IsCallable(Args[2]));
end;

{ 22.1.3.19 String.prototype.replace }
function Str_Replace(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(TNativeFn(ObjOf(StrReplaceOrig)^.Native)(Args, Argc, Res));
  if FastStrRe(Args, Argc, MaskReplace) and FastReplaceValue(Args, Argc) then Exit(ReplaceWithRegExp(Args, Argc, Res));
  Result := CallJSMethod(JSF_Replace, Args, Argc, Res);
end;

{ 22.1.3.20 String.prototype.replaceAll }
function Str_ReplaceAll(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(TNativeFn(ObjOf(StrReplaceAllOrig)^.Native)(Args, Argc, Res));
  if FastStrRe(Args, Argc, MaskReplaceAll) and FastReplaceValue(Args, Argc) then
  begin
    if not CompiledOf(Args[1])^.Global then Exit(ThrowTypeError('String.prototype.replaceAll called with a non-global RegExp'));
    Exit(ReplaceWithRegExp(Args, Argc, Res));
  end;
  Result := CallJSMethod(JSF_ReplaceAll, Args, Argc, Res);
end;

{ ToUint32 }
function ToUint32Of(D: Double): Int64;
begin
  if IsNaN(D) or IsInfinite(D) then Exit(0);
  D := Int(D);
  D := D - 4294967296.0 * Floor(D / 4294967296.0);
  Result := Trunc(D);
end;

{ 22.2.6.14 for an unmodified RegExp (and RegExp's own species): the sticky splitter is unobservable }
function Str_Split(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var S: PStr; C: PCompiled; A: PObj; Sc: TRegexMatcher; Ok: Boolean; P, Q, St, En, G, K, L: Integer;
  Lim: Int64; V: TValue;
begin
  if not IsObject(Arg(Args, Argc, 1)) then Exit(TNativeFn(ObjOf(StrSplitOrig)^.Native)(Args, Argc, Res));
  if not (FastStrRe(Args, Argc, MaskSplit) and ((Argc < 2) or (Args[2] = VUndefined) or IsNumber(Args[2])) and SpeciesIntact) then
    Exit(CallJSMethod(JSF_Split, Args, Argc, Res));
  Lim := 4294967295;
  if (Argc >= 2) and (Args[2] <> VUndefined) then Lim := ToUint32Of(NumOf(Args[2]));
  C := CompiledOf(Args[1]);
  A := NewArray(0, 8);
  PushRoot(MkPtr(A));
  S := StrOf(Args[0]);
  if Lim = 0 then begin Res := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
  if S^.Len = 0 then
  begin
    Sc := C^.M;
    Ok := Sc.Exec(StrChars(S), S^.Len, 0, True);
    if Sc.Failed then begin PopRoots(1); Exit(ThrowError('SyntaxError', Sc.ErrorMessage)); end;
    if Ok then RecordLegacy(RegExpProto, Sc, StrOf(Args[0]));
    if not Ok then ArrayPush(ObjOf(RootAt(TempTop - 1)), Args[0]);
    Res := RootAt(TempTop - 1);
    PopRoots(1);
    Exit(True);
  end;
  P := 0; Q := 0;
  while Q < S^.Len do
  begin
    S := StrOf(Args[0]);
    Sc := C^.M;
    Ok := Sc.Exec(StrChars(S), S^.Len, Q, True);
    if Sc.Failed then begin PopRoots(1); Exit(ThrowError('SyntaxError', Sc.ErrorMessage)); end;
    if not Ok then begin Q := NextIndex(S, Q, C^.Unicode); Continue; end;
    RecordLegacy(RegExpProto, Sc, StrOf(Args[0]));
    Sc.TryGetGroup(0, St, En);
    if En > S^.Len then En := S^.Len;
    if En = P then begin Q := NextIndex(S, Q, C^.Unicode); Continue; end;
    V := SubString(S, P, Q - P);
    ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
    if ArrayLen(ObjOf(RootAt(TempTop - 1))) >= Lim then begin Res := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
    for G := 1 to Sc.CaptureCount do
    begin
      S := StrOf(Args[0]);
      if Sc.TryGetGroup(G, K, L) then V := SubString(S, K, L - K) else V := VUndefined;
      ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
      if ArrayLen(ObjOf(RootAt(TempTop - 1))) >= Lim then begin Res := RootAt(TempTop - 1); PopRoots(1); Exit(True); end;
    end;
    P := En;
    Q := P;
  end;
  S := StrOf(Args[0]);
  V := SubString(S, P, S^.Len - P);
  ArrayPush(ObjOf(RootAt(TempTop - 1)), V);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

procedure InitRegExp;
var C, StrProto_, Getr, Setr: TValue; I, K: Integer; LName: UnicodeString;
  procedure Getter(const Name: UnicodeString; Fn: TNativeFn);
  var F: TValue;
  begin
    F := NewNativeFunction(Fn, 'get ' + Name, 0);
    F := MkPtr(NewAccessor(F, VUndefined));
    DefineOwn(RegExpProto, AtomOf(Name), F, pfConfigurable or pfAccessor);
  end;
begin
  Compiled := TDictionary<UnicodeString, PCompiled>.Create;
  AddRoot(@RegExpProto); AddRoot(@StrReplaceOrig); AddRoot(@StrSplitOrig); AddRoot(@StrReplaceAllOrig); AddRoot(@Scratch);
  AddRoot(@IntactOrig); AddRoot(@SpeciesOrig); AddRoot(@RegExpCtorV); AddRoot(@JSFns);
  New(Legacy);
  AddRoot(@Legacy^.MatchStr); AddRoot(@Legacy^.InputStr);
  Legacy^.State := 0;
  Legacy^.MatchStr := VUndefined;
  Legacy^.InputStr := VUndefined;
  AtomLastIndex := AtomOf('lastIndex');
  AtomIndex := AtomOf('index');
  AtomInput := AtomOf('input');
  AtomGroups := AtomOf('groups');
  RegExpProto := MkPtr(NewPlainObject(ObjectProto, 16));
  C := NewNativeFunction(Re_Ctor, 'RegExp', 2);
  Scratch := C;
  RegExpCtorV := C;
  DefineOwn(Scratch, AtomPrototype, RegExpProto, 0);
  DefineOwn(RegExpProto, AtomConstructor, Scratch, pfHidden);
  DefineOwn(GlobalObj, AtomOf('RegExp'), Scratch, pfHidden);
  for I := 0 to 18 do
  begin
    case I of
      0, 1: begin LName := 'input'; if I = 1 then LName := '$_'; K := 0; end;
      2, 3: begin LName := 'lastMatch'; if I = 3 then LName := '$&'; K := 1; end;
      4, 5: begin LName := 'lastParen'; if I = 5 then LName := '$+'; K := 2; end;
      6, 7: begin LName := 'leftContext'; if I = 7 then LName := '$`'; K := 3; end;
      8, 9: begin LName := 'rightContext'; if I = 9 then LName := '$'''; K := 4; end;
    else begin LName := '$' + IntToStr(I - 9); K := I - 5; end;
    end;
    Getr := NewNativeFunctionData(Legacy_Get, 'get ' + LName, 0, K);
    PushRoot(Getr);
    if K = 0 then Setr := NewNativeFunction(Legacy_SetInput, 'set ' + LName, 1) else Setr := VUndefined;
    Getr := MkPtr(NewAccessor(RootAt(TempTop - 1), Setr));
    PopRoots(1);
    DefineOwn(Scratch, AtomOf(LName), Getr, pfConfigurable or pfAccessor);
  end;
  DefineNative(RegExpProto, 'exec', Re_Exec, 1);
  DefineNative(RegExpProto, 'test', Re_Test, 1);
  Getter('dotAll', Re_DotAll);
  Getter('global', Re_Global);
  Getter('hasIndices', Re_HasIndices);
  Getter('ignoreCase', Re_IgnoreCase);
  Getter('multiline', Re_Multiline);
  Getter('source', Re_Source);
  Getter('sticky', Re_Sticky);
  Getter('unicode', Re_Unicode);
  Getter('unicodeSets', Re_UnicodeSets);
  { String methods learn RegExp arguments; the string versions stay the fallback }
  StrProto_ := StringProto;
  if not GetV(StrProto_, AtomOf('replace'), StrReplaceOrig) then Pending := VUndefined;
  if not GetV(StrProto_, AtomOf('split'), StrSplitOrig) then Pending := VUndefined;
  if not GetV(StrProto_, AtomOf('replaceAll'), StrReplaceAllOrig) then Pending := VUndefined;
  DefineNative(StringProto, 'replace', Str_Replace, 2);
  DefineNative(StringProto, 'replaceAll', Str_ReplaceAll, 2);
  DefineNative(StringProto, 'split', Str_Split, 2);
  DefineNative(StringProto, 'match', Str_Match, 1);
  DefineNative(StringProto, 'matchAll', Str_MatchAll, 1);
  DefineNative(StringProto, 'search', Str_Search, 1);
  RegExpCreateHook := CreateRegExp;
  RegExpValidateHook := ValidateLiteral;
end;

{ the prelude (regexp.js): run once every other built-in exists; then the originals the fast paths check }
procedure InitRegExpPrelude;
var T: PFuncTemplate; Err: UnicodeString; Clo, Fn, R, NObj, V: TValue; I, S: Integer; CA: array[0..1] of TValue;
const
  JSNames: array[0..7] of UnicodeString = ('construct', 'test', 'match', 'matchAll', 'replace', 'replaceAll',
    'search', 'split');
  procedure Nat(const Name: UnicodeString; F: TNativeFn; Len: Integer);
  begin
    V := NewNativeFunction(F, Name, Len);
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), V, pfDefault);
  end;
  procedure Val(const Name: UnicodeString; X: TValue);
  begin
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), X, pfDefault);
  end;
begin
  if not CompilePrelude('<regexp>', RegExpSrc, T, Err) then raise Exception.Create('regexp prelude: ' + string(Err));
  if not MakeScriptClosure(T, Clo) then raise Exception.Create('regexp prelude closure');
  if not CallValue(Clo, VUndefined, nil, 0, Fn) then raise Exception.Create('regexp prelude run');
  PushRoot(Fn);
  NObj := MkPtr(NewPlainObject(ObjectProto, 16));
  PushRoot(NObj);
  Nat('exec', N_Exec, 2);
  Nat('isRE', N_IsRE, 1);
  Nat('alloc', N_Alloc, 1);
  Nat('init', N_Init, 3);
  Nat('source', N_Source, 1);
  Nat('flags', N_Flags, 1);
  Nat('legacy', N_Legacy, 1);
  Val('proto', RegExpProto);
  Val('ctor', RegExpCtorV);
  Val('strReplace', StrReplaceOrig);
  Val('strReplaceAll', StrReplaceAllOrig);
  Val('strSplit', StrSplitOrig);
  CA[0] := GlobalObj;
  CA[1] := RootAt(TempTop - 1);
  if not CallValue(RootAt(TempTop - 2), VUndefined, @CA[0], 2, R) then
  begin
    PopRoots(2);
    if ToStringV(Pending, R) and IsString(R) then
      raise Exception.Create('regexp prelude call: ' + string(StrToUnicode(StrOf(R))));
    raise Exception.Create('regexp prelude call');
  end;
  PopRoots(2);
  PushRoot(R);
  JSFns := MkPtr(NewArray(8, 8));
  for I := 0 to 7 do
  begin
    if not GetV(RootAt(TempTop - 1), AtomOf(JSNames[I]), V) then raise Exception.Create('regexp prelude result');
    ValueArrayStore(ObjOf(JSFns)^.Elements, I, V);
  end;
  PopRoots(1);
  { the keys whose slots on RegExp.prototype the fast paths depend on, and their values now }
  SetLength(IntactKeys, IntactCount);
  SetLength(IntactSlots, IntactCount);
  IntactKeys[0] := AtomOf('exec'); IntactKeys[1] := AtomOf('flags'); IntactKeys[2] := AtomOf('global');
  IntactKeys[3] := AtomOf('ignoreCase'); IntactKeys[4] := AtomOf('multiline'); IntactKeys[5] := AtomOf('dotAll');
  IntactKeys[6] := AtomOf('unicode'); IntactKeys[7] := AtomOf('unicodeSets'); IntactKeys[8] := AtomOf('sticky');
  IntactKeys[9] := AtomOf('hasIndices'); IntactKeys[10] := SymKey(SymMatch); IntactKeys[11] := SymKey(SymMatchAll);
  IntactKeys[12] := SymKey(SymReplace); IntactKeys[13] := SymKey(SymSearch); IntactKeys[14] := SymKey(SymSplit);
  IntactKeys[15] := AtomConstructor;
  IntactOrig := MkPtr(NewArray(IntactCount, IntactCount));
  for I := 0 to IntactCount - 1 do
  begin
    S := ShapeLookup(ObjOf(RegExpProto)^.Shape, IntactKeys[I]);
    if S < 0 then raise Exception.Create('regexp prelude: RegExp.prototype lacks ' + string(StrToUnicode(IntactKeys[I])));
    ValueArrayStore(ObjOf(IntactOrig)^.Elements, I, GetSlot(ObjOf(RegExpProto), S));
  end;
  IntactShape := nil;
  S := ShapeLookup(ObjOf(RegExpCtorV)^.Shape, SymKey(SymSpecies));
  if S >= 0 then SpeciesOrig := GetSlot(ObjOf(RegExpCtorV), S) else SpeciesOrig := VUndefined;
end;

end.
