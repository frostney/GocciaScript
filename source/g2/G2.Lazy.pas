unit G2.Lazy;
{ Lazy compilation (the default; G2_LAZY=0 turns it off): a Script is pre-parsed (G2.PreParse: syntax, early
  errors, every function's free names), then Goccia's parser builds the AST with the bodies of the eligible
  functions skipped (TGocciaLazyFunctionBody), and G2.Compiler makes each a stub template: its header, and
  upvalues for the names the pre-parse found it uses. The first call compiles it (G2.Eval's LazyCompile):
  Goccia's parser on the function's own source range, compiled with the stub's upvalues as its outer environment.
  A TLazySource is one pre-parsed source: its text, the line index Goccia's lexer built for it, and per function
  record the offsets and what the compiler needs. Offsets are UTF-16 indices, Goccia's as G2.Scan's. }
{$mode delphiunicode}{$H+}

interface

uses
  SysUtils, Goccia.SourceSpan, G2.Core, G2.PreParse;

type
  TLazyRec = record
    Start, Stop, Body, Close: Integer;   { 0-based offsets; Close the body's right brace }
    Flags: TFuncFlags;
    Name: UnicodeString;
    Free: array of UnicodeString;
  end;

  TLazySource = class
  public
    Text: UnicodeString;
    FileName: UnicodeString;
    Coords: TGocciaSourceCoordinates;
    CoordsRef: IGocciaSourceCoordinates;
    Recs: array of TLazyRec;
    { body offset -> record index (open addressing), for the eligible records only }
    BodyKeys, BodyVals: array of Integer;
    BodyMask: Integer;
    { the record a lazy compile is parsing: its own body is not skipped }
    Compiling: Integer;
    function Lookup(Body: Integer): Integer;
  end;

  { TFuncTemplate.Lazy: a stub template's compilation, deferred }
  PLazyStub = ^TLazyStub;
  TLazyStub = record
    Src: TLazySource;
    Id: Integer;                 { its record in Src }
    CompileName: UnicodeString;  { the name CompileFunction was given }
    IsArrow, IsGenerator, IsAsync, IsStrict: Boolean;
    InDerivedCtor: Boolean;      { an arrow whose this is a derived constructor's (TDZ until super()) }
    { the stub's upvalues, in order: the captured names, and per name efConst / efLexical (a TDZ check needed) /
      efFnName }
    Names: array of UnicodeString;
    Flags: array of Integer;
  end;

{ whether a pre-parsed function can be compiled on its first call: not when names in it are dynamic (a direct eval
  in it, around it or inside it; a with around it), it uses private names, or its context is not reproducible from
  its own source (methods, accessors, constructors, an arrow's super, new.target or arguments), or its body is an
  expression }
function LazyEligible(const R: TFuncRecord): Boolean;
{ the pre-parse's functions; nil when no function is eligible }
function BuildLazySource(const Source, FileName: UnicodeString; P: TPreParser): TLazySource;
{ TGocciaParser's body skipper (Data: the TLazySource) }
function LazyBodySkipper(const AData: Pointer; const ABodyOffset: Integer; out ACloseOffset,
  ALazyId: Integer): Boolean;

threadvar
  { the source G2.Compiler is compiling with skipped bodies (their records), nil otherwise }
  CurLazySource: TLazySource;
  { G2_PHASES: the functions compiled on their first call, and the time that took }
  LazyCompileCount: Integer;
  LazyCompileNs, LazySetupNs, LazyParseNs, LazyTokens, LazyChars: Int64;

{ every TLazySource made by this thread's engine }
procedure FreeLazySources;

implementation

threadvar
  AllSources: array of TLazySource;
  SourceCount: Integer;

function LazyEligible(const R: TFuncRecord): Boolean;
begin
  Result := (R.BodyStart >= 0) and
    (R.Flags * [ffMethod, ffGetter, ffSetter, ffClassCtor, ffFieldInit, ffStaticBlock, ffDirectEval, ffInnerEval,
      ffEvalScope, ffInWith, ffPrivateRef] = []) and
    not ((ffArrow in R.Flags) and (R.Flags * [ffSuperProp, ffSuperCall, ffNewTarget, ffUsesArguments] <> []));
end;

function HashOffset(V: Integer): Integer; inline;
begin
  Result := Integer((Cardinal(V) * Cardinal(2654435769)) shr 8);
end;

function TLazySource.Lookup(Body: Integer): Integer;
var I: Integer;
begin
  if BodyMask = 0 then Exit(-1);
  I := HashOffset(Body) and BodyMask;
  while BodyKeys[I] <> -1 do
  begin
    if BodyKeys[I] = Body then Exit(BodyVals[I]);
    I := (I + 1) and BodyMask;
  end;
  Result := -1;
end;

function LazyBodySkipper(const AData: Pointer; const ABodyOffset: Integer; out ACloseOffset,
  ALazyId: Integer): Boolean;
var Src: TLazySource; Id: Integer;
begin
  Src := TLazySource(AData);
  Id := Src.Lookup(ABodyOffset);
  if (Id < 0) or (Id = Src.Compiling) then Exit(False);
  ACloseOffset := Src.Recs[Id].Close;
  ALazyId := Id;
  Result := True;
end;

function BuildLazySource(const Source, FileName: UnicodeString; P: TPreParser): TLazySource;
var I, J, K, Eligible: Integer; R: ^TLazyRec;
begin
  Result := nil;
  Eligible := 0;
  for I := 0 to P.FuncCount - 1 do
    if LazyEligible(P.Funcs[I]) then Inc(Eligible);
  if Eligible = 0 then Exit;
  Result := TLazySource.Create;
  Result.Text := Source;
  Result.FileName := FileName;
  Result.Compiling := -1;
  SetLength(Result.Recs, P.FuncCount);
  for I := 0 to P.FuncCount - 1 do
  begin
    R := @Result.Recs[I];
    R^.Flags := P.Funcs[I].Flags;
    if P.Funcs[I].Name <> nil then R^.Name := StrToUnicode(P.Funcs[I].Name);
    R^.Start := P.Funcs[I].Start;
    R^.Stop := P.Funcs[I].Stop;
    R^.Body := P.Funcs[I].BodyStart;
    R^.Close := P.Funcs[I].Stop - 1;
    if LazyEligible(P.Funcs[I]) then
    begin
      SetLength(R^.Free, P.Funcs[I].FreeCount);
      for J := 0 to P.Funcs[I].FreeCount - 1 do
        R^.Free[J] := StrToUnicode(P.FreeNames[P.Funcs[I].FreeStart + J]);
    end;
  end;
  K := 16;
  while K < Eligible * 2 do K := K * 2;
  SetLength(Result.BodyKeys, K);
  SetLength(Result.BodyVals, K);
  for I := 0 to K - 1 do Result.BodyKeys[I] := -1;
  Result.BodyMask := K - 1;
  for I := 0 to P.FuncCount - 1 do
    if LazyEligible(P.Funcs[I]) then
    begin
      J := HashOffset(Result.Recs[I].Body) and Result.BodyMask;
      while Result.BodyKeys[J] <> -1 do J := (J + 1) and Result.BodyMask;
      Result.BodyKeys[J] := Result.Recs[I].Body;
      Result.BodyVals[J] := I;
    end;
  if SourceCount = Length(AllSources) then SetLength(AllSources, SourceCount * 2 + 8);
  AllSources[SourceCount] := Result;
  Inc(SourceCount);
end;

procedure FreeLazySources;
var I: Integer;
begin
  for I := 0 to SourceCount - 1 do AllSources[I].Free;
  AllSources := nil;
  SourceCount := 0;
end;

end.
