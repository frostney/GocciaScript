unit Goccia.Compiler.Scope;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  OrderedStringMap,

  Goccia.Bytecode.Chunk,
  Goccia.Compiler.ConstantValue,
  Goccia.Modules;

type
  TGocciaCompilerVariableKind = (cvkLocal, cvkUpvalue, cvkGlobal);

  TGocciaCompilerLocal = record
    Name: string;
    Slot: UInt8;
    Depth: Integer;
    IsCaptured: Boolean;
    IsConst: Boolean;
    IsVar: Boolean;
    IsGlobalBacked: Boolean;
    IsArrayTyped: Boolean;
    TypeHint: TGocciaLocalType;
    IsStrictlyTyped: Boolean;
    ReturnTypeHint: TGocciaLocalType;
    ParamTypeSignature: string;
    TypeAnnotation: string;
    ElementTypeAnnotation: string;
    HasConstantValue: Boolean;
    ConstantValue: TGocciaCompileTimeValue;
    IsImportBinding: Boolean;
    ImportPhase: TGocciaImportCallPhase;
    ImportModulePath: string;
    ImportExportName: string;
    ExportNames: array of string;
    ExportNameCount: Integer;
  end;

  TGocciaCompilerUpvalue = record
    Name: string;
    Index: UInt8;
    IsLocal: Boolean;
    IsConst: Boolean;
    IsVar: Boolean;
    IsGlobalBacked: Boolean;
    TypeHint: TGocciaLocalType;
    IsStrictlyTyped: Boolean;
    ReturnTypeHint: TGocciaLocalType;
    ParamTypeSignature: string;
    IsImportBinding: Boolean;
    ImportPhase: TGocciaImportCallPhase;
    ImportModulePath: string;
    ImportExportName: string;
    ExportNames: array of string;
    ExportNameCount: Integer;
  end;

  TGocciaCompilerScope = class
  private
    FParent: TGocciaCompilerScope;
    FLocals: array of TGocciaCompilerLocal;
    FLocalIndex: TOrderedStringMap<Integer>;
    FLocalCount: Integer;
    FUpvalues: array of TGocciaCompilerUpvalue;
    FUpvalueCount: Integer;
    FDepth: Integer;
    FNextSlot: UInt8;
    FMaxSlot: UInt8;
    FPrivatePrefix: string;
    FPrivateNames: array of string;
    FPrivatePrefixes: array of string;
    FPrivateNameCount: Integer;
    FIsArrow: Boolean;
    FDirectEvalSyntheticArgumentsSlot: Integer;
    FWithBindingNames: array of string;
    FWithBindingDepths: array of Integer;
    FWithBindingCount: Integer;
    procedure EnsureLocalIndex;
    procedure RestoreLocalIndexBinding(const ARemovedName: string);
  public
    constructor Create(const AParent: TGocciaCompilerScope;
      const ADepth: Integer);
    destructor Destroy; override;

    function DeclareLocal(const AName: string;
      const AIsConst: Boolean): UInt8;
    function DeclareVarLocal(const AName: string): UInt8;
    function ResolveLocal(const AName: string): Integer;
    function ResolveUpvalue(const AName: string): Integer;
    function AddUpvalue(const AName: string; const AIndex: UInt8;
      const AIsLocal: Boolean; const AIsConst: Boolean = False;
      const AIsVar: Boolean = False): Integer;

    function AllocateRegister: UInt8;
    procedure FreeRegister;
    procedure BeginScope;
    procedure EndScope(out AClosedLocals: array of UInt8;
      out AClosedCount: Integer);

    property Parent: TGocciaCompilerScope read FParent;
    property LocalCount: Integer read FLocalCount;
    property UpvalueCount: Integer read FUpvalueCount;
    property Depth: Integer read FDepth;
    property MaxSlot: UInt8 read FMaxSlot;
    property NextSlot: UInt8 read FNextSlot;

    function GetLocal(const AIndex: Integer): TGocciaCompilerLocal;
    function GetUpvalue(const AIndex: Integer): TGocciaCompilerUpvalue;
    procedure MarkCaptured(const AIndex: Integer);
    procedure MarkGlobalBacked(const AIndex: Integer);
    procedure SetLocalTypeHint(const AIndex: Integer;
      const ATypeHint: TGocciaLocalType);
    procedure SetLocalStrictlyTyped(const AIndex: Integer;
      const AStrictlyTyped: Boolean);
    procedure SetLocalArrayTyped(const AIndex: Integer;
      const AArrayTyped: Boolean);
    procedure SetLocalReturnTypeHint(const AIndex: Integer;
      const AReturnTypeHint: TGocciaLocalType);
    procedure SetLocalParamTypeSignature(const AIndex: Integer;
      const ASignature: string);
    procedure SetLocalTypeAnnotation(const AIndex: Integer;
      const AAnnotation: string);
    procedure SetLocalElementTypeAnnotation(const AIndex: Integer;
      const AAnnotation: string);
    procedure SetLocalConstantValue(const AIndex: Integer;
      const AValue: TGocciaCompileTimeValue);
    procedure ClearLocalConstantValue(const AIndex: Integer);
    procedure MarkImportBinding(const AIndex: Integer;
      const APhase: TGocciaImportCallPhase; const AModulePath,
      AExportName: string);
    procedure MarkExportBinding(const AIndex: Integer;
      const AExportName: string);
    function TryGetVisibleConstantValue(const AName: string;
      out AValue: TGocciaCompileTimeValue): Boolean;
    function HasVisibleLocal(const AName: string): Boolean;
    function ResolvePrivatePrefix: string;
    function ResolvePrivatePrefixForName(const AName: string): string;
    procedure DeclarePrivateNamePrefix(const AName, APrefix: string);
    function PrivateNameMark: Integer;
    procedure RestorePrivateNameMark(const AMark: Integer);
    procedure PushWithBinding(const AHiddenName: string);
    procedure PopWithBinding;
    function GetWithBindingName(const AIndex: Integer): string;
    function GetWithBindingDepth(const AIndex: Integer): Integer;
    property PrivatePrefix: string read FPrivatePrefix write FPrivatePrefix;
    property IsArrow: Boolean read FIsArrow write FIsArrow;
    property DirectEvalSyntheticArgumentsSlot: Integer read FDirectEvalSyntheticArgumentsSlot write FDirectEvalSyntheticArgumentsSlot;
    property WithBindingCount: Integer read FWithBindingCount;
  end;

function NextClassPrivatePrefix: string;

implementation

uses
  SysUtils;

const
  COMPILER_LOCAL_INDEX_THRESHOLD = 64;
  COMPILER_SUPER_LOCAL = '__super__';

{ TGocciaCompilerScope }

procedure TGocciaCompilerScope.EnsureLocalIndex;
var
  I: Integer;
begin
  if Assigned(FLocalIndex) then
    Exit;
  if FLocalCount < COMPILER_LOCAL_INDEX_THRESHOLD then
    Exit;

  FLocalIndex := TOrderedStringMap<Integer>.Create(FLocalCount * 2);
  for I := 0 to FLocalCount - 1 do
    FLocalIndex.Add(FLocals[I].Name, I);
end;

procedure TGocciaCompilerScope.RestoreLocalIndexBinding(
  const ARemovedName: string);
var
  I: Integer;
begin
  if not Assigned(FLocalIndex) then
    Exit;

  FLocalIndex.Remove(ARemovedName);
  for I := FLocalCount - 1 downto 0 do
    if FLocals[I].Name = ARemovedName then
    begin
      FLocalIndex.Add(ARemovedName, I);
      Break;
    end;

  if FLocalCount < COMPILER_LOCAL_INDEX_THRESHOLD then
    FreeAndNil(FLocalIndex);
end;

constructor TGocciaCompilerScope.Create(const AParent: TGocciaCompilerScope;
  const ADepth: Integer);
var
  I: Integer;
begin
  inherited Create;
  FParent := AParent;
  FLocalIndex := nil;
  FLocalCount := 0;
  FUpvalueCount := 0;
  FDepth := ADepth;
  FNextSlot := 0;
  FMaxSlot := 0;
  FPrivateNameCount := 0;
  FDirectEvalSyntheticArgumentsSlot := -1;
  FWithBindingCount := 0;
  if Assigned(AParent) and (AParent.FWithBindingCount > 0) then
  begin
    FWithBindingCount := AParent.FWithBindingCount;
    SetLength(FWithBindingNames, FWithBindingCount);
    SetLength(FWithBindingDepths, FWithBindingCount);
    for I := 0 to FWithBindingCount - 1 do
    begin
      FWithBindingNames[I] := AParent.FWithBindingNames[I];
      FWithBindingDepths[I] := -1;
    end;
  end;
end;

destructor TGocciaCompilerScope.Destroy;
begin
  FLocalIndex.Free;
  inherited;
end;

function TGocciaCompilerScope.DeclareLocal(const AName: string;
  const AIsConst: Boolean): UInt8;
begin
  if FNextSlot >= High(UInt8) then
    raise Exception.Create('Compiler error: local variable slot overflow (>255)');
  if FLocalCount >= Length(FLocals) then
    SetLength(FLocals, FLocalCount * 2 + 8);
  FLocals[FLocalCount].Name := AName;
  FLocals[FLocalCount].Slot := FNextSlot;
  FLocals[FLocalCount].Depth := FDepth;
  FLocals[FLocalCount].IsCaptured := False;
  FLocals[FLocalCount].IsConst := AIsConst;
  FLocals[FLocalCount].IsVar := False;
  FLocals[FLocalCount].IsGlobalBacked := False;
  FLocals[FLocalCount].IsArrayTyped := False;
  FLocals[FLocalCount].TypeHint := sltUntyped;
  FLocals[FLocalCount].IsStrictlyTyped := False;
  FLocals[FLocalCount].ReturnTypeHint := sltUntyped;
  FLocals[FLocalCount].ParamTypeSignature := '';
  FLocals[FLocalCount].TypeAnnotation := '';
  FLocals[FLocalCount].ElementTypeAnnotation := '';
  FLocals[FLocalCount].HasConstantValue := False;
  FLocals[FLocalCount].ConstantValue := UnknownCompileTimeValue;
  FLocals[FLocalCount].IsImportBinding := False;
  FLocals[FLocalCount].ImportPhase := icpEvaluation;
  FLocals[FLocalCount].ImportModulePath := '';
  FLocals[FLocalCount].ImportExportName := '';
  FLocals[FLocalCount].ExportNameCount := 0;
  SetLength(FLocals[FLocalCount].ExportNames, 0);
  Result := FNextSlot;
  EnsureLocalIndex;
  if Assigned(FLocalIndex) then
    FLocalIndex.Add(AName, FLocalCount);
  Inc(FLocalCount);
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := FNextSlot;
end;

function TGocciaCompilerScope.DeclareVarLocal(const AName: string): UInt8;
var
  I: Integer;
begin
  // Check if already declared at var/function scope (var redeclaration is allowed).
  // Block-scoped declarations with the same name must not capture var writes.
  for I := 0 to FLocalCount - 1 do
    if (FLocals[I].Name = AName) and (FLocals[I].Depth = 0) then
    begin
      if FLocals[I].Slot = FDirectEvalSyntheticArgumentsSlot then
        Continue;
      Exit(FLocals[I].Slot);
    end;

  // Declare at depth 0 so EndScope never pops it
  if FNextSlot >= High(UInt8) then
    raise Exception.Create('Compiler error: local variable slot overflow (>255)');
  if FLocalCount >= Length(FLocals) then
    SetLength(FLocals, FLocalCount * 2 + 8);
  FLocals[FLocalCount].Name := AName;
  FLocals[FLocalCount].Slot := FNextSlot;
  FLocals[FLocalCount].Depth := 0;
  FLocals[FLocalCount].IsCaptured := False;
  FLocals[FLocalCount].IsConst := False;
  FLocals[FLocalCount].IsVar := True;
  FLocals[FLocalCount].IsGlobalBacked := False;
  FLocals[FLocalCount].IsArrayTyped := False;
  FLocals[FLocalCount].TypeHint := sltUntyped;
  FLocals[FLocalCount].IsStrictlyTyped := False;
  FLocals[FLocalCount].ReturnTypeHint := sltUntyped;
  FLocals[FLocalCount].ParamTypeSignature := '';
  FLocals[FLocalCount].TypeAnnotation := '';
  FLocals[FLocalCount].ElementTypeAnnotation := '';
  FLocals[FLocalCount].HasConstantValue := False;
  FLocals[FLocalCount].ConstantValue := UnknownCompileTimeValue;
  FLocals[FLocalCount].IsImportBinding := False;
  FLocals[FLocalCount].ImportPhase := icpEvaluation;
  FLocals[FLocalCount].ImportModulePath := '';
  FLocals[FLocalCount].ImportExportName := '';
  FLocals[FLocalCount].ExportNameCount := 0;
  SetLength(FLocals[FLocalCount].ExportNames, 0);
  Result := FNextSlot;
  EnsureLocalIndex;
  if Assigned(FLocalIndex) then
    FLocalIndex.Add(AName, FLocalCount);
  Inc(FLocalCount);
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := FNextSlot;
end;

function TGocciaCompilerScope.ResolveLocal(const AName: string): Integer;
var
  I: Integer;
begin
  if Assigned(FLocalIndex) then
  begin
    if FLocalIndex.TryGetValue(AName, Result) then
      Exit;
    Exit(-1);
  end;

  for I := FLocalCount - 1 downto 0 do
    if FLocals[I].Name = AName then
      Exit(I);
  Result := -1;
end;

function TGocciaCompilerScope.ResolveUpvalue(const AName: string): Integer;
var
  LocalIdx: Integer;
  UpvalueIdx: Integer;
  Idx: Integer;
begin
  if not Assigned(FParent) then
    Exit(-1);

  LocalIdx := FParent.ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    FParent.MarkCaptured(LocalIdx);
    Idx := AddUpvalue(AName, FParent.FLocals[LocalIdx].Slot, True,
      FParent.FLocals[LocalIdx].IsConst, FParent.FLocals[LocalIdx].IsVar);
    FUpvalues[Idx].IsGlobalBacked := FParent.FLocals[LocalIdx].IsGlobalBacked;
    FUpvalues[Idx].TypeHint := FParent.FLocals[LocalIdx].TypeHint;
    FUpvalues[Idx].IsStrictlyTyped := FParent.FLocals[LocalIdx].IsStrictlyTyped;
    FUpvalues[Idx].ReturnTypeHint := FParent.FLocals[LocalIdx].ReturnTypeHint;
    FUpvalues[Idx].ParamTypeSignature := FParent.FLocals[LocalIdx].ParamTypeSignature;
    FUpvalues[Idx].IsImportBinding := FParent.FLocals[LocalIdx].IsImportBinding;
    FUpvalues[Idx].ImportPhase := FParent.FLocals[LocalIdx].ImportPhase;
    FUpvalues[Idx].ImportModulePath := FParent.FLocals[LocalIdx].ImportModulePath;
    FUpvalues[Idx].ImportExportName := FParent.FLocals[LocalIdx].ImportExportName;
    FUpvalues[Idx].ExportNames := Copy(FParent.FLocals[LocalIdx].ExportNames);
    FUpvalues[Idx].ExportNameCount := FParent.FLocals[LocalIdx].ExportNameCount;
    Exit(Idx);
  end;

  if not FIsArrow and (AName = COMPILER_SUPER_LOCAL) then
    Exit(-1);

  UpvalueIdx := FParent.ResolveUpvalue(AName);
  if UpvalueIdx >= 0 then
  begin
    if UpvalueIdx > High(UInt8) then
      raise Exception.Create('Compiler error: upvalue index overflow (>255)');
    Idx := AddUpvalue(AName, UInt8(UpvalueIdx), False,
      FParent.FUpvalues[UpvalueIdx].IsConst,
      FParent.FUpvalues[UpvalueIdx].IsVar);
    FUpvalues[Idx].IsGlobalBacked := FParent.FUpvalues[UpvalueIdx].IsGlobalBacked;
    FUpvalues[Idx].TypeHint := FParent.FUpvalues[UpvalueIdx].TypeHint;
    FUpvalues[Idx].IsStrictlyTyped := FParent.FUpvalues[UpvalueIdx].IsStrictlyTyped;
    FUpvalues[Idx].ReturnTypeHint := FParent.FUpvalues[UpvalueIdx].ReturnTypeHint;
    FUpvalues[Idx].ParamTypeSignature := FParent.FUpvalues[UpvalueIdx].ParamTypeSignature;
    FUpvalues[Idx].IsImportBinding := FParent.FUpvalues[UpvalueIdx].IsImportBinding;
    FUpvalues[Idx].ImportPhase := FParent.FUpvalues[UpvalueIdx].ImportPhase;
    FUpvalues[Idx].ImportModulePath := FParent.FUpvalues[UpvalueIdx].ImportModulePath;
    FUpvalues[Idx].ImportExportName := FParent.FUpvalues[UpvalueIdx].ImportExportName;
    FUpvalues[Idx].ExportNames := Copy(FParent.FUpvalues[UpvalueIdx].ExportNames);
    FUpvalues[Idx].ExportNameCount := FParent.FUpvalues[UpvalueIdx].ExportNameCount;
    Exit(Idx);
  end;

  Result := -1;
end;

function TGocciaCompilerScope.AddUpvalue(const AName: string; const AIndex: UInt8;
  const AIsLocal: Boolean; const AIsConst: Boolean;
  const AIsVar: Boolean): Integer;
var
  I: Integer;
begin
  for I := 0 to FUpvalueCount - 1 do
    if (FUpvalues[I].Index = AIndex) and (FUpvalues[I].IsLocal = AIsLocal) then
      Exit(I);

  if FUpvalueCount >= High(UInt8) then
    raise Exception.Create('Compiler error: upvalue count overflow (>255)');
  if FUpvalueCount >= Length(FUpvalues) then
    SetLength(FUpvalues, FUpvalueCount * 2 + 4);
  FUpvalues[FUpvalueCount].Name := AName;
  FUpvalues[FUpvalueCount].Index := AIndex;
  FUpvalues[FUpvalueCount].IsLocal := AIsLocal;
  FUpvalues[FUpvalueCount].IsConst := AIsConst;
  FUpvalues[FUpvalueCount].IsVar := AIsVar;
  FUpvalues[FUpvalueCount].IsGlobalBacked := False;
  FUpvalues[FUpvalueCount].TypeHint := sltUntyped;
  FUpvalues[FUpvalueCount].IsStrictlyTyped := False;
  FUpvalues[FUpvalueCount].ReturnTypeHint := sltUntyped;
  FUpvalues[FUpvalueCount].ParamTypeSignature := '';
  FUpvalues[FUpvalueCount].IsImportBinding := False;
  FUpvalues[FUpvalueCount].ImportPhase := icpEvaluation;
  FUpvalues[FUpvalueCount].ImportModulePath := '';
  FUpvalues[FUpvalueCount].ImportExportName := '';
  FUpvalues[FUpvalueCount].ExportNameCount := 0;
  SetLength(FUpvalues[FUpvalueCount].ExportNames, 0);
  Result := FUpvalueCount;
  Inc(FUpvalueCount);
end;

function TGocciaCompilerScope.AllocateRegister: UInt8;
begin
  if FNextSlot >= High(UInt8) then
    raise Exception.Create('Compiler error: register slot overflow (>255)');
  Result := FNextSlot;
  Inc(FNextSlot);
  if FNextSlot > FMaxSlot then
    FMaxSlot := FNextSlot;
end;

procedure TGocciaCompilerScope.FreeRegister;
begin
  if FNextSlot > 0 then
    Dec(FNextSlot);
end;

procedure TGocciaCompilerScope.BeginScope;
begin
  Inc(FDepth);
end;

procedure TGocciaCompilerScope.EndScope(out AClosedLocals: array of UInt8;
  out AClosedCount: Integer);
var
  RemovedName: string;
begin
  AClosedCount := 0;
  while (FLocalCount > 0) and (FLocals[FLocalCount - 1].Depth = FDepth) do
  begin
    RemovedName := FLocals[FLocalCount - 1].Name;
    Dec(FLocalCount);
    if FLocals[FLocalCount].IsCaptured then
    begin
      if AClosedCount >= Length(AClosedLocals) then
        raise Exception.Create('AClosedLocals buffer too small for captured locals');
      AClosedLocals[AClosedCount] := FLocals[FLocalCount].Slot;
      Inc(AClosedCount);
    end;
    Dec(FNextSlot);
    RestoreLocalIndexBinding(RemovedName);
  end;
  Dec(FDepth);
end;

function TGocciaCompilerScope.GetLocal(
  const AIndex: Integer): TGocciaCompilerLocal;
begin
  Result := FLocals[AIndex];
end;

function TGocciaCompilerScope.GetUpvalue(
  const AIndex: Integer): TGocciaCompilerUpvalue;
begin
  Result := FUpvalues[AIndex];
end;

procedure TGocciaCompilerScope.MarkCaptured(const AIndex: Integer);
begin
  FLocals[AIndex].IsCaptured := True;
end;

procedure TGocciaCompilerScope.MarkGlobalBacked(const AIndex: Integer);
begin
  FLocals[AIndex].IsGlobalBacked := True;
end;

procedure TGocciaCompilerScope.SetLocalTypeHint(const AIndex: Integer;
  const ATypeHint: TGocciaLocalType);
begin
  FLocals[AIndex].TypeHint := ATypeHint;
end;

procedure TGocciaCompilerScope.SetLocalReturnTypeHint(const AIndex: Integer;
  const AReturnTypeHint: TGocciaLocalType);
begin
  FLocals[AIndex].ReturnTypeHint := AReturnTypeHint;
end;

procedure TGocciaCompilerScope.SetLocalParamTypeSignature(const AIndex: Integer;
  const ASignature: string);
begin
  FLocals[AIndex].ParamTypeSignature := ASignature;
end;

procedure TGocciaCompilerScope.SetLocalStrictlyTyped(const AIndex: Integer;
  const AStrictlyTyped: Boolean);
begin
  FLocals[AIndex].IsStrictlyTyped := AStrictlyTyped;
end;

procedure TGocciaCompilerScope.SetLocalArrayTyped(const AIndex: Integer;
  const AArrayTyped: Boolean);
begin
  FLocals[AIndex].IsArrayTyped := AArrayTyped;
end;

procedure TGocciaCompilerScope.SetLocalTypeAnnotation(const AIndex: Integer;
  const AAnnotation: string);
begin
  FLocals[AIndex].TypeAnnotation := AAnnotation;
end;

procedure TGocciaCompilerScope.SetLocalElementTypeAnnotation(const AIndex: Integer;
  const AAnnotation: string);
begin
  FLocals[AIndex].ElementTypeAnnotation := AAnnotation;
end;

procedure TGocciaCompilerScope.SetLocalConstantValue(const AIndex: Integer;
  const AValue: TGocciaCompileTimeValue);
begin
  FLocals[AIndex].HasConstantValue := True;
  FLocals[AIndex].ConstantValue := AValue;
end;

procedure TGocciaCompilerScope.ClearLocalConstantValue(const AIndex: Integer);
begin
  FLocals[AIndex].HasConstantValue := False;
  FLocals[AIndex].ConstantValue := UnknownCompileTimeValue;
end;

procedure TGocciaCompilerScope.MarkImportBinding(const AIndex: Integer;
  const APhase: TGocciaImportCallPhase; const AModulePath,
  AExportName: string);
begin
  FLocals[AIndex].IsImportBinding := True;
  FLocals[AIndex].ImportPhase := APhase;
  FLocals[AIndex].ImportModulePath := AModulePath;
  FLocals[AIndex].ImportExportName := AExportName;
end;

procedure TGocciaCompilerScope.MarkExportBinding(const AIndex: Integer;
  const AExportName: string);
var
  I: Integer;
begin
  for I := 0 to FLocals[AIndex].ExportNameCount - 1 do
    if FLocals[AIndex].ExportNames[I] = AExportName then
      Exit;

  if FLocals[AIndex].ExportNameCount >= Length(FLocals[AIndex].ExportNames) then
    SetLength(FLocals[AIndex].ExportNames,
      FLocals[AIndex].ExportNameCount * 2 + 2);
  FLocals[AIndex].ExportNames[FLocals[AIndex].ExportNameCount] :=
    AExportName;
  Inc(FLocals[AIndex].ExportNameCount);
end;

function TGocciaCompilerScope.TryGetVisibleConstantValue(const AName: string;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  LocalIdx: Integer;
begin
  LocalIdx := ResolveLocal(AName);
  if LocalIdx >= 0 then
  begin
    Result := FLocals[LocalIdx].HasConstantValue;
    if Result then
      AValue := FLocals[LocalIdx].ConstantValue
    else
      AValue := UnknownCompileTimeValue;
    Exit;
  end;

  if Assigned(FParent) then
    Exit(FParent.TryGetVisibleConstantValue(AName, AValue));

  AValue := UnknownCompileTimeValue;
  Result := False;
end;

function TGocciaCompilerScope.HasVisibleLocal(const AName: string): Boolean;
begin
  if ResolveLocal(AName) >= 0 then
    Exit(True);

  Result := Assigned(FParent) and FParent.HasVisibleLocal(AName);
end;

function TGocciaCompilerScope.ResolvePrivatePrefix: string;
var
  S: TGocciaCompilerScope;
begin
  S := Self;
  while Assigned(S) do
  begin
    if S.FPrivatePrefix <> '' then
      Exit(S.FPrivatePrefix);
    S := S.FParent;
  end;
  Result := '';
end;

function TGocciaCompilerScope.ResolvePrivatePrefixForName(
  const AName: string): string;
var
  I: Integer;
begin
  for I := FPrivateNameCount - 1 downto 0 do
    if FPrivateNames[I] = AName then
      Exit(FPrivatePrefixes[I]);

  if Assigned(FParent) then
    Exit(FParent.ResolvePrivatePrefixForName(AName));

  Result := ResolvePrivatePrefix;
end;

procedure TGocciaCompilerScope.DeclarePrivateNamePrefix(const AName,
  APrefix: string);
begin
  if FPrivateNameCount >= Length(FPrivateNames) then
  begin
    SetLength(FPrivateNames, FPrivateNameCount * 2 + 8);
    SetLength(FPrivatePrefixes, FPrivateNameCount * 2 + 8);
  end;
  FPrivateNames[FPrivateNameCount] := AName;
  FPrivatePrefixes[FPrivateNameCount] := APrefix;
  Inc(FPrivateNameCount);
end;

function TGocciaCompilerScope.PrivateNameMark: Integer;
begin
  Result := FPrivateNameCount;
end;

procedure TGocciaCompilerScope.RestorePrivateNameMark(const AMark: Integer);
begin
  if AMark < 0 then
    Exit;
  if AMark < FPrivateNameCount then
    FPrivateNameCount := AMark;
end;

procedure TGocciaCompilerScope.PushWithBinding(const AHiddenName: string);
begin
  if FWithBindingCount >= Length(FWithBindingNames) then
  begin
    SetLength(FWithBindingNames, FWithBindingCount * 2 + 4);
    SetLength(FWithBindingDepths, FWithBindingCount * 2 + 4);
  end;
  FWithBindingNames[FWithBindingCount] := AHiddenName;
  FWithBindingDepths[FWithBindingCount] := FDepth;
  Inc(FWithBindingCount);
end;

procedure TGocciaCompilerScope.PopWithBinding;
begin
  if FWithBindingCount > 0 then
    Dec(FWithBindingCount);
end;

function TGocciaCompilerScope.GetWithBindingName(
  const AIndex: Integer): string;
begin
  Result := FWithBindingNames[AIndex];
end;

function TGocciaCompilerScope.GetWithBindingDepth(
  const AIndex: Integer): Integer;
begin
  Result := FWithBindingDepths[AIndex];
end;

threadvar
  GClassPrivateCounter: Integer;

function NextClassPrivatePrefix: string;
begin
  Result := IntToStr(GClassPrivateCounter) + '$';
  Inc(GClassPrivateCounter);
end;

end.
