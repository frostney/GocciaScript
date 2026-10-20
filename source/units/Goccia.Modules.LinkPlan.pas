unit Goccia.Modules.LinkPlan;

{$I Goccia.inc}

interface

uses
  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Modules;

type
  TGocciaLinkBindingKind = (lbLet, lbConst, lbVar);
  TGocciaLinkBinding = record
    Name: string;
    Kind: TGocciaLinkBindingKind;
  end;
  TGocciaLinkPair = record
    Name: string;
    Value: string;
  end;
  TGocciaLinkDeclarationKind = (ldImport, ldLocalExport, ldReExport);
  TGocciaLinkDeclaration = record
    Kind: TGocciaLinkDeclarationKind;
    ModulePath: string;
    AttributeType: string;
    Phase: TGocciaImportCallPhase;
    NamespaceName: string;
    IsStarExport: Boolean;
    Pairs: array of TGocciaLinkPair;
  end;

  { The declarations required to link compiled module code. Materialize only
    adapts these records to the loader's declaration interface: the resulting
    program must never be compiled or evaluated. }
  TGocciaModuleLinkPlan = class
  private
    procedure AddBinding(const AName: string; const AKind: TGocciaLinkBindingKind;
      const AVarBindings: TOrderedStringMap<Integer>);
  public
    Bindings: array of TGocciaLinkBinding;
    Declarations: array of TGocciaLinkDeclaration;
    HasTopLevelAwait: Boolean;
    constructor CreateFromProgram(const AProgram: TGocciaProgram);
    function Materialize: TGocciaProgram;
    procedure Validate;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,

  UnicodeStringList,

  Goccia.AST.BindingPatterns,
  Goccia.AST.Statements,
  Goccia.NativeLimits,
  Goccia.Scope.Declarations,
  Goccia.SourceSpan;

procedure TGocciaModuleLinkPlan.AddBinding(const AName: string;
  const AKind: TGocciaLinkBindingKind;
  const AVarBindings: TOrderedStringMap<Integer>);
var
  I: Integer;
begin
  { VarDeclaredNames includes top-level functions. Those are lexical module
    bindings, and the later declaration pass supplies their actual kind. }
  if (AKind <> lbVar) and AVarBindings.TryGetValue(AName, I) and
     (Bindings[I].Kind = lbVar) then
  begin
    Bindings[I].Kind := AKind;
    Exit;
  end;
  I := Length(Bindings);
  CheckedNativeByteSize(Int64(I) + 1, SizeOf(TGocciaLinkBinding));
  SetLength(Bindings, I + 1);
  Bindings[I].Name := AName;
  Bindings[I].Kind := AKind;
  if AKind = lbVar then
    AVarBindings.Add(AName, I);
end;

constructor TGocciaModuleLinkPlan.CreateFromProgram(const AProgram: TGocciaProgram);
var
  I, J: Integer;
  Stmt, Decl: TGocciaStatement;
  Names: TUnicodeStringList;
  VarBindings: TOrderedStringMap<Integer>;
  Kind: TGocciaLinkBindingKind;
  Item: TGocciaLinkDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  ReExportDecl: TGocciaReExportDeclaration;
  DefaultDecl: TGocciaExportDefaultDeclaration;
  IsExport: Boolean;

  procedure CopyPairs(const AMap: TStringStringMap);
  var
    Index: Integer;
    Entry: TStringStringMap.TKeyValuePair;
  begin
    CheckedNativeByteSize(AMap.Count, SizeOf(TGocciaLinkPair));
    SetLength(Item.Pairs, AMap.Count);
    Index := 0;
    for Entry in AMap do
    begin
      Item.Pairs[Index].Name := Entry.Key;
      Item.Pairs[Index].Value := Entry.Value;
      Inc(Index);
    end;
  end;

  procedure AppendDeclaration;
  var
    Index: Integer;
  begin
    Index := Length(Declarations);
    CheckedNativeByteSize(Int64(Index) + 1, SizeOf(TGocciaLinkDeclaration));
    SetLength(Declarations, Index + 1);
    Declarations[Index] := Item;
  end;
begin
  inherited Create;
  HasTopLevelAwait := AProgram.HasTopLevelAwait;
  Names := TUnicodeStringList.Create;
  VarBindings := nil;
  try
    VarBindings := TOrderedStringMap<Integer>.Create;
    CollectVarBindingNamesFromStatements(AProgram.Body, Names,
      VarBindingNameCollectionMode(False, False));
    for I := 0 to Names.Count - 1 do
      AddBinding(Names[I], lbVar, VarBindings);
    for I := 0 to AProgram.Body.Count - 1 do
    begin
      Stmt := AProgram.Body[I];
      Item := Default(TGocciaLinkDeclaration);
      if Stmt is TGocciaImportDeclaration then
      begin
        ImportDecl := TGocciaImportDeclaration(Stmt);
        Item.Kind := ldImport;
        Item.ModulePath := ImportDecl.ModulePath;
        Item.AttributeType := ImportDecl.AttributeType;
        Item.Phase := ImportDecl.Phase;
        Item.NamespaceName := ImportDecl.NamespaceName;
        CopyPairs(ImportDecl.Imports);
        AppendDeclaration;
        Continue;
      end;
      if Stmt is TGocciaReExportDeclaration then
      begin
        ReExportDecl := TGocciaReExportDeclaration(Stmt);
        Item.Kind := ldReExport;
        Item.ModulePath := ReExportDecl.ModulePath;
        Item.AttributeType := ReExportDecl.AttributeType;
        Item.NamespaceName := ReExportDecl.NamespaceName;
        Item.IsStarExport := ReExportDecl.IsStarExport;
        CopyPairs(ReExportDecl.ExportsTable);
        AppendDeclaration;
        Continue;
      end;
      if Stmt is TGocciaExportDeclaration then
      begin
        Item.Kind := ldLocalExport;
        CopyPairs(TGocciaExportDeclaration(Stmt).ExportsTable);
        AppendDeclaration;
        Continue;
      end;
      Names.Clear;
      Kind := lbLet;
      Decl := Stmt;
      IsExport := True;
      if Stmt is TGocciaExportVariableDeclaration then
        Decl := TGocciaExportVariableDeclaration(Stmt).Declaration
      else if Stmt is TGocciaExportDestructuringDeclaration then
        Decl := TGocciaExportDestructuringDeclaration(Stmt).Declaration
      else if Stmt is TGocciaExportFunctionDeclaration then
        Decl := TGocciaExportFunctionDeclaration(Stmt).Declaration
      else if Stmt is TGocciaExportClassDeclaration then
        Decl := TGocciaExportClassDeclaration(Stmt).Declaration
      else if Stmt is TGocciaExportEnumDeclaration then
        Decl := TGocciaExportEnumDeclaration(Stmt).Declaration
      else
        IsExport := False;
      if Decl is TGocciaVariableDeclaration then
      begin
        CollectVariableDeclarationBindingNames(TGocciaVariableDeclaration(Decl), Names, True);
        if TGocciaVariableDeclaration(Decl).IsVar then
          Kind := lbVar
        else if TGocciaVariableDeclaration(Decl).IsConst then
          Kind := lbConst;
      end
      else if Decl is TGocciaDestructuringDeclaration then
      begin
        CollectPatternBindingNames(TGocciaDestructuringDeclaration(Decl).Pattern, Names, True);
        if TGocciaDestructuringDeclaration(Decl).IsVar then
          Kind := lbVar
        else if TGocciaDestructuringDeclaration(Decl).IsConst then
          Kind := lbConst;
      end
      else if Decl is TGocciaFunctionDeclaration then
        Names.Add(TGocciaFunctionDeclaration(Decl).Name)
      else if Decl is TGocciaClassDeclaration then
        Names.Add(TGocciaClassDeclaration(Decl).ClassDefinition.Name)
      else if Decl is TGocciaEnumDeclaration then
        Names.Add(TGocciaEnumDeclaration(Decl).Name)
      else if Decl is TGocciaUsingDeclaration then
      begin
        Kind := lbConst;
        for J := 0 to High(TGocciaUsingDeclaration(Decl).Variables) do
          Names.Add(TGocciaUsingDeclaration(Decl).Variables[J].Name);
      end
      else if Decl is TGocciaExportDefaultDeclaration then
      begin
        DefaultDecl := TGocciaExportDefaultDeclaration(Decl);
        Names.Add(DefaultDecl.LocalName);
        if not (IsNamedDefaultFunctionDeclaration(DefaultDecl) or
           IsNamedDefaultClassDeclaration(DefaultDecl)) then
          Kind := lbConst;
        Item.Kind := ldLocalExport;
        SetLength(Item.Pairs, 1);
        Item.Pairs[0].Name := 'default';
        Item.Pairs[0].Value := DefaultDecl.LocalName;
        AppendDeclaration;
      end;
      if Kind <> lbVar then
        for J := 0 to Names.Count - 1 do
          AddBinding(Names[J], Kind, VarBindings);
      if IsExport then
      begin
        Item.Kind := ldLocalExport;
        CheckedNativeByteSize(Names.Count, SizeOf(TGocciaLinkPair));
        SetLength(Item.Pairs, Names.Count);
        for J := 0 to Names.Count - 1 do
        begin
          Item.Pairs[J].Name := Names[J];
          Item.Pairs[J].Value := Names[J];
        end;
        AppendDeclaration;
      end;
    end;
  finally
    VarBindings.Free;
    Names.Free;
  end;
end;

procedure TGocciaModuleLinkPlan.Validate;
var
  Names, ExportNames: TOrderedStringMap<Boolean>;
  I, J: Integer;
  AllocationBudget: Int64;
  procedure AddName(const AName: string);
  begin
    if (AName = '') or Names.ContainsKey(AName) then
      raise EArgumentException.Create('Invalid bytecode module binding');
    Names.Add(AName, True);
  end;
  procedure AddExport(const AName: string);
  begin
    if ExportNames.ContainsKey(AName) then
      raise EArgumentException.Create('Duplicate bytecode module export');
    ExportNames.Add(AName, True);
  end;
begin
  CheckedNativeByteSize(Length(Bindings), SizeOf(TGocciaLinkBinding));
  CheckedNativeByteSize(Length(Declarations), SizeOf(TGocciaLinkDeclaration));
  { Budget the adapter's arena, nodes and map entries as well as wire data. }
  AllocationBudget := CheckedNativeByteSize(Int64(Length(Bindings)) + Length(Declarations), 512);
  for I := 0 to High(Declarations) do
  begin
    AllocationBudget := AllocationBudget +
      CheckedNativeByteSize(Length(Declarations[I].Pairs), 256);
    CheckedNativeByteSize(AllocationBudget, 1);
  end;
  Names := TOrderedStringMap<Boolean>.Create;
  ExportNames := TOrderedStringMap<Boolean>.Create;
  try
    for I := 0 to High(Bindings) do
    begin
      if (Ord(Bindings[I].Kind) < Ord(Low(TGocciaLinkBindingKind))) or
         (Ord(Bindings[I].Kind) > Ord(High(TGocciaLinkBindingKind))) then
        raise EArgumentException.Create('Invalid bytecode binding kind');
      AddName(Bindings[I].Name);
    end;
    for I := 0 to High(Declarations) do
    begin
      if (Ord(Declarations[I].Kind) > Ord(High(TGocciaLinkDeclarationKind))) or
         (Ord(Declarations[I].Phase) > Ord(High(TGocciaImportCallPhase))) then
        raise EArgumentException.Create('Invalid bytecode module declaration');
      case Declarations[I].Kind of
        ldImport:
          if Declarations[I].IsStarExport or
             ((Declarations[I].Phase = icpSource) and
              ((Declarations[I].NamespaceName <> '') or
               (Length(Declarations[I].Pairs) <> 1))) or
             ((Declarations[I].Phase = icpDefer) and
              ((Declarations[I].NamespaceName = '') or
               (Length(Declarations[I].Pairs) <> 0))) then
            raise EArgumentException.Create('Invalid bytecode import declaration');
        ldLocalExport:
          if (Declarations[I].ModulePath <> '') or
             (Declarations[I].AttributeType <> '') or
             (Declarations[I].NamespaceName <> '') or
             Declarations[I].IsStarExport or
             (Declarations[I].Phase <> icpEvaluation) then
            raise EArgumentException.Create('Invalid bytecode local export');
        ldReExport:
          if (Declarations[I].Phase <> icpEvaluation) or
             (Declarations[I].IsStarExport and (Length(Declarations[I].Pairs) <> 0)) or
             ((not Declarations[I].IsStarExport) and
              (Declarations[I].NamespaceName <> '')) then
            raise EArgumentException.Create('Invalid bytecode re-export');
      end;
      if (Declarations[I].Kind = ldImport) and
         (Declarations[I].Phase = icpSource) and
         (Declarations[I].Pairs[0].Value <> 'default') then
        raise EArgumentException.Create('Invalid bytecode source import');
      if Declarations[I].Kind = ldImport then
      begin
        if Declarations[I].NamespaceName <> '' then
          AddName(Declarations[I].NamespaceName);
        for J := 0 to High(Declarations[I].Pairs) do
          AddName(Declarations[I].Pairs[J].Name);
      end
      else
      begin
        if (Declarations[I].Kind = ldReExport) and
           Declarations[I].IsStarExport and
           (Declarations[I].NamespaceName <> '') then
          AddExport(Declarations[I].NamespaceName);
        for J := 0 to High(Declarations[I].Pairs) do
          AddExport(Declarations[I].Pairs[J].Name);
      end;
    end;
    for I := 0 to High(Declarations) do
      if Declarations[I].Kind = ldLocalExport then
        for J := 0 to High(Declarations[I].Pairs) do
          if not Names.ContainsKey(Declarations[I].Pairs[J].Value) then
            raise EArgumentException.Create('Unknown bytecode export binding');
  finally
    ExportNames.Free;
    Names.Free;
  end;
end;

function TGocciaModuleLinkPlan.Materialize: TGocciaProgram;
var
  Arena, PreviousArena: TGocciaASTArena;
  Body: TObjectList<TGocciaStatement>;
  Info: TArray<TGocciaVariableInfo>;
  Map: TStringStringMap;
  I, J: Integer;
  Node: TGocciaStatement;
begin
  Validate;
  Arena := TGocciaASTArena.Create;
  PreviousArena := ActivateASTArena(Arena);
  Body := nil;
  try
    Body := TObjectList<TGocciaStatement>.Create(False);
    SetLength(Info, 1);
    for I := 0 to High(Bindings) do
    begin
      Info[0] := Default(TGocciaVariableInfo);
      Info[0].Name := Bindings[I].Name;
      Body.Add(TGocciaVariableDeclaration.Create(Copy(Info),
        Bindings[I].Kind = lbConst, TGocciaSourceSpan.Empty,
        Bindings[I].Kind = lbVar));
    end;
    for I := 0 to High(Declarations) do
    begin
      Map := TStringStringMap.Create;
      try
        for J := 0 to High(Declarations[I].Pairs) do
          Map.Add(Declarations[I].Pairs[J].Name, Declarations[I].Pairs[J].Value);
        case Declarations[I].Kind of
          ldImport:
            Node := TGocciaImportDeclaration.Create(Map,
              Declarations[I].ModulePath, TGocciaSourceSpan.Empty,
              Declarations[I].NamespaceName, Declarations[I].Phase,
              Declarations[I].AttributeType);
          ldLocalExport:
            Node := TGocciaExportDeclaration.Create(Map, TGocciaSourceSpan.Empty);
          ldReExport:
            Node := TGocciaReExportDeclaration.Create(Map,
              Declarations[I].ModulePath, TGocciaSourceSpan.Empty,
              Declarations[I].IsStarExport, Declarations[I].NamespaceName,
              Declarations[I].AttributeType);
        end;
        Map := nil;
        Body.Add(Node);
      finally
        Map.Free;
      end;
    end;
    Result := TGocciaProgram.Create(Body);
    Body := nil;
    Result.AdoptArena(Arena);
    Arena := nil;
    Result.HasTopLevelAwait := HasTopLevelAwait;
  finally
    RestoreASTArena(PreviousArena);
    Body.Free;
    Arena.Free;
  end;
end;

end.
