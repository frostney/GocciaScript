unit Goccia.Scope.Redeclaration;

{$I Goccia.inc}

interface

uses
  UnicodeStringList,

  Goccia.AST.Node,
  Goccia.Scope;

procedure CheckTopLevelRedeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope; const ASourcePath: string);

{ ES2026 §19.2.1.3 EvalDeclarationInstantiation steps 3.a, 10 and 12 for a
  non-strict eval whose variable environment is the global environment
  AGlobalScope, with the name lists of CollectGlobalEvalDeclarationNames. A
  var-declared name that a global lexical declaration has is a SyntaxError; a
  function or a var the global object cannot take is a TypeError (the
  CanDeclareGlobalFunction and CanDeclareGlobalVar checks of §9.1.1.4.15 and
  §9.1.1.4.14). The checks run before any binding is created (step 14), so a
  failed eval leaves the global object as it was. }
procedure CheckGlobalEvalDeclarations(const AGlobalScope: TGocciaScope;
  const AVarNames, AFunctionNames, ADeclaredVarNames: TUnicodeStringList);

implementation

uses
  SysUtils,

  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper;

procedure CheckGlobalEvalDeclarations(const AGlobalScope: TGocciaScope;
  const AVarNames, AFunctionNames, ADeclaredVarNames: TUnicodeStringList);
var
  I: Integer;
begin
  for I := 0 to AVarNames.Count - 1 do
    if AGlobalScope.HasLexicalDeclaration(AVarNames[I]) then
      ThrowSyntaxError(
        SysUtils.Format(SErrorIdentifierAlreadyDeclared, [AVarNames[I]]),
        SSuggestAlreadyDeclared);
  // Step 10 visits the declarations in reverse List order.
  for I := AFunctionNames.Count - 1 downto 0 do
    if not AGlobalScope.CanDeclareGlobalFunction(AFunctionNames[I]) then
      ThrowTypeError(SysUtils.Format('Cannot declare global function ''%s''',
        [AFunctionNames[I]]));
  for I := 0 to ADeclaredVarNames.Count - 1 do
    if not AGlobalScope.CanDeclareGlobalVar(ADeclaredVarNames[I]) then
      ThrowTypeError(SysUtils.Format('Cannot declare global var ''%s''',
        [ADeclaredVarNames[I]]));
end;

procedure CheckLexicalRedeclarationName(
  const AName: string;
  const AScope: TGocciaScope; const ASourcePath: string;
  const ALine, AColumn: Integer);
begin
  if AScope.ContainsOwnVarBinding(AName) or
     AScope.HasLexicalDeclaration(AName) or
     AScope.HasRestrictedGlobalProperty(AName) then
    raise TGocciaSyntaxError.Create(
      SysUtils.Format('Identifier ''%s'' has already been declared',
        [AName]), ALine, AColumn, ASourcePath, nil);
end;

procedure CheckPatternRedeclarations(
  const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaScope; const ASourcePath: string;
  const AIsVar: Boolean; const ALine, AColumn: Integer);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  DeclName: string;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    DeclName := TGocciaIdentifierDestructuringPattern(APattern).Name;
    if AIsVar and AScope.ContainsOwnLexicalBinding(DeclName) then
    begin
      if AScope.IsBuiltInBinding(DeclName) then
        Exit;
      raise TGocciaSyntaxError.Create(
        SysUtils.Format('Identifier ''%s'' has already been declared',
          [DeclName]), ALine, AColumn, ASourcePath, nil);
    end;
    if AIsVar then
    begin
      if (AScope.ScopeKind = skGlobal) and
         not AScope.CanDeclareGlobalVar(DeclName) then
        raise TGocciaTypeError.Create(
          SysUtils.Format('Cannot declare global var ''%s''', [DeclName]),
          ALine, AColumn, ASourcePath, nil);
    end
    else
      CheckLexicalRedeclarationName(DeclName, AScope, ASourcePath, ALine,
        AColumn);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      CheckPatternRedeclarations(ObjPat.Properties[I].Pattern,
        AScope, ASourcePath, AIsVar, ALine, AColumn);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        CheckPatternRedeclarations(ArrPat.Elements[I],
          AScope, ASourcePath, AIsVar, ALine, AColumn);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
    CheckPatternRedeclarations(
      TGocciaAssignmentDestructuringPattern(APattern).Left,
      AScope, ASourcePath, AIsVar, ALine, AColumn)
  else if APattern is TGocciaRestDestructuringPattern then
    CheckPatternRedeclarations(
      TGocciaRestDestructuringPattern(APattern).Argument,
      AScope, ASourcePath, AIsVar, ALine, AColumn);
end;

procedure CheckTopLevelRedeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope; const ASourcePath: string);
var
  I, J: Integer;
  Stmt: TGocciaStatement;
  VarDecl: TGocciaVariableDeclaration;
  DeclName: string;

  procedure CheckVarLikeName(const AName: string; const ALine, AColumn: Integer);
  begin
    if AScope.ContainsOwnLexicalBinding(AName) then
    begin
      // §16.1.7: var/function declarations may shadow built-in globals in
      // script source, but not user-declared bindings.
      if AScope.IsBuiltInBinding(AName) then
        Exit;
      raise TGocciaSyntaxError.Create(
        SysUtils.Format('Identifier ''%s'' has already been declared',
          [AName]), ALine, AColumn, ASourcePath, nil);
    end;
    if (AScope.ScopeKind = skGlobal) and
       not AScope.CanDeclareGlobalFunction(AName) then
      raise TGocciaTypeError.Create(
        SysUtils.Format('Cannot declare global function ''%s''', [AName]),
        ALine, AColumn, ASourcePath, nil);
  end;
begin
  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];
    if Stmt is TGocciaVariableDeclaration then
    begin
      VarDecl := TGocciaVariableDeclaration(Stmt);
      for J := 0 to High(VarDecl.Variables) do
      begin
        if VarDecl.Variables[J].IsPattern or Assigned(VarDecl.Variables[J].Pattern) then
        begin
          CheckPatternRedeclarations(VarDecl.Variables[J].Pattern, AScope,
            ASourcePath, VarDecl.IsVar, Stmt.Line, Stmt.Column);
          Continue;
        end;

        DeclName := VarDecl.Variables[J].Name;
        if VarDecl.IsVar then
        begin
          if AScope.ContainsOwnLexicalBinding(DeclName) then
          begin
            // §16.1.7: var declarations may shadow built-in globals in
            // script source, but not user-declared lexical bindings.
            if AScope.IsBuiltInBinding(DeclName) then
              Continue;
            raise TGocciaSyntaxError.Create(
              SysUtils.Format('Identifier ''%s'' has already been declared',
                [DeclName]), Stmt.Line, Stmt.Column, ASourcePath, nil);
          end;
          if (AScope.ScopeKind = skGlobal) and
             not AScope.CanDeclareGlobalVar(DeclName) then
            raise TGocciaTypeError.Create(
              SysUtils.Format('Cannot declare global var ''%s''', [DeclName]),
              Stmt.Line, Stmt.Column, ASourcePath, nil);
        end;
        if not VarDecl.IsVar then
          CheckLexicalRedeclarationName(DeclName, AScope, ASourcePath,
            Stmt.Line, Stmt.Column);
      end;
    end
    else if Stmt is TGocciaFunctionDeclaration then
      CheckVarLikeName(TGocciaFunctionDeclaration(Stmt).Name,
        Stmt.Line, Stmt.Column)
    else if Stmt is TGocciaExportFunctionDeclaration then
      CheckVarLikeName(
        TGocciaExportFunctionDeclaration(Stmt).Declaration.Name,
        Stmt.Line, Stmt.Column)
    else if Stmt is TGocciaClassDeclaration then
    begin
      DeclName := TGocciaClassDeclaration(Stmt).ClassDefinition.Name;
      CheckLexicalRedeclarationName(DeclName, AScope, ASourcePath,
        Stmt.Line, Stmt.Column);
    end
    else if Stmt is TGocciaDestructuringDeclaration then
      CheckPatternRedeclarations(
        TGocciaDestructuringDeclaration(Stmt).Pattern, AScope, ASourcePath,
        TGocciaDestructuringDeclaration(Stmt).IsVar, Stmt.Line, Stmt.Column)
    else if Stmt is TGocciaEnumDeclaration then
    begin
      DeclName := TGocciaEnumDeclaration(Stmt).Name;
      CheckLexicalRedeclarationName(DeclName, AScope, ASourcePath,
        Stmt.Line, Stmt.Column);
    end;
  end;
end;

end.
