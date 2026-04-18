unit Goccia.Scope.Redeclaration;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node,
  Goccia.Scope;

procedure CheckTopLevelRedeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope; const ASourcePath: string);

implementation

uses
  SysUtils,

  Goccia.AST.Expressions,
  Goccia.AST.Statements,
  Goccia.Error;

procedure CheckPatternRedeclarations(
  const APattern: TGocciaDestructuringPattern;
  const AScope: TGocciaScope; const ASourcePath: string);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  I: Integer;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
  begin
    if AScope.ContainsOwnLexicalBinding(
         TGocciaIdentifierDestructuringPattern(APattern).Name) then
      raise TGocciaSyntaxError.Create(
        SysUtils.Format('Identifier ''%s'' has already been declared',
          [TGocciaIdentifierDestructuringPattern(APattern).Name]),
        0, 0, ASourcePath, nil);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjPat.Properties.Count - 1 do
      CheckPatternRedeclarations(ObjPat.Properties[I].Pattern,
        AScope, ASourcePath);
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrPat.Elements.Count - 1 do
      if Assigned(ArrPat.Elements[I]) then
        CheckPatternRedeclarations(ArrPat.Elements[I],
          AScope, ASourcePath);
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
    CheckPatternRedeclarations(
      TGocciaAssignmentDestructuringPattern(APattern).Left,
      AScope, ASourcePath)
  else if APattern is TGocciaRestDestructuringPattern then
    CheckPatternRedeclarations(
      TGocciaRestDestructuringPattern(APattern).Argument,
      AScope, ASourcePath);
end;

procedure CheckTopLevelRedeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope; const ASourcePath: string);
var
  I, J: Integer;
  Stmt: TGocciaStatement;
  VarDecl: TGocciaVariableDeclaration;
  DeclName: string;
begin
  for I := 0 to AProgram.Body.Count - 1 do
  begin
    Stmt := AProgram.Body[I];
    if Stmt is TGocciaVariableDeclaration then
    begin
      VarDecl := TGocciaVariableDeclaration(Stmt);
      for J := 0 to High(VarDecl.Variables) do
      begin
        DeclName := VarDecl.Variables[J].Name;
        if AScope.ContainsOwnLexicalBinding(DeclName) then
          raise TGocciaSyntaxError.Create(
            SysUtils.Format('Identifier ''%s'' has already been declared',
              [DeclName]), Stmt.Line, Stmt.Column, ASourcePath, nil);
      end;
    end
    else if Stmt is TGocciaClassDeclaration then
    begin
      DeclName := TGocciaClassDeclaration(Stmt).ClassDefinition.Name;
      if AScope.ContainsOwnLexicalBinding(DeclName) then
        raise TGocciaSyntaxError.Create(
          SysUtils.Format('Identifier ''%s'' has already been declared',
            [DeclName]), Stmt.Line, Stmt.Column, ASourcePath, nil);
    end
    else if Stmt is TGocciaDestructuringDeclaration then
      CheckPatternRedeclarations(
        TGocciaDestructuringDeclaration(Stmt).Pattern, AScope, ASourcePath)
    else if Stmt is TGocciaEnumDeclaration then
    begin
      DeclName := TGocciaEnumDeclaration(Stmt).Name;
      if AScope.ContainsOwnLexicalBinding(DeclName) then
        raise TGocciaSyntaxError.Create(
          SysUtils.Format('Identifier ''%s'' has already been declared',
            [DeclName]), Stmt.Line, Stmt.Column, ASourcePath, nil);
    end;
  end;
end;

end.
