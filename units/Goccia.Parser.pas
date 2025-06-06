unit Goccia.Parser;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node, Goccia.Token, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Error, Goccia.Values.UndefinedValue,
  Goccia.Values.BooleanValue, Goccia.Values.NumberValue, Goccia.Values.ObjectValue, Goccia.Values.StringValue,
  Goccia.Values.ArrayValue, Goccia.Values.FunctionValue, Goccia.Values.ClassValue, Goccia.Values.NullValue,
  Generics.Collections, Classes, SysUtils;

type
  TGocciaParser = class
  private
    FTokens: TObjectList<TGocciaToken>;
    FCurrent: Integer;
    FFileName: string;
    FSourceLines: TStringList;

    function IsAtEnd: Boolean; inline;
    function Peek: TGocciaToken; inline;
    function Previous: TGocciaToken; inline;
    function Advance: TGocciaToken;
    function Check(TokenType: TGocciaTokenType): Boolean; inline;
    function Match(TokenTypes: array of TGocciaTokenType): Boolean;
    function Consume(TokenType: TGocciaTokenType; const Message: string): TGocciaToken;

    // Expression parsing
    function Expression: TGocciaExpression;
    function Conditional: TGocciaExpression;
    function LogicalOr: TGocciaExpression;
    function LogicalAnd: TGocciaExpression;
    function Equality: TGocciaExpression;
    function Comparison: TGocciaExpression;
    function Addition: TGocciaExpression;
    function Multiplication: TGocciaExpression;
    function Exponentiation: TGocciaExpression;
    function Unary: TGocciaExpression;
    function Call: TGocciaExpression;
    function Primary: TGocciaExpression;
    function ArrayLiteral: TGocciaExpression;
    function ObjectLiteral: TGocciaExpression;
    function ArrowFunction: TGocciaExpression;
    function Assignment: TGocciaExpression;

    // Statement parsing
    // TODO: Make these types more strict
    function Statement: TGocciaStatement;
    function DeclarationStatement: TGocciaStatement;
    function ExpressionStatement: TGocciaStatement;
    function BlockStatement: TGocciaBlockStatement;
    function IfStatement: TGocciaStatement;
    function ReturnStatement: TGocciaStatement;
    function ThrowStatement: TGocciaStatement;
    function TryStatement: TGocciaStatement;
    function ClassMethod: TGocciaClassMethod;
    function ClassDeclaration: TGocciaStatement;
    function ImportDeclaration: TGocciaStatement;
    function ExportDeclaration: TGocciaStatement;
  public
    constructor Create(ATokens: TObjectList<TGocciaToken>;
      const AFileName: string; ASourceLines: TStringList);
    function Parse: TGocciaProgram;
  end;

implementation

constructor TGocciaParser.Create(ATokens: TObjectList<TGocciaToken>;
  const AFileName: string; ASourceLines: TStringList);
begin
  FTokens := ATokens;
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FCurrent := 0;
end;

function TGocciaParser.IsAtEnd: Boolean;
begin
  Result := Peek.TokenType = gttEOF;
end;

function TGocciaParser.Peek: TGocciaToken;
begin
  Result := FTokens[FCurrent];
end;

function TGocciaParser.Previous: TGocciaToken;
begin
  Result := FTokens[FCurrent - 1];
end;

function TGocciaParser.Advance: TGocciaToken;
begin
  if not IsAtEnd then
    Inc(FCurrent);
  Result := Previous;
end;

function TGocciaParser.Check(TokenType: TGocciaTokenType): Boolean;
begin
  if IsAtEnd then
    Exit(False);
  Result := Peek.TokenType = TokenType;
end;

function TGocciaParser.Match(TokenTypes: array of TGocciaTokenType): Boolean;
var
  TokenType: TGocciaTokenType;
begin
  for TokenType in TokenTypes do
  begin
    if Check(TokenType) then
    begin
      Advance;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TGocciaParser.Consume(TokenType: TGocciaTokenType;
  const Message: string): TGocciaToken;
begin
  if Check(TokenType) then
    Exit(Advance);

  raise TGocciaSyntaxError.Create(Message, Peek.Line, Peek.Column,
    FFileName, FSourceLines);
end;

function TGocciaParser.Expression: TGocciaExpression;
begin
  Result := Assignment;
end;

function TGocciaParser.Conditional: TGocciaExpression;
var
  Condition, Consequent, Alternate: TGocciaExpression;
  Line, Column: Integer;
begin
  Result := LogicalOr;

  if Match([gttQuestion]) then
  begin
    Line := Previous.Line;
    Column := Previous.Column;
    Condition := Result;
    Consequent := Expression;
    Consume(gttColon, 'Expected ":" in conditional expression');
    Alternate := Conditional;
    Result := TGocciaConditionalExpression.Create(Condition, Consequent,
      Alternate, Line, Column);
  end;
end;

function TGocciaParser.LogicalOr: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := LogicalAnd;

  while Match([gttOr]) do
  begin
    Operator := Previous;
    Right := LogicalAnd;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.LogicalAnd: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Equality;

  while Match([gttAnd]) do
  begin
    Operator := Previous;
    Right := Equality;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Equality: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Comparison;

  while Match([gttEqual, gttNotEqual]) do
  begin
    Operator := Previous;
    Right := Comparison;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Comparison: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Addition;

  while Match([gttGreater, gttGreaterEqual, gttLess, gttLessEqual]) do
  begin
    Operator := Previous;
    Right := Addition;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Addition: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Multiplication;

  while Match([gttPlus, gttMinus]) do
  begin
    Operator := Previous;
    Right := Multiplication;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Multiplication: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Exponentiation;

  while Match([gttStar, gttSlash, gttPercent]) do
  begin
    Operator := Previous;
    Right := Exponentiation;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Exponentiation: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Unary;

  if Match([gttPower]) then
  begin
    Operator := Previous;
    Right := Exponentiation; // Right associative
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Unary: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  if Match([gttNot, gttMinus]) then
  begin
    Operator := Previous;
    Right := Unary;
    Result := TGocciaUnaryExpression.Create(Operator.TokenType, Right,
      Operator.Line, Operator.Column);
  end
  else
    Result := Call;
end;

function TGocciaParser.Call: TGocciaExpression;
var
  Arguments: TObjectList<TGocciaExpression>;
  Arg: TGocciaExpression;
  PropertyName: string;
  Line, Column: Integer;
begin
  Result := Primary;

  while True do
  begin
    if Match([gttLeftParen]) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Arguments := TObjectList<TGocciaExpression>.Create(True);

      if not Check(gttRightParen) then
      begin
        repeat
          Arg := Expression;
          Arguments.Add(Arg);
        until not Match([gttComma]);
      end;

      Consume(gttRightParen, 'Expected ")" after arguments');
      Result := TGocciaCallExpression.Create(Result, Arguments, Line, Column);
    end
    else if Match([gttDot]) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      PropertyName := Consume(gttIdentifier, 'Expected property name after "."').Lexeme;
      Result := TGocciaMemberExpression.Create(Result, PropertyName, False,
        Line, Column);
    end
    else if Match([gttLeftBracket]) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Arg := Expression;
      Consume(gttRightBracket, 'Expected "]" after computed member expression');
      // For computed access, we'll store the expression as a string representation
      // This is a simplification; in a real implementation, we'd handle this differently
      if Arg is TGocciaLiteralExpression then
        PropertyName := TGocciaLiteralExpression(Arg).Value.ToString
      else
        PropertyName := '[computed]';
      Result := TGocciaMemberExpression.Create(Result, PropertyName, True,
        Line, Column);
    end
    else
      Break;
  end;
end;

function TGocciaParser.Primary: TGocciaExpression;
var
  Token: TGocciaToken;
  Expr: TGocciaExpression;
  Name: string;
  Args: TObjectList<TGocciaExpression>;
begin
  if Match([gttTrue]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaBooleanValue.Create(True), Token.Line, Token.Column);
  end
  else if Match([gttFalse]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaBooleanValue.Create(False), Token.Line, Token.Column);
  end
  else if Match([gttNull]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaNullValue.Create, Token.Line, Token.Column);
  end
  else if Match([gttUndefined]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaUndefinedValue.Create, Token.Line, Token.Column);
  end
  else if Match([gttNumber]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaNumberValue.Create(StrToFloat(Token.Lexeme)), Token.Line, Token.Column);
  end
  else if Match([gttString]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaStringValue.Create(Token.Lexeme), Token.Line, Token.Column);
  end
  else if Match([gttThis]) then
  begin
    Token := Previous;
    Result := TGocciaThisExpression.Create(Token.Line, Token.Column);
  end
  else if Match([gttNew]) then
  begin
    Token := Previous;
    Expr := Call;
    if Expr is TGocciaCallExpression then
    begin
      Result := TGocciaNewExpression.Create(
        TGocciaCallExpression(Expr).Callee,
        TGocciaCallExpression(Expr).Arguments,
        Token.Line, Token.Column);
    end
    else
    begin
      Args := TObjectList<TGocciaExpression>.Create(True);
      Result := TGocciaNewExpression.Create(Expr, Args, Token.Line, Token.Column);
    end;
  end
  else if Match([gttIdentifier]) then
  begin
    Token := Previous;
    Name := Token.Lexeme;
    Result := TGocciaIdentifierExpression.Create(Name, Token.Line, Token.Column);
  end
  else if Match([gttLeftParen]) then
  begin
    Token := Previous;

    // Check for arrow function
    if Check(gttRightParen) or Check(gttIdentifier) then
      Result := ArrowFunction
    else
    begin
      Expr := Expression;
      Consume(gttRightParen, 'Expected ")" after expression');
      Result := Expr;
    end;
  end
  else if Match([gttLeftBracket]) then
    Result := ArrayLiteral
  else if Match([gttLeftBrace]) then
    Result := ObjectLiteral
  else
    raise TGocciaSyntaxError.Create('Expected expression',
      Peek.Line, Peek.Column, FFileName, FSourceLines);
end;

function TGocciaParser.ArrayLiteral: TGocciaExpression;
var
  Elements: TObjectList<TGocciaExpression>;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Elements := TObjectList<TGocciaExpression>.Create(True);

  while not Check(gttRightBracket) and not IsAtEnd do
  begin
    Elements.Add(Expression);
    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBracket, 'Expected "]" after array elements');
  Result := TGocciaArrayExpression.Create(Elements, Line, Column);
end;

function TGocciaParser.ObjectLiteral: TGocciaExpression;
var
  Properties: TDictionary<string, TGocciaExpression>;
  Key: string;
  Value: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Properties := TDictionary<string, TGocciaExpression>.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    if Check(gttString) then
      Key := Advance.Lexeme
    else
      Key := Consume(gttIdentifier, 'Expected property name').Lexeme;

    Consume(gttColon, 'Expected ":" after property key');
    Value := Expression;
    Properties.Add(Key, Value);

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after object properties');
  Result := TGocciaObjectExpression.Create(Properties, Line, Column);
end;

function TGocciaParser.ArrowFunction: TGocciaExpression;
var
  Parameters: TStringList;
  Body: TGocciaASTNode;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Parameters := TStringList.Create;

  // Parse parameters
  if not Check(gttRightParen) then
  begin
    repeat
      Parameters.Add(Consume(gttIdentifier, 'Expected parameter name').Lexeme);
    until not Match([gttComma]);
  end;

  Consume(gttRightParen, 'Expected ")" after parameters');
  Consume(gttArrow, 'Expected "=>" in arrow function');

  // Parse body
  if Match([gttLeftBrace]) then
    Body := BlockStatement
  else
    Body := Expression;

  Result := TGocciaArrowFunctionExpression.Create(Parameters, Body, Line, Column);
end;

function TGocciaParser.Assignment: TGocciaExpression;
var
  Left, Right: TGocciaExpression;
  Line, Column: Integer;
begin
  Left := Conditional;
  if Match([gttAssign]) then
  begin
    Line := Previous.Line;
    Column := Previous.Column;
    // Only allow assignment to identifier or member expression
    if Left is TGocciaIdentifierExpression then
    begin
      Right := Assignment;
      Result := TGocciaAssignmentExpression.Create(TGocciaIdentifierExpression(Left).Name, Right, Line, Column);
    end
    else if Left is TGocciaMemberExpression then
    begin
      Right := Assignment;
      Result := TGocciaPropertyAssignmentExpression.Create(
        TGocciaMemberExpression(Left).ObjectExpr,
        TGocciaMemberExpression(Left).PropertyName,
        Right,
        Line,
        Column
      );
    end
    else
      raise TGocciaSyntaxError.Create('Invalid assignment target', Left.Line, Left.Column, FFileName, FSourceLines);
  end
  else
    Result := Left;
end;

function TGocciaParser.Statement: TGocciaStatement;
begin
  if Match([gttConst, gttLet]) then
    Result := DeclarationStatement
  else if Match([gttClass]) then
    Result := ClassDeclaration
  else if Match([gttImport]) then
    Result := ImportDeclaration
  else if Match([gttExport]) then
    Result := ExportDeclaration
  else if Match([gttIf]) then
    Result := IfStatement
  else if Match([gttReturn]) then
    Result := ReturnStatement
  else if Match([gttThrow]) then
    Result := ThrowStatement
  else if Match([gttTry]) then
    Result := TryStatement
  else if Match([gttLeftBrace]) then
    Result := BlockStatement
  else
    Result := ExpressionStatement;
end;

function TGocciaParser.DeclarationStatement: TGocciaStatement;
var
  IsConst: Boolean;
  Name: string;
  Initializer: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  IsConst := Previous.TokenType = gttConst;

  Name := Consume(gttIdentifier, 'Expected variable name').Lexeme;

  if Match([gttAssign]) then
    Initializer := Expression
  else if IsConst then
    raise TGocciaSyntaxError.Create('const declarations must have an initializer',
      Line, Column, FFileName, FSourceLines)
  else
    Initializer := TGocciaLiteralExpression.Create(
      TGocciaUndefinedValue.Create, Line, Column);

  Consume(gttSemicolon, 'Expected ";" after variable declaration');
  Result := TGocciaVariableDeclaration.Create(Name, Initializer, IsConst,
    Line, Column);
end;

function TGocciaParser.ExpressionStatement: TGocciaStatement;
var
  Expr: TGocciaExpression;
  Line, Column: Integer;
begin
  Expr := Expression;
  Line := Expr.Line;
  Column := Expr.Column;

  // Only require semicolon if we're not in a class method
  if not Check(gttSemicolon) then
    Result := TGocciaExpressionStatement.Create(Expr, Line, Column)
  else
  begin
    Advance; // Consume the semicolon
    Result := TGocciaExpressionStatement.Create(Expr, Line, Column);
  end;
end;

function TGocciaParser.BlockStatement: TGocciaBlockStatement;
var
  Nodes: TObjectList<TGocciaASTNode>;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Nodes := TObjectList<TGocciaASTNode>.Create(True);

  while not Check(gttRightBrace) and not IsAtEnd do
    Nodes.Add(Statement);

  Consume(gttRightBrace, 'Expected "}" after block');
  Result := TGocciaBlockStatement.Create(Nodes, Line, Column);
end;

function TGocciaParser.IfStatement: TGocciaStatement;
var
  Condition: TGocciaExpression;
  Consequent, Alternate: TGocciaStatement;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Consume(gttLeftParen, 'Expected "(" after "if"');
  Condition := Expression;
  Consume(gttRightParen, 'Expected ")" after if condition');

  Consequent := Statement;

  if Match([gttElse]) then
    Alternate := Statement
  else
    Alternate := nil;

  Result := TGocciaIfStatement.Create(Condition, Consequent, Alternate,
    Line, Column);
end;

function TGocciaParser.ReturnStatement: TGocciaStatement;
var
  Value: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  if Check(gttSemicolon) then
    Value := nil
  else
    Value := Expression;

  Consume(gttSemicolon, 'Expected ";" after return value');
  Result := TGocciaReturnStatement.Create(Value, Line, Column);
end;

function TGocciaParser.ThrowStatement: TGocciaStatement;
var
  Value: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Value := Expression;
  Consume(gttSemicolon, 'Expected ";" after throw value');
  Result := TGocciaThrowStatement.Create(Value, Line, Column);
end;

function TGocciaParser.TryStatement: TGocciaStatement;
var
  Block, CatchBlock, FinallyBlock: TGocciaBlockStatement;
  CatchParam: string;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Block := BlockStatement;
  CatchParam := '';
  CatchBlock := nil;
  FinallyBlock := nil;

  if Match([gttCatch]) then
  begin
    Consume(gttLeftParen, 'Expected "(" after "catch"');
    CatchParam := Consume(gttIdentifier, 'Expected catch parameter').Lexeme;
    Consume(gttRightParen, 'Expected ")" after catch parameter');
    CatchBlock := BlockStatement;
  end;

  if Match([gttFinally]) then
    FinallyBlock := BlockStatement;

  if (CatchBlock = nil) and (FinallyBlock = nil) then
    raise TGocciaSyntaxError.Create('Missing catch or finally after try',
      Line, Column, FFileName, FSourceLines);

  Result := TGocciaTryStatement.Create(Block, CatchParam, CatchBlock,
    FinallyBlock, Line, Column);
end;

function TGocciaParser.ClassMethod: TGocciaClassMethod;
var
  Parameters: TStringList;
  Body: TGocciaASTNode;
  Name: string;
  Line, Column: Integer;
  Statements: TObjectList<TGocciaStatement>;
  Stmt: TGocciaStatement;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Parameters := TStringList.Create;
  try
    // Parse parameters
    Consume(gttLeftParen, 'Expected "(" after method name');
    if not Check(gttRightParen) then
    begin
      repeat
        Parameters.Add(Consume(gttIdentifier, 'Expected parameter name').Lexeme);
      until not Match([gttComma]);
    end;
    Consume(gttRightParen, 'Expected ")" after parameters');

    // Parse method body
    Consume(gttLeftBrace, 'Expected "{" before method body');

    // Create a block statement for the method body
    Statements := TObjectList<TGocciaStatement>.Create(True);
    try
      while not Check(gttRightBrace) and not IsAtEnd do
      begin
        Stmt := Statement;
        if Stmt is TGocciaExpressionStatement then
        begin
          // For expression statements in class methods, don't require semicolons
          if not Check(gttSemicolon) then
            Statements.Add(Stmt)
          else
          begin
            Advance; // Consume the semicolon
            Statements.Add(Stmt);
          end;
        end
        else
          Statements.Add(Stmt);
      end;

      Consume(gttRightBrace, 'Expected "}" after method body');
      Body := TGocciaBlockStatement.Create(TObjectList<TGocciaASTNode>(Statements), Line, Column);
      Result := TGocciaClassMethod.Create(Name, Parameters, Body, Line, Column);
    except
      Statements.Free;
      raise;
    end;
  except
    Parameters.Free;
    raise;
  end;
end;

function TGocciaParser.ClassDeclaration: TGocciaStatement;
var
  Name, SuperClass: string;
  Methods: TDictionary<string, TGocciaClassMethod>;
  MethodName: string;
  Method: TGocciaClassMethod;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Name := Consume(gttIdentifier, 'Expected class name').Lexeme;

  if Match([gttExtends]) then
    SuperClass := Consume(gttIdentifier, 'Expected superclass name').Lexeme
  else
    SuperClass := '';

  Consume(gttLeftBrace, 'Expected "{" before class body');

  Methods := TDictionary<string, TGocciaClassMethod>.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    MethodName := Consume(gttIdentifier, 'Expected method name').Lexeme;
    Method := ClassMethod;
    Method.Name := MethodName; // Set the method name
    Methods.Add(MethodName, Method);
  end;

  Consume(gttRightBrace, 'Expected "}" after class body');
  Result := TGocciaClassDeclaration.Create(Name, SuperClass, Methods, Line, Column);
end;

function TGocciaParser.ImportDeclaration: TGocciaStatement;
var
  Imports: TDictionary<string, string>;
  ImportedName, LocalName, ModulePath: string;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Consume(gttLeftBrace, 'Expected "{" after "import"');

  Imports := TDictionary<string, string>.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    ImportedName := Consume(gttIdentifier, 'Expected import name').Lexeme;

    if Match([gttAs]) then
      LocalName := Consume(gttIdentifier, 'Expected local name after "as"').Lexeme
    else
      LocalName := ImportedName;

    Imports.Add(LocalName, ImportedName);

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after imports');
  Consume(gttFrom, 'Expected "from" after imports');
  ModulePath := Consume(gttString, 'Expected module path').Lexeme;
  Consume(gttSemicolon, 'Expected ";" after import declaration');

  Result := TGocciaImportDeclaration.Create(Imports, ModulePath, Line, Column);
end;

function TGocciaParser.ExportDeclaration: TGocciaStatement;
var
  ExportsTable: TDictionary<string, string>;
  LocalName, ExportedName: string;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Consume(gttLeftBrace, 'Expected "{" after "export"');

  ExportsTable := TDictionary<string, string>.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    LocalName := Consume(gttIdentifier, 'Expected export name').Lexeme;

    if Match([gttAs]) then
      ExportedName := Consume(gttIdentifier, 'Expected exported name after "as"').Lexeme
    else
      ExportedName := LocalName;

    ExportsTable.Add(ExportedName, LocalName);

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after exports');
  Consume(gttSemicolon, 'Expected ";" after export declaration');

  Result := TGocciaExportDeclaration.Create(ExportsTable, Line, Column);
end;

function TGocciaParser.Parse: TGocciaProgram;
var
  Statements: TObjectList<TGocciaStatement>;
begin
  Statements := TObjectList<TGocciaStatement>.Create(True);

  while not IsAtEnd do
    Statements.Add(Statement);

  Result := TGocciaProgram.Create(Statements);
end;

end.
