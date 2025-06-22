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
    function CheckNext(TokenType: TGocciaTokenType): Boolean; inline;
    function Match(TokenTypes: array of TGocciaTokenType): Boolean;
    function Consume(TokenType: TGocciaTokenType; const Message: string): TGocciaToken;
    function IsArrowFunction: Boolean;
    function ConvertNumberLiteral(const Lexeme: string): Double;

    // Expression parsing (private)
    function Conditional: TGocciaExpression;
    function LogicalOr: TGocciaExpression;
    function NullishCoalescing: TGocciaExpression;
    function LogicalAnd: TGocciaExpression;

    // Destructuring pattern parsing
    function ParsePattern: TGocciaDestructuringPattern;
    function ParseArrayPattern: TGocciaArrayDestructuringPattern;
    function ParseObjectPattern: TGocciaObjectDestructuringPattern;
    function IsAssignmentPattern(Expr: TGocciaExpression): Boolean;
    function ConvertToPattern(Expr: TGocciaExpression): TGocciaDestructuringPattern;
    procedure SkipDestructuringPattern;
    procedure SkipExpression;
    function BitwiseOr: TGocciaExpression;
    function BitwiseXor: TGocciaExpression;
    function BitwiseAnd: TGocciaExpression;
    function Equality: TGocciaExpression;
    function Comparison: TGocciaExpression;
    function Shift: TGocciaExpression;
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
    function ClassExpression: TGocciaExpression;

    // Statement parsing
    // TODO: Make these types more strict
    function Statement: TGocciaStatement;
    function DeclarationStatement: TGocciaStatement;
    function ExpressionStatement: TGocciaStatement;
    function BlockStatement: TGocciaBlockStatement;
    function IfStatement: TGocciaStatement;
    function ForStatement: TGocciaStatement;
    function WhileStatement: TGocciaStatement;
    function DoWhileStatement: TGocciaStatement;
    function ReturnStatement: TGocciaStatement;
    function ThrowStatement: TGocciaStatement;
    function TryStatement: TGocciaStatement;
    function ClassMethod(IsStatic: Boolean = False): TGocciaClassMethod;
    function ClassDeclaration: TGocciaStatement;
    function ImportDeclaration: TGocciaStatement;
    function ExportDeclaration: TGocciaStatement;
    function ParseClassBody(const ClassName: string): TGocciaClassDefinition;
    function SwitchStatement: TGocciaStatement;
    function BreakStatement: TGocciaStatement;
  public
    constructor Create(ATokens: TObjectList<TGocciaToken>;
      const AFileName: string; ASourceLines: TStringList);
    function Parse: TGocciaProgram;
    function Expression: TGocciaExpression;
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

function TGocciaParser.CheckNext(TokenType: TGocciaTokenType): Boolean;
begin
  if FCurrent + 1 >= FTokens.Count then
    Exit(False);
  Result := FTokens[FCurrent + 1].TokenType = TokenType;
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
  Result := NullishCoalescing;

  while Match([gttOr]) do
  begin
    Operator := Previous;
    Right := NullishCoalescing;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.NullishCoalescing: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := LogicalAnd;

  while Match([gttNullishCoalescing]) do
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
  Result := BitwiseOr;

  while Match([gttAnd]) do
  begin
    Operator := Previous;
    Right := BitwiseOr;
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
  Result := Shift;

  while Match([gttGreater, gttGreaterEqual, gttLess, gttLessEqual, gttInstanceof, gttIn]) do
  begin
    Operator := Previous;
    Right := Shift;
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
  if Match([gttNot, gttMinus, gttPlus, gttTypeof, gttBitwiseNot, gttDelete]) then
  begin
    Operator := Previous;
    Right := Unary;
    Result := TGocciaUnaryExpression.Create(Operator.TokenType, Right,
      Operator.Line, Operator.Column);
  end
  else if Match([gttIncrement, gttDecrement]) then
  begin
    // Prefix increment/decrement (++x, --x)
    Operator := Previous;
    Right := Unary;

    // Only allow on identifiers and member expressions
    if not ((Right is TGocciaIdentifierExpression) or (Right is TGocciaMemberExpression)) then
      raise TGocciaSyntaxError.Create('Invalid target for increment/decrement',
        Operator.Line, Operator.Column, FFileName, FSourceLines);

    Result := TGocciaIncrementExpression.Create(Right, Operator.TokenType, True,
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
          if Match([gttSpread]) then
            Arg := TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column)
          else
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

      // Check if this is a private field access (this.#field)
      if Check(gttHash) then
      begin
        Advance; // consume the #
        PropertyName := Consume(gttIdentifier, 'Expected private field name after "#"').Lexeme;
        Result := TGocciaPrivateMemberExpression.Create(Result, PropertyName, Line, Column);
      end
      else
      begin
        if Check(gttIdentifier) then
          PropertyName := Advance.Lexeme
        else if Match([gttIf, gttElse, gttConst, gttLet, gttClass, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
                       gttReturn, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak,
                       gttThrow, gttTry, gttCatch, gttFinally, gttImport, gttExport, gttFrom, gttAs,
                       gttTrue, gttFalse, gttNull, gttUndefined, gttTypeof, gttInstanceof]) then
          PropertyName := Previous.Lexeme  // Reserved words are allowed as property names
        else
          raise TGocciaSyntaxError.Create('Expected property name after "."', Peek.Line, Peek.Column, FFileName, FSourceLines);

        Result := TGocciaMemberExpression.Create(Result, PropertyName, False,
          Line, Column);
      end;
    end
    else if Match([gttHash]) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      PropertyName := Consume(gttIdentifier, 'Expected private field name after "#"').Lexeme;
      Result := TGocciaPrivateMemberExpression.Create(Result, PropertyName, Line, Column);
    end
    else if Match([gttLeftBracket]) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Arg := Expression;
      Consume(gttRightBracket, 'Expected "]" after computed member expression');
      // For computed access, store the expression directly to be evaluated at runtime
      Result := TGocciaMemberExpression.Create(Result, Arg, Line, Column);
    end
    else if Match([gttIncrement, gttDecrement]) then
    begin
      // Postfix increment/decrement (x++, x--)
      Line := Previous.Line;
      Column := Previous.Column;

      // Only allow on identifiers and member expressions
      if not ((Result is TGocciaIdentifierExpression) or (Result is TGocciaMemberExpression)) then
        raise TGocciaSyntaxError.Create('Invalid target for increment/decrement',
          Line, Column, FFileName, FSourceLines);

      Result := TGocciaIncrementExpression.Create(Result, Previous.TokenType, False,
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
      TGocciaNumberValue.Create(ConvertNumberLiteral(Token.Lexeme)), Token.Line, Token.Column);
  end
  else if Match([gttString]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaStringValue.Create(Token.Lexeme), Token.Line, Token.Column);
  end
  else if Match([gttTemplate]) then
  begin
    Token := Previous;
    Result := TGocciaTemplateLiteralExpression.Create(Token.Lexeme, Token.Line, Token.Column);
  end
  else if Match([gttThis]) then
  begin
    Token := Previous;
    Result := TGocciaThisExpression.Create(Token.Line, Token.Column);
  end
  else if Match([gttSuper]) then
  begin
    Token := Previous;
    Result := TGocciaSuperExpression.Create(Token.Line, Token.Column);
  end
  else if Match([gttClass]) then
  begin
    Token := Previous;
    Result := ClassExpression;
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
  else if Match([gttHash]) then
  begin
    Token := Previous;
    Name := Consume(gttIdentifier, 'Expected private field name after "#"').Lexeme;
    // Private field access is equivalent to this.#fieldName
    Result := TGocciaPrivateMemberExpression.Create(
      TGocciaThisExpression.Create(Token.Line, Token.Column),
      Name, Token.Line, Token.Column);
  end
  else if Match([gttLeftParen]) then
  begin
    // Check for arrow function by looking for pattern: () => or (id) => or (id, id) =>
    if IsArrowFunction() then
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
    // Check for holes (consecutive commas or leading comma)
    if Check(gttComma) then
    begin
      // This is a hole in the array
      Elements.Add(TGocciaHoleExpression.Create(Peek.Line, Peek.Column));
    end
    else if Match([gttSpread]) then
    begin
      // Spread expression: ...array
      Elements.Add(TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column));
    end
    else
    begin
      // Regular expression
      Elements.Add(Expression);
    end;

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBracket, 'Expected "]" after array elements');
  Result := TGocciaArrayExpression.Create(Elements, Line, Column);
end;

function TGocciaParser.ObjectLiteral: TGocciaExpression;
var
  Properties: TDictionary<string, TGocciaExpression>;
  PropertyOrder: TStringList;
  ComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
  ComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
  PropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
  Getters: TDictionary<string, TGocciaGetterExpression>;
  Setters: TDictionary<string, TGocciaSetterExpression>;
  Key: string;
  KeyExpression: TGocciaExpression;
  Value: TGocciaExpression;
  Line, Column: Integer;
  NumericValue: Double;
  IsComputed: Boolean;
  IsGetter, IsSetter: Boolean;
  ComputedCount, SourceOrderCount: Integer;
  // Method shorthand variables
  Parameters: TGocciaParameterArray;
  ParamCount: Integer;
  ParamName: string;
  DefaultValue: TGocciaExpression;
  Body: TGocciaASTNode;
  Statements: TObjectList<TGocciaStatement>;
  Stmt: TGocciaStatement;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Properties := TDictionary<string, TGocciaExpression>.Create;
  PropertyOrder := TStringList.Create;
  PropertyOrder.Duplicates := dupIgnore;
  ComputedProperties := TDictionary<TGocciaExpression, TGocciaExpression>.Create;
  Getters := TDictionary<string, TGocciaGetterExpression>.Create;
  Setters := TDictionary<string, TGocciaSetterExpression>.Create;

  // Initialize source order tracking
  ComputedCount := 0;
  SourceOrderCount := 0;
  SetLength(ComputedPropertiesInOrder, 0);
  SetLength(PropertySourceOrder, 0);

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    IsComputed := False;
    IsGetter := False;
    IsSetter := False;

    // Check for getter/setter syntax (contextual keywords)
    // Only treat as getter/setter if followed by identifier (not colon, parenthesis, comma, or right brace)
    if Check(gttIdentifier) and (Peek.Lexeme = 'get') and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) then
    begin
      Advance; // consume 'get'
      IsGetter := True;
    end
    else if Check(gttIdentifier) and (Peek.Lexeme = 'set') and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) then
    begin
      Advance; // consume 'set'
      IsSetter := True;
    end;

        // Handle spread syntax: ...obj
    if not IsGetter and not IsSetter and Match([gttSpread]) then
    begin
      // Create a spread expression
      Line := Previous.Line;
      Column := Previous.Column;
      KeyExpression := TGocciaSpreadExpression.Create(Expression, Line, Column);
      ComputedProperties.Add(KeyExpression, nil); // nil value for spread

      // Track in source order
      Inc(ComputedCount);
      SetLength(ComputedPropertiesInOrder, ComputedCount);
      ComputedPropertiesInOrder[ComputedCount - 1].Key := KeyExpression;
      ComputedPropertiesInOrder[ComputedCount - 1].Value := nil;

      Inc(SourceOrderCount);
      SetLength(PropertySourceOrder, SourceOrderCount);
      PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstComputed;
      PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := ComputedCount - 1;
      PropertySourceOrder[SourceOrderCount - 1].StaticKey := '';

      // For spread expressions, no further processing is needed
      if not Match([gttComma]) then
        Break;
      Continue;
    end
    else if Match([gttLeftBracket]) then
    begin
      // Computed property name: [expr]: value
      IsComputed := True;
      KeyExpression := Expression;
      Consume(gttRightBracket, 'Expected "]" after computed property name');
    end
    else if Check(gttString) then
      Key := Advance.Lexeme
    else if Check(gttIdentifier) then
      Key := Advance.Lexeme
    else if Check(gttNumber) then
    begin
      // Convert numeric literals to their decimal string representation
      NumericValue := ConvertNumberLiteral(Advance.Lexeme);
      Key := FloatToStr(NumericValue);
    end
    else if Match([gttIf, gttElse, gttConst, gttLet, gttClass, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
                   gttReturn, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak,
                   gttThrow, gttTry, gttCatch, gttFinally, gttImport, gttExport, gttFrom, gttAs,
                   gttTrue, gttFalse, gttNull, gttUndefined, gttTypeof, gttInstanceof]) then
      Key := Previous.Lexeme  // Reserved words are allowed as property names
    else
      raise TGocciaSyntaxError.Create('Expected property name', Peek.Line, Peek.Column, FFileName, FSourceLines);

    // Handle getter/setter syntax
    if IsGetter then
    begin
      // Getter: get propertyName() { ... }
      Consume(gttLeftParen, 'Expected "(" after getter name');
      Consume(gttRightParen, 'Getters cannot have parameters');

      // Parse getter body
      Consume(gttLeftBrace, 'Expected "{" before getter body');
      Statements := TObjectList<TGocciaStatement>.Create(True);
      try
        while not Check(gttRightBrace) and not IsAtEnd do
        begin
          Stmt := Statement;
          Statements.Add(Stmt);
        end;
        Consume(gttRightBrace, 'Expected "}" after getter body');
        Body := TGocciaBlockStatement.Create(TObjectList<TGocciaASTNode>(Statements), Line, Column);

                // Add getter to dictionary
        Getters.Add(Key, TGocciaGetterExpression.Create(Body, Line, Column));

        // Track in source order
        Inc(SourceOrderCount);
        SetLength(PropertySourceOrder, SourceOrderCount);
        PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstGetter;
        PropertySourceOrder[SourceOrderCount - 1].StaticKey := Key;
        PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := -1;
      except
        Statements.Free;
        raise;
      end;
    end
    else if IsSetter then
    begin
      // Setter: set propertyName(value) { ... }
      Consume(gttLeftParen, 'Expected "(" after setter name');
      ParamName := Consume(gttIdentifier, 'Expected parameter name in setter').Lexeme;
      Consume(gttRightParen, 'Expected ")" after setter parameter');

      // Parse setter body
      Consume(gttLeftBrace, 'Expected "{" before setter body');
      Statements := TObjectList<TGocciaStatement>.Create(True);
      try
        while not Check(gttRightBrace) and not IsAtEnd do
        begin
          Stmt := Statement;
          Statements.Add(Stmt);
        end;
        Consume(gttRightBrace, 'Expected "}" after setter body');
        Body := TGocciaBlockStatement.Create(TObjectList<TGocciaASTNode>(Statements), Line, Column);

                // Add setter to dictionary
        Setters.Add(Key, TGocciaSetterExpression.Create(ParamName, Body, Line, Column));

        // Track in source order
        Inc(SourceOrderCount);
        SetLength(PropertySourceOrder, SourceOrderCount);
        PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstSetter;
        PropertySourceOrder[SourceOrderCount - 1].StaticKey := Key;
        PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := -1;
      except
        Statements.Free;
        raise;
      end;
    end
    // Check for method shorthand syntax: methodName() { ... } or [expr]() { ... }
    else if Check(gttLeftParen) then
    begin
      // Method shorthand syntax (works for both computed and non-computed)
      Line := Peek.Line;
      Column := Peek.Column;

      SetLength(Parameters, 0);
      ParamCount := 0;

      // Parse parameters
      Consume(gttLeftParen, 'Expected "(" after method name');
      if not Check(gttRightParen) then
      begin
        repeat
          ParamName := Consume(gttIdentifier, 'Expected parameter name').Lexeme;

          // Check for default value
          if Match([gttAssign]) then
            DefaultValue := Assignment
          else
            DefaultValue := nil;

          // Add parameter to array
          Inc(ParamCount);
          SetLength(Parameters, ParamCount);
          Parameters[ParamCount - 1].Name := ParamName;
          Parameters[ParamCount - 1].DefaultValue := DefaultValue;

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
          Statements.Add(Stmt);
        end;

        Consume(gttRightBrace, 'Expected "}" after method body');
        Body := TGocciaBlockStatement.Create(TObjectList<TGocciaASTNode>(Statements), Line, Column);

        // Create function expression for the method
        Value := TGocciaArrowFunctionExpression.Create(Parameters, Body, Line, Column);
      except
        Statements.Free;
        raise;
      end;
    end
    else
    begin
      // Check for shorthand property syntax: { name } means { name: name }
      if Check(gttColon) then
      begin
        // Regular property: key: value
        Consume(gttColon, 'Expected ":" after property key');
        Value := Expression;
      end
            else
      begin
        // Shorthand property: { name } means { name: name }
        // Also handle default values in shorthand: { name = defaultValue }
        if IsComputed then
          raise TGocciaSyntaxError.Create('Computed property names require a value', Peek.Line, Peek.Column, FFileName, FSourceLines);

        Value := TGocciaIdentifierExpression.Create(Key, Previous.Line, Previous.Column);

        // Check for default value in shorthand property
        if Match([gttAssign]) then
        begin
          // This creates an assignment expression for use in destructuring patterns
          Value := TGocciaAssignmentExpression.Create(Key, Expression, Previous.Line, Previous.Column);
        end;
      end;
    end;

        if not IsGetter and not IsSetter then
    begin
      if IsComputed then
      begin
        // Store computed properties separately - they'll be evaluated at runtime
        ComputedProperties.Add(KeyExpression, Value);

        // Track in source order
        Inc(ComputedCount);
        SetLength(ComputedPropertiesInOrder, ComputedCount);
        ComputedPropertiesInOrder[ComputedCount - 1].Key := KeyExpression;
        ComputedPropertiesInOrder[ComputedCount - 1].Value := Value;

        Inc(SourceOrderCount);
        SetLength(PropertySourceOrder, SourceOrderCount);
        PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstComputed;
        PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := ComputedCount - 1;
        PropertySourceOrder[SourceOrderCount - 1].StaticKey := '';
      end
      else
      begin
        // JavaScript allows duplicate keys - last one wins
        if Properties.ContainsKey(Key) then
          Properties[Key] := Value
        else
        begin
          Properties.Add(Key, Value);
          PropertyOrder.Add(Key); // Track insertion order
        end;

        // Track in source order
        Inc(SourceOrderCount);
        SetLength(PropertySourceOrder, SourceOrderCount);
        PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstStatic;
        PropertySourceOrder[SourceOrderCount - 1].StaticKey := Key;
        PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := -1;
      end;
    end;

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after object properties');
  Result := TGocciaObjectExpression.Create(Properties, PropertyOrder, ComputedProperties, ComputedPropertiesInOrder, Getters, Setters, PropertySourceOrder, Line, Column);
end;

function TGocciaParser.ArrowFunction: TGocciaExpression;
var
  Parameters: TGocciaParameterArray;
  ParamCount: Integer;
  ParamName: string;
  DefaultValue: TGocciaExpression;
  Pattern: TGocciaDestructuringPattern;
  Body: TGocciaASTNode;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  ParamCount := 0;

  // Parse parameters
  if not Check(gttRightParen) then
  begin
    repeat
      // Increase array size
      SetLength(Parameters, ParamCount + 1);

      // Check if this is a destructuring pattern or simple identifier
      if Check(gttLeftBracket) or Check(gttLeftBrace) then
      begin
        // Parse destructuring pattern parameter
        Pattern := ParsePattern;
        Parameters[ParamCount].Pattern := Pattern;
        Parameters[ParamCount].IsPattern := True;
        Parameters[ParamCount].Name := ''; // Not used for patterns

        // Check for default value for the whole pattern
        if Match([gttAssign]) then
          Parameters[ParamCount].DefaultValue := Assignment
        else
          Parameters[ParamCount].DefaultValue := nil;
      end
      else
      begin
        // Parse simple identifier parameter
        ParamName := Consume(gttIdentifier, 'Expected parameter name').Lexeme;
        Parameters[ParamCount].Name := ParamName;
        Parameters[ParamCount].IsPattern := False;
        Parameters[ParamCount].Pattern := nil;

        // Check for default value
        if Match([gttAssign]) then
          Parameters[ParamCount].DefaultValue := Assignment
        else
          Parameters[ParamCount].DefaultValue := nil;
      end;

      Inc(ParamCount);
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
  Operator: TGocciaTokenType;
  Pattern: TGocciaDestructuringPattern;
begin
  Left := Conditional;
  if Match([gttAssign, gttPlusAssign, gttMinusAssign, gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign,
             gttBitwiseAndAssign, gttBitwiseOrAssign, gttBitwiseXorAssign, gttLeftShiftAssign, gttRightShiftAssign, gttUnsignedRightShiftAssign]) then
  begin
    Operator := Previous.TokenType;
    Line := Previous.Line;
    Column := Previous.Column;
    Right := Assignment;

    // Check for destructuring assignment
    if (Operator = gttAssign) and IsAssignmentPattern(Left) then
    begin
      // Convert expression to pattern for destructuring
      Pattern := ConvertToPattern(Left);
      Result := TGocciaDestructuringAssignmentExpression.Create(Pattern, Right, Line, Column);
    end
    // Only allow assignment to identifier or member expression
    else if Left is TGocciaIdentifierExpression then
    begin
      if Operator = gttAssign then
        Result := TGocciaAssignmentExpression.Create(TGocciaIdentifierExpression(Left).Name, Right, Line, Column)
      else
        Result := TGocciaCompoundAssignmentExpression.Create(TGocciaIdentifierExpression(Left).Name, Operator, Right, Line, Column);
    end
    else if Left is TGocciaMemberExpression then
    begin
      if TGocciaMemberExpression(Left).Computed then
      begin
        // Handle computed property assignment: obj[expr] = value
        if Operator = gttAssign then
          Result := TGocciaComputedPropertyAssignmentExpression.Create(
            TGocciaMemberExpression(Left).ObjectExpr,
            TGocciaMemberExpression(Left).PropertyExpression,
            Right,
            Line,
            Column
          )
        else
          Result := TGocciaComputedPropertyCompoundAssignmentExpression.Create(
            TGocciaMemberExpression(Left).ObjectExpr,
            TGocciaMemberExpression(Left).PropertyExpression,
            Operator,
            Right,
            Line,
            Column
          );
      end
      else
      begin
        // Handle static property assignment: obj.prop = value
        if Operator = gttAssign then
          Result := TGocciaPropertyAssignmentExpression.Create(
            TGocciaMemberExpression(Left).ObjectExpr,
            TGocciaMemberExpression(Left).PropertyName,
            Right,
            Line,
            Column
          )
        else
          Result := TGocciaPropertyCompoundAssignmentExpression.Create(
            TGocciaMemberExpression(Left).ObjectExpr,
            TGocciaMemberExpression(Left).PropertyName,
            Operator,
            Right,
            Line,
            Column
          );
      end;
    end
    else if Left is TGocciaPrivateMemberExpression then
    begin
      if Operator = gttAssign then
        Result := TGocciaPrivatePropertyAssignmentExpression.Create(
          TGocciaPrivateMemberExpression(Left).ObjectExpr,
          TGocciaPrivateMemberExpression(Left).PrivateName,
          Right,
          Line,
          Column
        )
      else
        Result := TGocciaPrivatePropertyCompoundAssignmentExpression.Create(
          TGocciaPrivateMemberExpression(Left).ObjectExpr,
          TGocciaPrivateMemberExpression(Left).PrivateName,
          Operator,
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

function TGocciaParser.ClassExpression: TGocciaExpression;
var
  Name: string;
  ClassDef: TGocciaClassDefinition;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // Class name is optional in class expressions
  if Check(gttIdentifier) then
    Name := Advance.Lexeme
  else
    Name := ''; // Anonymous class

  ClassDef := ParseClassBody(Name);
  Result := TGocciaClassExpression.Create(ClassDef, Line, Column);
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
  else if Match([gttFor]) then
    Result := ForStatement
  else if Match([gttWhile]) then
    Result := WhileStatement
  else if Match([gttDo]) then
    Result := DoWhileStatement
  else if Match([gttSwitch]) then
    Result := SwitchStatement
  else if Match([gttBreak]) then
    Result := BreakStatement
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
  Variables: TArray<TGocciaVariableInfo>;
  VariableCount: Integer;
  Pattern: TGocciaDestructuringPattern;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  IsConst := Previous.TokenType = gttConst;
  VariableCount := 0;

  // Check for destructuring pattern
  if Check(gttLeftBracket) or Check(gttLeftBrace) then
  begin
    // Destructuring declaration
    Pattern := ParsePattern;
    Consume(gttAssign, 'Destructuring declarations must have an initializer');
    Initializer := Expression;
    Consume(gttSemicolon, 'Expected ";" after destructuring declaration');
    Result := TGocciaDestructuringDeclaration.Create(Pattern, Initializer, IsConst, Line, Column);
  end
  else
  begin
    // Regular variable declarations separated by commas
    repeat
      // Increase array size
      SetLength(Variables, VariableCount + 1);

      Name := Consume(gttIdentifier, 'Expected variable name').Lexeme;
      Variables[VariableCount].Name := Name;

      if Match([gttAssign]) then
        Variables[VariableCount].Initializer := Expression
      else if IsConst then
        raise TGocciaSyntaxError.Create('const declarations must have an initializer',
          Line, Column, FFileName, FSourceLines)
      else
        Variables[VariableCount].Initializer := TGocciaLiteralExpression.Create(
          TGocciaUndefinedValue.Create, Line, Column);

      Inc(VariableCount);
    until not Match([gttComma]);

    Consume(gttSemicolon, 'Expected ";" after variable declaration');
    Result := TGocciaVariableDeclaration.Create(Variables, IsConst, Line, Column);
  end;
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

function TGocciaParser.ForStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // For now, just consume the basic for loop structure and return a simple statement
  Consume(gttLeftParen, 'Expected "(" after "for"');

  // Skip everything until the closing parenthesis
  while not Check(gttRightParen) and not IsAtEnd do
    Advance;

  Consume(gttRightParen, 'Expected ")" after for clauses');

  // Parse body - can be a block or single statement
  if Check(gttLeftBrace) then
  begin
    Advance; // consume the '{'
    Result := BlockStatement;
  end
  else
    Result := Statement;
end;

function TGocciaParser.WhileStatement: TGocciaStatement;
var
  Condition: TGocciaExpression;
  Body: TGocciaStatement;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Consume(gttLeftParen, 'Expected "(" after "while"');
  Condition := Expression;
  Consume(gttRightParen, 'Expected ")" after while condition');

  Body := Statement;

  Result := TGocciaWhileStatement.Create(Condition, Body, Line, Column);
end;

function TGocciaParser.DoWhileStatement: TGocciaStatement;
var
  Body: TGocciaStatement;
  Condition: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // Parse body first
  Body := Statement;

  // Expect 'while' keyword
  Consume(gttWhile, 'Expected "while" after do body');

  // Parse condition
  Consume(gttLeftParen, 'Expected "(" after "while"');
  Condition := Expression;
  Consume(gttRightParen, 'Expected ")" after do-while condition');

  // Expect semicolon
  Consume(gttSemicolon, 'Expected ";" after do-while statement');

  Result := TGocciaDoWhileStatement.Create(Body, Condition, Line, Column);
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

  Consume(gttLeftBrace, 'Expected "{" after "try"');
  Block := BlockStatement;
  CatchParam := '';
  CatchBlock := nil;
  FinallyBlock := nil;

  if Match([gttCatch]) then
  begin
    // Check if catch has a parameter: catch (e) vs catch
    if Check(gttLeftParen) then
    begin
      Consume(gttLeftParen, 'Expected "(" after "catch"');
      CatchParam := Consume(gttIdentifier, 'Expected catch parameter').Lexeme;
      Consume(gttRightParen, 'Expected ")" after catch parameter');
    end
    else
    begin
      // No parameter - empty catch parameter
      CatchParam := '';
    end;

    Consume(gttLeftBrace, 'Expected "{" after catch clause');
    CatchBlock := BlockStatement;
  end;

  if Match([gttFinally]) then
  begin
    Consume(gttLeftBrace, 'Expected "{" after "finally"');
    FinallyBlock := BlockStatement;
  end;

  if (CatchBlock = nil) and (FinallyBlock = nil) then
    raise TGocciaSyntaxError.Create('Missing catch or finally after try',
      Line, Column, FFileName, FSourceLines);

  Result := TGocciaTryStatement.Create(Block, CatchParam, CatchBlock,
    FinallyBlock, Line, Column);
end;

function TGocciaParser.ClassMethod(IsStatic: Boolean = False): TGocciaClassMethod;
var
  Parameters: TGocciaParameterArray;
  Body: TGocciaASTNode;
  Name: string;
  Line, Column: Integer;
  Statements: TObjectList<TGocciaStatement>;
  Stmt: TGocciaStatement;
  ParamCount: Integer;
  ParamName: string;
  DefaultValue: TGocciaExpression;
  Pattern: TGocciaDestructuringPattern;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  SetLength(Parameters, 0);
  ParamCount := 0;

  // Parse parameters with default value support including destructuring
  Consume(gttLeftParen, 'Expected "(" after method name');
  if not Check(gttRightParen) then
  begin
    repeat
      // Check if this is a destructuring pattern or simple identifier
      if Check(gttLeftBracket) or Check(gttLeftBrace) then
      begin
        // Parse destructuring pattern parameter
        Pattern := ParsePattern;

        // Check for default value for the whole pattern
        if Match([gttAssign]) then
          DefaultValue := Assignment
        else
          DefaultValue := nil;

        // Add parameter to array
        Inc(ParamCount);
        SetLength(Parameters, ParamCount);
        Parameters[ParamCount - 1].Pattern := Pattern;
        Parameters[ParamCount - 1].IsPattern := True;
        Parameters[ParamCount - 1].Name := ''; // Not used for patterns
        Parameters[ParamCount - 1].DefaultValue := DefaultValue;
      end
      else
      begin
        // Parse simple identifier parameter
        ParamName := Consume(gttIdentifier, 'Expected parameter name').Lexeme;

        // Check for default value
        if Match([gttAssign]) then
          DefaultValue := Assignment
        else
          DefaultValue := nil;

        // Add parameter to array
        Inc(ParamCount);
        SetLength(Parameters, ParamCount);
        Parameters[ParamCount - 1].Name := ParamName;
        Parameters[ParamCount - 1].IsPattern := False;
        Parameters[ParamCount - 1].Pattern := nil;
        Parameters[ParamCount - 1].DefaultValue := DefaultValue;
      end;

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
    Result := TGocciaClassMethod.Create(Name, Parameters, Body, IsStatic, Line, Column);
  except
    Statements.Free;
    raise;
  end;
end;

function TGocciaParser.ClassDeclaration: TGocciaStatement;
var
  Name: string;
  ClassDef: TGocciaClassDefinition;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Name := Consume(gttIdentifier, 'Expected class name').Lexeme;
  ClassDef := ParseClassBody(Name);
  Result := TGocciaClassDeclaration.Create(ClassDef, Line, Column);
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

function TGocciaParser.ParseClassBody(const ClassName: string): TGocciaClassDefinition;
var
  SuperClass: string;
  Methods: TDictionary<string, TGocciaClassMethod>;
  Getters: TDictionary<string, TGocciaGetterExpression>;
  Setters: TDictionary<string, TGocciaSetterExpression>;
  StaticProperties: TDictionary<string, TGocciaExpression>;
  InstanceProperties: TDictionary<string, TGocciaExpression>;
  PrivateInstanceProperties: TDictionary<string, TGocciaExpression>;
  PrivateStaticProperties: TDictionary<string, TGocciaExpression>;
  PrivateMethods: TDictionary<string, TGocciaClassMethod>;
  MemberName: string;
  Method: TGocciaClassMethod;
  Getter: TGocciaGetterExpression;
  Setter: TGocciaSetterExpression;
  PropertyValue: TGocciaExpression;
  ParamName: string;
  IsStatic: Boolean;
  IsPrivate: Boolean;
  IsGetter: Boolean;
  IsSetter: Boolean;
begin
  if Match([gttExtends]) then
    SuperClass := Consume(gttIdentifier, 'Expected superclass name').Lexeme
  else
    SuperClass := '';

  Consume(gttLeftBrace, 'Expected "{" before class body');

  Methods := TDictionary<string, TGocciaClassMethod>.Create;
  Getters := TDictionary<string, TGocciaGetterExpression>.Create;
  Setters := TDictionary<string, TGocciaSetterExpression>.Create;
  StaticProperties := TDictionary<string, TGocciaExpression>.Create;
  InstanceProperties := TDictionary<string, TGocciaExpression>.Create;
  PrivateInstanceProperties := TDictionary<string, TGocciaExpression>.Create;
  PrivateStaticProperties := TDictionary<string, TGocciaExpression>.Create;
  PrivateMethods := TDictionary<string, TGocciaClassMethod>.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    IsStatic := Match([gttStatic]);
    IsPrivate := Match([gttHash]);
    IsGetter := False;
    IsSetter := False;

    // Check for getter/setter syntax (contextual keywords)
    if Check(gttIdentifier) and (Peek.Lexeme = 'get') and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) then
    begin
      Advance; // consume 'get'
      IsGetter := True;

      // Handle private property names: get #propName() or get propName()
      if Check(gttHash) then
      begin
        Advance; // consume '#'
        IsPrivate := True; // Mark as private
        MemberName := Consume(gttIdentifier, 'Expected property name after "#"').Lexeme;
      end
      else
      begin
        MemberName := Consume(gttIdentifier, 'Expected property name after "get"').Lexeme;
      end;
    end
    else if Check(gttIdentifier) and (Peek.Lexeme = 'set') and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) then
    begin
      Advance; // consume 'set'
      IsSetter := True;

      // Handle private property names: set #propName(value) or set propName(value)
      if Check(gttHash) then
      begin
        Advance; // consume '#'
        IsPrivate := True; // Mark as private
        MemberName := Consume(gttIdentifier, 'Expected property name after "#"').Lexeme;
      end
      else
      begin
        MemberName := Consume(gttIdentifier, 'Expected property name after "set"').Lexeme;
      end;
    end
    else
    begin
      MemberName := Consume(gttIdentifier, 'Expected method or property name').Lexeme;
    end;

    if Check(gttAssign) then
    begin
      // Property: [static] [#]name = value
      Consume(gttAssign, 'Expected "=" in property');
      PropertyValue := Expression;
      Consume(gttSemicolon, 'Expected ";" after property');

      if IsPrivate and IsStatic then
        PrivateStaticProperties.Add(MemberName, PropertyValue)
      else if IsPrivate then
        PrivateInstanceProperties.Add(MemberName, PropertyValue)
      else if IsStatic then
        StaticProperties.Add(MemberName, PropertyValue)
      else
        InstanceProperties.Add(MemberName, PropertyValue);
    end
    else if Check(gttSemicolon) then
    begin
      // Property declaration without initializer: [static] [#]name;
      Consume(gttSemicolon, 'Expected ";" after property declaration');
      PropertyValue := TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, Peek.Line, Peek.Column);

      if IsPrivate and IsStatic then
        PrivateStaticProperties.Add(MemberName, PropertyValue)
      else if IsPrivate then
        PrivateInstanceProperties.Add(MemberName, PropertyValue)
      else if IsStatic then
        StaticProperties.Add(MemberName, PropertyValue)
      else
        InstanceProperties.Add(MemberName, PropertyValue);
    end
    else if IsGetter then
    begin
      // Getter: [static] get name() { ... } or get #name() { ... }
      if IsStatic then
        raise TGocciaSyntaxError.Create('Static getters not supported yet', Peek.Line, Peek.Column, FFileName, FSourceLines);

            Consume(gttLeftParen, 'Expected "(" after getter name');
      Consume(gttRightParen, 'Expected ")" after getter parameters - getters cannot have parameters');

      Consume(gttLeftBrace, 'Expected "{" before getter body');
      Getter := TGocciaGetterExpression.Create(BlockStatement, Previous.Line, Previous.Column);
      Getters.Add(MemberName, Getter);
    end
    else if IsSetter then
    begin
      // Setter: [static] set name(value) { ... } or set #name(value) { ... }
      if IsStatic then
        raise TGocciaSyntaxError.Create('Static setters not supported yet', Peek.Line, Peek.Column, FFileName, FSourceLines);

            Consume(gttLeftParen, 'Expected "(" after setter name');
      if Check(gttRightParen) then
        raise TGocciaSyntaxError.Create('Setter must have exactly one parameter', Peek.Line, Peek.Column, FFileName, FSourceLines);

            ParamName := Consume(gttIdentifier, 'Expected parameter name').Lexeme;
      Consume(gttRightParen, 'Expected ")" after setter parameter');
      Consume(gttLeftBrace, 'Expected "{" before setter body');

      Setter := TGocciaSetterExpression.Create(ParamName, BlockStatement, Previous.Line, Previous.Column);
      Setters.Add(MemberName, Setter);
    end
    else if Check(gttLeftParen) then
    begin
      // Method: [static] [#]name() { ... }
      Method := ClassMethod(IsStatic);
      Method.Name := MemberName; // Set the method name

      if IsPrivate then
        PrivateMethods.Add(MemberName, Method)
      else
        Methods.Add(MemberName, Method);
    end
    else
      raise TGocciaSyntaxError.Create('Expected "(" for method, "=" for property assignment, or ";" for property declaration',
        Peek.Line, Peek.Column, FFileName, FSourceLines);
  end;

  Consume(gttRightBrace, 'Expected "}" after class body');
  Result := TGocciaClassDefinition.Create(ClassName, SuperClass, Methods, Getters, Setters, StaticProperties, InstanceProperties, PrivateInstanceProperties, PrivateMethods, PrivateStaticProperties);
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

function TGocciaParser.IsArrowFunction: Boolean;
var
  SavedCurrent: Integer;
  ParenCount: Integer;
begin
  // Save current position for rollback
  SavedCurrent := FCurrent;
  Result := False;

  try
    // Look for pattern: () => or (id) => or (id, id) =>
    // We're already past the opening (
    ParenCount := 1;

    // Special case: empty parameter list ()
    if Check(gttRightParen) then
    begin
      Advance; // consume )
      Result := Check(gttArrow);
      Exit;
    end;

    // Look for parameter list patterns
    while not IsAtEnd and (ParenCount > 0) do
    begin
      case Peek.TokenType of
        gttLeftParen:
          begin
            Inc(ParenCount);
            Advance;
          end;
        gttRightParen:
          begin
            Dec(ParenCount);
            Advance;
          end;
        gttLeftBracket, gttLeftBrace:
          begin
            // Handle destructuring patterns in parameters
            Advance; // consume [ or {
            // Skip the destructuring pattern content (simplified)
            SkipDestructuringPattern;
                         // Skip optional default value assignment for the whole pattern
             if Check(gttAssign) then
             begin
               Advance; // consume =
               // Skip the default value expression (handle nested parentheses)
               SkipExpression;
             end;
            // Skip optional comma
            if Check(gttComma) then
              Advance;
          end;
        gttIdentifier:
          begin
            Advance;
            // Skip optional default value assignment
            if Check(gttAssign) then
            begin
              Advance; // consume =
              // Skip the default value expression (handle nested parentheses)
              SkipExpression;
            end;
            // Skip optional comma
            if Check(gttComma) then
              Advance;
          end;
        gttComma:
          Advance;
      else
        // If we hit anything else, it's probably not an arrow function
        Exit;
      end;
    end;

    // Check if we have => after the parameters
    Result := Check(gttArrow);
  finally
    // Restore position
    FCurrent := SavedCurrent;
  end;
end;

function TGocciaParser.BitwiseOr: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := BitwiseXor;

  while Match([gttBitwiseOr]) do
  begin
    Operator := Previous;
    Right := BitwiseXor;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.BitwiseXor: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := BitwiseAnd;

  while Match([gttBitwiseXor]) do
  begin
    Operator := Previous;
    Right := BitwiseAnd;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.BitwiseAnd: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Equality;

  while Match([gttBitwiseAnd]) do
  begin
    Operator := Previous;
    Right := Equality;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.Shift: TGocciaExpression;
var
  Operator: TGocciaToken;
  Right: TGocciaExpression;
begin
  Result := Addition;

  while Match([gttLeftShift, gttRightShift, gttUnsignedRightShift]) do
  begin
    Operator := Previous;
    Right := Addition;
    Result := TGocciaBinaryExpression.Create(Result, Operator.TokenType,
      Right, Operator.Line, Operator.Column);
  end;
end;

function TGocciaParser.ConvertNumberLiteral(const Lexeme: string): Double;
var
  Value: Double;
  IntValue: Int64;
  HexStr, BinStr, OctStr: string;
  I: Integer;
begin
  // Handle special number formats
  if Length(Lexeme) >= 3 then
  begin
    // Hexadecimal: 0x10, 0X10
    if (Lexeme[1] = '0') and ((Lexeme[2] = 'x') or (Lexeme[2] = 'X')) then
    begin
      HexStr := Copy(Lexeme, 3, Length(Lexeme) - 2);
      IntValue := 0;
      for I := 1 to Length(HexStr) do
      begin
        IntValue := IntValue * 16;
        case HexStr[I] of
          '0'..'9': IntValue := IntValue + Ord(HexStr[I]) - Ord('0');
          'a'..'f': IntValue := IntValue + Ord(HexStr[I]) - Ord('a') + 10;
          'A'..'F': IntValue := IntValue + Ord(HexStr[I]) - Ord('A') + 10;
        end;
      end;
      Exit(IntValue);
    end

    // Binary: 0b1010, 0B1010
    else if (Lexeme[1] = '0') and ((Lexeme[2] = 'b') or (Lexeme[2] = 'B')) then
    begin
      BinStr := Copy(Lexeme, 3, Length(Lexeme) - 2);
      IntValue := 0;
      for I := 1 to Length(BinStr) do
      begin
        IntValue := IntValue * 2;
        if BinStr[I] = '1' then
          IntValue := IntValue + 1;
      end;
      Exit(IntValue);
    end

    // Octal: 0o12, 0O12
    else if (Lexeme[1] = '0') and ((Lexeme[2] = 'o') or (Lexeme[2] = 'O')) then
    begin
      OctStr := Copy(Lexeme, 3, Length(Lexeme) - 2);
      IntValue := 0;
      for I := 1 to Length(OctStr) do
      begin
        IntValue := IntValue * 8;
        IntValue := IntValue + Ord(OctStr[I]) - Ord('0');
      end;
      Exit(IntValue);
    end;
  end;

  // Regular decimal numbers (including scientific notation)
  if TryStrToFloat(Lexeme, Value) then
    Exit(Value);

  raise TGocciaSyntaxError.Create('Invalid number format: ' + Lexeme, Peek.Line, Peek.Column,
    FFileName, FSourceLines);
end;

function TGocciaParser.IsAssignmentPattern(Expr: TGocciaExpression): Boolean;
begin
  // Check if an expression could be a destructuring pattern
  // Only arrays and objects are destructuring patterns for assignments
  // Simple identifiers should be regular assignments, not destructuring
  Result := (Expr is TGocciaArrayExpression) or (Expr is TGocciaObjectExpression);
end;

function TGocciaParser.ParsePattern: TGocciaDestructuringPattern;
begin
  if Check(gttLeftBracket) then
  begin
    Advance; // consume '['
    Result := ParseArrayPattern;
  end
  else if Check(gttLeftBrace) then
  begin
    Advance; // consume '{'
    Result := ParseObjectPattern;
  end
  else if Check(gttIdentifier) then
  begin
    Result := TGocciaIdentifierDestructuringPattern.Create(Advance.Lexeme, Previous.Line, Previous.Column);
  end
  else
    raise TGocciaSyntaxError.Create('Expected destructuring pattern', Peek.Line, Peek.Column, FFileName, FSourceLines);
end;

function TGocciaParser.ParseArrayPattern: TGocciaArrayDestructuringPattern;
var
  Elements: TObjectList<TGocciaDestructuringPattern>;
  Pattern: TGocciaDestructuringPattern;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Elements := TObjectList<TGocciaDestructuringPattern>.Create(True);

  while not Check(gttRightBracket) and not IsAtEnd do
  begin
    if Check(gttComma) then
    begin
      // Hole in array pattern
      Elements.Add(nil);
    end
    else if Match([gttSpread]) then
    begin
      // Rest pattern: ...rest
      Pattern := ParsePattern;
      Elements.Add(TGocciaRestDestructuringPattern.Create(Pattern, Previous.Line, Previous.Column));
    end
    else
    begin
      Pattern := ParsePattern;

      // Check for default value
      if Match([gttAssign]) then
      begin
        Pattern := TGocciaAssignmentDestructuringPattern.Create(Pattern, Expression, Pattern.Line, Pattern.Column);
      end;

      Elements.Add(Pattern);
    end;

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBracket, 'Expected "]" after array pattern');
  Result := TGocciaArrayDestructuringPattern.Create(Elements, Line, Column);
end;

function TGocciaParser.ParseObjectPattern: TGocciaObjectDestructuringPattern;
var
  Properties: TObjectList<TGocciaDestructuringProperty>;
  Key: string;
  Pattern: TGocciaDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  Line, Column: Integer;
  IsComputed: Boolean;
  KeyExpression: TGocciaExpression;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Properties := TObjectList<TGocciaDestructuringProperty>.Create(True);

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    IsComputed := False;
    KeyExpression := nil;

    if Match([gttSpread]) then
    begin
      // Rest pattern in object: ...rest
      Pattern := ParsePattern;
      Prop := TGocciaDestructuringProperty.Create('', TGocciaRestDestructuringPattern.Create(Pattern, Previous.Line, Previous.Column));
      Properties.Add(Prop);
    end
    else
    begin
      // Parse property key
      if Match([gttLeftBracket]) then
      begin
        // Computed property: [expr]: pattern
        IsComputed := True;
        KeyExpression := Expression;
        Key := ''; // Will be computed at runtime
        Consume(gttRightBracket, 'Expected "]" after computed property key');
      end
      else if Check(gttIdentifier) then
      begin
        Key := Advance.Lexeme;
      end
      else if Check(gttString) then
      begin
        Key := Advance.Lexeme;
      end
      else
        raise TGocciaSyntaxError.Create('Expected property name in object pattern', Peek.Line, Peek.Column, FFileName, FSourceLines);

      // Check for shorthand or full syntax
      if Match([gttColon]) then
      begin
        // Full syntax: key: pattern
        Pattern := ParsePattern;

        // Check for default value
        if Match([gttAssign]) then
        begin
          Pattern := TGocciaAssignmentDestructuringPattern.Create(Pattern, Expression, Pattern.Line, Pattern.Column);
        end;
      end
      else
      begin
        // Shorthand syntax: {name} means {name: name}
        Pattern := TGocciaIdentifierDestructuringPattern.Create(Key, Previous.Line, Previous.Column);

        // Check for default value in shorthand
        if Match([gttAssign]) then
        begin
          Pattern := TGocciaAssignmentDestructuringPattern.Create(Pattern, Expression, Pattern.Line, Pattern.Column);
        end;
      end;

      Prop := TGocciaDestructuringProperty.Create(Key, Pattern, IsComputed, KeyExpression);
      Properties.Add(Prop);
    end;

    if not Match([gttComma]) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after object pattern');
  Result := TGocciaObjectDestructuringPattern.Create(Properties, Line, Column);
end;

function TGocciaParser.ConvertToPattern(Expr: TGocciaExpression): TGocciaDestructuringPattern;
var
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  IdentifierExpr: TGocciaIdentifierExpression;
  AssignmentExpr: TGocciaAssignmentExpression;
  Elements: TObjectList<TGocciaDestructuringPattern>;
  Properties: TObjectList<TGocciaDestructuringProperty>;
  I: Integer;
  Pair: TPair<string, TGocciaExpression>;
  ComputedPair: TPair<TGocciaExpression, TGocciaExpression>;
  Prop: TGocciaDestructuringProperty;
begin
  if Expr is TGocciaIdentifierExpression then
  begin
    IdentifierExpr := TGocciaIdentifierExpression(Expr);
    Result := TGocciaIdentifierDestructuringPattern.Create(IdentifierExpr.Name, Expr.Line, Expr.Column);
  end
  else if Expr is TGocciaAssignmentExpression then
  begin
    // Handle assignment expressions (for default values)
    AssignmentExpr := TGocciaAssignmentExpression(Expr);
    Result := TGocciaAssignmentDestructuringPattern.Create(
      TGocciaIdentifierDestructuringPattern.Create(AssignmentExpr.Name, Expr.Line, Expr.Column),
      AssignmentExpr.Value,
      Expr.Line,
      Expr.Column
    );
  end
  else if Expr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(Expr);
    Elements := TObjectList<TGocciaDestructuringPattern>.Create(True);

    for I := 0 to ArrayExpr.Elements.Count - 1 do
    begin
      if ArrayExpr.Elements[I] = nil then
        Elements.Add(nil)  // Hole
      else if ArrayExpr.Elements[I] is TGocciaSpreadExpression then
        Elements.Add(TGocciaRestDestructuringPattern.Create(
          ConvertToPattern(TGocciaSpreadExpression(ArrayExpr.Elements[I]).Argument),
          ArrayExpr.Elements[I].Line, ArrayExpr.Elements[I].Column))
      else
        Elements.Add(ConvertToPattern(ArrayExpr.Elements[I]));
    end;

    Result := TGocciaArrayDestructuringPattern.Create(Elements, Expr.Line, Expr.Column);
  end
  else if Expr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(Expr);
    Properties := TObjectList<TGocciaDestructuringProperty>.Create(True);

    // Handle static properties
    for Pair in ObjectExpr.Properties do
    begin
      Prop := TGocciaDestructuringProperty.Create(Pair.Key, ConvertToPattern(Pair.Value));
      Properties.Add(Prop);
    end;

    // Handle computed properties (including spread) in source order
    if Assigned(ObjectExpr.ComputedPropertiesInOrder) then
    begin
      for I := 0 to Length(ObjectExpr.ComputedPropertiesInOrder) - 1 do
      begin
        ComputedPair := ObjectExpr.ComputedPropertiesInOrder[I];
        if ComputedPair.Key is TGocciaSpreadExpression then
        begin
          // Rest pattern
          Prop := TGocciaDestructuringProperty.Create('', TGocciaRestDestructuringPattern.Create(
            ConvertToPattern(TGocciaSpreadExpression(ComputedPair.Key).Argument),
            ComputedPair.Key.Line, ComputedPair.Key.Column));
          Properties.Add(Prop);
        end
        else
        begin
          // Regular computed property
          Prop := TGocciaDestructuringProperty.Create('', ConvertToPattern(ComputedPair.Value), True, ComputedPair.Key);
          Properties.Add(Prop);
        end;
      end;
    end;

    Result := TGocciaObjectDestructuringPattern.Create(Properties, Expr.Line, Expr.Column);
  end
  else
    raise TGocciaSyntaxError.Create('Invalid destructuring target', Expr.Line, Expr.Column, FFileName, FSourceLines);
end;

function TGocciaParser.SwitchStatement: TGocciaStatement;
var
  Discriminant: TGocciaExpression;
  Cases: TObjectList<TGocciaCaseClause>;
  CaseClause: TGocciaCaseClause;
  TestExpression: TGocciaExpression;
  Statements: TObjectList<TGocciaStatement>;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // Parse discriminant: switch (expression)
  Consume(gttLeftParen, 'Expected "(" after "switch"');
  Discriminant := Expression;
  Consume(gttRightParen, 'Expected ")" after switch discriminant');

  // Parse switch body
  Consume(gttLeftBrace, 'Expected "{" before switch body');

  Cases := TObjectList<TGocciaCaseClause>.Create(True);

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    if Match([gttCase]) then
    begin
      // Parse case value
      TestExpression := Expression;
      Consume(gttColon, 'Expected ":" after case value');
    end
    else if Match([gttDefault]) then
    begin
      // Default case
      TestExpression := nil;
      Consume(gttColon, 'Expected ":" after default');
    end
    else
    begin
      raise TGocciaSyntaxError.Create('Expected "case" or "default" in switch body',
        Peek.Line, Peek.Column, FFileName, FSourceLines);
    end;

    // Parse statements until next case/default or end of switch
    Statements := TObjectList<TGocciaStatement>.Create(True);
    while not Check(gttCase) and not Check(gttDefault) and not Check(gttRightBrace) and not IsAtEnd do
    begin
      Statements.Add(Statement);
    end;

    CaseClause := TGocciaCaseClause.Create(TestExpression, Statements, Line, Column);
    Cases.Add(CaseClause);
  end;

  Consume(gttRightBrace, 'Expected "}" after switch body');
  Result := TGocciaSwitchStatement.Create(Discriminant, Cases, Line, Column);
end;

function TGocciaParser.BreakStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Consume(gttSemicolon, 'Expected ";" after break statement');
  Result := TGocciaBreakStatement.Create(Line, Column);
end;

procedure TGocciaParser.SkipDestructuringPattern;
var
  BracketCount, BraceCount: Integer;
begin
  BracketCount := 0;
  BraceCount := 0;

  // The opening bracket/brace was already consumed by the caller, count it
  if Previous.TokenType = gttLeftBracket then
    BracketCount := 1
  else if Previous.TokenType = gttLeftBrace then
    BraceCount := 1;

  // Skip tokens until we've closed all brackets/braces
  while not IsAtEnd and ((BracketCount > 0) or (BraceCount > 0)) do
  begin
    case Peek.TokenType of
      gttLeftBracket:
        begin
          Inc(BracketCount);
          Advance;
        end;
      gttRightBracket:
        begin
          Dec(BracketCount);
          Advance;
        end;
      gttLeftBrace:
        begin
          Inc(BraceCount);
          Advance;
        end;
      gttRightBrace:
        begin
          Dec(BraceCount);
          Advance;
        end;
    else
      Advance;
    end;
  end;
end;

procedure TGocciaParser.SkipExpression;
var
  ParenCount, BracketCount, BraceCount: Integer;
begin
  ParenCount := 0;
  BracketCount := 0;
  BraceCount := 0;

  // Skip tokens until we reach a comma or right parenthesis at the same nesting level
  while not IsAtEnd do
  begin
    // Check if we've reached the end of the parameter at the top level
    if (ParenCount = 0) and (BracketCount = 0) and (BraceCount = 0) and
       (Check(gttComma) or Check(gttRightParen)) then
      Break;

    case Peek.TokenType of
      gttLeftParen:
        begin
          Inc(ParenCount);
          Advance;
        end;
      gttRightParen:
        begin
          Dec(ParenCount);
          Advance;
        end;
      gttLeftBracket:
        begin
          Inc(BracketCount);
          Advance;
        end;
      gttRightBracket:
        begin
          Dec(BracketCount);
          Advance;
        end;
      gttLeftBrace:
        begin
          Inc(BraceCount);
          Advance;
        end;
      gttRightBrace:
        begin
          Dec(BraceCount);
          Advance;
        end;
    else
      Advance;
    end;
  end;
end;

end.
