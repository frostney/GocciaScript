unit Goccia.Parser;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaParser = class
  private
    type
      TParseFunction = function: TGocciaExpression of object;
  private
    FTokens: TObjectList<TGocciaToken>;
    FCurrent: Integer;
    FFileName: string;
    FSourceLines: TStringList;

    function IsAtEnd: Boolean; inline;
    function Peek: TGocciaToken; inline;
    function Previous: TGocciaToken; inline;
    function Advance: TGocciaToken;
    function Check(const ATokenType: TGocciaTokenType): Boolean; inline;
    function CheckNext(const ATokenType: TGocciaTokenType): Boolean; inline;
    function Match(const ATokenTypes: array of TGocciaTokenType): Boolean;
    function Consume(const ATokenType: TGocciaTokenType; const AMessage: string): TGocciaToken;
    function IsArrowFunction: Boolean;
    function ConvertNumberLiteral(const ALexeme: string): Double;
    function ParseBinaryExpression(const ANextLevel: TParseFunction; const AOperators: array of TGocciaTokenType): TGocciaExpression;

    // Expression parsing (private)
    function Conditional: TGocciaExpression;
    function LogicalOr: TGocciaExpression;
    function NullishCoalescing: TGocciaExpression;
    function LogicalAnd: TGocciaExpression;

    // Parameter list parsing (shared by arrow functions, class methods, object methods)
    function ParseParameterList: TGocciaParameterArray;

    // Getter/setter expression parsing (shared by object literals and class bodies)
    function ParseGetterExpression: TGocciaGetterExpression;
    function ParseSetterExpression: TGocciaSetterExpression;

    // Object method body parsing: (params) { stmts } -> method expression
    function ParseObjectMethodBody(const ALine, AColumn: Integer): TGocciaExpression;

    // Destructuring pattern parsing
    function ParsePattern: TGocciaDestructuringPattern;
    function ParseArrayPattern: TGocciaArrayDestructuringPattern;
    function ParseObjectPattern: TGocciaObjectDestructuringPattern;
    function IsAssignmentPattern(const AExpr: TGocciaExpression): Boolean;
    function ConvertToPattern(const AExpr: TGocciaExpression): TGocciaDestructuringPattern;
    procedure SkipDestructuringPattern;
    procedure SkipExpression;
    // Type annotation helpers (Types as Comments)
    function CollectTypeAnnotation(const ATerminators: array of TGocciaTokenType): string;
    function CollectGenericParameters: string;
    procedure SkipUntilSemicolon;
    procedure SkipInterfaceDeclaration;
    function IsTypeDeclaration: Boolean;

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
    // These return the base TGocciaStatement type; narrowing to specific subtypes
    // (e.g., TGocciaIfStatement) would improve type safety but requires updating all callers.
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
    function ClassMethod(const AIsStatic: Boolean = False): TGocciaClassMethod;
    function ClassDeclaration: TGocciaStatement;
    function ImportDeclaration: TGocciaStatement;
    function ExportDeclaration: TGocciaStatement;
    function ParseClassBody(const AClassName: string): TGocciaClassDefinition;
    function SwitchStatement: TGocciaStatement;
    function BreakStatement: TGocciaStatement;
  public
    constructor Create(const ATokens: TObjectList<TGocciaToken>;
      const AFileName: string; const ASourceLines: TStringList);
    function Parse: TGocciaProgram;
    function Expression: TGocciaExpression;
  end;

implementation

constructor TGocciaParser.Create(const ATokens: TObjectList<TGocciaToken>;
  const AFileName: string; const ASourceLines: TStringList);
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

function TGocciaParser.Check(const ATokenType: TGocciaTokenType): Boolean;
begin
  if IsAtEnd then
    Exit(False);
  Result := Peek.TokenType = ATokenType;
end;

function TGocciaParser.CheckNext(const ATokenType: TGocciaTokenType): Boolean;
begin
  if FCurrent + 1 >= FTokens.Count then
    Exit(False);
  Result := FTokens[FCurrent + 1].TokenType = ATokenType;
end;

function TGocciaParser.Match(const ATokenTypes: array of TGocciaTokenType): Boolean;
var
  TokenType: TGocciaTokenType;
begin
  for TokenType in ATokenTypes do
  begin
    if Check(TokenType) then
    begin
      Advance;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TGocciaParser.Consume(const ATokenType: TGocciaTokenType;
  const AMessage: string): TGocciaToken;
begin
  if Check(ATokenType) then
    Exit(Advance);

  raise TGocciaSyntaxError.Create(AMessage, Peek.Line, Peek.Column,
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
    Consequent := Assignment;  // ECMAScript spec: AssignmentExpression, not Expression
    Consume(gttColon, 'Expected ":" in conditional expression');
    Alternate := Assignment;   // ECMAScript spec: AssignmentExpression
    Result := TGocciaConditionalExpression.Create(Condition, Consequent,
      Alternate, Line, Column);
  end;
end;

function TGocciaParser.ParseBinaryExpression(const ANextLevel: TParseFunction; const AOperators: array of TGocciaTokenType): TGocciaExpression;
var
  Op: TGocciaToken;
begin
  Result := ANextLevel();
  while Match(AOperators) do
  begin
    Op := Previous;
    Result := TGocciaBinaryExpression.Create(Result, Op.TokenType, ANextLevel(), Op.Line, Op.Column);
  end;
end;

function TGocciaParser.LogicalOr: TGocciaExpression;
begin
  Result := ParseBinaryExpression(NullishCoalescing, [gttOr]);
end;

function TGocciaParser.NullishCoalescing: TGocciaExpression;
begin
  Result := ParseBinaryExpression(LogicalAnd, [gttNullishCoalescing]);
end;

function TGocciaParser.LogicalAnd: TGocciaExpression;
begin
  Result := ParseBinaryExpression(BitwiseOr, [gttAnd]);
end;

function TGocciaParser.Equality: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Comparison, [gttEqual, gttNotEqual]);
end;

function TGocciaParser.Comparison: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Shift, [gttGreater, gttGreaterEqual, gttLess, gttLessEqual, gttInstanceof, gttIn]);

  while Check(gttAs) do
  begin
    Advance;
    if Check(gttConst) then
      Advance
    else
      CollectTypeAnnotation([gttSemicolon, gttComma, gttRightParen, gttRightBracket, gttRightBrace, gttColon, gttQuestion,
        gttAnd, gttOr, gttNullishCoalescing,
        gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower,
        gttEqual, gttNotEqual,
        gttAssign, gttPlusAssign, gttMinusAssign, gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign,
        gttInstanceof, gttIn]);
  end;
end;

function TGocciaParser.Addition: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Multiplication, [gttPlus, gttMinus]);
end;

function TGocciaParser.Multiplication: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Exponentiation, [gttStar, gttSlash, gttPercent]);
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
  IsOptionalChain: Boolean;
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
    else if Match([gttDot, gttOptionalChaining]) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      IsOptionalChain := (Previous.TokenType = gttOptionalChaining);

      // Check if this is a private field access (this.#field)
      if Check(gttHash) then
      begin
        Advance; // consume the #
        PropertyName := Consume(gttIdentifier, 'Expected private field name after "#"').Lexeme;
        Result := TGocciaPrivateMemberExpression.Create(Result, PropertyName, Line, Column);
      end
      else if Check(gttLeftBracket) and IsOptionalChain then
      begin
        // Optional chaining with computed property: obj?.[expr]
        Advance; // consume [
        Arg := Expression;
        Consume(gttRightBracket, 'Expected "]" after computed member expression');
        Result := TGocciaMemberExpression.Create(Result, Arg, Line, Column, True);
      end
      else if Check(gttLeftParen) and IsOptionalChain then
      begin
        // Optional chaining with call: func?.()
        Advance; // consume (
        Arguments := TObjectList<TGocciaExpression>.Create(True);
        try
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
          // Wrap call in optional chaining by wrapping the callee in an optional member
          // For func?.(), we create a call on an optional member access
          Result := TGocciaCallExpression.Create(Result, Arguments, Line, Column);
        except
          Arguments.Free;
          raise;
        end;
      end
      else
      begin
        if Check(gttIdentifier) then
          PropertyName := Advance.Lexeme
        else if Match([gttIf, gttElse, gttConst, gttLet, gttClass, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
                       gttReturn, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak,
                       gttThrow, gttTry, gttCatch, gttFinally, gttImport, gttExport, gttFrom, gttAs,
                       gttTrue, gttFalse, gttNull, gttUndefined, gttTypeof, gttInstanceof, gttIn, gttDelete]) then
          PropertyName := Previous.Lexeme  // Reserved words are allowed as property names
        else
          raise TGocciaSyntaxError.Create('Expected property name after "."', Peek.Line, Peek.Column, FFileName, FSourceLines);

        Result := TGocciaMemberExpression.Create(Result, PropertyName, False,
          Line, Column, IsOptionalChain);
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
      TGocciaBooleanLiteralValue.TrueValue, Token.Line, Token.Column);
  end
  else if Match([gttFalse]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaBooleanLiteralValue.FalseValue, Token.Line, Token.Column);
  end
  else if Match([gttNull]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaNullLiteralValue.Create, Token.Line, Token.Column);
  end
  else if Match([gttUndefined]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue, Token.Line, Token.Column);
  end
  else if Match([gttNumber]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaNumberLiteralValue.Create(ConvertNumberLiteral(Token.Lexeme)), Token.Line, Token.Column);
  end
  else if Match([gttString]) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaStringLiteralValue.Create(Token.Lexeme), Token.Line, Token.Column);
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
    // Parse the callee: class name with optional member access (e.g. new a.B.C())
    // but NOT call expressions — those belong to the outer Call loop.
    Expr := Primary;
    while Check(gttDot) do
    begin
      Advance;
      if Check(gttIdentifier) then
        Expr := TGocciaMemberExpression.Create(Expr, Advance.Lexeme, False, Previous.Line, Previous.Column, False)
      else
        raise TGocciaSyntaxError.Create('Expected property name after "."', Peek.Line, Peek.Column, FFileName, FSourceLines);
    end;
    // Parse constructor arguments if present
    if Match([gttLeftParen]) then
    begin
      Args := TObjectList<TGocciaExpression>.Create(True);
      if not Check(gttRightParen) then
      begin
        repeat
          if Match([gttSpread]) then
            Args.Add(TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column))
          else
            Args.Add(Expression);
        until not Match([gttComma]);
      end;
      Consume(gttRightParen, 'Expected ")" after constructor arguments');
      Result := TGocciaNewExpression.Create(Expr, Args, Token.Line, Token.Column);
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
                   gttTrue, gttFalse, gttNull, gttUndefined, gttTypeof, gttInstanceof, gttIn, gttDelete]) then
      Key := Previous.Lexeme  // Reserved words are allowed as property names
    else
      raise TGocciaSyntaxError.Create('Expected property name', Peek.Line, Peek.Column, FFileName, FSourceLines);

    // Handle getter/setter syntax
    if IsGetter then
    begin
      Getters.Add(Key, ParseGetterExpression);

      // Track in source order
      Inc(SourceOrderCount);
      SetLength(PropertySourceOrder, SourceOrderCount);
      PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstGetter;
      PropertySourceOrder[SourceOrderCount - 1].StaticKey := Key;
      PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := -1;
    end
    else if IsSetter then
    begin
      Setters.Add(Key, ParseSetterExpression);

      // Track in source order
      Inc(SourceOrderCount);
      SetLength(PropertySourceOrder, SourceOrderCount);
      PropertySourceOrder[SourceOrderCount - 1].PropertyType := pstSetter;
      PropertySourceOrder[SourceOrderCount - 1].StaticKey := Key;
      PropertySourceOrder[SourceOrderCount - 1].ComputedIndex := -1;
    end
    // Check for method shorthand syntax: methodName() { ... } or [expr]() { ... }
    else if Check(gttLeftParen) then
      Value := ParseObjectMethodBody(Peek.Line, Peek.Column)
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

function TGocciaParser.ParseParameterList: TGocciaParameterArray;
var
  ParamCount: Integer;
  ParamName: string;
  DefaultValue: TGocciaExpression;
  Pattern: TGocciaDestructuringPattern;
begin
  ParamCount := 0;

  if not Check(gttRightParen) then
  begin
    repeat
      SetLength(Result, ParamCount + 1);
      Result[ParamCount].IsRest := False;
      Result[ParamCount].IsOptional := False;
      Result[ParamCount].TypeAnnotation := '';

      if Match([gttSpread]) then
      begin
        ParamName := Consume(gttIdentifier, 'Expected parameter name after "..."').Lexeme;
        Result[ParamCount].Name := ParamName;
        Result[ParamCount].IsPattern := False;
        Result[ParamCount].Pattern := nil;
        Result[ParamCount].DefaultValue := nil;
        Result[ParamCount].IsRest := True;
        if Check(gttColon) then
        begin
          Advance;
          Result[ParamCount].TypeAnnotation := CollectTypeAnnotation([gttRightParen, gttComma]);
        end;
        Inc(ParamCount);
        Break;
      end
      else if Check(gttLeftBracket) or Check(gttLeftBrace) then
      begin
        Pattern := ParsePattern;
        Result[ParamCount].Pattern := Pattern;
        Result[ParamCount].IsPattern := True;
        Result[ParamCount].Name := '';

        if Check(gttColon) then
        begin
          Advance;
          Result[ParamCount].TypeAnnotation := CollectTypeAnnotation([gttAssign, gttRightParen, gttComma]);
        end;

        if Match([gttAssign]) then
          Result[ParamCount].DefaultValue := Assignment
        else
          Result[ParamCount].DefaultValue := nil;
      end
      else
      begin
        ParamName := Consume(gttIdentifier, 'Expected parameter name').Lexeme;
        Result[ParamCount].Name := ParamName;
        Result[ParamCount].IsPattern := False;
        Result[ParamCount].Pattern := nil;

        if Check(gttQuestion) then
        begin
          Advance;
          Result[ParamCount].IsOptional := True;
        end;

        if Check(gttColon) then
        begin
          Advance;
          Result[ParamCount].TypeAnnotation := CollectTypeAnnotation([gttAssign, gttRightParen, gttComma]);
        end;

        if Match([gttAssign]) then
          Result[ParamCount].DefaultValue := Assignment
        else
          Result[ParamCount].DefaultValue := nil;
      end;

      Inc(ParamCount);
    until not Match([gttComma]);
  end;

  SetLength(Result, ParamCount);
end;

function TGocciaParser.ParseGetterExpression: TGocciaGetterExpression;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Consume(gttLeftParen, 'Expected "(" after getter name');
  Consume(gttRightParen, 'Getters cannot have parameters');
  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttLeftBrace]);
  end;
  Consume(gttLeftBrace, 'Expected "{" before getter body');
  Result := TGocciaGetterExpression.Create(BlockStatement, Line, Column);
end;

function TGocciaParser.ParseSetterExpression: TGocciaSetterExpression;
var
  ParamName: string;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Consume(gttLeftParen, 'Expected "(" after setter name');
  if Check(gttRightParen) then
    raise TGocciaSyntaxError.Create('Setter must have exactly one parameter',
      Peek.Line, Peek.Column, FFileName, FSourceLines);
  ParamName := Consume(gttIdentifier, 'Expected parameter name in setter').Lexeme;
  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttRightParen]);
  end;
  Consume(gttRightParen, 'Expected ")" after setter parameter');
  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttLeftBrace]);
  end;
  Consume(gttLeftBrace, 'Expected "{" before setter body');
  Result := TGocciaSetterExpression.Create(ParamName, BlockStatement, Line, Column);
end;

function TGocciaParser.ParseObjectMethodBody(const ALine, AColumn: Integer): TGocciaExpression;
var
  Parameters: TGocciaParameterArray;
  Body: TGocciaASTNode;
  Statements: TObjectList<TGocciaStatement>;
  Stmt: TGocciaStatement;
begin
  Consume(gttLeftParen, 'Expected "(" after method name');
  Parameters := ParseParameterList;
  Consume(gttRightParen, 'Expected ")" after parameters');

  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttLeftBrace]);
  end;

  Consume(gttLeftBrace, 'Expected "{" before method body');

  Statements := TObjectList<TGocciaStatement>.Create(True);
  try
    while not Check(gttRightBrace) and not IsAtEnd do
    begin
      Stmt := Statement;
      Statements.Add(Stmt);
    end;

    Consume(gttRightBrace, 'Expected "}" after method body');
    Body := TGocciaBlockStatement.Create(TObjectList<TGocciaASTNode>(Statements), ALine, AColumn);
    Result := TGocciaMethodExpression.Create(Parameters, Body, ALine, AColumn);
  except
    Statements.Free;
    raise;
  end;
end;

function TGocciaParser.ArrowFunction: TGocciaExpression;
var
  Parameters: TGocciaParameterArray;
  Body: TGocciaASTNode;
  Line, Column: Integer;
  ArrowFn: TGocciaArrowFunctionExpression;
  FnReturnType: string;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Parameters := ParseParameterList;
  Consume(gttRightParen, 'Expected ")" after parameters');

  FnReturnType := '';
  if Check(gttColon) then
  begin
    Advance;
    FnReturnType := CollectTypeAnnotation([gttArrow]);
  end;

  Consume(gttArrow, 'Expected "=>" in arrow function');

  if Match([gttLeftBrace]) then
    Body := BlockStatement
  else
    Body := Expression;

  ArrowFn := TGocciaArrowFunctionExpression.Create(Parameters, Body, Line, Column);
  ArrowFn.ReturnType := FnReturnType;
  Result := ArrowFn;
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
var
  Line, Column: Integer;
begin
  if Check(gttIdentifier) and (Peek.Lexeme = 'type') and IsTypeDeclaration then
  begin
    Line := Peek.Line;
    Column := Peek.Column;
    Advance;
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
  end
  else if Check(gttIdentifier) and (Peek.Lexeme = 'interface') and IsTypeDeclaration then
  begin
    Line := Peek.Line;
    Column := Peek.Column;
    Advance;
    Advance;
    SkipInterfaceDeclaration;
    Result := TGocciaEmptyStatement.Create(Line, Column);
  end
  else if Match([gttConst, gttLet]) then
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
  DestructuringType: string;
  DestructuringDecl: TGocciaDestructuringDeclaration;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  IsConst := Previous.TokenType = gttConst;
  VariableCount := 0;

  // Check for destructuring pattern
  if Check(gttLeftBracket) or Check(gttLeftBrace) then
  begin
    Pattern := ParsePattern;
    DestructuringType := '';
    if Check(gttColon) then
    begin
      Advance;
      DestructuringType := CollectTypeAnnotation([gttAssign]);
    end;
    Consume(gttAssign, 'Destructuring declarations must have an initializer');
    Initializer := Expression;
    Consume(gttSemicolon, 'Expected ";" after destructuring declaration');
    DestructuringDecl := TGocciaDestructuringDeclaration.Create(Pattern, Initializer, IsConst, Line, Column);
    DestructuringDecl.TypeAnnotation := DestructuringType;
    Result := DestructuringDecl;
  end
  else
  begin
    repeat
      SetLength(Variables, VariableCount + 1);

      Name := Consume(gttIdentifier, 'Expected variable name').Lexeme;
      Variables[VariableCount].Name := Name;

      if Check(gttColon) then
      begin
        Advance;
        Variables[VariableCount].TypeAnnotation := CollectTypeAnnotation([gttAssign, gttSemicolon, gttComma]);
      end;

      if Match([gttAssign]) then
        Variables[VariableCount].Initializer := Expression
      else if IsConst then
        raise TGocciaSyntaxError.Create('const declarations must have an initializer',
          Line, Column, FFileName, FSourceLines)
      else
        Variables[VariableCount].Initializer := TGocciaLiteralExpression.Create(
          TGocciaUndefinedLiteralValue.UndefinedValue, Line, Column);

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
  TryStmt: TGocciaTryStatement;
  CatchType: string;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Consume(gttLeftBrace, 'Expected "{" after "try"');
  Block := BlockStatement;
  CatchParam := '';
  CatchBlock := nil;
  FinallyBlock := nil;
  CatchType := '';

  if Match([gttCatch]) then
  begin
    if Check(gttLeftParen) then
    begin
      Consume(gttLeftParen, 'Expected "(" after "catch"');
      CatchParam := Consume(gttIdentifier, 'Expected catch parameter').Lexeme;
      if Check(gttColon) then
      begin
        Advance;
        CatchType := CollectTypeAnnotation([gttRightParen]);
      end;
      Consume(gttRightParen, 'Expected ")" after catch parameter');
    end
    else
      CatchParam := '';

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

  TryStmt := TGocciaTryStatement.Create(Block, CatchParam, CatchBlock,
    FinallyBlock, Line, Column);
  TryStmt.CatchParamType := CatchType;
  Result := TryStmt;
end;

function TGocciaParser.ClassMethod(const AIsStatic: Boolean = False): TGocciaClassMethod;
var
  Parameters: TGocciaParameterArray;
  Body: TGocciaASTNode;
  Name: string;
  Line, Column: Integer;
  Statements: TObjectList<TGocciaStatement>;
  Stmt: TGocciaStatement;
  MethodGenericParams, MethodReturnType: string;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  MethodGenericParams := CollectGenericParameters;

  Consume(gttLeftParen, 'Expected "(" after method name');
  Parameters := ParseParameterList;
  Consume(gttRightParen, 'Expected ")" after parameters');

  MethodReturnType := '';
  if Check(gttColon) then
  begin
    Advance;
    MethodReturnType := CollectTypeAnnotation([gttLeftBrace]);
  end;

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
    Result := TGocciaClassMethod.Create(Name, Parameters, Body, AIsStatic, Line, Column);
    Result.GenericParams := MethodGenericParams;
    Result.ReturnType := MethodReturnType;
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

  if Check(gttIdentifier) and (Peek.Lexeme = 'type') then
  begin
    Advance;
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

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
  LocalName, ExportedName, ModulePath: string;
  Line, Column: Integer;
  InnerDecl: TGocciaStatement;
  VarDecl: TGocciaVariableDeclaration;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  if Check(gttIdentifier) and (Peek.Lexeme = 'type') then
  begin
    Advance;
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  if Match([gttConst, gttLet]) then
  begin
    InnerDecl := DeclarationStatement;
    if not (InnerDecl is TGocciaVariableDeclaration) then
      raise TGocciaSyntaxError.Create('Destructuring exports are not supported; use export { name } instead',
        Line, Column, FFileName, FSourceLines);
    VarDecl := TGocciaVariableDeclaration(InnerDecl);
    Result := TGocciaExportVariableDeclaration.Create(VarDecl, Line, Column);
    Exit;
  end;

  Consume(gttLeftBrace, 'Expected "{", "const", or "let" after "export"');

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

  if Match([gttFrom]) then
  begin
    ModulePath := Consume(gttString, 'Expected module path after "from"').Lexeme;
    Consume(gttSemicolon, 'Expected ";" after re-export declaration');
    Result := TGocciaReExportDeclaration.Create(ExportsTable, ModulePath, Line, Column);
  end
  else
  begin
    Consume(gttSemicolon, 'Expected ";" after export declaration');
    Result := TGocciaExportDeclaration.Create(ExportsTable, Line, Column);
  end;
end;

function TGocciaParser.ParseClassBody(const AClassName: string): TGocciaClassDefinition;
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
  InstancePropertyOrder: TStringList;
  PrivateInstancePropertyOrder: TStringList;
  MemberName: string;
  Method: TGocciaClassMethod;
  Getter: TGocciaGetterExpression;
  Setter: TGocciaSetterExpression;
  PropertyValue: TGocciaExpression;
  IsStatic: Boolean;
  IsPrivate: Boolean;
  IsGetter: Boolean;
  IsSetter: Boolean;
  ClassGenericParams, ClassImplementsClause, FieldType: string;
  InstancePropertyTypes: TDictionary<string, string>;
begin
  ClassGenericParams := CollectGenericParameters;

  if Match([gttExtends]) then
  begin
    SuperClass := Consume(gttIdentifier, 'Expected superclass name').Lexeme;
    CollectGenericParameters;
  end
  else
    SuperClass := '';

  ClassImplementsClause := '';
  if Check(gttIdentifier) and (Peek.Lexeme = 'implements') then
  begin
    Advance;
    ClassImplementsClause := CollectTypeAnnotation([gttLeftBrace]);
  end;

  Consume(gttLeftBrace, 'Expected "{" before class body');

  Methods := TDictionary<string, TGocciaClassMethod>.Create;
  Getters := TDictionary<string, TGocciaGetterExpression>.Create;
  Setters := TDictionary<string, TGocciaSetterExpression>.Create;
  StaticProperties := TDictionary<string, TGocciaExpression>.Create;
  InstanceProperties := TDictionary<string, TGocciaExpression>.Create;
  PrivateInstanceProperties := TDictionary<string, TGocciaExpression>.Create;
  PrivateStaticProperties := TDictionary<string, TGocciaExpression>.Create;
  PrivateMethods := TDictionary<string, TGocciaClassMethod>.Create;
  InstancePropertyOrder := TStringList.Create;
  PrivateInstancePropertyOrder := TStringList.Create;
  InstancePropertyTypes := TDictionary<string, string>.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    IsStatic := Match([gttStatic]);

    while Check(gttIdentifier) and
      ((Peek.Lexeme = 'public') or (Peek.Lexeme = 'protected') or (Peek.Lexeme = 'private') or
       (Peek.Lexeme = 'readonly') or (Peek.Lexeme = 'override') or (Peek.Lexeme = 'abstract')) do
      Advance;

    if not IsStatic and Check(gttStatic) then
      IsStatic := Match([gttStatic]);

    IsPrivate := Match([gttHash]);
    IsGetter := False;
    IsSetter := False;

    if Check(gttIdentifier) and (Peek.Lexeme = 'get') and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) and not CheckNext(gttSemicolon) and not CheckNext(gttAssign) and not CheckNext(gttQuestion) then
    begin
      Advance;
      IsGetter := True;

      if Check(gttHash) then
      begin
        Advance;
        IsPrivate := True;
        MemberName := Consume(gttIdentifier, 'Expected property name after "#"').Lexeme;
      end
      else
        MemberName := Consume(gttIdentifier, 'Expected property name after "get"').Lexeme;
    end
    else if Check(gttIdentifier) and (Peek.Lexeme = 'set') and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) and not CheckNext(gttSemicolon) and not CheckNext(gttAssign) and not CheckNext(gttQuestion) then
    begin
      Advance; // consume 'set'
      IsSetter := True;

      if Check(gttHash) then
      begin
        Advance;
        IsPrivate := True;
        MemberName := Consume(gttIdentifier, 'Expected property name after "#"').Lexeme;
      end
      else
        MemberName := Consume(gttIdentifier, 'Expected property name after "set"').Lexeme;
    end
    else
    begin
      MemberName := Consume(gttIdentifier, 'Expected method or property name').Lexeme;
    end;

    FieldType := '';
    if Check(gttQuestion) then
      Advance;
    if Check(gttColon) and not IsGetter and not IsSetter then
    begin
      Advance;
      FieldType := CollectTypeAnnotation([gttAssign, gttSemicolon, gttLeftParen]);
    end;

    if Check(gttAssign) then
    begin
      Consume(gttAssign, 'Expected "=" in property');
      PropertyValue := Expression;
      Consume(gttSemicolon, 'Expected ";" after property');

      if IsPrivate and IsStatic then
        PrivateStaticProperties.Add(MemberName, PropertyValue)
      else if IsPrivate then
      begin
        PrivateInstanceProperties.Add(MemberName, PropertyValue);
        PrivateInstancePropertyOrder.Add(MemberName);
      end
      else if IsStatic then
        StaticProperties.Add(MemberName, PropertyValue)
      else
      begin
        InstanceProperties.Add(MemberName, PropertyValue);
        InstancePropertyOrder.Add(MemberName);
        if FieldType <> '' then
          InstancePropertyTypes.Add(MemberName, FieldType);
      end;
    end
    else if Check(gttSemicolon) then
    begin
      Consume(gttSemicolon, 'Expected ";" after property declaration');
      PropertyValue := TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, Peek.Line, Peek.Column);

      if IsPrivate and IsStatic then
        PrivateStaticProperties.Add(MemberName, PropertyValue)
      else if IsPrivate then
      begin
        PrivateInstanceProperties.Add(MemberName, PropertyValue);
        PrivateInstancePropertyOrder.Add(MemberName);
      end
      else if IsStatic then
        StaticProperties.Add(MemberName, PropertyValue)
      else
      begin
        InstanceProperties.Add(MemberName, PropertyValue);
        InstancePropertyOrder.Add(MemberName);
        if FieldType <> '' then
          InstancePropertyTypes.Add(MemberName, FieldType);
      end;
    end
    else if IsGetter then
    begin
      if IsStatic then
        raise TGocciaSyntaxError.Create('Static getters not supported yet', Peek.Line, Peek.Column, FFileName, FSourceLines);

      Getter := ParseGetterExpression;
      if IsPrivate then
        Getters.Add('#' + MemberName, Getter)
      else
        Getters.Add(MemberName, Getter);
    end
    else if IsSetter then
    begin
      if IsStatic then
        raise TGocciaSyntaxError.Create('Static setters not supported yet', Peek.Line, Peek.Column, FFileName, FSourceLines);

      Setter := ParseSetterExpression;
      if IsPrivate then
        Setters.Add('#' + MemberName, Setter)
      else
        Setters.Add(MemberName, Setter);
    end
    else if Check(gttLeftParen) or Check(gttLess) then
    begin
      Method := ClassMethod(IsStatic);
      Method.Name := MemberName;

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
  Result := TGocciaClassDefinition.Create(AClassName, SuperClass, Methods, Getters, Setters, StaticProperties, InstanceProperties, PrivateInstanceProperties, PrivateMethods, PrivateStaticProperties);
  Result.GenericParams := ClassGenericParams;
  Result.ImplementsClause := ClassImplementsClause;
  Result.FInstancePropertyOrder.Assign(InstancePropertyOrder);
  Result.FPrivateInstancePropertyOrder.Assign(PrivateInstancePropertyOrder);
  for MemberName in InstancePropertyTypes.Keys do
    Result.FInstancePropertyTypes.Add(MemberName, InstancePropertyTypes[MemberName]);
  InstancePropertyOrder.Free;
  PrivateInstancePropertyOrder.Free;
  InstancePropertyTypes.Free;
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

    if Check(gttRightParen) then
    begin
      Advance;
      if Check(gttColon) then
      begin
        Advance;
        CollectTypeAnnotation([gttArrow]);
      end;
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
            Advance;
            SkipDestructuringPattern;
            if Check(gttColon) then
            begin
              Advance;
              CollectTypeAnnotation([gttAssign, gttRightParen, gttComma]);
            end;
            if Check(gttAssign) then
            begin
              Advance;
              SkipExpression;
            end;
            if Check(gttComma) then
              Advance;
          end;
        gttSpread:
          begin
            Advance;
            if Check(gttIdentifier) then
              Advance;
            if Check(gttColon) then
            begin
              Advance;
              CollectTypeAnnotation([gttRightParen, gttComma]);
            end;
          end;
        gttIdentifier:
          begin
            Advance;
            if Check(gttQuestion) then
              Advance;
            if Check(gttColon) then
            begin
              Advance;
              CollectTypeAnnotation([gttAssign, gttRightParen, gttComma]);
            end;
            if Check(gttAssign) then
            begin
              Advance;
              SkipExpression;
            end;
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

    if Check(gttColon) then
    begin
      Advance;
      CollectTypeAnnotation([gttArrow]);
    end;

    Result := Check(gttArrow);
  finally
    FCurrent := SavedCurrent;
  end;
end;

{ Type annotation helpers (Types as Comments) }

function TGocciaParser.CollectTypeAnnotation(const ATerminators: array of TGocciaTokenType): string;
var
  Depth: Integer;
  I: Integer;
  TokenType: TGocciaTokenType;
  IsTerminator: Boolean;
begin
  Result := '';
  Depth := 0;

  while not IsAtEnd do
  begin
    TokenType := Peek.TokenType;

    if Depth = 0 then
    begin
      IsTerminator := False;
      for I := 0 to High(ATerminators) do
        if TokenType = ATerminators[I] then
        begin
          IsTerminator := True;
          Break;
        end;
      if IsTerminator then
        Exit;
    end;

    case TokenType of
      gttLeftParen: Inc(Depth);
      gttRightParen: Dec(Depth);
      gttLeftBracket: Inc(Depth);
      gttRightBracket: Dec(Depth);
      gttLeftBrace: Inc(Depth);
      gttRightBrace: Dec(Depth);
      gttLess: Inc(Depth);
      gttGreater: Dec(Depth);
      gttRightShift: Dec(Depth, 2);
      gttUnsignedRightShift: Dec(Depth, 3);
    end;

    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Peek.Lexeme;
    Advance;
  end;
end;

function TGocciaParser.CollectGenericParameters: string;
var
  Depth: Integer;
begin
  Result := '';
  if not Check(gttLess) then
    Exit;

  Result := Peek.Lexeme;
  Advance;
  Depth := 1;

  while not IsAtEnd and (Depth > 0) do
  begin
    case Peek.TokenType of
      gttLess: Inc(Depth);
      gttGreater: Dec(Depth);
      gttRightShift:
      begin
        Dec(Depth, 2);
        Result := Result + ' ' + Peek.Lexeme;
        Advance;
        Continue;
      end;
      gttUnsignedRightShift:
      begin
        Dec(Depth, 3);
        Result := Result + ' ' + Peek.Lexeme;
        Advance;
        Continue;
      end;
    end;
    Result := Result + ' ' + Peek.Lexeme;
    Advance;
  end;

  Result := Trim(Result);
end;

procedure TGocciaParser.SkipUntilSemicolon;
var
  Depth: Integer;
begin
  Depth := 0;
  while not IsAtEnd do
  begin
    case Peek.TokenType of
      gttLeftParen, gttLeftBracket, gttLeftBrace, gttLess: Inc(Depth);
      gttRightParen, gttRightBracket, gttRightBrace, gttGreater: Dec(Depth);
      gttRightShift: Dec(Depth, 2);
      gttUnsignedRightShift: Dec(Depth, 3);
      gttSemicolon:
        if Depth = 0 then
        begin
          Advance;
          Exit;
        end;
    end;
    Advance;
  end;
end;

procedure TGocciaParser.SkipInterfaceDeclaration;
var
  Depth: Integer;
begin
  while not IsAtEnd and not Check(gttLeftBrace) do
    Advance;

  if Check(gttLeftBrace) then
  begin
    Advance;
    Depth := 1;
    while not IsAtEnd and (Depth > 0) do
    begin
      if Check(gttLeftBrace) then Inc(Depth);
      if Check(gttRightBrace) then Dec(Depth);
      Advance;
    end;
  end;
end;

function TGocciaParser.IsTypeDeclaration: Boolean;
var
  SavedCurrent: Integer;
begin
  SavedCurrent := FCurrent;
  try
    Advance;
    Result := Check(gttIdentifier);
  finally
    FCurrent := SavedCurrent;
  end;
end;

function TGocciaParser.BitwiseOr: TGocciaExpression;
begin
  Result := ParseBinaryExpression(BitwiseXor, [gttBitwiseOr]);
end;

function TGocciaParser.BitwiseXor: TGocciaExpression;
begin
  Result := ParseBinaryExpression(BitwiseAnd, [gttBitwiseXor]);
end;

function TGocciaParser.BitwiseAnd: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Equality, [gttBitwiseAnd]);
end;

function TGocciaParser.Shift: TGocciaExpression;
begin
  Result := ParseBinaryExpression(Addition, [gttLeftShift, gttRightShift, gttUnsignedRightShift]);
end;

function TGocciaParser.ConvertNumberLiteral(const ALexeme: string): Double;
var
  Value: Double;
  IntValue: Int64;
  HexStr, BinStr, OctStr: string;
  I: Integer;
begin
  // Handle special number formats
  if Length(ALexeme) >= 3 then
  begin
    // Hexadecimal: 0x10, 0X10
    if (ALexeme[1] = '0') and ((ALexeme[2] = 'x') or (ALexeme[2] = 'X')) then
    begin
      HexStr := Copy(ALexeme, 3, Length(ALexeme) - 2);
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
    else if (ALexeme[1] = '0') and ((ALexeme[2] = 'b') or (ALexeme[2] = 'B')) then
    begin
      BinStr := Copy(ALexeme, 3, Length(ALexeme) - 2);
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
    else if (ALexeme[1] = '0') and ((ALexeme[2] = 'o') or (ALexeme[2] = 'O')) then
    begin
      OctStr := Copy(ALexeme, 3, Length(ALexeme) - 2);
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
  if TryStrToFloat(ALexeme, Value) then
    Exit(Value);

  raise TGocciaSyntaxError.Create('Invalid number format: ' + ALexeme, Peek.Line, Peek.Column,
    FFileName, FSourceLines);
end;

function TGocciaParser.IsAssignmentPattern(const AExpr: TGocciaExpression): Boolean;
begin
  // Check if an expression could be a destructuring pattern
  // Only arrays and objects are destructuring patterns for assignments
  // Simple identifiers should be regular assignments, not destructuring
  Result := (AExpr is TGocciaArrayExpression) or (AExpr is TGocciaObjectExpression);
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

function TGocciaParser.ConvertToPattern(const AExpr: TGocciaExpression): TGocciaDestructuringPattern;
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
  if AExpr is TGocciaIdentifierExpression then
  begin
    IdentifierExpr := TGocciaIdentifierExpression(AExpr);
    Result := TGocciaIdentifierDestructuringPattern.Create(IdentifierExpr.Name, AExpr.Line, AExpr.Column);
  end
  else if AExpr is TGocciaAssignmentExpression then
  begin
    // Handle assignment expressions (for default values)
    AssignmentExpr := TGocciaAssignmentExpression(AExpr);
    Result := TGocciaAssignmentDestructuringPattern.Create(
      TGocciaIdentifierDestructuringPattern.Create(AssignmentExpr.Name, AExpr.Line, AExpr.Column),
      AssignmentExpr.Value,
      AExpr.Line,
      AExpr.Column
    );
  end
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
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

    Result := TGocciaArrayDestructuringPattern.Create(Elements, AExpr.Line, AExpr.Column);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
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

    Result := TGocciaObjectDestructuringPattern.Create(Properties, AExpr.Line, AExpr.Column);
  end
  else
    raise TGocciaSyntaxError.Create('Invalid destructuring target', AExpr.Line, AExpr.Column, FFileName, FSourceLines);
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
