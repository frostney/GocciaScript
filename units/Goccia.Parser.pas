unit Goccia.Parser;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  HashMap,
  OrderedStringMap,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.Token;

type
  TGocciaParserWarning = record
    Message: string;
    Suggestion: string;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaPrivateNameReference = record
    Name: string;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaPrivateClassContext = class
  private
    FDeclaredNames: TStringList;
    FReferences: array of TGocciaPrivateNameReference;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DeclareName(const AName: string);
    procedure AddReference(const AName: string; const ALine, AColumn: Integer);
    function HasDeclaration(const AName: string): Boolean;
    function ReferenceCount: Integer;
    function GetReference(const AIndex: Integer): TGocciaPrivateNameReference;
  end;

  TGocciaParser = class
  private
    type
      TParseFunction = function: TGocciaExpression of object;
  private
    FTokens: TObjectList<TGocciaToken>;
    FCurrent: Integer;
    FFileName: string;
    FSourceLines: TStringList;
    FWarnings: array of TGocciaParserWarning;
    FWarningCount: Integer;
    FInAsyncFunction: Integer;
    FFunctionDepth: Integer;
    FPrivateClassContexts: TObjectList<TGocciaPrivateClassContext>;
    FSkipPrivateNameValidation: Integer;
    FAutomaticSemicolonInsertion: Boolean;

    procedure AddWarning(const AMessage, ASuggestion: string; const ALine, AColumn: Integer);
    procedure PushPrivateClassContext;
    procedure PopPrivateClassContext;
    procedure ValidateCurrentPrivateClassContext;
    procedure DeclarePrivateName(const AName: string);
    procedure RecordPrivateNameReference(const AName: string; const ALine, AColumn: Integer);
    function CurrentPrivateClassContext: TGocciaPrivateClassContext;

    function IsAtEnd: Boolean; inline;
    function Peek: TGocciaToken; inline;
    function Previous: TGocciaToken; inline;
    function Advance: TGocciaToken;
    function Check(const ATokenType: TGocciaTokenType): Boolean; inline;
    function CheckNext(const ATokenType: TGocciaTokenType): Boolean; inline;
    function Match(const ATokenTypes: array of TGocciaTokenType): Boolean; overload;
    function Match(const ATokenType: TGocciaTokenType): Boolean; overload; inline;
    function Consume(const ATokenType: TGocciaTokenType; const AMessage: string): TGocciaToken; overload;
    function Consume(const ATokenType: TGocciaTokenType; const AMessage, ASuggestion: string): TGocciaToken; overload;
    procedure ConsumeSemicolonOrASI(const AMessage, ASuggestion: string);
    function CheckSemicolonOrASI: Boolean;
    function IsIdentifierNameToken(
      const ATokenType: TGocciaTokenType): Boolean;
    function ConsumeModuleExportName(const AMessage: string): TGocciaToken; overload;
    function ConsumeModuleExportName(const AMessage, ASuggestion: string): TGocciaToken; overload;
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
    procedure SkipBlock;
    procedure SkipBalancedParens;
    procedure SkipStatementOrBlock;
    procedure SkipUnsupportedFunctionSignature;
    function IsTypeOnlySpecifierModifier: Boolean;
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
    function ParseTaggedTemplate(const ATag: TGocciaExpression;
      const ATemplateToken: TGocciaToken;
      const ALine, AColumn: Integer): TGocciaTaggedTemplateExpression;
    function ParseTemplateLiteral(const AToken: TGocciaToken): TGocciaExpression;
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
    function VarStatement: TGocciaStatement;
    function ForStatement: TGocciaStatement;
    function WhileStatement: TGocciaStatement;
    function DoWhileStatement: TGocciaStatement;
    function WithStatement: TGocciaStatement;
    function FunctionStatement: TGocciaStatement;
    function ReturnStatement: TGocciaStatement;
    function ThrowStatement: TGocciaStatement;
    function TryStatement: TGocciaStatement;
    function ClassMethod(const AIsStatic: Boolean = False): TGocciaClassMethod;
    function ClassDeclaration: TGocciaStatement;
    function DecoratedClassDeclaration(const ADecorators: TGocciaDecoratorList): TGocciaStatement;
    function ParseDecorators: TGocciaDecoratorList;
    function ParseDecoratorExpression: TGocciaExpression;
    function EnumDeclaration: TGocciaStatement;
    function ImportDeclaration: TGocciaStatement;
    function ExportDeclaration: TGocciaStatement;
    function ParseClassBody(const AClassName: string): TGocciaClassDefinition;
    function UsingStatement: TGocciaStatement;
    function SwitchStatement: TGocciaStatement;
    function BreakStatement: TGocciaStatement;
  public
    constructor Create(const ATokens: TObjectList<TGocciaToken>;
      const AFileName: string; const ASourceLines: TStringList);
    destructor Destroy; override;
    function Parse: TGocciaProgram;
    function ParseUnchecked: TGocciaProgram;
    function ParseExpressionWithPrivateNames(const ADeclaredNames: TStrings): TGocciaExpression;
    function ParseExpressionUnchecked: TGocciaExpression;
    function Expression: TGocciaExpression;
    function GetWarning(const AIndex: Integer): TGocciaParserWarning; inline;
    property AutomaticSemicolonInsertion: Boolean
      read FAutomaticSemicolonInsertion write FAutomaticSemicolonInsertion;
    property WarningCount: Integer read FWarningCount;
  end;

implementation

uses
  SysUtils,

  StringBuffer,

  Goccia.Error,
  Goccia.Error.Suggestions,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved,
  Goccia.Lexer,
  Goccia.Values.Primitives;

{ TGocciaPrivateClassContext }

constructor TGocciaPrivateClassContext.Create;
begin
  inherited Create;
  FDeclaredNames := TStringList.Create;
  FDeclaredNames.Sorted := False;
  FDeclaredNames.Duplicates := dupIgnore;
end;

destructor TGocciaPrivateClassContext.Destroy;
begin
  FDeclaredNames.Free;
  inherited;
end;

procedure TGocciaPrivateClassContext.DeclareName(const AName: string);
begin
  if FDeclaredNames.IndexOf(AName) < 0 then
    FDeclaredNames.Add(AName);
end;

procedure TGocciaPrivateClassContext.AddReference(const AName: string; const ALine, AColumn: Integer);
var
  Index: Integer;
begin
  Index := Length(FReferences);
  SetLength(FReferences, Index + 1);
  FReferences[Index].Name := AName;
  FReferences[Index].Line := ALine;
  FReferences[Index].Column := AColumn;
end;

function TGocciaPrivateClassContext.HasDeclaration(const AName: string): Boolean;
begin
  Result := FDeclaredNames.IndexOf(AName) >= 0;
end;

function TGocciaPrivateClassContext.ReferenceCount: Integer;
begin
  Result := Length(FReferences);
end;

function TGocciaPrivateClassContext.GetReference(const AIndex: Integer): TGocciaPrivateNameReference;
begin
  Result := FReferences[AIndex];
end;

constructor TGocciaParser.Create(const ATokens: TObjectList<TGocciaToken>;
  const AFileName: string; const ASourceLines: TStringList);
begin
  FTokens := ATokens;
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FCurrent := 0;
  FWarningCount := 0;
  FInAsyncFunction := 0;
  FFunctionDepth := 0;
  FPrivateClassContexts := TObjectList<TGocciaPrivateClassContext>.Create(True);
end;

destructor TGocciaParser.Destroy;
begin
  FPrivateClassContexts.Free;
  inherited;
end;

procedure TGocciaParser.AddWarning(const AMessage, ASuggestion: string; const ALine, AColumn: Integer);
begin
  Inc(FWarningCount);
  SetLength(FWarnings, FWarningCount);
  FWarnings[FWarningCount - 1].Message := AMessage;
  FWarnings[FWarningCount - 1].Suggestion := ASuggestion;
  FWarnings[FWarningCount - 1].Line := ALine;
  FWarnings[FWarningCount - 1].Column := AColumn;
end;

procedure TGocciaParser.PushPrivateClassContext;
begin
  FPrivateClassContexts.Add(TGocciaPrivateClassContext.Create);
end;

procedure TGocciaParser.PopPrivateClassContext;
begin
  if FPrivateClassContexts.Count > 0 then
    FPrivateClassContexts.Delete(FPrivateClassContexts.Count - 1);
end;

function TGocciaParser.CurrentPrivateClassContext: TGocciaPrivateClassContext;
begin
  if FPrivateClassContexts.Count = 0 then
    Exit(nil);
  Result := FPrivateClassContexts[FPrivateClassContexts.Count - 1];
end;

// ES2026 §15.7.7 Static Semantics: AllPrivateIdentifiersValid
procedure TGocciaParser.ValidateCurrentPrivateClassContext;
var
  Context: TGocciaPrivateClassContext;
  Ref: TGocciaPrivateNameReference;
  I: Integer;
begin
  Context := CurrentPrivateClassContext;
  if not Assigned(Context) then
    Exit;
  if FSkipPrivateNameValidation > 0 then
    Exit;

  for I := 0 to Context.ReferenceCount - 1 do
  begin
    Ref := Context.GetReference(I);
    if not Context.HasDeclaration(Ref.Name) then
      raise TGocciaSyntaxError.Create(
        Format('Private field ''#%s'' must be declared in an enclosing class', [Ref.Name]),
        Ref.Line, Ref.Column, FFileName, FSourceLines,
        SSuggestDeclarePrivateField);
  end;
end;

procedure TGocciaParser.DeclarePrivateName(const AName: string);
var
  Context: TGocciaPrivateClassContext;
begin
  Context := CurrentPrivateClassContext;
  if Assigned(Context) then
    Context.DeclareName(AName);
end;

procedure TGocciaParser.RecordPrivateNameReference(const AName: string; const ALine, AColumn: Integer);
var
  Context: TGocciaPrivateClassContext;
begin
  Context := CurrentPrivateClassContext;
  if not Assigned(Context) then
  begin
    if FSkipPrivateNameValidation > 0 then
      Exit;
    raise TGocciaSyntaxError.Create(
      Format('Private field ''#%s'' must be declared in an enclosing class', [AName]),
      ALine, AColumn, FFileName, FSourceLines,
      SSuggestDeclarePrivateField);
  end;
  Context.AddReference(AName, ALine, AColumn);
end;

function TGocciaParser.GetWarning(const AIndex: Integer): TGocciaParserWarning;
begin
  Result := FWarnings[AIndex];
end;

function TGocciaParser.ParseExpressionWithPrivateNames(const ADeclaredNames: TStrings): TGocciaExpression;
var
  I: Integer;
begin
  PushPrivateClassContext;
  try
    if Assigned(ADeclaredNames) then
      for I := 0 to ADeclaredNames.Count - 1 do
        DeclarePrivateName(ADeclaredNames[I]);

    Result := Expression;
    ValidateCurrentPrivateClassContext;
  finally
    PopPrivateClassContext;
  end;
end;

function TGocciaParser.ParseExpressionUnchecked: TGocciaExpression;
begin
  Inc(FSkipPrivateNameValidation);
  try
    Result := Expression;
  finally
    Dec(FSkipPrivateNameValidation);
  end;
end;

function TGocciaParser.ParseUnchecked: TGocciaProgram;
begin
  Inc(FSkipPrivateNameValidation);
  try
    Result := Parse;
  finally
    Dec(FSkipPrivateNameValidation);
  end;
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

function TGocciaParser.Match(const ATokenType: TGocciaTokenType): Boolean;
begin
  if Check(ATokenType) then
  begin
    Advance;
    Exit(True);
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

function TGocciaParser.Consume(const ATokenType: TGocciaTokenType;
  const AMessage, ASuggestion: string): TGocciaToken;
var
  Error: TGocciaSyntaxError;
begin
  if Check(ATokenType) then
    Exit(Advance);

  Error := TGocciaSyntaxError.Create(AMessage, Peek.Line, Peek.Column,
    FFileName, FSourceLines);
  Error.Suggestion := ASuggestion;
  raise Error;
end;

// ES2026 §12.10 Automatic Semicolon Insertion
procedure TGocciaParser.ConsumeSemicolonOrASI(const AMessage, ASuggestion: string);
begin
  if Check(gttSemicolon) then
  begin
    Advance;
    Exit;
  end;

  // ES2026 §12.10 rules 2-3: } and EOF always terminate a statement.
  // These are mandatory ASI points in the spec and apply regardless of
  // whether full ASI is enabled.
  if Check(gttRightBrace) or Check(gttEOF) then
    Exit;

  if not FAutomaticSemicolonInsertion then
  begin
    Consume(gttSemicolon, AMessage, ASuggestion);
    Exit;
  end;

  // ES2026 §12.10 rule 1: newline between previous and current token
  if Previous.Line < Peek.Line then
    Exit;

  Consume(gttSemicolon, AMessage, ASuggestion);
end;

// ES2026 §12.10 Check whether ASI would apply at current position
function TGocciaParser.CheckSemicolonOrASI: Boolean;
begin
  if Check(gttSemicolon) then
    Exit(True);
  // ES2026 §12.10 rules 2-3: } and EOF always terminate a statement
  if Check(gttRightBrace) or Check(gttEOF) then
    Exit(True);
  if not FAutomaticSemicolonInsertion then
    Exit(False);
  Result := Previous.Line < Peek.Line;
end;

function TGocciaParser.IsIdentifierNameToken(
  const ATokenType: TGocciaTokenType): Boolean;
begin
  case ATokenType of
    gttIdentifier,
    gttTrue, gttFalse, gttNull,
    gttConst, gttLet, gttClass, gttEnum, gttExtends, gttNew, gttThis,
    gttSuper, gttStatic, gttReturn, gttIf, gttElse, gttFor, gttWhile, gttDo,
    gttSwitch, gttCase, gttDefault, gttBreak, gttThrow, gttTry, gttCatch,
    gttFinally, gttImport, gttExport, gttFrom, gttAs, gttGet, gttSet,
    gttVar, gttWith, gttFunction, gttTypeof, gttInstanceof, gttIn,
    gttDelete:
      Exit(True);
  end;
  Result := False;
end;

function TGocciaParser.ConsumeModuleExportName(
  const AMessage: string): TGocciaToken;
begin
  if Check(gttString) or IsIdentifierNameToken(Peek.TokenType) then
    Exit(Advance);

  raise TGocciaSyntaxError.Create(AMessage, Peek.Line, Peek.Column,
    FFileName, FSourceLines);
end;

function TGocciaParser.ConsumeModuleExportName(
  const AMessage, ASuggestion: string): TGocciaToken;
var
  Error: TGocciaSyntaxError;
begin
  if Check(gttString) or IsIdentifierNameToken(Peek.TokenType) then
    Exit(Advance);

  Error := TGocciaSyntaxError.Create(AMessage, Peek.Line, Peek.Column,
    FFileName, FSourceLines);
  Error.Suggestion := ASuggestion;
  raise Error;
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

  if Match(gttQuestion) then
  begin
    Line := Previous.Line;
    Column := Previous.Column;
    Condition := Result;
    Consequent := Assignment;  // ECMAScript spec: AssignmentExpression, not Expression
    Consume(gttColon, 'Expected ":" in conditional expression',
      SSuggestTernaryColon);
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
var
  Op: TGocciaToken;
begin
  Result := Comparison;
  while Match([gttEqual, gttNotEqual, gttLooseEqual, gttLooseNotEqual]) do
  begin
    Op := Previous;
    if Op.TokenType in [gttLooseEqual, gttLooseNotEqual] then
    begin
      if Op.TokenType = gttLooseEqual then
        AddWarning('''=='' (loose equality) is not supported in GocciaScript',
          'Use ''==='' (strict equality) instead', Op.Line, Op.Column)
      else
        AddWarning('''!='' (loose inequality) is not supported in GocciaScript',
          'Use ''!=='' (strict inequality) instead', Op.Line, Op.Column);
      Comparison;
      Result := TGocciaLiteralExpression.Create(
        TGocciaUndefinedLiteralValue.UndefinedValue, Op.Line, Op.Column);
    end
    else
      Result := TGocciaBinaryExpression.Create(
        Result, Op.TokenType, Comparison, Op.Line, Op.Column);
  end;
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
        gttAssign, gttPlusAssign, gttMinusAssign, gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign, gttNullishCoalescingAssign,
        gttLogicalAndAssign, gttLogicalOrAssign,
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

  if Match(gttPower) then
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
  // ES2026 §16.1 top-level await / §15.8.2 AwaitExpression: await UnaryExpression
  if ((FInAsyncFunction > 0) or (FFunctionDepth = 0)) and Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_AWAIT) then
  begin
    Operator := Advance; // consume 'await'
    Right := Unary;
    Result := TGocciaAwaitExpression.Create(Right, Operator.Line, Operator.Column);
    Exit;
  end;

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
        Operator.Line, Operator.Column, FFileName, FSourceLines,
        SSuggestValidIncrementTarget);

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
  Token: TGocciaToken;
  Line, Column: Integer;
  IsOptionalChain: Boolean;
begin
  Result := Primary;

  while True do
  begin
    if Match(gttLeftParen) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Arguments := TObjectList<TGocciaExpression>.Create(True);

      if not Check(gttRightParen) then
      begin
        repeat
          if Match(gttSpread) then
            Arg := TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column)
          else
            Arg := Expression;
          Arguments.Add(Arg);
        until not Match(gttComma) or Check(gttRightParen);
      end;

      Consume(gttRightParen, 'Expected ")" after arguments',
        SSuggestCloseParenArguments);
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
        Token := Consume(gttIdentifier, 'Expected private field name after "#"',
          SSuggestPrivateFieldMustFollow);
        PropertyName := Token.Lexeme;
        RecordPrivateNameReference(PropertyName, Token.Line, Token.Column);
        Result := TGocciaPrivateMemberExpression.Create(Result, PropertyName, Line, Column);
      end
      else if Check(gttLeftBracket) and IsOptionalChain then
      begin
        // Optional chaining with computed property: obj?.[expr]
        Advance; // consume [
        Arg := Expression;
        Consume(gttRightBracket, 'Expected "]" after computed member expression',
          SSuggestCloseBracketComputedProperty);
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
              if Match(gttSpread) then
                Arg := TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column)
              else
                Arg := Expression;
              Arguments.Add(Arg);
            until not Match(gttComma) or Check(gttRightParen);
          end;
          Consume(gttRightParen, 'Expected ")" after arguments',
            SSuggestCloseParenArguments);
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
        else if Match([gttIf, gttElse, gttConst, gttLet, gttClass, gttEnum, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
                       gttReturn, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak,
                       gttThrow, gttTry, gttCatch, gttFinally, gttImport, gttExport, gttFrom, gttAs,
                       gttTrue, gttFalse, gttNull, gttTypeof, gttInstanceof, gttIn, gttDelete, gttVar, gttWith]) then
          PropertyName := Previous.Lexeme  // Reserved words are allowed as property names
        else
          raise TGocciaSyntaxError.Create('Expected property name after "."', Peek.Line, Peek.Column, FFileName, FSourceLines,
            SSuggestPropertyNameIdentifier);

        Result := TGocciaMemberExpression.Create(Result, PropertyName, False,
          Line, Column, IsOptionalChain);
      end;
    end
    else if Match(gttHash) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Token := Consume(gttIdentifier, 'Expected private field name after "#"',
        SSuggestPrivateFieldMustFollow);
      PropertyName := Token.Lexeme;
      RecordPrivateNameReference(PropertyName, Token.Line, Token.Column);
      Result := TGocciaPrivateMemberExpression.Create(Result, PropertyName, Line, Column);
    end
    else if Match(gttLeftBracket) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Arg := Expression;
      Consume(gttRightBracket, 'Expected "]" after computed member expression',
        SSuggestCloseBracketComputedProperty);
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
          Line, Column, FFileName, FSourceLines,
          SSuggestValidIncrementTarget);

      Result := TGocciaIncrementExpression.Create(Result, Previous.TokenType, False,
        Line, Column);
    end
    // ES2026 §13.3.11 Tagged Templates: tag`...`
    else if Check(gttTemplate) then
    begin
      Token := Advance;
      Line := Token.Line;
      Column := Token.Column;
      Result := ParseTaggedTemplate(Result, Token, Line, Column);
    end
    else
      Break;
  end;
end;

// Validate that all characters in a string are hexadecimal digits.
function IsValidHexString(const AValue: string): Boolean;
var
  I: Integer;
begin
  if Length(AValue) = 0 then
    Exit(False);
  for I := 1 to Length(AValue) do
    if not CharInSet(AValue[I], ['0'..'9', 'a'..'f', 'A'..'F']) then
      Exit(False);
  Result := True;
end;


// Split a string at #3 (TEMPLATE_EXPRESSION_BOUNDARY) markers embedded by the
// lexer during ScanTemplate.  Returns all parts: even-indexed parts are static
// template segments, odd-indexed parts are interpolation expression texts.
procedure SplitOnBoundaryMarker(const AStr: string; out AParts: TGocciaTemplateStrings);
const
  TEMPLATE_EXPRESSION_BOUNDARY = #3;
var
  I, Start, PartCount: Integer;
begin
  SetLength(AParts, 0);
  PartCount := 0;
  Start := 1;
  for I := 1 to Length(AStr) do
  begin
    if AStr[I] = TEMPLATE_EXPRESSION_BOUNDARY then
    begin
      SetLength(AParts, PartCount + 1);
      AParts[PartCount] := Copy(AStr, Start, I - Start);
      Inc(PartCount);
      Start := I + 1;
    end;
  end;
  // Final part (after last marker, or entire string if no markers)
  SetLength(AParts, PartCount + 1);
  AParts[PartCount] := Copy(AStr, Start, Length(AStr) - Start + 1);
end;

// Split a template token's cooked and raw strings at interpolation boundaries.
// The lexer embeds #3 boundary markers around expression texts during
// ScanTemplate with full lexical awareness (strings, comments, nested
// templates), so this function simply splits on those markers.
// Even-indexed parts are static segments; odd-indexed parts are expressions.
procedure SplitTemplateAtBoundaries(const ACookedFull, ARawFull: string;
  out ACookedSegments, ARawSegments: TGocciaTemplateStrings;
  out AExpressionTexts: TGocciaTemplateStrings);
var
  CookedParts, RawParts: TGocciaTemplateStrings;
  I, StaticCount, ExprCount: Integer;
begin
  SplitOnBoundaryMarker(ACookedFull, CookedParts);
  SplitOnBoundaryMarker(ARawFull, RawParts);

  // Even-indexed parts are static segments, odd-indexed are expressions
  StaticCount := 0;
  ExprCount := 0;
  for I := 0 to Length(RawParts) - 1 do
  begin
    if I mod 2 = 0 then
      Inc(StaticCount)
    else
      Inc(ExprCount);
  end;

  SetLength(ACookedSegments, StaticCount);
  SetLength(ARawSegments, StaticCount);
  SetLength(AExpressionTexts, ExprCount);

  StaticCount := 0;
  ExprCount := 0;
  for I := 0 to Length(RawParts) - 1 do
  begin
    if I mod 2 = 0 then
    begin
      ARawSegments[StaticCount] := RawParts[I];
      if I < Length(CookedParts) then
        ACookedSegments[StaticCount] := CookedParts[I]
      else
        ACookedSegments[StaticCount] := '';
      Inc(StaticCount);
    end
    else
    begin
      AExpressionTexts[ExprCount] := RawParts[I];
      Inc(ExprCount);
    end;
  end;
end;

// Extract cooked and raw full strings from a template token lexeme.
// AHasInvalidEscapes is True when the lexer used the #2 separator to signal
// that the template contains at least one malformed escape sequence.
procedure ExtractTemplateParts(const ALexeme: string;
  out ACookedFull, ARawFull: string; out AHasInvalidEscapes: Boolean);
const
  TEMPLATE_RAW_SEPARATOR = #1;
  TEMPLATE_INVALID_ESCAPE_SEPARATOR = #2;
var
  SepPos: Integer;
begin
  AHasInvalidEscapes := False;
  // Check for the invalid-escape separator first
  SepPos := Pos(TEMPLATE_INVALID_ESCAPE_SEPARATOR, ALexeme);
  if SepPos > 0 then
  begin
    AHasInvalidEscapes := True;
    ACookedFull := Copy(ALexeme, 1, SepPos - 1);
    ARawFull := Copy(ALexeme, SepPos + 1, MaxInt);
    Exit;
  end;
  SepPos := Pos(TEMPLATE_RAW_SEPARATOR, ALexeme);
  if SepPos > 0 then
  begin
    ACookedFull := Copy(ALexeme, 1, SepPos - 1);
    ARawFull := Copy(ALexeme, SepPos + 1, MaxInt);
  end
  else
  begin
    ACookedFull := ALexeme;
    ARawFull := ALexeme;
  end;
end;

// TC39 Template Literal Revision — split a raw template string at interpolation
// boundaries.  The lexer embeds #3 boundary markers with full lexical awareness,
// so this function simply splits on those markers.
procedure SplitRawAtBoundaries(const ARawFull: string;
  out ARawSegments, AExpressionTexts: TGocciaTemplateStrings);
var
  Parts: TGocciaTemplateStrings;
  I, StaticCount, ExprCount: Integer;
begin
  SplitOnBoundaryMarker(ARawFull, Parts);

  StaticCount := 0;
  ExprCount := 0;
  for I := 0 to Length(Parts) - 1 do
  begin
    if I mod 2 = 0 then
      Inc(StaticCount)
    else
      Inc(ExprCount);
  end;

  SetLength(ARawSegments, StaticCount);
  SetLength(AExpressionTexts, ExprCount);

  StaticCount := 0;
  ExprCount := 0;
  for I := 0 to Length(Parts) - 1 do
  begin
    if I mod 2 = 0 then
    begin
      ARawSegments[StaticCount] := Parts[I];
      Inc(StaticCount);
    end
    else
    begin
      AExpressionTexts[ExprCount] := Parts[I];
      Inc(ExprCount);
    end;
  end;
end;

// Convert a Unicode code point to its UTF-8 string representation.
// Returns empty string for code points above U+10FFFF.
function CodePointToUTF8(const ACodePoint: Cardinal): string;
begin
  if ACodePoint <= $7F then
    Result := Chr(ACodePoint)
  else if ACodePoint <= $7FF then
    Result := Chr($C0 or (ACodePoint shr 6)) + Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $FFFF then
    Result := Chr($E0 or (ACodePoint shr 12)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $10FFFF then
    Result := Chr($F0 or (ACodePoint shr 18)) +
              Chr($80 or ((ACodePoint shr 12) and $3F)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else
    Result := '';
end;

// TC39 Template Literal Revision — cook a Unicode escape from a raw segment.
// AStr is the raw text, APos is the current position (after 'u' has been
// consumed). On success, appends the resolved character(s) to ASB and advances
// APos. Returns False on malformed escape.
function CookUnicodeEscape(const AStr: string; var APos: Integer;
  var ASB: TStringBuffer): Boolean;
var
  CodePoint, LowSurrogate: Cardinal;
  CodePointValue: QWord;
  HexStr: string;
  HexStart, I, SavedPos: Integer;
begin
  if APos > Length(AStr) then
    Exit(False);

  if AStr[APos] = '{' then
  begin
    Inc(APos); // skip '{'
    HexStart := APos;
    while (APos <= Length(AStr)) and (AStr[APos] <> '}') do
      Inc(APos);
    if APos > Length(AStr) then
      Exit(False); // unterminated
    HexStr := Copy(AStr, HexStart, APos - HexStart);
    if (HexStr = '') or not IsValidHexString(HexStr) then
      Exit(False);
    Inc(APos); // skip '}'
    // Use TryStrToQWord to safely handle arbitrarily long hex payloads
    // without raising on overflow (e.g. \u{FFFFFFFF}).
    if not TryStrToQWord('$' + HexStr, CodePointValue) or
       (CodePointValue > $10FFFF) then
      Exit(False);
    CodePoint := Cardinal(CodePointValue);
  end
  else
  begin
    HexStart := APos;
    for I := 1 to 4 do
    begin
      if APos > Length(AStr) then
        Exit(False);
      Inc(APos);
    end;
    HexStr := Copy(AStr, HexStart, 4);
    if not IsValidHexString(HexStr) then
      Exit(False);
    CodePoint := StrToInt('$' + HexStr);
  end;

  // ES2026 §12.9.4: Combine UTF-16 surrogate pairs
  if (CodePoint >= $D800) and (CodePoint <= $DBFF) and
     (APos + 5 <= Length(AStr)) and
     (AStr[APos] = '\') and (AStr[APos + 1] = 'u') then
  begin
    SavedPos := APos;
    Inc(APos, 2); // skip \u
    HexStr := Copy(AStr, APos, 4);
    if (Length(HexStr) = 4) and IsValidHexString(HexStr) then
    begin
      LowSurrogate := StrToInt('$' + HexStr);
      if (LowSurrogate >= $DC00) and (LowSurrogate <= $DFFF) then
      begin
        CodePoint := $10000 + ((CodePoint - $D800) shl 10) + (LowSurrogate - $DC00);
        Inc(APos, 4);
      end
      else
        APos := SavedPos; // not a surrogate pair, restore
    end
    else
      APos := SavedPos;
  end;

  // Convert code point to UTF-8 via shared helper
  Result := CodePoint <= $10FFFF;
  if Result then
    ASB.Append(CodePointToUTF8(CodePoint));
end;

// TC39 Template Literal Revision — cook a hex escape from a raw segment.
// AStr is the raw text, APos is the current position (after 'x' has been
// consumed). On success, appends the resolved character to ASB and advances
// APos. Returns False on malformed escape.
function CookHexEscape(const AStr: string; var APos: Integer;
  var ASB: TStringBuffer): Boolean;
var
  HexStr: string;
  CodePoint: Cardinal;
begin
  if APos + 1 > Length(AStr) then
    Exit(False);
  HexStr := Copy(AStr, APos, 2);
  if not IsValidHexString(HexStr) then
    Exit(False);
  Inc(APos, 2);
  CodePoint := StrToInt('$' + HexStr);
  ASB.AppendChar(Chr(CodePoint));
  Result := True;
end;

// TC39 Template Literal Revision — cook a raw template segment into its
// cooked string value by resolving escape sequences.  Returns True if all
// escapes are valid (ACooked receives the cooked value).  Returns False if
// any escape is malformed (the segment's cooked value should be undefined).
function CookRawSegment(const ARawSegment: string; out ACooked: string): Boolean;
var
  SB: TStringBuffer;
  I: Integer;
begin
  SB := TStringBuffer.Create;
  Result := True;
  I := 1;
  while I <= Length(ARawSegment) do
  begin
    if ARawSegment[I] = '\' then
    begin
      Inc(I); // skip '\'
      if I > Length(ARawSegment) then
      begin
        Result := False;
        Break;
      end;
      case ARawSegment[I] of
        'n': begin SB.AppendChar(#10); Inc(I); end;
        'r': begin SB.AppendChar(#13); Inc(I); end;
        't': begin SB.AppendChar(#9); Inc(I); end;
        '\': begin SB.AppendChar('\'); Inc(I); end;
        '0': begin SB.AppendChar(#0); Inc(I); end;
        '`': begin SB.AppendChar('`'); Inc(I); end;
        '$': begin SB.AppendChar('$'); Inc(I); end;
        'u':
        begin
          Inc(I); // skip 'u'
          if not CookUnicodeEscape(ARawSegment, I, SB) then
          begin
            Result := False;
            Break;
          end;
        end;
        'x':
        begin
          Inc(I); // skip 'x'
          if not CookHexEscape(ARawSegment, I, SB) then
          begin
            Result := False;
            Break;
          end;
        end;
      else
        SB.AppendChar(ARawSegment[I]);
        Inc(I);
      end;
    end
    else
    begin
      SB.AppendChar(ARawSegment[I]);
      Inc(I);
    end;
  end;

  if Result then
    ACooked := SB.ToString
  else
    ACooked := '';
end;

// Parse a template interpolation expression text into an AST expression node.
// Note: Tokens are owned by the Lexer (FTokens with OwnsObjects=True) and freed
// when the Lexer is destroyed — they must not be freed separately.
function ParseInterpolationExpression(const AExprText, AFileName: string): TGocciaExpression;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
  SourceLines: TStringList;
begin
  Result := nil;
  if AExprText = '' then
    Exit;

  Lexer := TGocciaLexer.Create('(' + AExprText + ');', AFileName);
  try
    Tokens := Lexer.ScanTokens;
    SourceLines := TStringList.Create;
    SourceLines.Text := AExprText;
    try
      Parser := TGocciaParser.Create(Tokens, AFileName, SourceLines);
      try
        ProgramNode := Parser.ParseUnchecked;
        try
          if (ProgramNode.Body.Count > 0) and
             (ProgramNode.Body[0] is TGocciaExpressionStatement) then
            Result := TGocciaExpressionStatement(ProgramNode.Body[0]).Expression;
        finally
          ProgramNode.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      SourceLines.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

// ES2026 §13.3.11 Tagged Templates
// TC39 Template Literal Revision: when the template contains malformed escapes,
// split from the raw string and re-cook each segment individually, marking
// invalid segments so the evaluator emits undefined for their cooked values.
function TGocciaParser.ParseTaggedTemplate(const ATag: TGocciaExpression;
  const ATemplateToken: TGocciaToken;
  const ALine, AColumn: Integer): TGocciaTaggedTemplateExpression;
var
  CookedFull, RawFull: string;
  CookedStrings, RawStrings, ExprTexts: TGocciaTemplateStrings;
  CookedValid: TGocciaTemplateCookedValid;
  Expressions: TObjectList<TGocciaExpression>;
  ParsedExpr: TGocciaExpression;
  HasInvalidEscapes: Boolean;
  I: Integer;
begin
  ExtractTemplateParts(ATemplateToken.Lexeme, CookedFull, RawFull, HasInvalidEscapes);

  if HasInvalidEscapes then
  begin
    // TC39 Template Literal Revision: the cooked string from the lexer is
    // unreliable because invalid escapes were skipped.  Split using the raw
    // string only and re-cook each segment individually.
    SplitRawAtBoundaries(RawFull, RawStrings, ExprTexts);
    SetLength(CookedStrings, Length(RawStrings));
    SetLength(CookedValid, Length(RawStrings));
    for I := 0 to Length(RawStrings) - 1 do
      CookedValid[I] := CookRawSegment(RawStrings[I], CookedStrings[I]);
  end
  else
  begin
    // No invalid escapes — use the existing dual-tracking split
    SplitTemplateAtBoundaries(CookedFull, RawFull, CookedStrings, RawStrings, ExprTexts);
    SetLength(CookedValid, Length(CookedStrings));
    for I := 0 to Length(CookedStrings) - 1 do
      CookedValid[I] := True;
  end;

  // Parse each interpolation expression into an AST node
  Expressions := TObjectList<TGocciaExpression>.Create(True);
  for I := 0 to Length(ExprTexts) - 1 do
  begin
    ParsedExpr := ParseInterpolationExpression(ExprTexts[I], FFileName);
    if Assigned(ParsedExpr) then
      Expressions.Add(ParsedExpr);
  end;

  Result := TGocciaTaggedTemplateExpression.Create(ATag, CookedStrings, RawStrings,
    CookedValid, Expressions, ALine, AColumn);
end;

// ES2026 §13.2.8 Template Literals — pre-segment non-tagged templates at parse
// time using raw-string boundary detection, so that escaped dollar signs (\$)
// are never mistaken for interpolation boundaries at evaluation time.
// TC39 Template Literal Revision: untagged templates with malformed escape
// sequences raise SyntaxError at parse time (preserving existing behavior).
function TGocciaParser.ParseTemplateLiteral(const AToken: TGocciaToken): TGocciaExpression;
var
  CookedFull, RawFull: string;
  HasInvalidEscapes: Boolean;
  CookedSegments, RawSegments, ExprTexts: TGocciaTemplateStrings;
  Parts: TObjectList<TGocciaExpression>;
  ParsedExpr: TGocciaExpression;
  I: Integer;
begin
  ExtractTemplateParts(AToken.Lexeme, CookedFull, RawFull, HasInvalidEscapes);

  // TC39 Template Literal Revision: untagged templates must still reject
  // malformed escape sequences at parse time.  HasInvalidEscapes is set for
  // the whole flat token (including ${...} bodies), so we split into static
  // segments first and only reject if a static segment has an invalid escape.
  if HasInvalidEscapes then
  begin
    SplitRawAtBoundaries(RawFull, RawSegments, ExprTexts);
    SetLength(CookedSegments, Length(RawSegments));
    for I := 0 to Length(RawSegments) - 1 do
      if not CookRawSegment(RawSegments[I], CookedSegments[I]) then
        raise TGocciaSyntaxError.Create('Invalid escape sequence in template literal',
          AToken.Line, AToken.Column, FFileName, FSourceLines, '');
  end
  else
    SplitTemplateAtBoundaries(CookedFull, RawFull, CookedSegments, RawSegments, ExprTexts);

  // No real interpolations — return a simple template literal
  if Length(ExprTexts) = 0 then
  begin
    Result := TGocciaTemplateLiteralExpression.Create(CookedSegments[0], AToken.Line, AToken.Column);
    Exit;
  end;

  // Has real interpolations — build a pre-segmented template expression.
  // Parts alternate: string literal, expression, string literal, expression, ..., string literal
  Parts := TObjectList<TGocciaExpression>.Create(True);

  for I := 0 to Length(ExprTexts) - 1 do
  begin
    // Add the cooked static text segment before this interpolation
    Parts.Add(TGocciaLiteralExpression.Create(
      TGocciaStringLiteralValue.Create(CookedSegments[I]),
      AToken.Line, AToken.Column));

    // Parse and add the interpolation expression
    ParsedExpr := ParseInterpolationExpression(ExprTexts[I], FFileName);
    if Assigned(ParsedExpr) then
      Parts.Add(ParsedExpr);
  end;

  // Add the final cooked static text segment
  Parts.Add(TGocciaLiteralExpression.Create(
    TGocciaStringLiteralValue.Create(CookedSegments[Length(CookedSegments) - 1]),
    AToken.Line, AToken.Column));

  Result := TGocciaTemplateWithInterpolationExpression.Create(Parts, AToken.Line, AToken.Column);
end;

function TGocciaParser.Primary: TGocciaExpression;
const
  REGEX_SEPARATOR = #0;
var
  Token: TGocciaToken;
  Expr: TGocciaExpression;
  Name: string;
  Args: TObjectList<TGocciaExpression>;
  Parameters: TGocciaParameterArray;
  ArrowFn: TGocciaArrowFunctionExpression;
  ArrowBody: TGocciaASTNode;
  SeparatorPos: Integer;
begin
  if Match(gttTrue) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaBooleanLiteralValue.TrueValue, Token.Line, Token.Column);
  end
  else if Match(gttFalse) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaBooleanLiteralValue.FalseValue, Token.Line, Token.Column);
  end
  else if Match(gttNull) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaNullLiteralValue.NullValue, Token.Line, Token.Column);
  end
  else if Match(gttNumber) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaNumberLiteralValue.Create(ConvertNumberLiteral(Token.Lexeme)), Token.Line, Token.Column);
  end
  else if Match(gttString) then
  begin
    Token := Previous;
    Result := TGocciaLiteralExpression.Create(
      TGocciaStringLiteralValue.Create(Token.Lexeme), Token.Line, Token.Column);
  end
  else if Match(gttTemplate) then
  begin
    Token := Previous;
    Result := ParseTemplateLiteral(Token);
  end
  else if Match(gttRegex) then
  begin
    Token := Previous;
    SeparatorPos := Pos(REGEX_SEPARATOR, Token.Lexeme);
    if SeparatorPos > 0 then
      Result := TGocciaRegexLiteralExpression.Create(
        Copy(Token.Lexeme, 1, SeparatorPos - 1),
        Copy(Token.Lexeme, SeparatorPos + 1, MaxInt),
        Token.Line, Token.Column)
    else
      Result := TGocciaRegexLiteralExpression.Create(Token.Lexeme, '', Token.Line, Token.Column);
  end
  else if Match(gttThis) then
  begin
    Token := Previous;
    Result := TGocciaThisExpression.Create(Token.Line, Token.Column);
  end
  else if Match(gttSuper) then
  begin
    Token := Previous;
    Result := TGocciaSuperExpression.Create(Token.Line, Token.Column);
  end
  else if Match(gttImport) then
  begin
    Token := Previous;
    if Check(gttLeftParen) then
    begin
      // ES2026 §13.3.10 ImportCall — import(specifier)
      Advance; // consume '('
      Expr := Expression;
      Consume(gttRightParen, 'Expected ")" after import() specifier',
        SSuggestDynamicImportSyntax);
      Result := TGocciaImportCallExpression.Create(Expr, Token.Line, Token.Column);
    end
    else
    begin
      // ES2026 §13.3.12 MetaProperty — import.meta
      Consume(gttDot, 'Expected "." or "(" after "import" in expression context',
        SSuggestDynamicImportSyntax);
      if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_META) then
      begin
        Advance;
        Result := TGocciaImportMetaExpression.Create(Token.Line, Token.Column);
      end
      else
        raise TGocciaSyntaxError.Create(
          'The only valid meta property for import is "import.meta"',
          Peek.Line, Peek.Column, FFileName, FSourceLines,
          SSuggestImportMetaSyntax);
    end;
  end
  else if Match(gttClass) then
  begin
    Token := Previous;
    Result := ClassExpression;
  end
  else if Match(gttNew) then
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
        raise TGocciaSyntaxError.Create('Expected property name after "."', Peek.Line, Peek.Column, FFileName, FSourceLines,
          SSuggestPropertyNameIdentifier);
    end;
    // Parse constructor arguments if present
    if Match(gttLeftParen) then
    begin
      Args := TObjectList<TGocciaExpression>.Create(True);
      if not Check(gttRightParen) then
      begin
        repeat
          if Match(gttSpread) then
            Args.Add(TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column))
          else
            Args.Add(Expression);
        until not Match(gttComma) or Check(gttRightParen);
      end;
      Consume(gttRightParen, 'Expected ")" after constructor arguments',
        SSuggestCloseParenConstructorArguments);
      Result := TGocciaNewExpression.Create(Expr, Args, Token.Line, Token.Column);
    end
    else
    begin
      Args := TObjectList<TGocciaExpression>.Create(True);
      Result := TGocciaNewExpression.Create(Expr, Args, Token.Line, Token.Column);
    end;
  end
  else if Match(gttIdentifier) then
  begin
    Token := Previous;
    Name := Token.Lexeme;

    // async arrow function: async (params) => body
    if (Name = KEYWORD_ASYNC) and Check(gttLeftParen) then
    begin
      Advance; // consume '('
      if IsArrowFunction() then
      begin
        Inc(FInAsyncFunction);
        try
          Expr := ArrowFunction;
        finally
          Dec(FInAsyncFunction);
        end;
        TGocciaArrowFunctionExpression(Expr).IsAsync := True;
        Result := Expr;
      end
      else
      begin
        FCurrent := FCurrent - 1; // back up past '('
        Result := TGocciaIdentifierExpression.Create(Name, Token.Line, Token.Column);
      end;
    end
    // async single-param arrow: async x => body
    else if (Name = KEYWORD_ASYNC) and Check(gttIdentifier) and CheckNext(gttArrow) then
    begin
      Token := Advance; // consume param identifier
      Name := Token.Lexeme;
      Consume(gttArrow, 'Expected "=>" in async arrow function',
        SSuggestArrowFunctionSyntax);

      Inc(FInAsyncFunction);
      Inc(FFunctionDepth);
      try
        if Match(gttLeftBrace) then
          ArrowBody := BlockStatement
        else
          ArrowBody := Expression;
      finally
        Dec(FFunctionDepth);
        Dec(FInAsyncFunction);
      end;

      SetLength(Parameters, 1);
      Parameters[0].Name := Name;
      Parameters[0].DefaultValue := nil;
      Parameters[0].Pattern := nil;
      Parameters[0].IsPattern := False;
      Parameters[0].IsRest := False;
      Parameters[0].IsOptional := False;
      Parameters[0].TypeAnnotation := '';

      ArrowFn := TGocciaArrowFunctionExpression.Create(Parameters, ArrowBody, Token.Line, Token.Column);
      ArrowFn.IsAsync := True;
      Result := ArrowFn;
    end
    else
      Result := TGocciaIdentifierExpression.Create(Name, Token.Line, Token.Column);
  end
  else if Match(gttHash) then
  begin
    Token := Previous;
    Token := Consume(gttIdentifier, 'Expected private field name after "#"',
      SSuggestPrivateFieldMustFollow);
    Name := Token.Lexeme;
    RecordPrivateNameReference(Name, Token.Line, Token.Column);
    // Private field access is equivalent to this.#fieldName
    Result := TGocciaPrivateMemberExpression.Create(
      TGocciaThisExpression.Create(Token.Line, Token.Column),
      Name, Token.Line, Token.Column);
  end
  else if Match(gttLeftParen) then
  begin
    // Check for arrow function by looking for pattern: () => or (id) => or (id, id) =>
    if IsArrowFunction() then
      Result := ArrowFunction
    else
    begin
      Expr := Expression;
      Consume(gttRightParen, 'Expected ")" after expression',
        SSuggestCloseParenExpression);
      Result := Expr;
    end;
  end
  else if Match(gttFunction) then
  begin
    Token := Previous;
    AddWarning('''function'' expressions are not supported in GocciaScript',
      'Use arrow functions instead: const name = (...) => { ... }',
      Token.Line, Token.Column);
    SkipUnsupportedFunctionSignature;
    Result := TGocciaLiteralExpression.Create(
      TGocciaUndefinedLiteralValue.UndefinedValue, Token.Line, Token.Column);
  end
  else if Match(gttLeftBracket) then
    Result := ArrayLiteral
  else if Match(gttLeftBrace) then
    Result := ObjectLiteral
  else
    raise TGocciaSyntaxError.Create('Expected expression',
      Peek.Line, Peek.Column, FFileName, FSourceLines,
      SSuggestExpressionExpected);
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
    else if Match(gttSpread) then
    begin
      // Spread expression: ...array
      Elements.Add(TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column));
    end
    else
    begin
      // Regular expression
      Elements.Add(Expression);
    end;

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBracket, 'Expected "]" after array elements',
    SSuggestCloseBracketArray);
  Result := TGocciaArrayExpression.Create(Elements, Line, Column);
end;

function TGocciaParser.ObjectLiteral: TGocciaExpression;
var
  Properties: TGocciaExpressionMap;
  PropertyOrder: TStringList;
  ComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
  ComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
  PropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
  Getters: TGocciaGetterExpressionMap;
  Setters: TGocciaSetterExpressionMap;
  Key: string;
  KeyExpression: TGocciaExpression;
  Value: TGocciaExpression;
  Line, Column: Integer;
  NumericValue: Double;
  IsComputed: Boolean;
  IsGetter, IsSetter: Boolean;
  IsAsync: Boolean;
  ComputedCount, SourceOrderCount: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Properties := TGocciaExpressionMap.Create;
  PropertyOrder := TStringList.Create;
  PropertyOrder.Duplicates := dupIgnore;
  ComputedProperties := THashMap<TGocciaExpression, TGocciaExpression>.Create;
  Getters := TGocciaGetterExpressionMap.Create;
  Setters := TGocciaSetterExpressionMap.Create;

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
    IsAsync := False;

    // Check for async method syntax
    if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_ASYNC) and not CheckNext(gttColon) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) and not CheckNext(gttLeftParen) then
    begin
      Advance;
      IsAsync := True;
    end;

    // Check for getter/setter syntax (contextual keywords)
    // Only treat as getter/setter if followed by identifier (not colon, parenthesis, comma, or right brace)
    if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_GET) and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) then
    begin
      Advance;
      IsGetter := True;
    end
    else if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_SET) and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) then
    begin
      Advance;
      IsSetter := True;
    end;

        // Handle spread syntax: ...obj
    if not IsGetter and not IsSetter and Match(gttSpread) then
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
      if not Match(gttComma) then
        Break;
      Continue;
    end
    else if Match(gttLeftBracket) then
    begin
      // Computed property name: [expr]: value
      IsComputed := True;
      KeyExpression := Expression;
      Consume(gttRightBracket, 'Expected "]" after computed property name',
        SSuggestCloseBracketComputedPropertyName);
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
    else if Match([gttIf, gttElse, gttConst, gttLet, gttClass, gttEnum, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
                   gttReturn, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak,
                   gttThrow, gttTry, gttCatch, gttFinally, gttImport, gttExport, gttFrom, gttAs,
                   gttTrue, gttFalse, gttNull, gttTypeof, gttInstanceof, gttIn, gttDelete, gttVar, gttWith]) then
      Key := Previous.Lexeme  // Reserved words are allowed as property names
    else
      raise TGocciaSyntaxError.Create('Expected property name', Peek.Line, Peek.Column, FFileName, FSourceLines,
        SSuggestPropertyKeysFormat);

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
    begin
      if IsAsync then Inc(FInAsyncFunction);
      try
        Value := ParseObjectMethodBody(Peek.Line, Peek.Column);
      finally
        if IsAsync then Dec(FInAsyncFunction);
      end;
      if IsAsync then
        TGocciaMethodExpression(Value).IsAsync := True;
    end
    else
    begin
      // Check for shorthand property syntax: { name } means { name: name }
      if Check(gttColon) then
      begin
        // Regular property: key: value
        Consume(gttColon, 'Expected ":" after property key',
          SSuggestAddColonPropertyValue);
        Value := Expression;
      end
            else
      begin
        // Shorthand property: { name } means { name: name }
        // Also handle default values in shorthand: { name = defaultValue }
        if IsComputed then
          raise TGocciaSyntaxError.Create('Computed property names require a value', Peek.Line, Peek.Column, FFileName, FSourceLines,
            SSuggestComputedPropertyNeedsValue);

        Value := TGocciaIdentifierExpression.Create(Key, Previous.Line, Previous.Column);

        // Check for default value in shorthand property
        if Match(gttAssign) then
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

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after object properties',
    SSuggestCloseObject);
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

      if Match(gttSpread) then
      begin
        ParamName := Consume(gttIdentifier, 'Expected parameter name after "..."',
          SSuggestProvideRestParameterName).Lexeme;
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

        if Match(gttAssign) then
          Result[ParamCount].DefaultValue := Assignment
        else
          Result[ParamCount].DefaultValue := nil;
      end
      else
      begin
        ParamName := Consume(gttIdentifier, 'Expected parameter name',
          SSuggestProvideParameterName).Lexeme;
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

        if Match(gttAssign) then
          Result[ParamCount].DefaultValue := Assignment
        else
          Result[ParamCount].DefaultValue := nil;
      end;

      Inc(ParamCount);
    until not Match(gttComma) or Check(gttRightParen);
  end;

  SetLength(Result, ParamCount);
end;

function TGocciaParser.ParseGetterExpression: TGocciaGetterExpression;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Consume(gttLeftParen, 'Expected "(" after getter name',
    SSuggestOpenParenGetterParameterList);
  Consume(gttRightParen, 'Getters cannot have parameters',
    SSuggestGetterNoParameters);
  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttLeftBrace]);
  end;
  Consume(gttLeftBrace, 'Expected "{" before getter body',
    SSuggestOpenBraceGetterBody);
  Inc(FFunctionDepth);
  try
    Result := TGocciaGetterExpression.Create(BlockStatement, Line, Column);
  finally
    Dec(FFunctionDepth);
  end;
end;

function TGocciaParser.ParseSetterExpression: TGocciaSetterExpression;
var
  ParamName: string;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;
  Consume(gttLeftParen, 'Expected "(" after setter name',
    SSuggestOpenParenSetterParameterList);
  if Check(gttRightParen) then
    raise TGocciaSyntaxError.Create('Setter must have exactly one parameter',
      Peek.Line, Peek.Column, FFileName, FSourceLines,
      SSuggestSetterOneParameter);
  ParamName := Consume(gttIdentifier, 'Expected parameter name in setter',
    SSuggestProvideSetterParameterName).Lexeme;
  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttRightParen]);
  end;
  Consume(gttRightParen, 'Expected ")" after setter parameter',
    SSuggestCloseParenSetterParameter);
  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttLeftBrace]);
  end;
  Consume(gttLeftBrace, 'Expected "{" before setter body',
    SSuggestOpenBraceSetterBody);
  Inc(FFunctionDepth);
  try
    Result := TGocciaSetterExpression.Create(ParamName, BlockStatement, Line, Column);
  finally
    Dec(FFunctionDepth);
  end;
end;

function TGocciaParser.ParseObjectMethodBody(const ALine, AColumn: Integer): TGocciaExpression;
var
  Parameters: TGocciaParameterArray;
  Body: TGocciaASTNode;
  Statements: TObjectList<TGocciaASTNode>;
  Stmt: TGocciaStatement;
begin
  Consume(gttLeftParen, 'Expected "(" after method name',
    SSuggestOpenParenMethodParameterList);
  Parameters := ParseParameterList;
  Consume(gttRightParen, 'Expected ")" after parameters',
    SSuggestCloseParenParameterList);

  if Check(gttColon) then
  begin
    Advance;
    CollectTypeAnnotation([gttLeftBrace]);
  end;

  Consume(gttLeftBrace, 'Expected "{" before method body',
    SSuggestOpenBraceMethodBody);

  Inc(FFunctionDepth);
  try
    Statements := TObjectList<TGocciaASTNode>.Create(True);
    try
      while not Check(gttRightBrace) and not IsAtEnd do
      begin
        Stmt := Statement;
        Statements.Add(Stmt);
      end;

      Consume(gttRightBrace, 'Expected "}" after method body',
        SSuggestCloseBlock);
      Body := TGocciaBlockStatement.Create(Statements, ALine, AColumn);
      Result := TGocciaMethodExpression.Create(Parameters, Body, ALine, AColumn);
    except
      Statements.Free;
      raise;
    end;
  finally
    Dec(FFunctionDepth);
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
  Consume(gttRightParen, 'Expected ")" after parameters',
    SSuggestCloseParenParameterList);

  FnReturnType := '';
  if Check(gttColon) then
  begin
    Advance;
    FnReturnType := CollectTypeAnnotation([gttArrow]);
  end;

  Consume(gttArrow, 'Expected "=>" in arrow function',
    SSuggestArrowFunctionSyntax);

  Inc(FFunctionDepth);
  try
    if Match(gttLeftBrace) then
      Body := BlockStatement
    else
      Body := Expression;
  finally
    Dec(FFunctionDepth);
  end;

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
  if Match([gttAssign, gttPlusAssign, gttMinusAssign, gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign, gttNullishCoalescingAssign,
             gttLogicalAndAssign, gttLogicalOrAssign,
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
      raise TGocciaSyntaxError.Create('Invalid assignment target', Left.Line, Left.Column, FFileName, FSourceLines,
        SSuggestValidAssignmentTarget);
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
  if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_TYPE) and IsTypeDeclaration then
  begin
    Line := Peek.Line;
    Column := Peek.Column;
    Advance;
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
  end
  else if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_INTERFACE) and IsTypeDeclaration then
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
  else if Match(gttVar) then
    Result := VarStatement
  else if Check(gttAt) then
  begin
    Result := DecoratedClassDeclaration(ParseDecorators);
  end
  else if Match(gttClass) then
    Result := ClassDeclaration
  else if Match(gttEnum) then
    Result := EnumDeclaration
  else if Check(gttImport) and CheckNext(gttDot) then
    Result := ExpressionStatement
  else if Check(gttImport) and CheckNext(gttLeftParen) then
    Result := ExpressionStatement
  else if Match(gttImport) then
    Result := ImportDeclaration
  else if Match(gttExport) then
    Result := ExportDeclaration
  else if Match(gttIf) then
    Result := IfStatement
  else if Match(gttFor) then
    Result := ForStatement
  else if Match(gttWhile) then
    Result := WhileStatement
  else if Match(gttDo) then
    Result := DoWhileStatement
  else if Match(gttWith) then
    Result := WithStatement
  else if Match(gttFunction) then
    Result := FunctionStatement
  else if Match(gttSwitch) then
    Result := SwitchStatement
  else if Match(gttBreak) then
    Result := BreakStatement
  else if Match(gttReturn) then
    Result := ReturnStatement
  else if Match(gttThrow) then
    Result := ThrowStatement
  else if Match(gttTry) then
    Result := TryStatement
  else if Match(gttLeftBrace) then
    Result := BlockStatement
  // TC39 Explicit Resource Management: using x = expr;
  // Disambiguate: 'using' followed by identifier is a declaration,
  // 'using(' or 'using.x' is an expression (function call / member access).
  else if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_USING) and
          CheckNext(gttIdentifier) then
    Result := UsingStatement
  // TC39 Explicit Resource Management: await using x = expr;
  // Detect 'await using identifier' regardless of context, then validate.
  else if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_AWAIT) and
          (FCurrent + 2 < FTokens.Count) and
          (FTokens[FCurrent + 1].TokenType = gttIdentifier) and
          (FTokens[FCurrent + 1].Lexeme = KEYWORD_USING) and
          (FTokens[FCurrent + 2].TokenType = gttIdentifier) then
  begin
    // Reject await using in synchronous function bodies
    if (FInAsyncFunction = 0) and (FFunctionDepth > 0) then
      raise TGocciaSyntaxError.Create(
        '''await using'' is only valid in async functions or at the top level',
        Peek.Line, Peek.Column, FFileName, FSourceLines,
        'Wrap in an async function or use ''using'' for synchronous disposal');
    Result := UsingStatement;
  end
  else if Check(gttIdentifier) and CheckNext(gttColon) then
  begin
    Line := Peek.Line;
    Column := Peek.Column;
    AddWarning('Labeled statements are not supported in GocciaScript', '',
      Line, Column);
    Advance;
    Advance;
    Result := Statement;
  end
  else if Match(gttSemicolon) then
  begin
    Result := TGocciaEmptyStatement.Create(Previous.Line, Previous.Column);
  end
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
    Consume(gttAssign, 'Destructuring declarations must have an initializer',
      SSuggestDestructuringRequiresInitializer);
    Initializer := Expression;
    ConsumeSemicolonOrASI('Expected ";" after destructuring declaration',
      SSuggestAddSemicolon);
    DestructuringDecl := TGocciaDestructuringDeclaration.Create(Pattern, Initializer, IsConst, Line, Column);
    DestructuringDecl.TypeAnnotation := DestructuringType;
    Result := DestructuringDecl;
  end
  else
  begin
    repeat
      SetLength(Variables, VariableCount + 1);

      Name := Consume(gttIdentifier, 'Expected variable name',
        SSuggestProvideVariableName).Lexeme;
      Variables[VariableCount].Name := Name;

      if Check(gttColon) then
      begin
        Advance;
        Variables[VariableCount].TypeAnnotation := CollectTypeAnnotation([gttAssign, gttSemicolon, gttComma]);
      end;

      if Match(gttAssign) then
        Variables[VariableCount].Initializer := Expression
      else if IsConst then
        raise TGocciaSyntaxError.Create('const declarations must have an initializer',
          Line, Column, FFileName, FSourceLines,
          SSuggestAddConstInitializer)
      else
        Variables[VariableCount].Initializer := TGocciaLiteralExpression.Create(
          TGocciaUndefinedLiteralValue.UndefinedValue, Line, Column);

      Inc(VariableCount);
    until not Match(gttComma);

    ConsumeSemicolonOrASI('Expected ";" after variable declaration',
      SSuggestAddSemicolon);
    Result := TGocciaVariableDeclaration.Create(Variables, IsConst, Line, Column);
  end;
end;

// TC39 Explicit Resource Management: using / await using declarations
function TGocciaParser.UsingStatement: TGocciaStatement;
var
  IsAwait: Boolean;
  Name: string;
  Initializer: TGocciaExpression;
  Line, Column: Integer;
  Variables: TArray<TGocciaVariableInfo>;
  VariableCount: Integer;
begin
  Line := Peek.Line;
  Column := Peek.Column;
  IsAwait := False;
  VariableCount := 0;

  // Check for 'await using' — consume 'await' first
  if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_AWAIT) then
  begin
    IsAwait := True;
    Advance; // consume 'await'
  end;

  // Consume 'using' (contextual keyword, parsed as identifier)
  Advance; // consume 'using'

  // Parse one or more variable bindings: using x = expr, y = expr;
  repeat
    SetLength(Variables, VariableCount + 1);

    Name := Consume(gttIdentifier, 'Expected variable name after "using"',
      'Provide an identifier for the disposable resource').Lexeme;
    Variables[VariableCount].Name := Name;

    // Optional type annotation: using x: Type = expr
    if Check(gttColon) then
    begin
      Advance;
      Variables[VariableCount].TypeAnnotation :=
        CollectTypeAnnotation([gttAssign, gttSemicolon, gttComma]);
    end;

    // using declarations must have an initializer
    Consume(gttAssign, '"using" declarations must have an initializer',
      'Add " = <expression>" after the variable name');
    Initializer := Expression;
    Variables[VariableCount].Initializer := Initializer;

    Inc(VariableCount);
  until not Match(gttComma);

  ConsumeSemicolonOrASI('Expected ";" after using declaration',
    SSuggestAddSemicolon);
  Result := TGocciaUsingDeclaration.Create(Variables, IsAwait, Line, Column);
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

  Consume(gttRightBrace, 'Expected "}" after block',
    SSuggestCloseBlock);
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

  Consume(gttLeftParen, 'Expected "(" after "if"',
    SSuggestOpenParenCondition);
  Condition := Expression;
  Consume(gttRightParen, 'Expected ")" after if condition',
    SSuggestCloseParenCondition);

  Consequent := Statement;

  if Match(gttElse) then
    Alternate := Statement
  else
    Alternate := nil;

  Result := TGocciaIfStatement.Create(Condition, Consequent, Alternate,
    Line, Column);
end;

procedure TGocciaParser.SkipBlock;
var
  Depth: Integer;
begin
  Consume(gttLeftBrace, 'Expected "{"',
    SSuggestOpenBraceBlock);
  Depth := 1;
  while not IsAtEnd and (Depth > 0) do
  begin
    if Check(gttLeftBrace) then Inc(Depth)
    else if Check(gttRightBrace) then Dec(Depth);
    if Depth > 0 then Advance;
  end;
  Consume(gttRightBrace, 'Expected "}"',
    SSuggestCloseBlock);
end;

procedure TGocciaParser.SkipBalancedParens;
var
  Depth: Integer;
begin
  Consume(gttLeftParen, 'Expected "("',
    SSuggestOpenParenExpression);
  Depth := 1;
  while not IsAtEnd and (Depth > 0) do
  begin
    if Check(gttLeftParen) then Inc(Depth)
    else if Check(gttRightParen) then Dec(Depth);
    if Depth > 0 then Advance;
  end;
  Consume(gttRightParen, 'Expected ")"',
    SSuggestCloseParenExpression);
end;

procedure TGocciaParser.SkipStatementOrBlock;
begin
  if Check(gttLeftBrace) then
    SkipBlock
  else
    SkipUntilSemicolon;
end;

procedure TGocciaParser.SkipUnsupportedFunctionSignature;
  function TokenAfterBalancedBraces: TGocciaToken;
  var
    Depth: Integer;
    ScanIndex: Integer;
  begin
    ScanIndex := FCurrent;
    Depth := 0;

    while ScanIndex < FTokens.Count do
    begin
      case FTokens[ScanIndex].TokenType of
        gttLeftBrace:
          Inc(Depth);
        gttRightBrace:
          begin
            Dec(Depth);
            if Depth = 0 then
            begin
              Inc(ScanIndex);
              Break;
            end;
          end;
      end;
      Inc(ScanIndex);
    end;

    if ScanIndex < FTokens.Count then
      Result := FTokens[ScanIndex]
    else
      Result := FTokens[FTokens.Count - 1];
  end;

  function IsReturnTypeContinuationToken(const AToken: TGocciaToken): Boolean;
  begin
    case AToken.TokenType of
      gttBitwiseAnd,
      gttBitwiseOr,
      gttQuestion,
      gttLeftBracket,
      gttLess,
      gttGreater,
      gttRightShift,
      gttUnsignedRightShift:
        Exit(True);
    end;

    if (AToken.TokenType = gttIdentifier) and
      ((AToken.Lexeme = KEYWORD_AS) or
       (AToken.Lexeme = KEYWORD_EXTENDS)) then
      Exit(True);

    Result := False;
  end;

var
  NestingDepth: Integer;
  FollowingToken: TGocciaToken;
begin
  if Check(gttStar) then
    Advance;
  if Check(gttIdentifier) then
    Advance;

  CollectGenericParameters;

  if Check(gttLeftParen) then
    SkipBalancedParens;

  if Check(gttColon) then
  begin
    Advance;

    NestingDepth := 0;
    while not IsAtEnd do
    begin
      if (NestingDepth = 0) and Check(gttSemicolon) then
        Break;

      if Check(gttLeftBrace) then
      begin
        FollowingToken := TokenAfterBalancedBraces;
        if (NestingDepth > 0) or
          (FollowingToken.TokenType = gttLeftBrace) or
          IsReturnTypeContinuationToken(FollowingToken) then
        begin
          SkipBlock;
          Continue;
        end;
        Break;
      end;

      case Peek.TokenType of
        gttLeftParen,
        gttLeftBracket,
        gttLess:
          Inc(NestingDepth);
        gttRightParen,
        gttRightBracket,
        gttGreater:
          if NestingDepth > 0 then
            Dec(NestingDepth);
        gttRightShift:
          if NestingDepth > 0 then
          begin
            Dec(NestingDepth);
            if NestingDepth > 0 then
              Dec(NestingDepth);
          end;
        gttUnsignedRightShift:
          while NestingDepth > 0 do
            Dec(NestingDepth);
      end;

      Advance;
    end;
  end;

  if Check(gttLeftBrace) then
    SkipBlock
  else if Check(gttSemicolon) then
    Advance;
end;

function TGocciaParser.IsTypeOnlySpecifierModifier: Boolean;
begin
  Result := Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_TYPE) and
    (FCurrent + 1 < FTokens.Count) and
    (FTokens[FCurrent + 1].TokenType in [gttIdentifier, gttString]);
end;

function TGocciaParser.VarStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  AddWarning('''var'' declarations are not supported in GocciaScript',
    'Use ''let'' or ''const'' instead',
    Line, Column);

  SkipUntilSemicolon;

  Result := TGocciaEmptyStatement.Create(Line, Column);
end;

// ES2026 §14.7.5 ForIn/OfStatement
function TGocciaParser.ForStatement: TGocciaStatement;
var
  Line, Column: Integer;
  IsAwait: Boolean;
  IsConst: Boolean;
  BindingName: string;
  BindingPattern: TGocciaDestructuringPattern;
  IterableExpr: TGocciaExpression;
  BodyStmt: TGocciaStatement;
  SavedCurrent: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // Try to parse for...of / for-await-of
  SavedCurrent := FCurrent;
  IsAwait := False;
  BindingPattern := nil;

  // for-await-of: 'await' appears between 'for' and '(' (async or top-level)
  if ((FInAsyncFunction > 0) or (FFunctionDepth = 0)) and Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_AWAIT) then
  begin
    IsAwait := True;
    Advance; // consume 'await'
  end;

  if Check(gttLeftParen) then
  begin
    Advance; // consume '('

    // Check for const/let binding
    if Check(gttConst) or Check(gttLet) then
    begin
      IsConst := Check(gttConst);
      Advance; // consume const/let

      // Check if this is a for...of: need binding + 'of'
      if Check(gttLeftBracket) or Check(gttLeftBrace) then
      begin
        BindingPattern := ParsePattern;
        BindingName := '';
      end
      else if Check(gttIdentifier) then
      begin
        BindingName := Advance.Lexeme;
      end
      else
      begin
        FCurrent := SavedCurrent;
        AddWarning('''for'' loops are not supported in GocciaScript',
          'Use array methods like .forEach(), .map(), .filter(), or .reduce() instead',
          Line, Column);
        SkipBalancedParens;
        SkipStatementOrBlock;
        Result := TGocciaEmptyStatement.Create(Line, Column);
        Exit;
      end;

      // Check for 'of' keyword
      if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_OF) then
      begin
        Advance; // consume 'of'

        IterableExpr := Expression;
        Consume(gttRightParen, 'Expected ")" after for...of expression',
          SSuggestCloseParenForOf);

        if Check(gttLeftBrace) then
        begin
          Advance;
          BodyStmt := BlockStatement;
        end
        else
          BodyStmt := Statement;

        if IsAwait then
          Result := TGocciaForAwaitOfStatement.Create(IsConst, BindingName, BindingPattern, IterableExpr, BodyStmt, Line, Column)
        else
          Result := TGocciaForOfStatement.Create(IsConst, BindingName, BindingPattern, IterableExpr, BodyStmt, Line, Column);
        Exit;
      end;
    end;

    // Not a for...of — fall back to traditional for loop (unsupported, skip with warning)
    FCurrent := SavedCurrent;
  end;

  AddWarning('''for'' loops are not supported in GocciaScript',
    'Use array methods like .forEach(), .map(), .filter(), or .reduce() instead',
    Line, Column);

  SkipBalancedParens;

  SkipStatementOrBlock;

  Result := TGocciaEmptyStatement.Create(Line, Column);
end;

function TGocciaParser.WhileStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  AddWarning('''while'' loops are not supported in GocciaScript',
    'Use array methods like .forEach(), .map(), .filter(), or .reduce() instead',
    Line, Column);

  SkipBalancedParens;

  SkipStatementOrBlock;

  Result := TGocciaEmptyStatement.Create(Line, Column);
end;

function TGocciaParser.DoWhileStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  AddWarning('''do...while'' loops are not supported in GocciaScript',
    'Use array methods like .forEach(), .map(), .filter(), or .reduce() instead',
    Line, Column);

  SkipStatementOrBlock;

  Consume(gttWhile, 'Expected "while" after do body',
    SSuggestAddWhileAfterDo);
  SkipBalancedParens;
  ConsumeSemicolonOrASI('Expected ";" after do-while statement',
    SSuggestAddSemicolon);

  Result := TGocciaEmptyStatement.Create(Line, Column);
end;

function TGocciaParser.WithStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  AddWarning('The ''with'' statement is not supported in GocciaScript', '',
    Line, Column);

  SkipBalancedParens;

  SkipStatementOrBlock;

  Result := TGocciaEmptyStatement.Create(Line, Column);
end;

function TGocciaParser.FunctionStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  AddWarning('''function'' declarations are not supported in GocciaScript',
    'Use arrow functions instead: const name = (...) => { ... }',
    Line, Column);

  SkipUnsupportedFunctionSignature;

  Result := TGocciaEmptyStatement.Create(Line, Column);
end;

function TGocciaParser.ReturnStatement: TGocciaStatement;
var
  Value: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // ES2026 §12.10.1 restricted production: return [no LineTerminator here] Expression
  if CheckSemicolonOrASI then
  begin
    Value := nil;
    if Check(gttSemicolon) then
      Advance;
  end
  else
  begin
    Value := Expression;
    ConsumeSemicolonOrASI('Expected ";" after return value',
      SSuggestAddSemicolon);
  end;
  Result := TGocciaReturnStatement.Create(Value, Line, Column);
end;

function TGocciaParser.ThrowStatement: TGocciaStatement;
var
  Value: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  // ES2026 §12.10.1 restricted production: throw [no LineTerminator here] Expression
  if FAutomaticSemicolonInsertion and (Previous.Line < Peek.Line) then
    raise TGocciaSyntaxError.Create('Illegal newline after throw',
      Line, Column, FFileName, FSourceLines,
      'Move the throw expression to the same line as the throw keyword');

  Value := Expression;
  ConsumeSemicolonOrASI('Expected ";" after throw value',
    SSuggestAddSemicolon);
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

  Consume(gttLeftBrace, 'Expected "{" after "try"',
    SSuggestOpenBraceTryBlock);
  Block := BlockStatement;
  CatchParam := '';
  CatchBlock := nil;
  FinallyBlock := nil;
  CatchType := '';

  if Match(gttCatch) then
  begin
    if Check(gttLeftParen) then
    begin
      Consume(gttLeftParen, 'Expected "(" after "catch"',
        SSuggestOpenParenCatchClause);
      CatchParam := Consume(gttIdentifier, 'Expected catch parameter',
        SSuggestProvideCatchParameter).Lexeme;
      if Check(gttColon) then
      begin
        Advance;
        CatchType := CollectTypeAnnotation([gttRightParen]);
      end;
      Consume(gttRightParen, 'Expected ")" after catch parameter',
        SSuggestCloseParenCatchClause);
    end
    else
      CatchParam := '';

    Consume(gttLeftBrace, 'Expected "{" after catch clause',
      SSuggestOpenBraceCatchBlock);
    CatchBlock := BlockStatement;
  end;

  if Match(gttFinally) then
  begin
    Consume(gttLeftBrace, 'Expected "{" after "finally"',
      SSuggestOpenBraceFinallyBlock);
    FinallyBlock := BlockStatement;
  end;

  if (CatchBlock = nil) and (FinallyBlock = nil) then
    raise TGocciaSyntaxError.Create('Missing catch or finally after try',
      Line, Column, FFileName, FSourceLines,
      SSuggestMissingCatchOrFinally);

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
  Statements: TObjectList<TGocciaASTNode>;
  Stmt: TGocciaStatement;
  MethodGenericParams, MethodReturnType: string;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  MethodGenericParams := CollectGenericParameters;

  Consume(gttLeftParen, 'Expected "(" after method name',
    SSuggestOpenParenMethodParameterList);
  Parameters := ParseParameterList;
  Consume(gttRightParen, 'Expected ")" after parameters',
    SSuggestCloseParenParameterList);

  MethodReturnType := '';
  if Check(gttColon) then
  begin
    Advance;
    MethodReturnType := CollectTypeAnnotation([gttLeftBrace]);
  end;

  Consume(gttLeftBrace, 'Expected "{" before method body',
    SSuggestOpenBraceMethodBody);

  Inc(FFunctionDepth);
  try
    Statements := TObjectList<TGocciaASTNode>.Create(True);
    try
      while not Check(gttRightBrace) and not IsAtEnd do
      begin
        Stmt := Statement;
        if Stmt is TGocciaExpressionStatement then
        begin
          if not Check(gttSemicolon) then
            Statements.Add(Stmt)
          else
          begin
            Advance;
            Statements.Add(Stmt);
          end;
        end
        else
          Statements.Add(Stmt);
      end;

      Consume(gttRightBrace, 'Expected "}" after method body',
        SSuggestCloseBlock);
      Body := TGocciaBlockStatement.Create(Statements, Line, Column);
      Result := TGocciaClassMethod.Create(Name, Parameters, Body, AIsStatic, Line, Column);
      Result.GenericParams := MethodGenericParams;
      Result.ReturnType := MethodReturnType;
    except
      Statements.Free;
      raise;
    end;
  finally
    Dec(FFunctionDepth);
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

  Name := Consume(gttIdentifier, 'Expected class name',
    SSuggestProvideClassName).Lexeme;
  ClassDef := ParseClassBody(Name);
  Result := TGocciaClassDeclaration.Create(ClassDef, Line, Column);
end;

// TC39 proposal-decorators
function TGocciaParser.ParseDecoratorExpression: TGocciaExpression;
var
  Arguments: TObjectList<TGocciaExpression>;
  Arg: TGocciaExpression;
  PropertyName: string;
  Line, Column: Integer;
begin
  if Match(gttLeftParen) then
  begin
    Result := Expression;
    Consume(gttRightParen, 'Expected ")" after decorator expression',
      SSuggestCloseParenDecoratorExpression);
    Exit;
  end;

  Result := Primary;

  while True do
  begin
    if Match(gttLeftParen) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      Arguments := TObjectList<TGocciaExpression>.Create(True);
      try
        if not Check(gttRightParen) then
        begin
          repeat
            if Match(gttSpread) then
              Arg := TGocciaSpreadExpression.Create(Expression, Previous.Line, Previous.Column)
            else
              Arg := Expression;
            Arguments.Add(Arg);
          until not Match(gttComma) or Check(gttRightParen);
        end;
        Consume(gttRightParen, 'Expected ")" after arguments',
          SSuggestCloseParenArguments);
        Result := TGocciaCallExpression.Create(Result, Arguments, Line, Column);
      except
        Arguments.Free;
        raise;
      end;
    end
    else if Match(gttDot) then
    begin
      Line := Previous.Line;
      Column := Previous.Column;
      PropertyName := Consume(gttIdentifier, 'Expected property name after "."',
        SSuggestPropertyNameIdentifier).Lexeme;
      Result := TGocciaMemberExpression.Create(Result, PropertyName, False, Line, Column);
    end
    else
      Break;
  end;
end;

function TGocciaParser.ParseDecorators: TGocciaDecoratorList;
begin
  SetLength(Result, 0);
  while Check(gttAt) do
  begin
    Advance;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := ParseDecoratorExpression;
  end;
end;

function TGocciaParser.DecoratedClassDeclaration(const ADecorators: TGocciaDecoratorList): TGocciaStatement;
var
  Name: string;
  ClassDef: TGocciaClassDefinition;
  Line, Column: Integer;
begin
  Consume(gttClass, 'Decorators can only be applied to class declarations',
    SSuggestDecoratorsOnlyClasses);
  Line := Previous.Line;
  Column := Previous.Column;

  Name := Consume(gttIdentifier, 'Expected class name',
    SSuggestProvideClassName).Lexeme;
  ClassDef := ParseClassBody(Name);
  ClassDef.FDecorators := ADecorators;
  Result := TGocciaClassDeclaration.Create(ClassDef, Line, Column);
end;

// TC39 proposal-enum
function TGocciaParser.EnumDeclaration: TGocciaStatement;
var
  Name, MemberName: string;
  Members: TArray<TGocciaEnumMember>;
  MemberCount: Integer;
  Initializer: TGocciaExpression;
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  Name := Consume(gttIdentifier, 'Expected enum name',
    SSuggestEnumName).Lexeme;
  Consume(gttLeftBrace, 'Expected "{" after enum name',
    SSuggestOpenBraceEnumBody);

  MemberCount := 0;
  SetLength(Members, 0);

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    if Check(gttString) then
      MemberName := Advance.Lexeme
    else
      MemberName := Consume(gttIdentifier, 'Expected enum member name',
        SSuggestEnumMemberName).Lexeme;

    Consume(gttAssign, 'Expected "=" after enum member name (enum members require explicit initializers)',
      SSuggestEnumExplicitValues);
    Initializer := Expression;

    Inc(MemberCount);
    SetLength(Members, MemberCount);
    Members[MemberCount - 1].Name := MemberName;
    Members[MemberCount - 1].Initializer := Initializer;

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after enum members',
    SSuggestCloseEnum);

  Result := TGocciaEnumDeclaration.Create(Name, Members, Line, Column);
end;

function TGocciaParser.ImportDeclaration: TGocciaStatement;
var
  Imports: TStringStringMap;
  ImportedName, LocalName, ModulePath, NamespaceName: string;
  ImportedNameToken: TGocciaToken;
  Line, Column: Integer;
  IsTypeOnlyBinding: Boolean;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_TYPE) then
  begin
    Advance;
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  if Check(gttStar) then
  begin
    Advance;
    Consume(gttAs, 'Expected "as" after "*" in namespace import',
      SSuggestNamespaceImportAs);
    NamespaceName := Consume(gttIdentifier,
      'Expected local name after "as"',
      SSuggestProvideLocalName).Lexeme;
    Consume(gttFrom, 'Expected "from" after namespace import',
      SSuggestAddFromAfterImport);
    ModulePath := Consume(gttString, 'Expected module path',
      SSuggestProvideModulePath).Lexeme;
    ConsumeSemicolonOrASI('Expected ";" after import declaration',
      SSuggestAddSemicolon);
    Result := TGocciaImportDeclaration.Create(TStringStringMap.Create,
      ModulePath, Line, Column, NamespaceName);
    Exit;
  end;

  if Check(gttIdentifier) then
  begin
    AddWarning('Default imports are not supported in GocciaScript',
      'Use named imports instead: import { name } from ''module''',
      Line, Column);
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  if Check(gttString) then
  begin
    AddWarning('Side-effect imports (import ''module'') are not supported in GocciaScript',
      'Use named imports instead: import { name } from ''module''',
      Line, Column);
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  Consume(gttLeftBrace, 'Expected "{" after "import"',
    SSuggestOpenBraceImportList);

  Imports := TStringStringMap.Create;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    IsTypeOnlyBinding := IsTypeOnlySpecifierModifier;
    if IsTypeOnlyBinding then
      Advance;

    ImportedNameToken := ConsumeModuleExportName('Expected import name',
      SSuggestProvideImportName);
    ImportedName := ImportedNameToken.Lexeme;

    if Match(gttAs) then
      LocalName := Consume(gttIdentifier, 'Expected local name after "as"',
        SSuggestProvideLocalName).Lexeme
    else if ImportedNameToken.TokenType = gttString then
      raise TGocciaSyntaxError.Create(
        'String-literal import names require "as" and a local identifier',
        ImportedNameToken.Line, ImportedNameToken.Column, FFileName,
        FSourceLines,
        SSuggestStringImportAs)
    else
      LocalName := ImportedName;

    if not IsTypeOnlyBinding then
      Imports.Add(LocalName, ImportedName);

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after imports',
    SSuggestCloseImportList);
  Consume(gttFrom, 'Expected "from" after imports',
    SSuggestAddFromAfterImportList);
  ModulePath := Consume(gttString, 'Expected module path',
    SSuggestProvideModulePath).Lexeme;
  ConsumeSemicolonOrASI('Expected ";" after import declaration',
    SSuggestAddSemicolon);

  Result := TGocciaImportDeclaration.Create(Imports, ModulePath, Line, Column);
end;

function TGocciaParser.ExportDeclaration: TGocciaStatement;
var
  ExportsTable: TStringStringMap;
  LocalName, ExportedName, ModulePath: string;
  Line, Column: Integer;
  InnerDecl: TGocciaStatement;
  VarDecl: TGocciaVariableDeclaration;
  LocalNames: array of string;
  ExportedNames: array of string;
  LocalNameTokenTypes: array of TGocciaTokenType;
  LocalNameLines: array of Integer;
  LocalNameColumns: array of Integer;
  SpecifierCount: Integer;
  NameToken: TGocciaToken;
  IsReExport: Boolean;
  IsTypeOnlyBinding: Boolean;
  I: Integer;

  function HasFromClauseAfterNamedExports: Boolean;
  var
    ScanIndex: Integer;
  begin
    ScanIndex := FCurrent;
    while (ScanIndex < FTokens.Count) and
      (FTokens[ScanIndex].TokenType <> gttRightBrace) do
      Inc(ScanIndex);
    Result := (ScanIndex + 1 < FTokens.Count) and
      (FTokens[ScanIndex + 1].TokenType = gttFrom);
  end;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_TYPE) then
  begin
    Advance;
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_INTERFACE) and IsTypeDeclaration then
  begin
    Advance;
    Advance;
    SkipInterfaceDeclaration;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  if Check(gttDefault) then
  begin
    AddWarning('Default exports are not supported in GocciaScript',
      'Use named exports instead: export const name = value; or export { name }',
      Line, Column);
    Advance;
    if Check(gttClass) or Check(gttEnum) then
    begin
      Advance;
      if Check(gttIdentifier) then Advance;
      SkipBlock;
    end
    else if Check(gttFunction) then
    begin
      Advance;
      SkipUnsupportedFunctionSignature;
    end
    else
      SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  if Match(gttEnum) then
  begin
    InnerDecl := EnumDeclaration;
    Result := TGocciaExportEnumDeclaration.Create(TGocciaEnumDeclaration(InnerDecl), Line, Column);
    Exit;
  end;

  if Match([gttConst, gttLet]) then
  begin
    InnerDecl := DeclarationStatement;
    if not (InnerDecl is TGocciaVariableDeclaration) then
      raise TGocciaSyntaxError.Create('Destructuring exports are not supported; use export { name } instead',
        Line, Column, FFileName, FSourceLines,
        SSuggestDestructuringExportDeclareFirst);
    VarDecl := TGocciaVariableDeclaration(InnerDecl);
    Result := TGocciaExportVariableDeclaration.Create(VarDecl, Line, Column);
    Exit;
  end;

  if Check(gttStar) then
  begin
    AddWarning('Wildcard re-exports (export * from ...) are not supported in GocciaScript',
      'Use named re-exports instead: export { name } from ''module''',
      Line, Column);
    SkipUntilSemicolon;
    Result := TGocciaEmptyStatement.Create(Line, Column);
    Exit;
  end;

  Consume(gttLeftBrace, 'Expected "{", "const", or "let" after "export"',
    SSuggestOpenBraceExportOrDeclaration);

  IsReExport := HasFromClauseAfterNamedExports;
  SpecifierCount := 0;

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    IsTypeOnlyBinding := IsTypeOnlySpecifierModifier;
    if IsTypeOnlyBinding then
      Advance;

    NameToken := ConsumeModuleExportName('Expected export name',
      SSuggestProvideExportName);
    LocalName := NameToken.Lexeme;

    if Match(gttAs) then
      ExportedName := ConsumeModuleExportName(
        'Expected exported name after "as"',
        SSuggestProvideExportedName).Lexeme
    else
      ExportedName := LocalName;

    if not IsTypeOnlyBinding then
    begin
      SetLength(LocalNames, SpecifierCount + 1);
      SetLength(ExportedNames, SpecifierCount + 1);
      SetLength(LocalNameTokenTypes, SpecifierCount + 1);
      SetLength(LocalNameLines, SpecifierCount + 1);
      SetLength(LocalNameColumns, SpecifierCount + 1);
      LocalNames[SpecifierCount] := LocalName;
      ExportedNames[SpecifierCount] := ExportedName;
      LocalNameTokenTypes[SpecifierCount] := NameToken.TokenType;
      LocalNameLines[SpecifierCount] := NameToken.Line;
      LocalNameColumns[SpecifierCount] := NameToken.Column;
      Inc(SpecifierCount);
    end;

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after exports',
    SSuggestCloseExportList);

  if not IsReExport then
    for I := 0 to SpecifierCount - 1 do
      if LocalNameTokenTypes[I] = gttString then
        raise TGocciaSyntaxError.Create(
          'String-literal local exports require an identifier before "as"',
          LocalNameLines[I], LocalNameColumns[I], FFileName, FSourceLines,
          SSuggestStringExportAs);

  ExportsTable := TStringStringMap.Create;
  for I := 0 to SpecifierCount - 1 do
    ExportsTable.Add(ExportedNames[I], LocalNames[I]);

  if Match(gttFrom) then
  begin
    ModulePath := Consume(gttString, 'Expected module path after "from"',
      SSuggestProvideModulePath).Lexeme;
    ConsumeSemicolonOrASI('Expected ";" after re-export declaration',
      SSuggestAddSemicolon);
    Result := TGocciaReExportDeclaration.Create(ExportsTable, ModulePath, Line, Column);
  end
  else
  begin
    ConsumeSemicolonOrASI('Expected ";" after export declaration',
      SSuggestAddSemicolon);
    Result := TGocciaExportDeclaration.Create(ExportsTable, Line, Column);
  end;
end;

function TGocciaParser.ParseClassBody(const AClassName: string): TGocciaClassDefinition;
var
  SuperClass: string;
  Methods: TGocciaClassMethodMap;
  Getters: TGocciaGetterExpressionMap;
  Setters: TGocciaSetterExpressionMap;
  StaticProperties: TGocciaExpressionMap;
  InstanceProperties: TGocciaExpressionMap;
  PrivateInstanceProperties: TGocciaExpressionMap;
  PrivateStaticProperties: TGocciaExpressionMap;
  PrivateMethods: TGocciaClassMethodMap;
  MemberName: string;
  Method: TGocciaClassMethod;
  Getter: TGocciaGetterExpression;
  Setter: TGocciaSetterExpression;
  PropertyValue: TGocciaExpression;
  IsStatic: Boolean;
  IsPrivate: Boolean;
  IsGetter: Boolean;
  IsSetter: Boolean;
  IsComputed: Boolean;
  ComputedKeyExpression: TGocciaExpression;
  StaticGetters: TGocciaGetterExpressionMap;
  StaticSetters: TGocciaSetterExpressionMap;
  ComputedStaticGetters: array of TGocciaComputedGetterEntry;
  ComputedStaticSetters: array of TGocciaComputedSetterEntry;
  ComputedInstanceGetters: array of TGocciaComputedGetterEntry;
  ComputedInstanceSetters: array of TGocciaComputedSetterEntry;
  ClassGenericParams, ClassImplementsClause, FieldType: string;
  InstancePropertyTypes: TStringStringMap;
  MemberDecorators: TGocciaDecoratorList;
  Elements: array of TGocciaClassElement;
  FieldOrder: array of TGocciaFieldOrderEntry;
  IsAccessor: Boolean;
  IsAsync: Boolean;
  TypePair: TStringStringMap.TKeyValuePair;
begin
  SetLength(Elements, 0);
  SetLength(FieldOrder, 0);
  ClassGenericParams := CollectGenericParameters;

  if Match(gttExtends) then
  begin
    SuperClass := Consume(gttIdentifier, 'Expected superclass name',
      SSuggestProvideSuperclassName).Lexeme;
    CollectGenericParameters;
  end
  else
    SuperClass := '';

  ClassImplementsClause := '';
  if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_IMPLEMENTS) then
  begin
    Advance;
    ClassImplementsClause := CollectTypeAnnotation([gttLeftBrace]);
  end;

  Consume(gttLeftBrace, 'Expected "{" before class body',
    SSuggestOpenBraceClassBody);
  PushPrivateClassContext;

  Methods := TGocciaClassMethodMap.Create;
  Getters := TGocciaGetterExpressionMap.Create;
  Setters := TGocciaSetterExpressionMap.Create;
  StaticProperties := TGocciaExpressionMap.Create;
  InstanceProperties := TGocciaExpressionMap.Create;
  PrivateInstanceProperties := TGocciaExpressionMap.Create;
  PrivateStaticProperties := TGocciaExpressionMap.Create;
  PrivateMethods := TGocciaClassMethodMap.Create;
  InstancePropertyTypes := TStringStringMap.Create;
  StaticGetters := TGocciaGetterExpressionMap.Create;
  StaticSetters := TGocciaSetterExpressionMap.Create;
  SetLength(ComputedStaticGetters, 0);
  SetLength(ComputedStaticSetters, 0);
  SetLength(ComputedInstanceGetters, 0);
  SetLength(ComputedInstanceSetters, 0);

  try
    while not Check(gttRightBrace) and not IsAtEnd do
    begin
      MemberDecorators := ParseDecorators;
      IsAccessor := False;

      IsStatic := Match(gttStatic);

      // ES2022 §15.7.14 ClassStaticBlockDefinition: static { ... }
      if IsStatic and Check(gttLeftBrace) then
      begin
        if Length(MemberDecorators) > 0 then
          raise TGocciaSyntaxError.Create(
            'Decorators cannot be applied to static blocks',
            Peek.Line, Peek.Column, FFileName, FSourceLines,
            SSuggestStaticBlockNoDecorators);
        Consume(gttLeftBrace, 'Expected "{" after "static"',
          SSuggestOpenBraceClassBody);
        SetLength(Elements, Length(Elements) + 1);
        Elements[High(Elements)].Kind := cekStaticBlock;
        Elements[High(Elements)].Name := '';
        Elements[High(Elements)].IsStatic := True;
        Elements[High(Elements)].IsPrivate := False;
        Elements[High(Elements)].IsComputed := False;
        Elements[High(Elements)].ComputedKeyExpression := nil;
        Elements[High(Elements)].Decorators := nil;
        Elements[High(Elements)].StaticBlockBody := BlockStatement;
        Continue;
      end;

      while Check(gttIdentifier) and
        ((Peek.Lexeme = KEYWORD_PUBLIC) or (Peek.Lexeme = KEYWORD_PROTECTED) or (Peek.Lexeme = KEYWORD_PRIVATE) or
         (Peek.Lexeme = KEYWORD_READONLY) or (Peek.Lexeme = KEYWORD_OVERRIDE) or (Peek.Lexeme = KEYWORD_ABSTRACT)) do
        Advance;

      if not IsStatic and Check(gttStatic) then
        IsStatic := Match(gttStatic);

      if Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_ACCESSOR) and not CheckNext(gttLeftParen) and not CheckNext(gttColon) and not CheckNext(gttSemicolon) and not CheckNext(gttAssign) then
      begin
        Advance;
        IsAccessor := True;
      end;

      IsAsync := False;
      if not IsAccessor and Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_ASYNC) and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) and not CheckNext(gttSemicolon) and not CheckNext(gttAssign) and not CheckNext(gttQuestion) then
      begin
        Advance;
        IsAsync := True;
      end;

      IsPrivate := Match(gttHash);
      IsGetter := False;
      IsSetter := False;
      IsComputed := False;
      ComputedKeyExpression := nil;

      if not IsAccessor and Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_GET) and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) and not CheckNext(gttSemicolon) and not CheckNext(gttAssign) and not CheckNext(gttQuestion) then
      begin
        Advance;
        IsGetter := True;

        if Check(gttLeftBracket) then
        begin
          Advance;
          ComputedKeyExpression := Expression;
          Consume(gttRightBracket, 'Expected "]" after computed property name',
            SSuggestCloseBracketComputedPropertyName);
          IsComputed := True;
          MemberName := '';
        end
        else if Check(gttHash) then
        begin
          Advance;
          IsPrivate := True;
          MemberName := Consume(gttIdentifier, 'Expected property name after "#"',
            SSuggestPrivateFieldMustFollow).Lexeme;
        end
        else
          MemberName := Consume(gttIdentifier, 'Expected property name after "get"',
            SSuggestProvideGetterPropertyName).Lexeme;
      end
      else if not IsAccessor and Check(gttIdentifier) and (Peek.Lexeme = KEYWORD_SET) and not CheckNext(gttColon) and not CheckNext(gttLeftParen) and not CheckNext(gttComma) and not CheckNext(gttRightBrace) and not CheckNext(gttSemicolon) and not CheckNext(gttAssign) and not CheckNext(gttQuestion) then
      begin
        Advance;
        IsSetter := True;

        if Check(gttLeftBracket) then
        begin
          Advance;
          ComputedKeyExpression := Expression;
          Consume(gttRightBracket, 'Expected "]" after computed property name',
            SSuggestCloseBracketComputedPropertyName);
          IsComputed := True;
          MemberName := '';
        end
        else if Check(gttHash) then
        begin
          Advance;
          IsPrivate := True;
          MemberName := Consume(gttIdentifier, 'Expected property name after "#"',
            SSuggestPrivateFieldMustFollow).Lexeme;
        end
        else
          MemberName := Consume(gttIdentifier, 'Expected property name after "set"',
            SSuggestProvideSetterPropertyName).Lexeme;
      end
      else if Check(gttLeftBracket) then
      begin
        Advance;
        ComputedKeyExpression := Expression;
        Consume(gttRightBracket, 'Expected "]" after computed property name',
          SSuggestCloseBracketComputedPropertyName);
        IsComputed := True;
        MemberName := '';
      end
      else if Check(gttNumber) then
      begin
        MemberName := FloatToStr(ConvertNumberLiteral(Advance.Lexeme));
      end
      else if Check(gttString) then
      begin
        MemberName := Advance.Lexeme;
      end
      else
      begin
        MemberName := Consume(gttIdentifier, 'Expected method or property name',
          SSuggestClassMemberSyntax).Lexeme;
      end;

      if IsPrivate and not IsComputed and (MemberName <> '') then
        DeclarePrivateName(MemberName);

      FieldType := '';
      if Check(gttQuestion) then
        Advance;
      if Check(gttColon) and not IsGetter and not IsSetter then
      begin
        Advance;
        FieldType := CollectTypeAnnotation([gttAssign, gttSemicolon]);
      end;

      if IsAccessor then
      begin
        if Check(gttAssign) then
        begin
          Advance;
          PropertyValue := Expression;
        end
        else
          PropertyValue := nil;
        ConsumeSemicolonOrASI('Expected ";" after accessor declaration',
          SSuggestAddSemicolon);

        SetLength(Elements, Length(Elements) + 1);
        Elements[High(Elements)].Kind := cekAccessor;
        Elements[High(Elements)].Name := MemberName;
        Elements[High(Elements)].IsStatic := IsStatic;
        Elements[High(Elements)].IsPrivate := IsPrivate;
        Elements[High(Elements)].IsComputed := False;
        Elements[High(Elements)].ComputedKeyExpression := nil;
        Elements[High(Elements)].Decorators := MemberDecorators;
        Elements[High(Elements)].FieldInitializer := PropertyValue;
        Elements[High(Elements)].TypeAnnotation := FieldType;
      end
      else if Check(gttAssign) then
      begin
        Consume(gttAssign, 'Expected "=" in property',
          SSuggestAddPropertyInitializer);
        PropertyValue := Expression;
        ConsumeSemicolonOrASI('Expected ";" after property',
          SSuggestAddSemicolon);

        if (Length(MemberDecorators) > 0) or IsStatic then
        begin
          SetLength(Elements, Length(Elements) + 1);
          Elements[High(Elements)].Kind := cekField;
          Elements[High(Elements)].Name := MemberName;
          Elements[High(Elements)].IsStatic := IsStatic;
          Elements[High(Elements)].IsPrivate := IsPrivate;
          Elements[High(Elements)].IsComputed := False;
          Elements[High(Elements)].ComputedKeyExpression := nil;
          Elements[High(Elements)].Decorators := MemberDecorators;
          Elements[High(Elements)].FieldInitializer := PropertyValue;
          Elements[High(Elements)].TypeAnnotation := FieldType;
        end;

        if IsPrivate and IsStatic then
          PrivateStaticProperties.Add(MemberName, PropertyValue)
        else if IsPrivate then
        begin
          PrivateInstanceProperties.Add(MemberName, PropertyValue);
          SetLength(FieldOrder, Length(FieldOrder) + 1);
          FieldOrder[High(FieldOrder)].Name := MemberName;
          FieldOrder[High(FieldOrder)].IsPrivate := True;
        end
        else if IsStatic then
          StaticProperties.Add(MemberName, PropertyValue)
        else
        begin
          InstanceProperties.Add(MemberName, PropertyValue);
          SetLength(FieldOrder, Length(FieldOrder) + 1);
          FieldOrder[High(FieldOrder)].Name := MemberName;
          FieldOrder[High(FieldOrder)].IsPrivate := False;
          if FieldType <> '' then
            InstancePropertyTypes.Add(MemberName, FieldType);
        end;
      end
      else if CheckSemicolonOrASI then
      begin
        if Check(gttSemicolon) then
          Advance;
        PropertyValue := TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, Peek.Line, Peek.Column);

        if (Length(MemberDecorators) > 0) or IsStatic then
        begin
          SetLength(Elements, Length(Elements) + 1);
          Elements[High(Elements)].Kind := cekField;
          Elements[High(Elements)].Name := MemberName;
          Elements[High(Elements)].IsStatic := IsStatic;
          Elements[High(Elements)].IsPrivate := IsPrivate;
          Elements[High(Elements)].IsComputed := False;
          Elements[High(Elements)].ComputedKeyExpression := nil;
          Elements[High(Elements)].Decorators := MemberDecorators;
          Elements[High(Elements)].FieldInitializer := PropertyValue;
          Elements[High(Elements)].TypeAnnotation := FieldType;
        end;

        if IsPrivate and IsStatic then
          PrivateStaticProperties.Add(MemberName, PropertyValue)
        else if IsPrivate then
        begin
          PrivateInstanceProperties.Add(MemberName, PropertyValue);
          SetLength(FieldOrder, Length(FieldOrder) + 1);
          FieldOrder[High(FieldOrder)].Name := MemberName;
          FieldOrder[High(FieldOrder)].IsPrivate := True;
        end
        else if IsStatic then
          StaticProperties.Add(MemberName, PropertyValue)
        else
        begin
          InstanceProperties.Add(MemberName, PropertyValue);
          SetLength(FieldOrder, Length(FieldOrder) + 1);
          FieldOrder[High(FieldOrder)].Name := MemberName;
          FieldOrder[High(FieldOrder)].IsPrivate := False;
          if FieldType <> '' then
            InstancePropertyTypes.Add(MemberName, FieldType);
        end;
      end
      else if IsGetter then
      begin
        Getter := ParseGetterExpression;

        if (Length(MemberDecorators) > 0) or IsComputed then
        begin
          SetLength(Elements, Length(Elements) + 1);
          Elements[High(Elements)].Kind := cekGetter;
          Elements[High(Elements)].Name := MemberName;
          Elements[High(Elements)].IsStatic := IsStatic;
          Elements[High(Elements)].IsPrivate := IsPrivate;
          Elements[High(Elements)].IsComputed := IsComputed;
          Elements[High(Elements)].ComputedKeyExpression := ComputedKeyExpression;
          Elements[High(Elements)].Decorators := MemberDecorators;
          Elements[High(Elements)].GetterNode := Getter;
        end;

        if IsStatic then
        begin
          if IsComputed then
          begin
            SetLength(ComputedStaticGetters, Length(ComputedStaticGetters) + 1);
            ComputedStaticGetters[High(ComputedStaticGetters)].KeyExpression := ComputedKeyExpression;
            ComputedStaticGetters[High(ComputedStaticGetters)].GetterExpression := Getter;
          end
          else if IsPrivate then
            StaticGetters.Add('#' + MemberName, Getter)
          else
            StaticGetters.Add(MemberName, Getter);
        end
        else if IsComputed then
        begin
          SetLength(ComputedInstanceGetters, Length(ComputedInstanceGetters) + 1);
          ComputedInstanceGetters[High(ComputedInstanceGetters)].KeyExpression := ComputedKeyExpression;
          ComputedInstanceGetters[High(ComputedInstanceGetters)].GetterExpression := Getter;
        end
        else if IsPrivate then
          Getters.Add('#' + MemberName, Getter)
        else
          Getters.Add(MemberName, Getter);
      end
      else if IsSetter then
      begin
        Setter := ParseSetterExpression;

        if (Length(MemberDecorators) > 0) or IsComputed then
        begin
          SetLength(Elements, Length(Elements) + 1);
          Elements[High(Elements)].Kind := cekSetter;
          Elements[High(Elements)].Name := MemberName;
          Elements[High(Elements)].IsStatic := IsStatic;
          Elements[High(Elements)].IsPrivate := IsPrivate;
          Elements[High(Elements)].IsComputed := IsComputed;
          Elements[High(Elements)].ComputedKeyExpression := ComputedKeyExpression;
          Elements[High(Elements)].Decorators := MemberDecorators;
          Elements[High(Elements)].SetterNode := Setter;
        end;

        if IsStatic then
        begin
          if IsComputed then
          begin
            SetLength(ComputedStaticSetters, Length(ComputedStaticSetters) + 1);
            ComputedStaticSetters[High(ComputedStaticSetters)].KeyExpression := ComputedKeyExpression;
            ComputedStaticSetters[High(ComputedStaticSetters)].SetterExpression := Setter;
          end
          else if IsPrivate then
            StaticSetters.Add('#' + MemberName, Setter)
          else
            StaticSetters.Add(MemberName, Setter);
        end
        else if IsComputed then
        begin
          SetLength(ComputedInstanceSetters, Length(ComputedInstanceSetters) + 1);
          ComputedInstanceSetters[High(ComputedInstanceSetters)].KeyExpression := ComputedKeyExpression;
          ComputedInstanceSetters[High(ComputedInstanceSetters)].SetterExpression := Setter;
        end
        else if IsPrivate then
          Setters.Add('#' + MemberName, Setter)
        else
          Setters.Add(MemberName, Setter);
      end
      else if Check(gttLeftParen) or Check(gttLess) then
      begin
        if IsAsync then Inc(FInAsyncFunction);
        try
          Method := ClassMethod(IsStatic);
        finally
          if IsAsync then Dec(FInAsyncFunction);
        end;
        Method.Name := MemberName;
        Method.IsAsync := IsAsync;

        if (Length(MemberDecorators) > 0) or IsComputed then
        begin
          SetLength(Elements, Length(Elements) + 1);
          Elements[High(Elements)].Kind := cekMethod;
          Elements[High(Elements)].Name := MemberName;
          Elements[High(Elements)].IsStatic := IsStatic;
          Elements[High(Elements)].IsPrivate := IsPrivate;
          Elements[High(Elements)].IsComputed := IsComputed;
          Elements[High(Elements)].ComputedKeyExpression := ComputedKeyExpression;
          Elements[High(Elements)].Decorators := MemberDecorators;
          Elements[High(Elements)].MethodNode := Method;
          Elements[High(Elements)].IsAsync := IsAsync;
        end;

        if IsComputed then
        begin
          // Computed methods are stored in Elements; evaluated at runtime
        end
        else if IsPrivate then
          PrivateMethods.Add(MemberName, Method)
        else
          Methods.Add(MemberName, Method);
      end
      else
        raise TGocciaSyntaxError.Create('Expected "(" for method, "=" for property assignment, or ";" for property declaration',
          Peek.Line, Peek.Column, FFileName, FSourceLines,
          SSuggestClassMemberExpectedSyntax);
    end;

    Consume(gttRightBrace, 'Expected "}" after class body',
      SSuggestCloseClassBody);
    ValidateCurrentPrivateClassContext;
  finally
    PopPrivateClassContext;
  end;

  Result := TGocciaClassDefinition.Create(AClassName, SuperClass, Methods, Getters, Setters, StaticProperties, InstanceProperties, PrivateInstanceProperties, PrivateMethods, PrivateStaticProperties);
  Result.GenericParams := ClassGenericParams;
  Result.ImplementsClause := ClassImplementsClause;
  for TypePair in InstancePropertyTypes do
    Result.FInstancePropertyTypes.Add(TypePair.Key, TypePair.Value);

  Result.FStaticGetters.Free;
  Result.FStaticGetters := StaticGetters;
  Result.FStaticSetters.Free;
  Result.FStaticSetters := StaticSetters;
  Result.FComputedStaticGetters := ComputedStaticGetters;
  Result.FComputedStaticSetters := ComputedStaticSetters;
  Result.FComputedInstanceGetters := ComputedInstanceGetters;
  Result.FComputedInstanceSetters := ComputedInstanceSetters;
  Result.FElements := Elements;
  Result.FFieldOrder := FieldOrder;

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
    FFileName, FSourceLines,
    SSuggestNumberFormatValid);
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
    raise TGocciaSyntaxError.Create('Expected destructuring pattern', Peek.Line, Peek.Column, FFileName, FSourceLines,
      SSuggestDestructuringArrayOrObject);
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
    else if Match(gttSpread) then
    begin
      // Rest pattern: ...rest
      Pattern := ParsePattern;
      Elements.Add(TGocciaRestDestructuringPattern.Create(Pattern, Previous.Line, Previous.Column));
    end
    else
    begin
      Pattern := ParsePattern;

      // Check for default value
      if Match(gttAssign) then
      begin
        Pattern := TGocciaAssignmentDestructuringPattern.Create(Pattern, Expression, Pattern.Line, Pattern.Column);
      end;

      Elements.Add(Pattern);
    end;

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBracket, 'Expected "]" after array pattern',
    SSuggestCloseBracketDestructuringPattern);
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

    if Match(gttSpread) then
    begin
      // Rest pattern in object: ...rest
      Pattern := ParsePattern;
      Prop := TGocciaDestructuringProperty.Create('', TGocciaRestDestructuringPattern.Create(Pattern, Previous.Line, Previous.Column));
      Properties.Add(Prop);
    end
    else
    begin
      // Parse property key
      if Match(gttLeftBracket) then
      begin
        // Computed property: [expr]: pattern
        IsComputed := True;
        KeyExpression := Expression;
        Key := ''; // Will be computed at runtime
        Consume(gttRightBracket, 'Expected "]" after computed property key',
          SSuggestCloseBracketComputedPropertyKey);
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
        raise TGocciaSyntaxError.Create('Expected property name in object pattern', Peek.Line, Peek.Column, FFileName, FSourceLines,
          SSuggestObjectPatternPropertyName);

      // Check for shorthand or full syntax
      if Match(gttColon) then
      begin
        // Full syntax: key: pattern
        Pattern := ParsePattern;

        // Check for default value
        if Match(gttAssign) then
        begin
          Pattern := TGocciaAssignmentDestructuringPattern.Create(Pattern, Expression, Pattern.Line, Pattern.Column);
        end;
      end
      else
      begin
        // Shorthand syntax: {name} means {name: name}
        Pattern := TGocciaIdentifierDestructuringPattern.Create(Key, Previous.Line, Previous.Column);

        // Check for default value in shorthand
        if Match(gttAssign) then
        begin
          Pattern := TGocciaAssignmentDestructuringPattern.Create(Pattern, Expression, Pattern.Line, Pattern.Column);
        end;
      end;

      Prop := TGocciaDestructuringProperty.Create(Key, Pattern, IsComputed, KeyExpression);
      Properties.Add(Prop);
    end;

    if not Match(gttComma) then
      Break;
  end;

  Consume(gttRightBrace, 'Expected "}" after object pattern',
    SSuggestCloseDestructuringPattern);
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
  PropPair: TGocciaExpressionMap.TKeyValuePair;
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

    for PropPair in ObjectExpr.Properties do
    begin
      Prop := TGocciaDestructuringProperty.Create(PropPair.Key, ConvertToPattern(PropPair.Value));
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
    raise TGocciaSyntaxError.Create('Invalid destructuring target', AExpr.Line, AExpr.Column, FFileName, FSourceLines,
      SSuggestDestructuringInvalidTarget);
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
  Consume(gttLeftParen, 'Expected "(" after "switch"',
    SSuggestOpenParenSwitchExpression);
  Discriminant := Expression;
  Consume(gttRightParen, 'Expected ")" after switch discriminant',
    SSuggestCloseParenSwitchExpression);

  // Parse switch body
  Consume(gttLeftBrace, 'Expected "{" before switch body',
    SSuggestOpenBraceSwitchBody);

  Cases := TObjectList<TGocciaCaseClause>.Create(True);

  while not Check(gttRightBrace) and not IsAtEnd do
  begin
    if Match(gttCase) then
    begin
      // Parse case value
      TestExpression := Expression;
      Consume(gttColon, 'Expected ":" after case value',
        SSuggestAddColonAfterCase);
    end
    else if Match(gttDefault) then
    begin
      // Default case
      TestExpression := nil;
      Consume(gttColon, 'Expected ":" after default',
        SSuggestAddColonAfterDefault);
    end
    else
    begin
      raise TGocciaSyntaxError.Create('Expected "case" or "default" in switch body',
        Peek.Line, Peek.Column, FFileName, FSourceLines,
        SSuggestSwitchCaseOrDefault);
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

  Consume(gttRightBrace, 'Expected "}" after switch body',
    SSuggestCloseSwitchBody);
  Result := TGocciaSwitchStatement.Create(Discriminant, Cases, Line, Column);
end;

function TGocciaParser.BreakStatement: TGocciaStatement;
var
  Line, Column: Integer;
begin
  Line := Previous.Line;
  Column := Previous.Column;

  ConsumeSemicolonOrASI('Expected ";" after break statement',
    SSuggestAddSemicolon);
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
