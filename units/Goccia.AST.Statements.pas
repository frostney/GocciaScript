unit Goccia.AST.Statements;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node, Goccia.AST.Expressions, Generics.Collections, Classes, Goccia.Values.Primitives;

type
  TGocciaVariableInfo = record
    Name: string;
    Initializer: TGocciaExpression;
  end;
  TGocciaExpressionStatement = class(TGocciaStatement)
  private
    FExpression: TGocciaExpression;
  public
    constructor Create(AExpression: TGocciaExpression; ALine, AColumn: Integer);
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaVariableDeclaration = class(TGocciaStatement)
  private
    FVariables: TArray<TGocciaVariableInfo>;
    FIsConst: Boolean;
  public
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      AIsConst: Boolean; ALine, AColumn: Integer);
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsConst: Boolean read FIsConst;
  end;

  TGocciaDestructuringDeclaration = class(TGocciaStatement)
  private
    FPattern: TGocciaDestructuringPattern;
    FInitializer: TGocciaExpression;
    FIsConst: Boolean;
  public
    constructor Create(APattern: TGocciaDestructuringPattern; AInitializer: TGocciaExpression; AIsConst: Boolean; ALine, AColumn: Integer);
    property Pattern: TGocciaDestructuringPattern read FPattern;
    property Initializer: TGocciaExpression read FInitializer;
    property IsConst: Boolean read FIsConst;
  end;

  TGocciaBlockStatement = class(TGocciaStatement)
  private
    FNodes: TObjectList<TGocciaASTNode>;
  public
    constructor Create(ANodes: TObjectList<TGocciaASTNode>;
      ALine, AColumn: Integer);
    property Nodes: TObjectList<TGocciaASTNode> read FNodes;
  end;

  TGocciaIfStatement = class(TGocciaStatement)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaStatement;
    FAlternate: TGocciaStatement;
  public
    constructor Create(ACondition: TGocciaExpression;
      AConsequent, AAlternate: TGocciaStatement; ALine, AColumn: Integer);
    property Condition: TGocciaExpression read FCondition;
    property Consequent: TGocciaStatement read FConsequent;
    property Alternate: TGocciaStatement read FAlternate;
  end;

  TGocciaForStatement = class(TGocciaStatement)
  private
    FInit: TGocciaStatement;  // Can be variable declaration or expression statement
    FCondition: TGocciaExpression;  // Loop condition
    FUpdate: TGocciaExpression;     // Update expression
    FBody: TGocciaStatement;        // Loop body
  public
    constructor Create(AInit: TGocciaStatement; ACondition: TGocciaExpression;
      AUpdate: TGocciaExpression; ABody: TGocciaStatement; ALine, AColumn: Integer);
    property Init: TGocciaStatement read FInit;
    property Condition: TGocciaExpression read FCondition;
    property Update: TGocciaExpression read FUpdate;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaWhileStatement = class(TGocciaStatement)
  private
    FCondition: TGocciaExpression;  // Loop condition
    FBody: TGocciaStatement;        // Loop body
  public
    constructor Create(ACondition: TGocciaExpression; ABody: TGocciaStatement; ALine, AColumn: Integer);
    property Condition: TGocciaExpression read FCondition;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaDoWhileStatement = class(TGocciaStatement)
  private
    FBody: TGocciaStatement;        // Loop body (executed first)
    FCondition: TGocciaExpression;  // Loop condition (checked after body)
  public
    constructor Create(ABody: TGocciaStatement; ACondition: TGocciaExpression; ALine, AColumn: Integer);
    property Body: TGocciaStatement read FBody;
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaReturnStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(AValue: TGocciaExpression; ALine, AColumn: Integer);
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaThrowStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(AValue: TGocciaExpression; ALine, AColumn: Integer);
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaTryStatement = class(TGocciaStatement)
  private
    FBlock: TGocciaBlockStatement;
    FCatchParam: string;
    FCatchBlock: TGocciaBlockStatement;
    FFinallyBlock: TGocciaBlockStatement;
  public
    constructor Create(ABlock: TGocciaBlockStatement; const ACatchParam: string;
      ACatchBlock, AFinallyBlock: TGocciaBlockStatement; ALine, AColumn: Integer);
    property Block: TGocciaBlockStatement read FBlock;
    property CatchParam: string read FCatchParam;
    property CatchBlock: TGocciaBlockStatement read FCatchBlock;
    property FinallyBlock: TGocciaBlockStatement read FFinallyBlock;
  end;

  // Is this not a statement?
  TGocciaClassMethod = class(TGocciaExpression)
  private
    FName: string;
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FIsStatic: Boolean;
  public
    constructor Create(const AName: string; AParameters: TGocciaParameterArray;
      ABody: TGocciaASTNode; AIsStatic: Boolean; ALine, AColumn: Integer);
    property Name: string read FName write FName;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property IsStatic: Boolean read FIsStatic;
  end;

  // Shared class definition structure
  TGocciaClassDefinition = class
  public
    FName: string;
    FSuperClass: string;
    FMethods: TDictionary<string, TGocciaClassMethod>;
    FGetters: TDictionary<string, TGocciaGetterExpression>;
    FSetters: TDictionary<string, TGocciaSetterExpression>;
    FStaticProperties: TDictionary<string, TGocciaExpression>;
    FInstanceProperties: TDictionary<string, TGocciaExpression>;
    FInstancePropertyOrder: TStringList; // Preserves declaration order
    FPrivateInstanceProperties: TDictionary<string, TGocciaExpression>;
    FPrivateInstancePropertyOrder: TStringList; // Preserves declaration order
    FPrivateStaticProperties: TDictionary<string, TGocciaExpression>;
    FPrivateMethods: TDictionary<string, TGocciaClassMethod>;

    constructor Create(const AName, ASuperClass: string;
      AMethods: TDictionary<string, TGocciaClassMethod>;
      AGetters: TDictionary<string, TGocciaGetterExpression>;
      ASetters: TDictionary<string, TGocciaSetterExpression>;
      AStaticProperties: TDictionary<string, TGocciaExpression>;
      AInstanceProperties: TDictionary<string, TGocciaExpression>;
      APrivateInstanceProperties: TDictionary<string, TGocciaExpression> = nil;
      APrivateMethods: TDictionary<string, TGocciaClassMethod> = nil;
      APrivateStaticProperties: TDictionary<string, TGocciaExpression> = nil);
    destructor Destroy; override;
    property Name: string read FName;
    property SuperClass: string read FSuperClass;
    property Methods: TDictionary<string, TGocciaClassMethod> read FMethods;
    property Getters: TDictionary<string, TGocciaGetterExpression> read FGetters;
    property Setters: TDictionary<string, TGocciaSetterExpression> read FSetters;
    property StaticProperties: TDictionary<string, TGocciaExpression> read FStaticProperties;
    property InstanceProperties: TDictionary<string, TGocciaExpression> read FInstanceProperties;
    property InstancePropertyOrder: TStringList read FInstancePropertyOrder;
    property PrivateInstanceProperties: TDictionary<string, TGocciaExpression> read FPrivateInstanceProperties;
    property PrivateInstancePropertyOrder: TStringList read FPrivateInstancePropertyOrder;
    property PrivateStaticProperties: TDictionary<string, TGocciaExpression> read FPrivateStaticProperties;
    property PrivateMethods: TDictionary<string, TGocciaClassMethod> read FPrivateMethods;
  end;

  TGocciaClassDeclaration = class(TGocciaStatement)
  private
    FClassDefinition: TGocciaClassDefinition;
  public
    constructor Create(AClassDefinition: TGocciaClassDefinition; ALine, AColumn: Integer);
    destructor Destroy; override;
    property ClassDefinition: TGocciaClassDefinition read FClassDefinition;
  end;

  TGocciaClassExpression = class(TGocciaExpression)
  private
    FClassDefinition: TGocciaClassDefinition;
  public
    constructor Create(AClassDefinition: TGocciaClassDefinition; ALine, AColumn: Integer);
    destructor Destroy; override;
    property ClassDefinition: TGocciaClassDefinition read FClassDefinition;
  end;

  // Modules
  TGocciaImportDeclaration = class(TGocciaStatement)
  private
    FImports: TDictionary<string, string>; // local name -> imported name
    FModulePath: string;
  public
    constructor Create(AImports: TDictionary<string, string>;
      const AModulePath: string; ALine, AColumn: Integer);
    property Imports: TDictionary<string, string> read FImports;
    property ModulePath: string read FModulePath;
  end;

  TGocciaExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TDictionary<string, string>; // exported name -> local name
  public
    constructor Create(AExportsTable: TDictionary<string, string>;
      ALine, AColumn: Integer);
    property ExportsTable: TDictionary<string, string> read FExportsTable;
  end;

  TGocciaCaseClause = class(TGocciaASTNode)
  private
    FTest: TGocciaExpression;  // Case value expression (nil for default case)
    FConsequent: TObjectList<TGocciaStatement>;  // Statements to execute
  public
    constructor Create(ATest: TGocciaExpression; AConsequent: TObjectList<TGocciaStatement>; ALine, AColumn: Integer);
    destructor Destroy; override;
    property Test: TGocciaExpression read FTest;
    property Consequent: TObjectList<TGocciaStatement> read FConsequent;
  end;

  TGocciaSwitchStatement = class(TGocciaStatement)
  private
    FDiscriminant: TGocciaExpression;  // Expression to switch on
    FCases: TObjectList<TGocciaCaseClause>;  // List of case clauses
  public
    constructor Create(ADiscriminant: TGocciaExpression; ACases: TObjectList<TGocciaCaseClause>; ALine, AColumn: Integer);
    destructor Destroy; override;
    property Discriminant: TGocciaExpression read FDiscriminant;
    property Cases: TObjectList<TGocciaCaseClause> read FCases;
  end;

  TGocciaBreakStatement = class(TGocciaStatement)
  public
    constructor Create(ALine, AColumn: Integer);
  end;

implementation

{ TGocciaExpressionStatement }

  constructor TGocciaExpressionStatement.Create(AExpression: TGocciaExpression;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExpression := AExpression;
  end;

  { TGocciaVariableDeclaration }

  constructor TGocciaVariableDeclaration.Create(const AVariables: TArray<TGocciaVariableInfo>;
    AIsConst: Boolean; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FVariables := AVariables;
    FIsConst := AIsConst;
  end;

  { TGocciaDestructuringDeclaration }

  constructor TGocciaDestructuringDeclaration.Create(APattern: TGocciaDestructuringPattern; AInitializer: TGocciaExpression; AIsConst: Boolean; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FPattern := APattern;
    FInitializer := AInitializer;
    FIsConst := AIsConst;
  end;

  { TGocciaBlockStatement }

  constructor TGocciaBlockStatement.Create(ANodes: TObjectList<TGocciaASTNode>;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FNodes := ANodes;
  end;

  { TGocciaClassDefinition }

  constructor TGocciaClassDefinition.Create(const AName, ASuperClass: string;
    AMethods: TDictionary<string, TGocciaClassMethod>;
    AGetters: TDictionary<string, TGocciaGetterExpression>;
    ASetters: TDictionary<string, TGocciaSetterExpression>;
    AStaticProperties: TDictionary<string, TGocciaExpression>;
    AInstanceProperties: TDictionary<string, TGocciaExpression>;
    APrivateInstanceProperties: TDictionary<string, TGocciaExpression> = nil;
    APrivateMethods: TDictionary<string, TGocciaClassMethod> = nil;
    APrivateStaticProperties: TDictionary<string, TGocciaExpression> = nil);
  begin
    FName := AName;
    FSuperClass := ASuperClass;
    FMethods := AMethods;
    FGetters := AGetters;
    FSetters := ASetters;
    FStaticProperties := AStaticProperties;
    FInstanceProperties := AInstanceProperties;
    FInstancePropertyOrder := TStringList.Create;
    FPrivateInstancePropertyOrder := TStringList.Create;

    if Assigned(APrivateInstanceProperties) then
      FPrivateInstanceProperties := APrivateInstanceProperties
    else
      FPrivateInstanceProperties := TDictionary<string, TGocciaExpression>.Create;

    if Assigned(APrivateMethods) then
      FPrivateMethods := APrivateMethods
    else
      FPrivateMethods := TDictionary<string, TGocciaClassMethod>.Create;

    if Assigned(APrivateStaticProperties) then
      FPrivateStaticProperties := APrivateStaticProperties
    else
      FPrivateStaticProperties := TDictionary<string, TGocciaExpression>.Create;
  end;

  destructor TGocciaClassDefinition.Destroy;
  begin
    FMethods.Free;
    FGetters.Free;
    FSetters.Free;
    FStaticProperties.Free;
    FInstanceProperties.Free;
    FInstancePropertyOrder.Free;
    FPrivateInstancePropertyOrder.Free;
    if Assigned(FPrivateInstanceProperties) then
      FPrivateInstanceProperties.Free;
    if Assigned(FPrivateMethods) then
      FPrivateMethods.Free;
    if Assigned(FPrivateStaticProperties) then
      FPrivateStaticProperties.Free;
    inherited;
  end;

  { TGocciaIfStatement }

  constructor TGocciaIfStatement.Create(ACondition: TGocciaExpression;
    AConsequent, AAlternate: TGocciaStatement; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FCondition := ACondition;
    FConsequent := AConsequent;
    FAlternate := AAlternate;
  end;

  { TGocciaForStatement }

  constructor TGocciaForStatement.Create(AInit: TGocciaStatement; ACondition: TGocciaExpression;
    AUpdate: TGocciaExpression; ABody: TGocciaStatement; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FInit := AInit;
    FCondition := ACondition;
    FUpdate := AUpdate;
    FBody := ABody;
  end;

  { TGocciaReturnStatement }

  constructor TGocciaReturnStatement.Create(AValue: TGocciaExpression;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    if AValue = nil then
      FValue := TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, ALine, AColumn)
    else
      FValue := AValue;
  end;

  { TGocciaThrowStatement }

  constructor TGocciaThrowStatement.Create(AValue: TGocciaExpression;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FValue := AValue;
  end;

  { TGocciaTryStatement }

  constructor TGocciaTryStatement.Create(ABlock: TGocciaBlockStatement;
    const ACatchParam: string; ACatchBlock, AFinallyBlock: TGocciaBlockStatement;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FBlock := ABlock;
    FCatchParam := ACatchParam;
    FCatchBlock := ACatchBlock;
    FFinallyBlock := AFinallyBlock;
  end;

  { TGocciaClassMethod }

    constructor TGocciaClassMethod.Create(const AName: string; AParameters: TGocciaParameterArray;
    ABody: TGocciaASTNode; AIsStatic: Boolean; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FParameters := AParameters;
    FBody := ABody;
    FIsStatic := AIsStatic;
  end;

  { TGocciaClassDeclaration }

  constructor TGocciaClassDeclaration.Create(AClassDefinition: TGocciaClassDefinition; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FClassDefinition := AClassDefinition;
  end;

  destructor TGocciaClassDeclaration.Destroy;
  begin
    FClassDefinition.Free;
    inherited;
  end;

  { TGocciaClassExpression }

  constructor TGocciaClassExpression.Create(AClassDefinition: TGocciaClassDefinition; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FClassDefinition := AClassDefinition;
  end;

  destructor TGocciaClassExpression.Destroy;
  begin
    FClassDefinition.Free;
    inherited;
  end;

  { TGocciaImportDeclaration }

  constructor TGocciaImportDeclaration.Create(AImports: TDictionary<string, string>;
    const AModulePath: string; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FImports := AImports;
    FModulePath := AModulePath;
  end;

  { TGocciaExportDeclaration }

  constructor TGocciaExportDeclaration.Create(AExportsTable: TDictionary<string, string>;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExportsTable := AExportsTable;
  end;

  { TGocciaWhileStatement }

  constructor TGocciaWhileStatement.Create(ACondition: TGocciaExpression; ABody: TGocciaStatement; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FCondition := ACondition;
    FBody := ABody;
  end;

  { TGocciaDoWhileStatement }

  constructor TGocciaDoWhileStatement.Create(ABody: TGocciaStatement; ACondition: TGocciaExpression; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FBody := ABody;
    FCondition := ACondition;
  end;

  { TGocciaCaseClause }

  constructor TGocciaCaseClause.Create(ATest: TGocciaExpression; AConsequent: TObjectList<TGocciaStatement>; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FTest := ATest;
    FConsequent := AConsequent;
  end;

  destructor TGocciaCaseClause.Destroy;
  begin
    FTest.Free;
    FConsequent.Free;
    inherited;
  end;

  { TGocciaSwitchStatement }

  constructor TGocciaSwitchStatement.Create(ADiscriminant: TGocciaExpression; ACases: TObjectList<TGocciaCaseClause>; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDiscriminant := ADiscriminant;
    FCases := ACases;
  end;

  destructor TGocciaSwitchStatement.Destroy;
  begin
    FDiscriminant.Free;
    FCases.Free;
    inherited;
  end;

  { TGocciaBreakStatement }

  constructor TGocciaBreakStatement.Create(ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
  end;

end.
