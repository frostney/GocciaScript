unit Goccia.AST.Statements;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Values.Primitives;

type
  TGocciaVariableInfo = record
    Name: string;
    Initializer: TGocciaExpression;
  end;
  TGocciaExpressionStatement = class(TGocciaStatement)
  private
    FExpression: TGocciaExpression;
  public
    constructor Create(const AExpression: TGocciaExpression; const ALine, AColumn: Integer);
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaVariableDeclaration = class(TGocciaStatement)
  private
    FVariables: TArray<TGocciaVariableInfo>;
    FIsConst: Boolean;
  public
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      const AIsConst: Boolean; const ALine, AColumn: Integer);
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsConst: Boolean read FIsConst;
  end;

  TGocciaDestructuringDeclaration = class(TGocciaStatement)
  private
    FPattern: TGocciaDestructuringPattern;
    FInitializer: TGocciaExpression;
    FIsConst: Boolean;
  public
    constructor Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ALine, AColumn: Integer);
    property Pattern: TGocciaDestructuringPattern read FPattern;
    property Initializer: TGocciaExpression read FInitializer;
    property IsConst: Boolean read FIsConst;
  end;

  TGocciaBlockStatement = class(TGocciaStatement)
  private
    FNodes: TObjectList<TGocciaASTNode>;
  public
    constructor Create(const ANodes: TObjectList<TGocciaASTNode>;
      const ALine, AColumn: Integer);
    property Nodes: TObjectList<TGocciaASTNode> read FNodes;
  end;

  TGocciaIfStatement = class(TGocciaStatement)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaStatement;
    FAlternate: TGocciaStatement;
  public
    constructor Create(const ACondition: TGocciaExpression;
      const AConsequent, AAlternate: TGocciaStatement; const ALine, AColumn: Integer);
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
    constructor Create(const AInit: TGocciaStatement; const ACondition: TGocciaExpression;
      const AUpdate: TGocciaExpression; const ABody: TGocciaStatement; const ALine, AColumn: Integer);
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
    constructor Create(const ACondition: TGocciaExpression; const ABody: TGocciaStatement; const ALine, AColumn: Integer);
    property Condition: TGocciaExpression read FCondition;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaDoWhileStatement = class(TGocciaStatement)
  private
    FBody: TGocciaStatement;        // Loop body (executed first)
    FCondition: TGocciaExpression;  // Loop condition (checked after body)
  public
    constructor Create(const ABody: TGocciaStatement; const ACondition: TGocciaExpression; const ALine, AColumn: Integer);
    property Body: TGocciaStatement read FBody;
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaReturnStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaThrowStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaTryStatement = class(TGocciaStatement)
  private
    FBlock: TGocciaBlockStatement;
    FCatchParam: string;
    FCatchBlock: TGocciaBlockStatement;
    FFinallyBlock: TGocciaBlockStatement;
  public
    constructor Create(const ABlock: TGocciaBlockStatement; const ACatchParam: string;
      const ACatchBlock, AFinallyBlock: TGocciaBlockStatement; const ALine, AColumn: Integer);
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
    constructor Create(const AName: string; const AParameters: TGocciaParameterArray;
      const ABody: TGocciaASTNode; const AIsStatic: Boolean; const ALine, AColumn: Integer);
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
      const AMethods: TDictionary<string, TGocciaClassMethod>;
      const AGetters: TDictionary<string, TGocciaGetterExpression>;
      const ASetters: TDictionary<string, TGocciaSetterExpression>;
      const AStaticProperties: TDictionary<string, TGocciaExpression>;
      const AInstanceProperties: TDictionary<string, TGocciaExpression>;
      const APrivateInstanceProperties: TDictionary<string, TGocciaExpression> = nil;
      const APrivateMethods: TDictionary<string, TGocciaClassMethod> = nil;
      const APrivateStaticProperties: TDictionary<string, TGocciaExpression> = nil);
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
    constructor Create(const AClassDefinition: TGocciaClassDefinition; const ALine, AColumn: Integer);
    destructor Destroy; override;
    property ClassDefinition: TGocciaClassDefinition read FClassDefinition;
  end;

  TGocciaClassExpression = class(TGocciaExpression)
  private
    FClassDefinition: TGocciaClassDefinition;
  public
    constructor Create(const AClassDefinition: TGocciaClassDefinition; const ALine, AColumn: Integer);
    destructor Destroy; override;
    property ClassDefinition: TGocciaClassDefinition read FClassDefinition;
  end;

  // Modules
  TGocciaImportDeclaration = class(TGocciaStatement)
  private
    FImports: TDictionary<string, string>; // local name -> imported name
    FModulePath: string;
  public
    constructor Create(const AImports: TDictionary<string, string>;
      const AModulePath: string; const ALine, AColumn: Integer);
    property Imports: TDictionary<string, string> read FImports;
    property ModulePath: string read FModulePath;
  end;

  TGocciaExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TDictionary<string, string>; // exported name -> local name
  public
    constructor Create(const AExportsTable: TDictionary<string, string>;
      const ALine, AColumn: Integer);
    property ExportsTable: TDictionary<string, string> read FExportsTable;
  end;

  TGocciaCaseClause = class(TGocciaASTNode)
  private
    FTest: TGocciaExpression;  // Case value expression (nil for default case)
    FConsequent: TObjectList<TGocciaStatement>;  // Statements to execute
  public
    constructor Create(const ATest: TGocciaExpression; const AConsequent: TObjectList<TGocciaStatement>; const ALine, AColumn: Integer);
    destructor Destroy; override;
    property Test: TGocciaExpression read FTest;
    property Consequent: TObjectList<TGocciaStatement> read FConsequent;
  end;

  TGocciaSwitchStatement = class(TGocciaStatement)
  private
    FDiscriminant: TGocciaExpression;  // Expression to switch on
    FCases: TObjectList<TGocciaCaseClause>;  // List of case clauses
  public
    constructor Create(const ADiscriminant: TGocciaExpression; const ACases: TObjectList<TGocciaCaseClause>; const ALine, AColumn: Integer);
    destructor Destroy; override;
    property Discriminant: TGocciaExpression read FDiscriminant;
    property Cases: TObjectList<TGocciaCaseClause> read FCases;
  end;

  TGocciaBreakStatement = class(TGocciaStatement)
  public
    constructor Create(const ALine, AColumn: Integer);
  end;

implementation

{ TGocciaExpressionStatement }

  constructor TGocciaExpressionStatement.Create(const AExpression: TGocciaExpression;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExpression := AExpression;
  end;

  { TGocciaVariableDeclaration }

  constructor TGocciaVariableDeclaration.Create(const AVariables: TArray<TGocciaVariableInfo>;
    const AIsConst: Boolean; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FVariables := AVariables;
    FIsConst := AIsConst;
  end;

  { TGocciaDestructuringDeclaration }

  constructor TGocciaDestructuringDeclaration.Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FPattern := APattern;
    FInitializer := AInitializer;
    FIsConst := AIsConst;
  end;

  { TGocciaBlockStatement }

  constructor TGocciaBlockStatement.Create(const ANodes: TObjectList<TGocciaASTNode>;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FNodes := ANodes;
  end;

  { TGocciaClassDefinition }

  constructor TGocciaClassDefinition.Create(const AName, ASuperClass: string;
    const AMethods: TDictionary<string, TGocciaClassMethod>;
    const AGetters: TDictionary<string, TGocciaGetterExpression>;
    const ASetters: TDictionary<string, TGocciaSetterExpression>;
    const AStaticProperties: TDictionary<string, TGocciaExpression>;
    const AInstanceProperties: TDictionary<string, TGocciaExpression>;
    const APrivateInstanceProperties: TDictionary<string, TGocciaExpression> = nil;
    const APrivateMethods: TDictionary<string, TGocciaClassMethod> = nil;
    const APrivateStaticProperties: TDictionary<string, TGocciaExpression> = nil);
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

  constructor TGocciaIfStatement.Create(const ACondition: TGocciaExpression;
    const AConsequent, AAlternate: TGocciaStatement; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FCondition := ACondition;
    FConsequent := AConsequent;
    FAlternate := AAlternate;
  end;

  { TGocciaForStatement }

  constructor TGocciaForStatement.Create(const AInit: TGocciaStatement; const ACondition: TGocciaExpression;
    const AUpdate: TGocciaExpression; const ABody: TGocciaStatement; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FInit := AInit;
    FCondition := ACondition;
    FUpdate := AUpdate;
    FBody := ABody;
  end;

  { TGocciaReturnStatement }

  constructor TGocciaReturnStatement.Create(const AValue: TGocciaExpression;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    if AValue = nil then
      FValue := TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, ALine, AColumn)
    else
      FValue := AValue;
  end;

  { TGocciaThrowStatement }

  constructor TGocciaThrowStatement.Create(const AValue: TGocciaExpression;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FValue := AValue;
  end;

  { TGocciaTryStatement }

  constructor TGocciaTryStatement.Create(const ABlock: TGocciaBlockStatement;
    const ACatchParam: string; const ACatchBlock, AFinallyBlock: TGocciaBlockStatement;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FBlock := ABlock;
    FCatchParam := ACatchParam;
    FCatchBlock := ACatchBlock;
    FFinallyBlock := AFinallyBlock;
  end;

  { TGocciaClassMethod }

    constructor TGocciaClassMethod.Create(const AName: string; const AParameters: TGocciaParameterArray;
    const ABody: TGocciaASTNode; const AIsStatic: Boolean; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FParameters := AParameters;
    FBody := ABody;
    FIsStatic := AIsStatic;
  end;

  { TGocciaClassDeclaration }

  constructor TGocciaClassDeclaration.Create(const AClassDefinition: TGocciaClassDefinition; const ALine, AColumn: Integer);
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

  constructor TGocciaClassExpression.Create(const AClassDefinition: TGocciaClassDefinition; const ALine, AColumn: Integer);
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

  constructor TGocciaImportDeclaration.Create(const AImports: TDictionary<string, string>;
    const AModulePath: string; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FImports := AImports;
    FModulePath := AModulePath;
  end;

  { TGocciaExportDeclaration }

  constructor TGocciaExportDeclaration.Create(const AExportsTable: TDictionary<string, string>;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExportsTable := AExportsTable;
  end;

  { TGocciaWhileStatement }

  constructor TGocciaWhileStatement.Create(const ACondition: TGocciaExpression; const ABody: TGocciaStatement; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FCondition := ACondition;
    FBody := ABody;
  end;

  { TGocciaDoWhileStatement }

  constructor TGocciaDoWhileStatement.Create(const ABody: TGocciaStatement; const ACondition: TGocciaExpression; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FBody := ABody;
    FCondition := ACondition;
  end;

  { TGocciaCaseClause }

  constructor TGocciaCaseClause.Create(const ATest: TGocciaExpression; const AConsequent: TObjectList<TGocciaStatement>; const ALine, AColumn: Integer);
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

  constructor TGocciaSwitchStatement.Create(const ADiscriminant: TGocciaExpression; const ACases: TObjectList<TGocciaCaseClause>; const ALine, AColumn: Integer);
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

  constructor TGocciaBreakStatement.Create(const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
  end;

end.
