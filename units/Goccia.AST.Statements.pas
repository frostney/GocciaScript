unit Goccia.AST.Statements;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedMap,

  Goccia.AST.Expressions,
  Goccia.AST.Node;

type
  TGocciaVariableInfo = record
    Name: string;
    Initializer: TGocciaExpression;
    TypeAnnotation: string;
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
    FTypeAnnotation: string;
  public
    constructor Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ALine, AColumn: Integer);
    property Pattern: TGocciaDestructuringPattern read FPattern;
    property Initializer: TGocciaExpression read FInitializer;
    property IsConst: Boolean read FIsConst;
    property TypeAnnotation: string read FTypeAnnotation write FTypeAnnotation;
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
    FCatchParamType: string;
  public
    constructor Create(const ABlock: TGocciaBlockStatement; const ACatchParam: string;
      const ACatchBlock, AFinallyBlock: TGocciaBlockStatement; const ALine, AColumn: Integer);
    property Block: TGocciaBlockStatement read FBlock;
    property CatchParam: string read FCatchParam;
    property CatchBlock: TGocciaBlockStatement read FCatchBlock;
    property FinallyBlock: TGocciaBlockStatement read FFinallyBlock;
    property CatchParamType: string read FCatchParamType write FCatchParamType;
  end;

  // Is this not a statement?
  TGocciaClassMethod = class(TGocciaExpression)
  private
    FName: string;
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FIsStatic: Boolean;
    FReturnType: string;
    FGenericParams: string;
  public
    constructor Create(const AName: string; const AParameters: TGocciaParameterArray;
      const ABody: TGocciaASTNode; const AIsStatic: Boolean; const ALine, AColumn: Integer);
    property Name: string read FName write FName;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property IsStatic: Boolean read FIsStatic;
    property ReturnType: string read FReturnType write FReturnType;
    property GenericParams: string read FGenericParams write FGenericParams;
  end;

  TGocciaComputedStaticAccessorEntry = record
    KeyExpression: TGocciaExpression;
    GetterExpression: TGocciaGetterExpression;
  end;

  // TC39 proposal-decorators: class element kinds
  TGocciaClassElementKind = (
    cekMethod,
    cekGetter,
    cekSetter,
    cekField,
    cekAccessor
  );

  // TC39 proposal-decorators: unified class element with optional decorators
  TGocciaClassElement = record
    Kind: TGocciaClassElementKind;
    Name: string;
    IsStatic: Boolean;
    IsPrivate: Boolean;
    IsComputed: Boolean;
    ComputedKeyExpression: TGocciaExpression;
    Decorators: TGocciaDecoratorList;
    MethodNode: TGocciaClassMethod;
    GetterNode: TGocciaGetterExpression;
    SetterNode: TGocciaSetterExpression;
    FieldInitializer: TGocciaExpression;
    TypeAnnotation: string;
  end;

  // Shared class definition structure
  TGocciaClassDefinition = class
  public
    FName: string;
    FSuperClass: string;
    FMethods: TDictionary<string, TGocciaClassMethod>;
    FGetters: TDictionary<string, TGocciaGetterExpression>;
    FSetters: TDictionary<string, TGocciaSetterExpression>;
    FStaticGetters: TDictionary<string, TGocciaGetterExpression>;
    FStaticSetters: TDictionary<string, TGocciaSetterExpression>;
    FComputedStaticGetters: array of TGocciaComputedStaticAccessorEntry;
    FStaticProperties: TDictionary<string, TGocciaExpression>;
    FInstanceProperties: TOrderedMap<TGocciaExpression>;
    FPrivateInstanceProperties: TOrderedMap<TGocciaExpression>;
    FPrivateStaticProperties: TDictionary<string, TGocciaExpression>;
    FPrivateMethods: TDictionary<string, TGocciaClassMethod>;
    FGenericParams: string;
    FImplementsClause: string;
    FInstancePropertyTypes: TDictionary<string, string>;
    FDecorators: TGocciaDecoratorList;
    FElements: array of TGocciaClassElement;

    constructor Create(const AName, ASuperClass: string;
      const AMethods: TDictionary<string, TGocciaClassMethod>;
      const AGetters: TDictionary<string, TGocciaGetterExpression>;
      const ASetters: TDictionary<string, TGocciaSetterExpression>;
      const AStaticProperties: TDictionary<string, TGocciaExpression>;
      const AInstanceProperties: TOrderedMap<TGocciaExpression>;
      const APrivateInstanceProperties: TOrderedMap<TGocciaExpression>;
      const APrivateMethods: TDictionary<string, TGocciaClassMethod> = nil;
      const APrivateStaticProperties: TDictionary<string, TGocciaExpression> = nil);
    destructor Destroy; override;
    property Name: string read FName;
    property SuperClass: string read FSuperClass;
    property Methods: TDictionary<string, TGocciaClassMethod> read FMethods;
    property Getters: TDictionary<string, TGocciaGetterExpression> read FGetters;
    property Setters: TDictionary<string, TGocciaSetterExpression> read FSetters;
    property StaticGetters: TDictionary<string, TGocciaGetterExpression> read FStaticGetters;
    property StaticSetters: TDictionary<string, TGocciaSetterExpression> read FStaticSetters;
    property StaticProperties: TDictionary<string, TGocciaExpression> read FStaticProperties;
    property InstanceProperties: TOrderedMap<TGocciaExpression> read FInstanceProperties;
    property PrivateInstanceProperties: TOrderedMap<TGocciaExpression> read FPrivateInstanceProperties;
    property PrivateStaticProperties: TDictionary<string, TGocciaExpression> read FPrivateStaticProperties;
    property PrivateMethods: TDictionary<string, TGocciaClassMethod> read FPrivateMethods;
    property GenericParams: string read FGenericParams write FGenericParams;
    property ImplementsClause: string read FImplementsClause write FImplementsClause;
    property InstancePropertyTypes: TDictionary<string, string> read FInstancePropertyTypes;
    property Decorators: TGocciaDecoratorList read FDecorators write FDecorators;
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

  // Enums
  TGocciaEnumMember = record
    Name: string;
    Initializer: TGocciaExpression;
  end;

  TGocciaEnumDeclaration = class(TGocciaStatement)
  private
    FName: string;
    FMembers: TArray<TGocciaEnumMember>;
  public
    constructor Create(const AName: string; const AMembers: TArray<TGocciaEnumMember>;
      const ALine, AColumn: Integer);
    property Name: string read FName;
    property Members: TArray<TGocciaEnumMember> read FMembers;
  end;

  TGocciaExportEnumDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaEnumDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaEnumDeclaration;
      const ALine, AColumn: Integer);
    property Declaration: TGocciaEnumDeclaration read FDeclaration;
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

  TGocciaExportVariableDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaVariableDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaVariableDeclaration;
      const ALine, AColumn: Integer);
    property Declaration: TGocciaVariableDeclaration read FDeclaration;
  end;

  TGocciaReExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TDictionary<string, string>; // exported name -> source name
    FModulePath: string;
  public
    constructor Create(const AExportsTable: TDictionary<string, string>;
      const AModulePath: string; const ALine, AColumn: Integer);
    property ExportsTable: TDictionary<string, string> read FExportsTable;
    property ModulePath: string read FModulePath;
  end;

  TGocciaEmptyStatement = class(TGocciaStatement)
  public
    constructor Create(const ALine, AColumn: Integer);
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

uses
  Goccia.Values.Primitives;

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
    const AInstanceProperties: TOrderedMap<TGocciaExpression>;
    const APrivateInstanceProperties: TOrderedMap<TGocciaExpression>;
    const APrivateMethods: TDictionary<string, TGocciaClassMethod> = nil;
    const APrivateStaticProperties: TDictionary<string, TGocciaExpression> = nil);
  begin
    FName := AName;
    FSuperClass := ASuperClass;
    FMethods := AMethods;
    FGetters := AGetters;
    FSetters := ASetters;
    FStaticGetters := TDictionary<string, TGocciaGetterExpression>.Create;
    FStaticSetters := TDictionary<string, TGocciaSetterExpression>.Create;
    SetLength(FComputedStaticGetters, 0);
    FStaticProperties := AStaticProperties;
    FInstanceProperties := AInstanceProperties;
    FPrivateInstanceProperties := APrivateInstanceProperties;

    if Assigned(APrivateMethods) then
      FPrivateMethods := APrivateMethods
    else
      FPrivateMethods := TDictionary<string, TGocciaClassMethod>.Create;

    if Assigned(APrivateStaticProperties) then
      FPrivateStaticProperties := APrivateStaticProperties
    else
      FPrivateStaticProperties := TDictionary<string, TGocciaExpression>.Create;

    FInstancePropertyTypes := TDictionary<string, string>.Create;
  end;

  destructor TGocciaClassDefinition.Destroy;
  begin
    FMethods.Free;
    FGetters.Free;
    FSetters.Free;
    FStaticGetters.Free;
    FStaticSetters.Free;
    FStaticProperties.Free;
    FInstanceProperties.Free;
    FPrivateInstanceProperties.Free;
    FInstancePropertyTypes.Free;
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

  { TGocciaEnumDeclaration }

  constructor TGocciaEnumDeclaration.Create(const AName: string;
    const AMembers: TArray<TGocciaEnumMember>; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FMembers := AMembers;
  end;

  { TGocciaExportEnumDeclaration }

  constructor TGocciaExportEnumDeclaration.Create(const ADeclaration: TGocciaEnumDeclaration;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDeclaration := ADeclaration;
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

  { TGocciaExportVariableDeclaration }

  constructor TGocciaExportVariableDeclaration.Create(const ADeclaration: TGocciaVariableDeclaration;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDeclaration := ADeclaration;
  end;

  { TGocciaReExportDeclaration }

  constructor TGocciaReExportDeclaration.Create(const AExportsTable: TDictionary<string, string>;
    const AModulePath: string; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExportsTable := AExportsTable;
    FModulePath := AModulePath;
  end;

  { TGocciaEmptyStatement }

  constructor TGocciaEmptyStatement.Create(const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
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
