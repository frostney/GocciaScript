unit Goccia.AST.Statements;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.ControlFlow,
  Goccia.Evaluator.Context,
  Goccia.Values.Primitives;

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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaVariableDeclaration = class(TGocciaStatement)
  private
    FVariables: TArray<TGocciaVariableInfo>;
    FIsConst: Boolean;
    FIsVar: Boolean;
    FIsFunctionDeclaration: Boolean;
  public
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      const AIsConst: Boolean; const ALine, AColumn: Integer;
      const AIsVar: Boolean = False);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar;
    property IsFunctionDeclaration: Boolean read FIsFunctionDeclaration write FIsFunctionDeclaration;
  end;

  TGocciaDestructuringDeclaration = class(TGocciaStatement)
  private
    FPattern: TGocciaDestructuringPattern;
    FInitializer: TGocciaExpression;
    FIsConst: Boolean;
    FIsVar: Boolean;
    FTypeAnnotation: string;
  public
    constructor Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ALine, AColumn: Integer; const AIsVar: Boolean = False);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Pattern: TGocciaDestructuringPattern read FPattern;
    property Initializer: TGocciaExpression read FInitializer;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar;
    property TypeAnnotation: string read FTypeAnnotation write FTypeAnnotation;
  end;

  TGocciaBlockStatement = class(TGocciaStatement)
  private
    FNodes: TObjectList<TGocciaASTNode>;
  public
    constructor Create(const ANodes: TObjectList<TGocciaASTNode>;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Condition: TGocciaExpression read FCondition;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaDoWhileStatement = class(TGocciaStatement)
  private
    FBody: TGocciaStatement;        // Loop body (executed first)
    FCondition: TGocciaExpression;  // Loop condition (checked after body)
  public
    constructor Create(const ABody: TGocciaStatement; const ACondition: TGocciaExpression; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Body: TGocciaStatement read FBody;
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaForOfStatement = class(TGocciaStatement)
  private
    FIsConst: Boolean;
    FIsVar: Boolean;
    FBindingName: string;
    FBindingPattern: TGocciaDestructuringPattern;
    FIterable: TGocciaExpression;
    FBody: TGocciaStatement;
  public
    constructor Create(const AIsConst: Boolean; const ABindingName: string;
      const ABindingPattern: TGocciaDestructuringPattern; const AIterable: TGocciaExpression;
      const ABody: TGocciaStatement; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar write FIsVar;
    property BindingName: string read FBindingName;
    property BindingPattern: TGocciaDestructuringPattern read FBindingPattern;
    property Iterable: TGocciaExpression read FIterable;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaForAwaitOfStatement = class(TGocciaForOfStatement)
  public
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
  end;

  TGocciaReturnStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaThrowStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
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
    FIsAsync: Boolean;
    FReturnType: string;
    FGenericParams: string;
    FSourceText: string;
  public
    constructor Create(const AName: string; const AParameters: TGocciaParameterArray;
      const ABody: TGocciaASTNode; const AIsStatic: Boolean; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName write FName;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property IsStatic: Boolean read FIsStatic;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property ReturnType: string read FReturnType write FReturnType;
    property GenericParams: string read FGenericParams write FGenericParams;
    property SourceText: string read FSourceText write FSourceText;
  end;

  TGocciaClassMethodMap = TOrderedStringMap<TGocciaClassMethod>;

  TGocciaComputedGetterEntry = record
    KeyExpression: TGocciaExpression;
    GetterExpression: TGocciaGetterExpression;
  end;

  TGocciaComputedSetterEntry = record
    KeyExpression: TGocciaExpression;
    SetterExpression: TGocciaSetterExpression;
  end;

  TGocciaComputedStaticAccessorEntry = TGocciaComputedGetterEntry;

  // TC39 proposal-decorators: class element kinds
  TGocciaClassElementKind = (
    cekMethod,
    cekGetter,
    cekSetter,
    cekField,
    cekAccessor,
    cekStaticBlock
  );

  // TC39 proposal-decorators: unified class element with optional decorators
  TGocciaClassElement = record
    Kind: TGocciaClassElementKind;
    Name: string;
    IsStatic: Boolean;
    IsPrivate: Boolean;
    IsComputed: Boolean;
    IsAsync: Boolean;
    ComputedKeyExpression: TGocciaExpression;
    Decorators: TGocciaDecoratorList;
    MethodNode: TGocciaClassMethod;
    GetterNode: TGocciaGetterExpression;
    SetterNode: TGocciaSetterExpression;
    FieldInitializer: TGocciaExpression;
    StaticBlockBody: TGocciaBlockStatement;
    TypeAnnotation: string;
  end;

  TGocciaFieldOrderEntry = record
    Name: string;
    IsPrivate: Boolean;
  end;

  // Shared class definition structure
  TGocciaClassDefinition = class
  public
    FName: string;
    FSuperClass: string;
    FMethods: TGocciaClassMethodMap;
    FGetters: TGocciaGetterExpressionMap;
    FSetters: TGocciaSetterExpressionMap;
    FStaticGetters: TGocciaGetterExpressionMap;
    FStaticSetters: TGocciaSetterExpressionMap;
    FComputedStaticGetters: array of TGocciaComputedGetterEntry;
    FComputedStaticSetters: array of TGocciaComputedSetterEntry;
    FComputedInstanceGetters: array of TGocciaComputedGetterEntry;
    FComputedInstanceSetters: array of TGocciaComputedSetterEntry;
    FStaticProperties: TGocciaExpressionMap;
    FInstanceProperties: TGocciaExpressionMap;
    FPrivateInstanceProperties: TGocciaExpressionMap;
    FPrivateStaticProperties: TGocciaExpressionMap;
    FPrivateMethods: TGocciaClassMethodMap;
    FGenericParams: string;
    FImplementsClause: string;
    FInstancePropertyTypes: TStringStringMap;
    FDecorators: TGocciaDecoratorList;
    FElements: array of TGocciaClassElement;
    FFieldOrder: array of TGocciaFieldOrderEntry;

    constructor Create(const AName, ASuperClass: string;
      const AMethods: TGocciaClassMethodMap;
      const AGetters: TGocciaGetterExpressionMap;
      const ASetters: TGocciaSetterExpressionMap;
      const AStaticProperties: TGocciaExpressionMap;
      const AInstanceProperties: TGocciaExpressionMap;
      const APrivateInstanceProperties: TGocciaExpressionMap;
      const APrivateMethods: TGocciaClassMethodMap = nil;
      const APrivateStaticProperties: TGocciaExpressionMap = nil);
    destructor Destroy; override;
    property Name: string read FName;
    property SuperClass: string read FSuperClass;
    property Methods: TGocciaClassMethodMap read FMethods;
    property Getters: TGocciaGetterExpressionMap read FGetters;
    property Setters: TGocciaSetterExpressionMap read FSetters;
    property StaticGetters: TGocciaGetterExpressionMap read FStaticGetters;
    property StaticSetters: TGocciaSetterExpressionMap read FStaticSetters;
    property StaticProperties: TGocciaExpressionMap read FStaticProperties;
    property InstanceProperties: TGocciaExpressionMap read FInstanceProperties;
    property PrivateInstanceProperties: TGocciaExpressionMap read FPrivateInstanceProperties;
    property PrivateStaticProperties: TGocciaExpressionMap read FPrivateStaticProperties;
    property PrivateMethods: TGocciaClassMethodMap read FPrivateMethods;
    property GenericParams: string read FGenericParams write FGenericParams;
    property ImplementsClause: string read FImplementsClause write FImplementsClause;
    property InstancePropertyTypes: TStringStringMap read FInstancePropertyTypes;
    property Decorators: TGocciaDecoratorList read FDecorators write FDecorators;
  end;

  TGocciaClassDeclaration = class(TGocciaStatement)
  private
    FClassDefinition: TGocciaClassDefinition;
  public
    constructor Create(const AClassDefinition: TGocciaClassDefinition; const ALine, AColumn: Integer);
    destructor Destroy; override;
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property ClassDefinition: TGocciaClassDefinition read FClassDefinition;
  end;

  TGocciaClassExpression = class(TGocciaExpression)
  private
    FClassDefinition: TGocciaClassDefinition;
  public
    constructor Create(const AClassDefinition: TGocciaClassDefinition; const ALine, AColumn: Integer);
    destructor Destroy; override;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Name: string read FName;
    property Members: TArray<TGocciaEnumMember> read FMembers;
  end;

  TGocciaExportEnumDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaEnumDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaEnumDeclaration;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Declaration: TGocciaEnumDeclaration read FDeclaration;
  end;

  // Modules
  TGocciaImportDeclaration = class(TGocciaStatement)
  private
    FImports: TStringStringMap; // local name -> imported name
    FModulePath: string;
    FNamespaceName: string;
  public
    constructor Create(const AImports: TStringStringMap;
      const AModulePath: string; const ALine, AColumn: Integer;
      const ANamespaceName: string = '');
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Imports: TStringStringMap read FImports;
    property ModulePath: string read FModulePath;
    property NamespaceName: string read FNamespaceName;
  end;

  TGocciaExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TStringStringMap; // exported name -> local name
  public
    constructor Create(const AExportsTable: TStringStringMap;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property ExportsTable: TStringStringMap read FExportsTable;
  end;

  TGocciaExportVariableDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaVariableDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaVariableDeclaration;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Declaration: TGocciaVariableDeclaration read FDeclaration;
  end;

  TGocciaReExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TStringStringMap; // exported name -> source name
    FModulePath: string;
  public
    constructor Create(const AExportsTable: TStringStringMap;
      const AModulePath: string; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property ExportsTable: TStringStringMap read FExportsTable;
    property ModulePath: string read FModulePath;
  end;

  TGocciaEmptyStatement = class(TGocciaStatement)
  public
    constructor Create(const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
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
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Discriminant: TGocciaExpression read FDiscriminant;
    property Cases: TObjectList<TGocciaCaseClause> read FCases;
  end;

  TGocciaBreakStatement = class(TGocciaStatement)
  public
    constructor Create(const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
  end;

  TGocciaUsingDeclaration = class(TGocciaStatement)
  private
    FVariables: TArray<TGocciaVariableInfo>;
    FIsAwait: Boolean;
  public
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      const AIsAwait: Boolean; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsAwait: Boolean read FIsAwait;
  end;

implementation

uses
  SysUtils,

  Goccia.Evaluator,
  Goccia.GarbageCollector,
  Goccia.Modules,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Token,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue;

function CreateModuleNamespaceObject(const AModule: TGocciaModule): TGocciaValue;
begin
  Result := AModule.GetNamespaceObject;
end;

{ TGocciaExpressionStatement }

  constructor TGocciaExpressionStatement.Create(const AExpression: TGocciaExpression;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExpression := AExpression;
  end;

  { TGocciaVariableDeclaration }

  constructor TGocciaVariableDeclaration.Create(const AVariables: TArray<TGocciaVariableInfo>;
    const AIsConst: Boolean; const ALine, AColumn: Integer;
    const AIsVar: Boolean = False);
  begin
    inherited Create(ALine, AColumn);
    FVariables := AVariables;
    FIsConst := AIsConst;
    FIsVar := AIsVar;
  end;

  { TGocciaDestructuringDeclaration }

  constructor TGocciaDestructuringDeclaration.Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ALine, AColumn: Integer; const AIsVar: Boolean = False);
  begin
    inherited Create(ALine, AColumn);
    FPattern := APattern;
    FInitializer := AInitializer;
    FIsConst := AIsConst;
    FIsVar := AIsVar;
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
    const AMethods: TGocciaClassMethodMap;
    const AGetters: TGocciaGetterExpressionMap;
    const ASetters: TGocciaSetterExpressionMap;
    const AStaticProperties: TGocciaExpressionMap;
    const AInstanceProperties: TGocciaExpressionMap;
    const APrivateInstanceProperties: TGocciaExpressionMap;
    const APrivateMethods: TGocciaClassMethodMap = nil;
    const APrivateStaticProperties: TGocciaExpressionMap = nil);
  begin
    FName := AName;
    FSuperClass := ASuperClass;
    FMethods := AMethods;
    FGetters := AGetters;
    FSetters := ASetters;
    FStaticGetters := TGocciaGetterExpressionMap.Create;
    FStaticSetters := TGocciaSetterExpressionMap.Create;
    SetLength(FComputedStaticGetters, 0);
    SetLength(FComputedStaticSetters, 0);
    SetLength(FComputedInstanceGetters, 0);
    SetLength(FComputedInstanceSetters, 0);
    FStaticProperties := AStaticProperties;
    FInstanceProperties := AInstanceProperties;
    FPrivateInstanceProperties := APrivateInstanceProperties;

    if Assigned(APrivateMethods) then
      FPrivateMethods := APrivateMethods
    else
      FPrivateMethods := TGocciaClassMethodMap.Create;

    if Assigned(APrivateStaticProperties) then
      FPrivateStaticProperties := APrivateStaticProperties
    else
      FPrivateStaticProperties := TGocciaExpressionMap.Create;

    FInstancePropertyTypes := TStringStringMap.Create;
  end;

  destructor TGocciaClassDefinition.Destroy;
  var
    I: Integer;
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
    for I := 0 to High(FElements) do
      if (FElements[I].Kind = cekStaticBlock) and Assigned(FElements[I].StaticBlockBody) then
        FElements[I].StaticBlockBody.Free;
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

  { TGocciaForOfStatement }

  constructor TGocciaForOfStatement.Create(const AIsConst: Boolean; const ABindingName: string;
    const ABindingPattern: TGocciaDestructuringPattern; const AIterable: TGocciaExpression;
    const ABody: TGocciaStatement; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FIsConst := AIsConst;
    FBindingName := ABindingName;
    FBindingPattern := ABindingPattern;
    FIterable := AIterable;
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

  constructor TGocciaImportDeclaration.Create(const AImports: TStringStringMap;
    const AModulePath: string; const ALine, AColumn: Integer;
    const ANamespaceName: string);
  begin
    inherited Create(ALine, AColumn);
    FImports := AImports;
    FModulePath := AModulePath;
    FNamespaceName := ANamespaceName;
  end;

  { TGocciaExportDeclaration }

  constructor TGocciaExportDeclaration.Create(const AExportsTable: TStringStringMap;
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

  constructor TGocciaReExportDeclaration.Create(const AExportsTable: TStringStringMap;
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

  { Execute / Evaluate overrides }

  function TGocciaExpressionStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(Expression.Evaluate(AContext));
  end;

  function TGocciaVariableDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    I: Integer;
    Value: TGocciaValue;
    HasRealInit: Boolean;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
    // Function declarations are no-ops at runtime — already hoisted with their value
    if IsFunctionDeclaration then
      Exit;
    for I := 0 to Length(Variables) - 1 do
    begin
      Value := Variables[I].Initializer.Evaluate(AContext);
      if (Value is TGocciaFunctionValue) and (TGocciaFunctionValue(Value).Name = '') then
        TGocciaFunctionValue(Value).Name := Variables[I].Name
      else if Value is TGocciaClassValue then
        TGocciaClassValue(Value).SetInferredName(Variables[I].Name);
      if IsVar then
      begin
        HasRealInit := not ((Variables[I].Initializer is TGocciaLiteralExpression) and
          (Value is TGocciaUndefinedLiteralValue));
        AContext.Scope.DefineVariableBinding(Variables[I].Name, Value, HasRealInit);
      end
      else if IsConst then
        AContext.Scope.DefineFromToken(Variables[I].Name, Value, gttConst)
      else
        AContext.Scope.DefineFromToken(Variables[I].Name, Value, gttLet);
    end;
  end;

  function TGocciaDestructuringDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(EvaluateDestructuringDeclaration(Self, AContext));
  end;

  function TGocciaBlockStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateBlock(Self, AContext);
  end;

  function TGocciaIfStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateIf(Self, AContext);
  end;

  function TGocciaForStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  function TGocciaWhileStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  function TGocciaDoWhileStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  function TGocciaForOfStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateForOf(Self, AContext);
  end;

  function TGocciaForAwaitOfStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateForAwaitOf(Self, AContext);
  end;

  function TGocciaReturnStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    ReturnValue: TGocciaValue;
  begin
    if Assigned(Self.Value) then
    begin
      ReturnValue := Self.Value.Evaluate(AContext);
      if ReturnValue = nil then
        ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
      ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result := TGocciaControlFlow.Return(ReturnValue);
  end;

  function TGocciaThrowStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    ThrowValue: TGocciaValue;
  begin
    ThrowValue := Self.Value.Evaluate(AContext);
    raise TGocciaThrowValue.Create(ThrowValue);
  end;

  function TGocciaTryStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateTry(Self, AContext);
  end;

  function TGocciaSwitchStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateSwitch(Self, AContext);
  end;

  function TGocciaBreakStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Break;
  end;

  function TGocciaClassDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(EvaluateClass(Self, AContext));
  end;

  function TGocciaEnumDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(EvaluateEnumDeclaration(Self, AContext));
  end;

  function TGocciaExportEnumDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(EvaluateEnumDeclaration(Declaration, AContext));
  end;

  function TGocciaImportDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    Module: TGocciaModule;
    ImportPair: TStringStringMap.TKeyValuePair;
    NamespaceObject: TGocciaValue;
    Value: TGocciaValue;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
    Module := AContext.LoadModule(ModulePath, AContext.CurrentFilePath);
    for ImportPair in Imports do
    begin
      if Module.ExportsTable.TryGetValue(ImportPair.Value, Value) then
      begin
        AContext.Scope.DefineLexicalBinding(ImportPair.Key, Value, dtLet);
      end
      else
      begin
        AContext.OnError(Format('Module "%s" has no export named "%s"',
          [ModulePath, ImportPair.Value]), Line, Column);
      end;
    end;

    if NamespaceName <> '' then
    begin
      NamespaceObject := CreateModuleNamespaceObject(Module);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(NamespaceObject);
      try
        AContext.Scope.DefineLexicalBinding(NamespaceName, NamespaceObject,
          dtConst);
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(NamespaceObject);
      end;
    end;
  end;

  function TGocciaExportDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  function TGocciaExportVariableDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := Declaration.Execute(AContext);
  end;

  function TGocciaReExportDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  function TGocciaEmptyStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  function TGocciaClassMethod.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
  begin
    Result := EvaluateClassMethod(Self, AContext);
  end;

  function TGocciaClassExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
  begin
    Result := EvaluateClassExpression(Self, AContext);
  end;

  { TGocciaUsingDeclaration }

  constructor TGocciaUsingDeclaration.Create(const AVariables: TArray<TGocciaVariableInfo>;
    const AIsAwait: Boolean; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FVariables := AVariables;
    FIsAwait := AIsAwait;
  end;

  function TGocciaUsingDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateUsingDeclaration(Self, AContext);
  end;

end.
