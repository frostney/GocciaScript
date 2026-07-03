unit Goccia.AST.Statements;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.ControlFlow,
  Goccia.Evaluator.Context,
  Goccia.Modules,
  Goccia.Values.Primitives;

const
  GOCCIA_DEFAULT_EXPORT_BINDING = '*default*';

type
  TGocciaVariableInfo = record
    Name: string;
    Initializer: TGocciaExpression;
    HasInitializer: Boolean;
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
  public
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      const AIsConst: Boolean; const ALine, AColumn: Integer;
      const AIsVar: Boolean = False);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar;
  end;

  TGocciaFunctionDeclaration = class(TGocciaStatement)
  private
    FName: string;
    FFunctionExpression: TGocciaFunctionExpression;
  public
    constructor Create(const AName: string;
      const AFunctionExpression: TGocciaFunctionExpression;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Name: string read FName;
    property FunctionExpression: TGocciaFunctionExpression read FFunctionExpression;
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

  TGocciaWithStatement = class(TGocciaStatement)
  private
    FObjectExpression: TGocciaExpression;
    FBody: TGocciaStatement;
  public
    constructor Create(const AObjectExpression: TGocciaExpression;
      const ABody: TGocciaStatement; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property ObjectExpression: TGocciaExpression read FObjectExpression;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaForOfStatement = class(TGocciaStatement)
  private
    FIsConst: Boolean;
    FIsVar: Boolean;
    FBindingName: string;
    FBindingPattern: TGocciaDestructuringPattern;
    FAssignmentTarget: TGocciaDestructuringPattern;
    FMatchPattern: TGocciaMatchPattern;
    FIterable: TGocciaExpression;
    FBody: TGocciaStatement;
    FIsUsing: Boolean;
    FIsAwaitUsing: Boolean;
  public
    constructor Create(const AIsConst: Boolean; const ABindingName: string;
      const ABindingPattern: TGocciaDestructuringPattern; const AIterable: TGocciaExpression;
      const ABody: TGocciaStatement; const ALine, AColumn: Integer;
      const AMatchPattern: TGocciaMatchPattern = nil;
      const AAssignmentTarget: TGocciaDestructuringPattern = nil;
      const AIsUsing: Boolean = False;
      const AIsAwaitUsing: Boolean = False);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar write FIsVar;
    property BindingName: string read FBindingName;
    property BindingPattern: TGocciaDestructuringPattern read FBindingPattern;
    property AssignmentTarget: TGocciaDestructuringPattern read FAssignmentTarget;
    property MatchPattern: TGocciaMatchPattern read FMatchPattern;
    property Iterable: TGocciaExpression read FIterable;
    property Body: TGocciaStatement read FBody;
    property IsUsing: Boolean read FIsUsing;
    property IsAwaitUsing: Boolean read FIsAwaitUsing;
  end;

  TGocciaForInStatement = class(TGocciaStatement)
  private
    FIsConst: Boolean;
    FIsVar: Boolean;
    FBindingName: string;
    FBindingPattern: TGocciaDestructuringPattern;
    FAssignmentTarget: TGocciaDestructuringPattern;
    FObjectExpression: TGocciaExpression;
    FBody: TGocciaStatement;
  public
    constructor Create(const AIsConst: Boolean; const ABindingName: string;
      const ABindingPattern: TGocciaDestructuringPattern;
      const AObjectExpression: TGocciaExpression; const ABody: TGocciaStatement;
      const ALine, AColumn: Integer;
      const AAssignmentTarget: TGocciaDestructuringPattern = nil);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar write FIsVar;
    property BindingName: string read FBindingName;
    property BindingPattern: TGocciaDestructuringPattern read FBindingPattern;
    property AssignmentTarget: TGocciaDestructuringPattern read FAssignmentTarget;
    property ObjectExpression: TGocciaExpression read FObjectExpression;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaForAwaitOfStatement = class(TGocciaForOfStatement)
  public
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
  end;

  TGocciaReturnStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
    FHasExpression: Boolean;
  public
    constructor Create(const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property HasExpression: Boolean read FHasExpression;
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
    FCatchPattern: TGocciaMatchPattern;
    FCatchBindingPattern: TGocciaDestructuringPattern;
  public
    constructor Create(const ABlock: TGocciaBlockStatement; const ACatchParam: string;
      const ACatchBlock, AFinallyBlock: TGocciaBlockStatement; const ALine, AColumn: Integer;
      const ACatchPattern: TGocciaMatchPattern = nil;
      const ACatchBindingPattern: TGocciaDestructuringPattern = nil);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Block: TGocciaBlockStatement read FBlock;
    property CatchParam: string read FCatchParam;
    property CatchBlock: TGocciaBlockStatement read FCatchBlock;
    property FinallyBlock: TGocciaBlockStatement read FFinallyBlock;
    property CatchParamType: string read FCatchParamType write FCatchParamType;
    property CatchPattern: TGocciaMatchPattern read FCatchPattern;
    property CatchBindingPattern: TGocciaDestructuringPattern read FCatchBindingPattern;
  end;

  // Is this not a statement?
  TGocciaClassMethod = class(TGocciaExpression)
  private
    FName: string;
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FIsStatic: Boolean;
    FIsAsync: Boolean;
    FIsGenerator: Boolean;
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
    property IsGenerator: Boolean read FIsGenerator write FIsGenerator;
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
    IsGenerator: Boolean;
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
    IsComputed: Boolean;
    ElementIndex: Integer;
    ComputedKeyExpression: TGocciaExpression;
    FieldInitializer: TGocciaExpression;
  end;

  // Shared class definition structure
  TGocciaClassDefinition = class
  public
    FName: string;
    FSuperClass: string;
    FSuperClassExpression: TGocciaExpression;
    FMethods: TGocciaClassMethodMap;
    FStaticMethods: TGocciaClassMethodMap;
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
    FSourceText: string;

    constructor Create(const AName, ASuperClass: string;
      const AMethods: TGocciaClassMethodMap;
      const AStaticMethods: TGocciaClassMethodMap;
      const AGetters: TGocciaGetterExpressionMap;
      const ASetters: TGocciaSetterExpressionMap;
      const AStaticProperties: TGocciaExpressionMap;
      const AInstanceProperties: TGocciaExpressionMap;
      const APrivateInstanceProperties: TGocciaExpressionMap;
      const APrivateMethods: TGocciaClassMethodMap = nil;
      const APrivateStaticProperties: TGocciaExpressionMap = nil;
      const ASuperClassExpression: TGocciaExpression = nil);
    destructor Destroy; override;
    property Name: string read FName;
    property SuperClass: string read FSuperClass;
    property SourceText: string read FSourceText write FSourceText;
    property SuperClassExpression: TGocciaExpression read FSuperClassExpression;
    property Methods: TGocciaClassMethodMap read FMethods;
    property StaticMethods: TGocciaClassMethodMap read FStaticMethods;
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
    FAttributeType: string;
    FModulePath: string;
    FNamespaceName: string;
    FPhase: TGocciaImportCallPhase;
  public
    constructor Create(const AImports: TStringStringMap;
      const AModulePath: string; const ALine, AColumn: Integer;
      const ANamespaceName: string = '';
      const APhase: TGocciaImportCallPhase = icpEvaluation;
      const AAttributeType: string = '');
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Imports: TStringStringMap read FImports;
    property AttributeType: string read FAttributeType;
    property ModulePath: string read FModulePath;
    property NamespaceName: string read FNamespaceName;
    property Phase: TGocciaImportCallPhase read FPhase;
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

  TGocciaExportDefaultDeclaration = class(TGocciaStatement)
  private
    FExpression: TGocciaExpression;
    FIsDirectDeclaration: Boolean;
    FLocalName: string;
  public
    constructor Create(const AExpression: TGocciaExpression;
      const ALocalName: string; const AIsDirectDeclaration: Boolean;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Expression: TGocciaExpression read FExpression;
    property IsDirectDeclaration: Boolean read FIsDirectDeclaration;
    property LocalName: string read FLocalName;
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

  TGocciaExportDestructuringDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaDestructuringDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaDestructuringDeclaration;
      const ALine, AColumn: Integer);
    destructor Destroy; override;
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Declaration: TGocciaDestructuringDeclaration read FDeclaration;
  end;

  TGocciaExportFunctionDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaFunctionDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaFunctionDeclaration;
      const ALine, AColumn: Integer);
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Declaration: TGocciaFunctionDeclaration read FDeclaration;
  end;

  TGocciaExportClassDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaClassDeclaration;
  public
    constructor Create(const ADeclaration: TGocciaClassDeclaration;
      const ALine, AColumn: Integer);
    destructor Destroy; override;
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property Declaration: TGocciaClassDeclaration read FDeclaration;
  end;

  TGocciaReExportDeclaration = class(TGocciaStatement)
  private
    FAttributeType: string;
    FExportsTable: TStringStringMap; // exported name -> source name
    FIsStarExport: Boolean;
    FModulePath: string;
    FNamespaceName: string;
  public
    constructor Create(const AExportsTable: TStringStringMap;
      const AModulePath: string; const ALine, AColumn: Integer;
      const AIsStarExport: Boolean = False;
      const ANamespaceName: string = '';
      const AAttributeType: string = '');
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property AttributeType: string read FAttributeType;
    property ExportsTable: TStringStringMap read FExportsTable;
    property IsStarExport: Boolean read FIsStarExport;
    property ModulePath: string read FModulePath;
    property NamespaceName: string read FNamespaceName;
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
  private
    FTargetLabel: string;
  public
    constructor Create(const ALine, AColumn: Integer;
      const ATargetLabel: string = '');
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property TargetLabel: string read FTargetLabel;
  end;

  TGocciaContinueStatement = class(TGocciaStatement)
  private
    FTargetLabel: string;
  public
    constructor Create(const ALine, AColumn: Integer;
      const ATargetLabel: string = '');
    function Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow; override;
    property TargetLabel: string read FTargetLabel;
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

function HasUseStrictDirective(const ABody: TGocciaASTNode): Boolean; overload;
function HasUseStrictDirective(const AProgram: TGocciaProgram): Boolean; overload;

implementation

uses
  SysUtils,

  Goccia.Constants,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.GarbageCollector,
  Goccia.Generator.Continuation,
  Goccia.Keywords.Reserved,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Token,
  Goccia.Types.Enforcement,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.ToObject;

function CreateModuleNamespaceObject(const AModule: TGocciaModule): TGocciaValue;
begin
  Result := AModule.GetNamespaceObject;
end;

function DirectiveStatementIsUseStrict(const AStatement: TGocciaASTNode;
  out AContinuesDirectivePrologue: Boolean): Boolean;
var
  LiteralExpression: TGocciaLiteralExpression;
  Expression: TGocciaExpression;
  Literal: TGocciaValue;
  SourceText: string;
begin
  Result := False;
  AContinuesDirectivePrologue := False;

  if not (AStatement is TGocciaExpressionStatement) then
    Exit;

  Expression := TGocciaExpressionStatement(AStatement).Expression;
  if not (Expression is TGocciaLiteralExpression) then
    Exit;

  LiteralExpression := TGocciaLiteralExpression(Expression);
  Literal := LiteralExpression.Value;
  if not (Literal is TGocciaStringLiteralValue) then
    Exit;

  AContinuesDirectivePrologue := True;
  SourceText := LiteralExpression.SourceText;
  Result := (SourceText = #34 + USE_STRICT_DIRECTIVE + #34) or
    (SourceText = #39 + USE_STRICT_DIRECTIVE + #39);
end;

function IsAnonymousFunctionNameInitializer(
  const AExpression: TGocciaExpression): Boolean;
begin
  Result := Assigned(AExpression) and
    ((AExpression is TGocciaArrowFunctionExpression) or
     ((AExpression is TGocciaFunctionExpression) and
      (TGocciaFunctionExpression(AExpression).Name = '')) or
     ((AExpression is TGocciaClassExpression) and
      (TGocciaClassExpression(AExpression).ClassDefinition.Name = '')));
end;

procedure ApplyInferredNameForVariableInitializer(
  const AInitializer: TGocciaExpression; const AValue: TGocciaValue;
  const AName: string);
begin
  if not IsAnonymousFunctionNameInitializer(AInitializer) then
    Exit;

  if AValue is TGocciaFunctionValue then
    TGocciaFunctionValue(AValue).SetInferredName(AName)
  else if AValue is TGocciaClassValue then
    TGocciaClassValue(AValue).SetInferredName(AName);
end;

// ES2026 §11.2.2 Directive Prologues and the Use Strict Directive
function HasUseStrictDirective(const ABody: TGocciaASTNode): Boolean;
var
  Block: TGocciaBlockStatement;
  I: Integer;
  ContinuesDirectivePrologue: Boolean;
begin
  Result := False;
  if not (ABody is TGocciaBlockStatement) then
    Exit;

  Block := TGocciaBlockStatement(ABody);
  for I := 0 to Block.Nodes.Count - 1 do
  begin
    if DirectiveStatementIsUseStrict(Block.Nodes[I],
      ContinuesDirectivePrologue) then
      Exit(True);
    if not ContinuesDirectivePrologue then
      Exit;
  end;
end;

function HasUseStrictDirective(const AProgram: TGocciaProgram): Boolean;
var
  I: Integer;
  ContinuesDirectivePrologue: Boolean;
begin
  Result := False;
  if not Assigned(AProgram) then
    Exit;

  for I := 0 to AProgram.Body.Count - 1 do
  begin
    if DirectiveStatementIsUseStrict(AProgram.Body[I],
      ContinuesDirectivePrologue) then
      Exit(True);
    if not ContinuesDirectivePrologue then
      Exit;
  end;
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

  { TGocciaFunctionDeclaration }

  constructor TGocciaFunctionDeclaration.Create(const AName: string;
    const AFunctionExpression: TGocciaFunctionExpression;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FFunctionExpression := AFunctionExpression;
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
    const AStaticMethods: TGocciaClassMethodMap;
    const AGetters: TGocciaGetterExpressionMap;
    const ASetters: TGocciaSetterExpressionMap;
    const AStaticProperties: TGocciaExpressionMap;
    const AInstanceProperties: TGocciaExpressionMap;
    const APrivateInstanceProperties: TGocciaExpressionMap;
    const APrivateMethods: TGocciaClassMethodMap = nil;
    const APrivateStaticProperties: TGocciaExpressionMap = nil;
    const ASuperClassExpression: TGocciaExpression = nil);
  begin
    FName := AName;
    FSuperClass := ASuperClass;
    FSuperClassExpression := ASuperClassExpression;
    FMethods := AMethods;
    FStaticMethods := AStaticMethods;
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
    FStaticMethods.Free;
    FGetters.Free;
    FSetters.Free;
    FStaticGetters.Free;
    FStaticSetters.Free;
    FStaticProperties.Free;
    FInstanceProperties.Free;
    FPrivateInstanceProperties.Free;
    FSuperClassExpression.Free;
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
    const ABody: TGocciaStatement; const ALine, AColumn: Integer;
    const AMatchPattern: TGocciaMatchPattern = nil;
    const AAssignmentTarget: TGocciaDestructuringPattern = nil;
    const AIsUsing: Boolean = False;
    const AIsAwaitUsing: Boolean = False);
begin
  inherited Create(ALine, AColumn);
  FIsConst := AIsConst;
  FBindingName := ABindingName;
  FBindingPattern := ABindingPattern;
  FAssignmentTarget := AAssignmentTarget;
  FMatchPattern := AMatchPattern;
  FIterable := AIterable;
  FBody := ABody;
  FIsUsing := AIsUsing;
  FIsAwaitUsing := AIsAwaitUsing;
end;

{ TGocciaForInStatement }

constructor TGocciaForInStatement.Create(const AIsConst: Boolean;
  const ABindingName: string;
  const ABindingPattern: TGocciaDestructuringPattern;
  const AObjectExpression: TGocciaExpression; const ABody: TGocciaStatement;
  const ALine, AColumn: Integer;
  const AAssignmentTarget: TGocciaDestructuringPattern = nil);
begin
  inherited Create(ALine, AColumn);
  FIsConst := AIsConst;
  FBindingName := ABindingName;
  FBindingPattern := ABindingPattern;
  FAssignmentTarget := AAssignmentTarget;
  FObjectExpression := AObjectExpression;
  FBody := ABody;
end;

  { TGocciaReturnStatement }

  constructor TGocciaReturnStatement.Create(const AValue: TGocciaExpression;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FHasExpression := AValue <> nil;
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
    const ALine, AColumn: Integer; const ACatchPattern: TGocciaMatchPattern = nil;
    const ACatchBindingPattern: TGocciaDestructuringPattern = nil);
begin
  inherited Create(ALine, AColumn);
  FBlock := ABlock;
  FCatchParam := ACatchParam;
  FCatchBlock := ACatchBlock;
  FFinallyBlock := AFinallyBlock;
  FCatchPattern := ACatchPattern;
  FCatchBindingPattern := ACatchBindingPattern;
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
    const ANamespaceName: string; const APhase: TGocciaImportCallPhase;
    const AAttributeType: string);
  begin
    inherited Create(ALine, AColumn);
    FImports := AImports;
    FAttributeType := AAttributeType;
    FModulePath := AModulePath;
    FNamespaceName := ANamespaceName;
    FPhase := APhase;
  end;

  { TGocciaExportDeclaration }

  constructor TGocciaExportDeclaration.Create(const AExportsTable: TStringStringMap;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExportsTable := AExportsTable;
  end;

  { TGocciaExportDefaultDeclaration }

  constructor TGocciaExportDefaultDeclaration.Create(
    const AExpression: TGocciaExpression; const ALocalName: string;
    const AIsDirectDeclaration: Boolean; const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExpression := AExpression;
    FIsDirectDeclaration := AIsDirectDeclaration;
    FLocalName := ALocalName;
  end;

  { TGocciaExportVariableDeclaration }

  constructor TGocciaExportVariableDeclaration.Create(const ADeclaration: TGocciaVariableDeclaration;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDeclaration := ADeclaration;
  end;

  { TGocciaExportDestructuringDeclaration }

  constructor TGocciaExportDestructuringDeclaration.Create(
    const ADeclaration: TGocciaDestructuringDeclaration;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDeclaration := ADeclaration;
  end;

  destructor TGocciaExportDestructuringDeclaration.Destroy;
  begin
    FDeclaration.Free;
    inherited;
  end;

  { TGocciaExportFunctionDeclaration }

  constructor TGocciaExportFunctionDeclaration.Create(
    const ADeclaration: TGocciaFunctionDeclaration;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDeclaration := ADeclaration;
  end;

  { TGocciaExportClassDeclaration }

  constructor TGocciaExportClassDeclaration.Create(
    const ADeclaration: TGocciaClassDeclaration;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FDeclaration := ADeclaration;
  end;

  destructor TGocciaExportClassDeclaration.Destroy;
  begin
    FDeclaration.Free;
    inherited;
  end;

  { TGocciaReExportDeclaration }

  constructor TGocciaReExportDeclaration.Create(const AExportsTable: TStringStringMap;
    const AModulePath: string; const ALine, AColumn: Integer;
    const AIsStarExport: Boolean = False; const ANamespaceName: string = '';
    const AAttributeType: string = '');
  begin
    inherited Create(ALine, AColumn);
    FAttributeType := AAttributeType;
    FExportsTable := AExportsTable;
    FIsStarExport := AIsStarExport;
    FModulePath := AModulePath;
    FNamespaceName := ANamespaceName;
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

  { TGocciaWithStatement }

  constructor TGocciaWithStatement.Create(
    const AObjectExpression: TGocciaExpression; const ABody: TGocciaStatement;
    const ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FObjectExpression := AObjectExpression;
    FBody := ABody;
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

  constructor TGocciaBreakStatement.Create(const ALine, AColumn: Integer;
    const ATargetLabel: string);
  begin
    inherited Create(ALine, AColumn);
    FTargetLabel := ATargetLabel;
  end;

  { TGocciaContinueStatement }

  constructor TGocciaContinueStatement.Create(const ALine, AColumn: Integer;
    const ATargetLabel: string);
  begin
    inherited Create(ALine, AColumn);
    FTargetLabel := ATargetLabel;
  end;

  { Execute / Evaluate overrides }

  function TGocciaExpressionStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Normal(EvaluateExpression(Expression, AContext));
  end;

  function TGocciaVariableDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    I: Integer;
    Value: TGocciaValue;
    ResolvedObjectBinding: TGocciaObjectValue;
    ResolvedScopeBinding: TGocciaScope;
    HasRealStrictInit: Boolean;
    AnnotationType, TypeHint: TGocciaLocalType;
    Continuation: TGocciaGeneratorContinuation;
  begin
    Result := TGocciaControlFlow.Empty;
    Continuation := CurrentGeneratorContinuation;
    if Assigned(Continuation) then
      I := Continuation.GetStatementIndex(Self)
    else
      I := 0;
    while I < Length(Variables) do
    begin
      ResolvedObjectBinding := nil;
      ResolvedScopeBinding := nil;
      if IsVar and Variables[I].HasInitializer then
      begin
        if not AContext.InEvalCode then
          AContext.Scope.DefineVariableBinding(Variables[I].Name,
            TGocciaUndefinedLiteralValue.UndefinedValue, False);
        AContext.Scope.ResolveAssignmentTarget(Variables[I].Name,
          ResolvedObjectBinding, ResolvedScopeBinding);
      end;
      try
        Value := EvaluateExpression(Variables[I].Initializer, AContext);
      except
        on E: EGocciaGeneratorYield do
        begin
          if Assigned(Continuation) then
            Continuation.SaveStatementIndex(Self, I);
          raise;
        end;
        else
        begin
          if Assigned(Continuation) then
            Continuation.ClearStatementIndex(Self);
          raise;
        end;
      end;
      ApplyInferredNameForVariableInitializer(Variables[I].Initializer,
        Value, Variables[I].Name);

      { Strict-types enforcement: when --strict-types is enabled, an
        explicit type annotation (e.g. `let x: number = ...`) is
        enforced on the initial value, and an inferred type from a
        primitive-literal initializer locks the binding's type.  An
        implicit-undefined initializer (parser-emitted for `let x;`)
        records the type hint without enforcing on the placeholder
        value, matching the bytecode compiler's behaviour. }
      TypeHint := sltUntyped;
      if AContext.StrictTypes then
      begin
        AnnotationType := TypeAnnotationToLocalType(Variables[I].TypeAnnotation);
        HasRealStrictInit := Assigned(Variables[I].Initializer)
          and not ((Variables[I].Initializer is TGocciaLiteralExpression)
            and (TGocciaLiteralExpression(Variables[I].Initializer).Value
              is TGocciaUndefinedLiteralValue));

        if AnnotationType <> sltUntyped then
          TypeHint := AnnotationType
        else if (Variables[I].TypeAnnotation = '') and HasRealStrictInit then
          TypeHint := InferLocalType(Variables[I].Initializer);

        if (TypeHint <> sltUntyped) and HasRealStrictInit then
          EnforceStrictType(Value, TypeHint);
      end;

      if IsVar then
      begin
        if Variables[I].HasInitializer then
        begin
          if Assigned(ResolvedObjectBinding) then
          begin
            if AContext.NonStrictMode then
              ResolvedObjectBinding.AssignPropertyWithReceiver(
                Variables[I].Name, Value, ResolvedObjectBinding)
            else
              ResolvedObjectBinding.AssignProperty(Variables[I].Name, Value);
          end
          else if Assigned(ResolvedScopeBinding) then
            ResolvedScopeBinding.AssignBinding(Variables[I].Name, Value, Line,
              Column, AContext.NonStrictMode)
          else
            AContext.Scope.AssignBinding(Variables[I].Name, Value, Line,
              Column, AContext.NonStrictMode);
        end
        else
        begin
          if not AContext.InEvalCode then
            AContext.Scope.DefineVariableBinding(Variables[I].Name, Value,
              Variables[I].HasInitializer);
        end;
      end
      else if IsConst then
        AContext.Scope.DefineFromToken(Variables[I].Name, Value, gttConst)
      else
        AContext.Scope.DefineFromToken(Variables[I].Name, Value, gttLet);

      if TypeHint <> sltUntyped then
      begin
        // var bindings are hoisted to the nearest function/module/global scope
        // by DefineVariableBinding; record the type hint on that scope so the
        // hint sticks to the actual binding rather than silently no-oping when
        // SetOwnBindingTypeHint cannot find the name on AContext.Scope.
        if IsVar then
          AContext.Scope.FindFunctionOrModuleScope.SetOwnBindingTypeHint(
            Variables[I].Name, TypeHint)
        else
          AContext.Scope.SetOwnBindingTypeHint(Variables[I].Name, TypeHint);
      end;
      if Assigned(Continuation) then
        Continuation.SaveStatementIndex(Self, I + 1);
      Inc(I);
    end;
    if Assigned(Continuation) then
      Continuation.ClearStatementIndex(Self);
  end;

  function TGocciaDestructuringDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    EvaluateDestructuringDeclaration(Self, AContext);
    Result := TGocciaControlFlow.Empty;
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
    Result := EvaluateFor(Self, AContext);
  end;

  function TGocciaWhileStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateWhile(Self, AContext);
  end;

  function TGocciaDoWhileStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateDoWhile(Self, AContext);
  end;

  function TGocciaWithStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    WithContext: TGocciaEvaluationContext;
    WithObject: TGocciaObjectValue;
    WithScope: TGocciaWithScope;
    GC: TGarbageCollector;
  begin
    if not AContext.NonStrictMode then
      raise TGocciaSyntaxError.Create(
        '''with'' statements are not allowed in strict mode',
        Line, Column, AContext.CurrentFilePath, nil);

    WithObject := ToObject(EvaluateExpression(ObjectExpression, AContext));
    WithScope := TGocciaWithScope.Create(AContext.Scope, WithObject);
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.AddTempRoot(WithScope);
    try
      WithContext := AContext;
      WithContext.Scope := WithScope;
      Result := EvaluateStatement(Body, WithContext).UpdateEmpty(
        TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      if Assigned(GC) then
        GC.RemoveTempRoot(WithScope);
    end;
  end;

  function TGocciaForOfStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateForOf(Self, AContext);
  end;

  function TGocciaForInStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateForIn(Self, AContext);
  end;

  function TGocciaForAwaitOfStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := EvaluateForAwaitOf(Self, AContext);
  end;

  function TGocciaReturnStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    ReturnValue: TGocciaValue;
  begin
    if Self.HasExpression and Assigned(Self.Value) then
    begin
      ReturnValue := EvaluateExpression(Self.Value, AContext);
      if ReturnValue = nil then
        ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
      ReturnValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    Result := TGocciaControlFlow.Return(ReturnValue, Self.HasExpression);
  end;

  function TGocciaThrowStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    ThrowValue: TGocciaValue;
  begin
    ThrowValue := EvaluateExpression(Self.Value, AContext);
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
    Result := TGocciaControlFlow.Break(FTargetLabel);
  end;

  function TGocciaContinueStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Continue(FTargetLabel);
  end;

  function TGocciaClassDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    EvaluateClass(Self, AContext);
    Result := TGocciaControlFlow.Empty;
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
    DeferredLoader: TLoadDeferredModuleCallback;
    Module: TGocciaModule;
    ImportPair: TStringStringMap.TKeyValuePair;
    NamespaceObject: TGocciaValue;
    SourceLoader: TLoadModuleSourceCallback;
    Value: TGocciaValue;

    procedure BindImportValue(const AName: string; const AValue: TGocciaValue);
    begin
      if AContext.Scope.ContainsOwnLexicalBinding(AName) then
        AContext.Scope.ForceUpdateBinding(AName, AValue)
      else
        AContext.Scope.DefineLexicalBinding(AName, AValue, dtConst);
    end;
  begin
    Result := TGocciaControlFlow.Empty;

    if Phase = icpDefer then
    begin
      if NamespaceName = '' then
      begin
        AContext.OnError('Deferred imports require a namespace binding',
          Line, Column);
        Exit;
      end;
      DeferredLoader := AContext.LoadDeferredModule;
      if (not Assigned(DeferredLoader)) and Assigned(AContext.Scope) then
        DeferredLoader := AContext.Scope.LoadDeferredModule;
      if not Assigned(DeferredLoader) then
      begin
        AContext.OnError('Deferred module loader is not available.',
          Line, Column);
        Exit;
      end;

      NamespaceObject := DeferredLoader(EncodeImportSpecifierAttribute(
        ModulePath, AttributeType), AContext.CurrentFilePath);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(NamespaceObject);
      try
        BindImportValue(NamespaceName, NamespaceObject);
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(NamespaceObject);
      end;
      Exit;
    end;

    if Phase = icpSource then
    begin
      SourceLoader := AContext.LoadModuleSource;
      if (not Assigned(SourceLoader)) and Assigned(AContext.Scope) then
        SourceLoader := AContext.Scope.LoadModuleSource;
      if not Assigned(SourceLoader) then
      begin
        AContext.OnError('Module source loader is not available.',
          Line, Column);
        Exit;
      end;

      Value := SourceLoader(EncodeImportSpecifierAttribute(
        ModulePath, AttributeType), AContext.CurrentFilePath);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(Value);
      try
        for ImportPair in Imports do
          BindImportValue(ImportPair.Key, Value);
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(Value);
      end;
      Exit;
    end;

    Module := AContext.LoadModule(EncodeImportSpecifierAttribute(ModulePath,
      AttributeType), AContext.CurrentFilePath);
    for ImportPair in Imports do
    begin
      if Module.TryGetExportValue(ImportPair.Value, Value) then
        BindImportValue(ImportPair.Key, Value)
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
        BindImportValue(NamespaceName, NamespaceObject);
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(NamespaceObject);
      end;
    end;
  end;

  function TGocciaExportDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Empty;
  end;

  function TGocciaFunctionDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    // Function declarations are no-ops at runtime: their bindings and function
    // values are installed by HoistFunctionDeclarations before statement execution.
    Result := TGocciaControlFlow.Empty;
  end;

  function TGocciaExportDefaultDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  var
    DeclarationType: TGocciaDeclarationType;
    Value: TGocciaValue;
  begin
    if IsDirectDeclaration and (Expression is TGocciaFunctionExpression) and
       (LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
       AContext.Scope.ContainsOwnLexicalBinding(LocalName) then
      Exit(TGocciaControlFlow.Empty);

    Value := EvaluateExpression(Expression, AContext);
    if ((Expression is TGocciaArrowFunctionExpression) or
       ((Expression is TGocciaFunctionExpression) and
       (TGocciaFunctionExpression(Expression).Name = ''))) and
       (Value is TGocciaFunctionValue) then
      TGocciaFunctionValue(Value).SetInferredName(KEYWORD_DEFAULT)
    else if (Expression is TGocciaClassExpression) and
            (TGocciaClassExpression(Expression).ClassDefinition.Name = '') and
            (Value is TGocciaClassValue) then
      TGocciaClassValue(Value).SetInferredName(KEYWORD_DEFAULT);

    if AContext.Scope.ContainsOwnLexicalBinding(LocalName) then
      AContext.Scope.ForceUpdateBinding(LocalName, Value)
    else
    begin
      if IsDirectDeclaration and (LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
         (((Expression is TGocciaFunctionExpression) and
         (TGocciaFunctionExpression(Expression).Name = LocalName)) or
         ((Expression is TGocciaClassExpression) and
         (TGocciaClassExpression(Expression).ClassDefinition.Name =
         LocalName))) then
        DeclarationType := dtLet
      else
        DeclarationType := dtConst;
      AContext.Scope.DefineLexicalBinding(LocalName, Value, DeclarationType,
        False, Line, Column);
    end;
    Result := TGocciaControlFlow.Empty;
  end;

  function TGocciaExportVariableDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := Declaration.Execute(AContext);
  end;

  function TGocciaExportDestructuringDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := Declaration.Execute(AContext);
  end;

  function TGocciaExportFunctionDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := Declaration.Execute(AContext);
  end;

  function TGocciaExportClassDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := Declaration.Execute(AContext);
  end;

  function TGocciaReExportDeclaration.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    if Assigned(AContext.LoadModule) then
      AContext.LoadModule(EncodeImportSpecifierAttribute(ModulePath,
        AttributeType), AContext.CurrentFilePath);
    Result := TGocciaControlFlow.Empty;
  end;

  function TGocciaEmptyStatement.Execute(const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
  begin
    Result := TGocciaControlFlow.Empty;
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
