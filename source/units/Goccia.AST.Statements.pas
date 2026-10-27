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
  Goccia.Modules,
  Goccia.SourceSpan,
  Goccia.Values.Primitives;

const
  GOCCIA_DEFAULT_EXPORT_BINDING = '*default*';

type
  TGocciaVariableInfo = record
    Name: string;
    Pattern: TGocciaDestructuringPattern;
    Initializer: TGocciaExpression;
    HasInitializer: Boolean;
    IsPattern: Boolean;
    TypeAnnotation: string;
  end;
  TGocciaExpressionStatement = class(TGocciaStatement)
  private
    FExpression: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExpression: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaVariableDeclaration = class(TGocciaStatement)
  private
    FVariables: TArray<TGocciaVariableInfo>;
    FIsConst: Boolean;
    FIsVar: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      const AIsConst: Boolean; const ASpan: TGocciaSourceSpan;
      const AIsVar: Boolean = False);
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsConst: Boolean read FIsConst;
    property IsVar: Boolean read FIsVar;
  end;

  TGocciaFunctionDeclaration = class(TGocciaStatement)
  private
    FName: string;
    FFunctionExpression: TGocciaFunctionExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AName: string;
      const AFunctionExpression: TGocciaFunctionExpression;
      const ASpan: TGocciaSourceSpan);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ASpan: TGocciaSourceSpan; const AIsVar: Boolean = False);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ANodes: TObjectList<TGocciaASTNode>;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Nodes: TObjectList<TGocciaASTNode> read FNodes;
  end;

  TGocciaIfStatement = class(TGocciaStatement)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaStatement;
    FAlternate: TGocciaStatement;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ACondition: TGocciaExpression;
      const AConsequent, AAlternate: TGocciaStatement; const ASpan: TGocciaSourceSpan);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AInit: TGocciaStatement; const ACondition: TGocciaExpression;
      const AUpdate: TGocciaExpression; const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ACondition: TGocciaExpression; const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan);
    property Condition: TGocciaExpression read FCondition;
    property Body: TGocciaStatement read FBody;
  end;

  TGocciaDoWhileStatement = class(TGocciaStatement)
  private
    FBody: TGocciaStatement;        // Loop body (executed first)
    FCondition: TGocciaExpression;  // Loop condition (checked after body)
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ABody: TGocciaStatement; const ACondition: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Body: TGocciaStatement read FBody;
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaWithStatement = class(TGocciaStatement)
  private
    FObjectExpression: TGocciaExpression;
    FBody: TGocciaStatement;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObjectExpression: TGocciaExpression;
      const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AIsConst: Boolean; const ABindingName: string;
      const ABindingPattern: TGocciaDestructuringPattern; const AIterable: TGocciaExpression;
      const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan;
      const AMatchPattern: TGocciaMatchPattern = nil;
      const AAssignmentTarget: TGocciaDestructuringPattern = nil;
      const AIsUsing: Boolean = False;
      const AIsAwaitUsing: Boolean = False);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AIsConst: Boolean; const ABindingName: string;
      const ABindingPattern: TGocciaDestructuringPattern;
      const AObjectExpression: TGocciaExpression; const ABody: TGocciaStatement;
      const ASpan: TGocciaSourceSpan;
      const AAssignmentTarget: TGocciaDestructuringPattern = nil);
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
  end;

  TGocciaReturnStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
    FHasExpression: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property HasExpression: Boolean read FHasExpression;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaThrowStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ABlock: TGocciaBlockStatement; const ACatchParam: string;
      const ACatchBlock, AFinallyBlock: TGocciaBlockStatement; const ASpan: TGocciaSourceSpan;
      const ACatchPattern: TGocciaMatchPattern = nil;
      const ACatchBindingPattern: TGocciaDestructuringPattern = nil);
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
    FDirectEvalUsage: TGocciaDirectEvalUsage;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AName: string; const AParameters: TGocciaParameterArray;
      const ABody: TGocciaASTNode; const AIsStatic: Boolean; const ASpan: TGocciaSourceSpan);
    property Name: string read FName write FName;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property IsStatic: Boolean read FIsStatic;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property IsGenerator: Boolean read FIsGenerator write FIsGenerator;
    property ReturnType: string read FReturnType write FReturnType;
    property GenericParams: string read FGenericParams write FGenericParams;
    property SourceText: string read FSourceText write FSourceText;
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
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

  TGocciaComputedGetterEntryArray = array of TGocciaComputedGetterEntry;
  TGocciaComputedSetterEntryArray = array of TGocciaComputedSetterEntry;

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
    { cekAccessor only: the private name that holds the value. A private
      auto-accessor stores it under its own name; a public one under a name
      no source text can spell (see TGocciaParser.AutoAccessorStorageName). }
    AccessorStorageName: string;
    { The member's extent in the parsed text, from its first decorator to its
      last token. Equal offsets mean the parser did not record one. }
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TGocciaFieldOrderEntry = record
    Name: string;
    IsPrivate: Boolean;
    IsComputed: Boolean;
    ElementIndex: Integer;
    ComputedKeyExpression: TGocciaExpression;
    FieldInitializer: TGocciaExpression;
    { The entry stores an auto-accessor's value; ElementIndex names the
      accessor, whose own name is the one its initializer is named after. }
    IsAutoAccessorStorage: Boolean;
    { As TGocciaClassElement.StartOffset and EndOffset, for a field the
      element array does not hold (ElementIndex < 0). }
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TGocciaClassElementArray = array of TGocciaClassElement;
  TGocciaFieldOrderEntryArray = array of TGocciaFieldOrderEntry;

  // Shared class definition structure
  TGocciaClassDefinition = class(TGocciaASTObject)
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
    FComputedStaticGetters: TGocciaComputedGetterEntryArray;
    FComputedStaticSetters: TGocciaComputedSetterEntryArray;
    FComputedInstanceGetters: TGocciaComputedGetterEntryArray;
    FComputedInstanceSetters: TGocciaComputedSetterEntryArray;
    FStaticProperties: TGocciaExpressionMap;
    FInstanceProperties: TGocciaExpressionMap;
    FPrivateInstanceProperties: TGocciaExpressionMap;
    FPrivateStaticProperties: TGocciaExpressionMap;
    FPrivateMethods: TGocciaClassMethodMap;
    FGenericParams: string;
    FImplementsClause: string;
    FInstancePropertyTypes: TStringStringMap;
    FDecorators: TGocciaDecoratorList;
    FElements: TGocciaClassElementArray;
    FFieldOrder: TGocciaFieldOrderEntryArray;
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AClassDefinition: TGocciaClassDefinition; const ASpan: TGocciaSourceSpan);
    property ClassDefinition: TGocciaClassDefinition read FClassDefinition;
  end;

  TGocciaClassExpression = class(TGocciaExpression)
  private
    FClassDefinition: TGocciaClassDefinition;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AClassDefinition: TGocciaClassDefinition; const ASpan: TGocciaSourceSpan);
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
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AName: string; const AMembers: TArray<TGocciaEnumMember>;
      const ASpan: TGocciaSourceSpan);
    property Name: string read FName;
    property Members: TArray<TGocciaEnumMember> read FMembers;
  end;

  TGocciaExportEnumDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaEnumDeclaration;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ADeclaration: TGocciaEnumDeclaration;
      const ASpan: TGocciaSourceSpan);
    property Declaration: TGocciaEnumDeclaration read FDeclaration;
  end;

  // Modules
  TGocciaImportDeclaration = class(TGocciaStatement)
  private
    FModuleSpecifierStart: Integer;
    FModuleSpecifierEnd: Integer;
    FImports: TStringStringMap; // local name -> imported name
    FAttributeType: string;
    FDuplicateBinding: string;
    FModulePath: string;
    FNamespaceName: string;
    FPhase: TGocciaImportCallPhase;
  public
    { Where the module path's string literal is, in the parsed text. }
    procedure SetModuleSpecifierRange(const AStartOffset, AEndOffset: Integer);
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AImports: TStringStringMap;
      const AModulePath: string; const ASpan: TGocciaSourceSpan;
      const ANamespaceName: string = '';
      const APhase: TGocciaImportCallPhase = icpEvaluation;
      const AAttributeType: string = '');
    destructor Destroy; override;
    property Imports: TStringStringMap read FImports;
    property AttributeType: string read FAttributeType;
    // The first local name this declaration binds twice (two named imports
    // with one local name, or a default binding and a named one), which
    // Imports holds once. The source pipeline raises it as a Module early
    // error (ES2026 §16.2.1.1).
    property DuplicateBinding: string read FDuplicateBinding
      write FDuplicateBinding;
    property ModulePath: string read FModulePath;
    property NamespaceName: string read FNamespaceName;
    property Phase: TGocciaImportCallPhase read FPhase;
  end;

  TGocciaExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TStringStringMap; // exported name -> local name
  public
    constructor Create(const AExportsTable: TStringStringMap;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property ExportsTable: TStringStringMap read FExportsTable;
  end;

  TGocciaExportDefaultDeclaration = class(TGocciaStatement)
  private
    FExpression: TGocciaExpression;
    FIsDirectDeclaration: Boolean;
    FLocalName: string;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExpression: TGocciaExpression;
      const ALocalName: string; const AIsDirectDeclaration: Boolean;
      const ASpan: TGocciaSourceSpan);
    property Expression: TGocciaExpression read FExpression;
    property IsDirectDeclaration: Boolean read FIsDirectDeclaration;
    property LocalName: string read FLocalName;
  end;

  TGocciaExportVariableDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaVariableDeclaration;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ADeclaration: TGocciaVariableDeclaration;
      const ASpan: TGocciaSourceSpan);
    property Declaration: TGocciaVariableDeclaration read FDeclaration;
  end;

  TGocciaExportDestructuringDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaDestructuringDeclaration;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ADeclaration: TGocciaDestructuringDeclaration;
      const ASpan: TGocciaSourceSpan);
    property Declaration: TGocciaDestructuringDeclaration read FDeclaration;
  end;

  TGocciaExportFunctionDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaFunctionDeclaration;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ADeclaration: TGocciaFunctionDeclaration;
      const ASpan: TGocciaSourceSpan);
    property Declaration: TGocciaFunctionDeclaration read FDeclaration;
  end;

  TGocciaExportClassDeclaration = class(TGocciaStatement)
  private
    FDeclaration: TGocciaClassDeclaration;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ADeclaration: TGocciaClassDeclaration;
      const ASpan: TGocciaSourceSpan);
    property Declaration: TGocciaClassDeclaration read FDeclaration;
  end;

  TGocciaReExportDeclaration = class(TGocciaStatement)
  private
    FModuleSpecifierStart: Integer;
    FModuleSpecifierEnd: Integer;
    FAttributeType: string;
    FExportsTable: TStringStringMap; // exported name -> source name
    FIsStarExport: Boolean;
    FModulePath: string;
    FNamespaceName: string;
  public
    { Where the module path's string literal is, in the parsed text. }
    procedure SetModuleSpecifierRange(const AStartOffset, AEndOffset: Integer);
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExportsTable: TStringStringMap;
      const AModulePath: string; const ASpan: TGocciaSourceSpan;
      const AIsStarExport: Boolean = False;
      const ANamespaceName: string = '';
      const AAttributeType: string = '');
    destructor Destroy; override;
    property AttributeType: string read FAttributeType;
    property ExportsTable: TStringStringMap read FExportsTable;
    property IsStarExport: Boolean read FIsStarExport;
    property ModulePath: string read FModulePath;
    property NamespaceName: string read FNamespaceName;
  end;

  TGocciaEmptyStatement = class(TGocciaStatement)
  public
    constructor Create(const ASpan: TGocciaSourceSpan);
  end;

  TGocciaCaseClause = class(TGocciaASTNode)
  private
    FTest: TGocciaExpression;  // Case value expression (nil for default case)
    FConsequent: TObjectList<TGocciaStatement>;  // Statements to execute
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ATest: TGocciaExpression; const AConsequent: TObjectList<TGocciaStatement>; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Test: TGocciaExpression read FTest;
    property Consequent: TObjectList<TGocciaStatement> read FConsequent;
  end;

  TGocciaSwitchStatement = class(TGocciaStatement)
  private
    FDiscriminant: TGocciaExpression;  // Expression to switch on
    FCases: TObjectList<TGocciaCaseClause>;  // List of case clauses
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ADiscriminant: TGocciaExpression; const ACases: TObjectList<TGocciaCaseClause>; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Discriminant: TGocciaExpression read FDiscriminant;
    property Cases: TObjectList<TGocciaCaseClause> read FCases;
  end;

  TGocciaBreakStatement = class(TGocciaStatement)
  private
    FTargetLabel: string;
  public
    constructor Create(const ASpan: TGocciaSourceSpan;
      const ATargetLabel: string = '');
    property TargetLabel: string read FTargetLabel;
  end;

  TGocciaContinueStatement = class(TGocciaStatement)
  private
    FTargetLabel: string;
  public
    constructor Create(const ASpan: TGocciaSourceSpan;
      const ATargetLabel: string = '');
    property TargetLabel: string read FTargetLabel;
  end;

  TGocciaUsingDeclaration = class(TGocciaStatement)
  private
    FVariables: TArray<TGocciaVariableInfo>;
    FIsAwait: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AVariables: TArray<TGocciaVariableInfo>;
      const AIsAwait: Boolean; const ASpan: TGocciaSourceSpan);
    property Variables: TArray<TGocciaVariableInfo> read FVariables;
    property IsAwait: Boolean read FIsAwait;
  end;

function HasUseStrictDirective(const ABody: TGocciaASTNode): Boolean; overload;
function HasUseStrictDirective(const AProgram: TGocciaProgram): Boolean; overload;

implementation

uses
  SysUtils,

  Goccia.Constants,
  Goccia.Execution.CallSite,
  Goccia.GarbageCollector,
  Goccia.Keywords.Reserved,
  Goccia.Values.Error,
  Goccia.Values.ObjectPropertyDescriptor;

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
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FExpression := AExpression;
  end;

  { TGocciaVariableDeclaration }

  constructor TGocciaVariableDeclaration.Create(const AVariables: TArray<TGocciaVariableInfo>;
    const AIsConst: Boolean; const ASpan: TGocciaSourceSpan;
    const AIsVar: Boolean = False);
  begin
    inherited Create(ASpan);
    FVariables := AVariables;
    FIsConst := AIsConst;
    FIsVar := AIsVar;
  end;

  { TGocciaFunctionDeclaration }

  constructor TGocciaFunctionDeclaration.Create(const AName: string;
    const AFunctionExpression: TGocciaFunctionExpression;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FName := AName;
    FFunctionExpression := AFunctionExpression;
  end;

  { TGocciaDestructuringDeclaration }

  constructor TGocciaDestructuringDeclaration.Create(const APattern: TGocciaDestructuringPattern; const AInitializer: TGocciaExpression; const AIsConst: Boolean; const ASpan: TGocciaSourceSpan; const AIsVar: Boolean = False);
  begin
    inherited Create(ASpan);
    FPattern := APattern;
    FInitializer := AInitializer;
    FIsConst := AIsConst;
    FIsVar := AIsVar;
  end;

  { TGocciaBlockStatement }

  constructor TGocciaBlockStatement.Create(const ANodes: TObjectList<TGocciaASTNode>;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
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
    FInstancePropertyTypes.Free;
    FPrivateMethods.Free;
    FPrivateStaticProperties.Free;
    inherited;
  end;

  { TGocciaIfStatement }

  constructor TGocciaIfStatement.Create(const ACondition: TGocciaExpression;
    const AConsequent, AAlternate: TGocciaStatement; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FCondition := ACondition;
    FConsequent := AConsequent;
    FAlternate := AAlternate;
  end;

  { TGocciaForStatement }

  constructor TGocciaForStatement.Create(const AInit: TGocciaStatement; const ACondition: TGocciaExpression;
    const AUpdate: TGocciaExpression; const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FInit := AInit;
    FCondition := ACondition;
    FUpdate := AUpdate;
    FBody := ABody;
  end;

  { TGocciaForOfStatement }

  constructor TGocciaForOfStatement.Create(const AIsConst: Boolean; const ABindingName: string;
    const ABindingPattern: TGocciaDestructuringPattern; const AIterable: TGocciaExpression;
    const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan;
    const AMatchPattern: TGocciaMatchPattern = nil;
    const AAssignmentTarget: TGocciaDestructuringPattern = nil;
    const AIsUsing: Boolean = False;
    const AIsAwaitUsing: Boolean = False);
begin
  inherited Create(ASpan);
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
  const ASpan: TGocciaSourceSpan;
  const AAssignmentTarget: TGocciaDestructuringPattern = nil);
begin
  inherited Create(ASpan);
  FIsConst := AIsConst;
  FBindingName := ABindingName;
  FBindingPattern := ABindingPattern;
  FAssignmentTarget := AAssignmentTarget;
  FObjectExpression := AObjectExpression;
  FBody := ABody;
end;

  { TGocciaReturnStatement }

  constructor TGocciaReturnStatement.Create(const AValue: TGocciaExpression;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FHasExpression := AValue <> nil;
    if AValue = nil then
      FValue := TGocciaLiteralExpression.Create(TGocciaUndefinedLiteralValue.UndefinedValue, ASpan)
    else
      FValue := AValue;
  end;

  { TGocciaThrowStatement }

  constructor TGocciaThrowStatement.Create(const AValue: TGocciaExpression;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FValue := AValue;
  end;

  { TGocciaTryStatement }

  constructor TGocciaTryStatement.Create(const ABlock: TGocciaBlockStatement;
    const ACatchParam: string; const ACatchBlock, AFinallyBlock: TGocciaBlockStatement;
    const ASpan: TGocciaSourceSpan; const ACatchPattern: TGocciaMatchPattern = nil;
    const ACatchBindingPattern: TGocciaDestructuringPattern = nil);
begin
  inherited Create(ASpan);
  FBlock := ABlock;
  FCatchParam := ACatchParam;
  FCatchBlock := ACatchBlock;
  FFinallyBlock := AFinallyBlock;
  FCatchPattern := ACatchPattern;
  FCatchBindingPattern := ACatchBindingPattern;
end;

  { TGocciaClassMethod }

    constructor TGocciaClassMethod.Create(const AName: string; const AParameters: TGocciaParameterArray;
    const ABody: TGocciaASTNode; const AIsStatic: Boolean; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FName := AName;
    FParameters := AParameters;
    FBody := ABody;
    FIsStatic := AIsStatic;
  end;

  { TGocciaClassDeclaration }

  constructor TGocciaClassDeclaration.Create(const AClassDefinition: TGocciaClassDefinition; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FClassDefinition := AClassDefinition;
  end;

  { TGocciaClassExpression }

  constructor TGocciaClassExpression.Create(const AClassDefinition: TGocciaClassDefinition; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FClassDefinition := AClassDefinition;
  end;

  { TGocciaEnumDeclaration }

  constructor TGocciaEnumDeclaration.Create(const AName: string;
    const AMembers: TArray<TGocciaEnumMember>; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FName := AName;
    FMembers := AMembers;
  end;

  { TGocciaExportEnumDeclaration }

  constructor TGocciaExportEnumDeclaration.Create(const ADeclaration: TGocciaEnumDeclaration;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FDeclaration := ADeclaration;
  end;

  { TGocciaImportDeclaration }

  constructor TGocciaImportDeclaration.Create(const AImports: TStringStringMap;
    const AModulePath: string; const ASpan: TGocciaSourceSpan;
    const ANamespaceName: string; const APhase: TGocciaImportCallPhase;
    const AAttributeType: string);
  begin
    inherited Create(ASpan);
    FImports := AImports;
    FAttributeType := AAttributeType;
    FModulePath := AModulePath;
    FNamespaceName := ANamespaceName;
    FPhase := APhase;
  end;

  { TGocciaExportDeclaration }

  constructor TGocciaExportDeclaration.Create(const AExportsTable: TStringStringMap;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FExportsTable := AExportsTable;
  end;

  { TGocciaExportDefaultDeclaration }

  constructor TGocciaExportDefaultDeclaration.Create(
    const AExpression: TGocciaExpression; const ALocalName: string;
    const AIsDirectDeclaration: Boolean; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FExpression := AExpression;
    FIsDirectDeclaration := AIsDirectDeclaration;
    FLocalName := ALocalName;
  end;

  { TGocciaExportVariableDeclaration }

  constructor TGocciaExportVariableDeclaration.Create(const ADeclaration: TGocciaVariableDeclaration;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FDeclaration := ADeclaration;
  end;

  { TGocciaExportDestructuringDeclaration }

  constructor TGocciaExportDestructuringDeclaration.Create(
    const ADeclaration: TGocciaDestructuringDeclaration;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FDeclaration := ADeclaration;
  end;

  { TGocciaExportFunctionDeclaration }

  constructor TGocciaExportFunctionDeclaration.Create(
    const ADeclaration: TGocciaFunctionDeclaration;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FDeclaration := ADeclaration;
  end;

  { TGocciaExportClassDeclaration }

  constructor TGocciaExportClassDeclaration.Create(
    const ADeclaration: TGocciaClassDeclaration;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FDeclaration := ADeclaration;
  end;

  { TGocciaReExportDeclaration }

  constructor TGocciaReExportDeclaration.Create(const AExportsTable: TStringStringMap;
    const AModulePath: string; const ASpan: TGocciaSourceSpan;
    const AIsStarExport: Boolean = False; const ANamespaceName: string = '';
    const AAttributeType: string = '');
  begin
    inherited Create(ASpan);
    FAttributeType := AAttributeType;
    FExportsTable := AExportsTable;
    FIsStarExport := AIsStarExport;
    FModulePath := AModulePath;
    FNamespaceName := ANamespaceName;
  end;

  { TGocciaEmptyStatement }

  constructor TGocciaEmptyStatement.Create(const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
  end;

  { TGocciaWhileStatement }

  constructor TGocciaWhileStatement.Create(const ACondition: TGocciaExpression; const ABody: TGocciaStatement; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FCondition := ACondition;
    FBody := ABody;
  end;

  { TGocciaDoWhileStatement }

  constructor TGocciaDoWhileStatement.Create(const ABody: TGocciaStatement; const ACondition: TGocciaExpression; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FBody := ABody;
    FCondition := ACondition;
  end;

  { TGocciaWithStatement }

  constructor TGocciaWithStatement.Create(
    const AObjectExpression: TGocciaExpression; const ABody: TGocciaStatement;
    const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FObjectExpression := AObjectExpression;
    FBody := ABody;
  end;

  { TGocciaCaseClause }

  constructor TGocciaCaseClause.Create(const ATest: TGocciaExpression; const AConsequent: TObjectList<TGocciaStatement>; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FTest := ATest;
    FConsequent := AConsequent;
  end;

  destructor TGocciaCaseClause.Destroy;
  begin
    if Assigned(FConsequent) then
      FConsequent.OwnsObjects := False;
    FConsequent.Free;
    inherited;
  end;

  { TGocciaSwitchStatement }

  constructor TGocciaSwitchStatement.Create(const ADiscriminant: TGocciaExpression; const ACases: TObjectList<TGocciaCaseClause>; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FDiscriminant := ADiscriminant;
    FCases := ACases;
  end;

  destructor TGocciaSwitchStatement.Destroy;
  begin
    if Assigned(FCases) then
      FCases.OwnsObjects := False;
    FCases.Free;
    inherited;
  end;

  { TGocciaBreakStatement }

  constructor TGocciaBreakStatement.Create(const ASpan: TGocciaSourceSpan;
    const ATargetLabel: string);
  begin
    inherited Create(ASpan);
    FTargetLabel := ATargetLabel;
  end;

  { TGocciaContinueStatement }

  constructor TGocciaContinueStatement.Create(const ASpan: TGocciaSourceSpan;
    const ATargetLabel: string);
  begin
    inherited Create(ASpan);
    FTargetLabel := ATargetLabel;
  end;

  { Execute / Evaluate overrides }

  { TGocciaUsingDeclaration }

  constructor TGocciaUsingDeclaration.Create(const AVariables: TArray<TGocciaVariableInfo>;
    const AIsAwait: Boolean; const ASpan: TGocciaSourceSpan);
  begin
    inherited Create(ASpan);
    FVariables := AVariables;
    FIsAwait := AIsAwait;
  end;

  destructor TGocciaBlockStatement.Destroy;
  begin
    if Assigned(FNodes) then
      FNodes.OwnsObjects := False;
    FNodes.Free;
    inherited;
  end;

  destructor TGocciaImportDeclaration.Destroy;
  begin
    FImports.Free;
    inherited;
  end;

  destructor TGocciaExportDeclaration.Destroy;
  begin
    FExportsTable.Free;
    inherited;
  end;

  destructor TGocciaReExportDeclaration.Destroy;
  begin
    FExportsTable.Free;
    inherited;
  end;


{ ForEachChild }

procedure VisitVariables(const AVisitor: TGocciaASTChildVisitor;
  const AVariables: TArray<TGocciaVariableInfo>);
var
  I: Integer;
begin
  for I := 0 to High(AVariables) do
  begin
    AVisitor.VisitChild(AVariables[I].Pattern);
    AVisitor.VisitChild(AVariables[I].Initializer);
  end;
end;

procedure VisitDecorators(const AVisitor: TGocciaASTChildVisitor;
  const ADecorators: TGocciaDecoratorList);
var
  I: Integer;
begin
  for I := 0 to High(ADecorators) do
    AVisitor.VisitChild(ADecorators[I]);
end;

function MemberPartKind(const AKind: TGocciaClassElementKind): TGocciaASTPartKind;
begin
  case AKind of
    cekField: Result := apkClassField;
    cekAccessor: Result := apkClassAccessor;
    cekStaticBlock: Result := apkStaticBlock;
  else
    Result := apkClassMethod;
  end;
end;

{ A class's children are its decorators, its heritage, and its members in
  source order, each member a part around its decorators, its computed key
  and its value. The element array holds every member but a plain instance
  field, which only the field order holds; the per-kind maps hold the same
  members again, grouped by kind, for the evaluator. The two lists are each
  in source order and are merged by position. }
procedure ForEachClassChild(const AVisitor: TGocciaASTChildVisitor;
  const ADefinition: TGocciaClassDefinition);
var
  ElementIndex, FieldIndex, I: Integer;
  Flags: TGocciaASTPartFlags;

  procedure NextField;
  begin
    while (FieldIndex <= High(ADefinition.FFieldOrder)) and
          (ADefinition.FFieldOrder[FieldIndex].ElementIndex >= 0) do
      Inc(FieldIndex);
  end;

  function FieldComesFirst: Boolean;
  begin
    if FieldIndex > High(ADefinition.FFieldOrder) then
      Exit(False);
    if ElementIndex > High(ADefinition.FElements) then
      Exit(True);
    Result := ADefinition.FFieldOrder[FieldIndex].StartOffset <
      ADefinition.FElements[ElementIndex].StartOffset;
  end;

begin
  if not Assigned(ADefinition) then
    Exit;
  VisitDecorators(AVisitor, ADefinition.Decorators);
  AVisitor.VisitChild(ADefinition.SuperClassExpression);

  ElementIndex := 0;
  FieldIndex := 0;
  NextField;
  while (ElementIndex <= High(ADefinition.FElements)) or
        (FieldIndex <= High(ADefinition.FFieldOrder)) do
  begin
    if FieldComesFirst then
    begin
      with ADefinition.FFieldOrder[FieldIndex] do
      begin
        Flags := [];
        if IsComputed then
          Include(Flags, apfComputed);
        AVisitor.EnterPart(apkClassField, StartOffset, EndOffset, Flags);
        AVisitor.VisitChild(ComputedKeyExpression);
        AVisitor.VisitChild(FieldInitializer);
        AVisitor.LeavePart;
      end;
      Inc(FieldIndex);
      NextField;
    end
    else
    begin
      with ADefinition.FElements[ElementIndex] do
      begin
        Flags := [];
        if IsStatic then
          Include(Flags, apfStatic);
        if IsComputed then
          Include(Flags, apfComputed);
        AVisitor.EnterPart(MemberPartKind(Kind), StartOffset, EndOffset, Flags);
        for I := 0 to High(Decorators) do
          AVisitor.VisitChild(Decorators[I]);
        AVisitor.VisitChild(ComputedKeyExpression);
        AVisitor.VisitChild(MethodNode);
        AVisitor.VisitChild(GetterNode);
        AVisitor.VisitChild(SetterNode);
        AVisitor.VisitChild(FieldInitializer);
        AVisitor.VisitChild(StaticBlockBody);
        AVisitor.LeavePart;
      end;
      Inc(ElementIndex);
    end;
  end;
end;

procedure TGocciaExpressionStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FExpression);
end;

procedure TGocciaVariableDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitVariables(AVisitor, FVariables);
end;

procedure TGocciaUsingDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitVariables(AVisitor, FVariables);
end;

procedure TGocciaFunctionDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FFunctionExpression);
end;

procedure TGocciaDestructuringDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FPattern);
  AVisitor.VisitChild(FInitializer);
end;

procedure TGocciaBlockStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  for I := 0 to FNodes.Count - 1 do
    AVisitor.VisitChild(FNodes[I]);
end;

procedure TGocciaIfStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FCondition);
  AVisitor.VisitChild(FConsequent);
  AVisitor.VisitChild(FAlternate);
end;

procedure TGocciaForStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FInit);
  AVisitor.VisitChild(FCondition);
  AVisitor.VisitChild(FUpdate);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaWhileStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FCondition);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaDoWhileStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FBody);
  AVisitor.VisitChild(FCondition);
end;

procedure TGocciaWithStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FObjectExpression);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaForOfStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FBindingPattern);
  AVisitor.VisitChild(FAssignmentTarget);
  AVisitor.VisitChild(FMatchPattern);
  AVisitor.VisitChild(FIterable);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaForInStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FBindingPattern);
  AVisitor.VisitChild(FAssignmentTarget);
  AVisitor.VisitChild(FObjectExpression);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaReturnStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaThrowStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaTryStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FBlock);
  AVisitor.VisitChild(FCatchBindingPattern);
  AVisitor.VisitChild(FCatchPattern);
  AVisitor.VisitChild(FCatchBlock);
  AVisitor.VisitChild(FFinallyBlock);
end;

procedure TGocciaClassMethod.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  ForEachParameterChild(AVisitor, FParameters);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaClassDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  ForEachClassChild(AVisitor, FClassDefinition);
end;

procedure TGocciaClassExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  ForEachClassChild(AVisitor, FClassDefinition);
end;

procedure TGocciaEnumDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  for I := 0 to High(FMembers) do
    AVisitor.VisitChild(FMembers[I].Initializer);
end;

procedure TGocciaExportEnumDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FDeclaration);
end;

procedure TGocciaExportDefaultDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FExpression);
end;

procedure TGocciaExportVariableDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FDeclaration);
end;

procedure TGocciaExportDestructuringDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FDeclaration);
end;

procedure TGocciaExportFunctionDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FDeclaration);
end;

procedure TGocciaExportClassDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FDeclaration);
end;

procedure TGocciaCaseClause.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  AVisitor.VisitChild(FTest);
  for I := 0 to FConsequent.Count - 1 do
    AVisitor.VisitChild(FConsequent[I]);
end;

procedure TGocciaSwitchStatement.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  AVisitor.VisitChild(FDiscriminant);
  for I := 0 to FCases.Count - 1 do
    AVisitor.VisitChild(FCases[I]);
end;


procedure TGocciaImportDeclaration.SetModuleSpecifierRange(const AStartOffset,
  AEndOffset: Integer);
begin
  FModuleSpecifierStart := AStartOffset;
  FModuleSpecifierEnd := AEndOffset;
end;

procedure TGocciaImportDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if FModuleSpecifierEnd > FModuleSpecifierStart then
  begin
    AVisitor.EnterPart(apkModuleSpecifier, FModuleSpecifierStart,
      FModuleSpecifierEnd, []);
    AVisitor.LeavePart;
  end;
end;

procedure TGocciaReExportDeclaration.SetModuleSpecifierRange(
  const AStartOffset, AEndOffset: Integer);
begin
  FModuleSpecifierStart := AStartOffset;
  FModuleSpecifierEnd := AEndOffset;
end;

procedure TGocciaReExportDeclaration.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if FModuleSpecifierEnd > FModuleSpecifierStart then
  begin
    AVisitor.EnterPart(apkModuleSpecifier, FModuleSpecifierStart,
      FModuleSpecifierEnd, []);
    AVisitor.LeavePart;
  end;
end;

end.
