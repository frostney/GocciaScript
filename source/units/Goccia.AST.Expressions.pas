unit Goccia.AST.Expressions;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  CriticalSections,
  HashMap,
  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Error.CallDiagnostics,
  Goccia.Modules,
  Goccia.Scope.BindingMap,
  Goccia.SourceSpan,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  // Forward declarations
  TGocciaDestructuringPattern = class;
  TGocciaGetterExpression = class;
  TGocciaMatchPattern = class;
  TGocciaPrivateMemberExpression = class;
  TGocciaSetterExpression = class;

  TGocciaParameter = record
    Name: string;                                 // For simple parameters
    Pattern: TGocciaDestructuringPattern;         // For destructuring parameters
    DefaultValue: TGocciaExpression;              // nil if no default value
    IsPattern: Boolean;                           // True if this is a destructuring pattern
    IsRest: Boolean;                              // True if this is a rest parameter (...args)
    IsOptional: Boolean;                          // True if optional parameter (x?)
    TypeAnnotation: string;                       // Stored for future optimization; ignored at runtime
    { The parameter's extent in the parsed text, from its first token (the
      `...` of a rest parameter) to the end of its type annotation or default
      value. Equal offsets mean the parser did not record one. }
    StartOffset: Integer;
    EndOffset: Integer;
  end;
  TGocciaParameterArray = array of TGocciaParameter;

  TGocciaExpressionMap = TOrderedStringMap<TGocciaExpression>;
  TGocciaGetterExpressionMap = TOrderedStringMap<TGocciaGetterExpression>;
  TGocciaMatchPatternList = TObjectList<TGocciaMatchPattern>;
  TGocciaSetterExpressionMap = TOrderedStringMap<TGocciaSetterExpression>;

  TGocciaLiteralExpression = class(TGocciaExpression)
  private
    FValue: TGocciaValue;
    FSourceText: string;
  public
    constructor Create(const AValue: TGocciaValue; const ASpan: TGocciaSourceSpan;
      const ASourceText: string = '');
    destructor Destroy; override;
    property Value: TGocciaValue read FValue;
    property SourceText: string read FSourceText;
  end;

  TGocciaTemplateLiteralExpression = class(TGocciaExpression)
  private
    FValue: string;
  public
    constructor Create(const AValue: string; const ASpan: TGocciaSourceSpan);
    property Value: string read FValue;
  end;

  TGocciaRegexLiteralExpression = class(TGocciaExpression)
  private
    FPattern: string;
    FFlags: string;
  public
    constructor Create(const APattern, AFlags: string; const ASpan: TGocciaSourceSpan);
    property Pattern: string read FPattern;
    property Flags: string read FFlags;
  end;

  TGocciaTemplateWithInterpolationExpression = class(TGocciaExpression)
  private
    FParts: TObjectList<TGocciaExpression>; // Mix of string literals and expressions
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AParts: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Parts: TObjectList<TGocciaExpression> read FParts;
  end;

  TGocciaTemplateStrings = array of string;
  // TC39 Template Literal Revision: per-segment validity flags for cooked strings.
  // True means the cooked value is valid; False means the escape was malformed
  // and the cooked value should be undefined.
  TGocciaTemplateCookedValid = array of Boolean;

  TGocciaTaggedTemplateExpression = class(TGocciaExpression)
  private
    FTag: TGocciaExpression;
    FCookedStrings: TGocciaTemplateStrings;
    FRawStrings: TGocciaTemplateStrings;
    FCookedValid: TGocciaTemplateCookedValid;
    FExpressions: TObjectList<TGocciaExpression>;
    FTemplateSiteId: UInt64;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ATag: TGocciaExpression;
      const ACookedStrings, ARawStrings: TGocciaTemplateStrings;
      const ACookedValid: TGocciaTemplateCookedValid;
      const AExpressions: TObjectList<TGocciaExpression>;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Tag: TGocciaExpression read FTag;
    property CookedStrings: TGocciaTemplateStrings read FCookedStrings;
    property RawStrings: TGocciaTemplateStrings read FRawStrings;
    property CookedValid: TGocciaTemplateCookedValid read FCookedValid;
    property Expressions: TObjectList<TGocciaExpression> read FExpressions;
    property TemplateSiteId: UInt64 read FTemplateSiteId;
  end;

  TGocciaIdentifierExpression = class(TGocciaExpression)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ASpan: TGocciaSourceSpan);
    property Name: string read FName;
  end;

  TGocciaBinaryExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaExpression;
    FOperator: TGocciaTokenType;
    FRight: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ALeft: TGocciaExpression; const AOperator: TGocciaTokenType;
      const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Left: TGocciaExpression read FLeft;
    property Operator: TGocciaTokenType read FOperator;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaSequenceExpression = class(TGocciaExpression)
  private
    FExpressions: TObjectList<TGocciaExpression>;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExpressions: TObjectList<TGocciaExpression>;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Expressions: TObjectList<TGocciaExpression> read FExpressions;
  end;

  TGocciaUnaryExpression = class(TGocciaExpression)
  private
    FOperator: TGocciaTokenType;
    FOperand: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AOperator: TGocciaTokenType; const AOperand: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    property Operator: TGocciaTokenType read FOperator;
    property Operand: TGocciaExpression read FOperand;
  end;

  { The assignment forms decompose their left-hand side into the fields the
    evaluator and the compiler need: a name, or an object and a key. Target
    keeps the node the parser built for it before it knew an `=` followed,
    for a tool that reads the tree's syntax rather than running it. nil when
    the parser did not keep one. }
  TGocciaTargetedAssignmentExpression = class(TGocciaExpression)
  private
    FTarget: TGocciaExpression;
  public
    property Target: TGocciaExpression read FTarget write FTarget;
  end;

  TGocciaAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FName: string;
    FValue: TGocciaExpression;
    FInferName: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan;
      const AInferName: Boolean = True);
    property Name: string read FName;
    property Value: TGocciaExpression read FValue;
    property InferName: Boolean read FInferName;
  end;

  TGocciaPropertyAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APropertyName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FPropertyName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaComputedPropertyAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FObject: TGocciaExpression;
    FPropertyExpression: TGocciaExpression;
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyExpression: TGocciaExpression read FPropertyExpression;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaCompoundAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
    FInferName: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression;
      const ASpan: TGocciaSourceSpan; const AInferName: Boolean = True);
    property Name: string read FName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
    property InferName: Boolean read FInferName;
  end;

  TGocciaPropertyCompoundAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APropertyName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FPropertyName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaComputedPropertyCompoundAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FObject: TGocciaExpression;
    FPropertyExpression: TGocciaExpression;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyExpression: TGocciaExpression read FPropertyExpression;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaIncrementExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
    FOperator: TGocciaTokenType; // gttIncrement or gttDecrement
    FIsPrefix: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AOperand: TGocciaExpression; const AOperator: TGocciaTokenType; const AIsPrefix: Boolean; const ASpan: TGocciaSourceSpan);
    property Operand: TGocciaExpression read FOperand;
    property Operator: TGocciaTokenType read FOperator;
    property IsPrefix: Boolean read FIsPrefix;
  end;

  // Whether an expression is or contains an optional chain, once known.
  TGocciaOptionalChainPresence = (ocpUnknown, ocpAbsent, ocpPresent);

  TGocciaCallExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
    FOptional: Boolean;
    FOptionalChainPresence: TGocciaOptionalChainPresence;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ACallee: TGocciaExpression;
      const AArguments: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan;
      const AOptional: Boolean = False);
    destructor Destroy; override;
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
    property Optional: Boolean read FOptional;
    // Remembered by the compiler, which asks it of every call it compiles:
    // the answer for a call covers the chain of calls and members before it,
    // so without it a chain of n calls is walked n times.
    property OptionalChainPresence: TGocciaOptionalChainPresence
      read FOptionalChainPresence write FOptionalChainPresence;
  end;

  TGocciaMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FProperty: string;
    FPropertyExpression: TGocciaExpression;
    FComputed: Boolean;
    FOptional: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const AProperty: string;
      const AComputed: Boolean; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False); overload;
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression;
      const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False); overload;
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FProperty;
    property PropertyExpression: TGocciaExpression read FPropertyExpression;
    property Computed: Boolean read FComputed;
    property Optional: Boolean read FOptional;
  end;

  TGocciaArrayExpression = class(TGocciaExpression)
  private
    FElements: TObjectList<TGocciaExpression>;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AElements: TObjectList<TGocciaExpression>;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Elements: TObjectList<TGocciaExpression> read FElements;
  end;

  // Property source order tracking
  TGocciaPropertySourceType = (
    pstStatic,    // Regular property: {key: value}
    pstComputed,  // Computed property: {[expr]: value}
    pstGetter,    // Getter: {get prop() {}}
    pstSetter,    // Setter: {set prop(val) {}}
    pstComputedGetter, // Getter: {get [expr]() {}}
    pstComputedSetter  // Setter: {set [expr](val) {}}
  );

  TGocciaPropertySourceOrder = record
    PropertyType: TGocciaPropertySourceType;
    StaticKey: string;           // For static properties, getters, setters
    ComputedIndex: Integer;      // Index into ComputedProperties list
    Expression: TGocciaExpression; // Source-order static value expression
    UsesColonSyntax: Boolean;    // True for PropertyName : AssignmentExpression
    { The getter or setter of a pstGetter / pstSetter entry. The maps keep
      only the last accessor for a key; this keeps each one. }
    Accessor: TGocciaExpression;
    { The member's extent in the parsed text, from its first token to the end
      of its value. Equal offsets mean the parser did not record one. }
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TGocciaObjectExpression = class(TGocciaExpression)
  private
    FProperties: TGocciaExpressionMap;
    FPropertyInsertionOrder: TStringList;
    FComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
    FGetters: TGocciaGetterExpressionMap;
    FSetters: TGocciaSetterExpressionMap;

    // New: tracks source order of ALL property types
    FPropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
    FComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const ASpan: TGocciaSourceSpan); overload;
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
      const ASpan: TGocciaSourceSpan); overload;
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
      const AGetters: TGocciaGetterExpressionMap;
      const ASetters: TGocciaSetterExpressionMap;
      const ASpan: TGocciaSourceSpan); overload;
    // New constructor with source order tracking
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
      const AComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
      const AGetters: TGocciaGetterExpressionMap;
      const ASetters: TGocciaSetterExpressionMap;
      const APropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
      const ASpan: TGocciaSourceSpan); overload;
    destructor Destroy; override;

    property Properties: TGocciaExpressionMap read FProperties;
    property PropertyInsertionOrder: TStringList read FPropertyInsertionOrder;
    property ComputedProperties: THashMap<TGocciaExpression, TGocciaExpression> read FComputedProperties;
    property ComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>> read FComputedPropertiesInOrder;
    property Getters: TGocciaGetterExpressionMap read FGetters;
    property Setters: TGocciaSetterExpressionMap read FSetters;
    property PropertySourceOrder: TArray<TGocciaPropertySourceOrder> read FPropertySourceOrder;
    function GetPropertyNamesInOrder: TStringList;
  end;

  TGocciaArrowFunctionExpression = class(TGocciaExpression)
  private
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FReturnType: string;
    FIsAsync: Boolean;
    FSourceText: string;
    FDirectEvalUsage: TGocciaDirectEvalUsage;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ASpan: TGocciaSourceSpan);
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property ReturnType: string read FReturnType write FReturnType;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property SourceText: string read FSourceText write FSourceText;
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
  end;

  TGocciaFunctionExpression = class(TGocciaExpression)
  private
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FIsAsync: Boolean;
    FIsGenerator: Boolean;
    FHasOwnPrototype: Boolean;
    FParsedInStrictMode: Boolean;
    FSourceText: string;
    FName: string;
    FDirectEvalUsage: TGocciaDirectEvalUsage;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ASpan: TGocciaSourceSpan);
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property IsGenerator: Boolean read FIsGenerator write FIsGenerator;
    // True when this function node represents a `function`/`function*`
    // declaration or expression (or async generator) that, per ES2026 §10.2.5 MakeConstructor,
    // requires its own `prototype` data property pointing to a fresh object whose
    // `constructor` back-references the function. False for concise methods,
    // arrow functions, getters/setters, and plain async functions.
    property HasOwnPrototype: Boolean read FHasOwnPrototype write FHasOwnPrototype;
    property ParsedInStrictMode: Boolean read FParsedInStrictMode write FParsedInStrictMode;
    property SourceText: string read FSourceText write FSourceText;
    property Name: string read FName write FName;
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
  end;

  TGocciaObjectMethodDefinition = class(TGocciaExpression)
  private
    FFunctionExpression: TGocciaFunctionExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AFunctionExpression: TGocciaFunctionExpression; const ASpan: TGocciaSourceSpan);
    property FunctionExpression: TGocciaFunctionExpression read FFunctionExpression;
  end;

  TGocciaYieldExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
    FIsDelegate: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AOperand: TGocciaExpression; const AIsDelegate: Boolean;
      const ASpan: TGocciaSourceSpan);
    property Operand: TGocciaExpression read FOperand;
    property IsDelegate: Boolean read FIsDelegate;
  end;

  TGocciaAwaitExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AOperand: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Operand: TGocciaExpression read FOperand;
  end;

  TGocciaConditionalExpression = class(TGocciaExpression)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaExpression;
    FAlternate: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ACondition, AConsequent, AAlternate: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    property Condition: TGocciaExpression read FCondition;
    property Consequent: TGocciaExpression read FConsequent;
    property Alternate: TGocciaExpression read FAlternate;
  end;

  TGocciaNewExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ACallee: TGocciaExpression;
      const AArguments: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
  end;

  TGocciaThisExpression = class(TGocciaExpression)
  public
  end;

  TGocciaSuperExpression = class(TGocciaExpression)
  public
  end;

  TGocciaImportMetaExpression = class(TGocciaExpression)
  public
  end;

  TGocciaNewTargetExpression = class(TGocciaExpression)
  public
  end;

  TGocciaImportCallExpression = class(TGocciaExpression)
  private
    FSpecifier: TGocciaExpression;
    FOptions: TGocciaExpression;
    FPhase: TGocciaImportCallPhase;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ASpecifier: TGocciaExpression; const AOptions: TGocciaExpression;
      const APhase: TGocciaImportCallPhase; const ASpan: TGocciaSourceSpan);
    property Specifier: TGocciaExpression read FSpecifier;
    property Options: TGocciaExpression read FOptions;
    property Phase: TGocciaImportCallPhase read FPhase;
    { True when the specifier is a string literal: the request is then part of
      the statically known module graph (ADR 0122). A computed specifier is
      not, and reading the host for it needs a read grant. }
    function HasLiteralSpecifier: Boolean;
  end;

  TGocciaHoleExpression = class(TGocciaExpression)
  public
    constructor Create(const ASpan: TGocciaSourceSpan);
  end;

  TGocciaSpreadExpression = class(TGocciaExpression)
  private
    FArgument: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AArgument: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Argument: TGocciaExpression read FArgument;
  end;

  // Getter method: get propertyName() { ... }
  TGocciaGetterExpression = class(TGocciaExpression)
  private
    FBody: TGocciaASTNode;
    FSourceText: string;
    FDirectEvalUsage: TGocciaDirectEvalUsage;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
    property Body: TGocciaASTNode read FBody;
    property SourceText: string read FSourceText write FSourceText;
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
  end;

  // Setter method: set propertyName(value) { ... }
  TGocciaSetterExpression = class(TGocciaExpression)
  private
    FParameter: string;
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FSourceText: string;
    FDirectEvalUsage: TGocciaDirectEvalUsage;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AParameter: string; const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan); overload;
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan); overload;
    property Parameter: string read FParameter;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property SourceText: string read FSourceText write FSourceText;
    property DirectEvalUsage: TGocciaDirectEvalUsage read FDirectEvalUsage
      write FDirectEvalUsage;
  end;

  // Destructuring pattern base class - complete definition
  TGocciaDestructuringPattern = class(TGocciaExpression)
  public
  end;

  // Property in object destructuring: key: pattern or key (shorthand)
  TGocciaDestructuringProperty = class(TGocciaASTObject)
  private
    FKey: string;
    FPattern: TGocciaDestructuringPattern;
    FComputed: Boolean;
    FKeyExpression: TGocciaExpression;
  public
    constructor Create(const AKey: string; const APattern: TGocciaDestructuringPattern; const AComputed: Boolean = False; const AKeyExpression: TGocciaExpression = nil);
    property Key: string read FKey;
    property Pattern: TGocciaDestructuringPattern read FPattern;
    property Computed: Boolean read FComputed;
    property KeyExpression: TGocciaExpression read FKeyExpression;
  end;

  // Array destructuring pattern: [a, b, c] = array
  TGocciaArrayDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FElements: TObjectList<TGocciaDestructuringPattern>;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AElements: TObjectList<TGocciaDestructuringPattern>; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Elements: TObjectList<TGocciaDestructuringPattern> read FElements;
  end;

  // Object destructuring pattern: {name, age} = object
  TGocciaObjectDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FProperties: TObjectList<TGocciaDestructuringProperty>;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AProperties: TObjectList<TGocciaDestructuringProperty>; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Properties: TObjectList<TGocciaDestructuringProperty> read FProperties;
  end;

  // Rest pattern: ...rest
  TGocciaRestDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FArgument: TGocciaDestructuringPattern;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AArgument: TGocciaDestructuringPattern; const ASpan: TGocciaSourceSpan);
    property Argument: TGocciaDestructuringPattern read FArgument;
  end;

  // Assignment pattern with default value: a = defaultValue
  TGocciaAssignmentDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  // Identifier pattern: just a variable name
  TGocciaIdentifierDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ASpan: TGocciaSourceSpan);
    property Name: string read FName;
  end;

  // Member expression pattern: obj.prop, this.x, arr[i], obj[key]
  TGocciaMemberExpressionDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FExpression: TGocciaMemberExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExpression: TGocciaMemberExpression; const ASpan: TGocciaSourceSpan);
    property Expression: TGocciaMemberExpression read FExpression;
  end;

  // Private member expression pattern: this.#x
  TGocciaPrivateMemberExpressionDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FExpression: TGocciaPrivateMemberExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExpression: TGocciaPrivateMemberExpression; const ASpan: TGocciaSourceSpan);
    property Expression: TGocciaPrivateMemberExpression read FExpression;
  end;

  // Destructuring assignment expression
  TGocciaDestructuringAssignmentExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaObjectMatchProperty = class(TGocciaASTObject)
  private
    FKey: string;
    FPattern: TGocciaMatchPattern;
    FComputed: Boolean;
    FKeyExpression: TGocciaExpression;
  public
    constructor Create(const AKey: string; const APattern: TGocciaMatchPattern;
      const AComputed: Boolean = False; const AKeyExpression: TGocciaExpression = nil);
    property Key: string read FKey;
    property Pattern: TGocciaMatchPattern read FPattern;
    property Computed: Boolean read FComputed;
    property KeyExpression: TGocciaExpression read FKeyExpression;
  end;

  TGocciaObjectMatchPropertyList = TObjectList<TGocciaObjectMatchProperty>;

  TGocciaMatchPattern = class(TGocciaASTNode)
  end;

  TGocciaWildcardMatchPattern = class(TGocciaMatchPattern)
  end;

  TGocciaValueMatchPattern = class(TGocciaMatchPattern)
  private
    FExpression: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AExpression: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaBindingMatchPattern = class(TGocciaMatchPattern)
  private
    FName: string;
    FDeclarationType: TGocciaDeclarationType;
  public
    constructor Create(const AName: string; const ADeclarationType: TGocciaDeclarationType;
      const ASpan: TGocciaSourceSpan);
    property Name: string read FName;
    property DeclarationType: TGocciaDeclarationType read FDeclarationType;
  end;

  TGocciaArrayMatchPattern = class(TGocciaMatchPattern)
  private
    FElements: TGocciaMatchPatternList;
    FRestPattern: TGocciaMatchPattern;
    FHasRestWildcard: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AElements: TGocciaMatchPatternList;
      const ARestPattern: TGocciaMatchPattern; const AHasRestWildcard: Boolean;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Elements: TGocciaMatchPatternList read FElements;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
    property HasRestWildcard: Boolean read FHasRestWildcard;
  end;

  TGocciaObjectMatchPattern = class(TGocciaMatchPattern)
  private
    FProperties: TGocciaObjectMatchPropertyList;
    FRestPattern: TGocciaMatchPattern;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AProperties: TGocciaObjectMatchPropertyList;
      const ARestPattern: TGocciaMatchPattern; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Properties: TGocciaObjectMatchPropertyList read FProperties;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
  end;

  TGocciaRelationalMatchPattern = class(TGocciaMatchPattern)
  private
    FOperator: TGocciaTokenType;
    FExpression: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AOperator: TGocciaTokenType; const AExpression: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    property Operator: TGocciaTokenType read FOperator;
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaGuardMatchPattern = class(TGocciaMatchPattern)
  private
    FCondition: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ACondition: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaAsMatchPattern = class(TGocciaMatchPattern)
  private
    FPattern: TGocciaMatchPattern;
    FName: string;
    FDeclarationType: TGocciaDeclarationType;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const APattern: TGocciaMatchPattern; const AName: string;
      const ADeclarationType: TGocciaDeclarationType; const ASpan: TGocciaSourceSpan);
    property Pattern: TGocciaMatchPattern read FPattern;
    property Name: string read FName;
    property DeclarationType: TGocciaDeclarationType read FDeclarationType;
  end;

  TGocciaAndMatchPattern = class(TGocciaMatchPattern)
  private
    FPatterns: TGocciaMatchPatternList;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const APatterns: TGocciaMatchPatternList; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Patterns: TGocciaMatchPatternList read FPatterns;
  end;

  TGocciaOrMatchPattern = class(TGocciaMatchPattern)
  private
    FPatterns: TGocciaMatchPatternList;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const APatterns: TGocciaMatchPatternList; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Patterns: TGocciaMatchPatternList read FPatterns;
  end;

  TGocciaNotMatchPattern = class(TGocciaMatchPattern)
  private
    FPattern: TGocciaMatchPattern;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const APattern: TGocciaMatchPattern; const ASpan: TGocciaSourceSpan);
    property Pattern: TGocciaMatchPattern read FPattern;
  end;

  TGocciaExtractorMatchPattern = class(TGocciaMatchPattern)
  private
    FMatcherExpression: TGocciaExpression;
    FArguments: TGocciaMatchPatternList;
    FRestPattern: TGocciaMatchPattern;
    FHasRestWildcard: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AMatcherExpression: TGocciaExpression;
      const AArguments: TGocciaMatchPatternList; const ARestPattern: TGocciaMatchPattern;
      const AHasRestWildcard: Boolean; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property MatcherExpression: TGocciaExpression read FMatcherExpression;
    property Arguments: TGocciaMatchPatternList read FArguments;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
    property HasRestWildcard: Boolean read FHasRestWildcard;
  end;

  TGocciaMatchClause = class(TGocciaASTObject)
  private
    FPattern: TGocciaMatchPattern;
    FExpression: TGocciaExpression;
  public
    constructor Create(const APattern: TGocciaMatchPattern; const AExpression: TGocciaExpression);
    property Pattern: TGocciaMatchPattern read FPattern;
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaMatchClauseList = TObjectList<TGocciaMatchClause>;

  TGocciaIsExpression = class(TGocciaExpression)
  private
    FSubject: TGocciaExpression;
    FPattern: TGocciaMatchPattern;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ASubject: TGocciaExpression; const APattern: TGocciaMatchPattern;
      const ASpan: TGocciaSourceSpan);
    property Subject: TGocciaExpression read FSubject;
    property Pattern: TGocciaMatchPattern read FPattern;
  end;

  TGocciaMatchExpression = class(TGocciaExpression)
  private
    FSubject: TGocciaExpression;
    FClauses: TGocciaMatchClauseList;
    FDefaultExpression: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const ASubject: TGocciaExpression; const AClauses: TGocciaMatchClauseList;
      const ADefaultExpression: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    property Subject: TGocciaExpression read FSubject;
    property Clauses: TGocciaMatchClauseList read FClauses;
    property DefaultExpression: TGocciaExpression read FDefaultExpression;
  end;

  TGocciaPrivateMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FOptional: Boolean;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False);
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Optional: Boolean read FOptional;
  end;

  TGocciaPrivatePropertyAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaPrivatePropertyCompoundAssignmentExpression = class(TGocciaTargetedAssignmentExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    procedure ForEachChild(const AVisitor: TGocciaASTChildVisitor); override;
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaDecoratorList = array of TGocciaExpression;

{ Describes a call/new callee for the shared "not callable"/"not a constructor"
  diagnostics. The bytecode compiler calls this once per call site and stores
  the result on the function template, so the VM produces the text without
  the AST. AKind
  carries a tagged-template site through to the shared suggestion. }
{ Reports each parameter as an apkParameter part holding its pattern and
  default value. For the ForEachChild of every function-like node. }
procedure ForEachParameterChild(const AVisitor: TGocciaASTChildVisitor;
  const AParameters: TGocciaParameterArray);

function CalleeDescriptorFor(const ACallee: TGocciaExpression;
  const AKind: TGocciaCalleeKind = cckPlain): TGocciaCalleeDescriptor;

implementation

uses
  SysUtils,

  Goccia.Execution.CallSite,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.InstructionLimit,
  Goccia.RegExp.Runtime,
  Goccia.Timeout,
  Goccia.Values.BigIntValue,
  Goccia.Values.Error,
  Goccia.Values.PromiseValue;

var
  GTemplateSiteIdLock: TGocciaCriticalSection;
  GNextTemplateSiteId: UInt64;

function AllocateTemplateSiteId: UInt64;
begin
  CriticalSectionEnter(GTemplateSiteIdLock);
  try
    Inc(GNextTemplateSiteId);
    Result := GNextTemplateSiteId;
  finally
    CriticalSectionLeave(GTemplateSiteIdLock);
  end;
end;

{ A member expression's own span starts at the '.' or '[' — the parser hangs
  the accessor off the object expression rather than re-spanning the pair — so
  naming `obj.method` back to the author means covering the object's span too,
  recursively for a chain like `o.a.b`. }
function FullCalleeSpan(const AExpression: TGocciaExpression): TGocciaSourceSpan;
begin
  Result := AExpression.Span;
  if AExpression is TGocciaMemberExpression then
    Result := FullCalleeSpan(
      TGocciaMemberExpression(AExpression).ObjectExpr).Cover(Result)
  else if AExpression is TGocciaPrivateMemberExpression then
    Result := FullCalleeSpan(
      TGocciaPrivateMemberExpression(AExpression).ObjectExpr).Cover(Result);
end;

function CalleeDescriptorFor(const ACallee: TGocciaExpression;
  const AKind: TGocciaCalleeKind = cckPlain): TGocciaCalleeDescriptor;
var
  Member: TGocciaMemberExpression;
begin
  Result := EmptyCalleeDescriptor;
  Result.Kind := AKind;
  if not Assigned(ACallee) then
    Exit;
  Result.CalleeText := NormalizeCalleeText(FullCalleeSpan(ACallee).Text);
  // A tagged template's fault is "the tag is not callable", not a missing
  // method, so its member shape is not carried — the shared suggestion keys on
  // Kind alone.
  if (AKind = cckPlain) and (ACallee is TGocciaMemberExpression) then
  begin
    Member := TGocciaMemberExpression(ACallee);
    // Only a non-computed member has a statically known method name, and only
    // an identifier object can be named back to the author verbatim.
    if not Member.Computed then
    begin
      Result.PropertyName := Member.PropertyName;
      if Member.ObjectExpr is TGocciaIdentifierExpression then
        Result.ObjectText := TGocciaIdentifierExpression(Member.ObjectExpr).Name;
    end;
  end;
end;

{ TGocciaLiteralExpression }

constructor TGocciaLiteralExpression.Create(const AValue: TGocciaValue;
  const ASpan: TGocciaSourceSpan; const ASourceText: string);
begin
  inherited Create(ASpan);
  FValue := AValue;
  FSourceText := ASourceText;
  // Remove from GC -- the AST owns this value, not the garbage collector.
  // A primitive singleton (undefined, true, ...) stays with the collector it
  // was created in, which frees it when the main thread's runtime shuts down.
  if Assigned(FValue) and (TGarbageCollector.Instance <> nil) and
     not IsPrimitiveSingleton(FValue) then
    TGarbageCollector.Instance.UnregisterObject(FValue);
end;

destructor TGocciaLiteralExpression.Destroy;
begin
  // Free the owned value unless it is a primitive singleton, which the
  // collector owns.
  if Assigned(FValue) and not IsPrimitiveSingleton(FValue) then
    FValue.Free;
  inherited;
end;

{ TGocciaTemplateLiteralExpression }

constructor TGocciaTemplateLiteralExpression.Create(const AValue: string;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FValue := AValue;
end;

{ TGocciaRegexLiteralExpression }

constructor TGocciaRegexLiteralExpression.Create(const APattern, AFlags: string;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FPattern := APattern;
  FFlags := AFlags;
end;

{ TGocciaTemplateWithInterpolationExpression }

constructor TGocciaTemplateWithInterpolationExpression.Create(const AParts: TObjectList<TGocciaExpression>;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FParts := AParts;
end;

{ TGocciaTaggedTemplateExpression }

constructor TGocciaTaggedTemplateExpression.Create(const ATag: TGocciaExpression;
  const ACookedStrings, ARawStrings: TGocciaTemplateStrings;
  const ACookedValid: TGocciaTemplateCookedValid;
  const AExpressions: TObjectList<TGocciaExpression>;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FTag := ATag;
  FCookedStrings := ACookedStrings;
  FRawStrings := ARawStrings;
  FCookedValid := ACookedValid;
  FExpressions := AExpressions;
  FTemplateSiteId := AllocateTemplateSiteId;
end;

destructor TGocciaTaggedTemplateExpression.Destroy;
begin
  if Assigned(FExpressions) then
    FExpressions.OwnsObjects := False;
  FExpressions.Free;
  inherited;
end;

{ TGocciaIdentifierExpression }

constructor TGocciaIdentifierExpression.Create(const AName: string;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FName := AName;
end;

{ TGocciaBinaryExpression }

constructor TGocciaBinaryExpression.Create(const ALeft: TGocciaExpression;
  const AOperator: TGocciaTokenType; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FLeft := ALeft;
  FOperator := AOperator;
  FRight := ARight;
end;

{ TGocciaSequenceExpression }

constructor TGocciaSequenceExpression.Create(const AExpressions: TObjectList<TGocciaExpression>;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FExpressions := AExpressions;
end;

destructor TGocciaSequenceExpression.Destroy;
begin
  if Assigned(FExpressions) then
    FExpressions.OwnsObjects := False;
  FExpressions.Free;
  inherited;
end;

{ TGocciaUnaryExpression }

constructor TGocciaUnaryExpression.Create(const AOperator: TGocciaTokenType;
  const AOperand: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FOperator := AOperator;
  FOperand := AOperand;
end;

{ TGocciaAssignmentExpression }

constructor TGocciaAssignmentExpression.Create(const AName: string;
  const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan;
  const AInferName: Boolean);
begin
  inherited Create(ASpan);
  FName := AName;
  FValue := AValue;
  FInferName := AInferName;
end;

{ TGocciaPropertyAssignmentExpression }

constructor TGocciaPropertyAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPropertyName := APropertyName;
  FValue := AValue;
end;

{ TGocciaComputedPropertyAssignmentExpression }

constructor TGocciaComputedPropertyAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPropertyExpression := APropertyExpression;
  FValue := AValue;
end;

{ TGocciaCompoundAssignmentExpression }

constructor TGocciaCompoundAssignmentExpression.Create(const AName: string;
  const AOperator: TGocciaTokenType; const AValue: TGocciaExpression;
  const ASpan: TGocciaSourceSpan; const AInferName: Boolean);
begin
  inherited Create(ASpan);
  FName := AName;
  FOperator := AOperator;
  FValue := AValue;
  FInferName := AInferName;
end;

{ TGocciaPropertyCompoundAssignmentExpression }

constructor TGocciaPropertyCompoundAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPropertyName := APropertyName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaComputedPropertyCompoundAssignmentExpression }

constructor TGocciaComputedPropertyCompoundAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPropertyExpression := APropertyExpression;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaIncrementExpression }

constructor TGocciaIncrementExpression.Create(const AOperand: TGocciaExpression; const AOperator: TGocciaTokenType; const AIsPrefix: Boolean; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FOperand := AOperand;
  FOperator := AOperator;
  FIsPrefix := AIsPrefix;
end;

{ TGocciaCallExpression }

constructor TGocciaCallExpression.Create(const ACallee: TGocciaExpression;
  const AArguments: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan;
  const AOptional: Boolean = False);
begin
  inherited Create(ASpan);
  FCallee := ACallee;
  FArguments := AArguments;
  FOptional := AOptional;
  FOptionalChainPresence := ocpUnknown;
end;

{ TGocciaMemberExpression }

constructor TGocciaMemberExpression.Create(const AObject: TGocciaExpression;
  const AProperty: string; const AComputed: Boolean; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FProperty := AProperty;
  FPropertyExpression := nil;
  FComputed := AComputed;
  FOptional := AOptional;
end;

constructor TGocciaMemberExpression.Create(const AObject: TGocciaExpression;
  const APropertyExpression: TGocciaExpression; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FProperty := '';
  FPropertyExpression := APropertyExpression;
  FComputed := True;
  FOptional := AOptional;
end;

{ TGocciaArrayExpression }

constructor TGocciaArrayExpression.Create(const AElements: TObjectList<TGocciaExpression>;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FElements := AElements;
end;

{ TGocciaObjectExpression }

constructor TGocciaObjectExpression.Create(const AProperties: TGocciaExpressionMap;
  const APropertyOrder: TStringList;
  const ASpan: TGocciaSourceSpan);
var
  I: Integer;
begin
  inherited Create(ASpan);
  FProperties := AProperties;
  FPropertyInsertionOrder := TStringList.Create;
  FPropertyInsertionOrder.Duplicates := dupIgnore;

  // Use the provided insertion order
  if Assigned(APropertyOrder) then
    for I := 0 to APropertyOrder.Count - 1 do
      FPropertyInsertionOrder.Add(APropertyOrder[I]);

  FComputedProperties := nil; // No computed properties in this constructor
  FGetters := nil; // No getters in this constructor
  FSetters := nil; // No setters in this constructor
end;

constructor TGocciaObjectExpression.Create(const AProperties: TGocciaExpressionMap;
  const APropertyOrder: TStringList;
  const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
  const ASpan: TGocciaSourceSpan);
var
  I: Integer;
begin
  inherited Create(ASpan);
  FProperties := AProperties;
  FPropertyInsertionOrder := TStringList.Create;
  FPropertyInsertionOrder.Duplicates := dupIgnore;

  // Use the provided insertion order
  if Assigned(APropertyOrder) then
    for I := 0 to APropertyOrder.Count - 1 do
      FPropertyInsertionOrder.Add(APropertyOrder[I]);

  FComputedProperties := AComputedProperties;
  FGetters := nil; // No getters in this constructor
  FSetters := nil; // No setters in this constructor
end;

constructor TGocciaObjectExpression.Create(const AProperties: TGocciaExpressionMap;
  const APropertyOrder: TStringList;
  const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
  const AGetters: TGocciaGetterExpressionMap;
  const ASetters: TGocciaSetterExpressionMap;
  const ASpan: TGocciaSourceSpan);
var
  I: Integer;
begin
  inherited Create(ASpan);
  FProperties := AProperties;
  FPropertyInsertionOrder := TStringList.Create;
  FPropertyInsertionOrder.Duplicates := dupIgnore;

  // Use the provided insertion order
  if Assigned(APropertyOrder) then
    for I := 0 to APropertyOrder.Count - 1 do
      FPropertyInsertionOrder.Add(APropertyOrder[I]);

  FComputedProperties := AComputedProperties;
  FGetters := AGetters;
  FSetters := ASetters;
end;

constructor TGocciaObjectExpression.Create(const AProperties: TGocciaExpressionMap;
  const APropertyOrder: TStringList;
  const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
  const AComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
  const AGetters: TGocciaGetterExpressionMap;
  const ASetters: TGocciaSetterExpressionMap;
  const APropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
  const ASpan: TGocciaSourceSpan);
var
  I: Integer;
begin
  inherited Create(ASpan);
  FProperties := AProperties;
  FPropertyInsertionOrder := TStringList.Create;
  FPropertyInsertionOrder.Duplicates := dupIgnore;

  // Use the provided insertion order
  if Assigned(APropertyOrder) then
    for I := 0 to APropertyOrder.Count - 1 do
      FPropertyInsertionOrder.Add(APropertyOrder[I]);

  FComputedProperties := AComputedProperties;
  FComputedPropertiesInOrder := AComputedPropertiesInOrder;
  FGetters := AGetters;
  FSetters := ASetters;
  FPropertySourceOrder := APropertySourceOrder;
end;

destructor TGocciaObjectExpression.Destroy;
begin
  FProperties.Free;
  FPropertyInsertionOrder.Free;
  FComputedProperties.Free;
  FGetters.Free;
  FSetters.Free;
  inherited Destroy;
end;

function TGocciaObjectExpression.GetPropertyNamesInOrder: TStringList;
begin
  Result := FPropertyInsertionOrder;
end;

{ TGocciaArrowFunctionExpression }

constructor TGocciaArrowFunctionExpression.Create(const AParameters: TGocciaParameterArray;
  const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FParameters := AParameters;
  FBody := ABody;
end;

{ TGocciaFunctionExpression }

constructor TGocciaFunctionExpression.Create(const AParameters: TGocciaParameterArray;
  const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FParameters := AParameters;
  FBody := ABody;
  FHasOwnPrototype := False;
  FParsedInStrictMode := False;
end;

{ TGocciaObjectMethodDefinition }

constructor TGocciaObjectMethodDefinition.Create(
  const AFunctionExpression: TGocciaFunctionExpression;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FFunctionExpression := AFunctionExpression;
end;

{ TGocciaAwaitExpression }

constructor TGocciaAwaitExpression.Create(const AOperand: TGocciaExpression;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FOperand := AOperand;
end;

{ TGocciaYieldExpression }

constructor TGocciaYieldExpression.Create(const AOperand: TGocciaExpression;
  const AIsDelegate: Boolean; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FOperand := AOperand;
  FIsDelegate := AIsDelegate;
end;

{ TGocciaConditionalExpression }

constructor TGocciaConditionalExpression.Create(const ACondition, AConsequent,
  AAlternate: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FCondition := ACondition;
  FConsequent := AConsequent;
  FAlternate := AAlternate;
end;

{ TGocciaNewExpression }

constructor TGocciaNewExpression.Create(const ACallee: TGocciaExpression;
  const AArguments: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FCallee := ACallee;
  FArguments := AArguments;
end;

{ TGocciaPrivateMemberExpression }

constructor TGocciaPrivateMemberExpression.Create(const AObject: TGocciaExpression; const APrivateName: string; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPrivateName := APrivateName;
  FOptional := AOptional;
end;

{ TGocciaPrivatePropertyAssignmentExpression }

constructor TGocciaPrivatePropertyAssignmentExpression.Create(const AObject: TGocciaExpression; const APrivateName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPrivateName := APrivateName;
  FValue := AValue;
end;

{ TGocciaPrivatePropertyCompoundAssignmentExpression }

constructor TGocciaPrivatePropertyCompoundAssignmentExpression.Create(const AObject: TGocciaExpression; const APrivateName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FObject := AObject;
  FPrivateName := APrivateName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaHoleExpression }

constructor TGocciaHoleExpression.Create(const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
end;

{ TGocciaSpreadExpression }

constructor TGocciaSpreadExpression.Create(const AArgument: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FArgument := AArgument;
end;

{ TGocciaGetterExpression }

constructor TGocciaGetterExpression.Create(const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FBody := ABody;
end;

{ TGocciaSetterExpression }

constructor TGocciaSetterExpression.Create(const AParameter: string; const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FParameter := AParameter;
  SetLength(FParameters, 1);
  FParameters[0].Name := AParameter;
  FParameters[0].DefaultValue := nil;
  FParameters[0].IsPattern := False;
  FParameters[0].Pattern := nil;
  FParameters[0].IsRest := False;
  FParameters[0].IsOptional := False;
  FParameters[0].TypeAnnotation := '';
  FBody := ABody;
end;

constructor TGocciaSetterExpression.Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FParameters := AParameters;
  if Length(FParameters) > 0 then
    FParameter := FParameters[0].Name
  else
    FParameter := '';
  FBody := ABody;
end;

{ TGocciaDestructuringProperty }

constructor TGocciaDestructuringProperty.Create(const AKey: string; const APattern: TGocciaDestructuringPattern; const AComputed: Boolean; const AKeyExpression: TGocciaExpression);
begin
  FKey := AKey;
  FPattern := APattern;
  FComputed := AComputed;
  FKeyExpression := AKeyExpression;
end;

{ TGocciaArrayPattern }

constructor TGocciaArrayDestructuringPattern.Create(const AElements: TObjectList<TGocciaDestructuringPattern>; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FElements := AElements;
end;

{ TGocciaObjectPattern }

constructor TGocciaObjectDestructuringPattern.Create(const AProperties: TObjectList<TGocciaDestructuringProperty>; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FProperties := AProperties;
end;

{ TGocciaRestPattern }

constructor TGocciaRestDestructuringPattern.Create(const AArgument: TGocciaDestructuringPattern; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FArgument := AArgument;
end;

{ TGocciaAssignmentPattern }

constructor TGocciaAssignmentDestructuringPattern.Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FLeft := ALeft;
  FRight := ARight;
end;

{ TGocciaIdentifierPattern }

constructor TGocciaIdentifierDestructuringPattern.Create(const AName: string; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FName := AName;
end;

{ TGocciaMemberExpressionDestructuringPattern }

constructor TGocciaMemberExpressionDestructuringPattern.Create(const AExpression: TGocciaMemberExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FExpression := AExpression;
end;

{ TGocciaPrivateMemberExpressionDestructuringPattern }

constructor TGocciaPrivateMemberExpressionDestructuringPattern.Create(const AExpression: TGocciaPrivateMemberExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FExpression := AExpression;
end;

{ TGocciaDestructuringAssignmentExpression }

constructor TGocciaDestructuringAssignmentExpression.Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FLeft := ALeft;
  FRight := ARight;
end;

{ TGocciaObjectMatchProperty }

constructor TGocciaObjectMatchProperty.Create(const AKey: string;
  const APattern: TGocciaMatchPattern; const AComputed: Boolean;
  const AKeyExpression: TGocciaExpression);
begin
  FKey := AKey;
  FPattern := APattern;
  FComputed := AComputed;
  FKeyExpression := AKeyExpression;
end;

{ TGocciaValueMatchPattern }

constructor TGocciaValueMatchPattern.Create(const AExpression: TGocciaExpression;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FExpression := AExpression;
end;

{ TGocciaBindingMatchPattern }

constructor TGocciaBindingMatchPattern.Create(const AName: string;
  const ADeclarationType: TGocciaDeclarationType; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FName := AName;
  FDeclarationType := ADeclarationType;
end;

{ TGocciaArrayMatchPattern }

constructor TGocciaArrayMatchPattern.Create(const AElements: TGocciaMatchPatternList;
  const ARestPattern: TGocciaMatchPattern; const AHasRestWildcard: Boolean;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FElements := AElements;
  FRestPattern := ARestPattern;
  FHasRestWildcard := AHasRestWildcard;
end;

{ TGocciaObjectMatchPattern }

constructor TGocciaObjectMatchPattern.Create(const AProperties: TGocciaObjectMatchPropertyList;
  const ARestPattern: TGocciaMatchPattern; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FProperties := AProperties;
  FRestPattern := ARestPattern;
end;

{ TGocciaRelationalMatchPattern }

constructor TGocciaRelationalMatchPattern.Create(const AOperator: TGocciaTokenType;
  const AExpression: TGocciaExpression; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FOperator := AOperator;
  FExpression := AExpression;
end;

{ TGocciaGuardMatchPattern }

constructor TGocciaGuardMatchPattern.Create(const ACondition: TGocciaExpression;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FCondition := ACondition;
end;

{ TGocciaAsMatchPattern }

constructor TGocciaAsMatchPattern.Create(const APattern: TGocciaMatchPattern;
  const AName: string; const ADeclarationType: TGocciaDeclarationType;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FPattern := APattern;
  FName := AName;
  FDeclarationType := ADeclarationType;
end;

{ TGocciaAndMatchPattern }

constructor TGocciaAndMatchPattern.Create(const APatterns: TGocciaMatchPatternList;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FPatterns := APatterns;
end;

{ TGocciaOrMatchPattern }

constructor TGocciaOrMatchPattern.Create(const APatterns: TGocciaMatchPatternList;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FPatterns := APatterns;
end;

{ TGocciaNotMatchPattern }

constructor TGocciaNotMatchPattern.Create(const APattern: TGocciaMatchPattern;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FPattern := APattern;
end;

{ TGocciaExtractorMatchPattern }

constructor TGocciaExtractorMatchPattern.Create(
  const AMatcherExpression: TGocciaExpression; const AArguments: TGocciaMatchPatternList;
  const ARestPattern: TGocciaMatchPattern; const AHasRestWildcard: Boolean;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FMatcherExpression := AMatcherExpression;
  FArguments := AArguments;
  FRestPattern := ARestPattern;
  FHasRestWildcard := AHasRestWildcard;
end;

{ TGocciaMatchClause }

constructor TGocciaMatchClause.Create(const APattern: TGocciaMatchPattern;
  const AExpression: TGocciaExpression);
begin
  FPattern := APattern;
  FExpression := AExpression;
end;

{ TGocciaIsExpression }

constructor TGocciaIsExpression.Create(const ASubject: TGocciaExpression;
  const APattern: TGocciaMatchPattern; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FSubject := ASubject;
  FPattern := APattern;
end;

{ TGocciaMatchExpression }

constructor TGocciaMatchExpression.Create(const ASubject: TGocciaExpression;
  const AClauses: TGocciaMatchClauseList; const ADefaultExpression: TGocciaExpression;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FSubject := ASubject;
  FClauses := AClauses;
  FDefaultExpression := ADefaultExpression;
end;

constructor TGocciaImportCallExpression.Create(const ASpecifier: TGocciaExpression;
  const AOptions: TGocciaExpression; const APhase: TGocciaImportCallPhase;
  const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FSpecifier := ASpecifier;
  FOptions := AOptions;
  FPhase := APhase;
end;

function TGocciaImportCallExpression.HasLiteralSpecifier: Boolean;
begin
  Result := (FSpecifier is TGocciaLiteralExpression) and
    (TGocciaLiteralExpression(FSpecifier).Value is TGocciaStringLiteralValue);
end;

destructor TGocciaTemplateWithInterpolationExpression.Destroy;
begin
  if Assigned(FParts) then
    FParts.OwnsObjects := False;
  FParts.Free;
  inherited;
end;

destructor TGocciaCallExpression.Destroy;
begin
  if Assigned(FArguments) then
    FArguments.OwnsObjects := False;
  FArguments.Free;
  inherited;
end;

destructor TGocciaArrayExpression.Destroy;
begin
  if Assigned(FElements) then
    FElements.OwnsObjects := False;
  FElements.Free;
  inherited;
end;

destructor TGocciaNewExpression.Destroy;
begin
  if Assigned(FArguments) then
    FArguments.OwnsObjects := False;
  FArguments.Free;
  inherited;
end;

destructor TGocciaArrayDestructuringPattern.Destroy;
begin
  if Assigned(FElements) then
    FElements.OwnsObjects := False;
  FElements.Free;
  inherited;
end;

destructor TGocciaObjectDestructuringPattern.Destroy;
begin
  if Assigned(FProperties) then
    FProperties.OwnsObjects := False;
  FProperties.Free;
  inherited;
end;

destructor TGocciaArrayMatchPattern.Destroy;
begin
  if Assigned(FElements) then
    FElements.OwnsObjects := False;
  FElements.Free;
  inherited;
end;

destructor TGocciaObjectMatchPattern.Destroy;
begin
  if Assigned(FProperties) then
    FProperties.OwnsObjects := False;
  FProperties.Free;
  inherited;
end;

destructor TGocciaAndMatchPattern.Destroy;
begin
  if Assigned(FPatterns) then
    FPatterns.OwnsObjects := False;
  FPatterns.Free;
  inherited;
end;

destructor TGocciaOrMatchPattern.Destroy;
begin
  if Assigned(FPatterns) then
    FPatterns.OwnsObjects := False;
  FPatterns.Free;
  inherited;
end;

destructor TGocciaExtractorMatchPattern.Destroy;
begin
  if Assigned(FArguments) then
    FArguments.OwnsObjects := False;
  FArguments.Free;
  inherited;
end;

destructor TGocciaMatchExpression.Destroy;
begin
  if Assigned(FClauses) then
    FClauses.OwnsObjects := False;
  FClauses.Free;
  inherited;
end;

{ ForEachChild }

procedure VisitExpressionList(const AVisitor: TGocciaASTChildVisitor;
  const AList: TObjectList<TGocciaExpression>);
var
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for I := 0 to AList.Count - 1 do
    AVisitor.VisitChild(AList[I]);
end;

procedure VisitMatchPatternList(const AVisitor: TGocciaASTChildVisitor;
  const AList: TGocciaMatchPatternList);
var
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  for I := 0 to AList.Count - 1 do
    AVisitor.VisitChild(AList[I]);
end;

procedure ForEachParameterChild(const AVisitor: TGocciaASTChildVisitor;
  const AParameters: TGocciaParameterArray);
var
  I: Integer;
  Flags: TGocciaASTPartFlags;
begin
  for I := 0 to High(AParameters) do
  begin
    Flags := [];
    if AParameters[I].IsRest then
      Include(Flags, apfRest);
    AVisitor.EnterPart(apkParameter, AParameters[I].StartOffset,
      AParameters[I].EndOffset, Flags);
    AVisitor.VisitChild(AParameters[I].Pattern);
    AVisitor.VisitChild(AParameters[I].DefaultValue);
    AVisitor.LeavePart;
  end;
end;

procedure TGocciaTemplateWithInterpolationExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  { Parts alternate a cooked string and an interpolated expression, starting
    and ending with a string. The strings are the template's own text, not
    nodes in the source, so only the expressions are children. }
  I := 1;
  while I < FParts.Count do
  begin
    AVisitor.VisitChild(FParts[I]);
    Inc(I, 2);
  end;
end;

procedure TGocciaTaggedTemplateExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FTag);
  VisitExpressionList(AVisitor, FExpressions);
end;

procedure TGocciaBinaryExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FLeft);
  AVisitor.VisitChild(FRight);
end;

procedure TGocciaSequenceExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitExpressionList(AVisitor, FExpressions);
end;

procedure TGocciaUnaryExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FOperand);
end;

procedure TGocciaAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(Target);
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaPropertyAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if Assigned(Target) then
    AVisitor.VisitChild(Target)
  else
    AVisitor.VisitChild(FObject);
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaComputedPropertyAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if Assigned(Target) then
    AVisitor.VisitChild(Target)
  else
  begin
    AVisitor.VisitChild(FObject);
    AVisitor.VisitChild(FPropertyExpression);
  end;
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaCompoundAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(Target);
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaPropertyCompoundAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if Assigned(Target) then
    AVisitor.VisitChild(Target)
  else
    AVisitor.VisitChild(FObject);
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaComputedPropertyCompoundAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if Assigned(Target) then
    AVisitor.VisitChild(Target)
  else
  begin
    AVisitor.VisitChild(FObject);
    AVisitor.VisitChild(FPropertyExpression);
  end;
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaPrivatePropertyAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if Assigned(Target) then
    AVisitor.VisitChild(Target)
  else
    AVisitor.VisitChild(FObject);
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaPrivatePropertyCompoundAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  if Assigned(Target) then
    AVisitor.VisitChild(Target)
  else
    AVisitor.VisitChild(FObject);
  AVisitor.VisitChild(FValue);
end;

procedure TGocciaIncrementExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FOperand);
end;

procedure TGocciaCallExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FCallee);
  VisitExpressionList(AVisitor, FArguments);
end;

procedure TGocciaMemberExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FObject);
  AVisitor.VisitChild(FPropertyExpression);
end;

procedure TGocciaPrivateMemberExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FObject);
end;

procedure TGocciaArrayExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitExpressionList(AVisitor, FElements);
end;

procedure TGocciaObjectExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
  Order: TGocciaPropertySourceOrder;
  Key, Value: TGocciaExpression;
  Flags: TGocciaASTPartFlags;
begin
  for I := 0 to High(FPropertySourceOrder) do
  begin
    Order := FPropertySourceOrder[I];
    Key := nil;
    Value := nil;
    Flags := [];
    case Order.PropertyType of
      pstStatic:
        begin
          Value := Order.Expression;
          if Value is TGocciaObjectMethodDefinition then
            Include(Flags, apfMethod)
          else if not Order.UsesColonSyntax then
            Include(Flags, apfShorthand);
        end;
      pstGetter, pstSetter:
        begin
          Value := Order.Accessor;
          Include(Flags, apfMethod);
        end;
      pstComputed, pstComputedGetter, pstComputedSetter:
        begin
          if (Order.ComputedIndex >= 0) and
             (Order.ComputedIndex <= High(FComputedPropertiesInOrder)) then
          begin
            Key := FComputedPropertiesInOrder[Order.ComputedIndex].Key;
            Value := FComputedPropertiesInOrder[Order.ComputedIndex].Value;
          end;
          { A spread is kept as a computed entry with no value. It is an
            element of the literal in its own right, not a key. }
          if (Key is TGocciaSpreadExpression) and not Assigned(Value) then
          begin
            AVisitor.VisitChild(Key);
            Continue;
          end;
          Include(Flags, apfComputed);
          if (Order.PropertyType <> pstComputed) or
             (Value is TGocciaObjectMethodDefinition) then
            Include(Flags, apfMethod);
        end;
    end;
    AVisitor.EnterPart(apkProperty, Order.StartOffset, Order.EndOffset, Flags);
    AVisitor.VisitChild(Key);
    AVisitor.VisitChild(Value);
    AVisitor.LeavePart;
  end;
end;

procedure TGocciaArrowFunctionExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  ForEachParameterChild(AVisitor, FParameters);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaFunctionExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  ForEachParameterChild(AVisitor, FParameters);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaObjectMethodDefinition.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FFunctionExpression);
end;

procedure TGocciaYieldExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FOperand);
end;

procedure TGocciaAwaitExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FOperand);
end;

procedure TGocciaConditionalExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FCondition);
  AVisitor.VisitChild(FConsequent);
  AVisitor.VisitChild(FAlternate);
end;

procedure TGocciaNewExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FCallee);
  VisitExpressionList(AVisitor, FArguments);
end;

procedure TGocciaImportCallExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FSpecifier);
  AVisitor.VisitChild(FOptions);
end;

procedure TGocciaSpreadExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FArgument);
end;

procedure TGocciaGetterExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaSetterExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  ForEachParameterChild(AVisitor, FParameters);
  AVisitor.VisitChild(FBody);
end;

procedure TGocciaArrayDestructuringPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  for I := 0 to FElements.Count - 1 do
    AVisitor.VisitChild(FElements[I]);
end;

procedure TGocciaObjectDestructuringPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
  begin
    AVisitor.VisitChild(FProperties[I].KeyExpression);
    AVisitor.VisitChild(FProperties[I].Pattern);
  end;
end;

procedure TGocciaRestDestructuringPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FArgument);
end;

procedure TGocciaAssignmentDestructuringPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FLeft);
  AVisitor.VisitChild(FRight);
end;

procedure TGocciaMemberExpressionDestructuringPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FExpression);
end;

procedure TGocciaPrivateMemberExpressionDestructuringPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FExpression);
end;

procedure TGocciaDestructuringAssignmentExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FLeft);
  AVisitor.VisitChild(FRight);
end;

procedure TGocciaValueMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FExpression);
end;

procedure TGocciaArrayMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitMatchPatternList(AVisitor, FElements);
  AVisitor.VisitChild(FRestPattern);
end;

procedure TGocciaObjectMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  if Assigned(FProperties) then
    for I := 0 to FProperties.Count - 1 do
    begin
      AVisitor.VisitChild(FProperties[I].KeyExpression);
      AVisitor.VisitChild(FProperties[I].Pattern);
    end;
  AVisitor.VisitChild(FRestPattern);
end;

procedure TGocciaRelationalMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FExpression);
end;

procedure TGocciaGuardMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FCondition);
end;

procedure TGocciaAsMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FPattern);
end;

procedure TGocciaAndMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitMatchPatternList(AVisitor, FPatterns);
end;

procedure TGocciaOrMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  VisitMatchPatternList(AVisitor, FPatterns);
end;

procedure TGocciaNotMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FPattern);
end;

procedure TGocciaExtractorMatchPattern.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FMatcherExpression);
  VisitMatchPatternList(AVisitor, FArguments);
  AVisitor.VisitChild(FRestPattern);
end;

procedure TGocciaIsExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
begin
  AVisitor.VisitChild(FSubject);
  AVisitor.VisitChild(FPattern);
end;

procedure TGocciaMatchExpression.ForEachChild(
  const AVisitor: TGocciaASTChildVisitor);
var
  I: Integer;
begin
  AVisitor.VisitChild(FSubject);
  if Assigned(FClauses) then
    for I := 0 to FClauses.Count - 1 do
    begin
      AVisitor.VisitChild(FClauses[I].Pattern);
      AVisitor.VisitChild(FClauses[I].Expression);
    end;
  AVisitor.VisitChild(FDefaultExpression);
end;

initialization
  CriticalSectionInit(GTemplateSiteIdLock);
  GNextTemplateSiteId := 0;

finalization
  CriticalSectionDone(GTemplateSiteIdLock);

end.
