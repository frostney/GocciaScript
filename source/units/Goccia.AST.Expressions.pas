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
  Goccia.Evaluator.Context,
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
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Value: TGocciaValue read FValue;
    property SourceText: string read FSourceText;
  end;

  TGocciaTemplateLiteralExpression = class(TGocciaExpression)
  private
    FValue: string;
  public
    constructor Create(const AValue: string; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Value: string read FValue;
  end;

  TGocciaRegexLiteralExpression = class(TGocciaExpression)
  private
    FPattern: string;
    FFlags: string;
  public
    constructor Create(const APattern, AFlags: string; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Pattern: string read FPattern;
    property Flags: string read FFlags;
  end;

  TGocciaTemplateWithInterpolationExpression = class(TGocciaExpression)
  private
    FParts: TObjectList<TGocciaExpression>; // Mix of string literals and expressions
  public
    constructor Create(const AParts: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
    // ES2026 §13.2.8.3: cached per-call-site template object (nil until first evaluation)
    FTemplateObject: TGocciaValue;
  public
    constructor Create(const ATag: TGocciaExpression;
      const ACookedStrings, ARawStrings: TGocciaTemplateStrings;
      const ACookedValid: TGocciaTemplateCookedValid;
      const AExpressions: TObjectList<TGocciaExpression>;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    // Update the call-site template object cache: no-op when AValue equals the
    // current cached value; otherwise unpins the old value, pins AValue, and
    // stores it.  Passing nil clears and unpins the cache.
    procedure SetCachedTemplateObject(const AValue: TGocciaValue);
    property Tag: TGocciaExpression read FTag;
    property CookedStrings: TGocciaTemplateStrings read FCookedStrings;
    property RawStrings: TGocciaTemplateStrings read FRawStrings;
    property CookedValid: TGocciaTemplateCookedValid read FCookedValid;
    property Expressions: TObjectList<TGocciaExpression> read FExpressions;
    property TemplateSiteId: UInt64 read FTemplateSiteId;
    property TemplateObject: TGocciaValue read FTemplateObject;
  end;

  TGocciaIdentifierExpression = class(TGocciaExpression)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
  end;

  TGocciaBinaryExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaExpression;
    FOperator: TGocciaTokenType;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaExpression; const AOperator: TGocciaTokenType;
      const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Left: TGocciaExpression read FLeft;
    property Operator: TGocciaTokenType read FOperator;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaSequenceExpression = class(TGocciaExpression)
  private
    FExpressions: TObjectList<TGocciaExpression>;
  public
    constructor Create(const AExpressions: TObjectList<TGocciaExpression>;
      const ASpan: TGocciaSourceSpan);
    destructor Destroy; override;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Expressions: TObjectList<TGocciaExpression> read FExpressions;
  end;

  TGocciaUnaryExpression = class(TGocciaExpression)
  private
    FOperator: TGocciaTokenType;
    FOperand: TGocciaExpression;
  public
    constructor Create(const AOperator: TGocciaTokenType; const AOperand: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Operator: TGocciaTokenType read FOperator;
    property Operand: TGocciaExpression read FOperand;
  end;

  TGocciaAssignmentExpression = class(TGocciaExpression)
  private
    FName: string;
    FValue: TGocciaExpression;
    FInferName: Boolean;
  public
    constructor Create(const AName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan;
      const AInferName: Boolean = True);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
    property Value: TGocciaExpression read FValue;
    property InferName: Boolean read FInferName;
  end;

  TGocciaPropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APropertyName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FPropertyName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaComputedPropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyExpression: TGocciaExpression;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyExpression: TGocciaExpression read FPropertyExpression;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaCompoundAssignmentExpression = class(TGocciaExpression)
  private
    FName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
    FInferName: Boolean;
  public
    constructor Create(const AName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression;
      const ASpan: TGocciaSourceSpan; const AInferName: Boolean = True);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
    property InferName: Boolean read FInferName;
  end;

  TGocciaPropertyCompoundAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APropertyName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FPropertyName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaComputedPropertyCompoundAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyExpression: TGocciaExpression;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
    constructor Create(const AOperand: TGocciaExpression; const AOperator: TGocciaTokenType; const AIsPrefix: Boolean; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Operand: TGocciaExpression read FOperand;
    property Operator: TGocciaTokenType read FOperator;
    property IsPrefix: Boolean read FIsPrefix;
  end;

  TGocciaCallExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
    FOptional: Boolean;
  public
    constructor Create(const ACallee: TGocciaExpression;
      const AArguments: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan;
      const AOptional: Boolean = False);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
    property Optional: Boolean read FOptional;
  end;

  TGocciaMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FProperty: string;
    FPropertyExpression: TGocciaExpression;
    FComputed: Boolean;
    FOptional: Boolean;
  public
    constructor Create(const AObject: TGocciaExpression; const AProperty: string;
      const AComputed: Boolean; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False); overload;
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression;
      const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False); overload;
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FProperty;
    property PropertyExpression: TGocciaExpression read FPropertyExpression;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Computed: Boolean read FComputed;
    property Optional: Boolean read FOptional;
  end;

  TGocciaArrayExpression = class(TGocciaExpression)
  private
    FElements: TObjectList<TGocciaExpression>;
  public
    constructor Create(const AElements: TObjectList<TGocciaExpression>;
      const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ASpan: TGocciaSourceSpan);
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ReturnType: string read FReturnType write FReturnType;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property SourceText: string read FSourceText write FSourceText;
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
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ASpan: TGocciaSourceSpan);
    property Parameters: TGocciaParameterArray read FParameters;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
  end;

  TGocciaObjectMethodDefinition = class(TGocciaExpression)
  private
    FFunctionExpression: TGocciaFunctionExpression;
  public
    constructor Create(const AFunctionExpression: TGocciaFunctionExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property FunctionExpression: TGocciaFunctionExpression read FFunctionExpression;
  end;

  TGocciaYieldExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
    FIsDelegate: Boolean;
  public
    constructor Create(const AOperand: TGocciaExpression; const AIsDelegate: Boolean;
      const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Operand: TGocciaExpression read FOperand;
    property IsDelegate: Boolean read FIsDelegate;
  end;

  TGocciaAwaitExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
  public
    constructor Create(const AOperand: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Operand: TGocciaExpression read FOperand;
  end;

  TGocciaConditionalExpression = class(TGocciaExpression)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaExpression;
    FAlternate: TGocciaExpression;
  public
    constructor Create(const ACondition, AConsequent, AAlternate: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Condition: TGocciaExpression read FCondition;
    property Consequent: TGocciaExpression read FConsequent;
    property Alternate: TGocciaExpression read FAlternate;
  end;

  TGocciaNewExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
  public
    constructor Create(const ACallee: TGocciaExpression;
      const AArguments: TObjectList<TGocciaExpression>; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
  end;

  TGocciaThisExpression = class(TGocciaExpression)
  public
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  TGocciaSuperExpression = class(TGocciaExpression)
  public
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  TGocciaImportMetaExpression = class(TGocciaExpression)
  public
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  TGocciaNewTargetExpression = class(TGocciaExpression)
  public
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  TGocciaImportCallExpression = class(TGocciaExpression)
  private
    FSpecifier: TGocciaExpression;
    FOptions: TGocciaExpression;
    FPhase: TGocciaImportCallPhase;
  public
    constructor Create(const ASpecifier: TGocciaExpression; const AOptions: TGocciaExpression;
      const APhase: TGocciaImportCallPhase; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Specifier: TGocciaExpression read FSpecifier;
    property Options: TGocciaExpression read FOptions;
    property Phase: TGocciaImportCallPhase read FPhase;
  end;

  TGocciaHoleExpression = class(TGocciaExpression)
  public
    constructor Create(const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  TGocciaSpreadExpression = class(TGocciaExpression)
  private
    FArgument: TGocciaExpression;
  public
    constructor Create(const AArgument: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Argument: TGocciaExpression read FArgument;
  end;

  // Getter method: get propertyName() { ... }
  TGocciaGetterExpression = class(TGocciaExpression)
  private
    FBody: TGocciaASTNode;
    FSourceText: string;
  public
    constructor Create(const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Body: TGocciaASTNode read FBody;
    property SourceText: string read FSourceText write FSourceText;
  end;

  // Setter method: set propertyName(value) { ... }
  TGocciaSetterExpression = class(TGocciaExpression)
  private
    FParameter: string;
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FSourceText: string;
  public
    constructor Create(const AParameter: string; const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan); overload;
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode; const ASpan: TGocciaSourceSpan); overload;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Parameter: string read FParameter;
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    property SourceText: string read FSourceText write FSourceText;
  end;

  // Destructuring pattern base class - complete definition
  TGocciaDestructuringPattern = class(TGocciaExpression)
  public
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  // Property in object destructuring: key: pattern or key (shorthand)
  TGocciaDestructuringProperty = class
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
    constructor Create(const AElements: TObjectList<TGocciaDestructuringPattern>; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Elements: TObjectList<TGocciaDestructuringPattern> read FElements;
  end;

  // Object destructuring pattern: {name, age} = object
  TGocciaObjectDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FProperties: TObjectList<TGocciaDestructuringProperty>;
  public
    constructor Create(const AProperties: TObjectList<TGocciaDestructuringProperty>; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Properties: TObjectList<TGocciaDestructuringProperty> read FProperties;
  end;

  // Rest pattern: ...rest
  TGocciaRestDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FArgument: TGocciaDestructuringPattern;
  public
    constructor Create(const AArgument: TGocciaDestructuringPattern; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Argument: TGocciaDestructuringPattern read FArgument;
  end;

  // Assignment pattern with default value: a = defaultValue
  TGocciaAssignmentDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  // Identifier pattern: just a variable name
  TGocciaIdentifierDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
  end;

  // Member expression pattern: obj.prop, this.x, arr[i], obj[key]
  TGocciaMemberExpressionDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FExpression: TGocciaMemberExpression;
  public
    constructor Create(const AExpression: TGocciaMemberExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Expression: TGocciaMemberExpression read FExpression;
  end;

  // Private member expression pattern: this.#x
  TGocciaPrivateMemberExpressionDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FExpression: TGocciaPrivateMemberExpression;
  public
    constructor Create(const AExpression: TGocciaPrivateMemberExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Expression: TGocciaPrivateMemberExpression read FExpression;
  end;

  // Destructuring assignment expression
  TGocciaDestructuringAssignmentExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaObjectMatchProperty = class
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
    FUseSameValueZero: Boolean;
  public
    constructor Create(const AExpression: TGocciaExpression;
      const AUseSameValueZero: Boolean; const ASpan: TGocciaSourceSpan);
    property Expression: TGocciaExpression read FExpression;
    property UseSameValueZero: Boolean read FUseSameValueZero;
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
    constructor Create(const AElements: TGocciaMatchPatternList;
      const ARestPattern: TGocciaMatchPattern; const AHasRestWildcard: Boolean;
      const ASpan: TGocciaSourceSpan);
    property Elements: TGocciaMatchPatternList read FElements;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
    property HasRestWildcard: Boolean read FHasRestWildcard;
  end;

  TGocciaObjectMatchPattern = class(TGocciaMatchPattern)
  private
    FProperties: TGocciaObjectMatchPropertyList;
    FRestPattern: TGocciaMatchPattern;
  public
    constructor Create(const AProperties: TGocciaObjectMatchPropertyList;
      const ARestPattern: TGocciaMatchPattern; const ASpan: TGocciaSourceSpan);
    property Properties: TGocciaObjectMatchPropertyList read FProperties;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
  end;

  TGocciaRelationalMatchPattern = class(TGocciaMatchPattern)
  private
    FOperator: TGocciaTokenType;
    FExpression: TGocciaExpression;
  public
    constructor Create(const AOperator: TGocciaTokenType; const AExpression: TGocciaExpression;
      const ASpan: TGocciaSourceSpan);
    property Operator: TGocciaTokenType read FOperator;
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaGuardMatchPattern = class(TGocciaMatchPattern)
  private
    FCondition: TGocciaExpression;
  public
    constructor Create(const ACondition: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaAsMatchPattern = class(TGocciaMatchPattern)
  private
    FPattern: TGocciaMatchPattern;
    FName: string;
    FDeclarationType: TGocciaDeclarationType;
  public
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
    constructor Create(const APatterns: TGocciaMatchPatternList; const ASpan: TGocciaSourceSpan);
    property Patterns: TGocciaMatchPatternList read FPatterns;
  end;

  TGocciaOrMatchPattern = class(TGocciaMatchPattern)
  private
    FPatterns: TGocciaMatchPatternList;
  public
    constructor Create(const APatterns: TGocciaMatchPatternList; const ASpan: TGocciaSourceSpan);
    property Patterns: TGocciaMatchPatternList read FPatterns;
  end;

  TGocciaNotMatchPattern = class(TGocciaMatchPattern)
  private
    FPattern: TGocciaMatchPattern;
  public
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
    constructor Create(const AMatcherExpression: TGocciaExpression;
      const AArguments: TGocciaMatchPatternList; const ARestPattern: TGocciaMatchPattern;
      const AHasRestWildcard: Boolean; const ASpan: TGocciaSourceSpan);
    property MatcherExpression: TGocciaExpression read FMatcherExpression;
    property Arguments: TGocciaMatchPatternList read FArguments;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
    property HasRestWildcard: Boolean read FHasRestWildcard;
  end;

  TGocciaMatchClause = class
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
    constructor Create(const ASubject: TGocciaExpression; const APattern: TGocciaMatchPattern;
      const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Subject: TGocciaExpression read FSubject;
    property Pattern: TGocciaMatchPattern read FPattern;
  end;

  TGocciaMatchExpression = class(TGocciaExpression)
  private
    FSubject: TGocciaExpression;
    FClauses: TGocciaMatchClauseList;
    FDefaultExpression: TGocciaExpression;
  public
    constructor Create(const ASubject: TGocciaExpression; const AClauses: TGocciaMatchClauseList;
      const ADefaultExpression: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
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
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const ASpan: TGocciaSourceSpan; const AOptional: Boolean = False);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Optional: Boolean read FOptional;
  end;

  TGocciaPrivatePropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaPrivatePropertyCompoundAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ASpan: TGocciaSourceSpan);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaDecoratorList = array of TGocciaExpression;

implementation

uses
  SysUtils,

  Goccia.Arithmetic,
  Goccia.AST.Statements,
  Goccia.Constants.ErrorNames,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator,
  Goccia.Evaluator.Assignment,
  Goccia.Evaluator.PatternMatching,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.InstructionLimit,
  Goccia.RegExp.Runtime,
  Goccia.Scope,
  Goccia.Timeout,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToPrimitive;

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

function IsNullishAssignmentValue(const AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := not Assigned(AValue) or
            (AValue is TGocciaUndefinedLiteralValue) or
            (AValue is TGocciaNullLiteralValue);
end;

function IsAnonymousFunctionNameExpression(
  const AExpression: TGocciaExpression): Boolean;
begin
  Result := (AExpression is TGocciaArrowFunctionExpression) or
    ((AExpression is TGocciaFunctionExpression) and
     (TGocciaFunctionExpression(AExpression).Name = '')) or
    ((AExpression is TGocciaClassExpression) and
     (TGocciaClassExpression(AExpression).ClassDefinition.Name = ''));
end;

function EvaluateWithInferredName(const AExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext; const AName: string): TGocciaValue;
begin
  Result := AExpression.Evaluate(AContext);
  if not IsAnonymousFunctionNameExpression(AExpression) then
    Exit;

  if Result is TGocciaFunctionValue then
    TGocciaFunctionValue(Result).SetInferredName(AName)
  else if Result is TGocciaClassValue then
    TGocciaClassValue(Result).SetInferredName(AName);
end;

function NormalizeAssignmentValue(const AValue: TGocciaValue): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  if Assigned(AValue) then
    Result := AValue
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ TGocciaLiteralExpression }

constructor TGocciaLiteralExpression.Create(const AValue: TGocciaValue;
  const ASpan: TGocciaSourceSpan; const ASourceText: string);
begin
  inherited Create(ASpan);
  FValue := AValue;
  FSourceText := ASourceText;
  // Remove from GC -- the AST owns this value, not the garbage collector.
  // Singletons (UndefinedValue, TrueValue, FalseValue) are pinned separately
  // and safe to unregister (Remove is a no-op if not found).
  if Assigned(FValue) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.UnregisterObject(FValue);
end;

destructor TGocciaLiteralExpression.Destroy;
begin
  // Free the owned value unless it's a singleton (singletons are process-lifetime)
  if Assigned(FValue)
     and (FValue <> TGocciaUndefinedLiteralValue.UndefinedValue)
     and (FValue <> TGocciaBooleanLiteralValue.TrueValue)
     and (FValue <> TGocciaBooleanLiteralValue.FalseValue) then
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
  FTag.Free;
  FExpressions.Free;
  // Release the GC pin so the template object can be collected once no live
  // references remain.  Guard against the GC already having been shut down.
  if Assigned(FTemplateObject) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.UnpinObject(FTemplateObject);
  inherited;
end;

// ES2026 §13.2.8.3 GetTemplateObject(templateLiteral)
procedure TGocciaTaggedTemplateExpression.SetCachedTemplateObject(const AValue: TGocciaValue);
begin
  if AValue = FTemplateObject then
    Exit;
  if Assigned(FTemplateObject) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.UnpinObject(FTemplateObject);
  if Assigned(AValue) and (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.PinObject(AValue);
  FTemplateObject := AValue;
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
  FPropertyInsertionOrder.Free;
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
  const AUseSameValueZero: Boolean; const ASpan: TGocciaSourceSpan);
begin
  inherited Create(ASpan);
  FExpression := AExpression;
  FUseSameValueZero := AUseSameValueZero;
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

{ Evaluate overrides }

function TGocciaLiteralExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := Value.RuntimeCopy;
end;

function TGocciaTemplateLiteralExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateTemplateLiteral(Self, AContext);
end;

// ES2026 §13.2.7.1 Runtime Semantics: Evaluation
function TGocciaRegexLiteralExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := CreateRegExpObject(FPattern, FFlags);
end;

function TGocciaTemplateWithInterpolationExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateTemplateWithInterpolation(Self, AContext);
end;

// ES2026 §13.3.11 Runtime Semantics: Evaluation — Tagged Templates
function TGocciaTaggedTemplateExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateTaggedTemplate(Self, AContext);
end;

function TGocciaIdentifierExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AContext.Scope.GetValue(Name);
end;

function TGocciaBinaryExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateBinary(Self, AContext);
end;

// ES2026 §13.16 Comma Operator (,)
function TGocciaSequenceExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  for I := 0 to FExpressions.Count - 1 do
    Result := EvaluateExpression(FExpressions[I], AContext);
end;

function TGocciaUnaryExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateUnary(Self, AContext);
end;

function TGocciaAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectBinding: TGocciaObjectValue;
  ScopeBinding: TGocciaScope;
begin
  AContext.Scope.ResolveAssignmentTarget(Name, ObjectBinding, ScopeBinding);
  if InferName then
    Result := EvaluateWithInferredName(Value, AContext, Name)
  else
    Result := Value.Evaluate(AContext);

  if Assigned(ObjectBinding) then
  begin
    if AContext.NonStrictMode then
      ObjectBinding.AssignPropertyWithReceiver(Name, Result, ObjectBinding)
    else
      ObjectBinding.AssignProperty(Name, Result);
    Exit;
  end;

  ScopeBinding.AssignBinding(Name, Result, Line, Column,
    AContext.NonStrictMode);
end;

function TGocciaPropertyAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaValue;
begin
  if ObjectExpr is TGocciaSuperExpression then
  begin
    Result := Value.Evaluate(AContext);
    AssignSuperProperty(AContext,
      TGocciaStringLiteralValue.Create(PropertyName), Result);
    Exit;
  end;

  Obj := ObjectExpr.Evaluate(AContext);
  Result := Value.Evaluate(AContext);
  AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column,
    AContext.NonStrictMode);
end;

function TGocciaComputedPropertyAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, PropertyValue: TGocciaValue;
  PropName: string;
begin
  if ObjectExpr is TGocciaSuperExpression then
  begin
    PropertyValue := PropertyExpression.Evaluate(AContext);
    Result := Value.Evaluate(AContext);
    AssignSuperProperty(AContext, PropertyValue, Result);
    Exit;
  end;

  Obj := ObjectExpr.Evaluate(AContext);
  PropertyValue := ToPropertyKey(PropertyExpression.Evaluate(AContext));
  Result := Value.Evaluate(AContext);
  if PropertyValue is TGocciaSymbolValue then
    AssignSymbolProperty(Obj, TGocciaSymbolValue(PropertyValue),
      Result, AContext.OnError, Line, Column, AContext.NonStrictMode)
  else
  begin
    PropName := TGocciaStringLiteralValue(PropertyValue).Value;
    AssignProperty(Obj, PropName, Result, AContext.OnError, Line, Column,
      AContext.NonStrictMode);
  end;
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
function TGocciaCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  CurrentValue, RhsValue: TGocciaValue;
  ObjectBinding: TGocciaObjectValue;
  ScopeBinding: TGocciaScope;

  procedure AssignResolvedTarget(const AValue: TGocciaValue);
  begin
    if Assigned(ObjectBinding) then
    begin
      if AContext.NonStrictMode then
        ObjectBinding.AssignPropertyWithReceiver(Name, AValue, ObjectBinding)
      else
        ObjectBinding.AssignProperty(Name, AValue);
      Exit;
    end;

    ScopeBinding.AssignBinding(Name, AValue, Line, Column,
      AContext.NonStrictMode);
  end;
begin
  AContext.Scope.ResolveAssignmentTarget(Name, ObjectBinding, ScopeBinding);
  if Assigned(ObjectBinding) then
    CurrentValue := NormalizeAssignmentValue(ObjectBinding.GetProperty(Name))
  else
    CurrentValue := ScopeBinding.GetValue(Name);

  // ES2026 §13.15.2 step 3: ??=
  if Operator = gttNullishCoalescingAssign then
  begin
    if not IsNullishAssignmentValue(CurrentValue) then
      Exit(CurrentValue);

    if InferName then
      Result := EvaluateWithInferredName(Value, AContext, Name)
    else
      Result := Value.Evaluate(AContext);
    AssignResolvedTarget(Result);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: &&=
  if Operator = gttLogicalAndAssign then
  begin
    if not CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    if InferName then
      Result := EvaluateWithInferredName(Value, AContext, Name)
    else
      Result := Value.Evaluate(AContext);
    AssignResolvedTarget(Result);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: ||=
  if Operator = gttLogicalOrAssign then
  begin
    if CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    if InferName then
      Result := EvaluateWithInferredName(Value, AContext, Name)
    else
      Result := Value.Evaluate(AContext);
    AssignResolvedTarget(Result);
    Exit;
  end;

  Result := CurrentValue;
  RhsValue := Value.Evaluate(AContext);
  Result := Goccia.Arithmetic.CompoundOperations(
      Result, RhsValue, Operator);
  AssignResolvedTarget(Result);
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
function TGocciaPropertyCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, PropertyKeyValue, CurrentValue, RhsValue: TGocciaValue;
begin
  if ObjectExpr is TGocciaSuperExpression then
  begin
    PropertyKeyValue := TGocciaStringLiteralValue.Create(PropertyName);
    CurrentValue := NormalizeAssignmentValue(GetSuperProperty(AContext,
      PropertyKeyValue));
    if Operator = gttNullishCoalescingAssign then
    begin
      if not IsNullishAssignmentValue(CurrentValue) then
        Exit(CurrentValue);

      Result := Value.Evaluate(AContext);
      AssignSuperProperty(AContext, PropertyKeyValue, Result);
      Exit;
    end;

    if Operator = gttLogicalAndAssign then
    begin
      if not CurrentValue.ToBooleanLiteral.Value then
        Exit(CurrentValue);

      Result := Value.Evaluate(AContext);
      AssignSuperProperty(AContext, PropertyKeyValue, Result);
      Exit;
    end;

    if Operator = gttLogicalOrAssign then
    begin
      if CurrentValue.ToBooleanLiteral.Value then
        Exit(CurrentValue);

      Result := Value.Evaluate(AContext);
      AssignSuperProperty(AContext, PropertyKeyValue, Result);
      Exit;
    end;

    RhsValue := Value.Evaluate(AContext);
    Result := Goccia.Arithmetic.CompoundOperations(CurrentValue, RhsValue,
      Operator);
    AssignSuperProperty(AContext, PropertyKeyValue, Result);
    Exit;
  end;

  Obj := ObjectExpr.Evaluate(AContext);
  CurrentValue := NormalizeAssignmentValue(Obj.GetProperty(PropertyName));
  // ES2026 §13.15.2 step 3: ??=
  if Operator = gttNullishCoalescingAssign then
  begin
    if not IsNullishAssignmentValue(CurrentValue) then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column,
      AContext.NonStrictMode);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: &&=
  if Operator = gttLogicalAndAssign then
  begin
    if not CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column,
      AContext.NonStrictMode);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: ||=
  if Operator = gttLogicalOrAssign then
  begin
    if CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column,
      AContext.NonStrictMode);
    Exit;
  end;

  RhsValue := Value.Evaluate(AContext);
  Result := PerformPropertyCompoundAssignment(Obj, PropertyName, RhsValue,
    Operator, AContext.OnError, Line, Column, AContext.NonStrictMode);
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
function TGocciaComputedPropertyCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, PropertyKeyValue, CurrentValue, RhsValue: TGocciaValue;
  BoxedValue: TGocciaObjectValue;
  PropName: string;

  function ShortCircuits: Boolean;
  begin
    case Operator of
      gttNullishCoalescingAssign: Result := not IsNullishAssignmentValue(CurrentValue);
      gttLogicalAndAssign: Result := not CurrentValue.ToBooleanLiteral.Value;
      gttLogicalOrAssign: Result := CurrentValue.ToBooleanLiteral.Value;
    else
      Result := False;
    end;
  end;

  function IsShortCircuitOperator: Boolean;
  begin
    Result := Operator in [gttNullishCoalescingAssign, gttLogicalAndAssign, gttLogicalOrAssign];
  end;

  function ReadSymbolProperty(const AObj: TGocciaValue; const ASymbol: TGocciaSymbolValue): TGocciaValue;
  begin
    if AObj is TGocciaNullLiteralValue then
      ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, [ASymbol.ToDisplayString.Value]),
        SSuggestCheckNullBeforeAccess)
    else if AObj is TGocciaUndefinedLiteralValue then
      ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined, [ASymbol.ToDisplayString.Value]),
        SSuggestCheckNullBeforeAccess);

    if AObj is TGocciaClassValue then
      Result := TGocciaClassValue(AObj).GetSymbolProperty(ASymbol)
    else if AObj is TGocciaObjectValue then
      Result := TGocciaObjectValue(AObj).GetSymbolProperty(ASymbol)
    else if AObj is TGocciaSymbolValue then
    begin
      if TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue then
        Result := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
          .GetSymbolPropertyWithReceiver(ASymbol, AObj)
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
    begin
      BoxedValue := AObj.Box;
      if Assigned(BoxedValue) then
        Result := BoxedValue.GetSymbolPropertyWithReceiver(ASymbol, AObj)
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  end;

begin
  if ObjectExpr is TGocciaSuperExpression then
  begin
    PropertyKeyValue := ToPropertyKey(PropertyExpression.Evaluate(AContext));
    CurrentValue := NormalizeAssignmentValue(GetSuperProperty(AContext,
      PropertyKeyValue));

    if IsShortCircuitOperator then
    begin
      if ShortCircuits then
        Exit(CurrentValue);

      Result := Value.Evaluate(AContext);
      AssignSuperProperty(AContext, PropertyKeyValue, Result);
      Exit;
    end;

    RhsValue := Value.Evaluate(AContext);
    Result := Goccia.Arithmetic.CompoundOperations(CurrentValue, RhsValue,
      Operator);
    AssignSuperProperty(AContext, PropertyKeyValue, Result);
    Exit;
  end;

  Obj := ObjectExpr.Evaluate(AContext);
  PropertyKeyValue := ToPropertyKey(PropertyExpression.Evaluate(AContext));
  if PropertyKeyValue is TGocciaSymbolValue then
  begin
    CurrentValue := NormalizeAssignmentValue(ReadSymbolProperty(Obj,
      TGocciaSymbolValue(PropertyKeyValue)));

    if IsShortCircuitOperator then
    begin
      if ShortCircuits then
        Exit(CurrentValue);

      Result := Value.Evaluate(AContext);
      AssignSymbolProperty(Obj, TGocciaSymbolValue(PropertyKeyValue),
        Result, AContext.OnError, Line, Column, AContext.NonStrictMode);
      Exit;
    end;

    RhsValue := Value.Evaluate(AContext);
    Result := PerformSymbolPropertyCompoundAssignment(Obj,
      TGocciaSymbolValue(PropertyKeyValue), RhsValue, Operator,
      AContext.OnError, Line, Column, AContext.NonStrictMode);
    Exit;
  end;

  PropName := TGocciaStringLiteralValue(PropertyKeyValue).Value;
  CurrentValue := NormalizeAssignmentValue(Obj.GetProperty(PropName));
  if IsShortCircuitOperator then
  begin
    if ShortCircuits then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropName, Result, AContext.OnError, Line, Column,
      AContext.NonStrictMode);
    Exit;
  end;

  RhsValue := Value.Evaluate(AContext);
  Result := PerformPropertyCompoundAssignment(Obj, PropName, RhsValue,
    Operator, AContext.OnError, Line, Column, AContext.NonStrictMode);
end;

function ToNumericValue(const AValue: TGocciaValue): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := ToPrimitive(AValue, tphNumber);
  if not (Result is TGocciaBigIntValue) then
    Result := Result.ToNumberLiteral;
end;

function TGocciaIncrementExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, OldValue, NewValue, PropertyKeyValue: TGocciaValue;
  MemberExpr: TGocciaMemberExpression;
  PrivateExpr: TGocciaPrivateMemberExpression;
  PropName: string;
begin
  if Operand is TGocciaIdentifierExpression then
  begin
    PropName := TGocciaIdentifierExpression(Operand).Name;
    OldValue := ToNumericValue(AContext.Scope.GetValue(PropName));
    NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
    AContext.Scope.AssignBinding(PropName, NewValue, Line, Column,
      AContext.NonStrictMode);
    if IsPrefix then
      Result := NewValue
    else
      Result := OldValue;
  end
  else if Operand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(Operand);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      if MemberExpr.Computed then
        PropertyKeyValue := ToPropertyKey(
          MemberExpr.PropertyExpression.Evaluate(AContext))
      else
        PropertyKeyValue := TGocciaStringLiteralValue.Create(
          MemberExpr.PropertyName);
      OldValue := ToNumericValue(NormalizeAssignmentValue(
        GetSuperProperty(AContext, PropertyKeyValue)));
      NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
      AssignSuperProperty(AContext, PropertyKeyValue, NewValue);
      if IsPrefix then
        Result := NewValue
      else
        Result := OldValue;
      Exit;
    end;

    Obj := MemberExpr.ObjectExpr.Evaluate(AContext);
    if MemberExpr.Computed then
    begin
      PropertyKeyValue := ToPropertyKey(MemberExpr.PropertyExpression.Evaluate(AContext));
      if (PropertyKeyValue is TGocciaSymbolValue) and ((Obj is TGocciaClassValue) or (Obj is TGocciaObjectValue)) then
      begin
        if Obj is TGocciaClassValue then
          OldValue := TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue))
        else
          OldValue := TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue));
        if OldValue = nil then
          OldValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        OldValue := ToNumericValue(OldValue);
        NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
        AssignSymbolProperty(Obj, TGocciaSymbolValue(PropertyKeyValue),
          NewValue, AContext.OnError, Line, Column, AContext.NonStrictMode);
        if IsPrefix then
          Result := NewValue
        else
          Result := OldValue;
        Exit;
      end;
      PropName := PropertyKeyValue.ToStringLiteral.Value;
    end
    else
      PropName := MemberExpr.PropertyName;
    OldValue := Obj.GetProperty(PropName);
    if OldValue = nil then
      OldValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    OldValue := ToNumericValue(OldValue);
    NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
    AssignProperty(Obj, PropName, NewValue, AContext.OnError, Line, Column,
      AContext.NonStrictMode);
    if IsPrefix then
      Result := NewValue
    else
      Result := OldValue;
  end
  else if Operand is TGocciaPrivateMemberExpression then
  begin
    PrivateExpr := TGocciaPrivateMemberExpression(Operand);
    OldValue := ToNumericValue(EvaluatePrivateMember(PrivateExpr, AContext,
      Obj));
    NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
    AssignPrivateMemberValue(Obj, PrivateExpr.PrivateName, NewValue, AContext,
      Line, Column);
    if IsPrefix then
      Result := NewValue
    else
      Result := OldValue;
  end
  else
  begin
    AContext.OnError('Invalid target for increment/decrement', Line, Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaCallExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateCall(Self, AContext);
end;

function TGocciaMemberExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateMember(Self, AContext);
end;

function TGocciaArrayExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateArray(Self, AContext);
end;

function TGocciaObjectExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateObject(Self, AContext);
end;

function TGocciaFunctionExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateFunctionExpression(Self, AContext);
end;

function TGocciaObjectMethodDefinition.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateFunctionExpression(FFunctionExpression, AContext);
end;

function TGocciaAwaitExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateAwait(Self, AContext);
end;

function TGocciaYieldExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateYield(Self, AContext);
end;

function TGocciaArrowFunctionExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateArrowFunction(Self, AContext);
end;

function TGocciaConditionalExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ConditionResult: Boolean;
begin
  ConditionResult := Condition.Evaluate(AContext).ToBooleanLiteral.Value;
  if AContext.CoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) then
  begin
    if ConditionResult then
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, Line, Column, 0)
    else
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, Line, Column, 1);
  end;
  if ConditionResult then
    Result := Consequent.Evaluate(AContext)
  else
    Result := Alternate.Evaluate(AContext);
end;

function TGocciaNewExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateNewExpression(Self, AContext);
end;

function TGocciaThisExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AContext.Scope.ThisValue;
end;

function TGocciaSuperExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AContext.Scope.FindSuperClass;
  if not Assigned(Result) then
    ThrowTypeError('super can only be used in a class method');
end;

// ES2026 §13.3.12.1 ImportMeta : import . meta
function TGocciaImportMetaExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := GetOrCreateImportMeta(AContext.CurrentFilePath,
    AContext.ResolveModuleURL);
end;

// ES2026 §13.3.12.1 NewTarget : new . target
function TGocciaNewTargetExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := AContext.Scope.FindNewTarget;
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
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

// ES2026 §13.3.10.1 Runtime Semantics: Evaluation — ImportCall
function TGocciaImportCallExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AttributeType: string;
  DeferredLoader: TLoadDeferredModuleCallback;
  HasAttributeType: Boolean;
  SourceLoader: TLoadModuleSourceCallback;
  OptionsValue: TGocciaValue;
  SpecifierString: string;
  SpecifierValue: TGocciaValue;
  Module: TGocciaModule;
  Promise: TGocciaPromiseValue;
  GC: TGarbageCollector;
begin
  Promise := TGocciaPromiseValue.Create;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(Promise);
  try
    try
      // ES2026 §13.3.10.1 step 3: Evaluate specifier
      SpecifierValue := EvaluateExpression(FSpecifier, AContext);
      SpecifierString := ToPrimitive(SpecifierValue, tphString).ToStringLiteral.Value;
      OptionsValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FOptions) then
        OptionsValue := EvaluateExpression(FOptions, AContext);
      TryReadImportAttributeType(OptionsValue, AttributeType,
        HasAttributeType);
      if HasAttributeType and (AttributeType <> 'json') and
         (AttributeType <> 'text') and (AttributeType <> 'bytes') then
        raise TGocciaTypeError.Create(
          'Unsupported import attribute type "' + AttributeType + '"',
          Line, Column, '', nil,
          'Use type "json", "text", or "bytes" for import attributes.');

      case FPhase of
        icpSource:
          begin
            SourceLoader := AContext.LoadModuleSource;
            if (not Assigned(SourceLoader)) and Assigned(AContext.Scope) then
              SourceLoader := AContext.Scope.LoadModuleSource;
            if not Assigned(SourceLoader) then
              raise Exception.Create('Module source loader is not available.');
            Promise.Resolve(SourceLoader(
              EncodeImportSpecifierAttribute(
              SpecifierString, AttributeType),
              AContext.CurrentFilePath));
          end;
        icpDefer:
          begin
            DeferredLoader := AContext.LoadDeferredModule;
            if (not Assigned(DeferredLoader)) and Assigned(AContext.Scope) then
              DeferredLoader := AContext.Scope.LoadDeferredModule;
            if not Assigned(DeferredLoader) then
              raise Exception.Create('Deferred module loader is not available.');
            Promise.Resolve(DeferredLoader(
              EncodeImportSpecifierAttribute(
              SpecifierString, AttributeType),
              AContext.CurrentFilePath));
          end;
      else
        // ES2026 §13.3.10.1 step 6-7: HostLoadImportedModule
        Module := AContext.LoadModule(EncodeImportSpecifierAttribute(
          SpecifierString, AttributeType),
          AContext.CurrentFilePath);
        // ES2026 §13.3.10.1 step 11: Resolve promise with namespace
        Promise.Resolve(Module.GetNamespaceObject);
      end;
    except
      on E: TGocciaThrowValue do
        Promise.Reject(E.Value);
      on E: TGocciaSyntaxError do
        Promise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
      on E: TGocciaTypeError do
        Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
      on E: TGocciaReferenceError do
        Promise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
      on E: TGocciaTimeoutError do
        raise;
      on E: TGocciaInstructionLimitError do
        raise;
      on E: Exception do
        Promise.Reject(CreateErrorObject(ERROR_NAME, E.Message));
    end;
    Result := Promise;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Promise);
  end;
end;

function TGocciaHoleExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := nil;
end;

function TGocciaSpreadExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  AContext.OnError('Unexpected spread syntax', Line, Column);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGetterExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateGetter(Self, AContext);
end;

function TGocciaSetterExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateSetter(Self, AContext);
end;

function TGocciaDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaArrayDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaObjectDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaRestDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaAssignmentDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaIdentifierDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaMemberExpressionDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaPrivateMemberExpressionDestructuringPattern.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaDestructuringAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateDestructuringAssignment(Self, AContext);
end;

function TGocciaIsExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateIsExpression(Self, AContext);
end;

function TGocciaMatchExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateMatchExpression(Self, AContext);
end;

function TGocciaPrivateMemberExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluatePrivateMember(Self, AContext);
end;

function TGocciaPrivatePropertyAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluatePrivatePropertyAssignment(Self, AContext);
end;

function TGocciaPrivatePropertyCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluatePrivatePropertyCompoundAssignment(Self, AContext);
end;

initialization
  CriticalSectionInit(GTemplateSiteIdLock);
  GNextTemplateSiteId := 0;

finalization
  CriticalSectionDone(GTemplateSiteIdLock);

end.
