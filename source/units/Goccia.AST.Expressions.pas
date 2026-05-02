unit Goccia.AST.Expressions;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  HashMap,
  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Evaluator.Context,
  Goccia.Scope.BindingMap,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  // Forward declarations
  TGocciaDestructuringPattern = class;
  TGocciaGetterExpression = class;
  TGocciaMatchPattern = class;
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
  public
    constructor Create(const AValue: TGocciaValue; const ALine, AColumn: Integer);
    destructor Destroy; override;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Value: TGocciaValue read FValue;
  end;

  TGocciaTemplateLiteralExpression = class(TGocciaExpression)
  private
    FValue: string;
  public
    constructor Create(const AValue: string; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Value: string read FValue;
  end;

  TGocciaRegexLiteralExpression = class(TGocciaExpression)
  private
    FPattern: string;
    FFlags: string;
  public
    constructor Create(const APattern, AFlags: string; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Pattern: string read FPattern;
    property Flags: string read FFlags;
  end;

  TGocciaTemplateWithInterpolationExpression = class(TGocciaExpression)
  private
    FParts: TObjectList<TGocciaExpression>; // Mix of string literals and expressions
  public
    constructor Create(const AParts: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer);
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
    // ES2026 §13.2.8.3: cached per-call-site template object (nil until first evaluation)
    FTemplateObject: TGocciaValue;
  public
    constructor Create(const ATag: TGocciaExpression;
      const ACookedStrings, ARawStrings: TGocciaTemplateStrings;
      const ACookedValid: TGocciaTemplateCookedValid;
      const AExpressions: TObjectList<TGocciaExpression>;
      const ALine, AColumn: Integer);
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
    property TemplateObject: TGocciaValue read FTemplateObject;
  end;

  TGocciaIdentifierExpression = class(TGocciaExpression)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ALine, AColumn: Integer);
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
      const ARight: TGocciaExpression; const ALine, AColumn: Integer);
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
      const ALine, AColumn: Integer);
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
      const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Operator: TGocciaTokenType read FOperator;
    property Operand: TGocciaExpression read FOperand;
  end;

  TGocciaAssignmentExpression = class(TGocciaExpression)
  private
    FName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaPropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APropertyName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
  public
    constructor Create(const AName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaPropertyCompoundAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APropertyName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
    constructor Create(const AOperand: TGocciaExpression; const AOperator: TGocciaTokenType; const AIsPrefix: Boolean; const ALine, AColumn: Integer);
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
      const AArguments: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer;
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
      const AComputed: Boolean; const ALine, AColumn: Integer; const AOptional: Boolean = False); overload;
    constructor Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression;
      const ALine, AColumn: Integer; const AOptional: Boolean = False); overload;
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
      const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Elements: TObjectList<TGocciaExpression> read FElements;
  end;

  // Property source order tracking
  TGocciaPropertySourceType = (
    pstStatic,    // Regular property: {key: value}
    pstComputed,  // Computed property: {[expr]: value}
    pstGetter,    // Getter: {get prop() {}}
    pstSetter     // Setter: {set prop(val) {}}
  );

  TGocciaPropertySourceOrder = record
    PropertyType: TGocciaPropertySourceType;
    StaticKey: string;           // For static properties, getters, setters
    ComputedIndex: Integer;      // Index into ComputedProperties list
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
      const ALine, AColumn: Integer); overload;
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
      const ALine, AColumn: Integer); overload;
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
      const AGetters: TGocciaGetterExpressionMap;
      const ASetters: TGocciaSetterExpressionMap;
      const ALine, AColumn: Integer); overload;
    // New constructor with source order tracking
    constructor Create(const AProperties: TGocciaExpressionMap;
      const APropertyOrder: TStringList;
      const AComputedProperties: THashMap<TGocciaExpression, TGocciaExpression>;
      const AComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
      const AGetters: TGocciaGetterExpressionMap;
      const ASetters: TGocciaSetterExpressionMap;
      const APropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
      const ALine, AColumn: Integer); overload;
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
      const ALine, AColumn: Integer);
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ReturnType: string read FReturnType write FReturnType;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property SourceText: string read FSourceText write FSourceText;
  end;

  TGocciaMethodExpression = class(TGocciaExpression)
  private
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
    FIsAsync: Boolean;
    FIsGenerator: Boolean;
    FHasOwnPrototype: Boolean;
    FSourceText: string;
    FName: string;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ALine, AColumn: Integer);
    property Parameters: TGocciaParameterArray read FParameters;
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Body: TGocciaASTNode read FBody;
    property IsAsync: Boolean read FIsAsync write FIsAsync;
    property IsGenerator: Boolean read FIsGenerator write FIsGenerator;
    // True when this method node represents a `function`/`function*` declaration
    // or expression (or async generator) that, per ES2026 §10.2.5 MakeConstructor,
    // requires its own `prototype` data property pointing to a fresh object whose
    // `constructor` back-references the function. False for concise methods,
    // arrow functions, getters/setters, and plain async functions.
    property HasOwnPrototype: Boolean read FHasOwnPrototype write FHasOwnPrototype;
    property SourceText: string read FSourceText write FSourceText;
    property Name: string read FName write FName;
  end;

  TGocciaYieldExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
    FIsDelegate: Boolean;
  public
    constructor Create(const AOperand: TGocciaExpression; const AIsDelegate: Boolean;
      const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Operand: TGocciaExpression read FOperand;
    property IsDelegate: Boolean read FIsDelegate;
  end;

  TGocciaAwaitExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
  public
    constructor Create(const AOperand: TGocciaExpression; const ALine, AColumn: Integer);
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
      const ALine, AColumn: Integer);
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
      const AArguments: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer);
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

  TGocciaImportCallExpression = class(TGocciaExpression)
  private
    FSpecifier: TGocciaExpression;
  public
    constructor Create(const ASpecifier: TGocciaExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Specifier: TGocciaExpression read FSpecifier;
  end;

  TGocciaHoleExpression = class(TGocciaExpression)
  public
    constructor Create(const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
  end;

  TGocciaSpreadExpression = class(TGocciaExpression)
  private
    FArgument: TGocciaExpression;
  public
    constructor Create(const AArgument: TGocciaExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Argument: TGocciaExpression read FArgument;
  end;

  // Getter method: get propertyName() { ... }
  TGocciaGetterExpression = class(TGocciaExpression)
  private
    FBody: TGocciaASTNode;
    FSourceText: string;
  public
    constructor Create(const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Body: TGocciaASTNode read FBody;
    property SourceText: string read FSourceText write FSourceText;
  end;

  // Setter method: set propertyName(value) { ... }
  TGocciaSetterExpression = class(TGocciaExpression)
  private
    FParameter: string;
    FBody: TGocciaASTNode;
    FSourceText: string;
  public
    constructor Create(const AParameter: string; const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Parameter: string read FParameter;
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
    constructor Create(const AElements: TObjectList<TGocciaDestructuringPattern>; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Elements: TObjectList<TGocciaDestructuringPattern> read FElements;
  end;

  // Object destructuring pattern: {name, age} = object
  TGocciaObjectDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FProperties: TObjectList<TGocciaDestructuringProperty>;
  public
    constructor Create(const AProperties: TObjectList<TGocciaDestructuringProperty>; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Properties: TObjectList<TGocciaDestructuringProperty> read FProperties;
  end;

  // Rest pattern: ...rest
  TGocciaRestDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FArgument: TGocciaDestructuringPattern;
  public
    constructor Create(const AArgument: TGocciaDestructuringPattern; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Argument: TGocciaDestructuringPattern read FArgument;
  end;

  // Assignment pattern with default value: a = defaultValue
  TGocciaAssignmentDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  // Identifier pattern: just a variable name
  TGocciaIdentifierDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Name: string read FName;
  end;

  // Member expression pattern: obj.prop, this.x, arr[i], obj[key]
  TGocciaMemberExpressionDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FExpression: TGocciaMemberExpression;
  public
    constructor Create(const AExpression: TGocciaMemberExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Expression: TGocciaMemberExpression read FExpression;
  end;

  // Destructuring assignment expression
  TGocciaDestructuringAssignmentExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
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
      const AUseSameValueZero: Boolean; const ALine, AColumn: Integer);
    property Expression: TGocciaExpression read FExpression;
    property UseSameValueZero: Boolean read FUseSameValueZero;
  end;

  TGocciaBindingMatchPattern = class(TGocciaMatchPattern)
  private
    FName: string;
    FDeclarationType: TGocciaDeclarationType;
  public
    constructor Create(const AName: string; const ADeclarationType: TGocciaDeclarationType;
      const ALine, AColumn: Integer);
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
      const ALine, AColumn: Integer);
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
      const ARestPattern: TGocciaMatchPattern; const ALine, AColumn: Integer);
    property Properties: TGocciaObjectMatchPropertyList read FProperties;
    property RestPattern: TGocciaMatchPattern read FRestPattern;
  end;

  TGocciaRelationalMatchPattern = class(TGocciaMatchPattern)
  private
    FOperator: TGocciaTokenType;
    FExpression: TGocciaExpression;
  public
    constructor Create(const AOperator: TGocciaTokenType; const AExpression: TGocciaExpression;
      const ALine, AColumn: Integer);
    property Operator: TGocciaTokenType read FOperator;
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaGuardMatchPattern = class(TGocciaMatchPattern)
  private
    FCondition: TGocciaExpression;
  public
    constructor Create(const ACondition: TGocciaExpression; const ALine, AColumn: Integer);
    property Condition: TGocciaExpression read FCondition;
  end;

  TGocciaAsMatchPattern = class(TGocciaMatchPattern)
  private
    FPattern: TGocciaMatchPattern;
    FName: string;
    FDeclarationType: TGocciaDeclarationType;
  public
    constructor Create(const APattern: TGocciaMatchPattern; const AName: string;
      const ADeclarationType: TGocciaDeclarationType; const ALine, AColumn: Integer);
    property Pattern: TGocciaMatchPattern read FPattern;
    property Name: string read FName;
    property DeclarationType: TGocciaDeclarationType read FDeclarationType;
  end;

  TGocciaAndMatchPattern = class(TGocciaMatchPattern)
  private
    FPatterns: TGocciaMatchPatternList;
  public
    constructor Create(const APatterns: TGocciaMatchPatternList; const ALine, AColumn: Integer);
    property Patterns: TGocciaMatchPatternList read FPatterns;
  end;

  TGocciaOrMatchPattern = class(TGocciaMatchPattern)
  private
    FPatterns: TGocciaMatchPatternList;
  public
    constructor Create(const APatterns: TGocciaMatchPatternList; const ALine, AColumn: Integer);
    property Patterns: TGocciaMatchPatternList read FPatterns;
  end;

  TGocciaNotMatchPattern = class(TGocciaMatchPattern)
  private
    FPattern: TGocciaMatchPattern;
  public
    constructor Create(const APattern: TGocciaMatchPattern; const ALine, AColumn: Integer);
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
      const AHasRestWildcard: Boolean; const ALine, AColumn: Integer);
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
      const ALine, AColumn: Integer);
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
      const ADefaultExpression: TGocciaExpression; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property Subject: TGocciaExpression read FSubject;
    property Clauses: TGocciaMatchClauseList read FClauses;
    property DefaultExpression: TGocciaExpression read FDefaultExpression;
  end;

  TGocciaPrivateMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
  public
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const ALine, AColumn: Integer);
    function Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue; override;
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
  end;

  TGocciaPrivatePropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
  Goccia.Constants.ErrorNames,
  Goccia.Coverage,
  Goccia.Error,
  Goccia.Evaluator,
  Goccia.Evaluator.Assignment,
  Goccia.Evaluator.PatternMatching,
  Goccia.GarbageCollector,
  Goccia.ImportMeta,
  Goccia.Modules,
  Goccia.RegExp.Runtime,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue;

function IsNullishAssignmentValue(const AValue: TGocciaValue): Boolean; inline;
begin
  Result := not Assigned(AValue) or
            (AValue is TGocciaUndefinedLiteralValue) or
            (AValue is TGocciaNullLiteralValue);
end;

function NormalizeAssignmentValue(const AValue: TGocciaValue): TGocciaValue; inline;
begin
  if Assigned(AValue) then
    Result := AValue
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ TGocciaLiteralExpression }

constructor TGocciaLiteralExpression.Create(const AValue: TGocciaValue;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FValue := AValue;
  // Remove from GC -- the AST owns this value, not the garbage collector.
  // Singletons (UndefinedValue, TrueValue, FalseValue) are pinned separately
  // and safe to unregister (Remove is a no-op if not found).
  if Assigned(FValue) and Assigned(TGarbageCollector.Instance) then
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
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FValue := AValue;
end;

{ TGocciaRegexLiteralExpression }

constructor TGocciaRegexLiteralExpression.Create(const APattern, AFlags: string;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FPattern := APattern;
  FFlags := AFlags;
end;

{ TGocciaTemplateWithInterpolationExpression }

constructor TGocciaTemplateWithInterpolationExpression.Create(const AParts: TObjectList<TGocciaExpression>;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FParts := AParts;
end;

{ TGocciaTaggedTemplateExpression }

constructor TGocciaTaggedTemplateExpression.Create(const ATag: TGocciaExpression;
  const ACookedStrings, ARawStrings: TGocciaTemplateStrings;
  const ACookedValid: TGocciaTemplateCookedValid;
  const AExpressions: TObjectList<TGocciaExpression>;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FTag := ATag;
  FCookedStrings := ACookedStrings;
  FRawStrings := ARawStrings;
  FCookedValid := ACookedValid;
  FExpressions := AExpressions;
end;

destructor TGocciaTaggedTemplateExpression.Destroy;
begin
  FTag.Free;
  FExpressions.Free;
  // Release the GC pin so the template object can be collected once no live
  // references remain.  Guard against the GC already having been shut down.
  if Assigned(FTemplateObject) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnpinObject(FTemplateObject);
  inherited;
end;

// ES2026 §13.2.8.3 GetTemplateObject(templateLiteral)
procedure TGocciaTaggedTemplateExpression.SetCachedTemplateObject(const AValue: TGocciaValue);
begin
  if AValue = FTemplateObject then
    Exit;
  if Assigned(FTemplateObject) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.UnpinObject(FTemplateObject);
  if Assigned(AValue) and Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.PinObject(AValue);
  FTemplateObject := AValue;
end;

{ TGocciaIdentifierExpression }

constructor TGocciaIdentifierExpression.Create(const AName: string;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
end;

{ TGocciaBinaryExpression }

constructor TGocciaBinaryExpression.Create(const ALeft: TGocciaExpression;
  const AOperator: TGocciaTokenType; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FLeft := ALeft;
  FOperator := AOperator;
  FRight := ARight;
end;

{ TGocciaSequenceExpression }

constructor TGocciaSequenceExpression.Create(const AExpressions: TObjectList<TGocciaExpression>;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FExpressions := AExpressions;
end;

destructor TGocciaSequenceExpression.Destroy;
begin
  FExpressions.Free;
  inherited;
end;

{ TGocciaUnaryExpression }

constructor TGocciaUnaryExpression.Create(const AOperator: TGocciaTokenType;
  const AOperand: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperator := AOperator;
  FOperand := AOperand;
end;

{ TGocciaAssignmentExpression }

constructor TGocciaAssignmentExpression.Create(const AName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
  FValue := AValue;
end;

{ TGocciaPropertyAssignmentExpression }

constructor TGocciaPropertyAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPropertyName := APropertyName;
  FValue := AValue;
end;

{ TGocciaComputedPropertyAssignmentExpression }

constructor TGocciaComputedPropertyAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPropertyExpression := APropertyExpression;
  FValue := AValue;
end;

{ TGocciaCompoundAssignmentExpression }

constructor TGocciaCompoundAssignmentExpression.Create(const AName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaPropertyCompoundAssignmentExpression }

constructor TGocciaPropertyCompoundAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPropertyName := APropertyName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaComputedPropertyCompoundAssignmentExpression }

constructor TGocciaComputedPropertyCompoundAssignmentExpression.Create(const AObject: TGocciaExpression; const APropertyExpression: TGocciaExpression; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPropertyExpression := APropertyExpression;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaIncrementExpression }

constructor TGocciaIncrementExpression.Create(const AOperand: TGocciaExpression; const AOperator: TGocciaTokenType; const AIsPrefix: Boolean; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperand := AOperand;
  FOperator := AOperator;
  FIsPrefix := AIsPrefix;
end;

{ TGocciaCallExpression }

constructor TGocciaCallExpression.Create(const ACallee: TGocciaExpression;
  const AArguments: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer;
  const AOptional: Boolean = False);
begin
  inherited Create(ALine, AColumn);
  FCallee := ACallee;
  FArguments := AArguments;
  FOptional := AOptional;
end;

{ TGocciaMemberExpression }

constructor TGocciaMemberExpression.Create(const AObject: TGocciaExpression;
  const AProperty: string; const AComputed: Boolean; const ALine, AColumn: Integer; const AOptional: Boolean = False);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FProperty := AProperty;
  FPropertyExpression := nil;
  FComputed := AComputed;
  FOptional := AOptional;
end;

constructor TGocciaMemberExpression.Create(const AObject: TGocciaExpression;
  const APropertyExpression: TGocciaExpression; const ALine, AColumn: Integer; const AOptional: Boolean = False);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FProperty := '';
  FPropertyExpression := APropertyExpression;
  FComputed := True;
  FOptional := AOptional;
end;

{ TGocciaArrayExpression }

constructor TGocciaArrayExpression.Create(const AElements: TObjectList<TGocciaExpression>;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FElements := AElements;
end;

{ TGocciaObjectExpression }

constructor TGocciaObjectExpression.Create(const AProperties: TGocciaExpressionMap;
  const APropertyOrder: TStringList;
  const ALine, AColumn: Integer);
var
  I: Integer;
begin
  inherited Create(ALine, AColumn);
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
  const ALine, AColumn: Integer);
var
  I: Integer;
begin
  inherited Create(ALine, AColumn);
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
  const ALine, AColumn: Integer);
var
  I: Integer;
begin
  inherited Create(ALine, AColumn);
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
  const ALine, AColumn: Integer);
var
  I: Integer;
begin
  inherited Create(ALine, AColumn);
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
  const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FParameters := AParameters;
  FBody := ABody;
end;

{ TGocciaMethodExpression }

constructor TGocciaMethodExpression.Create(const AParameters: TGocciaParameterArray;
  const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FParameters := AParameters;
  FBody := ABody;
  FHasOwnPrototype := False;
end;

{ TGocciaAwaitExpression }

constructor TGocciaAwaitExpression.Create(const AOperand: TGocciaExpression;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperand := AOperand;
end;

{ TGocciaYieldExpression }

constructor TGocciaYieldExpression.Create(const AOperand: TGocciaExpression;
  const AIsDelegate: Boolean; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperand := AOperand;
  FIsDelegate := AIsDelegate;
end;

{ TGocciaConditionalExpression }

constructor TGocciaConditionalExpression.Create(const ACondition, AConsequent,
  AAlternate: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCondition := ACondition;
  FConsequent := AConsequent;
  FAlternate := AAlternate;
end;

{ TGocciaNewExpression }

constructor TGocciaNewExpression.Create(const ACallee: TGocciaExpression;
  const AArguments: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCallee := ACallee;
  FArguments := AArguments;
end;

{ TGocciaPrivateMemberExpression }

constructor TGocciaPrivateMemberExpression.Create(const AObject: TGocciaExpression; const APrivateName: string; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPrivateName := APrivateName;
end;

{ TGocciaPrivatePropertyAssignmentExpression }

constructor TGocciaPrivatePropertyAssignmentExpression.Create(const AObject: TGocciaExpression; const APrivateName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPrivateName := APrivateName;
  FValue := AValue;
end;

{ TGocciaPrivatePropertyCompoundAssignmentExpression }

constructor TGocciaPrivatePropertyCompoundAssignmentExpression.Create(const AObject: TGocciaExpression; const APrivateName: string; const AOperator: TGocciaTokenType; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPrivateName := APrivateName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaHoleExpression }

constructor TGocciaHoleExpression.Create(const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
end;

{ TGocciaSpreadExpression }

constructor TGocciaSpreadExpression.Create(const AArgument: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FArgument := AArgument;
end;

{ TGocciaGetterExpression }

constructor TGocciaGetterExpression.Create(const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FBody := ABody;
end;

{ TGocciaSetterExpression }

constructor TGocciaSetterExpression.Create(const AParameter: string; const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FParameter := AParameter;
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

constructor TGocciaArrayDestructuringPattern.Create(const AElements: TObjectList<TGocciaDestructuringPattern>; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FElements := AElements;
end;

{ TGocciaObjectPattern }

constructor TGocciaObjectDestructuringPattern.Create(const AProperties: TObjectList<TGocciaDestructuringProperty>; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FProperties := AProperties;
end;

{ TGocciaRestPattern }

constructor TGocciaRestDestructuringPattern.Create(const AArgument: TGocciaDestructuringPattern; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FArgument := AArgument;
end;

{ TGocciaAssignmentPattern }

constructor TGocciaAssignmentDestructuringPattern.Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FLeft := ALeft;
  FRight := ARight;
end;

{ TGocciaIdentifierPattern }

constructor TGocciaIdentifierDestructuringPattern.Create(const AName: string; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
end;

{ TGocciaMemberExpressionDestructuringPattern }

constructor TGocciaMemberExpressionDestructuringPattern.Create(const AExpression: TGocciaMemberExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FExpression := AExpression;
end;

{ TGocciaDestructuringAssignmentExpression }

constructor TGocciaDestructuringAssignmentExpression.Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
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
  const AUseSameValueZero: Boolean; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FExpression := AExpression;
  FUseSameValueZero := AUseSameValueZero;
end;

{ TGocciaBindingMatchPattern }

constructor TGocciaBindingMatchPattern.Create(const AName: string;
  const ADeclarationType: TGocciaDeclarationType; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
  FDeclarationType := ADeclarationType;
end;

{ TGocciaArrayMatchPattern }

constructor TGocciaArrayMatchPattern.Create(const AElements: TGocciaMatchPatternList;
  const ARestPattern: TGocciaMatchPattern; const AHasRestWildcard: Boolean;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FElements := AElements;
  FRestPattern := ARestPattern;
  FHasRestWildcard := AHasRestWildcard;
end;

{ TGocciaObjectMatchPattern }

constructor TGocciaObjectMatchPattern.Create(const AProperties: TGocciaObjectMatchPropertyList;
  const ARestPattern: TGocciaMatchPattern; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FProperties := AProperties;
  FRestPattern := ARestPattern;
end;

{ TGocciaRelationalMatchPattern }

constructor TGocciaRelationalMatchPattern.Create(const AOperator: TGocciaTokenType;
  const AExpression: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperator := AOperator;
  FExpression := AExpression;
end;

{ TGocciaGuardMatchPattern }

constructor TGocciaGuardMatchPattern.Create(const ACondition: TGocciaExpression;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCondition := ACondition;
end;

{ TGocciaAsMatchPattern }

constructor TGocciaAsMatchPattern.Create(const APattern: TGocciaMatchPattern;
  const AName: string; const ADeclarationType: TGocciaDeclarationType;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FPattern := APattern;
  FName := AName;
  FDeclarationType := ADeclarationType;
end;

{ TGocciaAndMatchPattern }

constructor TGocciaAndMatchPattern.Create(const APatterns: TGocciaMatchPatternList;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FPatterns := APatterns;
end;

{ TGocciaOrMatchPattern }

constructor TGocciaOrMatchPattern.Create(const APatterns: TGocciaMatchPatternList;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FPatterns := APatterns;
end;

{ TGocciaNotMatchPattern }

constructor TGocciaNotMatchPattern.Create(const APattern: TGocciaMatchPattern;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FPattern := APattern;
end;

{ TGocciaExtractorMatchPattern }

constructor TGocciaExtractorMatchPattern.Create(
  const AMatcherExpression: TGocciaExpression; const AArguments: TGocciaMatchPatternList;
  const ARestPattern: TGocciaMatchPattern; const AHasRestWildcard: Boolean;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
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
  const APattern: TGocciaMatchPattern; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FSubject := ASubject;
  FPattern := APattern;
end;

{ TGocciaMatchExpression }

constructor TGocciaMatchExpression.Create(const ASubject: TGocciaExpression;
  const AClauses: TGocciaMatchClauseList; const ADefaultExpression: TGocciaExpression;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
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
begin
  Result := Value.Evaluate(AContext);
  AContext.Scope.AssignBinding(Name, Result);
end;

function TGocciaPropertyAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaValue;
begin
  Obj := ObjectExpr.Evaluate(AContext);
  Result := Value.Evaluate(AContext);
  AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column);
end;

function TGocciaComputedPropertyAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, PropertyValue: TGocciaValue;
  PropName: string;
begin
  Obj := ObjectExpr.Evaluate(AContext);
  PropertyValue := PropertyExpression.Evaluate(AContext);
  Result := Value.Evaluate(AContext);
  if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaClassValue) then
    TGocciaClassValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), Result)
  else if (PropertyValue is TGocciaSymbolValue) and (Obj is TGocciaObjectValue) then
    TGocciaObjectValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyValue), Result)
  else
  begin
    PropName := PropertyValue.ToStringLiteral.Value;
    AssignProperty(Obj, PropName, Result, AContext.OnError, Line, Column);
  end;
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
function TGocciaCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  CurrentValue, RhsValue: TGocciaValue;
begin
  CurrentValue := AContext.Scope.GetValue(Name);
  // ES2026 §13.15.2 step 3: ??=
  if Operator = gttNullishCoalescingAssign then
  begin
    if not IsNullishAssignmentValue(CurrentValue) then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AContext.Scope.AssignBinding(Name, Result);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: &&=
  if Operator = gttLogicalAndAssign then
  begin
    if not CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AContext.Scope.AssignBinding(Name, Result);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: ||=
  if Operator = gttLogicalOrAssign then
  begin
    if CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AContext.Scope.AssignBinding(Name, Result);
    Exit;
  end;

  Result := CurrentValue;
  RhsValue := Value.Evaluate(AContext);
    Result := Goccia.Arithmetic.CompoundOperations(
      Result, RhsValue, Operator);
  AContext.Scope.AssignBinding(Name, Result);
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
function TGocciaPropertyCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, CurrentValue, RhsValue: TGocciaValue;
begin
  Obj := ObjectExpr.Evaluate(AContext);
  CurrentValue := NormalizeAssignmentValue(Obj.GetProperty(PropertyName));
  // ES2026 §13.15.2 step 3: ??=
  if Operator = gttNullishCoalescingAssign then
  begin
    if not IsNullishAssignmentValue(CurrentValue) then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: &&=
  if Operator = gttLogicalAndAssign then
  begin
    if not CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column);
    Exit;
  end;

  // ES2026 §13.15.2 step 3: ||=
  if Operator = gttLogicalOrAssign then
  begin
    if CurrentValue.ToBooleanLiteral.Value then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropertyName, Result, AContext.OnError, Line, Column);
    Exit;
  end;

  RhsValue := Value.Evaluate(AContext);
  PerformPropertyCompoundAssignment(Obj, PropertyName, RhsValue, Operator, AContext.OnError, Line, Column);
  Result := NormalizeAssignmentValue(Obj.GetProperty(PropertyName));
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression AssignmentOperator AssignmentExpression
function TGocciaComputedPropertyCompoundAssignmentExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, PropertyKeyValue, CurrentValue, RhsValue: TGocciaValue;
  PropName: string;

  function ShortCircuits: Boolean; inline;
  begin
    case Operator of
      gttNullishCoalescingAssign: Result := not IsNullishAssignmentValue(CurrentValue);
      gttLogicalAndAssign: Result := not CurrentValue.ToBooleanLiteral.Value;
      gttLogicalOrAssign: Result := CurrentValue.ToBooleanLiteral.Value;
    else
      Result := False;
    end;
  end;

  function IsShortCircuitOperator: Boolean; inline;
  begin
    Result := Operator in [gttNullishCoalescingAssign, gttLogicalAndAssign, gttLogicalOrAssign];
  end;

begin
  Obj := ObjectExpr.Evaluate(AContext);
  PropertyKeyValue := PropertyExpression.Evaluate(AContext);
  if (PropertyKeyValue is TGocciaSymbolValue) and ((Obj is TGocciaClassValue) or (Obj is TGocciaObjectValue)) then
  begin
    if Obj is TGocciaClassValue then
      CurrentValue := NormalizeAssignmentValue(
        TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue)))
    else
      CurrentValue := NormalizeAssignmentValue(
        TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue)));

    if IsShortCircuitOperator then
    begin
      if ShortCircuits then
        Exit(CurrentValue);

      Result := Value.Evaluate(AContext);
      if Obj is TGocciaClassValue then
        TGocciaClassValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyKeyValue), Result)
      else
        TGocciaObjectValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyKeyValue), Result);
      Exit;
    end;

    RhsValue := Value.Evaluate(AContext);
    PerformSymbolPropertyCompoundAssignment(Obj, TGocciaSymbolValue(PropertyKeyValue), RhsValue, Operator, AContext.OnError, Line, Column);
    if Obj is TGocciaClassValue then
      Result := NormalizeAssignmentValue(
        TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue)))
    else
      Result := NormalizeAssignmentValue(
        TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue)));
    Exit;
  end;

  PropName := PropertyKeyValue.ToStringLiteral.Value;
  CurrentValue := NormalizeAssignmentValue(Obj.GetProperty(PropName));
  if IsShortCircuitOperator then
  begin
    if ShortCircuits then
      Exit(CurrentValue);

    Result := Value.Evaluate(AContext);
    AssignProperty(Obj, PropName, Result, AContext.OnError, Line, Column);
    Exit;
  end;

  RhsValue := Value.Evaluate(AContext);
  PerformPropertyCompoundAssignment(Obj, PropName, RhsValue, Operator, AContext.OnError, Line, Column);
  Result := NormalizeAssignmentValue(Obj.GetProperty(PropName));
end;

function TGocciaIncrementExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj, OldValue, NewValue, PropertyKeyValue: TGocciaValue;
  MemberExpr: TGocciaMemberExpression;
  PropName: string;
begin
  if Operand is TGocciaIdentifierExpression then
  begin
    PropName := TGocciaIdentifierExpression(Operand).Name;
    OldValue := AContext.Scope.GetValue(PropName);
    OldValue := OldValue.ToNumberLiteral;
    NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
    AContext.Scope.AssignBinding(PropName, NewValue);
    if IsPrefix then
      Result := NewValue
    else
      Result := OldValue;
  end
  else if Operand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(Operand);
    Obj := MemberExpr.ObjectExpr.Evaluate(AContext);
    if MemberExpr.Computed then
    begin
      PropertyKeyValue := MemberExpr.PropertyExpression.Evaluate(AContext);
      if (PropertyKeyValue is TGocciaSymbolValue) and ((Obj is TGocciaClassValue) or (Obj is TGocciaObjectValue)) then
      begin
        if Obj is TGocciaClassValue then
          OldValue := TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue))
        else
          OldValue := TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKeyValue));
        if OldValue = nil then
          OldValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        OldValue := OldValue.ToNumberLiteral;
        NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
        if Obj is TGocciaClassValue then
          TGocciaClassValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyKeyValue), NewValue)
        else
          TGocciaObjectValue(Obj).AssignSymbolProperty(TGocciaSymbolValue(PropertyKeyValue), NewValue);
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
    begin
      AContext.OnError('Cannot access property on non-object', Line, Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
    OldValue := OldValue.ToNumberLiteral;
    NewValue := PerformIncrement(OldValue, Operator = gttIncrement);
    AssignProperty(Obj, PropName, NewValue, AContext.OnError, Line, Column);
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

function TGocciaMethodExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateMethodExpression(Self, AContext);
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
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
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
  begin
    AContext.OnError('super can only be used in a class method', Line, Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §13.3.12.1 ImportMeta : import . meta
function TGocciaImportMetaExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := GetOrCreateImportMeta(AContext.CurrentFilePath);
end;

constructor TGocciaImportCallExpression.Create(const ASpecifier: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FSpecifier := ASpecifier;
end;

// ES2026 §13.3.10.1 Runtime Semantics: Evaluation — ImportCall
function TGocciaImportCallExpression.Evaluate(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
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
      // ES2026 §13.3.10.1 step 6-7: HostLoadImportedModule
      Module := AContext.LoadModule(SpecifierValue.ToStringLiteral.Value,
        AContext.CurrentFilePath);
      // ES2026 §13.3.10.1 step 11: Resolve promise with namespace
      Promise.Resolve(Module.GetNamespaceObject);
    except
      on E: TGocciaThrowValue do
        Promise.Reject(E.Value);
      on E: TGocciaSyntaxError do
        Promise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
      on E: TGocciaTypeError do
        Promise.Reject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
      on E: TGocciaReferenceError do
        Promise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
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

end.
