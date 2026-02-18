unit Goccia.AST.Expressions;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.AST.Node,
  Goccia.GarbageCollector,
  Goccia.Token,
  Goccia.Values.Primitives;

type
  // Forward declaration
  TGocciaDestructuringPattern = class;

  TGocciaParameter = record
    Name: string;                                 // For simple parameters
    Pattern: TGocciaDestructuringPattern;         // For destructuring parameters
    DefaultValue: TGocciaExpression;              // nil if no default value
    IsPattern: Boolean;                           // True if this is a destructuring pattern
    IsRest: Boolean;                              // True if this is a rest parameter (...args)
  end;
  TGocciaParameterArray = array of TGocciaParameter;

  TGocciaLiteralExpression = class(TGocciaExpression)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue; const ALine, AColumn: Integer);
    destructor Destroy; override;
    property Value: TGocciaValue read FValue;
  end;

  TGocciaTemplateLiteralExpression = class(TGocciaExpression)
  private
    FValue: string;
  public
    constructor Create(const AValue: string; const ALine, AColumn: Integer);
    property Value: string read FValue;
  end;

  TGocciaTemplateWithInterpolationExpression = class(TGocciaExpression)
  private
    FParts: TObjectList<TGocciaExpression>; // Mix of string literals and expressions
  public
    constructor Create(const AParts: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer);
    property Parts: TObjectList<TGocciaExpression> read FParts;
  end;

  TGocciaIdentifierExpression = class(TGocciaExpression)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ALine, AColumn: Integer);
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
    property Left: TGocciaExpression read FLeft;
    property Operator: TGocciaTokenType read FOperator;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaUnaryExpression = class(TGocciaExpression)
  private
    FOperator: TGocciaTokenType;
    FOperand: TGocciaExpression;
  public
    constructor Create(const AOperator: TGocciaTokenType; const AOperand: TGocciaExpression;
      const ALine, AColumn: Integer);
    property Operator: TGocciaTokenType read FOperator;
    property Operand: TGocciaExpression read FOperand;
  end;

  TGocciaAssignmentExpression = class(TGocciaExpression)
  private
    FName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AName: string; const AValue: TGocciaExpression; const ALine, AColumn: Integer);
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
    property Operand: TGocciaExpression read FOperand;
    property Operator: TGocciaTokenType read FOperator;
    property IsPrefix: Boolean read FIsPrefix;
  end;

  TGocciaCallExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
  public
    constructor Create(const ACallee: TGocciaExpression;
      const AArguments: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer);
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
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
    property Computed: Boolean read FComputed;
    property Optional: Boolean read FOptional;
  end;

  TGocciaArrayExpression = class(TGocciaExpression)
  private
    FElements: TObjectList<TGocciaExpression>;
  public
    constructor Create(const AElements: TObjectList<TGocciaExpression>;
      const ALine, AColumn: Integer);
    property Elements: TObjectList<TGocciaExpression> read FElements;
  end;

    // Forward declarations for getter/setter expressions
  TGocciaGetterExpression = class;
  TGocciaSetterExpression = class;

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
    FProperties: TDictionary<string, TGocciaExpression>;
    FPropertyInsertionOrder: TStringList;
    FComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
    FGetters: TDictionary<string, TGocciaGetterExpression>;
    FSetters: TDictionary<string, TGocciaSetterExpression>;

    // New: tracks source order of ALL property types
    FPropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
    FComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
  public
    constructor Create(const AProperties: TDictionary<string, TGocciaExpression>;
      const APropertyOrder: TStringList;
      const ALine, AColumn: Integer); overload;
    constructor Create(const AProperties: TDictionary<string, TGocciaExpression>;
      const APropertyOrder: TStringList;
      const AComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
      const ALine, AColumn: Integer); overload;
    constructor Create(const AProperties: TDictionary<string, TGocciaExpression>;
      const APropertyOrder: TStringList;
      const AComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
      const AGetters: TDictionary<string, TGocciaGetterExpression>;
      const ASetters: TDictionary<string, TGocciaSetterExpression>;
      const ALine, AColumn: Integer); overload;
    // New constructor with source order tracking
    constructor Create(const AProperties: TDictionary<string, TGocciaExpression>;
      const APropertyOrder: TStringList;
      const AComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
      const AComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
      const AGetters: TDictionary<string, TGocciaGetterExpression>;
      const ASetters: TDictionary<string, TGocciaSetterExpression>;
      const APropertySourceOrder: TArray<TGocciaPropertySourceOrder>;
      const ALine, AColumn: Integer); overload;
    destructor Destroy; override;

    property Properties: TDictionary<string, TGocciaExpression> read FProperties;
    property PropertyInsertionOrder: TStringList read FPropertyInsertionOrder;
    property ComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression> read FComputedProperties;
    property ComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>> read FComputedPropertiesInOrder;
    property Getters: TDictionary<string, TGocciaGetterExpression> read FGetters;
    property Setters: TDictionary<string, TGocciaSetterExpression> read FSetters;
    property PropertySourceOrder: TArray<TGocciaPropertySourceOrder> read FPropertySourceOrder;
    function GetPropertyNamesInOrder: TStringList;
  end;

  TGocciaArrowFunctionExpression = class(TGocciaExpression)
  private
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ALine, AColumn: Integer);
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
  end;

  TGocciaMethodExpression = class(TGocciaExpression)
  private
    FParameters: TGocciaParameterArray;
    FBody: TGocciaASTNode;
  public
    constructor Create(const AParameters: TGocciaParameterArray; const ABody: TGocciaASTNode;
      const ALine, AColumn: Integer);
    property Parameters: TGocciaParameterArray read FParameters;
    property Body: TGocciaASTNode read FBody;
  end;

  TGocciaConditionalExpression = class(TGocciaExpression)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaExpression;
    FAlternate: TGocciaExpression;
  public
    constructor Create(const ACondition, AConsequent, AAlternate: TGocciaExpression;
      const ALine, AColumn: Integer);
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
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
  end;

  TGocciaThisExpression = class(TGocciaExpression);

  TGocciaSuperExpression = class(TGocciaExpression);

  TGocciaHoleExpression = class(TGocciaExpression)
  public
    constructor Create(const ALine, AColumn: Integer);
  end;

  TGocciaSpreadExpression = class(TGocciaExpression)
  private
    FArgument: TGocciaExpression;
  public
    constructor Create(const AArgument: TGocciaExpression; const ALine, AColumn: Integer);
    property Argument: TGocciaExpression read FArgument;
  end;

  // Getter method: get propertyName() { ... }
  TGocciaGetterExpression = class(TGocciaExpression)
  private
    FBody: TGocciaASTNode;
  public
    constructor Create(const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
    property Body: TGocciaASTNode read FBody;
  end;

  // Setter method: set propertyName(value) { ... }
  TGocciaSetterExpression = class(TGocciaExpression)
  private
    FParameter: string;
    FBody: TGocciaASTNode;
  public
    constructor Create(const AParameter: string; const ABody: TGocciaASTNode; const ALine, AColumn: Integer);
    property Parameter: string read FParameter;
    property Body: TGocciaASTNode read FBody;
  end;

  // Destructuring pattern base class - complete definition
  TGocciaDestructuringPattern = class(TGocciaExpression)
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
    property Elements: TObjectList<TGocciaDestructuringPattern> read FElements;
  end;

  // Object destructuring pattern: {name, age} = object
  TGocciaObjectDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FProperties: TObjectList<TGocciaDestructuringProperty>;
  public
    constructor Create(const AProperties: TObjectList<TGocciaDestructuringProperty>; const ALine, AColumn: Integer);
    property Properties: TObjectList<TGocciaDestructuringProperty> read FProperties;
  end;

  // Rest pattern: ...rest
  TGocciaRestDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FArgument: TGocciaDestructuringPattern;
  public
    constructor Create(const AArgument: TGocciaDestructuringPattern; const ALine, AColumn: Integer);
    property Argument: TGocciaDestructuringPattern read FArgument;
  end;

  // Assignment pattern with default value: a = defaultValue
  TGocciaAssignmentDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  // Identifier pattern: just a variable name
  TGocciaIdentifierDestructuringPattern = class(TGocciaDestructuringPattern)
  private
    FName: string;
  public
    constructor Create(const AName: string; const ALine, AColumn: Integer);
    property Name: string read FName;
  end;

  // Destructuring assignment expression
  TGocciaDestructuringAssignmentExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaDestructuringPattern;
    FRight: TGocciaExpression;
  public
    constructor Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
    property Left: TGocciaDestructuringPattern read FLeft;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaPrivateMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
  public
    constructor Create(const AObject: TGocciaExpression; const APrivateName: string; const ALine, AColumn: Integer);
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
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

implementation

{ TGocciaLiteralExpression }

constructor TGocciaLiteralExpression.Create(const AValue: TGocciaValue;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FValue := AValue;
  // Remove from GC -- the AST owns this value, not the garbage collector.
  // Singletons (UndefinedValue, TrueValue, FalseValue) are pinned separately
  // and safe to unregister (Remove is a no-op if not found).
  if Assigned(FValue) and Assigned(TGocciaGarbageCollector.Instance) then
    TGocciaGarbageCollector.Instance.UnregisterValue(FValue);
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

{ TGocciaTemplateWithInterpolationExpression }

constructor TGocciaTemplateWithInterpolationExpression.Create(const AParts: TObjectList<TGocciaExpression>;
  const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FParts := AParts;
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
  const AArguments: TObjectList<TGocciaExpression>; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCallee := ACallee;
  FArguments := AArguments;
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

constructor TGocciaObjectExpression.Create(const AProperties: TDictionary<string, TGocciaExpression>;
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

constructor TGocciaObjectExpression.Create(const AProperties: TDictionary<string, TGocciaExpression>;
  const APropertyOrder: TStringList;
  const AComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
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

constructor TGocciaObjectExpression.Create(const AProperties: TDictionary<string, TGocciaExpression>;
  const APropertyOrder: TStringList;
  const AComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
  const AGetters: TDictionary<string, TGocciaGetterExpression>;
  const ASetters: TDictionary<string, TGocciaSetterExpression>;
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

constructor TGocciaObjectExpression.Create(const AProperties: TDictionary<string, TGocciaExpression>;
  const APropertyOrder: TStringList;
  const AComputedProperties: TDictionary<TGocciaExpression, TGocciaExpression>;
  const AComputedPropertiesInOrder: TArray<TPair<TGocciaExpression, TGocciaExpression>>;
  const AGetters: TDictionary<string, TGocciaGetterExpression>;
  const ASetters: TDictionary<string, TGocciaSetterExpression>;
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

{ TGocciaDestructuringAssignmentExpression }

constructor TGocciaDestructuringAssignmentExpression.Create(const ALeft: TGocciaDestructuringPattern; const ARight: TGocciaExpression; const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FLeft := ALeft;
  FRight := ARight;
end;

end.
