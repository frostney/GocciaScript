unit Goccia.AST.Expressions;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node, Goccia.Token, Goccia.Values.Base, Generics.Collections, Classes, SysUtils;

type
  TGocciaLiteralExpression = class(TGocciaExpression)
  private
    FValue: TGocciaValue;
  public
    constructor Create(AValue: TGocciaValue; ALine, AColumn: Integer);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaIdentifierExpression = class(TGocciaExpression)
  private
    FName: string;
  public
    constructor Create(const AName: string; ALine, AColumn: Integer);
    property Name: string read FName;
  end;

  TGocciaBinaryExpression = class(TGocciaExpression)
  private
    FLeft: TGocciaExpression;
    FOperator: TGocciaTokenType;
    FRight: TGocciaExpression;
  public
    constructor Create(ALeft: TGocciaExpression; AOperator: TGocciaTokenType;
      ARight: TGocciaExpression; ALine, AColumn: Integer);
    property Left: TGocciaExpression read FLeft;
    property Operator: TGocciaTokenType read FOperator;
    property Right: TGocciaExpression read FRight;
  end;

  TGocciaUnaryExpression = class(TGocciaExpression)
  private
    FOperator: TGocciaTokenType;
    FOperand: TGocciaExpression;
  public
    constructor Create(AOperator: TGocciaTokenType; AOperand: TGocciaExpression;
      ALine, AColumn: Integer);
    property Operator: TGocciaTokenType read FOperator;
    property Operand: TGocciaExpression read FOperand;
  end;

  TGocciaAssignmentExpression = class(TGocciaExpression)
  private
    FName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(const AName: string; AValue: TGocciaExpression; ALine, AColumn: Integer);
    property Name: string read FName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaPropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPropertyName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(AObject: TGocciaExpression; const APropertyName: string; AValue: TGocciaExpression; ALine, AColumn: Integer);
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FPropertyName;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaCompoundAssignmentExpression = class(TGocciaExpression)
  private
    FName: string;
    FOperator: TGocciaTokenType;
    FValue: TGocciaExpression;
  public
    constructor Create(const AName: string; AOperator: TGocciaTokenType; AValue: TGocciaExpression; ALine, AColumn: Integer);
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
    constructor Create(AObject: TGocciaExpression; const APropertyName: string; AOperator: TGocciaTokenType; AValue: TGocciaExpression; ALine, AColumn: Integer);
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FPropertyName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaIncrementExpression = class(TGocciaExpression)
  private
    FOperand: TGocciaExpression;
    FOperator: TGocciaTokenType; // gttIncrement or gttDecrement
    FIsPrefix: Boolean;
  public
    constructor Create(AOperand: TGocciaExpression; AOperator: TGocciaTokenType; AIsPrefix: Boolean; ALine, AColumn: Integer);
    property Operand: TGocciaExpression read FOperand;
    property Operator: TGocciaTokenType read FOperator;
    property IsPrefix: Boolean read FIsPrefix;
  end;

  TGocciaCallExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
  public
    constructor Create(ACallee: TGocciaExpression;
      AArguments: TObjectList<TGocciaExpression>; ALine, AColumn: Integer);
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
  end;

  TGocciaMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FProperty: string;
    FPropertyExpression: TGocciaExpression;
    FComputed: Boolean;
  public
    constructor Create(AObject: TGocciaExpression; const AProperty: string;
      AComputed: Boolean; ALine, AColumn: Integer); overload;
    constructor Create(AObject: TGocciaExpression; APropertyExpression: TGocciaExpression;
      ALine, AColumn: Integer); overload;
    property ObjectExpr: TGocciaExpression read FObject;
    property PropertyName: string read FProperty;
    property PropertyExpression: TGocciaExpression read FPropertyExpression;
    property Computed: Boolean read FComputed;
  end;

  TGocciaArrayExpression = class(TGocciaExpression)
  private
    FElements: TObjectList<TGocciaExpression>;
  public
    constructor Create(AElements: TObjectList<TGocciaExpression>;
      ALine, AColumn: Integer);
    property Elements: TObjectList<TGocciaExpression> read FElements;
  end;

  TGocciaObjectExpression = class(TGocciaExpression)
  private
    FProperties: TDictionary<string, TGocciaExpression>;
  public
    constructor Create(AProperties: TDictionary<string, TGocciaExpression>;
      ALine, AColumn: Integer);
    property Properties: TDictionary<string, TGocciaExpression> read FProperties;
  end;

  TGocciaArrowFunctionExpression = class(TGocciaExpression)
  private
    FParameters: TStringList;
    FBody: TGocciaASTNode;
  public
    constructor Create(AParameters: TStringList; ABody: TGocciaASTNode;
      ALine, AColumn: Integer);
    property Parameters: TStringList read FParameters;
    property Body: TGocciaASTNode read FBody;
  end;

  TGocciaConditionalExpression = class(TGocciaExpression)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaExpression;
    FAlternate: TGocciaExpression;
  public
    constructor Create(ACondition, AConsequent, AAlternate: TGocciaExpression;
      ALine, AColumn: Integer);
    property Condition: TGocciaExpression read FCondition;
    property Consequent: TGocciaExpression read FConsequent;
    property Alternate: TGocciaExpression read FAlternate;
  end;

  TGocciaNewExpression = class(TGocciaExpression)
  private
    FCallee: TGocciaExpression;
    FArguments: TObjectList<TGocciaExpression>;
  public
    constructor Create(ACallee: TGocciaExpression;
      AArguments: TObjectList<TGocciaExpression>; ALine, AColumn: Integer);
    property Callee: TGocciaExpression read FCallee;
    property Arguments: TObjectList<TGocciaExpression> read FArguments;
  end;

  TGocciaThisExpression = class(TGocciaExpression);

  TGocciaSuperExpression = class(TGocciaExpression);

  TGocciaPrivateMemberExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
  public
    constructor Create(AObject: TGocciaExpression; const APrivateName: string; ALine, AColumn: Integer);
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
  end;

  TGocciaPrivatePropertyAssignmentExpression = class(TGocciaExpression)
  private
    FObject: TGocciaExpression;
    FPrivateName: string;
    FValue: TGocciaExpression;
  public
    constructor Create(AObject: TGocciaExpression; const APrivateName: string; AValue: TGocciaExpression; ALine, AColumn: Integer);
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
    constructor Create(AObject: TGocciaExpression; const APrivateName: string; AOperator: TGocciaTokenType; AValue: TGocciaExpression; ALine, AColumn: Integer);
    property ObjectExpr: TGocciaExpression read FObject;
    property PrivateName: string read FPrivateName;
    property Operator: TGocciaTokenType read FOperator;
    property Value: TGocciaExpression read FValue;
  end;

implementation

{ TGocciaLiteralExpression }

constructor TGocciaLiteralExpression.Create(AValue: TGocciaValue;
  ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FValue := AValue;
end;

{ TGocciaIdentifierExpression }

constructor TGocciaIdentifierExpression.Create(const AName: string;
  ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
end;

{ TGocciaBinaryExpression }

constructor TGocciaBinaryExpression.Create(ALeft: TGocciaExpression;
  AOperator: TGocciaTokenType; ARight: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FLeft := ALeft;
  FOperator := AOperator;
  FRight := ARight;
end;

{ TGocciaUnaryExpression }

constructor TGocciaUnaryExpression.Create(AOperator: TGocciaTokenType;
  AOperand: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperator := AOperator;
  FOperand := AOperand;
end;

{ TGocciaAssignmentExpression }

constructor TGocciaAssignmentExpression.Create(const AName: string; AValue: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
  FValue := AValue;
end;

{ TGocciaPropertyAssignmentExpression }

constructor TGocciaPropertyAssignmentExpression.Create(AObject: TGocciaExpression; const APropertyName: string; AValue: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPropertyName := APropertyName;
  FValue := AValue;
end;

{ TGocciaCompoundAssignmentExpression }

constructor TGocciaCompoundAssignmentExpression.Create(const AName: string; AOperator: TGocciaTokenType; AValue: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FName := AName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaPropertyCompoundAssignmentExpression }

constructor TGocciaPropertyCompoundAssignmentExpression.Create(AObject: TGocciaExpression; const APropertyName: string; AOperator: TGocciaTokenType; AValue: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPropertyName := APropertyName;
  FOperator := AOperator;
  FValue := AValue;
end;

{ TGocciaIncrementExpression }

constructor TGocciaIncrementExpression.Create(AOperand: TGocciaExpression; AOperator: TGocciaTokenType; AIsPrefix: Boolean; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FOperand := AOperand;
  FOperator := AOperator;
  FIsPrefix := AIsPrefix;
end;

{ TGocciaCallExpression }

constructor TGocciaCallExpression.Create(ACallee: TGocciaExpression;
  AArguments: TObjectList<TGocciaExpression>; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCallee := ACallee;
  FArguments := AArguments;
end;

{ TGocciaMemberExpression }

constructor TGocciaMemberExpression.Create(AObject: TGocciaExpression;
  const AProperty: string; AComputed: Boolean; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FProperty := AProperty;
  FPropertyExpression := nil;
  FComputed := AComputed;
end;

constructor TGocciaMemberExpression.Create(AObject: TGocciaExpression;
  APropertyExpression: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FProperty := '';
  FPropertyExpression := APropertyExpression;
  FComputed := True;
end;

{ TGocciaArrayExpression }

constructor TGocciaArrayExpression.Create(AElements: TObjectList<TGocciaExpression>;
  ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FElements := AElements;
end;

{ TGocciaObjectExpression }

constructor TGocciaObjectExpression.Create(AProperties: TDictionary<string, TGocciaExpression>;
  ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FProperties := AProperties;
end;

{ TGocciaArrowFunctionExpression }

constructor TGocciaArrowFunctionExpression.Create(AParameters: TStringList;
  ABody: TGocciaASTNode; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FParameters := AParameters;
  FBody := ABody;
end;

{ TGocciaConditionalExpression }

constructor TGocciaConditionalExpression.Create(ACondition, AConsequent,
  AAlternate: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCondition := ACondition;
  FConsequent := AConsequent;
  FAlternate := AAlternate;
end;

{ TGocciaNewExpression }

constructor TGocciaNewExpression.Create(ACallee: TGocciaExpression;
  AArguments: TObjectList<TGocciaExpression>; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FCallee := ACallee;
  FArguments := AArguments;
end;

{ TGocciaPrivateMemberExpression }

constructor TGocciaPrivateMemberExpression.Create(AObject: TGocciaExpression; const APrivateName: string; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPrivateName := APrivateName;
end;

{ TGocciaPrivatePropertyAssignmentExpression }

constructor TGocciaPrivatePropertyAssignmentExpression.Create(AObject: TGocciaExpression; const APrivateName: string; AValue: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPrivateName := APrivateName;
  FValue := AValue;
end;

{ TGocciaPrivatePropertyCompoundAssignmentExpression }

constructor TGocciaPrivatePropertyCompoundAssignmentExpression.Create(AObject: TGocciaExpression; const APrivateName: string; AOperator: TGocciaTokenType; AValue: TGocciaExpression; ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  FObject := AObject;
  FPrivateName := APrivateName;
  FOperator := AOperator;
  FValue := AValue;
end;

end.
