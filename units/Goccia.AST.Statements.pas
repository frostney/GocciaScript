unit Goccia.AST.Statements;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Node, Goccia.AST.Expressions, Generics.Collections, Classes, Goccia.Values.UndefinedValue;

type
  TGocciaExpressionStatement = class(TGocciaStatement)
  private
    FExpression: TGocciaExpression;
  public
    constructor Create(AExpression: TGocciaExpression; ALine, AColumn: Integer);
    property Expression: TGocciaExpression read FExpression;
  end;

  TGocciaVariableDeclaration = class(TGocciaStatement)
  private
    FName: string;
    FInitializer: TGocciaExpression;
    FIsConst: Boolean;
  public
    constructor Create(const AName: string; AInitializer: TGocciaExpression;
      AIsConst: Boolean; ALine, AColumn: Integer);
    property Name: string read FName;
    property Initializer: TGocciaExpression read FInitializer;
    property IsConst: Boolean read FIsConst;
  end;

  TGocciaBlockStatement = class(TGocciaStatement)
  private
    FNodes: TObjectList<TGocciaASTNode>;
  public
    constructor Create(ANodes: TObjectList<TGocciaASTNode>;
      ALine, AColumn: Integer);
    property Nodes: TObjectList<TGocciaASTNode> read FNodes;
  end;

  TGocciaIfStatement = class(TGocciaStatement)
  private
    FCondition: TGocciaExpression;
    FConsequent: TGocciaStatement;
    FAlternate: TGocciaStatement;
  public
    constructor Create(ACondition: TGocciaExpression;
      AConsequent, AAlternate: TGocciaStatement; ALine, AColumn: Integer);
    property Condition: TGocciaExpression read FCondition;
    property Consequent: TGocciaStatement read FConsequent;
    property Alternate: TGocciaStatement read FAlternate;
  end;

  TGocciaReturnStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(AValue: TGocciaExpression; ALine, AColumn: Integer);
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaThrowStatement = class(TGocciaStatement)
  private
    FValue: TGocciaExpression;
  public
    constructor Create(AValue: TGocciaExpression; ALine, AColumn: Integer);
    property Value: TGocciaExpression read FValue;
  end;

  TGocciaTryStatement = class(TGocciaStatement)
  private
    FBlock: TGocciaBlockStatement;
    FCatchParam: string;
    FCatchBlock: TGocciaBlockStatement;
    FFinallyBlock: TGocciaBlockStatement;
  public
    constructor Create(ABlock: TGocciaBlockStatement; const ACatchParam: string;
      ACatchBlock, AFinallyBlock: TGocciaBlockStatement; ALine, AColumn: Integer);
    property Block: TGocciaBlockStatement read FBlock;
    property CatchParam: string read FCatchParam;
    property CatchBlock: TGocciaBlockStatement read FCatchBlock;
    property FinallyBlock: TGocciaBlockStatement read FFinallyBlock;
  end;

  // Is this not a statement?
  TGocciaClassMethod = class(TGocciaExpression)
  private
    FName: string;
    FParameters: TStringList;
    FBody: TGocciaASTNode;
    FIsStatic: Boolean;
  public
    constructor Create(const AName: string; AParameters: TStringList;
      ABody: TGocciaASTNode; AIsStatic: Boolean; ALine, AColumn: Integer);
    property Name: string read FName write FName;
    property Parameters: TStringList read FParameters;
    property Body: TGocciaASTNode read FBody;
    property IsStatic: Boolean read FIsStatic;
  end;

  TGocciaClassDeclaration = class(TGocciaStatement)
  private
    FName: string;
    FSuperClass: string;
    FMethods: TDictionary<string, TGocciaClassMethod>;
  public
    constructor Create(const AName, ASuperClass: string;
      AMethods: TDictionary<string, TGocciaClassMethod>;
      ALine, AColumn: Integer);
    property Name: string read FName;
    property SuperClass: string read FSuperClass;
    property Methods: TDictionary<string, TGocciaClassMethod> read FMethods;
  end;

  // Modules
  TGocciaImportDeclaration = class(TGocciaStatement)
  private
    FImports: TDictionary<string, string>; // local name -> imported name
    FModulePath: string;
  public
    constructor Create(AImports: TDictionary<string, string>;
      const AModulePath: string; ALine, AColumn: Integer);
    property Imports: TDictionary<string, string> read FImports;
    property ModulePath: string read FModulePath;
  end;

  TGocciaExportDeclaration = class(TGocciaStatement)
  private
    FExportsTable: TDictionary<string, string>; // exported name -> local name
  public
    constructor Create(AExportsTable: TDictionary<string, string>;
      ALine, AColumn: Integer);
    property ExportsTable: TDictionary<string, string> read FExportsTable;
  end;

implementation

{ TGocciaExpressionStatement }

  constructor TGocciaExpressionStatement.Create(AExpression: TGocciaExpression;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExpression := AExpression;
  end;

  { TGocciaVariableDeclaration }

  constructor TGocciaVariableDeclaration.Create(const AName: string;
    AInitializer: TGocciaExpression; AIsConst: Boolean; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FInitializer := AInitializer;
    FIsConst := AIsConst;
  end;

  { TGocciaBlockStatement }

  constructor TGocciaBlockStatement.Create(ANodes: TObjectList<TGocciaASTNode>;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FNodes := ANodes;
  end;

  { TGocciaIfStatement }

  constructor TGocciaIfStatement.Create(ACondition: TGocciaExpression;
    AConsequent, AAlternate: TGocciaStatement; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FCondition := ACondition;
    FConsequent := AConsequent;
    FAlternate := AAlternate;
  end;

  { TGocciaReturnStatement }

  constructor TGocciaReturnStatement.Create(AValue: TGocciaExpression;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    if AValue = nil then
      FValue := TGocciaLiteralExpression.Create(TGocciaUndefinedValue.Create, ALine, AColumn)
    else
      FValue := AValue;
  end;

  { TGocciaThrowStatement }

  constructor TGocciaThrowStatement.Create(AValue: TGocciaExpression;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FValue := AValue;
  end;

  { TGocciaTryStatement }

  constructor TGocciaTryStatement.Create(ABlock: TGocciaBlockStatement;
    const ACatchParam: string; ACatchBlock, AFinallyBlock: TGocciaBlockStatement;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FBlock := ABlock;
    FCatchParam := ACatchParam;
    FCatchBlock := ACatchBlock;
    FFinallyBlock := AFinallyBlock;
  end;

  { TGocciaClassMethod }

  constructor TGocciaClassMethod.Create(const AName: string; AParameters: TStringList;
    ABody: TGocciaASTNode; AIsStatic: Boolean; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FParameters := AParameters;
    FBody := ABody;
    FIsStatic := AIsStatic;
  end;

  { TGocciaClassDeclaration }

  constructor TGocciaClassDeclaration.Create(const AName, ASuperClass: string;
    AMethods: TDictionary<string, TGocciaClassMethod>;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FName := AName;
    FSuperClass := ASuperClass;
    FMethods := AMethods;
  end;

  { TGocciaImportDeclaration }

  constructor TGocciaImportDeclaration.Create(AImports: TDictionary<string, string>;
    const AModulePath: string; ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FImports := AImports;
    FModulePath := AModulePath;
  end;

  { TGocciaExportDeclaration }

  constructor TGocciaExportDeclaration.Create(AExportsTable: TDictionary<string, string>;
    ALine, AColumn: Integer);
  begin
    inherited Create(ALine, AColumn);
    FExportsTable := AExportsTable;
  end;

end.
