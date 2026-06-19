unit Goccia.Evaluator;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.ControlFlow,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Evaluator.Context,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ClassValue,
  Goccia.Values.HoleValue,
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  PGocciaValue = ^TGocciaValue;
  TGocciaInstanceInitializationMode = (
    iimFirstPass,
    iimEagerReplacement,
    iimReplay
  );

function Evaluate(const ANode: TGocciaASTNode; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateExpression(const AExpression: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateStatements(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateBinary(const ABinaryExpression: TGocciaBinaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateUnary(const AUnaryExpression: TGocciaUnaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDelete(const AOperand: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateCall(const ACallExpression: TGocciaCallExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue; overload;
function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue; overload;
function GetSuperProperty(const AContext: TGocciaEvaluationContext;
  const APropertyKey: TGocciaValue): TGocciaValue;
procedure AssignSuperProperty(const AContext: TGocciaEvaluationContext;
  const APropertyKey, AValue: TGocciaValue);
function EvaluateArray(const AArrayExpression: TGocciaArrayExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateObject(const AObjectExpression: TGocciaObjectExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateObjectMethodDefinition(
  const AMethodDefinition: TGocciaObjectMethodDefinition;
  const AContext: TGocciaEvaluationContext;
  const AHomeObject: TGocciaObjectValue;
  const AName: string): TGocciaValue;
function EvaluateGetter(const AGetterExpression: TGocciaGetterExpression; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil; const AAsMethod: Boolean = False): TGocciaValue;
function EvaluateSetter(const ASetterExpression: TGocciaSetterExpression; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil; const AAsMethod: Boolean = False): TGocciaValue;
function EvaluateArrowFunction(const AArrowFunctionExpression: TGocciaArrowFunctionExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateFunctionExpression(const AFunctionExpression: TGocciaFunctionExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateBlock(const ABlockStatement: TGocciaBlockStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateIf(const AIfStatement: TGocciaIfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateTry(const ATryStatement: TGocciaTryStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateSwitch(const ASwitchStatement: TGocciaSwitchStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateClassMethod(const AClassMethod: TGocciaClassMethod; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil): TGocciaValue;
function EvaluateClass(const AClassDeclaration: TGocciaClassDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassExpression(const AClassExpression: TGocciaClassExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateClassDefinition(const AClassDef: TGocciaClassDefinition; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaClassValue;
function EvaluateNewExpression(const ANewExpression: TGocciaNewExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue; overload;
function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue; overload;
function EvaluatePrivateInOperator(
  const APrivateMemberExpression: TGocciaPrivateMemberExpression;
  const ARightExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyAssignment(const APrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluatePrivatePropertyCompoundAssignment(const APrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringAssignment(const ADestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateEnumDeclaration(const AEnumDeclaration: TGocciaEnumDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateDestructuringDeclaration(const ADestructuringDeclaration: TGocciaDestructuringDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateLiteral(const ATemplateLiteralExpression: TGocciaTemplateLiteralExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateWithInterpolation(const ATemplateWithInterpolationExpression: TGocciaTemplateWithInterpolationExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTaggedTemplate(const ATaggedTemplateExpression: TGocciaTaggedTemplateExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateTemplateExpression(const AExpressionText: string; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaValue;
function EvaluateAwait(const AAwaitExpression: TGocciaAwaitExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateYield(const AYieldExpression: TGocciaYieldExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
function EvaluateUsingDeclaration(const AUsingDeclaration: TGocciaUsingDeclaration; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateForOf(const AForOfStatement: TGocciaForOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateForIn(const AForInStatement: TGocciaForInStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateForAwaitOf(const AForAwaitOfStatement: TGocciaForAwaitOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateFor(const AForStatement: TGocciaForStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateWhile(const AWhileStatement: TGocciaWhileStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
function EvaluateDoWhile(const ADoWhileStatement: TGocciaDoWhileStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;

// Destructuring pattern assignment procedures
procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
procedure StampRawPrivateInstanceBrand(const AReceiver: TGocciaObjectValue;
  const AAccessClass: TGocciaClassValue);

procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
procedure InitializePrivateInstanceProperties(const AInstance: TGocciaObjectValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext; const AInitializationMode: TGocciaInstanceInitializationMode = iimFirstPass);
function InstantiateClass(const AClassValue: TGocciaClassValue; const AArguments: TGocciaArgumentsCollection; const AContext: TGocciaEvaluationContext): TGocciaValue;
procedure ValidateClassConstructorReturn(const AClassValue: TGocciaClassValue; const AValue: TGocciaValue);

function IsObjectInstanceOfClass(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;

procedure AssignVariablePattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext);

procedure HoistVarDeclarations(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext); overload;
procedure HoistVarDeclarations(const ANodes: TObjectList<TGocciaASTNode>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext); overload;

procedure HoistFunctionDeclarations(const AStatements: TObjectList<TGocciaStatement>; const AContext: TGocciaEvaluationContext; const ABlockScoped: Boolean = False); overload;
procedure HoistFunctionDeclarations(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext; const ABlockScoped: Boolean = False); overload;

procedure PredeclareModuleLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
procedure PredeclareScriptLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
procedure PredeclareFunctionBodyLexicalDeclarations(
  const ANodes: TObjectList<TGocciaASTNode>; const AScope: TGocciaScope);

function EvaluateEvalProgram(const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext; const AVarScope,
  ALexicalScope: TGocciaScope; const AStrictEval: Boolean;
  const ARejectArgumentsVarDeclaration: Boolean;
  const ARejectVarDeclarationNames: TGocciaEvalRejectNameArray;
  const AAllowNewTarget: Boolean = False;
  const AAllowSuperProperty: Boolean = False;
  const AAllowSuperCall: Boolean = False;
  const ARejectArgumentsReference: Boolean = False): TGocciaValue;

implementation

uses
  Classes,
  SysUtils,

  OrderedStringMap,
  StringBuffer,

  Goccia.Arithmetic,
  Goccia.AST.BindingPatterns,
  Goccia.Bytecode.Chunk,
  Goccia.CallStack,
  Goccia.Constants,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Coverage,
  Goccia.DisposalTracker,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator.Assignment,
  Goccia.Evaluator.Decorators,
  Goccia.Evaluator.PatternMatching,
  Goccia.Evaluator.TypeOperations,
  Goccia.GarbageCollector,
  Goccia.Generator.Continuation,
  Goccia.InstructionLimit,
  Goccia.Intrinsics.FunctionObjects,
  Goccia.Keywords.Reserved,
  Goccia.SourcePipeline,
  Goccia.StackLimit,
  Goccia.Timeout,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.AsyncFunctionValue,
  Goccia.Values.Await,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.EnumValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.GeneratorValue,
  Goccia.Values.IteratorSupport,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.ProxyValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject,
  Goccia.Values.ToPrimitive;

procedure RunClassInstanceInitializers(const AClassValue: TGocciaClassValue;
  const AInstance: TGocciaObjectValue;
  const AContext: TGocciaEvaluationContext;
  const AInitializationMode: TGocciaInstanceInitializationMode); forward;
function DisposeTrackedResources(const ATracker: TGocciaDisposalTracker;
  const AExistingError: TGocciaValue): TGocciaValue; forward;
function DisposeTrackedResourcesAsync(const ATracker: TGocciaDisposalTracker;
  const AExistingError: TGocciaValue): TGocciaValue; forward;
function HasAsyncDisposals(const ATracker: TGocciaDisposalTracker): Boolean; forward;
function CollectDeclaredPrivateNames(
  const AContext: TGocciaEvaluationContext): TStringList; forward;

const
  FOR_IN_ENTRY_OWNER = '__gocciaForInOwner';
  FOR_IN_ENTRY_KEY = '__gocciaForInKey';
  FOR_IN_MAX_PROTOTYPE_CHAIN_DEPTH = 256;

type
  TForInArrayIndexKey = record
    Key: string;
    Index: Int64;
  end;

  TGocciaTemplateObjectArrayValue = class(TGocciaArrayValue)
  public
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;
    procedure SetProperty(const AName: string;
      const AValue: TGocciaValue); override;
    function DeleteProperty(const AName: string): Boolean; override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
  end;

  TPreparedDestructuringReferenceKind = (
    pdrNone,
    pdrStringProperty,
    pdrComputedProperty,
    pdrSymbolProperty,
    pdrPrivateProperty
  );

  TPreparedDestructuringReference = record
    Kind: TPreparedDestructuringReferenceKind;
    ObjectValue: TGocciaValue;
    PropertyName: string;
    ComputedKeyValue: TGocciaValue;
    SymbolValue: TGocciaSymbolValue;
    PrivateName: string;
    Line: Integer;
    Column: Integer;
    ObjectRooted: Boolean;
    ComputedKeyRooted: Boolean;
    SymbolRooted: Boolean;
  end;

  TClassCallableEvalKind = (
    ccekMethod,
    ccekGetter,
    ccekSetter,
    ccekElement
  );

  TClassCallableEvalEntry = record
    Kind: TClassCallableEvalKind;
    Name: string;
    IsStatic: Boolean;
    IsPrivate: Boolean;
    Line: Integer;
    Column: Integer;
    Order: Integer;
    ElementIndex: Integer;
    MethodNode: TGocciaClassMethod;
    GetterNode: TGocciaGetterExpression;
    SetterNode: TGocciaSetterExpression;
  end;

  TClassCallableEvalEntries = array of TClassCallableEvalEntry;

function FunctionIntrinsicKind(const AIsAsync,
  AIsGenerator: Boolean): TGocciaFunctionObjectIntrinsicKind; inline;
begin
  if AIsAsync and AIsGenerator then
    Result := foikAsyncGenerator
  else if AIsGenerator then
    Result := foikGenerator
  else if AIsAsync then
    Result := foikAsync
  else
    Result := foikOrdinary;
end;

function TGocciaTemplateObjectArrayValue.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
begin
  Result := inherited GetOwnPropertyDescriptor(AName);
end;

procedure TGocciaTemplateObjectArrayValue.SetProperty(const AName: string;
  const AValue: TGocciaValue);
begin
  inherited SetProperty(AName, AValue);
end;

function TGocciaTemplateObjectArrayValue.DeleteProperty(
  const AName: string): Boolean;
begin
  Result := inherited DeleteProperty(AName);
end;

function TGocciaTemplateObjectArrayValue.TryDefineProperty(
  const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := inherited TryDefineProperty(AName, ADescriptor);
end;

function TryParseForInArrayIndex(const AKey: string; out AIndex: Int64): Boolean;
var
  I: Integer;
  Digit: Int64;
  MaxIndex: Int64;
begin
  AIndex := 0;
  Result := False;
  if AKey = '' then
    Exit;
  if (AKey[1] = '0') and (Length(AKey) > 1) then
    Exit;

  MaxIndex := MAX_ARRAY_LENGTH - 1;
  for I := 1 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then
      Exit;
    Digit := Ord(AKey[I]) - Ord('0');
    if AIndex > (MaxIndex - Digit) div 10 then
      Exit;
    AIndex := AIndex * 10 + Digit;
  end;

  Result := True;
end;

function OrderForInPropertyKeys(const AKeys: TArray<string>): TArray<string>;
var
  IndexKeys: TArray<TForInArrayIndexKey>;
  OtherKeys: TArray<string>;
  IndexCount, OtherCount: Integer;
  I, J, ResultIndex: Integer;
  Index: Int64;
  Current: TForInArrayIndexKey;
begin
  SetLength(IndexKeys, Length(AKeys));
  SetLength(OtherKeys, Length(AKeys));
  IndexCount := 0;
  OtherCount := 0;

  for I := 0 to High(AKeys) do
  begin
    if TryParseForInArrayIndex(AKeys[I], Index) then
    begin
      IndexKeys[IndexCount].Key := AKeys[I];
      IndexKeys[IndexCount].Index := Index;
      Inc(IndexCount);
    end
    else
    begin
      OtherKeys[OtherCount] := AKeys[I];
      Inc(OtherCount);
    end;
  end;

  for I := 1 to IndexCount - 1 do
  begin
    Current := IndexKeys[I];
    J := I - 1;
    while (J >= 0) and (IndexKeys[J].Index > Current.Index) do
    begin
      IndexKeys[J + 1] := IndexKeys[J];
      Dec(J);
    end;
    IndexKeys[J + 1] := Current;
  end;

  SetLength(Result, IndexCount + OtherCount);
  ResultIndex := 0;
  for I := 0 to IndexCount - 1 do
  begin
    Result[ResultIndex] := IndexKeys[I].Key;
    Inc(ResultIndex);
  end;
  for I := 0 to OtherCount - 1 do
  begin
    Result[ResultIndex] := OtherKeys[I];
    Inc(ResultIndex);
  end;
end;

procedure EnsureObjectPrototypeInitialized; inline;
begin
  if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
    TGocciaObjectValue.InitializeSharedPrototype;
end;

function CopyDataPropertyKeyExcluded(const AKey: TGocciaValue;
  const AExcludedStringKeys: TStringList;
  const AExcludedSymbolKeys: TList<TGocciaSymbolValue>): Boolean;
begin
  if AKey is TGocciaSymbolValue then
  begin
    Result := Assigned(AExcludedSymbolKeys) and
      (AExcludedSymbolKeys.IndexOf(TGocciaSymbolValue(AKey)) >= 0);
    Exit;
  end;

  Result := Assigned(AExcludedStringKeys) and
    (AExcludedStringKeys.IndexOf(AKey.ToStringLiteral.Value) >= 0);
end;

function TryParseArrayPropertyIndex(const AKey: string;
  out AIndex: Int64): Boolean;
var
  Digit: Int64;
  I: Integer;
begin
  AIndex := 0;
  Result := False;
  if AKey = '' then
    Exit;
  if (AKey[1] = '0') and (Length(AKey) > 1) then
    Exit;

  for I := 1 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then
      Exit;
    Digit := Ord(AKey[I]) - Ord('0');
    if AIndex > (MAX_SAFE_INTEGER - Digit) div 10 then
      Exit;
    AIndex := AIndex * 10 + Digit;
  end;

  Result := AIndex < MAX_ARRAY_LENGTH;
end;

function OrderOwnPropertyStringKeys(const AKeys: TArray<string>):
  TArray<string>;
var
  ParsedIndex, TempIndex: Int64;
  NumericKeys: TArray<Int64>;
  OtherKeys: TArray<string>;
  I, J, K, Count: Integer;
begin
  SetLength(NumericKeys, Length(AKeys));
  SetLength(OtherKeys, Length(AKeys));
  Count := 0;
  J := 0;

  for I := 0 to High(AKeys) do
  begin
    if TryParseArrayPropertyIndex(AKeys[I], ParsedIndex) then
    begin
      NumericKeys[Count] := ParsedIndex;
      Inc(Count);
    end
    else
    begin
      OtherKeys[J] := AKeys[I];
      Inc(J);
    end;
  end;

  for I := 1 to Count - 1 do
  begin
    TempIndex := NumericKeys[I];
    K := I - 1;
    while (K >= 0) and (NumericKeys[K] > TempIndex) do
    begin
      NumericKeys[K + 1] := NumericKeys[K];
      Dec(K);
    end;
    NumericKeys[K + 1] := TempIndex;
  end;

  SetLength(Result, Count + J);
  for I := 0 to Count - 1 do
    Result[I] := IntToStr(NumericKeys[I]);
  for I := 0 to J - 1 do
    Result[Count + I] := OtherKeys[I];
end;

function OwnPropertyKeysAsValues(const ASource: TGocciaObjectValue):
  TArray<TGocciaValue>;
var
  I, Count: Integer;
  StringKeys: TArray<string>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
begin
  if ASource is TGocciaProxyValue then
    Exit(TGocciaProxyValue(ASource).GetOwnPropertyKeyValues);

  StringKeys := OrderOwnPropertyStringKeys(ASource.GetAllPropertyNames);
  SymbolKeys := ASource.GetOwnSymbols;
  SetLength(Result, Length(StringKeys) + Length(SymbolKeys));
  Count := 0;
  for I := 0 to High(StringKeys) do
  begin
    Result[Count] := TGocciaStringLiteralValue.Create(StringKeys[I]);
    Inc(Count);
  end;
  for I := 0 to High(SymbolKeys) do
  begin
    Result[Count] := SymbolKeys[I];
    Inc(Count);
  end;
end;

// ES2026 §7.3.25 CopyDataProperties(target, source, excludedItems).
procedure CopyDataProperties(const ATarget: TGocciaObjectValue;
  const ASource: TGocciaValue;
  const AExcludedStringKeys: TStringList = nil;
  const AExcludedSymbolKeys: TList<TGocciaSymbolValue> = nil);
var
  Descriptor: TGocciaPropertyDescriptor;
  Key: TGocciaValue;
  Keys: TArray<TGocciaValue>;
  KeyName: string;
  SourceObject: TGocciaObjectValue;
  SourceRooted: Boolean;
  SymbolKey: TGocciaSymbolValue;
  Value: TGocciaValue;
begin
  if (ASource is TGocciaUndefinedLiteralValue) or
     (ASource is TGocciaNullLiteralValue) then
    Exit;

  SourceObject := ToObject(ASource);
  SourceRooted := Assigned(TGarbageCollector.Instance) and
    not (ASource is TGocciaObjectValue);
  if SourceRooted then
    TGarbageCollector.Instance.AddTempRoot(SourceObject);

  try
    Keys := OwnPropertyKeysAsValues(SourceObject);
    for Key in Keys do
    begin
      if CopyDataPropertyKeyExcluded(Key, AExcludedStringKeys,
        AExcludedSymbolKeys) then
        Continue;

      if Key is TGocciaSymbolValue then
      begin
        SymbolKey := TGocciaSymbolValue(Key);
        Descriptor := SourceObject.GetOwnSymbolPropertyDescriptor(SymbolKey);
        if Assigned(Descriptor) and Descriptor.Enumerable then
        begin
          Value := SourceObject.GetSymbolProperty(SymbolKey);
          ATarget.CreateDataPropertyOrThrow(SymbolKey, Value);
        end;
      end
      else
      begin
        KeyName := Key.ToStringLiteral.Value;
        Descriptor := SourceObject.GetOwnPropertyDescriptor(KeyName);
        if Assigned(Descriptor) and Descriptor.Enumerable then
        begin
          Value := SourceObject.GetProperty(KeyName);
          ATarget.CreateDataPropertyOrThrow(KeyName, Value);
        end;
      end;
    end;
  finally
    if SourceRooted then
      TGarbageCollector.Instance.RemoveTempRoot(SourceObject);
  end;
end;

function UndefinedCompletionValue: TGocciaValue; inline;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure UpdateValueFromCompletion(const ACompletion: TGocciaControlFlow;
  var AValue: TGocciaValue); inline;
begin
  if Assigned(ACompletion.Value) then
    AValue := ACompletion.Value;
end;

function NormalCompletionFromAbrupt(const ACompletion: TGocciaControlFlow;
  const AValue: TGocciaValue): TGocciaControlFlow; inline;
var
  UpdatedCompletion: TGocciaControlFlow;
begin
  UpdatedCompletion := ACompletion.UpdateEmpty(AValue);
  Result := TGocciaControlFlow.Normal(UpdatedCompletion.Value);
end;

function EvaluateExpressionWithLoopHeadTDZ(
  const AExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext;
  const ABindingName: string;
  const ABindingPattern: TGocciaDestructuringPattern;
  const AHasLexicalDeclaration: Boolean): TGocciaValue;
var
  Names: TStringList;
  I: Integer;
  TDZScope: TGocciaScope;
  TDZContext: TGocciaEvaluationContext;
  ScopeRooted: Boolean;
begin
  if not AHasLexicalDeclaration then
    Exit(EvaluateExpression(AExpression, AContext));

  Names := TStringList.Create;
  try
    Names.CaseSensitive := True;
    if Assigned(ABindingPattern) then
      CollectPatternBindingNames(ABindingPattern, Names, True)
    else if ABindingName <> '' then
      Names.Add(ABindingName);

    if Names.Count = 0 then
      Exit(EvaluateExpression(AExpression, AContext));

    TDZScope := AContext.Scope.CreateChild(skBlock);
    for I := 0 to Names.Count - 1 do
      TDZScope.PredeclareLexicalBinding(Names[I], dtLet);

    TDZContext := AContext;
    TDZContext.Scope := TDZScope;
    ScopeRooted := Assigned(TGarbageCollector.Instance);
    if ScopeRooted then
      TGarbageCollector.Instance.AddTempRoot(TDZScope);
    try
      Result := EvaluateExpression(AExpression, TDZContext);
    finally
      if ScopeRooted then
        TGarbageCollector.Instance.RemoveTempRoot(TDZScope);
    end;
  finally
    Names.Free;
  end;
end;

function CurrentFunctionObjectPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
begin
  EnsureObjectPrototypeInitialized;
  IteratorPrototype := nil;
  if AKind = foikGenerator then
    IteratorPrototype := TGocciaIteratorValue.SharedPrototype;
  Result := FunctionObjectIntrinsicPrototype(AKind,
    TGocciaFunctionBase.GetSharedPrototype,
    TGocciaObjectValue.SharedObjectPrototype,
    IteratorPrototype);
end;

function CurrentGeneratorObjectPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
begin
  EnsureObjectPrototypeInitialized;
  IteratorPrototype := nil;
  if AKind = foikGenerator then
    IteratorPrototype := TGocciaIteratorValue.SharedPrototype;
  Result := GeneratorObjectIntrinsicPrototype(AKind,
    TGocciaFunctionBase.GetSharedPrototype,
    TGocciaObjectValue.SharedObjectPrototype,
    IteratorPrototype);
end;

procedure ApplyFunctionObjectPrototype(const AFunction: TGocciaValue;
  const AKind: TGocciaFunctionObjectIntrinsicKind);
var
  Prototype: TGocciaObjectValue;
begin
  if not (AFunction is TGocciaObjectValue) then
    Exit;
  Prototype := CurrentFunctionObjectPrototype(AKind);
  if Assigned(Prototype) then
    TGocciaObjectValue(AFunction).Prototype := Prototype;
end;

procedure InstallFunctionOwnPrototypeProperty(const AFunction: TGocciaValue;
  const AKind: TGocciaFunctionObjectIntrinsicKind);
var
  PrototypeObj: TGocciaObjectValue;
begin
  if not (AFunction is TGocciaObjectValue) then
    Exit;

  if AKind in [foikGenerator, foikAsyncGenerator] then
    PrototypeObj := TGocciaObjectValue.Create(CurrentGeneratorObjectPrototype(AKind))
  else
  begin
    PrototypeObj := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype);
    PrototypeObj.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(AFunction, [pfWritable, pfConfigurable]));
  end;

  TGocciaObjectValue(AFunction).DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(PrototypeObj, [pfWritable]));
end;

procedure AddValueRoot(var ARoots: TGocciaActiveRootFrame;
  const AValue: TGocciaValue); inline;
begin
  if Assigned(AValue) then
    ARoots.Add(AValue);
end;

procedure RootArgumentsFrom(var ARoots: TGocciaActiveRootFrame;
  const AArguments: TGocciaArgumentsCollection; const AStartIndex: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AArguments.Length - 1 do
    AddValueRoot(ARoots, AArguments.GetElement(I));
end;

procedure CollectInterpreterMemoryPressure(const AProtect: TGocciaValue); inline;
var
  GC: TGarbageCollector;
begin
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.CollectForMemoryPressure(AProtect);
end;

function VarBindingNameCollectionMode(
  const ANonStrictMode, ACompatibilityNonStrictMode: Boolean):
  TGocciaVarBindingNameCollectionMode;
begin
  if ANonStrictMode and ACompatibilityNonStrictMode then
    Result := vbnNonStrictScriptCompatibility
  else
    Result := vbnStandard;
end;

// Helper: create a non-owning copy of a statement list (AST owns the nodes)
function CopyStatementList(const ASource: TObjectList<TGocciaASTNode>): TObjectList<TGocciaASTNode>;
var
  I: Integer;
begin
  Result := TObjectList<TGocciaASTNode>.Create(False);
  for I := 0 to ASource.Count - 1 do
    Result.Add(ASource[I]);
end;

procedure HoistVarDeclarations(const AStatements: TObjectList<TGocciaStatement>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext);
var
  Names: TStringList;
  I: Integer;
begin
  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectVarBindingNamesFromStatements(AStatements, Names,
      VarBindingNameCollectionMode(AContext.NonStrictMode,
        AContext.CompatibilityNonStrictMode));
    for I := 0 to Names.Count - 1 do
      AScope.DefineVariableBinding(Names[I], TGocciaUndefinedLiteralValue.UndefinedValue, False);
  finally
    Names.Free;
  end;
end;

procedure HoistVarDeclarations(const ANodes: TObjectList<TGocciaASTNode>;
  const AScope: TGocciaScope;
  const AContext: TGocciaEvaluationContext);
var
  Names: TStringList;
  I: Integer;
begin
  Names := TStringList.Create;
  Names.CaseSensitive := True;
  try
    CollectVarBindingNamesFromNodes(ANodes, Names,
      VarBindingNameCollectionMode(AContext.NonStrictMode,
        AContext.CompatibilityNonStrictMode));
    for I := 0 to Names.Count - 1 do
      AScope.DefineVariableBinding(Names[I], TGocciaUndefinedLiteralValue.UndefinedValue, False);
  finally
    Names.Free;
  end;
end;

function BlockLexicalDeclarationType(const AIsConst: Boolean): TGocciaDeclarationType;
begin
  if AIsConst then
    Result := dtConst
  else
    Result := dtLet;
end;

function IsNamedDefaultFunctionDeclaration(
  const ADecl: TGocciaExportDefaultDeclaration): Boolean;
begin
  Result := (ADecl.LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (ADecl.Expression is TGocciaFunctionExpression) and
    (TGocciaFunctionExpression(ADecl.Expression).Name = ADecl.LocalName);
end;

function IsNamedDefaultClassDeclaration(
  const ADecl: TGocciaExportDefaultDeclaration): Boolean;
begin
  Result := (ADecl.LocalName <> GOCCIA_DEFAULT_EXPORT_BINDING) and
    (ADecl.Expression is TGocciaClassExpression) and
    (TGocciaClassExpression(ADecl.Expression).ClassDefinition.Name =
    ADecl.LocalName);
end;

procedure PredeclareBlockLexicalName(const AScope: TGocciaScope; const AName: string;
  const ADeclarationType: TGocciaDeclarationType; const ALine, AColumn: Integer);
begin
  AScope.PredeclareLexicalBinding(AName, ADeclarationType, ALine, AColumn);
end;

procedure PredeclareBlockLexicalBinding(const ANode: TGocciaASTNode;
  const AScope: TGocciaScope; const AIncludeFunctionDeclarations: Boolean = True);
var
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  ImportDecl: TGocciaImportDeclaration;
  ImportPair: TStringStringMap.TKeyValuePair;
  UsingDecl: TGocciaUsingDeclaration;
  Names: TStringList;
  I: Integer;
begin
  if ANode is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(ANode);
    if VarDecl.IsVar then
      Exit;
    for I := 0 to High(VarDecl.Variables) do
      PredeclareBlockLexicalName(AScope, VarDecl.Variables[I].Name,
        BlockLexicalDeclarationType(VarDecl.IsConst), VarDecl.Line,
        VarDecl.Column);
  end
  else if ANode is TGocciaExportVariableDeclaration then
  begin
    VarDecl := TGocciaExportVariableDeclaration(ANode).Declaration;
    if VarDecl.IsVar then
      Exit;
    for I := 0 to High(VarDecl.Variables) do
      PredeclareBlockLexicalName(AScope, VarDecl.Variables[I].Name,
        BlockLexicalDeclarationType(VarDecl.IsConst), VarDecl.Line,
        VarDecl.Column);
  end
  else if (ANode is TGocciaFunctionDeclaration) and
          AIncludeFunctionDeclarations then
    PredeclareBlockLexicalName(AScope,
      TGocciaFunctionDeclaration(ANode).Name, dtLet, ANode.Line,
      ANode.Column)
  else if (ANode is TGocciaExportFunctionDeclaration) and
          AIncludeFunctionDeclarations then
    PredeclareBlockLexicalName(AScope,
      TGocciaExportFunctionDeclaration(ANode).Declaration.Name, dtLet,
      ANode.Line, ANode.Column)
  else if ANode is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(ANode);
    if DestructDecl.IsVar then
      Exit;
    Names := TStringList.Create;
    Names.CaseSensitive := True;
    try
      CollectPatternBindingNames(DestructDecl.Pattern, Names, True);
      for I := 0 to Names.Count - 1 do
        PredeclareBlockLexicalName(AScope, Names[I],
          BlockLexicalDeclarationType(DestructDecl.IsConst), DestructDecl.Line,
          DestructDecl.Column);
    finally
      Names.Free;
    end;
  end
  else if ANode is TGocciaExportDestructuringDeclaration then
  begin
    DestructDecl := TGocciaExportDestructuringDeclaration(ANode).Declaration;
    if DestructDecl.IsVar then
      Exit;
    Names := TStringList.Create;
    Names.CaseSensitive := True;
    try
      CollectPatternBindingNames(DestructDecl.Pattern, Names, True);
      for I := 0 to Names.Count - 1 do
        PredeclareBlockLexicalName(AScope, Names[I],
          BlockLexicalDeclarationType(DestructDecl.IsConst), DestructDecl.Line,
          DestructDecl.Column);
    finally
      Names.Free;
    end;
  end
  else if ANode is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(ANode);
    for I := 0 to High(UsingDecl.Variables) do
      PredeclareBlockLexicalName(AScope, UsingDecl.Variables[I].Name, dtConst,
        UsingDecl.Line, UsingDecl.Column);
  end
  else if ANode is TGocciaImportDeclaration then
  begin
    ImportDecl := TGocciaImportDeclaration(ANode);
    if ImportDecl.NamespaceName <> '' then
      PredeclareBlockLexicalName(AScope, ImportDecl.NamespaceName, dtConst,
        ImportDecl.Line, ImportDecl.Column);
    for ImportPair in ImportDecl.Imports do
      PredeclareBlockLexicalName(AScope, ImportPair.Key, dtConst,
        ImportDecl.Line, ImportDecl.Column);
  end
  else if ANode is TGocciaClassDeclaration then
    PredeclareBlockLexicalName(AScope,
      TGocciaClassDeclaration(ANode).ClassDefinition.Name, dtLet, ANode.Line,
      ANode.Column)
  else if ANode is TGocciaExportClassDeclaration then
    PredeclareBlockLexicalName(AScope,
      TGocciaExportClassDeclaration(ANode).Declaration.ClassDefinition.Name,
      dtLet, ANode.Line, ANode.Column)
  else if ANode is TGocciaExportDefaultDeclaration then
  begin
    ExportDefaultDecl := TGocciaExportDefaultDeclaration(ANode);
    PredeclareBlockLexicalName(AScope,
      ExportDefaultDecl.LocalName,
      BlockLexicalDeclarationType(
        not (IsNamedDefaultFunctionDeclaration(ExportDefaultDecl) or
        IsNamedDefaultClassDeclaration(ExportDefaultDecl))),
      ANode.Line, ANode.Column);
  end
  else if ANode is TGocciaEnumDeclaration then
    PredeclareBlockLexicalName(AScope, TGocciaEnumDeclaration(ANode).Name,
      dtLet, ANode.Line, ANode.Column)
  else if ANode is TGocciaExportEnumDeclaration then
    PredeclareBlockLexicalName(AScope,
      TGocciaExportEnumDeclaration(ANode).Declaration.Name, dtLet,
      ANode.Line, ANode.Column);
end;

procedure PredeclareBlockLexicalBindings(const ANodes: TObjectList<TGocciaASTNode>;
  const AContext: TGocciaEvaluationContext);
var
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
    PredeclareBlockLexicalBinding(ANodes[I], AContext.Scope);
end;

procedure PredeclareModuleLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    PredeclareBlockLexicalBinding(AProgram.Body[I], AScope, True);
end;

procedure PredeclareScriptLexicalDeclarations(const AProgram: TGocciaProgram;
  const AScope: TGocciaScope);
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    PredeclareBlockLexicalBinding(AProgram.Body[I], AScope, False);
end;

procedure PredeclareFunctionBodyLexicalDeclarations(
  const ANodes: TObjectList<TGocciaASTNode>; const AScope: TGocciaScope);
var
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
    PredeclareBlockLexicalBinding(ANodes[I], AScope, False);
end;

procedure HoistSingleFunctionDeclaration(const ANode: TGocciaASTNode;
  const AContext: TGocciaEvaluationContext; const ABlockScoped: Boolean);
var
  ExportDefaultDecl: TGocciaExportDefaultDeclaration;
  FuncDecl: TGocciaFunctionDeclaration;
  FuncExpr: TGocciaFunctionExpression;
  TargetScope: TGocciaScope;
  Value: TGocciaValue;
  Name: string;
  ExistingBinding: TLexicalBinding;
begin
  FuncDecl := nil;
  FuncExpr := nil;
  Name := '';
  if ANode is TGocciaFunctionDeclaration then
  begin
    FuncDecl := TGocciaFunctionDeclaration(ANode)
  end
  else if ANode is TGocciaExportFunctionDeclaration then
  begin
    FuncDecl := TGocciaExportFunctionDeclaration(ANode).Declaration
  end
  else if ANode is TGocciaExportDefaultDeclaration then
  begin
    ExportDefaultDecl := TGocciaExportDefaultDeclaration(ANode);
    if IsNamedDefaultFunctionDeclaration(ExportDefaultDecl) then
    begin
      FuncExpr := TGocciaFunctionExpression(ExportDefaultDecl.Expression);
      Name := ExportDefaultDecl.LocalName;
    end;
  end
  else
    Exit;

  if Assigned(FuncDecl) then
  begin
    Name := FuncDecl.Name;
    FuncExpr := FuncDecl.FunctionExpression;
  end;

  if not Assigned(FuncExpr) then
    Exit;

  Value := FuncExpr.Evaluate(AContext);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Value);
  try
    if (Value is TGocciaFunctionValue) and (TGocciaFunctionValue(Value).Name = '') then
      TGocciaFunctionValue(Value).Name := Name;
    if AContext.Scope.ContainsOwnLexicalBinding(Name) then
      AContext.Scope.ForceUpdateBinding(Name, Value)
    else if ABlockScoped then
    begin
      AContext.Scope.DefineLexicalBinding(Name, Value, dtLet);
    end
    else
    begin
      TargetScope := AContext.Scope.FindFunctionOrModuleScope;
      if not Assigned(TargetScope) then
        TargetScope := AContext.Scope;
      if Assigned(TargetScope) and (TargetScope.ScopeKind = skGlobal) then
        TargetScope.CreateGlobalFunctionBinding(Name, Value, False)
      else if TargetScope.TryGetOwnBinding(Name, ExistingBinding) and
              (ExistingBinding.DeclarationType = dtParameter) then
        TargetScope.ForceUpdateBinding(Name, Value)
      else
        TargetScope.DefineVariableBinding(Name, Value, True);
    end;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Value);
  end;
end;

procedure HoistFunctionDeclarations(const AStatements: TObjectList<TGocciaStatement>;
  const AContext: TGocciaEvaluationContext; const ABlockScoped: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to AStatements.Count - 1 do
    HoistSingleFunctionDeclaration(AStatements[I], AContext, ABlockScoped);
end;

procedure HoistFunctionDeclarations(const ANodes: TObjectList<TGocciaASTNode>;
  const AContext: TGocciaEvaluationContext; const ABlockScoped: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
    HoistSingleFunctionDeclaration(ANodes[I], AContext, ABlockScoped);
end;

procedure ActivateCompatBlockFunctionDeclaration(
  const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext);
var
  FuncDecl: TGocciaFunctionDeclaration;
  Name: string;
  Value: TGocciaValue;
begin
  if (not AContext.NonStrictMode) or
     (not AContext.CompatibilityNonStrictMode) then
    Exit;
  if AStatement is TGocciaFunctionDeclaration then
    FuncDecl := TGocciaFunctionDeclaration(AStatement)
  else if AStatement is TGocciaExportFunctionDeclaration then
    FuncDecl := TGocciaExportFunctionDeclaration(AStatement).Declaration
  else
    Exit;

  if FuncDecl.FunctionExpression.IsAsync or FuncDecl.FunctionExpression.IsGenerator then
    Exit;

  Name := FuncDecl.Name;
  if not AContext.Scope.ContainsOwnLexicalBinding(Name) then
    Exit;

  Value := AContext.Scope.GetBinding(Name, AStatement.Line, AStatement.Column).Value;
  if AContext.InEvalCode and Assigned(AContext.EvalVarScope) then
  begin
    AContext.EvalVarScope.DefineVariableBinding(Name, Value, True, True);
    Exit;
  end;

  AContext.Scope.DefineVariableBinding(Name, Value, True);
end;

procedure AddUniqueEvalName(const ANames: TStringList; const AName: string);
begin
  if (AName <> '') and (ANames.IndexOf(AName) < 0) then
    ANames.Add(AName);
end;

function EvalFunctionDeclarationFromNode(
  const ANode: TGocciaASTNode): TGocciaFunctionDeclaration;
begin
  if ANode is TGocciaFunctionDeclaration then
    Result := TGocciaFunctionDeclaration(ANode)
  else if ANode is TGocciaExportFunctionDeclaration then
    Result := TGocciaExportFunctionDeclaration(ANode).Declaration
  else
    Result := nil;
end;

procedure CollectEvalFunctionDeclarations(
  const ANodes: TObjectList<TGocciaStatement>;
  const ADeclarations: TObjectList<TGocciaFunctionDeclaration>);
var
  FuncDecl: TGocciaFunctionDeclaration;
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
  begin
    FuncDecl := EvalFunctionDeclarationFromNode(ANodes[I]);
    if Assigned(FuncDecl) then
      ADeclarations.Add(FuncDecl);
  end;
end;

procedure CollectTopLevelEvalLexicalNames(const ANodes: TObjectList<TGocciaStatement>;
  const ANames: TStringList);
var
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  Names: TStringList;
  I, J: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
  begin
    if ANodes[I] is TGocciaVariableDeclaration then
    begin
      VarDecl := TGocciaVariableDeclaration(ANodes[I]);
      if VarDecl.IsVar then
        Continue;
      for J := 0 to High(VarDecl.Variables) do
        AddUniqueEvalName(ANames, VarDecl.Variables[J].Name);
    end
    else if ANodes[I] is TGocciaExportVariableDeclaration then
    begin
      VarDecl := TGocciaExportVariableDeclaration(ANodes[I]).Declaration;
      if VarDecl.IsVar then
        Continue;
      for J := 0 to High(VarDecl.Variables) do
        AddUniqueEvalName(ANames, VarDecl.Variables[J].Name);
    end
    else if ANodes[I] is TGocciaDestructuringDeclaration then
    begin
      DestructDecl := TGocciaDestructuringDeclaration(ANodes[I]);
      if DestructDecl.IsVar then
        Continue;
      Names := TStringList.Create;
      try
        Names.CaseSensitive := True;
        CollectPatternBindingNames(DestructDecl.Pattern, Names, True);
        for J := 0 to Names.Count - 1 do
          AddUniqueEvalName(ANames, Names[J]);
      finally
        Names.Free;
      end;
    end
    else if ANodes[I] is TGocciaClassDeclaration then
      AddUniqueEvalName(ANames,
        TGocciaClassDeclaration(ANodes[I]).ClassDefinition.Name)
    else if ANodes[I] is TGocciaEnumDeclaration then
      AddUniqueEvalName(ANames, TGocciaEnumDeclaration(ANodes[I]).Name)
    else if ANodes[I] is TGocciaExportEnumDeclaration then
      AddUniqueEvalName(ANames,
        TGocciaExportEnumDeclaration(ANodes[I]).Declaration.Name);
  end;
end;

procedure PredeclareTopLevelEvalLexicalBindings(
  const ANodes: TObjectList<TGocciaStatement>; const AScope: TGocciaScope);
var
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  Names: TStringList;
  I, J: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
  begin
    if ANodes[I] is TGocciaVariableDeclaration then
    begin
      VarDecl := TGocciaVariableDeclaration(ANodes[I]);
      if VarDecl.IsVar then
        Continue;
      for J := 0 to High(VarDecl.Variables) do
        PredeclareBlockLexicalName(AScope, VarDecl.Variables[J].Name,
          BlockLexicalDeclarationType(VarDecl.IsConst), VarDecl.Line,
          VarDecl.Column);
    end
    else if ANodes[I] is TGocciaExportVariableDeclaration then
    begin
      VarDecl := TGocciaExportVariableDeclaration(ANodes[I]).Declaration;
      if VarDecl.IsVar then
        Continue;
      for J := 0 to High(VarDecl.Variables) do
        PredeclareBlockLexicalName(AScope, VarDecl.Variables[J].Name,
          BlockLexicalDeclarationType(VarDecl.IsConst), VarDecl.Line,
          VarDecl.Column);
    end
    else if ANodes[I] is TGocciaDestructuringDeclaration then
    begin
      DestructDecl := TGocciaDestructuringDeclaration(ANodes[I]);
      if DestructDecl.IsVar then
        Continue;
      Names := TStringList.Create;
      try
        Names.CaseSensitive := True;
        CollectPatternBindingNames(DestructDecl.Pattern, Names, True);
        for J := 0 to Names.Count - 1 do
          PredeclareBlockLexicalName(AScope, Names[J],
            BlockLexicalDeclarationType(DestructDecl.IsConst),
            DestructDecl.Line, DestructDecl.Column);
      finally
        Names.Free;
      end;
    end
    else if ANodes[I] is TGocciaClassDeclaration then
      PredeclareBlockLexicalName(AScope,
        TGocciaClassDeclaration(ANodes[I]).ClassDefinition.Name, dtLet,
        ANodes[I].Line, ANodes[I].Column)
    else if ANodes[I] is TGocciaEnumDeclaration then
      PredeclareBlockLexicalName(AScope, TGocciaEnumDeclaration(ANodes[I]).Name,
        dtLet, ANodes[I].Line, ANodes[I].Column)
    else if ANodes[I] is TGocciaExportEnumDeclaration then
      PredeclareBlockLexicalName(AScope,
        TGocciaExportEnumDeclaration(ANodes[I]).Declaration.Name, dtLet,
        ANodes[I].Line, ANodes[I].Column);
  end;
end;

procedure ValidateEvalScriptBody(const ANodes: TObjectList<TGocciaStatement>);
var
  I: Integer;
begin
  for I := 0 to ANodes.Count - 1 do
  begin
    if (ANodes[I] is TGocciaImportDeclaration) or
       (ANodes[I] is TGocciaExportDeclaration) or
       (ANodes[I] is TGocciaExportDefaultDeclaration) or
       (ANodes[I] is TGocciaExportVariableDeclaration) or
       (ANodes[I] is TGocciaExportFunctionDeclaration) or
       (ANodes[I] is TGocciaExportEnumDeclaration) or
       (ANodes[I] is TGocciaReExportDeclaration) then
      ThrowSyntaxError('Module declarations are not allowed in eval code');
    if ANodes[I] is TGocciaUsingDeclaration then
      ThrowSyntaxError(
        'Using declarations are not allowed at the top level of eval');
  end;
end;

function EvalExpressionContainsArgumentsReference(
  const AExpr: TGocciaExpression): Boolean; forward;
function EvalPatternContainsArgumentsReference(
  const APattern: TGocciaDestructuringPattern): Boolean; forward;
function EvalStatementContainsArgumentsReference(
  const AStmt: TGocciaStatement): Boolean; forward;

function EvalParameterListContainsArgumentsReference(
  const AParameters: TGocciaParameterArray): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(AParameters) do
  begin
    if AParameters[I].IsPattern and
       EvalPatternContainsArgumentsReference(AParameters[I].Pattern) then
      Exit(True);
    if EvalExpressionContainsArgumentsReference(AParameters[I].DefaultValue) then
      Exit(True);
  end;
  Result := False;
end;

function EvalASTNodeContainsArgumentsReference(
  const ANode: TGocciaASTNode): Boolean;
begin
  if ANode is TGocciaStatement then
    Exit(EvalStatementContainsArgumentsReference(TGocciaStatement(ANode)));
  if ANode is TGocciaExpression then
    Exit(EvalExpressionContainsArgumentsReference(TGocciaExpression(ANode)));
  Result := False;
end;

function EvalExpressionContainsArgumentsReference(
  const AExpr: TGocciaExpression): Boolean;
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  ArrowExpr: TGocciaArrowFunctionExpression;
  NewExpr: TGocciaNewExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit(False);

  if AExpr is TGocciaIdentifierExpression then
    Exit(TGocciaIdentifierExpression(AExpr).Name = IDENTIFIER_ARGUMENTS)
  else if AExpr is TGocciaArrowFunctionExpression then
  begin
    ArrowExpr := TGocciaArrowFunctionExpression(AExpr);
    if EvalParameterListContainsArgumentsReference(ArrowExpr.Parameters) then
      Exit(True);
    Exit(EvalASTNodeContainsArgumentsReference(ArrowExpr.Body));
  end
  else if AExpr is TGocciaFunctionExpression then
    Exit(False)
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if EvalExpressionContainsArgumentsReference(CallExpr.Callee) then
      Exit(True);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      if EvalExpressionContainsArgumentsReference(CallExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if EvalExpressionContainsArgumentsReference(MemberExpr.ObjectExpr) then
      Exit(True);
    if MemberExpr.Computed and
       EvalExpressionContainsArgumentsReference(MemberExpr.PropertyExpression) then
      Exit(True);
  end
  else if AExpr is TGocciaBinaryExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaBinaryExpression(AExpr).Left) or
      EvalExpressionContainsArgumentsReference(
        TGocciaBinaryExpression(AExpr).Right))
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      if EvalExpressionContainsArgumentsReference(
        TGocciaSequenceExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaUnaryExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaUnaryExpression(AExpr).Operand))
  else if AExpr is TGocciaAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression) or
      EvalExpressionContainsArgumentsReference(
        TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaIncrementExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaIncrementExpression(AExpr).Operand))
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      if EvalExpressionContainsArgumentsReference(ArrayExpr.Elements[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          if EvalExpressionContainsArgumentsReference(
              ObjectExpr.PropertySourceOrder[I].Expression) then
            Exit(True);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if EvalExpressionContainsArgumentsReference(Pair.Key) or
               EvalExpressionContainsArgumentsReference(Pair.Value) then
              Exit(True);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            if EvalExpressionContainsArgumentsReference(Pair.Key) then
              Exit(True);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaYieldExpression(AExpr).Operand))
  else if AExpr is TGocciaAwaitExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaAwaitExpression(AExpr).Operand))
  else if AExpr is TGocciaConditionalExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaConditionalExpression(AExpr).Condition) or
      EvalExpressionContainsArgumentsReference(
        TGocciaConditionalExpression(AExpr).Consequent) or
      EvalExpressionContainsArgumentsReference(
        TGocciaConditionalExpression(AExpr).Alternate))
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    if EvalExpressionContainsArgumentsReference(NewExpr.Callee) then
      Exit(True);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      if EvalExpressionContainsArgumentsReference(NewExpr.Arguments[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaSpreadExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaSpreadExpression(AExpr).Argument))
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      if EvalExpressionContainsArgumentsReference(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    if EvalExpressionContainsArgumentsReference(
        TGocciaTaggedTemplateExpression(AExpr).Tag) then
      Exit(True);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      if EvalExpressionContainsArgumentsReference(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]) then
        Exit(True);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaDestructuringAssignmentExpression(AExpr).Right))
  else if AExpr is TGocciaPrivateMemberExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr))
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPrivatePropertyAssignmentExpression(AExpr).Value))
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr) or
      EvalExpressionContainsArgumentsReference(
        TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value));

  Result := False;
end;

function EvalPatternContainsArgumentsReference(
  const APattern: TGocciaDestructuringPattern): Boolean;
var
  ArrayPattern: TGocciaArrayDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit(False);

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      if EvalPatternContainsArgumentsReference(ArrayPattern.Elements[I]) then
        Exit(True);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if Prop.Computed and
         EvalExpressionContainsArgumentsReference(Prop.KeyExpression) then
        Exit(True);
      if EvalPatternContainsArgumentsReference(Prop.Pattern) then
        Exit(True);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    Exit(EvalPatternContainsArgumentsReference(AssignmentPattern.Left) or
      EvalExpressionContainsArgumentsReference(AssignmentPattern.Right));
  end
  else if APattern is TGocciaRestDestructuringPattern then
    Exit(EvalPatternContainsArgumentsReference(
      TGocciaRestDestructuringPattern(APattern).Argument))
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression))
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression));

  Result := False;
end;

function EvalStatementContainsArgumentsReference(
  const AStmt: TGocciaStatement): Boolean;
var
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  BlockStmt: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForStmt: TGocciaForStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForInStmt: TGocciaForInStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  WithStmt: TGocciaWithStatement;
  ReturnStmt: TGocciaReturnStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  UsingDecl: TGocciaUsingDeclaration;
  I, J: Integer;
begin
  if not Assigned(AStmt) then
    Exit(False);

  if AStmt is TGocciaExpressionStatement then
    Exit(EvalExpressionContainsArgumentsReference(
      TGocciaExpressionStatement(AStmt).Expression))
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
      if EvalExpressionContainsArgumentsReference(
          VarDecl.Variables[I].Initializer) then
        Exit(True);
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(AStmt);
    Exit(EvalPatternContainsArgumentsReference(DestructDecl.Pattern) or
      EvalExpressionContainsArgumentsReference(DestructDecl.Initializer));
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if EvalASTNodeContainsArgumentsReference(BlockStmt.Nodes[I]) then
        Exit(True);
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(IfStmt.Condition) or
      EvalStatementContainsArgumentsReference(IfStmt.Consequent) or
      EvalStatementContainsArgumentsReference(IfStmt.Alternate));
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    Exit(EvalStatementContainsArgumentsReference(ForStmt.Init) or
      EvalExpressionContainsArgumentsReference(ForStmt.Condition) or
      EvalExpressionContainsArgumentsReference(ForStmt.Update) or
      EvalStatementContainsArgumentsReference(ForStmt.Body));
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ForOfStmt.Iterable) or
      EvalStatementContainsArgumentsReference(ForOfStmt.Body));
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ForInStmt.ObjectExpression) or
      EvalStatementContainsArgumentsReference(ForInStmt.Body));
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(WhileStmt.Condition) or
      EvalStatementContainsArgumentsReference(WhileStmt.Body));
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    Exit(EvalStatementContainsArgumentsReference(DoWhileStmt.Body) or
      EvalExpressionContainsArgumentsReference(DoWhileStmt.Condition));
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(WithStmt.ObjectExpression) or
      EvalStatementContainsArgumentsReference(WithStmt.Body));
  end
  else if AStmt is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ReturnStmt.Value));
  end
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    Exit(EvalExpressionContainsArgumentsReference(ThrowStmt.Value));
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    Exit(EvalStatementContainsArgumentsReference(TryStmt.Block) or
      EvalStatementContainsArgumentsReference(TryStmt.CatchBlock) or
      EvalStatementContainsArgumentsReference(TryStmt.FinallyBlock));
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    if EvalExpressionContainsArgumentsReference(SwitchStmt.Discriminant) then
      Exit(True);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      if EvalExpressionContainsArgumentsReference(SwitchStmt.Cases[I].Test) then
        Exit(True);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        if EvalStatementContainsArgumentsReference(
            SwitchStmt.Cases[I].Consequent[J]) then
          Exit(True);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      if EvalExpressionContainsArgumentsReference(
          UsingDecl.Variables[I].Initializer) then
        Exit(True);
  end;

  Result := False;
end;

function EvalProgramContainsArgumentsReference(
  const AProgram: TGocciaProgram): Boolean;
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    if EvalStatementContainsArgumentsReference(AProgram.Body[I]) then
      Exit(True);
  Result := False;
end;

procedure ValidateEvalEarlyErrorExpression(const AExpr: TGocciaExpression;
  const AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall: Boolean); forward;
procedure ValidateEvalEarlyErrorPattern(
  const APattern: TGocciaDestructuringPattern; const AAllowNewTarget,
  AAllowSuperProperty, AAllowSuperCall: Boolean); forward;

procedure ValidateEvalEarlyErrorStatement(const AStmt: TGocciaStatement;
  const AStrictEval, AAllowNewTarget, AAllowSuperProperty,
  AAllowSuperCall: Boolean);
var
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  BlockStmt: TGocciaBlockStatement;
  IfStmt: TGocciaIfStatement;
  ForStmt: TGocciaForStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForInStmt: TGocciaForInStatement;
  WhileStmt: TGocciaWhileStatement;
  DoWhileStmt: TGocciaDoWhileStatement;
  WithStmt: TGocciaWithStatement;
  ReturnStmt: TGocciaReturnStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  SwitchStmt: TGocciaSwitchStatement;
  UsingDecl: TGocciaUsingDeclaration;
  I, J: Integer;
  function IsLabelledFunctionDeclaration(
    const AStatement: TGocciaStatement): Boolean;
  begin
    Result := Assigned(AStatement) and (AStatement.LabelCount > 0) and
      (AStatement is TGocciaFunctionDeclaration);
  end;
begin
  if not Assigned(AStmt) then
    Exit;

  if AStrictEval and IsLabelledFunctionDeclaration(AStmt) then
    ThrowSyntaxError('Invalid labelled function declaration');

  if AStmt is TGocciaExpressionStatement then
    ValidateEvalEarlyErrorExpression(
      TGocciaExpressionStatement(AStmt).Expression, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall)
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
      ValidateEvalEarlyErrorExpression(VarDecl.Variables[I].Initializer,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(AStmt);
    ValidateEvalEarlyErrorPattern(DestructDecl.Pattern, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(DestructDecl.Initializer,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateEvalEarlyErrorStatement(TGocciaStatement(BlockStmt.Nodes[I]),
          AStrictEval, AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
      else if BlockStmt.Nodes[I] is TGocciaExpression then
        ValidateEvalEarlyErrorExpression(TGocciaExpression(BlockStmt.Nodes[I]),
          AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    if AStrictEval and (IsLabelledFunctionDeclaration(IfStmt.Consequent) or
       IsLabelledFunctionDeclaration(IfStmt.Alternate)) then
      ThrowSyntaxError('Invalid labelled function declaration');
    ValidateEvalEarlyErrorExpression(IfStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(IfStmt.Consequent, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(IfStmt.Alternate, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    ValidateEvalEarlyErrorStatement(ForStmt.Init, AStrictEval, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(ForStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(ForStmt.Update, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(ForStmt.Body, AStrictEval, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ForOfStmt.Iterable, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(ForOfStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ForInStmt.ObjectExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(ForInStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    ValidateEvalEarlyErrorExpression(WhileStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(WhileStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    ValidateEvalEarlyErrorStatement(DoWhileStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(DoWhileStmt.Condition, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    ValidateEvalEarlyErrorExpression(WithStmt.ObjectExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(WithStmt.Body, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaReturnStatement then
  begin
    ReturnStmt := TGocciaReturnStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ReturnStmt.Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    ValidateEvalEarlyErrorExpression(ThrowStmt.Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateEvalEarlyErrorStatement(TryStmt.Block, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(TryStmt.CatchBlock, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorStatement(TryStmt.FinallyBlock, AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    ValidateEvalEarlyErrorExpression(SwitchStmt.Discriminant, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      ValidateEvalEarlyErrorExpression(SwitchStmt.Cases[I].Test,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        ValidateEvalEarlyErrorStatement(SwitchStmt.Cases[I].Consequent[J],
          AStrictEval, AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      ValidateEvalEarlyErrorExpression(UsingDecl.Variables[I].Initializer,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end;
end;

procedure ValidateEvalEarlyErrorExpression(const AExpr: TGocciaExpression;
  const AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall: Boolean);
var
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  ArrayExpr: TGocciaArrayExpression;
  ObjectExpr: TGocciaObjectExpression;
  ArrowExpr: TGocciaArrowFunctionExpression;
  NewExpr: TGocciaNewExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit;

  if AExpr is TGocciaNewTargetExpression then
  begin
    if not AAllowNewTarget then
      ThrowSyntaxError('new.target is not allowed in this eval context');
    Exit;
  end;

  if AExpr is TGocciaArrowFunctionExpression then
  begin
    ArrowExpr := TGocciaArrowFunctionExpression(AExpr);
    for I := 0 to High(ArrowExpr.Parameters) do
    begin
      if ArrowExpr.Parameters[I].IsPattern then
        ValidateEvalEarlyErrorPattern(ArrowExpr.Parameters[I].Pattern,
          AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
      ValidateEvalEarlyErrorExpression(ArrowExpr.Parameters[I].DefaultValue,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    end;
    if ArrowExpr.Body is TGocciaStatement then
      ValidateEvalEarlyErrorStatement(TGocciaStatement(ArrowExpr.Body),
        False, AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
    else if ArrowExpr.Body is TGocciaExpression then
      ValidateEvalEarlyErrorExpression(TGocciaExpression(ArrowExpr.Body),
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    if CallExpr.Callee is TGocciaSuperExpression then
    begin
      if not AAllowSuperCall then
        ThrowSyntaxError('super() is not allowed in this eval context');
    end
    else
      ValidateEvalEarlyErrorExpression(CallExpr.Callee, AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      ValidateEvalEarlyErrorExpression(CallExpr.Arguments[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      if not AAllowSuperProperty then
        ThrowSyntaxError('super property access is not allowed in this eval context');
    end
    else
      ValidateEvalEarlyErrorExpression(MemberExpr.ObjectExpr, AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
    if MemberExpr.Computed then
      ValidateEvalEarlyErrorExpression(MemberExpr.PropertyExpression,
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaSuperExpression then
  begin
    if not AAllowSuperProperty then
      ThrowSyntaxError('super is not allowed in this eval context');
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    ValidateEvalEarlyErrorExpression(TGocciaBinaryExpression(AExpr).Left,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(TGocciaBinaryExpression(AExpr).Right,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      ValidateEvalEarlyErrorExpression(
        TGocciaSequenceExpression(AExpr).Expressions[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaUnaryExpression then
    ValidateEvalEarlyErrorExpression(TGocciaUnaryExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaAssignmentExpression then
    ValidateEvalEarlyErrorExpression(TGocciaAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaPropertyAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyAssignmentExpression(AExpr).Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaCompoundAssignmentExpression then
    ValidateEvalEarlyErrorExpression(
      TGocciaCompoundAssignmentExpression(AExpr).Value, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaIncrementExpression then
    ValidateEvalEarlyErrorExpression(TGocciaIncrementExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      ValidateEvalEarlyErrorExpression(ArrayExpr.Elements[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          ValidateEvalEarlyErrorExpression(
            ObjectExpr.PropertySourceOrder[I].Expression, AAllowNewTarget,
            AAllowSuperProperty, AAllowSuperCall);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateEvalEarlyErrorExpression(Pair.Key, AAllowNewTarget,
              AAllowSuperProperty, AAllowSuperCall);
            ValidateEvalEarlyErrorExpression(Pair.Value, AAllowNewTarget,
              AAllowSuperProperty, AAllowSuperCall);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateEvalEarlyErrorExpression(Pair.Key, AAllowNewTarget,
              AAllowSuperProperty, AAllowSuperCall);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    ValidateEvalEarlyErrorExpression(TGocciaYieldExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaAwaitExpression then
    ValidateEvalEarlyErrorExpression(TGocciaAwaitExpression(AExpr).Operand,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaConditionalExpression then
  begin
    ValidateEvalEarlyErrorExpression(TGocciaConditionalExpression(AExpr).Condition,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(TGocciaConditionalExpression(AExpr).Consequent,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(TGocciaConditionalExpression(AExpr).Alternate,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    ValidateEvalEarlyErrorExpression(NewExpr.Callee, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      ValidateEvalEarlyErrorExpression(NewExpr.Arguments[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaSpreadExpression then
    ValidateEvalEarlyErrorExpression(TGocciaSpreadExpression(AExpr).Argument,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      ValidateEvalEarlyErrorExpression(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I],
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    ValidateEvalEarlyErrorExpression(TGocciaTaggedTemplateExpression(AExpr).Tag,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      ValidateEvalEarlyErrorExpression(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I],
        AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
  begin
    ValidateEvalEarlyErrorPattern(
      TGocciaDestructuringAssignmentExpression(AExpr).Left, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaDestructuringAssignmentExpression(AExpr).Right, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaPrivateMemberExpression then
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall)
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
  end;
end;

procedure ValidateEvalEarlyErrorPattern(
  const APattern: TGocciaDestructuringPattern; const AAllowNewTarget,
  AAllowSuperProperty, AAllowSuperCall: Boolean);
var
  ArrayPattern: TGocciaArrayDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  RestPattern: TGocciaRestDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      ValidateEvalEarlyErrorPattern(ArrayPattern.Elements[I], AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if Prop.Computed then
        ValidateEvalEarlyErrorExpression(Prop.KeyExpression, AAllowNewTarget,
          AAllowSuperProperty, AAllowSuperCall);
      ValidateEvalEarlyErrorPattern(Prop.Pattern, AAllowNewTarget,
        AAllowSuperProperty, AAllowSuperCall);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    ValidateEvalEarlyErrorPattern(AssignmentPattern.Left, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
    ValidateEvalEarlyErrorExpression(AssignmentPattern.Right, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    ValidateEvalEarlyErrorPattern(RestPattern.Argument, AAllowNewTarget,
      AAllowSuperProperty, AAllowSuperCall);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    ValidateEvalEarlyErrorExpression(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    ValidateEvalEarlyErrorExpression(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
end;

function IsStrictEvalRestrictedBindingName(const AName: string): Boolean;
begin
  Result := (AName = 'eval') or (AName = IDENTIFIER_ARGUMENTS);
end;

procedure ValidateStrictEvalAssignmentExpression(
  const AExpr: TGocciaExpression); forward;
procedure ValidateStrictEvalAssignmentPattern(
  const APattern: TGocciaDestructuringPattern); forward;
procedure ValidateStrictEvalAssignmentStatement(
  const AStmt: TGocciaStatement); forward;

procedure RejectStrictEvalAssignmentName(const AName: string;
  const ALine, AColumn: Integer);
begin
  if IsStrictEvalRestrictedBindingName(AName) then
    raise TGocciaSyntaxError.Create(
      Format('Invalid assignment to ''%s'' in strict mode', [AName]),
      ALine, AColumn, '', nil);
end;

procedure ValidateStrictFunctionExpression(
  const AFunction: TGocciaFunctionExpression);
var
  BindingNames, PatternNames: TStringList;
  I, J: Integer;
  procedure AddStrictParameterName(const AName: string);
  begin
    RejectStrictEvalAssignmentName(AName, AFunction.Line, AFunction.Column);
    if AName = '' then
      Exit;
    if BindingNames.IndexOf(AName) >= 0 then
      raise TGocciaSyntaxError.Create(
        'Duplicate parameter name not allowed in strict mode',
        AFunction.Line, AFunction.Column, '', nil);
    BindingNames.Add(AName);
  end;
begin
  if not Assigned(AFunction) then
    Exit;
  RejectStrictEvalAssignmentName(AFunction.Name, AFunction.Line,
    AFunction.Column);
  BindingNames := TStringList.Create;
  PatternNames := TStringList.Create;
  try
    BindingNames.CaseSensitive := True;
    PatternNames.CaseSensitive := True;
    for I := 0 to High(AFunction.Parameters) do
    begin
      if AFunction.Parameters[I].IsPattern then
      begin
        ValidateStrictEvalAssignmentPattern(AFunction.Parameters[I].Pattern);
        PatternNames.Clear;
        CollectPatternBindingNames(AFunction.Parameters[I].Pattern,
          PatternNames);
        for J := 0 to PatternNames.Count - 1 do
          AddStrictParameterName(PatternNames[J]);
      end
      else
        AddStrictParameterName(AFunction.Parameters[I].Name);
      ValidateStrictEvalAssignmentExpression(
        AFunction.Parameters[I].DefaultValue);
    end;
  finally
    PatternNames.Free;
    BindingNames.Free;
  end;
  if AFunction.Body is TGocciaStatement then
    ValidateStrictEvalAssignmentStatement(TGocciaStatement(AFunction.Body))
  else if AFunction.Body is TGocciaExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaExpression(AFunction.Body));
end;

procedure ValidateStrictEvalAssignmentStatement(
  const AStmt: TGocciaStatement);
var
  BlockStmt: TGocciaBlockStatement;
  DestructDecl: TGocciaDestructuringDeclaration;
  DoWhileStmt: TGocciaDoWhileStatement;
  FunctionDecl: TGocciaFunctionDeclaration;
  ForInStmt: TGocciaForInStatement;
  ForOfStmt: TGocciaForOfStatement;
  ForStmt: TGocciaForStatement;
  IfStmt: TGocciaIfStatement;
  SwitchStmt: TGocciaSwitchStatement;
  ThrowStmt: TGocciaThrowStatement;
  TryStmt: TGocciaTryStatement;
  UsingDecl: TGocciaUsingDeclaration;
  VarDecl: TGocciaVariableDeclaration;
  WhileStmt: TGocciaWhileStatement;
  WithStmt: TGocciaWithStatement;
  I, J: Integer;
begin
  if not Assigned(AStmt) then
    Exit;

  if AStmt is TGocciaExpressionStatement then
    ValidateStrictEvalAssignmentExpression(
      TGocciaExpressionStatement(AStmt).Expression)
  else if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
    begin
      RejectStrictEvalAssignmentName(VarDecl.Variables[I].Name, AStmt.Line,
        AStmt.Column);
      ValidateStrictEvalAssignmentExpression(VarDecl.Variables[I].Initializer);
    end;
  end
  else if AStmt is TGocciaDestructuringDeclaration then
  begin
    DestructDecl := TGocciaDestructuringDeclaration(AStmt);
    ValidateStrictEvalAssignmentPattern(DestructDecl.Pattern);
    ValidateStrictEvalAssignmentExpression(DestructDecl.Initializer);
  end
  else if AStmt is TGocciaFunctionDeclaration then
  begin
    FunctionDecl := TGocciaFunctionDeclaration(AStmt);
    RejectStrictEvalAssignmentName(FunctionDecl.Name, AStmt.Line, AStmt.Column);
    ValidateStrictFunctionExpression(FunctionDecl.FunctionExpression);
  end
  else if AStmt is TGocciaBlockStatement then
  begin
    BlockStmt := TGocciaBlockStatement(AStmt);
    for I := 0 to BlockStmt.Nodes.Count - 1 do
      if BlockStmt.Nodes[I] is TGocciaStatement then
        ValidateStrictEvalAssignmentStatement(TGocciaStatement(BlockStmt.Nodes[I]))
      else if BlockStmt.Nodes[I] is TGocciaExpression then
        ValidateStrictEvalAssignmentExpression(TGocciaExpression(BlockStmt.Nodes[I]));
  end
  else if AStmt is TGocciaIfStatement then
  begin
    IfStmt := TGocciaIfStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(IfStmt.Condition);
    ValidateStrictEvalAssignmentStatement(IfStmt.Consequent);
    ValidateStrictEvalAssignmentStatement(IfStmt.Alternate);
  end
  else if AStmt is TGocciaForStatement then
  begin
    ForStmt := TGocciaForStatement(AStmt);
    ValidateStrictEvalAssignmentStatement(ForStmt.Init);
    ValidateStrictEvalAssignmentExpression(ForStmt.Condition);
    ValidateStrictEvalAssignmentExpression(ForStmt.Update);
    ValidateStrictEvalAssignmentStatement(ForStmt.Body);
  end
  else if AStmt is TGocciaForOfStatement then
  begin
    ForOfStmt := TGocciaForOfStatement(AStmt);
    RejectStrictEvalAssignmentName(ForOfStmt.BindingName, AStmt.Line,
      AStmt.Column);
    ValidateStrictEvalAssignmentPattern(ForOfStmt.BindingPattern);
    ValidateStrictEvalAssignmentPattern(ForOfStmt.AssignmentTarget);
    ValidateStrictEvalAssignmentExpression(ForOfStmt.Iterable);
    ValidateStrictEvalAssignmentStatement(ForOfStmt.Body);
  end
  else if AStmt is TGocciaForInStatement then
  begin
    ForInStmt := TGocciaForInStatement(AStmt);
    RejectStrictEvalAssignmentName(ForInStmt.BindingName, AStmt.Line,
      AStmt.Column);
    ValidateStrictEvalAssignmentPattern(ForInStmt.BindingPattern);
    ValidateStrictEvalAssignmentPattern(ForInStmt.AssignmentTarget);
    ValidateStrictEvalAssignmentExpression(ForInStmt.ObjectExpression);
    ValidateStrictEvalAssignmentStatement(ForInStmt.Body);
  end
  else if AStmt is TGocciaWhileStatement then
  begin
    WhileStmt := TGocciaWhileStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(WhileStmt.Condition);
    ValidateStrictEvalAssignmentStatement(WhileStmt.Body);
  end
  else if AStmt is TGocciaDoWhileStatement then
  begin
    DoWhileStmt := TGocciaDoWhileStatement(AStmt);
    ValidateStrictEvalAssignmentStatement(DoWhileStmt.Body);
    ValidateStrictEvalAssignmentExpression(DoWhileStmt.Condition);
  end
  else if AStmt is TGocciaWithStatement then
  begin
    WithStmt := TGocciaWithStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(WithStmt.ObjectExpression);
    ValidateStrictEvalAssignmentStatement(WithStmt.Body);
  end
  else if AStmt is TGocciaReturnStatement then
    ValidateStrictEvalAssignmentExpression(
      TGocciaReturnStatement(AStmt).Value)
  else if AStmt is TGocciaThrowStatement then
  begin
    ThrowStmt := TGocciaThrowStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(ThrowStmt.Value);
  end
  else if AStmt is TGocciaTryStatement then
  begin
    TryStmt := TGocciaTryStatement(AStmt);
    ValidateStrictEvalAssignmentStatement(TryStmt.Block);
    RejectStrictEvalAssignmentName(TryStmt.CatchParam, AStmt.Line,
      AStmt.Column);
    ValidateStrictEvalAssignmentPattern(TryStmt.CatchBindingPattern);
    ValidateStrictEvalAssignmentStatement(TryStmt.CatchBlock);
    ValidateStrictEvalAssignmentStatement(TryStmt.FinallyBlock);
  end
  else if AStmt is TGocciaSwitchStatement then
  begin
    SwitchStmt := TGocciaSwitchStatement(AStmt);
    ValidateStrictEvalAssignmentExpression(SwitchStmt.Discriminant);
    for I := 0 to SwitchStmt.Cases.Count - 1 do
    begin
      ValidateStrictEvalAssignmentExpression(SwitchStmt.Cases[I].Test);
      for J := 0 to SwitchStmt.Cases[I].Consequent.Count - 1 do
        ValidateStrictEvalAssignmentStatement(SwitchStmt.Cases[I].Consequent[J]);
    end;
  end
  else if AStmt is TGocciaUsingDeclaration then
  begin
    UsingDecl := TGocciaUsingDeclaration(AStmt);
    for I := 0 to High(UsingDecl.Variables) do
      ValidateStrictEvalAssignmentExpression(UsingDecl.Variables[I].Initializer);
  end;
end;

procedure ValidateStrictEvalAssignmentExpression(
  const AExpr: TGocciaExpression);
var
  ArrayExpr: TGocciaArrayExpression;
  CallExpr: TGocciaCallExpression;
  MemberExpr: TGocciaMemberExpression;
  NewExpr: TGocciaNewExpression;
  ObjectExpr: TGocciaObjectExpression;
  Pair: TPair<TGocciaExpression, TGocciaExpression>;
  I: Integer;
begin
  if not Assigned(AExpr) then
    Exit;

  if AExpr is TGocciaAssignmentExpression then
  begin
    RejectStrictEvalAssignmentName(TGocciaAssignmentExpression(AExpr).Name,
      AExpr.Line, AExpr.Column);
    ValidateStrictEvalAssignmentExpression(
      TGocciaAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaCompoundAssignmentExpression then
  begin
    RejectStrictEvalAssignmentName(TGocciaCompoundAssignmentExpression(AExpr).Name,
      AExpr.Line, AExpr.Column);
    ValidateStrictEvalAssignmentExpression(
      TGocciaCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaIncrementExpression then
  begin
    if TGocciaIncrementExpression(AExpr).Operand is TGocciaIdentifierExpression then
      RejectStrictEvalAssignmentName(
        TGocciaIdentifierExpression(TGocciaIncrementExpression(AExpr).Operand).Name,
        AExpr.Line, AExpr.Column);
    ValidateStrictEvalAssignmentExpression(
      TGocciaIncrementExpression(AExpr).Operand);
  end
  else if AExpr is TGocciaFunctionExpression then
    ValidateStrictFunctionExpression(TGocciaFunctionExpression(AExpr))
  else if AExpr is TGocciaArrowFunctionExpression then
  begin
    for I := 0 to High(TGocciaArrowFunctionExpression(AExpr).Parameters) do
    begin
      if TGocciaArrowFunctionExpression(AExpr).Parameters[I].IsPattern then
        ValidateStrictEvalAssignmentPattern(
          TGocciaArrowFunctionExpression(AExpr).Parameters[I].Pattern)
      else
        RejectStrictEvalAssignmentName(
          TGocciaArrowFunctionExpression(AExpr).Parameters[I].Name,
          AExpr.Line, AExpr.Column);
      ValidateStrictEvalAssignmentExpression(
        TGocciaArrowFunctionExpression(AExpr).Parameters[I].DefaultValue);
    end;
    if TGocciaArrowFunctionExpression(AExpr).Body is TGocciaStatement then
      ValidateStrictEvalAssignmentStatement(
        TGocciaStatement(TGocciaArrowFunctionExpression(AExpr).Body))
    else if TGocciaArrowFunctionExpression(AExpr).Body is TGocciaExpression then
      ValidateStrictEvalAssignmentExpression(
        TGocciaExpression(TGocciaArrowFunctionExpression(AExpr).Body));
  end
  else if AExpr is TGocciaDestructuringAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentPattern(
      TGocciaDestructuringAssignmentExpression(AExpr).Left);
    ValidateStrictEvalAssignmentExpression(
      TGocciaDestructuringAssignmentExpression(AExpr).Right);
  end
  else if AExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AExpr);
    ValidateStrictEvalAssignmentExpression(CallExpr.Callee);
    for I := 0 to CallExpr.Arguments.Count - 1 do
      ValidateStrictEvalAssignmentExpression(CallExpr.Arguments[I]);
  end
  else if AExpr is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AExpr);
    ValidateStrictEvalAssignmentExpression(MemberExpr.ObjectExpr);
    if MemberExpr.Computed then
      ValidateStrictEvalAssignmentExpression(MemberExpr.PropertyExpression);
  end
  else if AExpr is TGocciaPropertyAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaComputedPropertyAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).PropertyExpression);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaPropertyCompoundAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaComputedPropertyCompoundAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).PropertyExpression);
    ValidateStrictEvalAssignmentExpression(
      TGocciaComputedPropertyCompoundAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    ValidateStrictEvalAssignmentExpression(TGocciaBinaryExpression(AExpr).Left);
    ValidateStrictEvalAssignmentExpression(TGocciaBinaryExpression(AExpr).Right);
  end
  else if AExpr is TGocciaSequenceExpression then
  begin
    for I := 0 to TGocciaSequenceExpression(AExpr).Expressions.Count - 1 do
      ValidateStrictEvalAssignmentExpression(
        TGocciaSequenceExpression(AExpr).Expressions[I]);
  end
  else if AExpr is TGocciaUnaryExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaUnaryExpression(AExpr).Operand)
  else if AExpr is TGocciaArrayExpression then
  begin
    ArrayExpr := TGocciaArrayExpression(AExpr);
    for I := 0 to ArrayExpr.Elements.Count - 1 do
      ValidateStrictEvalAssignmentExpression(ArrayExpr.Elements[I]);
  end
  else if AExpr is TGocciaObjectExpression then
  begin
    ObjectExpr := TGocciaObjectExpression(AExpr);
    for I := 0 to High(ObjectExpr.PropertySourceOrder) do
      case ObjectExpr.PropertySourceOrder[I].PropertyType of
        pstStatic:
          ValidateStrictEvalAssignmentExpression(
            ObjectExpr.PropertySourceOrder[I].Expression);
        pstComputed:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateStrictEvalAssignmentExpression(Pair.Key);
            ValidateStrictEvalAssignmentExpression(Pair.Value);
          end;
        pstComputedGetter,
        pstComputedSetter:
          begin
            Pair := ObjectExpr.ComputedPropertiesInOrder[
              ObjectExpr.PropertySourceOrder[I].ComputedIndex];
            ValidateStrictEvalAssignmentExpression(Pair.Key);
          end;
      end;
  end
  else if AExpr is TGocciaYieldExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaYieldExpression(AExpr).Operand)
  else if AExpr is TGocciaAwaitExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaAwaitExpression(AExpr).Operand)
  else if AExpr is TGocciaConditionalExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaConditionalExpression(AExpr).Condition);
    ValidateStrictEvalAssignmentExpression(
      TGocciaConditionalExpression(AExpr).Consequent);
    ValidateStrictEvalAssignmentExpression(
      TGocciaConditionalExpression(AExpr).Alternate);
  end
  else if AExpr is TGocciaNewExpression then
  begin
    NewExpr := TGocciaNewExpression(AExpr);
    ValidateStrictEvalAssignmentExpression(NewExpr.Callee);
    for I := 0 to NewExpr.Arguments.Count - 1 do
      ValidateStrictEvalAssignmentExpression(NewExpr.Arguments[I]);
  end
  else if AExpr is TGocciaSpreadExpression then
    ValidateStrictEvalAssignmentExpression(TGocciaSpreadExpression(AExpr).Argument)
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
  begin
    for I := 0 to TGocciaTemplateWithInterpolationExpression(AExpr).Parts.Count - 1 do
      ValidateStrictEvalAssignmentExpression(
        TGocciaTemplateWithInterpolationExpression(AExpr).Parts[I]);
  end
  else if AExpr is TGocciaTaggedTemplateExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaTaggedTemplateExpression(AExpr).Tag);
    for I := 0 to TGocciaTaggedTemplateExpression(AExpr).Expressions.Count - 1 do
      ValidateStrictEvalAssignmentExpression(
        TGocciaTaggedTemplateExpression(AExpr).Expressions[I]);
  end
  else if AExpr is TGocciaPrivateMemberExpression then
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivateMemberExpression(AExpr).ObjectExpr)
  else if AExpr is TGocciaPrivatePropertyAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyAssignmentExpression(AExpr).Value);
  end
  else if AExpr is TGocciaPrivatePropertyCompoundAssignmentExpression then
  begin
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).ObjectExpr);
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivatePropertyCompoundAssignmentExpression(AExpr).Value);
  end;
end;

procedure ValidateStrictEvalAssignmentPattern(
  const APattern: TGocciaDestructuringPattern);
var
  ArrayPattern: TGocciaArrayDestructuringPattern;
  AssignmentPattern: TGocciaAssignmentDestructuringPattern;
  ObjectPattern: TGocciaObjectDestructuringPattern;
  Prop: TGocciaDestructuringProperty;
  RestPattern: TGocciaRestDestructuringPattern;
  I: Integer;
begin
  if not Assigned(APattern) then
    Exit;

  if APattern is TGocciaIdentifierDestructuringPattern then
    RejectStrictEvalAssignmentName(
      TGocciaIdentifierDestructuringPattern(APattern).Name,
      APattern.Line, APattern.Column)
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    ArrayPattern := TGocciaArrayDestructuringPattern(APattern);
    for I := 0 to ArrayPattern.Elements.Count - 1 do
      ValidateStrictEvalAssignmentPattern(ArrayPattern.Elements[I]);
  end
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    ObjectPattern := TGocciaObjectDestructuringPattern(APattern);
    for I := 0 to ObjectPattern.Properties.Count - 1 do
    begin
      Prop := ObjectPattern.Properties[I];
      if Prop.Computed then
        ValidateStrictEvalAssignmentExpression(Prop.KeyExpression);
      ValidateStrictEvalAssignmentPattern(Prop.Pattern);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignmentPattern := TGocciaAssignmentDestructuringPattern(APattern);
    ValidateStrictEvalAssignmentPattern(AssignmentPattern.Left);
    ValidateStrictEvalAssignmentExpression(AssignmentPattern.Right);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPattern := TGocciaRestDestructuringPattern(APattern);
    ValidateStrictEvalAssignmentPattern(RestPattern.Argument);
  end
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    ValidateStrictEvalAssignmentExpression(
      TGocciaMemberExpressionDestructuringPattern(APattern).Expression)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    ValidateStrictEvalAssignmentExpression(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern).Expression);
end;

procedure ValidateEvalEarlyErrors(const AProgram: TGocciaProgram;
  const AStrictEval, AAllowNewTarget, AAllowSuperProperty,
  AAllowSuperCall: Boolean);
var
  I: Integer;
begin
  if AStrictEval then
    for I := 0 to AProgram.Body.Count - 1 do
      ValidateStrictEvalAssignmentStatement(AProgram.Body[I]);

  for I := 0 to AProgram.Body.Count - 1 do
    ValidateEvalEarlyErrorStatement(AProgram.Body[I], AStrictEval,
      AAllowNewTarget, AAllowSuperProperty, AAllowSuperCall);
end;

procedure EvalDeclarationInstantiation(const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext; const AVarScope,
  ALexicalScope: TGocciaScope; const AStrictEval: Boolean;
  const ARejectArgumentsVarDeclaration: Boolean;
  const ARejectVarDeclarationNames: TGocciaEvalRejectNameArray);
var
  VarNames: TStringList;
  LexNames: TStringList;
  DeclaredFunctionNames: TStringList;
  DeclaredVarNames: TStringList;
  FunctionDeclarations: TObjectList<TGocciaFunctionDeclaration>;
  FunctionsToInitialize: TObjectList<TGocciaFunctionDeclaration>;
  ScopeCursor: TGocciaScope;
  FuncDecl: TGocciaFunctionDeclaration;
  FunctionContext: TGocciaEvaluationContext;
  FunctionValue: TGocciaValue;
  Name: string;
  I: Integer;
  function RejectsVarDeclarationName(const AName: string): Boolean;
  var
    J: Integer;
  begin
    for J := 0 to High(ARejectVarDeclarationNames) do
      if ARejectVarDeclarationNames[J] = AName then
        Exit(True);
    Result := False;
  end;
begin
  VarNames := TStringList.Create;
  LexNames := TStringList.Create;
  DeclaredFunctionNames := TStringList.Create;
  DeclaredVarNames := TStringList.Create;
  FunctionDeclarations := TObjectList<TGocciaFunctionDeclaration>.Create(False);
  FunctionsToInitialize := TObjectList<TGocciaFunctionDeclaration>.Create(False);
  try
    VarNames.CaseSensitive := True;
    LexNames.CaseSensitive := True;
    DeclaredFunctionNames.CaseSensitive := True;
    DeclaredVarNames.CaseSensitive := True;

    CollectVarBindingNamesFromStatements(AProgram.Body, VarNames,
      VarBindingNameCollectionMode(not AStrictEval,
        AContext.CompatibilityNonStrictMode));
    CollectTopLevelEvalLexicalNames(AProgram.Body, LexNames);
    for I := 0 to LexNames.Count - 1 do
      if VarNames.IndexOf(LexNames[I]) >= 0 then
        ThrowSyntaxError(Format(SErrorIdentifierAlreadyDeclared, [LexNames[I]]),
          SSuggestAlreadyDeclared);

    if not AStrictEval then
    begin
      if AVarScope.ScopeKind = skGlobal then
        for I := 0 to VarNames.Count - 1 do
          if AVarScope.HasLexicalDeclaration(VarNames[I]) then
            ThrowSyntaxError(Format(SErrorIdentifierAlreadyDeclared,
              [VarNames[I]]), SSuggestAlreadyDeclared);

      ScopeCursor := ALexicalScope;
      while Assigned(ScopeCursor) and (ScopeCursor <> AVarScope) do
      begin
        if not (ScopeCursor is TGocciaWithScope) then
          for I := 0 to VarNames.Count - 1 do
            if ScopeCursor.ContainsOwnLexicalBinding(VarNames[I]) or
               ScopeCursor.ContainsOwnVarBinding(VarNames[I]) then
              ThrowSyntaxError(Format(SErrorIdentifierAlreadyDeclared,
                [VarNames[I]]), SSuggestAlreadyDeclared);
        ScopeCursor := ScopeCursor.Parent;
      end;

      if AVarScope.ScopeKind <> skGlobal then
        for I := 0 to VarNames.Count - 1 do
          if AVarScope.ContainsOwnLexicalBinding(VarNames[I]) then
            ThrowSyntaxError(Format(SErrorIdentifierAlreadyDeclared,
              [VarNames[I]]), SSuggestAlreadyDeclared);
    end;

    if (not AStrictEval) then
    begin
      for I := 0 to VarNames.Count - 1 do
        if RejectsVarDeclarationName(VarNames[I]) then
          ThrowSyntaxError(Format(SErrorIdentifierAlreadyDeclared,
            [VarNames[I]]), SSuggestAlreadyDeclared);
    end;

    if (not AStrictEval) and ARejectArgumentsVarDeclaration and
       (VarNames.IndexOf(IDENTIFIER_ARGUMENTS) >= 0) then
      ThrowSyntaxError(Format(SErrorIdentifierAlreadyDeclared,
        [IDENTIFIER_ARGUMENTS]), SSuggestAlreadyDeclared);

    CollectEvalFunctionDeclarations(AProgram.Body, FunctionDeclarations);
    for I := FunctionDeclarations.Count - 1 downto 0 do
    begin
      FuncDecl := FunctionDeclarations[I];
      Name := FuncDecl.Name;
      if DeclaredFunctionNames.IndexOf(Name) >= 0 then
        Continue;
      if (AVarScope.ScopeKind = skGlobal) and
         not AVarScope.CanDeclareGlobalFunction(Name) then
        ThrowTypeError(Format('Cannot declare global function ''%s''', [Name]));
      DeclaredFunctionNames.Add(Name);
      FunctionsToInitialize.Insert(0, FuncDecl);
    end;

    for I := 0 to VarNames.Count - 1 do
    begin
      Name := VarNames[I];
      if DeclaredFunctionNames.IndexOf(Name) >= 0 then
        Continue;
      if (AVarScope.ScopeKind = skGlobal) and
         not AVarScope.CanDeclareGlobalVar(Name) then
        ThrowTypeError(Format('Cannot declare global var ''%s''', [Name]));
      AddUniqueEvalName(DeclaredVarNames, Name);
    end;

    PredeclareTopLevelEvalLexicalBindings(AProgram.Body, ALexicalScope);

    FunctionContext := AContext;
    FunctionContext.Scope := ALexicalScope;
    for I := 0 to FunctionsToInitialize.Count - 1 do
    begin
      FuncDecl := FunctionsToInitialize[I];
      Name := FuncDecl.Name;
      FunctionValue := FuncDecl.FunctionExpression.Evaluate(FunctionContext);
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(FunctionValue);
      try
        if (FunctionValue is TGocciaFunctionValue) and
           (TGocciaFunctionValue(FunctionValue).Name = '') then
          TGocciaFunctionValue(FunctionValue).Name := Name;
        if AVarScope.ScopeKind = skGlobal then
          AVarScope.CreateGlobalFunctionBinding(Name, FunctionValue, True)
        else if (not AStrictEval) and AVarScope.Contains(Name) then
          AVarScope.AssignBinding(Name, FunctionValue, 0, 0, False)
        else
          AVarScope.DefineVariableBinding(Name, FunctionValue, True, True);
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(FunctionValue);
      end;
    end;

    for I := 0 to DeclaredVarNames.Count - 1 do
    begin
      Name := DeclaredVarNames[I];
      if AVarScope.ScopeKind = skGlobal then
        AVarScope.CreateGlobalVarBinding(Name, True)
      else if (not AStrictEval) and AVarScope.Contains(Name) then
        Continue
      else
        AVarScope.DefineVariableBinding(Name,
          TGocciaUndefinedLiteralValue.UndefinedValue, False, True);
    end;
  finally
    FunctionsToInitialize.Free;
    FunctionDeclarations.Free;
    DeclaredVarNames.Free;
    DeclaredFunctionNames.Free;
    LexNames.Free;
    VarNames.Free;
  end;
end;

function EvaluateEvalProgram(const AProgram: TGocciaProgram;
  const AContext: TGocciaEvaluationContext; const AVarScope,
  ALexicalScope: TGocciaScope; const AStrictEval: Boolean;
  const ARejectArgumentsVarDeclaration: Boolean;
  const ARejectVarDeclarationNames: TGocciaEvalRejectNameArray;
  const AAllowNewTarget: Boolean; const AAllowSuperProperty: Boolean;
  const AAllowSuperCall: Boolean;
  const ARejectArgumentsReference: Boolean): TGocciaValue;
var
  EvalContext: TGocciaEvaluationContext;
  CF: TGocciaControlFlow;
  I: Integer;
  LastValue: TGocciaValue;
begin
  ValidateEvalScriptBody(AProgram.Body);
  if ARejectArgumentsReference and
     EvalProgramContainsArgumentsReference(AProgram) then
    ThrowSyntaxError('arguments is not allowed in this eval context');
  ValidateEvalEarlyErrors(AProgram, AStrictEval, AAllowNewTarget,
    AAllowSuperProperty, AAllowSuperCall);
  EvalContext := AContext;
  EvalContext.Scope := ALexicalScope;
  EvalContext.NonStrictMode := not AStrictEval;
  EvalContext.CompatibilityNonStrictMode := AContext.CompatibilityNonStrictMode;
  EvalContext.InEvalCode := True;
  EvalContext.EvalVarScope := AVarScope;
  ALexicalScope.NonStrictMode := not AStrictEval;
  AVarScope.NonStrictMode := not AStrictEval;

  EvalDeclarationInstantiation(AProgram, EvalContext, AVarScope, ALexicalScope,
    AStrictEval, ARejectArgumentsVarDeclaration, ARejectVarDeclarationNames);

  LastValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  CF := TGocciaControlFlow.Normal(LastValue);
  for I := 0 to AProgram.Body.Count - 1 do
  begin
    CF := EvaluateStatement(AProgram.Body[I], EvalContext);
    if (CF.Kind = cfkNormal) and Assigned(CF.Value) then
      LastValue := CF.Value;
    if CF.Kind <> cfkNormal then
      Break;
  end;

  case CF.Kind of
    cfkNormal:
      Result := LastValue;
    cfkReturn:
      ThrowSyntaxError('Illegal return statement', SSuggestExpressionExpected);
    cfkBreak:
      ThrowSyntaxError(SErrorIllegalBreakStatement, SSuggestExpressionExpected);
    cfkContinue:
      ThrowSyntaxError(SErrorIllegalContinueStatement,
        SSuggestExpressionExpected);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluateStatements(const ANodes: TObjectList<TGocciaASTNode>; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  I: Integer;
  Continuation: TGocciaGeneratorContinuation;
  PreviousValue: TGocciaValue;
begin
  if Assigned(AContext.OnError) and not Assigned(AContext.Scope.OnError) then
    AContext.Scope.OnError := AContext.OnError;

  Continuation := CurrentGeneratorContinuation;
  Result := TGocciaControlFlow.Empty;
  if Assigned(Continuation) then
    I := Continuation.GetStatementIndex(ANodes)
  else
    I := 0;
  while I < ANodes.Count do
  begin
    try
      PreviousValue := Result.Value;
      if ANodes[I] is TGocciaExpression then
        Result := TGocciaControlFlow.Normal(
          EvaluateExpression(TGocciaExpression(ANodes[I]), AContext))
      else
        Result := EvaluateStatement(TGocciaStatement(ANodes[I]), AContext);
      Result := Result.UpdateEmpty(PreviousValue);
      if Assigned(Continuation) then
      begin
        Continuation.SaveStatementIndex(ANodes, I + 1);
        Continuation.ClearExpressionValues;
      end;
    except
      on E: EGocciaGeneratorYield do
      begin
        if Assigned(Continuation) then
          Continuation.SaveStatementIndex(ANodes, I);
        raise;
      end;
      else
      begin
        if Assigned(Continuation) then
        begin
          Continuation.ClearStatementIndex(ANodes);
          Continuation.ClearExpressionValues;
        end;
        raise;
      end;
    end;
    if Result.Kind <> cfkNormal then
    begin
      if Assigned(Continuation) then
        Continuation.ClearStatementIndex(ANodes);
      Exit;
    end;
    Inc(I);
  end;
  if Assigned(Continuation) then
    Continuation.ClearStatementIndex(ANodes);
end;

procedure SpreadIterableInto(const ASpreadValue: TGocciaValue; const ATarget: TGocciaValueList);
var
  SpreadArray: TGocciaArrayValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  J: Integer;
begin
  if ASpreadValue is TGocciaArrayValue then
  begin
    SpreadArray := TGocciaArrayValue(ASpreadValue);
    for J := 0 to SpreadArray.Elements.Count - 1 do
    begin
      if SpreadArray.Elements[J] = TGocciaHoleValue.HoleValue then
        ATarget.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
      else
        ATarget.Add(SpreadArray.Elements[J]);
    end;
  end
  else
  begin
    Iterator := GetIteratorFromValue(ASpreadValue);
    if Assigned(Iterator) then
    begin
      TGarbageCollector.Instance.AddTempRoot(Iterator);
      try
        IterResult := Iterator.AdvanceNext;
        while not IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value do
        begin
          ATarget.Add(IterResult.GetProperty(PROP_VALUE));
          IterResult := Iterator.AdvanceNext;
        end;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(Iterator);
      end;
    end
    else
      ThrowTypeError(
        SErrorSpreadRequiresIterable,
        SSuggestSpreadRequiresIterable);
  end;
end;

procedure SpreadIterableIntoArgs(const ASpreadValue: TGocciaValue; const AArgs: TGocciaArgumentsCollection);
begin
  SpreadIterableInto(ASpreadValue, AArgs.Items);
end;

function Evaluate(const ANode: TGocciaASTNode; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
begin
  // Propagate OnError onto the scope so closures inherit it
  if Assigned(AContext.OnError) and not Assigned(AContext.Scope.OnError) then
    AContext.Scope.OnError := AContext.OnError;

  if ANode is TGocciaExpression then
    Result := TGocciaControlFlow.Normal(EvaluateExpression(TGocciaExpression(ANode), AContext))
  else if ANode is TGocciaStatement then
    Result := EvaluateStatement(TGocciaStatement(ANode), AContext)
  else
    Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function EvaluateExpression(const AExpression: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Continuation: TGocciaGeneratorContinuation;
begin
  Continuation := CurrentGeneratorContinuation;
  if Assigned(Continuation) and Continuation.TakeCompletedExpressionValue(AExpression, Result) then
  begin
    CollectInterpreterMemoryPressure(Result);
    Exit;
  end;
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      AContext.CurrentFilePath, AExpression.Line);
  Result := AExpression.Evaluate(AContext);
  if Assigned(Continuation) then
    Continuation.SaveCompletedExpressionValue(AExpression, Result);
  CollectInterpreterMemoryPressure(Result);
end;

function EvaluateStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
begin
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      AContext.CurrentFilePath, AStatement.Line);
  ActivateCompatBlockFunctionDeclaration(AStatement, AContext);
  Result := AStatement.Execute(AContext);
  if (Result.Kind = cfkBreak) and (Result.TargetLabel <> '') and
     AStatement.HasLabel(Result.TargetLabel) then
    Result := TGocciaControlFlow.Normal(Result.Value);
end;

function TargetsStatementOrUnlabeled(const AControlFlow: TGocciaControlFlow;
  const AStatement: TGocciaStatement): Boolean;
begin
  Result := (AControlFlow.TargetLabel = '') or
    AStatement.HasLabel(AControlFlow.TargetLabel);
end;

function EvaluateLoopBodyWithActiveScope(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  Continuation: TGocciaGeneratorContinuation;
  GC: TGarbageCollector;
  ScopeRooted: Boolean;
begin
  Continuation := CurrentGeneratorContinuation;
  GC := TGarbageCollector.Instance;
  ScopeRooted := Assigned(GC) and Assigned(AContext.Scope);
  if ScopeRooted then
    GC.PushActiveRoot(AContext.Scope);
  try
    try
      Result := EvaluateStatement(AStatement, AContext);
      if Assigned(Continuation) then
        Continuation.ClearExpressionValues;
    except
      on E: EGocciaGeneratorYield do
        raise;
      else
      begin
        if Assigned(Continuation) then
          Continuation.ClearExpressionValues;
        raise;
      end;
    end;
  finally
    if ScopeRooted then
      GC.PopActiveRoot;
  end;
end;

function EvaluateLoopBodyStatement(const AStatement: TGocciaStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
begin
  Result := EvaluateLoopBodyWithActiveScope(AStatement, AContext);
end;

function EvaluateBinary(const ABinaryExpression: TGocciaBinaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Left, Right: TGocciaValue;
  Roots: TGocciaActiveRootFrame;
begin
  Roots.Initialize;
  // Handle short-circuiting logical operators first. The left value is not
  // needed across right-side evaluation when the right side is taken, and
  // CollectForMemoryPressure protects the returned value directly.
  if ABinaryExpression.Operator = gttAnd then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    if not Left.ToBooleanLiteral.Value then
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 0);
      Result := Left  // Short-circuit: return left operand if falsy
    end
    else
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 1);
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;  // Return right operand
    end;
    CollectInterpreterMemoryPressure(Result);
    Exit;
  end
  else if ABinaryExpression.Operator = gttOr then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    if Left.ToBooleanLiteral.Value then
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 0);
      Result := Left  // Short-circuit: return left operand if truthy
    end
    else
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 1);
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;  // Return right operand
    end;
    CollectInterpreterMemoryPressure(Result);
    Exit;
  end
  else if ABinaryExpression.Operator = gttNullishCoalescing then
  begin
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
    // Return right operand only if left is null or undefined
    if (Left is TGocciaNullLiteralValue) or (Left is TGocciaUndefinedLiteralValue) then
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 0);
      Right := EvaluateExpression(ABinaryExpression.Right, AContext);
      Result := Right;
    end
    else
    begin
      if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
        TGocciaCoverageTracker.Instance.RecordBranchHit(
          AContext.CurrentFilePath, ABinaryExpression.Line,
          ABinaryExpression.Column, 1);
      Result := Left;  // Return left operand for all other values (including falsy ones)
    end;
    CollectInterpreterMemoryPressure(Result);
    Exit;
  end;

  if (ABinaryExpression.Operator = gttIn) and
     (ABinaryExpression.Left is TGocciaPrivateMemberExpression) then
  begin
    Result := EvaluatePrivateInOperator(
      TGocciaPrivateMemberExpression(ABinaryExpression.Left),
      ABinaryExpression.Right, AContext);
    CollectInterpreterMemoryPressure(Result);
    Exit;
  end;

  // For all other operators, evaluate both operands.  Generators can suspend
  // while evaluating the right operand; keep the left value so resuming does
  // not replay left-side side effects.
  if not Assigned(CurrentGeneratorContinuation) or
     not CurrentGeneratorContinuation.TakeExpressionValue(ABinaryExpression, Left) then
    Left := EvaluateExpression(ABinaryExpression.Left, AContext);
  AddValueRoot(Roots, Left);
  try
    Right := EvaluateExpression(ABinaryExpression.Right, AContext);
    AddValueRoot(Roots, Right);
    if Assigned(CurrentGeneratorContinuation) then
      CurrentGeneratorContinuation.ClearExpressionValue(ABinaryExpression);

    case ABinaryExpression.Operator of
      gttPlus:
        Result := EvaluateAddition(Left, Right);
      gttMinus:
        Result := EvaluateSubtraction(Left, Right);
      gttStar:
        Result := EvaluateMultiplication(Left, Right);
      gttSlash:
        Result := EvaluateDivision(Left, Right);
      gttPercent:
        Result := EvaluateModulo(Left, Right);
      gttPower:
        Result := EvaluateExponentiation(Left, Right);
      gttEqual:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.IsStrictEqual(Left, Right));
      gttNotEqual:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.IsNotStrictEqual(Left, Right));
      gttLooseEqual:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.IsLooselyEqual(Left, Right));
      gttLooseNotEqual:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.IsNotLooselyEqual(Left, Right));
      gttLess:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.LessThan(Left, Right));
      gttGreater:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.GreaterThan(Left, Right));
      gttLessEqual:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.LessThanOrEqual(Left, Right));
      gttGreaterEqual:
        Result := TGocciaBooleanLiteralValue.FromBoolean(
          Goccia.Arithmetic.GreaterThanOrEqual(Left, Right));
      gttInstanceof:
        Result := EvaluateInstanceof(Left, Right, IsObjectInstanceOfClass);
      gttIn:
        Result := EvaluateInOperator(Left, Right);
      // Bitwise operators
      gttBitwiseAnd:
        Result := EvaluateBitwiseAnd(Left, Right);
      gttBitwiseOr:
        Result := EvaluateBitwiseOr(Left, Right);
      gttBitwiseXor:
        Result := EvaluateBitwiseXor(Left, Right);
      gttLeftShift:
        Result := EvaluateLeftShift(Left, Right);
      gttRightShift:
        Result := EvaluateRightShift(Left, Right);
      gttUnsignedRightShift:
        Result := EvaluateUnsignedRightShift(Left, Right);
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  except
    on E: EGocciaGeneratorYield do
    begin
      if Assigned(CurrentGeneratorContinuation) then
        CurrentGeneratorContinuation.SaveExpressionValue(ABinaryExpression, Left);
      Roots.Clear;
      raise;
    end;
    else
    begin
      Roots.Clear;
      raise;
    end;
  end;
  AddValueRoot(Roots, Result);
  CollectInterpreterMemoryPressure(Result);
  Roots.Clear;
end;

function EvaluateUnary(const AUnaryExpression: TGocciaUnaryExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Operand: TGocciaValue;
  Roots: TGocciaActiveRootFrame;
begin
  Roots.Initialize;
  try
  // Special handling for delete operator
  if AUnaryExpression.Operator = gttDelete then
  begin
    Result := EvaluateDelete(AUnaryExpression.Operand, AContext);
    Exit;
  end;

  // Special handling for typeof: must not throw for undeclared variables
  if AUnaryExpression.Operator = gttTypeof then
  begin
    try
      Operand := EvaluateExpression(AUnaryExpression.Operand, AContext);
      AddValueRoot(Roots, Operand);
    except
      on E: TGocciaReferenceError do
      begin
        if (AUnaryExpression.Operand is TGocciaIdentifierExpression) and
           AContext.Scope.Contains(
             TGocciaIdentifierExpression(AUnaryExpression.Operand).Name) then
          raise;
        // typeof on undeclared variable returns "undefined" per ECMAScript spec
        Result := TGocciaStringLiteralValue.Create('undefined');
        Exit;
      end;
    end;
    Result := EvaluateTypeof(Operand);
    Exit;
  end;

  Operand := EvaluateExpression(AUnaryExpression.Operand, AContext);
  AddValueRoot(Roots, Operand);

  case AUnaryExpression.Operator of
    gttNot:
      if Operand.ToBooleanLiteral.Value then Result := TGocciaBooleanLiteralValue.FalseValue else Result := TGocciaBooleanLiteralValue.TrueValue;
    gttMinus:
      begin
        if Operand is TGocciaSymbolValue then
          ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
        // ES2026 §13.5.5 UnaryMinus — operand goes through ToNumeric,
        // which is ToPrimitive then a BigInt? branch.  Apply
        // ToPrimitive here so boxed BigInts (Object(1n)) unbox to
        // their primitive and take the BigInt::unaryMinus path
        // instead of being silently coerced to NaN by the boxed
        // object's ToNumberLiteral.
        Operand := ToPrimitive(Operand);
        AddValueRoot(Roots, Operand);
        if Operand is TGocciaSymbolValue then
          ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
        // ES2026 §6.1.6.2.1 BigInt::unaryMinus
        if Operand is TGocciaBigIntValue then
        begin
          Result := TGocciaBigIntValue.Create(
            TGocciaBigIntValue(Operand).Value.Negate);
        end
        else if (Operand is TGocciaNumberLiteralValue) then
        begin
          if TGocciaNumberLiteralValue(Operand).IsInfinity then
            Result := TGocciaNumberLiteralValue.NegativeInfinityValue  // -Infinity = -Infinity
          else if TGocciaNumberLiteralValue(Operand).IsNegativeInfinity then
            Result := TGocciaNumberLiteralValue.InfinityValue  // -(-Infinity) = Infinity
          // Handle signed zero: -0 should create negative zero, -(negative zero) should create positive zero
          else if TGocciaNumberLiteralValue(Operand).Value = 0 then
          begin
            if TGocciaNumberLiteralValue(Operand).IsNegativeZero then
              Result := TGocciaNumberLiteralValue.ZeroValue  // -(-0) = +0
            else
              Result := TGocciaNumberLiteralValue.NegativeZeroValue;  // -0 = -0
          end
          else
            Result := TGocciaNumberLiteralValue.Create(-Operand.ToNumberLiteral.Value);
        end
        else
          Result := TGocciaNumberLiteralValue.Create(-Operand.ToNumberLiteral.Value);
      end;
    gttPlus:
    begin
      if Operand is TGocciaSymbolValue then
        ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
      // ES2026 §13.5.4 UnaryPlus — ToNumber (ToPrimitive then a
      // BigInt? throw).  Apply ToPrimitive here so boxed BigInts
      // surface the spec-mandated TypeError instead of silently
      // producing a number from the boxed object's coercion path.
      Operand := ToPrimitive(Operand);
      AddValueRoot(Roots, Operand);
      if Operand is TGocciaSymbolValue then
        ThrowTypeError(SErrorSymbolToNumber, SSuggestSymbolNoImplicitConversion);
      // ES2026 §7.1.4: unary + on BigInt throws TypeError
      if Operand is TGocciaBigIntValue then
        ThrowTypeError(SErrorBigIntUnaryPlus, SSuggestBigIntNoImplicitConversion);
      Result := Operand.ToNumberLiteral;
    end;
    gttTypeof:
      Result := EvaluateTypeof(Operand);
    gttVoid:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    gttBitwiseNot:
      Result := EvaluateBitwiseNot(Operand);
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
  AddValueRoot(Roots, Result);
  CollectInterpreterMemoryPressure(Result);
  finally
    Roots.Clear;
  end;
end;

function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
begin
  Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
end;

function ClassRequiresObjectConstructorReturn(
  const AClassValue: TGocciaClassValue): Boolean;
begin
  Result := Assigned(AClassValue) and
    (Assigned(AClassValue.SuperClass) or
     Assigned(AClassValue.NativeSuperConstructor));
end;

procedure ValidateClassConstructorReturn(
  const AClassValue: TGocciaClassValue; const AValue: TGocciaValue);
begin
  if ClassRequiresObjectConstructorReturn(AClassValue) and
     not (AValue is TGocciaObjectValue) and
     not IsUndefinedConstructedValue(AValue) then
    ThrowTypeError(
      'Derived constructor returned non-object',
      SSuggestNotConstructorType);
end;

function InvokeConstructableWithReceiver(const AConstructor: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AReceiver: TGocciaValue;
  const AContext: TGocciaEvaluationContext;
  const ANewTarget: TGocciaValue = nil): TGocciaValue;
var
  BoundFunction: TGocciaBoundFunctionValue;
  ClassConstructor: TGocciaClassValue;
  CombinedArgs: TGocciaArgumentsCollection;
  SuperResult: TGocciaValue;
  ConstructorThisValue: TGocciaValue;
  EffectiveNewTarget: TGocciaValue;
  I: Integer;

  function InvokeImplicitClassConstructor: TGocciaValue;
  begin
    Result := AReceiver;

    if Assigned(ClassConstructor.SuperClass) then
      Result := InvokeConstructableWithReceiver(ClassConstructor.SuperClass,
        AArguments, AReceiver, AContext, EffectiveNewTarget)
    else if Assigned(ClassConstructor.NativeSuperConstructor) then
      Result := InvokeConstructableWithReceiver(
        ClassConstructor.NativeSuperConstructor, AArguments, AReceiver,
        AContext, EffectiveNewTarget)
    else if AReceiver is TGocciaInstanceValue then
      TGocciaInstanceValue(AReceiver).InitializeNativeFromArguments(AArguments);

    ValidateClassConstructorReturn(ClassConstructor, Result);
    if Result is TGocciaObjectValue then
      RunClassInstanceInitializers(ClassConstructor,
        TGocciaObjectValue(Result), AContext, iimFirstPass)
    else if AReceiver is TGocciaObjectValue then
    begin
      RunClassInstanceInitializers(ClassConstructor,
        TGocciaObjectValue(AReceiver), AContext, iimFirstPass);
      Result := AReceiver;
    end;
  end;
begin
  if Assigned(ANewTarget) then
    EffectiveNewTarget := ANewTarget
  else
    EffectiveNewTarget := AConstructor;

  if AConstructor is TGocciaBoundFunctionValue then
  begin
    BoundFunction := TGocciaBoundFunctionValue(AConstructor);
    if IsSameValue(BoundFunction, EffectiveNewTarget) then
      EffectiveNewTarget := BoundFunction.OriginalFunction;
    CombinedArgs := TGocciaArgumentsCollection.CreateWithCapacity(
      BoundFunction.BoundArgCount + AArguments.Length);
    try
      for I := 0 to BoundFunction.BoundArgCount - 1 do
        CombinedArgs.Add(BoundFunction.GetBoundArg(I));
      for I := 0 to AArguments.Length - 1 do
        CombinedArgs.Add(AArguments.GetElement(I));
      Exit(InvokeConstructableWithReceiver(BoundFunction.OriginalFunction,
        CombinedArgs, AReceiver, AContext, EffectiveNewTarget));
    finally
      CombinedArgs.Free;
    end;
  end;

  if AConstructor is TGocciaProxyValue then
    SuperResult := TGocciaProxyValue(AConstructor).ConstructTrap(
      AArguments, EffectiveNewTarget)
  else if AConstructor is TGocciaNativeFunctionValue then
  begin
    if TGocciaNativeFunctionValue(AConstructor).NotConstructable then
      ThrowTypeError(
        Format(SErrorNotConstructor,
          [TGocciaNativeFunctionValue(AConstructor).Name]),
        Format('''%s'' is not a constructor',
          [TGocciaNativeFunctionValue(AConstructor).Name]));
    SuperResult := TGocciaNativeFunctionValue(AConstructor).Construct(
      AArguments, EffectiveNewTarget);
  end
  else if AConstructor is TGocciaClassValue then
  begin
    ClassConstructor := TGocciaClassValue(AConstructor);
    if Assigned(ClassConstructor.ConstructorMethod) then
    begin
      if AReceiver is TGocciaObjectValue then
        RunClassInstanceInitializers(ClassConstructor,
          TGocciaObjectValue(AReceiver), AContext, iimFirstPass);
      SuperResult := ClassConstructor.ConstructorMethod.CallWithThisValue(
        AArguments, AReceiver, ConstructorThisValue, EffectiveNewTarget);
      ValidateClassConstructorReturn(ClassConstructor, SuperResult);
      if not (SuperResult is TGocciaObjectValue) and
         (ConstructorThisValue is TGocciaObjectValue) then
        SuperResult := ConstructorThisValue;
      if (SuperResult is TGocciaObjectValue) and
         (SuperResult <> AReceiver) and
         (SuperResult = ConstructorThisValue) then
        RunClassInstanceInitializers(ClassConstructor,
          TGocciaObjectValue(SuperResult), AContext, iimFirstPass);
    end
    else
      SuperResult := InvokeImplicitClassConstructor;
  end
  else if AConstructor is TGocciaFunctionBase then
    SuperResult := TGocciaFunctionBase(AConstructor).ConstructWithReceiver(
      AArguments, AReceiver, EffectiveNewTarget)
  else
    ThrowTypeError(Format(SErrorValueNotConstructor, [AConstructor.TypeName]),
      SSuggestNotConstructorType);

  if SuperResult is TGocciaObjectValue then
    Result := SuperResult
  else
    Result := AReceiver;
end;

function FindDirectEvalCallerFunctionScope(
  const AScope: TGocciaScope): TGocciaScope;
var
  Current: TGocciaScope;
begin
  Current := AScope;
  while Assigned(Current) do
  begin
    if (Current is TGocciaCallScope) and
       not (Current is TGocciaArrowCallScope) then
      Exit(Current);
    if Current.ScopeKind in [skGlobal, skModule] then
      Exit(nil);
    Current := Current.Parent;
  end;
  Result := nil;
end;

function DirectEvalAllowsSuperCall(const AScope: TGocciaScope): Boolean;
var
  CallerFunctionScope: TGocciaScope;
begin
  CallerFunctionScope := FindDirectEvalCallerFunctionScope(AScope);
  Result := (CallerFunctionScope is TGocciaMethodCallScope) and
    (TGocciaMethodCallScope(CallerFunctionScope).CustomLabel = 'constructor') and
    Assigned(TGocciaMethodCallScope(CallerFunctionScope).SuperClass);
end;

function DirectEvalRejectsArgumentsReference(
  const AScope: TGocciaScope): Boolean;
var
  Current: TGocciaScope;
begin
  Current := AScope;
  while Assigned(Current) do
  begin
    if (Current is TGocciaCallScope) and
       not (Current is TGocciaArrowCallScope) then
      Exit(False);
    if Current.RejectArgumentsReferenceInDirectEval then
      Exit(True);
    if Current.ScopeKind in [skGlobal, skModule] then
      Exit(False);
    Current := Current.Parent;
  end;
  Result := False;
end;

function IsCurrentRealmEvalFunction(const ACallee: TGocciaValue;
  const AContext: TGocciaEvaluationContext): Boolean;
var
  GlobalObject: TGocciaObjectValue;
  EvalValue: TGocciaValue;
begin
  Result := False;
  if not Assigned(AContext.Realm) or
     not (AContext.Realm.GlobalObject is TGocciaObjectValue) then
    Exit;

  GlobalObject := TGocciaObjectValue(AContext.Realm.GlobalObject);
  if not GlobalObject.HasProperty('eval') then
    Exit;

  EvalValue := GlobalObject.GetProperty('eval');
  Result := EvalValue = ACallee;
end;

function TryEvaluateInterpreterDirectEval(
  const ACallExpression: TGocciaCallExpression;
  const AContext: TGocciaEvaluationContext;
  const ACallee: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  out AResult: TGocciaValue): Boolean;
var
  SourceValue: TGocciaValue;
  SourceText: string;
  DeclaredPrivateNames: TStringList;
  EvalSource: TStringList;
  EvalSourcePath: string;
  EvalOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  EvalContext: TGocciaEvaluationContext;
  EvalScope: TGocciaScope;
  VarScope: TGocciaScope;
  CallerFunctionScope: TGocciaScope;
  CallerStrict: Boolean;
  StrictEval: Boolean;
  AllowNewTarget: Boolean;
  AllowSuperProperty: Boolean;
  AllowSuperCall: Boolean;
begin
  Result := False;
  if not ((ACallee is TGocciaNativeFunctionValue) and
     TGocciaNativeFunctionValue(ACallee).DirectEvalHost and
     IsCurrentRealmEvalFunction(ACallee, AContext)) then
    Exit;
  if not (ACallExpression.Callee is TGocciaIdentifierExpression) or
     (TGocciaIdentifierExpression(ACallExpression.Callee).Name <> 'eval') then
    Exit;

  Result := True;
  if AArguments.Length = 0 then
  begin
    AResult := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  SourceValue := AArguments.GetElement(0);
  if not (SourceValue is TGocciaStringLiteralValue) then
  begin
    AResult := SourceValue;
    Exit;
  end;

  SourceText := TGocciaStringLiteralValue(SourceValue).Value;
  DeclaredPrivateNames := nil;
  EvalSource := TStringList.Create;
  try
    EvalSource.Text := SourceText;
    EvalOptions := TGocciaSourcePipeline.CurrentOptionsOrDefault;
    EvalOptions.SourceType := stScript;
    CallerStrict := not AContext.NonStrictMode;
    EvalOptions.InheritedStrictMode := CallerStrict;
    if CallerStrict then
      Exclude(EvalOptions.Compatibility, cfNonStrictMode);

    EvalSourcePath := Format('%s::<direct-eval>', [AContext.CurrentFilePath]);
    if Pos('#', SourceText) > 0 then
      DeclaredPrivateNames := CollectDeclaredPrivateNames(AContext);
    try
      PipelineResult := TGocciaSourcePipeline.Parse(EvalSource,
        EvalSourcePath, EvalOptions, DeclaredPrivateNames);
    finally
      FreeAndNil(DeclaredPrivateNames);
    end;
    try
      StrictEval := CallerStrict or HasUseStrictDirective(PipelineResult.ProgramNode);
      if StrictEval then
        EvalScope := AContext.Scope.CreateChild(skFunction,
          'StrictInterpreterDirectEval')
      else
        EvalScope := AContext.Scope.CreateChild(skBlock,
          'InterpreterDirectEval');
      EvalScope.ThisValue := AContext.Scope.ThisValue;
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AddTempRoot(EvalScope);
      try
        EvalContext := AContext;
        EvalContext.Scope := EvalScope;
        EvalContext.CurrentFilePath := EvalSourcePath;
        EvalContext.NonStrictMode := not StrictEval;
        EvalContext.CompatibilityNonStrictMode :=
          AContext.CompatibilityNonStrictMode;
        if StrictEval then
          VarScope := EvalScope
        else
        begin
          VarScope := AContext.Scope.FindFunctionOrModuleScope;
          if not Assigned(VarScope) then
            VarScope := AContext.Scope;
        end;
        CallerFunctionScope := FindDirectEvalCallerFunctionScope(AContext.Scope);
        AllowNewTarget := Assigned(CallerFunctionScope);
        AllowSuperProperty := Assigned(CallerFunctionScope) and
          Assigned(CallerFunctionScope.FindSuperClass);
        AllowSuperCall := DirectEvalAllowsSuperCall(CallerFunctionScope);
        AResult := EvaluateEvalProgram(PipelineResult.ProgramNode,
          EvalContext, VarScope, EvalScope, StrictEval,
          EvalContext.RejectArgumentsVarDeclarationInEval,
          EvalContext.RejectVarDeclarationNamesInEval, AllowNewTarget,
          AllowSuperProperty, AllowSuperCall,
          DirectEvalRejectsArgumentsReference(AContext.Scope));
      finally
        if Assigned(TGarbageCollector.Instance) then
          TGarbageCollector.Instance.RemoveTempRoot(EvalScope);
      end;
    finally
      PipelineResult.Free;
    end;
  finally
    EvalSource.Free;
  end;
end;

function EvaluateCallWithOptionalShortCircuit(
  const ACallExpression: TGocciaCallExpression;
  const AContext: TGocciaEvaluationContext;
  out AShortCircuited: Boolean): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  ThisValue: TGocciaValue;
  ArgumentExpr: TGocciaExpression;
  SuperClass: TGocciaClassValue;
  SuperClassValue, SuperResult: TGocciaValue;
  MemberExpr: TGocciaMemberExpression;
  PrivateExpr: TGocciaPrivateMemberExpression;
  SpreadValue: TGocciaValue;
  ArgumentValue: TGocciaValue;
  CalleeName: string;
  FirstAddedIndex, I: Integer;
  ThisScope: TGocciaScope;
  ConstructorThisValue: TGocciaValue;
  CurrentCtorClassValue: TGocciaValue;
  CurrentCtorClass: TGocciaClassValue;
  Roots: TGocciaActiveRootFrame;
  DirectEvalResult: TGocciaValue;
  function TryGetParenthesizedMemberReference(
    const AExpression: TGocciaExpression;
    out AMemberExpression: TGocciaMemberExpression): Boolean;
  var
    Sequence: TGocciaSequenceExpression;
  begin
    AMemberExpression := nil;
    if not (AExpression is TGocciaSequenceExpression) then
      Exit(False);
    Sequence := TGocciaSequenceExpression(AExpression);
    if (Sequence.Expressions.Count <> 1) or
       not (Sequence.Expressions[0] is TGocciaMemberExpression) then
      Exit(False);
    AMemberExpression := TGocciaMemberExpression(Sequence.Expressions[0]);
    Result := True;
  end;
  function TryGetParenthesizedPrivateReference(
    const AExpression: TGocciaExpression;
    out APrivateExpression: TGocciaPrivateMemberExpression): Boolean;
  var
    Sequence: TGocciaSequenceExpression;
  begin
    APrivateExpression := nil;
    if not (AExpression is TGocciaSequenceExpression) then
      Exit(False);
    Sequence := TGocciaSequenceExpression(AExpression);
    if (Sequence.Expressions.Count <> 1) or
       not (Sequence.Expressions[0] is TGocciaPrivateMemberExpression) then
      Exit(False);
    APrivateExpression := TGocciaPrivateMemberExpression(
      Sequence.Expressions[0]);
    Result := True;
  end;
  procedure InitializeReplacementThis(const AReplacement: TGocciaObjectValue;
    const APreviousThis: TGocciaValue);
  begin
    if (not Assigned(AReplacement)) or (AReplacement = APreviousThis) then
      Exit;
    CurrentCtorClassValue := AContext.Scope.FindOwningClass;
    if not (CurrentCtorClassValue is TGocciaClassValue) then
      Exit;
    CurrentCtorClass := TGocciaClassValue(CurrentCtorClassValue);
    RunClassInstanceInitializers(CurrentCtorClass, AReplacement, AContext,
      iimEagerReplacement);
  end;
  procedure MarkSuperConstructorCalled;
  var
    CurrentScope: TGocciaScope;
  begin
    CurrentScope := AContext.Scope;
    while Assigned(CurrentScope) do
    begin
      if CurrentScope.MarkSuperConstructorCalled then
        Exit;
      CurrentScope := CurrentScope.Parent;
    end;
  end;
begin
  AShortCircuited := False;
  Roots.Initialize;
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;
  // Handle super() calls specially
  if ACallExpression.Callee is TGocciaSuperExpression then
  begin
    SuperClassValue := AContext.Scope.FindSuperConstructor;
    if not Assigned(SuperClassValue) then
      SuperClassValue := EvaluateExpression(ACallExpression.Callee, AContext);
    AddValueRoot(Roots, SuperClassValue);
    if SuperClassValue is TGocciaClassValue then
      SuperClass := TGocciaClassValue(SuperClassValue)
    else
      SuperClass := nil;
    if (not Assigned(SuperClass)) and
       (not ((SuperClassValue is TGocciaObjectValue) and SuperClassValue.IsConstructable)) then
    begin
      AContext.OnError('super() can only be called within a method with a superclass',
        ACallExpression.Line, ACallExpression.Column);
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Roots.Clear;
      Exit;
    end;

    Arguments := TGocciaArgumentsCollection.Create;

    try
      for ArgumentExpr in ACallExpression.Arguments do
      begin
        if ArgumentExpr is TGocciaSpreadExpression then
        begin
          SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArgumentExpr).Argument, AContext);
          AddValueRoot(Roots, SpreadValue);
          FirstAddedIndex := Arguments.Length;
          SpreadIterableIntoArgs(SpreadValue, Arguments);
          RootArgumentsFrom(Roots, Arguments, FirstAddedIndex);
        end
        else
        begin
          ArgumentValue := EvaluateExpression(ArgumentExpr, AContext);
          Arguments.Add(ArgumentValue);
          AddValueRoot(Roots, ArgumentValue);
        end;
      end;

      if Assigned(SuperClass) and Assigned(SuperClass.ConstructorMethod) then
      begin
        SuperResult := SuperClass.ConstructorMethod.CallWithThisValue(
          Arguments, AContext.Scope.ThisValue, ConstructorThisValue,
          AContext.Scope.FindNewTarget);
        if SuperResult is TGocciaObjectValue then
        begin
          InitializeReplacementThis(TGocciaObjectValue(SuperResult),
            AContext.Scope.ThisValue);
          AContext.Scope.ThisValue := TGocciaObjectValue(SuperResult);
          ThisScope := AContext.Scope.FindFunctionOrModuleScope;
          if Assigned(ThisScope) then
            ThisScope.ThisValue := AContext.Scope.ThisValue;
          Result := AContext.Scope.ThisValue;
        end
        else if (Assigned(SuperClass.SuperClass) or
                 Assigned(SuperClass.NativeSuperConstructor)) and
                not ((not Assigned(SuperResult)) or
                     (SuperResult is TGocciaUndefinedLiteralValue)) then
          ThrowTypeError(
            'Derived constructor returned non-object',
            SSuggestNotConstructorType)
        else if ConstructorThisValue is TGocciaObjectValue then
        begin
          InitializeReplacementThis(TGocciaObjectValue(ConstructorThisValue),
            AContext.Scope.ThisValue);
          AContext.Scope.ThisValue := TGocciaObjectValue(ConstructorThisValue);
          ThisScope := AContext.Scope.FindFunctionOrModuleScope;
          if Assigned(ThisScope) then
            ThisScope.ThisValue := AContext.Scope.ThisValue;
          Result := AContext.Scope.ThisValue;
        end
        else
          Result := AContext.Scope.ThisValue;
      end
      else if Assigned(SuperClass) then
      begin
        if AContext.Scope.ThisValue is TGocciaInstanceValue then
          TGocciaInstanceValue(AContext.Scope.ThisValue).InitializeNativeFromArguments(Arguments);
        Result := AContext.Scope.ThisValue;
      end
      else if SuperClassValue is TGocciaObjectValue then
      begin
        SuperResult := InvokeConstructableWithReceiver(SuperClassValue,
          Arguments, AContext.Scope.ThisValue, AContext,
          AContext.Scope.FindNewTarget);
        if SuperResult is TGocciaObjectValue then
        begin
          InitializeReplacementThis(TGocciaObjectValue(SuperResult),
            AContext.Scope.ThisValue);
          AContext.Scope.ThisValue := TGocciaObjectValue(SuperResult);
          ThisScope := AContext.Scope.FindFunctionOrModuleScope;
          if Assigned(ThisScope) then
            ThisScope.ThisValue := AContext.Scope.ThisValue;
        end;
        Result := AContext.Scope.ThisValue;
      end
      else
      begin
        ThrowTypeError(Format(SErrorValueNotFunction, [SuperClassValue.TypeName]),
          SSuggestNotFunctionType);
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      end;
    finally
      Arguments.Free;
      Roots.Clear;
    end;
    MarkSuperConstructorCalled;
    AddValueRoot(Roots, Result);
    CollectInterpreterMemoryPressure(Result);
    Roots.Clear;
    Exit;
  end;

  // Handle method calls vs ordinary function calls
  if ACallExpression.Callee is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(ACallExpression.Callee);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      // Super method calls: evaluate normally
      Callee := EvaluateExpression(ACallExpression.Callee, AContext);
      ThisValue := AContext.Scope.ThisValue;  // Use current instance's 'this'
      AddValueRoot(Roots, Callee);
      AddValueRoot(Roots, ThisValue);
    end
    else
    begin
      // Regular method calls: use overloaded function to get both method and object
      Callee := EvaluateMember(MemberExpr, AContext, ThisValue);
      AddValueRoot(Roots, Callee);
      AddValueRoot(Roots, ThisValue);
      if MemberExpr.Optional and
         ((ThisValue is TGocciaNullLiteralValue) or (ThisValue is TGocciaUndefinedLiteralValue)) then
      begin
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Roots.Clear;
        Exit;
      end;
    end;
  end
  else if ACallExpression.Callee is TGocciaPrivateMemberExpression then
  begin
    // Private method calls: use overloaded function to get both method and object
    Callee := EvaluatePrivateMember(TGocciaPrivateMemberExpression(ACallExpression.Callee), AContext, ThisValue);
    AddValueRoot(Roots, Callee);
    AddValueRoot(Roots, ThisValue);
  end
  else if TryGetParenthesizedMemberReference(ACallExpression.Callee,
    MemberExpr) then
  begin
    Callee := EvaluateMember(MemberExpr, AContext, ThisValue);
    AddValueRoot(Roots, Callee);
    AddValueRoot(Roots, ThisValue);
  end
  else if TryGetParenthesizedPrivateReference(ACallExpression.Callee,
    PrivateExpr) then
  begin
    Callee := EvaluatePrivateMember(PrivateExpr, AContext, ThisValue);
    AddValueRoot(Roots, Callee);
    AddValueRoot(Roots, ThisValue);
  end
  else
  begin
    // Regular function calls
    if ACallExpression.Callee is TGocciaIdentifierExpression then
      AContext.Scope.ResolveIdentifierReference(
        TGocciaIdentifierExpression(ACallExpression.Callee).Name,
        Callee, ThisValue)
    else
    begin
      Callee := EvaluateExpression(ACallExpression.Callee, AContext);
      ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
    AddValueRoot(Roots, Callee);
    AddValueRoot(Roots, ThisValue);
  end;

  if ACallExpression.Optional and
     ((Callee is TGocciaNullLiteralValue) or (Callee is TGocciaUndefinedLiteralValue)) then
  begin
    AShortCircuited := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Roots.Clear;
    Exit;
  end;

  Arguments := TGocciaArgumentsCollection.Create;
  try
    for ArgumentExpr in ACallExpression.Arguments do
    begin
      if ArgumentExpr is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ArgumentExpr).Argument, AContext);
        AddValueRoot(Roots, SpreadValue);
        FirstAddedIndex := Arguments.Length;
        SpreadIterableIntoArgs(SpreadValue, Arguments);
        RootArgumentsFrom(Roots, Arguments, FirstAddedIndex);
      end
      else
      begin
        ArgumentValue := EvaluateExpression(ArgumentExpr, AContext);
        Arguments.Add(ArgumentValue);
        AddValueRoot(Roots, ArgumentValue);
      end;
    end;

    if Callee is TGocciaNativeFunctionValue then
      CalleeName := TGocciaNativeFunctionValue(Callee).Name
    else if Callee is TGocciaFunctionValue then
      CalleeName := TGocciaFunctionValue(Callee).Name
    else if Callee is TGocciaClassValue then
      CalleeName := TGocciaClassValue(Callee).Name
    else
      CalleeName := '';

    if TryEvaluateInterpreterDirectEval(ACallExpression, AContext, Callee,
      Arguments, DirectEvalResult) then
    begin
      Result := DirectEvalResult;
      AddValueRoot(Roots, Result);
      CollectInterpreterMemoryPressure(Result);
      Exit;
    end;

    if Assigned(TGocciaCallStack.Instance) then
    begin
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ACallExpression.Line, ACallExpression.Column);
    end;
    try
      if Assigned(TGocciaCallStack.Instance) then
        CheckStackDepth(TGocciaCallStack.Instance.Count);
      if Assigned(Callee) and Callee.IsCallable then
        Result := DispatchCall(Callee, Arguments, ThisValue)
      else
      begin
        MemberExpr := nil;
        if ACallExpression.Callee is TGocciaMemberExpression then
          MemberExpr := TGocciaMemberExpression(ACallExpression.Callee);

        if Assigned(MemberExpr) and (MemberExpr.ObjectExpr is TGocciaIdentifierExpression) then
          ThrowTypeError(
            Format(SErrorMemberNotFunction,
              [TGocciaIdentifierExpression(MemberExpr.ObjectExpr).Name,
               MemberExpr.PropertyName]),
            Format('''%s'' is of type ''%s'' which does not have method ''%s''',
              [TGocciaIdentifierExpression(MemberExpr.ObjectExpr).Name,
               ThisValue.TypeName,
               MemberExpr.PropertyName]))
        else if Assigned(MemberExpr) then
          ThrowTypeError(
            Format(SErrorMemberNotFunction,
              [ThisValue.TypeName, MemberExpr.PropertyName]),
            Format('''%s'' is of type ''%s'' which does not have method ''%s''',
              [ThisValue.TypeName, ThisValue.TypeName, MemberExpr.PropertyName]))
        else if ACallExpression.Callee is TGocciaIdentifierExpression then
          ThrowTypeError(
            Format(SErrorNotFunction,
              [TGocciaIdentifierExpression(ACallExpression.Callee).Name]),
            Format('''%s'' is of type ''%s'' and cannot be called as a function',
              [TGocciaIdentifierExpression(ACallExpression.Callee).Name,
               Callee.TypeName]))
        else
          ThrowTypeError(Format(SErrorValueNotFunction, [Callee.TypeName]),
            SSuggestNotFunctionType);
      end;
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
    AddValueRoot(Roots, Result);
    CollectInterpreterMemoryPressure(Result);

  finally
    Arguments.Free;
    Roots.Clear;
  end;
end;

function EvaluateCall(const ACallExpression: TGocciaCallExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ShortCircuited: Boolean;
begin
  Result := EvaluateCallWithOptionalShortCircuit(ACallExpression, AContext,
    ShortCircuited);
end;

function EvaluateMemberCore(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; const AOutObjectValue: PGocciaValue): TGocciaValue; forward;
function ResolveSuperThisValue(const AContext: TGocciaEvaluationContext): TGocciaValue; forward;
function ResolveSuperPropertyBase(const AContext: TGocciaEvaluationContext;
  const AThisValue: TGocciaValue): TGocciaValue; forward;

procedure AssignSuperProperty(const AContext: TGocciaEvaluationContext;
  const APropertyKey, AValue: TGocciaValue);
var
  BaseObject: TGocciaObjectValue;
  BaseValue: TGocciaValue;
  KeyValue: TGocciaValue;
  PropertyName: string;
  ThisValue: TGocciaValue;
  Success: Boolean;
begin
  ThisValue := ResolveSuperThisValue(AContext);
  BaseValue := ResolveSuperPropertyBase(AContext, ThisValue);
  if not (BaseValue is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, ['super']),
      SSuggestCheckNullBeforeAccess);

  KeyValue := ToPropertyKey(APropertyKey);
  if KeyValue is TGocciaSymbolValue then
    PropertyName := TGocciaSymbolValue(KeyValue).ToDisplayString.Value
  else
    PropertyName := TGocciaStringLiteralValue(KeyValue).Value;

  BaseObject := TGocciaObjectValue(BaseValue);
  if KeyValue is TGocciaSymbolValue then
    Success := BaseObject.AssignSymbolPropertyWithReceiver(
      TGocciaSymbolValue(KeyValue), AValue, ThisValue)
  else
    Success := BaseObject.AssignPropertyWithReceiver(PropertyName, AValue,
      ThisValue);

  if not Success then
  begin
    if AContext.NonStrictMode then
      Exit;
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [PropertyName]),
      SSuggestCannotDeleteNonConfigurable);
  end;
end;

function ResolveSuperThisValue(const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ScopeCursor: TGocciaScope;
begin
  ScopeCursor := AContext.Scope;
  while Assigned(ScopeCursor) do
  begin
    if ScopeCursor is TGocciaMethodCallScope then
    begin
      if (ScopeCursor.CustomLabel = PROP_CONSTRUCTOR) and
         Assigned(TGocciaMethodCallScope(ScopeCursor).SuperClass) and
         not TGocciaMethodCallScope(ScopeCursor).SuperConstructorCalled then
        ThrowReferenceError(
          'Must call super constructor before accessing this',
          'call super() before reading this or super properties');
      Break;
    end;
    ScopeCursor := ScopeCursor.Parent;
  end;
  Result := AContext.Scope.ThisValue;
end;

function ResolveSuperPropertyBase(const AContext: TGocciaEvaluationContext;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  OwningClass: TGocciaClassValue;
  OwningClassValue: TGocciaValue;
  SuperClassValue: TGocciaValue;
begin
  Result := nil;
  OwningClassValue := AContext.Scope.FindOwningClass;

  if AThisValue is TGocciaClassValue then
  begin
    if OwningClassValue is TGocciaObjectValue then
      Exit(TGocciaObjectValue(OwningClassValue).Prototype);

    SuperClassValue := AContext.Scope.FindSuperClass;
    if SuperClassValue is TGocciaObjectValue then
      Exit(SuperClassValue);
    Exit(nil);
  end;

  if OwningClassValue is TGocciaClassValue then
  begin
    OwningClass := TGocciaClassValue(OwningClassValue);
    if Assigned(OwningClass.Prototype) then
      Exit(OwningClass.Prototype.Prototype);
  end;
  if (OwningClassValue is TGocciaObjectValue) and
     not (OwningClassValue is TGocciaClassValue) then
    Exit(TGocciaObjectValue(OwningClassValue).Prototype);

  SuperClassValue := AContext.Scope.FindSuperClass;
  if SuperClassValue is TGocciaClassValue then
    Exit(TGocciaClassValue(SuperClassValue).Prototype);
  if SuperClassValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(SuperClassValue).Prototype);
end;

function GetSuperProperty(const AContext: TGocciaEvaluationContext;
  const APropertyKey: TGocciaValue): TGocciaValue;
var
  BaseObject: TGocciaObjectValue;
  BaseValue: TGocciaValue;
  KeyValue: TGocciaValue;
  PropertyName: string;
  ThisValue: TGocciaValue;
begin
  ThisValue := ResolveSuperThisValue(AContext);
  BaseValue := ResolveSuperPropertyBase(AContext, ThisValue);
  if not (BaseValue is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull, ['super']),
      SSuggestCheckNullBeforeAccess);

  KeyValue := ToPropertyKey(APropertyKey);
  if KeyValue is TGocciaSymbolValue then
    PropertyName := TGocciaSymbolValue(KeyValue).ToDisplayString.Value
  else
    PropertyName := TGocciaStringLiteralValue(KeyValue).Value;

  BaseObject := TGocciaObjectValue(BaseValue);
  if KeyValue is TGocciaSymbolValue then
    Result := BaseObject.GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue(KeyValue), ThisValue)
  else
    Result := BaseObject.GetPropertyWithContext(PropertyName,
      ThisValue);
end;

function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateMemberCore(AMemberExpression, AContext, nil);
end;

function EvaluateMember(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue;
begin
  Result := EvaluateMemberCore(AMemberExpression, AContext, @AObjectValue);
end;

function EvaluateMemberCore(const AMemberExpression: TGocciaMemberExpression; const AContext: TGocciaEvaluationContext; const AOutObjectValue: PGocciaValue): TGocciaValue;
var
  Obj: TGocciaValue;
  ThisValue: TGocciaValue;
  CallExpr: TGocciaCallExpression;
  PropertyName: string;
  PropertyValue: TGocciaValue;
  PropertyKey: TGocciaValue;
  BoxedValue: TGocciaObjectValue;
  ObjectEvaluated: Boolean;
  ShortCircuited: Boolean;
begin
  ObjectEvaluated := False;

  if AMemberExpression.ObjectExpr is TGocciaCallExpression then
  begin
    CallExpr := TGocciaCallExpression(AMemberExpression.ObjectExpr);
    Obj := EvaluateCallWithOptionalShortCircuit(CallExpr, AContext,
      ShortCircuited);
    ObjectEvaluated := True;
    if Assigned(AOutObjectValue) then
      AOutObjectValue^ := Obj;
    if ShortCircuited then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  // Handle optional chaining: obj?.prop returns undefined if obj is null/undefined
  if AMemberExpression.Optional then
  begin
    if not ObjectEvaluated then
    begin
      Obj := EvaluateExpression(AMemberExpression.ObjectExpr, AContext);
      ObjectEvaluated := True;
      if Assigned(AOutObjectValue) then
        AOutObjectValue^ := Obj;
    end;
    if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;
  end;

  // Handle super property references.
  if AMemberExpression.ObjectExpr is TGocciaSuperExpression then
  begin
    ThisValue := ResolveSuperThisValue(AContext);
    if Assigned(AOutObjectValue) then
      AOutObjectValue^ := ThisValue;

    if AMemberExpression.Computed and Assigned(AMemberExpression.PropertyExpression) then
    begin
      PropertyValue := EvaluateExpression(AMemberExpression.PropertyExpression,
        AContext);
    end
    else
    begin
      PropertyValue := TGocciaStringLiteralValue.Create(
        AMemberExpression.PropertyName);
    end;

    Result := GetSuperProperty(AContext, PropertyValue);
    Exit;
  end;

  if not ObjectEvaluated then
  begin
    Obj := EvaluateExpression(AMemberExpression.ObjectExpr, AContext);
    if Assigned(AOutObjectValue) then
      AOutObjectValue^ := Obj;
  end;

  // Determine the property name. ES2026 §7.1.19 ToPropertyKey on the
  // computed expression: symbols pass through; otherwise coerce via
  // ToPrimitive(string) → ToString.
  if AMemberExpression.Computed and Assigned(AMemberExpression.PropertyExpression) then
  begin
    PropertyValue := EvaluateExpression(AMemberExpression.PropertyExpression, AContext);
    PropertyKey := ToPropertyKey(PropertyValue);

    if PropertyKey is TGocciaSymbolValue then
    begin
      if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
        ThrowTypeError(Format(SErrorCannotReadPropertiesOf, [Obj.ToStringLiteral.Value, 'Symbol()']),
          SSuggestCheckNullBeforeAccess);

      if Obj is TGocciaClassValue then
      begin
        Result := TGocciaClassValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKey));
        Exit;
      end
      else if Obj is TGocciaObjectValue then
      begin
        Result := TGocciaObjectValue(Obj).GetSymbolProperty(TGocciaSymbolValue(PropertyKey));
        Exit;
      end
      else if Obj is TGocciaSymbolValue then
      begin
        // Symbol primitive: look up symbol-keyed members on Symbol.prototype
        if Assigned(TGocciaSymbolValue.SharedPrototype) then
        begin
          Result := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
            .GetSymbolPropertyWithReceiver(TGocciaSymbolValue(PropertyKey), Obj);
        end
        else
          Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end
      else
      begin
        BoxedValue := Obj.Box;
        if Assigned(BoxedValue) then
        begin
          Result := BoxedValue.GetSymbolPropertyWithReceiver(
            TGocciaSymbolValue(PropertyKey), Obj);
          Exit;
        end;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    end;

    PropertyName := TGocciaStringLiteralValue(PropertyKey).Value;
  end
  else
  begin
    // Static access: use the property name directly
    PropertyName := AMemberExpression.PropertyName;
  end;

  Result := Obj.GetProperty(PropertyName);
  if Result = nil then
  begin
    // Handle primitive boxing for property access
    BoxedValue := Obj.Box;
    if Assigned(BoxedValue) then
    begin
      Result := BoxedValue.GetPropertyWithContext(PropertyName, Obj);
    end
    else if (Obj is TGocciaNullLiteralValue) or (Obj is TGocciaUndefinedLiteralValue) then
    begin
      if AMemberExpression.ObjectExpr is TGocciaMemberExpression then
        ThrowTypeError(
          Format(SErrorCannotReadPropertyOf,
            [PropertyName, Obj.ToStringLiteral.Value]),
          Format('''%s'' evaluated to %s and does not have property ''%s''',
            [TGocciaMemberExpression(AMemberExpression.ObjectExpr).PropertyName,
             Obj.ToStringLiteral.Value, PropertyName]))
      else if AMemberExpression.ObjectExpr is TGocciaIdentifierExpression then
        ThrowTypeError(
          Format(SErrorCannotReadPropertyOf,
            [PropertyName, Obj.ToStringLiteral.Value]),
          Format('''%s'' is %s and does not have property ''%s''',
            [TGocciaIdentifierExpression(AMemberExpression.ObjectExpr).Name,
             Obj.ToStringLiteral.Value, PropertyName]))
      else
        ThrowTypeError(Format(
          SErrorCannotReadPropertyOf,
          [PropertyName, Obj.ToStringLiteral.Value]),
          SSuggestCheckNullBeforeAccess);
    end
    else
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
  end;
end;

function IsAnonymousFunctionNameExpression(
  const AExpression: TGocciaExpression): Boolean;
begin
  Result := (AExpression is TGocciaObjectMethodDefinition) or
    (AExpression is TGocciaArrowFunctionExpression) or
    ((AExpression is TGocciaFunctionExpression) and
     (TGocciaFunctionExpression(AExpression).Name = '')) or
    ((AExpression is TGocciaClassExpression) and
     (TGocciaClassExpression(AExpression).ClassDefinition.Name = ''));
end;

procedure ApplyInferredNameForExpression(const AExpression: TGocciaExpression;
  const AValue: TGocciaValue; const AName: string);
begin
  if not IsAnonymousFunctionNameExpression(AExpression) then
    Exit;

  if AValue is TGocciaFunctionValue then
    TGocciaFunctionValue(AValue).SetInferredName(AName)
  else if AValue is TGocciaClassValue then
    TGocciaClassValue(AValue).SetInferredName(AName);
end;

function EvaluateArray(const AArrayExpression: TGocciaArrayExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
  ElementValue: TGocciaValue;
  SpreadValue: TGocciaValue;
begin
  Arr := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(Arr);
  try
    for I := 0 to AArrayExpression.Elements.Count - 1 do
    begin
      if AArrayExpression.Elements[I] is TGocciaHoleExpression then
        Arr.Elements.Add(TGocciaHoleValue.HoleValue)
      else if AArrayExpression.Elements[I] is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(AArrayExpression.Elements[I]).Argument, AContext);
        SpreadIterableInto(SpreadValue, Arr.Elements);
      end
      else
      begin
        ElementValue := EvaluateExpression(AArrayExpression.Elements[I], AContext);
        Arr.Elements.Add(ElementValue);
      end;
    end;
    Result := Arr;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Arr);
  end;
end;

function EvaluateObjectMethodDefinition(
  const AMethodDefinition: TGocciaObjectMethodDefinition;
  const AContext: TGocciaEvaluationContext;
  const AHomeObject: TGocciaObjectValue;
  const AName: string): TGocciaValue;
var
  FunctionExpression: TGocciaFunctionExpression;
  HasStrictDirective: Boolean;
  MethodValue: TGocciaMethodValue;
  Statements: TObjectList<TGocciaASTNode>;
begin
  FunctionExpression := AMethodDefinition.FunctionExpression;
  HasStrictDirective := HasUseStrictDirective(FunctionExpression.Body);
  if FunctionExpression.Body is TGocciaBlockStatement then
    Statements := CopyStatementList(
      TGocciaBlockStatement(FunctionExpression.Body).Nodes)
  else
  begin
    Statements := TObjectList<TGocciaASTNode>.Create(False);
    Statements.Add(FunctionExpression.Body);
  end;

  if FunctionExpression.IsGenerator and FunctionExpression.IsAsync then
    Result := TGocciaAsyncGeneratorMethodValue.Create(
      FunctionExpression.Parameters, Statements, AContext.Scope.CreateChild,
      AName, nil)
  else if FunctionExpression.IsGenerator then
    Result := TGocciaGeneratorMethodValue.Create(
      FunctionExpression.Parameters, Statements, AContext.Scope.CreateChild,
      AName, nil)
  else if FunctionExpression.IsAsync then
    Result := TGocciaAsyncMethodValue.Create(
      FunctionExpression.Parameters, Statements, AContext.Scope.CreateChild,
      AName, nil)
  else
    Result := TGocciaMethodValue.Create(
      FunctionExpression.Parameters, Statements, AContext.Scope.CreateChild,
      AName, nil);

  ApplyFunctionObjectPrototype(Result,
    FunctionIntrinsicKind(FunctionExpression.IsAsync,
      FunctionExpression.IsGenerator));
  if FunctionExpression.IsGenerator then
    InstallFunctionOwnPrototypeProperty(Result,
      FunctionIntrinsicKind(FunctionExpression.IsAsync,
        FunctionExpression.IsGenerator));
  MethodValue := TGocciaMethodValue(Result);
  MethodValue.OwningClass := AHomeObject;
  if AContext.NonStrictMode and not HasStrictDirective then
  begin
    MethodValue.StrictThis := False;
    MethodValue.StrictCode := False;
  end
  else
    MethodValue.StrictCode := True;
  MethodValue.SourceFilePath := AContext.CurrentFilePath;
  MethodValue.SourceLine := FunctionExpression.Line;
  MethodValue.SourceText := FunctionExpression.SourceText;
end;

function EvaluateObject(const AObjectExpression: TGocciaObjectExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  ComputedPair: TPair<TGocciaExpression, TGocciaExpression>;
  ComputedKey: string;
  SpreadValue: TGocciaValue;
  I: Integer;
  GetterFunction: TGocciaValue;
  SetterFunction: TGocciaValue;
  PropertyName: string;
  PropertyExpression: TGocciaExpression;
  FinalPropertyExpression: TGocciaExpression;
  PropertyValue: TGocciaValue;
  PropertyKey: TGocciaValue;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
  ExistingGetter: TGocciaValue;
  IsProtoSetter: Boolean;
begin
  Obj := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Obj);

  try
    // Process all properties in source order
    for I := 0 to High(AObjectExpression.PropertySourceOrder) do
    begin
      case AObjectExpression.PropertySourceOrder[I].PropertyType of
        pstStatic:
          begin
            // Static property: {key: value}
            PropertyName := AObjectExpression.PropertySourceOrder[I].StaticKey;
            PropertyExpression := AObjectExpression.PropertySourceOrder[I].Expression;
            if (not Assigned(PropertyExpression)) and
               AObjectExpression.Properties.TryGetValue(PropertyName, FinalPropertyExpression) then
              PropertyExpression := FinalPropertyExpression;

            if Assigned(PropertyExpression) then
            begin
              if PropertyExpression is TGocciaObjectMethodDefinition then
                PropertyValue := EvaluateObjectMethodDefinition(
                  TGocciaObjectMethodDefinition(PropertyExpression), AContext,
                  Obj, PropertyName)
              else
                PropertyValue := EvaluateExpression(PropertyExpression, AContext);
              IsProtoSetter := (PropertyName = PROP_PROTO) and
                AObjectExpression.PropertySourceOrder[I].UsesColonSyntax;
              if not IsProtoSetter then
                ApplyInferredNameForExpression(PropertyExpression,
                  PropertyValue, PropertyName);

              if IsProtoSetter then
              begin
                if PropertyValue is TGocciaObjectValue then
                  Obj.Prototype := TGocciaObjectValue(PropertyValue)
                else if PropertyValue is TGocciaNullLiteralValue then
                  Obj.Prototype := nil;
              end
              else if AObjectExpression.Properties.TryGetValue(PropertyName, FinalPropertyExpression) and
                 (FinalPropertyExpression = PropertyExpression) then
                Obj.DefineProperty(PropertyName, TGocciaPropertyDescriptorData.Create(PropertyValue, [pfEnumerable, pfConfigurable, pfWritable]));
            end;
          end;

        pstComputed:
          begin
            // Computed property or spread: {[expr]: value} or {...obj}
            ComputedPair := AObjectExpression.ComputedPropertiesInOrder[AObjectExpression.PropertySourceOrder[I].ComputedIndex];

            if ComputedPair.Key is TGocciaSpreadExpression then
            begin
              // Spread expression: copy all enumerable properties
              SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ComputedPair.Key).Argument, AContext);
              CopyDataProperties(Obj, SpreadValue);
            end
            else
            begin
              // Regular computed property: {[expr]: value}. ES2026 §13.2.5.5
              // PropertyDefinitionEvaluation routes through ToPropertyKey.
              PropertyValue := EvaluateExpression(ComputedPair.Key, AContext);
              PropertyKey := ToPropertyKey(PropertyValue);
              PropertyExpression := ComputedPair.Value;
              if PropertyExpression is TGocciaObjectMethodDefinition then
                PropertyValue := EvaluateObjectMethodDefinition(
                  TGocciaObjectMethodDefinition(PropertyExpression), AContext,
                  Obj, FunctionNameFromPropertyKey(PropertyKey))
              else
              begin
                PropertyValue := EvaluateExpression(PropertyExpression, AContext);
                ApplyInferredNameForExpression(PropertyExpression, PropertyValue,
                  FunctionNameFromPropertyKey(PropertyKey));
              end;
              if PropertyKey is TGocciaSymbolValue then
                Obj.DefineSymbolProperty(TGocciaSymbolValue(PropertyKey), TGocciaPropertyDescriptorData.Create(PropertyValue, [pfEnumerable, pfConfigurable, pfWritable]))
              else
              begin
                ComputedKey := TGocciaStringLiteralValue(PropertyKey).Value;
                Obj.DefineProperty(ComputedKey, TGocciaPropertyDescriptorData.Create(PropertyValue, [pfEnumerable, pfConfigurable, pfWritable]));
              end;
            end;
          end;

        pstGetter:
          begin
            // Getter: {get prop() {...}}
            GetterFunction := EvaluateGetter(AObjectExpression.Getters[AObjectExpression.PropertySourceOrder[I].StaticKey], AContext);
            if GetterFunction is TGocciaFunctionValue then
              TGocciaFunctionValue(GetterFunction).SetInferredName('get ' + AObjectExpression.PropertySourceOrder[I].StaticKey);
            // Check if there's already a setter for this property
            ExistingDescriptor := Obj.GetOwnPropertyDescriptor(AObjectExpression.PropertySourceOrder[I].StaticKey);
            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
            begin
              // Merge with existing setter
              ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, ExistingSetter, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              // No existing setter, create getter-only descriptor
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil, [pfEnumerable, pfConfigurable]));
            end;
          end;

        pstSetter:
          begin
            // Setter: {set prop(val) {...}}
            SetterFunction := EvaluateSetter(AObjectExpression.Setters[AObjectExpression.PropertySourceOrder[I].StaticKey], AContext);
            if SetterFunction is TGocciaFunctionValue then
              TGocciaFunctionValue(SetterFunction).SetInferredName('set ' + AObjectExpression.PropertySourceOrder[I].StaticKey);
            // Check if there's already a getter for this property
            ExistingDescriptor := Obj.GetOwnPropertyDescriptor(AObjectExpression.PropertySourceOrder[I].StaticKey);
            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
            begin
              // Merge with existing getter
              ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(ExistingGetter, SetterFunction, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              // No existing getter, create setter-only descriptor
              Obj.DefineProperty(AObjectExpression.PropertySourceOrder[I].StaticKey, TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction, [pfEnumerable, pfConfigurable]));
            end;
          end;

        pstComputedGetter:
          begin
            ComputedPair := AObjectExpression.ComputedPropertiesInOrder[
              AObjectExpression.PropertySourceOrder[I].ComputedIndex];
            PropertyValue := EvaluateExpression(ComputedPair.Key, AContext);
            PropertyKey := ToPropertyKey(PropertyValue);
            GetterFunction := EvaluateGetter(
              TGocciaGetterExpression(ComputedPair.Value), AContext);
            if GetterFunction is TGocciaFunctionValue then
              TGocciaFunctionValue(GetterFunction).SetInferredName(
                FunctionNameFromPropertyKey(PropertyKey, 'get'));

            if PropertyKey is TGocciaSymbolValue then
            begin
              ExistingDescriptor := Obj.GetOwnSymbolPropertyDescriptor(
                TGocciaSymbolValue(PropertyKey));
              ExistingSetter := nil;
              if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                 Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
                ExistingSetter :=
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              Obj.DefineSymbolProperty(TGocciaSymbolValue(PropertyKey),
                TGocciaPropertyDescriptorAccessor.Create(GetterFunction,
                  ExistingSetter, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              ComputedKey := TGocciaStringLiteralValue(PropertyKey).Value;
              ExistingDescriptor := Obj.GetOwnPropertyDescriptor(ComputedKey);
              ExistingSetter := nil;
              if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                 Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
                ExistingSetter :=
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              Obj.DefineProperty(ComputedKey,
                TGocciaPropertyDescriptorAccessor.Create(GetterFunction,
                  ExistingSetter, [pfEnumerable, pfConfigurable]));
            end;
          end;

        pstComputedSetter:
          begin
            ComputedPair := AObjectExpression.ComputedPropertiesInOrder[
              AObjectExpression.PropertySourceOrder[I].ComputedIndex];
            PropertyValue := EvaluateExpression(ComputedPair.Key, AContext);
            PropertyKey := ToPropertyKey(PropertyValue);
            SetterFunction := EvaluateSetter(
              TGocciaSetterExpression(ComputedPair.Value), AContext);
            if SetterFunction is TGocciaFunctionValue then
              TGocciaFunctionValue(SetterFunction).SetInferredName(
                FunctionNameFromPropertyKey(PropertyKey, 'set'));

            if PropertyKey is TGocciaSymbolValue then
            begin
              ExistingDescriptor := Obj.GetOwnSymbolPropertyDescriptor(
                TGocciaSymbolValue(PropertyKey));
              ExistingGetter := nil;
              if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                 Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
                ExistingGetter :=
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
              Obj.DefineSymbolProperty(TGocciaSymbolValue(PropertyKey),
                TGocciaPropertyDescriptorAccessor.Create(ExistingGetter,
                  SetterFunction, [pfEnumerable, pfConfigurable]));
            end
            else
            begin
              ComputedKey := TGocciaStringLiteralValue(PropertyKey).Value;
              ExistingDescriptor := Obj.GetOwnPropertyDescriptor(ComputedKey);
              ExistingGetter := nil;
              if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                 Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
                ExistingGetter :=
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
              Obj.DefineProperty(ComputedKey,
                TGocciaPropertyDescriptorAccessor.Create(ExistingGetter,
                  SetterFunction, [pfEnumerable, pfConfigurable]));
            end;
          end;
      end;
    end;

  Result := Obj;
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Obj);
  end;
end;

function EvaluateGetter(const AGetterExpression: TGocciaGetterExpression; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil; const AAsMethod: Boolean = False): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  EmptyParameters: TGocciaParameterArray;
  HasStrictDirective: Boolean;
begin
  // Getter has no parameters
  SetLength(EmptyParameters, 0);

  Statements := CopyStatementList(TGocciaBlockStatement(AGetterExpression.Body).Nodes);
  HasStrictDirective := HasUseStrictDirective(AGetterExpression.Body);

  // Create function with closure scope
  if AAsMethod or Assigned(ASuperClass) then
    Result := TGocciaMethodValue.Create(EmptyParameters, Statements, AContext.Scope.CreateChild, '', ASuperClass)
  else
    Result := TGocciaFunctionValue.Create(EmptyParameters, Statements, AContext.Scope.CreateChild);
  if AContext.NonStrictMode and not HasStrictDirective and
     not (AAsMethod or Assigned(ASuperClass)) then
  begin
    TGocciaFunctionValue(Result).StrictThis := False;
    TGocciaFunctionValue(Result).StrictCode := False;
  end
  else
    TGocciaFunctionValue(Result).StrictCode := True;
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AGetterExpression.Line;
  TGocciaFunctionValue(Result).SourceText := AGetterExpression.SourceText;
end;

function EvaluateSetter(const ASetterExpression: TGocciaSetterExpression; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil; const AAsMethod: Boolean = False): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  Parameters: TGocciaParameterArray;
  HasStrictDirective: Boolean;
begin
  Parameters := ASetterExpression.Parameters;

  Statements := CopyStatementList(TGocciaBlockStatement(ASetterExpression.Body).Nodes);
  HasStrictDirective := HasUseStrictDirective(ASetterExpression.Body);

  // Create function with closure scope
  if AAsMethod or Assigned(ASuperClass) then
    Result := TGocciaMethodValue.Create(Parameters, Statements, AContext.Scope.CreateChild, '', ASuperClass)
  else
    Result := TGocciaFunctionValue.Create(Parameters, Statements, AContext.Scope.CreateChild);
  if AContext.NonStrictMode and not HasStrictDirective and
     not (AAsMethod or Assigned(ASuperClass)) then
  begin
    TGocciaFunctionValue(Result).StrictThis := False;
    TGocciaFunctionValue(Result).StrictCode := False;
  end
  else
    TGocciaFunctionValue(Result).StrictCode := True;
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := ASetterExpression.Line;
  TGocciaFunctionValue(Result).SourceText := ASetterExpression.SourceText;
end;

// ES2026 §27.7.5.3 Await(value)
function EvaluateAwait(const AAwaitExpression: TGocciaAwaitExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  if AsyncAwaitSuspensionEnabled and Assigned(CurrentGeneratorContinuation) then
    Exit(EvaluateGeneratorAwait(AAwaitExpression, AContext));

  Result := AwaitValue(EvaluateExpression(AAwaitExpression.Operand, AContext));
end;

function EvaluateYield(const AYieldExpression: TGocciaYieldExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := EvaluateGeneratorYield(AYieldExpression, AContext);
end;

// ES2026 §14.7.5.6 ForIn/OfBodyEvaluation(lhs, stmt, iteratorRecord, iterationKind, lhsKind)
function EvaluateForOf(const AForOfStatement: TGocciaForOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  IterableValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  IterResult: TGocciaObjectValue;
  CurrentValue: TGocciaValue;
  LoopValue: TGocciaValue;
  CF: TGocciaControlFlow;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
  MatchContext, MatchBaseContext: TGocciaEvaluationContext;
  Continuation: TGocciaGeneratorContinuation;
  SavedIteratorValue, SavedCurrentValue, SavedNextMethod: TGocciaValue;
  SavedIterScope, SavedActiveScope: TGocciaScope;
  HasSavedLoopState: Boolean;
  HeadCompleted, HeadYielding: Boolean;
  BodyYielding: Boolean;
  ShouldCloseIterator: Boolean;
  IterationTracker: TGocciaDisposalTracker;
  DisposalError: TGocciaValue;

  procedure RegisterForOfUsingResource;
  var
    DisposeMethod: TGocciaValue;
    Hint: TGocciaDisposalHint;
  begin
    if not AForOfStatement.IsUsing then
      Exit;

    if AForOfStatement.IsAwaitUsing then
      Hint := dhAsyncDispose
    else
      Hint := dhSyncDispose;

    IterationTracker := TGocciaDisposalTracker.Create;
    try
      if (CurrentValue is TGocciaUndefinedLiteralValue) or
         (CurrentValue is TGocciaNullLiteralValue) then
      begin
        if Hint = dhAsyncDispose then
          IterationTracker.AddResource(nil, nil, dhAsyncDispose);
        Exit;
      end;

      DisposeMethod := GetDisposeMethod(CurrentValue, Hint);
      if not Assigned(DisposeMethod) then
      begin
        if Hint = dhAsyncDispose then
          ThrowTypeError(SErrorNotAsyncDisposable, SSuggestDisposable)
        else
          ThrowTypeError(SErrorNotDisposable, SSuggestDisposable);
      end;
      IterationTracker.AddResource(CurrentValue, DisposeMethod, Hint);
    except
      IterationTracker.Free;
      IterationTracker := nil;
      raise;
    end;
  end;

  function DisposeForOfUsingResource(const AExistingError: TGocciaValue): TGocciaValue;
  var
    GC: TGarbageCollector;
    ScopeRooted: Boolean;
  begin
    Result := nil;
    if not Assigned(IterationTracker) then
      Exit;

    GC := TGarbageCollector.Instance;
    ScopeRooted := Assigned(GC) and Assigned(IterScope);
    if ScopeRooted then
      GC.AddTempRoot(IterScope);
    try
      if HasAsyncDisposals(IterationTracker) then
        Result := DisposeTrackedResourcesAsync(IterationTracker, AExistingError)
      else
        Result := DisposeTrackedResources(IterationTracker, AExistingError);
    finally
      if ScopeRooted then
        GC.RemoveTempRoot(IterScope);
      IterationTracker.Free;
      IterationTracker := nil;
    end;
  end;

  procedure ClearSavedLoopState;
  var
    SavedTracker: TGocciaDisposalTracker;
  begin
    if not Assigned(Continuation) then
      Exit;

    SavedTracker := Continuation.TakeLoopDisposalTracker(AForOfStatement);
    if Assigned(SavedTracker) and (SavedTracker <> IterationTracker) then
    begin
      if Assigned(IterationTracker) then
      begin
        IterationTracker.MoveResourcesFrom(SavedTracker);
        SavedTracker.Free;
      end
      else
        IterationTracker := SavedTracker;
    end;
    Continuation.ClearLoopState(AForOfStatement);
  end;
begin
  LoopValue := UndefinedCompletionValue;
  Result := TGocciaControlFlow.Normal(LoopValue);
  IterationTracker := nil;

  Continuation := CurrentGeneratorContinuation;
  HasSavedLoopState := Assigned(Continuation) and
    Continuation.GetLoopState(AForOfStatement, SavedIteratorValue,
      SavedCurrentValue, SavedNextMethod, SavedIterScope, SavedActiveScope);
  if HasSavedLoopState then
  begin
    Iterator := TGocciaIteratorValue(SavedIteratorValue);
    IterationTracker :=
      Continuation.GetLoopDisposalTracker(AForOfStatement);
  end
  else
  begin
    IterableValue := EvaluateExpressionWithLoopHeadTDZ(
      AForOfStatement.Iterable,
      AContext,
      AForOfStatement.BindingName,
      AForOfStatement.BindingPattern,
      (not AForOfStatement.IsVar) and
      not Assigned(AForOfStatement.AssignmentTarget));
    Iterator := GetIteratorFromValue(IterableValue);
    if Iterator = nil then
    begin
      if AForOfStatement.Iterable is TGocciaIdentifierExpression then
        ThrowTypeError(
          Format(SErrorNotIterable,
            [TGocciaIdentifierExpression(AForOfStatement.Iterable).Name]),
          Format('''%s'' is of type ''%s'' which does not implement the iterator protocol',
            [TGocciaIdentifierExpression(AForOfStatement.Iterable).Name,
             IterableValue.TypeName]))
      else
        ThrowTypeError(
          Format(SErrorNotIterable, [IterableValue.TypeName]),
          SSuggestIteratorProtocol);
    end;
  end;

  if AForOfStatement.IsConst then
    DeclarationType := dtConst
  else
    DeclarationType := dtLet;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    if not HasSavedLoopState then
      IterResult := Iterator.AdvanceNext;
    while True do
    begin
      if not HasSavedLoopState then
        IterationTracker := nil;
      IterScope := nil;
      if HasSavedLoopState then
      begin
        CurrentValue := SavedCurrentValue;
        IterScope := SavedIterScope;
        HasSavedLoopState := False;
      end
      else
      begin
        if IterResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
          Break;
        CurrentValue := IterResult.GetProperty(PROP_VALUE);
      end;
      if Assigned(Continuation) and not Assigned(IterScope) then
        Continuation.SaveLoopState(AForOfStatement, Iterator, CurrentValue);

      if Assigned(IterScope) then
      begin
        IterContext := AContext;
        if Assigned(SavedActiveScope) then
          IterContext.Scope := SavedActiveScope
        else
          IterContext.Scope := IterScope;
        MatchBaseContext := AContext;
        MatchBaseContext.Scope := IterScope;
      end
      else
      begin
        HeadCompleted := False;
        HeadYielding := False;
        ShouldCloseIterator := True;
        try
          try
            CheckExecutionTimeout;
            IncrementInstructionCounter;
            CheckInstructionLimit;

            IterScope := AContext.Scope.CreateChild(skBlock);
            IterContext := AContext;
            IterContext.Scope := IterScope;

            if Assigned(AForOfStatement.AssignmentTarget) then
              AssignPattern(AForOfStatement.AssignmentTarget, CurrentValue,
                IterContext)
            else if AForOfStatement.IsVar then
            begin
              // var binding: define/update on function/module scope
              if AForOfStatement.BindingPattern <> nil then
                AssignPattern(AForOfStatement.BindingPattern, CurrentValue,
                  IterContext)
              else
                AContext.Scope.DefineVariableBinding(AForOfStatement.BindingName, CurrentValue, True);
            end
            else if AForOfStatement.BindingPattern <> nil then
              AssignPattern(AForOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
            else
              IterScope.DefineLexicalBinding(AForOfStatement.BindingName, CurrentValue, DeclarationType);

            RegisterForOfUsingResource;

            if Assigned(AForOfStatement.MatchPattern) then
            begin
              MatchBaseContext := IterContext;
              if not TryEvaluateMatchPatternInContext(CurrentValue,
                 AForOfStatement.MatchPattern, IterContext, MatchContext) then
              begin
                ClearSavedLoopState;
                DisposalError := DisposeForOfUsingResource(nil);
                if Assigned(DisposalError) then
                  raise TGocciaThrowValue.Create(DisposalError);
                ShouldCloseIterator := False;
                IterResult := Iterator.AdvanceNext;
                Continue;
              end;
              IterContext := MatchContext;
            end;
            HeadCompleted := True;
            if Assigned(Continuation) then
              Continuation.SaveLoopState(AForOfStatement, Iterator, CurrentValue,
                nil, IterScope, IterContext.Scope, IterationTracker);
          except
            on E: EGocciaGeneratorYield do
            begin
              HeadYielding := True;
              raise;
            end;
            on E: TGocciaThrowValue do
            begin
              DisposalError := DisposeForOfUsingResource(E.Value);
              if ShouldCloseIterator then
                Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
              if Assigned(DisposalError) then
                raise TGocciaThrowValue.Create(DisposalError);
              raise;
            end;
            on E: Exception do
            begin
              DisposeForOfUsingResource(nil);
              if ShouldCloseIterator then
                Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
              raise;
            end;
          end;
        finally
          if (not HeadCompleted) and (not HeadYielding) and
             Assigned(Continuation) then
          begin
            ClearSavedLoopState;
            Continuation.ClearExpressionValues;
          end;
        end;
      end;

      try
        BodyYielding := False;
        try
          CF := EvaluateLoopBodyStatement(AForOfStatement.Body, IterContext);
          ClearSavedLoopState;
        except
          on E: EGocciaGeneratorYield do
          begin
            BodyYielding := True;
            raise;
          end;
          on E: TGocciaThrowValue do
          begin
            ClearSavedLoopState;
            DisposalError := DisposeForOfUsingResource(E.Value);
            Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
            if Assigned(DisposalError) then
              raise TGocciaThrowValue.Create(DisposalError);
            raise;
          end;
          else
          begin
            ClearSavedLoopState;
            DisposeForOfUsingResource(nil);
            Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
            raise;
          end;
        end;
      finally
        if (not BodyYielding) and Assigned(AForOfStatement.MatchPattern) and
           (IterContext.Scope <> IterScope) then
          ReleaseMatchContext(IterContext, MatchBaseContext);
      end;
      DisposalError := DisposeForOfUsingResource(nil);
      if Assigned(DisposalError) then
      begin
        Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
        raise TGocciaThrowValue.Create(DisposalError);
      end;
      if CF.Kind = cfkBreak then
      begin
        if not TargetsStatementOrUnlabeled(CF, AForOfStatement) then
        begin
          ClearSavedLoopState;
          Goccia.Values.IteratorSupport.CloseIterator(Iterator);
          Result := CF.UpdateEmpty(LoopValue);
          Exit;
        end;
        ClearSavedLoopState;
        Goccia.Values.IteratorSupport.CloseIterator(Iterator);
        Result := NormalCompletionFromAbrupt(CF, LoopValue);
        LoopValue := Result.Value;
        Break;
      end;
      if CF.Kind = cfkReturn then
      begin
        ClearSavedLoopState;
        Goccia.Values.IteratorSupport.CloseIterator(Iterator);
        Result := CF.UpdateEmpty(LoopValue);
        Exit;
      end;
      if (CF.Kind = cfkContinue) and
         not TargetsStatementOrUnlabeled(CF, AForOfStatement) then
      begin
        ClearSavedLoopState;
        Goccia.Values.IteratorSupport.CloseIterator(Iterator);
        Result := CF.UpdateEmpty(LoopValue);
        Exit;
      end;
      // cfkContinue: skip remaining body, advance to next iteration
      UpdateValueFromCompletion(CF, LoopValue);

      IterResult := Iterator.AdvanceNext;
    end;
    ClearSavedLoopState;
    Result := TGocciaControlFlow.Normal(LoopValue);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

function CreateForInEntriesArray(const AValue: TGocciaValue): TGocciaArrayValue;
var
  Current, Obj, EntryObj: TGocciaObjectValue;
  Keys: TArray<string>;
  Key: string;
  KeyValue: TGocciaStringLiteralValue;
  Visited: TStringList;
  GC: TGarbageCollector;
  ChainDepth: Integer;
begin
  GC := TGarbageCollector.Instance;
  Result := TGocciaArrayValue.Create;
  if Assigned(GC) then
    GC.AddTempRoot(Result);
  try
    if (AValue is TGocciaUndefinedLiteralValue) or
       (AValue is TGocciaNullLiteralValue) then
      Exit;

    Obj := ToObject(AValue);
    if Assigned(GC) then
      GC.AddTempRoot(Obj);
    Visited := TStringList.Create;
    try
      Visited.CaseSensitive := True;
      Current := Obj;
      ChainDepth := 0;
      while Assigned(Current) do
      begin
        Inc(ChainDepth);
        if ChainDepth > FOR_IN_MAX_PROTOTYPE_CHAIN_DEPTH then
          ThrowTypeError(Format(SErrorProtoChainDepthExceeded, ['for...in']),
            SSuggestPrototypeChainTooDeep);

        Keys := OrderForInPropertyKeys(Current.GetAllPropertyNames);
        for Key in Keys do
        begin
          if Visited.IndexOf(Key) >= 0 then
            Continue;

          Visited.Add(Key);
          EntryObj := TGocciaObjectValue.Create;
          if Assigned(GC) then
            GC.AddTempRoot(EntryObj);
          try
            EntryObj.DefineProperty(FOR_IN_ENTRY_OWNER,
              TGocciaPropertyDescriptorData.Create(Current, []));
            KeyValue := TGocciaStringLiteralValue.Create(Key);
            if Assigned(GC) then
              GC.AddTempRoot(KeyValue);
            try
              EntryObj.DefineProperty(FOR_IN_ENTRY_KEY,
                TGocciaPropertyDescriptorData.Create(KeyValue, []));
            finally
              if Assigned(GC) then
                GC.RemoveTempRoot(KeyValue);
            end;
            Result.Elements.Add(EntryObj);
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(EntryObj);
          end;
        end;
        Current := Current.Prototype;
      end;
    finally
      Visited.Free;
      if Assigned(GC) then
        GC.RemoveTempRoot(Obj);
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Result);
  end;
end;

function TryForInEntryKey(const AEntry: TGocciaValue; out AKey: string): Boolean;
var
  EntryObj, Owner: TGocciaObjectValue;
  OwnerValue, KeyValue: TGocciaValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AKey := '';
  if not (AEntry is TGocciaObjectValue) then
    Exit(False);

  EntryObj := TGocciaObjectValue(AEntry);
  OwnerValue := EntryObj.GetProperty(FOR_IN_ENTRY_OWNER);
  KeyValue := EntryObj.GetProperty(FOR_IN_ENTRY_KEY);
  if not (OwnerValue is TGocciaObjectValue) or
     not (KeyValue is TGocciaStringLiteralValue) then
    Exit(False);

  Owner := TGocciaObjectValue(OwnerValue);
  AKey := TGocciaStringLiteralValue(KeyValue).Value;
  Descriptor := Owner.GetOwnPropertyDescriptor(AKey);
  Result := Assigned(Descriptor) and Descriptor.Enumerable;
end;

// ES2026 §14.7.5.5 Runtime Semantics: ForInOfLoopEvaluation
function EvaluateForIn(const AForInStatement: TGocciaForInStatement;
  const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  SourceValue: TGocciaValue;
  EntriesArray: TGocciaArrayValue;
  EntryIndex: Integer;
  EntryValue: TGocciaValue;
  CurrentValue: TGocciaValue;
  LoopValue: TGocciaValue;
  CurrentKey: string;
  FoundKey: Boolean;
  CF: TGocciaControlFlow;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
  Continuation: TGocciaGeneratorContinuation;
  SavedIteratorValue, SavedCurrentValue, SavedNextMethod: TGocciaValue;
  SavedIterScope, SavedActiveScope: TGocciaScope;
  HasSavedLoopState: Boolean;
begin
  LoopValue := UndefinedCompletionValue;
  Result := TGocciaControlFlow.Normal(LoopValue);

  Continuation := CurrentGeneratorContinuation;
  HasSavedLoopState := Assigned(Continuation) and
    Continuation.GetLoopState(AForInStatement, SavedIteratorValue,
      SavedCurrentValue, SavedNextMethod, SavedIterScope, SavedActiveScope);
  if HasSavedLoopState then
  begin
    EntriesArray := TGocciaArrayValue(SavedIteratorValue);
    EntryIndex := 0;
    if SavedNextMethod is TGocciaNumberLiteralValue then
      EntryIndex := Trunc(TGocciaNumberLiteralValue(SavedNextMethod).Value);
  end
  else
  begin
    SourceValue := EvaluateExpressionWithLoopHeadTDZ(
      AForInStatement.ObjectExpression,
      AContext,
      AForInStatement.BindingName,
      AForInStatement.BindingPattern,
      (not AForInStatement.IsVar) and
      not Assigned(AForInStatement.AssignmentTarget));
    EntriesArray := CreateForInEntriesArray(SourceValue);
    EntryIndex := 0;
  end;

  if AForInStatement.IsConst then
    DeclarationType := dtConst
  else
    DeclarationType := dtLet;

  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AddTempRoot(EntriesArray);
  try
    while True do
    begin
      IterScope := nil;
      if HasSavedLoopState then
      begin
        CurrentValue := SavedCurrentValue;
        IterScope := SavedIterScope;
        HasSavedLoopState := False;
      end
      else
      begin
        CurrentKey := '';
        FoundKey := False;
        while EntryIndex < EntriesArray.Elements.Count do
        begin
          EntryValue := EntriesArray.Elements[EntryIndex];
          Inc(EntryIndex);
          if TryForInEntryKey(EntryValue, CurrentKey) then
          begin
            FoundKey := True;
            Break;
          end;
        end;
        if not FoundKey then
          Break;
        CurrentValue := TGocciaStringLiteralValue.Create(CurrentKey);
      end;

      CheckExecutionTimeout;
      IncrementInstructionCounter;
      CheckInstructionLimit;

      if Assigned(IterScope) then
      begin
        IterContext := AContext;
        if Assigned(SavedActiveScope) then
          IterContext.Scope := SavedActiveScope
        else
          IterContext.Scope := IterScope;
      end
      else
      begin
        IterScope := AContext.Scope.CreateChild(skBlock);
        IterContext := AContext;
        IterContext.Scope := IterScope;

        if Assigned(Continuation) then
          Continuation.SaveLoopState(AForInStatement, EntriesArray,
            CurrentValue, TGocciaNumberLiteralValue.Create(EntryIndex), nil,
            nil);

        try
          if AForInStatement.IsVar then
          begin
            if AForInStatement.BindingPattern <> nil then
              AssignPattern(AForInStatement.BindingPattern, CurrentValue,
                IterContext)
            else
              AContext.Scope.DefineVariableBinding(
                AForInStatement.BindingName, CurrentValue, True);
          end
          else if AForInStatement.AssignmentTarget <> nil then
            AssignPattern(AForInStatement.AssignmentTarget, CurrentValue,
              IterContext)
          else if AForInStatement.BindingPattern <> nil then
            AssignPattern(AForInStatement.BindingPattern, CurrentValue,
              IterContext, True, DeclarationType)
          else
            IterScope.DefineLexicalBinding(AForInStatement.BindingName,
              CurrentValue, DeclarationType);
        except
          on E: EGocciaGeneratorYield do
            raise;
          else
          begin
            if Assigned(Continuation) then
              Continuation.ClearLoopState(AForInStatement);
            raise;
          end;
        end;

        if Assigned(Continuation) then
          Continuation.SaveLoopState(AForInStatement, EntriesArray, CurrentValue,
            TGocciaNumberLiteralValue.Create(EntryIndex), IterScope,
            IterContext.Scope);
      end;

      try
        CF := EvaluateLoopBodyStatement(AForInStatement.Body, IterContext);
        if Assigned(Continuation) then
          Continuation.ClearLoopState(AForInStatement);
      except
        on E: EGocciaGeneratorYield do
          raise;
        else
        begin
          if Assigned(Continuation) then
            Continuation.ClearLoopState(AForInStatement);
          raise;
        end;
      end;

      if CF.Kind = cfkBreak then
      begin
        if not TargetsStatementOrUnlabeled(CF, AForInStatement) then
        begin
          Result := CF.UpdateEmpty(LoopValue);
          Exit;
        end;
        Result := NormalCompletionFromAbrupt(CF, LoopValue);
        LoopValue := Result.Value;
        Break;
      end;
      if CF.Kind = cfkReturn then
      begin
        Result := CF.UpdateEmpty(LoopValue);
        Exit;
      end;
      if (CF.Kind = cfkContinue) and
         not TargetsStatementOrUnlabeled(CF, AForInStatement) then
      begin
        Result := CF.UpdateEmpty(LoopValue);
        Exit;
      end;
      UpdateValueFromCompletion(CF, LoopValue);
    end;
    if Assigned(Continuation) then
      Continuation.ClearLoopState(AForInStatement);
    Result := TGocciaControlFlow.Normal(LoopValue);
  finally
    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.RemoveTempRoot(EntriesArray);
  end;
end;

// ES2026 §14.7.4.2 ForBodyEvaluation — traditional for(init; test; update) loop
function EvaluateFor(const AForStatement: TGocciaForStatement;
  const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  HeaderScope, IterScope, UpdateScope: TGocciaScope;
  HeaderContext, IterContext, UpdateContext: TGocciaEvaluationContext;
  IsLexical: Boolean;
  PerIterNames: TStringList;
  PrevValues: array of TGocciaValue;
  CondValue: TGocciaValue;
  LoopValue: TGocciaValue;
  CF: TGocciaControlFlow;
  I: Integer;
  Name: string;
  HeaderBinding, IterBinding: TLexicalBinding;
  VarDecl: TGocciaVariableDeclaration;
  DestructDecl: TGocciaDestructuringDeclaration;
  DeclarationType: TGocciaDeclarationType;
  Continuation: TGocciaGeneratorContinuation;
  ForState: TGocciaGeneratorForLoopState;
  ResumePhase: TGocciaGeneratorForLoopPhase;
  HasResumeState: Boolean;
  HasPerIterationValues: Boolean;
begin
  LoopValue := UndefinedCompletionValue;
  Result := TGocciaControlFlow.Normal(LoopValue);

  IsLexical := False;
  PerIterNames := nil;
  HeaderScope := nil;
  IterScope := nil;
  UpdateScope := nil;
  HeaderContext := AContext;
  IterContext := AContext;
  UpdateContext := AContext;

  if Assigned(AForStatement.Init) then
  begin
    if (AForStatement.Init is TGocciaVariableDeclaration) and
       not TGocciaVariableDeclaration(AForStatement.Init).IsVar then
    begin
      IsLexical := True;
      VarDecl := TGocciaVariableDeclaration(AForStatement.Init);
      if VarDecl.IsConst then
        DeclarationType := dtConst
      else
        DeclarationType := dtLet;
      PerIterNames := TStringList.Create;
      for I := 0 to High(VarDecl.Variables) do
        PerIterNames.Add(VarDecl.Variables[I].Name);
    end
    else if (AForStatement.Init is TGocciaDestructuringDeclaration)
            and not TGocciaDestructuringDeclaration(AForStatement.Init).IsVar then
    begin
      IsLexical := True;
      DestructDecl := TGocciaDestructuringDeclaration(AForStatement.Init);
      if DestructDecl.IsConst then
        DeclarationType := dtConst
      else
        DeclarationType := dtLet;
      PerIterNames := TStringList.Create;
      CollectPatternBindingNames(DestructDecl.Pattern, PerIterNames, True);
    end;
  end;

  Continuation := CurrentGeneratorContinuation;
  ForState := nil;
  HasResumeState := False;
  HasPerIterationValues := False;
  if Assigned(Continuation) then
  begin
    ForState := Continuation.GetForLoopState(AForStatement);
    HasResumeState := Assigned(ForState);
  end;

  if HasResumeState then
  begin
    // Resume from a previously suspended evaluation: restore the scopes the
    // generator captured at suspension and skip whichever sub-step already
    // completed. The phase records the next step to run.
    if IsLexical then
    begin
      HeaderScope := ForState.HeaderScope;
      HeaderContext.Scope := HeaderScope;
    end;
    ResumePhase := ForState.Phase;
    if (ResumePhase in [gflpTest, gflpBody]) and IsLexical then
    begin
      IterScope := ForState.IterScope;
      IterContext.Scope := IterScope;
    end
    else if (ResumePhase = gflpUpdate) and IsLexical then
    begin
      UpdateScope := ForState.UpdateScope;
      UpdateContext.Scope := UpdateScope;
    end
    else
      IterContext := HeaderContext;
  end
  else
    ResumePhase := gflpIterStart;

  try
    try
      if not HasResumeState then
      begin
        if IsLexical then
        begin
          HeaderScope := AContext.Scope.CreateChild(skBlock, 'ForHeader');
          HeaderContext.Scope := HeaderScope;
        end;

        if Assigned(Continuation) then
        begin
          ForState := Continuation.EnsureForLoopState(AForStatement, HeaderScope);
          ForState.Phase := gflpInit;
        end;
      end;

      if (not HasResumeState) or (ResumePhase = gflpInit) then
      begin
        if Assigned(AForStatement.Init) then
        begin
          CF := EvaluateStatement(AForStatement.Init, HeaderContext);
          if CF.Kind = cfkReturn then
          begin
            Result := CF;
            Exit;
          end;
        end;

        // Init completed — persist HeaderScope so a yield in a later sub-step
        // does not cause the next resume to re-run Init.
        if Assigned(Continuation) then
        begin
          ForState := Continuation.EnsureForLoopState(AForStatement, HeaderScope);
          ForState.Phase := gflpIterStart;
        end;
        ResumePhase := gflpIterStart;
      end;

      while True do
      begin
        CheckExecutionTimeout;
        IncrementInstructionCounter;
        CheckInstructionLimit;

        if ResumePhase = gflpIterStart then
        begin
          if IsLexical then
          begin
            SetLength(PrevValues, PerIterNames.Count);
            if not HasPerIterationValues then
            begin
              for I := 0 to PerIterNames.Count - 1 do
              begin
                Name := PerIterNames[I];
                HeaderBinding := HeaderScope.GetBinding(Name);
                PrevValues[I] := HeaderBinding.Value;
              end;
              HasPerIterationValues := True;
            end;
            IterScope := AContext.Scope.CreateChild(skBlock, 'ForIter');
            IterContext := AContext;
            IterContext.Scope := IterScope;
            for I := 0 to PerIterNames.Count - 1 do
            begin
              Name := PerIterNames[I];
              HeaderBinding := HeaderScope.GetBinding(Name);
              IterScope.DefineLexicalBinding(Name, PrevValues[I], DeclarationType);
              if HeaderBinding.TypeHint <> sltUntyped then
                IterScope.SetOwnBindingTypeHint(Name, HeaderBinding.TypeHint);
            end;
            if Assigned(ForState) then
            begin
              ForState.Phase := gflpTest;
              ForState.IterScope := IterScope;
            end;
          end
          else
            IterContext := HeaderContext;
          ResumePhase := gflpTest;
        end;

        if ResumePhase = gflpTest then
        begin
          if Assigned(AForStatement.Condition) then
          begin
            CondValue := EvaluateExpression(AForStatement.Condition, IterContext);
            if not CondValue.ToBooleanLiteral.Value then
            begin
              if Assigned(Continuation) then
                Continuation.ClearForLoopState(AForStatement);
              Break;
            end;
          end;

          // Condition passed; commit IterScope so a yield in the body resumes
          // into the same iteration rather than re-snapshotting from Header.
          if Assigned(ForState) then
          begin
            ForState.Phase := gflpBody;
            if IsLexical then
              ForState.IterScope := IterScope;
          end;
          ResumePhase := gflpBody;
        end;

        if ResumePhase = gflpBody then
        begin
          CF := EvaluateLoopBodyStatement(AForStatement.Body, IterContext);
          if CF.Kind = cfkBreak then
          begin
            if not TargetsStatementOrUnlabeled(CF, AForStatement) then
            begin
              if Assigned(Continuation) then
                Continuation.ClearForLoopState(AForStatement);
              Result := CF.UpdateEmpty(LoopValue);
              Exit;
            end;
            if Assigned(Continuation) then
              Continuation.ClearForLoopState(AForStatement);
            Result := NormalCompletionFromAbrupt(CF, LoopValue);
            LoopValue := Result.Value;
            Break;
          end;
          if CF.Kind = cfkReturn then
          begin
            if Assigned(Continuation) then
              Continuation.ClearForLoopState(AForStatement);
            Result := CF.UpdateEmpty(LoopValue);
            Exit;
          end;
          if (CF.Kind = cfkContinue) and
             not TargetsStatementOrUnlabeled(CF, AForStatement) then
          begin
            if Assigned(Continuation) then
              Continuation.ClearForLoopState(AForStatement);
            Result := CF.UpdateEmpty(LoopValue);
            Exit;
          end;
          // cfkContinue and cfkNormal both fall through to update.
          UpdateValueFromCompletion(CF, LoopValue);

          // ES2026 §14.7.4.4 step 3.e: create the next per-iteration
          // environment after the body and before the update expression.
          // Closures created in the body keep IterScope; closures created in
          // the update expression capture this fresh UpdateScope.
          if IsLexical then
          begin
            UpdateScope := AContext.Scope.CreateChild(skBlock, 'ForUpdate');
            UpdateContext := AContext;
            UpdateContext.Scope := UpdateScope;
            for I := 0 to PerIterNames.Count - 1 do
            begin
              Name := PerIterNames[I];
              IterBinding := IterScope.GetBinding(Name);
              UpdateScope.DefineLexicalBinding(Name, IterBinding.Value, DeclarationType);
              if IterBinding.TypeHint <> sltUntyped then
                UpdateScope.SetOwnBindingTypeHint(Name, IterBinding.TypeHint);
            end;
          end;

          if Assigned(ForState) then
          begin
            ForState.Phase := gflpUpdate;
            ForState.IterScope := nil;
            if IsLexical then
              ForState.UpdateScope := UpdateScope;
          end;
          ResumePhase := gflpUpdate;
        end;

        if Assigned(AForStatement.Update) then
        begin
          if IsLexical then
            EvaluateExpression(AForStatement.Update, UpdateContext)
          else
            EvaluateExpression(AForStatement.Update, IterContext);
        end;

        if IsLexical then
        begin
          SetLength(PrevValues, PerIterNames.Count);
          for I := 0 to PerIterNames.Count - 1 do
          begin
            Name := PerIterNames[I];
            PrevValues[I] := UpdateScope.GetBinding(Name).Value;
          end;
          HasPerIterationValues := True;
        end;

        if Assigned(ForState) then
        begin
          ForState.Phase := gflpIterStart;
          ForState.UpdateScope := nil;
        end;
        ResumePhase := gflpIterStart;
      end;
    except
      on E: EGocciaGeneratorYield do
        raise;
      else
      begin
        if Assigned(Continuation) then
          Continuation.ClearForLoopState(AForStatement);
        raise;
      end;
    end;
  finally
    if Assigned(PerIterNames) then
      PerIterNames.Free;
  end;
  Result := TGocciaControlFlow.Normal(LoopValue);
end;

function InitializeGeneratorLoopState(const ALoopStatement: TObject;
  const AInitialPhase: TGocciaGeneratorForLoopPhase;
  out AContinuation: TGocciaGeneratorContinuation;
  out ALoopState: TGocciaGeneratorForLoopState): TGocciaGeneratorForLoopPhase;
begin
  AContinuation := CurrentGeneratorContinuation;
  ALoopState := nil;
  Result := AInitialPhase;
  if not Assigned(AContinuation) then
    Exit;

  ALoopState := AContinuation.GetForLoopState(ALoopStatement);
  if Assigned(ALoopState) then
    Result := ALoopState.Phase
  else
  begin
    ALoopState := AContinuation.EnsureForLoopState(ALoopStatement, nil);
    ALoopState.Phase := AInitialPhase;
  end;
end;

procedure ClearGeneratorLoopState(const AContinuation: TGocciaGeneratorContinuation;
  const ALoopStatement: TObject);
begin
  if Assigned(AContinuation) then
    AContinuation.ClearForLoopState(ALoopStatement);
end;

procedure MarkGeneratorLoopPhase(const ALoopState: TGocciaGeneratorForLoopState;
  const APhase: TGocciaGeneratorForLoopPhase;
  var AResumePhase: TGocciaGeneratorForLoopPhase);
begin
  if Assigned(ALoopState) then
    ALoopState.Phase := APhase;
  AResumePhase := APhase;
end;

// ES2026 §14.7.3.2 Runtime Semantics: WhileLoopEvaluation
function EvaluateWhile(const AWhileStatement: TGocciaWhileStatement;
  const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  ConditionValue: TGocciaValue;
  LoopValue: TGocciaValue;
  CF: TGocciaControlFlow;
  Continuation: TGocciaGeneratorContinuation;
  LoopState: TGocciaGeneratorForLoopState;
  ResumePhase: TGocciaGeneratorForLoopPhase;
begin
  LoopValue := UndefinedCompletionValue;
  Result := TGocciaControlFlow.Normal(LoopValue);

  ResumePhase := InitializeGeneratorLoopState(AWhileStatement, gflpTest,
    Continuation, LoopState);

  try
    while True do
    begin
      CheckExecutionTimeout;
      IncrementInstructionCounter;
      CheckInstructionLimit;

      if ResumePhase = gflpTest then
      begin
        MarkGeneratorLoopPhase(LoopState, gflpTest, ResumePhase);
        ConditionValue := EvaluateExpression(AWhileStatement.Condition, AContext);
        if not ConditionValue.ToBooleanLiteral.Value then
        begin
          ClearGeneratorLoopState(Continuation, AWhileStatement);
          Break;
        end;

        MarkGeneratorLoopPhase(LoopState, gflpBody, ResumePhase);
      end;

      if ResumePhase = gflpBody then
      begin
        CF := EvaluateLoopBodyStatement(AWhileStatement.Body, AContext);
        case CF.Kind of
          cfkBreak:
          begin
            if not TargetsStatementOrUnlabeled(CF, AWhileStatement) then
            begin
              ClearGeneratorLoopState(Continuation, AWhileStatement);
              Result := CF.UpdateEmpty(LoopValue);
              Exit;
            end;
            ClearGeneratorLoopState(Continuation, AWhileStatement);
            Result := NormalCompletionFromAbrupt(CF, LoopValue);
            LoopValue := Result.Value;
            Break;
          end;
          cfkReturn:
          begin
            ClearGeneratorLoopState(Continuation, AWhileStatement);
            Result := CF.UpdateEmpty(LoopValue);
            Exit;
          end;
          cfkContinue:
          begin
            if not TargetsStatementOrUnlabeled(CF, AWhileStatement) then
            begin
              ClearGeneratorLoopState(Continuation, AWhileStatement);
              Result := CF.UpdateEmpty(LoopValue);
              Exit;
            end;
            UpdateValueFromCompletion(CF, LoopValue);
            MarkGeneratorLoopPhase(LoopState, gflpTest, ResumePhase);
            Continue;
          end;
        end;

        UpdateValueFromCompletion(CF, LoopValue);
        MarkGeneratorLoopPhase(LoopState, gflpTest, ResumePhase);
      end;
    end;
  except
    on E: EGocciaGeneratorYield do
      raise;
    else
    begin
      ClearGeneratorLoopState(Continuation, AWhileStatement);
      raise;
    end;
  end;
  Result := TGocciaControlFlow.Normal(LoopValue);
end;

// ES2026 §14.7.2.2 Runtime Semantics: DoWhileLoopEvaluation
function EvaluateDoWhile(const ADoWhileStatement: TGocciaDoWhileStatement;
  const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  ConditionValue: TGocciaValue;
  LoopValue: TGocciaValue;
  CF: TGocciaControlFlow;
  Continuation: TGocciaGeneratorContinuation;
  LoopState: TGocciaGeneratorForLoopState;
  ResumePhase: TGocciaGeneratorForLoopPhase;
begin
  LoopValue := UndefinedCompletionValue;
  Result := TGocciaControlFlow.Normal(LoopValue);

  ResumePhase := InitializeGeneratorLoopState(ADoWhileStatement, gflpBody,
    Continuation, LoopState);

  try
    while True do
    begin
      CheckExecutionTimeout;
      IncrementInstructionCounter;
      CheckInstructionLimit;

      if ResumePhase = gflpBody then
      begin
        MarkGeneratorLoopPhase(LoopState, gflpBody, ResumePhase);
        CF := EvaluateLoopBodyStatement(ADoWhileStatement.Body, AContext);
        case CF.Kind of
          cfkBreak:
          begin
            if not TargetsStatementOrUnlabeled(CF, ADoWhileStatement) then
            begin
              ClearGeneratorLoopState(Continuation, ADoWhileStatement);
              Result := CF.UpdateEmpty(LoopValue);
              Exit;
            end;
            ClearGeneratorLoopState(Continuation, ADoWhileStatement);
            Result := NormalCompletionFromAbrupt(CF, LoopValue);
            LoopValue := Result.Value;
            Break;
          end;
          cfkReturn:
          begin
            ClearGeneratorLoopState(Continuation, ADoWhileStatement);
            Result := CF.UpdateEmpty(LoopValue);
            Exit;
          end;
          cfkContinue:
          begin
            if not TargetsStatementOrUnlabeled(CF, ADoWhileStatement) then
            begin
              ClearGeneratorLoopState(Continuation, ADoWhileStatement);
              Result := CF.UpdateEmpty(LoopValue);
              Exit;
            end;
          end;
        end;

        UpdateValueFromCompletion(CF, LoopValue);
        MarkGeneratorLoopPhase(LoopState, gflpTest, ResumePhase);
      end;

      if ResumePhase = gflpTest then
      begin
        MarkGeneratorLoopPhase(LoopState, gflpTest, ResumePhase);
        ConditionValue := EvaluateExpression(ADoWhileStatement.Condition, AContext);
        if not ConditionValue.ToBooleanLiteral.Value then
        begin
          ClearGeneratorLoopState(Continuation, ADoWhileStatement);
          Break;
        end;

        MarkGeneratorLoopPhase(LoopState, gflpBody, ResumePhase);
      end;
    end;
  except
    on E: EGocciaGeneratorYield do
      raise;
    else
    begin
      ClearGeneratorLoopState(Continuation, ADoWhileStatement);
      raise;
    end;
  end;
  Result := TGocciaControlFlow.Normal(LoopValue);
end;

// ES2026 §14.7.5.6 ForIn/OfBodyEvaluation — for-await-of variant
function EvaluateForAwaitOf(const AForAwaitOfStatement: TGocciaForAwaitOfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  IterableValue, IteratorMethod, IteratorMethodValue, IteratorObj, NextMethod, NextResult, DoneValue, CurrentValue: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  GenericNextResult: TGocciaObjectValue;
  CF: TGocciaControlFlow;
  IterScope: TGocciaScope;
  IterContext: TGocciaEvaluationContext;
  DeclarationType: TGocciaDeclarationType;
  EmptyArgs: TGocciaArgumentsCollection;
  MatchContext, MatchBaseContext: TGocciaEvaluationContext;
  Continuation: TGocciaGeneratorContinuation;
  SavedIteratorValue, SavedCurrentValue, SavedNextMethod: TGocciaValue;
  SavedIterScope, SavedActiveScope: TGocciaScope;
  HasSavedLoopState: Boolean;
  HeadCompleted, HeadYielding: Boolean;
  BodyYielding: Boolean;
  ShouldCloseIterator: Boolean;

  procedure CloseAsyncIterator(const AIter: TGocciaValue);
  var
    ReturnMethod, ReturnResult: TGocciaValue;
    CloseArgs: TGocciaArgumentsCollection;
  begin
    if not Assigned(AIter) or not (AIter is TGocciaObjectValue) then
      Exit;
    ReturnMethod := AIter.GetProperty(PROP_RETURN);
    if not Assigned(ReturnMethod) or
       (ReturnMethod is TGocciaUndefinedLiteralValue) or
       (ReturnMethod is TGocciaNullLiteralValue) then
      Exit;
    if not ReturnMethod.IsCallable then
      ThrowTypeError(SErrorIteratorReturnMustBeCallable,
        SSuggestIteratorProtocol);
    CloseArgs := TGocciaArgumentsCollection.Create;
    try
      ReturnResult := TGocciaFunctionBase(ReturnMethod).Call(CloseArgs, AIter);
    finally
      CloseArgs.Free;
    end;
    ReturnResult := AwaitValue(ReturnResult);
    if (ReturnResult is TGocciaUndefinedLiteralValue)
        or (ReturnResult is TGocciaNullLiteralValue)
        or ReturnResult.IsPrimitive then
      ThrowTypeError(SErrorIteratorReturnObject,
        SSuggestIteratorResultObject);
  end;

  procedure CloseAsyncIteratorPreservingError(const AIter: TGocciaValue);
  begin
    try
      CloseAsyncIterator(AIter);
    except
    end;
  end;

begin
  Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);

  Continuation := CurrentGeneratorContinuation;
  HasSavedLoopState := Assigned(Continuation) and
    Continuation.GetLoopState(AForAwaitOfStatement, SavedIteratorValue,
      SavedCurrentValue, SavedNextMethod, SavedIterScope, SavedActiveScope);
  if not HasSavedLoopState then
    IterableValue := EvaluateExpressionWithLoopHeadTDZ(
      AForAwaitOfStatement.Iterable,
      AContext,
      AForAwaitOfStatement.BindingName,
      AForAwaitOfStatement.BindingPattern,
      (not AForAwaitOfStatement.IsVar) and
      not Assigned(AForAwaitOfStatement.AssignmentTarget))
  else
    IterableValue := nil;

  if AForAwaitOfStatement.IsConst then
    DeclarationType := dtConst
  else
    DeclarationType := dtLet;

  IteratorMethod := nil;
  IteratorMethodValue := nil;
  if HasSavedLoopState and Assigned(SavedNextMethod) then
  begin
    IteratorObj := SavedIteratorValue;
    NextMethod := SavedNextMethod;
  end
  else if IterableValue is TGocciaObjectValue then
  begin
    IteratorMethodValue := TGocciaObjectValue(IterableValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownAsyncIterator);
    if Assigned(IteratorMethodValue) and
       not (IteratorMethodValue is TGocciaUndefinedLiteralValue) and
       not (IteratorMethodValue is TGocciaNullLiteralValue) then
    begin
      if not IteratorMethodValue.IsCallable then
        ThrowTypeError('Async iterator method is not callable');
      IteratorMethod := IteratorMethodValue;
    end;
  end;

  if (HasSavedLoopState and Assigned(SavedNextMethod)) or
     (Assigned(IteratorMethod) and IteratorMethod.IsCallable) then
  begin
    if not (HasSavedLoopState and Assigned(SavedNextMethod)) then
    begin
      EmptyArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(EmptyArgs, IterableValue);
      finally
        EmptyArgs.Free;
      end;
      if not (IteratorObj is TGocciaObjectValue) then
        ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
          SSuggestAsyncIteratorProtocol);
    end;

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(IteratorObj);
    try
      EmptyArgs := TGocciaArgumentsCollection.Create;
      try
        if not HasSavedLoopState then
        begin
          NextMethod := IteratorObj.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or not NextMethod.IsCallable then
            ThrowTypeError(SErrorAsyncIteratorNextNotCallable, SSuggestAsyncIteratorProtocol);
        end;

        while True do
        begin
          IterScope := nil;
          CheckExecutionTimeout;
          IncrementInstructionCounter;
          CheckInstructionLimit;
          if HasSavedLoopState then
          begin
            CurrentValue := SavedCurrentValue;
            IterScope := SavedIterScope;
            HasSavedLoopState := False;
          end
          else
          begin
            NextResult := TGocciaFunctionBase(NextMethod).Call(EmptyArgs, IteratorObj);
            NextResult := AwaitValue(NextResult);

            // ES2026 §7.4.2 step 5: If nextResult is not an Object, throw a TypeError
            if NextResult.IsPrimitive then
              ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
                SSuggestIteratorResultObject);

            DoneValue := NextResult.GetProperty(PROP_DONE);
            if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
              Break;

            CurrentValue := NextResult.GetProperty(PROP_VALUE);
            if not Assigned(CurrentValue) then
              CurrentValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          end;
          if Assigned(Continuation) and not Assigned(IterScope) then
            Continuation.SaveLoopState(AForAwaitOfStatement, IteratorObj,
              CurrentValue, NextMethod);

          if Assigned(IterScope) then
          begin
            IterContext := AContext;
            if Assigned(SavedActiveScope) then
              IterContext.Scope := SavedActiveScope
            else
              IterContext.Scope := IterScope;
            MatchBaseContext := AContext;
            MatchBaseContext.Scope := IterScope;
          end
          else
          begin
            HeadCompleted := False;
            HeadYielding := False;
            ShouldCloseIterator := True;
            try
              try
                IterScope := AContext.Scope.CreateChild(skBlock);
                IterContext := AContext;
                IterContext.Scope := IterScope;

                if Assigned(AForAwaitOfStatement.AssignmentTarget) then
                  AssignPattern(AForAwaitOfStatement.AssignmentTarget,
                    CurrentValue, IterContext)
                else if AForAwaitOfStatement.IsVar then
                begin
                  if AForAwaitOfStatement.BindingPattern <> nil then
                    AssignPattern(AForAwaitOfStatement.BindingPattern,
                      CurrentValue, IterContext)
                  else
                    AContext.Scope.DefineVariableBinding(AForAwaitOfStatement.BindingName, CurrentValue, True);
                end
                else if AForAwaitOfStatement.BindingPattern <> nil then
                  AssignPattern(AForAwaitOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
                else
                  IterScope.DefineLexicalBinding(AForAwaitOfStatement.BindingName, CurrentValue, DeclarationType);

                if Assigned(AForAwaitOfStatement.MatchPattern) then
                begin
                  MatchBaseContext := IterContext;
                  if not TryEvaluateMatchPatternInContext(CurrentValue,
                     AForAwaitOfStatement.MatchPattern, IterContext, MatchContext) then
                  begin
                    if Assigned(Continuation) then
                      Continuation.ClearLoopState(AForAwaitOfStatement);
                    ShouldCloseIterator := False;
                    Continue;
                  end;
                  IterContext := MatchContext;
                end;
                HeadCompleted := True;
                if Assigned(Continuation) then
                  Continuation.SaveLoopState(AForAwaitOfStatement, IteratorObj,
                    CurrentValue, NextMethod, IterScope, IterContext.Scope);
              except
                on E: EGocciaGeneratorYield do
                begin
                  HeadYielding := True;
                  raise;
                end;
                on E: Exception do
                begin
                  if ShouldCloseIterator then
                    CloseAsyncIteratorPreservingError(IteratorObj);
                  raise;
                end;
              end;
            finally
              if (not HeadCompleted) and (not HeadYielding) and
                 Assigned(Continuation) then
              begin
                Continuation.ClearLoopState(AForAwaitOfStatement);
                Continuation.ClearExpressionValues;
              end;
            end;
          end;

          try
            BodyYielding := False;
            try
              CF := EvaluateLoopBodyStatement(AForAwaitOfStatement.Body, IterContext);
              if Assigned(Continuation) then
                Continuation.ClearLoopState(AForAwaitOfStatement);
            except
              on E: EGocciaGeneratorYield do
              begin
                BodyYielding := True;
                raise;
              end;
              else
              begin
                if Assigned(Continuation) then
                  Continuation.ClearLoopState(AForAwaitOfStatement);
                CloseAsyncIteratorPreservingError(IteratorObj);
                raise;
              end;
            end;
          finally
            if (not BodyYielding) and Assigned(AForAwaitOfStatement.MatchPattern) and
               (IterContext.Scope <> IterScope) then
              ReleaseMatchContext(IterContext, MatchBaseContext);
          end;
          if CF.Kind = cfkBreak then
          begin
            if not TargetsStatementOrUnlabeled(CF, AForAwaitOfStatement) then
            begin
              if Assigned(Continuation) then
                Continuation.ClearLoopState(AForAwaitOfStatement);
              CloseAsyncIterator(IteratorObj);
              Result := CF;
              Exit;
            end;
            if Assigned(Continuation) then
              Continuation.ClearLoopState(AForAwaitOfStatement);
            CloseAsyncIterator(IteratorObj);
            Break;
          end;
          if CF.Kind = cfkReturn then
          begin
            if Assigned(Continuation) then
              Continuation.ClearLoopState(AForAwaitOfStatement);
            CloseAsyncIterator(IteratorObj);
            Result := CF;
            Exit;
          end;
          if (CF.Kind = cfkContinue) and
             not TargetsStatementOrUnlabeled(CF, AForAwaitOfStatement) then
          begin
            if Assigned(Continuation) then
              Continuation.ClearLoopState(AForAwaitOfStatement);
            CloseAsyncIterator(IteratorObj);
            Result := CF;
            Exit;
          end;
        end;
        if Assigned(Continuation) then
          Continuation.ClearLoopState(AForAwaitOfStatement);
      finally
        EmptyArgs.Free;
      end;
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(IteratorObj);
    end;
  end
  else
  begin
    if HasSavedLoopState then
      Iterator := TGocciaIteratorValue(SavedIteratorValue)
    else
    begin
      Iterator := GetIteratorFromValue(IterableValue);
      if Iterator = nil then
        ThrowTypeError(
          Format(SErrorNotIterable, [IterableValue.TypeName]),
          SSuggestIteratorProtocol);
    end;

    if Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(Iterator);
    try
      if not HasSavedLoopState then
        GenericNextResult := Iterator.AdvanceNext;
      while True do
      begin
        IterScope := nil;
        CheckExecutionTimeout;
        IncrementInstructionCounter;
        CheckInstructionLimit;
        if HasSavedLoopState then
        begin
          CurrentValue := SavedCurrentValue;
          IterScope := SavedIterScope;
          HasSavedLoopState := False;
        end
        else
        begin
          if GenericNextResult.GetProperty(PROP_DONE).ToBooleanLiteral.Value then
            Break;
          CurrentValue := GenericNextResult.GetProperty(PROP_VALUE);
          CurrentValue := AwaitValue(CurrentValue);
        end;
        if Assigned(Continuation) and not Assigned(IterScope) then
          Continuation.SaveLoopState(AForAwaitOfStatement, Iterator,
            CurrentValue);

        if Assigned(IterScope) then
        begin
          IterContext := AContext;
          if Assigned(SavedActiveScope) then
            IterContext.Scope := SavedActiveScope
          else
            IterContext.Scope := IterScope;
          MatchBaseContext := AContext;
          MatchBaseContext.Scope := IterScope;
        end
        else
        begin
          HeadCompleted := False;
          HeadYielding := False;
          ShouldCloseIterator := True;
          try
            try
              IterScope := AContext.Scope.CreateChild(skBlock);
              IterContext := AContext;
              IterContext.Scope := IterScope;

              if Assigned(AForAwaitOfStatement.AssignmentTarget) then
                AssignPattern(AForAwaitOfStatement.AssignmentTarget,
                  CurrentValue, IterContext)
              else if AForAwaitOfStatement.IsVar then
              begin
                if AForAwaitOfStatement.BindingPattern <> nil then
                  AssignPattern(AForAwaitOfStatement.BindingPattern, CurrentValue,
                    IterContext)
                else
                  AContext.Scope.DefineVariableBinding(AForAwaitOfStatement.BindingName, CurrentValue, True);
              end
              else if AForAwaitOfStatement.BindingPattern <> nil then
                AssignPattern(AForAwaitOfStatement.BindingPattern, CurrentValue, IterContext, True, DeclarationType)
              else
                IterScope.DefineLexicalBinding(AForAwaitOfStatement.BindingName, CurrentValue, DeclarationType);

              if Assigned(AForAwaitOfStatement.MatchPattern) then
              begin
                MatchBaseContext := IterContext;
                if not TryEvaluateMatchPatternInContext(CurrentValue,
                   AForAwaitOfStatement.MatchPattern, IterContext, MatchContext) then
                begin
                  if Assigned(Continuation) then
                    Continuation.ClearLoopState(AForAwaitOfStatement);
                  ShouldCloseIterator := False;
                  GenericNextResult := Iterator.AdvanceNext;
                  Continue;
                end;
                IterContext := MatchContext;
              end;
              HeadCompleted := True;
              if Assigned(Continuation) then
                Continuation.SaveLoopState(AForAwaitOfStatement, Iterator,
                  CurrentValue, nil, IterScope, IterContext.Scope);
            except
              on E: EGocciaGeneratorYield do
              begin
                HeadYielding := True;
                raise;
              end;
              on E: Exception do
              begin
                if ShouldCloseIterator then
                  Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
                raise;
              end;
            end;
          finally
            if (not HeadCompleted) and (not HeadYielding) and
               Assigned(Continuation) then
            begin
              Continuation.ClearLoopState(AForAwaitOfStatement);
              Continuation.ClearExpressionValues;
            end;
          end;
        end;

        try
          BodyYielding := False;
          try
            CF := EvaluateLoopBodyStatement(AForAwaitOfStatement.Body, IterContext);
            if Assigned(Continuation) then
              Continuation.ClearLoopState(AForAwaitOfStatement);
          except
            on E: EGocciaGeneratorYield do
            begin
              BodyYielding := True;
              raise;
            end;
            else
            begin
              if Assigned(Continuation) then
                Continuation.ClearLoopState(AForAwaitOfStatement);
              Goccia.Values.IteratorSupport.CloseIteratorPreservingError(Iterator);
              raise;
            end;
          end;
        finally
          if (not BodyYielding) and Assigned(AForAwaitOfStatement.MatchPattern) and
             (IterContext.Scope <> IterScope) then
            ReleaseMatchContext(IterContext, MatchBaseContext);
        end;
        if CF.Kind = cfkBreak then
        begin
          if not TargetsStatementOrUnlabeled(CF, AForAwaitOfStatement) then
          begin
            if Assigned(Continuation) then
              Continuation.ClearLoopState(AForAwaitOfStatement);
            Iterator.Close;
            Result := CF;
            Exit;
          end;
          if Assigned(Continuation) then
            Continuation.ClearLoopState(AForAwaitOfStatement);
          Iterator.Close;
          Break;
        end;
        if CF.Kind = cfkReturn then
        begin
          if Assigned(Continuation) then
            Continuation.ClearLoopState(AForAwaitOfStatement);
          Iterator.Close;
          Result := CF;
          Exit;
        end;
        if (CF.Kind = cfkContinue) and
           not TargetsStatementOrUnlabeled(CF, AForAwaitOfStatement) then
        begin
          if Assigned(Continuation) then
            Continuation.ClearLoopState(AForAwaitOfStatement);
          Iterator.Close;
          Result := CF;
          Exit;
        end;

        GenericNextResult := Iterator.AdvanceNext;
      end;
      if Assigned(Continuation) then
        Continuation.ClearLoopState(AForAwaitOfStatement);
    finally
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(Iterator);
    end;
  end;
end;

function EvaluateArrowFunction(const AArrowFunctionExpression: TGocciaArrowFunctionExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  if AArrowFunctionExpression.Body is TGocciaBlockStatement then
    Statements := CopyStatementList(TGocciaBlockStatement(AArrowFunctionExpression.Body).Nodes)
  else
  begin
    // Body is a single expression: (n) => n * 2
    Statements := TObjectList<TGocciaASTNode>.Create(False);
    Statements.Add(AArrowFunctionExpression.Body);
  end;

  if AArrowFunctionExpression.IsAsync then
    Result := TGocciaAsyncArrowFunctionValue.Create(AArrowFunctionExpression.Parameters, Statements, AContext.Scope.CreateChild)
  else
    Result := TGocciaArrowFunctionValue.Create(AArrowFunctionExpression.Parameters, Statements, AContext.Scope.CreateChild);
  ApplyFunctionObjectPrototype(Result,
    FunctionIntrinsicKind(AArrowFunctionExpression.IsAsync, False));
  TGocciaFunctionValue(Result).StrictCode :=
    (not AContext.NonStrictMode) or
    HasUseStrictDirective(AArrowFunctionExpression.Body);
  TGocciaFunctionValue(Result).IsExpressionBody := not (AArrowFunctionExpression.Body is TGocciaBlockStatement);
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AArrowFunctionExpression.Line;
  TGocciaFunctionValue(Result).SourceText := AArrowFunctionExpression.SourceText;
end;

function EvaluateFunctionExpression(const AFunctionExpression: TGocciaFunctionExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
  ClosureScope: TGocciaScope;
  NameScope: TGocciaFunctionNameScope;
  HasStrictDirective: Boolean;
begin
  NameScope := nil;
  HasStrictDirective := AFunctionExpression.ParsedInStrictMode or
    HasUseStrictDirective(AFunctionExpression.Body);
  if (not AContext.NonStrictMode) or HasStrictDirective then
    ValidateStrictFunctionExpression(AFunctionExpression);
  if AFunctionExpression.Body is TGocciaBlockStatement then
    Statements := CopyStatementList(TGocciaBlockStatement(AFunctionExpression.Body).Nodes)
  else
  begin
    Statements := TObjectList<TGocciaASTNode>.Create(False);
    Statements.Add(AFunctionExpression.Body);
  end;

  // ES2026 §15.2.5: Named function expressions get an intermediate scope
  // with a read-only binding of the function name visible inside the body
  if AFunctionExpression.Name <> '' then
  begin
    NameScope := TGocciaFunctionNameScope.Create(AContext.Scope,
      AFunctionExpression.Name);
    ClosureScope := NameScope.CreateChild;
  end
  else
    ClosureScope := AContext.Scope.CreateChild;

  if AFunctionExpression.IsGenerator and AFunctionExpression.IsAsync then
    Result := TGocciaAsyncGeneratorFunctionValue.Create(AFunctionExpression.Parameters, Statements, ClosureScope)
  else if AFunctionExpression.IsGenerator then
    Result := TGocciaGeneratorFunctionValue.Create(AFunctionExpression.Parameters, Statements, ClosureScope)
  else if AFunctionExpression.IsAsync then
    Result := TGocciaAsyncFunctionValue.Create(AFunctionExpression.Parameters, Statements, ClosureScope)
  else
    Result := TGocciaFunctionValue.Create(AFunctionExpression.Parameters, Statements, ClosureScope);
  ApplyFunctionObjectPrototype(Result,
    FunctionIntrinsicKind(AFunctionExpression.IsAsync,
      AFunctionExpression.IsGenerator));
  TGocciaFunctionValue(Result).Name := AFunctionExpression.Name;
  if AContext.NonStrictMode and not HasStrictDirective then
  begin
    TGocciaFunctionValue(Result).StrictThis := False;
    TGocciaFunctionValue(Result).StrictCode := False;
    if AFunctionExpression.HasOwnPrototype and
       (not AFunctionExpression.IsGenerator) and
       (not AFunctionExpression.IsAsync) then
      TGocciaFunctionBase(Result).InstallSloppyFunctionCallerArgumentsProperties;
  end
  else
    TGocciaFunctionValue(Result).StrictCode := True;
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AFunctionExpression.Line;
  TGocciaFunctionValue(Result).SourceText := AFunctionExpression.SourceText;

  // ES2026 §10.2.5 MakeConstructor: function declarations / expressions and
  // (async) generator declarations / expressions get their own `prototype`
  // data property.  Concise methods, arrow functions, getters, setters, and
  // plain async functions do not.
  //
  // The shape differs by kind:
  //   - Ordinary function (§15.2): prototype is { writable, !enumerable,
  //     !configurable } and has an own `constructor` data property pointing
  //     back at the function.
  //   - (Async) generator (§15.5 / §15.6): prototype is also writable, but
  //     has NO own `constructor`.  Per spec it inherits `constructor` from
  //     %GeneratorFunction.prototype.prototype% (which itself points at
  //     %GeneratorFunction.prototype%, not the specific generator), so an own
  //     back-reference here would be wrong.
  if AFunctionExpression.HasOwnPrototype then
    InstallFunctionOwnPrototypeProperty(Result,
      FunctionIntrinsicKind(AFunctionExpression.IsAsync,
        AFunctionExpression.IsGenerator));

  // Bind the function name in the intermediate scope (parent of the closure)
  if Assigned(NameScope) then
    NameScope.DefineFunctionNameBinding(Result);
end;

// TC39 Explicit Resource Management §3.6 DisposeResources — sync disposal
function DisposeTrackedResources(const ATracker: TGocciaDisposalTracker;
  const AExistingError: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
begin
  CurrentError := AExistingError;
  HasError := Assigned(AExistingError);

  // Dispose in reverse order (LIFO)
  for I := ATracker.Count - 1 downto 0 do
  begin
    Resource := ATracker.GetResource(I);
    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        TGocciaFunctionBase(Resource.DisposeMethod).CallNoArgs(Resource.ResourceValue);
      except
        on E: TGocciaThrowValue do
        begin
          if HasError then
            CurrentError := CreateSuppressedErrorObject(E.Value, CurrentError)
          else
            CurrentError := E.Value;
          HasError := True;
        end;
      end;
    end;
  end;

  if HasError then
    Result := CurrentError
  else
    Result := nil;
end;

// TC39 Explicit Resource Management §3.6 DisposeResources — async disposal
function DisposeTrackedResourcesAsync(const ATracker: TGocciaDisposalTracker;
  const AExistingError: TGocciaValue): TGocciaValue;
var
  I: Integer;
  Resource: TGocciaDisposableResource;
  CurrentError: TGocciaValue;
  HasError: Boolean;
  CallResult: TGocciaValue;
begin
  CurrentError := AExistingError;
  HasError := Assigned(AExistingError);

  // Dispose in reverse order (LIFO)
  for I := ATracker.Count - 1 downto 0 do
  begin
    Resource := ATracker.GetResource(I);
    if Assigned(Resource.DisposeMethod) and Resource.DisposeMethod.IsCallable then
    begin
      try
        CallResult := TGocciaFunctionBase(Resource.DisposeMethod)
          .CallNoArgs(Resource.ResourceValue);
        // For async disposal, await the result
        if Resource.Hint = dhAsyncDispose then
        begin
          if Assigned(CallResult) then
            AwaitValue(CallResult);
        end;
      except
        on E: TGocciaThrowValue do
        begin
          if HasError then
            CurrentError := CreateSuppressedErrorObject(E.Value, CurrentError)
          else
            CurrentError := E.Value;
          HasError := True;
        end;
      end;
    end
    else if Resource.Hint = dhAsyncDispose then
    begin
      // Null/undefined value in await using — ensure at least one await point
      AwaitValue(TGocciaUndefinedLiteralValue.UndefinedValue);
    end;
  end;

  if HasError then
    Result := CurrentError
  else
    Result := nil;
end;

function HasAsyncDisposals(const ATracker: TGocciaDisposalTracker): Boolean;
var
  I: Integer;
begin
  for I := 0 to ATracker.Count - 1 do
    if ATracker.GetResource(I).Hint = dhAsyncDispose then
      Exit(True);
  Result := False;
end;

function EvaluateBlock(const ABlockStatement: TGocciaBlockStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  I: Integer;
  BlockContext: TGocciaEvaluationContext;
  NeedsChildScope: Boolean;
  HasUsingDeclarations: Boolean;
  Tracker: TGocciaDisposalTracker;
  DisposalError: TGocciaValue;
  CaughtError: TGocciaValue;
  HasCaughtError: Boolean;
  GC: TGarbageCollector;
begin
  NeedsChildScope := False;
  HasUsingDeclarations := False;
  for I := 0 to ABlockStatement.Nodes.Count - 1 do
  begin
    if (ABlockStatement.Nodes[I] is TGocciaVariableDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaFunctionDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaExportFunctionDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaDestructuringDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaClassDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaEnumDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaExportEnumDeclaration) or
       (ABlockStatement.Nodes[I] is TGocciaUsingDeclaration) then
    begin
      NeedsChildScope := True;
      if ABlockStatement.Nodes[I] is TGocciaUsingDeclaration then
        HasUsingDeclarations := True;
    end;
  end;

  // Fast path: no using declarations — original block evaluation
  if not HasUsingDeclarations then
  begin
    BlockContext := AContext;
    if NeedsChildScope then
      BlockContext.Scope := AContext.Scope.CreateChild(skBlock, 'BlockScope')
    else
      BlockContext.Scope := AContext.Scope;
    GC := TGarbageCollector.Instance;
    if NeedsChildScope and Assigned(GC) then
      GC.AddTempRoot(BlockContext.Scope);
    try
      if NeedsChildScope then
      PredeclareBlockLexicalBindings(ABlockStatement.Nodes, BlockContext);
      HoistFunctionDeclarations(ABlockStatement.Nodes, BlockContext,
        NeedsChildScope);
      Result := EvaluateStatements(ABlockStatement.Nodes, BlockContext);
    finally
      if NeedsChildScope and Assigned(GC) then
        GC.RemoveTempRoot(BlockContext.Scope);
    end;
    Exit;
  end;

  // Slow path: block has using declarations — need disposal at exit
  BlockContext := AContext;
  BlockContext.Scope := AContext.Scope.CreateChild(skBlock, 'BlockScope');

  Tracker := TGocciaDisposalTracker.Create;
  BlockContext.DisposalTracker := Tracker;

  CaughtError := nil;
  HasCaughtError := False;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.AddTempRoot(BlockContext.Scope);
  try
    try
      PredeclareBlockLexicalBindings(ABlockStatement.Nodes, BlockContext);
      HoistFunctionDeclarations(ABlockStatement.Nodes, BlockContext, True);
      Result := EvaluateStatements(ABlockStatement.Nodes, BlockContext);
    except
      on E: TGocciaThrowValue do
      begin
        CaughtError := E.Value;
        HasCaughtError := True;
        // Protect the caught error value from GC during disposal
        if Assigned(GC) and Assigned(CaughtError) then
          GC.AddTempRoot(CaughtError);
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end;
  finally
    // TC39 Explicit Resource Management: dispose resources at block exit
    // Route through async path when tracker contains await using entries
    if HasCaughtError then
    begin
      if HasAsyncDisposals(Tracker) then
        DisposalError := DisposeTrackedResourcesAsync(Tracker, CaughtError)
      else
        DisposalError := DisposeTrackedResources(Tracker, CaughtError);
    end
    else
    begin
      if HasAsyncDisposals(Tracker) then
        DisposalError := DisposeTrackedResourcesAsync(Tracker, nil)
      else
        DisposalError := DisposeTrackedResources(Tracker, nil);
    end;
    Tracker.Free;
    BlockContext.DisposalTracker := nil;

    if Assigned(GC) then
      GC.RemoveTempRoot(BlockContext.Scope);

    // Remove GC temp root before re-raising
    if HasCaughtError and Assigned(GC) and Assigned(CaughtError) then
      GC.RemoveTempRoot(CaughtError);

    // If disposal produced an error, throw it (may be SuppressedError)
    if Assigned(DisposalError) then
      raise TGocciaThrowValue.Create(DisposalError)
    else if HasCaughtError then
      raise TGocciaThrowValue.Create(CaughtError);
  end;
end;

// TC39 Explicit Resource Management: using / await using declaration evaluation
function EvaluateUsingDeclaration(const AUsingDeclaration: TGocciaUsingDeclaration; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  I: Integer;
  Value: TGocciaValue;
  DisposeMethod: TGocciaValue;
  Tracker: TGocciaDisposalTracker;
  Hint: TGocciaDisposalHint;
begin
  Result := TGocciaControlFlow.Empty;

  // The disposal tracker should have been set by EvaluateBlock
  if (AContext.DisposalTracker = nil) or
     not (AContext.DisposalTracker is TGocciaDisposalTracker) then
    ThrowTypeError(SErrorUsingOutsideBlock, SSuggestUsingInsideBlock);
  Tracker := TGocciaDisposalTracker(AContext.DisposalTracker);

  if AUsingDeclaration.IsAwait then
    Hint := dhAsyncDispose
  else
    Hint := dhSyncDispose;

  for I := 0 to Length(AUsingDeclaration.Variables) - 1 do
  begin
    // TC39 Explicit Resource Management §3.4 CreateDisposableResource
    Value := EvaluateExpression(AUsingDeclaration.Variables[I].Initializer, AContext);
    if (Value is TGocciaFunctionValue) and
       (TGocciaFunctionValue(Value).Name = '') then
      TGocciaFunctionValue(Value).SetInferredName(
        AUsingDeclaration.Variables[I].Name)
    else if Value is TGocciaClassValue then
      TGocciaClassValue(Value).SetInferredName(
        AUsingDeclaration.Variables[I].Name);

    // null and undefined are silently skipped (no error, no disposal)
    if (Value is TGocciaUndefinedLiteralValue) or (Value is TGocciaNullLiteralValue) then
    begin
      // Bind the variable but don't track for disposal
      AContext.Scope.DefineLexicalBinding(AUsingDeclaration.Variables[I].Name, Value, dtConst);

      // For await using with null/undefined, still record to ensure an await point
      if Hint = dhAsyncDispose then
        Tracker.AddResource(nil, nil, dhAsyncDispose);
    end
    else
    begin
      // Value must be an object with the appropriate @@dispose/@@asyncDispose method
      DisposeMethod := GetDisposeMethod(Value, Hint);
      if not Assigned(DisposeMethod) then
      begin
        if Hint = dhAsyncDispose then
          ThrowTypeError(SErrorNotAsyncDisposable, SSuggestDisposable)
        else
          ThrowTypeError(SErrorNotDisposable, SSuggestDisposable);
      end;

      // Bind the variable as const (using bindings are not reassignable)
      AContext.Scope.DefineLexicalBinding(AUsingDeclaration.Variables[I].Name, Value, dtConst);

      // Track for disposal at block exit
      Tracker.AddResource(Value, DisposeMethod, Hint);
    end;
  end;
end;

function EvaluateIf(const AIfStatement: TGocciaIfStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  ConditionResult: Boolean;
  BodyContext: TGocciaEvaluationContext;
  PatternHandled: Boolean;
  procedure ActivateCompatFunctionDeclaration(
    const AStatement: TGocciaStatement;
    const ABodyContext: TGocciaEvaluationContext);
  var
    FuncDecl: TGocciaFunctionDeclaration;
  begin
    if not ABodyContext.NonStrictMode then
      Exit;
    if AStatement is TGocciaFunctionDeclaration then
      FuncDecl := TGocciaFunctionDeclaration(AStatement)
    else if AStatement is TGocciaExportFunctionDeclaration then
      FuncDecl := TGocciaExportFunctionDeclaration(AStatement).Declaration
    else
      Exit;
    if FuncDecl.FunctionExpression.IsAsync or FuncDecl.FunctionExpression.IsGenerator then
      Exit;
    HoistSingleFunctionDeclaration(AStatement, ABodyContext, False);
  end;
begin
  ConditionResult := EvaluateConditionWithPatternBindings(AIfStatement.Condition,
    AContext, BodyContext, PatternHandled);
  if not PatternHandled then
    BodyContext := AContext;
  if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
  begin
    if ConditionResult then
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, AIfStatement.Line, AIfStatement.Column, 0)
    else
      TGocciaCoverageTracker.Instance.RecordBranchHit(
        AContext.CurrentFilePath, AIfStatement.Line, AIfStatement.Column, 1);
  end;
  if ConditionResult then
  begin
    try
      ActivateCompatFunctionDeclaration(AIfStatement.Consequent, BodyContext);
      Result := EvaluateStatement(AIfStatement.Consequent, BodyContext);
      Result := Result.UpdateEmpty(UndefinedCompletionValue);
    finally
      if PatternHandled then
        ReleaseMatchContext(BodyContext, AContext);
    end;
  end
  else if Assigned(AIfStatement.Alternate) then
  begin
    ActivateCompatFunctionDeclaration(AIfStatement.Alternate, AContext);
    Result := EvaluateStatement(AIfStatement.Alternate, AContext);
    Result := Result.UpdateEmpty(UndefinedCompletionValue);
  end
  else
    Result := TGocciaControlFlow.Normal(UndefinedCompletionValue);
end;

function ExecuteCatchBlock(const ATryStatement: TGocciaTryStatement; const AErrorValue: TGocciaValue; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  CatchScope: TGocciaScope;
  CatchContext, MatchContext: TGocciaEvaluationContext;
  PatternNames: TStringList;
  I: Integer;
begin
  if ATryStatement.CatchParam <> '' then
  begin
    CatchScope := TGocciaCatchScope.Create(AContext.Scope, ATryStatement.CatchParam);
    try
      CatchScope.DefineLexicalBinding(ATryStatement.CatchParam, AErrorValue, dtParameter);
      CatchContext := AContext;
      CatchContext.Scope := CatchScope;
      if Assigned(ATryStatement.CatchPattern) then
      begin
        if not TryEvaluateMatchPatternInContext(AErrorValue,
           ATryStatement.CatchPattern, CatchContext, MatchContext) then
          raise TGocciaThrowValue.Create(AErrorValue);
        try
          Result := EvaluateBlock(ATryStatement.CatchBlock, MatchContext);
        finally
          ReleaseMatchContext(MatchContext, CatchContext);
        end;
      end
      else
        Result := EvaluateBlock(ATryStatement.CatchBlock, CatchContext);
    finally
      CatchScope.Free;
    end;
  end
  else if Assigned(ATryStatement.CatchBindingPattern) then
  begin
    CatchScope := AContext.Scope.CreateChild(skBlock, 'CatchBlock');
    try
      CatchContext := AContext;
      CatchContext.Scope := CatchScope;
      PatternNames := TStringList.Create;
      PatternNames.CaseSensitive := True;
      try
        CollectPatternBindingNames(ATryStatement.CatchBindingPattern,
          PatternNames, True);
        for I := 0 to PatternNames.Count - 1 do
          if not CatchScope.ContainsOwnLexicalBinding(PatternNames[I]) then
            CatchScope.PredeclareLexicalBinding(PatternNames[I], dtLet,
              ATryStatement.Line, ATryStatement.Column);
      finally
        PatternNames.Free;
      end;
      AssignPattern(ATryStatement.CatchBindingPattern, AErrorValue, CatchContext,
        True, dtParameter);
      Result := EvaluateBlock(ATryStatement.CatchBlock, CatchContext);
    finally
      CatchScope.Free;
    end;
  end
  else
    Result := EvaluateBlock(ATryStatement.CatchBlock, AContext);
end;

function PascalExceptionToErrorObject(const E: Exception): TGocciaValue;
begin
  if E is TGocciaTypeError then
    Result := CreateErrorObject(TYPE_ERROR_NAME, E.Message)
  else if E is TGocciaReferenceError then
    Result := CreateErrorObject(REFERENCE_ERROR_NAME, E.Message)
  else if E is TGocciaSyntaxError then
    Result := CreateErrorObject(SYNTAX_ERROR_NAME, E.Message)
  else if E is TGocciaRuntimeError then
    Result := CreateErrorObject(ERROR_NAME, E.Message)
  else if E is EStackOverflow then
    Result := CreateErrorObject(RANGE_ERROR_NAME, SErrorMaxCallStackExceeded)
  else
    Result := CreateErrorObject(ERROR_NAME, E.Message);
end;

function EvaluateTry(const ATryStatement: TGocciaTryStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  CatchValue: TGocciaValue;
  Continuation: TGocciaGeneratorContinuation;
  Phase: TGocciaGeneratorTryPhase;
  TryState: TGocciaGeneratorTryState;
  ThrownValue: TGocciaValue;
  HasUnhandledThrow: Boolean;
  HasGeneratorReturn: Boolean;
  GeneratorReturnValue: TGocciaValue;
  FinallyCF: TGocciaControlFlow;

  procedure SaveTryState(const APhase: TGocciaGeneratorTryPhase);
  begin
    if not Assigned(Continuation) then
      Exit;
    TryState := Continuation.EnsureTryState(ATryStatement);
    TryState.Phase := APhase;
    TryState.ResultFlow := Result;
    TryState.CatchValue := CatchValue;
    TryState.ThrownValue := ThrownValue;
    TryState.GeneratorReturnValue := GeneratorReturnValue;
    TryState.HasUnhandledThrow := HasUnhandledThrow;
    TryState.HasGeneratorReturn := HasGeneratorReturn;
  end;

  procedure ClearTryState;
  begin
    if Assigned(Continuation) then
      Continuation.ClearTryState(ATryStatement);
  end;

  procedure ExecuteCatchWithState(const AErrorValue: TGocciaValue);
  begin
    CatchValue := AErrorValue;
    SaveTryState(gtpCatch);
    try
      Result := ExecuteCatchBlock(ATryStatement, AErrorValue, AContext);
    except
      on E: EGocciaGeneratorYield do
        raise;
      on E: EGocciaGeneratorReturn do
      begin
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
        HasGeneratorReturn := True;
        GeneratorReturnValue := E.Value;
      end;
      on E: TGocciaThrowValue do
      begin
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
        HasUnhandledThrow := True;
        ThrownValue := E.Value;
      end;
      on E: TGocciaTimeoutError do
        raise;
      on E: TGocciaInstructionLimitError do
        raise;
      on E: Exception do
      begin
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
        HasUnhandledThrow := True;
        ThrownValue := PascalExceptionToErrorObject(E);
      end;
    end;
    CatchValue := nil;
  end;
begin
  Continuation := CurrentGeneratorContinuation;
  TryState := nil;
  if Assigned(Continuation) then
    TryState := Continuation.GetTryState(ATryStatement);
  if Assigned(TryState) then
  begin
    Phase := TryState.Phase;
    Result := TryState.ResultFlow;
    CatchValue := TryState.CatchValue;
    ThrownValue := TryState.ThrownValue;
    GeneratorReturnValue := TryState.GeneratorReturnValue;
    HasUnhandledThrow := TryState.HasUnhandledThrow;
    HasGeneratorReturn := TryState.HasGeneratorReturn;
  end
  else
  begin
    Phase := gtpTry;
    CatchValue := nil;
    HasUnhandledThrow := False;
    HasGeneratorReturn := False;
    ThrownValue := nil;
    GeneratorReturnValue := nil;
    Result := TGocciaControlFlow.Empty;
  end;

  // Phase 1: Execute try block, capturing throws
  if Phase = gtpTry then
  begin
    try
      Result := EvaluateBlock(ATryStatement.Block, AContext);
    except
      on E: EGocciaGeneratorYield do
        raise;
      on E: EGocciaGeneratorReturn do
      begin
        Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
        HasGeneratorReturn := True;
        GeneratorReturnValue := E.Value;
      end;
      on E: TGocciaThrowValue do
      begin
        if Assigned(ATryStatement.CatchBlock) then
          ExecuteCatchWithState(E.Value)
        else
        begin
          Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
          HasUnhandledThrow := True;
          ThrownValue := E.Value;
        end;
      end;
      on E: TGocciaTimeoutError do
        raise;
      on E: TGocciaInstructionLimitError do
        raise;
      on E: Exception do
      begin
        if Assigned(ATryStatement.CatchBlock) then
          ExecuteCatchWithState(PascalExceptionToErrorObject(E))
        else
        begin
          Result := TGocciaControlFlow.Normal(TGocciaUndefinedLiteralValue.UndefinedValue);
          HasUnhandledThrow := True;
          ThrownValue := PascalExceptionToErrorObject(E);
        end;
      end;
    end;
  end;

  if Phase = gtpCatch then
    ExecuteCatchWithState(CatchValue);

  // Phase 2: Execute finally block (always runs)
  if Assigned(ATryStatement.FinallyBlock) then
  begin
    SaveTryState(gtpFinally);
    if HasUnhandledThrow and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(ThrownValue);
    if HasGeneratorReturn and Assigned(TGarbageCollector.Instance) then
      TGarbageCollector.Instance.AddTempRoot(GeneratorReturnValue);
    try
      try
        FinallyCF := EvaluateBlock(ATryStatement.FinallyBlock, AContext);
        // Per JS semantics: finally's control flow overrides try/catch result AND pending throw
        if FinallyCF.Kind <> cfkNormal then
        begin
          ClearTryState;
          Result := FinallyCF.UpdateEmpty(UndefinedCompletionValue);
          Exit;
        end;
        // If finally throws (TGocciaThrowValue), it propagates naturally and overrides everything
      except
        on E: EGocciaGeneratorYield do
          raise;
        on E: Exception do
        begin
          ClearTryState;
          raise;
        end;
      end;
    finally
      if HasUnhandledThrow and Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(ThrownValue);
      if HasGeneratorReturn and Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.RemoveTempRoot(GeneratorReturnValue);
    end;
  end;

  ClearTryState;
  if HasGeneratorReturn then
    raise EGocciaGeneratorReturn.Create(GeneratorReturnValue);

  // Phase 3: Re-raise unhandled throw (if not overridden by finally)
  if HasUnhandledThrow then
    raise TGocciaThrowValue.Create(ThrownValue);

  Result := Result.UpdateEmpty(UndefinedCompletionValue);
end;

function EvaluateClassMethod(const AClassMethod: TGocciaClassMethod; const AContext: TGocciaEvaluationContext; const ASuperClass: TGocciaValue = nil): TGocciaValue;
var
  Statements: TObjectList<TGocciaASTNode>;
begin
  Statements := CopyStatementList(TGocciaBlockStatement(AClassMethod.Body).Nodes);

  if AClassMethod.IsGenerator and AClassMethod.IsAsync then
    Result := TGocciaAsyncGeneratorMethodValue.Create(AClassMethod.Parameters, Statements, AContext.Scope.CreateChild, AClassMethod.Name, ASuperClass)
  else if AClassMethod.IsGenerator then
    Result := TGocciaGeneratorMethodValue.Create(AClassMethod.Parameters, Statements, AContext.Scope.CreateChild, AClassMethod.Name, ASuperClass)
  else if AClassMethod.IsAsync then
    Result := TGocciaAsyncMethodValue.Create(AClassMethod.Parameters, Statements, AContext.Scope.CreateChild, AClassMethod.Name, ASuperClass)
  else
    Result := TGocciaMethodValue.Create(AClassMethod.Parameters, Statements, AContext.Scope.CreateChild, AClassMethod.Name, ASuperClass);
  ApplyFunctionObjectPrototype(Result,
    FunctionIntrinsicKind(AClassMethod.IsAsync, AClassMethod.IsGenerator));
  if AClassMethod.IsGenerator then
    InstallFunctionOwnPrototypeProperty(Result,
      FunctionIntrinsicKind(AClassMethod.IsAsync, AClassMethod.IsGenerator));
  TGocciaFunctionValue(Result).SourceFilePath := AContext.CurrentFilePath;
  TGocciaFunctionValue(Result).SourceLine := AClassMethod.Line;
  TGocciaFunctionValue(Result).SourceText := AClassMethod.SourceText;
end;

function EvaluateClass(const AClassDeclaration: TGocciaClassDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
begin
  ClassDef := AClassDeclaration.ClassDefinition;
  Result := EvaluateClassDefinition(ClassDef, AContext, AClassDeclaration.Line, AClassDeclaration.Column);

  // For class declarations, bind the class name to the scope
  AContext.Scope.DefineLexicalBinding(ClassDef.Name, Result, dtLet);
end;

// TC39 proposal-enum
function EvaluateEnumDeclaration(const AEnumDeclaration: TGocciaEnumDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  EnumValue: TGocciaEnumValue;
  EnumEntries: TGocciaArrayValue;
  ChildScope: TGocciaScope;
  ChildContext: TGocciaEvaluationContext;
  I: Integer;
  MemberValue: TGocciaValue;
  EntryPair: TGocciaArrayValue;
begin
  EnumValue := TGocciaEnumValue.Create(AEnumDeclaration.Name);
  EnumEntries := TGocciaArrayValue.Create;

  ChildScope := AContext.Scope.CreateChild(skBlock);
  ChildContext := AContext;
  ChildContext.Scope := ChildScope;

  ChildScope.DefineLexicalBinding(AEnumDeclaration.Name, EnumValue, dtLet);

  TGarbageCollector.Instance.AddTempRoot(EnumEntries);
  try
    for I := 0 to Length(AEnumDeclaration.Members) - 1 do
    begin
      MemberValue := EvaluateExpression(AEnumDeclaration.Members[I].Initializer, ChildContext);

      if not ((MemberValue is TGocciaNumberLiteralValue) or
              (MemberValue is TGocciaStringLiteralValue) or
              (MemberValue is TGocciaSymbolValue)) then
        ThrowTypeError(SErrorEnumInitializer, SSuggestEnumValueType);

      EnumValue.DefineProperty(AEnumDeclaration.Members[I].Name,
        TGocciaPropertyDescriptorData.Create(MemberValue, [pfEnumerable]));

      ChildScope.DefineLexicalBinding(AEnumDeclaration.Members[I].Name, MemberValue, dtLet);

      EntryPair := TGocciaArrayValue.Create;
      EntryPair.Elements.Add(TGocciaStringLiteralValue.Create(AEnumDeclaration.Members[I].Name));
      EntryPair.Elements.Add(MemberValue);
      EnumEntries.Elements.Add(EntryPair);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(EnumEntries);
  end;

  EnumValue.Entries := EnumEntries;
  InitializeEnumSymbols(EnumValue);
  EnumValue.PreventExtensions;

  AContext.Scope.DefineLexicalBinding(AEnumDeclaration.Name, EnumValue, dtLet);
  Result := EnumValue;
end;

procedure InitializeInstanceProperties(const AInstance: TGocciaInstanceValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext);
var
  PropertyValue: TGocciaValue;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  I: Integer;
  FOEntry: TGocciaClassFieldOrderEntry;
  Expr: TGocciaExpression;
  LocalContext: TGocciaEvaluationContext;
  LocalScope: TGocciaScope;
begin
  LocalContext := AContext;
  LocalScope := TGocciaClassInitScope.Create(AContext.Scope, AClassValue);
  LocalScope.ThisValue := AInstance;
  LocalContext.Scope := LocalScope;

  if Assigned(AClassValue.SuperClass) then
    InitializeInstanceProperties(AInstance, AClassValue.SuperClass, LocalContext);

  if AClassValue.HasPrivateInstanceElements then
    StampRawPrivateInstanceBrand(AInstance, AClassValue);

  if AClassValue.FieldOrderCount > 0 then
  begin
    for I := 0 to AClassValue.FieldOrderCount - 1 do
    begin
      FOEntry := AClassValue.FieldOrderEntry(I);
      Expr := nil;
      if FOEntry.IsComputed then
      begin
        if Assigned(FOEntry.Initializer) then
          PropertyValue := EvaluateExpression(FOEntry.Initializer, LocalContext)
        else
          PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if FOEntry.ComputedKey is TGocciaSymbolValue then
          AInstance.DefineSymbolProperty(
            TGocciaSymbolValue(FOEntry.ComputedKey),
            TGocciaPropertyDescriptorData.Create(PropertyValue,
              [pfEnumerable, pfConfigurable, pfWritable]))
        else if Assigned(FOEntry.ComputedKey) then
          AInstance.DefineProperty(FOEntry.ComputedKey.ToStringLiteral.Value,
            TGocciaPropertyDescriptorData.Create(PropertyValue,
              [pfEnumerable, pfConfigurable, pfWritable]));
      end
      else if FOEntry.IsPrivate then
      begin
        if AClassValue.PrivateInstancePropertyDefs.TryGetValue(
          FOEntry.Name, Expr) then
        begin
          if Assigned(Expr) then
            PropertyValue := EvaluateExpression(Expr, LocalContext)
          else
            PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          AInstance.SetPrivateProperty(FOEntry.Name, PropertyValue,
            AClassValue, True);
        end;
      end
      else
      begin
        if AClassValue.InstancePropertyDefs.TryGetValue(FOEntry.Name, Expr) and Assigned(Expr) then
        begin
          PropertyValue := EvaluateExpression(Expr, LocalContext);
          AInstance.AssignProperty(FOEntry.Name, PropertyValue);
        end;
      end;
    end;
  end
  else
  begin
    for I := 0 to AClassValue.InstancePropertyDefs.Count - 1 do
    begin
      Entry := AClassValue.InstancePropertyDefs.EntryAt(I);
      PropertyValue := EvaluateExpression(Entry.Value, LocalContext);
      AInstance.AssignProperty(Entry.Key, PropertyValue);
    end;
  end;
end;

procedure InitializeRawPrivateInstanceProperty(
  const AReceiver: TGocciaObjectValue; const APrivateName: string;
  const AValue: TGocciaValue; const AAccessClass: TGocciaClassValue;
  const AInitializationMode: TGocciaInstanceInitializationMode); forward;

procedure InitializeObjectInstanceProperties(const AInstance: TGocciaObjectValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext; const AInitializationMode: TGocciaInstanceInitializationMode);
var
  PropertyValue: TGocciaValue;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  I: Integer;
  FOEntry: TGocciaClassFieldOrderEntry;
  Expr: TGocciaExpression;
  SuperInitContext: TGocciaEvaluationContext;
  SuperInitScope: TGocciaScope;
begin
  if (AInitializationMode <> iimEagerReplacement) and
     Assigned(AClassValue.SuperClass) then
  begin
    SuperInitContext := AContext;
    SuperInitScope := TGocciaClassInitScope.Create(AContext.Scope, AClassValue.SuperClass);
    SuperInitScope.ThisValue := AInstance;
    SuperInitContext.Scope := SuperInitScope;
    InitializeObjectInstanceProperties(AInstance, AClassValue.SuperClass,
      SuperInitContext, AInitializationMode);
  end;

  if AClassValue.HasPrivateInstanceElements then
    StampRawPrivateInstanceBrand(AInstance, AClassValue);

  if AClassValue.FieldOrderCount > 0 then
  begin
    for I := 0 to AClassValue.FieldOrderCount - 1 do
    begin
      FOEntry := AClassValue.FieldOrderEntry(I);
      Expr := nil;
      if FOEntry.IsComputed then
      begin
        if Assigned(FOEntry.Initializer) then
          PropertyValue := EvaluateExpression(FOEntry.Initializer, AContext)
        else
          PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if FOEntry.ComputedKey is TGocciaSymbolValue then
          AInstance.DefineSymbolProperty(
            TGocciaSymbolValue(FOEntry.ComputedKey),
            TGocciaPropertyDescriptorData.Create(PropertyValue,
              [pfEnumerable, pfConfigurable, pfWritable]))
        else if Assigned(FOEntry.ComputedKey) then
          AInstance.DefineProperty(FOEntry.ComputedKey.ToStringLiteral.Value,
            TGocciaPropertyDescriptorData.Create(PropertyValue,
              [pfEnumerable, pfConfigurable, pfWritable]));
      end
      else if FOEntry.IsPrivate then
      begin
        if AClassValue.PrivateInstancePropertyDefs.TryGetValue(
          FOEntry.Name, Expr) then
        begin
          if Assigned(Expr) then
            PropertyValue := EvaluateExpression(Expr, AContext)
          else
            PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          InitializeRawPrivateInstanceProperty(AInstance, FOEntry.Name,
            PropertyValue, AClassValue, AInitializationMode);
        end;
      end
      else
      begin
        if AClassValue.InstancePropertyDefs.TryGetValue(FOEntry.Name, Expr) and Assigned(Expr) then
        begin
          PropertyValue := EvaluateExpression(Expr, AContext);
          AInstance.AssignProperty(FOEntry.Name, PropertyValue);
        end;
      end;
    end;
  end
  else
  begin
    for I := 0 to AClassValue.InstancePropertyDefs.Count - 1 do
    begin
      Entry := AClassValue.InstancePropertyDefs.EntryAt(I);
      PropertyValue := EvaluateExpression(Entry.Value, AContext);
      AInstance.AssignProperty(Entry.Key, PropertyValue);
    end;
    InitializePrivateInstanceProperties(AInstance, AClassValue, AContext,
      AInitializationMode);
  end;
end;

function EvaluateSwitch(const ASwitchStatement: TGocciaSwitchStatement; const AContext: TGocciaEvaluationContext): TGocciaControlFlow;
var
  Discriminant: TGocciaValue;
  SwitchValue: TGocciaValue;
  CaseClause: TGocciaCaseClause;
  CaseTest: TGocciaValue;
  CF: TGocciaControlFlow;
  I: Integer;
  Matched: Boolean;
  DefaultIndex: Integer;
  Done: Boolean;
  SwitchBlockContext: TGocciaEvaluationContext;
  NeedsSwitchScope: Boolean;
  HasUsingDeclarations: Boolean;
  Tracker: TGocciaDisposalTracker;
  DisposalError: TGocciaValue;
  CaughtError: TGocciaValue;
  HasCaughtError: Boolean;
  GC: TGarbageCollector;

  function ConsequentNeedsChildScope(
    const AConsequent: TObjectList<TGocciaStatement>): Boolean;
  var
    K: Integer;
    VarDecl: TGocciaVariableDeclaration;
  begin
    Result := False;
    for K := 0 to AConsequent.Count - 1 do
    begin
      if AConsequent[K] is TGocciaVariableDeclaration then
      begin
        VarDecl := TGocciaVariableDeclaration(AConsequent[K]);
        if not VarDecl.IsVar then
          Exit(True);
      end
      else if AConsequent[K] is TGocciaExportVariableDeclaration then
      begin
        VarDecl := TGocciaExportVariableDeclaration(AConsequent[K]).Declaration;
        if not VarDecl.IsVar then
          Exit(True);
      end
      else if (AConsequent[K] is TGocciaFunctionDeclaration) or
              (AConsequent[K] is TGocciaExportFunctionDeclaration) then
        Exit(True)
      else if ((AConsequent[K] is TGocciaDestructuringDeclaration) and
              (not TGocciaDestructuringDeclaration(AConsequent[K]).IsVar)) or
              (AConsequent[K] is TGocciaClassDeclaration) or
              (AConsequent[K] is TGocciaEnumDeclaration) or
              (AConsequent[K] is TGocciaExportEnumDeclaration) or
              (AConsequent[K] is TGocciaUsingDeclaration) then
        Exit(True);
    end;
  end;

  function SwitchNeedsChildScope: Boolean;
  var
    K: Integer;
  begin
    Result := False;
    for K := 0 to ASwitchStatement.Cases.Count - 1 do
      if ConsequentNeedsChildScope(ASwitchStatement.Cases[K].Consequent) then
        Exit(True);
  end;

  function SwitchHasUsingDeclarations: Boolean;
  var
    K, L: Integer;
  begin
    Result := False;
    for K := 0 to ASwitchStatement.Cases.Count - 1 do
      for L := 0 to ASwitchStatement.Cases[K].Consequent.Count - 1 do
        if ASwitchStatement.Cases[K].Consequent[L] is TGocciaUsingDeclaration then
          Exit(True);
  end;

  procedure HoistSwitchConsequents;
  var
    K: Integer;
  begin
    for K := 0 to ASwitchStatement.Cases.Count - 1 do
      HoistFunctionDeclarations(ASwitchStatement.Cases[K].Consequent,
        SwitchBlockContext, True);
  end;

  procedure PredeclareSwitchConsequents;
  var
    K, L: Integer;
  begin
    for K := 0 to ASwitchStatement.Cases.Count - 1 do
      for L := 0 to ASwitchStatement.Cases[K].Consequent.Count - 1 do
        PredeclareBlockLexicalBinding(ASwitchStatement.Cases[K].Consequent[L],
          SwitchBlockContext.Scope);
  end;

  function EvaluateCaseConsequent(
    const AConsequent: TObjectList<TGocciaStatement>): TGocciaControlFlow;
  var
    K: Integer;
    ConsequentNodes: TObjectList<TGocciaASTNode>;
  begin
    ConsequentNodes := TObjectList<TGocciaASTNode>.Create(False);
    try
      for K := 0 to AConsequent.Count - 1 do
        ConsequentNodes.Add(AConsequent[K]);
      Result := EvaluateStatements(ConsequentNodes, SwitchBlockContext);
    finally
      ConsequentNodes.Free;
    end;
  end;
begin
  SwitchValue := UndefinedCompletionValue;
  Result := TGocciaControlFlow.Normal(SwitchValue);
  Discriminant := EvaluateExpression(ASwitchStatement.Discriminant, AContext);

  NeedsSwitchScope := SwitchNeedsChildScope;
  HasUsingDeclarations := SwitchHasUsingDeclarations;
  SwitchBlockContext := AContext;
  if NeedsSwitchScope then
    SwitchBlockContext.Scope := AContext.Scope.CreateChild(skBlock,
      'SwitchScope')
  else
    SwitchBlockContext.Scope := AContext.Scope;

  Tracker := nil;
  DisposalError := nil;
  CaughtError := nil;
  HasCaughtError := False;
  GC := TGarbageCollector.Instance;
  if NeedsSwitchScope and Assigned(GC) then
    GC.AddTempRoot(SwitchBlockContext.Scope);
  if HasUsingDeclarations then
  begin
    Tracker := TGocciaDisposalTracker.Create;
    SwitchBlockContext.DisposalTracker := Tracker;
  end;

  try
    try
      if NeedsSwitchScope then
      begin
        PredeclareSwitchConsequents;
        HoistSwitchConsequents;
      end;

      Matched := False;
      DefaultIndex := -1;
      Done := False;

      for I := 0 to ASwitchStatement.Cases.Count - 1 do
      begin
        CaseClause := ASwitchStatement.Cases[I];

        if not Assigned(CaseClause.Test) then
        begin
          DefaultIndex := I;
          if not Matched then
            Continue;
        end;

        if not Matched then
        begin
          CaseTest := EvaluateExpression(CaseClause.Test, SwitchBlockContext);
          if Goccia.Arithmetic.IsStrictEqual(Discriminant, CaseTest) then
          begin
            Matched := True;
            if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
              TGocciaCoverageTracker.Instance.RecordBranchHit(
                AContext.CurrentFilePath, ASwitchStatement.Line,
                ASwitchStatement.Column, I);
          end;
        end;

        if Matched then
        begin
          CF := EvaluateCaseConsequent(CaseClause.Consequent);
          UpdateValueFromCompletion(CF, SwitchValue);
          if CF.Kind = cfkBreak then
          begin
            if TargetsStatementOrUnlabeled(CF, ASwitchStatement) then
            begin
              Result := NormalCompletionFromAbrupt(CF, SwitchValue);
              SwitchValue := Result.Value;
              Done := True
            end
            else
            begin
              Result := CF.UpdateEmpty(SwitchValue);
              Exit;
            end;
          end
          else if CF.Kind in [cfkReturn, cfkContinue] then
          begin
            Result := CF.UpdateEmpty(SwitchValue);
            Exit;
          end
          else
            Result := TGocciaControlFlow.Normal(SwitchValue);
          if Done then Break;
        end
      end;

      if not Matched and not Done and (DefaultIndex >= 0) then
      begin
        if AContext.CoverageEnabled and Assigned(TGocciaCoverageTracker.Instance) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            AContext.CurrentFilePath, ASwitchStatement.Line,
            ASwitchStatement.Column, DefaultIndex);
        for I := DefaultIndex to ASwitchStatement.Cases.Count - 1 do
        begin
          CaseClause := ASwitchStatement.Cases[I];
          CF := EvaluateCaseConsequent(CaseClause.Consequent);
          UpdateValueFromCompletion(CF, SwitchValue);
          if CF.Kind = cfkBreak then
          begin
            if TargetsStatementOrUnlabeled(CF, ASwitchStatement) then
            begin
              Result := NormalCompletionFromAbrupt(CF, SwitchValue);
              SwitchValue := Result.Value;
              Done := True
            end
            else
            begin
              Result := CF.UpdateEmpty(SwitchValue);
              Exit;
            end;
          end
          else if CF.Kind in [cfkReturn, cfkContinue] then
          begin
            Result := CF.UpdateEmpty(SwitchValue);
            Exit;
          end
          else
            Result := TGocciaControlFlow.Normal(SwitchValue);
          if Done then Break;
        end;
      end;
      Result := TGocciaControlFlow.Normal(SwitchValue);
    except
      on E: TGocciaThrowValue do
      begin
        if not HasUsingDeclarations then
          raise;
        CaughtError := E.Value;
        HasCaughtError := True;
        if Assigned(GC) and Assigned(CaughtError) then
          GC.AddTempRoot(CaughtError);
        Result := TGocciaControlFlow.Normal(
          TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end;
  finally
    if HasUsingDeclarations then
    begin
      if HasCaughtError then
      begin
        if HasAsyncDisposals(Tracker) then
          DisposalError := DisposeTrackedResourcesAsync(Tracker, CaughtError)
        else
          DisposalError := DisposeTrackedResources(Tracker, CaughtError);
      end
      else
      begin
        if HasAsyncDisposals(Tracker) then
          DisposalError := DisposeTrackedResourcesAsync(Tracker, nil)
        else
          DisposalError := DisposeTrackedResources(Tracker, nil);
      end;
      Tracker.Free;
      SwitchBlockContext.DisposalTracker := nil;
    end;

    if NeedsSwitchScope and Assigned(GC) then
      GC.RemoveTempRoot(SwitchBlockContext.Scope);

    if HasCaughtError and Assigned(GC) and Assigned(CaughtError) then
      GC.RemoveTempRoot(CaughtError);

    if Assigned(DisposalError) then
      raise TGocciaThrowValue.Create(DisposalError)
    else if HasCaughtError then
      raise TGocciaThrowValue.Create(CaughtError);
  end;
end;

// ES2026 §10.2.2 [[Construct]] (argumentsList, newTarget)
function ConstructOrdinaryFunction(const AConstructor: TGocciaFunctionBase;
  const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  PrototypeValue, ReturnValue: TGocciaValue;
  PrototypeObj, Instance: TGocciaObjectValue;
begin
  PrototypeValue := AConstructor.GetProperty(PROP_PROTOTYPE);
  if PrototypeValue is TGocciaObjectValue then
    PrototypeObj := TGocciaObjectValue(PrototypeValue)
  else
  begin
    if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
      TGocciaObjectValue.InitializeSharedPrototype;
    PrototypeObj := TGocciaObjectValue.SharedObjectPrototype;
  end;

  Instance := TGocciaObjectValue.Create(PrototypeObj);
  TGarbageCollector.Instance.AddTempRoot(Instance);
  try
    ReturnValue := AConstructor.Call(AArguments, Instance);
    if ReturnValue is TGocciaObjectValue then
      Result := ReturnValue
    else
      Result := Instance;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Instance);
  end;
end;

function ConstructBoundFunction(const AConstructor: TGocciaBoundFunctionValue;
  const AArguments: TGocciaArgumentsCollection;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  BoundArgs: TGocciaArgumentsCollection;
  I: Integer;
  Target: TGocciaValue;
begin
  Target := AConstructor.OriginalFunction;
  BoundArgs := TGocciaArgumentsCollection.CreateWithCapacity(
    AConstructor.BoundArgCount + AArguments.Length);
  try
    for I := 0 to AConstructor.BoundArgCount - 1 do
      BoundArgs.Add(AConstructor.GetBoundArg(I));
    for I := 0 to AArguments.Length - 1 do
      BoundArgs.Add(AArguments.GetElement(I));

    if Target is TGocciaBoundFunctionValue then
      Result := ConstructBoundFunction(TGocciaBoundFunctionValue(Target),
        BoundArgs, AContext)
    else if Target is TGocciaProxyValue then
      Result := TGocciaProxyValue(Target).ConstructTrap(BoundArgs)
    else if Target is TGocciaClassValue then
      Result := InstantiateClass(TGocciaClassValue(Target), BoundArgs, AContext)
    else if Target is TGocciaNativeFunctionValue then
    begin
      if TGocciaNativeFunctionValue(Target).NotConstructable then
        ThrowTypeError(
          Format(SErrorNotConstructor,
            [TGocciaNativeFunctionValue(Target).Name]),
          Format('''%s'' is not a constructor',
            [TGocciaNativeFunctionValue(Target).Name]));
      Result := TGocciaNativeFunctionValue(Target).Construct(BoundArgs,
        Target);
    end
    else if (Target is TGocciaFunctionBase) and
            not (Target is TGocciaGeneratorFunctionValue) and
            TGocciaFunctionBase(Target).IsConstructable then
      Result := ConstructOrdinaryFunction(TGocciaFunctionBase(Target),
        BoundArgs)
    else
      ThrowTypeError(
        Format(SErrorValueNotConstructor, [Target.TypeName]),
        Format('values of type ''%s'' cannot be used with ''new''',
          [Target.TypeName]));
  finally
    BoundArgs.Free;
  end;
end;

function EvaluateNewExpression(const ANewExpression: TGocciaNewExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Callee: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  SpreadValue: TGocciaValue;
  ArgumentValue: TGocciaValue;
  CalleeName: string;
  FirstAddedIndex, I: Integer;
  Instance: TGocciaInstanceValue;
  NativeInstance: TGocciaObjectValue;
  WalkClass, ClassValue: TGocciaClassValue;
  InitContext, SuperInitContext: TGocciaEvaluationContext;
  InitScope, SuperInitScope: TGocciaScope;
  FunctionCallee: TGocciaFunctionBase;
  PrototypeTarget: TGocciaValue;
  PrototypeValue: TGocciaValue;
  ReceiverPrototype: TGocciaObjectValue;
  ReceiverInstance: TGocciaObjectValue;
  Roots: TGocciaActiveRootFrame;
begin
  Roots.Initialize;
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;
  Callee := EvaluateExpression(ANewExpression.Callee, AContext);
  AddValueRoot(Roots, Callee);

  Arguments := TGocciaArgumentsCollection.Create;
  try
    for I := 0 to ANewExpression.Arguments.Count - 1 do
    begin
      if ANewExpression.Arguments[I] is TGocciaSpreadExpression then
      begin
        SpreadValue := EvaluateExpression(TGocciaSpreadExpression(ANewExpression.Arguments[I]).Argument, AContext);
        AddValueRoot(Roots, SpreadValue);
        FirstAddedIndex := Arguments.Length;
        SpreadIterableIntoArgs(SpreadValue, Arguments);
        RootArgumentsFrom(Roots, Arguments, FirstAddedIndex);
      end
      else
      begin
        ArgumentValue := EvaluateExpression(ANewExpression.Arguments[I], AContext);
        Arguments.Add(ArgumentValue);
        AddValueRoot(Roots, ArgumentValue);
      end;
    end;

    if Callee is TGocciaClassValue then
      CalleeName := TGocciaClassValue(Callee).Name
    else if Callee is TGocciaNativeFunctionValue then
      CalleeName := TGocciaNativeFunctionValue(Callee).Name
    else if Callee is TGocciaFunctionBase then
      CalleeName := TGocciaFunctionBase(Callee).GetProperty(PROP_NAME).ToStringLiteral.Value
    else
      CalleeName := '';

    if Assigned(TGocciaCallStack.Instance) then
    begin
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ANewExpression.Line, ANewExpression.Column);
    end;
    try
      if Assigned(TGocciaCallStack.Instance) then
        CheckStackDepth(TGocciaCallStack.Instance.Count);
      if Callee is TGocciaProxyValue then
      begin
        Result := TGocciaProxyValue(Callee).ConstructTrap(Arguments);
      end
      else if Callee is TGocciaClassValue then
      begin
        Result := InstantiateClass(TGocciaClassValue(Callee), Arguments, AContext);
      end
      else if Callee is TGocciaNativeFunctionValue then
      begin
        if TGocciaNativeFunctionValue(Callee).NotConstructable then
          ThrowTypeError(
            Format(SErrorNotConstructor,
              [TGocciaNativeFunctionValue(Callee).Name]),
            Format('''%s'' is not a constructor',
              [TGocciaNativeFunctionValue(Callee).Name]));
        Result := TGocciaNativeFunctionValue(Callee).Construct(Arguments,
          Callee);
      end
      else if Callee is TGocciaFunctionBase then
      begin
        // ES2026 §10.2.2 [[Construct]] for ordinary function objects:
        // OrdinaryCreateFromConstructor produces a fresh object whose
        // [[Prototype]] is constructor.prototype (or %Object.prototype% when
        // that property is not an Object — §10.1.14 GetPrototypeFromConstructor).
        // For bound function exotic objects (§10.4.1.2) the receiver's
        // [[Prototype]] comes from the underlying [[BoundTargetFunction]] —
        // the bound wrapper itself has no own `prototype` data property.
        FunctionCallee := TGocciaFunctionBase(Callee);
        if not FunctionCallee.IsConstructable then
          ThrowTypeError(Format(SErrorNotConstructor, [CalleeName]),
            SSuggestNotConstructorType);
        PrototypeTarget := FunctionCallee;
        while PrototypeTarget is TGocciaBoundFunctionValue do
          PrototypeTarget := TGocciaBoundFunctionValue(PrototypeTarget).OriginalFunction;
        if PrototypeTarget is TGocciaObjectValue then
          PrototypeValue := TGocciaObjectValue(PrototypeTarget).GetProperty(PROP_PROTOTYPE)
        else
          PrototypeValue := nil;
        if PrototypeValue is TGocciaObjectValue then
          ReceiverPrototype := TGocciaObjectValue(PrototypeValue)
        else
        begin
          if not Assigned(TGocciaObjectValue.SharedObjectPrototype) then
            TGocciaObjectValue.InitializeSharedPrototype;
          ReceiverPrototype := TGocciaObjectValue.SharedObjectPrototype;
        end;
        ReceiverInstance := TGocciaObjectValue.Create(ReceiverPrototype);
        TGarbageCollector.Instance.AddTempRoot(ReceiverInstance);
        try
          // InvokeConstructableWithReceiver merges bound args, dispatches to
          // the underlying target, and applies the spec return rules
          // (explicit Object return wins; otherwise the receiver).
          Result := InvokeConstructableWithReceiver(FunctionCallee, Arguments,
            ReceiverInstance, AContext);
        finally
          TGarbageCollector.Instance.RemoveTempRoot(ReceiverInstance);
        end;
      end
      else
      begin
        if ANewExpression.Callee is TGocciaIdentifierExpression then
          ThrowTypeError(
            Format(SErrorNotConstructor,
              [TGocciaIdentifierExpression(ANewExpression.Callee).Name]),
            Format('''%s'' is of type ''%s'' and cannot be used with ''new''',
              [TGocciaIdentifierExpression(ANewExpression.Callee).Name,
               Callee.TypeName]))
        else
          ThrowTypeError(
            Format(SErrorValueNotConstructor, [Callee.TypeName]),
            Format('values of type ''%s'' cannot be used with ''new''',
              [Callee.TypeName]));
      end;
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
    AddValueRoot(Roots, Result);
    CollectInterpreterMemoryPressure(Result);
  finally
    Arguments.Free;
    Roots.Clear;
  end;
end;

function EvaluateClassExpression(const AClassExpression: TGocciaClassExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ClassDef: TGocciaClassDefinition;
  InnerContext: TGocciaEvaluationContext;
  InnerScope: TGocciaScope;
begin
  ClassDef := AClassExpression.ClassDefinition;

  if ClassDef.Name <> '' then
  begin
    // ES2026 §15.7.14: Named class expressions create an inner binding
    // visible only inside the class body
    InnerScope := AContext.Scope.CreateChild(skBlock);
    InnerScope.DefineLexicalBinding(ClassDef.Name,
      TGocciaUndefinedLiteralValue.UndefinedValue, dtConst);
    InnerContext := AContext;
    InnerContext.Scope := InnerScope;
    Result := EvaluateClassDefinition(ClassDef, InnerContext,
      AClassExpression.Line, AClassExpression.Column);
    InnerScope.ForceUpdateBinding(ClassDef.Name, Result);
  end
  else
    Result := EvaluateClassDefinition(ClassDef, AContext,
      AClassExpression.Line, AClassExpression.Column);
end;

// ES2022 §15.7.14 ClassStaticBlockDefinition: execute static block body
procedure ExecuteStaticBlock(const ABody: TGocciaBlockStatement;
  const AContext: TGocciaEvaluationContext; const AClassValue: TGocciaClassValue);
var
  BlockScope: TGocciaClassInitScope;
  BlockContext: TGocciaEvaluationContext;
begin
  BlockScope := TGocciaClassInitScope.Create(AContext.Scope, AClassValue);
  BlockScope.ThisValue := AClassValue;
  BlockContext := AContext;
  BlockContext.Scope := BlockScope;
  EvaluateBlock(ABody, BlockContext);
end;

procedure AddClassCallableEvalEntry(var AEntries: TClassCallableEvalEntries;
  const AKind: TClassCallableEvalKind; const AName: string;
  const AIsStatic, AIsPrivate: Boolean; const ALine, AColumn: Integer;
  var AOrder: Integer; const AElementIndex: Integer = -1;
  const AMethodNode: TGocciaClassMethod = nil;
  const AGetterNode: TGocciaGetterExpression = nil;
  const ASetterNode: TGocciaSetterExpression = nil);
var
  Index: Integer;
begin
  Index := Length(AEntries);
  SetLength(AEntries, Index + 1);
  AEntries[Index].Kind := AKind;
  AEntries[Index].Name := AName;
  AEntries[Index].IsStatic := AIsStatic;
  AEntries[Index].IsPrivate := AIsPrivate;
  AEntries[Index].Line := ALine;
  AEntries[Index].Column := AColumn;
  AEntries[Index].Order := AOrder;
  AEntries[Index].ElementIndex := AElementIndex;
  AEntries[Index].MethodNode := AMethodNode;
  AEntries[Index].GetterNode := AGetterNode;
  AEntries[Index].SetterNode := ASetterNode;
  Inc(AOrder);
end;

function ClassCallableEvalEntryAfter(const ALeft,
  ARight: TClassCallableEvalEntry): Boolean;
begin
  if ALeft.Line <> ARight.Line then
    Exit(ALeft.Line > ARight.Line);
  if ALeft.Column <> ARight.Column then
    Exit(ALeft.Column > ARight.Column);
  Result := ALeft.Order > ARight.Order;
end;

procedure SortClassCallableEvalEntries(var AEntries: TClassCallableEvalEntries);
var
  I, J: Integer;
  Entry: TClassCallableEvalEntry;
begin
  for I := 1 to High(AEntries) do
  begin
    Entry := AEntries[I];
    J := I - 1;
    while (J >= 0) and ClassCallableEvalEntryAfter(AEntries[J], Entry) do
    begin
      AEntries[J + 1] := AEntries[J];
      Dec(J);
    end;
    AEntries[J + 1] := Entry;
  end;
end;

function ClassEvalElementSourceLine(const AElement: TGocciaClassElement): Integer;
begin
  if Assigned(AElement.MethodNode) then
    Exit(AElement.MethodNode.Line);
  if Assigned(AElement.GetterNode) then
    Exit(AElement.GetterNode.Line);
  if Assigned(AElement.SetterNode) then
    Exit(AElement.SetterNode.Line);
  if Assigned(AElement.ComputedKeyExpression) then
    Exit(AElement.ComputedKeyExpression.Line);
  if Assigned(AElement.FieldInitializer) then
    Exit(AElement.FieldInitializer.Line);
  if Assigned(AElement.StaticBlockBody) then
    Exit(AElement.StaticBlockBody.Line);
  Result := 0;
end;

function ClassEvalElementSourceColumn(
  const AElement: TGocciaClassElement): Integer;
begin
  if Assigned(AElement.MethodNode) then
    Exit(AElement.MethodNode.Column);
  if Assigned(AElement.GetterNode) then
    Exit(AElement.GetterNode.Column);
  if Assigned(AElement.SetterNode) then
    Exit(AElement.SetterNode.Column);
  if Assigned(AElement.ComputedKeyExpression) then
    Exit(AElement.ComputedKeyExpression.Column);
  if Assigned(AElement.FieldInitializer) then
    Exit(AElement.FieldInitializer.Column);
  if Assigned(AElement.StaticBlockBody) then
    Exit(AElement.StaticBlockBody.Column);
  Result := 0;
end;

function EvaluateClassDefinition(const AClassDef: TGocciaClassDefinition; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaClassValue;
var
  SuperClass: TGocciaClassValue;
  SuperClassValue: TGocciaValue;
  MethodSuperClass: TGocciaValue;
  SuperPrototype: TGocciaValue;
  ClassValue: TGocciaClassValue;
  MethodPair: TGocciaClassMethodMap.TKeyValuePair;
  PropertyPair: TGocciaExpressionMap.TKeyValuePair;
  PropertyEntry: TGocciaExpressionMap.TKeyValuePair;
  GetterPair: TGocciaGetterExpressionMap.TKeyValuePair;
  SetterPair: TGocciaSetterExpressionMap.TKeyValuePair;
  Method: TGocciaMethodValue;
  ComputedKey: TGocciaValue;
  PropertyValue: TGocciaValue;
  PropertyExpr: TGocciaExpression;
  GetterFunction, SetterFunction: TGocciaFunctionValue;
  ClassName: string;
  I, J: Integer;
  HasDecorators: Boolean;
  HasReplayedComputedKey: Boolean;
  FieldOrderEntries: array of TGocciaClassFieldOrderEntry;
  ResolvedComputedElementKeys: array of TGocciaValue;
  Continuation: TGocciaGeneratorContinuation;
  MetadataObject: TGocciaObjectValue;
  SuperMetadata: TGocciaValue;
  EvaluatedElementDecorators: array of array of TGocciaValue;
  EvaluatedClassDecorators: array of TGocciaValue;
  DecoratorFn, DecoratorResult: TGocciaValue;
  DecoratorArgs, InitArgs: TGocciaArgumentsCollection;
  ContextObject, AccessObject, AutoAccessorValue, DecResultObj: TGocciaObjectValue;
  Elem: TGocciaClassElement;
  ElementName: string;
  ElementKey: TGocciaValue;
  CurrentMethod, GetterFnValue, SetterFnValue: TGocciaValue;
  NewGetter, NewSetter, NewInit: TGocciaValue;
  ExistingGetterValue, ExistingSetterValue: TGocciaValue;
  MethodCollector, FieldCollector, StaticFieldCollector, ClassCollector: TGocciaInitializerCollector;
  OriginalClassValue: TGocciaClassValue;
  AccessGetterHelper: TGocciaAccessGetter;
  AccessSetterHelper: TGocciaAccessSetter;
  InitializerResults: TArray<TGocciaValue>;
  AccessorBackingName: string;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  StaticFieldContext: TGocciaEvaluationContext;
  StaticFieldScope: TGocciaScope;
  HeritageContext: TGocciaEvaluationContext;

  function BuildClassGetter(const AGetterExpression: TGocciaGetterExpression): TGocciaFunctionValue;
  begin
    Result := TGocciaFunctionValue(EvaluateGetter(
      AGetterExpression, AContext, MethodSuperClass, True));
    TGocciaMethodValue(Result).OwningClass := ClassValue;
  end;

  function BuildClassSetter(const ASetterExpression: TGocciaSetterExpression): TGocciaFunctionValue;
  begin
    Result := TGocciaFunctionValue(EvaluateSetter(
      ASetterExpression, AContext, MethodSuperClass, True));
    TGocciaMethodValue(Result).OwningClass := ClassValue;
  end;

  function GetDecoratedDataProperty(const AIsStatic: Boolean;
    const AName: string; const AKey: TGocciaValue): TGocciaValue;
  begin
    if AKey is TGocciaSymbolValue then
    begin
      if AIsStatic then
        Result := ClassValue.GetSymbolProperty(TGocciaSymbolValue(AKey))
      else
        Result := ClassValue.Prototype.GetSymbolProperty(
          TGocciaSymbolValue(AKey));
    end
    else if AIsStatic then
      Result := ClassValue.GetProperty(AName)
    else
      Result := ClassValue.Prototype.GetProperty(AName);
  end;

  function GetPrivateMethodValue(const AName: string;
    const AIsStatic: Boolean): TGocciaValue;
  begin
    if AIsStatic then
    begin
      if not ClassValue.PrivateStaticMethods.TryGetValue(AName, Result) then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
      Result := ClassValue.GetPrivateMethod(AName);
  end;

  procedure DefinePrivateMethodValue(const AName: string;
    const AIsStatic: Boolean; const AValue: TGocciaValue);
  begin
    if AIsStatic then
      ClassValue.AddPrivateStaticMethod(AName, AValue)
    else if AValue is TGocciaMethodValue then
      ClassValue.AddPrivateMethod(AName, TGocciaMethodValue(AValue));
  end;

  procedure DefineDecoratedDataProperty(const AIsStatic: Boolean;
    const AName: string; const AKey, AValue: TGocciaValue);
  begin
    if AKey is TGocciaSymbolValue then
    begin
      if AIsStatic then
        ClassValue.DefineSymbolProperty(
          TGocciaSymbolValue(AKey),
          TGocciaPropertyDescriptorData.Create(
            AValue, [pfConfigurable, pfWritable]))
      else
        ClassValue.Prototype.DefineSymbolProperty(
          TGocciaSymbolValue(AKey),
          TGocciaPropertyDescriptorData.Create(
            AValue, [pfConfigurable, pfWritable]));
    end
    else if AIsStatic then
      ClassValue.DefineProperty(AName,
        TGocciaPropertyDescriptorData.Create(
          AValue, [pfConfigurable, pfWritable]))
    else
      ClassValue.Prototype.AssignProperty(AName, AValue);
  end;

  function GetDecoratedAccessorDescriptor(const AIsStatic: Boolean;
    const AName: string; const AKey: TGocciaValue): TGocciaPropertyDescriptor;
  begin
    if AKey is TGocciaSymbolValue then
    begin
      if AIsStatic then
        Result := ClassValue.GetOwnStaticSymbolDescriptor(
          TGocciaSymbolValue(AKey))
      else
        Result := ClassValue.Prototype.GetOwnSymbolPropertyDescriptor(
          TGocciaSymbolValue(AKey));
    end
    else if AIsStatic then
      Result := ClassValue.GetOwnPropertyDescriptor(AName)
    else
      Result := ClassValue.Prototype.GetOwnPropertyDescriptor(AName);
  end;

  procedure DefineDecoratedAccessorProperty(const AIsStatic: Boolean;
    const AName: string; const AKey, AGetter, ASetter: TGocciaValue);
  begin
    if AKey is TGocciaSymbolValue then
    begin
      if AIsStatic then
        ClassValue.DefineSymbolProperty(
          TGocciaSymbolValue(AKey),
          TGocciaPropertyDescriptorAccessor.Create(
            AGetter, ASetter, [pfConfigurable]))
      else
        ClassValue.Prototype.DefineSymbolProperty(
          TGocciaSymbolValue(AKey),
          TGocciaPropertyDescriptorAccessor.Create(
            AGetter, ASetter, [pfConfigurable]));
    end
    else if AIsStatic then
      ClassValue.DefineProperty(AName,
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ASetter, [pfConfigurable]))
    else
      ClassValue.Prototype.DefineProperty(AName,
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ASetter, [pfConfigurable, pfWritable]));
  end;

  procedure EvaluateClassCallableElements;
  var
    Entries: TClassCallableEvalEntries;
    Entry: TClassCallableEvalEntry;
    Order, K: Integer;
  begin
    SetLength(ResolvedComputedElementKeys, Length(AClassDef.FElements));
    SetLength(Entries, 0);
    Order := 0;

    for MethodPair in AClassDef.Methods do
      AddClassCallableEvalEntry(Entries, ccekMethod, MethodPair.Key, False,
        False, MethodPair.Value.Line, MethodPair.Value.Column, Order, -1,
        MethodPair.Value);

    for MethodPair in AClassDef.StaticMethods do
      AddClassCallableEvalEntry(Entries, ccekMethod, MethodPair.Key, True,
        False, MethodPair.Value.Line, MethodPair.Value.Column, Order, -1,
        MethodPair.Value);

    for MethodPair in AClassDef.PrivateMethods do
      AddClassCallableEvalEntry(Entries, ccekMethod, MethodPair.Key,
        MethodPair.Value.IsStatic, True, MethodPair.Value.Line,
        MethodPair.Value.Column, Order, -1, MethodPair.Value);

    for GetterPair in AClassDef.Getters do
      AddClassCallableEvalEntry(Entries, ccekGetter, GetterPair.Key, False,
        (GetterPair.Key <> '') and (GetterPair.Key[1] = '#'),
        GetterPair.Value.Line, GetterPair.Value.Column, Order, -1, nil,
        GetterPair.Value);

    for SetterPair in AClassDef.Setters do
      AddClassCallableEvalEntry(Entries, ccekSetter, SetterPair.Key, False,
        (SetterPair.Key <> '') and (SetterPair.Key[1] = '#'),
        SetterPair.Value.Line, SetterPair.Value.Column, Order, -1, nil, nil,
        SetterPair.Value);

    for GetterPair in AClassDef.FStaticGetters do
      AddClassCallableEvalEntry(Entries, ccekGetter, GetterPair.Key, True,
        (GetterPair.Key <> '') and (GetterPair.Key[1] = '#'),
        GetterPair.Value.Line, GetterPair.Value.Column, Order, -1, nil,
        GetterPair.Value);

    for SetterPair in AClassDef.FStaticSetters do
      AddClassCallableEvalEntry(Entries, ccekSetter, SetterPair.Key, True,
        (SetterPair.Key <> '') and (SetterPair.Key[1] = '#'),
        SetterPair.Value.Line, SetterPair.Value.Column, Order, -1, nil, nil,
        SetterPair.Value);

    for K := 0 to High(AClassDef.FElements) do
    begin
      Elem := AClassDef.FElements[K];
      if Elem.IsComputed and
         (Elem.Kind in [cekMethod, cekGetter, cekSetter, cekField, cekAccessor]) then
        AddClassCallableEvalEntry(Entries, ccekElement, '', Elem.IsStatic,
          Elem.IsPrivate, ClassEvalElementSourceLine(Elem),
          ClassEvalElementSourceColumn(Elem), Order, K);
    end;

    SortClassCallableEvalEntries(Entries);

    for K := 0 to High(Entries) do
    begin
      Entry := Entries[K];

      case Entry.Kind of
        ccekMethod:
        begin
          Method := TGocciaMethodValue(EvaluateClassMethod(
            Entry.MethodNode, AContext, MethodSuperClass));
          Method.OwningClass := ClassValue;
          if Entry.IsPrivate then
          begin
            if Entry.IsStatic then
              ClassValue.AddPrivateStaticMethod(Entry.Name, Method)
            else
              ClassValue.AddPrivateMethod(Entry.Name, Method);
          end
          else if Entry.IsStatic then
            ClassValue.DefineProperty(Entry.Name,
              TGocciaPropertyDescriptorData.Create(Method,
                [pfConfigurable, pfWritable]))
          else
            ClassValue.AddMethod(Entry.Name, Method);
          Continue;
        end;
        ccekGetter:
        begin
          GetterFunction := BuildClassGetter(Entry.GetterNode);
          GetterFunction.SetInferredName('get ' + Entry.Name);
          if Entry.IsPrivate then
            ClassValue.AddPrivateGetter(Copy(Entry.Name, 2, MaxInt),
              GetterFunction)
          else if Entry.IsStatic then
            ClassValue.AddStaticGetter(Entry.Name, GetterFunction)
          else
            ClassValue.AddGetter(Entry.Name, GetterFunction);
          Continue;
        end;
        ccekSetter:
        begin
          SetterFunction := BuildClassSetter(Entry.SetterNode);
          SetterFunction.SetInferredName('set ' + Entry.Name);
          if Entry.IsPrivate then
            ClassValue.AddPrivateSetter(Copy(Entry.Name, 2, MaxInt),
              SetterFunction)
          else if Entry.IsStatic then
            ClassValue.AddStaticSetter(Entry.Name, SetterFunction)
          else
            ClassValue.AddSetter(Entry.Name, SetterFunction);
          Continue;
        end;
      end;

      Elem := AClassDef.FElements[Entry.ElementIndex];
      if not Elem.IsComputed then
        Continue;
      if not (Elem.Kind in [cekMethod, cekGetter, cekSetter, cekField, cekAccessor]) then
        Continue;

      HasReplayedComputedKey := False;
      if Assigned(Continuation) then
        HasReplayedComputedKey := Continuation.TakeCompletedExpressionValue(
          Elem.ComputedKeyExpression, ComputedKey);
      if not HasReplayedComputedKey then
        ComputedKey := ToPropertyKey(EvaluateExpression(
          Elem.ComputedKeyExpression, AContext));
      if Assigned(Continuation) then
        Continuation.SaveCompletedExpressionValue(
          Elem.ComputedKeyExpression, ComputedKey);

      ResolvedComputedElementKeys[Entry.ElementIndex] := ComputedKey;

      case Elem.Kind of
        cekField:
          ;
        cekAccessor:
          ;
        cekMethod:
        begin
          Method := TGocciaMethodValue(EvaluateClassMethod(
            Elem.MethodNode, AContext, MethodSuperClass));
          Method.OwningClass := ClassValue;
          Method.SetInferredName(FunctionNameFromPropertyKey(ComputedKey));
          if ComputedKey is TGocciaSymbolValue then
          begin
            if Elem.IsStatic then
              ClassValue.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorData.Create(Method,
                  [pfWritable, pfConfigurable]))
            else
              ClassValue.Prototype.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorData.Create(Method,
                  [pfWritable, pfConfigurable]));
          end
          else if Elem.IsStatic then
            ClassValue.DefineProperty(ComputedKey.ToStringLiteral.Value,
              TGocciaPropertyDescriptorData.Create(Method,
                [pfConfigurable, pfWritable]))
          else
            ClassValue.AddMethod(ComputedKey.ToStringLiteral.Value, Method);
        end;
        cekGetter:
        begin
          GetterFunction := BuildClassGetter(Elem.GetterNode);
          GetterFunction.SetInferredName(
            FunctionNameFromPropertyKey(ComputedKey, 'get'));
          if ComputedKey is TGocciaSymbolValue then
          begin
            if Elem.IsStatic then
              ClassValue.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil,
                  [pfConfigurable]))
            else
              ClassValue.Prototype.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(GetterFunction, nil,
                  [pfConfigurable]));
          end
          else if Elem.IsStatic then
            ClassValue.AddStaticGetter(ComputedKey.ToStringLiteral.Value,
              GetterFunction)
          else
            ClassValue.AddGetter(ComputedKey.ToStringLiteral.Value,
              GetterFunction);
        end;
        cekSetter:
        begin
          SetterFunction := BuildClassSetter(Elem.SetterNode);
          SetterFunction.SetInferredName(
            FunctionNameFromPropertyKey(ComputedKey, 'set'));
          if ComputedKey is TGocciaSymbolValue then
          begin
            if Elem.IsStatic then
              ExistingDescriptor := ClassValue.GetOwnStaticSymbolDescriptor(
                TGocciaSymbolValue(ComputedKey))
            else
              ExistingDescriptor :=
                ClassValue.Prototype.GetOwnSymbolPropertyDescriptor(
                  TGocciaSymbolValue(ComputedKey));

            if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
               Assigned(TGocciaPropertyDescriptorAccessor(
                 ExistingDescriptor).Getter) then
            begin
              if Elem.IsStatic then
                ClassValue.DefineSymbolProperty(
                  TGocciaSymbolValue(ComputedKey),
                  TGocciaPropertyDescriptorAccessor.Create(
                    TGocciaPropertyDescriptorAccessor(
                      ExistingDescriptor).Getter, SetterFunction,
                    [pfConfigurable]))
              else
                ClassValue.Prototype.DefineSymbolProperty(
                  TGocciaSymbolValue(ComputedKey),
                  TGocciaPropertyDescriptorAccessor.Create(
                    TGocciaPropertyDescriptorAccessor(
                      ExistingDescriptor).Getter, SetterFunction,
                    [pfConfigurable]));
            end
            else if Elem.IsStatic then
              ClassValue.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction,
                  [pfConfigurable]))
            else
              ClassValue.Prototype.DefineSymbolProperty(
                TGocciaSymbolValue(ComputedKey),
                TGocciaPropertyDescriptorAccessor.Create(nil, SetterFunction,
                  [pfConfigurable]));
          end
          else if Elem.IsStatic then
            ClassValue.AddStaticSetter(ComputedKey.ToStringLiteral.Value,
              SetterFunction)
          else
            ClassValue.AddSetter(ComputedKey.ToStringLiteral.Value,
              SetterFunction);
        end;
      end;
    end;
  end;
begin
  Continuation := CurrentGeneratorContinuation;
  SuperClass := nil;
  SuperClassValue := nil;
  MethodSuperClass := nil;
  if Assigned(AClassDef.SuperClassExpression) then
  begin
    HeritageContext := AContext;
    HeritageContext.NonStrictMode := False;
    SuperClassValue := EvaluateExpression(AClassDef.SuperClassExpression,
      HeritageContext);
    if SuperClassValue is TGocciaClassValue then
    begin
      SuperClass := TGocciaClassValue(SuperClassValue);
      MethodSuperClass := SuperClass;
    end
    else if not ((SuperClassValue is TGocciaObjectValue) and
       SuperClassValue.IsConstructable) then
      ThrowTypeError(Format('Superclass expression is not a constructor (found %s)',
        [SuperClassValue.TypeName]), SSuggestNotConstructorType)
    else
      MethodSuperClass := SuperClassValue;
  end
  else if AClassDef.SuperClass <> '' then
  begin
    SuperClassValue := AContext.Scope.GetValue(AClassDef.SuperClass);
    if SuperClassValue = nil then
      AContext.OnError(Format('Superclass "%s" not found', [AClassDef.SuperClass]), ALine, AColumn)
    else if SuperClassValue is TGocciaClassValue then
    begin
      SuperClass := TGocciaClassValue(SuperClassValue);
      MethodSuperClass := SuperClass;
    end
    else if not ((SuperClassValue is TGocciaObjectValue) and SuperClassValue.IsConstructable) then
      ThrowTypeError(Format('Superclass "%s" is not a constructor (found %s)',
        [AClassDef.SuperClass, SuperClassValue.TypeName]),
        SSuggestNotConstructorType)
    else
      MethodSuperClass := SuperClassValue;
  end;

  // Use the class name if provided, otherwise create an anonymous class
  if AClassDef.Name <> '' then
    ClassName := AClassDef.Name
  else
    ClassName := '<anonymous>';

  ClassValue := TGocciaClassValue.Create(ClassName, SuperClass);
  if (SuperClass = nil) and (SuperClassValue is TGocciaObjectValue) and
     SuperClassValue.IsConstructable then
  begin
    ClassValue.LinkNativeSuperConstructor(
      TGocciaObjectValue(SuperClassValue));
    SuperPrototype := SuperClassValue.GetProperty(PROP_PROTOTYPE);
    if SuperPrototype is TGocciaNullLiteralValue then
      ClassValue.Prototype.Prototype := nil
    else if SuperPrototype is TGocciaObjectValue then
      ClassValue.Prototype.Prototype := TGocciaObjectValue(SuperPrototype)
    else
      ThrowTypeError(
        'Superclass prototype must be an object or null',
        'set the superclass prototype property to an object or null');
  end;
  // ES §14.3.7: constructor property is non-enumerable
  ClassValue.Prototype.DefineProperty(PROP_CONSTRUCTOR,
    TGocciaPropertyDescriptorData.Create(ClassValue, [pfConfigurable, pfWritable]));

  if Length(AClassDef.FElements) > 0 then
    EvaluateClassCallableElements
  else
  begin
  for MethodPair in AClassDef.Methods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, AContext, MethodSuperClass));
    Method.OwningClass := ClassValue;

    ClassValue.AddMethod(MethodPair.Key, Method);
  end;

  for MethodPair in AClassDef.StaticMethods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, AContext, MethodSuperClass));
    Method.OwningClass := ClassValue;

    ClassValue.DefineProperty(MethodPair.Key,
      TGocciaPropertyDescriptorData.Create(Method, [pfConfigurable, pfWritable]));
  end;
  end;

  // Static fields without FElements entries (legacy / no static blocks)
  if Length(AClassDef.FElements) = 0 then
  begin
    for PropertyPair in AClassDef.StaticProperties do
    begin
      PropertyValue := EvaluateExpression(PropertyPair.Value, AContext);
      ClassValue.DefineProperty(PropertyPair.Key,
        TGocciaPropertyDescriptorData.Create(PropertyValue,
          [pfEnumerable, pfConfigurable, pfWritable]));
    end;

    for PropertyPair in AClassDef.PrivateStaticProperties do
    begin
      PropertyValue := EvaluateExpression(PropertyPair.Value, AContext);
      ClassValue.AddPrivateStaticProperty(PropertyPair.Key, PropertyValue);
    end;
  end;

  // Store instance property definitions on the class in declaration order
  for I := 0 to AClassDef.InstanceProperties.Count - 1 do
  begin
    PropertyEntry := AClassDef.InstanceProperties.EntryAt(I);
    ClassValue.AddInstanceProperty(PropertyEntry.Key, PropertyEntry.Value);
  end;

  // Store private instance property definitions on the class in declaration order
  for I := 0 to AClassDef.PrivateInstanceProperties.Count - 1 do
  begin
    PropertyEntry := AClassDef.PrivateInstanceProperties.EntryAt(I);
    ClassValue.AddPrivateInstanceProperty(PropertyEntry.Key, PropertyEntry.Value);
  end;

  if Length(AClassDef.FElements) = 0 then
  begin
  for MethodPair in AClassDef.PrivateMethods do
  begin
    Method := TGocciaMethodValue(EvaluateClassMethod(MethodPair.Value, AContext, MethodSuperClass));
    Method.OwningClass := ClassValue;
    if MethodPair.Value.IsStatic then
      ClassValue.AddPrivateStaticMethod(MethodPair.Key, Method)
    else
      ClassValue.AddPrivateMethod(MethodPair.Key, Method);
  end;

  // Handle getters and setters

  for GetterPair in AClassDef.Getters do
  begin
    GetterFunction := BuildClassGetter(GetterPair.Value);
    GetterFunction.SetInferredName('get ' + GetterPair.Key);
    if (Length(GetterPair.Key) > 0) and (GetterPair.Key[1] = '#') then
      ClassValue.AddPrivateGetter(Copy(GetterPair.Key, 2, Length(GetterPair.Key) - 1), GetterFunction)
    else
      ClassValue.AddGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in AClassDef.Setters do
  begin
    SetterFunction := BuildClassSetter(SetterPair.Value);
    SetterFunction.SetInferredName('set ' + SetterPair.Key);
    if (Length(SetterPair.Key) > 0) and (SetterPair.Key[1] = '#') then
      ClassValue.AddPrivateSetter(Copy(SetterPair.Key, 2, Length(SetterPair.Key) - 1), SetterFunction)
    else
      ClassValue.AddSetter(SetterPair.Key, SetterFunction);
  end;

  for GetterPair in AClassDef.FStaticGetters do
  begin
    GetterFunction := BuildClassGetter(GetterPair.Value);
    GetterFunction.SetInferredName('get ' + GetterPair.Key);
    if (GetterPair.Key <> '') and (GetterPair.Key[1] = '#') then
      ClassValue.AddPrivateGetter(Copy(GetterPair.Key, 2, Length(GetterPair.Key) - 1), GetterFunction)
    else
      ClassValue.AddStaticGetter(GetterPair.Key, GetterFunction);
  end;

  for SetterPair in AClassDef.FStaticSetters do
  begin
    SetterFunction := BuildClassSetter(SetterPair.Value);
    SetterFunction.SetInferredName('set ' + SetterPair.Key);
    if (SetterPair.Key <> '') and (SetterPair.Key[1] = '#') then
      ClassValue.AddPrivateSetter(Copy(SetterPair.Key, 2, Length(SetterPair.Key) - 1), SetterFunction)
    else
      ClassValue.AddStaticSetter(SetterPair.Key, SetterFunction);
  end;
  end;

  // Copy field order for source-order initialization after computed names are resolved.
  if Length(AClassDef.FFieldOrder) > 0 then
  begin
    SetLength(FieldOrderEntries, Length(AClassDef.FFieldOrder));
    for I := 0 to High(AClassDef.FFieldOrder) do
    begin
      FieldOrderEntries[I].Name := AClassDef.FFieldOrder[I].Name;
      FieldOrderEntries[I].IsPrivate := AClassDef.FFieldOrder[I].IsPrivate;
      FieldOrderEntries[I].IsComputed := AClassDef.FFieldOrder[I].IsComputed;
      FieldOrderEntries[I].Initializer := AClassDef.FFieldOrder[I].FieldInitializer;
      FieldOrderEntries[I].ComputedKey := nil;
      if FieldOrderEntries[I].IsComputed and
         (AClassDef.FFieldOrder[I].ElementIndex >= 0) and
         (AClassDef.FFieldOrder[I].ElementIndex <= High(ResolvedComputedElementKeys)) then
        FieldOrderEntries[I].ComputedKey :=
          ResolvedComputedElementKeys[AClassDef.FFieldOrder[I].ElementIndex];
    end;
    ClassValue.SetFieldOrder(FieldOrderEntries);
  end;

  // TC39 proposal-decorators §3.1 ClassDefinitionEvaluation — auto-accessor setup
  for I := 0 to High(AClassDef.FElements) do
  begin
    if AClassDef.FElements[I].Kind = cekAccessor then
    begin
      Elem := AClassDef.FElements[I];
      ComputedKey := nil;
      AccessorBackingName := '__accessor_' + Elem.Name;
      if Elem.IsComputed then
      begin
        if I <= High(ResolvedComputedElementKeys) then
          ComputedKey := ResolvedComputedElementKeys[I];
        if not Assigned(ComputedKey) then
          ComputedKey := ToPropertyKey(EvaluateExpression(
            Elem.ComputedKeyExpression, AContext));
        AccessorBackingName := '__accessor_computed_' + IntToStr(I);
      end;

      if Assigned(Elem.FieldInitializer) then
        ClassValue.AddInstanceProperty(AccessorBackingName, Elem.FieldInitializer);

      if Elem.IsComputed then
        ClassValue.AddAutoAccessorWithKey(
          Elem.Name, ComputedKey, AccessorBackingName, Elem.IsStatic)
      else
        ClassValue.AddAutoAccessor(Elem.Name, AccessorBackingName, Elem.IsStatic);
    end;
  end;

  // ES2022 §15.7.14: evaluate static fields and static blocks in source order
  for I := 0 to High(AClassDef.FElements) do
  begin
    Elem := AClassDef.FElements[I];
    if Elem.Kind = cekStaticBlock then
      ExecuteStaticBlock(Elem.StaticBlockBody, AContext, ClassValue)
    else if (Elem.Kind = cekField) and Elem.IsStatic then
    begin
      if Assigned(Elem.FieldInitializer) then
      begin
        StaticFieldContext := AContext;
        StaticFieldScope := TGocciaClassInitScope.Create(AContext.Scope,
          ClassValue);
        StaticFieldScope.ThisValue := ClassValue;
        StaticFieldContext.Scope := StaticFieldScope;
        PropertyValue := EvaluateExpression(Elem.FieldInitializer,
          StaticFieldContext);
      end
      else
        PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Elem.IsPrivate then
        ClassValue.AddPrivateStaticProperty(Elem.Name, PropertyValue)
      else if Elem.IsComputed then
      begin
        ComputedKey := nil;
        if I <= High(ResolvedComputedElementKeys) then
          ComputedKey := ResolvedComputedElementKeys[I];
        if not Assigned(ComputedKey) then
          ComputedKey := ToPropertyKey(EvaluateExpression(
            Elem.ComputedKeyExpression, AContext));
        if ComputedKey is TGocciaSymbolValue then
          ClassValue.DefineSymbolProperty(
            TGocciaSymbolValue(ComputedKey),
            TGocciaPropertyDescriptorData.Create(PropertyValue,
              [pfEnumerable, pfConfigurable, pfWritable]))
        else
          ClassValue.DefineProperty(ComputedKey.ToStringLiteral.Value,
            TGocciaPropertyDescriptorData.Create(PropertyValue,
              [pfEnumerable, pfConfigurable, pfWritable]));
      end
      else
        ClassValue.DefineProperty(Elem.Name,
          TGocciaPropertyDescriptorData.Create(PropertyValue,
            [pfEnumerable, pfConfigurable, pfWritable]));
    end;
  end;

  // TC39 proposal-decorators §3.2 ApplyDecoratorsToClassDefinition
  HasDecorators := Length(AClassDef.FDecorators) > 0;
  if not HasDecorators then
    for I := 0 to High(AClassDef.FElements) do
      if (Length(AClassDef.FElements[I].Decorators) > 0) or
         (AClassDef.FElements[I].Kind = cekAccessor) then
      begin
        HasDecorators := True;
        Break;
      end;

  if HasDecorators then
  begin
    // Create metadata object; inherit from superclass metadata if present
    SuperMetadata := nil;
    if Assigned(SuperClass) then
      SuperMetadata := SuperClass.GetSymbolProperty(TGocciaSymbolValue.WellKnownMetadata);
    if (SuperMetadata <> nil) and (SuperMetadata is TGocciaObjectValue) then
      MetadataObject := TGocciaObjectValue.Create(TGocciaObjectValue(SuperMetadata))
    else
      MetadataObject := TGocciaObjectValue.Create;
    TGarbageCollector.Instance.AddTempRoot(MetadataObject);

    MethodCollector := TGocciaInitializerCollector.Create;
    FieldCollector := TGocciaInitializerCollector.Create;
    StaticFieldCollector := TGocciaInitializerCollector.Create;
    ClassCollector := TGocciaInitializerCollector.Create;
    try

    // Phase 1: Evaluate all decorator expressions in source order
    SetLength(EvaluatedElementDecorators, Length(AClassDef.FElements));
    for I := 0 to High(AClassDef.FElements) do
    begin
      SetLength(EvaluatedElementDecorators[I], Length(AClassDef.FElements[I].Decorators));
      for J := 0 to High(AClassDef.FElements[I].Decorators) do
        EvaluatedElementDecorators[I][J] := EvaluateExpression(AClassDef.FElements[I].Decorators[J], AContext);
    end;

    SetLength(EvaluatedClassDecorators, Length(AClassDef.FDecorators));
    for I := 0 to High(AClassDef.FDecorators) do
      EvaluatedClassDecorators[I] := EvaluateExpression(AClassDef.FDecorators[I], AContext);

    // Phase 2: Call element decorators (applied per-element, bottom-up within each element)

    for I := 0 to High(AClassDef.FElements) do
    begin
      Elem := AClassDef.FElements[I];
      if Length(EvaluatedElementDecorators[I]) = 0 then
        Continue;

      for J := High(EvaluatedElementDecorators[I]) downto 0 do
      begin
        DecoratorFn := EvaluatedElementDecorators[I][J];
        if not DecoratorFn.IsCallable then
          ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

        // Build context object
        ContextObject := TGocciaObjectValue.Create;

        case Elem.Kind of
          cekMethod: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('method'));
          cekGetter: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('getter'));
          cekSetter: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('setter'));
          cekField: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('field'));
          cekAccessor: ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('accessor'));
        end;

        ElementName := Elem.Name;
        ElementKey := nil;
        if (not Elem.IsPrivate) and Elem.IsComputed and
           (I <= High(ResolvedComputedElementKeys)) then
        begin
          ElementKey := ResolvedComputedElementKeys[I];
          if ElementKey is TGocciaSymbolValue then
            ContextObject.AssignProperty(PROP_NAME, ElementKey)
          else if Assigned(ElementKey) then
          begin
            ElementName := ElementKey.ToStringLiteral.Value;
            ContextObject.AssignProperty(PROP_NAME,
              TGocciaStringLiteralValue.Create(ElementName));
          end
          else
            ContextObject.AssignProperty(PROP_NAME,
              TGocciaStringLiteralValue.Create(Elem.Name));
        end
        else if Elem.IsPrivate then
          ContextObject.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create('#' + Elem.Name))
        else
          ContextObject.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(Elem.Name));

        if Elem.IsStatic then
          ContextObject.AssignProperty(PROP_STATIC, TGocciaBooleanLiteralValue.TrueValue)
        else
          ContextObject.AssignProperty(PROP_STATIC, TGocciaBooleanLiteralValue.FalseValue);
        if Elem.IsPrivate then
          ContextObject.AssignProperty(PROP_PRIVATE, TGocciaBooleanLiteralValue.TrueValue)
        else
          ContextObject.AssignProperty(PROP_PRIVATE, TGocciaBooleanLiteralValue.FalseValue);
        ContextObject.AssignProperty(PROP_METADATA, MetadataObject);

        // Build access object
        AccessObject := TGocciaObjectValue.Create;

        case Elem.Kind of
          cekMethod:
          begin
            if Elem.IsPrivate then
              CurrentMethod := GetPrivateMethodValue(
                ElementName, Elem.IsStatic)
            else
              CurrentMethod := GetDecoratedDataProperty(
                Elem.IsStatic, ElementName, ElementKey);

            if ElementKey is TGocciaSymbolValue then
              AccessGetterHelper := TGocciaAccessGetter.CreateWithKey(
                CurrentMethod, ElementKey)
            else
              AccessGetterHelper := TGocciaAccessGetter.Create(
                CurrentMethod, ElementName);
            AccessObject.AssignProperty(PROP_GET,
              TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessGetterHelper.Get, PROP_GET, 0));
            ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
          end;
          cekGetter:
          begin
            if not Elem.IsPrivate then
            begin
              if ElementKey is TGocciaSymbolValue then
                AccessGetterHelper := TGocciaAccessGetter.CreateWithKey(
                  nil, ElementKey)
              else
                AccessGetterHelper := TGocciaAccessGetter.Create(
                  nil, ElementName);
              AccessObject.AssignProperty(PROP_GET,
                TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessGetterHelper.Get, PROP_GET, 0));
              ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
            end;
          end;
          cekSetter:
          begin
            if not Elem.IsPrivate then
            begin
              if ElementKey is TGocciaSymbolValue then
                AccessSetterHelper := TGocciaAccessSetter.CreateWithKey(
                  ElementKey)
              else
                AccessSetterHelper := TGocciaAccessSetter.Create(ElementName);
              AccessObject.AssignProperty(PROP_SET,
                TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessSetterHelper.SetValue, PROP_SET, 1));
              ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
            end;
          end;
          cekField, cekAccessor:
          begin
            if ElementKey is TGocciaSymbolValue then
            begin
              AccessGetterHelper := TGocciaAccessGetter.CreateWithKey(
                nil, ElementKey);
              AccessSetterHelper := TGocciaAccessSetter.CreateWithKey(
                ElementKey);
            end
            else
            begin
              AccessGetterHelper := TGocciaAccessGetter.Create(
                nil, ElementName);
              AccessSetterHelper := TGocciaAccessSetter.Create(ElementName);
            end;
            AccessObject.AssignProperty(PROP_GET,
              TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessGetterHelper.Get, PROP_GET, 0));
            AccessObject.AssignProperty(PROP_SET,
              TGocciaNativeFunctionValue.CreateWithoutPrototype(AccessSetterHelper.SetValue, PROP_SET, 1));
            ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
          end;
        end;

        // addInitializer - use appropriate collector based on element kind
        if Elem.IsStatic then
          ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(StaticFieldCollector.AddInitializer, PROP_ADD_INITIALIZER, 1))
        else if Elem.Kind in [cekField, cekAccessor] then
          ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(FieldCollector.AddInitializer, PROP_ADD_INITIALIZER, 1))
        else
          ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(MethodCollector.AddInitializer, PROP_ADD_INITIALIZER, 1));

        // Call the decorator
        DecoratorArgs := TGocciaArgumentsCollection.Create;
        try
          case Elem.Kind of
            cekMethod:
            begin
              if Elem.IsPrivate then
                DecoratorArgs.Add(GetPrivateMethodValue(
                  ElementName, Elem.IsStatic))
              else
                DecoratorArgs.Add(GetDecoratedDataProperty(
                  Elem.IsStatic, ElementName, ElementKey));
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Method decorator must return a function or undefined', ALine, AColumn);

                if Elem.IsPrivate then
                  DefinePrivateMethodValue(
                    ElementName, Elem.IsStatic, DecoratorResult)
                else
                  DefineDecoratedDataProperty(
                    Elem.IsStatic, ElementName, ElementKey, DecoratorResult);
              end;
            end;

            cekGetter:
            begin
              if Elem.IsPrivate then
                GetterFnValue := ClassValue.PrivatePropertyGetter[ElementName]
              else
              begin
                ExistingDescriptor := GetDecoratedAccessorDescriptor(
                  Elem.IsStatic, ElementName, ElementKey);
                GetterFnValue := nil;
                if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                   Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
                  GetterFnValue :=
                    TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
              end;

              DecoratorArgs.Add(GetterFnValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Getter decorator must return a function or undefined', ALine, AColumn);

                if Elem.IsPrivate then
                  ClassValue.AddPrivateGetter(ElementName, TGocciaFunctionValue(DecoratorResult))
                else
                begin
                  ExistingDescriptor := GetDecoratedAccessorDescriptor(
                    Elem.IsStatic, ElementName, ElementKey);
                  ExistingSetterValue := nil;
                  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
                    ExistingSetterValue :=
                      TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
                  DefineDecoratedAccessorProperty(
                    Elem.IsStatic, ElementName, ElementKey,
                    DecoratorResult, ExistingSetterValue);
                end;
              end;
            end;

            cekSetter:
            begin
              if Elem.IsPrivate then
                SetterFnValue := ClassValue.PrivatePropertySetter[ElementName]
              else
              begin
                ExistingDescriptor := GetDecoratedAccessorDescriptor(
                  Elem.IsStatic, ElementName, ElementKey);
                SetterFnValue := nil;
                if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                   Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
                  SetterFnValue :=
                    TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              end;

              DecoratorArgs.Add(SetterFnValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Setter decorator must return a function or undefined', ALine, AColumn);

                if Elem.IsPrivate then
                  ClassValue.AddPrivateSetter(ElementName, TGocciaFunctionValue(DecoratorResult))
                else
                begin
                  ExistingDescriptor := GetDecoratedAccessorDescriptor(
                    Elem.IsStatic, ElementName, ElementKey);
                  ExistingGetterValue := nil;
                  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
                     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
                    ExistingGetterValue :=
                      TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
                  DefineDecoratedAccessorProperty(
                    Elem.IsStatic, ElementName, ElementKey,
                    ExistingGetterValue, DecoratorResult);
                end;
              end;
            end;

            cekField:
            begin
              DecoratorArgs.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not DecoratorResult.IsCallable then
                  AContext.OnError('Field decorator must return a function or undefined', ALine, AColumn);
                ClassValue.AddFieldInitializerWithKey(ElementName, ElementKey, DecoratorResult, Elem.IsPrivate, Elem.IsStatic);
              end;
            end;

            cekAccessor:
            begin
              AutoAccessorValue := TGocciaObjectValue.Create;
              ExistingDescriptor := GetDecoratedAccessorDescriptor(
                Elem.IsStatic, ElementName, ElementKey);
              ExistingGetterValue := nil;
              ExistingSetterValue := nil;
              if ExistingDescriptor is TGocciaPropertyDescriptorAccessor then
              begin
                ExistingGetterValue :=
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
                ExistingSetterValue :=
                  TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
              end;
              AutoAccessorValue.AssignProperty(PROP_GET, ExistingGetterValue);
              AutoAccessorValue.AssignProperty(PROP_SET, ExistingSetterValue);

              DecoratorArgs.Add(AutoAccessorValue);
              DecoratorArgs.Add(ContextObject);

              DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

              if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
              begin
                if not (DecoratorResult is TGocciaObjectValue) then
                  AContext.OnError('Accessor decorator must return an object or undefined', ALine, AColumn);
                DecResultObj := TGocciaObjectValue(DecoratorResult);
                NewGetter := DecResultObj.GetProperty(PROP_GET);
                NewSetter := DecResultObj.GetProperty(PROP_SET);
                NewInit := DecResultObj.GetProperty(PROP_INIT);

                if Assigned(NewGetter) and not (NewGetter is TGocciaUndefinedLiteralValue) then
                  ExistingGetterValue := NewGetter;
                if Assigned(NewSetter) and not (NewSetter is TGocciaUndefinedLiteralValue) then
                  ExistingSetterValue := NewSetter;
                if (Assigned(NewGetter) and not (NewGetter is TGocciaUndefinedLiteralValue)) or
                   (Assigned(NewSetter) and not (NewSetter is TGocciaUndefinedLiteralValue)) then
                  DefineDecoratedAccessorProperty(
                    Elem.IsStatic, ElementName, ElementKey,
                    ExistingGetterValue, ExistingSetterValue);
                if Assigned(NewInit) and not (NewInit is TGocciaUndefinedLiteralValue) and NewInit.IsCallable then
                  ClassValue.AddFieldInitializerWithKey(ElementName, ElementKey, NewInit, Elem.IsPrivate, Elem.IsStatic);
              end;
            end;
          end;
        finally
          DecoratorArgs.Free;
        end;
      end;
    end;

    OriginalClassValue := ClassValue;
    OriginalClassValue.RunDecoratorStaticFieldInitializers;

    // Phase 3: Call class decorators (bottom-up)
    for I := High(EvaluatedClassDecorators) downto 0 do
    begin
      DecoratorFn := EvaluatedClassDecorators[I];
      if not DecoratorFn.IsCallable then
        ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

      ContextObject := TGocciaObjectValue.Create;
      ContextObject.AssignProperty(PROP_KIND, TGocciaStringLiteralValue.Create('class'));
      if AClassDef.Name <> '' then
        ContextObject.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AClassDef.Name))
      else
        ContextObject.AssignProperty(PROP_NAME, TGocciaUndefinedLiteralValue.UndefinedValue);
      ContextObject.AssignProperty(PROP_METADATA, MetadataObject);
      ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
        TGocciaNativeFunctionValue.CreateWithoutPrototype(ClassCollector.AddInitializer, PROP_ADD_INITIALIZER, 1));

      DecoratorArgs := TGocciaArgumentsCollection.Create([ClassValue, ContextObject]);
      try
        DecoratorResult := TGocciaFunctionBase(DecoratorFn).Call(DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);

        if (DecoratorResult <> nil) and not (DecoratorResult is TGocciaUndefinedLiteralValue) then
        begin
          if not (DecoratorResult is TGocciaClassValue) then
            AContext.OnError('Class decorator must return a class or undefined', ALine, AColumn);
          ClassValue := TGocciaClassValue(DecoratorResult);
        end;
      finally
        DecoratorArgs.Free;
      end;
    end;

    // Assign Symbol.metadata on the class
    ClassValue.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata,
      TGocciaPropertyDescriptorData.Create(MetadataObject, [pfConfigurable]));

    // Store initializer lists on the class for execution during instantiation
    InitializerResults := MethodCollector.GetInitializers;
    ClassValue.SetMethodInitializers(InitializerResults);
    InitializerResults := FieldCollector.GetInitializers;
    ClassValue.SetFieldInitializers(InitializerResults);

    // Run class-level initializers after static fields
    InitializerResults := ClassCollector.GetInitializers;
    for I := 0 to High(InitializerResults) do
    begin
      InitArgs := TGocciaArgumentsCollection.Create;
      try
        TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassValue);
      finally
        InitArgs.Free;
      end;
    end;

    // Run static field initializers
    InitializerResults := StaticFieldCollector.GetInitializers;
    for I := 0 to High(InitializerResults) do
    begin
      InitArgs := TGocciaArgumentsCollection.Create;
      try
        TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassValue);
      finally
        InitArgs.Free;
      end;
    end;

    finally
      TGarbageCollector.Instance.RemoveTempRoot(MetadataObject);
      MethodCollector.Free;
      FieldCollector.Free;
      StaticFieldCollector.Free;
      ClassCollector.Free;
    end;
  end;

  if Assigned(Continuation) then
    for I := 0 to High(AClassDef.FElements) do
      if AClassDef.FElements[I].IsComputed and
         (AClassDef.FElements[I].Kind in [cekMethod, cekGetter, cekSetter, cekField, cekAccessor]) then
        Continuation.ClearCompletedExpressionValue(
          AClassDef.FElements[I].ComputedKeyExpression);

  Result := ClassValue;
end;

function ResolveLexicalPrivateAccessClass(
  const AContext: TGocciaEvaluationContext;
  const APrivateName: string): TGocciaClassValue;
var
  OwningClassValue: TGocciaValue;
  LastOwningClass: TGocciaValue;
  CandidateClass: TGocciaClassValue;
begin
  Result := nil;
  LastOwningClass := nil;
  repeat
    OwningClassValue := AContext.Scope.FindOwningClassAfter(LastOwningClass);
    if not (OwningClassValue is TGocciaClassValue) then
      Exit;

    CandidateClass := TGocciaClassValue(OwningClassValue);
    if CandidateClass.HasOwnPrivateName(APrivateName) then
      Exit(CandidateClass);

    LastOwningClass := OwningClassValue;
  until False;
end;

function ResolveOwningClass(const AInstance: TGocciaInstanceValue;
  const AContext: TGocciaEvaluationContext;
  const APrivateName: string): TGocciaClassValue;
begin
  Result := ResolveLexicalPrivateAccessClass(AContext, APrivateName);
  if Assigned(Result) then
  begin
    Exit;
  end;
  // Fallback: use the instance's class
  Result := AInstance.ClassValue;
end;

function ResolveLexicalOwningClass(
  const AContext: TGocciaEvaluationContext): TGocciaClassValue;
var
  OwningClassValue: TGocciaValue;
begin
  Result := nil;
  OwningClassValue := AContext.Scope.FindOwningClass;
  if OwningClassValue is TGocciaClassValue then
    Result := TGocciaClassValue(OwningClassValue);
end;

function CollectDeclaredPrivateNames(const AContext: TGocciaEvaluationContext): TStringList;
var
  OwningClassValue: TGocciaClassValue;
  LastOwningClass: TGocciaValue;
  CandidateClassValue: TGocciaValue;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  Result.Sorted := False;
  Result.Duplicates := dupIgnore;

  LastOwningClass := nil;
  repeat
    CandidateClassValue := AContext.Scope.FindOwningClassAfter(LastOwningClass);
    if not (CandidateClassValue is TGocciaClassValue) then
      Exit;
    OwningClassValue := TGocciaClassValue(CandidateClassValue);
    OwningClassValue.AppendOwnPrivateNames(Result);
    LastOwningClass := CandidateClassValue;
  until False;
end;

procedure ThrowPrivateGetterMissingError(const APrivateName: string);
begin
  ThrowTypeError(Format(SErrorPrivateGetterMissing,
    [APrivateName]), SSuggestPrivateFieldAccess);
end;

procedure ThrowPrivateSetterMissingError(const APrivateName: string);
begin
  ThrowTypeError(Format(SErrorPrivateSetterMissing,
    [APrivateName]), SSuggestPrivateFieldAccess);
end;

procedure ThrowPrivateBrandError(const APrivateName: string;
  const AAccessClass: TGocciaClassValue = nil);
begin
  if Assigned(AAccessClass) then
    ThrowTypeErrorInRealm(
      Format(SErrorPrivateFieldInaccessible, [APrivateName]),
      SSuggestPrivateFieldAccess,
      AAccessClass.CreationRealm)
  else
    ThrowTypeError(Format(SErrorPrivateFieldInaccessible, [APrivateName]),
      SSuggestPrivateFieldAccess);
end;

function PrivateLookupName(const AAccessClass: TGocciaClassValue;
  const APrivateName: string): string;
var
  InternalKey: string;
begin
  Result := APrivateName;
  if Assigned(AAccessClass) and
     AAccessClass.ResolveDeclaredPrivateKey(APrivateName, InternalKey) then
    Result := InternalKey;
end;

function BytecodePrivateBrandKeyForLookup(const APrivateLookupName: string;
  const AAccessClass: TGocciaClassValue): string;
var
  Body: string;
  DelimiterPos: SizeInt;
  BrandToken: string;
begin
  Result := '';
  if Copy(APrivateLookupName, 1, Length('#slot:')) <> '#slot:' then
    Exit;

  BrandToken := '';
  Body := Copy(APrivateLookupName, Length('#slot:') + 1, MaxInt);
  DelimiterPos := Pos(':', Body);
  if DelimiterPos > 1 then
    BrandToken := Copy(Body, 1, DelimiterPos - 1)
  else if Assigned(AAccessClass) then
    BrandToken := AAccessClass.PrivateBrandToken
  else if (Body <> '') and (Pos('$', Body) = 0) then
    BrandToken := Body;
  if BrandToken <> '' then
    Result := '#brand:' + BrandToken + ':' + APrivateLookupName;
end;

procedure EnsureBytecodePrivateBrand(const AReceiver: TGocciaObjectValue;
  const APrivateName, APrivateLookupName: string;
  const AAccessClass: TGocciaClassValue);
var
  BrandKey: string;
  BrandValue: TGocciaValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  BrandKey := BytecodePrivateBrandKeyForLookup(APrivateLookupName,
    AAccessClass);
  if BrandKey = '' then
    ThrowPrivateBrandError(APrivateName, AAccessClass);

  if AReceiver is TGocciaInstanceValue then
  begin
    if not TGocciaInstanceValue(AReceiver).TryGetRawPrivateProperty(
      BrandKey, BrandValue) then
      ThrowPrivateBrandError(APrivateName, AAccessClass);
    Exit;
  end;

  if not (AReceiver.Properties.TryGetValue(BrandKey, Descriptor) and
     (Descriptor is TGocciaPropertyDescriptorData)) then
    ThrowPrivateBrandError(APrivateName, AAccessClass);
end;

procedure EnsurePrivateStaticBrand(const AReceiver,
  AAccessClass: TGocciaClassValue; const APrivateName: string);
begin
  if AReceiver <> AAccessClass then
    ThrowPrivateBrandError(APrivateName, AAccessClass);
end;

function RawPrivateBrandKey(const AAccessClass: TGocciaClassValue): string;
begin
  Result := '#brand:' + AAccessClass.PrivateBrandToken;
end;

function RawPrivateInstanceKey(const AAccessClass: TGocciaClassValue;
  const APrivateName: string): string;
begin
  Result := '#slot:' + AAccessClass.PrivateBrandToken + ':' + APrivateName;
end;

function RawPrivateInitializedKey(const AAccessClass: TGocciaClassValue): string;
begin
  Result := '#initialized:' + AAccessClass.PrivateBrandToken;
end;

function TryGetRawObjectPrivateDescriptor(const AReceiver: TGocciaObjectValue;
  const AKey: string; out ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  ADescriptor := nil;
  Result := Assigned(AReceiver) and
    AReceiver.Properties.TryGetValue(AKey, ADescriptor);
end;

procedure DefineRawObjectPrivateDataProperty(
  const AReceiver: TGocciaObjectValue; const AKey: string;
  const AValue: TGocciaValue; const AFlags: TPropertyFlags);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
begin
  if not Assigned(AReceiver) then
    Exit;
  if (not AReceiver.Properties.TryGetValue(AKey, ExistingDescriptor)) and
     (not AReceiver.Extensible) then
    ThrowTypeError('Cannot add private elements to a non-extensible object',
      SSuggestObjectNotExtensible);
  if Assigned(ExistingDescriptor) then
    ExistingDescriptor.Free;
  AReceiver.Properties.Add(AKey,
    TGocciaPropertyDescriptorData.Create(AValue, AFlags));
end;

function HasRawPrivateInstanceInitializersApplied(
  const AReceiver: TGocciaObjectValue;
  const AAccessClass: TGocciaClassValue): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := False;
  if not Assigned(AAccessClass) then
    Exit;
  TryGetRawObjectPrivateDescriptor(AReceiver,
    RawPrivateInitializedKey(AAccessClass), Descriptor);
  Result := Descriptor is TGocciaPropertyDescriptorData;
end;

procedure StampRawPrivateInstanceInitializersApplied(
  const AReceiver: TGocciaObjectValue;
  const AAccessClass: TGocciaClassValue);
begin
  if not Assigned(AAccessClass) then
    Exit;
  if not AAccessClass.HasInstanceInitializerWork then
    Exit;
  if HasRawPrivateInstanceInitializersApplied(AReceiver, AAccessClass) then
    Exit;
  DefineRawObjectPrivateDataProperty(AReceiver,
    RawPrivateInitializedKey(AAccessClass),
    TGocciaBooleanLiteralValue.TrueValue, []);
end;

function HasRawPrivateInstanceBrand(const AReceiver: TGocciaObjectValue;
  const AAccessClass: TGocciaClassValue): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := False;
  if not Assigned(AAccessClass) then
    Exit;
  TryGetRawObjectPrivateDescriptor(AReceiver,
    RawPrivateBrandKey(AAccessClass), Descriptor);
  Result := Descriptor is TGocciaPropertyDescriptorData;
end;

function EvaluatePrivateInOperator(
  const APrivateMemberExpression: TGocciaPrivateMemberExpression;
  const ARightExpression: TGocciaExpression;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  RightValue: TGocciaValue;
  AccessClass: TGocciaClassValue;
begin
  RightValue := EvaluateExpression(ARightExpression, AContext);
  if RightValue.IsPrimitive then
    ThrowTypeError(Format(SErrorCannotUseInOperator,
      ['#' + APrivateMemberExpression.PrivateName,
       RightValue.ToStringLiteral.Value]),
      SSuggestCheckNullBeforeAccess);

  if not (RightValue is TGocciaObjectValue) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  AccessClass := ResolveLexicalPrivateAccessClass(AContext,
    APrivateMemberExpression.PrivateName);
  if not Assigned(AccessClass) then
    ThrowPrivateBrandError(APrivateMemberExpression.PrivateName);

  Result := TGocciaBooleanLiteralValue.FromBoolean(
    HasRawPrivateInstanceBrand(TGocciaObjectValue(RightValue), AccessClass));
end;

procedure EnsureRawPrivateInstanceBrand(const AReceiver: TGocciaObjectValue;
  const APrivateName: string; const AAccessClass: TGocciaClassValue);
begin
  if (not Assigned(AAccessClass)) or
     not HasRawPrivateInstanceBrand(AReceiver, AAccessClass) then
    ThrowPrivateBrandError(APrivateName, AAccessClass);
end;

procedure StampRawPrivateInstanceBrand(const AReceiver: TGocciaObjectValue;
  const AAccessClass: TGocciaClassValue);
begin
  if not Assigned(AAccessClass) then
    Exit;
  if HasRawPrivateInstanceBrand(AReceiver, AAccessClass) then
    Exit;
  DefineRawObjectPrivateDataProperty(AReceiver,
    RawPrivateBrandKey(AAccessClass),
    TGocciaBooleanLiteralValue.TrueValue, []);
end;

function TryGetRawPrivateInstanceProperty(const AReceiver: TGocciaObjectValue;
  const APrivateName: string; const AAccessClass: TGocciaClassValue;
  out AValue: TGocciaValue): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := False;
  AValue := nil;
  EnsureRawPrivateInstanceBrand(AReceiver, APrivateName, AAccessClass);
  TryGetRawObjectPrivateDescriptor(AReceiver,
    RawPrivateInstanceKey(AAccessClass, APrivateName), Descriptor);
  if Descriptor is TGocciaPropertyDescriptorData then
  begin
    AValue := TGocciaPropertyDescriptorData(Descriptor).Value;
    Result := True;
  end;
end;

procedure InitializeRawPrivateInstanceProperty(
  const AReceiver: TGocciaObjectValue; const APrivateName: string;
  const AValue: TGocciaValue; const AAccessClass: TGocciaClassValue;
  const AInitializationMode: TGocciaInstanceInitializationMode);
var
  PrivateSlotKey: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  if not Assigned(AAccessClass) then
    ThrowPrivateBrandError(APrivateName);
  StampRawPrivateInstanceBrand(AReceiver, AAccessClass);
  PrivateSlotKey := RawPrivateInstanceKey(AAccessClass, APrivateName);
  if TryGetRawObjectPrivateDescriptor(AReceiver, PrivateSlotKey,
     Descriptor) then
  begin
    if AInitializationMode = iimReplay then
      Exit;
    if (AInitializationMode = iimEagerReplacement) and
       not HasRawPrivateInstanceInitializersApplied(AReceiver,
         AAccessClass) then
    begin
      if not AReceiver.Extensible then
        ThrowTypeError('Cannot add private elements to a non-extensible object',
          SSuggestObjectNotExtensible);
    end
    else
      ThrowTypeError('Cannot initialize private elements twice',
        SSuggestPrivateFieldAccess);
  end;
  DefineRawObjectPrivateDataProperty(AReceiver, PrivateSlotKey, AValue,
    [pfWritable, pfConfigurable]);
end;

procedure SetRawPrivateInstanceProperty(const AReceiver: TGocciaObjectValue;
  const APrivateName: string; const AValue: TGocciaValue;
  const AAccessClass: TGocciaClassValue);
begin
  EnsureRawPrivateInstanceBrand(AReceiver, APrivateName, AAccessClass);
  DefineRawObjectPrivateDataProperty(AReceiver,
    RawPrivateInstanceKey(AAccessClass, APrivateName), AValue,
    [pfWritable, pfConfigurable]);
end;

// ES2026 §7.3.30 PrivateGet ( O, P )
function EvaluatePrivateMemberOnInstance(const AInstance: TGocciaInstanceValue; const APrivateName: string; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionBase;
  EmptyArgs: TGocciaArgumentsCollection;
  PrivateName: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AccessClass := ResolveOwningClass(AInstance, AContext, APrivateName);
  PrivateName := PrivateLookupName(AccessClass, APrivateName);

  // Check if this is a private getter
  if AccessClass.HasPrivateGetter(PrivateName) then
  begin
    if PrivateName <> APrivateName then
      EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
        AccessClass)
    else
      EnsureRawPrivateInstanceBrand(AInstance, APrivateName, AccessClass);
    GetterFn := AccessClass.PrivatePropertyGetter[PrivateName];
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      Result := GetterFn.Call(EmptyArgs, AInstance);
    finally
      EmptyArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasPrivateSetter(PrivateName) then
    ThrowPrivateGetterMissingError(APrivateName);

  // Check if this is a private method call
  if AccessClass.PrivateMethods.ContainsKey(PrivateName) then
  begin
    if PrivateName <> APrivateName then
      EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
        AccessClass)
    else
      EnsureRawPrivateInstanceBrand(AInstance, APrivateName, AccessClass);
    Result := AccessClass.GetPrivateMethod(PrivateName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else
  begin
    if (PrivateName <> APrivateName) and Assigned(AccessClass.Prototype) then
    begin
      Descriptor := AccessClass.Prototype.GetOwnPropertyDescriptor(PrivateName);
      if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
          AccessClass);
        if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
        begin
          EmptyArgs := TGocciaArgumentsCollection.Create;
          try
            Result := TGocciaFunctionBase(
              TGocciaPropertyDescriptorAccessor(Descriptor).Getter).Call(
                EmptyArgs, AInstance);
          finally
            EmptyArgs.Free;
          end;
          Exit;
        end;
        ThrowPrivateGetterMissingError(APrivateName);
      end
      else if Descriptor is TGocciaPropertyDescriptorData then
      begin
        EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
          AccessClass);
        Result := TGocciaPropertyDescriptorData(Descriptor).Value;
        Exit;
      end;
    end;

    if PrivateName <> APrivateName then
    begin
      EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
        AccessClass);
      if AInstance.TryGetRawPrivateProperty(PrivateName, Result) then
        Exit;
      Descriptor := AInstance.GetOwnPropertyDescriptor(PrivateName);
      if Descriptor is TGocciaPropertyDescriptorData then
      begin
        Result := TGocciaPropertyDescriptorData(Descriptor).Value;
        Exit;
      end;
    end;
    // It's a private property access
    Result := AInstance.GetPrivateProperty(APrivateName, AccessClass);
  end;
end;

function EvaluatePrivateMemberOnObject(const AReceiver: TGocciaObjectValue;
  const APrivateName: string; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionBase;
  EmptyArgs: TGocciaArgumentsCollection;
  PrivateName: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AccessClass := ResolveLexicalPrivateAccessClass(AContext, APrivateName);
  if not Assigned(AccessClass) then
    ThrowPrivateBrandError(APrivateName);
  PrivateName := PrivateLookupName(AccessClass, APrivateName);

  if PrivateName <> APrivateName then
    EnsureBytecodePrivateBrand(AReceiver, APrivateName, PrivateName,
      AccessClass)
  else
    EnsureRawPrivateInstanceBrand(AReceiver, APrivateName, AccessClass);

  if AccessClass.HasPrivateGetter(PrivateName) then
  begin
    GetterFn := AccessClass.PrivatePropertyGetter[PrivateName];
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      Result := GetterFn.Call(EmptyArgs, AReceiver);
    finally
      EmptyArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasPrivateSetter(PrivateName) then
    ThrowPrivateGetterMissingError(APrivateName);

  if AccessClass.PrivateMethods.ContainsKey(PrivateName) then
  begin
    Result := AccessClass.GetPrivateMethod(PrivateName);
    if Result = nil then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if PrivateName <> APrivateName then
  begin
    if (AReceiver is TGocciaInstanceValue) and
       TGocciaInstanceValue(AReceiver).TryGetRawPrivateProperty(
         PrivateName, Result) then
      Exit;
    TryGetRawObjectPrivateDescriptor(AReceiver, PrivateName, Descriptor);
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      Result := TGocciaPropertyDescriptorData(Descriptor).Value;
      Exit;
    end;
  end;

  if TryGetRawPrivateInstanceProperty(
    AReceiver, APrivateName, AccessClass, Result) then
    Exit;

  ThrowPrivateBrandError(APrivateName, AccessClass);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function EvaluatePrivateMemberOnClass(const AClassValue: TGocciaClassValue;
  const APrivateName: string; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  AccessClass: TGocciaClassValue;
  GetterFn: TGocciaFunctionBase;
  EmptyArgs: TGocciaArgumentsCollection;
  PrivateName: string;
begin
  AccessClass := ResolveLexicalPrivateAccessClass(AContext, APrivateName);
  if not Assigned(AccessClass) then
    AccessClass := AClassValue;
  PrivateName := PrivateLookupName(AccessClass, APrivateName);

  EnsurePrivateStaticBrand(AClassValue, AccessClass, APrivateName);

  if AccessClass.HasOwnPrivateGetter(PrivateName) then
  begin
    GetterFn := AccessClass.GetOwnPrivatePropertyGetter(PrivateName);
    EmptyArgs := TGocciaArgumentsCollection.Create;
    try
      Result := GetterFn.Call(EmptyArgs, AClassValue);
    finally
      EmptyArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasOwnPrivateSetter(PrivateName) then
    ThrowPrivateGetterMissingError(APrivateName);

  if AccessClass.PrivateStaticMethods.TryGetValue(PrivateName, Result) then
    Exit;

  if not AccessClass.PrivateStaticProperties.TryGetValue(PrivateName, Result) then
    ThrowPrivateBrandError(APrivateName, AccessClass);
end;

procedure AssignPrivateMemberOnClass(const AClassValue: TGocciaClassValue;
  const APrivateName: string; const AValue: TGocciaValue;
  const AContext: TGocciaEvaluationContext);
var
  AccessClass: TGocciaClassValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
  PrivateName: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AccessClass := ResolveLexicalPrivateAccessClass(AContext, APrivateName);
  if not Assigned(AccessClass) then
    AccessClass := AClassValue;
  PrivateName := PrivateLookupName(AccessClass, APrivateName);

  EnsurePrivateStaticBrand(AClassValue, AccessClass, APrivateName);

  if AccessClass.HasOwnPrivateSetter(PrivateName) then
  begin
    SetterFn := AccessClass.GetOwnPrivatePropertySetter(PrivateName);
    SetterArgs := TGocciaArgumentsCollection.Create;
    try
      SetterArgs.Add(AValue);
      SetterFn.Call(SetterArgs, AClassValue);
    finally
      SetterArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasOwnPrivateGetter(PrivateName) then
    ThrowPrivateSetterMissingError(APrivateName);

  if AccessClass.HasOwnPrivateStaticMethod(PrivateName) then
    ThrowTypeError(Format('Private method #%s is not writable', [APrivateName]),
      SSuggestPrivateFieldAccess);

  if not AccessClass.HasOwnPrivateStaticProperty(PrivateName) then
    ThrowPrivateBrandError(APrivateName, AccessClass);

  AccessClass.AddPrivateStaticProperty(PrivateName, AValue);
end;

procedure AssignPrivateMemberOnInstance(const AInstance: TGocciaInstanceValue;
  const APrivateName: string; const AValue: TGocciaValue;
  const AContext: TGocciaEvaluationContext);
var
  AccessClass: TGocciaClassValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
  PrivateName: string;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AccessClass := ResolveOwningClass(AInstance, AContext, APrivateName);
  PrivateName := PrivateLookupName(AccessClass, APrivateName);

  if AccessClass.HasPrivateSetter(PrivateName) then
  begin
    if PrivateName <> APrivateName then
      EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
        AccessClass)
    else
      EnsureRawPrivateInstanceBrand(AInstance, APrivateName, AccessClass);
    SetterFn := AccessClass.PrivatePropertySetter[PrivateName];
    SetterArgs := TGocciaArgumentsCollection.Create;
    try
      SetterArgs.Add(AValue);
      SetterFn.Call(SetterArgs, AInstance);
    finally
      SetterArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasPrivateGetter(PrivateName) then
    ThrowPrivateSetterMissingError(APrivateName);

  if AccessClass.PrivateMethods.ContainsKey(PrivateName) then
    ThrowTypeError(Format('Private method #%s is not writable', [APrivateName]),
      SSuggestPrivateFieldAccess);

  if (PrivateName <> APrivateName) and Assigned(AccessClass.Prototype) then
  begin
    Descriptor := AccessClass.Prototype.GetOwnPropertyDescriptor(PrivateName);
    if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
        AccessClass);
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
      begin
        SetterArgs := TGocciaArgumentsCollection.Create;
        try
          SetterArgs.Add(AValue);
          TGocciaFunctionBase(
            TGocciaPropertyDescriptorAccessor(Descriptor).Setter).Call(
              SetterArgs, AInstance);
        finally
          SetterArgs.Free;
        end;
        Exit;
      end;
      ThrowPrivateSetterMissingError(APrivateName);
    end
    else if Descriptor is TGocciaPropertyDescriptorData then
      ThrowTypeError(Format('Private method #%s is not writable', [APrivateName]),
        SSuggestPrivateFieldAccess);
  end;

  if PrivateName <> APrivateName then
  begin
    EnsureBytecodePrivateBrand(AInstance, APrivateName, PrivateName,
      AccessClass);
    AInstance.SetRawPrivateProperty(PrivateName, AValue);
    Exit;
  end;

  AInstance.SetPrivateProperty(APrivateName, AValue, AccessClass);
end;

procedure AssignPrivateMemberOnObject(const AReceiver: TGocciaObjectValue;
  const APrivateName: string; const AValue: TGocciaValue;
  const AContext: TGocciaEvaluationContext);
var
  AccessClass: TGocciaClassValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
  PendingNewTarget: TGocciaValue;
  FieldInitializer: TGocciaExpression;
  PrivateName: string;
begin
  AccessClass := ResolveLexicalPrivateAccessClass(AContext, APrivateName);
  if not Assigned(AccessClass) then
    ThrowPrivateBrandError(APrivateName);
  PrivateName := PrivateLookupName(AccessClass, APrivateName);

  if PrivateName <> APrivateName then
  begin
    EnsureBytecodePrivateBrand(AReceiver, APrivateName, PrivateName,
      AccessClass);
  end
  else if not HasRawPrivateInstanceBrand(AReceiver, AccessClass) then
  begin
    PendingNewTarget := AContext.Scope.FindNewTarget;
    if (AReceiver = AContext.Scope.ThisValue) and
       (PendingNewTarget is TGocciaClassValue) and
       (TGocciaClassValue(PendingNewTarget) = AccessClass) and
       AccessClass.PrivateInstancePropertyDefs.TryGetValue(
         APrivateName, FieldInitializer) then
    begin
      StampRawPrivateInstanceBrand(AReceiver, AccessClass);
      SetRawPrivateInstanceProperty(AReceiver, APrivateName, AValue,
        AccessClass);
      Exit;
    end;
    ThrowPrivateBrandError(APrivateName, AccessClass);
  end;

  if AccessClass.HasPrivateSetter(PrivateName) then
  begin
    SetterFn := AccessClass.PrivatePropertySetter[PrivateName];
    SetterArgs := TGocciaArgumentsCollection.Create;
    try
      SetterArgs.Add(AValue);
      SetterFn.Call(SetterArgs, AReceiver);
    finally
      SetterArgs.Free;
    end;
    Exit;
  end;

  if AccessClass.HasPrivateGetter(PrivateName) then
    ThrowPrivateSetterMissingError(APrivateName);

  if AccessClass.PrivateMethods.ContainsKey(PrivateName) then
    ThrowTypeError(Format('Private method #%s is not writable', [APrivateName]),
      SSuggestPrivateFieldAccess);

  if PrivateName <> APrivateName then
  begin
    if AReceiver is TGocciaInstanceValue then
      TGocciaInstanceValue(AReceiver).SetRawPrivateProperty(PrivateName, AValue)
    else
      DefineRawObjectPrivateDataProperty(AReceiver, PrivateName, AValue,
        [pfWritable, pfConfigurable]);
    Exit;
  end;

  SetRawPrivateInstanceProperty(AReceiver, APrivateName, AValue, AccessClass);
end;

function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivateMemberExpression.ObjectExpr, AContext);
  if APrivateMemberExpression.Optional and
     ((ObjectValue is TGocciaNullLiteralValue) or
      (ObjectValue is TGocciaUndefinedLiteralValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    Result := EvaluatePrivateMemberOnInstance(Instance, APrivateMemberExpression.PrivateName, AContext);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(ObjectValue);
    Result := EvaluatePrivateMemberOnClass(
      ClassValue, APrivateMemberExpression.PrivateName, AContext);
  end
  else if ObjectValue is TGocciaObjectValue then
    Result := EvaluatePrivateMemberOnObject(
      TGocciaObjectValue(ObjectValue),
      APrivateMemberExpression.PrivateName, AContext)
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivateMemberExpression.Line, APrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function EvaluatePrivateMember(const APrivateMemberExpression: TGocciaPrivateMemberExpression; const AContext: TGocciaEvaluationContext; out AObjectValue: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
begin
  // Evaluate the object expression and store it for this binding
  AObjectValue := EvaluateExpression(APrivateMemberExpression.ObjectExpr, AContext);
  if APrivateMemberExpression.Optional and
     ((AObjectValue is TGocciaNullLiteralValue) or
      (AObjectValue is TGocciaUndefinedLiteralValue)) then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  if AObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(AObjectValue);
    Result := EvaluatePrivateMemberOnInstance(Instance, APrivateMemberExpression.PrivateName, AContext);
  end
  else if AObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(AObjectValue);
    Result := EvaluatePrivateMemberOnClass(
      ClassValue, APrivateMemberExpression.PrivateName, AContext);
  end
  else if AObjectValue is TGocciaObjectValue then
    Result := EvaluatePrivateMemberOnObject(
      TGocciaObjectValue(AObjectValue),
      APrivateMemberExpression.PrivateName, AContext)
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [AObjectValue.TypeName]),
      APrivateMemberExpression.Line, APrivateMemberExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §7.3.31 PrivateSet ( O, P, value )
function EvaluatePrivatePropertyAssignment(const APrivatePropertyAssignmentExpression: TGocciaPrivatePropertyAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  Value: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivatePropertyAssignmentExpression.ObjectExpr, AContext);

  // Evaluate the value to assign
  Value := EvaluateExpression(APrivatePropertyAssignmentExpression.Value, AContext);

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    AssignPrivateMemberOnInstance(
      Instance, APrivatePropertyAssignmentExpression.PrivateName, Value,
      AContext);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(ObjectValue);
    AssignPrivateMemberOnClass(
      ClassValue, APrivatePropertyAssignmentExpression.PrivateName, Value,
      AContext);
  end
  else if ObjectValue is TGocciaObjectValue then
    AssignPrivateMemberOnObject(
      TGocciaObjectValue(ObjectValue),
      APrivatePropertyAssignmentExpression.PrivateName, Value, AContext)
  else
  begin
    AContext.OnError(Format('Private fields can only be assigned on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivatePropertyAssignmentExpression.Line, APrivatePropertyAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  Result := Value;
end;

// ES2026 §13.15.2 AssignmentExpression : LeftHandSideExpression ??= AssignmentExpression
function EvaluatePrivatePropertyCompoundAssignment(const APrivatePropertyCompoundAssignmentExpression: TGocciaPrivatePropertyCompoundAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  ObjectValue: TGocciaValue;
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  CurrentValue: TGocciaValue;
  Value: TGocciaValue;
begin
  // Evaluate the object expression
  ObjectValue := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.ObjectExpr, AContext);

  if ObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(ObjectValue);
    CurrentValue := EvaluatePrivateMemberOnInstance(
      Instance, APrivatePropertyCompoundAssignmentExpression.PrivateName, AContext);
  end
  else if ObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(ObjectValue);
    CurrentValue := EvaluatePrivateMemberOnClass(
      ClassValue, APrivatePropertyCompoundAssignmentExpression.PrivateName,
      AContext);
  end
  else if ObjectValue is TGocciaObjectValue then
    CurrentValue := EvaluatePrivateMemberOnObject(
      TGocciaObjectValue(ObjectValue),
      APrivatePropertyCompoundAssignmentExpression.PrivateName, AContext)
  else
  begin
    AContext.OnError(Format('Private fields can only be accessed on class instances or classes, not %s', [ObjectValue.TypeName]),
      APrivatePropertyCompoundAssignmentExpression.Line, APrivatePropertyCompoundAssignmentExpression.Column);
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // ES2026 §13.15.2 step 3: short-circuit logical/nullish assignment
  if APrivatePropertyCompoundAssignmentExpression.Operator in
     [gttNullishCoalescingAssign, gttLogicalAndAssign, gttLogicalOrAssign] then
  begin
    case APrivatePropertyCompoundAssignmentExpression.Operator of
      gttNullishCoalescingAssign:
        if not ((CurrentValue is TGocciaUndefinedLiteralValue) or
                (CurrentValue is TGocciaNullLiteralValue)) then
        begin
          Result := CurrentValue;
          Exit;
        end;
      gttLogicalAndAssign:
        if not CurrentValue.ToBooleanLiteral.Value then
        begin
          Result := CurrentValue;
          Exit;
        end;
      gttLogicalOrAssign:
        if CurrentValue.ToBooleanLiteral.Value then
        begin
          Result := CurrentValue;
          Exit;
        end;
    end;

    Value := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.Value, AContext);
    Result := Value;

    if ObjectValue is TGocciaInstanceValue then
      AssignPrivateMemberOnInstance(
        Instance, APrivatePropertyCompoundAssignmentExpression.PrivateName,
        Result, AContext)
    else if ObjectValue is TGocciaClassValue then
      AssignPrivateMemberOnClass(
        ClassValue, APrivatePropertyCompoundAssignmentExpression.PrivateName,
        Result, AContext)
    else if ObjectValue is TGocciaObjectValue then
      AssignPrivateMemberOnObject(
        TGocciaObjectValue(ObjectValue),
        APrivatePropertyCompoundAssignmentExpression.PrivateName, Result,
        AContext);
    Exit;
  end;

  // Evaluate the value to operate with
  Value := EvaluateExpression(APrivatePropertyCompoundAssignmentExpression.Value, AContext);

  // Use shared compound operation function
  Result := Goccia.Arithmetic.CompoundOperations(
    CurrentValue, Value, APrivatePropertyCompoundAssignmentExpression.Operator);

  // Set the new value
  if ObjectValue is TGocciaInstanceValue then
    AssignPrivateMemberOnInstance(
      Instance, APrivatePropertyCompoundAssignmentExpression.PrivateName,
      Result, AContext)
  else if ObjectValue is TGocciaClassValue then
    AssignPrivateMemberOnClass(
      ClassValue, APrivatePropertyCompoundAssignmentExpression.PrivateName,
      Result, AContext)
  else if ObjectValue is TGocciaObjectValue then
    AssignPrivateMemberOnObject(
      TGocciaObjectValue(ObjectValue),
      APrivatePropertyCompoundAssignmentExpression.PrivateName, Result,
      AContext);
end;

procedure InitializePrivateInstanceProperties(const AInstance: TGocciaObjectValue; const AClassValue: TGocciaClassValue; const AContext: TGocciaEvaluationContext; const AInitializationMode: TGocciaInstanceInitializationMode);
var
  PropertyValue: TGocciaValue;
  Entry: TGocciaExpressionMap.TKeyValuePair;
  I: Integer;
begin
  for I := 0 to AClassValue.PrivateInstancePropertyDefs.Count - 1 do
  begin
    Entry := AClassValue.PrivateInstancePropertyDefs.EntryAt(I);
    if Assigned(Entry.Value) then
      PropertyValue := EvaluateExpression(Entry.Value, AContext)
    else
      PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    if AInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(AInstance).SetPrivateProperty(
        Entry.Key, PropertyValue, AClassValue, True)
    else
      InitializeRawPrivateInstanceProperty(
        AInstance, Entry.Key, PropertyValue, AClassValue,
        AInitializationMode);
  end;
end;

procedure RunClassInstanceInitializers(const AClassValue: TGocciaClassValue;
  const AInstance: TGocciaObjectValue;
  const AContext: TGocciaEvaluationContext;
  const AInitializationMode: TGocciaInstanceInitializationMode);
var
  InitContext, SuperInitContext: TGocciaEvaluationContext;
  InitScope, SuperInitScope: TGocciaScope;
  WalkClass: TGocciaClassValue;
begin
  InitContext := AContext;
  InitScope := TGocciaClassInitScope.Create(AContext.Scope, AClassValue);
  InitScope.ThisValue := AInstance;
  InitContext.Scope := InitScope;

  if AInstance is TGocciaInstanceValue then
  begin
    InitializeInstanceProperties(TGocciaInstanceValue(AInstance), AClassValue, InitContext);

    if AClassValue.FieldOrderCount = 0 then
    begin
      WalkClass := AClassValue.SuperClass;
      while Assigned(WalkClass) do
      begin
        CheckExecutionTimeout;
        IncrementInstructionCounter;
        CheckInstructionLimit;
        SuperInitContext := AContext;
        SuperInitScope := TGocciaClassInitScope.Create(AContext.Scope, WalkClass);
        SuperInitScope.ThisValue := AInstance;
        SuperInitContext.Scope := SuperInitScope;
        if WalkClass.FieldOrderCount = 0 then
          InitializePrivateInstanceProperties(
            AInstance, WalkClass, SuperInitContext, AInitializationMode);
        WalkClass := WalkClass.SuperClass;
      end;

      InitializePrivateInstanceProperties(AInstance, AClassValue, InitContext,
        AInitializationMode);
    end;
  end
  else
    InitializeObjectInstanceProperties(AInstance, AClassValue, InitContext,
      AInitializationMode);

  AClassValue.RunMethodInitializers(AInstance);
  AClassValue.RunFieldInitializers(AInstance);
  AClassValue.RunDecoratorFieldInitializers(AInstance);
  if AInitializationMode = iimEagerReplacement then
    StampRawPrivateInstanceInitializersApplied(AInstance, AClassValue);
end;

function InstantiateClass(const AClassValue: TGocciaClassValue;
  const AArguments: TGocciaArgumentsCollection;
  const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  RootedInstance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructedValue: TGocciaValue;
  ConstructorThisValue: TGocciaValue;
  InitializerReplayReceiver: TGocciaObjectValue;
  function ConstructNativeSuperInstance(
    const AConstructor: TGocciaObjectValue): TGocciaObjectValue;
  var
    ConstructedValue: TGocciaValue;
  begin
    if AConstructor is TGocciaProxyValue then
      ConstructedValue := TGocciaProxyValue(AConstructor).ConstructTrap(AArguments)
    else if AConstructor is TGocciaNativeFunctionValue then
    begin
      if TGocciaNativeFunctionValue(AConstructor).NotConstructable then
        ThrowTypeError(
          Format(SErrorNotConstructor,
            [TGocciaNativeFunctionValue(AConstructor).Name]),
          Format('''%s'' is not a constructor',
            [TGocciaNativeFunctionValue(AConstructor).Name]));
      ConstructedValue := TGocciaNativeFunctionValue(AConstructor).Construct(
        AArguments, AConstructor);
    end
    else if AConstructor is TGocciaFunctionBase then
    begin
      Result := TGocciaInstanceValue.Create(AClassValue,
        AClassValue.EstimatedInstancePropertyCapacity);
      Exit;
    end
    else
      ThrowTypeError(Format(SErrorValueNotConstructor, [AConstructor.TypeName]),
        SSuggestNotConstructorType);

    if ConstructedValue is TGocciaObjectValue then
      Result := TGocciaObjectValue(ConstructedValue)
    else
    begin
      ThrowTypeError('Superclass constructor did not return an object',
        SSuggestNotConstructorType);
      Result := nil;
    end;
  end;
  function HasDerivedConstructorReturnRestriction: Boolean;
  begin
    Result := ClassRequiresObjectConstructorReturn(AClassValue);
  end;
  procedure SetFinalInstance(const AInstance: TGocciaObjectValue);
  begin
    if (not Assigned(AInstance)) or (AInstance = Instance) then
      Exit;
    TGarbageCollector.Instance.AddTempRoot(AInstance);
    TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
    RootedInstance := AInstance;
    Instance := AInstance;
  end;
  procedure ApplyOwnConstructorResult(const AValue,
    AConstructorThisValue: TGocciaValue);
  var
    ThisObject: TGocciaObjectValue;
    ReturnObject: TGocciaObjectValue;
  begin
    if (AConstructorThisValue is TGocciaObjectValue) and
       (AConstructorThisValue <> Instance) then
    begin
      ThisObject := TGocciaObjectValue(AConstructorThisValue);
      SetFinalInstance(ThisObject);
      InitializerReplayReceiver := ThisObject;
    end;

    if AValue is TGocciaObjectValue then
    begin
      ReturnObject := TGocciaObjectValue(AValue);
      if ReturnObject <> Instance then
      begin
        SetFinalInstance(ReturnObject);
        if ReturnObject <> InitializerReplayReceiver then
          InitializerReplayReceiver := nil;
      end;
    end
    else if HasDerivedConstructorReturnRestriction and
            not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure ApplyReplacementResult(const AValue: TGocciaValue);
  begin
    if AValue is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(AValue) = Instance then
        Exit;
      SetFinalInstance(TGocciaObjectValue(AValue));
      InitializerReplayReceiver := Instance;
    end;
  end;
  procedure RunInstanceInitializers(
    const AInitializationMode: TGocciaInstanceInitializationMode);
  begin
    RunClassInstanceInitializers(AClassValue, Instance, AContext,
      AInitializationMode);
  end;
begin
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;
  NativeInstance := nil;
  WalkClass := AClassValue;
  while Assigned(WalkClass) do
  begin
    CheckExecutionTimeout;
    IncrementInstructionCounter;
    CheckInstructionLimit;
    NativeInstance := WalkClass.CreateNativeInstance(AArguments);
    if (not Assigned(NativeInstance)) and
       Assigned(WalkClass.NativeSuperConstructor) then
      // The explicit super() path reuses this precreated native receiver.
      NativeInstance := ConstructNativeSuperInstance(
        WalkClass.NativeSuperConstructor);
    if Assigned(NativeInstance) then
      Break;
    WalkClass := WalkClass.SuperClass;
  end;

  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := AClassValue.Prototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := AClassValue;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(AClassValue);
    Instance.Prototype := AClassValue.Prototype;
  end;

  RootedInstance := Instance;
  InitializerReplayReceiver := nil;
  TGarbageCollector.Instance.AddTempRoot(RootedInstance);
  try
    RunInstanceInitializers(iimFirstPass);

    if Assigned(AClassValue.ConstructorMethod) then
    begin
      ConstructedValue := AClassValue.ConstructorMethod.CallWithThisValue(
        AArguments, Instance, ConstructorThisValue, AClassValue);
      ApplyOwnConstructorResult(ConstructedValue, ConstructorThisValue);
      if HasDerivedConstructorReturnRestriction and
         IsUndefinedConstructedValue(ConstructedValue) and
         not AClassValue.ConstructorMethod.LastSuperConstructorCalled then
        ThrowReferenceError(
          'Must call super constructor before returning from derived constructor');
    end
    else if Assigned(AClassValue.SuperClass) and Assigned(AClassValue.SuperClass.ConstructorMethod) then
    begin
      ConstructedValue := AClassValue.SuperClass.ConstructorMethod.CallWithThisValue(
        AArguments, Instance, ConstructorThisValue, AClassValue);
      ValidateClassConstructorReturn(AClassValue.SuperClass, ConstructedValue);
      if IsUndefinedConstructedValue(ConstructedValue) then
        ApplyReplacementResult(ConstructorThisValue)
      else
        ApplyReplacementResult(ConstructedValue);
    end
    else if Assigned(AClassValue.NativeSuperConstructor) and
            (AClassValue.NativeSuperConstructor is TGocciaFunctionBase) and
            not (AClassValue.NativeSuperConstructor is TGocciaNativeFunctionValue) then
    begin
      ConstructedValue := InvokeConstructableWithReceiver(
        AClassValue.NativeSuperConstructor, AArguments, Instance, AContext);
      ApplyReplacementResult(ConstructedValue);
    end
    else if Assigned(NativeInstance) and (NativeInstance is TGocciaInstanceValue) then
      TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(AArguments);

    if Assigned(InitializerReplayReceiver) and
       (Instance = InitializerReplayReceiver) and
       not HasRawPrivateInstanceInitializersApplied(Instance, AClassValue) then
      RunInstanceInitializers(iimReplay);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
  end;

  Result := Instance;
end;

// Template literals without real interpolations are returned as static strings.
// The parser pre-segments templates with interpolations into
// TGocciaTemplateWithInterpolationExpression, so this function only handles
// the no-interpolation case.
function EvaluateTemplateLiteral(const ATemplateLiteralExpression: TGocciaTemplateLiteralExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(ATemplateLiteralExpression.Value);
end;

// ES2026 §13.2.8 Template Literals — evaluate a pre-segmented template with
// interpolation expressions. Parts alternate between string literal nodes
// (static text) and expression nodes (interpolated values).
function EvaluateTemplateWithInterpolation(const ATemplateWithInterpolationExpression: TGocciaTemplateWithInterpolationExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  SB: TStringBuffer;
  I: Integer;
  PartValue: TGocciaValue;
begin
  SB := TStringBuffer.Create;
  for I := 0 to ATemplateWithInterpolationExpression.Parts.Count - 1 do
  begin
    PartValue := EvaluateExpression(ATemplateWithInterpolationExpression.Parts[I], AContext);
    if PartValue = nil then
      SB.Append('undefined')
    else
    begin
      if PartValue is TGocciaSymbolValue then
        ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
      // ES2026 §13.15.5.1 step 5e: ToString(value) on each substitution
      SB.Append(PartValue.ToStringLiteral.Value);
    end;
  end;
  Result := TGocciaStringLiteralValue.Create(SB.ToString);
end;

// ES2026 §13.3.11 Runtime Semantics: Evaluation — Tagged Templates
function EvaluateTaggedTemplate(const ATaggedTemplateExpression: TGocciaTaggedTemplateExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Callee, ThisValue, ExprValue, TemplateObject: TGocciaValue;
  MemberExpr: TGocciaMemberExpression;
  CookedArray, RawArray: TGocciaArrayValue;
  Arguments: TGocciaArgumentsCollection;
  I: Integer;
  CalleeName: string;
  TemplateKey: string;
begin
  CheckExecutionTimeout;
  IncrementInstructionCounter;
  CheckInstructionLimit;

  // ES2026 §13.3.11 step 1: Evaluate the tag expression
  if ATaggedTemplateExpression.Tag is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(ATaggedTemplateExpression.Tag);
    Callee := EvaluateMember(MemberExpr, AContext, ThisValue);
  end
  else if ATaggedTemplateExpression.Tag is TGocciaIdentifierExpression then
    AContext.Scope.ResolveIdentifierReference(
      TGocciaIdentifierExpression(ATaggedTemplateExpression.Tag).Name,
      Callee, ThisValue)
  else
  begin
    Callee := EvaluateExpression(ATaggedTemplateExpression.Tag, AContext);
    ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  // ES2026 §13.2.8.3 GetTemplateObject — return the cached template object for
  // this Parse Node, or build it on first evaluation and pin it for reuse.
  TemplateKey := 'ast:' + IntToHex(ATaggedTemplateExpression.TemplateSiteId,
    16);
  if Assigned(AContext.Realm) then
    TemplateObject := TGocciaValue(AContext.Realm.GetTemplateObject(TemplateKey))
  else if Assigned(ATaggedTemplateExpression.TemplateObject) then
    TemplateObject := ATaggedTemplateExpression.TemplateObject
  else
    TemplateObject := nil;

  if not Assigned(TemplateObject) then
  begin
    // Build the raw array
    RawArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(RawArray);
    try
      for I := 0 to Length(ATaggedTemplateExpression.RawStrings) - 1 do
        RawArray.Elements.Add(TGocciaStringLiteralValue.Create(ATaggedTemplateExpression.RawStrings[I]));
      RawArray.Freeze;

      // Build the cooked array (the template object)
      CookedArray := TGocciaArrayValue.Create;
      TGarbageCollector.Instance.AddTempRoot(CookedArray);
      try
        // TC39 Template Literal Revision: segments with malformed escapes get
        // cooked=undefined; valid segments get the resolved string value.
        for I := 0 to Length(ATaggedTemplateExpression.CookedStrings) - 1 do
        begin
          if ATaggedTemplateExpression.CookedValid[I] then
            CookedArray.Elements.Add(TGocciaStringLiteralValue.Create(ATaggedTemplateExpression.CookedStrings[I]))
          else
            CookedArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
        // ES2026 §13.2.8.3 step 8: Define raw as non-enumerable, non-writable, non-configurable
        CookedArray.DefineProperty(PROP_RAW,
          TGocciaPropertyDescriptorData.Create(RawArray, []));
        // ES2026 §13.2.8.3 step 12: Freeze the template object
        CookedArray.Freeze;
        // ES2026 §13.2.8.3 step 13: Store in realm [[TemplateMap]] keyed by this
        // Parse Node so subsequent evaluations return the identical object.
        if Assigned(AContext.Realm) then
          AContext.Realm.SetTemplateObject(TemplateKey, CookedArray)
        else
          ATaggedTemplateExpression.SetCachedTemplateObject(CookedArray);
        TemplateObject := CookedArray;
      finally
        TGarbageCollector.Instance.RemoveTempRoot(CookedArray);
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RawArray);
    end;
  end;

  // ES2026 §13.3.11 step 3: Evaluate substitution expressions and build arguments
  Arguments := TGocciaArgumentsCollection.Create;
  try
    Arguments.Add(TemplateObject);
    for I := 0 to ATaggedTemplateExpression.Expressions.Count - 1 do
    begin
      ExprValue := EvaluateExpression(ATaggedTemplateExpression.Expressions[I], AContext);
      Arguments.Add(ExprValue);
    end;

    // ES2026 §13.3.11 step 4: Call the tag function
    if Callee is TGocciaNativeFunctionValue then
      CalleeName := TGocciaNativeFunctionValue(Callee).Name
    else if Callee is TGocciaFunctionValue then
      CalleeName := TGocciaFunctionValue(Callee).Name
    else
      CalleeName := '';

    if Assigned(TGocciaCallStack.Instance) then
    begin
      TGocciaCallStack.Instance.Push(CalleeName, AContext.CurrentFilePath,
        ATaggedTemplateExpression.Line, ATaggedTemplateExpression.Column);
    end;
    try
      if Assigned(TGocciaCallStack.Instance) then
        CheckStackDepth(TGocciaCallStack.Instance.Count);
      if Assigned(Callee) and Callee.IsCallable then
        Result := DispatchCall(Callee, Arguments, ThisValue)
      else
        ThrowTypeError(
          Format(SErrorValueNotFunction, [Callee.TypeName]),
          SSuggestTaggedTemplateCallable);
    finally
      if Assigned(TGocciaCallStack.Instance) then
        TGocciaCallStack.Instance.Pop;
    end;
  finally
    Arguments.Free;
  end;
end;

// Lightweight template expression evaluator - handles 95% of common cases without full parsing
function EvaluateTemplateExpression(const AExpressionText: string; const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer): TGocciaValue;
var
  Trimmed: string;
  PlusPos, MinusPos, StarPos, SlashPos, DotPos: Integer;
  Left, Right, PropName: string;
  LeftVal, RightVal, ObjVal: TGocciaValue;
  Expression: TGocciaExpression;
  DeclaredPrivateNames: TStringList;
  PipelineOptions: TGocciaSourcePipelineOptions;
  SourceName: string;
  I: Integer;
  IsSimpleIdentifier: Boolean;
begin
  Trimmed := Trim(AExpressionText);

  // Handle empty expression
  if Trimmed = '' then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  // Check for complex expressions that need full parsing
  if (Pos('"', Trimmed) > 0) or (Pos('''', Trimmed) > 0) or  // String literals
     (Pos('(', Trimmed) > 0) or (Pos(')', Trimmed) > 0) or   // Parentheses
     (Pos('?', Trimmed) > 0) or (Pos(':', Trimmed) > 0) or   // Ternary operator
     (Pos('[', Trimmed) > 0) or (Pos(']', Trimmed) > 0) or   // Array access
     (Pos('#', Trimmed) > 0) or                               // Private field access
     (Pos('>=', Trimmed) > 0) or (Pos('<=', Trimmed) > 0) or // Comparison operators
     (Pos('==', Trimmed) > 0) or (Pos('!=', Trimmed) > 0) or
     (Pos('>', Trimmed) > 0) or (Pos('<', Trimmed) > 0) then
  begin
    // Complex expression - use full parser
  end
  else
  begin
    // Try simple variable access first (most common case)
    // Check if it's a valid identifier (letters, digits, underscore, dollar)
    IsSimpleIdentifier := True;
    if Length(Trimmed) > 0 then
    begin
      if not (Trimmed[1] in ['a'..'z', 'A'..'Z', '_', '$']) then
        IsSimpleIdentifier := False
      else
      begin
        for I := 2 to Length(Trimmed) do
        begin
          if not (Trimmed[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) then
          begin
            IsSimpleIdentifier := False;
            Break;
          end;
        end;
      end;
    end
    else
      IsSimpleIdentifier := False;

    if IsSimpleIdentifier then
    begin
      // Handle keyword literals before scope lookup
      if Trimmed = KEYWORD_TRUE then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end
      else if Trimmed = KEYWORD_FALSE then
      begin
        Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end
      else if Trimmed = KEYWORD_NULL then
      begin
        Result := TGocciaNullLiteralValue.NullValue;
        Exit;
      end
      else if Trimmed = NAN_LITERAL then
      begin
        Result := TGocciaNumberLiteralValue.NaNValue;
        Exit;
      end
      else if Trimmed = INFINITY_LITERAL then
      begin
        Result := TGocciaNumberLiteralValue.InfinityValue;
        Exit;
      end;

      Result := AContext.Scope.ResolveIdentifier(Trimmed);
      if Result = nil then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    // Try simple property access (obj.prop)
    DotPos := Pos('.', Trimmed);
    if (DotPos > 1) and (DotPos < Length(Trimmed)) and
       (Pos(' ', Trimmed) = 0) and (Pos('+', Trimmed) = 0) and
       (Pos('-', Trimmed) = 0) and (Pos('*', Trimmed) = 0) and
       (Pos('/', Trimmed) = 0) then
    begin
      Left := Copy(Trimmed, 1, DotPos - 1);
      PropName := Copy(Trimmed, DotPos + 1, Length(Trimmed));

      ObjVal := AContext.Scope.ResolveIdentifier(Left);
      if (ObjVal <> nil) and (ObjVal is TGocciaObjectValue) then
      begin
        Result := TGocciaObjectValue(ObjVal).GetProperty(PropName);
        Exit;
      end;
    end;

    // Try simple binary arithmetic (x + y, a - b, etc.) - ONLY if it looks simple
    PlusPos := Pos(' + ', Trimmed);
    MinusPos := Pos(' - ', Trimmed);
    StarPos := Pos(' * ', Trimmed);
    SlashPos := Pos(' / ', Trimmed);

    // Only handle binary operations if there's exactly one operator and no other complexity
    if (PlusPos > 0) and (PlusPos < Length(Trimmed) - 2) and
       (Pos(' + ', Copy(Trimmed, PlusPos + 3, Length(Trimmed))) = 0) then // No second +
    begin
      Left := Trim(Copy(Trimmed, 1, PlusPos - 1));
      Right := Trim(Copy(Trimmed, PlusPos + 3, Length(Trimmed)));

      // Only proceed if both sides are simple identifiers
      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          if (LeftVal is TGocciaSymbolValue) or (RightVal is TGocciaSymbolValue) then
            ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
          if (LeftVal is TGocciaStringLiteralValue) or (RightVal is TGocciaStringLiteralValue) then
            Result := TGocciaStringLiteralValue.Create(LeftVal.ToStringLiteral.Value + RightVal.ToStringLiteral.Value)
          else
            Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value + RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;

    if (MinusPos > 0) and (MinusPos < Length(Trimmed) - 2) and
       (Pos(' - ', Copy(Trimmed, MinusPos + 3, Length(Trimmed))) = 0) then // No second -
    begin
      Left := Trim(Copy(Trimmed, 1, MinusPos - 1));
      Right := Trim(Copy(Trimmed, MinusPos + 3, Length(Trimmed)));

      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value - RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;

    if (StarPos > 0) and (StarPos < Length(Trimmed) - 2) and
       (Pos(' * ', Copy(Trimmed, StarPos + 3, Length(Trimmed))) = 0) then // No second *
    begin
      Left := Trim(Copy(Trimmed, 1, StarPos - 1));
      Right := Trim(Copy(Trimmed, StarPos + 3, Length(Trimmed)));

      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value * RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;

    if (SlashPos > 0) and (SlashPos < Length(Trimmed) - 2) and
       (Pos(' / ', Copy(Trimmed, SlashPos + 3, Length(Trimmed))) = 0) then // No second /
    begin
      Left := Trim(Copy(Trimmed, 1, SlashPos - 1));
      Right := Trim(Copy(Trimmed, SlashPos + 3, Length(Trimmed)));

      if (Left <> '') and (Right <> '') and
         (Pos(' ', Left) = 0) and (Pos(' ', Right) = 0) and
         (Pos('.', Left) = 0) and (Pos('.', Right) = 0) then
      begin
        LeftVal := AContext.Scope.GetValue(Left);
        RightVal := AContext.Scope.GetValue(Right);

        if (LeftVal <> nil) and (RightVal <> nil) then
        begin
          if RightVal.ToNumberLiteral.Value = 0 then
            Result := TGocciaNumberLiteralValue.InfinityValue
          else
            Result := TGocciaNumberLiteralValue.Create(LeftVal.ToNumberLiteral.Value / RightVal.ToNumberLiteral.Value);
          Exit;
        end;
      end;
    end;
  end;

  // Fall back to full parsing for complex expressions
  // This handles complex cases like nested expressions, function calls, string literals, etc.
  Expression := nil;
  DeclaredPrivateNames := nil;

  try
    PipelineOptions := TGocciaSourcePipeline.CurrentOptionsOrDefault;
    SourceName := Format('%s:%d:%d', [AContext.CurrentFilePath, ALine,
      AColumn]);
    if Pos('#', Trimmed) > 0 then
    begin
      DeclaredPrivateNames := CollectDeclaredPrivateNames(AContext);
      Expression := TGocciaSourcePipeline.ParseExpression(AExpressionText,
        SourceName, PipelineOptions, DeclaredPrivateNames);
    end
    else
      Expression := TGocciaSourcePipeline.ParseExpression(AExpressionText,
        SourceName, PipelineOptions);

    if Expression = nil then
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      Exit;
    end;

    Result := EvaluateExpression(Expression, AContext);

  except
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  // Cleanup
  if Assigned(Expression) then Expression.Free;
  if Assigned(DeclaredPrivateNames) then DeclaredPrivateNames.Free;
end;

function EvaluateDestructuringAssignment(const ADestructuringAssignmentExpression: TGocciaDestructuringAssignmentExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Evaluate the right-hand side
  Value := EvaluateExpression(ADestructuringAssignmentExpression.Right, AContext);

  // Apply the destructuring pattern
  AssignPattern(ADestructuringAssignmentExpression.Left, Value, AContext);

  Result := Value;
end;

function EvaluateDestructuringDeclaration(const ADestructuringDeclaration: TGocciaDestructuringDeclaration; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  Value: TGocciaValue;
begin
  // Evaluate the initializer
  Value := EvaluateExpression(ADestructuringDeclaration.Initializer, AContext);

  // Apply the destructuring pattern to declare variables
  if ADestructuringDeclaration.IsVar then
    AssignVariablePattern(ADestructuringDeclaration.Pattern, Value, AContext)
  else if ADestructuringDeclaration.IsConst then
    AssignPattern(ADestructuringDeclaration.Pattern, Value, AContext, True, dtConst)
  else
    AssignPattern(ADestructuringDeclaration.Pattern, Value, AContext, True, dtLet);

  Result := Value;
end;

function SingleIdentifierPatternName(
  const APattern: TGocciaDestructuringPattern): string;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    Exit(TGocciaIdentifierDestructuringPattern(APattern).Name);
  Result := '';
end;

procedure ApplyInferredNameForDefaultInitializer(
  const APattern: TGocciaDestructuringPattern;
  const AInitializer: TGocciaExpression; const AValue: TGocciaValue);
var
  Name: string;
begin
  if not IsAnonymousFunctionNameExpression(AInitializer) then
    Exit;
  Name := SingleIdentifierPatternName(APattern);
  if Name = '' then
    Exit;
  if AValue is TGocciaFunctionValue then
    TGocciaFunctionValue(AValue).SetInferredName(Name)
  else if AValue is TGocciaClassValue then
    TGocciaClassValue(AValue).SetInferredName(Name);
end;

procedure InitPreparedDestructuringReference(
  var AReference: TPreparedDestructuringReference);
begin
  AReference.Kind := pdrNone;
  AReference.ObjectValue := nil;
  AReference.PropertyName := '';
  AReference.ComputedKeyValue := nil;
  AReference.SymbolValue := nil;
  AReference.PrivateName := '';
  AReference.Line := 0;
  AReference.Column := 0;
  AReference.ObjectRooted := False;
  AReference.ComputedKeyRooted := False;
  AReference.SymbolRooted := False;
end;

procedure ReleasePreparedDestructuringReference(
  var AReference: TPreparedDestructuringReference);
begin
  if AReference.SymbolRooted and Assigned(AReference.SymbolValue) then
    TGarbageCollector.Instance.RemoveTempRoot(AReference.SymbolValue);
  if AReference.ComputedKeyRooted and Assigned(AReference.ComputedKeyValue) then
    TGarbageCollector.Instance.RemoveTempRoot(AReference.ComputedKeyValue);
  if AReference.ObjectRooted and Assigned(AReference.ObjectValue) then
    TGarbageCollector.Instance.RemoveTempRoot(AReference.ObjectValue);
  InitPreparedDestructuringReference(AReference);
end;

procedure PrepareMemberDestructuringReference(
  const APattern: TGocciaMemberExpressionDestructuringPattern;
  const AContext: TGocciaEvaluationContext;
  var AReference: TPreparedDestructuringReference);
var
  MemberExpr: TGocciaMemberExpression;
begin
  MemberExpr := APattern.Expression;
  AReference.ObjectValue := EvaluateExpression(MemberExpr.ObjectExpr, AContext);
  if Assigned(AReference.ObjectValue) then
  begin
    TGarbageCollector.Instance.AddTempRoot(AReference.ObjectValue);
    AReference.ObjectRooted := True;
  end;
  AReference.Line := APattern.Line;
  AReference.Column := APattern.Column;

  if MemberExpr.Computed then
  begin
    AReference.Kind := pdrComputedProperty;
    AReference.ComputedKeyValue :=
      EvaluateExpression(MemberExpr.PropertyExpression, AContext);
    if Assigned(AReference.ComputedKeyValue) then
    begin
      TGarbageCollector.Instance.AddTempRoot(AReference.ComputedKeyValue);
      AReference.ComputedKeyRooted := True;
    end;
  end
  else
  begin
    AReference.Kind := pdrStringProperty;
    AReference.PropertyName := MemberExpr.PropertyName;
  end;
end;

procedure PreparePrivateMemberDestructuringReference(
  const APattern: TGocciaPrivateMemberExpressionDestructuringPattern;
  const AContext: TGocciaEvaluationContext;
  var AReference: TPreparedDestructuringReference);
begin
  AReference.ObjectValue := EvaluateExpression(APattern.Expression.ObjectExpr,
    AContext);
  if Assigned(AReference.ObjectValue) then
  begin
    TGarbageCollector.Instance.AddTempRoot(AReference.ObjectValue);
    AReference.ObjectRooted := True;
  end;
  AReference.Kind := pdrPrivateProperty;
  AReference.PrivateName := APattern.Expression.PrivateName;
  AReference.Line := APattern.Line;
  AReference.Column := APattern.Column;
end;

procedure PrepareDestructuringAssignmentReference(
  const APattern: TGocciaDestructuringPattern;
  const AContext: TGocciaEvaluationContext;
  var AReference: TPreparedDestructuringReference);
begin
  if APattern is TGocciaAssignmentDestructuringPattern then
    PrepareDestructuringAssignmentReference(
      TGocciaAssignmentDestructuringPattern(APattern).Left, AContext,
      AReference)
  else if APattern is TGocciaRestDestructuringPattern then
    PrepareDestructuringAssignmentReference(
      TGocciaRestDestructuringPattern(APattern).Argument, AContext,
      AReference)
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    PrepareMemberDestructuringReference(
      TGocciaMemberExpressionDestructuringPattern(APattern), AContext,
      AReference)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    PreparePrivateMemberDestructuringReference(
      TGocciaPrivateMemberExpressionDestructuringPattern(APattern), AContext,
      AReference);
end;

procedure AssignPrivateMemberValue(const AObjectValue: TGocciaValue;
  const APrivateName: string; const AValue: TGocciaValue;
  const AContext: TGocciaEvaluationContext; const ALine, AColumn: Integer);
var
  Instance: TGocciaInstanceValue;
  ClassValue: TGocciaClassValue;
  AccessClass: TGocciaClassValue;
  SetterFn: TGocciaFunctionBase;
  SetterArgs: TGocciaArgumentsCollection;
begin
  if AObjectValue is TGocciaInstanceValue then
  begin
    Instance := TGocciaInstanceValue(AObjectValue);
    AccessClass := ResolveOwningClass(Instance, AContext, APrivateName);

    if AccessClass.HasPrivateSetter(APrivateName) then
    begin
      SetterFn := AccessClass.PrivatePropertySetter[APrivateName];
      SetterArgs := TGocciaArgumentsCollection.Create;
      try
        SetterArgs.Add(AValue);
        SetterFn.Call(SetterArgs, Instance);
      finally
        SetterArgs.Free;
      end;
    end
    else if AccessClass.HasPrivateGetter(APrivateName) then
      ThrowPrivateSetterMissingError(APrivateName)
    else
      AssignPrivateMemberOnInstance(Instance, APrivateName, AValue, AContext);
  end
  else if AObjectValue is TGocciaClassValue then
  begin
    ClassValue := TGocciaClassValue(AObjectValue);
    AssignPrivateMemberOnClass(ClassValue, APrivateName, AValue, AContext);
  end
  else if AObjectValue is TGocciaObjectValue then
    AssignPrivateMemberOnObject(TGocciaObjectValue(AObjectValue),
      APrivateName, AValue, AContext)
  else
    AContext.OnError(
      Format('Private fields can only be assigned on class instances or classes, not %s',
        [AObjectValue.TypeName]), ALine, AColumn);
end;

procedure AssignPreparedDestructuringReference(
  const AReference: TPreparedDestructuringReference;
  const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext);
var
  PropValue: TGocciaValue;
  PropertyKey: TGocciaValue;
begin
  case AReference.Kind of
    pdrStringProperty:
      AssignProperty(AReference.ObjectValue, AReference.PropertyName, AValue,
        AContext.OnError, AReference.Line, AReference.Column,
        AContext.NonStrictMode);
    pdrComputedProperty:
      begin
        PropValue := ToPropertyKey(AReference.ComputedKeyValue);
        if PropValue is TGocciaSymbolValue then
          AssignSymbolProperty(AReference.ObjectValue,
            TGocciaSymbolValue(PropValue), AValue, AContext.OnError,
            AReference.Line, AReference.Column, AContext.NonStrictMode)
        else
          AssignProperty(AReference.ObjectValue,
            TGocciaStringLiteralValue(PropValue).Value, AValue,
            AContext.OnError, AReference.Line, AReference.Column,
            AContext.NonStrictMode);
      end;
    pdrSymbolProperty:
      AssignSymbolProperty(AReference.ObjectValue, AReference.SymbolValue,
        AValue, AContext.OnError, AReference.Line, AReference.Column,
        AContext.NonStrictMode);
    pdrPrivateProperty:
      AssignPrivateMemberValue(AReference.ObjectValue, AReference.PrivateName,
        AValue, AContext, AReference.Line, AReference.Column);
  end;
end;

procedure AssignPatternWithPreparedReference(
  const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue;
  const AContext: TGocciaEvaluationContext;
  const AIsDeclaration: Boolean;
  const ADeclarationType: TGocciaDeclarationType;
  const AReference: TPreparedDestructuringReference);
var
  AssignPat: TGocciaAssignmentDestructuringPattern;
  RestPat: TGocciaRestDestructuringPattern;
  DefaultValue: TGocciaValue;
begin
  if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    if AValue is TGocciaUndefinedLiteralValue then
    begin
      DefaultValue := EvaluateExpression(AssignPat.Right, AContext);
      ApplyInferredNameForDefaultInitializer(AssignPat.Left, AssignPat.Right,
        DefaultValue);
    end
    else
      DefaultValue := AValue;
    AssignPatternWithPreparedReference(AssignPat.Left, DefaultValue, AContext,
      AIsDeclaration, ADeclarationType, AReference);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPat := TGocciaRestDestructuringPattern(APattern);
    AssignPatternWithPreparedReference(RestPat.Argument, AValue, AContext,
      AIsDeclaration, ADeclarationType, AReference);
  end
  else if (AReference.Kind <> pdrNone) and
          ((APattern is TGocciaMemberExpressionDestructuringPattern) or
           (APattern is TGocciaPrivateMemberExpressionDestructuringPattern)) then
    AssignPreparedDestructuringReference(AReference, AValue, AContext)
  else
    AssignPattern(APattern, AValue, AContext, AIsDeclaration,
      ADeclarationType);
end;

// AssignVariablePattern: walk a destructuring pattern and call DefineVariableBinding
// for each leaf identifier. Uses the same value-extraction logic as AssignPattern
// but targets the var binding map on the function/module scope.
procedure AssignVariablePattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext);
var
  ObjPat: TGocciaObjectDestructuringPattern;
  ArrPat: TGocciaArrayDestructuringPattern;
  AssignPat: TGocciaAssignmentDestructuringPattern;
  RestPat: TGocciaRestDestructuringPattern;
  ObjectValue: TGocciaObjectValue;
  RestObject: TGocciaObjectValue;
  Iterator: TGocciaIteratorValue;
  PropValue, ElementValue, DefaultValue: TGocciaValue;
  PropertyKey: TGocciaValue;
  SymbolKey: TGocciaSymbolValue;
  UsedKeys: TStringList;
  UsedSymbolKeys: TList<TGocciaSymbolValue>;
  RestElements: TGocciaArrayValue;
  I: Integer;
  Exhausted: Boolean;
  DoneFlag: Boolean;
  ShouldCloseIterator: Boolean;
  Key: string;

  function DirectNextClosingOnThrow(out ADone: Boolean): TGocciaValue;
  begin
    try
      Result := Iterator.DirectNext(ADone);
    except
      on E: EGocciaGeneratorYield do
        raise;
      on E: EGocciaGeneratorReturn do
      begin
        if not Exhausted then
          CloseIterator(Iterator);
        raise;
      end;
      on E: TGocciaThrowValue do
      begin
        Exhausted := True;
        raise;
      end;
      on E: Exception do
      begin
        Exhausted := True;
        raise;
      end;
    end;
  end;
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AContext.Scope.DefineVariableBinding(
      TGocciaIdentifierDestructuringPattern(APattern).Name, AValue, True)
  else if APattern is TGocciaObjectDestructuringPattern then
  begin
    if (AValue is TGocciaNullLiteralValue) or (AValue is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(
        Format(SErrorCannotDestructure, [AValue.ToStringLiteral.Value]),
        SSuggestDestructureRequiresObject);
    ObjectValue := ToObject(AValue);
    ObjPat := TGocciaObjectDestructuringPattern(APattern);
    UsedKeys := TStringList.Create;
    UsedSymbolKeys := TList<TGocciaSymbolValue>.Create;
    try
      for I := 0 to ObjPat.Properties.Count - 1 do
      begin
        if ObjPat.Properties[I].Pattern is TGocciaRestDestructuringPattern then
        begin
          RestObject := TGocciaObjectValue.Create(
            TGocciaObjectValue.SharedObjectPrototype);
          TGarbageCollector.Instance.AddTempRoot(RestObject);
          try
            CopyDataProperties(RestObject, ObjectValue, UsedKeys,
              UsedSymbolKeys);
            AssignVariablePattern(
              TGocciaRestDestructuringPattern(ObjPat.Properties[I].Pattern).
                Argument,
              RestObject, AContext);
          finally
            TGarbageCollector.Instance.RemoveTempRoot(RestObject);
          end;
        end
        else
        begin
          SymbolKey := nil;
          if ObjPat.Properties[I].Computed and
             Assigned(ObjPat.Properties[I].KeyExpression) then
          begin
            // ES2026 §13.5.5 ObjectBindingPattern with ComputedPropertyName:
            // ToPropertyKey on the evaluated expression, dispatch by type.
            PropertyKey := ToPropertyKey(EvaluateExpression(
              ObjPat.Properties[I].KeyExpression, AContext));
            if PropertyKey is TGocciaSymbolValue then
            begin
              SymbolKey := TGocciaSymbolValue(PropertyKey);
              UsedSymbolKeys.Add(SymbolKey);
            end
            else
            begin
              Key := TGocciaStringLiteralValue(PropertyKey).Value;
              UsedKeys.Add(Key);
            end;
          end
          else
          begin
            Key := ObjPat.Properties[I].Key;
            UsedKeys.Add(Key);
          end;

          if Assigned(SymbolKey) then
          begin
            // Keep the class dispatch explicit to mirror assignment patterns.
            if ObjectValue is TGocciaClassValue then
              PropValue := TGocciaClassValue(ObjectValue).GetSymbolProperty(SymbolKey)
            else
              PropValue := ObjectValue.GetSymbolProperty(SymbolKey);
          end
          else
            PropValue := ObjectValue.GetProperty(Key);

          if not Assigned(PropValue) then
            PropValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          AssignVariablePattern(ObjPat.Properties[I].Pattern, PropValue,
            AContext);
        end
      end;
    finally
      UsedSymbolKeys.Free;
      UsedKeys.Free;
    end;
  end
  else if APattern is TGocciaArrayDestructuringPattern then
  begin
    if (AValue is TGocciaNullLiteralValue) or (AValue is TGocciaUndefinedLiteralValue) then
      ThrowTypeError(
        Format(SErrorCannotDestructure, [AValue.ToStringLiteral.Value]),
        SSuggestDestructureRequiresIterable);
    ArrPat := TGocciaArrayDestructuringPattern(APattern);
    // ES2024 §13.3.3.6 IteratorBindingInitialization always observes
    // @@iterator, even for arrays and strings.
    Iterator := GetIteratorFromValue(AValue);
    if not Assigned(Iterator) then
      ThrowTypeError(
        Format(SErrorNotIterable, [AValue.TypeName]),
        SSuggestDestructureRequiresIterable);
    TGarbageCollector.Instance.AddTempRoot(Iterator);
    try
      Exhausted := False;
      for I := 0 to ArrPat.Elements.Count - 1 do
      begin
        if ArrPat.Elements[I] = nil then
        begin
          if not Exhausted then
          begin
            DirectNextClosingOnThrow(DoneFlag);
            if DoneFlag then
              Exhausted := True;
          end;
          Continue;
        end;
        if ArrPat.Elements[I] is TGocciaRestDestructuringPattern then
        begin
          RestElements := TGocciaArrayValue.Create;
          TGarbageCollector.Instance.AddTempRoot(RestElements);
          try
            if not Exhausted then
            begin
              repeat
                ElementValue := DirectNextClosingOnThrow(DoneFlag);
                if DoneFlag then
                  Break;
                if not Assigned(ElementValue) then
                  ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
                RestElements.Elements.Add(ElementValue);
              until False;
              Exhausted := True;
            end;
            AssignVariablePattern(
              TGocciaRestDestructuringPattern(ArrPat.Elements[I]).Argument,
              RestElements, AContext);
          finally
            TGarbageCollector.Instance.RemoveTempRoot(RestElements);
          end;
          Break;
        end
        else
        begin
          if Exhausted then
            ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue
          else
          begin
            ElementValue := DirectNextClosingOnThrow(DoneFlag);
            if DoneFlag then
            begin
              Exhausted := True;
              ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
            end
            else if not Assigned(ElementValue) then
              ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          end;
          ShouldCloseIterator := not Exhausted;
          try
            AssignVariablePattern(ArrPat.Elements[I], ElementValue, AContext);
          except
            on E: EGocciaGeneratorYield do
              raise;
            on E: EGocciaGeneratorReturn do
            begin
              if ShouldCloseIterator then
                CloseIterator(Iterator);
              raise;
            end;
            on E: TGocciaThrowValue do
            begin
              if ShouldCloseIterator then
              begin
                AcquireExceptionObject;
                CloseIteratorPreservingError(Iterator);
              end;
              raise;
            end;
            on E: Exception do
            begin
              if ShouldCloseIterator then
              begin
                AcquireExceptionObject;
                CloseIteratorPreservingError(Iterator);
              end;
              raise;
            end;
          end;
        end;
      end;
      CloseIterator(Iterator);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Iterator);
    end;
  end
  else if APattern is TGocciaAssignmentDestructuringPattern then
  begin
    AssignPat := TGocciaAssignmentDestructuringPattern(APattern);
    if AValue is TGocciaUndefinedLiteralValue then
    begin
      DefaultValue := EvaluateExpression(AssignPat.Right, AContext);
      ApplyInferredNameForDefaultInitializer(AssignPat.Left, AssignPat.Right,
        DefaultValue);
    end
    else
      DefaultValue := AValue;
    AssignVariablePattern(AssignPat.Left, DefaultValue, AContext);
  end
  else if APattern is TGocciaRestDestructuringPattern then
  begin
    RestPat := TGocciaRestDestructuringPattern(APattern);
    AssignVariablePattern(RestPat.Argument, AValue, AContext);
  end;
end;

procedure AssignMemberExpressionPattern(const APattern: TGocciaMemberExpressionDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext);
var
  Obj, PropValue: TGocciaValue;
  MemberExpr: TGocciaMemberExpression;
begin
  MemberExpr := APattern.Expression;
  Obj := EvaluateExpression(MemberExpr.ObjectExpr, AContext);
  if MemberExpr.Computed then
  begin
    // ES2026 §13.5.1.2 PropertyDestructuringAssignmentEvaluation step 5:
    // ToPropertyKey on the computed key.
    PropValue := ToPropertyKey(EvaluateExpression(MemberExpr.PropertyExpression, AContext));
    if PropValue is TGocciaSymbolValue then
      AssignSymbolProperty(Obj, TGocciaSymbolValue(PropValue), AValue,
        AContext.OnError, APattern.Line, APattern.Column,
        AContext.NonStrictMode)
    else
      AssignProperty(Obj, TGocciaStringLiteralValue(PropValue).Value, AValue,
        AContext.OnError, APattern.Line, APattern.Column,
        AContext.NonStrictMode);
  end
  else
    AssignProperty(Obj, MemberExpr.PropertyName, AValue, AContext.OnError,
      APattern.Line, APattern.Column, AContext.NonStrictMode);
end;

procedure AssignPrivateMemberExpressionPattern(
  const APattern: TGocciaPrivateMemberExpressionDestructuringPattern;
  const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext);
var
  ObjectValue: TGocciaValue;
begin
  ObjectValue := EvaluateExpression(APattern.Expression.ObjectExpr, AContext);
  AssignPrivateMemberValue(ObjectValue, APattern.Expression.PrivateName,
    AValue, AContext, APattern.Line, APattern.Column);
end;

procedure AssignPattern(const APattern: TGocciaDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
begin
  if APattern is TGocciaIdentifierDestructuringPattern then
    AssignIdentifierPattern(TGocciaIdentifierDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaMemberExpressionDestructuringPattern then
    AssignMemberExpressionPattern(TGocciaMemberExpressionDestructuringPattern(APattern), AValue, AContext)
  else if APattern is TGocciaPrivateMemberExpressionDestructuringPattern then
    AssignPrivateMemberExpressionPattern(TGocciaPrivateMemberExpressionDestructuringPattern(APattern), AValue, AContext)
  else if APattern is TGocciaArrayDestructuringPattern then
    AssignArrayPattern(TGocciaArrayDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaObjectDestructuringPattern then
    AssignObjectPattern(TGocciaObjectDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaAssignmentDestructuringPattern then
    AssignAssignmentPattern(TGocciaAssignmentDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType)
  else if APattern is TGocciaRestDestructuringPattern then
    AssignRestPattern(TGocciaRestDestructuringPattern(APattern), AValue, AContext, AIsDeclaration, ADeclarationType);
end;

procedure AssignIdentifierPattern(const APattern: TGocciaIdentifierDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
begin
  if AIsDeclaration then
    AContext.Scope.DefineLexicalBinding(APattern.Name, AValue, ADeclarationType)
  else
    AContext.Scope.AssignBinding(APattern.Name, AValue, APattern.Line,
      APattern.Column, AContext.NonStrictMode);
end;

procedure AssignArrayPattern(const APattern: TGocciaArrayDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
var
  Iterator: TGocciaIteratorValue;
  I: Integer;
  ElementValue: TGocciaValue;
  RestElements: TGocciaArrayValue;
  Exhausted: Boolean;
  DoneFlag: Boolean;
  ShouldCloseIterator: Boolean;
  PreparedReference: TPreparedDestructuringReference;

  function DirectNextClosingOnThrow(out ADone: Boolean): TGocciaValue;
  begin
    try
      Result := Iterator.DirectNext(ADone);
    except
      on E: EGocciaGeneratorYield do
        raise;
      on E: EGocciaGeneratorReturn do
      begin
        if not Exhausted then
          CloseIterator(Iterator);
        raise;
      end;
      on E: TGocciaThrowValue do
      begin
        Exhausted := True;
        raise;
      end;
      on E: Exception do
      begin
        Exhausted := True;
        raise;
      end;
    end;
  end;
begin
  if (AValue is TGocciaNullLiteralValue) or (AValue is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(
      Format(SErrorCannotDestructure, [AValue.ToStringLiteral.Value]),
      SSuggestDestructureRequiresIterable);

  // ES2024 §13.15.5.4 IteratorDestructuringAssignmentEvaluation and
  // §13.3.3.6 IteratorBindingInitialization always observe @@iterator,
  // including for array and string values.
  Iterator := GetIteratorFromValue(AValue);
  if not Assigned(Iterator) then
    ThrowTypeError(
      Format(SErrorNotIterable, [AValue.TypeName]),
      SSuggestDestructureRequiresIterable);

  TGarbageCollector.Instance.AddTempRoot(Iterator);
  try
    // Track iterator exhaustion so we don't keep calling next() past done:true.
    // Calling next on an already-done iterator is observable.
    Exhausted := False;
    for I := 0 to APattern.Elements.Count - 1 do
    begin
      if APattern.Elements[I] = nil then
      begin
        if not Exhausted then
        begin
          DirectNextClosingOnThrow(DoneFlag);
          if DoneFlag then
            Exhausted := True;
        end;
        Continue;
      end;

      InitPreparedDestructuringReference(PreparedReference);
      try
        ShouldCloseIterator := not Exhausted;
        try
          PrepareDestructuringAssignmentReference(APattern.Elements[I],
            AContext, PreparedReference);
        except
          on E: EGocciaGeneratorYield do
            raise;
          on E: EGocciaGeneratorReturn do
          begin
            if ShouldCloseIterator then
              CloseIterator(Iterator);
            raise;
          end;
          on E: TGocciaThrowValue do
          begin
            if ShouldCloseIterator then
            begin
              AcquireExceptionObject;
              CloseIteratorPreservingError(Iterator);
            end;
            raise;
          end;
          on E: Exception do
          begin
            if ShouldCloseIterator then
            begin
              AcquireExceptionObject;
              CloseIteratorPreservingError(Iterator);
            end;
            raise;
          end;
        end;

        if APattern.Elements[I] is TGocciaRestDestructuringPattern then
        begin
          RestElements := TGocciaArrayValue.Create;
          TGarbageCollector.Instance.AddTempRoot(RestElements);
          try
            if not Exhausted then
            begin
              repeat
                ElementValue := DirectNextClosingOnThrow(DoneFlag);
                if DoneFlag then
                  Break;
                if not Assigned(ElementValue) then
                  ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
                RestElements.Elements.Add(ElementValue);
              until False;
              Exhausted := True;
            end;
            AssignPatternWithPreparedReference(APattern.Elements[I],
              RestElements, AContext, AIsDeclaration, ADeclarationType,
              PreparedReference);
          finally
            TGarbageCollector.Instance.RemoveTempRoot(RestElements);
          end;
          Break;
        end;

        if Exhausted then
          ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue
        else
        begin
          ElementValue := DirectNextClosingOnThrow(DoneFlag);
          if DoneFlag then
          begin
            Exhausted := True;
            ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          end
          else if not Assigned(ElementValue) then
            ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        end;

        ShouldCloseIterator := not Exhausted;
        try
          AssignPatternWithPreparedReference(APattern.Elements[I],
            ElementValue, AContext, AIsDeclaration, ADeclarationType,
            PreparedReference);
        except
          on E: EGocciaGeneratorYield do
            raise;
          on E: EGocciaGeneratorReturn do
          begin
            if ShouldCloseIterator then
              CloseIterator(Iterator);
            raise;
          end;
          on E: TGocciaThrowValue do
          begin
            if ShouldCloseIterator then
            begin
              AcquireExceptionObject;
              CloseIteratorPreservingError(Iterator);
            end;
            raise;
          end;
          on E: Exception do
          begin
            if ShouldCloseIterator then
            begin
              AcquireExceptionObject;
              CloseIteratorPreservingError(Iterator);
            end;
            raise;
          end;
        end;
      finally
        ReleasePreparedDestructuringReference(PreparedReference);
      end;
    end;
    CloseIterator(Iterator);
  finally
    TGarbageCollector.Instance.RemoveTempRoot(Iterator);
  end;
end;

procedure AssignObjectPattern(const APattern: TGocciaObjectDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
var
  ObjectValue: TGocciaObjectValue;
  Prop: TGocciaDestructuringProperty;
  PropValue, ComputedPropertyKey: TGocciaValue;
  Key: string;
  SymbolKey: TGocciaSymbolValue;
  RestObject: TGocciaObjectValue;
  UsedKeys: TStringList;
  UsedSymbolKeys: TList<TGocciaSymbolValue>;
  I: Integer;
  PreparedReference: TPreparedDestructuringReference;
begin
  if (AValue is TGocciaNullLiteralValue) or
     (AValue is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(
      Format(SErrorCannotDestructure, [AValue.ToStringLiteral.Value]),
      SSuggestDestructureRequiresObject);

  ObjectValue := ToObject(AValue);
  UsedKeys := TStringList.Create;
  UsedSymbolKeys := TList<TGocciaSymbolValue>.Create;

  try
    // Use indexed for loop to ensure properties are processed in source order
    for I := 0 to APattern.Properties.Count - 1 do
    begin
      Prop := APattern.Properties[I];
      if Prop.Pattern is TGocciaRestDestructuringPattern then
      begin
        // Rest pattern: collect remaining properties
        RestObject := TGocciaObjectValue.Create(
          TGocciaObjectValue.SharedObjectPrototype);
        CopyDataProperties(RestObject, ObjectValue, UsedKeys, UsedSymbolKeys);
        AssignPattern(TGocciaRestDestructuringPattern(Prop.Pattern).Argument, RestObject, AContext, AIsDeclaration, ADeclarationType);
      end
      else
      begin
        // Regular property — ES2026 §13.5.5 ObjectAssignmentPattern routes
        // computed keys through ToPropertyKey.
        InitPreparedDestructuringReference(PreparedReference);
        try
          SymbolKey := nil;
          if Prop.Computed then
          begin
            ComputedPropertyKey := ToPropertyKey(
              EvaluateExpression(Prop.KeyExpression, AContext));
            if ComputedPropertyKey is TGocciaSymbolValue then
            begin
              SymbolKey := TGocciaSymbolValue(ComputedPropertyKey);
              UsedSymbolKeys.Add(SymbolKey);
            end
            else
              Key := TGocciaStringLiteralValue(ComputedPropertyKey).Value;
          end
          else
            Key := Prop.Key;

          if not AIsDeclaration then
            PrepareDestructuringAssignmentReference(Prop.Pattern, AContext,
              PreparedReference);

          if Assigned(SymbolKey) then
          begin
            // Keep the class dispatch explicit to mirror binding patterns.
            if ObjectValue is TGocciaClassValue then
              PropValue := TGocciaClassValue(ObjectValue).GetSymbolProperty(SymbolKey)
            else
              PropValue := ObjectValue.GetSymbolProperty(SymbolKey);
          end
          else
          begin
            UsedKeys.Add(Key);
            PropValue := ObjectValue.GetProperty(Key);
          end;

          AssignPatternWithPreparedReference(Prop.Pattern, PropValue, AContext,
            AIsDeclaration, ADeclarationType, PreparedReference);
        finally
          ReleasePreparedDestructuringReference(PreparedReference);
        end;
      end;
    end;
  finally
    UsedSymbolKeys.Free;
    UsedKeys.Free;
  end;
end;

procedure AssignAssignmentPattern(const APattern: TGocciaAssignmentDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
var
  DefaultValue: TGocciaValue;
  PreparedReference: TPreparedDestructuringReference;
begin
  InitPreparedDestructuringReference(PreparedReference);
  try
    PrepareDestructuringAssignmentReference(APattern.Left, AContext,
      PreparedReference);

    if AValue is TGocciaUndefinedLiteralValue then
    begin
      DefaultValue := EvaluateExpression(APattern.Right, AContext);
      ApplyInferredNameForDefaultInitializer(APattern.Left, APattern.Right,
        DefaultValue);
    end
    else
      DefaultValue := AValue;

    AssignPatternWithPreparedReference(APattern.Left, DefaultValue, AContext,
      AIsDeclaration, ADeclarationType, PreparedReference);
  finally
    ReleasePreparedDestructuringReference(PreparedReference);
  end;
end;

procedure AssignRestPattern(const APattern: TGocciaRestDestructuringPattern; const AValue: TGocciaValue; const AContext: TGocciaEvaluationContext; const AIsDeclaration: Boolean = False; const ADeclarationType: TGocciaDeclarationType = dtLet);
begin
  AssignPattern(APattern.Argument, AValue, AContext, AIsDeclaration, ADeclarationType);
end;

function IsObjectInstanceOfClass(const AObj: TGocciaObjectValue; const AClassValue: TGocciaClassValue): Boolean;
var
  CurrentPrototype: TGocciaObjectValue;
  TargetPrototype: TGocciaObjectValue;
begin
  Result := False;

  // Get the target prototype we're looking for
  TargetPrototype := AClassValue.Prototype;
  if not Assigned(TargetPrototype) then
    Exit;

  // Walk up the prototype chain of the object
  CurrentPrototype := AObj.Prototype;
  while Assigned(CurrentPrototype) do
  begin
    // Check if the current prototype is the target prototype
    if CurrentPrototype = TargetPrototype then
    begin
      Result := True;
      Exit;
    end;

    // Move up the prototype chain
    CurrentPrototype := CurrentPrototype.Prototype;
  end;
end;

function EvaluateDelete(const AOperand: TGocciaExpression; const AContext: TGocciaEvaluationContext): TGocciaValue;
var
  MemberExpr: TGocciaMemberExpression;
  ObjValue: TGocciaValue;
  PropertyName: string;
  PropertyKey: TGocciaValue;
  IsSymbolKey: Boolean;
  SymbolKey: TGocciaSymbolValue;
  ArrayValue: TGocciaArrayValue;
  ObjectValue: TGocciaObjectValue;
  Index: Integer;
begin
  IsSymbolKey := False;
  SymbolKey := nil;
  PropertyName := '';
  // Handle member expressions (property deletion)
  if AOperand is TGocciaMemberExpression then
  begin
    MemberExpr := TGocciaMemberExpression(AOperand);
    if MemberExpr.ObjectExpr is TGocciaSuperExpression then
    begin
      ResolveSuperThisValue(AContext);
      ThrowReferenceError('Cannot delete super property',
        'super property references cannot be deleted');
      Result := TGocciaBooleanLiteralValue.TrueValue;
      Exit;
    end;

    ObjValue := EvaluateExpression(MemberExpr.ObjectExpr, AContext);

    if (ObjValue is TGocciaNullLiteralValue) or
       (ObjValue is TGocciaUndefinedLiteralValue) then
    begin
      if MemberExpr.Optional then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;

    if MemberExpr.Computed and Assigned(MemberExpr.PropertyExpression) then
    begin
      PropertyKey := EvaluateExpression(MemberExpr.PropertyExpression, AContext);

      if (ObjValue is TGocciaNullLiteralValue) or
         (ObjValue is TGocciaUndefinedLiteralValue) then
      begin
        if PropertyKey is TGocciaSymbolValue then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOf,
            [ObjValue.ToStringLiteral.Value,
             TGocciaSymbolValue(PropertyKey).ToDisplayString.Value]),
            SSuggestCheckNullBeforeAccess)
        else if PropertyKey is TGocciaStringLiteralValue then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOf,
            [ObjValue.ToStringLiteral.Value,
             TGocciaStringLiteralValue(PropertyKey).Value]),
            SSuggestCheckNullBeforeAccess)
        else if PropertyKey is TGocciaNumberLiteralValue then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOf,
            [ObjValue.ToStringLiteral.Value,
             FormatDouble(TGocciaNumberLiteralValue(PropertyKey).Value)]),
            SSuggestCheckNullBeforeAccess)
        else
          ThrowTypeError(Format(SErrorCannotReadPropertiesOf,
            [ObjValue.ToStringLiteral.Value, '<computed>']),
            SSuggestCheckNullBeforeAccess);
      end;

      PropertyKey := ToPropertyKey(PropertyKey);
      if PropertyKey is TGocciaSymbolValue then
      begin
        IsSymbolKey := True;
        SymbolKey := TGocciaSymbolValue(PropertyKey);
      end
      else
        PropertyName := TGocciaStringLiteralValue(PropertyKey).Value;
    end
    else
    begin
      PropertyName := MemberExpr.PropertyName;
      if (ObjValue is TGocciaNullLiteralValue) or
         (ObjValue is TGocciaUndefinedLiteralValue) then
        ThrowTypeError(Format(SErrorCannotReadPropertiesOf,
          [ObjValue.ToStringLiteral.Value, PropertyName]),
          SSuggestCheckNullBeforeAccess);
    end;

    if IsSymbolKey then
    begin
      if ObjValue is TGocciaObjectValue then
      begin
        if not TGocciaObjectValue(ObjValue).DeleteSymbolProperty(SymbolKey) then
        begin
          if AContext.NonStrictMode then
          begin
            Result := TGocciaBooleanLiteralValue.FalseValue;
            Exit;
          end;
          ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
            [SymbolKey.ToDisplayString.Value, '[object Object]']),
            SSuggestCannotDeleteNonConfigurable);
        end;
      end;
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    // Handle array element deletion
    else if ObjValue is TGocciaArrayValue then
    begin
      ArrayValue := TGocciaArrayValue(ObjValue);
      if not ArrayValue.DeleteProperty(PropertyName) then
      begin
        if AContext.NonStrictMode then
        begin
          Result := TGocciaBooleanLiteralValue.FalseValue;
          Exit;
        end;
        ThrowTypeError(Format(SErrorCannotDeletePropertyOf, [PropertyName, '[object Array]']),
          SSuggestCannotDeleteNonConfigurable);
      end;
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else if ObjValue is TGocciaObjectValue then
    begin
      ObjectValue := TGocciaObjectValue(ObjValue);
      if not ObjectValue.DeleteProperty(PropertyName) then
      begin
        if AContext.NonStrictMode then
        begin
          Result := TGocciaBooleanLiteralValue.FalseValue;
          Exit;
        end;
        ThrowTypeError(Format(SErrorCannotDeletePropertyOf, [PropertyName, '[object Object]']),
          SSuggestCannotDeleteNonConfigurable);
      end;
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end
    else
    begin
      // Cannot delete properties from primitives, null, or undefined
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end
  else
  begin
    // Handle other expressions according to strict mode semantics
    if AOperand is TGocciaIdentifierExpression then
    begin
      if AContext.NonStrictMode then
      begin
        if AContext.Scope.DeleteBinding(TGocciaIdentifierExpression(AOperand).Name) then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
        Exit;
      end;

      // In strict mode, attempting to delete variables/identifiers throws TypeError
      AContext.OnError('Delete of an unqualified identifier in strict mode',
        AOperand.Line, AOperand.Column);
      Result := TGocciaBooleanLiteralValue.TrueValue; // Fallback if OnError doesn't throw
    end
    else
    begin
      // For literals and other non-reference expressions, evaluate the
      // operand for side effects, then return true.
      EvaluateExpression(AOperand, AContext);
      Result := TGocciaBooleanLiteralValue.TrueValue;
    end;
  end;
end;

end.
