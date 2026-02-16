unit Goccia.Builtins.Globals;

{$I Goccia.inc}

interface

uses
  SysUtils, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.FunctionValue, 
  Goccia.Values.Primitives, Goccia.Arguments.Collection, Generics.Collections, Math, Goccia.Builtins.Base, Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback;

type
  TGocciaGlobals = class(TGocciaBuiltin)
  private
    FErrorProto: TGocciaObjectValue;
    FTypeErrorProto: TGocciaObjectValue;
    FReferenceErrorProto: TGocciaObjectValue;
    FRangeErrorProto: TGocciaObjectValue;
  protected
    // Error constructors
    function ErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function TypeErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function RangeErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses Goccia.Values.ClassHelper, Goccia.Values.ErrorHelper;

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  ErrorConstructorFunc: TGocciaNativeFunctionValue;
  TypeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ReferenceErrorConstructorFunc: TGocciaNativeFunctionValue;
  RangeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ErrorProto, TypeErrorProto, ReferenceErrorProto, RangeErrorProto: TGocciaObjectValue;
begin
  inherited Create(AName, AScope, AThrowError);

  // Global constants
  AScope.DefineLexicalBinding('undefined', TGocciaUndefinedLiteralValue.UndefinedValue, dtUnknown);
  AScope.DefineLexicalBinding('NaN', TGocciaNumberLiteralValue.NaNValue, dtUnknown);
  AScope.DefineLexicalBinding('Infinity', TGocciaNumberLiteralValue.InfinityValue, dtUnknown);

  // Set up Error prototype chain (ECMAScript spec):
  // TypeError.prototype -> Error.prototype -> Object.prototype
  FErrorProto := TGocciaObjectValue.Create;
  FErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('Error'));
  FErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FTypeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FTypeErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('TypeError'));
  FTypeErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FReferenceErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FReferenceErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('ReferenceError'));
  FReferenceErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  FRangeErrorProto := TGocciaObjectValue.Create(FErrorProto);
  FRangeErrorProto.AssignProperty('name', TGocciaStringLiteralValue.Create('RangeError'));
  FRangeErrorProto.AssignProperty('message', TGocciaStringLiteralValue.Create(''));

  // Error constructors - store references so we can use them in the constructor functions
  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, 'Error', 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, 'TypeError', 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, 'ReferenceError', 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, 'RangeError', 1);

  // Set .prototype on each constructor (used by instanceof)
  ErrorConstructorFunc.AssignProperty('prototype', FErrorProto);
  TypeErrorConstructorFunc.AssignProperty('prototype', FTypeErrorProto);
  ReferenceErrorConstructorFunc.AssignProperty('prototype', FReferenceErrorProto);
  RangeErrorConstructorFunc.AssignProperty('prototype', FRangeErrorProto);

  AScope.DefineLexicalBinding('Error', ErrorConstructorFunc, dtUnknown);
  AScope.DefineLexicalBinding('TypeError', TypeErrorConstructorFunc, dtUnknown);
  AScope.DefineLexicalBinding('ReferenceError', ReferenceErrorConstructorFunc, dtUnknown);
  AScope.DefineLexicalBinding('RangeError', RangeErrorConstructorFunc, dtUnknown);

  // Note: parseInt, parseFloat, isNaN, isFinite are intentionally NOT registered as globals.
  // They are available only on the Number object (e.g. Number.parseInt, Number.isNaN).
  // If needed, they can be polyfilled:
  //   const parseInt = Number.parseInt;
  //   const parseFloat = Number.parseFloat;
  //   const isNaN = Number.isNaN;
  //   const isFinite = Number.isFinite;
end;

function TGocciaGlobals.ErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('Error', Message);
  ErrorObj.Prototype := FErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.TypeErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('TypeError', Message);
  ErrorObj.Prototype := FTypeErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.ReferenceErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('ReferenceError', Message);
  ErrorObj.Prototype := FReferenceErrorProto;
  Result := ErrorObj;
end;

function TGocciaGlobals.RangeErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  Message: string;
  ErrorObj: TGocciaObjectValue;
begin
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj := CreateErrorObject('RangeError', Message);
  ErrorObj.Prototype := FRangeErrorProto;
  Result := ErrorObj;
end;

end.
