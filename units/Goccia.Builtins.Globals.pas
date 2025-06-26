unit Goccia.Builtins.Globals;

{$I Goccia.inc}

interface

uses
  SysUtils, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.FunctionValue, Goccia.Values.Primitives, Generics.Collections, Math, Goccia.Builtins.Base, Goccia.Values.Core, Goccia.Scope, Goccia.Error;

type
  TGocciaGlobals = class(TGocciaBuiltin)
  protected
    // Error constructors
    function ErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function TypeErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ReferenceErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function RangeErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
var
  NumberObject: TGocciaObjectValue;
  NumberParseInt: TGocciaValue;
  NumberParseFloat: TGocciaValue;
  NumberIsNaN: TGocciaValue;
  NumberIsFinite: TGocciaValue;
  ErrorConstructorFunc: TGocciaNativeFunctionValue;
  TypeErrorConstructorFunc: TGocciaNativeFunctionValue;
  ReferenceErrorConstructorFunc: TGocciaNativeFunctionValue;
  RangeErrorConstructorFunc: TGocciaNativeFunctionValue;
begin
  inherited Create(AName, AScope, AThrowError);

  // Global constants
  AScope.DefineBuiltin('undefined', TGocciaUndefinedLiteralValue.Create);
  AScope.DefineBuiltin('NaN', TGocciaNumberLiteralValue.NaNValue);
  AScope.DefineBuiltin('Infinity', TGocciaNumberLiteralValue.InfinityValue);

  // Error constructors - store references so we can use them in the constructor functions
  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, 'Error', 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, 'TypeError', 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, 'ReferenceError', 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, 'RangeError', 1);

  AScope.DefineBuiltin('Error', ErrorConstructorFunc);
  AScope.DefineBuiltin('TypeError', TypeErrorConstructorFunc);
  AScope.DefineBuiltin('ReferenceError', ReferenceErrorConstructorFunc);
  AScope.DefineBuiltin('RangeError', RangeErrorConstructorFunc);

  // Get Number object and its methods to make global aliases
  NumberObject := TGocciaObjectValue(AScope.GetValue('Number'));
  if Assigned(NumberObject) and (NumberObject is TGocciaObjectValue) then
  begin
    NumberParseInt := NumberObject.GetProperty('parseInt');
    NumberParseFloat := NumberObject.GetProperty('parseFloat');
    NumberIsNaN := NumberObject.GetProperty('isNaN');
    NumberIsFinite := NumberObject.GetProperty('isFinite');

    // Global functions as aliases to Number static methods
    if Assigned(NumberParseFloat) then
      AScope.DefineBuiltin('parseFloat', NumberParseFloat);
    if Assigned(NumberParseInt) then
      AScope.DefineBuiltin('parseInt', NumberParseInt);
    if Assigned(NumberIsNaN) then
      AScope.DefineBuiltin('isNaN', NumberIsNaN);
    if Assigned(NumberIsFinite) then
      AScope.DefineBuiltin('isFinite', NumberIsFinite);
  end;
  // Do we care if Number is not available?
  // Should we bind them direct from source?
end;

function TGocciaGlobals.ErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('Error'));

  // Set message property
  if Args.Count > 0 then
    Message := Args[0].ToString
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

function TGocciaGlobals.TypeErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('TypeError'));

  // Set message property
  if Args.Count > 0 then
    Message := Args[0].ToString
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

function TGocciaGlobals.ReferenceErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('ReferenceError'));

  // Set message property
  if Args.Count > 0 then
    Message := Args[0].ToString
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

function TGocciaGlobals.RangeErrorConstructor(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('RangeError'));

  // Set message property
  if Args.Count > 0 then
    Message := Args[0].ToString
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

end.
