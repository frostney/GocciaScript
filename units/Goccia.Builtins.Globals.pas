unit Goccia.Builtins.Globals;

{$I Goccia.inc}

interface

uses
  SysUtils, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.FunctionValue, 
  Goccia.Values.Primitives, Goccia.Arguments.Collection, Generics.Collections, Math, Goccia.Builtins.Base, Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback;

type
  TGocciaGlobals = class(TGocciaBuiltin)
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

uses Goccia.Values.ClassHelper;

constructor TGocciaGlobals.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  NumberValue: TGocciaValue;
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
  AScope.DefineLexicalBinding('undefined', TGocciaUndefinedLiteralValue.UndefinedValue, dtUnknown);
  AScope.DefineLexicalBinding('NaN', TGocciaNumberLiteralValue.NaNValue, dtUnknown);
  AScope.DefineLexicalBinding('Infinity', TGocciaNumberLiteralValue.InfinityValue, dtUnknown);

  // Error constructors - store references so we can use them in the constructor functions
  ErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ErrorConstructor, 'Error', 1);
  TypeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(TypeErrorConstructor, 'TypeError', 1);
  ReferenceErrorConstructorFunc := TGocciaNativeFunctionValue.Create(ReferenceErrorConstructor, 'ReferenceError', 1);
  RangeErrorConstructorFunc := TGocciaNativeFunctionValue.Create(RangeErrorConstructor, 'RangeError', 1);

  AScope.DefineLexicalBinding('Error', ErrorConstructorFunc, dtUnknown);
  AScope.DefineLexicalBinding('TypeError', TypeErrorConstructorFunc, dtUnknown);
  AScope.DefineLexicalBinding('ReferenceError', ReferenceErrorConstructorFunc, dtUnknown);
  AScope.DefineLexicalBinding('RangeError', RangeErrorConstructorFunc, dtUnknown);

  // Get Number object and its methods to make global aliases
  // Only proceed if Number is defined in scope
  if AScope.Contains('Number') then
  begin
    NumberValue := AScope.GetValue('Number');
    if Assigned(NumberValue) and (NumberValue is TGocciaObjectValue) then
    begin
      NumberObject := TGocciaObjectValue(NumberValue);
      NumberParseInt := NumberObject.GetProperty('parseInt');
      NumberParseFloat := NumberObject.GetProperty('parseFloat');
      NumberIsNaN := NumberObject.GetProperty('isNaN');
      NumberIsFinite := NumberObject.GetProperty('isFinite');

      // Global functions as aliases to Number static methods
      if Assigned(NumberParseFloat) then
        AScope.DefineLexicalBinding('parseFloat', NumberParseFloat, dtUnknown);
      if Assigned(NumberParseInt) then
        AScope.DefineLexicalBinding('parseInt', NumberParseInt, dtUnknown);
      if Assigned(NumberIsNaN) then
        AScope.DefineLexicalBinding('isNaN', NumberIsNaN, dtUnknown);
      if Assigned(NumberIsFinite) then
        AScope.DefineLexicalBinding('isFinite', NumberIsFinite, dtUnknown);
    end;
  end;
  // Do we care if Number is not available?
  // Should we bind them direct from source?
end;

function TGocciaGlobals.ErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('Error'));

  // Set message property
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

function TGocciaGlobals.TypeErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('TypeError'));

  // Set message property
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

function TGocciaGlobals.ReferenceErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('ReferenceError'));

  // Set message property
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

function TGocciaGlobals.RangeErrorConstructor(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  ErrorObj: TGocciaObjectValue;
  Message: string;
begin
  // Create a basic error object
  ErrorObj := TGocciaObjectValue.Create;

  // Set name property
  ErrorObj.AssignProperty('name', TGocciaStringLiteralValue.Create('RangeError'));

  // Set message property
  if Args.Length > 0 then
    Message := Args.GetElement(0).ToStringLiteral.Value
  else
    Message := '';
  ErrorObj.AssignProperty('message', TGocciaStringLiteralValue.Create(Message));

  Result := ErrorObj;
end;

end.
