unit Goccia.Builtins.Globals;

{$I Goccia.inc}

interface

uses
  SysUtils, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.FunctionValue, Goccia.Values.UndefinedValue, Generics.Collections, Math, Goccia.Builtins.Base, Goccia.Values.NumberValue, Goccia.Values.BooleanValue, Goccia.Values.Base, Goccia.Scope, Goccia.Error;

type
  TGocciaGlobals = class(TGocciaBuiltin)
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
begin
  inherited Create(AName, AScope, AThrowError);

  // Global constants
  AScope.DefineBuiltin('NaN', TGocciaNumberValue.Create(Math.NaN));
  AScope.DefineBuiltin('Infinity', TGocciaNumberValue.Create(Math.Infinity));

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


end.
