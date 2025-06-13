unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ArrayValue, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Builtins.Base, Generics.Collections, Goccia.Values.BooleanValue, Goccia.Values.Base, Goccia.Scope, Goccia.Error;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  protected
    function IsArray(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
  end;

implementation

constructor TGocciaGlobalArray.Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.SetProperty('isArray', TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1));

  AScope.SetValue(AName, FBuiltinObject);
end;

function TGocciaGlobalArray.IsArray(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanValue.Create(Args[0] is TGocciaArrayValue);
end;

end.