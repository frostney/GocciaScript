unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ArrayValue, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue,
  Goccia.Builtins.Base, Generics.Collections, Goccia.Values.BooleanValue, Goccia.Values.Base, Goccia.Scope,
  Goccia.Error, Goccia.Values.ObjectPropertyDescriptor;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  protected
    function IsArray(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.DefineProperty('isArray', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1), [pfConfigurable, pfWritable]));

  AScope.SetValue(AName, FBuiltinObject);
end;

function TGocciaGlobalArray.IsArray(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaBooleanValue.Create(Args[0] is TGocciaArrayValue);
end;

end.
