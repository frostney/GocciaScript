unit Goccia.Builtins.GlobalSet;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error.ThrowErrorCallback,
  Goccia.Values.Primitives, Goccia.Values.SetValue,
  Goccia.Values.NativeFunction, Goccia.Values.ArrayValue,
  Goccia.Arguments.Collection,
  Generics.Collections, SysUtils;

type
  TGocciaGlobalSet = class(TGocciaBuiltin)
  private
    FSetConstructor: TGocciaNativeFunctionValue;

    function SetConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalSet.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FSetConstructor := TGocciaNativeFunctionValue.Create(SetConstructorFn, 'Set', 0);

  AScope.DefineLexicalBinding(AName, FSetConstructor, dtLet);
end;

function TGocciaGlobalSet.SetConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  SetObj: TGocciaSetValue;
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  I: Integer;
begin
  SetObj := TGocciaSetValue.Create;

  if Args.Length > 0 then
  begin
    InitArg := Args.GetElement(0);

    // Initialize from array iterable
    if InitArg is TGocciaArrayValue then
    begin
      ArrValue := TGocciaArrayValue(InitArg);
      for I := 0 to ArrValue.Elements.Count - 1 do
      begin
        if ArrValue.Elements[I] <> nil then
          SetObj.AddItem(ArrValue.Elements[I]);
      end;
    end;
    // Initialize from another Set
    if InitArg is TGocciaSetValue then
    begin
      for I := 0 to TGocciaSetValue(InitArg).Items.Count - 1 do
        SetObj.AddItem(TGocciaSetValue(InitArg).Items[I]);
    end;
  end;

  Result := SetObj;
end;

end.
