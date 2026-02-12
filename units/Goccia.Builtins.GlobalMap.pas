unit Goccia.Builtins.GlobalMap;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error.ThrowErrorCallback,
  Goccia.Values.Primitives, Goccia.Values.MapValue,
  Goccia.Values.NativeFunction, Goccia.Values.ArrayValue,
  Goccia.Arguments.Collection,
  Generics.Collections, SysUtils;

type
  TGocciaGlobalMap = class(TGocciaBuiltin)
  private
    FMapConstructor: TGocciaNativeFunctionValue;

    function MapConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalMap.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FMapConstructor := TGocciaNativeFunctionValue.Create(MapConstructorFn, 'Map', 0);

  AScope.DefineBuiltin(AName, FMapConstructor);
end;

function TGocciaGlobalMap.MapConstructorFn(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  MapObj: TGocciaMapValue;
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  EntryArr: TGocciaArrayValue;
  I: Integer;
begin
  MapObj := TGocciaMapValue.Create;

  if Args.Length > 0 then
  begin
    InitArg := Args.GetElement(0);

    // Initialize from array of [key, value] pairs
    if InitArg is TGocciaArrayValue then
    begin
      ArrValue := TGocciaArrayValue(InitArg);
      for I := 0 to ArrValue.Elements.Count - 1 do
      begin
        if (ArrValue.Elements[I] <> nil) and (ArrValue.Elements[I] is TGocciaArrayValue) then
        begin
          EntryArr := TGocciaArrayValue(ArrValue.Elements[I]);
          if EntryArr.Elements.Count >= 2 then
            MapObj.SetEntry(EntryArr.Elements[0], EntryArr.Elements[1]);
        end;
      end;
    end;
  end;

  Result := MapObj;
end;

end.
