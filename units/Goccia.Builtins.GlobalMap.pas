unit Goccia.Builtins.GlobalMap;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ArrayValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaGlobalMap = class(TGocciaBuiltin)
  private
    FMapConstructor: TGocciaNativeFunctionValue;

    function MapConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalMap.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FMapConstructor := TGocciaNativeFunctionValue.Create(MapConstructorFn, 'Map', 0);
  TGocciaMapValue.ExposePrototype(FMapConstructor);
end;

function TGocciaGlobalMap.MapConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  MapObj: TGocciaMapValue;
  InitArg: TGocciaValue;
  ArrValue: TGocciaArrayValue;
  EntryArr: TGocciaArrayValue;
  I: Integer;
begin
  MapObj := TGocciaMapValue.Create;

  if AArgs.Length > 0 then
  begin
    InitArg := AArgs.GetElement(0);

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
