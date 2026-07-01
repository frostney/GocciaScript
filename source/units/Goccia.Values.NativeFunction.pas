unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunctionCallback;
    FConstructCallback: TGocciaNativeConstructorCallback;
    FCachedIntrinsicProto: TGocciaObjectValue;
    FName: string;
    FArity: Integer;
    FNotConstructable: Boolean;
    FDirectEvalHost: Boolean;
    FCapturedRoot: TGocciaValue;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AFunction: TGocciaNativeFunctionCallback; const AName: string;
      const AArity: Integer);
    constructor CreateWithoutPrototype(const AFunction: TGocciaNativeFunctionCallback; const AName: string;
      const AArity: Integer);
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function Construct(const AArguments: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
    function IsConstructable: Boolean; override;
    procedure MarkReferences; override;
    property NativeFunction: TGocciaNativeFunctionCallback read FFunction;
    property ConstructCallback: TGocciaNativeConstructorCallback read FConstructCallback write FConstructCallback;
    property Name: string read FName;
    property Arity: Integer read FArity;
    property NotConstructable: Boolean read FNotConstructable write FNotConstructable;
    property DirectEvalHost: Boolean read FDirectEvalHost write FDirectEvalHost;
    property CapturedRoot: TGocciaValue read FCapturedRoot write FCapturedRoot;
  end;


implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Realm;

constructor TGocciaNativeFunctionValue.Create(const AFunction: TGocciaNativeFunctionCallback;
  const AName: string; const AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create;
end;

constructor TGocciaNativeFunctionValue.CreateWithoutPrototype(const AFunction: TGocciaNativeFunctionCallback;
  const AName: string; const AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;
  FNotConstructable := True;

  inherited Create; // No prototype for methods that are part of the prototype
end;

function TGocciaNativeFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  PreviousRealm: TGocciaRealm;
begin
  PreviousRealm := CurrentRealm;
  if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
    SetCurrentRealm(FCreationRealm);
  try
    Result := FFunction(AArguments, AThisValue);
  finally
    if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function TGocciaNativeFunctionValue.Construct(const AArguments: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
var
  PreviousRealm: TGocciaRealm;
  ProtoValue: TGocciaValue;
begin
  PreviousRealm := CurrentRealm;
  if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
    SetCurrentRealm(FCreationRealm);
  try
    if Assigned(FConstructCallback) then
      Result := FConstructCallback(AArguments, ANewTarget)
    else
    begin
      Result := FFunction(AArguments, TGocciaHoleValue.HoleValue);
      // Error / Promise register explicit ConstructCallbacks because their
      // specs require prototype resolution before argument coercion.
      if (TGocciaValue(ANewTarget) <> TGocciaValue(Self)) and
         (Result is TGocciaObjectValue) then
      begin
        if not Assigned(FCachedIntrinsicProto) then
        begin
          ProtoValue := Self.GetProperty(PROP_PROTOTYPE);
          if ProtoValue is TGocciaObjectValue then
            FCachedIntrinsicProto := TGocciaObjectValue(ProtoValue)
          else
            FCachedIntrinsicProto := TGocciaObjectValue.SharedObjectPrototype;
        end;
        TGocciaObjectValue(Result).Prototype :=
          GetProtoFromConstructorWithIntrinsic(ANewTarget, FCachedIntrinsicProto);
      end;
    end;
  finally
    if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function TGocciaNativeFunctionValue.IsConstructable: Boolean;
begin
  Result := not FNotConstructable;
end;

procedure TGocciaNativeFunctionValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FCapturedRoot) then
    FCapturedRoot.MarkReferences;
end;

function TGocciaNativeFunctionValue.GetFunctionLength: Integer;
begin
  // -1 means variadic, report 0 for length per ECMAScript spec
  if FArity < 0 then
    Result := 0
  else
    Result := FArity;
end;

function TGocciaNativeFunctionValue.GetFunctionName: string;
begin
  Result := FName;
end;

end.
