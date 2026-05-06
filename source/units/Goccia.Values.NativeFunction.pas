unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives;

type
  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunctionCallback;
    FConstructCallback: TGocciaNativeConstructorCallback;
    FName: string;
    FArity: Integer;
    FNotConstructable: Boolean;
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
    property NativeFunction: TGocciaNativeFunctionCallback read FFunction;
    property ConstructCallback: TGocciaNativeConstructorCallback read FConstructCallback write FConstructCallback;
    property Name: string read FName;
    property Arity: Integer read FArity;
    property NotConstructable: Boolean read FNotConstructable write FNotConstructable;
  end;


implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.ObjectValue;

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
begin
  Result := FFunction(AArguments, AThisValue);
end;

function TGocciaNativeFunctionValue.Construct(const AArguments: TGocciaArgumentsCollection; const ANewTarget: TGocciaValue): TGocciaValue;
var
  Proto: TGocciaObjectValue;
  ProtoValue: TGocciaValue;
begin
  if Assigned(FConstructCallback) then
    Result := FConstructCallback(AArguments, ANewTarget)
  else
  begin
    Result := FFunction(AArguments, TGocciaHoleValue.HoleValue);
    // §10.1.14 auto-patch: when no explicit [[Construct]] callback is
    // registered and newTarget differs from the constructor, resolve the
    // prototype from newTarget and apply it to the result. Constructors
    // with ordering requirements (Error, Promise) register an explicit
    // ConstructCallback instead.
    if (Result is TGocciaObjectValue) and
       (TGocciaValue(ANewTarget) <> TGocciaValue(Self)) then
    begin
      ProtoValue := Self.GetProperty(PROP_PROTOTYPE);
      if ProtoValue is TGocciaObjectValue then
        Proto := TGocciaObjectValue(ProtoValue)
      else
        Proto := TGocciaObjectValue.SharedObjectPrototype;
      TGocciaObjectValue(Result).Prototype :=
        GetProtoFromConstructorWithIntrinsic(ANewTarget, Proto);
    end;
  end;
end;

function TGocciaNativeFunctionValue.IsConstructable: Boolean;
begin
  Result := not FNotConstructable;
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
