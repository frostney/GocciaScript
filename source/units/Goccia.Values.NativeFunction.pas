unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives;

type
  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunctionCallback;
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
    function IsConstructable: Boolean; override;
    property NativeFunction: TGocciaNativeFunctionCallback read FFunction;
    property Name: string read FName;
    property Arity: Integer read FArity;
    property NotConstructable: Boolean read FNotConstructable write FNotConstructable;
  end;


implementation

uses
  Goccia.GarbageCollector;

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
  GC: TGarbageCollector;
  I, RootCount: Integer;
begin
  GC := TGarbageCollector.Instance;
  RootCount := 0;
  if Assigned(GC) then
  begin
    GC.PushActiveRoot(Self);
    Inc(RootCount);
    if Assigned(AThisValue) then
    begin
      GC.PushActiveRoot(AThisValue);
      Inc(RootCount);
    end;
    if Assigned(AArguments) then
      for I := 0 to AArguments.Length - 1 do
        if Assigned(AArguments.GetElement(I)) then
        begin
          if AArguments.GetElement(I).IsCallable then
            AArguments.GetElement(I).MarkEscapedReferences;
          GC.PushActiveRoot(AArguments.GetElement(I));
          Inc(RootCount);
        end;
  end;
  try
    Result := FFunction(AArguments, AThisValue);
  finally
    while RootCount > 0 do
    begin
      GC.PopActiveRoot;
      Dec(RootCount);
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
