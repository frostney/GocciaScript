unit Souffle.VM.NativeFunction;

{$I Souffle.inc}

interface

uses
  Souffle.Heap,
  Souffle.Value;

type
  TSouffleNativeCallback = function(
    const AReceiver: TSouffleValue;
    const AArgs: PSouffleValue;
    const AArgCount: Integer): TSouffleValue;

  TSouffleNativeFunction = class(TSouffleHeapObject)
  private
    FCallback: TSouffleNativeCallback;
    FName: string;
    FArity: Integer;
  public
    constructor Create(const AName: string; const AArity: Integer;
      const ACallback: TSouffleNativeCallback);

    function Invoke(const AReceiver: TSouffleValue;
      const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; inline;

    function DebugString: string; override;

    property Name: string read FName;
    property Arity: Integer read FArity;
  end;

implementation

{ TSouffleNativeFunction }

constructor TSouffleNativeFunction.Create(const AName: string;
  const AArity: Integer; const ACallback: TSouffleNativeCallback);
begin
  inherited Create(SOUFFLE_HEAP_NATIVE_FUNCTION);
  FName := AName;
  FArity := AArity;
  FCallback := ACallback;
end;

function TSouffleNativeFunction.Invoke(const AReceiver: TSouffleValue;
  const AArgs: PSouffleValue; const AArgCount: Integer): TSouffleValue;
begin
  Result := FCallback(AReceiver, AArgs, AArgCount);
end;

function TSouffleNativeFunction.DebugString: string;
begin
  Result := '<native:' + FName + '>';
end;

end.
