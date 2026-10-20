program Goccia.Values.ProxyValue.Test;

{$I Goccia.inc}

uses
  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.TestSetup,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.ProxyValue;

type
  TCapabilityCountingFunction = class(TGocciaNativeFunctionValue)
  private
    function Invoke(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ConstructInvocation(const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaValue;
  public
    CallableReads, ConstructableReads, Calls, Constructions: Integer;
    LastNewTarget: TGocciaValue;
    constructor Create; reintroduce;
    function IsCallable: Boolean; override;
    function IsConstructable: Boolean; override;
  end;

  TTestProxyValue = class(TTestSuite)
  private
    procedure ExerciseChains(const AConstruct: Boolean);
    procedure TestCallCapabilityQueries;
    procedure TestConstructCapabilityQueries;
  public
    procedure SetupTests; override;
  end;

constructor TCapabilityCountingFunction.Create;
begin
  inherited Create(Invoke, 'countedTarget', 1);
  ConstructCallback := ConstructInvocation;
end;

function TCapabilityCountingFunction.IsCallable: Boolean;
begin
  Inc(CallableReads);
  Result := inherited IsCallable;
end;

function TCapabilityCountingFunction.IsConstructable: Boolean;
begin
  Inc(ConstructableReads);
  Result := inherited IsConstructable;
end;

function TCapabilityCountingFunction.Invoke(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Inc(Calls);
  Result := AArguments.GetElement(0);
end;

function TCapabilityCountingFunction.ConstructInvocation(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Inc(Constructions);
  LastNewTarget := ANewTarget;
  Result := AArguments.GetElement(0);
end;

procedure TTestProxyValue.SetupTests;
begin
  Test('Calls through Proxy chains do not re-query target capabilities',
    TestCallCapabilityQueries);
  Test('Construction through Proxy chains does not re-query target capabilities',
    TestConstructCapabilityQueries);
end;

procedure TTestProxyValue.ExerciseChains(const AConstruct: Boolean);
const
  DEPTHS: array[0..2] of Integer = (1, 8, 64);
  INVOCATIONS = 3;
var
  Target: TCapabilityCountingFunction;
  Handler, Sentinel: TGocciaObjectValue;
  Chain, ResultValue: TGocciaValue;
  Arguments: TGocciaArgumentsCollection;
  Roots: TGocciaActiveRootFrame;
  Depth, I, CreationCallableReads, CreationConstructableReads: Integer;
begin
  for Depth in DEPTHS do
  begin
    Roots.Initialize;
    Arguments := TGocciaArgumentsCollection.Create;
    try
      Target := TCapabilityCountingFunction.Create;
      Roots.Add(Target);
      Handler := TGocciaObjectValue.Create(nil);
      Roots.Add(Handler);
      Sentinel := TGocciaObjectValue.Create(nil);
      Roots.Add(Sentinel);
      Arguments.Add(Sentinel);
      Target.CallableReads := 0;
      Target.ConstructableReads := 0;
      Chain := Target;
      for I := 1 to Depth do
      begin
        Chain := TGocciaProxyValue.Create(Chain, Handler);
        Roots.Add(Chain);
      end;

      // Only the innermost Proxy needs the terminal target's capabilities.
      // Building or querying more wrappers must use the recorded answers.
      Expect<Boolean>(Chain.IsCallable).ToBe(True);
      Expect<Boolean>(Chain.IsConstructable).ToBe(True);
      CreationCallableReads := Target.CallableReads;
      CreationConstructableReads := Target.ConstructableReads;
      Target.CallableReads := 0;
      Target.ConstructableReads := 0;
      for I := 1 to INVOCATIONS do
      begin
        if AConstruct then
          ResultValue := ConstructValue(Chain, Arguments, Chain)
        else
          ResultValue := DispatchCall(Chain, Arguments,
            TGocciaUndefinedLiteralValue.UndefinedValue);
        Expect<TGocciaValue>(ResultValue).ToBe(Sentinel);
      end;
      Expect<Integer>(Target.CallableReads).ToBe(0);
      Expect<Integer>(Target.ConstructableReads).ToBe(0);
      Expect<Integer>(CreationCallableReads).ToBe(1);
      Expect<Integer>(CreationConstructableReads).ToBe(1);
      if AConstruct then
      begin
        Expect<Integer>(Target.Constructions).ToBe(INVOCATIONS);
        Expect<Integer>(Target.Calls).ToBe(0);
        Expect<TGocciaValue>(Target.LastNewTarget).ToBe(Chain);
      end
      else
      begin
        Expect<Integer>(Target.Calls).ToBe(INVOCATIONS);
        Expect<Integer>(Target.Constructions).ToBe(0);
      end;
    finally
      Arguments.Free;
      Roots.Clear;
    end;
  end;
end;

procedure TTestProxyValue.TestCallCapabilityQueries;
begin
  ExerciseChains(False);
end;

procedure TTestProxyValue.TestConstructCapabilityQueries;
begin
  ExerciseChains(True);
end;

begin
  TestRunnerProgram.AddSuite(TTestProxyValue.Create('Proxy capabilities'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
