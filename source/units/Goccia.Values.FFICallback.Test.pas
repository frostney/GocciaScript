program Goccia.Values.FFICallback.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.Arguments.Collection,
  Goccia.FFI.Types,
  Goccia.GarbageCollector,
  Goccia.TestSetup,
  Goccia.Values.FFICallback,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Exception;

type
  // Matches the trampoline generated for a callback taking no arguments and
  // returning void.
  TVoidCallbackTrampoline = procedure; cdecl;

  TFFICallbackGCTests = class(TTestSuite)
  private
    FThrownValue: TGocciaValue;
    function ThrowingCallbackBody(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    procedure SetupTests; override;
    procedure BeforeEach; override;
    procedure TestMarkReferencesKeepsBytecodeThrowPayloadReachable;
  end;

{ TFFICallbackGCTests }

function TFFICallbackGCTests.ThrowingCallbackBody(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  { A compiled callback body's JS throw leaves the VM as EGocciaBytecodeThrow.
    Raising it here reproduces exactly what InvokeFromNative parks. }
  raise EGocciaBytecodeThrow.Create(FThrownValue);
end;

procedure TFFICallbackGCTests.SetupTests;
begin
  Test('MarkReferences keeps a parked EGocciaBytecodeThrow payload reachable ' +
    'across a collection',
    TestMarkReferencesKeepsBytecodeThrowPayloadReachable);
end;

procedure TFFICallbackGCTests.BeforeEach;
begin
  FThrownValue := nil;
end;

procedure TFFICallbackGCTests.TestMarkReferencesKeepsBytecodeThrowPayloadReachable;
var
  Descriptor: TGocciaFFITypeDescriptor;
  Callable: TGocciaNativeFunctionValue;
  Callback: TGocciaFFICallbackValue;
  Trampoline: TVoidCallbackTrampoline;
begin
  { The thrown value is an ordinary managed object reachable from nothing but
    the exception the callback body raises. }
  FThrownValue := TGocciaObjectValue.Create;

  Descriptor := TGocciaFFITypeDescriptor.CreateCallback([],
    TGocciaFFITypeDescriptor.CreateScalar(fftVoid));
  Callable := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ThrowingCallbackBody, '<throwing-callback>', 0);
  Callback := TGocciaFFICallbackValue.Create(Descriptor, Callable);
  try
    Expect<Boolean>(Assigned(Callback.Pointer)).ToBe(True);

    { Invoke the native trampoline directly. No FFI call context is active on
      this thread, so InvokeFromNative parks the throw into the callback's
      FPendingException field — the exact path MarkReferences must scan — rather
      than the transient per-call context. }
    Trampoline := TVoidCallbackTrampoline(Callback.Pointer);
    Trampoline();

    { Simulate the start of a mark phase: advancing the generation makes every
      object read as unmarked. The only live reference to FThrownValue now is
      the parked EGocciaBytecodeThrow, so it survives the sweep iff
      Callback.MarkReferences marks it. }
    TGCManagedObject.AdvanceMark;
    Expect<Boolean>(FThrownValue.GCMarked).ToBe(False);

    Callback.MarkReferences;

    Expect<Boolean>(FThrownValue.GCMarked).ToBe(True);
  finally
    { Release the native slot and free the parked exception without re-raising
      it (Close would re-raise). }
    Callback.CloseForFFICallCleanup;
  end;
end;

begin
  TestRunnerProgram.AddSuite(
    TFFICallbackGCTests.Create('FFI callback GC'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.
