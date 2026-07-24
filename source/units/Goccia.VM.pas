unit Goccia.VM;

{$I Goccia.inc}
{$POINTERMATH ON}

interface

uses
  Classes,
  Math,

  Goccia.Arguments.Collection,
  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Module,
  Goccia.Evaluator.Context,
  Goccia.ExecutionContext,
  Goccia.GarbageCollector,
  Goccia.Intrinsics.FunctionObjects,
  Goccia.Keywords.Reserved,
  Goccia.Modules,
  Goccia.Realm,
  Goccia.Scope,
  Goccia.Scope.BindingMap,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.PromiseValue,
  Goccia.Values.SymbolValue,
  Goccia.VM.CallFrame,
  Goccia.VM.Closure,
  Goccia.VM.Exception,
  Goccia.VM.Registers,
  Goccia.VM.Upvalue;

type
  TGocciaVM = class;
  TASCIIStringCodeUnit = 0..127;

  TGocciaVMStackRoot = class(TGCManagedObject)
  private
    FVM: TGocciaVM;
    procedure MarkClosureReferences(const AClosure: TGocciaBytecodeClosure);
  public
    constructor Create(const AVM: TGocciaVM);
    procedure MarkReferences; override;
  end;

  TGocciaVMLiteralObjectValue = class(TGocciaObjectValue)
  private
    FFastLiteralMode: Boolean;
  public
    constructor Create(const APrototype: TGocciaObjectValue = nil;
      const APropertyCapacity: Integer = 0);
    function TryGetOwnDataPropertyFastRegister(const AName: string;
      out AValue: TGocciaRegister): Boolean;
    function TryGetOwnDataPropertyFast(const AName: string;
      out AValue: TGocciaValue): Boolean;
    function TrySetLiteralDataPropertyFast(const AName: string;
      const AValue: TGocciaValue): Boolean;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    procedure Freeze; override;
    procedure Seal; override;
    procedure PreventExtensions; override;
  end;

  TGocciaBytecodeCellArray = array of TGocciaBytecodeCell;
  PGocciaBytecodeCell = ^TGocciaBytecodeCell;
  TGocciaArgumentsPoolArray = array of TGocciaArgumentsCollection;
  TGocciaBytecodeGeneratorResumeKind = (bgrkNext, bgrkReturn, bgrkThrow);
  TGocciaBytecodeGeneratorState = (bgsSuspendedStart, bgsSuspendedYield,
    bgsExecuting, bgsCompleted);
  TGocciaBytecodeAsyncGeneratorRequest = record
    Kind: TGocciaBytecodeGeneratorResumeKind;
    Value: TGocciaValue;
    Promise: TGocciaPromiseValue;
  end;
  TGocciaVMSavedStateRoot = record
    Closure: TGocciaBytecodeClosure;
    NewTarget: TGocciaValue;
    ArgumentBase: Integer;
    ArgCount: Integer;
  end;
  TGocciaVMSavedStateRootArray = array of TGocciaVMSavedStateRoot;

  // Classified computed property key — the single home of the key-resolution
  // rule shared by the computed get/set/delete/define opcode arms:
  // direct symbol -> (optional) array-index probe -> ToPropertyKey for
  // object keys -> property-name string fallback.
  TGocciaPropertyKeyKind = (pkkSymbol, pkkIndex, pkkName);
  TGocciaPropertyKey = record
    Kind: TGocciaPropertyKeyKind;
    Index: Integer;               // valid when Kind = pkkIndex
    Symbol: TGocciaSymbolValue;   // valid when Kind = pkkSymbol
    Name: string;                 // valid when Kind = pkkName
    // True when the key came from ToPropertyKey on an object key. The
    // computed-get object arm historically applies private-key routing
    // only to keys that did NOT pass through ToPropertyKey.
    FromObjectKey: Boolean;
  end;

  // Per-opcode behaviour switches for the shared computed property access
  // cores. Each computed access opcode maps to a fixed option set; the
  // differences are load-bearing semantics, not drift.
  TGocciaComputedAccessOption = (
    // Get: throw TypeError on null/undefined receiver (OP_ARRAY_GET).
    caoThrowOnNullUndefined,
    // Get/Set: route bytecode-private keys through the brand-checked
    // GetPropertyValue/SetPropertyValue paths (OP_GET_INDEX, OP_SET_INDEX).
    caoHandlePrivateKeys,
    // Get: try the VM literal-object own-data fast path before the generic
    // GetProperty walk (OP_GET_INDEX).
    caoLiteralFastPath,
    // Set: class receivers install the value with DefineProperty
    // ([pfConfigurable, pfWritable]) instead of assignment (OP_SET_INDEX).
    caoClassDefineSemantics,
    // Set: home object is wired on array/object/fallback receivers too,
    // not only on class receivers (OP_SET_INDEX).
    caoHomeObjectAllReceivers
  );
  TGocciaComputedAccessOptions = set of TGocciaComputedAccessOption;

  // Minimal saved state for a compiler-proven numeric self-call. These frames
  // share the closure, lexical environment, local-cell window, argument
  // window, realm, and execution context of their generic entry frame.
  TGocciaClosedNumericFrame = record
    IP: Integer;
    ReturnRegister: UInt16;
    RegisterBase: Integer;
    PrevCovLine: UInt32;
    ProfileEntryTimestamp: Int64;
  end;

  TGocciaVM = class
  private
    FRegisterStack: TGocciaRegisterArray;
    FRegisterBase: Integer;
    FRegisters: PGocciaRegister;
    FRegisterCount: Integer;
    FLocalCellStack: TGocciaBytecodeCellArray;
    FLocalCellBase: Integer;
    FLocalCells: PGocciaBytecodeCell;
    FLocalCellCount: Integer;
    // Call arguments live in a growable arena window (FArgumentStack) with a
    // base+count window, mirroring the register and local-cell stacks. The
    // window holds the current frame's arguments; PushFrame/PopFrame and native
    // re-entry save/restore (base,count) integers instead of copying an array,
    // so an ordinary call allocates nothing here. FArguments points at the live
    // window and must be recomputed after any AcquireArgumentWindow (which may
    // reallocate the backing array) and after any frame restore.
    FArgumentStack: TGocciaRegisterArray;
    FArgumentBase: Integer;
    FArguments: PGocciaRegister;
    FArgCount: Integer;
    FGlobalScope: TGocciaScope;
    FGlobalThisValue: TGocciaValue;
    FRealm: TGocciaRealm;
    FLoadModule: TLoadModuleCallback;
    FLoadModuleSource: TLoadModuleSourceCallback;
    FLoadDeferredModule: TLoadDeferredModuleCallback;
    FResolveModuleURL: TResolveModuleURLCallback;
    FCurrentClosure: TGocciaBytecodeClosure;
    FHandlerStack: TGocciaBytecodeHandlerStack;
    FFrameDepth: Integer;
    // Depth of native VM re-entries (ExecuteClosureRegistersInternal invocations
    // nested via generator resume, host eval, or native callbacks). Bounded
    // separately from FFrameDepth because each native re-entry costs a real
    // native stack frame; see CheckNativeReentryDepth in Goccia.StackLimit.
    FNativeExecutionDepth: Integer;
    FMemoryPressureCheckCountdown: Integer;
    FFrameStack: array of TGocciaVMCallFrame;
    FFrameStackCount: Integer;
    FClosedNumericFrameStack: array of TGocciaClosedNumericFrame;
    FClosedNumericFrameStackCount: Integer;
    FCurrentExecutionContextPushed: Boolean;
    FCurrentModuleSourcePath: string;
    FCurrentRuntimeModule: TGocciaModule;
    FCurrentModuleExports: TGocciaValueMap;
    FPendingNewTarget: TGocciaValue;
    FCurrentNewTarget: TGocciaValue;
    FCurrentDynamicVarScope: TGocciaScope;
    FCurrentAsyncPromise: TGocciaPromiseValue;
    FActiveDecoratorSession: TObject;
    FGlobalBackedTopLevel: Boolean;
    FCoverageEnabled: Boolean;
    FProfilingOpcodes: Boolean;
    FProfilingFunctions: Boolean;
    FArgumentPool: TGocciaArgumentsPoolArray;
    FArgumentPoolCount: Integer;
    FLastClosureThisValue: TGocciaRegister;
    FCurrentConstructorSuperCalled: Boolean;
    FPrivateInitializerReceiver: TGocciaValue;
    FPrivateInitializerPreserveExisting: Boolean;
    FStackRoot: TGocciaVMStackRoot;
    FStackRootRegistered: Boolean;
    FTempSavedStateRoots: TGocciaVMSavedStateRootArray;
    FTempSavedStateRootCount: Integer;
    FASCIIStringValues: array[0..127] of TGocciaStringLiteralValue;
    function CachedASCIIStringValue(
      const ACodeUnit: TASCIIStringCodeUnit): TGocciaStringLiteralValue;
    function ConstantToValue(const AConstant: TGocciaBytecodeConstant): TGocciaValue;
    // ES2026 §13.2.8.3 GetTemplateObject — lazy-build helper for bckTemplateObject constants.
    function BuildTemplateObjectConstant(const ATemplate: TGocciaFunctionTemplate;
      const AConstantIndex: Integer): TGocciaValue;
    function BuildRegExpLiteralConstant(const ATemplate: TGocciaFunctionTemplate;
      const AConstantIndex: Integer): TGocciaValue;
    function AcquireArguments(const ACapacity: Integer = 0): TGocciaArgumentsCollection;
    procedure ReleaseArguments(const AArguments: TGocciaArgumentsCollection);
    procedure AcquireRegisters(const ACount: Integer);
    procedure AcquireLocalCells(const ACount: Integer);
    procedure AcquireArgumentWindow(const ACount: Integer);
    function CurrentArgumentsSnapshot: TGocciaRegisterArray;
    procedure EnsureRegisterCapacity(const ACount: Integer);
    procedure EnsureLocalCapacity(const ACount: Integer);
    function GetLocalCell(const AIndex: Integer): TGocciaBytecodeCell;
    function GetLocalRegister(const AIndex: Integer): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
    function GetRegister(const AIndex: Integer): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
    function GetRegisterFast(const AIndex: Integer): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
    procedure SetRegister(const AIndex: Integer; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}
    procedure SetRegisterFast(const AIndex: Integer; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}
    procedure SetRegisterRaw(const AIndex: Integer; const AValue: TGocciaRegister); {$IFDEF FPC}inline;{$ENDIF}
    procedure InstallFunctionPrototype(const AFunction: TGocciaObjectValue;
      const AKind: TGocciaFunctionObjectIntrinsicKind);
    function GetLocal(const AIndex: Integer): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
    function GetLocalFast(const AIndex: Integer): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
    procedure SetLocal(const AIndex: Integer; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}
    procedure SetLocalFast(const AIndex: Integer; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}
    procedure SetLocalRaw(const AIndex: Integer; const AValue: TGocciaRegister); {$IFDEF FPC}inline;{$ENDIF}
    function MatchesNullishKind(const AValue: TGocciaValue; const AKind: UInt8): Boolean;
    function TryGetArrayIndex(const AKey: TGocciaValue; out AIndex: Integer): Boolean;
    function TryGetArrayIndexRegister(const AKey: TGocciaRegister;
      out AIndex: Integer): Boolean;
    function KeyToPropertyName(const AKey: TGocciaValue): string;
    function KeyToPropertyNameRegister(const AKey: TGocciaRegister): string;
    function TryResolveObjectKey(const AKeyReg: TGocciaRegister; out AResolved: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
    // Shared computed-property-access cores. AProbeArrayIndex preserves the
    // historical per-receiver cascades: array/string receivers probe for an
    // array index, class/object receivers do not.
    function ClassifyPropertyKey(const AKeyReg: TGocciaRegister;
      const AProbeArrayIndex: Boolean): TGocciaPropertyKey;
    function PropertyKeyName(const AKey: TGocciaPropertyKey): string; {$IFDEF FPC}inline;{$ENDIF}
    // Register parameters are passed BY VALUE deliberately: callers pass
    // FRegisters[..] slots, and ClassifyPropertyKey can run user code
    // (ToPropertyKey -> toString) that grows and reallocates the register
    // stack. A `const` record parameter may be passed by reference on some
    // targets (e.g. win64), which would leave the parameter dangling into
    // the freed old block; a value copy taken at the call is immune.
    procedure ExecGetComputedProperty(const ADest: Integer;
      AObjReg, AKeyReg: TGocciaRegister;
      const AOptions: TGocciaComputedAccessOptions);
    procedure ExecSetComputedProperty(const ATargetIndex: Integer;
      AKeyReg, AValueReg: TGocciaRegister;
      const AOptions: TGocciaComputedAccessOptions);
    procedure ExecDeleteComputedProperty(const ADest: Integer;
      AObjReg, AKeyReg: TGocciaRegister;
      const AThrowOnFailure: Boolean);
    // Serve an own NON-data descriptor (accessor or exotic) exactly as the
    // receiver's generic own-descriptor branch would, without a second map
    // lookup: callable getter -> invoke with the receiver as this; anything
    // else -> undefined.
    procedure ServeOwnNonDataProperty(const ADest: Integer;
      const AReceiver: TGocciaValue;
      const ADescriptor: TGocciaPropertyDescriptor);
    procedure ExecGetComputedPropertyFallback(const ADest: Integer;
      const AReceiverReg, AKeyReg: TGocciaRegister);
    function CoerceNonStrictThisRegister(
      const AThisRegister: TGocciaRegister;
      const AGlobalThisValue: TGocciaValue = nil;
      const ARealm: TGocciaRealm = nil): TGocciaRegister;
    procedure SetFunctionNameFromKey(const AFunction, AKey: TGocciaValue;
      const APrefixKind: UInt8);
    function EnsureCurrentDynamicVarScope: TGocciaScope;
    function ResolveDynamicUpvalueScope(const AIndex: Integer;
      const AName: string): TGocciaScope;
    function KeyDisplaySafe(const AKey: TGocciaRegister): string;
    // ALimit semantics:
    //   ALimit < 0 → unbounded (drain until iterator returns done:true);
    //   ALimit = 0 → consume zero elements (used for `const [] = iter`);
    //   ALimit > 0 → consume exactly ALimit elements then close.
    // Default is unbounded so existing spread / Iterator.from / rest
    // call sites preserve their historical behaviour without explicit
    // arguments.
    function IterableToArray(const AIterable: TGocciaValue;
      const ATryAsync: Boolean = False;
      const ALimit: Integer = -1): TGocciaArrayValue;
    function TryIterableToArray(const AIterable: TGocciaValue;
      out AArray: TGocciaArrayValue): Boolean;
    procedure SpreadObjectIntoValue(const ATarget: TGocciaObjectValue;
      const ASource: TGocciaValue);
    function ObjectRestValue(const ASource: TGocciaValue;
      const AExclusionKeys: TGocciaArrayValue): TGocciaObjectValue;
    function ForInEntriesArray(const AValue: TGocciaValue): TGocciaArrayValue;
    function TryForInEntryKey(const AEntry: TGocciaValue; out AKey: string): Boolean;
    function PromiseResolveIntrinsic(const AValue: TGocciaValue): TGocciaPromiseValue;
    function GetIteratorValue(const AIterable: TGocciaValue;
      const ATryAsync: Boolean): TGocciaValue;
    function ConstructValue(const AConstructor: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue = nil): TGocciaValue;
    function ImportModuleValue(const APath: string): TGocciaValue; overload;
    function ImportModuleValue(const APath, AReferrer: string): TGocciaValue; overload;
    procedure ResolveDynamicImportPromise(const APromise: TGocciaPromiseValue;
      const APath, AReferrer: string);
    function ImportModuleSourceValue(const APath, AReferrer: string): TGocciaValue;
    function ImportDeferredModuleNamespaceValue(const APath,
      AReferrer: string): TGocciaValue;
    function DynamicImportAttributeType(const AOptions: TGocciaValue): string;
    procedure ExportBindingValue(const AName: string; const AValue: TGocciaValue;
      const ASourcePath: string = '');
    procedure DefineGetterProperty(const ATarget: TGocciaValue; const AName: string;
      const AGetter: TGocciaValue);
    procedure DefineSetterProperty(const ATarget: TGocciaValue; const AName: string;
      const ASetter: TGocciaValue);
    procedure DefineStaticGetterProperty(const ATarget: TGocciaValue; const AName: string;
      const AGetter: TGocciaValue);
    procedure DefineStaticSetterProperty(const ATarget: TGocciaValue; const AName: string;
      const ASetter: TGocciaValue);
    procedure DefineDataPropertyByKeyInternal(const ATarget: TGocciaValue;
      const AKey: TGocciaRegister; const AValue: TGocciaValue;
      const ASetHomeObject: Boolean);
    procedure DefineDataPropertyByKey(const ATarget: TGocciaValue;
      const AKey: TGocciaRegister; const AValue: TGocciaValue);
    procedure DefineMethodPropertyByKey(const ATarget: TGocciaValue;
      const AKey: TGocciaRegister; const AValue: TGocciaValue);
    procedure SetObjectLiteralPrototype(const ATarget, APrototype: TGocciaValue);
    procedure DefineGetterPropertyByKey(const ATarget, AKey, AGetter: TGocciaValue);
    procedure DefineSetterPropertyByKey(const ATarget, AKey, ASetter: TGocciaValue);
    procedure DefineStaticGetterPropertyByKey(const ATarget, AKey, AGetter: TGocciaValue);
    procedure DefineStaticSetterPropertyByKey(const ATarget, AKey, ASetter: TGocciaValue);
    procedure DefineGlobalBinding(const AName: string;
      const AValue: TGocciaValue;
      const ADeclarationType: TGocciaDeclarationType;
      const ANonStrictMode: Boolean = False);
    function FinalizeEnumValue(const AValue: TGocciaValue; const AName: string): TGocciaValue;
    procedure StampBytecodePrivateBrands(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue;
      const APreserveExistingPrivateSlots: Boolean);
    procedure SetupAutoAccessorValue(const AName: string; const AFlags: Integer;
      const AClassValue: TGocciaValue = nil);
    procedure SetupAutoAccessorValueByKey(const AKey: TGocciaValue;
      const ABackingName: string; const AFlags: Integer);
    procedure RunClassInitializers(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue;
      const APreserveExistingPrivateSlots: Boolean = False);
    function MaterializeArguments(
      const AArguments: TGocciaRegisterArray): TGocciaArgumentsCollection;
    function CreateArgumentsObjectFromCurrentFrame(
      const AUseMappedArguments: Boolean;
      const AFormalParameterCount: Integer): TGocciaObjectValue;
    function InvokeImplicitSuperInitialization(const AClassValue: TGocciaClassValue;
      const AInstance: TGocciaValue; const AArguments: TGocciaArgumentsCollection): TGocciaValue;
    function InvokeImplicitSuperInitializationRegisters(
      const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
      const AArguments: TGocciaRegisterArray): TGocciaValue;
    procedure BeginDecorators(const AClassValue, ASuperValue: TGocciaValue);
    procedure ApplyElementDecorator(const ADecoratorFn: TGocciaValue;
      const ADescriptor: string; const AComputedKey: TGocciaValue = nil);
    procedure ApplyClassDecorator(const ADecoratorFn: TGocciaValue);
    function FinishDecorators(const ACurrentValue: TGocciaValue): TGocciaValue;
    function GetSuperPropertyValue(const ASuperValue, AThisValue: TGocciaValue;
      const AName: string; const AUseSuperConstructor: Boolean): TGocciaValue;
    function GetSuperPropertyValueByKey(const ASuperValue, AThisValue,
      AKey: TGocciaValue; const AUseSuperConstructor: Boolean): TGocciaValue;
    function ResolveSuperPropertyBaseValue(const ASuperValue,
      AThisValue: TGocciaValue): TGocciaValue;
    function GetSuperPropertyValueFromBase(const ABaseValue, AThisValue,
      AKey: TGocciaValue): TGocciaValue;
    procedure SetSuperPropertyValueByKey(const ASuperValue, AThisValue,
      AKey, AValue: TGocciaValue);
    procedure SetSuperPropertyBaseValueByKey(const ABaseValue, AThisValue,
      AKey, AValue: TGocciaValue);
    function ResolveBytecodePrivateBrandToken(const AKey: string;
      const AObject: TGocciaValue): string;
    function CurrentBytecodePrivateAccessClass(
      const AKey: string): TGocciaClassValue;
    procedure ThrowBytecodePrivateTypeError(const AKey,
      AMessage: string);
    function GetPropertyValue(const AObject: TGocciaValue; const AKey: string): TGocciaValue;
    procedure SetPropertyValue(const AObject: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue);
    procedure SetPropertyValueLoose(const AObject: TGocciaValue;
      const AKey: string; const AValue: TGocciaValue);
    procedure SetSymbolPropertyValueLoose(const AObject: TGocciaValue;
      const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
    procedure SetIndexValueLoose(const AObject: TGocciaValue;
      const AKey: TGocciaRegister; const AValue: TGocciaValue);
    function TryGetRawPrivateValue(const AObject: TGocciaValue; const AKey: string;
      out AValue: TGocciaValue): Boolean;
    procedure SetRawPrivateValue(const AObject: TGocciaValue; const AKey: string;
      const AValue: TGocciaValue);
    function HasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
    function HasWithBindingValue(const AObject, AKey: TGocciaValue): TGocciaValue;
    function GetWithBindingValue(const AObject, AKey: TGocciaValue;
      const AStrict: Boolean): TGocciaValue;
    procedure SetWithBindingValue(const AObject, AKey, AValue: TGocciaValue;
      const AStrict: Boolean);
    function MatchHasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
    function MatchExtractorValue(const ASubject, AMatcher: TGocciaValue): TGocciaValue;
    function InvokeFunctionValue(const ACallee: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ExecuteDirectEval(const ASourceValue: TGocciaValue;
      const ATemplate: TGocciaFunctionTemplate; const APC: UInt32;
      const ACallerStrict: Boolean): TGocciaValue;
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    procedure PushFrame(const AResultRegister, AFrameIP: Integer;
      const ATemplate: TGocciaFunctionTemplate;
      const APrevCovLine: UInt32; const AProfileTimestamp: Int64);
    function PopFrame(var AFrame: TGocciaVMCallFrame;
      out ATemplate: TGocciaFunctionTemplate;
      out APrevCovLine: UInt32; out AProfileTimestamp: Int64): Integer;
    procedure TeardownCurrentFrame(const ATemplate: TGocciaFunctionTemplate;
      const AProfileTimestamp: Int64; const ATargetHandlerCount: Integer);
    procedure PushClosedNumericFrame(const AResultRegister, AArgumentBase,
      AArgumentCount: UInt16; var AFrame: TGocciaVMCallFrame;
      const ATemplate: TGocciaFunctionTemplate; var APrevCovLine: UInt32;
      var AProfileTimestamp: Int64; var AInitializedRegisterTop: Integer);
    function PopClosedNumericFrame(var AFrame: TGocciaVMCallFrame;
      const ATemplate: TGocciaFunctionTemplate; var APrevCovLine: UInt32;
      var AProfileTimestamp: Int64): Integer;
    procedure UnwindClosedNumericFrames(const ATargetCount: Integer;
      var AFrame: TGocciaVMCallFrame;
      const ATemplate: TGocciaFunctionTemplate; var APrevCovLine: UInt32;
      var AProfileTimestamp: Int64);
    procedure PrepareTailCallFrameReuse(const ATemplate: TGocciaFunctionTemplate;
      const AProfileTimestamp: Int64; const AInitialFrameStackCount,
      ASavedHandlerCount: Integer);
    procedure PushSavedStateRoot(const AClosure: TGocciaBytecodeClosure;
      const ANewTarget: TGocciaValue; const AArgumentBase, AArgCount: Integer);
    procedure PopSavedStateRoot;
    procedure SetupNewFrame(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
      const AArgCount: Integer; const AArg0, AArg1, AArg2: TGocciaRegister;
      const AUseFixedArgs: Boolean; const APushExecutionContext: Boolean;
      var AFrame: TGocciaVMCallFrame; out ATemplate: TGocciaFunctionTemplate;
      out APrevCovLine: UInt32; out AProfileTimestamp: Int64);
    procedure HandleExceptionUnwind(const AErrorValue: TGocciaValue;
      const AInitialFrameStackCount, AInitialClosedNumericFrameCount,
      ASavedHandlerCount: Integer;
      var AFrame: TGocciaVMCallFrame; var ATemplate: TGocciaFunctionTemplate;
      var APrevCovLine: UInt32; var AProfileTimestamp: Int64);
    procedure ExecuteGeneratorParameterPreamble(const AGenerator: TObject);
    function ExecuteClosureRegistersInternal(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
      const AArgCount: Integer; const AArg0, AArg1, AArg2: TGocciaRegister;
      const AUseFixedArgs: Boolean;
      const APushExecutionContext: Boolean;
      const AStopAtIP: Integer = -1;
      const AStopGenerator: TObject = nil): TGocciaRegister;
    function ExecuteClosureRegisters0(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister;
      const APushExecutionContext: Boolean = True): TGocciaRegister;
    function ExecuteClosureRegisters1(const AClosure: TGocciaBytecodeClosure;
      const AThisValue, AArg0: TGocciaRegister;
      const APushExecutionContext: Boolean = True): TGocciaRegister;
    function ExecuteClosureRegisters2(const AClosure: TGocciaBytecodeClosure;
      const AThisValue, AArg0, AArg1: TGocciaRegister;
      const APushExecutionContext: Boolean = True): TGocciaRegister;
    function ExecuteClosureRegisters3(const AClosure: TGocciaBytecodeClosure;
      const AThisValue, AArg0, AArg1, AArg2: TGocciaRegister;
      const APushExecutionContext: Boolean = True): TGocciaRegister;
    function ExecuteClosureRegisters(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaRegister;
      const AArguments: TGocciaRegisterArray;
      const APushExecutionContext: Boolean = True): TGocciaRegister;
    function ExecuteClosure(const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue;
      const AArguments: TGocciaArgumentsCollection;
      const APushExecutionContext: Boolean = True): TGocciaValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnsureStackRootRegistered;
    function ExecuteFunction(const ATemplate: TGocciaFunctionTemplate): TGocciaValue;
    function ExecuteModule(const AModule: TGocciaBytecodeModule): TGocciaValue;
    property GlobalScope: TGocciaScope read FGlobalScope write FGlobalScope;
    property GlobalThisValue: TGocciaValue read FGlobalThisValue write FGlobalThisValue;
    property Realm: TGocciaRealm read FRealm write FRealm;
    property LoadModule: TLoadModuleCallback read FLoadModule write FLoadModule;
    property LoadModuleSource: TLoadModuleSourceCallback
      read FLoadModuleSource write FLoadModuleSource;
    property LoadDeferredModule: TLoadDeferredModuleCallback
      read FLoadDeferredModule write FLoadDeferredModule;
    property ResolveModuleURL: TResolveModuleURLCallback
      read FResolveModuleURL write FResolveModuleURL;
    property CurrentRuntimeModule: TGocciaModule
      read FCurrentRuntimeModule write FCurrentRuntimeModule;
    property GlobalBackedTopLevel: Boolean read FGlobalBackedTopLevel
      write FGlobalBackedTopLevel;
    property CoverageEnabled: Boolean read FCoverageEnabled write FCoverageEnabled;
    property ProfilingOpcodes: Boolean read FProfilingOpcodes write FProfilingOpcodes;
    property ProfilingFunctions: Boolean read FProfilingFunctions write FProfilingFunctions;
  end;

implementation

uses
  Generics.Collections,
  SysUtils,

  BigInteger,
  NumberBits,
  OrderedStringMap,
  TextSemantics,
  TimingUtils,

  Goccia.Arithmetic,
  Goccia.AST.Node,
  Goccia.AST.Statements,
  Goccia.CallStack,
  Goccia.CapabilityAudit,
  Goccia.Constants,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.ControlFlow,
  Goccia.Coverage,
  Goccia.DisposalTracker,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Evaluator,
  Goccia.Evaluator.Decorators,
  Goccia.Execution.CallSite,
  Goccia.ImportMeta,
  Goccia.InstructionLimit,
  Goccia.MicrotaskQueue,
  Goccia.NumberConversion,
  Goccia.NumberExponentiation,
  Goccia.NumberRemainder,
  Goccia.PatternMatching,
  Goccia.Profiler,
  Goccia.RegExp.Runtime,
  Goccia.SourcePipeline,
  Goccia.StackLimit,
  Goccia.Timeout,
  Goccia.Types.Enforcement,
  Goccia.URI,
  Goccia.Utils,
  Goccia.Values.ArgumentsObjectValue,
  Goccia.Values.Await,
  Goccia.Values.BigIntValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.EnumValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.FunctionValue,
  Goccia.Values.GeneratorValue,
  Goccia.Values.HoleValue,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ProxyValue,
  Goccia.Values.Shape,
  Goccia.Values.StringObjectValue,
  Goccia.Values.ToObject,
  Goccia.Values.ToPrimitive,
  Goccia.Values.TypedArrayValue;

const
  BYTECODE_PRIVATE_SLOT_PREFIX = '#slot:';
  BYTECODE_PRIVATE_BRAND_PREFIX = '#brand:';
  BYTECODE_PRIVATE_INITIALIZED_PREFIX = '#initialized:';
  FOR_IN_ENTRY_OWNER = '__gocciaForInOwner';
  FOR_IN_ENTRY_KEY = '__gocciaForInKey';
  FOR_IN_MAX_PROTOTYPE_CHAIN_DEPTH = 256;
  DERIVED_THIS_INITIALIZED_LOCAL = '__derived_this_initialized';
  MEMORY_PRESSURE_CHECK_INTERVAL = 1024;

type
  TGocciaVMSuperConstructorValue = class(TGocciaFunctionBase)
  private
    FSuperClass: TGocciaValue;
    FNewTarget: TGocciaValue;
    FCurrentCtorClass: TGocciaClassValue;
  protected
    function GetFunctionName: string; override;
  public
    constructor Create(const ASuperClass, ANewTarget: TGocciaValue;
      const ACurrentCtorClass: TGocciaClassValue);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function IsConstructable: Boolean; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMDirectEvalScope = class(TGocciaScope)
  private
    FVM: TGocciaVM;
    FTemplate: TGocciaFunctionTemplate;
    FEnvironmentIndex: Integer;
    FHomeObject: TGocciaValue;
    FHomeClass: TGocciaValue;
    FNewTarget: TGocciaValue;
    FDeletedVarBindings: TStringList;
    function TryFindBinding(const AName: string;
      out ABinding: TGocciaDirectEvalBindingInfo): Boolean;
    function IsDeletedVarBinding(const AName: string): Boolean;
    procedure MarkDeletedVarBinding(const AName: string);
    function BindingValue(
      const ABinding: TGocciaDirectEvalBindingInfo): TGocciaValue;
    function HasWithObjectBinding(const AObject: TGocciaObjectValue;
      const AName: string): Boolean;
    function TryFindWithObjectBinding(const AName: string;
      out AObject: TGocciaObjectValue): Boolean;
    procedure SetBindingValue(const ABinding: TGocciaDirectEvalBindingInfo;
      const AValue: TGocciaValue);
  protected
    function GetOwningClass: TGocciaValue; override;
    function GetSuperClass: TGocciaValue; override;
    function GetSuperConstructor: TGocciaValue; override;
    function GetNewTarget: TGocciaValue; override;
    function GetThisValue: TGocciaValue; override;
    function IsFunctionBoundary: Boolean; override;
    function MarkSuperConstructorCalled: Boolean; override;
  public
    constructor Create(const AParent: TGocciaScope; const AVM: TGocciaVM;
      const ATemplate: TGocciaFunctionTemplate; const AEnvironmentIndex: Integer;
      const AUseGlobalVarEnvironment: Boolean;
      const ALexicalClosure: TGocciaBytecodeClosure;
      const ANewTarget: TGocciaValue);
    destructor Destroy; override;
    procedure MarkReferences; override;
    function TryGetBinding(const AName: string; out ABinding: TLexicalBinding;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; override;
    function TryAssignExistingBinding(const AName: string;
      const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
      const ALine: Integer = 0; const AColumn: Integer = 0): Boolean; override;
    function DeleteBinding(const AName: string): Boolean; override;
    procedure ResolveIdentifierReference(const AName: string;
      out AValue, AThisValue: TGocciaValue; const ALine: Integer = 0;
      const AColumn: Integer = 0); override;
    procedure ResolveAssignmentTarget(const AName: string;
      out AObjectBinding: TGocciaObjectValue; out AScopeBinding: TGocciaScope); override;
    function Contains(const AName: string): Boolean; override;
    function ContainsVarEnvironmentBinding(const AName: string): Boolean; override;
    procedure CopyBackVariableBindings;
    procedure CopyNewVariableBindingsToParent;
  end;

function BytecodeFunctionIntrinsicKind(const ATemplate: TGocciaFunctionTemplate):
  TGocciaFunctionObjectIntrinsicKind; {$IFDEF FPC}inline;{$ENDIF}
begin
  if Assigned(ATemplate) and ATemplate.IsAsync and ATemplate.IsGenerator then
    Result := foikAsyncGenerator
  else if Assigned(ATemplate) and ATemplate.IsGenerator then
    Result := foikGenerator
  else if Assigned(ATemplate) and ATemplate.IsAsync then
    Result := foikAsync
  else
    Result := foikOrdinary;
end;

function DirectEvalBindingIsNonStrictImmutable(
  const ABinding: TGocciaDirectEvalBindingInfo;
  const ATemplate: TGocciaFunctionTemplate): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := ABinding.IsConst and
    (ABinding.IsEvalSyntheticArguments or
     ((ABinding.Kind = debUpvalue) and Assigned(ATemplate) and
      (ATemplate.Name <> '') and (ABinding.Name = ATemplate.Name)));
end;

procedure EnsureVMObjectPrototypeInitialized; {$IFDEF FPC}inline;{$ENDIF}
begin
  if TGocciaObjectValue.SharedObjectPrototype = nil then
    TGocciaObjectValue.InitializeSharedPrototype;
end;

function VMFunctionObjectPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
begin
  EnsureVMObjectPrototypeInitialized;
  IteratorPrototype := nil;
  if AKind = foikGenerator then
    IteratorPrototype := TGocciaIteratorValue.SharedPrototype;
  Result := FunctionObjectIntrinsicPrototype(AKind,
    TGocciaFunctionBase.GetSharedPrototype,
    TGocciaObjectValue.SharedObjectPrototype,
    IteratorPrototype);
end;

function VMGeneratorObjectPrototype(
  const AKind: TGocciaFunctionObjectIntrinsicKind;
  const ARealm: TGocciaRealm = nil): TGocciaObjectValue;
var
  IteratorPrototype: TGocciaObjectValue;
  PreviousRealm: TGocciaRealm;
  ShouldSwitchRealm: Boolean;
begin
  PreviousRealm := CurrentRealm;
  ShouldSwitchRealm := Assigned(ARealm) and (ARealm <> PreviousRealm);
  if ShouldSwitchRealm then
    SetCurrentRealm(ARealm);
  try
    EnsureVMObjectPrototypeInitialized;
    IteratorPrototype := nil;
    if AKind = foikGenerator then
      IteratorPrototype := TGocciaIteratorValue.SharedPrototype;
    Result := GeneratorObjectIntrinsicPrototype(AKind,
      TGocciaFunctionBase.GetSharedPrototype,
      TGocciaObjectValue.SharedObjectPrototype,
      IteratorPrototype);
    if AKind = foikGenerator then
      EnsureGeneratorPrototypeMethods(Result)
    else if AKind = foikAsyncGenerator then
      EnsureAsyncGeneratorPrototypeMethods(Result);
  finally
    if ShouldSwitchRealm then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function BytecodeClosureExecutionRealm(
  const AClosure: TGocciaBytecodeClosure;
  const AFallbackRealm: TGocciaRealm): TGocciaRealm;
begin
  Result := AFallbackRealm;
  if Assigned(AClosure) and
     (AClosure.FunctionValue is TGocciaFunctionBase) and
     Assigned(TGocciaFunctionBase(AClosure.FunctionValue).CreationRealm) then
    Result := TGocciaFunctionBase(AClosure.FunctionValue).CreationRealm;
end;

function BytecodeClosureGlobalThis(
  const AClosure: TGocciaBytecodeClosure;
  const AFallbackGlobalThis: TGocciaValue): TGocciaValue;
var
  Root: TGocciaScope;
begin
  if Assigned(AClosure) and Assigned(AClosure.GlobalScope) then
  begin
    Root := AClosure.GlobalScope;
    while Assigned(Root.Parent) do
      Root := Root.Parent;
    if Assigned(Root.ThisValue) then
      Exit(Root.ThisValue);
  end;
  Result := AFallbackGlobalThis;
end;

function CoerceBytecodeNonStrictThisValue(
  const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AFallbackGlobalThis: TGocciaValue;
  const AFallbackRealm: TGocciaRealm): TGocciaValue;
var
  BoxingRealm, PreviousRealm: TGocciaRealm;
  ShouldSwitchRealm: Boolean;
begin
  BoxingRealm := BytecodeClosureExecutionRealm(AClosure, AFallbackRealm);
  PreviousRealm := CurrentRealm;
  ShouldSwitchRealm := Assigned(BoxingRealm) and
    (BoxingRealm <> PreviousRealm);
  if ShouldSwitchRealm then
    SetCurrentRealm(BoxingRealm);
  try
    Result := CoerceNonStrictThis(AThisValue,
      BytecodeClosureGlobalThis(AClosure, AFallbackGlobalThis));
  finally
    if ShouldSwitchRealm then
      SetCurrentRealm(PreviousRealm);
  end;
end;

function IsBytecodePrivateKey(const AKey: string): Boolean; forward;
function IsBytecodePrivateBrandKey(const AKey: string): Boolean; forward;
function HasBytecodePrivateInitializersApplied(const AInstance: TGocciaValue;
  const AClassValue: TGocciaClassValue): Boolean; forward;
procedure StampBytecodePrivateInitializersApplied(const AInstance: TGocciaValue;
  const AClassValue: TGocciaClassValue); forward;
function NormalizeBytecodePrivateKey(const AName,
  APrivateBrandToken: string): string; forward;
function BytecodePrivateRuntimeKey(const AName,
  APrivateBrandToken: string): string; forward;
function BytecodePrivateSourceName(const AName: string): string; forward;
function BytecodePrivateBrandKey(const AKey,
  APrivateBrandToken: string): string; forward;
function BytecodePrivateTokenForKey(const AKey,
  AFallbackPrivateBrandToken: string): string; forward;
function BytecodePrivateReceiverBrandToken(
  const AObject: TGocciaValue): string; forward;

function UTF16CodeUnitImmediateToString(const ACodeUnit: UInt16): string;
begin
  Result := Char(ACodeUnit);
end;

{ TGocciaVMDirectEvalScope }

constructor TGocciaVMDirectEvalScope.Create(const AParent: TGocciaScope;
  const AVM: TGocciaVM; const ATemplate: TGocciaFunctionTemplate;
  const AEnvironmentIndex: Integer; const AUseGlobalVarEnvironment: Boolean;
  const ALexicalClosure: TGocciaBytecodeClosure;
  const ANewTarget: TGocciaValue);
var
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
  BindingRuntimeValue: TGocciaValue;
  ThisBindingValue: TGocciaValue;
  I: Integer;
begin
  if AUseGlobalVarEnvironment then
    inherited Create(AParent, skGlobal, 'BytecodeDirectEval')
  else
    inherited Create(AParent, skFunction, 'BytecodeDirectEval');
  FVM := AVM;
  FTemplate := ATemplate;
  FEnvironmentIndex := AEnvironmentIndex;
  FNewTarget := ANewTarget;
  if Assigned(ALexicalClosure) then
  begin
    FHomeObject := ALexicalClosure.HomeObject;
    FHomeClass := ALexicalClosure.HomeClass;
  end;
  if Assigned(FVM) and Assigned(FVM.FGlobalThisValue) then
    ThisValue := FVM.FGlobalThisValue;
  if (FEnvironmentIndex < 0) or not Assigned(FTemplate) then
    Exit;

  Env := FTemplate.GetDirectEvalEnvironment(FEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
  begin
    Binding := Env.Bindings[I];
    if Binding.Name = KEYWORD_THIS then
    begin
      if Binding.Kind <> debGlobal then
      begin
        ThisBindingValue := BindingValue(Binding);
        if ThisBindingValue <> TGocciaHoleValue.HoleValue then
          ThisValue := ThisBindingValue;
      end;
      Continue;
    end;
    if Binding.IsVarEnvironmentBinding then
      Continue;
    if Binding.Kind in [debWithLocal, debWithUpvalue] then
      Continue;
    if Binding.Kind = debGlobal then
      Continue;
    BindingRuntimeValue := BindingValue(Binding);
    if BindingRuntimeValue = TGocciaHoleValue.HoleValue then
      Continue;
    if Binding.IsConst then
      DefineLexicalBinding(Binding.Name, BindingRuntimeValue, dtConst)
    else
      DefineLexicalBinding(Binding.Name, BindingRuntimeValue, dtLet);
  end;
end;

destructor TGocciaVMDirectEvalScope.Destroy;
begin
  FDeletedVarBindings.Free;
  inherited;
end;

procedure TGocciaVMDirectEvalScope.MarkReferences;
begin
  inherited;
  if Assigned(FHomeObject) then
    FHomeObject.MarkReferences;
  if Assigned(FHomeClass) then
    FHomeClass.MarkReferences;
  if Assigned(FNewTarget) then
    FNewTarget.MarkReferences;
end;

function TGocciaVMDirectEvalScope.TryFindBinding(const AName: string;
  out ABinding: TGocciaDirectEvalBindingInfo): Boolean;
var
  Env: TGocciaDirectEvalEnvironment;
  I: Integer;
begin
  if (FEnvironmentIndex < 0) or not Assigned(FTemplate) then
    Exit(False);
  Env := FTemplate.GetDirectEvalEnvironment(FEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
    if not (Env.Bindings[I].Kind in [debWithLocal, debWithUpvalue]) and
       (Env.Bindings[I].Name = AName) then
    begin
      ABinding := Env.Bindings[I];
      Exit(True);
    end;
  Result := False;
end;

function TGocciaVMDirectEvalScope.IsDeletedVarBinding(
  const AName: string): Boolean;
begin
  Result := Assigned(FDeletedVarBindings) and
    (FDeletedVarBindings.IndexOf(AName) >= 0) and
    not ContainsOwnVarBinding(AName);
end;

procedure TGocciaVMDirectEvalScope.MarkDeletedVarBinding(
  const AName: string);
begin
  if not Assigned(FDeletedVarBindings) then
  begin
    FDeletedVarBindings := TStringList.Create;
    FDeletedVarBindings.CaseSensitive := True;
    FDeletedVarBindings.Sorted := False;
    FDeletedVarBindings.Duplicates := dupIgnore;
  end;
  if FDeletedVarBindings.IndexOf(AName) < 0 then
    FDeletedVarBindings.Add(AName);
end;

function TGocciaVMDirectEvalScope.BindingValue(
  const ABinding: TGocciaDirectEvalBindingInfo): TGocciaValue;
var
  Upvalue: TGocciaBytecodeUpvalue;
begin
  case ABinding.Kind of
    debLocal:
      Result := FVM.GetLocal(ABinding.Index);
    debWithLocal:
      Result := FVM.GetLocal(ABinding.Index);
    debUpvalue:
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FVM.FCurrentClosure) then
      begin
        Upvalue := FVM.FCurrentClosure.GetUpvalue(ABinding.Index);
        if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          Result := RegisterToValue(Upvalue.Cell.Value);
      end;
    end;
    debWithUpvalue:
    begin
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Assigned(FVM.FCurrentClosure) then
      begin
        Upvalue := FVM.FCurrentClosure.GetUpvalue(ABinding.Index);
        if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          Result := RegisterToValue(Upvalue.Cell.Value);
      end;
    end;
    debGlobal:
      Result := FVM.FGlobalScope.GetBinding(ABinding.Name).Value;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaVMDirectEvalScope.HasWithObjectBinding(
  const AObject: TGocciaObjectValue; const AName: string): Boolean;
var
  Unscopables: TGocciaValue;
  Blocked: TGocciaValue;
begin
  Result := Assigned(AObject) and AObject.HasProperty(AName);
  if not Result then
    Exit;

  Unscopables := AObject.GetSymbolProperty(
    TGocciaSymbolValue.WellKnownUnscopables);
  if Unscopables is TGocciaObjectValue then
  begin
    Blocked := TGocciaObjectValue(Unscopables).GetProperty(AName);
    if Assigned(Blocked) and Blocked.ToBooleanLiteral.Value then
      Exit(False);
  end;
end;

function TGocciaVMDirectEvalScope.TryFindWithObjectBinding(
  const AName: string; out AObject: TGocciaObjectValue): Boolean;
var
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
  BindingObject: TGocciaValue;
  I: Integer;
begin
  AObject := nil;
  if (FEnvironmentIndex < 0) or not Assigned(FTemplate) then
    Exit(False);

  Env := FTemplate.GetDirectEvalEnvironment(FEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
  begin
    Binding := Env.Bindings[I];
    if not (Binding.Kind in [debWithLocal, debWithUpvalue]) then
      Continue;

    BindingObject := BindingValue(Binding);
    if (BindingObject is TGocciaObjectValue) and
       HasWithObjectBinding(TGocciaObjectValue(BindingObject), AName) then
    begin
      AObject := TGocciaObjectValue(BindingObject);
      Exit(True);
    end;
  end;

  Result := False;
end;

procedure TGocciaVMDirectEvalScope.SetBindingValue(
  const ABinding: TGocciaDirectEvalBindingInfo; const AValue: TGocciaValue);
var
  Upvalue: TGocciaBytecodeUpvalue;
begin
  case ABinding.Kind of
    debLocal:
      FVM.SetLocal(ABinding.Index, AValue);
    debWithLocal:
      ; // with-object bindings are object environment metadata, not variables.
    debUpvalue:
      if Assigned(FVM.FCurrentClosure) then
      begin
        Upvalue := FVM.FCurrentClosure.GetUpvalue(ABinding.Index);
        if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          Upvalue.Cell.Value := ValueToRegister(AValue);
      end;
    debWithUpvalue:
      ; // with-object bindings are object environment metadata, not variables.
    debGlobal:
      FVM.FGlobalScope.AssignBinding(ABinding.Name, AValue);
  end;
end;

function TemplateUsesGlobalEvalEnvironment(
  const ATemplate: TGocciaFunctionTemplate): Boolean;
begin
  Result := Assigned(ATemplate) and (ATemplate.Name = '<module>');
end;

function ClosureUsesGlobalScriptThis(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure): Boolean;
begin
  Result := Assigned(AVM) and AVM.FGlobalBackedTopLevel and
    Assigned(AClosure) and TemplateUsesGlobalEvalEnvironment(AClosure.Template);
end;

function DirectEvalLexicalClosure(const AVM: TGocciaVM): TGocciaBytecodeClosure;
var
  Candidate: TGocciaBytecodeClosure;
begin
  Result := nil;
  if not Assigned(AVM) then
    Exit;

  Candidate := AVM.FCurrentClosure;
  if not Assigned(Candidate) then
    Exit;

  if not (Assigned(Candidate.Template) and Candidate.Template.IsArrow) then
    Exit(Candidate);
  if Assigned(Candidate.HomeObject) or Assigned(Candidate.HomeClass) or
     Candidate.AllowsNewTarget then
    Exit(Candidate);
  Exit(nil);
end;

function CollectBytecodeDirectEvalPrivateNames(
  const AVM: TGocciaVM): TStringList;
var
  Closure: TGocciaBytecodeClosure;
  RawNames: TStringList;
  SourceName: string;
  I: Integer;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  Result.Sorted := False;
  Result.Duplicates := dupIgnore;

  Closure := DirectEvalLexicalClosure(AVM);
  if not (Assigned(Closure) and (Closure.HomeClass is TGocciaClassValue)) then
    Exit;

  RawNames := TStringList.Create;
  try
    RawNames.CaseSensitive := True;
    RawNames.Sorted := False;
    RawNames.Duplicates := dupIgnore;
    TGocciaClassValue(Closure.HomeClass).AppendOwnPrivateNames(RawNames);

    for I := 0 to RawNames.Count - 1 do
    begin
      SourceName := BytecodePrivateSourceName(RawNames[I]);
      if (SourceName <> '') and (Result.IndexOf(SourceName) < 0) then
        Result.Add(SourceName);
    end;
  finally
    RawNames.Free;
  end;
end;

function DirectEvalCapturedThisValue(const AVM: TGocciaVM;
  const ATemplate: TGocciaFunctionTemplate;
  const AEnvironmentIndex: Integer; out AValue: TGocciaValue): Boolean;
var
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
  Upvalue: TGocciaBytecodeUpvalue;
  I: Integer;
begin
  Result := False;
  if (not Assigned(AVM)) or (not Assigned(ATemplate)) or
     (AEnvironmentIndex < 0) then
    Exit;

  Env := ATemplate.GetDirectEvalEnvironment(AEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
  begin
    Binding := Env.Bindings[I];
    if Binding.Name <> KEYWORD_THIS then
      Continue;

    case Binding.Kind of
      debLocal:
        AValue := AVM.GetLocal(Binding.Index);
      debUpvalue:
      begin
        AValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if Assigned(AVM.FCurrentClosure) then
        begin
          Upvalue := AVM.FCurrentClosure.GetUpvalue(Binding.Index);
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
            AValue := RegisterToValue(Upvalue.Cell.Value);
        end;
      end;
      debGlobal:
        Exit(False);
    else
      AValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end;
    if AValue = TGocciaHoleValue.HoleValue then
      Exit(False);
    Exit(True);
  end;
end;

function DirectEvalLexicalThisValue(const AVM: TGocciaVM;
  const AUseGlobalVarEnvironment: Boolean;
  const ATemplate: TGocciaFunctionTemplate;
  const AEnvironmentIndex: Integer): TGocciaValue;
var
  Candidate: TGocciaBytecodeClosure;
  I, Slot: Integer;
begin
  if DirectEvalCapturedThisValue(AVM, ATemplate, AEnvironmentIndex, Result) then
    Exit;

  if Assigned(AVM) and Assigned(AVM.FCurrentClosure) and
     Assigned(AVM.FCurrentClosure.Template) and
     AVM.FCurrentClosure.Template.IsArrow then
    for I := AVM.FFrameStackCount - 1 downto 0 do
    begin
      Candidate := AVM.FFrameStack[I].Closure;
      if Assigned(Candidate) and Assigned(Candidate.Template) and
         not Candidate.Template.IsArrow then
      begin
        if ClosureUsesGlobalScriptThis(AVM, Candidate) then
        begin
          if Assigned(AVM.FGlobalThisValue) then
            Exit(AVM.FGlobalThisValue);
          Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
        if AVM.FFrameStack[I].LocalCellCount > 0 then
        begin
          Slot := AVM.FFrameStack[I].LocalCellBase;
          if (Slot >= 0) and (Slot < Length(AVM.FLocalCellStack)) and
             Assigned(AVM.FLocalCellStack[Slot]) then
            Exit(RegisterToValue(AVM.FLocalCellStack[Slot].Value));
        end;
        if AVM.FFrameStack[I].RegisterCount > 0 then
        begin
          Slot := AVM.FFrameStack[I].RegisterBase;
          if (Slot >= 0) and (Slot < Length(AVM.FRegisterStack)) then
            Exit(RegisterToValue(AVM.FRegisterStack[Slot]));
        end;
        Break;
      end;
    end;

  if Assigned(AVM) and ClosureUsesGlobalScriptThis(AVM, AVM.FCurrentClosure) then
  begin
    if Assigned(AVM.FGlobalThisValue) then
      Exit(AVM.FGlobalThisValue);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;
  if Assigned(AVM) and (AVM.FLocalCellCount > 0) then
    Exit(AVM.GetLocal(0));
  if AUseGlobalVarEnvironment then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if Assigned(AVM) and Assigned(AVM.FGlobalThisValue) then
    Exit(AVM.FGlobalThisValue);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVMDirectEvalScope.GetSuperClass: TGocciaValue;
var
  HomeClass: TGocciaClassValue;
begin
  Result := nil;

  if FHomeClass is TGocciaClassValue then
  begin
    HomeClass := TGocciaClassValue(FHomeClass);
    if Assigned(HomeClass.SuperClass) then
      Exit(HomeClass.SuperClass);
    if Assigned(HomeClass.NativeSuperConstructor) then
      Exit(HomeClass.NativeSuperConstructor);
  end;

  if Assigned(FHomeObject) then
    Result := FHomeObject;
end;

function TGocciaVMDirectEvalScope.GetOwningClass: TGocciaValue;
begin
  Result := FHomeClass;
end;

function TGocciaVMDirectEvalScope.GetSuperConstructor: TGocciaValue;
var
  HomeClass: TGocciaClassValue;
  SuperConstructor: TGocciaValue;
begin
  Result := nil;
  if not (Assigned(FVM) and (FHomeClass is TGocciaClassValue)) then
    Exit;

  HomeClass := TGocciaClassValue(FHomeClass);
  SuperConstructor := TGocciaObjectValue(HomeClass).Prototype;
  if not Assigned(SuperConstructor) then
    Exit;

  Result := TGocciaVMSuperConstructorValue.Create(SuperConstructor,
    FNewTarget, HomeClass);
end;

function TGocciaVMDirectEvalScope.GetNewTarget: TGocciaValue;
begin
  Result := FNewTarget;
end;

function TGocciaVMDirectEvalScope.GetThisValue: TGocciaValue;
begin
  Result := ThisValue;
end;

function TGocciaVMDirectEvalScope.MarkSuperConstructorCalled: Boolean;
var
  Binding: TGocciaDirectEvalBindingInfo;
begin
  Result := Assigned(FVM);
  if not Result then
    Exit;

  FVM.FCurrentConstructorSuperCalled := True;
  if TryFindBinding(DERIVED_THIS_INITIALIZED_LOCAL, Binding) then
    SetBindingValue(Binding, TGocciaBooleanLiteralValue.TrueValue);
  if FVM.FLocalCellCount > 0 then
    FVM.SetLocal(0, ThisValue);
  FVM.FLastClosureThisValue := ValueToRegister(ThisValue);
end;

function TGocciaVMDirectEvalScope.IsFunctionBoundary: Boolean;
begin
  Result := False;
end;

// Direct-eval environments resolve through their own binding table before
// the ordinary scope walk, so the Try* core must consult the with-object
// bindings and the captured binding table first and only then fall through
// to the inherited resolution.  GetBinding/AssignBinding need no overrides:
// the base wrappers over these Try* methods reproduce them exactly.
function TGocciaVMDirectEvalScope.TryGetBinding(const AName: string;
  out ABinding: TLexicalBinding; const ALine: Integer = 0;
  const AColumn: Integer = 0): Boolean;
var
  Binding: TGocciaDirectEvalBindingInfo;
  BindingRuntimeValue: TGocciaValue;
  WithObject: TGocciaObjectValue;
begin
  if TryFindWithObjectBinding(AName, WithObject) then
  begin
    ABinding.Value := WithObject.GetProperty(AName);
    ABinding.DeclarationType := dtVar;
    ABinding.Initialized := True;
    ABinding.BuiltIn := False;
    ABinding.GlobalObjectBacked := False;
    ABinding.CanDelete := False;
    ABinding.TypeHint := sltUntyped;
    Exit(True);
  end;

  if IsDeletedVarBinding(AName) then
  begin
    if Assigned(Parent) then
      Exit(Parent.TryGetBinding(AName, ABinding, ALine, AColumn));
    ABinding := Default(TLexicalBinding);
    Exit(False);
  end;

  if TryFindBinding(AName, Binding) then
  begin
    if (not Binding.IsConst) and ContainsOwnVarBinding(AName) then
      Exit(inherited TryGetBinding(AName, ABinding, ALine, AColumn));
    BindingRuntimeValue := BindingValue(Binding);
    if BindingRuntimeValue = TGocciaHoleValue.HoleValue then
      raise TGocciaReferenceError.Create(
        Format(SErrorCannotAccessBeforeInit, [AName]),
        ALine, AColumn, '', nil, SSuggestTemporalDeadZone);
    ABinding.Value := BindingRuntimeValue;
    if Binding.IsConst then
      ABinding.DeclarationType := dtConst
    else
      ABinding.DeclarationType := dtVar;
    ABinding.Initialized := True;
    ABinding.BuiltIn := False;
    ABinding.GlobalObjectBacked := False;
    ABinding.CanDelete := False;
    ABinding.TypeHint := sltUntyped;
    Exit(True);
  end;

  Result := inherited TryGetBinding(AName, ABinding, ALine, AColumn);
end;

function TGocciaVMDirectEvalScope.TryAssignExistingBinding(const AName: string;
  const AValue: TGocciaValue; const ANonStrictMode: Boolean = False;
  const ALine: Integer = 0; const AColumn: Integer = 0): Boolean;
var
  Binding: TGocciaDirectEvalBindingInfo;
  WithObject: TGocciaObjectValue;
begin
  if TryFindWithObjectBinding(AName, WithObject) then
  begin
    if ANonStrictMode then
      WithObject.AssignPropertyWithReceiver(AName, AValue, WithObject)
    else
      WithObject.AssignProperty(AName, AValue);
    Exit(True);
  end;

  if IsDeletedVarBinding(AName) then
  begin
    if Assigned(Parent) then
      Exit(Parent.TryAssignExistingBinding(AName, AValue, ANonStrictMode,
        ALine, AColumn));
    Exit(False);
  end;

  if ContainsOwnVarBinding(AName) then
    Exit(inherited TryAssignExistingBinding(AName, AValue, ANonStrictMode,
      ALine, AColumn));

  if TryFindBinding(AName, Binding) then
  begin
    // TDZ: a captured lexical binding still holding the hole sentinel has
    // not been initialized yet; assignment through direct eval must raise
    // the same ReferenceError the ordinary local/upvalue paths produce.
    if BindingValue(Binding) = TGocciaHoleValue.HoleValue then
      raise TGocciaReferenceError.Create(
        Format(SErrorCannotAccessBeforeInit, [AName]),
        ALine, AColumn, '', nil, SSuggestTemporalDeadZone);
    if Binding.IsConst then
    begin
      if ANonStrictMode and
         DirectEvalBindingIsNonStrictImmutable(Binding, FTemplate) then
        Exit(True);
      raise TGocciaTypeError.Create(
        Format(SErrorAssignToConstant, [AName]),
        ALine, AColumn, '', nil, SSuggestUseLetNotConst);
    end;
    // ES2026 §19.2.1.1 PerformEval step 18: direct eval's lexical
    // environment has the caller lexical environment as its outer
    // environment.  Captured bytecode locals are aliases for that caller
    // environment, so assignments update the captured slot/upvalue directly
    // instead of creating a local var shadow in this adapter scope.
    SetBindingValue(Binding, AValue);
    Exit(True);
  end;

  Result := inherited TryAssignExistingBinding(AName, AValue, ANonStrictMode,
    ALine, AColumn);
end;

function TGocciaVMDirectEvalScope.DeleteBinding(const AName: string): Boolean;
var
  WasOwnVarBinding: Boolean;
  WithObject: TGocciaObjectValue;
begin
  if TryFindWithObjectBinding(AName, WithObject) then
    Exit(WithObject.DeleteProperty(AName));

  WasOwnVarBinding := ContainsOwnVarBinding(AName);
  Result := inherited DeleteBinding(AName);
  if Result and WasOwnVarBinding then
    MarkDeletedVarBinding(AName);
end;

procedure TGocciaVMDirectEvalScope.ResolveIdentifierReference(
  const AName: string; out AValue, AThisValue: TGocciaValue;
  const ALine: Integer; const AColumn: Integer);
var
  WithObject: TGocciaObjectValue;
begin
  if TryFindWithObjectBinding(AName, WithObject) then
  begin
    AValue := WithObject.GetProperty(AName);
    AThisValue := WithObject;
    Exit;
  end;

  inherited ResolveIdentifierReference(AName, AValue, AThisValue, ALine,
    AColumn);
end;

procedure TGocciaVMDirectEvalScope.ResolveAssignmentTarget(const AName: string;
  out AObjectBinding: TGocciaObjectValue; out AScopeBinding: TGocciaScope);
var
  Binding: TGocciaDirectEvalBindingInfo;
  WithObject: TGocciaObjectValue;
begin
  if TryFindWithObjectBinding(AName, WithObject) then
  begin
    AObjectBinding := WithObject;
    AScopeBinding := nil;
    Exit;
  end;

  if IsDeletedVarBinding(AName) then
  begin
    if Assigned(Parent) then
      Parent.ResolveAssignmentTarget(AName, AObjectBinding, AScopeBinding)
    else
    begin
      AObjectBinding := nil;
      AScopeBinding := Self;
    end;
    Exit;
  end;

  if ContainsOwnVarBinding(AName) then
  begin
    AObjectBinding := nil;
    AScopeBinding := Self;
    Exit;
  end;

  if TryFindBinding(AName, Binding) then
  begin
    AObjectBinding := nil;
    AScopeBinding := Self;
    Exit;
  end;
  inherited ResolveAssignmentTarget(AName, AObjectBinding, AScopeBinding);
end;

function TGocciaVMDirectEvalScope.Contains(const AName: string): Boolean;
var
  Binding: TGocciaDirectEvalBindingInfo;
  WithObject: TGocciaObjectValue;
begin
  if IsDeletedVarBinding(AName) then
  begin
    if Assigned(Parent) then
      Exit(Parent.Contains(AName));
    Exit(False);
  end;

  if TryFindWithObjectBinding(AName, WithObject) or inherited Contains(AName) then
    Exit(True);
  Result := TryFindBinding(AName, Binding) and
    (Binding.Kind in [debLocal, debGlobal]);
end;

function TGocciaVMDirectEvalScope.ContainsVarEnvironmentBinding(
  const AName: string): Boolean;
var
  Binding: TGocciaDirectEvalBindingInfo;
begin
  if ContainsOwnLexicalBinding(AName) or ContainsOwnVarBinding(AName) then
    Exit(True);
  Result := TryFindBinding(AName, Binding) and
    (Binding.Kind = debLocal) and Binding.IsVarEnvironmentBinding;
end;

procedure TGocciaVMDirectEvalScope.CopyBackVariableBindings;
var
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
  LexicalBinding: TLexicalBinding;
  I: Integer;
begin
  if (FEnvironmentIndex < 0) or not Assigned(FTemplate) then
    Exit;
  Env := FTemplate.GetDirectEvalEnvironment(FEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
  begin
    Binding := Env.Bindings[I];
    if IsDeletedVarBinding(Binding.Name) then
      Continue;
    if Binding.IsConst or (Binding.Kind in [debGlobal, debWithLocal,
       debWithUpvalue]) then
      Continue;
    if ContainsOwnVarBinding(Binding.Name) and (Binding.Kind <> debUpvalue) then
    begin
      // Read this scope's own var environment directly: GetBinding would
      // dispatch back through this scope's TryGetBinding override, whose
      // with-object head shadows the var binding being copied back
      // (compat block function hoisting inside `with`).
      if TryGetOwnBinding(Binding.Name, LexicalBinding) then
        SetBindingValue(Binding, LexicalBinding.Value);
    end;
  end;
end;

procedure TGocciaVMDirectEvalScope.CopyNewVariableBindingsToParent;
var
  Names: TGocciaStringArray;
  Name: string;
  Binding: TLexicalBinding;
begin
  if not Assigned(Parent) then
    Exit;

  Names := GetOwnVarBindingNames;
  for Name in Names do
  begin
    if IsDeletedVarBinding(Name) then
      Continue;
    if not ContainsOwnVarBinding(Name) then
      Continue;
    if not TryGetOwnBinding(Name, Binding) then
      Continue;
    Parent.DefineVariableBinding(Name, Binding.Value, True, True);
    // ES2026 §19.2.1.3 EvalDeclarationInstantiation creates deletable
    // bindings in variableEnv. Remove only this adapter's temporary binding;
    // virtual DeleteBinding would resolve a same-named with-object property.
    if inherited DeleteBinding(Name) then
      MarkDeletedVarBinding(Name);
  end;
end;

function VMIsObjectInstanceOfClass(const AObj: TGocciaObjectValue;
  const AClassValue: TGocciaClassValue): Boolean;
var
  CurrentPrototype: TGocciaObjectValue;
  TargetPrototype: TGocciaObjectValue;
begin
  Result := False;
  TargetPrototype := AClassValue.Prototype;
  if not Assigned(TargetPrototype) then
    Exit;

  CurrentPrototype := AObj.Prototype;
  while Assigned(CurrentPrototype) do
  begin
    if CurrentPrototype = TargetPrototype then
      Exit(True);
    CurrentPrototype := CurrentPrototype.Prototype;
  end;
end;

function VMIsPrototypeInChain(const AObj: TGocciaObjectValue;
  const ATargetProto: TGocciaObjectValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  CurrentProto: TGocciaObjectValue;
begin
  Result := False;
  CurrentProto := AObj.Prototype;
  while Assigned(CurrentProto) do
  begin
    if CurrentProto = ATargetProto then
      Exit(True);
    CurrentProto := CurrentProto.Prototype;
  end;
end;

function VMHasSymbolPropertyInChain(const AObject: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Current: TGocciaObjectValue;
begin
  Current := AObject;
  while Assigned(Current) do
  begin
    if Current.HasSymbolProperty(ASymbol) then
      Exit(True);
    Current := Current.Prototype;
  end;
  Result := False;
end;

function VMGetOwnDataDescriptorValue(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // Exact-class match: a lazy descriptor (TGocciaLazyPropertyDescriptorData, the
  // only TGocciaPropertyDescriptorData subclass) must NOT be served raw here —
  // its Value is undefined until materialized. Excluding it routes the first
  // read through the virtual GetProperty path, which materializes and replaces
  // the entry in place with a plain descriptor, so later reads hit this fast path.
  Result := Assigned(AObject) and
    AObject.Properties.TryGetValue(AName, Descriptor) and
    (Descriptor.ClassType = TGocciaPropertyDescriptorData);
  if Result then
    AValue := TGocciaPropertyDescriptorData(Descriptor).Value
  else
    AValue := nil;
end;

function VMTrySetOwnWritableDataProperty(const AObject: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // This is the ordinary-object own writable-data branch of
  // OrdinarySetWithOwnDescriptor with Receiver = O.  Exact class checks keep
  // exotic/overridden assignment semantics on the virtual fallback, while
  // exact descriptor checks keep lazy properties on their materializing path.
  Result := Assigned(AObject) and
    (AObject.ClassType = TGocciaObjectValue) and
    AObject.Properties.TryGetValue(AName, Descriptor) and
    (Descriptor.ClassType = TGocciaPropertyDescriptorData) and
    Descriptor.Writable;
  if Result then
    TGocciaPropertyDescriptorData(Descriptor).Value := AValue;
end;

function VMTryGetCachedGlobalOwnDataProperty(
  const AObject: TGocciaObjectValue; const AEntryIndex: Integer;
  const AVersion: Cardinal; out AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // The exact object/descriptor checks exclude every exotic lookup override
  // and lazy data descriptors.  EntryVersion changes when deletion, clear, or
  // compaction could invalidate an entry index; value updates and unrelated
  // additions leave the cached index valid and the live descriptor is re-read.
  Result := Assigned(AObject) and
    (AObject.ClassType = TGocciaObjectValue) and
    (AObject.Properties.EntryVersion = AVersion) and
    AObject.Properties.TryGetValueAtEntry(AEntryIndex, Descriptor) and
    (Descriptor.ClassType = TGocciaPropertyDescriptorData);
  if Result then
    AValue := TGocciaPropertyDescriptorData(Descriptor).Value
  else
    AValue := nil;
end;

function VMTryGetGlobalOwnDataPropertyFillCache(
  const AObject: TGocciaObjectValue; const AName: string;
  out AEntryIndex: Integer; out AVersion: Cardinal): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  AEntryIndex := -1;
  AVersion := 0;
  Result := Assigned(AObject) and
    (AObject.ClassType = TGocciaObjectValue) and
    AObject.Properties.TryGetEntryIndex(AName, AEntryIndex) and
    AObject.Properties.TryGetValueAtEntry(AEntryIndex, Descriptor) and
    (Descriptor.ClassType = TGocciaPropertyDescriptorData);
  if Result then
    AVersion := AObject.Properties.EntryVersion;
end;

const
  GLOBAL_READ_OBJECT_BINDING_NONE = 0;
  GLOBAL_READ_OBJECT_BINDING_VAR = 1;
  GLOBAL_READ_OBJECT_BINDING_BUILTIN = 2;

function VMGlobalObjectBindingCacheStillPrecedes(
  const AScope: TGocciaScope; const ACache: PGocciaGlobalReadCacheEntry): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  case ACache^.ObjectBindingKind of
    GLOBAL_READ_OBJECT_BINDING_VAR:
      // Registered global var names cannot legally gain a same-name global
      // lexical declaration; global declaration instantiation rejects it.
      Result := True;
    GLOBAL_READ_OBJECT_BINDING_BUILTIN:
      Result := AScope.IsGlobalBuiltInObjectBindingAt(
        ACache^.BindingEntryIndex, ACache^.BindingVersion);
  else
    Result := False;
  end;
end;

function VMValueToRegisterFast(const AValue: TGocciaValue): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
var
  NumberValue: Double;
begin
  if not Assigned(AValue) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit(RegisterUndefined);
  if AValue is TGocciaNullLiteralValue then
    Exit(RegisterNull);
  if AValue = TGocciaHoleValue.HoleValue then
    Exit(RegisterHole);
  if AValue = TGocciaBooleanLiteralValue.TrueValue then
    Exit(RegisterBoolean(True));
  if AValue = TGocciaBooleanLiteralValue.FalseValue then
    Exit(RegisterBoolean(False));
  if AValue is TGocciaNumberLiteralValue then
  begin
    NumberValue := TGocciaNumberLiteralValue(AValue).Value;
    if NumberValue = 0.0 then
    begin
      if NumberBits.IsNegativeZero(NumberValue) then
        Exit(RegisterObject(AValue));
      Exit(RegisterInt(0));
    end;
    if NumberValue = 1.0 then
      Exit(RegisterInt(1));
    if (not TGocciaNumberLiteralValue(AValue).IsNaN) and
       (not TGocciaNumberLiteralValue(AValue).IsInfinite) and
       (Frac(NumberValue) = 0.0) and
       (NumberValue >= Low(LongInt)) and
       (NumberValue <= High(LongInt)) then
      Exit(RegisterInt(Trunc(NumberValue)));
    Exit(RegisterFloat(NumberValue));
  end;
  Result := RegisterObject(AValue);
end;

// Receivers whose own plain-data property reads are ordinary map lookups,
// so the OP_GET_PROP_CONST inline cache may serve them without going
// through their virtual GetProperty path. Exact-class checks exclude every
// subclass with overridden lookup semantics (proxies, exotic objects).
function VMPropertyReadCacheableReceiver(const AObject: TObject): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AObject.ClassType = TGocciaObjectValue) or
    (AObject.ClassType = TGocciaVMLiteralObjectValue) or
    (AObject.ClassType = TGocciaInstanceValue);
end;

// Validate a property-read inline cache entry against the receiver's shape
// and read the current value through the receiver's live map. The cached
// Shape pointer is compared for identity only; same shape implies the same
// key at the cached entry index. Every TGocciaObjectValue property map is
// constructed as TGocciaShapedPropertyMap (single construction site in
// TGocciaObjectValue.Create), so the static cast is structurally safe.
// The descriptor kind is re-checked on every hit because data->accessor
// redefinition replaces the descriptor without changing the layout
// (TOrderedStringMap.Add on an existing key).
function VMTryGetCachedOwnDataProperty(const AObject: TGocciaObjectValue;
  const ACache: PGocciaPropertyReadCacheEntry;
  out AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // Exact-class match excludes a not-yet-materialized lazy descriptor (see
  // VMGetOwnDataDescriptorValue); a cache miss here routes the first read
  // through GetProperty, which materializes and replaces the entry in place.
  Result := (ACache^.Shape = Pointer(
      TGocciaShapedPropertyMap(AObject.Properties).Shape)) and
    (ACache^.Shape <> nil) and
    AObject.Properties.TryGetValueAtEntry(ACache^.EntryIndex, Descriptor) and
    (Descriptor.ClassType = TGocciaPropertyDescriptorData);
  if Result then
  begin
    AValue := TGocciaPropertyDescriptorData(Descriptor).Value;
    // A validated hit proves the site is currently serving a stable shape:
    // reset the streak so only CONSECUTIVE misses count toward megamorphic
    // saturation (transient warm-up polymorphism must not permanently
    // disable a site that settles down).
    if ACache^.MissStreak <> 0 then
      ACache^.MissStreak := 0;
  end
  else
    AValue := nil;
end;

const
  // Consecutive different-map refills after which an OP_GET_PROP_CONST site
  // is treated as megamorphic: the cache stops being rewritten and reads use
  // the uncached own-data fast path instead.
  PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT = 16;


type
  // Outcome of one own-map probe; lets the OP_GET_PROP_CONST miss path
  // establish own-data / own-non-data / absent with a single hash lookup
  // instead of re-hashing the same name per fallback tier.
  TGocciaOwnPropertyProbe = (oppData, oppNonData, oppAbsent);

function VMProbeOwnProperty(const AObject: TGocciaObjectValue;
  const AName: string; out AEntryIndex: Integer;
  out ADescriptor: TGocciaPropertyDescriptor): TGocciaOwnPropertyProbe;
begin
  ADescriptor := nil;
  if AObject.Properties.TryGetEntryIndex(AName, AEntryIndex) and
     AObject.Properties.TryGetValueAtEntry(AEntryIndex, ADescriptor) then
  begin
    if ADescriptor is TGocciaPropertyDescriptorData then
      Result := oppData
    else
      Result := oppNonData;
  end
  else
    Result := oppAbsent;
end;

// Prime the own-tier cache from an already-completed probe (no second
// lookup). Declines silently for dictionary-mode maps (advancing the
// streak so those sites converge) and for entries beyond a prefix shape's
// covered depth.
procedure VMPrimeOwnPropertyCache(const AObject: TGocciaObjectValue;
  const AEntryIndex: Integer; const ACache: PGocciaPropertyReadCacheEntry);
var
  ReceiverShape: TGocciaShape;
begin
  ReceiverShape := TGocciaShapedPropertyMap(AObject.Properties).EnsureShape;
  if ReceiverShape = DictionaryShapeSentinel then
  begin
    Inc(ACache^.MissStreak);
    Exit;
  end;
  // A found entry implies a non-empty map, so a real shape exists; the
  // depth guard only blocks entries past a transition-capped prefix.
  if (not Assigned(ReceiverShape)) or
     (AEntryIndex >= ReceiverShape.Depth) then
    Exit;
  if (ACache^.Shape <> nil) and
     (ACache^.Shape <> Pointer(ReceiverShape)) then
    Inc(ACache^.MissStreak);
  ACache^.Shape := Pointer(ReceiverShape);
  ACache^.EntryIndex := AEntryIndex;
end;

// Presence probe for the holder level: pointer identity suffices — a
// prefix shape's covered entries stay valid as the holder map grows, and
// the descriptor is re-read by entry index on every hit.
function VMHolderShapeMatches(const AObject: TGocciaObjectValue;
  const ACachedShape: Pointer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := Pointer(
    TGocciaShapedPropertyMap(AObject.Properties).EnsureShape) = ACachedShape;
end;

// Absence probe for receiver/intermediate levels: pointer identity PLUS
// full coverage (Depth = Count). A transition-capped map can grow while
// EnsureShape keeps returning the same prefix pointer, so pointer equality
// alone cannot prove a name is still absent.
function VMAbsenceShapeMatches(const AObject: TGocciaObjectValue;
  const ACachedShape: Pointer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Map: TGocciaShapedPropertyMap;
  LevelShape: TGocciaShape;
begin
  Map := TGocciaShapedPropertyMap(AObject.Properties);
  LevelShape := Map.EnsureShape;
  Result := (Pointer(LevelShape) = ACachedShape) and
    ((not Assigned(LevelShape)) or (LevelShape.Depth = Map.CountFast));
end;

// Validate a prototype-holder cache entry: receiver gate, fresh-shape
// absence below the holder, fresh-shape presence at the holder, exact
// TGocciaObjectValue chain levels (exotic objects may share shapes but not
// lookup semantics), then re-read the holder descriptor by entry index.
function VMTryGetCachedProtoProperty(const AReceiver: TGocciaObjectValue;
  const ACache: PGocciaProtoReadCacheEntry;
  out AValue: TGocciaValue): Boolean;
var
  Level: Integer;
  Walk: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AValue := nil;
  Result := False;
  if ACache^.HolderLevel = 0 then
    Exit;
  if not VMPropertyReadCacheableReceiver(AReceiver) then
    Exit;
  if not VMAbsenceShapeMatches(AReceiver, ACache^.Shapes[0]) then
    Exit;
  Walk := AReceiver;
  for Level := 1 to ACache^.HolderLevel do
  begin
    Walk := Walk.Prototype;
    if (not Assigned(Walk)) or (Walk.ClassType <> TGocciaObjectValue) then
      Exit;
    if Level = ACache^.HolderLevel then
    begin
      if not VMHolderShapeMatches(Walk, ACache^.Shapes[Level]) then
        Exit;
    end
    else if not VMAbsenceShapeMatches(Walk, ACache^.Shapes[Level]) then
      Exit;
  end;
  if not Walk.Properties.TryGetValueAtEntry(ACache^.EntryIndex,
    Descriptor) then
    Exit;
  // Exact-class match: never serve a not-yet-materialized lazy descriptor raw
  // (see VMGetOwnDataDescriptorValue).
  if Descriptor.ClassType <> TGocciaPropertyDescriptorData then
    Exit;
  AValue := TGocciaPropertyDescriptorData(Descriptor).Value;
  // Validated hit: reset the streak so only consecutive misses count (see
  // VMTryGetCachedOwnDataProperty).
  if ACache^.MissStreak <> 0 then
    ACache^.MissStreak := 0;
  Result := True;
end;

// One uncached prototype-chain lookup (at most two levels) that also primes
// the prototype-holder cache. Declines — leaving the cache untouched — for
// dictionary-mode maps, exotic chain levels, own-present names, accessor
// holder properties, and chains deeper than two levels; all of those stay on
// the generic GetPropertyValue path.
function VMFillProtoReadCacheCore(const AReceiver: TGocciaObjectValue;
  const AName: string; const ACache: PGocciaProtoReadCacheEntry;
  out AValue: TGocciaValue): Boolean;
var
  Shapes: array [0 .. 2] of Pointer;
  WalkShape: TGocciaShape;
  Walk: TGocciaObjectValue;
  Level, EntryIndex: Integer;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AValue := nil;
  Result := False;
  // Contract: the caller has already established (via VMProbeOwnProperty)
  // that AName is not an own property of AReceiver — no own re-probe here.
  WalkShape := TGocciaShapedPropertyMap(AReceiver.Properties).EnsureShape;
  if WalkShape = DictionaryShapeSentinel then
    Exit;
  // Absence levels need full coverage: a transition-capped prefix shape
  // cannot prove the name is absent from the uncovered suffix.
  if Assigned(WalkShape) and
     (WalkShape.Depth <> AReceiver.Properties.CountFast) then
    Exit;
  Shapes[0] := Pointer(WalkShape);
  Shapes[1] := nil;
  Shapes[2] := nil;
  Walk := AReceiver;
  for Level := 1 to 2 do
  begin
    Walk := Walk.Prototype;
    if (not Assigned(Walk)) or (Walk.ClassType <> TGocciaObjectValue) then
      Exit;
    WalkShape := TGocciaShapedPropertyMap(Walk.Properties).EnsureShape;
    if WalkShape = DictionaryShapeSentinel then
      Exit;
    Shapes[Level] := Pointer(WalkShape);
    if not Walk.Properties.TryGetEntryIndex(AName, EntryIndex) then
    begin
      // Absent at this level: full coverage required for the absence proof.
      if Assigned(WalkShape) and
         (WalkShape.Depth <> Walk.Properties.CountFast) then
        Exit;
      Continue;
    end;
    if (not Walk.Properties.TryGetValueAtEntry(EntryIndex, Descriptor)) or
       (Descriptor.ClassType <> TGocciaPropertyDescriptorData) then
      Exit;
    // Presence at the holder: the entry must lie inside the shape's
    // covered prefix for the cached (shape, index) pair to stay valid.
    if (not Assigned(WalkShape)) or (EntryIndex >= WalkShape.Depth) then
      Exit;
    AValue := TGocciaPropertyDescriptorData(Descriptor).Value;
    if (ACache^.HolderLevel > 0) and
       (ACache^.Shapes[0] <> Shapes[0]) then
      Inc(ACache^.MissStreak);
    ACache^.Shapes[0] := Shapes[0];
    ACache^.Shapes[1] := Shapes[1];
    ACache^.Shapes[2] := Shapes[2];
    ACache^.EntryIndex := EntryIndex;
    ACache^.HolderLevel := Level;
    Result := True;
    Exit;
  end;
end;

function VMFillProtoReadCache(const AReceiver: TGocciaObjectValue;
  const AName: string; const ACache: PGocciaProtoReadCacheEntry;
  out AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := VMFillProtoReadCacheCore(AReceiver, AName, ACache, AValue);
  // Every decline advances the streak so sites that can never use this
  // tier (own-resolved names on megamorphic sites, accessor holders, deep
  // or exotic chains) converge on the dormant state instead of paying the
  // chain probe on every read. The caller gates fills on MissStreak, so
  // this cannot overflow.
  if not Result then
    Inc(ACache^.MissStreak);
end;

function VMGetOwnDataDescriptorRegister(const AObject: TGocciaObjectValue;
  const AName: string; out AValue: TGocciaRegister): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Value: TGocciaValue;
begin
  Result := VMGetOwnDataDescriptorValue(AObject, AName, Value);
  if not Result then
  begin
    AValue := RegisterUndefined;
    Exit;
  end;

  AValue := VMValueToRegisterFast(Value);
end;

function VMSetOwnWritableDataDescriptorValue(const AObject: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // Exact-class match: route writes to a not-yet-materialized lazy descriptor
  // through the slow [[Set]] path, so a write is not silently overwritten by a
  // later materialization (see VMGetOwnDataDescriptorValue).
  Result := Assigned(AObject) and
    AObject.Properties.TryGetValue(AName, Descriptor) and
    (Descriptor.ClassType = TGocciaPropertyDescriptorData);
  if not Result then
    Exit;
  if not TGocciaPropertyDescriptorData(Descriptor).Writable then
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]),
      SSuggestReadOnlyProperty);
  TGocciaPropertyDescriptorData(Descriptor).Value := AValue;
end;

// ES2026 §7.3.6 CreateDataPropertyOrThrow(O, P, V)
procedure DefineDataPropertyOnObject(const ATarget: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}
begin
  if (ATarget is TGocciaVMLiteralObjectValue) and
     TGocciaVMLiteralObjectValue(ATarget)
       .TrySetLiteralDataPropertyFast(AName, AValue) then
    Exit;

  ATarget.CreateDataPropertyOrThrow(AName, AValue);
end;

// ES2026 §7.3.6 CreateDataPropertyOrThrow(O, P, V) — symbol variant
procedure DefineSymbolDataPropertyOnObject(const ATarget: TGocciaObjectValue;
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}
begin
  ATarget.CreateDataPropertyOrThrow(ASymbol, AValue);
end;

function VMTryParseArrayPropertyIndex(const AKey: string;
  out AIndex: Int64): Boolean;
var
  Digit: Int64;
  I: Integer;
begin
  AIndex := 0;
  Result := False;
  if AKey = '' then
    Exit;
  if (AKey[1] = '0') and (Length(AKey) > 1) then
    Exit;

  for I := 1 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then
      Exit;
    Digit := Ord(AKey[I]) - Ord('0');
    if AIndex > (MAX_SAFE_INTEGER - Digit) div 10 then
      Exit;
    AIndex := AIndex * 10 + Digit;
  end;

  Result := AIndex < MAX_ARRAY_LENGTH;
end;

function VMOrderOwnPropertyStringKeys(const AKeys: TArray<string>):
  TArray<string>;
var
  ParsedIndex, TempIndex: Int64;
  NumericKeys: TArray<Int64>;
  OtherKeys: TArray<string>;
  I, J, K, Count: Integer;
begin
  SetLength(NumericKeys, Length(AKeys));
  SetLength(OtherKeys, Length(AKeys));
  Count := 0;
  J := 0;

  for I := 0 to High(AKeys) do
  begin
    if VMTryParseArrayPropertyIndex(AKeys[I], ParsedIndex) then
    begin
      NumericKeys[Count] := ParsedIndex;
      Inc(Count);
    end
    else
    begin
      OtherKeys[J] := AKeys[I];
      Inc(J);
    end;
  end;

  for I := 1 to Count - 1 do
  begin
    TempIndex := NumericKeys[I];
    K := I - 1;
    while (K >= 0) and (NumericKeys[K] > TempIndex) do
    begin
      NumericKeys[K + 1] := NumericKeys[K];
      Dec(K);
    end;
    NumericKeys[K + 1] := TempIndex;
  end;

  SetLength(Result, Count + J);
  for I := 0 to Count - 1 do
    Result[I] := IntToStr(NumericKeys[I]);
  for I := 0 to J - 1 do
    Result[Count + I] := OtherKeys[I];
end;

function VMOwnPropertyKeysAsValues(
  const ASource: TGocciaObjectValue): TArray<TGocciaValue>;
var
  Count: Integer;
  I: Integer;
  StringKeys: TArray<string>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
begin
  if ASource is TGocciaProxyValue then
    Exit(TGocciaProxyValue(ASource).GetOwnPropertyKeyValues);

  StringKeys := VMOrderOwnPropertyStringKeys(ASource.GetAllPropertyNames);
  SymbolKeys := ASource.GetOwnSymbols;
  SetLength(Result, Length(StringKeys) + Length(SymbolKeys));
  Count := 0;
  for I := 0 to High(StringKeys) do
  begin
    Result[Count] := TGocciaStringLiteralValue.Create(StringKeys[I]);
    Inc(Count);
  end;
  for I := 0 to High(SymbolKeys) do
  begin
    Result[Count] := SymbolKeys[I];
    Inc(Count);
  end;
end;

function VMCopyDataPropertyKeyExcluded(const AKey: TGocciaValue;
  const AExclusionKeys: TGocciaArrayValue): Boolean;
var
  ExclusionKey: TGocciaValue;
  I: Integer;
begin
  if not Assigned(AExclusionKeys) then
    Exit(False);

  for I := 0 to AExclusionKeys.Elements.Count - 1 do
  begin
    ExclusionKey := AExclusionKeys.GetElement(I);
    if IsSameValue(ExclusionKey, AKey) then
      Exit(True);
  end;
  Result := False;
end;

// ES2026 §7.3.25 CopyDataProperties(target, source, excludedItems)
procedure VMCopyDataProperties(const ATarget: TGocciaObjectValue;
  const ASource: TGocciaValue; const AExclusionKeys: TGocciaArrayValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  Key: TGocciaValue;
  KeyName: string;
  Keys: TArray<TGocciaValue>;
  SourceObject: TGocciaObjectValue;
  SourceRooted: Boolean;
  SymbolKey: TGocciaSymbolValue;
  Value: TGocciaValue;
begin
  if not Assigned(ATarget) or
     (ASource is TGocciaUndefinedLiteralValue) or
     (ASource is TGocciaNullLiteralValue) then
    Exit;

  SourceObject := ToObject(ASource);
  SourceRooted := (TGarbageCollector.Instance <> nil) and
    not (ASource is TGocciaObjectValue);
  if SourceRooted then
    TGarbageCollector.Instance.AddTempRoot(SourceObject);

  try
    Keys := VMOwnPropertyKeysAsValues(SourceObject);
    for Key in Keys do
    begin
      if VMCopyDataPropertyKeyExcluded(Key, AExclusionKeys) then
        Continue;

      if Key is TGocciaSymbolValue then
      begin
        SymbolKey := TGocciaSymbolValue(Key);
        Descriptor := SourceObject.GetOwnSymbolPropertyDescriptor(SymbolKey);
        if Assigned(Descriptor) and Descriptor.Enumerable then
        begin
          Value := SourceObject.GetSymbolProperty(SymbolKey);
          DefineSymbolDataPropertyOnObject(ATarget, SymbolKey, Value);
        end;
      end
      else
      begin
        KeyName := Key.ToStringLiteral.Value;
        Descriptor := SourceObject.GetOwnPropertyDescriptor(KeyName);
        if Assigned(Descriptor) and Descriptor.Enumerable then
        begin
          Value := SourceObject.GetProperty(KeyName);
          DefineDataPropertyOnObject(ATarget, KeyName, Value);
        end;
      end;
    end;
  finally
    if SourceRooted then
      TGarbageCollector.Instance.RemoveTempRoot(SourceObject);
  end;
end;

function VMNumberValue(const AValue: Double): TGocciaNumberLiteralValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  if Math.IsNaN(AValue) then
    Exit(TGocciaNumberLiteralValue.NaNValue);
  if Math.IsInfinite(AValue) then
  begin
    if AValue > 0 then
      Exit(TGocciaNumberLiteralValue.InfinityValue);
    Exit(TGocciaNumberLiteralValue.NegativeInfinityValue);
  end;
  if AValue = 0.0 then
  begin
    if NumberBits.IsNegativeZero(AValue) then
      Exit(TGocciaNumberLiteralValue.NegativeZeroValue);
    Exit(TGocciaNumberLiteralValue.ZeroValue);
  end;
  if AValue = 1.0 then
    Exit(TGocciaNumberLiteralValue.OneValue);
  Result := TGocciaNumberLiteralValue.Create(AValue);
end;

function VMNumberRegister(const AValue: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  if Math.IsNaN(AValue) or Math.IsInfinite(AValue) then
    Exit(RegisterObject(VMNumberValue(AValue)));
  if AValue = 0.0 then
  begin
    if NumberBits.IsNegativeZero(AValue) then
      Exit(RegisterObject(TGocciaNumberLiteralValue.NegativeZeroValue));
    Exit(RegisterInt(0));
  end;
  if (AValue >= Low(LongInt)) and (AValue <= High(LongInt)) and
     (Frac(AValue) = 0.0) then
    Exit(RegisterInt(Trunc(AValue)));
  Result := RegisterFloat(AValue);
end;

function VMModuloRegister(const ALeft, ARight: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := VMNumberRegister(NumberRemainder(ALeft, ARight));
end;

function VMPowerRegister(const ALeft, ARight: Double): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := VMNumberRegister(NumberExponentiation(ALeft, ARight));
end;


// ES2026 Types-as-comments: runtime guard for OP_CHECK_TYPE (compiler emits Ord(TGocciaLocalType) in operand B).
// Delegates to Goccia.Types.Enforcement.EnforceStrictType so the interpreter
// and bytecode VM share a single enforcement implementation.
procedure VMStrictTypeCheckRegisterValue(const AValue: TGocciaValue;
  const AExpected: TGocciaLocalType); {$IFDEF FPC}inline;{$ENDIF}
begin
  EnforceStrictType(AValue, AExpected);
end;

// Integer-only result: skips IsNaN/IsInfinite/Frac checks that VMNumberRegister
// performs, since integer arithmetic on LongInt-range inputs cannot produce
// NaN, Infinity, negative zero, or fractional results.
// Uses implicit Double assignment (not Int64 * 1.0) to avoid AArch64 FPC 3.2.2
// codegen bug where Int64 * 1.0 produces wrong results near LongInt boundaries.
function VMIntResult(const AValue: Int64): TGocciaRegister; {$IFDEF FPC}inline;{$ENDIF}
var
  FloatValue: Double;
begin
  if (AValue >= Low(LongInt)) and (AValue <= High(LongInt)) then
    Result := RegisterInt(AValue)
  else
  begin
    FloatValue := AValue;
    Result := RegisterFloat(FloatValue);
  end;
end;

function VMRegisterToStringFast(
  const AValue: TGocciaRegister): TGocciaStringLiteralValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  case AValue.Kind of
    grkUndefined:
      Exit(TGocciaStringLiteralValue.Create('undefined'));
    grkNull:
      Exit(TGocciaStringLiteralValue.Create('null'));
    grkHole:
      Exit(TGocciaStringLiteralValue.Create('undefined'));
    grkBoolean:
      if AValue.BoolValue then
        Exit(TGocciaStringLiteralValue.Create('true'))
      else
        Exit(TGocciaStringLiteralValue.Create('false'));
    grkInt:
      Exit(TGocciaStringLiteralValue.Create(IntToStr(AValue.IntValue)));
    grkFloat:
      Exit(RegisterToValue(AValue).ToStringLiteral);
    grkObject:
      begin
        if Assigned(AValue.ObjectValue) then
          // Spec ToString — for objects, dispatches through
          // TGocciaObjectValue.ToStringLiteral which performs ToPrimitive(string)
          // and may invoke user toString()/valueOf().
          Exit(AValue.ObjectValue.ToStringLiteral);
      end;
  end;
  Result := TGocciaStringLiteralValue.Create('');
end;

function VMGlobalConstructor(const AScope: TGocciaScope;
  const AName: string): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
var
  RootScope: TGocciaScope;
begin
  RootScope := AScope;
  while Assigned(RootScope) and Assigned(RootScope.Parent) do
    RootScope := RootScope.Parent;

  if Assigned(RootScope) and RootScope.ContainsOwnLexicalBinding(AName) then
    Result := RootScope.GetValue(AName)
  else
    Result := nil;
end;

function VMGlobalObjectConstructor(const AScope: TGocciaScope): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := VMGlobalConstructor(AScope, CONSTRUCTOR_OBJECT);
end;

function VMGlobalFunctionConstructor(const AScope: TGocciaScope): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := VMGlobalConstructor(AScope, CONSTRUCTOR_FUNCTION);
end;

function VMBuiltinConstructorMatchValue(const AMatcher, ASubject: TGocciaValue;
  const AScope: TGocciaScope; out AMatches: Boolean): Boolean; {$IFDEF FPC}inline;{$ENDIF}
var
  ObjectConstructor, ArrayConstructor, StringConstructor, NumberConstructor,
    BooleanConstructor, FunctionConstructor, BigIntConstructor,
    SymbolConstructor: TGocciaValue;
begin
  Result := False;
  AMatches := False;

  ObjectConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_OBJECT);
  if Assigned(ObjectConstructor) and (AMatcher = ObjectConstructor) then
  begin
    AMatches := ASubject is TGocciaObjectValue;
    Exit(True);
  end;

  ArrayConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_ARRAY);
  if Assigned(ArrayConstructor) and (AMatcher = ArrayConstructor) then
  begin
    AMatches := ASubject is TGocciaArrayValue;
    Exit(True);
  end;

  StringConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_STRING);
  if Assigned(StringConstructor) and (AMatcher = StringConstructor) then
  begin
    AMatches := ASubject is TGocciaStringLiteralValue;
    Exit(True);
  end;

  NumberConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_NUMBER);
  if Assigned(NumberConstructor) and (AMatcher = NumberConstructor) then
  begin
    AMatches := ASubject is TGocciaNumberLiteralValue;
    Exit(True);
  end;

  BooleanConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_BOOLEAN);
  if Assigned(BooleanConstructor) and (AMatcher = BooleanConstructor) then
  begin
    AMatches := ASubject is TGocciaBooleanLiteralValue;
    Exit(True);
  end;

  FunctionConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_FUNCTION);
  if Assigned(FunctionConstructor) and (AMatcher = FunctionConstructor) then
  begin
    AMatches := ASubject.IsCallable;
    Exit(True);
  end;

  BigIntConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_BIGINT);
  if Assigned(BigIntConstructor) and (AMatcher = BigIntConstructor) then
  begin
    AMatches := ASubject is TGocciaBigIntValue;
    Exit(True);
  end;

  SymbolConstructor := VMGlobalConstructor(AScope, CONSTRUCTOR_SYMBOL);
  if Assigned(SymbolConstructor) and (AMatcher = SymbolConstructor) then
  begin
    AMatches := ASubject is TGocciaSymbolValue;
    Exit(True);
  end;
end;

function VMInstanceOfValue(const ALeft, ARight,
  AObjectConstructor, AFunctionConstructor: TGocciaValue): TGocciaValue; {$IFDEF FPC}inline;{$ENDIF}
begin
  // Keep bytecode in lockstep with interpreter semantics for ES2026
  // §13.10.2, including @@hasInstance and non-callable target errors.
  if InstanceofOperatorResult(ALeft, ARight) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

procedure ParseElementDescriptor(const ADescriptor: string;
  out AKind: Char; out AName: string; out AFlags: Integer);
var
  P1, P2: Integer;
begin
  P1 := Pos(':', ADescriptor);
  AKind := ADescriptor[1];
  P2 := Pos(':', ADescriptor, P1 + 1);
  AName := Copy(ADescriptor, P1 + 1, P2 - P1 - 1);
  AFlags := StrToIntDef(Copy(ADescriptor, P2 + 1,
    Length(ADescriptor) - P2), 0);
end;

type
  TGocciaResolvedEnvironmentReferenceValue = class(TGocciaValue)
  private
    FScope: TGocciaScope;
    FStrict: Boolean;
  public
    constructor Create(const AScope: TGocciaScope; const AStrict: Boolean);
    procedure MarkReferences; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    property Scope: TGocciaScope read FScope;
    property Strict: Boolean read FStrict;
  end;

  TGocciaBytecodeGeneratorObjectValue = class;

  TGocciaVMDecoratorSession = class
  public
    MetadataObject: TGocciaObjectValue;
    MethodCollector: TGocciaInitializerCollector;
    FieldCollector: TGocciaInitializerCollector;
    StaticFieldCollector: TGocciaInitializerCollector;
    ClassCollector: TGocciaInitializerCollector;
    ClassValue: TGocciaValue;
    OriginalClassValue: TGocciaValue;
    StaticDecoratorInitializersRun: Boolean;
    constructor Create(const AMetadataObject: TGocciaObjectValue);
    destructor Destroy; override;
    procedure MarkReferences;
  end;

  TGocciaBytecodeFunctionValue = class(TGocciaFunctionBase)
  private
    FClosure: TGocciaBytecodeClosure;
    FConstructClassValue: TGocciaValue;
    FVM: TGocciaVM;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
    function GetSourceText: string; override;
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure);
    destructor Destroy; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function ConstructWithReceiver(const AArguments: TGocciaArgumentsCollection;
      const AReceiver: TGocciaValue; const ANewTarget: TGocciaValue): TGocciaValue; override;
    function CallPreparedArgs(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallOneArg(const AArg0, AThisValue: TGocciaValue): TGocciaValue; override;
    function CallTwoArgs(const AArg0, AArg1, AThisValue: TGocciaValue): TGocciaValue; override;
    function CallThreeArgs(const AArg0, AArg1, AArg2, AThisValue: TGocciaValue): TGocciaValue; override;
    function IsConstructable: Boolean; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMSyncDisposeFallbackValue = class(TGocciaFunctionBase)
  private
    FDisposeMethod: TGocciaValue;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const ADisposeMethod: TGocciaValue);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMAsyncDisposeMethodValue = class(TGocciaFunctionBase)
  private
    FDisposeMethod: TGocciaValue;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const ADisposeMethod: TGocciaValue);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function CallNoArgs(const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMAsyncAwaitContinuationValue = class(TGocciaFunctionBase)
  private
    FVM: TGocciaVM;
    FContinuation: TGocciaBytecodeGeneratorObjectValue;
    FPromise: TGocciaPromiseValue;
    FResumeKind: TGocciaBytecodeGeneratorResumeKind;
    FAsyncGeneratorResult: Boolean;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AVM: TGocciaVM;
      const AContinuation: TGocciaBytecodeGeneratorObjectValue;
      const APromise: TGocciaPromiseValue;
      const AResumeKind: TGocciaBytecodeGeneratorResumeKind;
      const AAsyncGeneratorResult: Boolean = False);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMDynamicImportFulfillValue = class(TGocciaFunctionBase)
  private
    FPromise: TGocciaPromiseValue;
    FNamespace: TGocciaValue;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const APromise: TGocciaPromiseValue;
      const ANamespace: TGocciaValue);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMDynamicImportRejectValue = class(TGocciaFunctionBase)
  private
    FPromise: TGocciaPromiseValue;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const APromise: TGocciaPromiseValue);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  TGocciaVMDynamicImportStartValue = class(TGocciaFunctionBase)
  private
    FVM: TGocciaVM;
    FPromise: TGocciaPromiseValue;
    FPath: string;
    FReferrer: string;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AVM: TGocciaVM; const APromise: TGocciaPromiseValue;
      const APath, AReferrer: string);
    function Call(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure MarkReferences; override;
  end;

  EGocciaBytecodeYield = class(Exception)
  private
    FValue: TGocciaRegister;
    FYieldIndex: Integer;
  public
    constructor Create(const AValue: TGocciaRegister; const AYieldIndex: Integer);
    property Value: TGocciaRegister read FValue;
    property YieldIndex: Integer read FYieldIndex;
  end;

  EGocciaBytecodeAsyncSuspend = class(Exception);

  EGocciaBytecodeGeneratorReturn = class(Exception)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue);
    property Value: TGocciaValue read FValue;
  end;

  TGocciaBytecodeGeneratorObjectValue = class(TGocciaGeneratorBaseValue)
  private
    FVM: TGocciaVM;
    FClosure: TGocciaBytecodeClosure;
    FThisValue: TGocciaRegister;
    FArguments: TGocciaRegisterArray;
    FState: TGocciaBytecodeGeneratorState;
    FResumeKind: TGocciaBytecodeGeneratorResumeKind;
    FResumeValue: TGocciaRegister;
    FResumeRegister: UInt16;
    FReturnSentinel: TGocciaValue;
    FReturnValue: TGocciaValue;
    FReturnRequiresAwait: Boolean;
    FReturnResumeValueAwaited: Boolean;
    FHasContinuation: Boolean;
    FContinuationIP: Integer;
    FContinuationRegisters: TGocciaRegisterArray;
    FContinuationLocalCells: TGocciaBytecodeCellArray;
    FContinuationHandlers: TGocciaBytecodeHandlerEntryArray;
    FContinuationPrevCovLine: UInt32;
    FContinuationDynamicVarScope: TGocciaScope;
    FDelegateActive: Boolean;
    FDelegateIteratorValue: TGocciaValue;
    FDelegateIterator: TGocciaIteratorValue;
    FDelegateNextMethod: TGocciaValue;
    FIgnoreNextResume: Boolean;
    FLastResumeResultIsIteratorResult: Boolean;
    function ResumeRaw(const AKind: TGocciaBytecodeGeneratorResumeKind;
      const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
    function WrapResumeResult(const AValue: TGocciaValue;
      const ADone: Boolean): TGocciaObjectValue;
    procedure CaptureContinuation(const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AResumeRegister: UInt16; const AContinuationIP: Integer);
    procedure CaptureInitialContinuation(const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AContinuationIP: Integer);
    function RestoreContinuation(var AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; out APrevCovLine: UInt32): Boolean;
    procedure ClearDelegateState;
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection);
    constructor CreateRegisters(const AVM: TGocciaVM;
      const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
      const AArguments: TGocciaRegisterArray;
      const ARunParameterPreamble: Boolean = True);
    destructor Destroy; override;
    function AdvanceNext: TGocciaObjectValue; override;
    function AdvanceNextValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    function DirectNextValue(const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue; override;
    function ReturnValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    function ThrowValue(const AValue: TGocciaValue): TGocciaObjectValue; override;
    procedure Close; override;
    function GeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function GeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function GeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    procedure HandleYield(const AValue: TGocciaRegister;
      const AResumeRegister: UInt16; const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AContinuationIP: Integer);
    procedure HandleYieldDelegate(const AIterable: TGocciaRegister;
      const AResumeRegister: UInt16; const AFrame: TGocciaVMCallFrame;
      const AHandlerBaseCount: Integer; const APrevCovLine: UInt32;
      const AContinuationIP: Integer);
    procedure MarkReferences; override;
    function ToStringTag: string; override;
    function BuiltinTagFallback: Boolean; override;
  end;

  TGocciaBytecodeAsyncGeneratorObjectValue = class(TGocciaAsyncGeneratorBaseValue)
  private
    FInner: TGocciaBytecodeGeneratorObjectValue;
    FQueue: array of TGocciaBytecodeAsyncGeneratorRequest;
    FQueueHead: Integer;
    FQueueCount: Integer;
    FQueueRunning: Boolean;
    function ContinueQueue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function DequeueRequest(
      out ARequest: TGocciaBytecodeAsyncGeneratorRequest): Boolean;
    procedure EnqueueRequest(
      const ARequest: TGocciaBytecodeAsyncGeneratorRequest);
    procedure FinishRequest;
    procedure ContinueWhenPromiseSettles(const APromise: TGocciaPromiseValue);
    procedure AwaitYieldValue(const ARequestPromise: TGocciaPromiseValue;
      const AValue: TGocciaValue);
    procedure AwaitReturnValue(const ARequestPromise: TGocciaPromiseValue;
      const AValue: TGocciaValue);
    procedure ResolveAwaitedYield(const ARequestPromise: TGocciaPromiseValue;
      const AValue: TGocciaValue);
    procedure RejectAwaitedYield(const ARequestPromise: TGocciaPromiseValue;
      const AReason: TGocciaValue);
    procedure ResolveAwaitedReturn(const ARequestPromise: TGocciaPromiseValue;
      const AValue: TGocciaValue);
    procedure RejectAwaitedReturn(const ARequestPromise: TGocciaPromiseValue;
      const AReason: TGocciaValue);
    procedure ProcessQueue;
    function ResumeAsPromise(const AKind: TGocciaBytecodeGeneratorResumeKind;
      const AValue: TGocciaValue): TGocciaValue;
    procedure StartRequest(const ARequest: TGocciaBytecodeAsyncGeneratorRequest);
  public
    constructor Create(const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure;
      const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection);
    constructor CreateRegisters(const AVM: TGocciaVM;
      const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
      const AArguments: TGocciaRegisterArray);
    function AsyncGeneratorNext(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncGeneratorReturn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncGeneratorThrow(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue; override;
    function AsyncIteratorSelf(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
    function ToStringTag: string; override;
    function BuiltinTagFallback: Boolean; override;
  end;

  TGocciaVMAsyncGeneratorYieldAwaitHandler = class(TGocciaObjectValue)
  private
    FGenerator: TGocciaBytecodeAsyncGeneratorObjectValue;
    FRequestPromise: TGocciaPromiseValue;
    FReject: Boolean;
    FReturn: Boolean;
  public
    constructor Create(const AGenerator: TGocciaBytecodeAsyncGeneratorObjectValue;
      const ARequestPromise: TGocciaPromiseValue; const AReject: Boolean;
      const AReturn: Boolean = False);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  TGocciaVMClassValue = class(TGocciaClassValue)
  private
    FVM: TGocciaVM;
    FConstructorValue: TGocciaValue;
    FNativeInstanceNewTarget: TGocciaValue;
    function CreateNativeInstanceWithNewTarget(
      const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaObjectValue;
  public
    constructor Create(const AVM: TGocciaVM; const AName: string;
      const ASuperClass: TGocciaClassValue);
    function CreateNativeInstance(
      const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function GetClassLength: Integer; override;
    function Instantiate(const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue = nil): TGocciaValue; override;
    function InstantiateRegisters(
      const AArguments: TGocciaRegisterArray): TGocciaRegister;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    procedure SetVMConstructor(const AValue: TGocciaValue);
    procedure MarkReferences; override;
  end;

constructor TGocciaResolvedEnvironmentReferenceValue.Create(
  const AScope: TGocciaScope; const AStrict: Boolean);
begin
  inherited Create;
  FScope := AScope;
  FStrict := AStrict;
end;

procedure TGocciaResolvedEnvironmentReferenceValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FScope) then
    FScope.MarkReferences;
end;

function TGocciaResolvedEnvironmentReferenceValue.TypeName: string;
begin
  Result := '<environment-reference>';
end;

function TGocciaResolvedEnvironmentReferenceValue.TypeOf: string;
begin
  Result := '<environment-reference>';
end;

function TGocciaResolvedEnvironmentReferenceValue.ToBooleanLiteral:
  TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaResolvedEnvironmentReferenceValue.ToNumberLiteral:
  TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaResolvedEnvironmentReferenceValue.ToStringLiteral:
  TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('<environment-reference>');
end;

{ TGocciaVMStackRoot }

constructor TGocciaVMStackRoot.Create(const AVM: TGocciaVM);
begin
  FVM := AVM;
  inherited Create;
  GCIndex := -1;
  if (TGarbageCollector.Instance <> nil) then
    TGarbageCollector.Instance.RegisterObject(Self);
end;

procedure TGocciaVMStackRoot.MarkClosureReferences(
  const AClosure: TGocciaBytecodeClosure);
var
  I: Integer;
  Upvalue: TGocciaBytecodeUpvalue;
begin
  if not Assigned(AClosure) then
    Exit;
  if Assigned(AClosure.HomeObject) then
    AClosure.HomeObject.MarkReferences;
  if Assigned(AClosure.HomeClass) then
    AClosure.HomeClass.MarkReferences;
  if Assigned(AClosure.NewTarget) then
    AClosure.NewTarget.MarkReferences;
  if Assigned(AClosure.GlobalScope) then
    AClosure.GlobalScope.MarkReferences;
  if Assigned(AClosure.DynamicVarScope) then
    AClosure.DynamicVarScope.MarkReferences;
  for I := 0 to AClosure.UpvalueCount - 1 do
  begin
    Upvalue := AClosure.GetUpvalue(I);
    if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
      MarkRegisterReferences(Upvalue.Cell.Value);
  end;
end;

procedure TGocciaVMStackRoot.MarkReferences;
var
  I, Limit: Integer;
  ExportPair: TGocciaValueMap.TKeyValuePair;

  procedure MarkRegisterRange(const ABase, ACount: Integer);
  var
    J, RangeLimit: Integer;
  begin
    if (ABase < 0) or (ACount <= 0) or (ABase >= Length(FVM.FRegisterStack)) then
      Exit;
    RangeLimit := ABase + ACount;
    if RangeLimit > Length(FVM.FRegisterStack) then
      RangeLimit := Length(FVM.FRegisterStack);
    for J := ABase to RangeLimit - 1 do
      MarkRegisterReferences(FVM.FRegisterStack[J]);
  end;

  procedure MarkLocalCellRange(const ABase, ACount: Integer);
  var
    J, RangeLimit: Integer;
  begin
    if (ABase < 0) or (ACount <= 0) or (ABase >= Length(FVM.FLocalCellStack)) then
      Exit;
    RangeLimit := ABase + ACount;
    if RangeLimit > Length(FVM.FLocalCellStack) then
      RangeLimit := Length(FVM.FLocalCellStack);
    for J := ABase to RangeLimit - 1 do
      if Assigned(FVM.FLocalCellStack[J]) then
        MarkRegisterReferences(FVM.FLocalCellStack[J].Value);
  end;

  procedure MarkArgumentRange(const ABase, ACount: Integer);
  var
    J, RangeLimit: Integer;
  begin
    if (ABase < 0) or (ACount <= 0) or (ABase >= Length(FVM.FArgumentStack)) then
      Exit;
    RangeLimit := ABase + ACount;
    if RangeLimit > Length(FVM.FArgumentStack) then
      RangeLimit := Length(FVM.FArgumentStack);
    for J := ABase to RangeLimit - 1 do
      MarkRegisterReferences(FVM.FArgumentStack[J]);
  end;

begin
  if GCMarked then Exit;
  inherited;
  if not Assigned(FVM) then
    Exit;

  if Assigned(FVM.FGlobalScope) then
    FVM.FGlobalScope.MarkReferences;
  if Assigned(FVM.FGlobalThisValue) then
    FVM.FGlobalThisValue.MarkReferences;
  MarkClosureReferences(FVM.FCurrentClosure);
  MarkRegisterReferences(FVM.FLastClosureThisValue);
  if Assigned(FVM.FPrivateInitializerReceiver) then
    FVM.FPrivateInitializerReceiver.MarkReferences;
  if Assigned(FVM.FPendingNewTarget) then
    FVM.FPendingNewTarget.MarkReferences;
  if Assigned(FVM.FCurrentNewTarget) then
    FVM.FCurrentNewTarget.MarkReferences;
  if Assigned(FVM.FCurrentDynamicVarScope) then
    FVM.FCurrentDynamicVarScope.MarkReferences;
  if Assigned(FVM.FCurrentAsyncPromise) then
    FVM.FCurrentAsyncPromise.MarkReferences;
  // The argument arena is stack-disciplined like the register/cell stacks, so
  // marking [0, current top) covers the current frame plus every suspended and
  // native-re-entry ancestor window. The per-frame and per-saved-root ranges
  // below are redundant with this but kept for parity with the register stack.
  MarkArgumentRange(0, FVM.FArgumentBase + FVM.FArgCount);

  Limit := FVM.FRegisterBase + FVM.FRegisterCount;
  if Limit > Length(FVM.FRegisterStack) then
    Limit := Length(FVM.FRegisterStack);
  for I := 0 to Limit - 1 do
    MarkRegisterReferences(FVM.FRegisterStack[I]);

  Limit := FVM.FLocalCellBase + FVM.FLocalCellCount;
  if Limit > Length(FVM.FLocalCellStack) then
    Limit := Length(FVM.FLocalCellStack);
  for I := 0 to Limit - 1 do
    if Assigned(FVM.FLocalCellStack[I]) then
      MarkRegisterReferences(FVM.FLocalCellStack[I].Value);

  for I := 0 to FVM.FFrameStackCount - 1 do
  begin
    MarkClosureReferences(FVM.FFrameStack[I].Closure);
    MarkArgumentRange(FVM.FFrameStack[I].ArgumentBase,
      FVM.FFrameStack[I].ArgCount);
    MarkRegisterRange(FVM.FFrameStack[I].RegisterBase,
      FVM.FFrameStack[I].RegisterCount);
    MarkLocalCellRange(FVM.FFrameStack[I].LocalCellBase,
      FVM.FFrameStack[I].LocalCellCount);
    if Assigned(FVM.FFrameStack[I].NewTarget) then
      TGocciaValue(FVM.FFrameStack[I].NewTarget).MarkReferences;
    if Assigned(FVM.FFrameStack[I].GlobalScope) then
      TGocciaScope(FVM.FFrameStack[I].GlobalScope).MarkReferences;
    if Assigned(FVM.FFrameStack[I].DynamicVarScope) then
      TGocciaScope(FVM.FFrameStack[I].DynamicVarScope).MarkReferences;
  end;

  for I := 0 to FVM.FTempSavedStateRootCount - 1 do
  begin
    MarkClosureReferences(FVM.FTempSavedStateRoots[I].Closure);
    if Assigned(FVM.FTempSavedStateRoots[I].NewTarget) then
      FVM.FTempSavedStateRoots[I].NewTarget.MarkReferences;
    MarkArgumentRange(FVM.FTempSavedStateRoots[I].ArgumentBase,
      FVM.FTempSavedStateRoots[I].ArgCount);
  end;

  if Assigned(FVM.FActiveDecoratorSession) then
    TGocciaVMDecoratorSession(FVM.FActiveDecoratorSession).MarkReferences;

  if Assigned(FVM.FCurrentModuleExports) then
    for ExportPair in FVM.FCurrentModuleExports do
      if Assigned(ExportPair.Value) then
        ExportPair.Value.MarkReferences;
end;

constructor TGocciaBytecodeFunctionValue.Create(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure);
var
  Kind: TGocciaFunctionObjectIntrinsicKind;
begin
  inherited Create;
  FVM := AVM;
  FClosure := AClosure;
  FConstructClassValue := nil;
  if Assigned(FClosure) then
    FClosure.FunctionValue := Self;
  if Assigned(AClosure) and Assigned(AClosure.Template) then
  begin
    FStrictThis := AClosure.Template.StrictThis;
    FStrictCode := AClosure.Template.StrictCode;
    Kind := BytecodeFunctionIntrinsicKind(AClosure.Template);
    Prototype := VMFunctionObjectPrototype(Kind);
    if (not FStrictCode) and AClosure.Template.HasOwnPrototype and
       (not AClosure.Template.IsArrow) and
       (not AClosure.Template.IsAsync) and
       (not AClosure.Template.IsGenerator) then
      InstallSloppyFunctionCallerArgumentsProperties;
  end;
end;

constructor TGocciaVMSyncDisposeFallbackValue.Create(
  const ADisposeMethod: TGocciaValue);
begin
  inherited Create;
  FDisposeMethod := ADisposeMethod;
end;

function TGocciaVMSyncDisposeFallbackValue.GetFunctionLength: Integer;
begin
  Result := 0;
end;

function TGocciaVMSyncDisposeFallbackValue.GetFunctionName: string;
begin
  Result := '[Symbol.dispose]';
end;

function TGocciaVMSyncDisposeFallbackValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(FDisposeMethod) and FDisposeMethod.IsCallable then
    InvokeCallable(FDisposeMethod, AArguments, AThisValue);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVMSyncDisposeFallbackValue.CallNoArgs(
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
begin
  if Assigned(FDisposeMethod) and FDisposeMethod.IsCallable then
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      InvokeCallable(FDisposeMethod, CallArgs, AThisValue);
    finally
      CallArgs.Free;
    end;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaVMSyncDisposeFallbackValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FDisposeMethod) then
    FDisposeMethod.MarkReferences;
end;

constructor TGocciaVMAsyncDisposeMethodValue.Create(
  const ADisposeMethod: TGocciaValue);
begin
  inherited Create;
  FDisposeMethod := ADisposeMethod;
end;

function TGocciaVMAsyncDisposeMethodValue.GetFunctionLength: Integer;
begin
  Result := 0;
end;

function TGocciaVMAsyncDisposeMethodValue.GetFunctionName: string;
begin
  Result := '[Symbol.asyncDispose]';
end;

function TGocciaVMAsyncDisposeMethodValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  DisposeResult: TGocciaValue;
begin
  if Assigned(FDisposeMethod) and FDisposeMethod.IsCallable then
  begin
    DisposeResult := InvokeCallable(FDisposeMethod, AArguments, AThisValue);
    if Assigned(DisposeResult) then
      AwaitValue(DisposeResult);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVMAsyncDisposeMethodValue.CallNoArgs(
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  DisposeResult: TGocciaValue;
begin
  if Assigned(FDisposeMethod) and FDisposeMethod.IsCallable then
  begin
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      DisposeResult := InvokeCallable(FDisposeMethod, CallArgs, AThisValue);
    finally
      CallArgs.Free;
    end;
    if Assigned(DisposeResult) then
      AwaitValue(DisposeResult);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaVMAsyncDisposeMethodValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FDisposeMethod) then
    FDisposeMethod.MarkReferences;
end;

function TGocciaVMLiteralObjectValue.TryGetOwnDataPropertyFast(
  const AName: string; out AValue: TGocciaValue): Boolean;
begin
  Result := VMGetOwnDataDescriptorValue(Self, AName, AValue);
end;

function TGocciaVMLiteralObjectValue.TryGetOwnDataPropertyFastRegister(
  const AName: string; out AValue: TGocciaRegister): Boolean;
begin
  Result := VMGetOwnDataDescriptorRegister(Self, AName, AValue);
end;

constructor TGocciaVMLiteralObjectValue.Create(const APrototype: TGocciaObjectValue;
  const APropertyCapacity: Integer);
begin
  inherited Create(APrototype, APropertyCapacity);
  FFastLiteralMode := True;
end;

function TGocciaVMLiteralObjectValue.TrySetLiteralDataPropertyFast(
  const AName: string; const AValue: TGocciaValue): Boolean;
var
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  ChainDepth: Integer;
begin
  if not FFastLiteralMode then
    Exit(False);

  if VMSetOwnWritableDataDescriptorValue(Self, AName, AValue) then
    Exit(True);

  Current := FPrototype;
  ChainDepth := 0;
  while Assigned(Current) do
  begin
    Inc(ChainDepth);
    if ChainDepth > FOR_IN_MAX_PROTOTYPE_CHAIN_DEPTH then
      Exit(False);
    Descriptor := Current.GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
      Exit(False);
    Current := Current.Prototype;
  end;

  inherited DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
  Result := True;
end;

function TGocciaVMLiteralObjectValue.GetProperty(const AName: string): TGocciaValue;
begin
  if not TryGetOwnDataPropertyFast(AName, Result) then
    Result := inherited GetProperty(AName);
end;

procedure TGocciaVMLiteralObjectValue.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  FFastLiteralMode := False;
  inherited DefineProperty(AName, ADescriptor);
end;

function TGocciaVMLiteralObjectValue.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  FFastLiteralMode := False;
  Result := inherited TryDefineProperty(AName, ADescriptor);
end;

procedure TGocciaVMLiteralObjectValue.Freeze;
begin
  FFastLiteralMode := False;
  inherited Freeze;
end;

procedure TGocciaVMLiteralObjectValue.Seal;
begin
  FFastLiteralMode := False;
  inherited Seal;
end;

procedure TGocciaVMLiteralObjectValue.PreventExtensions;
begin
  FFastLiteralMode := False;
  inherited PreventExtensions;
end;

constructor TGocciaVMClassValue.Create(const AVM: TGocciaVM; const AName: string;
  const ASuperClass: TGocciaClassValue);
begin
  inherited Create(AName, ASuperClass);
  FVM := AVM;
  FConstructorValue := nil;
  FNativeInstanceNewTarget := nil;
end;

function TGocciaVMClassValue.GetClassLength: Integer;
var
  Bytecode: TGocciaBytecodeFunctionValue;
begin
  // VM-compiled classes hold their constructor as a bytecode function value
  // rather than the interpreter's TGocciaMethodValue, so the inherited
  // implementation (which only looks at FConstructorMethod) would always
  // return 0. Bridge to the bytecode function's own length.
  if FConstructorValue is TGocciaBytecodeFunctionValue then
  begin
    Bytecode := TGocciaBytecodeFunctionValue(FConstructorValue);
    if Assigned(Bytecode.FClosure) and Assigned(Bytecode.FClosure.Template) then
      Exit(Bytecode.FClosure.Template.FormalParameterCount);
  end;
  Result := inherited GetClassLength;
end;

function TGocciaVMClassValue.CreateNativeInstance(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  ConstructedValue: TGocciaValue;
  EffectiveNewTarget: TGocciaValue;
begin
  Result := inherited CreateNativeInstance(AArguments);
  if Assigned(Result) or not Assigned(NativeSuperConstructor) then
    Exit;

  if NativeSuperConstructor = TGocciaFunctionBase.GetSharedPrototype then
    Exit(nil);

  if Assigned(FNativeInstanceNewTarget) then
    EffectiveNewTarget := FNativeInstanceNewTarget
  else
    EffectiveNewTarget := Self;
  ConstructedValue := FVM.ConstructValue(NativeSuperConstructor, AArguments,
    EffectiveNewTarget);
  if ConstructedValue is TGocciaObjectValue then
    Result := TGocciaObjectValue(ConstructedValue)
  else
  begin
    ThrowTypeError('Superclass constructor did not return an object',
      SSuggestNotConstructorType);
    Result := nil;
  end;
end;

function TGocciaVMClassValue.CreateNativeInstanceWithNewTarget(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaObjectValue;
var
  PreviousNewTarget: TGocciaValue;
begin
  PreviousNewTarget := FNativeInstanceNewTarget;
  FNativeInstanceNewTarget := ANewTarget;
  try
    Result := CreateNativeInstance(AArguments);
  finally
    FNativeInstanceNewTarget := PreviousNewTarget;
  end;
end;

constructor TGocciaVMSuperConstructorValue.Create(
  const ASuperClass, ANewTarget: TGocciaValue;
  const ACurrentCtorClass: TGocciaClassValue);
begin
  inherited Create;
  FSuperClass := ASuperClass;
  FNewTarget := ANewTarget;
  FCurrentCtorClass := ACurrentCtorClass;
end;

function TGocciaBytecodeFunctionValue.GetFunctionLength: Integer;
begin
  Result := FClosure.Template.FormalParameterCount;
end;

constructor TGocciaVMDecoratorSession.Create(
  const AMetadataObject: TGocciaObjectValue);
begin
  inherited Create;
  MetadataObject := AMetadataObject;
  MethodCollector := TGocciaInitializerCollector.Create;
  FieldCollector := TGocciaInitializerCollector.Create;
  StaticFieldCollector := TGocciaInitializerCollector.Create;
  ClassCollector := TGocciaInitializerCollector.Create;
  ClassValue := nil;
  OriginalClassValue := nil;
  StaticDecoratorInitializersRun := False;
end;

destructor TGocciaVMDecoratorSession.Destroy;
begin
  MethodCollector.Free;
  FieldCollector.Free;
  StaticFieldCollector.Free;
  ClassCollector.Free;
  inherited;
end;

procedure TGocciaVMDecoratorSession.MarkReferences;
  procedure MarkCollector(const ACollector: TGocciaInitializerCollector);
  var
    Initializers: TArray<TGocciaValue>;
    Initializer: TGocciaValue;
  begin
    if not Assigned(ACollector) then
      Exit;
    Initializers := ACollector.GetInitializers;
    for Initializer in Initializers do
      if Assigned(Initializer) then
        Initializer.MarkReferences;
  end;

begin
  if Assigned(MetadataObject) then
    MetadataObject.MarkReferences;
  if Assigned(ClassValue) then
    ClassValue.MarkReferences;
  if Assigned(OriginalClassValue) and (OriginalClassValue <> ClassValue) then
    OriginalClassValue.MarkReferences;
  MarkCollector(MethodCollector);
  MarkCollector(FieldCollector);
  MarkCollector(StaticFieldCollector);
  MarkCollector(ClassCollector);
end;

procedure RunStaticDecoratorInitializersForSession(
  const ASession: TGocciaVMDecoratorSession);
begin
  if not Assigned(ASession) or ASession.StaticDecoratorInitializersRun then
    Exit;

  if ASession.OriginalClassValue is TGocciaClassValue then
    TGocciaClassValue(ASession.OriginalClassValue)
      .RunDecoratorStaticFieldInitializers;
  ASession.StaticDecoratorInitializersRun := True;
end;

threadvar
  // Non-owning "active generator" pointer: GC-managed and save/restored around
  // each generator activation, so it is not a thread-exit leak (object-reference
  // threadvar audit, #892).
  GActiveBytecodeGenerator: TGocciaBytecodeGeneratorObjectValue;

type
  TGocciaVMAsyncIteratorRecordValue = class(TGocciaObjectValue)
  private
    FIteratorValue: TGocciaValue;
    FNextMethod: TGocciaValue;
    FLastReturnMethodMissing: Boolean;
    function Next(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ReturnValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function ThrowValue(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AIteratorValue, ANextMethod: TGocciaValue);
    function GetUnderlyingReturnMethod(out AReturnMethod: TGocciaValue): Boolean;
    property LastReturnMethodMissing: Boolean read FLastReturnMethodMissing;
    procedure MarkReferences; override;
  end;

  TGocciaVMAsyncFromSyncIteratorValue = class(TGocciaObjectValue)
  private
    FVM: TGocciaVM;
    FIteratorValue: TGocciaValue;
    FNextMethod: TGocciaValue;
    function PromiseResolve(const AValue: TGocciaValue): TGocciaValue;
    function PromiseReject(const AValue: TGocciaValue): TGocciaValue;
    procedure ClearIteratorState;
    procedure CloseIteratorAfterRejectedValue;
    function AsyncFromSyncIteratorContinuation(const AValue: TGocciaValue;
      const ADone, ACloseOnRejection: Boolean): TGocciaValue;
    function AwaitIteratorValue(const AValue: TGocciaValue;
      const ADone, ACloseOnRejection: Boolean): TGocciaValue;
    function Next(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ReturnValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ThrowValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AVM: TGocciaVM; const AIteratorValue,
      ANextMethod: TGocciaValue);
    procedure MarkReferences; override;
  end;

  TGocciaVMAsyncFromSyncFulfillValue = class(TGocciaObjectValue)
  private
    FDone: Boolean;
  public
    constructor Create(const ADone: Boolean);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaVMAsyncFromSyncRejectValue = class(TGocciaObjectValue)
  private
    FIterator: TGocciaVMAsyncFromSyncIteratorValue;
  public
    constructor Create(const AIterator: TGocciaVMAsyncFromSyncIteratorValue);
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

constructor TGocciaVMAsyncIteratorRecordValue.Create(
  const AIteratorValue, ANextMethod: TGocciaValue);
begin
  inherited Create;
  FIteratorValue := AIteratorValue;
  FNextMethod := ANextMethod;
  AssignProperty(PROP_NEXT, TGocciaNativeFunctionValue.Create(Next, PROP_NEXT, 1));
  AssignProperty(PROP_RETURN,
    TGocciaNativeFunctionValue.Create(ReturnValue, PROP_RETURN, 1));
  AssignProperty(PROP_THROW,
    TGocciaNativeFunctionValue.Create(ThrowValue, PROP_THROW, 1));
end;

function TGocciaVMAsyncIteratorRecordValue.Next(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not Assigned(FNextMethod) or
     (FNextMethod is TGocciaUndefinedLiteralValue) or
     not FNextMethod.IsCallable then
    ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
      SSuggestAsyncIteratorProtocol);
  Result := InvokeCallable(FNextMethod, AArgs, FIteratorValue);
end;

function TGocciaVMAsyncIteratorRecordValue.ReturnValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ReturnMethod: TGocciaValue;
  Value: TGocciaValue;
begin
  FLastReturnMethodMissing := False;
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FIteratorValue) then
  begin
    FLastReturnMethodMissing := True;
    Exit(CreateIteratorResult(Value, True));
  end;

  ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
  if not Assigned(ReturnMethod) or
     (ReturnMethod is TGocciaUndefinedLiteralValue) or
     (ReturnMethod is TGocciaNullLiteralValue) then
  begin
    FLastReturnMethodMissing := True;
    Exit(CreateIteratorResult(Value, True));
  end;
  if not ReturnMethod.IsCallable then
    ThrowTypeError('Iterator return is not callable');

  CallArgs := TGocciaArgumentsCollection.Create([Value]);
  try
    Result := InvokeCallable(ReturnMethod, CallArgs, FIteratorValue);
  finally
    CallArgs.Free;
  end;
end;

function TGocciaVMAsyncIteratorRecordValue.GetUnderlyingReturnMethod(
  out AReturnMethod: TGocciaValue): Boolean;
begin
  AReturnMethod := nil;
  FLastReturnMethodMissing := False;
  if not Assigned(FIteratorValue) then
  begin
    FLastReturnMethodMissing := True;
    Exit(False);
  end;

  AReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
  if not Assigned(AReturnMethod) or
     (AReturnMethod is TGocciaUndefinedLiteralValue) or
     (AReturnMethod is TGocciaNullLiteralValue) then
  begin
    FLastReturnMethodMissing := True;
    Exit(False);
  end;
  Result := True;
end;

function TGocciaVMAsyncIteratorRecordValue.ThrowValue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ReturnResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  ThrowMethod: TGocciaValue;
  Value: TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FIteratorValue) then
    raise TGocciaThrowValue.Create(Value);

  ThrowMethod := FIteratorValue.GetProperty(PROP_THROW);
  if Assigned(ThrowMethod) and
     not (ThrowMethod is TGocciaUndefinedLiteralValue) and
     not (ThrowMethod is TGocciaNullLiteralValue) then
  begin
    if not ThrowMethod.IsCallable then
      ThrowTypeError('Iterator throw is not callable');
    CallArgs := TGocciaArgumentsCollection.Create([Value]);
    try
      Exit(InvokeCallable(ThrowMethod, CallArgs, FIteratorValue));
    finally
      CallArgs.Free;
    end;
  end;

  ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
  if Assigned(ReturnMethod) and
     not (ReturnMethod is TGocciaUndefinedLiteralValue) and
     not (ReturnMethod is TGocciaNullLiteralValue) then
  begin
    if not ReturnMethod.IsCallable then
      ThrowTypeError('Iterator return is not callable');
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      ReturnResult := AwaitValue(InvokeCallable(ReturnMethod, CallArgs,
        FIteratorValue));
    finally
      CallArgs.Free;
    end;
    if not (ReturnResult is TGocciaObjectValue) then
      ThrowTypeError('Iterator return result is not an object');
  end;

  ThrowTypeError('Iterator throw is not callable');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaVMAsyncIteratorRecordValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FIteratorValue) then
    FIteratorValue.MarkReferences;
  if Assigned(FNextMethod) then
    FNextMethod.MarkReferences;
end;

constructor TGocciaVMAsyncFromSyncIteratorValue.Create(const AVM: TGocciaVM;
  const AIteratorValue, ANextMethod: TGocciaValue);
begin
  inherited Create;
  FVM := AVM;
  FIteratorValue := AIteratorValue;
  FNextMethod := ANextMethod;
  AssignProperty(PROP_NEXT, TGocciaNativeFunctionValue.Create(Next, PROP_NEXT, 1));
  AssignProperty(PROP_RETURN, TGocciaNativeFunctionValue.Create(ReturnValue, PROP_RETURN, 1));
  AssignProperty(PROP_THROW, TGocciaNativeFunctionValue.Create(ThrowValue, PROP_THROW, 1));
end;

procedure TGocciaVMAsyncFromSyncIteratorValue.ClearIteratorState;
begin
  FIteratorValue := nil;
  FNextMethod := nil;
end;

procedure TGocciaVMAsyncFromSyncIteratorValue.CloseIteratorAfterRejectedValue;
var
  CallArgs: TGocciaArgumentsCollection;
  ReturnMethod: TGocciaValue;
begin
  if not Assigned(FIteratorValue) then
    Exit;
  try
    if FIteratorValue is TGocciaIteratorValue then
      TGocciaIteratorValue(FIteratorValue).Close
    else if FIteratorValue is TGocciaObjectValue then
    begin
      ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
      if Assigned(ReturnMethod) and
         not (ReturnMethod is TGocciaUndefinedLiteralValue) and
         not (ReturnMethod is TGocciaNullLiteralValue) and
         ReturnMethod.IsCallable then
      begin
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          InvokeCallable(ReturnMethod, CallArgs, FIteratorValue);
        finally
          CallArgs.Free;
        end;
      end;
    end;
  except
    // AsyncFromSyncIteratorContinuation preserves the rejected value when
    // closing after a rejected wrapped value also fails.
  end;
  ClearIteratorState;
end;

function TGocciaVMAsyncFromSyncIteratorValue.AsyncFromSyncIteratorContinuation(
  const AValue: TGocciaValue; const ADone,
  ACloseOnRejection: Boolean): TGocciaValue;
var
  FulfillHandler: TGocciaVMAsyncFromSyncFulfillValue;
  FulfillFunction: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaVMAsyncFromSyncRejectValue;
  RejectFunction: TGocciaNativeFunctionValue;
  ValueWrapper: TGocciaPromiseValue;
begin
  try
    ValueWrapper := TGocciaPromiseValue(PromiseResolve(AValue));
  except
    // ES2026 §27.1.5.4 step 6: an abrupt PromiseResolve closes a
    // non-complete sync iterator when this continuation owns rejection
    // cleanup.  The caller converts the preserved exception into rejection.
    if ACloseOnRejection and not ADone then
      CloseIteratorAfterRejectedValue;
    raise;
  end;

  FulfillHandler := TGocciaVMAsyncFromSyncFulfillValue.Create(ADone);
  FulfillFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillHandler.Invoke, 'async-from-sync-fulfill', 1);
  FulfillFunction.CapturedRoot := FulfillHandler;

  RejectFunction := nil;
  if ACloseOnRejection and not ADone then
  begin
    RejectHandler := TGocciaVMAsyncFromSyncRejectValue.Create(Self);
    RejectFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
      RejectHandler.Invoke, 'async-from-sync-reject', 1);
    RejectFunction.CapturedRoot := RejectHandler;
  end;

  Result := ValueWrapper.InvokeThen(FulfillFunction, RejectFunction);
end;

function TGocciaVMAsyncFromSyncIteratorValue.AwaitIteratorValue(
  const AValue: TGocciaValue; const ADone, ACloseOnRejection: Boolean): TGocciaValue;
begin
  try
    Result := AwaitValue(AValue);
  except
    if ACloseOnRejection and not ADone then
      CloseIteratorAfterRejectedValue;
    raise;
  end;
end;

constructor TGocciaVMAsyncFromSyncFulfillValue.Create(const ADone: Boolean);
begin
  inherited Create;
  FDone := ADone;
end;

function TGocciaVMAsyncFromSyncFulfillValue.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  Result := CreateIteratorResult(Value, FDone);
end;

constructor TGocciaVMAsyncFromSyncRejectValue.Create(
  const AIterator: TGocciaVMAsyncFromSyncIteratorValue);
begin
  inherited Create;
  FIterator := AIterator;
end;

function TGocciaVMAsyncFromSyncRejectValue.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Reason: TGocciaValue;
begin
  if Assigned(FIterator) then
    FIterator.CloseIteratorAfterRejectedValue;
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Reason := AArgs.GetElement(0)
  else
    Reason := TGocciaUndefinedLiteralValue.UndefinedValue;
  raise TGocciaThrowValue.Create(Reason);
end;

procedure TGocciaVMAsyncFromSyncRejectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FIterator) then
    FIterator.MarkReferences;
end;

function TGocciaVMAsyncFromSyncIteratorValue.PromiseResolve(
  const AValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(FVM) then
    Result := FVM.PromiseResolveIntrinsic(AValue)
  else
    Result := AValue;
end;

function TGocciaVMAsyncFromSyncIteratorValue.PromiseReject(
  const AValue: TGocciaValue): TGocciaValue;
var
  IsRooted: Boolean;
  Promise: TGocciaPromiseValue;
begin
  Promise := TGocciaPromiseValue.Create;
  IsRooted := (TGarbageCollector.Instance <> nil);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Promise);
  try
    Promise.Reject(AValue);
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Promise);
      IsRooted := False;
    end;
    Promise.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Promise);
  Result := Promise;
end;

function TGocciaVMAsyncFromSyncIteratorValue.Next(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaValue;
  Value: TGocciaValue;
begin
  try
    if not Assigned(FIteratorValue) then
      Exit(PromiseResolve(CreateIteratorResult(
        TGocciaUndefinedLiteralValue.UndefinedValue, True)));

    if FIteratorValue is TGocciaIteratorValue then
    begin
      if AArgs.Length > 0 then
        Value := TGocciaIteratorValue(FIteratorValue).DirectNextValue(
          AArgs.GetElement(0), Done)
      else
        Value := TGocciaIteratorValue(FIteratorValue).DirectNext(Done);
      if Done then
        ClearIteratorState;
      Exit(AsyncFromSyncIteratorContinuation(Value, Done, True));
    end;

    if AArgs.Length > 0 then
      CallArgs := TGocciaArgumentsCollection.Create([AArgs.GetElement(0)])
    else
      CallArgs := TGocciaArgumentsCollection.Create;
    try
      IteratorResult := InvokeCallable(FNextMethod, CallArgs, FIteratorValue);
    finally
      CallArgs.Free;
    end;

    if not (IteratorResult is TGocciaObjectValue) then
      Exit(PromiseReject(CreateErrorObject(TYPE_ERROR_NAME,
        Format(SErrorIteratorResultNotObject, [IteratorResult.TypeName]))));

    DoneValue := IteratorResult.GetProperty(PROP_DONE);
    Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
    Value := IteratorResult.GetProperty(PROP_VALUE);
    if not Assigned(Value) then
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    if Done then
      ClearIteratorState;
    Result := AsyncFromSyncIteratorContinuation(Value, Done, True);
  except
    on E: EGocciaBytecodeThrow do
      Result := PromiseReject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaRuntimeError do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: EGocciaCapabilityAuditDeliveryError do
      raise;
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

function TGocciaVMAsyncFromSyncIteratorValue.ReturnValue(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  Value: TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  try
    if not Assigned(FIteratorValue) then
      Exit(AsyncFromSyncIteratorContinuation(Value, True, False));

    if FIteratorValue is TGocciaIteratorValue then
    begin
      if Assigned(AArgs) and (AArgs.Length > 0) then
        IteratorResult := TGocciaIteratorValue(FIteratorValue).ReturnValue(Value)
      else
        IteratorResult := TGocciaIteratorValue(FIteratorValue)
          .ReturnValueWithoutValue;
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Done then
        ClearIteratorState;
      Exit(AsyncFromSyncIteratorContinuation(Value, Done, False));
    end;

    ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
    if not Assigned(ReturnMethod) or
       (ReturnMethod is TGocciaUndefinedLiteralValue) or
       (ReturnMethod is TGocciaNullLiteralValue) then
    begin
      ClearIteratorState;
      Exit(AsyncFromSyncIteratorContinuation(Value, True, False));
    end;
    if not ReturnMethod.IsCallable then
      ThrowTypeError('Iterator return is not callable');

    if Assigned(AArgs) and (AArgs.Length > 0) then
      CallArgs := TGocciaArgumentsCollection.Create([Value])
    else
      CallArgs := TGocciaArgumentsCollection.Create;
    try
      IteratorResult := InvokeCallable(ReturnMethod, CallArgs, FIteratorValue);
    finally
      CallArgs.Free;
    end;
    if not (IteratorResult is TGocciaObjectValue) then
      ThrowTypeError('Iterator return result is not an object');
    DoneValue := IteratorResult.GetProperty(PROP_DONE);
    Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
    Value := IteratorResult.GetProperty(PROP_VALUE);
    if not Assigned(Value) then
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    if Done then
      ClearIteratorState;
    Result := AsyncFromSyncIteratorContinuation(Value, Done, False);
  except
    on E: EGocciaBytecodeThrow do
      Result := PromiseReject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaRuntimeError do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: EGocciaCapabilityAuditDeliveryError do
      raise;
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

function TGocciaVMAsyncFromSyncIteratorValue.ThrowValue(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorResult: TGocciaValue;
  ReturnMethod: TGocciaValue;
  ThrowMethod: TGocciaValue;
  Value: TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Value := AArgs.GetElement(0)
  else
    Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  try
    if not Assigned(FIteratorValue) then
      raise TGocciaThrowValue.Create(Value);

    if FIteratorValue is TGocciaIteratorValue then
    begin
      IteratorResult := TGocciaIteratorValue(FIteratorValue).ThrowValue(Value);
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Done then
        ClearIteratorState;
      Exit(AsyncFromSyncIteratorContinuation(Value, Done, True));
    end;

    ThrowMethod := FIteratorValue.GetProperty(PROP_THROW);
    if Assigned(ThrowMethod) and
       not (ThrowMethod is TGocciaUndefinedLiteralValue) and
       not (ThrowMethod is TGocciaNullLiteralValue) then
    begin
      if not ThrowMethod.IsCallable then
        ThrowTypeError('Iterator throw is not callable');
      CallArgs := TGocciaArgumentsCollection.Create([Value]);
      try
        IteratorResult := InvokeCallable(ThrowMethod, CallArgs, FIteratorValue);
      finally
        CallArgs.Free;
      end;
      if not (IteratorResult is TGocciaObjectValue) then
        ThrowTypeError('Iterator throw result is not an object');
      DoneValue := IteratorResult.GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      Value := IteratorResult.GetProperty(PROP_VALUE);
      if not Assigned(Value) then
        Value := TGocciaUndefinedLiteralValue.UndefinedValue;
      if Done then
        ClearIteratorState;
      Exit(AsyncFromSyncIteratorContinuation(Value, Done, True));
    end;

    ReturnMethod := FIteratorValue.GetProperty(PROP_RETURN);
    if Assigned(ReturnMethod) and
       not (ReturnMethod is TGocciaUndefinedLiteralValue) and
       not (ReturnMethod is TGocciaNullLiteralValue) then
    begin
      if not ReturnMethod.IsCallable then
        ThrowTypeError('Iterator return is not callable');
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        IteratorResult := InvokeCallable(ReturnMethod, CallArgs, FIteratorValue);
      finally
        CallArgs.Free;
      end;
      if not (IteratorResult is TGocciaObjectValue) then
        ThrowTypeError('Iterator return result is not an object');
    end;
    ClearIteratorState;
    ThrowTypeError('Iterator throw is not callable');
  except
    on E: EGocciaBytecodeThrow do
      Result := PromiseReject(E.ThrownValue);
    on E: TGocciaThrowValue do
      Result := PromiseReject(E.Value);
    on E: TGocciaTypeError do
      Result := PromiseReject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      Result := PromiseReject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaSyntaxError do
      Result := PromiseReject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaRuntimeError do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: EGocciaCapabilityAuditDeliveryError do
      raise;
    on E: Exception do
      Result := PromiseReject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

procedure TGocciaVMAsyncFromSyncIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FIteratorValue) then
    FIteratorValue.MarkReferences;
  if Assigned(FNextMethod) then
    FNextMethod.MarkReferences;
end;

function VMArgumentOrUndefined(const AArgs: TGocciaArgumentsCollection): TGocciaValue;
begin
  if Assigned(AArgs) and (AArgs.Length > 0) then
    Result := AArgs.GetElement(0)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function VMRejectedTypeErrorPromise(const AMessage: string): TGocciaPromiseValue;
var
  IsRooted: Boolean;
begin
  Result := TGocciaPromiseValue.Create;
  IsRooted := (TGarbageCollector.Instance <> nil);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(Result);
  try
    Result.Reject(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
  except
    if IsRooted then
    begin
      TGarbageCollector.Instance.RemoveTempRoot(Result);
      IsRooted := False;
    end;
    Result.Free;
    raise;
  end;
  if IsRooted then
    TGarbageCollector.Instance.RemoveTempRoot(Result);
end;

function VMGeneratorTypeError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME,
    'Generator method called on incompatible receiver');
end;

function VMGeneratorExecutingError: TGocciaValue;
begin
  Result := CreateErrorObject(TYPE_ERROR_NAME, 'Generator is already executing');
end;

constructor EGocciaBytecodeYield.Create(const AValue: TGocciaRegister;
  const AYieldIndex: Integer);
begin
  inherited Create('');
  FValue := AValue;
  FYieldIndex := AYieldIndex;
end;

constructor EGocciaBytecodeGeneratorReturn.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
end;

constructor TGocciaBytecodeGeneratorObjectValue.Create(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection);
var
  I: Integer;
begin
  inherited Create;
  Prototype := VMGeneratorObjectPrototype(foikGenerator);
  FVM := AVM;
  FClosure := AClosure.Clone;
  FThisValue := VMValueToRegisterFast(AThisValue);
  if Assigned(AArguments) then
  begin
    SetLength(FArguments, AArguments.Length);
    for I := 0 to AArguments.Length - 1 do
      FArguments[I] := VMValueToRegisterFast(AArguments.GetElement(I));
  end;
  FState := bgsSuspendedStart;
  FResumeValue := RegisterUndefined;
  FIgnoreNextResume := False;
  FReturnRequiresAwait := False;
  FReturnResumeValueAwaited := False;
  if Assigned(FClosure.Template) and (FClosure.Template.ParameterPreambleSize > 0) then
    FVM.ExecuteGeneratorParameterPreamble(Self);
end;

destructor TGocciaBytecodeGeneratorObjectValue.Destroy;
begin
  FClosure.Free;
  inherited;
end;

constructor TGocciaBytecodeGeneratorObjectValue.CreateRegisters(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
  const AArguments: TGocciaRegisterArray;
  const ARunParameterPreamble: Boolean);
var
  I: Integer;
begin
  inherited Create;
  Prototype := VMGeneratorObjectPrototype(foikGenerator);
  FVM := AVM;
  FClosure := AClosure.Clone;
  FThisValue := AThisValue;
  SetLength(FArguments, Length(AArguments));
  for I := 0 to High(AArguments) do
    FArguments[I] := AArguments[I];
  FState := bgsSuspendedStart;
  FResumeValue := RegisterUndefined;
  FIgnoreNextResume := False;
  FReturnRequiresAwait := False;
  FReturnResumeValueAwaited := False;
  if ARunParameterPreamble and Assigned(FClosure.Template) and
     (FClosure.Template.ParameterPreambleSize > 0) then
    FVM.ExecuteGeneratorParameterPreamble(Self);
end;

procedure TGocciaBytecodeGeneratorObjectValue.CaptureContinuation(
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AResumeRegister: UInt16;
  const AContinuationIP: Integer);
var
  I: Integer;
  LiveLocalCellCount: Integer;
  LiveRegisterCount: Integer;
begin
  FHasContinuation := True;
  FContinuationIP := AContinuationIP;
  FContinuationPrevCovLine := APrevCovLine;
  FResumeRegister := AResumeRegister;
  FContinuationDynamicVarScope := FVM.FCurrentDynamicVarScope;

  LiveLocalCellCount := FVM.FLocalCellCount;
  while (LiveLocalCellCount > 0) and
        not Assigned(FVM.FLocalCells[LiveLocalCellCount - 1]) do
    Dec(LiveLocalCellCount);

  LiveRegisterCount := FVM.FRegisterCount;
  while (LiveRegisterCount > 0) and
        (FVM.FRegisters[LiveRegisterCount - 1].Kind = grkUndefined) and
        (LiveRegisterCount > LiveLocalCellCount) do
    Dec(LiveRegisterCount);

  SetLength(FContinuationRegisters, LiveRegisterCount);
  for I := 0 to High(FContinuationRegisters) do
    FContinuationRegisters[I] := FVM.FRegisters[I];

  SetLength(FContinuationLocalCells, LiveLocalCellCount);
  for I := 0 to High(FContinuationLocalCells) do
    FContinuationLocalCells[I] := FVM.FLocalCells[I];

  FVM.FHandlerStack.CopyFrom(AHandlerBaseCount, FContinuationHandlers);
end;

procedure TGocciaBytecodeGeneratorObjectValue.CaptureInitialContinuation(
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AContinuationIP: Integer);
begin
  CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine, 0,
    AContinuationIP);
  FIgnoreNextResume := True;
end;

function TGocciaBytecodeGeneratorObjectValue.RestoreContinuation(
  var AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  out APrevCovLine: UInt32): Boolean;
var
  I: Integer;
begin
  Result := FHasContinuation;
  if not Result then
    Exit;

  FHasContinuation := False;
  FVM.EnsureRegisterCapacity(Length(FContinuationRegisters));
  for I := 0 to High(FContinuationRegisters) do
    FVM.FRegisters[I] := FContinuationRegisters[I];
  FVM.EnsureLocalCapacity(Length(FContinuationLocalCells));
  for I := 0 to High(FContinuationLocalCells) do
    FVM.FLocalCells[I] := FContinuationLocalCells[I];

  while FVM.FHandlerStack.Count > AHandlerBaseCount do
    FVM.FHandlerStack.Pop;
  FVM.FHandlerStack.RestoreFrom(FContinuationHandlers, FVM.FFrameDepth);

  AFrame.IP := FContinuationIP;
  FVM.FCurrentDynamicVarScope := FContinuationDynamicVarScope;
  FContinuationDynamicVarScope := nil;
  APrevCovLine := FContinuationPrevCovLine;
  SetLength(FContinuationRegisters, 0);
  SetLength(FContinuationLocalCells, 0);
  SetLength(FContinuationHandlers, 0);
end;

procedure TGocciaBytecodeGeneratorObjectValue.ClearDelegateState;
begin
  FDelegateActive := False;
  FDelegateIteratorValue := nil;
  FDelegateIterator := nil;
  FDelegateNextMethod := nil;
end;

procedure TGocciaBytecodeGeneratorObjectValue.HandleYield(
  const AValue: TGocciaRegister; const AResumeRegister: UInt16;
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AContinuationIP: Integer);
begin
  ClearDelegateState;
  CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
    AResumeRegister, AContinuationIP);
  raise EGocciaBytecodeYield.Create(AValue, 0);
end;

procedure TGocciaBytecodeGeneratorObjectValue.HandleYieldDelegate(
  const AIterable: TGocciaRegister; const AResumeRegister: UInt16;
  const AFrame: TGocciaVMCallFrame; const AHandlerBaseCount: Integer;
  const APrevCovLine: UInt32; const AContinuationIP: Integer);
var
  CallArgs: TGocciaArgumentsCollection;
  Done: Boolean;
  DoneValue: TGocciaValue;
  IteratorValue: TGocciaValue;
  NextResult: TGocciaValue;
  OriginalReturnResumeValue: TGocciaValue;
  ReturnMethod: TGocciaValue;
  ReturnMethodCalled: Boolean;
  ReturnResumeValue: TGocciaValue;
  YieldedValue: TGocciaValue;
  HadDelegateContinuation: Boolean;
  NextArgument: TGocciaValue;

  function IsAsyncGeneratorClosure: Boolean;
  begin
    Result := Assigned(FClosure) and Assigned(FClosure.Template) and
      FClosure.Template.IsAsync;
  end;

  procedure ClearDelegateAndThrowTypeError(const AMessage: string);
  begin
    ClearDelegateState;
    ThrowTypeError(AMessage);
  end;

  procedure ClearDelegateAndThrowTypeErrorWithSuggestion(
    const AMessage, ASuggestion: string);
  begin
    ClearDelegateState;
    ThrowTypeError(AMessage, ASuggestion);
  end;

  function AwaitDelegateResult(const AResult: TGocciaValue): TGocciaValue;
  begin
    Result := AResult;
    if IsAsyncGeneratorClosure then
      Result := AwaitValue(Result);
  end;

  function CallDelegateMethod(const AMethod, AThisValue: TGocciaValue;
    const AArgs: TGocciaArgumentsCollection): TGocciaValue;
  begin
    try
      Result := AwaitDelegateResult(InvokeCallable(AMethod, AArgs, AThisValue));
    except
      ClearDelegateState;
      raise;
    end;
  end;

  procedure CloseDelegateForMissingThrow;
  begin
    ReturnMethod := nil;
    if Assigned(FDelegateIteratorValue) then
      ReturnMethod := FDelegateIteratorValue.GetProperty(PROP_RETURN);
    if Assigned(ReturnMethod) and
       not (ReturnMethod is TGocciaUndefinedLiteralValue) and
       not (ReturnMethod is TGocciaNullLiteralValue) then
    begin
      if not ReturnMethod.IsCallable then
      begin
        ClearDelegateState;
        ThrowTypeError('Iterator return is not callable');
      end;
      CallArgs := TGocciaArgumentsCollection.Create;
      try
        ReturnMethod := CallDelegateMethod(ReturnMethod,
          FDelegateIteratorValue, CallArgs);
      finally
        CallArgs.Free;
      end;
      if not (ReturnMethod is TGocciaObjectValue) then
      begin
        ClearDelegateState;
        ThrowTypeError('Iterator return result is not an object');
      end;
    end;
    ClearDelegateState;
    ThrowTypeError('Iterator throw is not callable');
  end;
begin
  try
    HadDelegateContinuation := FDelegateActive;
    if not FDelegateActive then
    begin
      IteratorValue := FVM.GetIteratorValue(RegisterToValue(AIterable),
        Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync);
      if not Assigned(IteratorValue) then
        Exit;

      FDelegateIteratorValue := IteratorValue;
      FDelegateIterator := nil;
      FDelegateNextMethod := nil;
      if IteratorValue is TGocciaIteratorValue then
        FDelegateIterator := TGocciaIteratorValue(IteratorValue)
      else if IteratorValue is TGocciaObjectValue then
        FDelegateNextMethod := IteratorValue.GetProperty(PROP_NEXT);
      FDelegateActive := True;
    end;

    if FResumeKind = bgrkReturn then
    begin
      NextResult := nil;
      FReturnValue := nil;
      ReturnMethodCalled := False;
      OriginalReturnResumeValue := RegisterToValue(FResumeValue);
      ReturnResumeValue := OriginalReturnResumeValue;
      if Assigned(FClosure) and Assigned(FClosure.Template) and
         FClosure.Template.IsAsync then
        ReturnResumeValue := AwaitValue(ReturnResumeValue);
      if Assigned(FDelegateIterator) then
        NextResult := FDelegateIterator.ReturnValue(ReturnResumeValue)
      else if Assigned(FDelegateIteratorValue) then
      begin
        if IsAsyncGeneratorClosure and
           (FDelegateIteratorValue is TGocciaVMAsyncIteratorRecordValue) then
        begin
          if TGocciaVMAsyncIteratorRecordValue(FDelegateIteratorValue)
             .GetUnderlyingReturnMethod(NextResult) then
          begin
            if not NextResult.IsCallable then
              ClearDelegateAndThrowTypeError('Iterator return is not callable');
            CallArgs := TGocciaArgumentsCollection.Create([ReturnResumeValue]);
            try
              NextResult := CallDelegateMethod(NextResult,
                TGocciaVMAsyncIteratorRecordValue(FDelegateIteratorValue)
                  .FIteratorValue, CallArgs);
              ReturnMethodCalled := True;
            finally
              CallArgs.Free;
            end;
          end
          else
            NextResult := nil;
        end
        else
          NextResult := FDelegateIteratorValue.GetProperty(PROP_RETURN);
        if not ReturnMethodCalled then
        begin
          if Assigned(NextResult) and
             not (NextResult is TGocciaUndefinedLiteralValue) and
             not (NextResult is TGocciaNullLiteralValue) then
          begin
            if not NextResult.IsCallable then
              ClearDelegateAndThrowTypeError('Iterator return is not callable');
            CallArgs := TGocciaArgumentsCollection.Create([ReturnResumeValue]);
            try
              NextResult := CallDelegateMethod(NextResult,
                FDelegateIteratorValue, CallArgs);
            finally
              CallArgs.Free;
            end;
          end
          else
            NextResult := nil;
        end
      end;
      if Assigned(NextResult) then
      begin
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeError('Iterator return result is not an object');
        DoneValue := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
        Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
        if not Done then
        begin
          if IsAsyncGeneratorClosure then
          begin
            YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
            if not Assigned(YieldedValue) then
              YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          end
          else
            YieldedValue := NextResult;
          CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
            AResumeRegister, AContinuationIP);
          raise EGocciaBytecodeYield.Create(
            VMValueToRegisterFast(YieldedValue),
            Ord(not IsAsyncGeneratorClosure));
        end;
        YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        if IsAsyncGeneratorClosure and
           (FDelegateIteratorValue is TGocciaVMAsyncIteratorRecordValue) and
           TGocciaVMAsyncIteratorRecordValue(FDelegateIteratorValue)
             .LastReturnMethodMissing then
        begin
          FReturnValue := OriginalReturnResumeValue;
          FReturnRequiresAwait := True;
        end
        else
          FReturnValue := YieldedValue;
      end;
      ClearDelegateState;
      if not Assigned(FReturnValue) then
      begin
        if Assigned(FClosure) and Assigned(FClosure.Template) and
           FClosure.Template.IsAsync then
        begin
          FReturnValue := OriginalReturnResumeValue;
          FReturnRequiresAwait := True;
        end
        else
          FReturnValue := ReturnResumeValue;
      end;
      if not Assigned(FReturnSentinel) then
        FReturnSentinel := TGocciaObjectValue.Create;
      raise EGocciaBytecodeThrow.Create(FReturnSentinel);
    end;

    if FResumeKind = bgrkThrow then
    begin
      NextResult := nil;
      if Assigned(FDelegateIterator) then
        NextResult := FDelegateIterator.ThrowValue(RegisterToValue(FResumeValue))
      else if Assigned(FDelegateIteratorValue) then
      begin
        NextResult := FDelegateIteratorValue.GetProperty(PROP_THROW);
        if Assigned(NextResult) and
           not (NextResult is TGocciaUndefinedLiteralValue) and
           not (NextResult is TGocciaNullLiteralValue) then
        begin
          if not NextResult.IsCallable then
            CloseDelegateForMissingThrow;
          CallArgs := TGocciaArgumentsCollection.Create([RegisterToValue(FResumeValue)]);
          try
            NextResult := CallDelegateMethod(NextResult,
              FDelegateIteratorValue, CallArgs);
          finally
            CallArgs.Free;
          end;
        end
        else
          CloseDelegateForMissingThrow;
      end;
      if Assigned(NextResult) then
      begin
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeError('Iterator throw result is not an object');
        DoneValue := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
        Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
        if Done then
        begin
          YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
          if not Assigned(YieldedValue) then
            YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          FVM.FRegisters[AResumeRegister] := VMValueToRegisterFast(YieldedValue);
          ClearDelegateState;
          Exit;
        end;
        if IsAsyncGeneratorClosure then
        begin
          YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
          if not Assigned(YieldedValue) then
            YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
        end
        else
          YieldedValue := NextResult;
        CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
          AResumeRegister, AContinuationIP);
        raise EGocciaBytecodeYield.Create(VMValueToRegisterFast(YieldedValue),
          Ord(not IsAsyncGeneratorClosure));
      end;
      ClearDelegateState;
      raise EGocciaBytecodeThrow.Create(RegisterToValue(FResumeValue));
    end;

    while True do
    begin
      if HadDelegateContinuation and (FResumeKind = bgrkNext) then
        NextArgument := RegisterToValue(FResumeValue)
      else
        NextArgument := TGocciaUndefinedLiteralValue.UndefinedValue;

      if Assigned(FDelegateIterator) then
      begin
        NextResult := FDelegateIterator.AdvanceNextResultValue(NextArgument);
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeErrorWithSuggestion(
            Format(SErrorIteratorResultNotObject,
            [NextResult.ToStringLiteral.Value]), SSuggestIteratorResultObject);
      end
      else
      begin
        if not Assigned(FDelegateNextMethod) or not FDelegateNextMethod.IsCallable then
          ClearDelegateAndThrowTypeError('Iterator.next is not a function');
        CallArgs := TGocciaArgumentsCollection.Create([NextArgument]);
        try
          NextResult := CallDelegateMethod(FDelegateNextMethod,
            FDelegateIteratorValue, CallArgs);
        finally
          CallArgs.Free;
        end;
        if not (NextResult is TGocciaObjectValue) then
          ClearDelegateAndThrowTypeErrorWithSuggestion(
            Format(SErrorIteratorResultNotObject,
            [NextResult.ToStringLiteral.Value]), SSuggestIteratorResultObject);
      end;

      DoneValue := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
      Done := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
      if Done or IsAsyncGeneratorClosure then
      begin
        YieldedValue := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
        if not Assigned(YieldedValue) then
          YieldedValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      end
      else
        YieldedValue := NextResult;

      if Done then
      begin
        FVM.FRegisters[AResumeRegister] := VMValueToRegisterFast(YieldedValue);
        ClearDelegateState;
        Exit;
      end;

      CaptureContinuation(AFrame, AHandlerBaseCount, APrevCovLine,
        AResumeRegister, AContinuationIP);
      raise EGocciaBytecodeYield.Create(VMValueToRegisterFast(YieldedValue),
        Ord(not IsAsyncGeneratorClosure));
    end;
  except
    on E: EGocciaBytecodeYield do
      raise;
    else
    begin
      ClearDelegateState;
      raise;
    end;
  end;
end;

function TGocciaBytecodeGeneratorObjectValue.ResumeRaw(
  const AKind: TGocciaBytecodeGeneratorResumeKind; const AValue: TGocciaValue;
  out ADone: Boolean): TGocciaValue;
var
  PreviousGenerator: TGocciaBytecodeGeneratorObjectValue;
  ReturnRegister: TGocciaRegister;
begin
  FLastResumeResultIsIteratorResult := False;
  if FState = bgsCompleted then
  begin
    ADone := True;
    if AKind = bgrkThrow then
      raise TGocciaThrowValue.Create(AValue);
    if AKind = bgrkReturn then
      Exit(AValue);
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  if FState = bgsExecuting then
    raise TGocciaThrowValue.Create(VMGeneratorExecutingError);

  if (FState = bgsSuspendedStart) and (AKind = bgrkReturn) then
  begin
    FState := bgsCompleted;
    ADone := True;
    Exit(AValue);
  end;

  if (FState = bgsSuspendedStart) and (AKind = bgrkThrow) then
  begin
    FState := bgsCompleted;
    raise TGocciaThrowValue.Create(AValue);
  end;

  FResumeKind := AKind;
  FResumeValue := VMValueToRegisterFast(AValue);
  FReturnRequiresAwait := False;
  if AKind <> bgrkReturn then
    FReturnResumeValueAwaited := False;
  FState := bgsExecuting;

  PreviousGenerator := GActiveBytecodeGenerator;
  GActiveBytecodeGenerator := Self;
  try
    try
      ReturnRegister := FVM.ExecuteClosureRegisters(FClosure, FThisValue, FArguments);
      FState := bgsCompleted;
      ClearDelegateState;
      ADone := True;
      Result := RegisterToValue(ReturnRegister);
    except
      on E: EGocciaBytecodeYield do
      begin
        FState := bgsSuspendedYield;
        ADone := False;
        Result := RegisterToValue(E.Value);
        FLastResumeResultIsIteratorResult := E.YieldIndex <> 0;
      end;
      on E: EGocciaBytecodeGeneratorReturn do
      begin
        FState := bgsCompleted;
        ClearDelegateState;
        ADone := True;
        Result := E.Value;
      end;
      on E: EGocciaBytecodeThrow do
      begin
        if Assigned(FReturnSentinel) and (E.ThrownValue = FReturnSentinel) then
        begin
          FState := bgsCompleted;
          ClearDelegateState;
          ADone := True;
          Result := FReturnValue;
        end
        else
        begin
          FState := bgsCompleted;
          ClearDelegateState;
          ADone := True;
          raise;
        end;
      end;
    end;
  finally
    GActiveBytecodeGenerator := PreviousGenerator;
  end;
end;

function TGocciaBytecodeGeneratorObjectValue.WrapResumeResult(
  const AValue: TGocciaValue; const ADone: Boolean): TGocciaObjectValue;
begin
  if FLastResumeResultIsIteratorResult and (AValue is TGocciaObjectValue) then
    Result := TGocciaObjectValue(AValue)
  else
    Result := CreateIteratorResult(AValue, ADone);
end;

function TGocciaBytecodeGeneratorObjectValue.AdvanceNext: TGocciaObjectValue;
begin
  Result := AdvanceNextValue(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

function TGocciaBytecodeGeneratorObjectValue.AdvanceNextValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkNext, AValue, Done);
  Result := WrapResumeResult(ResultValue, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.DirectNext(out ADone: Boolean): TGocciaValue;
begin
  Result := DirectNextValue(TGocciaUndefinedLiteralValue.UndefinedValue, ADone);
end;

function TGocciaBytecodeGeneratorObjectValue.DirectNextValue(
  const AValue: TGocciaValue; out ADone: Boolean): TGocciaValue;
var
  DoneValue: TGocciaValue;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkNext, AValue, ADone);
  if FLastResumeResultIsIteratorResult and (ResultValue is TGocciaObjectValue) then
  begin
    DoneValue := TGocciaObjectValue(ResultValue).GetProperty(PROP_DONE);
    ADone := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
    Result := TGocciaObjectValue(ResultValue).GetProperty(PROP_VALUE);
    if not Assigned(Result) then
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;
  Result := ResultValue;
end;

function TGocciaBytecodeGeneratorObjectValue.ReturnValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkReturn, AValue, Done);
  Result := WrapResumeResult(ResultValue, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.ThrowValue(
  const AValue: TGocciaValue): TGocciaObjectValue;
var
  Done: Boolean;
  ResultValue: TGocciaValue;
begin
  ResultValue := ResumeRaw(bgrkThrow, AValue, Done);
  Result := WrapResumeResult(ResultValue, Done);
end;

procedure TGocciaBytecodeGeneratorObjectValue.Close;
begin
  if FState = bgsCompleted then
    Exit;
  ReturnValue(TGocciaUndefinedLiteralValue.UndefinedValue);
end;

constructor TGocciaVMAsyncAwaitContinuationValue.Create(const AVM: TGocciaVM;
  const AContinuation: TGocciaBytecodeGeneratorObjectValue;
  const APromise: TGocciaPromiseValue;
  const AResumeKind: TGocciaBytecodeGeneratorResumeKind;
  const AAsyncGeneratorResult: Boolean);
begin
  inherited Create;
  FVM := AVM;
  FContinuation := AContinuation;
  FPromise := APromise;
  FResumeKind := AResumeKind;
  FAsyncGeneratorResult := AAsyncGeneratorResult;
end;

function TGocciaVMAsyncAwaitContinuationValue.GetFunctionLength: Integer;
begin
  Result := 1;
end;

function TGocciaVMAsyncAwaitContinuationValue.GetFunctionName: string;
begin
  Result := 'async-await-continuation';
end;

function TGocciaVMAsyncAwaitContinuationValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  PreviousAsyncPromise: TGocciaPromiseValue;
  ResumeValue: TGocciaValue;
  ResultValue: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FContinuation) or not Assigned(FPromise) then
    Exit;

  ResumeValue := VMArgumentOrUndefined(AArguments);
  PreviousAsyncPromise := FVM.FCurrentAsyncPromise;
  FVM.FCurrentAsyncPromise := FPromise;
  try
    try
      ResultValue := FContinuation.ResumeRaw(FResumeKind, ResumeValue, Done);
      if FAsyncGeneratorResult and (FResumeKind = bgrkReturn) then
        FPromise.Resolve(CreateIteratorResult(ResultValue, Done))
      else if FAsyncGeneratorResult then
        FPromise.Resolve(CreateIteratorResult(AwaitValue(ResultValue), Done))
      else if Done then
        FPromise.Resolve(ResultValue);
    except
      on E: EGocciaBytecodeAsyncSuspend do
        Exit;
      on E: EGocciaBytecodeThrow do
        FPromise.Reject(E.ThrownValue);
      on E: TGocciaThrowValue do
        FPromise.Reject(E.Value);
    end;
  finally
    FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
  end;
end;

procedure TGocciaVMAsyncAwaitContinuationValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FContinuation) then
    FContinuation.MarkReferences;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
end;

{ TGocciaVMDynamicImportFulfillValue }

constructor TGocciaVMDynamicImportFulfillValue.Create(
  const APromise: TGocciaPromiseValue; const ANamespace: TGocciaValue);
begin
  inherited Create;
  FPromise := APromise;
  FNamespace := ANamespace;
end;

function TGocciaVMDynamicImportFulfillValue.GetFunctionLength: Integer;
begin
  Result := 1;
end;

function TGocciaVMDynamicImportFulfillValue.GetFunctionName: string;
begin
  Result := 'dynamic-import-fulfill';
end;

function TGocciaVMDynamicImportFulfillValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Assigned(FPromise) then
    FPromise.Resolve(FNamespace);
end;

procedure TGocciaVMDynamicImportFulfillValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
  if Assigned(FNamespace) then
    FNamespace.MarkReferences;
end;

{ TGocciaVMDynamicImportRejectValue }

constructor TGocciaVMDynamicImportRejectValue.Create(
  const APromise: TGocciaPromiseValue);
begin
  inherited Create;
  FPromise := APromise;
end;

function TGocciaVMDynamicImportRejectValue.GetFunctionLength: Integer;
begin
  Result := 1;
end;

function TGocciaVMDynamicImportRejectValue.GetFunctionName: string;
begin
  Result := 'dynamic-import-reject';
end;

function TGocciaVMDynamicImportRejectValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if Assigned(FPromise) then
    FPromise.Reject(VMArgumentOrUndefined(AArguments));
end;

procedure TGocciaVMDynamicImportRejectValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
end;

{ TGocciaVMDynamicImportStartValue }

constructor TGocciaVMDynamicImportStartValue.Create(const AVM: TGocciaVM;
  const APromise: TGocciaPromiseValue; const APath, AReferrer: string);
begin
  inherited Create;
  FVM := AVM;
  FPromise := APromise;
  FPath := APath;
  FReferrer := AReferrer;
end;

function TGocciaVMDynamicImportStartValue.GetFunctionLength: Integer;
begin
  Result := 0;
end;

function TGocciaVMDynamicImportStartValue.GetFunctionName: string;
begin
  Result := 'dynamic-import-start';
end;

function TGocciaVMDynamicImportStartValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FVM) or not Assigned(FPromise) then
    Exit;

  try
    FVM.ResolveDynamicImportPromise(FPromise, FPath, FReferrer);
  except
    on E: EGocciaBytecodeThrow do
      FPromise.Reject(E.ThrownValue);
    on E: TGocciaThrowValue do
      FPromise.Reject(E.Value);
    on E: TGocciaSyntaxError do
      FPromise.Reject(CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
    on E: TGocciaTypeError do
      FPromise.Reject(CreateErrorObject(TYPE_ERROR_NAME, E.Message));
    on E: TGocciaReferenceError do
      FPromise.Reject(CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
    on E: TGocciaTimeoutError do
      raise;
    on E: TGocciaInstructionLimitError do
      raise;
    on E: EGocciaCapabilityAuditDeliveryError do
      raise;
    on E: Exception do
      FPromise.Reject(CreateErrorObject(ERROR_NAME, E.Message));
  end;
end;

procedure TGocciaVMDynamicImportStartValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FPromise) then
    FPromise.MarkReferences;
end;

function TGocciaBytecodeGeneratorObjectValue.GeneratorNext(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(VMGeneratorTypeError);
  Value := TGocciaBytecodeGeneratorObjectValue(AThisValue).ResumeRaw(
    bgrkNext, VMArgumentOrUndefined(AArgs), Done);
  Result := TGocciaBytecodeGeneratorObjectValue(AThisValue).WrapResumeResult(
    Value, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.GeneratorReturn(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(VMGeneratorTypeError);
  Value := TGocciaBytecodeGeneratorObjectValue(AThisValue).ResumeRaw(
    bgrkReturn, VMArgumentOrUndefined(AArgs), Done);
  Result := TGocciaBytecodeGeneratorObjectValue(AThisValue).WrapResumeResult(
    Value, Done);
end;

function TGocciaBytecodeGeneratorObjectValue.GeneratorThrow(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Done: Boolean;
  Value: TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeGeneratorObjectValue) then
    raise TGocciaThrowValue.Create(VMGeneratorTypeError);
  Value := TGocciaBytecodeGeneratorObjectValue(AThisValue).ResumeRaw(
    bgrkThrow, VMArgumentOrUndefined(AArgs), Done);
  Result := TGocciaBytecodeGeneratorObjectValue(AThisValue).WrapResumeResult(
    Value, Done);
end;

procedure TGocciaBytecodeGeneratorObjectValue.MarkReferences;
var
  I: Integer;
  Upvalue: TGocciaBytecodeUpvalue;
begin
  inherited;
  if Assigned(FClosure) then
  begin
    if Assigned(FClosure.HomeObject) then
      FClosure.HomeObject.MarkReferences;
    if Assigned(FClosure.HomeClass) then
      FClosure.HomeClass.MarkReferences;
    if Assigned(FClosure.NewTarget) then
      FClosure.NewTarget.MarkReferences;
    if Assigned(FClosure.GlobalScope) then
      FClosure.GlobalScope.MarkReferences;
    if Assigned(FClosure.DynamicVarScope) then
      FClosure.DynamicVarScope.MarkReferences;
    for I := 0 to FClosure.UpvalueCount - 1 do
    begin
      Upvalue := FClosure.GetUpvalue(I);
      if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
        MarkRegisterReferences(Upvalue.Cell.Value);
    end;
  end;
  MarkRegisterReferences(FThisValue);
  MarkRegisterReferences(FResumeValue);
  for I := 0 to High(FArguments) do
    MarkRegisterReferences(FArguments[I]);
  for I := 0 to High(FContinuationRegisters) do
    MarkRegisterReferences(FContinuationRegisters[I]);
  for I := 0 to High(FContinuationLocalCells) do
    if Assigned(FContinuationLocalCells[I]) then
      MarkRegisterReferences(FContinuationLocalCells[I].Value);
  if Assigned(FDelegateIteratorValue) then
    FDelegateIteratorValue.MarkReferences;
  if Assigned(FDelegateIterator) then
    FDelegateIterator.MarkReferences;
  if Assigned(FDelegateNextMethod) then
    FDelegateNextMethod.MarkReferences;
  if Assigned(FReturnSentinel) then
    FReturnSentinel.MarkReferences;
  if Assigned(FReturnValue) then
    FReturnValue.MarkReferences;
  if Assigned(FContinuationDynamicVarScope) then
    FContinuationDynamicVarScope.MarkReferences;
end;

function TGocciaBytecodeGeneratorObjectValue.ToStringTag: string;
begin
  Result := 'Generator';
end;

function TGocciaBytecodeGeneratorObjectValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
end;

constructor TGocciaBytecodeAsyncGeneratorObjectValue.Create(const AVM: TGocciaVM;
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection);
begin
  inherited Create;
  Prototype := VMGeneratorObjectPrototype(foikAsyncGenerator);
  FInner := TGocciaBytecodeGeneratorObjectValue.Create(AVM, AClosure,
    AThisValue, AArguments);
end;

constructor TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(
  const AVM: TGocciaVM; const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray);
begin
  inherited Create;
  Prototype := VMGeneratorObjectPrototype(foikAsyncGenerator);
  FInner := TGocciaBytecodeGeneratorObjectValue.CreateRegisters(AVM, AClosure,
    AThisValue, AArguments);
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.ResumeAsPromise(
  const AKind: TGocciaBytecodeGeneratorResumeKind; const AValue: TGocciaValue): TGocciaValue;
var
  Request: TGocciaBytecodeAsyncGeneratorRequest;
begin
  Request.Kind := AKind;
  Request.Value := AValue;
  Request.Promise := TGocciaPromiseValue.Create;
  EnqueueRequest(Request);
  ProcessQueue;
  Result := Request.Promise;
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.ContinueQueue(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  FQueueRunning := False;
  ProcessQueue;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.DequeueRequest(
  out ARequest: TGocciaBytecodeAsyncGeneratorRequest): Boolean;
begin
  Result := FQueueCount > 0;
  if not Result then
    Exit;

  ARequest := FQueue[FQueueHead];
  FQueue[FQueueHead].Value := nil;
  FQueue[FQueueHead].Promise := nil;
  Inc(FQueueHead);
  Dec(FQueueCount);
  if FQueueCount = 0 then
    FQueueHead := 0;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.EnqueueRequest(
  const ARequest: TGocciaBytecodeAsyncGeneratorRequest);
var
  I: Integer;
begin
  if FQueueHead > 0 then
  begin
    for I := 0 to FQueueCount - 1 do
      FQueue[I] := FQueue[FQueueHead + I];
    for I := FQueueCount to High(FQueue) do
    begin
      FQueue[I].Value := nil;
      FQueue[I].Promise := nil;
    end;
    FQueueHead := 0;
  end;

  if FQueueCount >= Length(FQueue) then
    SetLength(FQueue, FQueueCount * 2 + 4);

  FQueue[FQueueCount] := ARequest;
  Inc(FQueueCount);
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.FinishRequest;
begin
  FQueueRunning := False;
  ProcessQueue;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.ContinueWhenPromiseSettles(
  const APromise: TGocciaPromiseValue);
var
  ContinueFunction: TGocciaNativeFunctionValue;
begin
  ContinueFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    ContinueQueue, 'async-generator-continue', 1);
  ContinueFunction.CapturedRoot := Self;
  APromise.InvokeThen(ContinueFunction, ContinueFunction);
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.AwaitYieldValue(
  const ARequestPromise: TGocciaPromiseValue; const AValue: TGocciaValue);
var
  AwaitPromise: TGocciaPromiseValue;
  FulfillFunction: TGocciaNativeFunctionValue;
  FulfillHandler: TGocciaVMAsyncGeneratorYieldAwaitHandler;
  RejectFunction: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaVMAsyncGeneratorYieldAwaitHandler;
begin
  AwaitPromise := FInner.FVM.PromiseResolveIntrinsic(AValue);

  FulfillHandler := TGocciaVMAsyncGeneratorYieldAwaitHandler.Create(
    Self, ARequestPromise, False);
  FulfillFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillHandler.Invoke, 'async-generator-yield-fulfill', 1);
  FulfillFunction.CapturedRoot := FulfillHandler;

  RejectHandler := TGocciaVMAsyncGeneratorYieldAwaitHandler.Create(
    Self, ARequestPromise, True);
  RejectFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    RejectHandler.Invoke, 'async-generator-yield-reject', 1);
  RejectFunction.CapturedRoot := RejectHandler;

  AwaitPromise.InvokeThen(FulfillFunction, RejectFunction);
end;

// ES2026 §27.6.3.9 AsyncGeneratorAwaitReturn(gen)
procedure TGocciaBytecodeAsyncGeneratorObjectValue.AwaitReturnValue(
  const ARequestPromise: TGocciaPromiseValue; const AValue: TGocciaValue);
var
  AwaitPromise: TGocciaPromiseValue;
  FulfillFunction: TGocciaNativeFunctionValue;
  FulfillHandler: TGocciaVMAsyncGeneratorYieldAwaitHandler;
  RejectFunction: TGocciaNativeFunctionValue;
  RejectHandler: TGocciaVMAsyncGeneratorYieldAwaitHandler;
begin
  AwaitPromise := FInner.FVM.PromiseResolveIntrinsic(AValue);

  FulfillHandler := TGocciaVMAsyncGeneratorYieldAwaitHandler.Create(
    Self, ARequestPromise, False, True);
  FulfillFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    FulfillHandler.Invoke, 'async-generator-return-fulfill', 1);
  FulfillFunction.CapturedRoot := FulfillHandler;

  RejectHandler := TGocciaVMAsyncGeneratorYieldAwaitHandler.Create(
    Self, ARequestPromise, True, True);
  RejectFunction := TGocciaNativeFunctionValue.CreateWithoutPrototype(
    RejectHandler.Invoke, 'async-generator-return-reject', 1);
  RejectFunction.CapturedRoot := RejectHandler;

  AwaitPromise.InvokeThen(FulfillFunction, RejectFunction);
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.ResolveAwaitedYield(
  const ARequestPromise: TGocciaPromiseValue; const AValue: TGocciaValue);
begin
  ARequestPromise.Resolve(CreateIteratorResult(AValue, False));
  FinishRequest;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.RejectAwaitedYield(
  const ARequestPromise: TGocciaPromiseValue; const AReason: TGocciaValue);
var
  Done: Boolean;
  PreviousAsyncPromise: TGocciaPromiseValue;
  ResultValue: TGocciaValue;
  UnwrappedValue: TGocciaValue;
begin
  PreviousAsyncPromise := FInner.FVM.FCurrentAsyncPromise;
  FInner.FVM.FCurrentAsyncPromise := ARequestPromise;
  try
    try
      ResultValue := FInner.ResumeRaw(bgrkThrow, AReason, Done);
      if Done then
      begin
        if FInner.FReturnRequiresAwait then
        begin
          AwaitReturnValue(ARequestPromise, ResultValue);
          Exit;
        end;
        UnwrappedValue := ResultValue;
        ARequestPromise.Resolve(CreateIteratorResult(UnwrappedValue, True));
        FinishRequest;
      end
      else if FInner.FDelegateActive then
      begin
        ARequestPromise.Resolve(CreateIteratorResult(ResultValue, False));
        FinishRequest;
      end
      else
        AwaitYieldValue(ARequestPromise, ResultValue);
    except
      on E: EGocciaBytecodeAsyncSuspend do
        ContinueWhenPromiseSettles(ARequestPromise);
      on E: EGocciaBytecodeThrow do
      begin
        ARequestPromise.Reject(E.ThrownValue);
        FinishRequest;
      end;
      on E: TGocciaThrowValue do
      begin
        ARequestPromise.Reject(E.Value);
        FinishRequest;
      end;
    end;
  finally
    FInner.FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
  end;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.ResolveAwaitedReturn(
  const ARequestPromise: TGocciaPromiseValue; const AValue: TGocciaValue);
begin
  ARequestPromise.Resolve(CreateIteratorResult(AValue, True));
  FinishRequest;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.RejectAwaitedReturn(
  const ARequestPromise: TGocciaPromiseValue; const AReason: TGocciaValue);
begin
  ARequestPromise.Reject(AReason);
  FinishRequest;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.ProcessQueue;
var
  Request: TGocciaBytecodeAsyncGeneratorRequest;
begin
  if FQueueRunning then
    Exit;
  if not DequeueRequest(Request) then
    Exit;

  FQueueRunning := True;
  StartRequest(Request);
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.StartRequest(
  const ARequest: TGocciaBytecodeAsyncGeneratorRequest);
var
  PreviousAsyncPromise: TGocciaPromiseValue;
  Done: Boolean;
  IsRooted: Boolean;
  UnwrappedValue: TGocciaValue;
  Value: TGocciaValue;
begin
  IsRooted := (TGarbageCollector.Instance <> nil);
  if IsRooted then
    TGarbageCollector.Instance.AddTempRoot(ARequest.Promise);
  try
    try
      try
        PreviousAsyncPromise := FInner.FVM.FCurrentAsyncPromise;
        FInner.FVM.FCurrentAsyncPromise := ARequest.Promise;
        try
          try
            if (ARequest.Kind = bgrkReturn) and
               (FInner.FState in [bgsSuspendedStart, bgsCompleted]) then
            begin
              FInner.FState := bgsCompleted;
              AwaitReturnValue(ARequest.Promise, ARequest.Value);
              Exit;
            end;

            if FInner.FState = bgsCompleted then
            begin
              case ARequest.Kind of
	                bgrkNext:
	                  begin
	                    ARequest.Promise.Resolve(CreateIteratorResult(
	                      TGocciaUndefinedLiteralValue.UndefinedValue, True));
                    FinishRequest;
	                    Exit;
	                  end;
	                bgrkThrow:
	                  begin
	                    ARequest.Promise.Reject(ARequest.Value);
                    FinishRequest;
	                    Exit;
	                  end;
	              else
	                begin
	                  AwaitReturnValue(ARequest.Promise, ARequest.Value);
	                  Exit;
	                end;
              end;
            end;

            Value := FInner.ResumeRaw(ARequest.Kind, ARequest.Value, Done);
            try
              if Done then
              begin
                if FInner.FReturnRequiresAwait then
                begin
                  AwaitReturnValue(ARequest.Promise, Value);
                  Exit;
                end;
                UnwrappedValue := Value;
              end
              else if FInner.FDelegateActive then
                UnwrappedValue := Value
              else
              begin
                AwaitYieldValue(ARequest.Promise, Value);
                Exit;
              end;
            except
              on E: TGocciaThrowValue do
              begin
	                FInner.FState := bgsCompleted;
	                FInner.ClearDelegateState;
	                ARequest.Promise.Reject(E.Value);
                FinishRequest;
	                Exit;
	              end;
	            end;
	            ARequest.Promise.Resolve(CreateIteratorResult(UnwrappedValue, Done));
            FinishRequest;
	          except
	            on E: EGocciaBytecodeAsyncSuspend do
	            begin
              ContinueWhenPromiseSettles(ARequest.Promise);
	            end;
	          end;
        finally
          FInner.FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
        end;
	      except
	        on E: EGocciaBytecodeThrow do
          begin
	          ARequest.Promise.Reject(E.ThrownValue);
            FinishRequest;
          end;
	        on E: TGocciaThrowValue do
          begin
	          ARequest.Promise.Reject(E.Value);
            FinishRequest;
          end;
	      end;
    except
      if IsRooted then
      begin
        TGarbageCollector.Instance.RemoveTempRoot(ARequest.Promise);
        IsRooted := False;
      end;
      raise;
    end;
  finally
    if IsRooted then
      TGarbageCollector.Instance.RemoveTempRoot(ARequest.Promise);
  end;
end;

constructor TGocciaVMAsyncGeneratorYieldAwaitHandler.Create(
  const AGenerator: TGocciaBytecodeAsyncGeneratorObjectValue;
  const ARequestPromise: TGocciaPromiseValue; const AReject: Boolean;
  const AReturn: Boolean = False);
begin
  inherited Create(nil);
  FGenerator := AGenerator;
  FRequestPromise := ARequestPromise;
  FReject := AReject;
  FReturn := AReturn;
end;

function TGocciaVMAsyncGeneratorYieldAwaitHandler.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Assigned(FGenerator) or not Assigned(FRequestPromise) then
    Exit;

  Value := VMArgumentOrUndefined(AArgs);
  if FReturn and FReject then
    FGenerator.RejectAwaitedReturn(FRequestPromise, Value)
  else if FReturn then
    FGenerator.ResolveAwaitedReturn(FRequestPromise, Value)
  else if FReject then
    FGenerator.RejectAwaitedYield(FRequestPromise, Value)
  else
    FGenerator.ResolveAwaitedYield(FRequestPromise, Value);
end;

procedure TGocciaVMAsyncGeneratorYieldAwaitHandler.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FGenerator) then
    FGenerator.MarkReferences;
  if Assigned(FRequestPromise) then
    FRequestPromise.MarkReferences;
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncGeneratorNext(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeAsyncGeneratorObjectValue) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  if not Assigned(TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).FInner) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    bgrkNext, VMArgumentOrUndefined(AArgs));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncGeneratorReturn(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeAsyncGeneratorObjectValue) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  if not Assigned(TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).FInner) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    bgrkReturn, VMArgumentOrUndefined(AArgs));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncGeneratorThrow(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBytecodeAsyncGeneratorObjectValue) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  if not Assigned(TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).FInner) then
    Exit(VMRejectedTypeErrorPromise('AsyncGenerator method called on incompatible receiver'));
  Result := TGocciaBytecodeAsyncGeneratorObjectValue(AThisValue).ResumeAsPromise(
    bgrkThrow, VMArgumentOrUndefined(AArgs));
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.AsyncIteratorSelf(
  const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

procedure TGocciaBytecodeAsyncGeneratorObjectValue.MarkReferences;
var
  I: Integer;
  Index: Integer;
begin
  inherited;
  if Assigned(FInner) then
    FInner.MarkReferences;
  for I := 0 to FQueueCount - 1 do
  begin
    Index := FQueueHead + I;
    if (Index < 0) or (Index > High(FQueue)) then
      Continue;
    if Assigned(FQueue[Index].Value) then
      FQueue[Index].Value.MarkReferences;
    if Assigned(FQueue[Index].Promise) then
      FQueue[Index].Promise.MarkReferences;
  end;
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.ToStringTag: string;
begin
  Result := 'AsyncGenerator';
end;

function TGocciaBytecodeAsyncGeneratorObjectValue.BuiltinTagFallback: Boolean;
begin
  Result := True;
end;

function TGocciaBytecodeFunctionValue.GetFunctionName: string;
begin
  Result := FClosure.Template.Name;
  if (Result = '<arrow>') or (Result = '<function>') or
     (Result = '<method>') or (Result = '<method [computed]>') then
    Result := '';
end;

function TGocciaBytecodeFunctionValue.GetSourceText: string;
begin
  Result := FClosure.Template.SourceText;
end;

function BytecodeGeneratorResultWithFunctionPrototype(
  const AFunction: TGocciaBytecodeFunctionValue;
  const AGeneratorObject: TGocciaObjectValue): TGocciaValue;
var
  Kind: TGocciaFunctionObjectIntrinsicKind;
  PrototypeValue: TGocciaValue;
begin
  PrototypeValue := AFunction.GetProperty(PROP_PROTOTYPE);
  if PrototypeValue is TGocciaObjectValue then
    AGeneratorObject.Prototype := TGocciaObjectValue(PrototypeValue)
  else if Assigned(AFunction.FClosure) and Assigned(AFunction.FClosure.Template) then
  begin
    Kind := BytecodeFunctionIntrinsicKind(AFunction.FClosure.Template);
    if Kind in [foikGenerator, foikAsyncGenerator] then
      AGeneratorObject.Prototype := VMGeneratorObjectPrototype(Kind,
        AFunction.CreationRealm);
  end;
  Result := AGeneratorObject;
end;

function TGocciaBytecodeFunctionValue.IsConstructable: Boolean;
begin
  if not inherited IsConstructable then
    Exit(False);

  Result := Assigned(FClosure) and Assigned(FClosure.Template) and
            (not FClosure.Template.IsGenerator) and
            (not FClosure.Template.IsAsync) and
            (not FClosure.Template.IsArrow);
end;

function TGocciaVMSuperConstructorValue.GetFunctionName: string;
begin
  Result := 'super';
end;

function TGocciaVMSuperConstructorValue.IsConstructable: Boolean;
begin
  Result := True;
end;

function InvokeConstructableWithReceiver(const AConstructor: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AReceiver: TGocciaValue;
  const ANewTarget: TGocciaValue = nil): TGocciaValue;
var
  BoundFunction: TGocciaBoundFunctionValue;
  ClassConstructor: TGocciaClassValue;
  CombinedArgs: TGocciaArgumentsCollection;
  SuperResult: TGocciaValue;
  ConstructorThisValue: TGocciaValue;
  VMClassConstructor: TGocciaVMClassValue;
  EffectiveNewTarget: TGocciaValue;
  I: Integer;
  function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := (not Assigned(AValue)) or
      (AValue is TGocciaUndefinedLiteralValue);
  end;
  function ClassRequiresObjectConstructorReturn(
    const AClassValue: TGocciaClassValue): Boolean;
  begin
    Result := Assigned(AClassValue) and
      (Assigned(AClassValue.SuperClass) or
       Assigned(AClassValue.NativeSuperConstructor));
  end;
  procedure ValidateClassConstructorReturn(
    const AClassValue: TGocciaClassValue; const AValue: TGocciaValue);
  begin
    if ClassRequiresObjectConstructorReturn(AClassValue) and
       not (AValue is TGocciaObjectValue) and
       not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
begin
  if Assigned(ANewTarget) then
    EffectiveNewTarget := ANewTarget
  else
    EffectiveNewTarget := AConstructor;

  if AConstructor is TGocciaBoundFunctionValue then
  begin
    BoundFunction := TGocciaBoundFunctionValue(AConstructor);
    if IsSameValue(BoundFunction, EffectiveNewTarget) then
      EffectiveNewTarget := BoundFunction.OriginalFunction;
    CombinedArgs := TGocciaArgumentsCollection.CreateWithCapacity(
      BoundFunction.BoundArgCount + AArguments.Length);
    try
      for I := 0 to BoundFunction.BoundArgCount - 1 do
        CombinedArgs.Add(BoundFunction.GetBoundArg(I));
      for I := 0 to AArguments.Length - 1 do
        CombinedArgs.Add(AArguments.GetElement(I));
      Exit(InvokeConstructableWithReceiver(BoundFunction.OriginalFunction,
        CombinedArgs, AReceiver, EffectiveNewTarget));
    finally
      CombinedArgs.Free;
    end;
  end;

  if AConstructor is TGocciaProxyValue then
    SuperResult := TGocciaProxyValue(AConstructor).ConstructTrap(
      AArguments, EffectiveNewTarget)
  else if AConstructor is TGocciaNativeFunctionValue then
  begin
    if TGocciaNativeFunctionValue(AConstructor).NotConstructable then
      ThrowTypeError(
        Format(SErrorNotConstructor,
          [TGocciaNativeFunctionValue(AConstructor).Name]),
        Format('''%s'' is not a constructor',
          [TGocciaNativeFunctionValue(AConstructor).Name]));
    SuperResult := TGocciaNativeFunctionValue(AConstructor).Construct(
      AArguments, EffectiveNewTarget);
  end
  else if AConstructor is TGocciaClassValue then
  begin
    ClassConstructor := TGocciaClassValue(AConstructor);
    if (ClassConstructor is TGocciaVMClassValue) and
       Assigned(TGocciaVMClassValue(ClassConstructor).FConstructorValue) then
    begin
      VMClassConstructor := TGocciaVMClassValue(ClassConstructor);
      VMClassConstructor.FVM.FPendingNewTarget := EffectiveNewTarget;
      VMClassConstructor.FVM.RunClassInitializers(ClassConstructor, AReceiver);
      SuperResult := VMClassConstructor.FVM.InvokeFunctionValue(
        VMClassConstructor.FConstructorValue, AArguments, AReceiver);
      ValidateClassConstructorReturn(ClassConstructor, SuperResult);
      if VMClassConstructor.FConstructorValue is TGocciaBytecodeFunctionValue then
        ConstructorThisValue := RegisterToValue(
          VMClassConstructor.FVM.FLastClosureThisValue)
      else
        ConstructorThisValue := nil;
      if not (SuperResult is TGocciaObjectValue) and
         (ConstructorThisValue is TGocciaObjectValue) then
        SuperResult := ConstructorThisValue;
      if (SuperResult is TGocciaObjectValue) and
         (SuperResult <> AReceiver) and
         (SuperResult = ConstructorThisValue) then
        VMClassConstructor.FVM.RunClassInitializers(ClassConstructor, SuperResult);
    end
    else if Assigned(ClassConstructor.ConstructorMethod) then
    begin
      // Mirror the Evaluator path: CallWithThisValue returns the constructor's
      // final `this` (which the body may have replaced via an explicit return)
      // so we can promote it as the receiver when the body returned a primitive.
      SuperResult := ClassConstructor.ConstructorMethod.CallWithThisValue(
        AArguments, AReceiver, ConstructorThisValue, EffectiveNewTarget);
      ValidateClassConstructorReturn(ClassConstructor, SuperResult);
      if not (SuperResult is TGocciaObjectValue) and
         (ConstructorThisValue is TGocciaObjectValue) then
        SuperResult := ConstructorThisValue;
    end
    else if ClassConstructor is TGocciaVMClassValue then
      SuperResult := TGocciaVMClassValue(ClassConstructor).FVM.InvokeImplicitSuperInitialization(
        ClassConstructor, AReceiver, AArguments)
    else
      SuperResult := AReceiver;
  end
  else if AConstructor is TGocciaFunctionBase then
  begin
    SuperResult := TGocciaFunctionBase(AConstructor).ConstructWithReceiver(
      AArguments, AReceiver, EffectiveNewTarget);
  end
  else
    ThrowTypeError(Format(SErrorValueNotConstructor, [AConstructor.TypeName]),
      SSuggestNotConstructorType);

  if SuperResult is TGocciaObjectValue then
    Result := SuperResult
  else
    Result := AReceiver;
end;

function TGocciaBytecodeFunctionValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  PreviousAsyncPromise: TGocciaPromiseValue;
  EffectiveThis: TGocciaValue;
  PromiseRoot: TGocciaTempRoot;
begin
  if FStrictThis then
    EffectiveThis := AThisValue
  else
    EffectiveThis := CoerceBytecodeNonStrictThisValue(FClosure,
      AThisValue, FVM.FGlobalThisValue, FVM.FRealm);

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
        TGocciaBytecodeAsyncGeneratorObjectValue.Create(FVM, FClosure,
          EffectiveThis, AArguments)));
    Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
      TGocciaBytecodeGeneratorObjectValue.Create(FVM, FClosure,
        EffectiveThis, AArguments)));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    InitializeTempRoot(PromiseRoot);
    AddTempRootIfNeeded(PromiseRoot, Promise);
    try
      try
        PreviousAsyncPromise := FVM.FCurrentAsyncPromise;
        FVM.FCurrentAsyncPromise := Promise;
        try
          try
            Promise.Resolve(FVM.ExecuteClosure(FClosure, EffectiveThis, AArguments));
          except
            on E: EGocciaBytecodeAsyncSuspend do
            begin
            end;
            on E: EGocciaBytecodeThrow do
              Promise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              Promise.Reject(E.Value);
          end;
        finally
          FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
        end;
      except
        Promise.Free;
        raise;
      end;
    finally
      RemoveTempRootIfNeeded(PromiseRoot);
    end;
    Exit(Promise);
  end;

  Result := FVM.ExecuteClosure(FClosure, EffectiveThis, AArguments);
end;

function TGocciaBytecodeFunctionValue.ConstructWithReceiver(
  const AArguments: TGocciaArgumentsCollection; const AReceiver: TGocciaValue;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  PreviousPendingNewTarget: TGocciaValue;
begin
  PreviousPendingNewTarget := FVM.FPendingNewTarget;
  FVM.FPendingNewTarget := ANewTarget;
  try
    Result := Call(AArguments, AReceiver);
  finally
    FVM.FPendingNewTarget := PreviousPendingNewTarget;
  end;
end;

function TGocciaBytecodeFunctionValue.CallPreparedArgs(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  case AArguments.Length of
    0: Result := CallNoArgs(AThisValue);
    1: Result := CallOneArg(AArguments.GetElement(0), AThisValue);
    2: Result := CallTwoArgs(AArguments.GetElement(0), AArguments.GetElement(1),
      AThisValue);
    3: Result := CallThreeArgs(AArguments.GetElement(0), AArguments.GetElement(1),
      AArguments.GetElement(2), AThisValue);
  else
    Result := Call(AArguments, AThisValue);
  end;
end;

function TGocciaBytecodeFunctionValue.CallNoArgs(
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  PreviousAsyncPromise: TGocciaPromiseValue;
  EffectiveThis: TGocciaValue;
  PromiseRoot: TGocciaTempRoot;
begin
  if FStrictThis then
    EffectiveThis := AThisValue
  else
    EffectiveThis := CoerceBytecodeNonStrictThisValue(FClosure,
      AThisValue, FVM.FGlobalThisValue, FVM.FRealm);

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
        TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
          VMValueToRegisterFast(EffectiveThis), TGocciaRegisterArray(nil))));
    Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
      TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(EffectiveThis), TGocciaRegisterArray(nil))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    InitializeTempRoot(PromiseRoot);
    AddTempRootIfNeeded(PromiseRoot, Promise);
    try
      try
        PreviousAsyncPromise := FVM.FCurrentAsyncPromise;
        FVM.FCurrentAsyncPromise := Promise;
        try
          try
            Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters0(FClosure,
              VMValueToRegisterFast(EffectiveThis))));
          except
            on E: EGocciaBytecodeAsyncSuspend do
            begin
            end;
            on E: EGocciaBytecodeThrow do
              Promise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              Promise.Reject(E.Value);
          end;
        finally
          FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
        end;
      except
        Promise.Free;
        raise;
      end;
    finally
      RemoveTempRootIfNeeded(PromiseRoot);
    end;
    Exit(Promise);
  end;

  Result := RegisterToValue(FVM.ExecuteClosureRegisters0(FClosure,
    VMValueToRegisterFast(EffectiveThis)));
end;

function TGocciaBytecodeFunctionValue.CallOneArg(const AArg0,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  PreviousAsyncPromise: TGocciaPromiseValue;
  EffectiveThis: TGocciaValue;
  PromiseRoot: TGocciaTempRoot;
begin
  if FStrictThis then
    EffectiveThis := AThisValue
  else
    EffectiveThis := CoerceBytecodeNonStrictThisValue(FClosure,
      AThisValue, FVM.FGlobalThisValue, FVM.FRealm);

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
        TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
          VMValueToRegisterFast(EffectiveThis),
          TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0)))));
    Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
      TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(EffectiveThis),
        TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0)))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    InitializeTempRoot(PromiseRoot);
    AddTempRootIfNeeded(PromiseRoot, Promise);
    try
      try
        PreviousAsyncPromise := FVM.FCurrentAsyncPromise;
        FVM.FCurrentAsyncPromise := Promise;
        try
          try
            Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters1(FClosure,
              VMValueToRegisterFast(EffectiveThis), VMValueToRegisterFast(AArg0))));
          except
            on E: EGocciaBytecodeAsyncSuspend do
            begin
            end;
            on E: EGocciaBytecodeThrow do
              Promise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              Promise.Reject(E.Value);
          end;
        finally
          FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
        end;
      except
        Promise.Free;
        raise;
      end;
    finally
      RemoveTempRootIfNeeded(PromiseRoot);
    end;
    Exit(Promise);
  end;

  Result := RegisterToValue(FVM.ExecuteClosureRegisters1(FClosure,
    VMValueToRegisterFast(EffectiveThis), VMValueToRegisterFast(AArg0)));
end;

function TGocciaBytecodeFunctionValue.CallTwoArgs(const AArg0, AArg1,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  PreviousAsyncPromise: TGocciaPromiseValue;
  EffectiveThis: TGocciaValue;
  PromiseRoot: TGocciaTempRoot;
begin
  if FStrictThis then
    EffectiveThis := AThisValue
  else
    EffectiveThis := CoerceBytecodeNonStrictThisValue(FClosure,
      AThisValue, FVM.FGlobalThisValue, FVM.FRealm);

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
        TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
          VMValueToRegisterFast(EffectiveThis),
          TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
            VMValueToRegisterFast(AArg1)))));
    Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
      TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(EffectiveThis),
        TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
          VMValueToRegisterFast(AArg1)))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    InitializeTempRoot(PromiseRoot);
    AddTempRootIfNeeded(PromiseRoot, Promise);
    try
      try
        PreviousAsyncPromise := FVM.FCurrentAsyncPromise;
        FVM.FCurrentAsyncPromise := Promise;
        try
          try
            Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters2(FClosure,
              VMValueToRegisterFast(EffectiveThis), VMValueToRegisterFast(AArg0),
              VMValueToRegisterFast(AArg1))));
          except
            on E: EGocciaBytecodeAsyncSuspend do
            begin
            end;
            on E: EGocciaBytecodeThrow do
              Promise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              Promise.Reject(E.Value);
          end;
        finally
          FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
        end;
      except
        Promise.Free;
        raise;
      end;
    finally
      RemoveTempRootIfNeeded(PromiseRoot);
    end;
    Exit(Promise);
  end;

  Result := RegisterToValue(FVM.ExecuteClosureRegisters2(FClosure,
    VMValueToRegisterFast(EffectiveThis), VMValueToRegisterFast(AArg0),
    VMValueToRegisterFast(AArg1)));
end;

function TGocciaBytecodeFunctionValue.CallThreeArgs(const AArg0, AArg1, AArg2,
  AThisValue: TGocciaValue): TGocciaValue;
var
  Promise: TGocciaPromiseValue;
  PreviousAsyncPromise: TGocciaPromiseValue;
  EffectiveThis: TGocciaValue;
  PromiseRoot: TGocciaTempRoot;
begin
  if FStrictThis then
    EffectiveThis := AThisValue
  else
    EffectiveThis := CoerceBytecodeNonStrictThisValue(FClosure,
      AThisValue, FVM.FGlobalThisValue, FVM.FRealm);

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsGenerator then
  begin
    if FClosure.Template.IsAsync then
      Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
        TGocciaBytecodeAsyncGeneratorObjectValue.CreateRegisters(FVM, FClosure,
          VMValueToRegisterFast(EffectiveThis),
          TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
            VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2)))));
    Exit(BytecodeGeneratorResultWithFunctionPrototype(Self,
      TGocciaBytecodeGeneratorObjectValue.CreateRegisters(FVM, FClosure,
        VMValueToRegisterFast(EffectiveThis),
        TGocciaRegisterArray.Create(VMValueToRegisterFast(AArg0),
          VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2)))));
  end;

  if Assigned(FClosure) and Assigned(FClosure.Template) and FClosure.Template.IsAsync then
  begin
    Promise := TGocciaPromiseValue.Create;
    InitializeTempRoot(PromiseRoot);
    AddTempRootIfNeeded(PromiseRoot, Promise);
    try
      try
        PreviousAsyncPromise := FVM.FCurrentAsyncPromise;
        FVM.FCurrentAsyncPromise := Promise;
        try
          try
            Promise.Resolve(RegisterToValue(FVM.ExecuteClosureRegisters3(FClosure,
              VMValueToRegisterFast(EffectiveThis), VMValueToRegisterFast(AArg0),
              VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2))));
          except
            on E: EGocciaBytecodeAsyncSuspend do
            begin
            end;
            on E: EGocciaBytecodeThrow do
              Promise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              Promise.Reject(E.Value);
          end;
        finally
          FVM.FCurrentAsyncPromise := PreviousAsyncPromise;
        end;
      except
        Promise.Free;
        raise;
      end;
    finally
      RemoveTempRootIfNeeded(PromiseRoot);
    end;
    Exit(Promise);
  end;

  Result := RegisterToValue(FVM.ExecuteClosureRegisters3(FClosure,
    VMValueToRegisterFast(EffectiveThis), VMValueToRegisterFast(AArg0),
    VMValueToRegisterFast(AArg1), VMValueToRegisterFast(AArg2)));
end;

function TGocciaVMSuperConstructorValue.Call(
  const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  EffectiveSuper: TGocciaValue;
  SuperClass: TGocciaClassValue;
  BytecodeConstructor: TGocciaBytecodeFunctionValue;
  NewThis: TGocciaValue;
  SuperResult: TGocciaValue;
  ConstructorThisValue: TGocciaValue;
  ImplicitSuperInitialized: Boolean;
  WasSuperAlreadyCalled: Boolean;
  ReceiverPrototype: TGocciaObjectValue;
  function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
  end;
  procedure ValidateSuperConstructorResult(const AValue: TGocciaValue);
  begin
    if (Assigned(SuperClass.SuperClass) or
        Assigned(SuperClass.NativeSuperConstructor)) and
       not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure InitializeCurrentCtorReceiver(const AReceiver: TGocciaValue);
  var
    CurrentVM: TGocciaVM;
    I: Integer;
    Desc: TGocciaUpvalueDescriptor;
    Upvalue: TGocciaBytecodeUpvalue;
  begin
    if (AReceiver is TGocciaObjectValue) and
       (FCurrentCtorClass is TGocciaVMClassValue) then
    begin
      CurrentVM := TGocciaVMClassValue(FCurrentCtorClass).FVM;
      CurrentVM.SetLocal(0, AReceiver);
      if Assigned(CurrentVM.FCurrentClosure) and
         Assigned(CurrentVM.FCurrentClosure.Template) then
      begin
        if CurrentVM.FCurrentClosure.Template.DerivedThisInitializedSlot >= 0 then
          CurrentVM.SetLocal(
            CurrentVM.FCurrentClosure.Template.DerivedThisInitializedSlot,
            TGocciaBooleanLiteralValue.TrueValue);
        for I := 0 to CurrentVM.FCurrentClosure.Template.UpvalueCount - 1 do
        begin
          Desc := CurrentVM.FCurrentClosure.Template.GetUpvalueDescriptor(I);
          if Desc.Name = DERIVED_THIS_INITIALIZED_LOCAL then
          begin
            Upvalue := CurrentVM.FCurrentClosure.GetUpvalue(I);
            if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
              Upvalue.Cell.Value := ValueToRegister(
                TGocciaBooleanLiteralValue.TrueValue);
          end;
        end;
      end;
      CurrentVM.FLastClosureThisValue := VMValueToRegisterFast(AReceiver);
      if HasBytecodePrivateInitializersApplied(AReceiver,
        TGocciaClassValue(FCurrentCtorClass)) then
      begin
        if AReceiver = AThisValue then
          ThrowReferenceError('Super constructor may only be called once');
        ThrowTypeError('Cannot initialize private elements twice',
          SSuggestPrivateFieldAccess);
      end;
      CurrentVM.RunClassInitializers(FCurrentCtorClass, AReceiver, False);
      StampBytecodePrivateInitializersApplied(AReceiver,
        FCurrentCtorClass);
    end;
  end;
  procedure MarkCurrentConstructorSuperCalled;
  begin
    if WasSuperAlreadyCalled then
      ThrowReferenceError(
        'Super constructor may only be called once');
    if FCurrentCtorClass is TGocciaVMClassValue then
      TGocciaVMClassValue(FCurrentCtorClass).FVM.FCurrentConstructorSuperCalled :=
        True;
  end;
begin
  WasSuperAlreadyCalled := (FCurrentCtorClass is TGocciaVMClassValue) and
    TGocciaVMClassValue(FCurrentCtorClass).FVM.FCurrentConstructorSuperCalled;

  EffectiveSuper := FSuperClass;

  if (EffectiveSuper is TGocciaObjectValue) and
     (not (EffectiveSuper is TGocciaClassValue)) and
     EffectiveSuper.IsConstructable then
  begin
    SuperResult := InvokeConstructableWithReceiver(EffectiveSuper, AArguments,
      AThisValue, FNewTarget);
    MarkCurrentConstructorSuperCalled;
    if SuperResult is TGocciaObjectValue then
      InitializeCurrentCtorReceiver(SuperResult)
    else
      InitializeCurrentCtorReceiver(AThisValue);
    Exit(SuperResult);
  end;

  if not (EffectiveSuper is TGocciaClassValue) then
    ThrowTypeError('Super constructor is not a constructor',
      SSuggestNotConstructorType);

  SuperClass := TGocciaClassValue(EffectiveSuper);
  NewThis := AThisValue;
  ImplicitSuperInitialized := False;

  if (SuperClass is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(SuperClass).FConstructorValue) then
  begin
    TGocciaVMClassValue(SuperClass).FVM.FPendingNewTarget := FNewTarget;
    TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
      SuperClass, AThisValue);
    SuperResult := TGocciaVMClassValue(SuperClass).FVM.InvokeFunctionValue(
      TGocciaVMClassValue(SuperClass).FConstructorValue,
      AArguments, AThisValue);
    if SuperResult is TGocciaObjectValue then
      begin
        if (SuperResult <> AThisValue) and
           not HasBytecodePrivateInitializersApplied(SuperResult, SuperClass) then
          TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
            SuperClass, SuperResult);
        MarkCurrentConstructorSuperCalled;
        InitializeCurrentCtorReceiver(SuperResult);
        Exit(SuperResult);
      end;
    ValidateSuperConstructorResult(SuperResult);
    if TGocciaVMClassValue(SuperClass).FConstructorValue is TGocciaBytecodeFunctionValue then
    begin
      BytecodeConstructor := TGocciaBytecodeFunctionValue(
        TGocciaVMClassValue(SuperClass).FConstructorValue);
      ConstructorThisValue := RegisterToValue(
        BytecodeConstructor.FVM.FLastClosureThisValue);
      if ConstructorThisValue is TGocciaObjectValue then
      begin
        if (ConstructorThisValue <> AThisValue) and
           not HasBytecodePrivateInitializersApplied(ConstructorThisValue, SuperClass) then
          TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
            SuperClass, ConstructorThisValue);
        MarkCurrentConstructorSuperCalled;
        InitializeCurrentCtorReceiver(ConstructorThisValue);
        Exit(ConstructorThisValue);
      end;
    end;
    MarkCurrentConstructorSuperCalled;
    InitializeCurrentCtorReceiver(AThisValue);
    Exit(AThisValue);
  end;

  if Assigned(SuperClass.ConstructorMethod) then
  begin
    if SuperClass is TGocciaVMClassValue then
      TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
        SuperClass, AThisValue);
    SuperResult := SuperClass.ConstructorMethod.CallWithThisValue(
      AArguments, AThisValue, ConstructorThisValue, FNewTarget);
    if SuperResult is TGocciaObjectValue then
      begin
        if (SuperResult <> AThisValue) and
           (SuperClass is TGocciaVMClassValue) and
           not HasBytecodePrivateInitializersApplied(SuperResult, SuperClass) then
          TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
            SuperClass, SuperResult);
        MarkCurrentConstructorSuperCalled;
        InitializeCurrentCtorReceiver(SuperResult);
        Exit(SuperResult);
      end;
    ValidateSuperConstructorResult(SuperResult);
    if ConstructorThisValue is TGocciaObjectValue then
    begin
        if (ConstructorThisValue <> AThisValue) and
           (SuperClass is TGocciaVMClassValue) and
           not HasBytecodePrivateInitializersApplied(ConstructorThisValue, SuperClass) then
          TGocciaVMClassValue(SuperClass).FVM.RunClassInitializers(
            SuperClass, ConstructorThisValue);
        MarkCurrentConstructorSuperCalled;
        InitializeCurrentCtorReceiver(ConstructorThisValue);
        Exit(ConstructorThisValue);
      end;
    MarkCurrentConstructorSuperCalled;
    InitializeCurrentCtorReceiver(AThisValue);
    Exit(AThisValue);
  end;

  if SuperClass.NativeInstanceDefaultPrototype <> nil then
  begin
    NewThis := SuperClass.CreateNativeInstance(AArguments);
    if not (NewThis is TGocciaObjectValue) then
      ThrowTypeError(
        'Superclass constructor did not return an object',
        SSuggestNotConstructorType);
    if NewThis is TGocciaInstanceValue then
    begin
      TGocciaInstanceValue(NewThis).ClassValue := FCurrentCtorClass;
      TGocciaInstanceValue(NewThis).InitializeNativeFromArguments(AArguments);
    end;
    ReceiverPrototype := GetProtoFromConstructor(FNewTarget);
    TGocciaObjectValue(NewThis).Prototype := ReceiverPrototype;
    if NewThis is TGocciaInstanceValue then
      TGocciaInstanceValue(NewThis).FinalizeNativeFromArguments(AArguments);
    MarkCurrentConstructorSuperCalled;
    InitializeCurrentCtorReceiver(NewThis);
    Exit(NewThis);
  end;

  if Assigned(SuperClass.SuperClass) and
     Assigned(SuperClass.SuperClass.ConstructorMethod) then
  begin
    if SuperClass is TGocciaVMClassValue then
    begin
      NewThis := TGocciaVMClassValue(SuperClass).FVM.InvokeImplicitSuperInitialization(
        SuperClass, AThisValue, AArguments);
      if not Assigned(NewThis) then
        NewThis := AThisValue;
      ImplicitSuperInitialized := True;
    end;
  end;

  if AThisValue is TGocciaInstanceValue then
  begin
    if (not ImplicitSuperInitialized) and
       (SuperClass is TGocciaVMClassValue) then
    begin
      NewThis := TGocciaVMClassValue(SuperClass).FVM.InvokeImplicitSuperInitialization(
        SuperClass, AThisValue, AArguments);
      if not Assigned(NewThis) then
        NewThis := AThisValue;
    end;
    if NewThis is TGocciaInstanceValue then
      TGocciaInstanceValue(NewThis).InitializeNativeFromArguments(AArguments);
    if NewThis is TGocciaObjectValue then
    begin
      MarkCurrentConstructorSuperCalled;
      InitializeCurrentCtorReceiver(NewThis);
      Exit(NewThis);
    end;
    MarkCurrentConstructorSuperCalled;
    InitializeCurrentCtorReceiver(AThisValue);
    Exit(AThisValue);
  end;

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// Resolves a deferred call-stack frame (pushed by SetupNewFrame as a bare
// template pointer) into its display name and own source path. Registered on
// the shared call stack so the hot call path pays no per-call string cost; the
// strings are materialised only when a stack trace is captured.
procedure VMTemplateTraceResolver(const ATemplate: Pointer;
  out AName, ASourcePath: string);
var
  Template: TGocciaFunctionTemplate;
begin
  AName := '';
  ASourcePath := '';
  if not Assigned(ATemplate) then
    Exit;
  Template := TGocciaFunctionTemplate(ATemplate);
  AName := Template.Name;
  if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
    ASourcePath := Template.DebugInfo.SourceFile;
end;

constructor TGocciaVM.Create;
const
  INITIAL_STACK_SIZE = 4096;
begin
  inherited Create;
  FHandlerStack := TGocciaBytecodeHandlerStack.Create;
  // Teach the shared call stack how to materialise the template-pointer frames
  // that SetupNewFrame pushes on the hot path. This is class-level, so it is
  // independent of which thread's call-stack instance ends up running.
  TGocciaCallStack.SetTemplateResolver(@VMTemplateTraceResolver);
  FArgumentPoolCount := 0;
  FActiveDecoratorSession := nil;
  FLastClosureThisValue := RegisterUndefined;
  FCurrentConstructorSuperCalled := False;
  FPrivateInitializerReceiver := nil;
  FPrivateInitializerPreserveExisting := False;
  FTempSavedStateRootCount := 0;
  FMemoryPressureCheckCountdown := MEMORY_PRESSURE_CHECK_INTERVAL;
  FCurrentExecutionContextPushed := False;
  FCurrentDynamicVarScope := nil;
  FGlobalBackedTopLevel := False;
  SetLength(FRegisterStack, INITIAL_STACK_SIZE);
  FRegisterBase := 0;
  FRegisters := nil;
  FRegisterCount := 0;
  SetLength(FLocalCellStack, INITIAL_STACK_SIZE);
  FLocalCellBase := 0;
  FLocalCells := nil;
  FLocalCellCount := 0;
  SetLength(FArgumentStack, INITIAL_STACK_SIZE);
  FArgumentBase := 0;
  FArguments := nil;
  FArgCount := 0;
  SetLength(FFrameStack, 64);
  FFrameStackCount := 0;
  SetLength(FClosedNumericFrameStack, 64);
  FClosedNumericFrameStackCount := 0;
  FStackRoot := TGocciaVMStackRoot.Create(Self);
  EnsureStackRootRegistered;
end;

destructor TGocciaVM.Destroy;
var
  I: Integer;
begin
  if (TGarbageCollector.Instance <> nil) and Assigned(FStackRoot) and
     FStackRootRegistered then
    TGarbageCollector.Instance.RemoveRootObject(FStackRoot);
  FStackRoot.Free;
  if Assigned(FActiveDecoratorSession) then
  begin
    TGarbageCollector.Instance.RemoveTempRoot(
      TGocciaVMDecoratorSession(FActiveDecoratorSession).MetadataObject);
    FActiveDecoratorSession.Free;
  end;
  for I := 0 to FArgumentPoolCount - 1 do
    FArgumentPool[I].Free;
  if (TGarbageCollector.Instance <> nil) then
    for I := Low(FASCIIStringValues) to High(FASCIIStringValues) do
      if Assigned(FASCIIStringValues[I]) then
        TGarbageCollector.Instance.UnpinObject(FASCIIStringValues[I]);
  SetLength(FArgumentPool, 0);
  SetLength(FTempSavedStateRoots, 0);
  FHandlerStack.Free;
  inherited;
end;

function TGocciaVM.CachedASCIIStringValue(
  const ACodeUnit: TASCIIStringCodeUnit): TGocciaStringLiteralValue;
begin
  Result := FASCIIStringValues[ACodeUnit];
  if Assigned(Result) then
    Exit;

  Result := TGocciaStringLiteralValue.Create(Chr(ACodeUnit));
  if (TGarbageCollector.Instance <> nil) then
  begin
    TGarbageCollector.Instance.PinObject(Result);
    FASCIIStringValues[ACodeUnit] := Result;
  end;
end;

procedure TGocciaVM.EnsureStackRootRegistered;
var
  GC: TGarbageCollector;
begin
  if FStackRootRegistered or not Assigned(FStackRoot) then
    Exit;

  GC := TGarbageCollector.Instance;
  if not Assigned(GC) then
    Exit;

  if FStackRoot.GCIndex < 0 then
    GC.RegisterObject(FStackRoot);
  GC.AddRootObject(FStackRoot);
  FStackRootRegistered := True;
end;

destructor TGocciaBytecodeFunctionValue.Destroy;
begin
  FClosure.Free;
  inherited;
end;

// ES2026 §10.2.2 [[Construct]](argumentsList, newTarget)
function TGocciaVMClassValue.Instantiate(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  RootedInstance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  ImplicitSuperClass: TGocciaClassValue;
  NativeClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  NativeSuperConstructorForPrototype: TGocciaObjectValue;
  NativeIntrinsicPrototype: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
  InstancePrototype: TGocciaObjectValue;
  ConstructedValue: TGocciaValue;
  ConstructorThisValue: TGocciaValue;
  InitializerReplayReceiver: TGocciaObjectValue;
  PreviousConstructorSuperCalled: Boolean;
  ConstructorSuperCalled: Boolean;
  DelayNativePrototypeLookup: Boolean;
  NativeInstanceInitialized: Boolean;
  NativeInstanceConstructedByNativeSuper: Boolean;
  function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
  end;
  function HasDerivedConstructorReturnRestriction: Boolean;
  begin
    Result := Assigned(SuperClass) or Assigned(NativeSuperConstructor);
  end;
  function EffectiveNewTarget: TGocciaValue;
  begin
    if Assigned(ANewTarget) then
      Exit(ANewTarget);
    Result := Self;
  end;
  function ClassRequiresObjectConstructorReturn(
    const AClassValue: TGocciaClassValue): Boolean;
  begin
    Result := Assigned(AClassValue) and
      (Assigned(AClassValue.SuperClass) or
       Assigned(AClassValue.NativeSuperConstructor));
  end;
  procedure ValidateClassConstructorReturn(
    const AClassValue: TGocciaClassValue; const AValue: TGocciaValue);
  begin
    if ClassRequiresObjectConstructorReturn(AClassValue) and
       not (AValue is TGocciaObjectValue) and
       not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure SetFinalInstance(const AInstance: TGocciaObjectValue);
  begin
    if (not Assigned(AInstance)) or (AInstance = Instance) then
      Exit;
    TGarbageCollector.Instance.AddTempRoot(AInstance);
    TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
    RootedInstance := AInstance;
    Instance := AInstance;
  end;
  procedure ApplyOwnConstructorResult(const AValue,
    AConstructorThisValue: TGocciaValue);
  var
    ThisObject: TGocciaObjectValue;
    ReturnObject: TGocciaObjectValue;
  begin
    if (AConstructorThisValue is TGocciaObjectValue) and
       (AConstructorThisValue <> Instance) then
    begin
      ThisObject := TGocciaObjectValue(AConstructorThisValue);
      SetFinalInstance(ThisObject);
      InitializerReplayReceiver := ThisObject;
    end;

    if AValue is TGocciaObjectValue then
    begin
      ReturnObject := TGocciaObjectValue(AValue);
      if ReturnObject <> Instance then
      begin
        SetFinalInstance(ReturnObject);
        if ReturnObject <> InitializerReplayReceiver then
          InitializerReplayReceiver := nil;
      end;
    end
    else if HasDerivedConstructorReturnRestriction and
            not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure RequireDerivedConstructorThisInitialized(
    const AValue: TGocciaValue);
  begin
    if HasDerivedConstructorReturnRestriction and
       IsUndefinedConstructedValue(AValue) and
       not ConstructorSuperCalled then
      ThrowReferenceError(
        'Must call super constructor before returning from derived constructor');
  end;
  procedure ApplyReplacementResult(const AValue: TGocciaValue);
  begin
    if AValue is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(AValue) = Instance then
        Exit;
      SetFinalInstance(TGocciaObjectValue(AValue));
      InitializerReplayReceiver := Instance;
    end;
  end;
begin
  NativeClass := nil;
  NativeSuperConstructorForPrototype := nil;
  NativeIntrinsicPrototype := nil;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    NativeIntrinsicPrototype := WalkClass.NativeInstanceDefaultPrototype;
    if Assigned(NativeIntrinsicPrototype) then
    begin
      NativeClass := WalkClass;
      Break;
    end;
    if Assigned(WalkClass.NativeSuperConstructor) then
    begin
      NativeSuperConstructorForPrototype := WalkClass.NativeSuperConstructor;
      if WalkClass.NativeSuperConstructor is TGocciaClassValue then
      begin
        NativeClass := TGocciaClassValue(WalkClass.NativeSuperConstructor);
        NativeIntrinsicPrototype := NativeClass.NativeInstanceDefaultPrototype;
      end;
      Break;
    end;
    WalkClass := WalkClass.SuperClass;
  end;
  DelayNativePrototypeLookup :=
    ShouldDelayNativePrototypeLookup(NativeClass, AArguments) or
    ShouldDelayNativeSuperPrototypeLookup(NativeSuperConstructorForPrototype);

  // ES2026 §10.2.2 step 5: Let proto be ? GetPrototypeFromConstructor(newTarget)
  if Assigned(ANewTarget) and not DelayNativePrototypeLookup then
    InstancePrototype := GetProtoFromConstructor(ANewTarget)
  else
    InstancePrototype := Prototype;

  NativeInstance := nil;
  NativeInstanceInitialized := False;
  NativeInstanceConstructedByNativeSuper := False;
  if not (Assigned(FConstructorValue) and HasDerivedConstructorReturnRestriction) then
  begin
    WalkClass := Self;
    while Assigned(WalkClass) do
    begin
      if WalkClass is TGocciaVMClassValue then
      begin
        NativeInstance := TGocciaVMClassValue(WalkClass)
          .CreateNativeInstanceWithNewTarget(AArguments, EffectiveNewTarget);
        NativeInstanceConstructedByNativeSuper := Assigned(NativeInstance) and
          Assigned(TGocciaVMClassValue(WalkClass).NativeSuperConstructor);
      end
      else
      begin
        NativeInstance := WalkClass.CreateNativeInstance(AArguments);
        NativeInstanceConstructedByNativeSuper := False;
      end;
      if Assigned(NativeInstance) then
        Break;
      WalkClass := WalkClass.SuperClass;
    end;
  end;

  if Assigned(NativeInstance) and DelayNativePrototypeLookup and
     (not NativeInstanceConstructedByNativeSuper) and
     (NativeInstance is TGocciaInstanceValue) then
  begin
    TGarbageCollector.Instance.AddTempRoot(NativeInstance);
    try
      TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(
        AArguments);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(NativeInstance);
    end;
    NativeInstanceInitialized := True;
  end;

  if Assigned(NativeInstance) and Assigned(NativeClass) and
     Assigned(ANewTarget) and DelayNativePrototypeLookup then
    InstancePrototype := GetNativePrototypeFromConstructor(WalkClass,
      ANewTarget, NativeIntrinsicPrototype);

  // ES2026 §10.2.2 step 6: Set proto on the instance before constructor runs
  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    if Assigned(NativeClass) or not DelayNativePrototypeLookup then
      Instance.Prototype := InstancePrototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := Self;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(Self);
    Instance.Prototype := InstancePrototype;
  end;

  if NativeInstanceInitialized and (NativeInstance is TGocciaInstanceValue) then
  begin
    TGarbageCollector.Instance.AddTempRoot(Instance);
    try
      TGocciaInstanceValue(NativeInstance).FinalizeNativeFromArguments(
        AArguments);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;
  end;

  if Assigned(FConstructorValue) then
  begin
    RootedInstance := Instance;
    InitializerReplayReceiver := nil;
    TGarbageCollector.Instance.AddTempRoot(RootedInstance);
    try
      if not HasDerivedConstructorReturnRestriction then
        FVM.RunClassInitializers(Self, Instance);
      FVM.FPendingNewTarget := ANewTarget;
      if not Assigned(FVM.FPendingNewTarget) then
        FVM.FPendingNewTarget := Self;
      PreviousConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
      FVM.FCurrentConstructorSuperCalled := False;
      try
        ConstructedValue := FVM.InvokeFunctionValue(
          FConstructorValue, AArguments, Instance);
        ConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
      finally
        FVM.FCurrentConstructorSuperCalled := PreviousConstructorSuperCalled;
      end;
      if FConstructorValue is TGocciaBytecodeFunctionValue then
        ConstructorThisValue := RegisterToValue(FVM.FLastClosureThisValue)
      else
        ConstructorThisValue := Instance;
      ApplyOwnConstructorResult(ConstructedValue, ConstructorThisValue);
      RequireDerivedConstructorThisInitialized(ConstructedValue);
      if Assigned(InitializerReplayReceiver) and
         (Instance = InitializerReplayReceiver) then
      begin
        if not HasBytecodePrivateInitializersApplied(Instance, Self) then
        begin
          FVM.RunClassInitializers(Self, Instance, False);
          StampBytecodePrivateInitializersApplied(Instance, Self);
        end;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
    end;
  end
  else
  begin
    RootedInstance := Instance;
    InitializerReplayReceiver := nil;
    TGarbageCollector.Instance.AddTempRoot(RootedInstance);
    try
      PreviousConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
      FVM.FCurrentConstructorSuperCalled := False;
      try
        ConstructorToCall := nil;
        if Assigned(NativeInstance) then
        begin
          // The native superclass constructor has already allocated and
          // selected the correct exotic receiver above. Re-entering implicit
          // super here would allocate a second receiver using the superclass
          // as newTarget and replace the derived instance.
          if (Instance is TGocciaInstanceValue) and
             not NativeInstanceConstructedByNativeSuper then
          begin
            if not NativeInstanceInitialized then
            begin
              TGocciaInstanceValue(Instance).InitializeNativeFromArguments(
                AArguments);
              TGocciaInstanceValue(Instance).FinalizeNativeFromArguments(
                AArguments);
            end;
          end;
        end
        else
        begin
          ImplicitSuperClass := SuperClass;
          if GetConstructorPrototype is TGocciaClassValue then
            ImplicitSuperClass := TGocciaClassValue(GetConstructorPrototype);

          if (ImplicitSuperClass is TGocciaVMClassValue) and
                  Assigned(TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue) then
          begin
            FVM.RunClassInitializers(ImplicitSuperClass, Instance);
            if Assigned(ANewTarget) then
              TGocciaVMClassValue(ImplicitSuperClass).FVM.FPendingNewTarget := ANewTarget
            else
              TGocciaVMClassValue(ImplicitSuperClass).FVM.FPendingNewTarget := Self;
            ConstructedValue := TGocciaVMClassValue(ImplicitSuperClass).FVM.InvokeFunctionValue(
              TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue,
              AArguments, Instance);
            if TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue is TGocciaBytecodeFunctionValue then
              ConstructorThisValue := RegisterToValue(
                TGocciaVMClassValue(ImplicitSuperClass).FVM.FLastClosureThisValue)
            else
              ConstructorThisValue := Instance;
            ValidateClassConstructorReturn(ImplicitSuperClass, ConstructedValue);
            if IsUndefinedConstructedValue(ConstructedValue) then
              ApplyReplacementResult(ConstructorThisValue)
            else
              ApplyReplacementResult(ConstructedValue);
          end
          else
          begin
            if Assigned(ImplicitSuperClass) then
              ConstructorToCall := ImplicitSuperClass.ConstructorMethod;

            if Assigned(ConstructorToCall) then
            begin
              FVM.RunClassInitializers(ImplicitSuperClass, Instance);
              if Assigned(ANewTarget) then
                ConstructedValue := ConstructorToCall.CallWithThisValue(
                  AArguments, Instance, ConstructorThisValue, ANewTarget)
              else
                ConstructedValue := ConstructorToCall.CallWithThisValue(
                  AArguments, Instance, ConstructorThisValue, Self);
              ValidateClassConstructorReturn(ImplicitSuperClass, ConstructedValue);
              if IsUndefinedConstructedValue(ConstructedValue) then
                ApplyReplacementResult(ConstructorThisValue)
              else
                ApplyReplacementResult(ConstructedValue);
            end
            else if Assigned(ImplicitSuperClass) then
            begin
              ConstructedValue := FVM.InvokeImplicitSuperInitialization(
                ImplicitSuperClass, Instance, AArguments);
              ApplyReplacementResult(ConstructedValue);
            end
            else if Assigned(NativeSuperConstructor) and
                    (NativeSuperConstructor = TGocciaFunctionBase.GetSharedPrototype) then
              ThrowTypeError('Super constructor is not a constructor',
                SSuggestNotConstructorType)
            else if Assigned(NativeSuperConstructor) and
                    (NativeSuperConstructor is TGocciaFunctionBase) and
                    not (NativeSuperConstructor is TGocciaNativeFunctionValue) then
            begin
              ConstructedValue := InvokeConstructableWithReceiver(
                NativeSuperConstructor, AArguments, Instance);
              ApplyReplacementResult(ConstructedValue);
            end
            else if (not Assigned(SuperClass)) and
                    (not Assigned(NativeSuperConstructor)) and
                    (Prototype.Prototype = nil) then
              ThrowTypeError('Super constructor is not a constructor',
                SSuggestNotConstructorType)
            else if Instance is TGocciaInstanceValue then
            begin
              TGocciaInstanceValue(Instance).InitializeNativeFromArguments(AArguments);
              TGocciaInstanceValue(Instance).FinalizeNativeFromArguments(AArguments);
            end;
          end;
        end;
      finally
        FVM.FCurrentConstructorSuperCalled := PreviousConstructorSuperCalled;
      end;

      if not Assigned(InitializerReplayReceiver) or
         (Instance <> InitializerReplayReceiver) then
        if (not HasDerivedConstructorReturnRestriction) or
           Assigned(NativeInstance) then
          FVM.RunClassInitializers(Self, Instance);

      if Assigned(InitializerReplayReceiver) and
         (Instance = InitializerReplayReceiver) then
      begin
        if HasBytecodePrivateInitializersApplied(Instance, Self) then
        begin
          if not Assigned(FConstructorValue) then
            ThrowTypeError('Cannot initialize private elements twice',
              SSuggestPrivateFieldAccess);
        end
        else
        begin
          FVM.RunClassInitializers(Self, Instance, False);
          StampBytecodePrivateInitializersApplied(Instance, Self);
        end;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
    end;
  end;

  Result := Instance;
end;

function TGocciaVMClassValue.InstantiateRegisters(
  const AArguments: TGocciaRegisterArray): TGocciaRegister;
var
  Instance: TGocciaObjectValue;
  RootedInstance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  ImplicitSuperClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  ConstructorToCall: TGocciaMethodValue;
  BoxedArgs: TGocciaArgumentsCollection;
  BytecodeConstructor: TGocciaBytecodeFunctionValue;
  BytecodeSuperConstructor: TGocciaBytecodeFunctionValue;
  ConstructedValue: TGocciaValue;
  ConstructorThisValue: TGocciaValue;
  ReturnRegister: TGocciaRegister;
  ConstructorThisRegister: TGocciaRegister;
  InitializerReplayReceiver: TGocciaObjectValue;
  PreviousConstructorSuperCalled: Boolean;
  ConstructorSuperCalled: Boolean;
  NativeInstanceConstructedByNativeSuper: Boolean;
  procedure EnsureBoxedArgs;
  begin
    if not Assigned(BoxedArgs) then
      BoxedArgs := FVM.MaterializeArguments(AArguments);
  end;
  function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
  end;
  function IsUndefinedConstructedRegister(const AValue: TGocciaRegister): Boolean;
  begin
    Result := AValue.Kind in [grkUndefined, grkHole];
  end;
  function HasDerivedConstructorReturnRestriction: Boolean;
  begin
    Result := Assigned(SuperClass) or Assigned(NativeSuperConstructor);
  end;
  function ClassRequiresObjectConstructorReturn(
    const AClassValue: TGocciaClassValue): Boolean;
  begin
    Result := Assigned(AClassValue) and
      (Assigned(AClassValue.SuperClass) or
       Assigned(AClassValue.NativeSuperConstructor));
  end;
  procedure ValidateClassConstructorReturn(
    const AClassValue: TGocciaClassValue; const AValue: TGocciaValue);
  begin
    if ClassRequiresObjectConstructorReturn(AClassValue) and
       not (AValue is TGocciaObjectValue) and
       not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure ValidateClassConstructorRegister(
    const AClassValue: TGocciaClassValue; const AValue: TGocciaRegister);
  begin
    if ClassRequiresObjectConstructorReturn(AClassValue) and
       not ((AValue.Kind = grkObject) and
            (AValue.ObjectValue is TGocciaObjectValue)) and
       not IsUndefinedConstructedRegister(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure SetFinalInstance(const AInstance: TGocciaObjectValue);
  begin
    if (not Assigned(AInstance)) or (AInstance = Instance) then
      Exit;
    TGarbageCollector.Instance.AddTempRoot(AInstance);
    TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
    RootedInstance := AInstance;
    Instance := AInstance;
  end;
  procedure ApplyOwnConstructorThisRegister(const AValue: TGocciaRegister);
  var
    ThisObject: TGocciaObjectValue;
  begin
    if (AValue.Kind = grkObject) and
       (AValue.ObjectValue is TGocciaObjectValue) and
       (AValue.ObjectValue <> Instance) then
    begin
      ThisObject := TGocciaObjectValue(AValue.ObjectValue);
      SetFinalInstance(ThisObject);
      InitializerReplayReceiver := ThisObject;
    end;
  end;
  procedure ApplyOwnConstructorResult(const AValue: TGocciaValue);
  var
    ReturnObject: TGocciaObjectValue;
  begin
    if AValue is TGocciaObjectValue then
    begin
      ReturnObject := TGocciaObjectValue(AValue);
      if ReturnObject <> Instance then
      begin
        SetFinalInstance(ReturnObject);
        if ReturnObject <> InitializerReplayReceiver then
          InitializerReplayReceiver := nil;
      end;
    end
    else if HasDerivedConstructorReturnRestriction and
            not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure ApplyOwnConstructorRegister(const AValue: TGocciaRegister);
  var
    ReturnObject: TGocciaObjectValue;
  begin
    if (AValue.Kind = grkObject) and
       (AValue.ObjectValue is TGocciaObjectValue) then
    begin
      ReturnObject := TGocciaObjectValue(AValue.ObjectValue);
      if ReturnObject <> Instance then
      begin
        SetFinalInstance(ReturnObject);
        if ReturnObject <> InitializerReplayReceiver then
          InitializerReplayReceiver := nil;
      end;
    end
    else if HasDerivedConstructorReturnRestriction and
            not IsUndefinedConstructedRegister(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure RequireDerivedConstructorThisInitializedValue(
    const AValue: TGocciaValue);
  begin
    if HasDerivedConstructorReturnRestriction and
       IsUndefinedConstructedValue(AValue) and
       not ConstructorSuperCalled then
      ThrowReferenceError(
        'Must call super constructor before returning from derived constructor');
  end;
  procedure RequireDerivedConstructorThisInitializedRegister(
    const AValue: TGocciaRegister);
  begin
    if HasDerivedConstructorReturnRestriction and
       IsUndefinedConstructedRegister(AValue) and
       not ConstructorSuperCalled then
      ThrowReferenceError(
        'Must call super constructor before returning from derived constructor');
  end;
  procedure ApplyReplacementResult(const AValue: TGocciaValue);
  begin
    if AValue is TGocciaObjectValue then
    begin
      if TGocciaObjectValue(AValue) = Instance then
        Exit;
      SetFinalInstance(TGocciaObjectValue(AValue));
      InitializerReplayReceiver := Instance;
    end;
  end;
  procedure ApplyReplacementRegister(const AValue: TGocciaRegister);
  begin
    if (AValue.Kind = grkObject) and
       (AValue.ObjectValue is TGocciaObjectValue) then
    begin
      if TGocciaObjectValue(AValue.ObjectValue) = Instance then
        Exit;
      SetFinalInstance(TGocciaObjectValue(AValue.ObjectValue));
      InitializerReplayReceiver := Instance;
    end;
  end;
begin
  CheckExecutionTimeout;
  CheckInstructionLimit;
  BoxedArgs := nil;
  try
    NativeInstance := nil;
    NativeInstanceConstructedByNativeSuper := False;
    if not (Assigned(FConstructorValue) and HasDerivedConstructorReturnRestriction) then
    begin
      WalkClass := Self;
      while Assigned(WalkClass) do
      begin
        if not (WalkClass is TGocciaVMClassValue) then
        begin
          EnsureBoxedArgs;
          NativeInstance := WalkClass.CreateNativeInstance(BoxedArgs);
          NativeInstanceConstructedByNativeSuper := False;
        end
        else if Assigned(TGocciaVMClassValue(WalkClass).NativeSuperConstructor) then
        begin
          EnsureBoxedArgs;
          NativeInstance := TGocciaVMClassValue(WalkClass)
            .CreateNativeInstanceWithNewTarget(BoxedArgs, Self);
          NativeInstanceConstructedByNativeSuper := Assigned(NativeInstance);
        end;
        if Assigned(NativeInstance) then
          Break;
        WalkClass := WalkClass.SuperClass;
      end;
    end;

    if Assigned(NativeInstance) then
    begin
      Instance := NativeInstance;
      Instance.Prototype := Prototype;
      if NativeInstance is TGocciaInstanceValue then
        TGocciaInstanceValue(NativeInstance).ClassValue := Self;
    end
    else
    begin
      Instance := TGocciaInstanceValue.Create(Self);
      Instance.Prototype := Prototype;
    end;

    RootedInstance := Instance;
    InitializerReplayReceiver := nil;
    TGarbageCollector.Instance.AddTempRoot(RootedInstance);
    try
      if Assigned(FConstructorValue) then
      begin
        if not HasDerivedConstructorReturnRestriction then
          FVM.RunClassInitializers(Self, Instance);
        FVM.FPendingNewTarget := Self;
        PreviousConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
        FVM.FCurrentConstructorSuperCalled := False;
        if FConstructorValue is TGocciaBytecodeFunctionValue then
        begin
          try
            BytecodeConstructor := TGocciaBytecodeFunctionValue(FConstructorValue);
            if Assigned(BytecodeConstructor.FClosure) and
               Assigned(BytecodeConstructor.FClosure.Template) and
               (not BytecodeConstructor.FClosure.Template.IsAsync) then
            begin
              ReturnRegister := FVM.ExecuteClosureRegisters(
                BytecodeConstructor.FClosure, RegisterObject(Instance),
                AArguments);
              ConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
              ConstructorThisRegister := FVM.FLastClosureThisValue;
              ApplyOwnConstructorThisRegister(ConstructorThisRegister);
              ApplyOwnConstructorRegister(ReturnRegister);
              RequireDerivedConstructorThisInitializedRegister(ReturnRegister);
            end
            else
            begin
              EnsureBoxedArgs;
              ConstructedValue := FVM.InvokeFunctionValue(
                FConstructorValue, BoxedArgs, Instance);
              ConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
              if FConstructorValue is TGocciaBytecodeFunctionValue then
                ApplyOwnConstructorThisRegister(FVM.FLastClosureThisValue);
              ApplyOwnConstructorResult(ConstructedValue);
              RequireDerivedConstructorThisInitializedValue(ConstructedValue);
            end;
          finally
            FVM.FCurrentConstructorSuperCalled :=
              PreviousConstructorSuperCalled;
          end;
        end
        else
        begin
          try
            EnsureBoxedArgs;
            ConstructedValue := FVM.InvokeFunctionValue(
              FConstructorValue, BoxedArgs, Instance);
            ConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
            if FConstructorValue is TGocciaBytecodeFunctionValue then
              ApplyOwnConstructorThisRegister(FVM.FLastClosureThisValue);
            ApplyOwnConstructorResult(ConstructedValue);
            RequireDerivedConstructorThisInitializedValue(ConstructedValue);
          finally
            FVM.FCurrentConstructorSuperCalled :=
              PreviousConstructorSuperCalled;
          end;
        end;
      end
      else
      begin
        PreviousConstructorSuperCalled := FVM.FCurrentConstructorSuperCalled;
        FVM.FCurrentConstructorSuperCalled := False;
        try
          ConstructorToCall := nil;

          if Assigned(NativeInstance) then
          begin
            // The native superclass constructor has already allocated and
            // selected the correct exotic receiver above. Re-entering implicit
            // super here would allocate a second receiver using the superclass
            // as newTarget and replace the derived instance.
            EnsureBoxedArgs;
            if (Instance is TGocciaInstanceValue) and
               not NativeInstanceConstructedByNativeSuper then
            begin
              TGocciaInstanceValue(Instance).InitializeNativeFromArguments(BoxedArgs);
              TGocciaInstanceValue(Instance).FinalizeNativeFromArguments(BoxedArgs);
            end;
          end
          else
          begin
            ImplicitSuperClass := SuperClass;
            if GetConstructorPrototype is TGocciaClassValue then
              ImplicitSuperClass := TGocciaClassValue(GetConstructorPrototype);

            if (ImplicitSuperClass is TGocciaVMClassValue) and
                    Assigned(TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue) then
            begin
              TGocciaVMClassValue(ImplicitSuperClass).FVM.RunClassInitializers(
                ImplicitSuperClass, Instance);
              TGocciaVMClassValue(ImplicitSuperClass).FVM.FPendingNewTarget := Self;
              if TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue is TGocciaBytecodeFunctionValue then
              begin
                BytecodeSuperConstructor := TGocciaBytecodeFunctionValue(
                  TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue);
                if Assigned(BytecodeSuperConstructor.FClosure) and
                   Assigned(BytecodeSuperConstructor.FClosure.Template) and
                   (not BytecodeSuperConstructor.FClosure.Template.IsAsync) then
                begin
                  ReturnRegister := TGocciaVMClassValue(ImplicitSuperClass).FVM.ExecuteClosureRegisters(
                    BytecodeSuperConstructor.FClosure,
                    RegisterObject(Instance), AArguments);
                  ConstructorThisRegister :=
                    TGocciaVMClassValue(ImplicitSuperClass).FVM.FLastClosureThisValue;
                  ValidateClassConstructorRegister(ImplicitSuperClass, ReturnRegister);
                  if IsUndefinedConstructedRegister(ReturnRegister) then
                    ApplyReplacementRegister(ConstructorThisRegister)
                  else
                    ApplyReplacementRegister(ReturnRegister);
                end
                else
                begin
                  EnsureBoxedArgs;
                  ConstructedValue := TGocciaVMClassValue(ImplicitSuperClass).FVM.InvokeFunctionValue(
                    TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue,
                    BoxedArgs, Instance);
                  ConstructorThisRegister :=
                    TGocciaVMClassValue(ImplicitSuperClass).FVM.FLastClosureThisValue;
                  ValidateClassConstructorReturn(ImplicitSuperClass, ConstructedValue);
                  if IsUndefinedConstructedValue(ConstructedValue) then
                    ApplyReplacementRegister(ConstructorThisRegister)
                  else
                    ApplyReplacementResult(ConstructedValue);
                end;
              end
              else
              begin
                EnsureBoxedArgs;
                ConstructedValue := TGocciaVMClassValue(ImplicitSuperClass).FVM.InvokeFunctionValue(
                  TGocciaVMClassValue(ImplicitSuperClass).FConstructorValue,
                  BoxedArgs, Instance);
                ValidateClassConstructorReturn(ImplicitSuperClass, ConstructedValue);
                ApplyReplacementResult(ConstructedValue);
              end;
            end
            else
            begin
              if Assigned(ImplicitSuperClass) then
                ConstructorToCall := ImplicitSuperClass.ConstructorMethod;

              if Assigned(ConstructorToCall) then
              begin
                EnsureBoxedArgs;
                FVM.RunClassInitializers(ImplicitSuperClass, Instance);
                ConstructedValue := ConstructorToCall.CallWithThisValue(
                  BoxedArgs, Instance, ConstructorThisValue, Self);
                ValidateClassConstructorReturn(ImplicitSuperClass, ConstructedValue);
                if IsUndefinedConstructedValue(ConstructedValue) then
                  ApplyReplacementResult(ConstructorThisValue)
                else
                  ApplyReplacementResult(ConstructedValue);
              end
              else if Assigned(ImplicitSuperClass) then
              begin
                ConstructedValue := FVM.InvokeImplicitSuperInitializationRegisters(
                  ImplicitSuperClass, Instance, AArguments);
                ApplyReplacementResult(ConstructedValue);
              end
              else if Assigned(NativeSuperConstructor) and
                      (NativeSuperConstructor = TGocciaFunctionBase.GetSharedPrototype) then
                ThrowTypeError('Super constructor is not a constructor',
                  SSuggestNotConstructorType)
              else if Assigned(NativeSuperConstructor) and
                      (NativeSuperConstructor is TGocciaFunctionBase) and
                      not (NativeSuperConstructor is TGocciaNativeFunctionValue) then
              begin
                EnsureBoxedArgs;
                ConstructedValue := InvokeConstructableWithReceiver(
                  NativeSuperConstructor, BoxedArgs, Instance);
                ApplyReplacementResult(ConstructedValue);
              end
              else
              begin
                if (not Assigned(SuperClass)) and
                   (not Assigned(NativeSuperConstructor)) and
                   (Prototype.Prototype = nil) then
                  ThrowTypeError('Super constructor is not a constructor',
                    SSuggestNotConstructorType);
                EnsureBoxedArgs;
                if Instance is TGocciaInstanceValue then
                begin
                  TGocciaInstanceValue(Instance).InitializeNativeFromArguments(BoxedArgs);
                  TGocciaInstanceValue(Instance).FinalizeNativeFromArguments(BoxedArgs);
                end;
              end;
            end;
          end;
        finally
          FVM.FCurrentConstructorSuperCalled := PreviousConstructorSuperCalled;
        end;

        if not Assigned(InitializerReplayReceiver) or
           (Instance <> InitializerReplayReceiver) then
          FVM.RunClassInitializers(Self, Instance);
      end;
      if Assigned(InitializerReplayReceiver) and
         (Instance = InitializerReplayReceiver) then
      begin
        if HasBytecodePrivateInitializersApplied(Instance, Self) then
        begin
          if not Assigned(FConstructorValue) then
            ThrowTypeError('Cannot initialize private elements twice',
              SSuggestPrivateFieldAccess);
        end
        else
        begin
          FVM.RunClassInitializers(Self, Instance, False);
          StampBytecodePrivateInitializersApplied(Instance, Self);
        end;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(RootedInstance);
    end;

    Result := RegisterObject(Instance);
  finally
    if Assigned(BoxedArgs) then
      FVM.ReleaseArguments(BoxedArgs);
  end;
end;

function TGocciaVMClassValue.GetProperty(const AName: string): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Getter: TGocciaFunctionBase;
  BytecodeGetter: TGocciaBytecodeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  Getter := nil;
  Descriptor := inherited GetOwnPropertyDescriptor(AName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
    if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaFunctionBase then
      Getter := TGocciaFunctionBase(TGocciaPropertyDescriptorAccessor(Descriptor).Getter);

  if Assigned(Getter) then
  begin
    if (Getter is TGocciaBytecodeFunctionValue) then
    begin
      BytecodeGetter := TGocciaBytecodeFunctionValue(Getter);
      if Assigned(BytecodeGetter.FClosure) and
         Assigned(BytecodeGetter.FClosure.Template) and
         (not BytecodeGetter.FClosure.Template.IsAsync) and
         (not BytecodeGetter.FClosure.Template.IsGenerator) then
        Exit(RegisterToValue(FVM.ExecuteClosureRegisters(
          BytecodeGetter.FClosure, RegisterObject(Self), [])));
    end;
    Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
    try
      Exit(Getter.Call(Args, Self));
    finally
      Args.Free;
    end;
  end;

  Result := inherited GetProperty(AName);
end;

procedure TGocciaVMClassValue.SetProperty(const AName: string;
  const AValue: TGocciaValue);
var
  Descriptor: TGocciaPropertyDescriptor;
  Setter: TGocciaFunctionBase;
  BytecodeSetter: TGocciaBytecodeFunctionValue;
  Args: TGocciaArgumentsCollection;
begin
  Setter := nil;
  Descriptor := inherited GetOwnPropertyDescriptor(AName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
    if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaFunctionBase then
      Setter := TGocciaFunctionBase(TGocciaPropertyDescriptorAccessor(Descriptor).Setter);

  if Assigned(Setter) then
  begin
    if Setter is TGocciaBytecodeFunctionValue then
    begin
      BytecodeSetter := TGocciaBytecodeFunctionValue(Setter);
      if Assigned(BytecodeSetter.FClosure) and
         Assigned(BytecodeSetter.FClosure.Template) and
         (not BytecodeSetter.FClosure.Template.IsAsync) and
         (not BytecodeSetter.FClosure.Template.IsGenerator) then
      begin
        FVM.ExecuteClosureRegisters(BytecodeSetter.FClosure, RegisterObject(Self),
          [ValueToRegister(AValue)]);
        Exit;
      end;
    end;

    Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
    try
      Args.Add(AValue);
      Setter.Call(Args, Self);
    finally
      Args.Free;
    end;
    Exit;
  end;

  inherited SetProperty(AName, AValue);
end;

procedure TGocciaVMClassValue.SetVMConstructor(const AValue: TGocciaValue);
begin
  FConstructorValue := AValue;
  if AValue is TGocciaBytecodeFunctionValue then
    TGocciaBytecodeFunctionValue(AValue).FConstructClassValue := Self;
end;

procedure TGocciaVMClassValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FConstructorValue) then
    FConstructorValue.MarkReferences;
  if Assigned(FNativeInstanceNewTarget) then
    FNativeInstanceNewTarget.MarkReferences;
end;

procedure TGocciaVMSuperConstructorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;
  if Assigned(FNewTarget) then
    FNewTarget.MarkReferences;
  if Assigned(FCurrentCtorClass) then
    FCurrentCtorClass.MarkReferences;
end;

procedure TGocciaBytecodeFunctionValue.MarkReferences;
var
  I: Integer;
  Upvalue: TGocciaBytecodeUpvalue;
begin
  if GCMarked then Exit;
  inherited;

  if not Assigned(FClosure) then
    Exit;

  if Assigned(FClosure.HomeObject) then
    FClosure.HomeObject.MarkReferences;
  if Assigned(FClosure.HomeClass) then
    FClosure.HomeClass.MarkReferences;
  if Assigned(FClosure.NewTarget) then
    FClosure.NewTarget.MarkReferences;
  if Assigned(FClosure.GlobalScope) then
    FClosure.GlobalScope.MarkReferences;
  if Assigned(FClosure.DynamicVarScope) then
    FClosure.DynamicVarScope.MarkReferences;
  if Assigned(FConstructClassValue) then
    FConstructClassValue.MarkReferences;

  for I := 0 to FClosure.UpvalueCount - 1 do
  begin
    Upvalue := FClosure.GetUpvalue(I);
    if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
      MarkRegisterReferences(Upvalue.Cell.Value);
  end;
end;

function TGocciaVM.ConstantToValue(const AConstant: TGocciaBytecodeConstant): TGocciaValue;
begin
  case AConstant.Kind of
    bckNil:
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    bckTrue:
      Result := TGocciaBooleanLiteralValue.TrueValue;
    bckFalse:
      Result := TGocciaBooleanLiteralValue.FalseValue;
    bckInteger:
      if AConstant.IntValue = 0 then
        Result := TGocciaNumberLiteralValue.ZeroValue
      else if AConstant.IntValue = 1 then
        Result := TGocciaNumberLiteralValue.OneValue
      else
        Result := TGocciaNumberLiteralValue.Create(AConstant.IntValue);
    bckFloat:
      Result := TGocciaNumberLiteralValue.Create(AConstant.FloatValue);
    bckString:
      Result := TGocciaStringLiteralValue.Create(AConstant.StringValue);
    bckBigInt:
      Result := TGocciaBigIntValue.Create(
        TBigInteger.FromDecimalString(AConstant.StringValue));
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §13.2.8.3 GetTemplateObject — return the cached frozen template object
// for the call site identified by AConstantIndex, building and pinning it on
// the first execution of this instruction for the given function template.
// Every subsequent execution of the same OP_LOAD_CONST instruction within the
// same template returns the identical object reference, satisfying the spec's
// per-Parse-Node identity requirement without any new opcodes.
function TGocciaVM.BuildTemplateObjectConstant(const ATemplate: TGocciaFunctionTemplate;
  const AConstantIndex: Integer): TGocciaValue;
var
  Constant: TGocciaBytecodeConstant;
  Slot: Integer;
  CookedArray, RawArray: TGocciaArrayValue;
  I: Integer;
  TemplateKey: string;
begin
  Constant := ATemplate.GetConstantUnchecked(AConstantIndex);
  Slot := Integer(Constant.IntValue);
  TemplateKey := 'bc:' + IntToHex(ATemplate.TemplateSiteId, 16) + ':' +
    IntToStr(Slot);
  if Assigned(FRealm) then
    Result := TGocciaValue(FRealm.GetTemplateObject(TemplateKey))
  else
    Result := TGocciaValue(ATemplate.GetTemplateObjectCache(Slot));
  if Assigned(Result) then
    Exit;

  // First execution: build, freeze, and pin the template object
  RawArray := TGocciaArrayValue.Create;
  TGarbageCollector.Instance.AddTempRoot(RawArray);
  try
    for I := 0 to Length(Constant.RawStrings) - 1 do
      RawArray.Elements.Add(TGocciaStringLiteralValue.Create(Constant.RawStrings[I]));
    RawArray.Freeze;

    CookedArray := TGocciaArrayValue.Create;
    TGarbageCollector.Instance.AddTempRoot(CookedArray);
    try
      // TC39 Template Literal Revision: segments with malformed escapes get
      // cooked=undefined; valid segments get the resolved string value.
      for I := 0 to Length(Constant.CookedStrings) - 1 do
      begin
        if (I < Length(Constant.CookedValid)) and not Constant.CookedValid[I] then
          CookedArray.Elements.Add(TGocciaUndefinedLiteralValue.UndefinedValue)
        else
          CookedArray.Elements.Add(TGocciaStringLiteralValue.Create(Constant.CookedStrings[I]));
      end;
      // ES2026 §13.2.8.3 step 8: raw is non-enumerable, non-writable, non-configurable
      CookedArray.DefineProperty(PROP_RAW,
        TGocciaPropertyDescriptorData.Create(RawArray, []));
      // ES2026 §13.2.8.3 step 12: freeze the template object
      CookedArray.Freeze;
      // ES2026 §13.2.8.3 step 13: cache keyed by this Parse Node (template slot)
      if Assigned(FRealm) then
        FRealm.SetTemplateObject(TemplateKey, CookedArray)
      else
      begin
        TGarbageCollector.Instance.PinObject(CookedArray);
        ATemplate.SetTemplateObjectCache(Slot, CookedArray);
      end;
      Result := CookedArray;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(CookedArray);
    end;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(RawArray);
  end;
end;

function TGocciaVM.BuildRegExpLiteralConstant(const ATemplate: TGocciaFunctionTemplate;
  const AConstantIndex: Integer): TGocciaValue;
var
  Constant: TGocciaBytecodeConstant;
  Slot: Integer;
  Cached, UpdatedCache: TObject;
begin
  Constant := ATemplate.GetConstantUnchecked(AConstantIndex);
  Slot := Integer(Constant.IntValue);
  Cached := ATemplate.GetRegExpProgramCache(Slot);
  Result := CreateRegExpLiteralObject(
    Constant.StringValue, Constant.RegExpFlags, Cached, UpdatedCache);
  if UpdatedCache <> Cached then
    ATemplate.SetRegExpProgramCache(Slot, UpdatedCache);
end;

function TGocciaVM.AcquireArguments(
  const ACapacity: Integer): TGocciaArgumentsCollection;
begin
  if FArgumentPoolCount > 0 then
  begin
    Dec(FArgumentPoolCount);
    Result := FArgumentPool[FArgumentPoolCount];
    FArgumentPool[FArgumentPoolCount] := nil;
    Result.Clear;
    Result.EnsureCapacity(ACapacity);
    Exit;
  end;

  if ACapacity > 0 then
    Result := TGocciaArgumentsCollection.CreateWithCapacity(ACapacity)
  else
    Result := TGocciaArgumentsCollection.Create;
end;

procedure TGocciaVM.ReleaseArguments(
  const AArguments: TGocciaArgumentsCollection);
begin
  if not Assigned(AArguments) then
    Exit;
  AArguments.Clear;
  if FArgumentPoolCount < 32 then
  begin
    if Length(FArgumentPool) <= FArgumentPoolCount then
      SetLength(FArgumentPool, FArgumentPoolCount + 8);
    FArgumentPool[FArgumentPoolCount] := AArguments;
    Inc(FArgumentPoolCount);
  end
  else
    AArguments.Free;
end;

procedure TGocciaVM.AcquireRegisters(const ACount: Integer);
var
  NewBase, Required: Integer;
begin
  NewBase := FRegisterBase + FRegisterCount;
  Required := NewBase + ACount;
  if Required > Length(FRegisterStack) then
    SetLength(FRegisterStack, Required * 2);
  FRegisterBase := NewBase;
  FRegisterCount := ACount;
  FRegisters := @FRegisterStack[FRegisterBase];
  FillChar(FRegisters^, ACount * SizeOf(TGocciaRegister), 0);
end;

procedure TGocciaVM.AcquireLocalCells(const ACount: Integer);
var
  NewBase, Required: Integer;
begin
  NewBase := FLocalCellBase + FLocalCellCount;
  Required := NewBase + ACount;
  if Required > Length(FLocalCellStack) then
    SetLength(FLocalCellStack, Required * 2);
  FLocalCellBase := NewBase;
  FLocalCellCount := ACount;
  FLocalCells := @FLocalCellStack[FLocalCellBase];
  FillChar(FLocalCells^, ACount * SizeOf(TGocciaBytecodeCell), 0);
end;

// Acquire a fresh argument window on the arena, stack-disciplined exactly like
// AcquireRegisters/AcquireLocalCells. The window is zero-filled so the GC, which
// marks the whole live arena ([0, top)), never dereferences stale slot contents:
// this keeps argument acquisition GC-safe regardless of caller ordering, at the
// cost of touching ACount register slots. (Scope (c) of #798 evaluated removing
// this and the register/cell fills; they are GC-safety/correctness critical on
// this hot path, so the substantive wins come from the allocation removal and
// cheap stack-trace push above, not from trimming these fills.)
procedure TGocciaVM.AcquireArgumentWindow(const ACount: Integer);
var
  NewBase, Required: Integer;
begin
  NewBase := FArgumentBase + FArgCount;
  // Reserve at least one slot beyond the base so @FArgumentStack[NewBase] is a
  // valid address even for a zero-argument call (ACount = 0) under range checks.
  Required := NewBase + ACount + 1;
  if Required > Length(FArgumentStack) then
    SetLength(FArgumentStack, Required * 2);
  FArgumentBase := NewBase;
  FArgCount := ACount;
  FArguments := @FArgumentStack[FArgumentBase];
  if ACount > 0 then
    FillChar(FArguments^, ACount * SizeOf(TGocciaRegister), 0);
end;

// Copy the current frame's live argument window out of the arena into a
// detached array. Used when an owner (e.g. a generator/async continuation)
// must retain the arguments beyond the lifetime of the arena window, which is
// reused once the frame is torn down.
function TGocciaVM.CurrentArgumentsSnapshot: TGocciaRegisterArray;
var
  I: Integer;
begin
  SetLength(Result, FArgCount);
  for I := 0 to FArgCount - 1 do
    Result[I] := FArguments[I];
end;

procedure TGocciaVM.EnsureRegisterCapacity(const ACount: Integer);
var
  Growth, Required: Integer;
begin
  if ACount > FRegisterCount then
  begin
    Growth := ACount - FRegisterCount;
    Required := FRegisterBase + ACount;
    if Required > Length(FRegisterStack) then
      SetLength(FRegisterStack, Required * 2);
    FillChar(FRegisterStack[FRegisterBase + FRegisterCount],
      Growth * SizeOf(TGocciaRegister), 0);
    FRegisterCount := ACount;
    FRegisters := @FRegisterStack[FRegisterBase];
  end;
end;

procedure TGocciaVM.EnsureLocalCapacity(const ACount: Integer);
var
  Growth, Required: Integer;
begin
  if ACount > FLocalCellCount then
  begin
    Growth := ACount - FLocalCellCount;
    Required := FLocalCellBase + ACount;
    if Required > Length(FLocalCellStack) then
      SetLength(FLocalCellStack, Required * 2);
    FillChar(FLocalCellStack[FLocalCellBase + FLocalCellCount],
      Growth * SizeOf(TGocciaBytecodeCell), 0);
    FLocalCellCount := ACount;
    FLocalCells := @FLocalCellStack[FLocalCellBase];
  end;
end;

function TGocciaVM.GetLocalCell(const AIndex: Integer): TGocciaBytecodeCell;
begin
  EnsureLocalCapacity(AIndex + 1);
  if not Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex] := TGocciaBytecodeCell.Create(
      GetLocalRegister(AIndex));
  Result := FLocalCells[AIndex];
end;

function TGocciaVM.GetLocalRegister(const AIndex: Integer): TGocciaRegister;
begin
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    Exit(FLocalCells[AIndex].Value);
  if (AIndex >= 0) and (AIndex < FRegisterCount) then
    Exit(FRegisters[AIndex]);
  Result := RegisterUndefined;
end;

function TGocciaVM.GetRegister(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FRegisterCount) then
    Result := RegisterToValue(FRegisters[AIndex])
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetRegisterFast(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex < 0) or (AIndex >= FRegisterCount) then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  Result := RegisterToValue(FRegisters[AIndex]);
end;

procedure TGocciaVM.SetRegister(const AIndex: Integer; const AValue: TGocciaValue);
begin
  EnsureRegisterCapacity(AIndex + 1);
  FRegisters[AIndex] := VMValueToRegisterFast(AValue);
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and
     Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := FRegisters[AIndex];
end;

procedure TGocciaVM.SetRegisterFast(const AIndex: Integer;
  const AValue: TGocciaValue);
begin
  if (AIndex < 0) or (AIndex >= FRegisterCount) then
  begin
    SetRegister(AIndex, AValue);
    Exit;
  end;
  FRegisters[AIndex] := VMValueToRegisterFast(AValue);
  if (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := FRegisters[AIndex];
end;

procedure TGocciaVM.SetRegisterRaw(const AIndex: Integer;
  const AValue: TGocciaRegister);
begin
  EnsureRegisterCapacity(AIndex + 1);
  FRegisters[AIndex] := AValue;
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and
     Assigned(FLocalCells[AIndex]) then
    FLocalCells[AIndex].Value := AValue;
end;

procedure TGocciaVM.InstallFunctionPrototype(
  const AFunction: TGocciaObjectValue;
  const AKind: TGocciaFunctionObjectIntrinsicKind);
var
  PrototypeObj: TGocciaObjectValue;
  PrototypeFlags: TPropertyFlags;
begin
  // ES2026 §10.2.5 MakeConstructor — adds a `prototype` data property to a
  // function whose value is a fresh ordinary object.  Shape differs by kind:
  //   - Ordinary function (§15.2): prototype is { writable, !enumerable,
  //     !configurable } with an own `constructor` pointing at the function.
  //   - (Async) generator (§15.5 / §15.6): prototype is also writable, but
  //     has NO own `constructor` — per spec it inherits `constructor` from
  //     %GeneratorFunction.prototype.prototype% (which points at
  //     %GeneratorFunction.prototype%, not the specific generator function),
  //     so an own back-reference would be wrong.
  //
  // Match the lazy-init guard used by OP_NEW_OBJECT — the bytecode VM can be
  // exercised outside the normal engine bootstrap (e.g. Goccia.VM.Test.pas),
  // so the realm slot may not be primed yet on the first OP_CLOSURE.
  EnsureVMObjectPrototypeInitialized;
  if AKind in [foikGenerator, foikAsyncGenerator] then
    PrototypeObj := TGocciaObjectValue.Create(VMGeneratorObjectPrototype(AKind))
  else
    PrototypeObj := TGocciaObjectValue.Create(
      TGocciaObjectValue.SharedObjectPrototype);
  PrototypeFlags := [pfWritable];
  if not (AKind in [foikGenerator, foikAsyncGenerator]) then
  begin
    PrototypeObj.DefineProperty(PROP_CONSTRUCTOR,
      TGocciaPropertyDescriptorData.Create(AFunction, [pfWritable, pfConfigurable]));
  end;
  AFunction.DefineProperty(PROP_PROTOTYPE,
    TGocciaPropertyDescriptorData.Create(PrototypeObj, PrototypeFlags));
end;

procedure SetBytecodeHomeObject(const AFunctionValue: TGocciaValue;
  const AHomeObject: TGocciaValue; const AStaticHome: Boolean = False);
var
  EffectiveHomeObject: TGocciaObjectValue;
  EffectiveHomeClass: TGocciaClassValue;
  Closure: TGocciaBytecodeClosure;
begin
  if AHomeObject is TGocciaClassValue then
  begin
    if AStaticHome then
      EffectiveHomeObject := TGocciaObjectValue(AHomeObject)
    else
      EffectiveHomeObject := TGocciaClassValue(AHomeObject).Prototype;
    EffectiveHomeClass := TGocciaClassValue(AHomeObject);
  end
  else if AHomeObject is TGocciaObjectValue then
  begin
    EffectiveHomeObject := TGocciaObjectValue(AHomeObject);
    EffectiveHomeClass := nil;
  end
  else
  begin
    EffectiveHomeObject := nil;
    EffectiveHomeClass := nil;
  end;

  if not ((AFunctionValue is TGocciaBytecodeFunctionValue) and
     Assigned(EffectiveHomeObject)) then
    Exit;

  Closure := TGocciaBytecodeFunctionValue(AFunctionValue).FClosure;
  if not Assigned(Closure) then
    Exit;

  if not Assigned(Closure.HomeObject) then
    Closure.HomeObject := EffectiveHomeObject;
  if Assigned(EffectiveHomeClass) and not Assigned(Closure.HomeClass) and
     (Closure.HomeObject = EffectiveHomeObject) then
    Closure.HomeClass := EffectiveHomeClass;
end;

procedure DeclareBytecodePrivateNameForClass(const AClassValue: TGocciaValue;
  const AName: string; const AUseRuntimeKey: Boolean = False);
var
  SourceName: string;
  InternalName: string;
begin
  if not (AClassValue is TGocciaClassValue) then
    Exit;
  SourceName := BytecodePrivateSourceName(AName);
  if SourceName <> '' then
  begin
    if AUseRuntimeKey then
      InternalName := BytecodePrivateRuntimeKey(AName,
        TGocciaClassValue(AClassValue).PrivateBrandToken)
    else
      InternalName := AName;
    TGocciaClassValue(AClassValue).DeclarePrivateName(SourceName,
      InternalName);
  end;
end;

procedure DeclareBytecodePrivateNamesFromTemplate(
  const AClassValue: TGocciaValue; const ATemplate: TGocciaFunctionTemplate);
var
  ConstantValue: TGocciaBytecodeConstant;
  I: Integer;
begin
  if not (Assigned(ATemplate) and (AClassValue is TGocciaClassValue)) then
    Exit;

  for I := 0 to ATemplate.ConstantCount - 1 do
  begin
    ConstantValue := ATemplate.GetConstantUnchecked(I);
    if (ConstantValue.Kind = bckString) and
       IsBytecodePrivateKey(ConstantValue.StringValue) then
      DeclareBytecodePrivateNameForClass(AClassValue, ConstantValue.StringValue);
  end;
end;

function TGocciaVM.GetLocal(const AIndex: Integer): TGocciaValue;
begin
  if (AIndex >= 0) and (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    Exit(RegisterToValue(FLocalCells[AIndex].Value));
  Result := GetRegister(AIndex);
end;

function TGocciaVM.GetLocalFast(const AIndex: Integer): TGocciaValue;
begin
  if AIndex < 0 then
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  if (AIndex < FLocalCellCount) and Assigned(FLocalCells[AIndex]) then
    Exit(RegisterToValue(FLocalCells[AIndex].Value));
  Result := GetRegisterFast(AIndex);
end;

procedure TGocciaVM.SetLocal(const AIndex: Integer; const AValue: TGocciaValue);
begin
  SetRegister(AIndex, AValue);
end;

procedure TGocciaVM.SetLocalFast(const AIndex: Integer; const AValue: TGocciaValue);
begin
  if AIndex < 0 then
    Exit;
  SetRegisterFast(AIndex, AValue);
end;

procedure TGocciaVM.SetLocalRaw(const AIndex: Integer;
  const AValue: TGocciaRegister);
begin
  if AIndex < 0 then
    Exit;
  SetRegisterRaw(AIndex, AValue);
end;

function TGocciaVM.MatchesNullishKind(const AValue: TGocciaValue;
  const AKind: UInt8): Boolean;
begin
  case AKind of
    GOCCIA_NULLISH_MATCH_ANY:
      Result := (AValue is TGocciaUndefinedLiteralValue) or
                (AValue is TGocciaNullLiteralValue) or
                (AValue = TGocciaHoleValue.HoleValue);
    GOCCIA_NULLISH_MATCH_UNDEFINED:
      Result := AValue is TGocciaUndefinedLiteralValue;
    GOCCIA_NULLISH_MATCH_NULL:
      Result := AValue is TGocciaNullLiteralValue;
    GOCCIA_NULLISH_MATCH_HOLE:
      Result := AValue = TGocciaHoleValue.HoleValue;
  else
    Result := False;
  end;
end;

function CreateModuleNamespaceObject(const AModule: TGocciaModule): TGocciaObjectValue;
begin
  Result := AModule.GetNamespaceObject;
end;

function RegisterMatchesNullishKind(const AValue: TGocciaRegister;
  const AKind: UInt8): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  case AKind of
    GOCCIA_NULLISH_MATCH_ANY:
      Result := AValue.Kind in [grkUndefined, grkNull, grkHole];
    GOCCIA_NULLISH_MATCH_UNDEFINED:
      Result := AValue.Kind = grkUndefined;
    GOCCIA_NULLISH_MATCH_NULL:
      Result := AValue.Kind = grkNull;
    GOCCIA_NULLISH_MATCH_HOLE:
      Result := AValue.Kind = grkHole;
  else
    Result := False;
  end;
end;

function TGocciaVM.TryGetArrayIndex(const AKey: TGocciaValue; out AIndex: Integer): Boolean;
var
  NumberValue: TGocciaNumberLiteralValue;
begin
  Result := False;
  AIndex := -1;
  if not Assigned(AKey) or not (AKey is TGocciaNumberLiteralValue) then
    Exit;

  NumberValue := AKey.ToNumberLiteral;
  if Frac(NumberValue.Value) <> 0.0 then
    Exit;
  if (NumberValue.Value < 0) or (NumberValue.Value > MaxInt) then
    Exit;

  AIndex := Trunc(NumberValue.Value);
  Result := AIndex >= 0;
end;

function TGocciaVM.TryGetArrayIndexRegister(const AKey: TGocciaRegister;
  out AIndex: Integer): Boolean;
begin
  Result := False;
  AIndex := -1;
  case AKey.Kind of
    grkInt:
      begin
        AIndex := AKey.IntValue;
        Result := AIndex >= 0;
      end;
    grkFloat:
      begin
        if Frac(AKey.FloatValue) <> 0.0 then
          Exit;
        if (AKey.FloatValue < 0) or (AKey.FloatValue > MaxInt) then
          Exit;
        AIndex := Trunc(AKey.FloatValue);
        Result := AIndex >= 0;
      end;
  else
    Result := TryGetArrayIndex(RegisterToValue(AKey), AIndex);
  end;
end;

function TGocciaVM.KeyToPropertyName(const AKey: TGocciaValue): string;
begin
  if not Assigned(AKey) then
    Exit('');
  Result := AKey.ToStringLiteral.Value;
end;

function TGocciaVM.KeyToPropertyNameRegister(const AKey: TGocciaRegister): string;
begin
  case AKey.Kind of
    grkUndefined:
      Result := 'undefined';
    grkNull:
      Result := 'null';
    grkBoolean:
      if AKey.BoolValue then
        Result := 'true'
      else
        Result := 'false';
    grkInt:
      Result := IntToStr(AKey.IntValue);
    grkFloat:
      Result := VMRegisterToStringFast(AKey).Value;
    grkObject:
      if Assigned(AKey.ObjectValue) then
        Result := VMRegisterToStringFast(AKey).Value
      else
        Result := '';
  else
    Result := '';
  end;
end;

function TGocciaVM.TryResolveObjectKey(const AKeyReg: TGocciaRegister; out AResolved: TGocciaValue): Boolean;
begin
  Result := (AKeyReg.Kind = grkObject) and (AKeyReg.ObjectValue is TGocciaObjectValue);
  if Result then
    AResolved := ToPropertyKey(AKeyReg.ObjectValue);
end;

const
  // Per-opcode option sets for the shared computed property access cores.
  // Primitive receivers share ExecGetComputedPropertyFallback across both
  // get opcodes — no per-opcode differences remain there since #748.
  ELEMENT_GET_OPTIONS: TGocciaComputedAccessOptions =
    [caoThrowOnNullUndefined];                                      // OP_ARRAY_GET
  MEMBER_GET_OPTIONS: TGocciaComputedAccessOptions =
    [caoHandlePrivateKeys, caoLiteralFastPath];                     // OP_GET_INDEX
  ELEMENT_SET_OPTIONS: TGocciaComputedAccessOptions = [];           // OP_ARRAY_SET
  MEMBER_SET_OPTIONS: TGocciaComputedAccessOptions =
    [caoClassDefineSemantics, caoHomeObjectAllReceivers,
     caoHandlePrivateKeys];                                         // OP_SET_INDEX

function TGocciaVM.ClassifyPropertyKey(const AKeyReg: TGocciaRegister;
  const AProbeArrayIndex: Boolean): TGocciaPropertyKey;
var
  Resolved: TGocciaValue;
begin
  Result.Index := -1;
  Result.Symbol := nil;
  Result.Name := '';
  Result.FromObjectKey := False;

  if (AKeyReg.Kind = grkObject) and
     (AKeyReg.ObjectValue is TGocciaSymbolValue) then
  begin
    Result.Kind := pkkSymbol;
    Result.Symbol := TGocciaSymbolValue(AKeyReg.ObjectValue);
    Exit;
  end;

  // Side-effect-free: TryGetArrayIndex only accepts number literals, so
  // probing never invokes user toString/valueOf on object keys.
  if AProbeArrayIndex and TryGetArrayIndexRegister(AKeyReg, Result.Index) then
  begin
    Result.Kind := pkkIndex;
    Exit;
  end;

  if TryResolveObjectKey(AKeyReg, Resolved) then
  begin
    Result.FromObjectKey := True;
    if Resolved is TGocciaSymbolValue then
    begin
      Result.Kind := pkkSymbol;
      Result.Symbol := TGocciaSymbolValue(Resolved);
    end
    else
    begin
      Result.Kind := pkkName;
      Result.Name := TGocciaStringLiteralValue(Resolved).Value;
    end;
    Exit;
  end;

  Result.Kind := pkkName;
  Result.Name := KeyToPropertyNameRegister(AKeyReg);
end;

function TGocciaVM.PropertyKeyName(const AKey: TGocciaPropertyKey): string;
begin
  if AKey.Kind = pkkIndex then
    Result := IntToStr(AKey.Index)
  else
    Result := AKey.Name;
end;

procedure TGocciaVM.ExecGetComputedProperty(const ADest: Integer;
  AObjReg, AKeyReg: TGocciaRegister;
  const AOptions: TGocciaComputedAccessOptions);
var
  Key: TGocciaPropertyKey;
  KeyName: string;
  ReceiverArray: TGocciaArrayValue;
  FastIndex: Integer;
  FastElement: Double;
begin
  if (caoThrowOnNullUndefined in AOptions) and
     (AObjReg.Kind in [grkUndefined, grkNull]) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess)
  else if (AObjReg.Kind = grkObject) and
          (AObjReg.ObjectValue is TGocciaTypedArrayValue) and
          TryGetArrayIndexRegister(AKeyReg, FastIndex) and
          TGocciaTypedArrayValue(AObjReg.ObjectValue)
            .TryReadIndexedScalar(FastIndex, FastElement) then
    // Typed-array unboxed element read: the element goes straight into the
    // destination register as a scalar, with no heap TGocciaNumberLiteralValue and
    // no IntToStr index name. Non-index keys, BigInt kinds, and out-of-range indices
    // fall through to the generic object branch below, which handles length, methods,
    // `undefined` for out-of-range reads, BigInt boxing, and symbol keys unchanged.
    FRegisters[ADest] := RegisterFromDouble(FastElement)
  else if (AObjReg.Kind = grkObject) and
          (AObjReg.ObjectValue is TGocciaArrayValue) then
  begin
    ReceiverArray := TGocciaArrayValue(AObjReg.ObjectValue);
    Key := ClassifyPropertyKey(AKeyReg, True);
    case Key.Kind of
      pkkSymbol:
        SetRegister(ADest, ReceiverArray.GetSymbolProperty(Key.Symbol));
      pkkIndex:
        if (Key.Index >= 0) and
           (Key.Index < ReceiverArray.Elements.Count) and
           (ReceiverArray.Elements[Key.Index] <>
            TGocciaHoleValue.HoleValue) then
          FRegisters[ADest] := VMValueToRegisterFast(
            ReceiverArray.Elements[Key.Index])
        else
          // Hole, out-of-range, or accessor-shadowed slot: take the slow
          // path so accessor descriptors and prototype lookups run.
          SetRegister(ADest, ReceiverArray.GetProperty(IntToStr(Key.Index)));
    else
      SetRegister(ADest, ReceiverArray.GetProperty(Key.Name));
    end;
  end
  else if (AObjReg.Kind = grkObject) and
          (AObjReg.ObjectValue is TGocciaClassValue) then
  begin
    // Classes must be matched before TGocciaObjectValue so that static
    // symbol-keyed properties (FStaticSymbolDescriptors) and the
    // superclass walk are reached. TGocciaClassValue.GetSymbolProperty
    // is not virtual on the base, so a TGocciaObjectValue cast would
    // bypass them.
    Key := ClassifyPropertyKey(AKeyReg, False);
    if Key.Kind = pkkSymbol then
      SetRegister(ADest, TGocciaClassValue(AObjReg.ObjectValue)
        .GetSymbolProperty(Key.Symbol))
    else
    begin
      KeyName := PropertyKeyName(Key);
      if (caoHandlePrivateKeys in AOptions) and
         IsBytecodePrivateKey(KeyName) then
        SetRegister(ADest, GetPropertyValue(AObjReg.ObjectValue, KeyName))
      else
        SetRegister(ADest, TGocciaClassValue(AObjReg.ObjectValue)
          .GetProperty(KeyName));
    end;
  end
  else if (AObjReg.Kind = grkObject) and
          (AObjReg.ObjectValue is TGocciaObjectValue) then
  begin
    Key := ClassifyPropertyKey(AKeyReg, False);
    if Key.Kind = pkkSymbol then
      SetRegister(ADest, TGocciaObjectValue(AObjReg.ObjectValue)
        .GetSymbolProperty(Key.Symbol))
    else
    begin
      KeyName := PropertyKeyName(Key);
      // Private-key routing historically applies only to keys that did
      // not pass through ToPropertyKey on this arm.
      if (caoHandlePrivateKeys in AOptions) and (not Key.FromObjectKey) and
         IsBytecodePrivateKey(KeyName) then
        SetRegister(ADest, GetPropertyValue(AObjReg.ObjectValue, KeyName))
      else if (caoLiteralFastPath in AOptions) and
              (AObjReg.ObjectValue is TGocciaVMLiteralObjectValue) and
              TGocciaVMLiteralObjectValue(AObjReg.ObjectValue)
                .TryGetOwnDataPropertyFastRegister(KeyName,
                  FRegisters[ADest]) then
        { fast path already assigned }
      else
        SetRegister(ADest, TGocciaObjectValue(AObjReg.ObjectValue)
          .GetProperty(KeyName));
    end;
  end
  else
    // Primitive receivers (strings, numbers, booleans, bigints, symbols):
    // one shared boxed-lookup fallback for both computed-get opcodes,
    // keeping the unboxed string fast paths and receiver-aware accessor
    // semantics from #748. Passing this core's by-value register copies
    // keeps the fallback's const parameters safe from register-stack
    // reallocation on every target.
    ExecGetComputedPropertyFallback(ADest, AObjReg, AKeyReg);
end;

procedure TGocciaVM.ExecSetComputedProperty(const ATargetIndex: Integer;
  AKeyReg, AValueReg: TGocciaRegister;
  const AOptions: TGocciaComputedAccessOptions);
var
  Key: TGocciaPropertyKey;
  KeyName: string;
  Value: TGocciaValue;
  TargetValue: TGocciaValue;
  BoxedTarget: TGocciaObjectValue;
  FastIndex: Integer;
begin
  // Typed-array unboxed element write: a numeric-scalar value going to a valid
  // integer index stores directly, with no heap TGocciaNumberLiteralValue and no
  // IntToStr index name. ToNumber on a Number is side-effect-free, so the spec's
  // observable conversion is preserved. BigInt kinds (a Number value must throw),
  // non-index keys, and non-scalar values fall through to the boxed path below.
  if (FRegisters[ATargetIndex].Kind = grkObject) and
     (FRegisters[ATargetIndex].ObjectValue is TGocciaTypedArrayValue) and
     RegisterIsNumericScalar(AValueReg) and
     TryGetArrayIndexRegister(AKeyReg, FastIndex) and
     TGocciaTypedArrayValue(FRegisters[ATargetIndex].ObjectValue)
       .TryWriteIndexedScalar(FastIndex, RegisterToDouble(AValueReg)) then
    Exit;

  Value := RegisterToValue(AValueReg);
  if (FRegisters[ATargetIndex].Kind = grkObject) and
     (FRegisters[ATargetIndex].ObjectValue is TGocciaArrayValue) then
  begin
    if caoHomeObjectAllReceivers in AOptions then
      SetBytecodeHomeObject(Value, FRegisters[ATargetIndex].ObjectValue);
    Key := ClassifyPropertyKey(AKeyReg, True);
    case Key.Kind of
      pkkSymbol:
        TGocciaArrayValue(FRegisters[ATargetIndex].ObjectValue)
          .AssignSymbolProperty(Key.Symbol, Value);
      pkkIndex:
        TGocciaArrayValue(FRegisters[ATargetIndex].ObjectValue)
          .SetIndexProperty(Key.Index, Value);
    else
      TGocciaArrayValue(FRegisters[ATargetIndex].ObjectValue)
        .SetProperty(Key.Name, Value);
    end;
  end
  else if (FRegisters[ATargetIndex].Kind = grkObject) and
          (FRegisters[ATargetIndex].ObjectValue is TGocciaClassValue) then
  begin
    SetBytecodeHomeObject(Value, FRegisters[ATargetIndex].ObjectValue);
    Key := ClassifyPropertyKey(AKeyReg, False);
    if caoClassDefineSemantics in AOptions then
    begin
      if Key.Kind = pkkSymbol then
        TGocciaClassValue(FRegisters[ATargetIndex].ObjectValue)
          .DefineSymbolProperty(Key.Symbol,
            TGocciaPropertyDescriptorData.Create(
              Value, [pfConfigurable, pfWritable]))
      else
      begin
        KeyName := PropertyKeyName(Key);
        if (caoHandlePrivateKeys in AOptions) and
           IsBytecodePrivateKey(KeyName) then
          SetPropertyValue(FRegisters[ATargetIndex].ObjectValue, KeyName,
            Value)
        else
          TGocciaClassValue(FRegisters[ATargetIndex].ObjectValue)
            .DefineProperty(KeyName,
              TGocciaPropertyDescriptorData.Create(
                Value, [pfConfigurable, pfWritable]));
      end;
    end
    else if Key.Kind = pkkSymbol then
      TGocciaClassValue(FRegisters[ATargetIndex].ObjectValue)
        .AssignSymbolProperty(Key.Symbol, Value)
    else
      TGocciaClassValue(FRegisters[ATargetIndex].ObjectValue)
        .SetProperty(PropertyKeyName(Key), Value);
  end
  else if (FRegisters[ATargetIndex].Kind = grkObject) and
          (FRegisters[ATargetIndex].ObjectValue is TGocciaObjectValue) then
  begin
    if caoHomeObjectAllReceivers in AOptions then
      SetBytecodeHomeObject(Value, FRegisters[ATargetIndex].ObjectValue);
    Key := ClassifyPropertyKey(AKeyReg, False);
    if Key.Kind = pkkSymbol then
      TGocciaObjectValue(FRegisters[ATargetIndex].ObjectValue)
        .AssignSymbolProperty(Key.Symbol, Value)
    else
    begin
      KeyName := PropertyKeyName(Key);
      if (caoHandlePrivateKeys in AOptions) and
         IsBytecodePrivateKey(KeyName) then
        SetPropertyValue(FRegisters[ATargetIndex].ObjectValue, KeyName, Value)
      else
        TGocciaObjectValue(FRegisters[ATargetIndex].ObjectValue)
          .SetProperty(KeyName, Value);
    end;
  end
  else
  begin
    // Primitive receivers: full key classification, so object keys coerce
    // through ToPropertyKey and symbol keys (direct or resolved) take the
    // receiver-aware boxed [[Set]] — a symbol-keyed setter on the
    // primitive's prototype runs with the primitive as receiver, matching
    // the interpreter; only unboxable receivers and failed assignments
    // throw.
    Key := ClassifyPropertyKey(AKeyReg, False);
    TargetValue := GetRegister(ATargetIndex);
    if Key.Kind = pkkSymbol then
    begin
      BoxedTarget := TargetValue.Box;
      if (not Assigned(BoxedTarget)) or
         (not BoxedTarget.AssignSymbolPropertyWithReceiver(Key.Symbol, Value,
           TargetValue)) then
        ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
          SSuggestCheckNullBeforeAccess);
    end
    else
    begin
      if (caoHomeObjectAllReceivers in AOptions) and
         ((TargetValue is TGocciaClassValue) or
          (TargetValue is TGocciaObjectValue)) then
        SetBytecodeHomeObject(Value, TargetValue);
      SetPropertyValue(TargetValue, PropertyKeyName(Key), Value);
    end;
  end;
end;

procedure TGocciaVM.ExecDeleteComputedProperty(const ADest: Integer;
  AObjReg, AKeyReg: TGocciaRegister; const AThrowOnFailure: Boolean);
var
  Key: TGocciaPropertyKey;
  Receiver: TGocciaObjectValue;
  Deleted: Boolean;
  KeyName: string;
begin
  if AObjReg.Kind = grkNull then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull,
      [KeyDisplaySafe(AKeyReg)]),
      SSuggestCheckNullBeforeAccess)
  else if AObjReg.Kind = grkUndefined then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined,
      [KeyDisplaySafe(AKeyReg)]),
      SSuggestCheckNullBeforeAccess)
  else if (AObjReg.Kind = grkObject) and
          (AObjReg.ObjectValue is TGocciaObjectValue) then
  begin
    Receiver := TGocciaObjectValue(AObjReg.ObjectValue);
    Key := ClassifyPropertyKey(AKeyReg,
      AObjReg.ObjectValue is TGocciaArrayValue);
    if Key.Kind = pkkSymbol then
    begin
      // ES2026 §13.5.1.2 step 5.b: a non-configurable symbol property
      // throws TypeError on delete (mirrors EvaluateDelete's symbol path
      // in the interpreter). Pre-refactor the bytecode threw via the
      // symbol-stringification side effect; the spec-correct path keeps
      // that throw.
      Deleted := Receiver.DeleteSymbolProperty(Key.Symbol);
      if Deleted then
        FRegisters[ADest] := RegisterBoolean(True)
      else if AThrowOnFailure then
        ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
          [Key.Symbol.ToDisplayString.Value, '[object Object]']),
          SSuggestCannotDeleteNonConfigurable)
      else
        FRegisters[ADest] := RegisterBoolean(False);
      Exit;
    end;
    KeyName := PropertyKeyName(Key);
    Deleted := Receiver.DeleteProperty(KeyName);
    if Deleted then
      FRegisters[ADest] := RegisterBoolean(True)
    else if AThrowOnFailure then
      ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
        [KeyName, '[object Object]']),
        SSuggestCannotDeleteNonConfigurable)
    else
      FRegisters[ADest] := RegisterBoolean(False);
  end
  else
  begin
    // Primitive receivers: the property key still coerces (ToPropertyKey
    // side effects) before delete yields true, matching the interpreter
    // and ES2026 property-reference evaluation.
    Key := ClassifyPropertyKey(AKeyReg, False);
    if (AObjReg.Kind = grkObject) and
       (AObjReg.ObjectValue is TGocciaStringLiteralValue) and
       (Key.Kind <> pkkSymbol) then
    begin
      KeyName := PropertyKeyName(Key);
      if IsNonConfigurableStringExoticProperty(
           TGocciaStringLiteralValue(AObjReg.ObjectValue), KeyName) then
      begin
        if AThrowOnFailure then
          ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
            [KeyName,
             TGocciaStringLiteralValue(AObjReg.ObjectValue).Value]),
            SSuggestCannotDeleteNonConfigurable)
        else
          FRegisters[ADest] := RegisterBoolean(False);
        Exit;
      end;
    end;
    FRegisters[ADest] := RegisterBoolean(True);
  end;
end;

procedure TGocciaVM.ServeOwnNonDataProperty(const ADest: Integer;
  const AReceiver: TGocciaValue;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Accessor: TGocciaPropertyDescriptorAccessor;
  CallArgs: TGocciaArgumentsCollection;
begin
  // Mirrors the own-descriptor branch of TGocciaObjectValue /
  // TGocciaInstanceValue GetPropertyWithContext: callable getter invoked
  // with the receiver as this; setter-only accessors and exotic
  // descriptor kinds yield undefined.
  if ADescriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Accessor := TGocciaPropertyDescriptorAccessor(ADescriptor);
    if Assigned(Accessor.Getter) and Accessor.Getter.IsCallable then
    begin
      CallArgs := AcquireArguments(0);
      try
        // Generic callable dispatch: IsCallable does not guarantee
        // TGocciaFunctionBase (proxies, class values), so route through
        // the VM's call path instead of a static cast.
        SetRegister(ADest, InvokeFunctionValue(Accessor.Getter,
          CallArgs, AReceiver));
      finally
        ReleaseArguments(CallArgs);
      end;
      Exit;
    end;
  end;
  SetRegister(ADest, TGocciaUndefinedLiteralValue.UndefinedValue);
end;

procedure TGocciaVM.SetFunctionNameFromKey(const AFunction, AKey: TGocciaValue;
  const APrefixKind: UInt8);
var
  Prefix: string;
begin
  if not (AFunction is TGocciaObjectValue) then
    Exit;

  case APrefixKind of
    FUNCTION_NAME_PREFIX_GET:
      Prefix := 'get';
    FUNCTION_NAME_PREFIX_SET:
      Prefix := 'set';
  else
    Prefix := '';
  end;

  TGocciaObjectValue(AFunction).DefineProperty(PROP_NAME,
    TGocciaPropertyDescriptorData.Create(
      TGocciaStringLiteralValue.Create(
        FunctionNameFromPropertyKey(AKey, Prefix)), [pfConfigurable]));
end;

function TGocciaVM.KeyDisplaySafe(const AKey: TGocciaRegister): string;
begin
  case AKey.Kind of
    grkInt:
      Result := IntToStr(AKey.IntValue);
    grkFloat:
      Result := FormatDouble(AKey.FloatValue);
    grkBoolean:
      if AKey.BoolValue then Result := 'true' else Result := 'false';
    grkNull:
      Result := 'null';
    grkUndefined:
      Result := 'undefined';
    grkObject:
      if AKey.ObjectValue is TGocciaStringLiteralValue then
        Result := TGocciaStringLiteralValue(AKey.ObjectValue).Value
      else if AKey.ObjectValue is TGocciaSymbolValue then
        Result := TGocciaSymbolValue(AKey.ObjectValue).ToDisplayString.Value
      else
        Result := '<computed>';
  else
    Result := '<computed>';
  end;
end;

// IteratorClose for the raw-object form returned by GetIteratorValue
// when the source supplies its own iterator (i.e. an object with next()
// rather than a TGocciaIteratorValue).  Normal-completion variant per
// ES2024 §7.4.10 IteratorClose:
//   - step 3.b: if `return` is missing/undefined/null, return the
//     completion as-is (only these three values skip silently —
//     non-callable returns are an error);
//   - step 3.c: call return; errors propagate;
//   - step 3.d: validate the result is an Object on normal completion;
//     a primitive result is a TypeError.
procedure CloseRawIterator(const AIteratorObject: TGocciaValue);
var
  ReturnMethod, ReturnResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(AIteratorObject) then
    Exit;
  if AIteratorObject is TGocciaIteratorValue then
  begin
    CloseIterator(TGocciaIteratorValue(AIteratorObject));
    Exit;
  end;
  if not (AIteratorObject is TGocciaObjectValue) then
    Exit;
  ReturnMethod := AIteratorObject.GetProperty(PROP_RETURN);
  // §7.4.10 step 3.b: only missing/undefined/null skip.
  if not Assigned(ReturnMethod) or
     (ReturnMethod is TGocciaUndefinedLiteralValue) or
     (ReturnMethod is TGocciaNullLiteralValue) then
    Exit;
  // §7.4.10 step 3.c (implicit): a present-but-non-callable `return`
  // is a TypeError, not a silent no-op.
  if not ReturnMethod.IsCallable then
    ThrowTypeError(SErrorIteratorReturnMustBeCallable,
      SSuggestIteratorProtocol);
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    ReturnResult := InvokeCallable(ReturnMethod, CallArgs, AIteratorObject);
  finally
    CallArgs.Free;
  end;
  // §7.4.10 step 3.d: on normal completion, the IteratorResult must be
  // an Object.  Primitive results (including undefined, numbers, etc.)
  // are a TypeError.
  if (ReturnResult is TGocciaUndefinedLiteralValue)
      or (ReturnResult is TGocciaNullLiteralValue)
      or ReturnResult.IsPrimitive then
    ThrowTypeError(SErrorIteratorReturnObject,
      SSuggestIteratorResultObject);
end;

procedure CloseRawAsyncIterator(const AIteratorObject: TGocciaValue);
var
  ReturnMethod, ReturnResult: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(AIteratorObject) then
    Exit;
  if not (AIteratorObject is TGocciaObjectValue) then
    Exit;
  ReturnMethod := AIteratorObject.GetProperty(PROP_RETURN);
  if not Assigned(ReturnMethod) or
     (ReturnMethod is TGocciaUndefinedLiteralValue) or
     (ReturnMethod is TGocciaNullLiteralValue) then
    Exit;
  if not ReturnMethod.IsCallable then
    ThrowTypeError(SErrorIteratorReturnMustBeCallable,
      SSuggestIteratorProtocol);
  CallArgs := TGocciaArgumentsCollection.Create;
  try
    ReturnResult := InvokeCallable(ReturnMethod, CallArgs, AIteratorObject);
  finally
    CallArgs.Free;
  end;
  ReturnResult := AwaitValue(ReturnResult);
  if (ReturnResult is TGocciaUndefinedLiteralValue)
      or (ReturnResult is TGocciaNullLiteralValue)
      or ReturnResult.IsPrimitive then
    ThrowTypeError(SErrorIteratorReturnObject,
      SSuggestIteratorResultObject);
end;

// Abrupt-completion variant of CloseRawIterator: per ES2024 §7.4.10
// step 5, when an iteration body completes abruptly the close must
// not let iter.return()'s own errors replace the original exception.
// Mirrors CloseIteratorPreservingError in Goccia.Values.IteratorSupport
// for the TGocciaIteratorValue case.
procedure CloseRawIteratorPreservingError(const AIteratorObject: TGocciaValue;
  const AAsync: Boolean = False);
begin
  if not Assigned(AIteratorObject) then
    Exit;
  try
    if AAsync then
      CloseRawAsyncIterator(AIteratorObject)
    else
      CloseRawIterator(AIteratorObject);
  except
    // Swallow: the original abrupt completion is the one that must
    // surface to the caller.
  end;
end;

function TGocciaVM.IterableToArray(const AIterable: TGocciaValue;
  const ATryAsync: Boolean; const ALimit: Integer): TGocciaArrayValue;
var
  IteratorValue: TGocciaValue;
  DoneFlag: Boolean;
  NextMethod: TGocciaValue;
  NextResult: TGocciaValue;
  DoneValue: TGocciaValue;
  Value: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  GC: TGarbageCollector;
  ArrayRooted, IteratorRooted: Boolean;
begin
  Result := TGocciaArrayValue.Create;
  // The materialised array and any newly-synthesised iterator wrapper
  // must outlive the consume/close re-entry into JS land: DirectNext
  // calls user next(), AwaitValue may pump microtasks, CloseIterator /
  // CloseRawIterator invoke user return().  Each of those can trigger a
  // GC sweep, and if `Result` or a fresh `IteratorValue` aren't on the
  // root set, they can be reclaimed mid-flight.  Mirrors the rooting
  // already done in TryIterableToArray below.
  GC := TGarbageCollector.Instance;
  ArrayRooted := False;
  IteratorRooted := False;
  IteratorValue := nil;
  if Assigned(GC) then
  begin
    GC.AddTempRoot(Result);
    ArrayRooted := True;
  end;
  try
    // GetIteratorValue can throw (e.g. for a non-iterable source, or
    // when [Symbol.iterator] returns a non-object).  Calling it inside
    // the try/finally is what guarantees Result is unrooted on the
    // throw path — otherwise it stays permanently temp-rooted and
    // accumulates across calls.
    IteratorValue := GetIteratorValue(AIterable, ATryAsync);
    if Assigned(GC) and Assigned(IteratorValue)
        and (IteratorValue <> AIterable)
        and (IteratorValue is TGocciaObjectValue) then
    begin
      GC.AddTempRoot(IteratorValue);
      IteratorRooted := True;
    end;

    if IteratorValue is TGocciaIteratorValue then
    begin
      // ES2024 §8.5.3 IteratorBindingInitialization: when an array
      // binding pattern has no rest element, the iterator must be
      // consumed for exactly N elements and then closed via
      // IteratorClose.  Without this bound, an iterator that returns
      // done:false indefinitely allocates unboundedly during
      // destructuring (test262 ary-init-iter-close cluster).
      //
      // ALimit semantics: <0 unbounded, 0 consume zero, >0 exact.  The
      // ALimit = 0 case must close the iterator before the first
      // next() call (per the spec, step 4 fires after zero
      // BindingElementList iterations for an empty pattern).
      if ALimit = 0 then
      begin
        CloseIterator(TGocciaIteratorValue(IteratorValue));
        Exit;
      end;
      try
        repeat
          CheckExecutionTimeout;
          CheckInstructionLimit;
          NextResult := TGocciaIteratorValue(IteratorValue).DirectNext(DoneFlag);
          if not DoneFlag then
            Result.Elements.Add(NextResult);
          if (ALimit > 0) and (Result.Elements.Count >= ALimit) then
          begin
            // ES2024 §7.4.10 step 5: normal-completion IteratorClose lets
            // errors from iter.return() propagate.  Use CloseIterator (not
            // PreservingError) so test262 tests like Iterator.from →
            // Iterator.prototype.return-throws are reported correctly.
            if not DoneFlag then
              CloseIterator(TGocciaIteratorValue(IteratorValue));
            Exit;
          end;
        until DoneFlag;
      except
        // IteratorNext/IteratorStepValue mark the iterator record done before
        // propagating these failures, so the outer binding/destructuring
        // algorithm must not call IteratorClose for this path.
        raise;
      end;
      Exit;
    end;

    if IteratorValue is TGocciaObjectValue then
    begin
      // Same ALimit = 0 short-circuit as above.
      if ALimit = 0 then
      begin
        CloseRawIterator(IteratorValue);
        Exit;
      end;
      // ES2024 §7.4.2 GetIteratorDirect: NextMethod is captured ONCE
      // at iteratorRecord creation, not re-resolved per IteratorStep.
      // Hoisting the GetProperty + IsCallable validation out of the
      // loop matches the spec semantic (post-acquisition mutations of
      // `iterator.next` are ignored — §7.4.5 IteratorStep calls the
      // captured iteratorRecord.[[NextMethod]]) and avoids a redundant
      // hash lookup per iteration.  A missing/non-callable next is a
      // TypeError, not silent termination.
      NextMethod := IteratorValue.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        ThrowTypeError(SErrorIteratorNextMustBeCallable,
          SSuggestIteratorProtocol);
      try
        repeat
          CheckExecutionTimeout;
          CheckInstructionLimit;
          CallArgs := AcquireArguments;
          try
            NextResult := InvokeCallable(NextMethod, CallArgs, IteratorValue);
          finally
            ReleaseArguments(CallArgs);
          end;
          // Only async iterators yield a Promise from next().  For sync
          // iteration (the default — destructuring, spread, for-of), the
          // result is the IteratorResult object directly; calling
          // AwaitValue on it would needlessly pump microtasks and open a
          // re-entrancy window where time-of-check / time-of-use bugs can
          // surface (e.g. user code mutating the result between unwrap and
          // PROP_DONE / PROP_VALUE reads).  Mirror the spec's split between
          // §7.4.7 IteratorStep (sync) and §7.4.13 AsyncIteratorStep.
          if ATryAsync then
            NextResult := AwaitValue(NextResult);
          if NextResult.IsPrimitive then
            ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
              SSuggestIteratorResultObject);
          DoneValue := NextResult.GetProperty(PROP_DONE);
          DoneFlag := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
          if not DoneFlag then
          begin
            // Normalize missing IteratorResult.value to undefined, matching
            // the well-defined behaviour in TryIterableToArray.  Without
            // this, GetProperty(PROP_VALUE) returning nil would store nil
            // in the array, causing later .Elements[I] reads to crash or
            // misclassify a missing result as "absent" in destructuring.
            Value := NextResult.GetProperty(PROP_VALUE);
            if not Assigned(Value) then
              Value := TGocciaUndefinedLiteralValue.UndefinedValue;
            Result.Elements.Add(Value);
          end;
          if (ALimit > 0) and (Result.Elements.Count >= ALimit) then
          begin
            // ES2024 §7.4.10 step 5 (normal completion): errors from
            // iter.return() propagate as the new completion.
            if not DoneFlag then
              CloseRawIterator(IteratorValue);
            Exit;
          end;
        until DoneFlag;
      except
        // IteratorNext/IteratorStepValue mark the iterator record done before
        // propagating these failures, so the outer binding/destructuring
        // algorithm must not call IteratorClose for this path.
        raise;
      end;
      Exit;
    end;
  finally
    if IteratorRooted then
      GC.RemoveTempRoot(IteratorValue);
    if ArrayRooted then
      GC.RemoveTempRoot(Result);
  end;
end;

function TGocciaVM.TryIterableToArray(const AIterable: TGocciaValue;
  out AArray: TGocciaArrayValue): Boolean;
var
  IteratorValue, IteratorMethod, IteratorObject, NextMethod, NextResult,
    DoneValue, Value: TGocciaValue;
  IteratorSource: TGocciaObjectValue;
  DoneFlag: Boolean;
  ArrayRooted, IteratorRooted: Boolean;
  CallArgs: TGocciaArgumentsCollection;
begin
  AArray := nil;
  ArrayRooted := False;
  IteratorRooted := False;
  // NextMethod is only consumed by the OBJECT loop further down; the
  // TGocciaIteratorValue path uses DirectNext and exits early.  Init
  // here so FPC's uninitialized-local analysis is unambiguous.
  NextMethod := nil;

  if AIterable is TGocciaIteratorValue then
    IteratorValue := AIterable
  else
  begin
    if AIterable is TGocciaObjectValue then
      IteratorSource := TGocciaObjectValue(AIterable)
    else
      IteratorSource := AIterable.Box;

    if not Assigned(IteratorSource) then
      Exit(False);

    IteratorMethod := IteratorSource.GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if not Assigned(IteratorMethod) or
       (IteratorMethod is TGocciaUndefinedLiteralValue) then
      Exit(False);
    if not IteratorMethod.IsCallable then
      ThrowTypeError('Object [Symbol.iterator] must be callable');

    CallArgs := AcquireArguments;
    try
      IteratorObject := InvokeCallable(IteratorMethod, CallArgs, AIterable);
    finally
      ReleaseArguments(CallArgs);
    end;

    if IteratorObject is TGocciaIteratorValue then
      IteratorValue := IteratorObject
    else if IteratorObject is TGocciaObjectValue then
    begin
      NextMethod := IteratorObject.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
      IteratorValue := IteratorObject;
    end
    else
      ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
  end;

  AArray := TGocciaArrayValue.Create;
  ArrayRooted := (TGarbageCollector.Instance <> nil);
  if ArrayRooted then
    TGarbageCollector.Instance.AddTempRoot(AArray);
  IteratorRooted := (TGarbageCollector.Instance <> nil) and (IteratorValue <> AIterable);
  if IteratorRooted then
    TGarbageCollector.Instance.AddTempRoot(IteratorValue);
  try
    if IteratorValue is TGocciaIteratorValue then
    begin
      try
        repeat
          CheckExecutionTimeout;
          CheckInstructionLimit;
          NextResult := TGocciaIteratorValue(IteratorValue).DirectNext(DoneFlag);
          if not DoneFlag then
            AArray.Elements.Add(NextResult);
        until DoneFlag;
      except
        // If IteratorStep/IteratorNext itself throws, do not call return().
        raise;
      end;
      Exit(True);
    end;

    // ES2024 §7.4.2 GetIteratorDirect captures NextMethod ONCE at
    // iteratorRecord creation; §7.4.5 IteratorStep then calls that
    // captured reference.  NextMethod was already resolved and
    // validated at the acquisition site above (the `if IteratorObject
    // is TGocciaObjectValue` arm), so the loop just reuses it instead
    // of re-running GetProperty(PROP_NEXT) per iteration.
    try
      repeat
        CheckExecutionTimeout;
        CheckInstructionLimit;
        CallArgs := AcquireArguments;
        try
          NextResult := InvokeCallable(NextMethod, CallArgs, IteratorValue);
        finally
          ReleaseArguments(CallArgs);
        end;
        if NextResult.IsPrimitive then
          ThrowTypeError(Format(SErrorIteratorResultNotObject, [NextResult.ToStringLiteral.Value]),
            SSuggestIteratorResultObject);
        DoneValue := NextResult.GetProperty(PROP_DONE);
        DoneFlag := Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value;
        if not DoneFlag then
        begin
          Value := NextResult.GetProperty(PROP_VALUE);
          if not Assigned(Value) then
            Value := TGocciaUndefinedLiteralValue.UndefinedValue;
          AArray.Elements.Add(Value);
        end;
      until DoneFlag;
    except
      // If IteratorStep/IteratorNext itself throws, do not call return().
      raise;
    end;
    Result := True;
  finally
    if IteratorRooted then
      TGarbageCollector.Instance.RemoveTempRoot(IteratorValue);
    if ArrayRooted then
      TGarbageCollector.Instance.RemoveTempRoot(AArray);
  end;
end;

procedure TGocciaVM.SpreadObjectIntoValue(const ATarget: TGocciaObjectValue;
  const ASource: TGocciaValue);
begin
  VMCopyDataProperties(ATarget, ASource, nil);
end;

function TGocciaVM.ObjectRestValue(const ASource: TGocciaValue;
  const AExclusionKeys: TGocciaArrayValue): TGocciaObjectValue;
begin
  Result := TGocciaObjectValue.Create(TGocciaObjectValue.SharedObjectPrototype);
  VMCopyDataProperties(Result, ASource, AExclusionKeys);
end;

function TGocciaVM.ForInEntriesArray(
  const AValue: TGocciaValue): TGocciaArrayValue;
var
  Current, Obj, EntryObj: TGocciaObjectValue;
  Keys: TArray<string>;
  Key: string;
  KeyValue: TGocciaStringLiteralValue;
  Visited: TOrderedStringMap<Boolean>;
  GC: TGarbageCollector;
  ChainDepth: Integer;
begin
  GC := TGarbageCollector.Instance;
  Result := TGocciaArrayValue.Create;
  if Assigned(GC) then
    GC.AddTempRoot(Result);
  try
    if (AValue is TGocciaUndefinedLiteralValue) or
       (AValue is TGocciaNullLiteralValue) then
      Exit;

    Obj := ToObject(AValue);
    if Assigned(GC) then
      GC.AddTempRoot(Obj);
    // Dedup keys across the prototype chain via O(1) hash-set membership
    // (native case-sensitive string equality); VMOrderOwnPropertyStringKeys
    // (above) owns per-level enumeration order.
    Visited := TOrderedStringMap<Boolean>.Create;
    try
      Current := Obj;
      ChainDepth := 0;
      while Assigned(Current) do
      begin
        Inc(ChainDepth);
        if ChainDepth > FOR_IN_MAX_PROTOTYPE_CHAIN_DEPTH then
          ThrowTypeError(Format(SErrorProtoChainDepthExceeded, ['for...in']),
            SSuggestPrototypeChainTooDeep);

        Keys := VMOrderOwnPropertyStringKeys(Current.GetAllPropertyNames);
        for Key in Keys do
        begin
          if Visited.ContainsKey(Key) then
            Continue;

          Visited.Add(Key, True);
          EntryObj := TGocciaObjectValue.Create;
          if Assigned(GC) then
            GC.AddTempRoot(EntryObj);
          try
            EntryObj.DefineProperty(FOR_IN_ENTRY_OWNER,
              TGocciaPropertyDescriptorData.Create(Current, []));
            KeyValue := TGocciaStringLiteralValue.Create(Key);
            if Assigned(GC) then
              GC.AddTempRoot(KeyValue);
            try
              EntryObj.DefineProperty(FOR_IN_ENTRY_KEY,
                TGocciaPropertyDescriptorData.Create(KeyValue, []));
            finally
              if Assigned(GC) then
                GC.RemoveTempRoot(KeyValue);
            end;
            Result.Elements.Add(EntryObj);
          finally
            if Assigned(GC) then
              GC.RemoveTempRoot(EntryObj);
          end;
        end;
        Current := Current.Prototype;
      end;
    finally
      Visited.Free;
      if Assigned(GC) then
        GC.RemoveTempRoot(Obj);
    end;
  finally
    if Assigned(GC) then
      GC.RemoveTempRoot(Result);
  end;
end;

function TGocciaVM.TryForInEntryKey(const AEntry: TGocciaValue;
  out AKey: string): Boolean;
var
  EntryObj, Owner: TGocciaObjectValue;
  OwnerValue, KeyValue: TGocciaValue;
  Descriptor: TGocciaPropertyDescriptor;
begin
  AKey := '';
  if not (AEntry is TGocciaObjectValue) then
    Exit(False);

  EntryObj := TGocciaObjectValue(AEntry);
  OwnerValue := EntryObj.GetProperty(FOR_IN_ENTRY_OWNER);
  KeyValue := EntryObj.GetProperty(FOR_IN_ENTRY_KEY);
  if not (OwnerValue is TGocciaObjectValue) or
     not (KeyValue is TGocciaStringLiteralValue) then
    Exit(False);

  Owner := TGocciaObjectValue(OwnerValue);
  AKey := TGocciaStringLiteralValue(KeyValue).Value;
  Descriptor := Owner.GetOwnPropertyDescriptor(AKey);
  Result := Assigned(Descriptor) and Descriptor.Enumerable;
end;

function TGocciaVM.PromiseResolveIntrinsic(
  const AValue: TGocciaValue): TGocciaPromiseValue;
begin
  Result := Goccia.Values.PromiseValue.PromiseResolveIntrinsic(AValue);
end;

function TGocciaVM.GetIteratorValue(const AIterable: TGocciaValue;
  const ATryAsync: Boolean): TGocciaValue;
var
  IteratorSource: TGocciaObjectValue;
  IteratorMethod: TGocciaValue;
  IteratorObject: TGocciaValue;
  NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if ATryAsync and (AIterable is TGocciaObjectValue) then
  begin
    IteratorMethod := TGocciaObjectValue(AIterable).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownAsyncIterator);
    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       IteratorMethod.IsCallable then
    begin
      CallArgs := AcquireArguments;
      try
        Result := InvokeCallable(IteratorMethod, CallArgs, AIterable);
      finally
        ReleaseArguments(CallArgs);
      end;
      if not (Result is TGocciaObjectValue) then
        ThrowTypeError(Format(SErrorIteratorResultNotObject,
          [Result.ToStringLiteral.Value]), SSuggestIteratorResultObject);
      NextMethod := Result.GetProperty(PROP_NEXT);
      if not Assigned(NextMethod) or
         (NextMethod is TGocciaUndefinedLiteralValue) or
         not NextMethod.IsCallable then
        ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
          SSuggestAsyncIteratorProtocol);
      // Note: the captured NextMethod is not propagated downstream
      // here.  TGocciaVMAsyncFromSyncIteratorValue is sync->async
      // wrapping only — for a TRUE async iterator (whose next()
      // already returns Promise<IteratorResult>) routing through
      // that wrapper would call GetProperty(PROP_DONE) on the
      // unresolved Promise and produce an infinite for-await-of
      // loop.  Capture-once for the async branch would require a
      // dedicated TGocciaGenericAsyncIteratorValue class; until
      // that exists, downstream consumers (for-await-of dispatch)
      // re-resolve PROP_NEXT per iteration.  The §7.4.2 validation
      // above still ensures `next` is callable at acquisition time.
      Result := TGocciaVMAsyncIteratorRecordValue.Create(Result, NextMethod);
      Exit;
    end
    else if Assigned(IteratorMethod) and
            not (IteratorMethod is TGocciaUndefinedLiteralValue) and
            not (IteratorMethod is TGocciaNullLiteralValue) then
      ThrowTypeError('Async iterator method is not callable');
  end;

  if AIterable is TGocciaObjectValue then
    IteratorSource := TGocciaObjectValue(AIterable)
  else if not (AIterable is TGocciaNullLiteralValue) and
          not (AIterable is TGocciaUndefinedLiteralValue) then
    IteratorSource := ToObject(AIterable)
  else
    IteratorSource := nil;

  if Assigned(IteratorSource) then
  begin
    // ES2024 §7.4.1 GetIterator resolves @@iterator through ordinary
    // property lookup.  Do not special-case arrays, strings, or native
    // iterator values here: deleting or replacing their iterator methods
    // must be observable in bytecode just as it is in the interpreter.
    IteratorMethod := IteratorSource.GetSymbolProperty(
      TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       not (IteratorMethod is TGocciaNullLiteralValue) then
    begin
      if not IteratorMethod.IsCallable then
        ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']),
          SSuggestIteratorProtocol);
      CallArgs := AcquireArguments;
      try
        IteratorObject := InvokeCallable(IteratorMethod, CallArgs, AIterable);
      finally
        ReleaseArguments(CallArgs);
      end;

      if IteratorObject is TGocciaIteratorValue then
      begin
        if ATryAsync then
          Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(Self, IteratorObject,
            IteratorObject.GetProperty(PROP_NEXT)));
        Exit(IteratorObject);
      end;

      if IteratorObject is TGocciaObjectValue then
      begin
        NextMethod := IteratorObject.GetProperty(PROP_NEXT);
        // ES2026 §7.4.2 GetIteratorDirect captures "next" without
        // validating callability.  The first IteratorStep reports a
        // missing/non-callable next, after destructuring assignment target
        // references have had their required chance to run.
        if ATryAsync then
          Exit(TGocciaVMAsyncFromSyncIteratorValue.Create(Self, IteratorObject,
            NextMethod));
        Exit(CreateRootedGenericIterator(IteratorObject, NextMethod));
      end;
    end;
  end;

  ThrowTypeError(Format(SErrorNotIterable, [AIterable.TypeName]),
    SSuggestNotIterable);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.ConstructValue(const AConstructor: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  BytecodeFunction: TGocciaBytecodeFunctionValue;
  BoundArgs: TGocciaArgumentsCollection;
  BoundFunction: TGocciaBoundFunctionValue;
  ClassConstructor: TGocciaClassValue;
  ConstructorName: string;
  EvalContext: TGocciaEvaluationContext;
  EffectiveNewTarget: TGocciaValue;
  I: Integer;
  ReceiverPrototype, ReceiverInstance: TGocciaObjectValue;
begin
  // ES2026 §28.1.1 [[Construct]](argumentsList, newTarget)
  if Assigned(ANewTarget) then
    EffectiveNewTarget := ANewTarget
  else
    EffectiveNewTarget := AConstructor;

  if AConstructor is TGocciaProxyValue then
  begin
    Result := TGocciaProxyValue(AConstructor).ConstructTrap(AArguments,
      EffectiveNewTarget);
    Exit;
  end;

  if AConstructor is TGocciaVMClassValue then
  begin
    Result := TGocciaVMClassValue(AConstructor).Instantiate(AArguments,
      EffectiveNewTarget);
    Exit;
  end;

  if AConstructor is TGocciaClassValue then
  begin
    ClassConstructor := TGocciaClassValue(AConstructor);
    if (ClassConstructor.SourceText <> '') and
       (ClassConstructor.NativeInstanceDefaultPrototype = nil) and
       (ClassConstructor.NativeSuperConstructor = nil) then
    begin
      EvalContext := Default(TGocciaEvaluationContext);
      EvalContext.Realm := FRealm;
      if Assigned(FCurrentDynamicVarScope) then
        EvalContext.Scope := FCurrentDynamicVarScope
      else
        EvalContext.Scope := FGlobalScope;
      EvalContext.OnError := ThrowError;
      EvalContext.LoadModule := FLoadModule;
      EvalContext.LoadModuleSource := FLoadModuleSource;
      EvalContext.ResolveModuleURL := FResolveModuleURL;
      EvalContext.CurrentFilePath := FCurrentModuleSourcePath;
      EvalContext.CoverageEnabled := FCoverageEnabled;
      EvalContext.StrictTypes := False;
      if Assigned(FGlobalScope) then
      begin
        EvalContext.StrictTypes := FGlobalScope.EffectiveStrictTypes;
        EvalContext.CompatibilityNonStrictMode :=
          FGlobalScope.EffectiveNonStrictMode;
      end;
      EvalContext.NonStrictMode := not (Assigned(FCurrentClosure) and
        Assigned(FCurrentClosure.Template) and
        FCurrentClosure.Template.StrictCode);
      Result := InstantiateClass(ClassConstructor, AArguments, EvalContext,
        EffectiveNewTarget);
    end
    else
      Result := ClassConstructor.Instantiate(AArguments, EffectiveNewTarget);
    Exit;
  end;

  if AConstructor is TGocciaBoundFunctionValue then
  begin
    BoundFunction := TGocciaBoundFunctionValue(AConstructor);
    if IsSameValue(BoundFunction, EffectiveNewTarget) then
      EffectiveNewTarget := BoundFunction.OriginalFunction;
    BoundArgs := TGocciaArgumentsCollection.CreateWithCapacity(
      BoundFunction.BoundArgCount + AArguments.Length);
    try
      for I := 0 to BoundFunction.BoundArgCount - 1 do
        BoundArgs.Add(BoundFunction.GetBoundArg(I));
      for I := 0 to AArguments.Length - 1 do
        BoundArgs.Add(AArguments.GetElement(I));
      Result := ConstructValue(BoundFunction.OriginalFunction, BoundArgs,
        EffectiveNewTarget);
    finally
      BoundArgs.Free;
    end;
    Exit;
  end;

  if AConstructor is TGocciaNativeFunctionValue then
  begin
    if TGocciaNativeFunctionValue(AConstructor).NotConstructable then
      ThrowTypeError(
        Format(SErrorNotConstructor,
          [TGocciaNativeFunctionValue(AConstructor).Name]),
        Format('''%s'' is not a constructor',
          [TGocciaNativeFunctionValue(AConstructor).Name]));
    ConstructorName := TGocciaNativeFunctionValue(AConstructor).Name;
    if (TGocciaCallStack.Instance <> nil) then
      TGocciaCallStack.Instance.Push(ConstructorName, '', 0, 0);
    try
      Result := TGocciaNativeFunctionValue(AConstructor).Construct(
        AArguments, EffectiveNewTarget);
    finally
      if (TGocciaCallStack.Instance <> nil) then
        TGocciaCallStack.Instance.Pop;
    end;
    Exit;
  end;

  if AConstructor is TGocciaBytecodeFunctionValue then
  begin
    BytecodeFunction := TGocciaBytecodeFunctionValue(AConstructor);
    if Assigned(BytecodeFunction.FClosure) and
       Assigned(BytecodeFunction.FClosure.Template) and
       (BytecodeFunction.FClosure.Template.IsGenerator or
        BytecodeFunction.FClosure.Template.IsAsync or
        BytecodeFunction.FClosure.Template.IsArrow) then
      ThrowTypeError(Format(SErrorNotConstructor,
        [BytecodeFunction.GetProperty(PROP_NAME).ToStringLiteral.Value]),
        SSuggestNotConstructorType);
    if (BytecodeFunction.FConstructClassValue is TGocciaVMClassValue) and
       (TGocciaVMClassValue(BytecodeFunction.FConstructClassValue)
          .FConstructorValue = AConstructor) then
    begin
      Result := TGocciaVMClassValue(BytecodeFunction.FConstructClassValue)
        .Instantiate(AArguments, EffectiveNewTarget);
      Exit;
    end;
  end;

  if AConstructor is TGocciaFunctionBase then
  begin
    if (AConstructor is TGocciaGeneratorFunctionValue) or
       not TGocciaFunctionBase(AConstructor).IsConstructable then
      ThrowTypeError(Format(SErrorValueNotConstructor,
        [AConstructor.TypeName]), SSuggestNotConstructorType);

    ConstructorName := TGocciaFunctionBase(AConstructor).GetProperty(PROP_NAME)
      .ToStringLiteral.Value;
    if not TGocciaFunctionBase(AConstructor).IsConstructable then
      ThrowTypeError(Format(SErrorNotConstructor, [ConstructorName]),
        SSuggestNotConstructorType);
    // ES2026 §10.2.2 [[Construct]] for ordinary function objects:
    // OrdinaryCreateFromConstructor allocates a fresh object whose
    // [[Prototype]] is constructor.prototype (or %Object.prototype% when
    // that property is not an Object — §10.1.14 GetPrototypeFromConstructor),
    // then calls the body with the new object bound as `this`.  Iff the body
    // explicitly returns an Object, that becomes the call result.
    ReceiverPrototype := GetProtoFromConstructor(EffectiveNewTarget);
    ReceiverInstance := TGocciaObjectValue.Create(ReceiverPrototype);
    TGarbageCollector.Instance.AddTempRoot(ReceiverInstance);
    try
      if (TGocciaCallStack.Instance <> nil) then
        TGocciaCallStack.Instance.Push(ConstructorName, '', 0, 0);
      try
        // InvokeConstructableWithReceiver merges bound args, dispatches to
        // the underlying target, and applies the spec return rules
        // (explicit Object return wins; otherwise the receiver).
        Result := InvokeConstructableWithReceiver(AConstructor, AArguments,
          ReceiverInstance, EffectiveNewTarget);
      finally
        if (TGocciaCallStack.Instance <> nil) then
          TGocciaCallStack.Instance.Pop;
      end;
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ReceiverInstance);
    end;
    Exit;
  end;

  ThrowTypeError(Format(SErrorValueNotConstructor, [AConstructor.TypeName]),
    SSuggestNotConstructorType);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.ImportModuleValue(const APath: string): TGocciaValue;
var
  Module: TGocciaModule;
begin
  if not Assigned(FLoadModule) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Module := FLoadModule(APath, FCurrentModuleSourcePath);
  Result := CreateModuleNamespaceObject(Module);
end;

function TGocciaVM.ImportModuleValue(const APath, AReferrer: string): TGocciaValue;
var
  Module: TGocciaModule;
begin
  if not Assigned(FLoadModule) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Module := FLoadModule(APath, AReferrer);
  Result := CreateModuleNamespaceObject(Module);
end;

procedure TGocciaVM.ResolveDynamicImportPromise(
  const APromise: TGocciaPromiseValue; const APath, AReferrer: string);
var
  EvaluationPromise: TGocciaPromiseValue;
  Module: TGocciaModule;
  Namespace: TGocciaValue;
begin
  if not Assigned(FLoadModule) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Module := FLoadModule(APath, AReferrer);
  Namespace := CreateModuleNamespaceObject(Module);
  if Assigned(Module) and
     (Module.EvaluationPromise is TGocciaPromiseValue) then
  begin
    EvaluationPromise := TGocciaPromiseValue(Module.EvaluationPromise);
    case EvaluationPromise.State of
      gpsFulfilled:
        APromise.Resolve(Namespace);
      gpsRejected:
        APromise.Reject(EvaluationPromise.PromiseResult);
      gpsPending:
        EvaluationPromise.InvokeThen(
          TGocciaVMDynamicImportFulfillValue.Create(APromise, Namespace),
          TGocciaVMDynamicImportRejectValue.Create(APromise));
    end;
  end
  else
    APromise.Resolve(Namespace);
end;

function TGocciaVM.ImportModuleSourceValue(const APath, AReferrer: string): TGocciaValue;
begin
  if not Assigned(FLoadModuleSource) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Result := FLoadModuleSource(APath, AReferrer);
end;

function TGocciaVM.ImportDeferredModuleNamespaceValue(const APath,
  AReferrer: string): TGocciaValue;
begin
  if not Assigned(FLoadDeferredModule) then
    ThrowTypeError(SErrorModuleNotAvailableInVM);

  Result := FLoadDeferredModule(APath, AReferrer);
end;

function TGocciaVM.DynamicImportAttributeType(
  const AOptions: TGocciaValue): string;
var
  AttributeType: string;
  HasAttributeType: Boolean;
begin
  TryReadImportAttributeType(AOptions, AttributeType, HasAttributeType);
  if HasAttributeType and (AttributeType <> 'json') and
     (AttributeType <> 'text') and (AttributeType <> 'bytes') then
    ThrowTypeError('Unsupported import attribute type "' + AttributeType + '"');
  if HasAttributeType then
    Result := AttributeType
  else
    Result := '';
end;

procedure TGocciaVM.ExportBindingValue(const AName: string;
  const AValue: TGocciaValue; const ASourcePath: string);
var
  RuntimeModule: TGocciaModule;

  function SameSourcePath(const ALeft, ARight: string): Boolean;
  begin
    Result := (ALeft <> '') and (ARight <> '') and
      SameFileName(ExpandFileName(ALeft), ExpandFileName(ARight));
  end;
begin
  if not Assigned(FCurrentModuleExports) then
    FCurrentModuleExports := TGocciaValueMap.Create;
  FCurrentModuleExports.AddOrSetValue(AName, AValue);
  RuntimeModule := nil;

  if Assigned(FCurrentRuntimeModule) and
     ((ASourcePath = '') or SameSourcePath(ASourcePath,
       FCurrentRuntimeModule.Path)) then
    RuntimeModule := FCurrentRuntimeModule;

  if (not Assigned(RuntimeModule)) and Assigned(FLoadModule) and
     (ASourcePath <> '') and
     (not SameSourcePath(ASourcePath, FCurrentModuleSourcePath)) then
    RuntimeModule := FLoadModule(ASourcePath, ASourcePath);

  if not Assigned(RuntimeModule) then
    RuntimeModule := FCurrentRuntimeModule;
  if Assigned(RuntimeModule) then
    RuntimeModule.UpdateExportValue(AName, AValue);
end;

// ES2026 §13.2.5.6 Runtime Semantics: PropertyDefinitionEvaluation
procedure TGocciaVM.DefineDataPropertyByKeyInternal(const ATarget: TGocciaValue;
  const AKey: TGocciaRegister; const AValue: TGocciaValue;
  const ASetHomeObject: Boolean);
var
  TargetObject: TGocciaObjectValue;
  Key: TGocciaPropertyKey;
begin
  if not (ATarget is TGocciaObjectValue) then
    Exit;

  TargetObject := TGocciaObjectValue(ATarget);
  if ASetHomeObject then
    SetBytecodeHomeObject(AValue, TargetObject);

  Key := ClassifyPropertyKey(AKey, False);
  if Key.Kind = pkkSymbol then
    DefineSymbolDataPropertyOnObject(TargetObject, Key.Symbol, AValue)
  else
    DefineDataPropertyOnObject(TargetObject, PropertyKeyName(Key), AValue);
end;

procedure TGocciaVM.DefineDataPropertyByKey(const ATarget: TGocciaValue;
  const AKey: TGocciaRegister; const AValue: TGocciaValue);
begin
  DefineDataPropertyByKeyInternal(ATarget, AKey, AValue, False);
end;

// ES2026 §13.2.5.5 MethodDefinition: concise object methods get [[HomeObject]].
procedure TGocciaVM.DefineMethodPropertyByKey(const ATarget: TGocciaValue;
  const AKey: TGocciaRegister; const AValue: TGocciaValue);
begin
  DefineDataPropertyByKeyInternal(ATarget, AKey, AValue, True);
end;

procedure TGocciaVM.SetObjectLiteralPrototype(const ATarget,
  APrototype: TGocciaValue);
var
  TargetObject: TGocciaObjectValue;
begin
  if not (ATarget is TGocciaObjectValue) then
    Exit;

  TargetObject := TGocciaObjectValue(ATarget);
  if APrototype is TGocciaObjectValue then
    TargetObject.Prototype := TGocciaObjectValue(APrototype)
  else if APrototype is TGocciaNullLiteralValue then
    TargetObject.Prototype := nil;
end;

procedure TGocciaVM.DefineGetterProperty(const ATarget: TGocciaValue;
  const AName: string; const AGetter: TGocciaValue);
var
  TargetObject: TGocciaObjectValue;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
  DescriptorFlags: TPropertyFlags;
begin
  if ATarget is TGocciaVMClassValue then
  begin
    TargetObject := TGocciaVMClassValue(ATarget).Prototype;
    DescriptorFlags := [pfConfigurable];
  end
  else if ATarget is TGocciaObjectValue then
  begin
    TargetObject := TGocciaObjectValue(ATarget);
    DescriptorFlags := [pfEnumerable, pfConfigurable, pfWritable];
  end
  else
    Exit;

  if ATarget is TGocciaVMClassValue then
    SetBytecodeHomeObject(AGetter, ATarget)
  else
    SetBytecodeHomeObject(AGetter, TargetObject);
  ExistingDescriptor := TargetObject.GetOwnPropertyDescriptor(AName);
  ExistingSetter := nil;
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
    ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
  TargetObject.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(
    AGetter, ExistingSetter, DescriptorFlags));
end;

procedure TGocciaVM.DefineSetterProperty(const ATarget: TGocciaValue;
  const AName: string; const ASetter: TGocciaValue);
var
  TargetObject: TGocciaObjectValue;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
  DescriptorFlags: TPropertyFlags;
begin
  if ATarget is TGocciaVMClassValue then
  begin
    TargetObject := TGocciaVMClassValue(ATarget).Prototype;
    DescriptorFlags := [pfConfigurable];
  end
  else if ATarget is TGocciaObjectValue then
  begin
    TargetObject := TGocciaObjectValue(ATarget);
    DescriptorFlags := [pfEnumerable, pfConfigurable, pfWritable];
  end
  else
    Exit;

  if ATarget is TGocciaVMClassValue then
    SetBytecodeHomeObject(ASetter, ATarget)
  else
    SetBytecodeHomeObject(ASetter, TargetObject);
  ExistingDescriptor := TargetObject.GetOwnPropertyDescriptor(AName);
  ExistingGetter := nil;
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
    ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
  TargetObject.DefineProperty(AName, TGocciaPropertyDescriptorAccessor.Create(
    ExistingGetter, ASetter, DescriptorFlags));
end;

procedure TGocciaVM.DefineStaticGetterProperty(const ATarget: TGocciaValue;
  const AName: string; const AGetter: TGocciaValue);
var
  PrivateBrandToken: string;
begin
  if ATarget is TGocciaClassValue then
  begin
    SetBytecodeHomeObject(AGetter, ATarget, True);
      if IsBytecodePrivateKey(AName) then
      begin
        PrivateBrandToken := BytecodePrivateTokenForKey(AName,
          TGocciaClassValue(ATarget).PrivateBrandToken);
        TGocciaClassValue(ATarget).AddPrivateGetter(
          BytecodePrivateRuntimeKey(AName, PrivateBrandToken),
          TGocciaFunctionBase(AGetter));
      end
    else
      TGocciaClassValue(ATarget).AddStaticGetter(AName, TGocciaFunctionBase(AGetter));
  end;
end;

procedure TGocciaVM.DefineStaticSetterProperty(const ATarget: TGocciaValue;
  const AName: string; const ASetter: TGocciaValue);
var
  PrivateBrandToken: string;
begin
  if ATarget is TGocciaClassValue then
  begin
    SetBytecodeHomeObject(ASetter, ATarget, True);
      if IsBytecodePrivateKey(AName) then
      begin
        PrivateBrandToken := BytecodePrivateTokenForKey(AName,
          TGocciaClassValue(ATarget).PrivateBrandToken);
        TGocciaClassValue(ATarget).AddPrivateSetter(
          BytecodePrivateRuntimeKey(AName, PrivateBrandToken),
          TGocciaFunctionBase(ASetter));
      end
    else
      TGocciaClassValue(ATarget).AddStaticSetter(AName, TGocciaFunctionBase(ASetter));
  end;
end;

procedure TGocciaVM.DefineGetterPropertyByKey(const ATarget, AKey,
  AGetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
  DescriptorFlags: TPropertyFlags;
begin
  if AKey is TGocciaSymbolValue then
  begin
    if ATarget is TGocciaVMClassValue then
    begin
      SetBytecodeHomeObject(AGetter, ATarget);
      DescriptorFlags := [pfConfigurable];
      ExistingDescriptor := TGocciaVMClassValue(ATarget).Prototype
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingSetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
        ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
      TGocciaVMClassValue(ATarget).Prototype.DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ExistingSetter, DescriptorFlags));
    end
    else if ATarget is TGocciaObjectValue then
    begin
      SetBytecodeHomeObject(AGetter, TGocciaObjectValue(ATarget));
      DescriptorFlags := [pfEnumerable, pfConfigurable, pfWritable];
      ExistingDescriptor := TGocciaObjectValue(ATarget)
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingSetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
        ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
      TGocciaObjectValue(ATarget).DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ExistingSetter, DescriptorFlags));
    end;
    Exit;
  end;

  DefineGetterProperty(ATarget, AKey.ToStringLiteral.Value, AGetter);
end;

procedure TGocciaVM.DefineSetterPropertyByKey(const ATarget, AKey,
  ASetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
  DescriptorFlags: TPropertyFlags;
begin
  if AKey is TGocciaSymbolValue then
  begin
    if ATarget is TGocciaVMClassValue then
    begin
      SetBytecodeHomeObject(ASetter, ATarget);
      DescriptorFlags := [pfConfigurable];
      ExistingDescriptor := TGocciaVMClassValue(ATarget).Prototype
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingGetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
        ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
      TGocciaVMClassValue(ATarget).Prototype.DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          ExistingGetter, ASetter, DescriptorFlags));
    end
    else if ATarget is TGocciaObjectValue then
    begin
      SetBytecodeHomeObject(ASetter, TGocciaObjectValue(ATarget));
      DescriptorFlags := [pfEnumerable, pfConfigurable, pfWritable];
      ExistingDescriptor := TGocciaObjectValue(ATarget)
        .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
      ExistingGetter := nil;
      if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
        ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
      TGocciaObjectValue(ATarget).DefineSymbolProperty(
        TGocciaSymbolValue(AKey),
        TGocciaPropertyDescriptorAccessor.Create(
          ExistingGetter, ASetter, DescriptorFlags));
    end;
    Exit;
  end;

  DefineSetterProperty(ATarget, AKey.ToStringLiteral.Value, ASetter);
end;

procedure TGocciaVM.DefineStaticGetterPropertyByKey(const ATarget, AKey,
  AGetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  if (ATarget is TGocciaClassValue) and (AKey is TGocciaSymbolValue) then
  begin
    SetBytecodeHomeObject(AGetter, ATarget, True);
    ExistingDescriptor := TGocciaClassValue(ATarget)
      .GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(AKey));
    ExistingSetter := nil;
    if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
      ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
    TGocciaClassValue(ATarget).DefineSymbolProperty(
      TGocciaSymbolValue(AKey),
      TGocciaPropertyDescriptorAccessor.Create(
        AGetter, ExistingSetter, [pfConfigurable]));
    Exit;
  end;

  DefineStaticGetterProperty(ATarget, AKey.ToStringLiteral.Value, AGetter);
end;

procedure TGocciaVM.DefineStaticSetterPropertyByKey(const ATarget, AKey,
  ASetter: TGocciaValue);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  if (ATarget is TGocciaClassValue) and (AKey is TGocciaSymbolValue) then
  begin
    SetBytecodeHomeObject(ASetter, ATarget, True);
    ExistingDescriptor := TGocciaClassValue(ATarget)
      .GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(AKey));
    ExistingGetter := nil;
    if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
      ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
    TGocciaClassValue(ATarget).DefineSymbolProperty(
      TGocciaSymbolValue(AKey),
      TGocciaPropertyDescriptorAccessor.Create(
        ExistingGetter, ASetter, [pfConfigurable]));
    Exit;
  end;

  DefineStaticSetterProperty(ATarget, AKey.ToStringLiteral.Value, ASetter);
end;

function TGocciaVM.EnsureCurrentDynamicVarScope: TGocciaScope;
var
  ParentScope: TGocciaScope;
begin
  if Assigned(FCurrentDynamicVarScope) then
    Exit(FCurrentDynamicVarScope);

  if not Assigned(FGlobalScope) then
    Exit(nil);

  ParentScope := FGlobalScope;
  if Assigned(FCurrentClosure) and
     Assigned(FCurrentClosure.DynamicVarScope) then
    ParentScope := FCurrentClosure.DynamicVarScope;
  FCurrentDynamicVarScope := ParentScope.CreateChild(skFunction,
    'BytecodeDynamicVarEnv');
  if FLocalCellCount > 0 then
    FCurrentDynamicVarScope.ThisValue := GetLocal(0)
  else if Assigned(FGlobalThisValue) then
    FCurrentDynamicVarScope.ThisValue := FGlobalThisValue;
  FCurrentDynamicVarScope.NonStrictMode := True;
  Result := FCurrentDynamicVarScope;
end;

function FindDynamicVarBindingScope(const AScope: TGocciaScope;
  const AName: string): TGocciaScope;
var
  ScopeCursor: TGocciaScope;
begin
  ScopeCursor := AScope;
  while Assigned(ScopeCursor) and (ScopeCursor.ScopeKind <> skGlobal) do
  begin
    if ScopeCursor.ContainsOwnVarBinding(AName) or
       ScopeCursor.ContainsOwnLexicalBinding(AName) then
      Exit(ScopeCursor);
    ScopeCursor := ScopeCursor.Parent;
  end;
  Result := nil;
end;

function HasDynamicVarBinding(const AScope: TGocciaScope;
  const AName: string): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := Assigned(FindDynamicVarBindingScope(AScope, AName));
end;

function HasOwnDynamicVarBinding(const AScope: TGocciaScope;
  const AName: string): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := Assigned(AScope) and
    (AScope.ContainsOwnVarBinding(AName) or
     AScope.ContainsOwnLexicalBinding(AName));
end;

function TGocciaVM.ResolveDynamicUpvalueScope(const AIndex: Integer;
  const AName: string): TGocciaScope;
begin
  Result := nil;
  if (AName = '') or not Assigned(FCurrentClosure) or
     not Assigned(FCurrentDynamicVarScope) then
    Exit;

  if (FCurrentDynamicVarScope <> FCurrentClosure.DynamicVarScope) and
     HasOwnDynamicVarBinding(FCurrentDynamicVarScope, AName) then
    Exit(FCurrentDynamicVarScope);

  if FCurrentClosure.IsDynamicVarUpvalue(AIndex) then
    Result := FindDynamicVarBindingScope(FCurrentClosure.DynamicVarScope,
      AName);
end;

procedure TGocciaVM.DefineGlobalBinding(const AName: string;
  const AValue: TGocciaValue;
  const ADeclarationType: TGocciaDeclarationType;
  const ANonStrictMode: Boolean = False);
begin
  if Assigned(FGlobalScope) then
  begin
    if ADeclarationType = dtVar then
    begin
      if ANonStrictMode then
      begin
        FGlobalScope.CreateGlobalVarBinding(AName, False);
        FGlobalScope.AssignBinding(AName, AValue, 0, 0, True);
      end
      else
        FGlobalScope.DefineVariableBinding(AName, AValue, True);
    end
    else
      FGlobalScope.DefineLexicalBinding(AName, AValue, ADeclarationType);
  end;
end;

function TGocciaVM.FinalizeEnumValue(const AValue: TGocciaValue;
  const AName: string): TGocciaValue;
var
  EnumObj: TGocciaEnumValue;
  Entries: TGocciaArrayValue;
  PairArr: TGocciaArrayValue;
  Names: TArray<string>;
  I: Integer;
  Key: string;
  MemberValue: TGocciaValue;
begin
  if not (AValue is TGocciaObjectValue) then
    Exit(AValue);

  EnumObj := TGocciaEnumValue.Create(AName);
  Entries := TGocciaArrayValue.Create;
  EnumObj.Entries := Entries;

  Names := TGocciaObjectValue(AValue).GetOwnPropertyNames;
  for I := 0 to High(Names) do
  begin
    Key := Names[I];
    MemberValue := TGocciaObjectValue(AValue).GetProperty(Key);

    if not (MemberValue is TGocciaNumberLiteralValue) and
       not (MemberValue is TGocciaStringLiteralValue) and
       not (MemberValue is TGocciaSymbolValue) then
      ThrowTypeError(Format(SErrorEnumMemberType, [Key]),
        SSuggestEnumValueType);

    EnumObj.DefineProperty(Key,
      TGocciaPropertyDescriptorData.Create(MemberValue, [pfEnumerable]));

    PairArr := TGocciaArrayValue.Create;
    PairArr.Elements.Add(TGocciaStringLiteralValue.Create(Key));
    PairArr.Elements.Add(MemberValue);
    Entries.Elements.Add(PairArr);
  end;

  InitializeEnumSymbols(EnumObj);
  EnumObj.PreventExtensions;
  Result := EnumObj;
end;

function IsBytecodePrivateKey(const AKey: string): Boolean;
begin
  Result := (Length(AKey) > Length(BYTECODE_PRIVATE_SLOT_PREFIX)) and
    (Copy(AKey, 1, Length(BYTECODE_PRIVATE_SLOT_PREFIX)) =
      BYTECODE_PRIVATE_SLOT_PREFIX);
end;

function IsBytecodePrivateBrandKey(const AKey: string): Boolean;
begin
  Result := (Length(AKey) > Length(BYTECODE_PRIVATE_BRAND_PREFIX)) and
    (Copy(AKey, 1, Length(BYTECODE_PRIVATE_BRAND_PREFIX)) =
      BYTECODE_PRIVATE_BRAND_PREFIX);
end;

function BytecodePrivateTokenForKey(const AKey,
  AFallbackPrivateBrandToken: string): string;
var
  KeyBody: string;
  DelimiterPos: NativeInt;
begin
  Result := AFallbackPrivateBrandToken;
  if IsBytecodePrivateBrandKey(AKey) then
  begin
    KeyBody := Copy(AKey, Length(BYTECODE_PRIVATE_BRAND_PREFIX) + 1, MaxInt);
    DelimiterPos := Pos(':', KeyBody);
    if DelimiterPos > 1 then
      Result := Copy(KeyBody, 1, DelimiterPos - 1);
  end
  else if IsBytecodePrivateKey(AKey) then
  begin
    KeyBody := Copy(AKey, Length(BYTECODE_PRIVATE_SLOT_PREFIX) + 1, MaxInt);
    DelimiterPos := Pos(':', KeyBody);
    if DelimiterPos > 1 then
      Result := Copy(KeyBody, 1, DelimiterPos - 1)
    else if (KeyBody <> '') and (Pos('$', KeyBody) = 0) then
      Result := KeyBody;
  end;
end;

function BytecodePrivateSourceName(const AName: string): string;
var
  Body: string;
  DelimiterPos: NativeInt;
begin
  Result := AName;
  if IsBytecodePrivateKey(Result) then
  begin
    Body := Copy(Result, Length(BYTECODE_PRIVATE_SLOT_PREFIX) + 1, MaxInt);
    DelimiterPos := Pos('$', Body);
    if DelimiterPos > 0 then
      Result := Copy(Body, DelimiterPos + 1, MaxInt)
    else
    begin
      DelimiterPos := Pos(':', Body);
      if DelimiterPos > 0 then
        Result := Copy(Body, DelimiterPos + 1, MaxInt)
      else
        Result := Body;
    end;
  end;
  if (Result <> '') and (Result[1] = '#') then
    Result := Copy(Result, 2, MaxInt);
end;

function BytecodePrivateReceiverBrandToken(
  const AObject: TGocciaValue): string;
begin
  Result := '';
  if AObject is TGocciaClassValue then
    Result := TGocciaClassValue(AObject).PrivateBrandToken
  else if (AObject is TGocciaInstanceValue) and
          Assigned(TGocciaInstanceValue(AObject).ClassValue) then
    Result := TGocciaInstanceValue(AObject).ClassValue.PrivateBrandToken;
end;

function NormalizeBytecodePrivateKey(const AName,
  APrivateBrandToken: string): string;
begin
  if IsBytecodePrivateBrandKey(AName) then
    Result := AName
  else if IsBytecodePrivateKey(AName) then
    Result := AName
  else if (AName <> '') and (AName[1] = '#') then
    Result := BYTECODE_PRIVATE_SLOT_PREFIX + APrivateBrandToken + ':' +
      Copy(AName, 2, MaxInt)
  else
    Result := BYTECODE_PRIVATE_SLOT_PREFIX + APrivateBrandToken + ':' + AName;
end;

function BytecodePrivateRuntimeKey(const AName,
  APrivateBrandToken: string): string;
var
  SourceName: string;
begin
  if IsBytecodePrivateBrandKey(AName) then
    Exit(AName);
  if IsBytecodePrivateKey(AName) then
  begin
    SourceName := BytecodePrivateSourceName(AName);
    if SourceName = '' then
      Exit(AName);
    Exit(BYTECODE_PRIVATE_SLOT_PREFIX + APrivateBrandToken + ':' +
      SourceName);
  end;
  Result := NormalizeBytecodePrivateKey(AName, APrivateBrandToken);
end;

function BytecodePrivateBrandKey(const AKey,
  APrivateBrandToken: string): string;
begin
  if IsBytecodePrivateBrandKey(AKey) then
    Result := AKey
  else
    Result := BYTECODE_PRIVATE_BRAND_PREFIX + APrivateBrandToken + ':' +
      NormalizeBytecodePrivateKey(AKey, APrivateBrandToken);
end;

function TGocciaVM.ResolveBytecodePrivateBrandToken(const AKey: string;
  const AObject: TGocciaValue): string;
var
  HomeClass: TGocciaClassValue;
  CandidateClass: TGocciaClassValue;
  ExactReceiverClass: TGocciaClassValue;
  SourceName: string;
  DeclaredKey: string;

  function IsSuperclassOfHome(const AClassValue: TGocciaClassValue): Boolean;
  var
    CurrentClass: TGocciaClassValue;
  begin
    Result := False;
    if (not Assigned(HomeClass)) or not Assigned(AClassValue) then
      Exit;
    CurrentClass := HomeClass.SuperClass;
    while Assigned(CurrentClass) do
    begin
      if CurrentClass = AClassValue then
        Exit(True);
      CurrentClass := CurrentClass.SuperClass;
    end;
  end;
begin
  Result := BytecodePrivateTokenForKey(AKey,
    BytecodePrivateReceiverBrandToken(AObject));

  SourceName := BytecodePrivateSourceName(AKey);
  HomeClass := nil;
  if Assigned(FCurrentClosure) and
     (FCurrentClosure.HomeClass is TGocciaClassValue) then
    HomeClass := TGocciaClassValue(FCurrentClosure.HomeClass);

  CandidateClass := nil;
  if AObject is TGocciaClassValue then
    CandidateClass := TGocciaClassValue(AObject)
  else if (AObject is TGocciaInstanceValue) and
          Assigned(TGocciaInstanceValue(AObject).ClassValue) then
    CandidateClass := TGocciaInstanceValue(AObject).ClassValue;

  ExactReceiverClass := nil;
  while Assigned(CandidateClass) do
  begin
    if (SourceName <> '') and
       CandidateClass.ResolveDeclaredPrivateKey(SourceName, DeclaredKey) and
       (DeclaredKey = AKey) then
    begin
      ExactReceiverClass := CandidateClass;
      Break;
    end;
    CandidateClass := CandidateClass.SuperClass;
  end;

  if Assigned(HomeClass) and (SourceName <> '') and
     HomeClass.ResolveDeclaredPrivateKey(SourceName, DeclaredKey) then
  begin
    if Assigned(ExactReceiverClass) and IsSuperclassOfHome(ExactReceiverClass) then
      Exit(ExactReceiverClass.PrivateBrandToken);
    Exit(HomeClass.PrivateBrandToken);
  end;

  if Assigned(ExactReceiverClass) then
    Exit(ExactReceiverClass.PrivateBrandToken);
end;

function TGocciaVM.CurrentBytecodePrivateAccessClass(
  const AKey: string): TGocciaClassValue;
var
  DeclaredKey: string;
  SourceName: string;
begin
  Result := nil;
  if (not Assigned(FCurrentClosure)) or
     not (FCurrentClosure.HomeClass is TGocciaClassValue) then
    Exit;

  SourceName := BytecodePrivateSourceName(AKey);
  Result := TGocciaClassValue(FCurrentClosure.HomeClass);
  if (SourceName = '') or
     Result.ResolveDeclaredPrivateKey(SourceName, DeclaredKey) then
    Exit;

  Result := nil;
end;

procedure TGocciaVM.ThrowBytecodePrivateTypeError(const AKey,
  AMessage: string);
var
  AccessClass: TGocciaClassValue;
begin
  AccessClass := CurrentBytecodePrivateAccessClass(AKey);
  if Assigned(AccessClass) then
    ThrowTypeErrorInRealm(AMessage, SSuggestPrivateFieldAccess,
      AccessClass.CreationRealm)
  else
    ThrowTypeError(AMessage, SSuggestPrivateFieldAccess);
end;

function BytecodePrivateInitializedKey(
  const APrivateBrandToken: string): string;
begin
  Result := BYTECODE_PRIVATE_INITIALIZED_PREFIX + APrivateBrandToken;
end;

function HasBytecodePrivateInitializersApplied(const AInstance: TGocciaValue;
  const AClassValue: TGocciaClassValue): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Result := False;
  if (not Assigned(AClassValue)) or
     not (AInstance is TGocciaObjectValue) then
    Exit;
  Descriptor := nil;
  TGocciaObjectValue(AInstance).Properties.TryGetValue(
    BytecodePrivateInitializedKey(AClassValue.PrivateBrandToken), Descriptor);
  Result := Descriptor is TGocciaPropertyDescriptorData;
end;

function TryGetRawObjectPrivateDescriptor(const AObject: TGocciaObjectValue;
  const AKey: string; out ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  ADescriptor := nil;
  Result := Assigned(AObject) and AObject.Properties.TryGetValue(AKey,
    ADescriptor);
end;

procedure DefineRawObjectPrivateProperty(const AObject: TGocciaObjectValue;
  const AKey: string; const AValue: TGocciaValue;
  const AFlags: TPropertyFlags);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if AObject.Properties.TryGetValue(AKey, Descriptor) then
    Descriptor.Free;
  AObject.Properties.Add(AKey,
    TGocciaPropertyDescriptorData.Create(AValue, AFlags));
end;

procedure DefineRawObjectPrivateDescriptor(const AObject: TGocciaObjectValue;
  const AKey: string; const ADescriptor: TGocciaPropertyDescriptor);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
begin
  if not Assigned(ADescriptor) then
    Exit;
  if AObject.Properties.TryGetValue(AKey, ExistingDescriptor) then
    ExistingDescriptor.Free;
  AObject.Properties.Add(AKey, ADescriptor);
end;

procedure StampBytecodePrivateInitializersApplied(const AInstance: TGocciaValue;
  const AClassValue: TGocciaClassValue);
begin
  if (not Assigned(AClassValue)) or
     HasBytecodePrivateInitializersApplied(AInstance, AClassValue) then
    Exit;
  if AInstance is TGocciaObjectValue then
    DefineRawObjectPrivateProperty(TGocciaObjectValue(AInstance),
      BytecodePrivateInitializedKey(AClassValue.PrivateBrandToken),
      TGocciaBooleanLiteralValue.TrueValue, []);
end;

procedure TGocciaVM.StampBytecodePrivateBrands(
  const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
  const APreserveExistingPrivateSlots: Boolean);
var
  Names: TStringList;
  PrototypeNames: TArray<string>;
  PrototypeName: string;
  PrivateBrandToken: string;
  BrandKey: string;
  ExistingValue: TGocciaValue;
  PrototypeDescriptor: TGocciaPropertyDescriptor;
  ReceiverObject: TGocciaObjectValue;
  SeenBrandKeys: TStringList;
  SourcePrivateName: string;
  IsPrivateFieldBrand: Boolean;
  I: Integer;
begin
  if (not Assigned(AClassValue)) or
     not (AInstance is TGocciaObjectValue) then
    Exit;

  ReceiverObject := TGocciaObjectValue(AInstance);

  Names := TStringList.Create;
  SeenBrandKeys := TStringList.Create;
  try
    Names.CaseSensitive := True;
    Names.Sorted := False;
    Names.Duplicates := dupIgnore;
    SeenBrandKeys.CaseSensitive := True;
    SeenBrandKeys.Sorted := False;
    SeenBrandKeys.Duplicates := dupIgnore;
    AClassValue.AppendOwnPrivateNames(Names);
    if Assigned(AClassValue.Prototype) then
    begin
      PrototypeNames := AClassValue.Prototype.GetOwnPropertyNames;
      for PrototypeName in PrototypeNames do
        if IsBytecodePrivateKey(PrototypeName) and
           (Names.IndexOf(PrototypeName) < 0) then
          Names.Add(PrototypeName);
    end;
    if (Names.Count > 0) and not ReceiverObject.Extensible then
      ThrowTypeError('Cannot add private elements to a non-extensible object',
        SSuggestObjectNotExtensible);
    for I := 0 to Names.Count - 1 do
    begin
      PrivateBrandToken := BytecodePrivateTokenForKey(Names[I],
        AClassValue.PrivateBrandToken);
      BrandKey := BytecodePrivateBrandKey(
        NormalizeBytecodePrivateKey(Names[I], PrivateBrandToken),
        PrivateBrandToken);
      if SeenBrandKeys.IndexOf(BrandKey) >= 0 then
        Continue;
      if TryGetRawObjectPrivateDescriptor(ReceiverObject, Names[I],
         PrototypeDescriptor) and
         not HasBytecodePrivateInitializersApplied(AInstance, AClassValue) then
      begin
        if not ReceiverObject.Extensible then
          ThrowTypeError(
            'Cannot add private elements to a non-extensible object',
            SSuggestObjectNotExtensible);
        if not ((AClassValue is TGocciaVMClassValue) and
           Assigned(TGocciaVMClassValue(AClassValue).FConstructorValue)) then
          ThrowTypeError('Cannot initialize private elements twice',
            SSuggestPrivateFieldAccess);
        Continue;
      end;
      if TryGetRawPrivateValue(AInstance, BrandKey, ExistingValue) then
      begin
        if not HasBytecodePrivateInitializersApplied(AInstance, AClassValue) then
        begin
          if not ReceiverObject.Extensible then
            ThrowTypeError(
              'Cannot add private elements to a non-extensible object',
              SSuggestObjectNotExtensible);
          if (AClassValue is TGocciaVMClassValue) and
             Assigned(TGocciaVMClassValue(AClassValue).FConstructorValue) then
            Continue;
        end;
        SourcePrivateName := NormalizeBytecodePrivateKey(Names[I],
          PrivateBrandToken);
        if IsBytecodePrivateKey(SourcePrivateName) then
        begin
          SourcePrivateName := Copy(SourcePrivateName,
            Length(BYTECODE_PRIVATE_SLOT_PREFIX) + 1, MaxInt);
          if Pos('$', SourcePrivateName) > 0 then
            SourcePrivateName := Copy(SourcePrivateName,
              Pos('$', SourcePrivateName) + 1, MaxInt)
          else if Pos(':', SourcePrivateName) > 0 then
            SourcePrivateName := Copy(SourcePrivateName,
              Pos(':', SourcePrivateName) + 1, MaxInt);
        end;
        IsPrivateFieldBrand :=
          (AClassValue.PrivateInstancePropertyDefs.Count > 0) or
          (AClassValue.FieldOrderCount > 0) or
          AClassValue.PrivateInstancePropertyDefs.ContainsKey(Names[I]) or
          AClassValue.PrivateInstancePropertyDefs.ContainsKey(SourcePrivateName);
        if IsPrivateFieldBrand then
        begin
          if not ReceiverObject.Extensible then
            ThrowTypeError(
              'Cannot add private elements to a non-extensible object',
              SSuggestObjectNotExtensible);
          Continue;
        end;
        if APreserveExistingPrivateSlots then
          Continue;
        ThrowTypeError('Cannot initialize private elements twice',
          SSuggestPrivateFieldAccess);
      end;
      SeenBrandKeys.Add(BrandKey);
      SetRawPrivateValue(AInstance, BrandKey,
        TGocciaBooleanLiteralValue.TrueValue);

      if (not (AInstance is TGocciaInstanceValue)) and
         Assigned(AClassValue.Prototype) and
         IsBytecodePrivateKey(Names[I]) then
      begin
        PrototypeDescriptor := AClassValue.Prototype.GetOwnPropertyDescriptor(
          Names[I]);
        if Assigned(PrototypeDescriptor) then
          DefineRawObjectPrivateDescriptor(ReceiverObject, Names[I],
            ClonePropertyDescriptor(PrototypeDescriptor));
      end;
    end;
  finally
    SeenBrandKeys.Free;
    Names.Free;
  end;
end;

procedure TGocciaVM.RunClassInitializers(const AClassValue: TGocciaClassValue;
  const AInstance: TGocciaValue;
  const APreserveExistingPrivateSlots: Boolean);
var
  PreviousPrivateInitializerReceiver: TGocciaValue;
  PreviousPrivateInitializerPreserveExisting: Boolean;
begin
  StampBytecodePrivateBrands(AClassValue, AInstance,
    APreserveExistingPrivateSlots);
  PreviousPrivateInitializerReceiver := FPrivateInitializerReceiver;
  PreviousPrivateInitializerPreserveExisting :=
    FPrivateInitializerPreserveExisting;
  FPrivateInitializerReceiver := AInstance;
  FPrivateInitializerPreserveExisting := APreserveExistingPrivateSlots;
  try
    AClassValue.RunMethodInitializers(AInstance);
    AClassValue.RunFieldInitializers(AInstance);
    AClassValue.RunDecoratorFieldInitializers(AInstance);
  finally
    FPrivateInitializerReceiver := PreviousPrivateInitializerReceiver;
    FPrivateInitializerPreserveExisting :=
      PreviousPrivateInitializerPreserveExisting;
  end;
end;

function TGocciaVM.MaterializeArguments(
  const AArguments: TGocciaRegisterArray): TGocciaArgumentsCollection;
var
  I: Integer;
begin
  Result := AcquireArguments(Length(AArguments));
  for I := 0 to High(AArguments) do
    Result.Add(RegisterToValue(AArguments[I]));
end;

function TGocciaVM.CreateArgumentsObjectFromCurrentFrame(
  const AUseMappedArguments: Boolean;
  const AFormalParameterCount: Integer): TGocciaObjectValue;
var
  Args: TGocciaArgumentsCollection;
  Callee: TGocciaValue;
  ParameterCells: array of TGocciaBytecodeCell;
  I: Integer;
  MappedCount: Integer;
begin
  Args := AcquireArguments(FArgCount);
  try
    for I := 0 to FArgCount - 1 do
      Args.Add(RegisterToValue(FArguments[I]));
    if AUseMappedArguments then
    begin
      MappedCount := AFormalParameterCount;
      if MappedCount > FArgCount then
        MappedCount := FArgCount;
      SetLength(ParameterCells, FArgCount);
      for I := 0 to MappedCount - 1 do
        ParameterCells[I] := GetLocalCell(I + 1);
      if Assigned(FCurrentClosure) then
        Callee := FCurrentClosure.FunctionValue
      else
        Callee := nil;
      Result := CreateMappedBytecodeArgumentsObject(Args, ParameterCells,
        Callee);
    end
    else
      Result := CreateUnmappedArgumentsObject(Args);
  finally
    ReleaseArguments(Args);
  end;
end;

function TGocciaVM.InvokeImplicitSuperInitialization(
  const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  ConstructorThisValue: TGocciaValue;
  ReceiverPrototype: TGocciaObjectValue;
  SuperResult: TGocciaValue;
  TargetInstance: TGocciaValue;
  function EffectiveNewTarget: TGocciaValue;
  begin
    if Assigned(FPendingNewTarget) then
      Exit(FPendingNewTarget);
    if Assigned(FCurrentNewTarget) then
      Exit(FCurrentNewTarget);
    Result := AClassValue;
  end;
  function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
  end;
  function RequiresObjectReturn: Boolean;
  begin
    Result := Assigned(AClassValue) and
      (Assigned(AClassValue.SuperClass) or
       Assigned(AClassValue.NativeSuperConstructor));
  end;
  procedure ValidateImplicitSuperResult(const AValue: TGocciaValue);
  begin
    if RequiresObjectReturn and
       not (AValue is TGocciaObjectValue) and
       not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  function SelectImplicitSuperResult(const AValue,
    AConstructorThisValue: TGocciaValue): TGocciaValue;
  begin
    if AValue is TGocciaObjectValue then
      Exit(AValue);
    if AConstructorThisValue is TGocciaObjectValue then
      Exit(AConstructorThisValue);
    Result := AInstance;
  end;
begin
  Result := AInstance;
  if not Assigned(AClassValue) then
    Exit;

  if AClassValue.NativeInstanceDefaultPrototype <> nil then
  begin
    TargetInstance := AClassValue.CreateNativeInstance(AArguments);
    if not (TargetInstance is TGocciaObjectValue) then
      ThrowTypeError(
        'Superclass constructor did not return an object',
        SSuggestNotConstructorType);
    if TargetInstance is TGocciaInstanceValue then
    begin
      if AInstance is TGocciaInstanceValue then
        TGocciaInstanceValue(TargetInstance).ClassValue :=
          TGocciaInstanceValue(AInstance).ClassValue
      else
        TGocciaInstanceValue(TargetInstance).ClassValue := AClassValue;
      TGocciaInstanceValue(TargetInstance).InitializeNativeFromArguments(AArguments);
    end;
    ReceiverPrototype := GetProtoFromConstructor(EffectiveNewTarget);
    TGocciaObjectValue(TargetInstance).Prototype := ReceiverPrototype;
    if TargetInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(TargetInstance).FinalizeNativeFromArguments(AArguments);
    Exit(TargetInstance);
  end;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).FConstructorValue) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    SuperResult := TGocciaVMClassValue(AClassValue).FVM.InvokeFunctionValue(
      TGocciaVMClassValue(AClassValue).FConstructorValue,
      AArguments, AInstance);
    ValidateImplicitSuperResult(SuperResult);
    if TGocciaVMClassValue(AClassValue).FConstructorValue is TGocciaBytecodeFunctionValue then
      ConstructorThisValue := RegisterToValue(
        TGocciaVMClassValue(AClassValue).FVM.FLastClosureThisValue)
    else
      ConstructorThisValue := nil;
    SuperResult := SelectImplicitSuperResult(SuperResult, ConstructorThisValue);
    if SuperResult is TGocciaObjectValue then
    begin
      if SuperResult <> AInstance then
        RunClassInitializers(AClassValue, SuperResult);
      Exit(SuperResult);
    end;
    Exit;
  end;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).NativeSuperConstructor) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    SuperResult := InvokeConstructableWithReceiver(
      TGocciaVMClassValue(AClassValue).NativeSuperConstructor,
      AArguments, AInstance);
    ValidateImplicitSuperResult(SuperResult);
    if SuperResult is TGocciaObjectValue then
    begin
      if SuperResult <> AInstance then
        RunClassInitializers(AClassValue, SuperResult);
      Exit(SuperResult);
    end;
    Exit;
  end;

  if Assigned(AClassValue.ConstructorMethod) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    SuperResult := AClassValue.ConstructorMethod.Call(AArguments, AInstance);
    ValidateImplicitSuperResult(SuperResult);
    if SuperResult is TGocciaObjectValue then
    begin
      if SuperResult <> AInstance then
        RunClassInitializers(AClassValue, SuperResult);
      Exit(SuperResult);
    end;
    Exit;
  end;

  TargetInstance := InvokeImplicitSuperInitialization(
    AClassValue.SuperClass, AInstance, AArguments);
  if not Assigned(TargetInstance) then
    TargetInstance := AInstance;
  if not (AClassValue is TGocciaVMClassValue) and
     (TargetInstance is TGocciaInstanceValue) then
    TGocciaInstanceValue(TargetInstance).InitializeNativeFromArguments(AArguments);
  RunClassInitializers(AClassValue, TargetInstance);
  Result := TargetInstance;
end;

function TGocciaVM.InvokeImplicitSuperInitializationRegisters(
  const AClassValue: TGocciaClassValue; const AInstance: TGocciaValue;
  const AArguments: TGocciaRegisterArray): TGocciaValue;
var
  BoxedArgs: TGocciaArgumentsCollection;
  BytecodeConstructor: TGocciaBytecodeFunctionValue;
  ConstructorThisValue: TGocciaValue;
  ReceiverPrototype: TGocciaObjectValue;
  SuperResult: TGocciaValue;
  SuperResultRegister: TGocciaRegister;
  TargetInstance: TGocciaValue;
  function EffectiveNewTarget: TGocciaValue;
  begin
    if Assigned(FPendingNewTarget) then
      Exit(FPendingNewTarget);
    if Assigned(FCurrentNewTarget) then
      Exit(FCurrentNewTarget);
    Result := AClassValue;
  end;
  function IsUndefinedConstructedValue(const AValue: TGocciaValue): Boolean;
  begin
    Result := (not Assigned(AValue)) or (AValue is TGocciaUndefinedLiteralValue);
  end;
  function IsUndefinedConstructedRegister(
    const AValue: TGocciaRegister): Boolean;
  begin
    Result := AValue.Kind in [grkUndefined, grkHole];
  end;
  function RequiresObjectReturn: Boolean;
  begin
    Result := Assigned(AClassValue) and
      (Assigned(AClassValue.SuperClass) or
       Assigned(AClassValue.NativeSuperConstructor));
  end;
  procedure ValidateImplicitSuperResult(const AValue: TGocciaValue);
  begin
    if RequiresObjectReturn and
       not (AValue is TGocciaObjectValue) and
       not IsUndefinedConstructedValue(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  procedure ValidateImplicitSuperRegister(const AValue: TGocciaRegister);
  begin
    if RequiresObjectReturn and
       not ((AValue.Kind = grkObject) and
            (AValue.ObjectValue is TGocciaObjectValue)) and
       not IsUndefinedConstructedRegister(AValue) then
      ThrowTypeError(
        'Derived constructor returned non-object',
        SSuggestNotConstructorType);
  end;
  function SelectImplicitSuperResult(const AValue,
    AConstructorThisValue: TGocciaValue): TGocciaValue;
  begin
    if AValue is TGocciaObjectValue then
      Exit(AValue);
    if AConstructorThisValue is TGocciaObjectValue then
      Exit(AConstructorThisValue);
    Result := AInstance;
  end;
begin
  Result := AInstance;
  if not Assigned(AClassValue) then
    Exit;

  if AClassValue.NativeInstanceDefaultPrototype <> nil then
  begin
    BoxedArgs := MaterializeArguments(AArguments);
    try
      TargetInstance := AClassValue.CreateNativeInstance(BoxedArgs);
      if not (TargetInstance is TGocciaObjectValue) then
        ThrowTypeError(
          'Superclass constructor did not return an object',
          SSuggestNotConstructorType);
      if TargetInstance is TGocciaInstanceValue then
      begin
        if AInstance is TGocciaInstanceValue then
          TGocciaInstanceValue(TargetInstance).ClassValue :=
            TGocciaInstanceValue(AInstance).ClassValue
        else
          TGocciaInstanceValue(TargetInstance).ClassValue := AClassValue;
        TGocciaInstanceValue(TargetInstance).InitializeNativeFromArguments(BoxedArgs);
      end;
      ReceiverPrototype := GetProtoFromConstructor(EffectiveNewTarget);
      TGocciaObjectValue(TargetInstance).Prototype := ReceiverPrototype;
      if TargetInstance is TGocciaInstanceValue then
        TGocciaInstanceValue(TargetInstance).FinalizeNativeFromArguments(BoxedArgs);
      Exit(TargetInstance);
    finally
      ReleaseArguments(BoxedArgs);
    end;
  end;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).FConstructorValue) then
  begin
    RunClassInitializers(AClassValue, AInstance);
    if TGocciaVMClassValue(AClassValue).FConstructorValue is TGocciaBytecodeFunctionValue then
    begin
      BytecodeConstructor := TGocciaBytecodeFunctionValue(
        TGocciaVMClassValue(AClassValue).FConstructorValue);
      if Assigned(BytecodeConstructor.FClosure) and
         Assigned(BytecodeConstructor.FClosure.Template) and
         (not BytecodeConstructor.FClosure.Template.IsAsync) then
      begin
        SuperResultRegister := TGocciaVMClassValue(AClassValue).FVM.ExecuteClosureRegisters(
          BytecodeConstructor.FClosure, RegisterObject(AInstance), AArguments);
        ValidateImplicitSuperRegister(SuperResultRegister);
        ConstructorThisValue := RegisterToValue(
          TGocciaVMClassValue(AClassValue).FVM.FLastClosureThisValue);
        SuperResult := RegisterToValue(SuperResultRegister);
        SuperResult := SelectImplicitSuperResult(SuperResult,
          ConstructorThisValue);
        if SuperResult is TGocciaObjectValue then
        begin
          if SuperResult <> AInstance then
            RunClassInitializers(AClassValue, SuperResult);
          Exit(SuperResult);
        end;
        Exit;
      end;
    end;

    BoxedArgs := MaterializeArguments(AArguments);
    try
      SuperResult := TGocciaVMClassValue(AClassValue).FVM.InvokeFunctionValue(
        TGocciaVMClassValue(AClassValue).FConstructorValue,
        BoxedArgs, AInstance);
    finally
      ReleaseArguments(BoxedArgs);
    end;
    ValidateImplicitSuperResult(SuperResult);
    if TGocciaVMClassValue(AClassValue).FConstructorValue is TGocciaBytecodeFunctionValue then
      ConstructorThisValue := RegisterToValue(
        TGocciaVMClassValue(AClassValue).FVM.FLastClosureThisValue)
    else
      ConstructorThisValue := nil;
    SuperResult := SelectImplicitSuperResult(SuperResult, ConstructorThisValue);
    if SuperResult is TGocciaObjectValue then
    begin
      if SuperResult <> AInstance then
        RunClassInitializers(AClassValue, SuperResult);
      Exit(SuperResult);
    end;
    Exit;
  end;

  if (AClassValue is TGocciaVMClassValue) and
     Assigned(TGocciaVMClassValue(AClassValue).NativeSuperConstructor) then
  begin
    BoxedArgs := MaterializeArguments(AArguments);
    try
      RunClassInitializers(AClassValue, AInstance);
      SuperResult := InvokeConstructableWithReceiver(
        TGocciaVMClassValue(AClassValue).NativeSuperConstructor,
        BoxedArgs, AInstance);
    finally
      ReleaseArguments(BoxedArgs);
    end;
    ValidateImplicitSuperResult(SuperResult);
    if SuperResult is TGocciaObjectValue then
    begin
      if SuperResult <> AInstance then
        RunClassInitializers(AClassValue, SuperResult);
      Exit(SuperResult);
    end;
    Exit;
  end;

  if Assigned(AClassValue.ConstructorMethod) then
  begin
    BoxedArgs := MaterializeArguments(AArguments);
    try
      RunClassInitializers(AClassValue, AInstance);
      SuperResult := AClassValue.ConstructorMethod.Call(BoxedArgs, AInstance);
    finally
      ReleaseArguments(BoxedArgs);
    end;
    ValidateImplicitSuperResult(SuperResult);
    if SuperResult is TGocciaObjectValue then
    begin
      if SuperResult <> AInstance then
        RunClassInitializers(AClassValue, SuperResult);
      Exit(SuperResult);
    end;
    Exit;
  end;

  TargetInstance := InvokeImplicitSuperInitializationRegisters(
    AClassValue.SuperClass, AInstance, AArguments);
  if not Assigned(TargetInstance) then
    TargetInstance := AInstance;
  if not (AClassValue is TGocciaVMClassValue) and
     (TargetInstance is TGocciaInstanceValue) then
  begin
    BoxedArgs := MaterializeArguments(AArguments);
    try
      TGocciaInstanceValue(TargetInstance).InitializeNativeFromArguments(BoxedArgs);
    finally
      ReleaseArguments(BoxedArgs);
    end;
  end;
  RunClassInitializers(AClassValue, TargetInstance);
  Result := TargetInstance;
end;

procedure TGocciaVM.SetupAutoAccessorValue(const AName: string;
  const AFlags: Integer; const AClassValue: TGocciaValue);
var
  ClassVal: TGocciaClassValue;
  IsStatic: Boolean;
  IsPrivate: Boolean;
  SourceName: string;
begin
  if Assigned(FActiveDecoratorSession) then
  begin
    if not (TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue is TGocciaClassValue) then
      Exit;
    ClassVal := TGocciaClassValue(
      TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue);
  end
  else if AClassValue is TGocciaClassValue then
    ClassVal := TGocciaClassValue(AClassValue)
  else
    Exit;
  IsStatic := (AFlags and 1) <> 0;
  IsPrivate := (AFlags and 2) <> 0;
  if IsPrivate then
  begin
    SourceName := BytecodePrivateSourceName(AName);
    if SourceName <> '' then
      ClassVal.DeclarePrivateName(SourceName, AName);
    Exit;
  end;
  ClassVal.AddAutoAccessor(AName, '__accessor_' + AName, IsStatic);
end;

procedure TGocciaVM.SetupAutoAccessorValueByKey(const AKey: TGocciaValue;
  const ABackingName: string; const AFlags: Integer);
var
  ClassVal: TGocciaClassValue;
  IsStatic: Boolean;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit;
  if not (TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue is TGocciaClassValue) then
    Exit;

  ClassVal := TGocciaClassValue(
    TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue);
  IsStatic := (AFlags and 1) <> 0;
  ClassVal.AddAutoAccessorWithKey('', AKey, ABackingName, IsStatic);
end;

procedure TGocciaVM.BeginDecorators(const AClassValue, ASuperValue: TGocciaValue);
var
  ClassVal: TGocciaClassValue;
  SuperMetadata: TGocciaValue;
  Meta: TGocciaObjectValue;
begin
  if not (AClassValue is TGocciaClassValue) then
    Exit;

  ClassVal := TGocciaClassValue(AClassValue);
  SuperMetadata := nil;

  if Assigned(ClassVal.SuperClass) then
    SuperMetadata := ClassVal.SuperClass.GetSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata)
  else if ASuperValue is TGocciaClassValue then
    SuperMetadata := TGocciaClassValue(ASuperValue).GetSymbolProperty(
      TGocciaSymbolValue.WellKnownMetadata);

  if (SuperMetadata <> nil) and (SuperMetadata is TGocciaObjectValue) then
    Meta := TGocciaObjectValue.Create(TGocciaObjectValue(SuperMetadata))
  else
    Meta := TGocciaObjectValue.Create;

  TGarbageCollector.Instance.AddTempRoot(Meta);

  if Assigned(FActiveDecoratorSession) then
  begin
    TGarbageCollector.Instance.RemoveTempRoot(
      TGocciaVMDecoratorSession(FActiveDecoratorSession).MetadataObject);
    FActiveDecoratorSession.Free;
  end;

  FActiveDecoratorSession := TGocciaVMDecoratorSession.Create(Meta);
  TGocciaVMDecoratorSession(FActiveDecoratorSession).ClassValue := ClassVal;
  TGocciaVMDecoratorSession(FActiveDecoratorSession).OriginalClassValue :=
    ClassVal;
end;

procedure TGocciaVM.ApplyClassDecorator(const ADecoratorFn: TGocciaValue);
var
  Session: TGocciaVMDecoratorSession;
  ClassVal, DecoratorResult: TGocciaValue;
  ContextObject: TGocciaObjectValue;
  DecoratorArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit;

  Session := TGocciaVMDecoratorSession(FActiveDecoratorSession);
  ClassVal := Session.ClassValue;
  if not (ClassVal is TGocciaClassValue) then
    Exit;
  if not ADecoratorFn.IsCallable then
    ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);
  RunStaticDecoratorInitializersForSession(Session);

  ContextObject := TGocciaObjectValue.Create;
  ContextObject.AssignProperty(PROP_KIND,
    TGocciaStringLiteralValue.Create('class'));
  ContextObject.AssignProperty(PROP_NAME,
    TGocciaStringLiteralValue.Create(TGocciaClassValue(ClassVal).Name));
  ContextObject.AssignProperty(PROP_METADATA, Session.MetadataObject);
  ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Session.ClassCollector.AddInitializer, PROP_ADD_INITIALIZER, 1));

  DecoratorArgs := AcquireArguments(2);
  DecoratorArgs.Add(ClassVal);
  DecoratorArgs.Add(ContextObject);
  try
    DecoratorResult := TGocciaFunctionBase(ADecoratorFn).Call(
      DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    ReleaseArguments(DecoratorArgs);
  end;

  if (DecoratorResult <> nil) and
     not (DecoratorResult is TGocciaUndefinedLiteralValue) then
  begin
    if not (DecoratorResult is TGocciaClassValue) then
      ThrowTypeError(SErrorClassDecoratorReturn, SSuggestDecoratorFunction);
    Session.ClassValue := DecoratorResult;
  end;
end;

procedure TGocciaVM.ApplyElementDecorator(const ADecoratorFn: TGocciaValue;
  const ADescriptor: string; const AComputedKey: TGocciaValue = nil);
var
  Session: TGocciaVMDecoratorSession;
  Kind: Char;
  Name: string;
  ElementName: string;
  ElementKey: TGocciaValue;
  Flags: Integer;
  IsStatic, IsPrivate: Boolean;
  ClassVal, DecoratorResult, ElementValue: TGocciaValue;
  ContextObject, AccessObject, AutoAccessorValue, DecResultObj: TGocciaObjectValue;
  AccessGetterHelper: TGocciaAccessGetter;
  AccessSetterHelper: TGocciaAccessSetter;
  Collector: TGocciaInitializerCollector;
  DecoratorArgs: TGocciaArgumentsCollection;
  KindStr: string;
  ExistingDescriptor: TGocciaPropertyDescriptor;
  GetterValue, SetterValue, NewGetter, NewSetter, NewInit: TGocciaValue;

  function GetDecoratedDataProperty(const AIsStatic: Boolean;
    const AName: string; const AKey: TGocciaValue): TGocciaValue;
  begin
    if AKey is TGocciaSymbolValue then
    begin
      if AIsStatic then
        Result := TGocciaClassValue(ClassVal).GetSymbolProperty(
          TGocciaSymbolValue(AKey))
      else
        Result := TGocciaClassValue(ClassVal).Prototype.GetSymbolProperty(
          TGocciaSymbolValue(AKey));
    end
    else if AIsStatic then
      Result := TGocciaClassValue(ClassVal).GetProperty(AName)
    else
      Result := TGocciaClassValue(ClassVal).Prototype.GetProperty(AName);
  end;

  function PrivateStaticKey(const AName: string): string;
  var
    Pair: TGocciaValueMap.TKeyValuePair;
    KeySuffix: string;
  begin
    Result := BytecodePrivateRuntimeKey(
      AName, TGocciaClassValue(ClassVal).PrivateBrandToken);
    if TGocciaClassValue(ClassVal).PrivateStaticMethods.ContainsKey(Result) then
      Exit;

    KeySuffix := '$' + AName;
    for Pair in TGocciaClassValue(ClassVal).PrivateStaticMethods do
      if (Length(Pair.Key) >= Length(KeySuffix)) and
         (Copy(Pair.Key, Length(Pair.Key) - Length(KeySuffix) + 1,
           Length(KeySuffix)) = KeySuffix) then
        Exit(Pair.Key);
  end;

  function GetPrivateMethodValue(const AName: string;
    const AIsStatic: Boolean): TGocciaValue;
  begin
    if AIsStatic then
    begin
      if not TGocciaClassValue(ClassVal).PrivateStaticMethods.TryGetValue(
        PrivateStaticKey(AName), Result) then
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
      Result := TGocciaClassValue(ClassVal).GetPrivateMethod(AName);
  end;

  procedure DefinePrivateMethodValue(const AName: string;
    const AIsStatic: Boolean; const AValue: TGocciaValue);
  begin
    if AIsStatic then
      TGocciaClassValue(ClassVal).AddPrivateStaticMethod(
        PrivateStaticKey(AName), AValue)
    else if AValue is TGocciaMethodValue then
      TGocciaClassValue(ClassVal).AddPrivateMethod(
        AName, TGocciaMethodValue(AValue));
  end;

  procedure DefineDecoratedMethodProperty(const AIsStatic: Boolean;
    const AName: string; const AKey, AValue: TGocciaValue);
  var
    TargetObject: TGocciaObjectValue;
    KeyValue: TGocciaValue;
  begin
    if AIsStatic then
      TargetObject := TGocciaClassValue(ClassVal)
    else
      TargetObject := TGocciaClassValue(ClassVal).Prototype;
    if Assigned(AKey) then
      KeyValue := AKey
    else
      KeyValue := TGocciaStringLiteralValue.Create(AName);
    if AIsStatic then
      SetBytecodeHomeObject(AValue, TGocciaClassValue(ClassVal), True)
    else
      SetBytecodeHomeObject(AValue, TargetObject);
    if KeyValue is TGocciaSymbolValue then
      TargetObject.DefineSymbolProperty(
        TGocciaSymbolValue(KeyValue),
        TGocciaPropertyDescriptorData.Create(
          AValue, [pfConfigurable, pfWritable]))
    else
      TargetObject.DefineProperty(
        KeyValue.ToStringLiteral.Value,
        TGocciaPropertyDescriptorData.Create(
          AValue, [pfConfigurable, pfWritable]));
  end;

  function GetDecoratedAccessorDescriptor(const AIsStatic: Boolean;
    const AName: string; const AKey: TGocciaValue): TGocciaPropertyDescriptor;
  begin
    if AKey is TGocciaSymbolValue then
    begin
      if AIsStatic then
        Result := TGocciaClassValue(ClassVal).GetOwnStaticSymbolDescriptor(
          TGocciaSymbolValue(AKey))
      else
        Result := TGocciaClassValue(ClassVal).Prototype
          .GetOwnSymbolPropertyDescriptor(TGocciaSymbolValue(AKey));
    end
    else if AIsStatic then
      Result := TGocciaClassValue(ClassVal).GetOwnPropertyDescriptor(AName)
    else
      Result := TGocciaClassValue(ClassVal).Prototype
        .GetOwnPropertyDescriptor(AName);
  end;

  procedure DefineDecoratedGetterProperty(const AIsStatic: Boolean;
    const AName: string; const AKey, AGetter: TGocciaValue);
  var
    TargetObject: TGocciaObjectValue;
    KeyValue: TGocciaValue;
    ExistingDescriptor: TGocciaPropertyDescriptor;
    ExistingSetter: TGocciaValue;
  begin
    if AIsStatic then
      TargetObject := TGocciaClassValue(ClassVal)
    else
      TargetObject := TGocciaClassValue(ClassVal).Prototype;
    if Assigned(AKey) then
      KeyValue := AKey
    else
      KeyValue := TGocciaStringLiteralValue.Create(AName);

    if AIsStatic then
      SetBytecodeHomeObject(AGetter, TGocciaClassValue(ClassVal), True)
    else
      SetBytecodeHomeObject(AGetter, TargetObject);
    if KeyValue is TGocciaSymbolValue then
    begin
      if AIsStatic then
        ExistingDescriptor := TGocciaClassValue(ClassVal)
          .GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(KeyValue))
      else
        ExistingDescriptor := TargetObject.GetOwnSymbolPropertyDescriptor(
          TGocciaSymbolValue(KeyValue));
    end
    else
      ExistingDescriptor := TargetObject.GetOwnPropertyDescriptor(
        KeyValue.ToStringLiteral.Value);

    ExistingSetter := nil;
    if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
      ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;

    if KeyValue is TGocciaSymbolValue then
      TargetObject.DefineSymbolProperty(
        TGocciaSymbolValue(KeyValue),
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ExistingSetter, [pfConfigurable]))
    else
      TargetObject.DefineProperty(
        KeyValue.ToStringLiteral.Value,
        TGocciaPropertyDescriptorAccessor.Create(
          AGetter, ExistingSetter, [pfConfigurable]));
  end;

  procedure DefineDecoratedSetterProperty(const AIsStatic: Boolean;
    const AName: string; const AKey, ASetter: TGocciaValue);
  var
    TargetObject: TGocciaObjectValue;
    KeyValue: TGocciaValue;
    ExistingDescriptor: TGocciaPropertyDescriptor;
    ExistingGetter: TGocciaValue;
  begin
    if AIsStatic then
      TargetObject := TGocciaClassValue(ClassVal)
    else
      TargetObject := TGocciaClassValue(ClassVal).Prototype;
    if Assigned(AKey) then
      KeyValue := AKey
    else
      KeyValue := TGocciaStringLiteralValue.Create(AName);

    if AIsStatic then
      SetBytecodeHomeObject(ASetter, TGocciaClassValue(ClassVal), True)
    else
      SetBytecodeHomeObject(ASetter, TargetObject);
    if KeyValue is TGocciaSymbolValue then
    begin
      if AIsStatic then
        ExistingDescriptor := TGocciaClassValue(ClassVal)
          .GetOwnStaticSymbolDescriptor(TGocciaSymbolValue(KeyValue))
      else
        ExistingDescriptor := TargetObject.GetOwnSymbolPropertyDescriptor(
          TGocciaSymbolValue(KeyValue));
    end
    else
      ExistingDescriptor := TargetObject.GetOwnPropertyDescriptor(
        KeyValue.ToStringLiteral.Value);

    ExistingGetter := nil;
    if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
       Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
      ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;

    if KeyValue is TGocciaSymbolValue then
      TargetObject.DefineSymbolProperty(
        TGocciaSymbolValue(KeyValue),
        TGocciaPropertyDescriptorAccessor.Create(
          ExistingGetter, ASetter, [pfConfigurable]))
    else
      TargetObject.DefineProperty(
        KeyValue.ToStringLiteral.Value,
        TGocciaPropertyDescriptorAccessor.Create(
          ExistingGetter, ASetter, [pfConfigurable]));
  end;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit;

  Session := TGocciaVMDecoratorSession(FActiveDecoratorSession);
  ClassVal := Session.ClassValue;
  if not (ClassVal is TGocciaClassValue) then
    Exit;
  if not ADecoratorFn.IsCallable then
    ThrowTypeError(SErrorDecoratorMustBeFunction, SSuggestDecoratorFunction);

  ParseElementDescriptor(ADescriptor, Kind, Name, Flags);
  IsStatic := (Flags and 1) <> 0;
  IsPrivate := (Flags and 2) <> 0;
  ElementName := Name;
  ElementKey := nil;
  if (not IsPrivate) and Assigned(AComputedKey) and
     not (AComputedKey is TGocciaUndefinedLiteralValue) then
  begin
    ElementKey := AComputedKey;
    if not (ElementKey is TGocciaSymbolValue) then
      ElementName := ElementKey.ToStringLiteral.Value;
  end;

  case Kind of
    'm': KindStr := 'method';
    'g': KindStr := 'getter';
    's': KindStr := 'setter';
    'f': KindStr := 'field';
    'a': KindStr := 'accessor';
  else
    KindStr := 'method';
  end;

  ContextObject := TGocciaObjectValue.Create;
  ContextObject.AssignProperty(PROP_KIND,
    TGocciaStringLiteralValue.Create(KindStr));
  if IsPrivate then
    ContextObject.AssignProperty(PROP_NAME,
      TGocciaStringLiteralValue.Create('#' + Name))
  else if ElementKey is TGocciaSymbolValue then
    ContextObject.AssignProperty(PROP_NAME, ElementKey)
  else if Assigned(ElementKey) then
    ContextObject.AssignProperty(PROP_NAME,
      TGocciaStringLiteralValue.Create(ElementName))
  else
    ContextObject.AssignProperty(PROP_NAME,
      TGocciaStringLiteralValue.Create(Name));
  if IsStatic then
    ContextObject.AssignProperty(PROP_STATIC,
      TGocciaBooleanLiteralValue.TrueValue)
  else
    ContextObject.AssignProperty(PROP_STATIC,
      TGocciaBooleanLiteralValue.FalseValue);
  if IsPrivate then
    ContextObject.AssignProperty(PROP_PRIVATE,
      TGocciaBooleanLiteralValue.TrueValue)
  else
    ContextObject.AssignProperty(PROP_PRIVATE,
      TGocciaBooleanLiteralValue.FalseValue);
  ContextObject.AssignProperty(PROP_METADATA, Session.MetadataObject);

  AccessObject := TGocciaObjectValue.Create;
  case Kind of
    'm':
      begin
        if IsPrivate then
          ElementValue := GetPrivateMethodValue(Name, IsStatic)
        else
          ElementValue := GetDecoratedDataProperty(
            IsStatic, ElementName, ElementKey);
        if ElementKey is TGocciaSymbolValue then
          AccessGetterHelper := TGocciaAccessGetter.CreateWithKey(
            ElementValue, ElementKey)
        else
          AccessGetterHelper := TGocciaAccessGetter.Create(
            ElementValue, ElementName);
        AccessObject.AssignProperty(PROP_GET,
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            AccessGetterHelper.Get, PROP_GET, 0));
        ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
      end;
    'g':
      begin
        if not IsPrivate then
        begin
          if ElementKey is TGocciaSymbolValue then
            AccessGetterHelper := TGocciaAccessGetter.CreateWithKey(
              nil, ElementKey)
          else
            AccessGetterHelper := TGocciaAccessGetter.Create(nil, ElementName);
          AccessObject.AssignProperty(PROP_GET,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(
              AccessGetterHelper.Get, PROP_GET, 0));
          ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
        end;
      end;
    's':
      begin
        if not IsPrivate then
        begin
          if ElementKey is TGocciaSymbolValue then
            AccessSetterHelper := TGocciaAccessSetter.CreateWithKey(
              ElementKey)
          else
            AccessSetterHelper := TGocciaAccessSetter.Create(ElementName);
          AccessObject.AssignProperty(PROP_SET,
            TGocciaNativeFunctionValue.CreateWithoutPrototype(
              AccessSetterHelper.SetValue, PROP_SET, 1));
          ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
        end;
      end;
    'f', 'a':
      begin
        if ElementKey is TGocciaSymbolValue then
        begin
          AccessGetterHelper := TGocciaAccessGetter.CreateWithKey(
            nil, ElementKey);
          AccessSetterHelper := TGocciaAccessSetter.CreateWithKey(
            ElementKey);
        end
        else
        begin
          AccessGetterHelper := TGocciaAccessGetter.Create(nil, ElementName);
          AccessSetterHelper := TGocciaAccessSetter.Create(ElementName);
        end;
        AccessObject.AssignProperty(PROP_GET,
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            AccessGetterHelper.Get, PROP_GET, 0));
        AccessObject.AssignProperty(PROP_SET,
          TGocciaNativeFunctionValue.CreateWithoutPrototype(
            AccessSetterHelper.SetValue, PROP_SET, 1));
        ContextObject.AssignProperty(PROP_ACCESS, AccessObject);
      end;
  end;

  if IsStatic then
    Collector := Session.StaticFieldCollector
  else if Kind in ['f', 'a'] then
    Collector := Session.FieldCollector
  else
    Collector := Session.MethodCollector;
  ContextObject.AssignProperty(PROP_ADD_INITIALIZER,
    TGocciaNativeFunctionValue.CreateWithoutPrototype(
      Collector.AddInitializer, PROP_ADD_INITIALIZER, 1));

  case Kind of
    'm':
      ; // already set above
    'g':
      begin
        if IsPrivate then
          ElementValue := TGocciaClassValue(ClassVal).PrivatePropertyGetter[Name]
        else
        begin
          ExistingDescriptor := GetDecoratedAccessorDescriptor(
            IsStatic, ElementName, ElementKey);
          ElementValue := nil;
          if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
             Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
            ElementValue :=
              TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;
        end;
      end;
    's':
      begin
        if IsPrivate then
          ElementValue := TGocciaClassValue(ClassVal).PrivatePropertySetter[Name]
        else
        begin
          ExistingDescriptor := GetDecoratedAccessorDescriptor(
            IsStatic, ElementName, ElementKey);
          ElementValue := nil;
          if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
             Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
            ElementValue :=
              TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;
        end;
      end;
    'f':
      ElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    'a':
      begin
        AutoAccessorValue := TGocciaObjectValue.Create;
        ExistingDescriptor := GetDecoratedAccessorDescriptor(
          IsStatic, ElementName, ElementKey);
        GetterValue := nil;
        SetterValue := nil;
        if ExistingDescriptor is TGocciaPropertyDescriptorAccessor then
        begin
          GetterValue := TGocciaPropertyDescriptorAccessor(
            ExistingDescriptor).Getter;
          SetterValue := TGocciaPropertyDescriptorAccessor(
            ExistingDescriptor).Setter;
        end;
        AutoAccessorValue.AssignProperty(PROP_GET, GetterValue);
        AutoAccessorValue.AssignProperty(PROP_SET, SetterValue);
        ElementValue := AutoAccessorValue;
      end;
  end;

  DecoratorArgs := AcquireArguments(2);
  DecoratorArgs.Add(ElementValue);
  DecoratorArgs.Add(ContextObject);
  try
    DecoratorResult := TGocciaFunctionBase(ADecoratorFn).Call(
      DecoratorArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
  finally
    ReleaseArguments(DecoratorArgs);
  end;

  if (DecoratorResult = nil) or
     (DecoratorResult is TGocciaUndefinedLiteralValue) then
    Exit;

  case Kind of
    'm':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError(SErrorMethodDecoratorReturn, SSuggestDecoratorFunction);
        if IsPrivate then
          DefinePrivateMethodValue(Name, IsStatic, DecoratorResult)
        else
          DefineDecoratedMethodProperty(
            IsStatic, ElementName, ElementKey, DecoratorResult);
      end;
    'g':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError(SErrorGetterDecoratorReturn, SSuggestDecoratorFunction);
        if IsPrivate then
          TGocciaClassValue(ClassVal).AddPrivateGetter(
            Name, TGocciaFunctionBase(DecoratorResult))
        else
          DefineDecoratedGetterProperty(
            IsStatic, ElementName, ElementKey, DecoratorResult);
      end;
    's':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError(SErrorSetterDecoratorReturn, SSuggestDecoratorFunction);
        if IsPrivate then
          TGocciaClassValue(ClassVal).AddPrivateSetter(
            Name, TGocciaFunctionBase(DecoratorResult))
        else
          DefineDecoratedSetterProperty(
            IsStatic, ElementName, ElementKey, DecoratorResult);
      end;
    'f':
      begin
        if not DecoratorResult.IsCallable then
          ThrowTypeError(SErrorFieldDecoratorReturn, SSuggestDecoratorFunction);
        TGocciaClassValue(ClassVal).AddFieldInitializerWithKey(
          ElementName, ElementKey, DecoratorResult, IsPrivate, IsStatic);
      end;
    'a':
      begin
        if not (DecoratorResult is TGocciaObjectValue) then
          ThrowTypeError(SErrorAccessorDecoratorReturn, SSuggestDecoratorFunction);
        DecResultObj := TGocciaObjectValue(DecoratorResult);
        NewGetter := DecResultObj.GetProperty(PROP_GET);
        NewSetter := DecResultObj.GetProperty(PROP_SET);
        NewInit := DecResultObj.GetProperty(PROP_INIT);

        if Assigned(NewGetter) and not (NewGetter is TGocciaUndefinedLiteralValue) then
        begin
          GetterValue := NewGetter;
          DefineDecoratedGetterProperty(
            IsStatic, ElementName, ElementKey, GetterValue);
        end;

        if Assigned(NewSetter) and not (NewSetter is TGocciaUndefinedLiteralValue) then
        begin
          SetterValue := NewSetter;
          DefineDecoratedSetterProperty(
            IsStatic, ElementName, ElementKey, SetterValue);
        end;

        if Assigned(NewInit) and not (NewInit is TGocciaUndefinedLiteralValue) and
           NewInit.IsCallable then
          TGocciaClassValue(ClassVal).AddFieldInitializerWithKey(
            ElementName, ElementKey, NewInit, IsPrivate, IsStatic);
      end;
  end;
end;

function TGocciaVM.FinishDecorators(const ACurrentValue: TGocciaValue): TGocciaValue;
var
  Session: TGocciaVMDecoratorSession;
  ClassVal: TGocciaClassValue;
  InitializerResults: TArray<TGocciaValue>;
  I: Integer;
  InitArgs: TGocciaArgumentsCollection;
begin
  if not Assigned(FActiveDecoratorSession) then
    Exit(ACurrentValue);

  Session := TGocciaVMDecoratorSession(FActiveDecoratorSession);
  if not (Session.ClassValue is TGocciaClassValue) then
  begin
    TGarbageCollector.Instance.RemoveTempRoot(Session.MetadataObject);
    FActiveDecoratorSession.Free;
    FActiveDecoratorSession := nil;
    Exit(ACurrentValue);
  end;

  RunStaticDecoratorInitializersForSession(Session);
  ClassVal := TGocciaClassValue(Session.ClassValue);
  ClassVal.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownMetadata,
    TGocciaPropertyDescriptorData.Create(
      Session.MetadataObject, [pfConfigurable]));

  InitializerResults := Session.MethodCollector.GetInitializers;
  ClassVal.AppendMethodInitializers(InitializerResults);
  InitializerResults := Session.FieldCollector.GetInitializers;
  ClassVal.AppendFieldInitializers(InitializerResults);

  InitializerResults := Session.ClassCollector.GetInitializers;
  for I := 0 to High(InitializerResults) do
  begin
    InitArgs := AcquireArguments;
    try
      TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassVal);
    finally
      ReleaseArguments(InitArgs);
    end;
  end;

  InitializerResults := Session.StaticFieldCollector.GetInitializers;
  for I := 0 to High(InitializerResults) do
  begin
    InitArgs := AcquireArguments;
    try
      TGocciaFunctionBase(InitializerResults[I]).Call(InitArgs, ClassVal);
    finally
      ReleaseArguments(InitArgs);
    end;
  end;

  Result := ClassVal;
  TGarbageCollector.Instance.RemoveTempRoot(Session.MetadataObject);
  FActiveDecoratorSession.Free;
  FActiveDecoratorSession := nil;
end;

function TGocciaVM.GetSuperPropertyValue(const ASuperValue, AThisValue: TGocciaValue;
  const AName: string; const AUseSuperConstructor: Boolean): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
  SuperObject: TGocciaObjectValue;
  HomeObject: TGocciaObjectValue;
  SuperPrototype: TGocciaValue;
  function ResolveCurrentCtorClass: TGocciaClassValue;
  begin
    Result := nil;
    if Assigned(FCurrentClosure) and
       (FCurrentClosure.HomeClass is TGocciaClassValue) then
      Exit(TGocciaClassValue(FCurrentClosure.HomeClass));
    if AThisValue is TGocciaInstanceValue then
      Result := TGocciaInstanceValue(AThisValue).ClassValue;
  end;
  function ResolveSuperConstructor: TGocciaValue;
  var
    CurrentCtorClass: TGocciaClassValue;
  begin
    CurrentCtorClass := ResolveCurrentCtorClass;
    if Assigned(CurrentCtorClass) then
      Exit(TGocciaObjectValue(CurrentCtorClass).Prototype);
    Result := ASuperValue;
  end;
begin
  HomeObject := nil;
  if Assigned(FCurrentClosure) then
    HomeObject := FCurrentClosure.HomeObject;

  if AUseSuperConstructor and (AName = PROP_CONSTRUCTOR) then
    Exit(TGocciaVMSuperConstructorValue.Create(ResolveSuperConstructor,
      FCurrentNewTarget, ResolveCurrentCtorClass));

  if (ASuperValue is TGocciaObjectValue) and
     (not (ASuperValue is TGocciaClassValue)) and
     ASuperValue.IsConstructable then
  begin
    SuperObject := TGocciaObjectValue(ASuperValue);
    if AUseSuperConstructor and (AName = PROP_CONSTRUCTOR) then
      Exit(TGocciaVMSuperConstructorValue.Create(SuperObject,
        FCurrentNewTarget, ResolveCurrentCtorClass));

    if AThisValue is TGocciaClassValue then
      Exit(SuperObject.GetPropertyWithContext(AName, AThisValue));

    if Assigned(HomeObject) then
    begin
      SuperPrototype := HomeObject.Prototype;
      if SuperPrototype is TGocciaObjectValue then
        Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
          AName, AThisValue));
      ThrowTypeError(SErrorCannotConvertNullOrUndefined,
        SSuggestCheckNullBeforeAccess);
    end;

    SuperPrototype := SuperObject.GetProperty(PROP_PROTOTYPE);
    if SuperPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
        AName, AThisValue));

    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);
  end;

  if not (ASuperValue is TGocciaClassValue) then
  begin
    if Assigned(HomeObject) then
    begin
      SuperPrototype := HomeObject.Prototype;
      if SuperPrototype is TGocciaObjectValue then
        Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
          AName, AThisValue));
      ThrowTypeError(SErrorCannotConvertNullOrUndefined,
        SSuggestCheckNullBeforeAccess);
    end;
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);
  end;

  SuperClass := TGocciaClassValue(ASuperValue);
  if AUseSuperConstructor and (AName = PROP_CONSTRUCTOR) then
    Exit(TGocciaVMSuperConstructorValue.Create(SuperClass,
      FCurrentNewTarget, ResolveCurrentCtorClass));

  if AThisValue is TGocciaClassValue then
    Exit(SuperClass.GetPropertyWithContext(AName, AThisValue));

  if Assigned(HomeObject) then
  begin
    SuperPrototype := HomeObject.Prototype;
    if SuperPrototype is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperPrototype).GetPropertyWithContext(
        AName, AThisValue));
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);
  end;

  if Assigned(SuperClass.Prototype) then
    Exit(SuperClass.Prototype.GetPropertyWithContext(AName, AThisValue));

  ThrowTypeError(SErrorCannotConvertNullOrUndefined,
    SSuggestCheckNullBeforeAccess);
end;

function TGocciaVM.GetSuperPropertyValueByKey(const ASuperValue, AThisValue,
  AKey: TGocciaValue; const AUseSuperConstructor: Boolean): TGocciaValue;
var
  SuperClass: TGocciaClassValue;
  SuperObject: TGocciaObjectValue;
  HomeObject: TGocciaObjectValue;
  SuperPrototype: TGocciaValue;
  KeyValue: TGocciaValue;
  function ResolveCurrentCtorClass: TGocciaClassValue;
  begin
    Result := nil;
    if Assigned(FCurrentClosure) and
       (FCurrentClosure.HomeClass is TGocciaClassValue) then
      Exit(TGocciaClassValue(FCurrentClosure.HomeClass));
    if AThisValue is TGocciaInstanceValue then
      Result := TGocciaInstanceValue(AThisValue).ClassValue;
  end;
  function ResolveSuperConstructor: TGocciaValue;
  var
    CurrentCtorClass: TGocciaClassValue;
  begin
    CurrentCtorClass := ResolveCurrentCtorClass;
    if Assigned(CurrentCtorClass) then
      Exit(TGocciaObjectValue(CurrentCtorClass).Prototype);
    Result := ASuperValue;
  end;
  function IsSuperConstructorKey: Boolean;
  begin
    Result := AUseSuperConstructor and
      not (KeyValue is TGocciaSymbolValue) and
      (KeyToPropertyName(KeyValue) = PROP_CONSTRUCTOR);
  end;
  function ReadSuperProperty(const AObject: TGocciaObjectValue): TGocciaValue;
  begin
    if KeyValue is TGocciaSymbolValue then
      Result := AObject.GetSymbolPropertyWithReceiver(
        TGocciaSymbolValue(KeyValue), AThisValue)
    else
      Result := AObject.GetPropertyWithContext(KeyToPropertyName(KeyValue),
        AThisValue);
  end;
begin
  HomeObject := nil;
  if Assigned(FCurrentClosure) then
    HomeObject := FCurrentClosure.HomeObject;

  // ES2026 §13.3.7.1 resolves the super reference before downstream
  // ToPropertyKey coercion. Capture every mutable base source before that
  // coercion can run user code.
  SuperPrototype := nil;
  if Assigned(HomeObject) then
    SuperPrototype := HomeObject.Prototype
  else if (ASuperValue is TGocciaObjectValue) and
          (not (ASuperValue is TGocciaClassValue)) and
          ASuperValue.IsConstructable and
          not (AThisValue is TGocciaClassValue) then
  begin
    SuperObject := TGocciaObjectValue(ASuperValue);
    SuperPrototype := SuperObject.GetProperty(PROP_PROTOTYPE);
  end
  else if (ASuperValue is TGocciaClassValue) and
          not (AThisValue is TGocciaClassValue) then
  begin
    SuperClass := TGocciaClassValue(ASuperValue);
    SuperPrototype := SuperClass.Prototype;
  end;

  KeyValue := ToPropertyKey(AKey);

  if IsSuperConstructorKey then
    Exit(TGocciaVMSuperConstructorValue.Create(ResolveSuperConstructor,
      FCurrentNewTarget, ResolveCurrentCtorClass));

  if (ASuperValue is TGocciaObjectValue) and
     (not (ASuperValue is TGocciaClassValue)) and
     ASuperValue.IsConstructable then
  begin
    SuperObject := TGocciaObjectValue(ASuperValue);
    if IsSuperConstructorKey then
      Exit(TGocciaVMSuperConstructorValue.Create(SuperObject,
        FCurrentNewTarget, ResolveCurrentCtorClass));

    if AThisValue is TGocciaClassValue then
      Exit(ReadSuperProperty(SuperObject));

    if Assigned(HomeObject) then
    begin
      if SuperPrototype is TGocciaObjectValue then
        Exit(ReadSuperProperty(TGocciaObjectValue(SuperPrototype)));
      ThrowTypeError(SErrorCannotConvertNullOrUndefined,
        SSuggestCheckNullBeforeAccess);
    end;

    if SuperPrototype is TGocciaObjectValue then
      Exit(ReadSuperProperty(TGocciaObjectValue(SuperPrototype)));

    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);
  end;

  if not (ASuperValue is TGocciaClassValue) then
  begin
    if Assigned(HomeObject) then
    begin
      if SuperPrototype is TGocciaObjectValue then
        Exit(ReadSuperProperty(TGocciaObjectValue(SuperPrototype)));
      ThrowTypeError(SErrorCannotConvertNullOrUndefined,
        SSuggestCheckNullBeforeAccess);
    end;
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);
  end;

  SuperClass := TGocciaClassValue(ASuperValue);
  if IsSuperConstructorKey then
    Exit(TGocciaVMSuperConstructorValue.Create(SuperClass,
      FCurrentNewTarget, ResolveCurrentCtorClass));

  if AThisValue is TGocciaClassValue then
    Exit(ReadSuperProperty(SuperClass));

  if Assigned(HomeObject) then
  begin
    if SuperPrototype is TGocciaObjectValue then
      Exit(ReadSuperProperty(TGocciaObjectValue(SuperPrototype)));
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);
  end;

  if SuperPrototype is TGocciaObjectValue then
    Exit(ReadSuperProperty(TGocciaObjectValue(SuperPrototype)));

  ThrowTypeError(SErrorCannotConvertNullOrUndefined,
    SSuggestCheckNullBeforeAccess);
end;

function TGocciaVM.ResolveSuperPropertyBaseValue(const ASuperValue,
  AThisValue: TGocciaValue): TGocciaValue;
var
  HomeObject: TGocciaObjectValue;
begin
  HomeObject := nil;
  if Assigned(FCurrentClosure) then
    HomeObject := FCurrentClosure.HomeObject;

  if Assigned(HomeObject) then
    Exit(HomeObject.Prototype);

  if AThisValue is TGocciaClassValue then
  begin
    if ASuperValue is TGocciaObjectValue then
      Exit(ASuperValue);
  end
  else if ASuperValue is TGocciaClassValue then
    Exit(TGocciaClassValue(ASuperValue).Prototype)
  else if ASuperValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ASuperValue).Prototype);

  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaVM.GetSuperPropertyValueFromBase(const ABaseValue,
  AThisValue, AKey: TGocciaValue): TGocciaValue;
var
  BaseObject: TGocciaObjectValue;
  KeyValue: TGocciaValue;
begin
  if not (ABaseValue is TGocciaObjectValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined,
      SSuggestCheckNullBeforeAccess);

  BaseObject := TGocciaObjectValue(ABaseValue);
  KeyValue := ToPropertyKey(AKey);
  if KeyValue is TGocciaSymbolValue then
    Result := BaseObject.GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue(KeyValue), AThisValue)
  else
    Result := BaseObject.GetPropertyWithContext(KeyToPropertyName(KeyValue),
      AThisValue);
end;

procedure TGocciaVM.SetSuperPropertyValueByKey(const ASuperValue, AThisValue,
  AKey, AValue: TGocciaValue);
var
  BaseObject: TGocciaObjectValue;
  BaseValue: TGocciaValue;
  HomeObject: TGocciaObjectValue;
  KeyValue: TGocciaValue;
  NonStrictSet: Boolean;
  PropertyName: string;
  Success: Boolean;
begin
  BaseValue := nil;
  HomeObject := nil;
  if Assigned(FCurrentClosure) then
    HomeObject := FCurrentClosure.HomeObject;

  if Assigned(HomeObject) then
    BaseValue := HomeObject.Prototype
  else if AThisValue is TGocciaClassValue then
  begin
    if ASuperValue is TGocciaObjectValue then
      BaseValue := ASuperValue;
  end
  else if ASuperValue is TGocciaClassValue then
    BaseValue := TGocciaClassValue(ASuperValue).Prototype
  else if ASuperValue is TGocciaObjectValue then
    BaseValue := TGocciaObjectValue(ASuperValue).Prototype;

  if not (BaseValue is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, ['super']),
      SSuggestCheckNullBeforeAccess);

  KeyValue := ToPropertyKey(AKey);
  if KeyValue is TGocciaSymbolValue then
    PropertyName := TGocciaSymbolValue(KeyValue).ToDisplayString.Value
  else
    PropertyName := TGocciaStringLiteralValue(KeyValue).Value;

  BaseObject := TGocciaObjectValue(BaseValue);
  if KeyValue is TGocciaSymbolValue then
    Success := BaseObject.AssignSymbolPropertyWithReceiver(
      TGocciaSymbolValue(KeyValue), AValue, AThisValue)
  else
    Success := BaseObject.AssignPropertyWithReceiver(PropertyName, AValue,
      AThisValue);

  if not Success then
  begin
    NonStrictSet := Assigned(FCurrentClosure) and
      Assigned(FCurrentClosure.Template) and
      not FCurrentClosure.Template.StrictCode;
    if NonStrictSet then
      Exit;
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [PropertyName]),
      SSuggestCannotDeleteNonConfigurable);
  end;
end;

procedure TGocciaVM.SetSuperPropertyBaseValueByKey(const ABaseValue,
  AThisValue, AKey, AValue: TGocciaValue);
var
  BaseObject: TGocciaObjectValue;
  KeyValue: TGocciaValue;
  NonStrictSet: Boolean;
  PropertyName: string;
  Success: Boolean;
begin
  if not (ABaseValue is TGocciaObjectValue) then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, ['super']),
      SSuggestCheckNullBeforeAccess);

  KeyValue := ToPropertyKey(AKey);
  if KeyValue is TGocciaSymbolValue then
    PropertyName := TGocciaSymbolValue(KeyValue).ToDisplayString.Value
  else
    PropertyName := TGocciaStringLiteralValue(KeyValue).Value;

  BaseObject := TGocciaObjectValue(ABaseValue);
  if KeyValue is TGocciaSymbolValue then
    Success := BaseObject.AssignSymbolPropertyWithReceiver(
      TGocciaSymbolValue(KeyValue), AValue, AThisValue)
  else
    Success := BaseObject.AssignPropertyWithReceiver(PropertyName, AValue,
      AThisValue);

  if not Success then
  begin
    NonStrictSet := Assigned(FCurrentClosure) and
      Assigned(FCurrentClosure.Template) and
      not FCurrentClosure.Template.StrictCode;
    if NonStrictSet then
      Exit;
    ThrowTypeError(Format(SErrorCannotAssignReadOnly, [PropertyName]),
      SSuggestCannotDeleteNonConfigurable);
  end;
end;


function TGocciaVM.GetPropertyValue(const AObject: TGocciaValue;
  const AKey: string): TGocciaValue;
var
  Boxed: TGocciaObjectValue;
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  PrivateName: string;
  PrivateBrandToken: string;
  BrandValue: TGocciaValue;
  EmptyArgs: TGocciaArgumentsCollection;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined, [AKey]),
      SSuggestCheckNullBeforeAccess);

  if IsBytecodePrivateKey(AKey) then
  begin
    PrivateBrandToken := ResolveBytecodePrivateBrandToken(AKey, AObject);
    if AObject is TGocciaClassValue then
      PrivateName := BytecodePrivateRuntimeKey(AKey, PrivateBrandToken)
    else
      PrivateName := NormalizeBytecodePrivateKey(AKey, PrivateBrandToken);
    if AObject is TGocciaClassValue then
    begin
      if TGocciaClassValue(AObject).HasOwnPrivateGetter(PrivateName) then
      begin
        EmptyArgs := TGocciaArgumentsCollection.Create;
        try
          Exit(TGocciaClassValue(AObject).GetOwnPrivatePropertyGetter(
            PrivateName).Call(
            EmptyArgs, AObject));
        finally
          EmptyArgs.Free;
        end;
      end;
      if TGocciaClassValue(AObject).HasOwnPrivateSetter(PrivateName) then
        ThrowBytecodePrivateTypeError(AKey,
          Format(SErrorPrivateAccessorNoGetter, [AKey]));
      if TGocciaClassValue(AObject).PrivateStaticMethods.TryGetValue(
        PrivateName, Result) then
        Exit;
      if TryGetRawPrivateValue(AObject, AKey, Result) then
        Exit;
      ThrowBytecodePrivateTypeError(AKey,
        Format(SErrorPrivateFieldNotAccessible, [AKey]));
    end;

    if AObject is TGocciaObjectValue then
    begin
      Current := TGocciaObjectValue(AObject);
      while Assigned(Current) do
      begin
        if not TryGetRawObjectPrivateDescriptor(Current, AKey, Descriptor) then
          Descriptor := Current.GetOwnPropertyDescriptor(AKey);
        if Descriptor is TGocciaPropertyDescriptorAccessor then
        begin
          if not TryGetRawPrivateValue(
            AObject, BytecodePrivateBrandKey(AKey, PrivateBrandToken),
            BrandValue) then
            ThrowBytecodePrivateTypeError(AKey,
              Format(SErrorPrivateFieldNotAccessible, [AKey]));
          if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) then
          begin
            EmptyArgs := TGocciaArgumentsCollection.Create;
            try
              Exit(InvokeFunctionValue(
                TGocciaPropertyDescriptorAccessor(Descriptor).Getter,
                EmptyArgs, AObject));
            finally
              EmptyArgs.Free;
            end;
          end;
          ThrowBytecodePrivateTypeError(AKey,
            Format(SErrorPrivateAccessorNoGetter, [AKey]));
        end;
        if Descriptor is TGocciaPropertyDescriptorData then
        begin
          if not TryGetRawPrivateValue(
            AObject, BytecodePrivateBrandKey(AKey, PrivateBrandToken),
            BrandValue) then
            ThrowBytecodePrivateTypeError(AKey,
              Format(SErrorPrivateFieldNotAccessible, [AKey]));
          Exit(TGocciaPropertyDescriptorData(Descriptor).Value);
        end;
        Current := Current.Prototype;
      end;
    end;

    if TryGetRawPrivateValue(AObject, AKey, Result) then
      Exit;
    ThrowBytecodePrivateTypeError(AKey,
      Format(SErrorPrivateFieldNotAccessible, [AKey]));
  end
  else if TryGetRawPrivateValue(AObject, AKey, Result) then
    Exit;

  Result := AObject.GetProperty(AKey);
  if Assigned(Result) then
    Exit;

  Boxed := AObject.Box;
  if Assigned(Boxed) then
    Result := Boxed.GetPropertyWithContext(AKey, AObject)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

// Computed GET for receivers that are not arrays, classes, or objects:
// string/number/boolean/bigint/symbol primitives resolve through their
// boxed object per OrdinaryGet, mirroring the interpreter. Deliberately
// bypasses GetPropertyValue so user string keys that collide with the
// '#slot:' private-key mangling stay ordinary property names.
procedure TGocciaVM.ExecGetComputedPropertyFallback(const ADest: Integer;
  const AReceiverReg, AKeyReg: TGocciaRegister);
var
  ReceiverValue: TGocciaValue;
  PropKeyValue: TGocciaValue;
  PropertyValue: TGocciaValue;
  Boxed: TGocciaObjectValue;
  PropertyName: string;
  StringUnit, StringValue: string;
  KeyIndex: Integer;
begin
  if (AReceiverReg.Kind = grkObject) and
     (AReceiverReg.ObjectValue is TGocciaStringLiteralValue) and
     TryGetArrayIndexRegister(AKeyReg, KeyIndex) then
  begin
    StringValue := TGocciaStringLiteralValue(AReceiverReg.ObjectValue).Value;
    StringUnit := UTF16CodeUnitAt(StringValue, KeyIndex);
    if StringUnit <> '' then
    begin
      if (Length(StringUnit) = 1) and (Ord(StringUnit[1]) <= 127) then
        SetRegisterFast(ADest,
          CachedASCIIStringValue(
            TASCIIStringCodeUnit(Ord(StringUnit[1]))))
      else
        SetRegisterFast(ADest, TGocciaStringLiteralValue.Create(StringUnit));
      Exit;
    end;
  end;

  ReceiverValue := RegisterToValue(AReceiverReg);

  if (AKeyReg.Kind = grkObject) and
     (AKeyReg.ObjectValue is TGocciaSymbolValue) then
    PropKeyValue := AKeyReg.ObjectValue
  else if not TryResolveObjectKey(AKeyReg, PropKeyValue) then
    PropKeyValue := nil;

  if PropKeyValue is TGocciaSymbolValue then
  begin
    if (ReceiverValue is TGocciaSymbolValue) and
       (TGocciaSymbolValue.SharedPrototype <> nil) then
      SetRegister(ADest, TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype)
        .GetSymbolPropertyWithReceiver(
          TGocciaSymbolValue(PropKeyValue), ReceiverValue))
    else
    begin
      Boxed := ReceiverValue.Box;
      if Assigned(Boxed) then
        SetRegister(ADest, Boxed.GetSymbolPropertyWithReceiver(
          TGocciaSymbolValue(PropKeyValue), ReceiverValue))
      else
        FRegisters[ADest] := RegisterUndefined;
    end;
    Exit;
  end;

  if PropKeyValue is TGocciaStringLiteralValue then
    PropertyName := TGocciaStringLiteralValue(PropKeyValue).Value
  else
    PropertyName := KeyToPropertyNameRegister(AKeyReg);

  if (ReceiverValue is TGocciaStringLiteralValue) and
     (PropertyName = PROP_LENGTH) then
  begin
    FRegisters[ADest] := VMNumberRegister(UTF16CodeUnitLength(
      TGocciaStringLiteralValue(ReceiverValue).Value));
    Exit;
  end;

  PropertyValue := ReceiverValue.GetProperty(PropertyName);
  if not Assigned(PropertyValue) then
  begin
    Boxed := ReceiverValue.Box;
    if Assigned(Boxed) then
      PropertyValue := Boxed.GetPropertyWithContext(PropertyName,
        ReceiverValue)
    else
      PropertyValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
  SetRegister(ADest, PropertyValue);
end;

// ES2026 §10.2.1.2 OrdinaryCallBindThis steps 5–6 for non-strict callees,
// operating on registers so the f.call/f.apply fast paths avoid a
// register/value round trip for receivers that are already objects.
function TGocciaVM.CoerceNonStrictThisRegister(
  const AThisRegister: TGocciaRegister;
  const AGlobalThisValue: TGocciaValue;
  const ARealm: TGocciaRealm): TGocciaRegister;
var
  BoxingRealm, PreviousRealm: TGocciaRealm;
  GlobalThisValue: TGocciaValue;
  ShouldSwitchRealm: Boolean;
begin
  if Assigned(AGlobalThisValue) then
    GlobalThisValue := AGlobalThisValue
  else
    GlobalThisValue := FGlobalThisValue;

  if AThisRegister.Kind in [grkUndefined, grkNull] then
  begin
    if Assigned(GlobalThisValue) then
      Result := VMValueToRegisterFast(GlobalThisValue)
    else
      Result := AThisRegister;
  end
  else if (AThisRegister.Kind = grkObject) and
          (AThisRegister.ObjectValue is TGocciaObjectValue) then
    Result := AThisRegister
  else
  begin
    BoxingRealm := ARealm;
    PreviousRealm := CurrentRealm;
    ShouldSwitchRealm := Assigned(BoxingRealm) and
      (BoxingRealm <> PreviousRealm);
    if ShouldSwitchRealm then
      SetCurrentRealm(BoxingRealm);
    try
      Result := VMValueToRegisterFast(CoerceNonStrictThis(
        RegisterToValue(AThisRegister), GlobalThisValue));
    finally
      if ShouldSwitchRealm then
        SetCurrentRealm(PreviousRealm);
    end;
  end;
end;

procedure TGocciaVM.SetPropertyValue(const AObject: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue);
var
  Current: TGocciaObjectValue;
  Descriptor: TGocciaPropertyDescriptor;
  SetterArgs: TGocciaArgumentsCollection;
  PrivateName: string;
  BoxedValue: TGocciaObjectValue;
  PrivateBrandToken: string;
  ExistingValue: TGocciaValue;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if IsBytecodePrivateKey(AKey) then
  begin
    PrivateBrandToken := ResolveBytecodePrivateBrandToken(AKey, AObject);
    if AObject is TGocciaClassValue then
      PrivateName := BytecodePrivateRuntimeKey(AKey, PrivateBrandToken)
    else
      PrivateName := NormalizeBytecodePrivateKey(AKey, PrivateBrandToken);
    if AObject is TGocciaClassValue then
    begin
      if TGocciaClassValue(AObject).HasOwnPrivateSetter(PrivateName) then
      begin
        SetterArgs := TGocciaArgumentsCollection.Create([AValue]);
        try
          TGocciaClassValue(AObject).GetOwnPrivatePropertySetter(
            PrivateName).Call(
            SetterArgs, AObject);
        finally
          SetterArgs.Free;
        end;
        Exit;
      end;
      if TGocciaClassValue(AObject).HasOwnPrivateGetter(PrivateName) then
        ThrowBytecodePrivateTypeError(AKey,
          Format(SErrorPrivateAccessorNoSetter, [AKey]));
      if TGocciaClassValue(AObject).HasOwnPrivateStaticMethod(PrivateName) then
        ThrowBytecodePrivateTypeError(AKey,
          Format('Private method %s is not writable', [AKey]));
      if TGocciaClassValue(AObject).HasOwnPrivateStaticProperty(PrivateName) then
      begin
        TGocciaClassValue(AObject).AddPrivateStaticProperty(
          PrivateName, AValue);
        Exit;
      end;
      ThrowBytecodePrivateTypeError(AKey,
        Format(SErrorPrivateFieldNotAccessible, [AKey]));
    end;

    if AObject is TGocciaObjectValue then
    begin
      Current := TGocciaObjectValue(AObject);
      while Assigned(Current) do
      begin
        if not TryGetRawObjectPrivateDescriptor(Current, AKey, Descriptor) then
          Descriptor := Current.GetOwnPropertyDescriptor(AKey);
        if (Descriptor is TGocciaPropertyDescriptorAccessor) and
           Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) then
        begin
          if not TryGetRawPrivateValue(
            AObject, BytecodePrivateBrandKey(AKey, PrivateBrandToken),
            ExistingValue) then
            ThrowBytecodePrivateTypeError(AKey,
              Format(SErrorPrivateFieldNotAccessible, [AKey]));
          SetterArgs := TGocciaArgumentsCollection.Create([AValue]);
          try
            InvokeFunctionValue(
              TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
              SetterArgs, AObject);
          finally
            SetterArgs.Free;
          end;
          Exit;
        end;
        if Descriptor is TGocciaPropertyDescriptorData then
          Break;
        if Descriptor is TGocciaPropertyDescriptorAccessor then
          ThrowBytecodePrivateTypeError(AKey,
            Format(SErrorPrivateAccessorNoSetter, [AKey]));
        Current := Current.Prototype;
      end;
    end;
    if TryGetRawPrivateValue(AObject, AKey, ExistingValue) then
    begin
      if (AObject = FPrivateInitializerReceiver) and
         FPrivateInitializerPreserveExisting then
        Exit;
    end
    else
      ThrowBytecodePrivateTypeError(AKey,
        Format(SErrorPrivateFieldNotAccessible, [AKey]));
    SetRawPrivateValue(AObject, AKey, AValue);
    Exit;
  end;

  if AObject is TGocciaObjectValue then
  begin
    AObject.SetProperty(AKey, AValue);
    Exit;
  end;

  if (AObject is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    if not BoxedValue.AssignPropertyWithReceiver(AKey, AValue, AObject) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;

  BoxedValue := AObject.Box;
  if Assigned(BoxedValue) then
  begin
    if not BoxedValue.AssignPropertyWithReceiver(AKey, AValue, AObject) then
      ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
        SSuggestCheckNullBeforeAccess);
    Exit;
  end;
  ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
    SSuggestCheckNullBeforeAccess);
end;

procedure TGocciaVM.SetPropertyValueLoose(const AObject: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue);
var
  BoxedValue: TGocciaObjectValue;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull, [AKey]),
      SSuggestCheckNullBeforeAccess);
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined, [AKey]),
      SSuggestCheckNullBeforeAccess);

  if IsBytecodePrivateKey(AKey) then
  begin
    SetPropertyValue(AObject, AKey, AValue);
    Exit;
  end;

  if AObject is TGocciaObjectValue then
  begin
    TGocciaObjectValue(AObject).AssignPropertyWithReceiver(AKey, AValue,
      AObject);
    Exit;
  end;

  if (AObject is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    BoxedValue.AssignPropertyWithReceiver(AKey, AValue, AObject);
    Exit;
  end;

  BoxedValue := AObject.Box;
  if Assigned(BoxedValue) then
    BoxedValue.AssignPropertyWithReceiver(AKey, AValue, AObject);
end;

procedure TGocciaVM.SetSymbolPropertyValueLoose(const AObject: TGocciaValue;
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue);
var
  BoxedValue: TGocciaObjectValue;
begin
  if AObject is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfNull,
      [ASymbol.ToDisplayString.Value]), SSuggestCheckNullBeforeAccess);
  if AObject is TGocciaUndefinedLiteralValue then
    ThrowTypeError(Format(SErrorCannotSetPropertiesOfUndefined,
      [ASymbol.ToDisplayString.Value]), SSuggestCheckNullBeforeAccess);

  if AObject is TGocciaObjectValue then
  begin
    TGocciaObjectValue(AObject).AssignSymbolPropertyWithReceiver(ASymbol,
      AValue, AObject);
    Exit;
  end;

  if (AObject is TGocciaSymbolValue) and
     (TGocciaSymbolValue.SharedPrototype is TGocciaObjectValue) then
  begin
    BoxedValue := TGocciaObjectValue(TGocciaSymbolValue.SharedPrototype);
    BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue, AObject);
    Exit;
  end;

  BoxedValue := AObject.Box;
  if Assigned(BoxedValue) then
    BoxedValue.AssignSymbolPropertyWithReceiver(ASymbol, AValue, AObject);
end;

procedure TGocciaVM.SetIndexValueLoose(const AObject: TGocciaValue;
  const AKey: TGocciaRegister; const AValue: TGocciaValue);
var
  Key: TGocciaPropertyKey;
begin
  Key := ClassifyPropertyKey(AKey, False);
  if Key.Kind = pkkSymbol then
    SetSymbolPropertyValueLoose(AObject, Key.Symbol, AValue)
  else
    SetPropertyValueLoose(AObject, PropertyKeyName(Key), AValue);
end;

function TGocciaVM.TryGetRawPrivateValue(const AObject: TGocciaValue;
  const AKey: string; out AValue: TGocciaValue): Boolean;
var
  BrandDescriptor: TGocciaPropertyDescriptor;
  PrivateBrandToken: string;
  BrandValue: TGocciaValue;
  Descriptor: TGocciaPropertyDescriptor;
  InstanceValue: TGocciaInstanceValue;
begin
  Result := False;
  AValue := nil;
  if not (IsBytecodePrivateKey(AKey) or IsBytecodePrivateBrandKey(AKey)) then
    Exit;

  if AObject is TGocciaInstanceValue then
  begin
    InstanceValue := TGocciaInstanceValue(AObject);
    if InstanceValue.TryGetRawPrivateProperty(AKey, AValue) then
    begin
      PrivateBrandToken := ResolveBytecodePrivateBrandToken(AKey, AObject);
      Result := IsBytecodePrivateBrandKey(AKey) or
        InstanceValue.TryGetRawPrivateProperty(
          BytecodePrivateBrandKey(AKey, PrivateBrandToken), BrandValue);
    end;
    Exit;
  end;

  if AObject is TGocciaClassValue then
  begin
    PrivateBrandToken := ResolveBytecodePrivateBrandToken(AKey, AObject);
    if TGocciaClassValue(AObject).PrivateStaticProperties.TryGetValue(
      BytecodePrivateRuntimeKey(AKey, PrivateBrandToken), AValue) then
      Result := True;
    Exit;
  end;

  if AObject is TGocciaObjectValue then
  begin
    if not IsBytecodePrivateBrandKey(AKey) then
    begin
      PrivateBrandToken := ResolveBytecodePrivateBrandToken(AKey, AObject);
      TryGetRawObjectPrivateDescriptor(TGocciaObjectValue(AObject),
        BytecodePrivateBrandKey(AKey, PrivateBrandToken), BrandDescriptor);
      if not (BrandDescriptor is TGocciaPropertyDescriptorData) then
        Exit;
    end;

    TryGetRawObjectPrivateDescriptor(TGocciaObjectValue(AObject), AKey,
      Descriptor);
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      AValue := TGocciaPropertyDescriptorData(Descriptor).Value;
      Result := True;
    end;
  end;
end;

procedure TGocciaVM.SetRawPrivateValue(const AObject: TGocciaValue;
  const AKey: string; const AValue: TGocciaValue);
begin
  if AObject is TGocciaInstanceValue then
  begin
    TGocciaInstanceValue(AObject).SetRawPrivateProperty(AKey, AValue);
    Exit;
  end;

  if AObject is TGocciaClassValue then
  begin
    TGocciaClassValue(AObject).AddPrivateStaticProperty(
      BytecodePrivateRuntimeKey(AKey,
        ResolveBytecodePrivateBrandToken(AKey, AObject)), AValue);
    Exit;
  end;

  if AObject is TGocciaObjectValue then
  begin
    DefineRawObjectPrivateProperty(TGocciaObjectValue(AObject), AKey,
      AValue, [pfWritable, pfConfigurable]);
    Exit;
  end;

  ThrowBytecodePrivateTypeError(AKey,
    Format(SErrorPrivateFieldNotAccessible, [AKey]));
end;

function TGocciaVM.HasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
var
  KeyStr: string;
  Prop: TGocciaValue;
  ResolvedKey: TGocciaValue;
  PrivateBrandToken: string;
begin
  if (AObject is TGocciaNullLiteralValue) or
     (AObject is TGocciaUndefinedLiteralValue) or
     (AObject is TGocciaBooleanLiteralValue) or
     (AObject is TGocciaNumberLiteralValue) or
     (AObject is TGocciaStringLiteralValue) then
  begin
    if AKey is TGocciaSymbolValue then
      ThrowTypeError(Format(SErrorCannotUseInOperator, [TGocciaSymbolValue(AKey).ToDisplayString.Value, AObject.ToStringLiteral.Value]),
        SSuggestCheckNullBeforeAccess)
    else
      ThrowTypeError(Format(SErrorCannotUseInOperator, [AKey.ToStringLiteral.Value, AObject.ToStringLiteral.Value]),
        SSuggestCheckNullBeforeAccess);
  end;

  if AObject is TGocciaObjectValue then
  begin
    if AKey is TGocciaStringLiteralValue then
    begin
      KeyStr := TGocciaStringLiteralValue(AKey).Value;
      if IsBytecodePrivateKey(KeyStr) or IsBytecodePrivateBrandKey(KeyStr) then
      begin
        if TryGetRawPrivateValue(AObject, KeyStr, Prop) then
          Exit(TGocciaBooleanLiteralValue.TrueValue);
        if IsBytecodePrivateKey(KeyStr) then
        begin
          PrivateBrandToken := ResolveBytecodePrivateBrandToken(KeyStr, AObject);
          if TryGetRawPrivateValue(AObject,
             BytecodePrivateBrandKey(KeyStr, PrivateBrandToken), Prop) then
            Exit(TGocciaBooleanLiteralValue.TrueValue);
        end;
        Exit(TGocciaBooleanLiteralValue.FalseValue);
      end;
    end;

    if AKey is TGocciaObjectValue then
      ResolvedKey := ToPropertyKey(AKey)
    else
      ResolvedKey := AKey;
    if ResolvedKey is TGocciaSymbolValue then
    begin
      if AObject is TGocciaProxyValue then
      begin
        if TGocciaProxyValue(AObject).HasSymbolTrap(TGocciaSymbolValue(ResolvedKey)) then
          Exit(TGocciaBooleanLiteralValue.TrueValue);
        Exit(TGocciaBooleanLiteralValue.FalseValue);
      end;
      if TGocciaObjectValue(AObject).HasSymbolProperty(TGocciaSymbolValue(ResolvedKey)) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;
  end;

  if (AKey is TGocciaObjectValue) and Assigned(ResolvedKey) then
    KeyStr := TGocciaStringLiteralValue(ResolvedKey).Value
  else
    KeyStr := KeyToPropertyName(AKey);

  // ES2026 §28.1.1 [[HasProperty]](P) — string key
  if AObject is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(AObject).HasTrap(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if AObject is TGocciaObjectValue then
  begin
    if TGocciaObjectValue(AObject).HasProperty(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Prop := AObject.GetProperty(KeyStr);
  if Assigned(Prop) and not (Prop is TGocciaUndefinedLiteralValue) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaVM.HasWithBindingValue(const AObject, AKey: TGocciaValue): TGocciaValue;
var
  BindingObject: TGocciaObjectValue;
  KeyStr: string;
  Unscopables: TGocciaValue;
  Blocked: TGocciaValue;
begin
  BindingObject := ToObject(AObject);
  KeyStr := KeyToPropertyName(AKey);

  if not BindingObject.HasProperty(KeyStr) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  Unscopables := BindingObject.GetSymbolProperty(
    TGocciaSymbolValue.WellKnownUnscopables);
  if Unscopables is TGocciaObjectValue then
  begin
    Blocked := TGocciaObjectValue(Unscopables).GetProperty(KeyStr);
    if Assigned(Blocked) and Blocked.ToBooleanLiteral.Value then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaVM.GetWithBindingValue(const AObject, AKey: TGocciaValue;
  const AStrict: Boolean): TGocciaValue;
var
  BindingObject: TGocciaObjectValue;
  KeyStr: string;
begin
  BindingObject := ToObject(AObject);
  KeyStr := KeyToPropertyName(AKey);

  if not BindingObject.HasProperty(KeyStr) then
  begin
    if AStrict then
      ThrowReferenceError(KeyStr + ' is not defined');
    Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
  end;

  Result := BindingObject.GetProperty(KeyStr);
  if not Assigned(Result) then
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaVM.SetWithBindingValue(const AObject, AKey,
  AValue: TGocciaValue; const AStrict: Boolean);
var
  BindingObject: TGocciaObjectValue;
  KeyStr: string;
  StillExists: Boolean;
begin
  BindingObject := ToObject(AObject);
  KeyStr := KeyToPropertyName(AKey);
  StillExists := BindingObject.HasProperty(KeyStr);

  if AStrict and not StillExists then
    ThrowReferenceError(KeyStr + ' is not defined');

  if AStrict then
    SetPropertyValue(BindingObject, KeyStr, AValue)
  else
    SetPropertyValueLoose(BindingObject, KeyStr, AValue);
end;

function TGocciaVM.MatchHasPropertyValue(const AObject, AKey: TGocciaValue): TGocciaValue;
var
  KeyStr: string;
  Boxed: TGocciaObjectValue;
  Prop: TGocciaValue;
begin
  if (AObject is TGocciaNullLiteralValue) or
     (AObject is TGocciaUndefinedLiteralValue) then
    Exit(TGocciaBooleanLiteralValue.FalseValue);

  if AKey is TGocciaSymbolValue then
  begin
    if AObject is TGocciaObjectValue then
    begin
      if AObject is TGocciaProxyValue then
      begin
        if TGocciaProxyValue(AObject).HasSymbolTrap(TGocciaSymbolValue(AKey)) then
          Exit(TGocciaBooleanLiteralValue.TrueValue);
        Exit(TGocciaBooleanLiteralValue.FalseValue);
      end;
      if VMHasSymbolPropertyInChain(TGocciaObjectValue(AObject),
        TGocciaSymbolValue(AKey)) then
        Exit(TGocciaBooleanLiteralValue.TrueValue);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;

    Boxed := AObject.Box;
    if Assigned(Boxed) and VMHasSymbolPropertyInChain(Boxed,
      TGocciaSymbolValue(AKey)) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  KeyStr := KeyToPropertyName(AKey);
  if AObject is TGocciaProxyValue then
  begin
    if TGocciaProxyValue(AObject).HasTrap(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  if AObject is TGocciaObjectValue then
  begin
    if TGocciaObjectValue(AObject).HasProperty(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Boxed := AObject.Box;
  if Assigned(Boxed) then
  begin
    if Boxed.HasProperty(KeyStr) then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;

  Prop := AObject.GetProperty(KeyStr);
  if Assigned(Prop) then
    Exit(TGocciaBooleanLiteralValue.TrueValue);
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaVM.MatchExtractorValue(const ASubject,
  AMatcher: TGocciaValue): TGocciaValue;
var
  CustomMatcher, Extracted: TGocciaValue;
  MatchHintObject: TGocciaObjectValue;
  ExtractedArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  ObjectConstructorValue, FunctionConstructorValue: TGocciaValue;
begin
  CustomMatcher := GetCustomMatcher(AMatcher);
  if not Assigned(CustomMatcher) then
  begin
    if AMatcher is TGocciaClassValue then
    begin
      ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
      FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
      if VMInstanceOfValue(ASubject, AMatcher, ObjectConstructorValue,
         FunctionConstructorValue).ToBooleanLiteral.Value then
        Exit(TGocciaArrayValue.Create);
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    end;

    ThrowTypeError('Extractor pattern requires a custom matcher');
  end;

  if not CustomMatcher.IsCallable then
    ThrowTypeError('Symbol.customMatcher must be callable');

  CallArgs := AcquireArguments(2);
  try
    MatchHintObject := TGocciaObjectValue.Create;
    MatchHintObject.AssignProperty(PROP_MATCH_TYPE,
      TGocciaStringLiteralValue.Create('extractor'));
    CallArgs.Add(ASubject);
    CallArgs.Add(MatchHintObject);
    Extracted := InvokeFunctionValue(CustomMatcher, CallArgs, AMatcher);
  finally
    ReleaseArguments(CallArgs);
  end;

  if Extracted is TGocciaBooleanLiteralValue then
  begin
    if not TGocciaBooleanLiteralValue(Extracted).Value then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    Exit(TGocciaArrayValue.Create);
  end;

  if not TryIterableToArray(Extracted, ExtractedArray) then
    ThrowTypeError('Extractor pattern result must be true, false, or iterable');
  Result := ExtractedArray;
end;

function TGocciaVM.InvokeFunctionValue(const ACallee: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  CalleeDesc: string;
begin
  // ES2026 §28.1.1 [[Call]](thisArgument, argumentsList)
  if ACallee is TGocciaProxyValue then
    Exit(TGocciaProxyValue(ACallee).ApplyTrap(AArguments, AThisValue));
  if ACallee is TGocciaBytecodeFunctionValue then
    Exit(TGocciaBytecodeFunctionValue(ACallee).Call(AArguments, AThisValue));
  if Assigned(ACallee) and ACallee.IsCallable then
    Exit(DispatchCall(ACallee, AArguments, AThisValue));
  if Assigned(ACallee) then
    CalleeDesc := ACallee.TypeName
  else
    CalleeDesc := 'undefined';
  ThrowTypeError(Format(SErrorValueNotFunction, [CalleeDesc]),
    SSuggestNotFunctionType);
end;

function HasDirectEvalTopLevelUsingDeclaration(
  const AProgram: TGocciaProgram): Boolean;
var
  I: Integer;
begin
  for I := 0 to AProgram.Body.Count - 1 do
    if AProgram.Body[I] is TGocciaUsingDeclaration then
      Exit(True);
  Result := False;
end;

function DirectEvalRejectsArgumentsVarDeclaration(
  const ATemplate: TGocciaFunctionTemplate; const AEnvironmentIndex: Integer;
  const APC: UInt32): Boolean;
var
  Env: TGocciaDirectEvalEnvironment;
  I: Integer;
begin
  if (not Assigned(ATemplate)) or (AEnvironmentIndex < 0) or
     (APC >= ATemplate.ParameterPreambleSize) then
    Exit(False);
  if ATemplate.IsArrow then
    Exit(False);
  Env := ATemplate.GetDirectEvalEnvironment(AEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
    if Env.Bindings[I].IsEvalSyntheticArguments then
      Exit(True);
  Result := False;
end;

function IsCurrentRealmEvalFunction(const ACallee: TGocciaValue;
  const ARealm: TGocciaRealm): Boolean;
var
  GlobalObject: TGocciaObjectValue;
  EvalValue: TGocciaValue;
begin
  Result := False;
  if not Assigned(ARealm) or not (ARealm.GlobalObject is TGocciaObjectValue) then
    Exit;

  GlobalObject := TGocciaObjectValue(ARealm.GlobalObject);
  if not GlobalObject.HasProperty('eval') then
    Exit;

  EvalValue := GlobalObject.GetProperty('eval');
  Result := EvalValue = ACallee;
end;

function DirectEvalParameterPreambleVarRejectNames(
  const ATemplate: TGocciaFunctionTemplate; const AEnvironmentIndex: Integer;
  const APC: UInt32): TGocciaEvalRejectNameArray;
var
  Env: TGocciaDirectEvalEnvironment;
  Binding: TGocciaDirectEvalBindingInfo;
  Names: TGocciaEvalRejectNameArray;
  I, Len: Integer;
  function IsVisibleParameterBindingName(const AName: string): Boolean;
  begin
    Result := (AName <> '') and
      (AName <> KEYWORD_THIS) and
      (AName <> '__receiver') and
      (AName <> '__super__') and
      (Copy(AName, 1, 1) <> '#') and
      (Copy(AName, 1, 7) <> '__param') and
      (Copy(AName, 1, 8) <> '`#param`') and
      (Copy(AName, 1, 19) <> '__accessor_computed');
  end;
  function NameSeen(const AName: string): Boolean;
  var
    J: Integer;
  begin
    for J := 0 to High(Names) do
      if Names[J] = AName then
        Exit(True);
    Result := False;
  end;
  procedure AddName(const AName: string);
  begin
    if (not IsVisibleParameterBindingName(AName)) or NameSeen(AName) then
      Exit;
    Len := Length(Names);
    SetLength(Names, Len + 1);
    Names[Len] := AName;
  end;
begin
  Names := nil;
  if (not Assigned(ATemplate)) or (AEnvironmentIndex < 0) or
     (APC >= ATemplate.ParameterPreambleSize) then
    Exit(nil);

  Env := ATemplate.GetDirectEvalEnvironment(AEnvironmentIndex);
  for I := 0 to High(Env.Bindings) do
  begin
    Binding := Env.Bindings[I];
    if (Binding.Kind = debLocal) and not Binding.IsVarEnvironmentBinding then
      AddName(Binding.Name);
  end;
  Result := Names;
end;

function TGocciaVM.ExecuteDirectEval(const ASourceValue: TGocciaValue;
  const ATemplate: TGocciaFunctionTemplate; const APC: UInt32;
  const ACallerStrict: Boolean): TGocciaValue;
var
  SourceText: string;
  EvalSource: TStringList;
  EvalOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  EnvIndex: Integer;
  DirectEvalEnv: TGocciaDirectEvalEnvironment;
  StrictEval: Boolean;
  CallerScope, EvalScope: TGocciaVMDirectEvalScope;
  CallerParentScope: TGocciaScope;
  ActiveScope: TGocciaScope;
  VarScope: TGocciaScope;
  EvalContext: TGocciaEvaluationContext;
  ExecutionContext: TGocciaExecutionContextScope;
  SourceName: string;
  CallerClosure: TGocciaBytecodeClosure;
  DeclaredPrivateNames: TStringList;
  CurrentFunctionValue: TGocciaValue;
  CurrentCtorClass: TGocciaClassValue;
  LexicalNewTarget: TGocciaValue;
  RejectArgumentsVarDeclaration: Boolean;
  RejectVarDeclarationNames: TGocciaEvalRejectNameArray;
  RejectArgumentsReference: Boolean;
  AllowNewTarget: Boolean;
  AllowSuperProperty: Boolean;
  AllowSuperCall: Boolean;
  UseGlobalVarEnvironment: Boolean;
begin
  if not (ASourceValue is TGocciaStringLiteralValue) then
    Exit(ASourceValue);

  SourceText := TGocciaStringLiteralValue(ASourceValue).Value;
  if FCurrentModuleSourcePath <> '' then
    SourceName := FCurrentModuleSourcePath
  else
    SourceName := '<bytecode-direct-eval>';
  EvalSource := CreateECMAScriptSourceLines(SourceText);
  try
    EvalOptions := TGocciaSourcePipeline.CurrentOptionsOrDefault;
    EvalOptions.SourceType := stScript;
    EvalOptions.InheritedStrictMode := ACallerStrict;
    if ACallerStrict then
      Exclude(EvalOptions.Compatibility, cfNonStrictMode);
    DeclaredPrivateNames := CollectBytecodeDirectEvalPrivateNames(Self);
    try
      PipelineResult := TGocciaSourcePipeline.Parse(EvalSource, SourceName,
        EvalOptions, DeclaredPrivateNames);
    finally
      DeclaredPrivateNames.Free;
    end;
    try
      if HasDirectEvalTopLevelUsingDeclaration(PipelineResult.ProgramNode) then
        ThrowSyntaxError(
          'Using declarations are not allowed at the top level of eval');

      if not ATemplate.FindDirectEvalEnvironment(APC, EnvIndex) then
        EnvIndex := -1;
      StrictEval := ACallerStrict or HasUseStrictDirective(PipelineResult.ProgramNode);
      UseGlobalVarEnvironment := TemplateUsesGlobalEvalEnvironment(ATemplate);
      CallerClosure := DirectEvalLexicalClosure(Self);
      if Assigned(CallerClosure) and Assigned(CallerClosure.Template) and
         CallerClosure.Template.IsArrow then
        LexicalNewTarget := CallerClosure.NewTarget
      else
        LexicalNewTarget := FCurrentNewTarget;
      if UseGlobalVarEnvironment then
        CallerParentScope := FGlobalScope
      else
        CallerParentScope := EnsureCurrentDynamicVarScope;

      CallerScope := TGocciaVMDirectEvalScope.Create(CallerParentScope, Self,
        ATemplate, EnvIndex, UseGlobalVarEnvironment, CallerClosure,
        LexicalNewTarget);
      CallerScope.ThisValue := DirectEvalLexicalThisValue(Self,
        UseGlobalVarEnvironment, ATemplate, EnvIndex);

      if (TGarbageCollector.Instance <> nil) then
        TGarbageCollector.Instance.AddTempRoot(CallerScope);
      try
        if StrictEval then
          ActiveScope := CallerScope.CreateChild(skFunction, 'StrictDirectEval')
        else
          ActiveScope := CallerScope.CreateChild(skBlock, 'DirectEval');
        if StrictEval then
          VarScope := ActiveScope
        else if UseGlobalVarEnvironment then
          VarScope := FGlobalScope
        else
          VarScope := CallerScope;
        ActiveScope.ThisValue := CallerScope.ThisValue;
        ActiveScope.NonStrictMode := not StrictEval;
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.AddTempRoot(ActiveScope);
        try
          EvalContext := Default(TGocciaEvaluationContext);
          EvalContext.Realm := FRealm;
          EvalContext.Scope := ActiveScope;
          EvalContext.OnError := ThrowError;
          EvalContext.LoadModule := FLoadModule;
          EvalContext.LoadModuleSource := FLoadModuleSource;
          EvalContext.ResolveModuleURL := FResolveModuleURL;
          EvalContext.CurrentFilePath := SourceName;
          EvalContext.CoverageEnabled := FCoverageEnabled;
          EvalContext.StrictTypes := False;
          if Assigned(FGlobalScope) then
            EvalContext.StrictTypes := FGlobalScope.EffectiveStrictTypes;
          EvalContext.NonStrictMode := not StrictEval;
          if Assigned(FGlobalScope) then
            EvalContext.CompatibilityNonStrictMode :=
              FGlobalScope.EffectiveNonStrictMode;

          if Assigned(CallerClosure) then
            CurrentFunctionValue := CallerClosure.FunctionValue
          else
            CurrentFunctionValue := nil;
          if Assigned(CallerClosure) and
             (CallerClosure.HomeClass is TGocciaClassValue) then
            CurrentCtorClass := TGocciaClassValue(CallerClosure.HomeClass)
          else
            CurrentCtorClass := nil;
          AllowNewTarget := Assigned(CallerClosure) and
            CallerClosure.AllowsNewTarget;
          AllowSuperProperty := Assigned(CallerClosure) and
            Assigned(CallerClosure.HomeObject);
          AllowSuperCall := Assigned(FCurrentNewTarget) and
            Assigned(CurrentCtorClass) and
            (Assigned(CurrentCtorClass.SuperClass) or
             Assigned(CurrentCtorClass.NativeSuperConstructor));
          ExecutionContext := TGocciaExecutionContextScope.Create(
            CreateExecutionContext(FRealm, ActiveScope, SourceName,
              PipelineResult.ProgramNode, CurrentFunctionValue));
            try
              RejectArgumentsVarDeclaration :=
                DirectEvalRejectsArgumentsVarDeclaration(ATemplate, EnvIndex,
                  APC);
            RejectVarDeclarationNames :=
              DirectEvalParameterPreambleVarRejectNames(
                ATemplate, EnvIndex, APC);
            RejectArgumentsReference := ATemplate.RejectArgumentsInDirectEval;
            if EnvIndex >= 0 then
            begin
              DirectEvalEnv := ATemplate.GetDirectEvalEnvironment(EnvIndex);
              RejectArgumentsReference := RejectArgumentsReference or
                DirectEvalEnv.RejectArgumentsReference;
            end;
            try
              Result := EvaluateEvalProgram(PipelineResult.ProgramNode,
                EvalContext, VarScope, ActiveScope, StrictEval,
                RejectArgumentsVarDeclaration, RejectVarDeclarationNames,
                AllowNewTarget,
                AllowSuperProperty, AllowSuperCall, RejectArgumentsReference);
            finally
              CallerScope.CopyBackVariableBindings;
              if not UseGlobalVarEnvironment then
                CallerScope.CopyNewVariableBindingsToParent;
            end;
          finally
            ExecutionContext.Free;
          end;
        finally
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.RemoveTempRoot(ActiveScope);
        end;
      finally
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.RemoveTempRoot(CallerScope);
      end;
    finally
      PipelineResult.Free;
    end;
  finally
    EvalSource.Free;
  end;
end;

procedure TGocciaVM.ThrowError(const AMessage: string; const ALine,
  AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn,
    FCurrentModuleSourcePath, nil);
end;

function TGocciaVM.ExecuteClosureRegisters(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
  const APushExecutionContext: Boolean): TGocciaRegister;
begin
  CheckExecutionTimeout;
  CheckInstructionLimit;
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue, AArguments,
    Length(AArguments), RegisterUndefined, RegisterUndefined, RegisterUndefined,
    False, APushExecutionContext);
end;

function TGocciaVM.ExecuteClosureRegisters0(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister;
  const APushExecutionContext: Boolean): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 0, RegisterUndefined, RegisterUndefined,
    RegisterUndefined, True, APushExecutionContext);
end;

function TGocciaVM.ExecuteClosureRegisters1(const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AArg0: TGocciaRegister;
  const APushExecutionContext: Boolean): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 1, AArg0, RegisterUndefined, RegisterUndefined,
    True, APushExecutionContext);
end;

function TGocciaVM.ExecuteClosureRegisters2(const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AArg0, AArg1: TGocciaRegister;
  const APushExecutionContext: Boolean): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 2, AArg0, AArg1, RegisterUndefined, True,
    APushExecutionContext);
end;

function TGocciaVM.ExecuteClosureRegisters3(const AClosure: TGocciaBytecodeClosure;
  const AThisValue, AArg0, AArg1, AArg2: TGocciaRegister;
  const APushExecutionContext: Boolean): TGocciaRegister;
begin
  Result := ExecuteClosureRegistersInternal(AClosure, AThisValue,
    TGocciaRegisterArray(nil), 3, AArg0, AArg1, AArg2, True,
    APushExecutionContext);
end;

procedure TGocciaVM.PushFrame(const AResultRegister, AFrameIP: Integer;
  const ATemplate: TGocciaFunctionTemplate;
  const APrevCovLine: UInt32; const AProfileTimestamp: Int64);
begin
  CheckStackDepth(FFrameDepth + 1);
  if FFrameStackCount >= Length(FFrameStack) then
    SetLength(FFrameStack, FFrameStackCount * 2 + 8);
  FFrameStack[FFrameStackCount].Template := ATemplate;
  FFrameStack[FFrameStackCount].IP := AFrameIP;
  FFrameStack[FFrameStackCount].ReturnRegister := AResultRegister;
  FFrameStack[FFrameStackCount].RegisterBase := FRegisterBase;
  FFrameStack[FFrameStackCount].RegisterCount := FRegisterCount;
  FFrameStack[FFrameStackCount].LocalCellBase := FLocalCellBase;
  FFrameStack[FFrameStackCount].LocalCellCount := FLocalCellCount;
  FFrameStack[FFrameStackCount].ArgumentBase := FArgumentBase;
  FFrameStack[FFrameStackCount].ArgCount := FArgCount;
  FFrameStack[FFrameStackCount].Closure := FCurrentClosure;
  FFrameStack[FFrameStackCount].HandlerCount := FHandlerStack.Count;
  FFrameStack[FFrameStackCount].PrevCovLine := APrevCovLine;
  FFrameStack[FFrameStackCount].ProfileEntryTimestamp := AProfileTimestamp;
  FFrameStack[FFrameStackCount].NewTarget := Pointer(FCurrentNewTarget);
  FFrameStack[FFrameStackCount].GlobalScope := Pointer(FGlobalScope);
  FFrameStack[FFrameStackCount].DynamicVarScope :=
    Pointer(FCurrentDynamicVarScope);
  FFrameStack[FFrameStackCount].ExecutionContextPushed :=
    FCurrentExecutionContextPushed;
  Inc(FFrameStackCount);
end;

function TGocciaVM.PopFrame(var AFrame: TGocciaVMCallFrame;
  out ATemplate: TGocciaFunctionTemplate;
  out APrevCovLine: UInt32; out AProfileTimestamp: Int64): Integer;
begin
  Dec(FFrameStackCount);
  ATemplate := FFrameStack[FFrameStackCount].Template;
  AFrame.IP := FFrameStack[FFrameStackCount].IP;
  AFrame.Template := ATemplate;
  FRegisterBase := FFrameStack[FFrameStackCount].RegisterBase;
  FRegisterCount := FFrameStack[FFrameStackCount].RegisterCount;
  FRegisters := @FRegisterStack[FRegisterBase];
  FLocalCellBase := FFrameStack[FFrameStackCount].LocalCellBase;
  FLocalCellCount := FFrameStack[FFrameStackCount].LocalCellCount;
  FLocalCells := @FLocalCellStack[FLocalCellBase];
  FArgumentBase := FFrameStack[FFrameStackCount].ArgumentBase;
  FArgCount := FFrameStack[FFrameStackCount].ArgCount;
  FArguments := @FArgumentStack[FArgumentBase];
  FCurrentClosure := FFrameStack[FFrameStackCount].Closure;
  APrevCovLine := FFrameStack[FFrameStackCount].PrevCovLine;
  AProfileTimestamp := FFrameStack[FFrameStackCount].ProfileEntryTimestamp;
  FCurrentNewTarget := TGocciaValue(FFrameStack[FFrameStackCount].NewTarget);
  FGlobalScope := TGocciaScope(FFrameStack[FFrameStackCount].GlobalScope);
  FCurrentDynamicVarScope :=
    TGocciaScope(FFrameStack[FFrameStackCount].DynamicVarScope);
  FCurrentExecutionContextPushed :=
    FFrameStack[FFrameStackCount].ExecutionContextPushed;
  Result := FFrameStack[FFrameStackCount].ReturnRegister;
end;

procedure TGocciaVM.TeardownCurrentFrame(const ATemplate: TGocciaFunctionTemplate;
  const AProfileTimestamp: Int64; const ATargetHandlerCount: Integer);
begin
  if FProfilingFunctions and Assigned(ATemplate) and
     (ATemplate.ProfileIndex >= 0) then
    TGocciaProfiler.Instance.PopFunction(ATemplate.ProfileIndex, GetNanoseconds);
  if (TGocciaCallStack.Instance <> nil) then
    TGocciaCallStack.Instance.Pop;
  if FCurrentExecutionContextPushed then
  begin
    TGocciaExecutionContextStack.Pop;
    FCurrentExecutionContextPushed := False;
  end;
  while FHandlerStack.Count > ATargetHandlerCount do
    FHandlerStack.Pop;
  Dec(FFrameDepth);
end;

procedure TGocciaVM.PushClosedNumericFrame(const AResultRegister,
  AArgumentBase, AArgumentCount: UInt16; var AFrame: TGocciaVMCallFrame;
  const ATemplate: TGocciaFunctionTemplate; var APrevCovLine: UInt32;
  var AProfileTimestamp: Int64; var AInitializedRegisterTop: Integer);
var
  Arguments: array[0..2] of TGocciaRegister;
  I: Integer;
  NewBase, Required, ClearStart: Integer;
begin
  if (AArgumentCount < 1) or (AArgumentCount > 3) or
     (AArgumentCount <> ATemplate.ParameterCount) then
    raise Exception.Create('Invalid OP_CALL_SELF_NUM argument contract');

  // Capture before growing the arena: SetLength may relocate FRegisterStack.
  for I := 0 to AArgumentCount - 1 do
  begin
    Arguments[I] := FRegisters[AArgumentBase + I];
    if not RegisterIsNumericScalar(Arguments[I]) then
      raise Exception.Create('Invalid non-numeric OP_CALL_SELF_NUM argument');
  end;

  CheckStackDepth(FFrameDepth + 1);
  if FClosedNumericFrameStackCount >= Length(FClosedNumericFrameStack) then
    SetLength(FClosedNumericFrameStack,
      FClosedNumericFrameStackCount * 2 + 8);
  FClosedNumericFrameStack[FClosedNumericFrameStackCount].IP := AFrame.IP;
  FClosedNumericFrameStack[FClosedNumericFrameStackCount].ReturnRegister :=
    AResultRegister;
  FClosedNumericFrameStack[FClosedNumericFrameStackCount].RegisterBase :=
    FRegisterBase;
  FClosedNumericFrameStack[FClosedNumericFrameStackCount].PrevCovLine :=
    APrevCovLine;
  FClosedNumericFrameStack[FClosedNumericFrameStackCount].
    ProfileEntryTimestamp := AProfileTimestamp;
  Inc(FClosedNumericFrameStackCount);

  NewBase := FRegisterBase + FRegisterCount;
  Required := NewBase + FRegisterCount;
  if Required > Length(FRegisterStack) then
    SetLength(FRegisterStack, Required * 2);
  if Required > AInitializedRegisterTop then
  begin
    ClearStart := Max(NewBase, AInitializedRegisterTop);
    if Required > ClearStart then
      FillChar(FRegisterStack[ClearStart],
        (Required - ClearStart) * SizeOf(TGocciaRegister), 0);
    AInitializedRegisterTop := Required;
  end;
  FRegisterBase := NewBase;
  FRegisters := @FRegisterStack[FRegisterBase];
  // Each depth is cleared on first use in this outer invocation. The proof
  // then admits only scalar-number expressions, numeric predicates, and direct
  // self-calls, so sibling reuse can contain only non-reference scalars or
  // undefined and no stale object pointer can reach the GC.
  FRegisters[0] := RegisterUndefined;
  for I := 0 to AArgumentCount - 1 do
    FRegisters[I + 1] := Arguments[I];
  AFrame.IP := 0;

  Inc(FFrameDepth);
  if (TGocciaCallStack.Instance <> nil) then
    if Assigned(ATemplate.DebugInfo) and
       (ATemplate.DebugInfo.SourceFile <> '') then
      TGocciaCallStack.Instance.PushTemplate(Pointer(ATemplate), '')
    else
      TGocciaCallStack.Instance.PushTemplate(Pointer(ATemplate),
        FCurrentModuleSourcePath);

  if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and
     Assigned(ATemplate.DebugInfo) and
     (ATemplate.DebugInfo.LineMapCount > 0) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      ATemplate.DebugInfo.SourceFile,
      ATemplate.DebugInfo.GetLineMapEntry(0).Line);

  if FProfilingFunctions and (TGocciaProfiler.Instance <> nil) then
  begin
    if ATemplate.ProfileIndex < 0 then
    begin
      if Assigned(ATemplate.DebugInfo) and
         (ATemplate.DebugInfo.LineMapCount > 0) then
        ATemplate.ProfileIndex := TGocciaProfiler.Instance.RegisterTemplate(
          ATemplate.Name, ATemplate.DebugInfo.SourceFile,
          ATemplate.DebugInfo.GetLineMapEntry(0).Line)
      else
        ATemplate.ProfileIndex := TGocciaProfiler.Instance.RegisterTemplate(
          ATemplate.Name, '', 0);
    end;
    AProfileTimestamp := GetNanoseconds;
    TGocciaProfiler.Instance.PushFunction(ATemplate.ProfileIndex,
      AProfileTimestamp);
  end;

  if FCoverageEnabled and Assigned(ATemplate.DebugInfo) and
     (ATemplate.DebugInfo.LineMapCount > 0) then
    APrevCovLine := ATemplate.DebugInfo.GetLineMapEntry(0).Line
  else
    APrevCovLine := 0;
end;

function TGocciaVM.PopClosedNumericFrame(var AFrame: TGocciaVMCallFrame;
  const ATemplate: TGocciaFunctionTemplate; var APrevCovLine: UInt32;
  var AProfileTimestamp: Int64): Integer;
begin
  if FProfilingFunctions and Assigned(ATemplate) and
     (ATemplate.ProfileIndex >= 0) then
    TGocciaProfiler.Instance.PopFunction(ATemplate.ProfileIndex,
      GetNanoseconds);
  if (TGocciaCallStack.Instance <> nil) then
    TGocciaCallStack.Instance.Pop;
  Dec(FFrameDepth);

  Dec(FClosedNumericFrameStackCount);
  AFrame.IP := FClosedNumericFrameStack[
    FClosedNumericFrameStackCount].IP;
  FRegisterBase := FClosedNumericFrameStack[
    FClosedNumericFrameStackCount].RegisterBase;
  FRegisters := @FRegisterStack[FRegisterBase];
  APrevCovLine := FClosedNumericFrameStack[
    FClosedNumericFrameStackCount].PrevCovLine;
  AProfileTimestamp := FClosedNumericFrameStack[
    FClosedNumericFrameStackCount].ProfileEntryTimestamp;
  Result := FClosedNumericFrameStack[
    FClosedNumericFrameStackCount].ReturnRegister;
end;

procedure TGocciaVM.UnwindClosedNumericFrames(const ATargetCount: Integer;
  var AFrame: TGocciaVMCallFrame;
  const ATemplate: TGocciaFunctionTemplate; var APrevCovLine: UInt32;
  var AProfileTimestamp: Int64);
begin
  while FClosedNumericFrameStackCount > ATargetCount do
    PopClosedNumericFrame(AFrame, ATemplate, APrevCovLine,
      AProfileTimestamp);
end;

// ES2026 §15.10.3 PrepareForTailCall.  Discards the current frame's resources so
// the impending SetupNewFrame reuses this frame instead of stacking a new one.
// The caller's saved frame slot (or the outermost return path) is left intact,
// so the tail-called function returns directly to the current frame's caller.
// Register and local-cell windows are collapsed in place: setting the live count
// to 0 makes the next AcquireRegisters reuse the same memory region, so a chain
// of tail calls grows neither the frame stack nor the register stack.
procedure TGocciaVM.PrepareTailCallFrameReuse(
  const ATemplate: TGocciaFunctionTemplate; const AProfileTimestamp: Int64;
  const AInitialFrameStackCount, ASavedHandlerCount: Integer);
var
  TargetHandlerCount: Integer;
begin
  // The handler count active when the current frame began: the parent trampoline
  // frame's saved count, or the entry handler count if this is the outermost
  // frame of the running ExecuteClosureRegistersInternal invocation.
  if FFrameStackCount > AInitialFrameStackCount then
    TargetHandlerCount := FFrameStack[FFrameStackCount - 1].HandlerCount
  else
    TargetHandlerCount := ASavedHandlerCount;
  TeardownCurrentFrame(ATemplate, AProfileTimestamp, TargetHandlerCount);
  // Collapse the register, local-cell, AND argument windows in place: setting
  // each live count to 0 makes the next Acquire* reuse the same base. The
  // argument window must be reset alongside the others or a tail-call chain
  // advances FArgumentBase unboundedly (the next AcquireArgumentWindow uses
  // NewBase := FArgumentBase + FArgCount), reintroducing the very unbounded
  // growth proper tail calls exist to avoid.
  FRegisterCount := 0;
  FLocalCellCount := 0;
  FArgCount := 0;
end;

procedure TGocciaVM.PushSavedStateRoot(const AClosure: TGocciaBytecodeClosure;
  const ANewTarget: TGocciaValue; const AArgumentBase, AArgCount: Integer);
begin
  if FTempSavedStateRootCount >= Length(FTempSavedStateRoots) then
    SetLength(FTempSavedStateRoots, FTempSavedStateRootCount * 2 + 4);
  FTempSavedStateRoots[FTempSavedStateRootCount].Closure := AClosure;
  FTempSavedStateRoots[FTempSavedStateRootCount].NewTarget := ANewTarget;
  FTempSavedStateRoots[FTempSavedStateRootCount].ArgumentBase := AArgumentBase;
  FTempSavedStateRoots[FTempSavedStateRootCount].ArgCount := AArgCount;
  Inc(FTempSavedStateRootCount);
end;

procedure TGocciaVM.PopSavedStateRoot;
begin
  if FTempSavedStateRootCount <= 0 then
    Exit;
  Dec(FTempSavedStateRootCount);
  FTempSavedStateRoots[FTempSavedStateRootCount].Closure := nil;
  FTempSavedStateRoots[FTempSavedStateRootCount].NewTarget := nil;
  FTempSavedStateRoots[FTempSavedStateRootCount].ArgumentBase := 0;
  FTempSavedStateRoots[FTempSavedStateRootCount].ArgCount := 0;
end;

procedure TGocciaVM.SetupNewFrame(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaRegister; const AArguments: TGocciaRegisterArray;
  const AArgCount: Integer; const AArg0, AArg1, AArg2: TGocciaRegister;
  const AUseFixedArgs: Boolean; const APushExecutionContext: Boolean;
  var AFrame: TGocciaVMCallFrame; out ATemplate: TGocciaFunctionTemplate;
  out APrevCovLine: UInt32; out AProfileTimestamp: Int64);
var
  I: Integer;
  ExecutionSourcePath: string;
  ExecutionRealm: TGocciaRealm;
begin
  AProfileTimestamp := 0;
  ATemplate := AClosure.Template;
  if Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.SourceFile <> '') then
    ExecutionSourcePath := ATemplate.DebugInfo.SourceFile
  else
    ExecutionSourcePath := FCurrentModuleSourcePath;

  AcquireRegisters(Max(ATemplate.MaxRegisters, 1));
  AcquireLocalCells(Max(ATemplate.MaxRegisters, 1));
  // Acquire the argument window on the arena (sets FArgumentBase/FArgCount and
  // FArguments) before reading the previous frame's FArgCount, then fill it.
  AcquireArgumentWindow(AArgCount);
  for I := 0 to AArgCount - 1 do
    if AUseFixedArgs then
      case I of
        0:
          FArguments[I] := AArg0;
        1:
          FArguments[I] := AArg1;
        2:
          FArguments[I] := AArg2;
      else
        FArguments[I] := RegisterUndefined;
      end
    else
      FArguments[I] := AArguments[I];
  FCurrentClosure := AClosure;
  if Assigned(AClosure) and Assigned(AClosure.GlobalScope) then
    FGlobalScope := AClosure.GlobalScope;
  if Assigned(AClosure) and
     (ATemplate.DirectEvalEnvironmentCount = 0) then
    FCurrentDynamicVarScope := AClosure.DynamicVarScope
  else
    FCurrentDynamicVarScope := nil;
  FCurrentExecutionContextPushed := False;
  Inc(FFrameDepth);
  // Push a deferred frame: store the template pointer and (only when the
  // template has no own source file) the module-path fallback, so an ordinary
  // call performs no per-call stack-trace string work. The resolver registered
  // in the constructor reproduces ATemplate.Name and ExecutionSourcePath at
  // capture time, keeping Error.stack output byte-identical.
  if (TGocciaCallStack.Instance <> nil) then
    if Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.SourceFile <> '') then
      TGocciaCallStack.Instance.PushTemplate(Pointer(ATemplate), '')
    else
      TGocciaCallStack.Instance.PushTemplate(Pointer(ATemplate), ExecutionSourcePath);

  AFrame := Default(TGocciaVMCallFrame);
  AFrame.Template := ATemplate;

  if not ATemplate.IsArrow then
    FCurrentNewTarget := nil;
  if ATemplate.IsArrow and Assigned(AClosure) then
    FCurrentNewTarget := AClosure.NewTarget;

  if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and
     Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.LineMapCount > 0) then
    TGocciaCoverageTracker.Instance.RecordLineHit(
      ATemplate.DebugInfo.SourceFile,
      ATemplate.DebugInfo.GetLineMapEntry(0).Line);

  if FProfilingFunctions and (TGocciaProfiler.Instance <> nil) then
  begin
    if ATemplate.ProfileIndex < 0 then
    begin
      if Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.LineMapCount > 0) then
        ATemplate.ProfileIndex := TGocciaProfiler.Instance.RegisterTemplate(
          ATemplate.Name, ATemplate.DebugInfo.SourceFile,
          ATemplate.DebugInfo.GetLineMapEntry(0).Line)
      else
        ATemplate.ProfileIndex := TGocciaProfiler.Instance.RegisterTemplate(
          ATemplate.Name, '', 0);
    end;
    AProfileTimestamp := GetNanoseconds;
    TGocciaProfiler.Instance.PushFunction(
      ATemplate.ProfileIndex, AProfileTimestamp);
  end;

  SetLocalRaw(0, AThisValue);
  for I := 0 to FArgCount - 1 do
    SetLocalRaw(I + 1, FArguments[I]);
  // ES2026 §10.2.11 FunctionDeclarationInstantiation steps 19-20: sloppy
  // parameter expressions need a separate var environment for direct eval.
  if (ATemplate.DirectEvalEnvironmentCount > 0) and
     not TemplateUsesGlobalEvalEnvironment(ATemplate) then
    EnsureCurrentDynamicVarScope;

  ExecutionRealm := BytecodeClosureExecutionRealm(AClosure, FRealm);

  if APushExecutionContext and Assigned(ExecutionRealm) then
  begin
    TGocciaExecutionContextStack.Push(
      CreateExecutionContext(ExecutionRealm, FGlobalScope, ExecutionSourcePath,
        nil, AClosure.FunctionValue));
    FCurrentExecutionContextPushed := True;
  end;

  if FCoverageEnabled and Assigned(ATemplate.DebugInfo) and
     (ATemplate.DebugInfo.LineMapCount > 0) then
    APrevCovLine := ATemplate.DebugInfo.GetLineMapEntry(0).Line
  else
    APrevCovLine := 0;
end;

procedure TGocciaVM.HandleExceptionUnwind(const AErrorValue: TGocciaValue;
  const AInitialFrameStackCount, AInitialClosedNumericFrameCount,
  ASavedHandlerCount: Integer;
  var AFrame: TGocciaVMCallFrame; var ATemplate: TGocciaFunctionTemplate;
  var APrevCovLine: UInt32; var AProfileTimestamp: Int64);
var
  Handler: TGocciaBytecodeHandlerEntry;
  TargetHandlerCount: Integer;
  IsGeneratorReturnCompletion: Boolean;
begin
  // Proven numeric frames contain no handlers. Restore their generic entry
  // frame before searching the ordinary handler stack.
  UnwindClosedNumericFrames(AInitialClosedNumericFrameCount, AFrame,
    ATemplate, APrevCovLine, AProfileTimestamp);
  IsGeneratorReturnCompletion := Assigned(GActiveBytecodeGenerator) and
    Assigned(GActiveBytecodeGenerator.FReturnSentinel) and
    (AErrorValue = GActiveBytecodeGenerator.FReturnSentinel);

  while True do
  begin
    if (not FHandlerStack.IsEmpty) and
       (FHandlerStack.Peek.FrameDepth = FFrameDepth) then
    begin
      Handler := FHandlerStack.Peek;
      FHandlerStack.Pop;
      if IsGeneratorReturnCompletion and (Handler.Kind = bhkCatch) then
        Continue;
      AFrame.IP := Handler.CatchIP;
      SetRegister(Handler.CatchRegister, AErrorValue);
      Exit;
    end;
    // Outermost frame: let the finally block handle teardown
    if FFrameStackCount <= AInitialFrameStackCount then
      raise EGocciaBytecodeThrow.Create(AErrorValue);
    // Intermediate trampoline frame: tear down and pop to parent
    TeardownCurrentFrame(ATemplate, AProfileTimestamp,
      FFrameStack[FFrameStackCount - 1].HandlerCount);
    PopFrame(AFrame, ATemplate, APrevCovLine, AProfileTimestamp);
  end;
end;

procedure TGocciaVM.ExecuteGeneratorParameterPreamble(const AGenerator: TObject);
var
  Generator: TGocciaBytecodeGeneratorObjectValue;
begin
  if not (AGenerator is TGocciaBytecodeGeneratorObjectValue) then
    Exit;

  Generator := TGocciaBytecodeGeneratorObjectValue(AGenerator);
  if not Assigned(Generator.FClosure) or
     not Assigned(Generator.FClosure.Template) or
     (Generator.FClosure.Template.ParameterPreambleSize = 0) then
    Exit;

  ExecuteClosureRegistersInternal(
    Generator.FClosure,
    Generator.FThisValue,
    Generator.FArguments,
    Length(Generator.FArguments),
    RegisterUndefined,
    RegisterUndefined,
    RegisterUndefined,
    False,
    True,
    Generator.FClosure.Template.ParameterPreambleSize,
    Generator);
end;

function TGocciaVM.ExecuteClosureRegistersInternal(
  const AClosure: TGocciaBytecodeClosure; const AThisValue: TGocciaRegister;
  const AArguments: TGocciaRegisterArray; const AArgCount: Integer;
  const AArg0, AArg1, AArg2: TGocciaRegister; const AUseFixedArgs: Boolean;
  const APushExecutionContext: Boolean; const AStopAtIP: Integer;
  const AStopGenerator: TObject): TGocciaRegister;
var
  Frame: TGocciaVMCallFrame;
  SavedRegisterBase: Integer;
  SavedRegisterCount: Integer;
  SavedLocalCellBase: Integer;
  SavedLocalCellCount: Integer;
  SavedArgumentBase: Integer;
  SavedArgCount: Integer;
  SavedClosure: TGocciaBytecodeClosure;
  SavedNewTarget: TGocciaValue;
  SavedGlobalScope: TGocciaScope;
  SavedDynamicVarScope: TGocciaScope;
  ResolvedDynamicVarScope: TGocciaScope;
  ResolvedEnvironmentReference: TGocciaResolvedEnvironmentReferenceValue;
  SavedExecutionContextPushed: Boolean;
  SavedHandlerCount: Integer;
  InitialFrameStackCount: Integer;
  InitialClosedNumericFrameCount: Integer;
  ReturnValue: TGocciaRegister;
  ResultReg: Integer;
  TargetHandlerCount: Integer;
  InstructionStartIP: Integer;
  Instruction: UInt32;
  Op: UInt8;
  A, B, C: UInt16;
  WideA, WideB, WideC: UInt16;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  KeyIndex: Integer;
  ArgsArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
  GlobalName: string;
  GlobalBindingValue: TGocciaValue;
  GlobalBindingEntryIndex: Integer;
  GlobalBindingVersion: Cardinal;
  GlobalReadCache: PGocciaGlobalReadCacheEntry;
  DebugLine, DebugColumn: Integer;
  PropertyReadCache: PGocciaPropertyReadCacheEntry;
  ProtoReadCache: PGocciaProtoReadCacheEntry;
  AttributeType: string;
  SpecifierString: string;
  Upvalue: TGocciaBytecodeUpvalue;
  ChildClosure: TGocciaBytecodeClosure;
  Desc: TGocciaUpvalueDescriptor;
  Handler: TGocciaBytecodeHandlerEntry;
  DoneValue: TGocciaValue;
  IteratorElementValue: TGocciaValue;
  IterResult: TGocciaValue;
  NextMethod: TGocciaValue;
  DoneFlag: Boolean;
  Running: Boolean;
  Template: TGocciaFunctionTemplate;
  Constant: TGocciaBytecodeConstant;
  ChildTemplate: TGocciaFunctionTemplate;
  LeftValue, RightValue, TargetValue, PropKeyValue, EvalSourceValue: TGocciaValue;
  NumericValue: Double;
  PropKey: TGocciaPropertyKey;
  PrivateDescriptor: TGocciaPropertyDescriptor;
  FunctionConstructorValue, ObjectConstructorValue: TGocciaValue;
  CustomMatcherValue, MatchResultValue: TGocciaValue;
  MatchHintObject: TGocciaObjectValue;
  BuiltinConstructorMatch: Boolean;
  NumericComparisonResult: Boolean;
  RegisterArgs: TGocciaRegisterArray;
  CallThisRegister: TGocciaRegister;
  CallGlobalThisValue: TGocciaValue;
  FixedArg0, FixedArg1, FixedArg2: TGocciaRegister;
  BytecodeFunction: TGocciaBytecodeFunctionValue;
  BoundFunction: TGocciaBoundFunctionValue;
  JumpOffset: Integer;
  PrevCovLine, CovLine: UInt32;
  ProfileEntryTimestamp: Int64;
  DynImportPromise: TGocciaPromiseValue;
  DynImportTask: TGocciaMicrotask;
  AwaitPromise: TGocciaPromiseValue;
  AwaitContinuation: TGocciaBytecodeGeneratorObjectValue;
  SpreadArray: TGocciaArrayValue;
  RestoredContinuation: Boolean;
  ReturnAwaitAbrupt: Boolean;
  ForInKey: string;
  PreviousRealm: TGocciaRealm;
  ExecutionRealm: TGocciaRealm;
  RealmSwitched: Boolean;
  PreviousCallSite: TGocciaCallSite;
  ClosedNumericInitializedRegisterTop: Integer;

  procedure CurrentInstructionDebugLocation(out ALine, AColumn: Integer);
  begin
    ALine := 0;
    AColumn := 0;
    if (not Assigned(Template)) or (not Assigned(Template.DebugInfo)) then
      Exit;
    ALine := Template.DebugInfo.GetLineForPC(InstructionStartIP);
    AColumn := Template.DebugInfo.GetColumnForPC(InstructionStartIP);
  end;

  procedure EnterCurrentInstructionCallSite(
    out APrevious: TGocciaCallSite);
  var
    SourcePath: string;
  begin
    CurrentInstructionDebugLocation(DebugLine, DebugColumn);
    if Assigned(Template) and Assigned(Template.DebugInfo) then
      SourcePath := Template.DebugInfo.SourceFile
    else
      SourcePath := '';
    EnterGocciaCallSite(SourcePath, DebugLine, DebugColumn, APrevious);
  end;

begin
  // This is a native VM re-entry: the bytecode loop runs on a fresh native stack
  // frame. Bound the native re-entry depth before any state is saved so the
  // throw unwinds cleanly (the matching Inc/Dec are paired with the try/finally
  // below). Without this, generator resume / eval / native-callback recursion
  // overflows the native stack (SIGSEGV) instead of throwing RangeError.
  CheckNativeReentryDepth(FNativeExecutionDepth + 1);
  PreviousRealm := CurrentRealm;
  ExecutionRealm := BytecodeClosureExecutionRealm(AClosure, FRealm);
  RealmSwitched := Assigned(ExecutionRealm) and (ExecutionRealm <> PreviousRealm);

  SavedRegisterBase := FRegisterBase;
  SavedRegisterCount := FRegisterCount;
  SavedLocalCellBase := FLocalCellBase;
  SavedLocalCellCount := FLocalCellCount;
  SavedArgumentBase := FArgumentBase;
  SavedArgCount := FArgCount;
  SavedClosure := FCurrentClosure;
  SavedNewTarget := FCurrentNewTarget;
  SavedGlobalScope := FGlobalScope;
  SavedDynamicVarScope := FCurrentDynamicVarScope;
  SavedExecutionContextPushed := FCurrentExecutionContextPushed;
  SavedHandlerCount := FHandlerStack.Count;
  InitialFrameStackCount := FFrameStackCount;
  InitialClosedNumericFrameCount := FClosedNumericFrameStackCount;
  FLastClosureThisValue := AThisValue;
  PushSavedStateRoot(SavedClosure, SavedNewTarget, SavedArgumentBase,
    SavedArgCount);
  if RealmSwitched then
    SetCurrentRealm(ExecutionRealm);
  try
    Inc(FNativeExecutionDepth);
    SetupNewFrame(AClosure, AThisValue, AArguments, AArgCount,
      AArg0, AArg1, AArg2, AUseFixedArgs, APushExecutionContext,
      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
    ClosedNumericInitializedRegisterTop := FRegisterBase + FRegisterCount;
    if Assigned(AClosure) and Assigned(AClosure.GlobalScope) then
      FGlobalScope := AClosure.GlobalScope;
    if Assigned(FPendingNewTarget) then
      FCurrentNewTarget := FPendingNewTarget;
    FPendingNewTarget := nil;
    RestoredContinuation := Assigned(GActiveBytecodeGenerator) and
      (GActiveBytecodeGenerator.FClosure = AClosure) and
      GActiveBytecodeGenerator.RestoreContinuation(
        Frame, SavedHandlerCount, PrevCovLine);
    if RestoredContinuation then
    begin
      if GActiveBytecodeGenerator.FDelegateActive then
      begin
        if GActiveBytecodeGenerator.FResumeKind = bgrkNext then
          SetRegisterRaw(GActiveBytecodeGenerator.FResumeRegister,
            GActiveBytecodeGenerator.FResumeValue);
      end
      else
      begin
        case GActiveBytecodeGenerator.FResumeKind of
          bgrkNext:
            begin
              if GActiveBytecodeGenerator.FIgnoreNextResume then
                GActiveBytecodeGenerator.FIgnoreNextResume := False
              else
                SetRegisterRaw(GActiveBytecodeGenerator.FResumeRegister,
                  GActiveBytecodeGenerator.FResumeValue);
            end;
          bgrkThrow:
            HandleExceptionUnwind(
              RegisterToValue(GActiveBytecodeGenerator.FResumeValue),
              InitialFrameStackCount, InitialClosedNumericFrameCount,
              SavedHandlerCount,
              Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          bgrkReturn:
            begin
              GActiveBytecodeGenerator.FReturnValue :=
                RegisterToValue(GActiveBytecodeGenerator.FResumeValue);
              ReturnAwaitAbrupt := False;
              if Assigned(Template) and Template.IsAsync and
                 Template.IsGenerator and
                 not GActiveBytecodeGenerator.FReturnResumeValueAwaited then
              begin
                try
                  AwaitPromise := PromiseResolveIntrinsic(
                    GActiveBytecodeGenerator.FReturnValue);
                except
                  on E: EGocciaBytecodeThrow do
                  begin
                    HandleExceptionUnwind(E.ThrownValue,
                      InitialFrameStackCount, InitialClosedNumericFrameCount,
                      SavedHandlerCount,
                      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    ReturnAwaitAbrupt := True;
                  end;
                  on E: TGocciaThrowValue do
                  begin
                    HandleExceptionUnwind(E.Value,
                      InitialFrameStackCount, InitialClosedNumericFrameCount,
                      SavedHandlerCount,
                      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    ReturnAwaitAbrupt := True;
                  end;
                end;
                if not ReturnAwaitAbrupt then
                begin
                  GActiveBytecodeGenerator.FReturnResumeValueAwaited := True;
                  GActiveBytecodeGenerator.CaptureContinuation(Frame,
                    SavedHandlerCount, PrevCovLine,
                    GActiveBytecodeGenerator.FResumeRegister, Frame.IP);
                  GActiveBytecodeGenerator.FState := bgsSuspendedYield;
                  AwaitPromise.InvokeThen(
                    TGocciaVMAsyncAwaitContinuationValue.Create(Self,
                      GActiveBytecodeGenerator, FCurrentAsyncPromise, bgrkReturn,
                      True),
                    TGocciaVMAsyncAwaitContinuationValue.Create(Self,
                      GActiveBytecodeGenerator, FCurrentAsyncPromise, bgrkThrow,
                      True));
                  raise EGocciaBytecodeAsyncSuspend.Create('');
                end;
              end;
              if not ReturnAwaitAbrupt then
              begin
                GActiveBytecodeGenerator.FReturnResumeValueAwaited := False;
                if not Assigned(GActiveBytecodeGenerator.FReturnSentinel) then
                  GActiveBytecodeGenerator.FReturnSentinel := TGocciaObjectValue.Create;
                HandleExceptionUnwind(GActiveBytecodeGenerator.FReturnSentinel,
                  InitialFrameStackCount, InitialClosedNumericFrameCount,
                  SavedHandlerCount,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              end;
            end;
        end;
      end;
    end;
    Running := True;
    while Running and (Frame.IP < Template.CodeCount) do
    begin
      try
        while Running and (Frame.IP < Template.CodeCount) do
        begin
          if (AStopAtIP >= 0) and (Frame.IP >= AStopAtIP) and
             Assigned(AStopGenerator) then
          begin
            TGocciaBytecodeGeneratorObjectValue(AStopGenerator).
              CaptureInitialContinuation(Frame, SavedHandlerCount, PrevCovLine,
                Frame.IP);
            Result := RegisterUndefined;
            Exit;
          end;

          PollInstructionLimit;
          InstructionStartIP := Frame.IP;
          Instruction := Template.GetInstructionUnchecked(Frame.IP);
          Inc(Frame.IP);

          WideA := 0;
          WideB := 0;
          WideC := 0;
          if DecodeOp(Instruction) = Ord(OP_WIDE) then
          begin
            WideA := UInt16(DecodeA(Instruction)) shl 8;
            WideB := UInt16(DecodeB(Instruction)) shl 8;
            WideC := UInt16(DecodeC(Instruction)) shl 8;
            if Frame.IP >= Template.CodeCount then
              raise Exception.Create('Truncated OP_WIDE bytecode prefix');
            Instruction := Template.GetInstructionUnchecked(Frame.IP);
            Inc(Frame.IP);
          end;

          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and
             Assigned(Template.DebugInfo) then
          begin
            CovLine := Template.DebugInfo.GetLineForPC(InstructionStartIP);
            if (CovLine <> 0) and (CovLine <> PrevCovLine) then
            begin
              TGocciaCoverageTracker.Instance.RecordLineHit(
                Template.DebugInfo.SourceFile, CovLine);
              PrevCovLine := CovLine;
            end;
          end;

          Op := DecodeOp(Instruction);
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordOpcode(Op);
          A := WideA or DecodeA(Instruction);
          B := WideB or DecodeB(Instruction);
          C := WideC or DecodeC(Instruction);
          case TGocciaOpCode(Op) of
      OP_LOAD_CONST:
        begin
          Constant := Template.GetConstantUnchecked(DecodeBx(Instruction));
          case Constant.Kind of
            // Keep numeric constants in the VM's scalar representation.  The
            // previous ConstantToValue -> ValueToRegister round trip allocated
            // a short-lived boxed Number for every execution of the
            // instruction.
            bckInteger:
              FRegisters[A] := VMIntResult(Constant.IntValue);
            bckFloat:
              FRegisters[A] := RegisterFromDouble(Constant.FloatValue);
            // ES2026 §13.2.8.3: template objects are lazily built and cached.
            bckTemplateObject:
              FRegisters[A] := ValueToRegister(BuildTemplateObjectConstant(
                Template, DecodeBx(Instruction)));
          else
            FRegisters[A] := ValueToRegister(ConstantToValue(Constant));
          end;
        end;

      OP_LOAD_CHAR:
        if DecodeBx(Instruction) <= 127 then
          FRegisters[A] := RegisterObject(
            CachedASCIIStringValue(
              TASCIIStringCodeUnit(DecodeBx(Instruction))))
        else
          FRegisters[A] := RegisterObject(TGocciaStringLiteralValue.Create(
            UTF16CodeUnitImmediateToString(DecodeBx(Instruction))));

      OP_LOAD_REGEXP:
        FRegisters[A] := ValueToRegister(
          BuildRegExpLiteralConstant(Template, DecodeBx(Instruction)));

      OP_LOAD_UNDEFINED:
        FRegisters[A] := RegisterUndefined;

      OP_GET_THIS_BINDING:
        // ES2026 §9.4.3 ResolveThisBinding falls through to GetThisBinding
        // on the surrounding environment record.  At Script top level the
        // global env's [[GlobalThisValue]] is the global object; at Module
        // top level the module env's binding resolves to undefined.  The
        // active FGlobalScope already encodes that distinction (the
        // module loader rewires FGlobalScope to the module scope while
        // executing module bodies), so reading ThisValue here is correct
        // for both kinds without needing a compile-time flag.
        if Assigned(FGlobalScope) then
          FRegisters[A] := VMValueToRegisterFast(FGlobalScope.ThisValue)
        else
          FRegisters[A] := RegisterUndefined;

      OP_LOAD_TRUE:
        FRegisters[A] := RegisterBoolean(True);

      OP_LOAD_FALSE:
        FRegisters[A] := RegisterBoolean(False);

      OP_LOAD_NULL:
        FRegisters[A] := RegisterNull;

      OP_LOAD_HOLE:
        FRegisters[A] := RegisterHole;

      OP_CHECK_TYPE:
        VMStrictTypeCheckRegisterValue(GetRegister(A), TGocciaLocalType(B));

      OP_TO_PRIMITIVE:
      begin
        KeyIndex := DecodeBx(Instruction);
        if FRegisters[KeyIndex].Kind <> grkObject then
          FRegisters[A] := FRegisters[KeyIndex]
        else
          SetRegisterFast(A, ToPrimitive(GetRegisterFast(KeyIndex)));
      end;

      OP_TO_OBJECT:
        SetRegister(A, ToObject(GetRegister(B)));

      // ES2026 §7.1.19 ToPropertyKey(argument)
      OP_TO_PROPERTY_KEY:
        SetRegister(A, ToPropertyKey(RegisterToValue(FRegisters[B])));

      OP_ENUM_KEYS:
        SetRegister(A, ForInEntriesArray(GetRegister(B)));

      OP_ENUM_ENTRY:
      begin
        if TryForInEntryKey(GetRegister(C), ForInKey) then
        begin
          FRegisters[A] := VMValueToRegisterFast(
            TGocciaStringLiteralValue.Create(ForInKey));
          FRegisters[B] := RegisterBoolean(True);
        end
        else
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(False);
        end;
      end;

      OP_LOAD_INT:
        FRegisters[A] := RegisterInt(DecodesBx(Instruction));

      OP_MOVE:
        SetRegisterRaw(A, FRegisters[B]);

      OP_GET_LOCAL:
      begin
        FRegisters[A] := GetLocalRegister(DecodeBx(Instruction));
        if FRegisters[A].Kind = grkHole then
          ThrowReferenceError('Cannot access lexical binding before initialization');
      end;

      OP_SET_LOCAL:
        SetLocalRaw(DecodeBx(Instruction), FRegisters[A]);

      OP_GET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Desc := Template.GetUpvalueDescriptor(DecodeBx(Instruction));
          ResolvedDynamicVarScope := ResolveDynamicUpvalueScope(
            DecodeBx(Instruction), Desc.Name);
          if Assigned(ResolvedDynamicVarScope) then
          begin
            FRegisters[A] := VMValueToRegisterFast(
              ResolvedDynamicVarScope.GetValue(Desc.Name));
            Continue;
          end;

          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError('Cannot access lexical binding before initialization');
            SetRegisterRaw(A, Upvalue.Cell.Value)
          end
          else
            FRegisters[A] := RegisterUndefined;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError('Cannot access lexical binding before initialization');
            Upvalue.Cell.Value := FRegisters[A];
          end;
        end;
      end;

      OP_SET_UPVALUE_DYNAMIC:
      begin
        Desc := Template.GetUpvalueDescriptor(DecodeBx(Instruction));
        ResolvedDynamicVarScope := ResolveDynamicUpvalueScope(
          DecodeBx(Instruction), Desc.Name);
        if Assigned(ResolvedDynamicVarScope) then
          ResolvedDynamicVarScope.AssignBinding(Desc.Name,
            RegisterToValue(FRegisters[A]))
        else if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError(
                'Cannot access lexical binding before initialization');
            Upvalue.Cell.Value := FRegisters[A];
          end;
        end;
      end;

      OP_RESOLVE_UPVALUE_REF:
      begin
        Desc := Template.GetUpvalueDescriptor(B);
        ResolvedDynamicVarScope := ResolveDynamicUpvalueScope(B, Desc.Name);
        if Assigned(ResolvedDynamicVarScope) then
          SetRegister(A, TGocciaResolvedEnvironmentReferenceValue.Create(
            ResolvedDynamicVarScope, C <> 0))
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_UPVALUE_REF:
      begin
        Desc := Template.GetUpvalueDescriptor(C);
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is
             TGocciaResolvedEnvironmentReferenceValue) then
        begin
          ResolvedEnvironmentReference :=
            TGocciaResolvedEnvironmentReferenceValue(
              FRegisters[B].ObjectValue);
          ResolvedEnvironmentReference.Scope.SetOwnMutableBinding(Desc.Name,
            RegisterToValue(FRegisters[A]),
            ResolvedEnvironmentReference.Strict);
        end
        else if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(C);
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError(
                'Cannot access lexical binding before initialization');
            Upvalue.Cell.Value := FRegisters[A];
          end;
        end;
      end;

      OP_SET_GLOBAL_STATIC:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and
           not FGlobalScope.TryAssignExistingBinding(GlobalName,
             RegisterToValue(FRegisters[A])) then
          ThrowReferenceError(GlobalName + ' is not defined');
      end;

      OP_CLOSE_UPVALUE:
      begin
        KeyIndex := DecodeBx(Instruction);
        if KeyIndex < FLocalCellCount then
          FLocalCells[KeyIndex] := nil;
      end;

      OP_ARG_COUNT:
        FRegisters[A] := RegisterInt(FArgCount);

      OP_LOAD_ARGUMENT:
        if (B < FArgCount) then
          SetRegisterRaw(A, FArguments[B])
        else
          FRegisters[A] := RegisterUndefined;

      OP_CHECK_DERIVED_THIS:
        if not FCurrentConstructorSuperCalled then
          ThrowReferenceError(
            'Must call super constructor before accessing this');

      OP_CREATE_ARGUMENTS:
        SetRegister(A, CreateArgumentsObjectFromCurrentFrame(B <> 0, C));

      OP_PACK_ARGS:
      begin
        ArgsArray := TGocciaArrayValue.Create;
        for I := B to FArgCount - 1 do
          ArgsArray.Elements.Add(RegisterToValue(FArguments[I]));
        FRegisters[A] := RegisterObject(ArgsArray);
      end;

      OP_JUMP:
      begin
        JumpOffset := DecodeAx(Instruction);
        Inc(Frame.IP, JumpOffset);
        if JumpOffset < 0 then
          CheckExecutionTimeout;
      end;

      OP_JUMP_IF_TRUE:
        if RegisterToBoolean(FRegisters[A]) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          JumpOffset := DecodesBx(Instruction);
          Inc(Frame.IP, JumpOffset);
          if JumpOffset < 0 then
            CheckExecutionTimeout;
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP_IF_FALSE:
        if not RegisterToBoolean(FRegisters[A]) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          JumpOffset := DecodesBx(Instruction);
          Inc(Frame.IP, JumpOffset);
          if JumpOffset < 0 then
            CheckExecutionTimeout;
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP_IF_NUM_NOT_LTE_IMM:
        begin
          if FRegisters[A].Kind = grkInt then
            NumericComparisonResult := FRegisters[A].IntValue <= Int16(B)
          else if FRegisters[A].Kind = grkFloat then
            NumericComparisonResult := FRegisters[A].FloatValue <= Int16(B)
          else
            raise Exception.Create(
              'Invalid non-numeric source for OP_JUMP_IF_NUM_NOT_LTE_IMM');
          if not NumericComparisonResult then
          begin
            if FCoverageEnabled and
               (TGocciaCoverageTracker.Instance <> nil) and
               Assigned(Template.DebugInfo) then
              TGocciaCoverageTracker.Instance.RecordBranchHit(
                Template.DebugInfo.SourceFile,
                Template.DebugInfo.GetLineForPC(InstructionStartIP),
                Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
            JumpOffset := Int16(C);
            Inc(Frame.IP, JumpOffset);
            if JumpOffset < 0 then
              CheckExecutionTimeout;
          end
          else if FCoverageEnabled and
                  (TGocciaCoverageTracker.Instance <> nil) and
                  Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);
        end;

      OP_JUMP_IF_NULLISH:
        if RegisterMatchesNullishKind(FRegisters[A], B) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          Inc(Frame.IP, C);
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP_IF_NOT_NULLISH:
        if not RegisterMatchesNullishKind(FRegisters[A], B) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          Inc(Frame.IP, C);
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_PUSH_HANDLER:
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth);

      OP_PUSH_FINALLY_HANDLER:
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth,
          bhkFinally);

      OP_POP_HANDLER:
        if not FHandlerStack.IsEmpty then
          FHandlerStack.Pop;

      OP_ADD_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
            RegisterToDouble(FRegisters[C]));

      OP_ADD_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
          RegisterToDouble(FRegisters[C]));

      OP_SUB_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
            RegisterToDouble(FRegisters[C]));

      OP_SUB_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
          RegisterToDouble(FRegisters[C]));

      OP_SUB_NUM_IMM:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue - Int16(C))
        else if FRegisters[B].Kind = grkFloat then
          FRegisters[A] := VMNumberRegister(FRegisters[B].FloatValue - Int16(C))
        else
          raise Exception.Create(
            'Invalid non-numeric source for OP_SUB_NUM_IMM');

      OP_MUL_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue *
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
            RegisterToDouble(FRegisters[C]));

      OP_MUL_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
          RegisterToDouble(FRegisters[C]));

      OP_DIV_INT, OP_DIV_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) /
          RegisterToDouble(FRegisters[C]));

      OP_MOD_INT, OP_MOD_FLOAT:
        FRegisters[A] := VMModuloRegister(RegisterToDouble(FRegisters[B]),
          RegisterToDouble(FRegisters[C]));

      OP_EQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) = RegisterToDouble(FRegisters[C]));

      OP_EQ_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) = RegisterToDouble(FRegisters[C]));

      OP_NEQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) <> RegisterToDouble(FRegisters[C]));

      OP_NEQ_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) <> RegisterToDouble(FRegisters[C]));

      OP_LT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue < FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) < RegisterToDouble(FRegisters[C]));

      OP_LT_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) < RegisterToDouble(FRegisters[C]));

      OP_GT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue > FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) > RegisterToDouble(FRegisters[C]));

      OP_GT_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) > RegisterToDouble(FRegisters[C]));

      OP_LTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <= FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) <= RegisterToDouble(FRegisters[C]));

      OP_LTE_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) <= RegisterToDouble(FRegisters[C]));

      OP_GTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue >= FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) >= RegisterToDouble(FRegisters[C]));

      OP_GTE_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) >= RegisterToDouble(FRegisters[C]));

      OP_NEG_INT, OP_NEG_FLOAT:
        FRegisters[A] := VMNumberRegister(-RegisterToDouble(FRegisters[B]));

      OP_CONCAT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value +
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMRegisterToStringFast(FRegisters[B]).Value +
            VMRegisterToStringFast(FRegisters[C]).Value));
      end;

      OP_NEW_ARRAY:
        SetRegister(A, TGocciaArrayValue.Create(nil, B));

      OP_ARRAY_POP:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          if TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count = 0 then
            FRegisters[A] := RegisterUndefined
          else
          begin
            FRegisters[A] := VMValueToRegisterFast(TGocciaArrayValue(
              FRegisters[B].ObjectValue).Elements[
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1]);
            TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Delete(
              TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1);
            if FRegisters[A].Kind = grkHole then
              FRegisters[A] := RegisterUndefined;
          end;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_ARRAY_PUSH:
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaArrayValue) then
          TGocciaArrayValue(FRegisters[A].ObjectValue).Elements.Add(
            RegisterToValue(FRegisters[B]));

      OP_ARRAY_GET:
        ExecGetComputedProperty(A, FRegisters[B], FRegisters[C],
          ELEMENT_GET_OPTIONS);

      OP_ARRAY_SET:
        ExecSetComputedProperty(A, FRegisters[B], FRegisters[C],
          ELEMENT_SET_OPTIONS);

      OP_GET_LENGTH:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
          FRegisters[A] := VMNumberRegister(
            TGocciaArrayValue(FRegisters[B].ObjectValue).GetLength)
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) then
          FRegisters[A] := VMNumberRegister(UTF16CodeUnitLength(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value))
        else
          FRegisters[A] := RegisterInt(0);
      end;

      OP_NEW_OBJECT:
      begin
        if TGocciaObjectValue.SharedObjectPrototype = nil then
          TGocciaObjectValue.InitializeSharedPrototype;
        FRegisters[A] := RegisterObject(TGocciaVMLiteralObjectValue.Create(
          TGocciaObjectValue.SharedObjectPrototype,
          DecodeBx(Instruction)));
      end;

      OP_NEW_CLASS:
      begin
        FRegisters[A] := RegisterObject(TGocciaVMClassValue.Create(Self,
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue, nil));
        TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
          PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(
            FRegisters[A].ObjectValue, [pfConfigurable, pfWritable]));
      end;

      OP_SET_CLASS_SOURCE_CONST:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaClassValue) then
          TGocciaClassValue(FRegisters[A].ObjectValue).SetSourceText(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue);
      end;

      OP_CLASS_SET_SUPER:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
           (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaClassValue) then
        begin
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SuperClass :=
            TGocciaClassValue(FRegisters[B].ObjectValue);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).NativeSuperConstructor :=
            nil;
          // Set [[Prototype]] of derived class constructor to superclass
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetConstructorPrototype(
            TGocciaObjectValue(FRegisters[B].ObjectValue));
          // Set .prototype chain: DerivedClass.prototype.[[Prototype]] = SuperClass.prototype
          TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype :=
            TGocciaClassValue(FRegisters[B].ObjectValue).Prototype;
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
                (FRegisters[B].Kind = grkNull) then
        begin
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SuperClass := nil;
          TGocciaVMClassValue(FRegisters[A].ObjectValue).NativeSuperConstructor :=
            TGocciaFunctionBase.GetSharedPrototype;
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetConstructorPrototype(
            TGocciaFunctionBase.GetSharedPrototype);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype := nil;
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
                (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaObjectValue) and
                FRegisters[B].ObjectValue.IsConstructable then
        begin
          // Native constructor superclass: preserve static and prototype
          // inheritance links, and remember the constructor for instantiation.
          TGocciaVMClassValue(FRegisters[A].ObjectValue).LinkNativeSuperConstructor(
            TGocciaObjectValue(FRegisters[B].ObjectValue));
          RightValue := FRegisters[B].ObjectValue.GetProperty(PROP_PROTOTYPE);
          if RightValue is TGocciaNullLiteralValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype := nil
          else if RightValue is TGocciaObjectValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype :=
              TGocciaObjectValue(RightValue)
          else
            ThrowTypeError(
              'Superclass prototype must be an object or null',
              'set the superclass prototype property to an object or null');
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
          ThrowTypeError(Format(SErrorValueNotConstructor,
            [RegisterToValue(FRegisters[B]).TypeName]),
            SSuggestNotConstructorType);
      end;

      OP_CLASS_ADD_METHOD_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          if IsBytecodePrivateKey(GlobalName) then
            DeclareBytecodePrivateNameForClass(
              FRegisters[A].ObjectValue, GlobalName);
          SetBytecodeHomeObject(RegisterToValue(FRegisters[C]),
            FRegisters[A].ObjectValue);
          if GlobalName = PROP_CONSTRUCTOR then
          begin
            TGocciaVMClassValue(FRegisters[A].ObjectValue).SetVMConstructor(
              RegisterToValue(FRegisters[C]));
          end
          else
            // ES §14.3.7: class prototype methods are non-enumerable
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
              GlobalName, TGocciaPropertyDescriptorData.Create(
                RegisterToValue(FRegisters[C]), [pfConfigurable, pfWritable]));
        end
        else if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
          SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RegisterToValue(FRegisters[C]))
        else
          SetPropertyValue(GetRegister(A), GlobalName, GetRegister(C));
      end;

      OP_CLASS_SET_FIELD_INITIALIZER:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[B]),
            FRegisters[A].ObjectValue);
          if RegisterToValue(FRegisters[B]) is TGocciaBytecodeFunctionValue then
            DeclareBytecodePrivateNamesFromTemplate(
              FRegisters[A].ObjectValue,
              TGocciaBytecodeFunctionValue(RegisterToValue(FRegisters[B]))
                .FClosure.Template);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetMethodInitializers(
            [RegisterToValue(FRegisters[B])]);
        end;
      end;

      OP_CLASS_DECLARE_PRIVATE_STATIC_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          DeclareBytecodePrivateNameForClass(FRegisters[A].ObjectValue,
            GlobalName, True);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).AddPrivateStaticProperty(
            BytecodePrivateRuntimeKey(GlobalName,
              TGocciaVMClassValue(FRegisters[A].ObjectValue)
                .PrivateBrandToken),
            TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
      end;

      // ES2022 §15.7.14: execute static block closure with this = class
      OP_CLASS_EXEC_STATIC_BLOCK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[B]),
            FRegisters[A].ObjectValue);
          PushFrame(B, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
          SetupNewFrame(
            TGocciaBytecodeFunctionValue(FRegisters[B].ObjectValue).FClosure,
            FRegisters[A], TGocciaRegisterArray(nil), 0,
            RegisterUndefined, RegisterUndefined, RegisterUndefined, True, True,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          Continue;
        end
        else if (FRegisters[B].Kind = grkObject) and
                Assigned(FRegisters[B].ObjectValue) and
                FRegisters[B].ObjectValue.IsCallable then
        begin
          CallArgs := AcquireArguments(0);
          try
            InvokeFunctionValue(RegisterToValue(FRegisters[B]),
              CallArgs, RegisterToValue(FRegisters[A]));
          finally
            ReleaseArguments(CallArgs);
          end;
        end;
      end;

      OP_GET_PROP_CONST:
        if (FRegisters[B].Kind = grkObject) and Assigned(FRegisters[B].ObjectValue) and
           (FRegisters[B].ObjectValue is TGocciaObjectValue) then
        begin
          // Hot shape: per-site inline cache keyed by the name-constant
          // index, validated against (own-map identity, map entry version).
          // Hits and fills serve only own plain data properties on
          // ordinary-lookup receivers; everything else degrades to the
          // generic GetPropertyValue path. Sites whose MissStreak saturated
          // are megamorphic: they skip the cache and use the uncached
          // own-data fast path. A nil slot (out-of-range constant index in
          // corrupt bytecode) runs fully uncached.
          PropertyReadCache := Template.PropertyReadCacheSlot(C);
          if Assigned(PropertyReadCache) and
             (PropertyReadCache^.MissStreak <
              PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
             VMPropertyReadCacheableReceiver(FRegisters[B].ObjectValue) and
             VMTryGetCachedOwnDataProperty(
               TGocciaObjectValue(FRegisters[B].ObjectValue),
               PropertyReadCache, GlobalBindingValue) then
            SetRegisterFast(A, GlobalBindingValue)
          else
          begin
            ProtoReadCache := Template.ProtoReadCacheSlot(C);
            if Assigned(ProtoReadCache) and
               (ProtoReadCache^.MissStreak <
                PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
               VMTryGetCachedProtoProperty(
                 TGocciaObjectValue(FRegisters[B].ObjectValue),
                 ProtoReadCache, GlobalBindingValue) then
              SetRegisterFast(A, GlobalBindingValue)
            else
            begin
              GlobalName := Template.GetConstantUnchecked(C).StringValue;
              if VMPropertyReadCacheableReceiver(FRegisters[B].ObjectValue) and
                 (not IsBytecodePrivateKey(GlobalName)) then
              begin
                // One own-map probe establishes own-data / own-non-data /
                // absent; no fallback tier re-hashes the same name on this
                // receiver.
                case VMProbeOwnProperty(
                  TGocciaObjectValue(FRegisters[B].ObjectValue), GlobalName,
                  KeyIndex, PrivateDescriptor) of
                  oppData:
                  // A not-yet-materialized lazy descriptor (the only
                  // TGocciaPropertyDescriptorData subclass) must not be read raw
                  // or cached here: route its first touch through
                  // GetPropertyValue, which materializes it and replaces the
                  // entry in place with a plain descriptor so later reads cache
                  // normally.
                  if PrivateDescriptor.ClassType =
                     TGocciaPropertyDescriptorData then
                  begin
                    if Assigned(PropertyReadCache) and
                       (PropertyReadCache^.MissStreak <
                        PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) then
                      VMPrimeOwnPropertyCache(
                        TGocciaObjectValue(FRegisters[B].ObjectValue),
                        KeyIndex, PropertyReadCache);
                    SetRegisterFast(A,
                      TGocciaPropertyDescriptorData(PrivateDescriptor).Value);
                  end
                  else
                    SetRegister(A, GetPropertyValue(
                      FRegisters[B].ObjectValue, GlobalName));
                  oppNonData:
                  begin
                    // Accessor/exotic own descriptor: never cacheable here;
                    // converge the own tier toward dormant.
                    if Assigned(PropertyReadCache) and
                       (PropertyReadCache^.MissStreak <
                        PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) then
                      Inc(PropertyReadCache^.MissStreak);
                    ServeOwnNonDataProperty(A, FRegisters[B].ObjectValue,
                      PrivateDescriptor);
                  end;
                else
                  // oppAbsent: own absence is established, so the proto
                  // fill may skip its own re-probe (see the core's
                  // contract); deeper or exotic resolutions stay generic.
                  if Assigned(ProtoReadCache) and
                     (ProtoReadCache^.MissStreak <
                      PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
                     VMFillProtoReadCache(
                       TGocciaObjectValue(FRegisters[B].ObjectValue),
                       GlobalName, ProtoReadCache, GlobalBindingValue) then
                    SetRegisterFast(A, GlobalBindingValue)
                  else
                    SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue,
                      GlobalName));
                end;
              end
              else
                SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue,
                  GlobalName));
            end;
          end;
        end
        else if (FRegisters[B].Kind = grkObject) and
                Assigned(FRegisters[B].ObjectValue) then
          SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue,
            Template.GetConstantUnchecked(C).StringValue))
        else
          SetRegister(A, GetPropertyValue(GetRegister(B),
            Template.GetConstantUnchecked(C).StringValue));

      OP_SET_PROP_CONST:
        if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
        begin
          GlobalName := Template.GetConstantUnchecked(B).StringValue;
          RightValue := RegisterToValue(FRegisters[C]);
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue,
              RegisterToValue(FRegisters[A]));
          if IsBytecodePrivateKey(GlobalName) then
            SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue)
          else if FRegisters[A].ObjectValue is TGocciaVMLiteralObjectValue then
          begin
            if not TGocciaVMLiteralObjectValue(FRegisters[A].ObjectValue)
              .TrySetLiteralDataPropertyFast(GlobalName, RightValue) then
              SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue);
          end
          else
            SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue);
        end
        else
          SetPropertyValue(GetRegister(A),
            Template.GetConstantUnchecked(B).StringValue,
            GetRegister(C));

      OP_SET_PROP_CONST_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if (TargetValue is TGocciaClassValue) or
           (TargetValue is TGocciaObjectValue) then
          SetBytecodeHomeObject(RightValue, TargetValue);
        SetPropertyValueLoose(TargetValue, GlobalName, RightValue);
      end;

      OP_DEFINE_STATIC_PROP_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
          if IsBytecodePrivateKey(GlobalName) then
          begin
            if (FRegisters[A].ObjectValue is TGocciaInstanceValue) then
            begin
              if (not TGocciaInstanceValue(FRegisters[A].ObjectValue)
                    .TryGetRawPrivateProperty(GlobalName, TargetValue)) and
                 (not TGocciaInstanceValue(FRegisters[A].ObjectValue)
                    .Extensible) then
                ThrowTypeError(
                  'Cannot add private elements to a non-extensible object',
                  SSuggestObjectNotExtensible);
            end
            else if (FRegisters[A].ObjectValue is TGocciaObjectValue) and
                    (not TryGetRawObjectPrivateDescriptor(
                      TGocciaObjectValue(FRegisters[A].ObjectValue),
                      GlobalName, PrivateDescriptor)) and
                    (not TGocciaObjectValue(FRegisters[A].ObjectValue)
                      .Extensible) then
              ThrowTypeError(
                'Cannot add private elements to a non-extensible object',
                SSuggestObjectNotExtensible);
            SetRawPrivateValue(FRegisters[A].ObjectValue, GlobalName,
              RightValue);
            Continue;
          end;
          TGocciaObjectValue(FRegisters[A].ObjectValue).DefineProperty(
            GlobalName,
            TGocciaPropertyDescriptorData.Create(
              RightValue, [pfEnumerable, pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(GetRegister(A), GlobalName, RightValue);
      end;

      OP_DEFINE_STATIC_PROP_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if TargetValue is TGocciaObjectValue then
        begin
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          if TargetValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, TargetValue, True);

          if PropKey.Kind = pkkSymbol then
            TGocciaObjectValue(TargetValue).DefineSymbolProperty(
              PropKey.Symbol,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]))
          else
          begin
            GlobalName := PropertyKeyName(PropKey);
            if IsBytecodePrivateKey(GlobalName) then
            begin
              if TargetValue is TGocciaInstanceValue then
              begin
                if (not TGocciaInstanceValue(TargetValue)
                      .TryGetRawPrivateProperty(GlobalName, LeftValue)) and
                   (not TGocciaInstanceValue(TargetValue).Extensible) then
                  ThrowTypeError(
                    'Cannot add private elements to a non-extensible object',
                    SSuggestObjectNotExtensible);
              end
              else if (not TryGetRawObjectPrivateDescriptor(
                       TGocciaObjectValue(TargetValue), GlobalName,
                       PrivateDescriptor)) and
                      (not TGocciaObjectValue(TargetValue).Extensible) then
                ThrowTypeError(
                  'Cannot add private elements to a non-extensible object',
                  SSuggestObjectNotExtensible);
              SetRawPrivateValue(TargetValue, GlobalName, RightValue);
              Continue;
            end;

            TGocciaObjectValue(TargetValue).DefineProperty(
              GlobalName,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]));
          end;
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_DEFINE_PROP_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if TargetValue is TGocciaObjectValue then
        begin
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          if PropKey.Kind = pkkSymbol then
            TGocciaObjectValue(TargetValue).DefineSymbolProperty(
              PropKey.Symbol,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]))
          else
            TGocciaObjectValue(TargetValue).DefineProperty(
              PropertyKeyName(PropKey),
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_DEFINE_STATIC_METHOD_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if IsBytecodePrivateKey(GlobalName) and
             (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
          begin
            DeclareBytecodePrivateNameForClass(FRegisters[A].ObjectValue,
              GlobalName, True);
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
            TGocciaVMClassValue(FRegisters[A].ObjectValue).AddPrivateStaticMethod(
              BytecodePrivateRuntimeKey(GlobalName,
                TGocciaVMClassValue(FRegisters[A].ObjectValue)
                  .PrivateBrandToken),
              RightValue);
            Continue;
          end;
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
          TGocciaObjectValue(FRegisters[A].ObjectValue).DefineProperty(
            GlobalName,
            TGocciaPropertyDescriptorData.Create(
              RightValue, [pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(GetRegister(A), GlobalName, RightValue);
      end;

      OP_DEFINE_DATA_PROP:
        DefineDataPropertyByKey(RegisterToValue(FRegisters[A]),
          FRegisters[B], RegisterToValue(FRegisters[C]));

      OP_DEFINE_METHOD_PROP:
        DefineMethodPropertyByKey(RegisterToValue(FRegisters[A]),
          FRegisters[B], RegisterToValue(FRegisters[C]));

      OP_DEFINE_CLASS_METHOD_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if TargetValue is TGocciaObjectValue then
        begin
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          if TargetValue is TGocciaClassValue then
            SetBytecodeHomeObject(RightValue, TargetValue, True)
          else
            SetBytecodeHomeObject(RightValue, TargetValue);

          if PropKey.Kind = pkkSymbol then
            TGocciaObjectValue(TargetValue).DefineSymbolProperty(
              PropKey.Symbol,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfConfigurable, pfWritable]))
          else
            TGocciaObjectValue(TargetValue).DefineProperty(
              PropertyKeyName(PropKey),
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_SET_OBJECT_PROTO:
        SetObjectLiteralPrototype(RegisterToValue(FRegisters[A]),
          RegisterToValue(FRegisters[B]));

      OP_DELETE_PROP_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if FRegisters[A].Kind = grkNull then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if FRegisters[A].Kind = grkUndefined then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaStringLiteralValue) and
           IsNonConfigurableStringExoticProperty(
             TGocciaStringLiteralValue(FRegisters[A].ObjectValue),
             GlobalName) then
          ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
            [GlobalName,
             TGocciaStringLiteralValue(FRegisters[A].ObjectValue).Value]),
            SSuggestCannotDeleteNonConfigurable)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if TGocciaObjectValue(FRegisters[A].ObjectValue).DeleteProperty(
            GlobalName) then
            FRegisters[A] := RegisterBoolean(True)
          else
            ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
              [GlobalName, '[object Object]']),
              SSuggestCannotDeleteNonConfigurable);
        end
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_DELETE_PROP_CONST_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if FRegisters[A].Kind = grkNull then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if FRegisters[A].Kind = grkUndefined then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaStringLiteralValue) and
           IsNonConfigurableStringExoticProperty(
             TGocciaStringLiteralValue(FRegisters[A].ObjectValue),
             GlobalName) then
          FRegisters[A] := RegisterBoolean(False)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if TGocciaObjectValue(FRegisters[A].ObjectValue).DeleteProperty(
            GlobalName) then
            FRegisters[A] := RegisterBoolean(True)
          else
            FRegisters[A] := RegisterBoolean(False);
        end
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_UNPACK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          ArgsArray := TGocciaArrayValue.Create;
          for I := C to TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1 do
            ArgsArray.Elements.Add(
              TGocciaArrayValue(FRegisters[B].ObjectValue).GetElement(I));
          FRegisters[A] := RegisterObject(ArgsArray);
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_GET_INDEX:
        ExecGetComputedProperty(A, FRegisters[B], FRegisters[C],
          MEMBER_GET_OPTIONS);

      OP_SET_INDEX:
        ExecSetComputedProperty(A, FRegisters[B], FRegisters[C],
          MEMBER_SET_OPTIONS);

      OP_GET_WITH_BINDING:
        SetRegister(A, GetWithBindingValue(GetRegister(B), GetRegister(C),
          False));

      OP_GET_WITH_BINDING_STRICT:
        SetRegister(A, GetWithBindingValue(GetRegister(B), GetRegister(C),
          True));

      OP_SET_WITH_BINDING:
        SetWithBindingValue(GetRegister(A), GetRegister(B), GetRegister(C),
          True);

      OP_SET_WITH_BINDING_LOOSE:
        SetWithBindingValue(GetRegister(A), GetRegister(B), GetRegister(C),
          False);

      OP_SET_INDEX_LOOSE:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if (TargetValue is TGocciaClassValue) or
           (TargetValue is TGocciaObjectValue) then
          SetBytecodeHomeObject(RightValue, TargetValue);
        SetIndexValueLoose(TargetValue, FRegisters[B], RightValue);
      end;

      OP_ADD:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
            RegisterToDouble(FRegisters[C]));
        end
        else begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarMiss;
          if (((FRegisters[B].Kind = grkObject) and
                  (FRegisters[B].ObjectValue is TGocciaStringLiteralValue)) or
                 ((FRegisters[C].Kind = grkObject) and
                  (FRegisters[C].ObjectValue is TGocciaStringLiteralValue))) and
                (not ((FRegisters[B].Kind = grkObject) and
                      Assigned(FRegisters[B].ObjectValue) and
                      (not FRegisters[B].ObjectValue.IsPrimitive))) and
                (not ((FRegisters[C].Kind = grkObject) and
                      Assigned(FRegisters[C].ObjectValue) and
                      (not FRegisters[C].ObjectValue.IsPrimitive))) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMRegisterToStringFast(FRegisters[B]).Value +
            VMRegisterToStringFast(FRegisters[C]).Value))
        else
        begin
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            SetRegisterFast(A, TGocciaStringLiteralValue.Create(
              TGocciaStringLiteralValue(LeftValue).Value +
              TGocciaStringLiteralValue(RightValue).Value))
          else if LeftValue.IsPrimitive and RightValue.IsPrimitive then
          begin
            if (LeftValue is TGocciaStringLiteralValue) or
               (RightValue is TGocciaStringLiteralValue) then
              SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                LeftValue.ToStringLiteral.Value + RightValue.ToStringLiteral.Value))
            else
              SetRegisterFast(A, EvaluateAddition(LeftValue, RightValue));
          end
          else
            SetRegister(A, EvaluateAddition(LeftValue, RightValue));
        end;
        end;
      end;

      OP_SUB:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, EvaluateSubtraction(
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_INC:
        if FRegisters[B].Kind = grkInt then
          SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue + 1))
        else if FRegisters[B].Kind = grkFloat then
          SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue + 1.0))
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaBigIntValue) then
          SetRegister(A, TGocciaBigIntValue.Create(
            TGocciaBigIntValue(FRegisters[B].ObjectValue).Value.Add(TBigInteger.One)))
        else
          SetRegister(A, VMNumberValue(GetRegisterFast(B).ToNumberLiteral.Value + 1));

      OP_DEC:
        if FRegisters[B].Kind = grkInt then
          SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue - 1))
        else if FRegisters[B].Kind = grkFloat then
          SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue - 1.0))
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaBigIntValue) then
          SetRegister(A, TGocciaBigIntValue.Create(
            TGocciaBigIntValue(FRegisters[B].ObjectValue).Value.Subtract(TBigInteger.One)))
        else
          SetRegister(A, VMNumberValue(GetRegisterFast(B).ToNumberLiteral.Value - 1));

      OP_INC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
            SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue + 1));
          grkFloat:
            SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue + 1.0));
          grkBoolean:
            if FRegisters[B].BoolValue then
              SetRegisterRaw(A, RegisterInt(2))
            else
              SetRegisterRaw(A, RegisterInt(1));
          grkNull:
            SetRegisterRaw(A, RegisterInt(1));
          grkUndefined, grkHole:
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Add(TBigInteger.One)))
          else
            SetRegister(A, VMNumberValue(LeftValue.ToNumberLiteral.Value + 1));
        end;

      OP_DEC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
            SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue - 1));
          grkFloat:
            SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue - 1.0));
          grkBoolean:
            if FRegisters[B].BoolValue then
              SetRegisterRaw(A, RegisterInt(0))
            else
              SetRegisterRaw(A, RegisterInt(-1));
          grkNull:
            SetRegisterRaw(A, RegisterInt(-1));
          grkUndefined, grkHole:
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Subtract(TBigInteger.One)))
          else
            SetRegister(A, VMNumberValue(LeftValue.ToNumberLiteral.Value - 1));
        end;

      OP_POST_INC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMIntResult(FRegisters[B].IntValue + 1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkFloat:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMNumberRegister(FRegisters[B].FloatValue + 1.0);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkBoolean:
          begin
            if FRegisters[B].BoolValue then
            begin
              FRegisters[A] := RegisterInt(1);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(2);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end
            else
            begin
              FRegisters[A] := RegisterInt(0);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(1);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end;
          end;
          grkNull:
          begin
            FRegisters[A] := RegisterInt(0);
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := RegisterInt(1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkUndefined, grkHole:
          begin
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
            SetRegister(B, TGocciaNumberLiteralValue.NaNValue);
          end;
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
          begin
            SetRegisterFast(A, LeftValue);
            SetRegister(B, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Add(TBigInteger.One)));
          end
          else
          begin
            NumericValue := LeftValue.ToNumberLiteral.Value;
            SetRegister(A, VMNumberValue(NumericValue));
            SetRegister(B, VMNumberValue(NumericValue + 1));
          end;
        end;

      OP_POST_DEC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMIntResult(FRegisters[B].IntValue - 1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkFloat:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMNumberRegister(FRegisters[B].FloatValue - 1.0);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkBoolean:
          begin
            if FRegisters[B].BoolValue then
            begin
              FRegisters[A] := RegisterInt(1);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(0);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end
            else
            begin
              FRegisters[A] := RegisterInt(0);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(-1);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end;
          end;
          grkNull:
          begin
            FRegisters[A] := RegisterInt(0);
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := RegisterInt(-1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkUndefined, grkHole:
          begin
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
            SetRegister(B, TGocciaNumberLiteralValue.NaNValue);
          end;
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
          begin
            SetRegisterFast(A, LeftValue);
            SetRegister(B, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Subtract(TBigInteger.One)));
          end
          else
          begin
            NumericValue := LeftValue.ToNumberLiteral.Value;
            SetRegister(A, VMNumberValue(NumericValue));
            SetRegister(B, VMNumberValue(NumericValue - 1));
          end;
        end;

      OP_MUL:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue *
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, EvaluateMultiplication(
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_DIV:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) /
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, EvaluateDivision(
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_MOD:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMModuloRegister(RegisterToDouble(FRegisters[B]),
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, EvaluateModulo(
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_POW:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMPowerRegister(RegisterToDouble(FRegisters[B]),
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, EvaluateExponentiation(
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_NEG:
        if RegisterIsNumericScalar(FRegisters[B]) then
          FRegisters[A] := VMNumberRegister(-RegisterToDouble(FRegisters[B]))
        else
        begin
          // ES2026 §13.5.5 UnaryMinus invokes ToNumeric (= ToPrimitive
          // then a BigInt? branch).  Apply ToPrimitive so boxed BigInts
          // (Object(1n)) unbox to their primitive and take the
          // BigInt::unaryMinus path; without it the box's
          // ToNumberLiteral coerces to NaN and we lose the BigInt.
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Negate))
          else
            SetRegister(A, VMNumberValue(-LeftValue.ToNumberLiteral.Value));
        end;

      OP_BAND:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) and
            LongInt(FRegisters[C].IntValue))
        else
          SetRegister(A, EvaluateBitwiseAnd(
            GetRegister(B), GetRegister(C)));

      OP_BOR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) or
            LongInt(FRegisters[C].IntValue))
        else
          SetRegister(A, EvaluateBitwiseOr(
            GetRegister(B), GetRegister(C)));

      OP_BXOR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) xor
            LongInt(FRegisters[C].IntValue))
        else
          SetRegister(A, EvaluateBitwiseXor(
            GetRegister(B), GetRegister(C)));

      OP_SHL:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(LongInt(
            LongWord(FRegisters[B].IntValue) shl
            (LongWord(FRegisters[C].IntValue) and 31)))
        else
          SetRegister(A, EvaluateLeftShift(
            GetRegister(B), GetRegister(C)));

      OP_SHR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(SignedRightShiftInt32(
            LongInt(FRegisters[B].IntValue),
            LongWord(FRegisters[C].IntValue)))
        else
          SetRegister(A, EvaluateRightShift(
            GetRegister(B), GetRegister(C)));

      OP_USHR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(Int64(LongWord(
            FRegisters[B].IntValue) shr
            (LongWord(FRegisters[C].IntValue) and 31)))
        else
          SetRegister(A, EvaluateUnsignedRightShift(
            GetRegister(B), GetRegister(C)));

      OP_BNOT:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := RegisterInt(not LongInt(FRegisters[B].IntValue))
        else
          SetRegister(A, EvaluateBitwiseNot(GetRegister(B)));

      OP_EQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else if (FRegisters[B].Kind = grkObject) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          FRegisters[A] := RegisterBoolean(UTF16StringsEqual(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value,
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegister(A, GetRegister(B).IsEqual(GetRegister(C)));

      OP_NEQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else if (FRegisters[B].Kind = grkObject) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          FRegisters[A] := RegisterBoolean(not UTF16StringsEqual(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value,
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegister(A, GetRegister(B).IsNotEqual(GetRegister(C)));

      OP_LOOSE_EQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FromBoolean(
            Goccia.Arithmetic.IsLooselyEqual(GetRegister(B), GetRegister(C))));

      OP_LOOSE_NEQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FromBoolean(
            Goccia.Arithmetic.IsNotLooselyEqual(GetRegister(B), GetRegister(C))));

      OP_LT:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) <
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) < 0)
          else
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.LessThan(LeftValue, RightValue));
        end;
      end;

      OP_GT:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue >
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) >
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) > 0)
          else
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.GreaterThan(LeftValue, RightValue));
        end;
      end;

      OP_LTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <=
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) <=
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) <= 0)
          else
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.LessThanOrEqual(LeftValue, RightValue));
        end;
      end;

      OP_GTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue >=
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) >=
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) >= 0)
          else
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.GreaterThanOrEqual(LeftValue, RightValue));
        end;
      end;

      OP_TYPEOF:
        case FRegisters[B].Kind of
          grkUndefined:
            SetRegister(A, TGocciaStringLiteralValue.Create('undefined'));
          grkNull, grkHole:
            SetRegister(A, TGocciaStringLiteralValue.Create('object'));
          grkBoolean:
            SetRegister(A, TGocciaStringLiteralValue.Create('boolean'));
          grkInt, grkFloat:
            SetRegister(A, TGocciaStringLiteralValue.Create('number'));
        else
          SetRegister(A, TGocciaStringLiteralValue.Create(GetRegister(B).TypeOf));
        end;

      OP_IS_INSTANCE:
      begin
        ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
        FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
        SetRegister(A, VMInstanceOfValue(GetRegister(B), GetRegister(C),
          ObjectConstructorValue, FunctionConstructorValue));
      end;

      OP_HAS_PROPERTY:
        SetRegister(A, HasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_HAS_WITH_BINDING:
        SetRegister(A, HasWithBindingValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_HAS_PROPERTY:
        SetRegister(A, MatchHasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_EXTRACTOR:
        SetRegister(A, MatchExtractorValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_VALUE:
      begin
        LeftValue := GetRegister(B);
        RightValue := GetRegister(C);
        CustomMatcherValue := GetCustomMatcher(RightValue);
        if Assigned(CustomMatcherValue) then
        begin
          if not CustomMatcherValue.IsCallable then
            ThrowTypeError('Symbol.customMatcher must be callable');
          CallArgs := AcquireArguments(2);
          try
            MatchHintObject := TGocciaObjectValue.Create;
            MatchHintObject.AssignProperty(PROP_MATCH_TYPE,
              TGocciaStringLiteralValue.Create('boolean'));
            CallArgs.Add(LeftValue);
            CallArgs.Add(MatchHintObject);
            MatchResultValue := InvokeFunctionValue(CustomMatcherValue,
              CallArgs, RightValue);
            SetRegister(A, MatchResultValue.ToBooleanLiteral);
          finally
            ReleaseArguments(CallArgs);
          end;
        end
        else if RightValue is TGocciaClassValue then
        begin
          ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
          FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
          if VMBuiltinConstructorMatchValue(RightValue, LeftValue,
            FGlobalScope, BuiltinConstructorMatch) then
            SetRegister(A, TGocciaBooleanLiteralValue.Create(BuiltinConstructorMatch))
          else
            SetRegister(A, VMInstanceOfValue(LeftValue, RightValue,
              ObjectConstructorValue, FunctionConstructorValue));
        end
        else if VMBuiltinConstructorMatchValue(RightValue, LeftValue,
          FGlobalScope, BuiltinConstructorMatch) then
          SetRegister(A, TGocciaBooleanLiteralValue.Create(BuiltinConstructorMatch))
        else
          SetRegister(A, TGocciaBooleanLiteralValue.Create(
            MatchValueEquals(LeftValue, RightValue)));
      end;

      OP_TO_NUMBER:
        case FRegisters[B].Kind of
          grkInt, grkFloat:
            FRegisters[A] := FRegisters[B];
          grkBoolean:
            if FRegisters[B].BoolValue then
              FRegisters[A] := RegisterInt(1)
            else
              FRegisters[A] := RegisterInt(0);
          grkNull:
            FRegisters[A] := RegisterInt(0);
          grkUndefined, grkHole:
            FRegisters[A] := RegisterObject(TGocciaNumberLiteralValue.NaNValue);
        else
          SetRegister(A, GetRegister(B).ToNumberLiteral);
        end;

      OP_TO_NUMERIC:
        case FRegisters[B].Kind of
          grkInt, grkFloat:
            FRegisters[A] := FRegisters[B];
          grkBoolean:
            if FRegisters[B].BoolValue then
              FRegisters[A] := RegisterInt(1)
            else
              FRegisters[A] := RegisterInt(0);
          grkNull:
            FRegisters[A] := RegisterInt(0);
          grkUndefined, grkHole:
            FRegisters[A] := RegisterObject(TGocciaNumberLiteralValue.NaNValue);
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegisterFast(A, LeftValue)
          else
            SetRegister(A, LeftValue.ToNumberLiteral);
        end;

      OP_TO_STRING:
        SetRegisterFast(A, VMRegisterToStringFast(FRegisters[B]));

      OP_DEL_INDEX:
        ExecDeleteComputedProperty(A, FRegisters[B], FRegisters[C], True);

      OP_DEL_INDEX_LOOSE:
        ExecDeleteComputedProperty(A, FRegisters[B], FRegisters[C], False);

      OP_CLOSURE:
      begin
        ChildTemplate := Template.GetFunctionUnchecked(DecodeBx(Instruction));
        ChildClosure := TGocciaBytecodeClosure.Create(
          ChildTemplate, ChildTemplate.UpvalueCount);
        ChildClosure.GlobalScope := FGlobalScope;
        ChildClosure.DynamicVarScope := FCurrentDynamicVarScope;
        if ChildTemplate.IsArrow and Assigned(FCurrentClosure) then
        begin
          ChildClosure.HomeObject := FCurrentClosure.HomeObject;
          ChildClosure.HomeClass := FCurrentClosure.HomeClass;
          ChildClosure.NewTarget := FCurrentNewTarget;
          if Assigned(FCurrentClosure.Template) and
             FCurrentClosure.Template.IsArrow then
            ChildClosure.AllowsNewTarget := FCurrentClosure.AllowsNewTarget
          else
            ChildClosure.AllowsNewTarget :=
              Assigned(FCurrentClosure.FunctionValue) and
              not TemplateUsesGlobalEvalEnvironment(FCurrentClosure.Template);
        end
        else
          ChildClosure.AllowsNewTarget := True;
        for I := 0 to ChildTemplate.UpvalueCount - 1 do
        begin
          Desc := ChildTemplate.GetUpvalueDescriptor(I);
          if Desc.IsLocal then
            ChildClosure.SetUpvalue(I, TGocciaBytecodeUpvalue.Create(
              GetLocalCell(Desc.Index)))
          else if Assigned(FCurrentClosure) then
          begin
            ChildClosure.SetUpvalue(I, FCurrentClosure.GetUpvalue(Desc.Index));
            ChildClosure.SetDynamicVarUpvalue(I,
              ((FCurrentDynamicVarScope <>
                FCurrentClosure.DynamicVarScope) and
               Assigned(FCurrentDynamicVarScope)) or
              FCurrentClosure.IsDynamicVarUpvalue(Desc.Index));
          end;
        end;
        BytecodeFunction := TGocciaBytecodeFunctionValue.Create(Self, ChildClosure);
        // ES2026 §10.2.5 MakeConstructor: install own `prototype` data property
        // for `function`/`function*` declarations and expressions (including
        // async generators).  The prototype is a fresh ordinary object whose
        // `constructor` data property back-references the function.
        if ChildTemplate.HasOwnPrototype then
          InstallFunctionPrototype(BytecodeFunction,
            BytecodeFunctionIntrinsicKind(ChildTemplate));
        SetRegister(A, BytecodeFunction);
      end;

      OP_CALL_SELF_NUM:
      begin
        CheckExecutionTimeout;
        PushClosedNumericFrame(A, B, C, Frame, Template, PrevCovLine,
          ProfileEntryTimestamp, ClosedNumericInitializedRegisterTop);
        Continue;
      end;

      OP_CALL:
      begin
        CheckExecutionTimeout;
        if ((C and CALL_FLAG_DIRECT_EVAL) <> 0) and
           (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) and
           TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).DirectEvalHost and
           IsCurrentRealmEvalFunction(FRegisters[A].ObjectValue, FRealm) then
        begin
          EvalSourceValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          if (C and CALL_FLAG_SPREAD) <> 0 then
          begin
	            if (FRegisters[B].Kind = grkObject) and
	               (FRegisters[B].ObjectValue is TGocciaArrayValue) and
	               (TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count > 0) then
	              EvalSourceValue := TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty('0');
          end
          else if B > 0 then
            EvalSourceValue := GetRegister(A + 1);
          SetRegister(A, ExecuteDirectEval(EvalSourceValue, Template,
            UInt32(InstructionStartIP), Template.StrictCode));
          Continue;
        end;
        if ((C and CALL_FLAG_SPREAD) = 0) and
           (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) and
           (TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
             CreationRealm = CurrentRealm) and
           (B = 1) and
           (FRegisters[A + 1].Kind = grkObject) and
           (FRegisters[A + 1].ObjectValue is TGocciaStringLiteralValue) then
        begin
          case TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
            IntrinsicKind of
            nikDecodeURI:
              begin
                SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                  DecodeURI(TGocciaStringLiteralValue(
                    FRegisters[A + 1].ObjectValue).Value)));
                Continue;
              end;
            nikDecodeURIComponent:
              begin
                SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                  DecodeURIComponent(TGocciaStringLiteralValue(
                    FRegisters[A + 1].ObjectValue).Value)));
                Continue;
              end;
          end;
        end;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBoundFunctionValue) then
        begin
          BoundFunction := TGocciaBoundFunctionValue(FRegisters[A].ObjectValue);
          if BoundFunction.OriginalFunction is TGocciaBytecodeFunctionValue then
          begin
            BytecodeFunction := TGocciaBytecodeFunctionValue(BoundFunction.OriginalFunction);
            if Assigned(BytecodeFunction.FClosure) and
               Assigned(BytecodeFunction.FClosure.Template) and
               (not BytecodeFunction.FClosure.Template.IsAsync) and
               (not BytecodeFunction.FClosure.Template.IsGenerator) then
            begin
              if (C and 1) = 0 then
              begin
                SetLength(RegisterArgs, BoundFunction.BoundArgCount + B);
                for I := 0 to BoundFunction.BoundArgCount - 1 do
                  RegisterArgs[I] := ValueToRegister(BoundFunction.GetBoundArg(I));
                for I := 0 to B - 1 do
                  RegisterArgs[BoundFunction.BoundArgCount + I] := FRegisters[A + 1 + I];
                CallThisRegister := ValueToRegister(BoundFunction.BoundThis);
                if not BytecodeFunction.FStrictThis then
                  CallThisRegister := CoerceNonStrictThisRegister(
                    CallThisRegister,
                    BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                      FGlobalThisValue),
                    BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                      FRealm));
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, RegisterArgs,
                  Length(RegisterArgs), RegisterUndefined, RegisterUndefined,
                  RegisterUndefined, False, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                Continue;
              end
              else if (FRegisters[B].Kind = grkObject) and
                      (FRegisters[B].ObjectValue is TGocciaArrayValue) then
              begin
                SetLength(RegisterArgs,
                  BoundFunction.BoundArgCount +
                  TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
                for I := 0 to BoundFunction.BoundArgCount - 1 do
                  RegisterArgs[I] := VMValueToRegisterFast(BoundFunction.GetBoundArg(I));
	                for I := 0 to TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1 do
	                  RegisterArgs[BoundFunction.BoundArgCount + I] := VMValueToRegisterFast(
	                    TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntToStr(I)));
                CallThisRegister := ValueToRegister(BoundFunction.BoundThis);
                if not BytecodeFunction.FStrictThis then
                  CallThisRegister := CoerceNonStrictThisRegister(
                    CallThisRegister,
                    BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                      FGlobalThisValue),
                    BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                      FRealm));
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, RegisterArgs,
                  Length(RegisterArgs), RegisterUndefined, RegisterUndefined,
                  RegisterUndefined, False, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                Continue;
              end;
            end;
          end;
        end;

        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue);
          if Assigned(BytecodeFunction.FClosure) and
             Assigned(BytecodeFunction.FClosure.Template) and
             (not BytecodeFunction.FClosure.Template.IsAsync) and
             (not BytecodeFunction.FClosure.Template.IsGenerator) then
          begin
            if not BytecodeFunction.FStrictThis then
            begin
              CallGlobalThisValue := BytecodeClosureGlobalThis(
                BytecodeFunction.FClosure, FGlobalThisValue);
              if Assigned(CallGlobalThisValue) then
                CallThisRegister := VMValueToRegisterFast(CallGlobalThisValue)
              else
                CallThisRegister := RegisterUndefined;
            end
            else
              CallThisRegister := RegisterUndefined;
            if (C and 1) = 0 then
            begin
              if B <= 3 then
              begin
                // Fixed-arg fast path: capture up to three arguments by value
                // before any frame push or tail-call window reuse, so they
                // survive AcquireRegisters' fill, and skip the RegisterArgs
                // staging array entirely. SetupNewFrame consumes only the first
                // B of these (bounded by AArgCount).
                if B >= 1 then FixedArg0 := FRegisters[A + 1]
                else FixedArg0 := RegisterUndefined;
                if B >= 2 then FixedArg1 := FRegisters[A + 2]
                else FixedArg1 := RegisterUndefined;
                if B >= 3 then FixedArg2 := FRegisters[A + 3]
                else FixedArg2 := RegisterUndefined;
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, TGocciaRegisterArray(nil), B,
                  FixedArg0, FixedArg1, FixedArg2, True, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              end
              else
              begin
                SetLength(RegisterArgs, B);
                for I := 0 to B - 1 do
                  RegisterArgs[I] := FRegisters[A + 1 + I];
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, RegisterArgs, B,
                  RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              end;
              Continue;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
	              for I := 0 to High(RegisterArgs) do
	                RegisterArgs[I] := ValueToRegister(
	                  TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntToStr(I)));
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, RegisterArgs, Length(RegisterArgs),
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end;
          end;
        end;

        if (C and 1) = 1 then
          CallArgs := AcquireArguments
        else
          CallArgs := AcquireArguments(B);
        try
          if (C and 1) = 1 then
          begin
	            if GetRegister(B) is TGocciaArrayValue then
	              for I := 0 to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
	                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetProperty(IntToStr(I)));
          end
          else
            for I := 0 to B - 1 do
              CallArgs.Add(GetRegister(A + 1 + I));
          if (GetRegister(A) is TGocciaNativeFunctionValue) or
             (GetRegister(A) is TGocciaFunctionConstructorClassValue) or
             (GetRegister(A) is TGocciaBoundFunctionValue) or
             (GetRegister(A) is TGocciaProxyValue) then
          begin
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
                TGocciaUndefinedLiteralValue.UndefinedValue));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          end
          else
            SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_CALL_METHOD:
      begin
        CheckExecutionTimeout;
        if ((C and CALL_FLAG_SPREAD) = 0) and (B = 2) and
           (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) and
           (TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
             IntrinsicKind = nikStringFromCharCode) and
           (TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
             CreationRealm = CurrentRealm) and
           (FRegisters[A + 1].Kind = grkInt) and
           (FRegisters[A + 2].Kind = grkInt) then
        begin
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            UTF16CodeUnitPairToString(
              Cardinal(FRegisters[A + 1].IntValue and $FFFF),
              Cardinal(FRegisters[A + 2].IntValue and $FFFF))));
          Continue;
        end;
        if (C and 1) = 0 then
        begin
          if (FRegisters[A - 1].Kind = grkObject) and
             (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) then
          begin
            GlobalName := TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).Name;
            if (GlobalName = 'bind') and
               (FRegisters[A - 1].ObjectValue is TGocciaFunctionBase) then
            begin
              case B of
                0:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      TGocciaUndefinedLiteralValue.UndefinedValue));
                1:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1])));
                2:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithSingleArg(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1]),
                      RegisterToValue(FRegisters[A + 2])));
              else
                BytecodeFunction := nil;
              end;
              if B <= 2 then
                Continue;
            end;

            if FRegisters[A - 1].ObjectValue is TGocciaBytecodeFunctionValue then
            begin
              BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A - 1].ObjectValue);
              if Assigned(BytecodeFunction.FClosure) and
                 Assigned(BytecodeFunction.FClosure.Template) and
                 (not BytecodeFunction.FClosure.Template.IsAsync) and
                 (not BytecodeFunction.FClosure.Template.IsGenerator) then
              begin
                if GlobalName = 'call' then
                begin
                  if B = 0 then
                    CallThisRegister := RegisterUndefined
                  else
                    CallThisRegister := FRegisters[A + 1];
                  if not BytecodeFunction.FStrictThis then
                    CallThisRegister := CoerceNonStrictThisRegister(
                      CallThisRegister,
                      BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                        FGlobalThisValue),
                      BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                        FRealm));
                  PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                  case B of
                    0:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    1:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    2:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 1,
                        FRegisters[A + 2], RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    3:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 2,
                        FRegisters[A + 2], FRegisters[A + 3], RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    4:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 3,
                        FRegisters[A + 2], FRegisters[A + 3], FRegisters[A + 4],
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  else
                    begin
                      SetLength(RegisterArgs, B - 1);
                      for I := 1 to B - 1 do
                        RegisterArgs[I - 1] := FRegisters[A + 1 + I];
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, RegisterArgs, Length(RegisterArgs),
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        False, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    end;
                  end;
                  Continue;
                end
                else if (GlobalName = 'apply') and (B >= 2) and
                        (FRegisters[A + 2].Kind = grkObject) and
                        (FRegisters[A + 2].ObjectValue is TGocciaArrayValue) then
                begin
                  ArgsArray := TGocciaArrayValue(FRegisters[A + 2].ObjectValue);
                  CallThisRegister := FRegisters[A + 1];
                  if not BytecodeFunction.FStrictThis then
                    CallThisRegister := CoerceNonStrictThisRegister(
                      CallThisRegister,
                      BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                        FGlobalThisValue),
                      BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                        FRealm));
                  PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                  case ArgsArray.Elements.Count of
                    0:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    1:
                      SetupNewFrame(BytecodeFunction.FClosure,
	                        CallThisRegister, TGocciaRegisterArray(nil), 1,
	                        VMValueToRegisterFast(ArgsArray.GetProperty('0')),
	                        RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    2:
                      SetupNewFrame(BytecodeFunction.FClosure,
	                        CallThisRegister, TGocciaRegisterArray(nil), 2,
	                        VMValueToRegisterFast(ArgsArray.GetProperty('0')),
	                        VMValueToRegisterFast(ArgsArray.GetProperty('1')),
	                        RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    3:
                      SetupNewFrame(BytecodeFunction.FClosure,
	                        CallThisRegister, TGocciaRegisterArray(nil), 3,
	                        VMValueToRegisterFast(ArgsArray.GetProperty('0')),
	                        VMValueToRegisterFast(ArgsArray.GetProperty('1')),
	                        VMValueToRegisterFast(ArgsArray.GetProperty('2')),
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  else
                    begin
	                      SetLength(RegisterArgs, ArgsArray.Elements.Count);
	                      for I := 0 to High(RegisterArgs) do
	                        RegisterArgs[I] := VMValueToRegisterFast(
	                          ArgsArray.GetProperty(IntToStr(I)));
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, RegisterArgs, Length(RegisterArgs),
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        False, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    end;
                  end;
                  Continue;
                end;
              end;
            end;
          end;
        end;

        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue);
          if Assigned(BytecodeFunction.FClosure) and
             Assigned(BytecodeFunction.FClosure.Template) and
             (not BytecodeFunction.FClosure.Template.IsAsync) and
             (not BytecodeFunction.FClosure.Template.IsGenerator) then
          begin
            CallThisRegister := FRegisters[A - 1];
            if not BytecodeFunction.FStrictThis then
              CallThisRegister := CoerceNonStrictThisRegister(
                CallThisRegister,
                BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                  FGlobalThisValue),
                BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                  FRealm));
            if (C and 1) = 0 then
            begin
              SetLength(RegisterArgs, B);
              for I := 0 to B - 1 do
                RegisterArgs[I] := FRegisters[A + 1 + I];
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, RegisterArgs, B,
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
	              for I := 0 to High(RegisterArgs) do
	                RegisterArgs[I] := VMValueToRegisterFast(
	                  TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntToStr(I)));
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, RegisterArgs, Length(RegisterArgs),
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end;
          end;
        end;

        if (C and 1) = 1 then
          CallArgs := AcquireArguments
        else
          CallArgs := AcquireArguments(B);
        try
          if (C and 1) = 1 then
          begin
            if GetRegister(B) is TGocciaArrayValue then
              for I := 0 to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetProperty(IntToStr(I)));
          end
          else
            for I := 0 to B - 1 do
              CallArgs.Add(GetRegister(A + 1 + I));
          if (GetRegister(A) is TGocciaNativeFunctionValue) or
             (GetRegister(A) is TGocciaFunctionConstructorClassValue) or
             (GetRegister(A) is TGocciaBoundFunctionValue) or
             (GetRegister(A) is TGocciaProxyValue) then
          begin
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
                GetRegister(A - 1)));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          end
          else
            SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
              GetRegister(A - 1)));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_CONSTRUCT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
        begin
          SetLength(RegisterArgs, C);
          for I := 0 to C - 1 do
            RegisterArgs[I] := FRegisters[B + 1 + I];
          FRegisters[A] := TGocciaVMClassValue(FRegisters[B].ObjectValue)
            .InstantiateRegisters(RegisterArgs);
        end
        else
        begin
          CallArgs := AcquireArguments(C);
          try
            for I := 0 to C - 1 do
              CallArgs.Add(GetRegister(B + 1 + I));
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          finally
            ReleaseArguments(CallArgs);
          end;
        end;
      end;

      OP_CONSTRUCT_SPREAD:
      begin
        SpreadArray := TGocciaArrayValue(FRegisters[C].ObjectValue);
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
        begin
          SetLength(RegisterArgs, SpreadArray.Elements.Count);
          for I := 0 to SpreadArray.Elements.Count - 1 do
            RegisterArgs[I] := VMValueToRegisterFast(
              SpreadArray.GetProperty(IntToStr(I)));
          FRegisters[A] := TGocciaVMClassValue(FRegisters[B].ObjectValue)
            .InstantiateRegisters(RegisterArgs);
        end
        else
        begin
          CallArgs := AcquireArguments(SpreadArray.Elements.Count);
          try
            for I := 0 to SpreadArray.Elements.Count - 1 do
              CallArgs.Add(SpreadArray.GetProperty(IntToStr(I)));
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          finally
            ReleaseArguments(CallArgs);
          end;
        end;
      end;

      OP_GET_ITER:
        SetRegister(A, GetIteratorValue(GetRegister(B), C <> 0));

      OP_ITER_NEXT:
      begin
        if (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaIteratorValue) then
        begin
          IterResult := TGocciaIteratorValue(FRegisters[C].ObjectValue).DirectNext(DoneFlag);
          if DoneFlag then
            FRegisters[A] := RegisterUndefined
          else
            FRegisters[A] := VMValueToRegisterFast(IterResult);
          if DoneFlag then
            FRegisters[B] := RegisterBoolean(True)
          else
            FRegisters[B] := RegisterBoolean(False);
        end
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaObjectValue) then
        begin
          IterResult := FRegisters[C].ObjectValue;
          NextMethod := IterResult.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
          begin
            FRegisters[A] := RegisterUndefined;
            FRegisters[B] := RegisterBoolean(True);
          end
          else
          begin
            CallArgs := AcquireArguments;
            try
              IterResult := InvokeCallable(NextMethod, CallArgs, IterResult);
            finally
              ReleaseArguments(CallArgs);
            end;

            IterResult := AwaitValue(IterResult);
            if IterResult.IsPrimitive then
              ThrowTypeError(Format(SErrorIteratorResultNotObject, [IterResult.ToStringLiteral.Value]),
                SSuggestIteratorResultObject);

            DoneValue := IterResult.GetProperty(PROP_DONE);
            if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
            begin
              FRegisters[A] := RegisterUndefined;
              FRegisters[B] := RegisterBoolean(True);
            end
            else
            begin
              FRegisters[A] := VMValueToRegisterFast(IterResult.GetProperty(PROP_VALUE));
              FRegisters[B] := RegisterBoolean(False);
            end;
          end;
        end
        else
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(True);
        end;
      end;

      OP_ASYNC_ITER_NEXT:
      begin
        if (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaIteratorValue) then
        begin
          IterResult := TGocciaIteratorValue(FRegisters[C].ObjectValue).DirectNext(DoneFlag);
          SetRegister(A, CreateIteratorResult(IterResult, DoneFlag));
        end
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaObjectValue) then
        begin
          IterResult := FRegisters[C].ObjectValue;
          NextMethod := IterResult.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
            ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
              SSuggestAsyncIteratorProtocol);

          CallArgs := AcquireArguments;
          try
            IterResult := InvokeCallable(NextMethod, CallArgs, IterResult);
          finally
            ReleaseArguments(CallArgs);
          end;
          SetRegister(A, IterResult);
        end
        else
          SetRegister(A, CreateIteratorResult(
            TGocciaUndefinedLiteralValue.UndefinedValue, True));
      end;

      OP_ITER_UNPACK:
      begin
        IterResult := GetRegister(C);
        if IterResult.IsPrimitive then
          ThrowTypeError(Format(SErrorIteratorResultNotObject,
            [IterResult.ToStringLiteral.Value]), SSuggestIteratorResultObject);

        DoneValue := IterResult.GetProperty(PROP_DONE);
        if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(True);
        end
        else
        begin
          IteratorElementValue := IterResult.GetProperty(PROP_VALUE);
          if not Assigned(IteratorElementValue) then
            IteratorElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          FRegisters[A] := VMValueToRegisterFast(IteratorElementValue);
          FRegisters[B] := RegisterBoolean(False);
        end;
      end;

      OP_SET_FUNCTION_NAME:
        SetFunctionNameFromKey(GetRegister(A), GetRegister(B), C);

      OP_ITER_CLOSE:
        if FRegisters[A].Kind = grkObject then
        begin
          if C = ITER_CLOSE_PRESERVE_UNLESS_GENERATOR_RETURN then
          begin
            if (FRegisters[B].Kind = grkObject) and
               Assigned(GActiveBytecodeGenerator) and
               Assigned(GActiveBytecodeGenerator.FReturnSentinel) and
               (FRegisters[B].ObjectValue =
                GActiveBytecodeGenerator.FReturnSentinel) then
              CloseRawIterator(FRegisters[A].ObjectValue)
            else
              CloseRawIteratorPreservingError(FRegisters[A].ObjectValue);
          end
          else if C = ITER_CLOSE_PRESERVE_ERROR then
            CloseRawIteratorPreservingError(FRegisters[A].ObjectValue, B <> 0)
          else if B <> 0 then
            CloseRawAsyncIterator(FRegisters[A].ObjectValue)
          else
            CloseRawIterator(FRegisters[A].ObjectValue);
        end;

      OP_AWAIT:
      begin
        if Assigned(FCurrentAsyncPromise) and Assigned(Template) and
           Template.IsAsync then
        begin
          if Template.IsGenerator and Assigned(GActiveBytecodeGenerator) then
            AwaitContinuation := GActiveBytecodeGenerator
          else if not Template.IsGenerator then
            AwaitContinuation := TGocciaBytecodeGeneratorObjectValue.CreateRegisters(
              Self, FCurrentClosure, GetLocalRegister(0),
              CurrentArgumentsSnapshot, False)
          else
            AwaitContinuation := nil;

          if Assigned(AwaitContinuation) then
          begin
            AwaitContinuation.CaptureContinuation(Frame, SavedHandlerCount,
              PrevCovLine, A, Frame.IP);
            AwaitContinuation.FState := bgsSuspendedYield;
            AwaitPromise := PromiseResolveIntrinsic(GetRegister(B));
            AwaitPromise.InvokeThen(
              TGocciaVMAsyncAwaitContinuationValue.Create(Self,
                AwaitContinuation, FCurrentAsyncPromise, bgrkNext,
                Template.IsGenerator),
              TGocciaVMAsyncAwaitContinuationValue.Create(Self,
                AwaitContinuation, FCurrentAsyncPromise, bgrkThrow,
                Template.IsGenerator));
            raise EGocciaBytecodeAsyncSuspend.Create('');
          end;
        end;
        SetRegister(A, AwaitValue(GetRegister(B)));
      end;

      OP_YIELD:
      begin
        if Assigned(GActiveBytecodeGenerator) then
        begin
          if (C and 1) <> 0 then
            GActiveBytecodeGenerator.HandleYieldDelegate(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              InstructionStartIP)
          else
            GActiveBytecodeGenerator.HandleYield(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              Frame.IP);
        end
        else if A <> B then
          FRegisters[B] := FRegisters[A];
      end;

      OP_SETUP_AUTO_ACCESSOR_CONST:
        SetupAutoAccessorValue(Template.GetConstantUnchecked(C).StringValue,
          B, RegisterToValue(FRegisters[A]));

      OP_SETUP_AUTO_ACCESSOR_DYNAMIC:
        SetupAutoAccessorValueByKey(RegisterToValue(FRegisters[A]),
          Template.GetConstantUnchecked(C).StringValue, B);

      OP_BEGIN_DECORATORS:
        BeginDecorators(RegisterToValue(FRegisters[A]), RegisterToValue(FRegisters[A + 1]));

      OP_APPLY_ELEMENT_DECORATOR_CONST:
        if B <> 0 then
          ApplyElementDecorator(RegisterToValue(FRegisters[A]),
            Template.GetConstantUnchecked(C).StringValue,
            RegisterToValue(FRegisters[B]))
        else
          ApplyElementDecorator(RegisterToValue(FRegisters[A]),
            Template.GetConstantUnchecked(C).StringValue);

      OP_APPLY_CLASS_DECORATOR:
        ApplyClassDecorator(RegisterToValue(FRegisters[A]));

      OP_FINISH_DECORATORS:
        SetRegister(A, FinishDecorators(RegisterToValue(FRegisters[A])));

      OP_GET_GLOBAL:
      begin
        if Assigned(FCurrentDynamicVarScope) or not Assigned(FGlobalScope) then
        begin
          GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
          if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
            FRegisters[A] := VMValueToRegisterFast(
              FCurrentDynamicVarScope.GetValue(GlobalName))
          else if Assigned(FGlobalScope) and
                  FGlobalScope.TryGetBindingValue(GlobalName, GlobalBindingValue) then
            FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
          else
            FRegisters[A] := RegisterUndefined;
        end
        else
        begin
          // Per-site inline cache keyed by the name-constant index.  It serves
          // either an own lexical-map entry or an ordinary global object's own
          // plain-data entry.  Both modes re-read the live value by a
          // version-validated entry index; exotic objects, accessors, lazy
          // descriptors, and dynamic scopes remain on the named lookup path.
          GlobalReadCache := Template.GlobalReadCacheSlot(DecodeBx(Instruction));
          if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (GlobalReadCache^.ObjectValue = nil) and
             FGlobalScope.TryGetLexicalValueAt(GlobalReadCache^.EntryIndex,
               GlobalReadCache^.Version, GlobalBindingValue) then
            FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
          else if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) and
             (GlobalReadCache^.ObjectValue =
               Pointer(FGlobalScope.ThisValue)) and
             VMGlobalObjectBindingCacheStillPrecedes(FGlobalScope,
               GlobalReadCache) and
             VMTryGetCachedGlobalOwnDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue),
               GlobalReadCache^.EntryIndex, GlobalReadCache^.Version,
               GlobalBindingValue) then
            FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
          else
          begin
            GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
            if Assigned(GlobalReadCache) then
            begin
              GlobalReadCache^.Scope := nil;
              GlobalReadCache^.ObjectValue := nil;
              GlobalReadCache^.ObjectBindingKind :=
                GLOBAL_READ_OBJECT_BINDING_NONE;
              if FGlobalScope.TryGetBindingValueFillCache(GlobalName,
                GlobalReadCache^.EntryIndex, GlobalReadCache^.Version,
                GlobalBindingValue) then
              begin
                GlobalBindingEntryIndex := GlobalReadCache^.EntryIndex;
                GlobalBindingVersion := GlobalReadCache^.Version;
                if (FGlobalScope.ThisValue is TGocciaObjectValue) and
                   ((not FGlobalScope.ContainsOwnLexicalBinding(GlobalName) and
                     FGlobalScope.ContainsOwnVarBinding(GlobalName)) or
                    (FGlobalScope.IsBuiltInBinding(GlobalName) and
                     FGlobalScope.IsGlobalObjectBackedBinding(GlobalName))) and
                   VMTryGetGlobalOwnDataPropertyFillCache(
                     TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
                     GlobalReadCache^.EntryIndex,
                     GlobalReadCache^.Version) then
                begin
                  GlobalReadCache^.Scope := Pointer(FGlobalScope);
                  GlobalReadCache^.ObjectValue :=
                    Pointer(FGlobalScope.ThisValue);
                  if FGlobalScope.ContainsOwnVarBinding(GlobalName) then
                    GlobalReadCache^.ObjectBindingKind :=
                      GLOBAL_READ_OBJECT_BINDING_VAR
                  else
                  begin
                    GlobalReadCache^.ObjectBindingKind :=
                      GLOBAL_READ_OBJECT_BINDING_BUILTIN;
                    GlobalReadCache^.BindingEntryIndex :=
                      GlobalBindingEntryIndex;
                    GlobalReadCache^.BindingVersion :=
                      GlobalBindingVersion;
                  end;
                end
                else if GlobalReadCache^.EntryIndex >= 0 then
                  GlobalReadCache^.Scope := Pointer(FGlobalScope);
                FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue);
              end
              else
                FRegisters[A] := RegisterUndefined;
            end
            else if FGlobalScope.TryGetBindingValue(GlobalName,
              GlobalBindingValue) then
              FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
            else
              FRegisters[A] := RegisterUndefined;
          end;
        end;
      end;

      OP_SET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
          FCurrentDynamicVarScope.AssignBinding(GlobalName,
            RegisterToValue(FRegisters[A]))
        else if Assigned(FGlobalScope) then
        begin
          if not FGlobalScope.TryAssignExistingBinding(GlobalName,
            RegisterToValue(FRegisters[A])) then
          begin
            if ((GlobalName = PROP_GOCCIA) or (GlobalName = PROP_GLOBAL_THIS)) and
               (FGlobalScope.ThisValue is TGocciaObjectValue) and
               TGocciaObjectValue(FGlobalScope.ThisValue).HasProperty(GlobalName) then
            begin
              CurrentInstructionDebugLocation(DebugLine, DebugColumn);
              raise TGocciaTypeError.Create(
                Format(SErrorAssignToConstant, [GlobalName]),
                DebugLine, DebugColumn,
                '', nil, SSuggestUseLetNotConst);
            end;
            ThrowReferenceError(GlobalName + ' is not defined');
          end;
        end;
      end;

      OP_SET_GLOBAL_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
          FCurrentDynamicVarScope.AssignBinding(GlobalName,
            RegisterToValue(FRegisters[A]), 0, 0, True)
        else if Assigned(FGlobalScope) then
        begin
          GlobalBindingValue := RegisterToValue(FRegisters[A]);
          if FGlobalScope.ContainsOwnVarBinding(GlobalName) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) and
             VMTrySetOwnWritableDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
               GlobalBindingValue) then
            Continue;
          if (not FGlobalScope.TryAssignExistingBinding(GlobalName,
            GlobalBindingValue, True)) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) then
          begin
            if ((GlobalName = PROP_GOCCIA) or (GlobalName = PROP_GLOBAL_THIS)) and
               TGocciaObjectValue(FGlobalScope.ThisValue).HasProperty(GlobalName) then
            begin
              CurrentInstructionDebugLocation(DebugLine, DebugColumn);
              raise TGocciaTypeError.Create(
                Format(SErrorAssignToConstant, [GlobalName]),
                DebugLine, DebugColumn,
                '', nil, SSuggestUseLetNotConst);
            end;
            TGocciaObjectValue(FGlobalScope.ThisValue).AssignPropertyWithReceiver(
              GlobalName, GlobalBindingValue, FGlobalScope.ThisValue);
          end;
        end;
      end;

      OP_HAS_GLOBAL:
      begin
        if not Assigned(FCurrentDynamicVarScope) and Assigned(FGlobalScope) then
        begin
          GlobalReadCache := Template.GlobalReadCacheSlot(
            DecodeBx(Instruction));
          if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (((GlobalReadCache^.ObjectValue = nil) and
               FGlobalScope.HasLexicalBindingAt(
                 GlobalReadCache^.EntryIndex, GlobalReadCache^.Version)) or
              ((FGlobalScope.ThisValue is TGocciaObjectValue) and
               (GlobalReadCache^.ObjectValue =
                 Pointer(FGlobalScope.ThisValue)) and
               VMGlobalObjectBindingCacheStillPrecedes(FGlobalScope,
                 GlobalReadCache) and
               VMTryGetCachedGlobalOwnDataProperty(
                 TGocciaObjectValue(FGlobalScope.ThisValue),
                 GlobalReadCache^.EntryIndex, GlobalReadCache^.Version,
                 GlobalBindingValue))) then
          begin
            FRegisters[A] := RegisterBoolean(True);
            Continue;
          end;
        end;

        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        FRegisters[A] := RegisterBoolean(
          HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) or
          (Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName)));
      end;

      OP_DELETE_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
          FRegisters[A] := RegisterBoolean(
            FCurrentDynamicVarScope.DeleteBinding(GlobalName))
        else if Assigned(FGlobalScope) then
          FRegisters[A] := RegisterBoolean(FGlobalScope.DeleteBinding(GlobalName))
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_IMPORT:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          SetRegister(A, ImportModuleValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GlobalName));
        end;

      OP_IMPORT_DEFER:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          SetRegister(A, ImportDeferredModuleNamespaceValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GlobalName));
        end;

      OP_IMPORT_SOURCE:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          SetRegister(A, ImportModuleSourceValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GlobalName));
        end;

      // ES2026 §16.2.1.7.3.1 InitializeEnvironment: resolving a named import to
      // a missing or ambiguous export is a SyntaxError. The module namespace is
      // already loaded into register A by a preceding OP_IMPORT; reject names it
      // cannot resolve before reading the binding value so bytecode matches the
      // interpreter even on the entry path, which skips link-time validation.
      // CanResolveExport (not HasExport) walks star/forwarding chains, matching
      // the loader's ValidateStaticNamedImports and re-export validators.
      OP_VALIDATE_IMPORT_BINDING:
        begin
          GlobalName := Template.GetConstantUnchecked(
            DecodeBx(Instruction)).StringValue;
          if (FRegisters[A].Kind <> grkObject) or
             not (FRegisters[A].ObjectValue is
               TGocciaModuleNamespaceObject) or
             not TGocciaModuleNamespaceObject(FRegisters[A].ObjectValue)
               .CanResolveExport(GlobalName) then
          begin
            if (FRegisters[A].Kind = grkObject) and
               (FRegisters[A].ObjectValue is
                 TGocciaModuleNamespaceObject) then
              SpecifierString :=
                TGocciaModuleNamespaceObject(FRegisters[A].ObjectValue)
                  .Module.Path
            else
              SpecifierString := '';
            ThrowSyntaxError(Format('Module "%s" has no export named "%s"',
              [SpecifierString, GlobalName]));
          end;
        end;

      OP_GET_IMPORT_BINDING:
        begin
          GlobalName := Template.GetConstantUnchecked(
            DecodeBx(Instruction)).StringValue;
          if (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaModuleNamespaceObject) then
          begin
            if not TGocciaModuleNamespaceObject(
               FRegisters[A].ObjectValue).TryGetExportValue(
               GlobalName, GlobalBindingValue) then
              ThrowSyntaxError(Format('Module "%s" has no export named "%s"',
                [TGocciaModuleNamespaceObject(FRegisters[A].ObjectValue)
                   .Module.Path, GlobalName]));
            SetRegister(A, GlobalBindingValue);
          end
          else
            SetRegister(A, GetPropertyValue(GetRegister(A), GlobalName));
        end;

      OP_EXPORT:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          ExportBindingValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GetRegister(A), GlobalName);
        end;

      // ES2026 §13.3.12.1 — import.meta binds lexically to the defining module
      OP_IMPORT_META:
        if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
          SetRegister(A, GetOrCreateImportMeta(Template.DebugInfo.SourceFile,
            FResolveModuleURL))
        else
          SetRegister(A, GetOrCreateImportMeta(FCurrentModuleSourcePath,
            FResolveModuleURL));

      // ES2026 §13.3.12.1 — new.target reads the current frame's newTarget
      OP_NEW_TARGET:
        if Assigned(FCurrentNewTarget) then
          SetRegister(A, FCurrentNewTarget)
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      // ES2026 §13.3.10.1 ImportCall — import(specifier)
      OP_DYNAMIC_IMPORT:
      begin
        DynImportPromise := TGocciaPromiseValue.Create;
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.AddTempRoot(DynImportPromise);
        try
          try
            if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
              GlobalName := Template.DebugInfo.SourceFile
            else
              GlobalName := FCurrentModuleSourcePath;

            SpecifierString := ToPrimitive(RegisterToValue(FRegisters[B]),
              tphString).ToStringLiteral.Value;
            case C of
              Ord(icpEvaluation):
                if (TGocciaMicrotaskQueue.Instance <> nil) then
                begin
                  DynImportTask.Handler := TGocciaVMDynamicImportStartValue.Create(
                    Self, DynImportPromise, SpecifierString, GlobalName);
                  DynImportTask.Value :=
                    TGocciaUndefinedLiteralValue.UndefinedValue;
                  DynImportTask.ResultPromise := nil;
                  DynImportTask.ReactionType := prtFulfill;
                  TGocciaMicrotaskQueue.Instance.Enqueue(DynImportTask);
                end
                else
                  ResolveDynamicImportPromise(DynImportPromise,
                    SpecifierString, GlobalName);
              Ord(icpSource):
                DynImportPromise.Resolve(ImportModuleSourceValue(
                  SpecifierString, GlobalName));
              Ord(icpDefer):
                DynImportPromise.Resolve(ImportDeferredModuleNamespaceValue(
                  SpecifierString, GlobalName));
            else
              raise Exception.CreateFmt(
                'Unsupported dynamic import phase: %d', [C]);
            end;
          except
            on E: EGocciaBytecodeThrow do
              DynImportPromise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              DynImportPromise.Reject(E.Value);
            on E: TGocciaSyntaxError do
              DynImportPromise.Reject(
                CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
            on E: TGocciaTypeError do
              DynImportPromise.Reject(
                CreateErrorObject(TYPE_ERROR_NAME, E.Message));
            on E: TGocciaReferenceError do
              DynImportPromise.Reject(
                CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
            on E: TGocciaTimeoutError do
              raise;
            on E: TGocciaInstructionLimitError do
              raise;
            on E: EGocciaCapabilityAuditDeliveryError do
              raise;
            on E: Exception do
              DynImportPromise.Reject(
                CreateErrorObject(ERROR_NAME, E.Message));
          end;
          SetRegister(A, DynImportPromise);
        finally
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.RemoveTempRoot(DynImportPromise);
        end;
      end;

      // ES2026 §13.3.10.1 ImportCall — import(specifier, options)
      OP_DYNAMIC_IMPORT_OPTIONS,
      OP_DYNAMIC_IMPORT_SOURCE_OPTIONS,
      OP_DYNAMIC_IMPORT_DEFER_OPTIONS:
      begin
        DynImportPromise := TGocciaPromiseValue.Create;
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.AddTempRoot(DynImportPromise);
        try
          try
            if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
              GlobalName := Template.DebugInfo.SourceFile
            else
              GlobalName := FCurrentModuleSourcePath;

            SpecifierString := ToPrimitive(RegisterToValue(FRegisters[B]),
              tphString).ToStringLiteral.Value;
            AttributeType := DynamicImportAttributeType(
              RegisterToValue(FRegisters[C]));
            SpecifierString := EncodeImportSpecifierAttribute(
              SpecifierString, AttributeType);
            case TGocciaOpCode(Op) of
              OP_DYNAMIC_IMPORT_OPTIONS:
                if (TGocciaMicrotaskQueue.Instance <> nil) then
                begin
                  DynImportTask.Handler := TGocciaVMDynamicImportStartValue.Create(
                    Self, DynImportPromise, SpecifierString, GlobalName);
                  DynImportTask.Value :=
                    TGocciaUndefinedLiteralValue.UndefinedValue;
                  DynImportTask.ResultPromise := nil;
                  DynImportTask.ReactionType := prtFulfill;
                  TGocciaMicrotaskQueue.Instance.Enqueue(DynImportTask);
                end
                else
                  ResolveDynamicImportPromise(DynImportPromise, SpecifierString,
                    GlobalName);
              OP_DYNAMIC_IMPORT_SOURCE_OPTIONS:
                DynImportPromise.Resolve(ImportModuleSourceValue(
                  SpecifierString, GlobalName));
              OP_DYNAMIC_IMPORT_DEFER_OPTIONS:
                DynImportPromise.Resolve(ImportDeferredModuleNamespaceValue(
                  SpecifierString, GlobalName));
            end;
          except
            on E: EGocciaBytecodeThrow do
              DynImportPromise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              DynImportPromise.Reject(E.Value);
            on E: TGocciaSyntaxError do
              DynImportPromise.Reject(
                CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
            on E: TGocciaTypeError do
              DynImportPromise.Reject(
                CreateErrorObject(TYPE_ERROR_NAME, E.Message));
            on E: TGocciaReferenceError do
              DynImportPromise.Reject(
                CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
            on E: TGocciaTimeoutError do
              raise;
            on E: TGocciaInstructionLimitError do
              raise;
            on E: EGocciaCapabilityAuditDeliveryError do
              raise;
            on E: Exception do
              DynImportPromise.Reject(
                CreateErrorObject(ERROR_NAME, E.Message));
          end;
          SetRegister(A, DynImportPromise);
        finally
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.RemoveTempRoot(DynImportPromise);
        end;
      end;

      // TC39 Explicit Resource Management: OP_USING_INIT
      // A=dest (dispose method), B=value, C=flags (0=sync, 1=async)
      // Validates value has [Symbol.dispose]/[Symbol.asyncDispose], stores method in A.
      // For null/undefined, stores null. Throws TypeError if not disposable.
      OP_USING_INIT:
      begin
        LeftValue := RegisterToValue(FRegisters[B]);
        if (LeftValue is TGocciaUndefinedLiteralValue) or
           (LeftValue is TGocciaNullLiteralValue) then
          FRegisters[A] := RegisterNull
        else
        begin
          if C = 1 then
          begin
            RightValue := nil;
            if LeftValue is TGocciaObjectValue then
            begin
              RightValue := TGocciaObjectValue(LeftValue).GetSymbolProperty(
                TGocciaSymbolValue.WellKnownAsyncDispose);
              if Assigned(RightValue) and
                 not (RightValue is TGocciaUndefinedLiteralValue) and
                 not (RightValue is TGocciaNullLiteralValue) then
              begin
                if not RightValue.IsCallable then
                  RightValue := GetDisposeMethod(LeftValue, dhAsyncDispose)
                else
                  RightValue := TGocciaVMAsyncDisposeMethodValue.Create(
                    RightValue);
              end
              else
              begin
                RightValue := TGocciaObjectValue(LeftValue).GetSymbolProperty(
                  TGocciaSymbolValue.WellKnownDispose);
                if Assigned(RightValue) and
                   not (RightValue is TGocciaUndefinedLiteralValue) and
                   not (RightValue is TGocciaNullLiteralValue) then
                begin
                  if not RightValue.IsCallable then
                    RightValue := GetDisposeMethod(LeftValue, dhAsyncDispose)
                  else
                    RightValue := TGocciaVMSyncDisposeFallbackValue.Create(
                      RightValue);
                end
                else
                  RightValue := nil;
              end;
            end;
          end
          else
            RightValue := GetDisposeMethod(LeftValue, dhSyncDispose);
          if not Assigned(RightValue) then
          begin
            if C = 1 then
              raise EGocciaBytecodeThrow.Create(
                CreateErrorObject(TYPE_ERROR_NAME,
                  'Value is not disposable (missing [Symbol.asyncDispose] and [Symbol.dispose])'))
            else
              raise EGocciaBytecodeThrow.Create(
                CreateErrorObject(TYPE_ERROR_NAME,
                  'Value is not disposable (missing [Symbol.dispose])'));
          end;
          SetRegister(A, RightValue);
        end;
      end;

      // TC39 Explicit Resource Management: OP_USING_DISPOSE
      // A=errorAccum, B=disposeMethod, C=resource
      // Calls disposeMethod.call(resource). On error, wraps with SuppressedError
      // if errorAccum already holds an error.
      // TC39 Explicit Resource Management: OP_USING_DISPOSE
      // A=errorAccum, B=disposeMethod (overwritten with call result), C=resource
      // Calls disposeMethod.call(resource). Stores result in B for OP_AWAIT.
      // On error, wraps with SuppressedError in A.
      OP_USING_DISPOSE:
      begin
        LeftValue := RegisterToValue(FRegisters[B]); // dispose method
        if Assigned(LeftValue) and not (LeftValue is TGocciaNullLiteralValue) and
           not (LeftValue is TGocciaUndefinedLiteralValue) and
           LeftValue.IsCallable then
        begin
          try
            // Clear B before the call so that if it throws, the follow-up
            // OP_AWAIT sees null instead of the stale dispose function.
            FRegisters[B] := RegisterNull;
            RightValue := TGocciaFunctionBase(LeftValue).CallNoArgs(
              RegisterToValue(FRegisters[C]));
            // Store result in B so a follow-up OP_AWAIT can await it
            if Assigned(RightValue) then
              SetRegister(B, RightValue);
          except
            on E: EGocciaBytecodeThrow do
            begin
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 (RightValue <> TGocciaHoleValue.HoleValue) then
                SetRegister(A, CreateSuppressedErrorObject(E.ThrownValue, RightValue))
              else
                SetRegister(A, E.ThrownValue);
            end;
            on E: TGocciaThrowValue do
            begin
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 (RightValue <> TGocciaHoleValue.HoleValue) then
                SetRegister(A, CreateSuppressedErrorObject(E.Value, RightValue))
              else
                SetRegister(A, E.Value);
            end;
            on E: TGocciaTimeoutError do
              raise;
            on E: TGocciaInstructionLimitError do
              raise;
            on E: EGocciaCapabilityAuditDeliveryError do
              raise;
            on E: Exception do
            begin
              // Preserve typed error names for native Goccia exceptions
              if E is TGocciaTypeError then
                LeftValue := CreateErrorObject(TYPE_ERROR_NAME, E.Message)
              else if E is TGocciaReferenceError then
                LeftValue := CreateErrorObject(REFERENCE_ERROR_NAME, E.Message)
              else if E is TGocciaSyntaxError then
                LeftValue := CreateErrorObject(SYNTAX_ERROR_NAME, E.Message)
              else
                LeftValue := CreateErrorObject(ERROR_NAME, E.Message);
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 (RightValue <> TGocciaHoleValue.HoleValue) then
                SetRegister(A, CreateSuppressedErrorObject(LeftValue, RightValue))
              else
                SetRegister(A, LeftValue);
            end;
          end;
        end;
      end;

      OP_THROW: raise EGocciaBytecodeThrow.Create(GetRegister(A));

      OP_NOT:
        FRegisters[A] := RegisterBoolean(not RegisterToBoolean(FRegisters[B]));

      OP_TO_BOOL:
        FRegisters[A] := RegisterBoolean(RegisterToBoolean(FRegisters[B]));

      OP_DEFINE_ACCESSOR_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(C).StringValue;
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if IsBytecodePrivateKey(GlobalName) then
            DeclareBytecodePrivateNameForClass(
              RegisterToValue(FRegisters[A]), GlobalName, True);
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]))
          else
            DefineStaticGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]));
        end
        else
        begin
          if IsBytecodePrivateKey(GlobalName) then
            DeclareBytecodePrivateNameForClass(
              RegisterToValue(FRegisters[A]), GlobalName);
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]))
          else
            DefineGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]));
        end;
      end;

      OP_DEFINE_ACCESSOR_DYNAMIC:
      begin
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]))
          else
            DefineStaticGetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]));
        end
        else
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]))
          else
            DefineGetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]));
        end;
      end;

      OP_COLLECTION_OP:
      begin
        case B of
          COLLECTION_OP_SPREAD_OBJECT:
            if (FRegisters[A].Kind = grkObject) and
               (FRegisters[A].ObjectValue is TGocciaObjectValue) then
              SpreadObjectIntoValue(TGocciaObjectValue(FRegisters[A].ObjectValue),
                RegisterToValue(FRegisters[C]));

          COLLECTION_OP_OBJECT_REST:
            begin
              if (A + 1 < FRegisterCount) and
                 (FRegisters[A + 1].Kind = grkObject) and
                 (FRegisters[A + 1].ObjectValue is TGocciaArrayValue) then
                SetRegister(A, ObjectRestValue(RegisterToValue(FRegisters[C]),
                  TGocciaArrayValue(FRegisters[A + 1].ObjectValue)))
              else
                SetRegister(A, ObjectRestValue(RegisterToValue(FRegisters[C]), nil));
            end;

          COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY:
            begin
              DoneValue := IterableToArray(RegisterToValue(FRegisters[C]));
              if (FRegisters[A].Kind = grkObject) and
                 (FRegisters[A].ObjectValue is TGocciaArrayValue) and
                 (DoneValue is TGocciaArrayValue) then
	                for I := 0 to TGocciaArrayValue(DoneValue).Elements.Count - 1 do
	                  TGocciaArrayValue(FRegisters[A].ObjectValue).Elements.Add(
	                    TGocciaArrayValue(DoneValue).GetProperty(IntToStr(I)));
            end;

          COLLECTION_OP_TRY_ITERABLE_TO_ARRAY:
            begin
              if TryIterableToArray(RegisterToValue(FRegisters[C]), SpreadArray) then
                SetRegister(A, SpreadArray)
              else
                FRegisters[A] := RegisterUndefined;
            end;

        else
          raise Exception.CreateFmt('Unsupported collection helper mode: %d', [B]);
        end;
      end;

      OP_VALIDATE_VALUE:
      begin
        case B of
          VALIDATE_OP_REQUIRE_OBJECT:
            begin
              if FRegisters[A].Kind in [grkNull, grkUndefined] then
                ThrowTypeError(Format(SErrorCannotDestructureNotObject, [RegisterToValue(FRegisters[A]).ToStringLiteral.Value]),
                  SSuggestDestructureRequiresObject);
            end;

          VALIDATE_OP_REQUIRE_ITERABLE:
            // Operand C is the iteration bound emitted by the compiler
            // for array destructuring (see ITERABLE_LIMIT_UNBOUNDED in
            // Goccia.Bytecode):
            //   0..254  = exact element count to consume; 0 means
            //             "consume zero elements" for `const [] = iter`
            //             then close;
            //   255     = unbounded (rest pattern present or pattern
            //             length exceeds the encoding range).
            // IterableToArray's ALimit uses -1 = unbounded, 0+ = exact
            // count, so translate the sentinel here.
            if C = ITERABLE_LIMIT_UNBOUNDED then
              SetRegister(A, IterableToArray(RegisterToValue(FRegisters[A]),
                False, -1))
            else
              SetRegister(A, IterableToArray(RegisterToValue(FRegisters[A]),
                False, C));
        else
          raise Exception.CreateFmt('Unsupported validation mode: %d', [B]);
        end;
      end;

      OP_THROW_TYPE_ERROR_CONST:
        ThrowTypeError(Template.GetConstantUnchecked(C).StringValue);

      OP_THROW_TYPE_ERROR_CONST_LONG:
        ThrowTypeError(
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue);

      OP_DEFINE_GLOBAL_VAR_DECL_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.DefineVariableBinding(GlobalName,
            TGocciaUndefinedLiteralValue.UndefinedValue, False);
      end;

      OP_DEFINE_GLOBAL_VAR_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        GlobalBindingValue := GetRegister(A);
        // Top-level var names are instantiated before body execution.
        // Initializers inside loops can therefore use ordinary assignment
        // resolution instead of repeating CreateGlobalVarBinding each time.
        if Assigned(FGlobalScope) and
           FGlobalScope.ContainsOwnVarBinding(GlobalName) then
        begin
          if (FGlobalScope.ThisValue is TGocciaObjectValue) and
             VMTrySetOwnWritableDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
               GlobalBindingValue) then
            Continue
          else if (FGlobalScope.ThisValue is TGocciaObjectValue) and
             TGocciaObjectValue(FGlobalScope.ThisValue).HasOwnProperty(
               GlobalName) then
          begin
            if Template.StrictCode then
              TGocciaObjectValue(FGlobalScope.ThisValue).AssignProperty(
                GlobalName, GlobalBindingValue)
            else
              TGocciaObjectValue(FGlobalScope.ThisValue).
                AssignPropertyWithReceiver(GlobalName, GlobalBindingValue,
                  FGlobalScope.ThisValue);
          end
          else
            FGlobalScope.AssignBinding(GlobalName, GlobalBindingValue, 0, 0,
              not Template.StrictCode);
        end
        else
          DefineGlobalBinding(GlobalName, GlobalBindingValue, dtVar,
            not Template.StrictCode);
      end;

      OP_DEFINE_GLOBAL_LET_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        DefineGlobalBinding(GlobalName, GetRegister(A), dtLet);
      end;

      OP_DEFINE_GLOBAL_CONST_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        DefineGlobalBinding(GlobalName, GetRegister(A), dtConst);
      end;

      OP_DEFINE_GLOBAL_FUNCTION_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.CreateGlobalFunctionBinding(GlobalName, GetRegister(A),
            False);
      end;

      OP_PREDECLARE_GLOBAL_LET_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.PredeclareLexicalBinding(GlobalName, dtLet);
      end;

      OP_PREDECLARE_GLOBAL_CONST_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.PredeclareLexicalBinding(GlobalName, dtConst);
      end;

      OP_FINALIZE_ENUM:
        SetRegister(A, FinalizeEnumValue(GetRegister(A),
          Template.GetConstantUnchecked(C).StringValue));

      OP_SUPER_GET_CONST:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValue(GetRegister(A + 1),
            GetRegister(A - 1), Template.GetConstantUnchecked(C).StringValue,
            B <> 0))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_GET:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValueByKey(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(C), B <> 0))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_SET:
        if A > 0 then
          SetSuperPropertyValueByKey(GetRegister(A + 1), GetRegister(A - 1),
            GetRegister(B), GetRegister(C))
        else
          ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
            SSuggestCheckNullBeforeAccess);

      OP_SUPER_BASE:
        SetRegister(A, ResolveSuperPropertyBaseValue(GetRegister(B),
          GetRegister(C)));

      OP_SUPER_GET_BASE:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValueFromBase(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(C)))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_SET_BASE:
        if A > 0 then
          SetSuperPropertyBaseValueByKey(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(B), GetRegister(C))
        else
          ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
            SSuggestCheckNullBeforeAccess);

      OP_RETURN:
      begin
        ReturnValue := FRegisters[A];
        if Assigned(GActiveBytecodeGenerator) and
           (GActiveBytecodeGenerator.FClosure = AClosure) then
          GActiveBytecodeGenerator.FReturnRequiresAwait := B <> 0;
        if FClosedNumericFrameStackCount >
           InitialClosedNumericFrameCount then
        begin
          ResultReg := PopClosedNumericFrame(Frame, Template, PrevCovLine,
            ProfileEntryTimestamp);
          SetRegisterRaw(ResultReg, ReturnValue);
          Continue;
        end;
        // Outermost frame: let the finally block handle teardown
        if FFrameStackCount <= InitialFrameStackCount then
        begin
          FLastClosureThisValue := GetLocalRegister(0);
          Exit(ReturnValue);
        end;
        // Intermediate trampoline frame: tear down and pop to parent
        TeardownCurrentFrame(Template, ProfileEntryTimestamp,
          FFrameStack[FFrameStackCount - 1].HandlerCount);
        ResultReg := PopFrame(Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        SetRegisterRaw(ResultReg, ReturnValue);
        Continue;
      end;
        else
          raise Exception.CreateFmt('Unsupported Goccia VM opcode in minimal executor: %d', [Op]);
        end;
        if FMemoryPressureCheckCountdown = 0 then
        begin
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.CollectForMemoryPressure(nil);
          FMemoryPressureCheckCountdown := MEMORY_PRESSURE_CHECK_INTERVAL;
        end
        else
          Dec(FMemoryPressureCheckCountdown);
        end;
      except
        on E: EGocciaBytecodeThrow do
          HandleExceptionUnwind(E.ThrownValue,
            InitialFrameStackCount, InitialClosedNumericFrameCount,
            SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaThrowValue do
          HandleExceptionUnwind(E.Value,
            InitialFrameStackCount, InitialClosedNumericFrameCount,
            SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaTypeError do
          HandleExceptionUnwind(
            CreateErrorObject(TYPE_ERROR_NAME, E.Message),
            InitialFrameStackCount, InitialClosedNumericFrameCount,
            SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaReferenceError do
          HandleExceptionUnwind(
            CreateErrorObject(REFERENCE_ERROR_NAME, E.Message),
            InitialFrameStackCount, InitialClosedNumericFrameCount,
            SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaSyntaxError do
          HandleExceptionUnwind(
            CreateErrorObject(SYNTAX_ERROR_NAME, E.Message),
            InitialFrameStackCount, InitialClosedNumericFrameCount,
            SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        on E: TGocciaRuntimeError do
          HandleExceptionUnwind(
            CreateErrorObject(ERROR_NAME, E.Message),
            InitialFrameStackCount, InitialClosedNumericFrameCount,
            SavedHandlerCount,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
      end;
    end;
    Result := RegisterUndefined;
  finally
    Dec(FNativeExecutionDepth);
    try
      UnwindClosedNumericFrames(InitialClosedNumericFrameCount, Frame,
        Template, PrevCovLine, ProfileEntryTimestamp);
      // Unwind any remaining trampoline frames (exception escape path)
      while FFrameStackCount > InitialFrameStackCount do
      begin
        TeardownCurrentFrame(Template, ProfileEntryTimestamp,
          FFrameStack[FFrameStackCount - 1].HandlerCount);
        PopFrame(Frame, Template, PrevCovLine, ProfileEntryTimestamp);
      end;
      // Teardown the outermost frame
      TeardownCurrentFrame(Template, ProfileEntryTimestamp, SavedHandlerCount);
      // Restore the caller's state
      FCurrentClosure := SavedClosure;
      FCurrentNewTarget := SavedNewTarget;
      FGlobalScope := SavedGlobalScope;
      FCurrentDynamicVarScope := SavedDynamicVarScope;
      FCurrentExecutionContextPushed := SavedExecutionContextPushed;
      FArgumentBase := SavedArgumentBase;
      FArgCount := SavedArgCount;
      FArguments := @FArgumentStack[FArgumentBase];
      FRegisterBase := SavedRegisterBase;
      FRegisterCount := SavedRegisterCount;
      FRegisters := @FRegisterStack[FRegisterBase];
      FLocalCellBase := SavedLocalCellBase;
      FLocalCellCount := SavedLocalCellCount;
      FLocalCells := @FLocalCellStack[FLocalCellBase];
      if RealmSwitched then
        SetCurrentRealm(PreviousRealm);
    finally
      PopSavedStateRoot;
    end;
  end;
end;

function TGocciaVM.ExecuteClosure(const AClosure: TGocciaBytecodeClosure;
  const AThisValue: TGocciaValue; const AArguments: TGocciaArgumentsCollection;
  const APushExecutionContext: Boolean): TGocciaValue;
var
  RegisterArgs: TGocciaRegisterArray;
  I: Integer;
begin
  SetLength(RegisterArgs, AArguments.Length);
  for I := 0 to AArguments.Length - 1 do
    RegisterArgs[I] := VMValueToRegisterFast(AArguments.GetElement(I));
  Result := RegisterToValue(ExecuteClosureRegisters(AClosure,
    VMValueToRegisterFast(AThisValue), RegisterArgs, APushExecutionContext));
end;

function TGocciaVM.ExecuteModule(const AModule: TGocciaBytecodeModule): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  PreviousAsyncPromise: TGocciaPromiseValue;
  Promise: TGocciaPromiseValue;
  PromiseRoot: TGocciaTempRoot;
  TopClosure: TGocciaBytecodeClosure;
  SavedModuleSourcePath: string;
  SavedModuleExports: TGocciaValueMap;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create;
  TopClosure := TGocciaBytecodeClosure.Create(AModule.TopLevel);
  TopClosure.GlobalScope := FGlobalScope;
  SavedModuleSourcePath := FCurrentModuleSourcePath;
  SavedModuleExports := FCurrentModuleExports;
  FCurrentModuleSourcePath := AModule.SourcePath;
  FCurrentModuleExports := TGocciaValueMap.Create;
  if Assigned(FRealm) then
    TGocciaExecutionContextStack.Push(
      CreateExecutionContext(FRealm, FGlobalScope, AModule.SourcePath, AModule));
  try
    try
      if Assigned(AModule.TopLevel) and AModule.TopLevel.IsAsync then
      begin
        Promise := TGocciaPromiseValue.Create;
        InitializeTempRoot(PromiseRoot);
        AddTempRootIfNeeded(PromiseRoot, Promise);
        try
          try
            PreviousAsyncPromise := FCurrentAsyncPromise;
            FCurrentAsyncPromise := Promise;
            try
              try
                Promise.Resolve(ExecuteClosure(TopClosure,
                  TGocciaUndefinedLiteralValue.UndefinedValue, EmptyArgs, False));
              except
                on E: EGocciaBytecodeAsyncSuspend do
                begin
                end;
                on E: EGocciaBytecodeThrow do
                  Promise.Reject(E.ThrownValue);
                on E: TGocciaThrowValue do
                  Promise.Reject(E.Value);
              end;
            finally
              FCurrentAsyncPromise := PreviousAsyncPromise;
            end;
          except
            Promise.Free;
            raise;
          end;
        finally
          RemoveTempRootIfNeeded(PromiseRoot);
        end;
        Result := Promise;
      end
      else
        Result := ExecuteClosure(TopClosure,
          TGocciaUndefinedLiteralValue.UndefinedValue, EmptyArgs, False);
    finally
      if Assigned(FRealm) then
        TGocciaExecutionContextStack.Pop;
      FCurrentModuleExports.Free;
      FCurrentModuleExports := SavedModuleExports;
      FCurrentModuleSourcePath := SavedModuleSourcePath;
    end;
  finally
    TopClosure.Free;
    EmptyArgs.Free;
  end;
end;

function TGocciaVM.ExecuteFunction(const ATemplate: TGocciaFunctionTemplate): TGocciaValue;
var
  EmptyArgs: TGocciaArgumentsCollection;
  TopClosure: TGocciaBytecodeClosure;
  ExecutionSourcePath: string;
begin
  EmptyArgs := TGocciaArgumentsCollection.Create;
  TopClosure := TGocciaBytecodeClosure.Create(ATemplate);
  if Assigned(ATemplate.DebugInfo) and (ATemplate.DebugInfo.SourceFile <> '') then
    ExecutionSourcePath := ATemplate.DebugInfo.SourceFile
  else
    ExecutionSourcePath := FCurrentModuleSourcePath;
  if Assigned(FRealm) then
    TGocciaExecutionContextStack.Push(
      CreateExecutionContext(FRealm, FGlobalScope, ExecutionSourcePath,
        ATemplate));
  try
    Result := ExecuteClosure(TopClosure,
      TGocciaUndefinedLiteralValue.UndefinedValue, EmptyArgs, False);
  finally
    if Assigned(FRealm) then
      TGocciaExecutionContextStack.Pop;
    TopClosure.Free;
    EmptyArgs.Free;
  end;
end;

function RedirectBytecodeClassConstruct(
  const ATarget: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue;
  out AResult: TGocciaValue): Boolean;
var
  BytecodeFunction: TGocciaBytecodeFunctionValue;
  ClassValue: TGocciaVMClassValue;
begin
  Result := False;
  if not (ATarget is TGocciaBytecodeFunctionValue) then
    Exit;

  BytecodeFunction := TGocciaBytecodeFunctionValue(ATarget);
  if not (BytecodeFunction.FConstructClassValue is TGocciaVMClassValue) then
    Exit;

  ClassValue := TGocciaVMClassValue(BytecodeFunction.FConstructClassValue);
  if ClassValue.FConstructorValue <> ATarget then
    Exit;

  AResult := ClassValue.Instantiate(AArguments, ANewTarget);
  Result := True;
end;

initialization
  RegisterFunctionConstructRedirectHook(RedirectBytecodeClassConstruct);

end.
