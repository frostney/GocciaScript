unit Goccia.Values.ClassValue;

{$I Goccia.inc}

interface

uses
  Classes,

  OrderedStringMap,

  Goccia.Arguments.Collection,
  Goccia.CapabilityAudit,
  Goccia.Constants,
  Goccia.Realm,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  // Forward declaration
  TGocciaInstanceValue = class;

  TGocciaDecoratorFieldInitializerEntry = record
    Name: string;
    ComputedKey: TGocciaValue;
    Initializer: TGocciaValue;
    IsPrivate: Boolean;
    IsStatic: Boolean;
  end;

  TGocciaClassValue = class(TGocciaObjectValue)
  private
    FName: string;
    FSuperClass: TGocciaClassValue;
    FClassPrototype: TGocciaObjectValue;
    FStaticMethods: TGocciaValueMap;
    FPrivateStaticProperties: TGocciaValueMap;
    FPrivateStaticMethods: TGocciaValueMap;
    // A private accessor is any callable: a decorator may replace it with a
    // callable Proxy, which is not a TGocciaFunctionBase.
    FPrivateGetters: TOrderedStringMap<TGocciaValue>;
    FPrivateSetters: TOrderedStringMap<TGocciaValue>;
    FCreationRealm: TGocciaRealm;
    // Each declared private name, in declaration order, mapped to its
    // internal storage key ('' when it was declared without one). Native
    // strings: a TStringList converts every entry through the ANSI widestring
    // manager, which put iconv on every bytecode private access.
    FDeclaredPrivateNames: TStringStringMap;
    // The reverse of FDeclaredPrivateNames: each internal key mapped to the
    // name that first declared it. Created on the first keyed declaration.
    FDeclaredPrivateKeyNames: TStringStringMap;
    FNativeSuperConstructor: TGocciaObjectValue;
    FPrivateBrandToken: string;
    FMethodInitializers: array of TGocciaValue;
    FFieldInitializers: array of TGocciaValue;
    FDecoratorFieldInitializers: array of TGocciaDecoratorFieldInitializerEntry;
    FStaticDecoratorFieldInitializers: array of TGocciaDecoratorFieldInitializerEntry;
    // Set once the synthesized "name" or "length" has been deleted, and never
    // cleared. A property of that key defined later is an ordinary entry in
    // FProperties, listed where it was created (ES2026 §10.1.11.1
    // OrdinaryOwnPropertyKeys) rather than in the synthesized slot that
    // GetAllPropertyNames gives the class's original name and length.
    FNameDeleted: Boolean;
    FLengthDeleted: Boolean;
    // Advances whenever this class gains a private name, so a cache derived
    // from the private-name set can tell it is stale. Declared beside the two
    // flags above so that on 64-bit targets it occupies their alignment
    // padding: every class value the engine builds carries this field, and
    // `--max-memory` charges each one its InstanceSize, so a field placed
    // between two pointers would grow every class value by a full pointer
    // width for four bytes of state. On 32-bit targets the flags leave only
    // two bytes of padding, so the field costs four bytes there wherever it
    // goes; a 16-bit counter would fit but could wrap and revalidate a stale
    // cache.
    FPrivateNamesVersion: Cardinal;
    FSourceText: string;
    function GetStaticPropertyGetter(const AName: string): TGocciaFunctionBase; {$IFDEF FPC}inline;{$ENDIF}
    function GetStaticPropertySetter(const AName: string): TGocciaFunctionBase; {$IFDEF FPC}inline;{$ENDIF}
    function GetPrivatePropertyGetter(const AName: string): TGocciaValue;
    function GetPrivatePropertySetter(const AName: string): TGocciaValue;
    procedure MaterializeIntrinsicProperty(const AName: string);
  public
    class procedure SetDefaultPrototype(const AProto: TGocciaObjectValue); static;
    class procedure PatchDefaultPrototype(const AClassValue: TGocciaClassValue); static;
    constructor Create(const AName: string; const ASuperClass: TGocciaClassValue);
    destructor Destroy; override;
    function IsCallable: Boolean; override;
    function IsConstructable: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    procedure AddStaticGetter(const AName: string; const AGetter: TGocciaFunctionBase);
    procedure AddStaticSetter(const AName: string; const ASetter: TGocciaFunctionBase);
    procedure AddPrivateGetter(const AName: string; const AGetter: TGocciaValue);
    procedure AddPrivateSetter(const AName: string; const ASetter: TGocciaValue);
    function HasOwnPrivateGetter(const AName: string): Boolean;
    function HasOwnPrivateSetter(const AName: string): Boolean;
    function HasOwnPrivateStaticProperty(const AName: string): Boolean;
    function HasOwnPrivateStaticMethod(const AName: string): Boolean;
    function GetOwnPrivatePropertyGetter(
      const AName: string): TGocciaValue;
    function GetOwnPrivatePropertySetter(
      const AName: string): TGocciaValue;
    procedure AddPrivateStaticProperty(const AName: string; const AValue: TGocciaValue);
    procedure AddPrivateStaticMethod(const AName: string; const AValue: TGocciaValue);
    procedure DeclarePrivateName(const AName: string;
      const AInternalKey: string = '');
    function ResolveDeclaredPrivateKey(const AName: string;
      out AInternalKey: string): Boolean;
    // The name AInternalKey was first declared under by DeclarePrivateName.
    function TryGetDeclaredPrivateKeyName(const AInternalKey: string;
      out AName: string): Boolean;
    property PrivateNamesVersion: Cardinal read FPrivateNamesVersion;
    // Every private name this class declares, appended to
    // ANames[0..ACount-1] without duplicates. ANames may be longer than
    // ACount.
    procedure CollectOwnPrivateNames(var ANames: TArray<string>;
      var ACount: Integer);
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; virtual;
    function NativeInstanceDefaultPrototype: TGocciaObjectValue; virtual;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; virtual;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; virtual;
    function Instantiate(const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue = nil): TGocciaValue; virtual;
    // True when this class value carries its own [[Construct]] implementation:
    // a compiled class runs its constructor closure, and %TypedArray% and the
    // typed array constructors allocate an exotic receiver or refuse to be
    // constructed. ResolveImplicitConstructorChain stops at such a class
    // instead of collapsing its implicit constructor into the walk.
    function UsesOwnInstantiation: Boolean; virtual;
    function EstimatedInstancePropertyCapacity: Integer;
    // Approximates ES2026 §15.7.14 ClassDefinitionEvaluation step 19's
    // [[ConstructorKind]] = ~derived~, which decides when instance elements
    // are initialized: a ~base~ constructor does it before its body (§10.2.2
    // step 5b), a ~derived~ one when super() returns (§13.3.7.1 step 11).
    // Reports True when a resolved superclass or a linked native super
    // constructor is present. `class A extends null {}` reports True, which is
    // the spec-correct answer (§15.7.14 step 9a makes it ~derived~): class
    // definition links Function.prototype as the native super constructor for
    // it, and the construction paths recognise that sentinel and raise the
    // TypeError §10.2.2 requires when the implicit derived constructor tries
    // to call super().
    function HasDerivedConstructorKind: Boolean;
    // ECMAScript: number of expected constructor parameters before the first
    // default/rest. Built-in classes default to 0; a compiled class reports
    // its constructor's.
    function GetClassLength: Integer; virtual;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure DefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string; const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetOwnPropertyKeys: TArray<string>; override;
    function HasOwnProperty(const AName: string): Boolean; override;
    function DeleteProperty(const AName: string): Boolean; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; virtual;

    procedure ReplacePrototype(const APrototype: TGocciaObjectValue);
    procedure SetConstructorPrototype(const APrototype: TGocciaObjectValue);
    function GetConstructorPrototype: TGocciaObjectValue;
    procedure LinkNativeSuperConstructor(const ASuperConstructor: TGocciaObjectValue);
    procedure SetInferredName(const AName: string);
    procedure SetSourceText(const ASourceText: string);
    function GetSourceText: string;
    function GetOwnStaticSymbolDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;

    property Name: string read FName;
    property SourceText: string read FSourceText write FSourceText;
    property CreationRealm: TGocciaRealm read FCreationRealm;
    property PrivateBrandToken: string read FPrivateBrandToken;
    property SuperClass: TGocciaClassValue read FSuperClass write FSuperClass;
    property NativeSuperConstructor: TGocciaObjectValue read FNativeSuperConstructor write FNativeSuperConstructor;
    property Prototype: TGocciaObjectValue read FClassPrototype;
    property PrivateStaticProperties: TGocciaValueMap read FPrivateStaticProperties;
    property PrivateStaticMethods: TGocciaValueMap read FPrivateStaticMethods;
    property StaticPropertyGetter[const AName: string]: TGocciaFunctionBase read GetStaticPropertyGetter;
    property StaticPropertySetter[const AName: string]: TGocciaFunctionBase read GetStaticPropertySetter;
    property PrivatePropertyGetter[const AName: string]: TGocciaValue read GetPrivatePropertyGetter;
    property PrivatePropertySetter[const AName: string]: TGocciaValue read GetPrivatePropertySetter;
    procedure MarkReferences; override;

    procedure AddFieldInitializer(const AName: string; const AInitializer: TGocciaValue; const AIsPrivate, AIsStatic: Boolean);
    procedure AddFieldInitializerWithKey(const AName: string; const AComputedKey: TGocciaValue; const AInitializer: TGocciaValue; const AIsPrivate, AIsStatic: Boolean);
    procedure SetMethodInitializers(const AInitializers: array of TGocciaValue);
    procedure AppendMethodInitializers(const AInitializers: array of TGocciaValue);
    procedure AppendFieldInitializers(const AInitializers: array of TGocciaValue);
    procedure RunMethodInitializers(const AInstance: TGocciaValue);
    procedure RunFieldInitializers(const AInstance: TGocciaValue);
    procedure RunDecoratorFieldInitializers(const AInstance: TGocciaValue);
    procedure RunDecoratorStaticFieldInitializers;
  end;

  // ES2026 §20.1.1.1 Object ( [ value ] ): a call, or a construction with no
  // NewTarget other than Object itself, returns ToObject(value) for a value
  // that is not undefined or null.
  TGocciaObjectClassValue = class(TGocciaClassValue)
    function Instantiate(const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue = nil): TGocciaValue; override;
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
  end;

  TGocciaArrayClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ES2026 §23.1.1.1 Array ( ...values ): a call constructs like new.
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    // ECMAScript 23.1.1.1: Array constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaMapClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; override;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; override;
  end;

  TGocciaSetClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; override;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; override;
  end;

  TGocciaWeakMapClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; override;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; override;
  end;

  TGocciaWeakSetClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; override;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; override;
  end;

  TGocciaWeakRefClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; override;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; override;
    function GetClassLength: Integer; override;
  end;

  TGocciaFinalizationRegistryClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    function SupportsRealmIntrinsicPrototypeFallback: Boolean; override;
    function IntrinsicPrototypeForRealm(
      const ARealm: TGocciaRealm): TGocciaObjectValue; override;
    function GetClassLength: Integer; override;
  end;

  TGocciaStringClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ES2026 §22.1.1.1 String ( value ): a call converts to a string primitive.
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function DefaultPrototypeForNewTarget(const ANewTarget: TGocciaValue;
      const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
    // ECMAScript 22.1.1.1: String constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaNumberClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ES2026 §21.1.1.1 Number ( value ): a call converts to a number primitive.
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function DefaultPrototypeForNewTarget(const ANewTarget: TGocciaValue;
      const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
    // ECMAScript 21.1.1.1: Number constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaBooleanClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ES2026 §20.3.1.1 Boolean ( value ): a call converts to a boolean primitive.
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function DefaultPrototypeForNewTarget(const ANewTarget: TGocciaValue;
      const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
    // ECMAScript 20.3.1.1: Boolean constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaArrayBufferClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 25.1.1.1: ArrayBuffer constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaSharedArrayBufferClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 25.2.1.1: SharedArrayBuffer constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaDataViewClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 25.3.2.1: DataView constructor length is 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaTextEncoderClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaTextDecoderClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaURLClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // WHATWG URL: URL(url, base?) — required url means length 1.
    function GetClassLength: Integer; override;
  end;

  TGocciaURLSearchParamsClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaHeadersClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaResponseClassValue = class(TGocciaClassValue)
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
  end;

  TGocciaCompileDynamicFunction = function(const AParamsSources: array of string;
    const ABodySource: string;
    const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBase of object;

  TGocciaFunctionConstructorClassValue = class(TGocciaClassValue)
  private
    FEnabled: Boolean;
    FCapabilityAuditEmitter: TGocciaCapabilityAuditEmitter;
    FCompileDynamicFunction: TGocciaCompileDynamicFunction;
    FDynamicKind: TGocciaDynamicFunctionKind;
    function BuildFunction(const AArguments: TGocciaArgumentsCollection): TGocciaFunctionBase;
  public
    constructor Create(const AName: string; const ASuperClass: TGocciaClassValue;
      const ADynamicKind: TGocciaDynamicFunctionKind = dfkNormal);
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    function CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue; override;
    // ECMAScript 20.2.2: Function constructor length is 1.
    function GetClassLength: Integer; override;

    property Enabled: Boolean read FEnabled write FEnabled;
    property CapabilityAuditEmitter: TGocciaCapabilityAuditEmitter
      read FCapabilityAuditEmitter write FCapabilityAuditEmitter;
    property CompileDynamicFunction: TGocciaCompileDynamicFunction
      read FCompileDynamicFunction write FCompileDynamicFunction;
    property DynamicKind: TGocciaDynamicFunctionKind read FDynamicKind;
  end;

  TGocciaInstanceValue = class(TGocciaObjectValue)
  private
    FClass: TGocciaClassValue;
    FPrivateProperties: TGocciaValueMap;
  public
    constructor Create(const AClass: TGocciaClassValue = nil;
      const APropertyCapacity: Integer = 0);
    destructor Destroy; override;
    function TypeName: string; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True); override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    function TryGetRawPrivateProperty(const AKey: string; out AValue: TGocciaValue): Boolean;
    procedure SetRawPrivateProperty(const AKey: string; const AValue: TGocciaValue);
    procedure InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); virtual;
    procedure FinalizeNativeFromArguments(const AArguments: TGocciaArgumentsCollection); virtual;
    procedure MarkReferences; override;

    property ClassValue: TGocciaClassValue read FClass write FClass;
  end;

function ShouldDelayTypedArrayPrototypeLookup(
  const ANativeClass: TGocciaClassValue;
  const AArguments: TGocciaArgumentsCollection): Boolean;
function ShouldDelayNativePrototypeLookup(
  const ANativeClass: TGocciaClassValue;
  const AArguments: TGocciaArgumentsCollection): Boolean;
function ShouldDelayNativeSuperPrototypeLookup(
  const ANativeSuperConstructor: TGocciaObjectValue): Boolean;
function GetNativePrototypeFromConstructor(
  const ANativeClass: TGocciaClassValue;
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;

type
  TGocciaClassChain = array of TGocciaClassValue;

  { What walking a class's implicit-constructor chain found. }
  TGocciaImplicitConstructorChain = record
    { Classes between the starting class and the constructor that actually
      runs, derived-most first: §15.7.14 step 15a still owes each of them an
      implicit constructor, so the caller owes their instance elements. }
    Collapsed: TGocciaClassChain;
    { The class whose constructor body the caller should run, or nil. }
    HostClass: TGocciaClassValue;
    { The object the chain's last super() reaches when it is not a class value
      — a built-in constructor exposed as a function, or, after an
      Object.setPrototypeOf, whatever the constructor was retargeted onto.
      Nil when the chain ends at a class or at a ~base~ constructor. }
    SuperConstructor: TGocciaObjectValue;
    { The class whose super() reaches SuperConstructor. }
    SuperConstructorOwner: TGocciaClassValue;
    { Set when the chain stops at a ~derived~ class that has no [[Prototype]]
      left at all — Object.setPrototypeOf(C, null). §13.3.7.3 hands super()
      undefined and §13.3.7.1 step 3 rejects it. }
    SuperConstructorAbsent: Boolean;
  end;

{ ES2026 §15.7.14 step 15a: a class with no constructor of its own runs an
  implicit constructor that forwards its arguments to super and then, per
  §13.3.7.1 step 11, initializes its own instance elements.

  Every construction path collapses that recursion — it walks the superclass
  chain in one go to find the built-in that has to allocate the receiver, then
  runs one constructor against it — and each class the walk stepped over lost
  both halves of its implicit constructor. A class extending Array with a field
  of its own, subclassed again by a class with another field, produced an
  instance carrying only the subclass's field.

  Every hop is §13.3.7.3 GetSuperConstructor, so the walk follows one chain —
  the constructor objects' [[Prototype]] — and never mixes it with the declared
  superclass. Mixing them was unsound as well as wrong: each relation alone is
  acyclic (ordinary [[SetPrototypeOf]] rejects a cycle), their union is not, and
  a plain Object.setPrototypeOf pair could spin this walk forever. }
procedure ResolveImplicitConstructorChain(
  const AClassValue: TGocciaClassValue;
  out AChain: TGocciaImplicitConstructorChain);

{ True when AChain ends at an object that cannot be constructed — §13.3.7.1
  SuperCall step 3 raises a TypeError for it. }
function ImplicitSuperConstructorIsUnusable(
  const AChain: TGocciaImplicitConstructorChain): Boolean;

{ True when AChain's super constructor is one Object.setPrototypeOf moved there
  rather than the one the class was declared with. The declared one is already
  driven by the construction paths' native-super branches; a retargeted one has
  to be constructed through instead of them. }
function ImplicitSuperConstructorIsRetargeted(
  const AChain: TGocciaImplicitConstructorChain): Boolean;

{ The object an implicit super() reaches: §13.3.7.3 GetSuperConstructor reads
  the active function object's [[GetPrototypeOf]], which Object.setPrototypeOf
  moves. Nil for a ~base~ class, which has no super() at all. }
function ImplicitSuperConstructorTarget(
  const AClassValue: TGocciaClassValue): TGocciaObjectValue;

{ The same hop, reported only when it lands on a class value. }
function ImplicitSuperConstructorClass(
  const AClassValue: TGocciaClassValue): TGocciaClassValue;

{ True when AClassValue is ~derived~ yet has nothing to resolve super()
  through, which only Object.setPrototypeOf(C, null) produces. }
function ImplicitSuperConstructorIsAbsent(
  const AClassValue: TGocciaClassValue): Boolean;

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Constants.TypeNames,
  Goccia.Error,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.InstructionLimit,
  Goccia.Intrinsics.FunctionObjects,
  Goccia.Timeout,
  Goccia.Utils,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ArrayValue,
  Goccia.Values.BigIntValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.ClassHelper,
  Goccia.Values.DataViewValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FinalizationRegistryValue,
  Goccia.Values.HeadersValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ResponseValue,
  Goccia.Values.SetValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.TextDecoderValue,
  Goccia.Values.TextEncoderValue,
  Goccia.Values.ToPrimitive,
  Goccia.Values.TypedArrayValue,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue,
  Goccia.Values.WeakMapValue,
  Goccia.Values.WeakRefValue,
  Goccia.Values.WeakSetValue;

function ImplicitSuperConstructorTarget(
  const AClassValue: TGocciaClassValue): TGocciaObjectValue;
begin
  Result := nil;
  if not Assigned(AClassValue) then
    Exit;
  { §15.7.14 step 9 fixes [[ConstructorKind]] when the class is defined, and
    §10.2.2 step 5 gives a ~base~ constructor no super() at all — so nothing
    about its [[Construct]] follows the constructor object's [[Prototype]].
    Probed against Node v24.0.1: an ordinary class retargeted with
    Object.setPrototypeOf onto a class that has a constructor still constructs
    a plain object and never runs it. }
  if not AClassValue.HasDerivedConstructorKind then
    Exit;
  { §13.3.7.3 GetSuperConstructor: a ~derived~ constructor resolves super()
    through the active function object's [[GetPrototypeOf]], which
    Object.setPrototypeOf does move — including onto something that is not a
    constructor at all, which §13.3.7.1 SuperCall step 3 rejects. Falling back
    to the declared superclass here instead was wrong twice over: it ran a
    constructor Node never runs, and it mixed two chains into a walk that could
    cycle. }
  Result := AClassValue.GetConstructorPrototype;
end;

function ImplicitSuperConstructorClass(
  const AClassValue: TGocciaClassValue): TGocciaClassValue;
var
  Target: TGocciaObjectValue;
begin
  Target := ImplicitSuperConstructorTarget(AClassValue);
  if Target is TGocciaClassValue then
    Result := TGocciaClassValue(Target)
  else
    Result := nil;
end;

function ImplicitSuperConstructorIsAbsent(
  const AClassValue: TGocciaClassValue): Boolean;
begin
  Result := Assigned(AClassValue) and AClassValue.HasDerivedConstructorKind and
    (not Assigned(AClassValue.GetConstructorPrototype));
end;

function ImplicitSuperConstructorIsUnusable(
  const AChain: TGocciaImplicitConstructorChain): Boolean;
begin
  Result := AChain.SuperConstructorAbsent or
    (Assigned(AChain.SuperConstructor) and
     ((AChain.SuperConstructor = TGocciaFunctionBase.GetSharedPrototype) or
      (not AChain.SuperConstructor.IsConstructable)));
end;

function ImplicitSuperConstructorIsRetargeted(
  const AChain: TGocciaImplicitConstructorChain): Boolean;
begin
  Result := Assigned(AChain.SuperConstructor) and
    Assigned(AChain.SuperConstructorOwner) and
    (AChain.SuperConstructor <>
     AChain.SuperConstructorOwner.NativeSuperConstructor);
end;

procedure ResolveImplicitConstructorChain(
  const AClassValue: TGocciaClassValue;
  out AChain: TGocciaImplicitConstructorChain);
var
  Count, Index: Integer;
  Current, Next: TGocciaClassValue;
  Target: TGocciaObjectValue;
  AlreadySeen: Boolean;
begin
  SetLength(AChain.Collapsed, 0);
  AChain.HostClass := nil;
  AChain.SuperConstructor := nil;
  AChain.SuperConstructorOwner := nil;
  AChain.SuperConstructorAbsent := False;
  Count := 0;
  Current := AClassValue;

  while Assigned(Current) do
  begin
    { The same bounds the sibling walk in InstantiateClass applies. Following
      one [[Prototype]] chain cannot cycle, so these are defence in depth
      rather than the correctness argument. }
    CheckExecutionTimeout;
    IncrementInstructionCounter;
    CheckInstructionLimit;

    Target := ImplicitSuperConstructorTarget(Current);
    if not Assigned(Target) then
    begin
      { Either a ~base~ constructor, which has no super() at all, or a derived
        one whose [[Prototype]] was set to null and so has nothing to call. }
      AChain.SuperConstructorAbsent := ImplicitSuperConstructorIsAbsent(Current);
      if AChain.SuperConstructorAbsent then
        AChain.SuperConstructorOwner := Current;
      Break;
    end;

    if not (Target is TGocciaClassValue) then
    begin
      AChain.SuperConstructor := Target;
      AChain.SuperConstructorOwner := Current;
      Break;
    end;

    Next := TGocciaClassValue(Target);
    if (Next.NativeInstanceDefaultPrototype <> nil) or
       Next.UsesOwnInstantiation then
    begin
      AChain.HostClass := Next;
      Break;
    end;

    { Defence in depth again: a repeat means the walk stopped following one
      chain, which should be impossible. Stopping beats spinning. }
    AlreadySeen := Next = AClassValue;
    for Index := 0 to Count - 1 do
      if AChain.Collapsed[Index] = Next then
        AlreadySeen := True;
    if AlreadySeen then
      Break;

    if Count = Length(AChain.Collapsed) then
      SetLength(AChain.Collapsed, Count * 2 + 4);
    AChain.Collapsed[Count] := Next;
    Inc(Count);
    Current := Next;
  end;

  SetLength(AChain.Collapsed, Count);
end;

function ToNumberConstructorValue(
  const AValue: TGocciaValue): TGocciaNumberLiteralValue;
begin
  // ES2026 §21.1.1.1 Number(value): explicit Number() conversion accepts
  // BigInt. Keep generic ToNumber paths throwing for implicit coercion.
  if AValue is TGocciaBigIntValue then
    Exit(TGocciaNumberLiteralValue.Create(
      TGocciaBigIntValue(AValue).Value.ToDouble));
  Result := AValue.ToNumberLiteral;
end;

function ToStringConstructorValue(const AValue: TGocciaValue;
  const AAllowSymbol: Boolean): TGocciaStringLiteralValue;
var
  Prim: TGocciaValue;
begin
  if AValue is TGocciaSymbolValue then
  begin
    if AAllowSymbol then
      Exit(TGocciaSymbolValue(AValue).ToDisplayString);
    ThrowTypeError(SErrorSymbolToString, SSuggestSymbolNoImplicitConversion);
  end;

  if AValue is TGocciaObjectValue then
  begin
    Prim := ToPrimitive(AValue, tphString);
    Exit(Prim.ToStringLiteral);
  end;

  Result := AValue.ToStringLiteral;
end;

function GetTypedArrayPrototypeFromConstructor(
  const ATypedArrayClass: TGocciaTypedArrayClassValue;
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := ATypedArrayClass.DefaultPrototypeForNewTarget(ANewTarget,
    ACurrentRealmDefault);
end;

function ShouldDelayTypedArrayPrototypeLookup(
  const ANativeClass: TGocciaClassValue;
  const AArguments: TGocciaArgumentsCollection): Boolean;
var
  FirstArg: TGocciaValue;
begin
  Result := False;
  if not (ANativeClass is TGocciaTypedArrayClassValue) or
     (AArguments = nil) or
     (AArguments.Length = 0) then
    Exit;

  FirstArg := AArguments.GetElement(0);
  Result := not (FirstArg is TGocciaArrayBufferValue) and
    not (FirstArg is TGocciaSharedArrayBufferValue) and
    not (FirstArg is TGocciaTypedArrayValue);
end;

function ShouldDelayNativePrototypeLookup(
  const ANativeClass: TGocciaClassValue;
  const AArguments: TGocciaArgumentsCollection): Boolean;
begin
  Result := Assigned(ANativeClass) and
    ((ANativeClass is TGocciaFunctionConstructorClassValue) or
     ShouldDelayTypedArrayPrototypeLookup(ANativeClass, AArguments) or
     (ANativeClass is TGocciaArrayBufferClassValue) or
     (ANativeClass is TGocciaSharedArrayBufferClassValue) or
     (ANativeClass is TGocciaDataViewClassValue));
end;

function ShouldDelayNativeSuperPrototypeLookup(
  const ANativeSuperConstructor: TGocciaObjectValue): Boolean;
begin
  Result := Assigned(ANativeSuperConstructor) and
    (ANativeSuperConstructor <> TGocciaFunctionBase.GetSharedPrototype) and
    (ANativeSuperConstructor is TGocciaClassValue) and
    (TGocciaClassValue(ANativeSuperConstructor).
      NativeInstanceDefaultPrototype <> nil);
end;

function GetArrayPrototypeFromConstructor(
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
var
  FallbackRealm: TGocciaRealm;
  ProtoValue: TGocciaValue;
begin
  if ANewTarget is TGocciaObjectValue then
    ProtoValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE)
  else
    ProtoValue := nil;

  if ProtoValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ProtoValue));

  FallbackRealm := GetFunctionRealm(ANewTarget);

  Result := GetSharedArrayPrototypeForRealm(FallbackRealm);
  if not Assigned(Result) then
    Result := ACurrentRealmDefault;
end;

function DefaultNativePrototypeForNewTarget(
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue;
  const AConstructorName: string): TGocciaObjectValue;
var
  FallbackPrototype: TGocciaObjectValue;
  FallbackRealm: TGocciaRealm;
  ProtoValue: TGocciaValue;
begin
  if ANewTarget is TGocciaObjectValue then
    ProtoValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE)
  else
    ProtoValue := nil;

  if ProtoValue is TGocciaObjectValue then
    Exit(TGocciaObjectValue(ProtoValue));

  FallbackRealm := GetFunctionRealm(ANewTarget);

  if Assigned(FallbackRealm) then
  begin
    if AConstructorName = CONSTRUCTOR_NUMBER then
      FallbackPrototype :=
        TGocciaNumberObjectValue.GetSharedPrototypeForRealm(FallbackRealm)
    else if AConstructorName = CONSTRUCTOR_STRING then
      FallbackPrototype :=
        TGocciaStringObjectValue.GetSharedPrototypeForRealm(FallbackRealm)
    else if AConstructorName = CONSTRUCTOR_BOOLEAN then
      FallbackPrototype :=
        TGocciaBooleanObjectValue.GetSharedPrototypeForRealm(FallbackRealm)
    else if AConstructorName = CONSTRUCTOR_ARRAY_BUFFER then
      FallbackPrototype :=
        TGocciaArrayBufferValue.GetSharedPrototypeForRealm(FallbackRealm)
    else if AConstructorName = CONSTRUCTOR_SHARED_ARRAY_BUFFER then
      FallbackPrototype :=
        TGocciaSharedArrayBufferValue.GetSharedPrototypeForRealm(FallbackRealm)
    else if AConstructorName = CONSTRUCTOR_DATA_VIEW then
      FallbackPrototype :=
        TGocciaDataViewValue.GetSharedPrototypeForRealm(FallbackRealm)
    else
      FallbackPrototype := nil;

    if Assigned(FallbackPrototype) then
      Exit(FallbackPrototype);
  end;

  Result := ACurrentRealmDefault;
end;

function GetNativePrototypeFromConstructor(
  const ANativeClass: TGocciaClassValue;
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
var
  FallbackPrototype: TGocciaObjectValue;
  FallbackRealm: TGocciaRealm;
  ProtoValue: TGocciaValue;
begin
  if ANativeClass.SupportsRealmIntrinsicPrototypeFallback then
  begin
    if ANewTarget is TGocciaObjectValue then
      ProtoValue := TGocciaObjectValue(ANewTarget).GetProperty(PROP_PROTOTYPE)
    else
      ProtoValue := nil;
    if ProtoValue is TGocciaObjectValue then
      Exit(TGocciaObjectValue(ProtoValue));

    FallbackRealm := GetFunctionRealm(ANewTarget);
    FallbackPrototype := ANativeClass.IntrinsicPrototypeForRealm(FallbackRealm);
    if Assigned(FallbackPrototype) then
      Exit(FallbackPrototype);
    Exit(ACurrentRealmDefault);
  end;

  if ANativeClass is TGocciaTypedArrayClassValue then
    Result := GetTypedArrayPrototypeFromConstructor(
      TGocciaTypedArrayClassValue(ANativeClass), ANewTarget,
      ACurrentRealmDefault)
  else if ANativeClass is TGocciaArrayClassValue then
    Result := GetArrayPrototypeFromConstructor(ANewTarget,
      ACurrentRealmDefault)
  else if ANativeClass is TGocciaStringClassValue then
    Result := TGocciaStringClassValue(ANativeClass).
      DefaultPrototypeForNewTarget(ANewTarget, ACurrentRealmDefault)
  else if ANativeClass is TGocciaNumberClassValue then
    Result := TGocciaNumberClassValue(ANativeClass).
      DefaultPrototypeForNewTarget(ANewTarget, ACurrentRealmDefault)
  else if ANativeClass is TGocciaBooleanClassValue then
    Result := TGocciaBooleanClassValue(ANativeClass).
      DefaultPrototypeForNewTarget(ANewTarget, ACurrentRealmDefault)
  else if ANativeClass is TGocciaFunctionConstructorClassValue then
    Result := FunctionObjectIntrinsicPrototypeFromConstructor(
      TGocciaFunctionConstructorClassValue(ANativeClass).DynamicKind,
      ANewTarget, ACurrentRealmDefault)
  else if ANativeClass is TGocciaArrayBufferClassValue then
    Result := DefaultNativePrototypeForNewTarget(ANewTarget,
      ACurrentRealmDefault, CONSTRUCTOR_ARRAY_BUFFER)
  else if ANativeClass is TGocciaSharedArrayBufferClassValue then
    Result := DefaultNativePrototypeForNewTarget(ANewTarget,
      ACurrentRealmDefault, CONSTRUCTOR_SHARED_ARRAY_BUFFER)
  else if ANativeClass is TGocciaDataViewClassValue then
    Result := DefaultNativePrototypeForNewTarget(ANewTarget,
      ACurrentRealmDefault, CONSTRUCTOR_DATA_VIEW)
  else
    Result := GetProtoFromConstructorWithIntrinsic(ANewTarget,
      ACurrentRealmDefault);
end;

// SetDefaultPrototype / PatchDefaultPrototype previously cached the
// "default constructor [[Prototype]]" (Function.prototype) in a threadvar.
// That cache held a stale reference across realm teardown: when the previous
// engine was freed its realm unpinned Function.prototype, but the threadvar
// still pointed at the dead object - so the next engine's PatchDefaultPrototype
// calls would patch new constructors with the prior realm's prototype.
// Now we read TGocciaFunctionBase.GetSharedPrototype directly each call,
// which always returns the current realm's Function.prototype.
class procedure TGocciaClassValue.SetDefaultPrototype(const AProto: TGocciaObjectValue);
begin
  // Intentionally a no-op.  The current-realm lookup in PatchDefaultPrototype
  // is the source of truth.  Kept for backwards compatibility with existing
  // engine bootstrap call sites.
end;

class procedure TGocciaClassValue.PatchDefaultPrototype(const AClassValue: TGocciaClassValue);
var
  FunctionPrototype: TGocciaObjectValue;
begin
  if not Assigned(AClassValue) then Exit;
  if Assigned(AClassValue.FPrototype) then Exit;
  FunctionPrototype := TGocciaFunctionBase.GetSharedPrototype;
  if Assigned(FunctionPrototype) then
    AClassValue.FPrototype := FunctionPrototype;
end;

function ClassPrototypeDescriptorChangeAllowed(
  const APrototype: TGocciaObjectValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
var
  DataDescriptor: TGocciaPropertyDescriptorData;
begin
  Result := False;
  if not Assigned(ADescriptor) then
    Exit(True);

  if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
    Exit;
  if ADescriptor.HasEnumerableField and ADescriptor.Enumerable then
    Exit;
  if IsAccessorDescriptor(ADescriptor) then
    Exit;

  if IsDataDescriptor(ADescriptor) then
  begin
    DataDescriptor := TGocciaPropertyDescriptorData(ADescriptor);
    if ADescriptor.HasWritableField and DataDescriptor.Writable then
      Exit;
    if ADescriptor.HasValue and
       not IsSameValue(APrototype, DataDescriptor.Value) then
      Exit;
  end;

  Result := True;
end;

constructor TGocciaClassValue.Create(const AName: string; const ASuperClass: TGocciaClassValue);
begin
  inherited Create;
  FName := AName;
  FCreationRealm := CurrentRealm;
  FPrivateBrandToken := IntToHex(NativeUInt(Self), SizeOf(Pointer) * 2);
  FSuperClass := ASuperClass;
  // Set [[Prototype]]: superclass for derived, Function.prototype for base classes
  if Assigned(FSuperClass) then
    FPrototype := FSuperClass
  else if TGocciaFunctionBase.GetSharedPrototype <> nil then
    FPrototype := TGocciaFunctionBase.GetSharedPrototype;
  FStaticMethods := TGocciaValueMap.Create;
  FPrivateStaticProperties := TGocciaValueMap.Create;
  FPrivateStaticMethods := TGocciaValueMap.Create;
  FPrivateGetters := TOrderedStringMap<TGocciaValue>.Create;
  FPrivateSetters := TOrderedStringMap<TGocciaValue>.Create;
  FDeclaredPrivateNames := TStringStringMap.Create;
  FNativeSuperConstructor := nil;
  FClassPrototype := TGocciaObjectValue.Create;
  FNameDeleted := False;
  FLengthDeleted := False;
  if Assigned(FSuperClass) then
    FClassPrototype.Prototype := FSuperClass.Prototype
  else if TGocciaObjectValue.SharedObjectPrototype <> nil then
    FClassPrototype.Prototype := TGocciaObjectValue.SharedObjectPrototype;

  // .name is synthesized in GetOwnPropertyDescriptor (like .prototype)
  // to allow static field overrides via SetProperty.
end;

destructor TGocciaClassValue.Destroy;
begin
  FStaticMethods.Free;
  FPrivateStaticProperties.Free;
  FPrivateStaticMethods.Free;
  FPrivateGetters.Free;
  FPrivateSetters.Free;
  FDeclaredPrivateNames.Free;
  FDeclaredPrivateKeyNames.Free;
  // Don't free FClassPrototype - it's GC-managed
  inherited;
end;

procedure TGocciaClassValue.MarkReferences;
var
  ValPair: TGocciaValueMap.TKeyValuePair;
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FSuperClass) then
    FSuperClass.MarkReferences;

  if Assigned(FNativeSuperConstructor) then
    FNativeSuperConstructor.MarkReferences;

  if Assigned(FClassPrototype) then
    FClassPrototype.MarkReferences;

  for ValPair in FStaticMethods do
    if Assigned(ValPair.Value) then
      ValPair.Value.MarkReferences;

  for ValPair in FPrivateStaticProperties do
    if Assigned(ValPair.Value) then
      ValPair.Value.MarkReferences;

  for ValPair in FPrivateStaticMethods do
    if Assigned(ValPair.Value) then
      ValPair.Value.MarkReferences;

  for ValPair in FPrivateGetters do
    ValPair.Value.MarkReferences;
  for ValPair in FPrivateSetters do
    ValPair.Value.MarkReferences;

  // Mark decorator initializer arrays
  for I := 0 to High(FMethodInitializers) do
    if Assigned(FMethodInitializers[I]) then
      FMethodInitializers[I].MarkReferences;
  for I := 0 to High(FFieldInitializers) do
    if Assigned(FFieldInitializers[I]) then
      FFieldInitializers[I].MarkReferences;
  for I := 0 to High(FDecoratorFieldInitializers) do
  begin
    if Assigned(FDecoratorFieldInitializers[I].Initializer) then
      FDecoratorFieldInitializers[I].Initializer.MarkReferences;
    if Assigned(FDecoratorFieldInitializers[I].ComputedKey) then
      FDecoratorFieldInitializers[I].ComputedKey.MarkReferences;
  end;
  for I := 0 to High(FStaticDecoratorFieldInitializers) do
  begin
    if Assigned(FStaticDecoratorFieldInitializers[I].Initializer) then
      FStaticDecoratorFieldInitializers[I].Initializer.MarkReferences;
    if Assigned(FStaticDecoratorFieldInitializers[I].ComputedKey) then
      FStaticDecoratorFieldInitializers[I].ComputedKey.MarkReferences;
  end;
end;

function TGocciaClassValue.IsCallable: Boolean;
begin
  Result := True;
end;

function TGocciaClassValue.IsConstructable: Boolean;
begin
  Result := True;
end;

function TGocciaClassValue.TypeName: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaClassValue.TypeOf: string;
begin
  Result := FUNCTION_TYPE_NAME;
end;

function TGocciaClassValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  // For error classes, just return the name to make toThrow work properly
  if (FName = RANGE_ERROR_NAME) or (FName = ERROR_NAME) or (FName = TYPE_ERROR_NAME) or (FName = REFERENCE_ERROR_NAME) then
    Result := TGocciaStringLiteralValue.Create(FName)
  else
    Result := TGocciaStringLiteralValue.Create(Format('[Class: %s]', [FName]));
end;

function TGocciaClassValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

procedure TGocciaClassValue.AddStaticGetter(const AName: string; const AGetter: TGocciaFunctionBase);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingSetter: TGocciaValue;
begin
  if AName = PROP_PROTOTYPE then
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
      SSuggestCannotDeleteNonConfigurable);

  ExistingDescriptor := inherited GetOwnPropertyDescriptor(AName);
  ExistingSetter := nil;
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter) then
    ExistingSetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Setter;

  inherited DefineProperty(AName,
    TGocciaPropertyDescriptorAccessor.Create(
      AGetter, ExistingSetter, [pfConfigurable]));
end;

procedure TGocciaClassValue.AddStaticSetter(const AName: string; const ASetter: TGocciaFunctionBase);
var
  ExistingDescriptor: TGocciaPropertyDescriptor;
  ExistingGetter: TGocciaValue;
begin
  if AName = PROP_PROTOTYPE then
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
      SSuggestCannotDeleteNonConfigurable);

  ExistingDescriptor := inherited GetOwnPropertyDescriptor(AName);
  ExistingGetter := nil;
  if (ExistingDescriptor is TGocciaPropertyDescriptorAccessor) and
     Assigned(TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter) then
    ExistingGetter := TGocciaPropertyDescriptorAccessor(ExistingDescriptor).Getter;

  inherited DefineProperty(AName,
    TGocciaPropertyDescriptorAccessor.Create(
      ExistingGetter, ASetter, [pfConfigurable]));
end;

procedure TGocciaClassValue.AddPrivateGetter(const AName: string; const AGetter: TGocciaValue);
begin
  FPrivateGetters.AddOrSetValue(AName, AGetter);
  Inc(FPrivateNamesVersion);
end;

procedure TGocciaClassValue.AddPrivateSetter(const AName: string; const ASetter: TGocciaValue);
begin
  FPrivateSetters.AddOrSetValue(AName, ASetter);
  Inc(FPrivateNamesVersion);
end;

function TGocciaClassValue.HasOwnPrivateGetter(const AName: string): Boolean;
begin
  Result := FPrivateGetters.ContainsKey(AName);
end;

function TGocciaClassValue.HasOwnPrivateSetter(const AName: string): Boolean;
begin
  Result := FPrivateSetters.ContainsKey(AName);
end;

function TGocciaClassValue.HasOwnPrivateStaticProperty(
  const AName: string): Boolean;
begin
  Result := FPrivateStaticProperties.ContainsKey(AName);
end;

function TGocciaClassValue.HasOwnPrivateStaticMethod(
  const AName: string): Boolean;
begin
  Result := FPrivateStaticMethods.ContainsKey(AName);
end;

function TGocciaClassValue.GetOwnPrivatePropertyGetter(
  const AName: string): TGocciaValue;
begin
  if not FPrivateGetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetOwnPrivatePropertySetter(
  const AName: string): TGocciaValue;
begin
  if not FPrivateSetters.TryGetValue(AName, Result) then
    Result := nil;
end;

function TGocciaClassValue.GetPrivatePropertyGetter(const AName: string): TGocciaValue;
begin
  if FPrivateGetters.TryGetValue(AName, Result) then
    Exit;
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetPrivatePropertyGetter(AName)
  else
    Result := nil;
end;

function TGocciaClassValue.GetPrivatePropertySetter(const AName: string): TGocciaValue;
begin
  if FPrivateSetters.TryGetValue(AName, Result) then
    Exit;
  if Assigned(FSuperClass) then
    Result := FSuperClass.GetPrivatePropertySetter(AName)
  else
    Result := nil;
end;

function TGocciaClassValue.GetStaticPropertyGetter(const AName: string): TGocciaFunctionBase;
var
  Descriptor: TGocciaPropertyDescriptor;
  Getter: TGocciaValue;
begin
  Result := nil;
  Descriptor := inherited GetOwnPropertyDescriptor(AName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Getter := TGocciaPropertyDescriptorAccessor(Descriptor).Getter;
    if Getter is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(Getter);
  end;
end;

function TGocciaClassValue.GetStaticPropertySetter(const AName: string): TGocciaFunctionBase;
var
  Descriptor: TGocciaPropertyDescriptor;
  Setter: TGocciaValue;
begin
  Result := nil;
  Descriptor := inherited GetOwnPropertyDescriptor(AName);
  if Descriptor is TGocciaPropertyDescriptorAccessor then
  begin
    Setter := TGocciaPropertyDescriptorAccessor(Descriptor).Setter;
    if Setter is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(Setter);
  end;
end;

procedure TGocciaClassValue.AddFieldInitializer(const AName: string; const AInitializer: TGocciaValue; const AIsPrivate, AIsStatic: Boolean);
begin
  AddFieldInitializerWithKey(AName, nil, AInitializer, AIsPrivate, AIsStatic);
end;

procedure TGocciaClassValue.AddFieldInitializerWithKey(const AName: string; const AComputedKey: TGocciaValue; const AInitializer: TGocciaValue; const AIsPrivate, AIsStatic: Boolean);
var
  EntryIndex: Integer;
begin
  if AIsStatic then
  begin
    SetLength(FStaticDecoratorFieldInitializers,
      Length(FStaticDecoratorFieldInitializers) + 1);
    EntryIndex := High(FStaticDecoratorFieldInitializers);
    FStaticDecoratorFieldInitializers[EntryIndex].Name := AName;
    FStaticDecoratorFieldInitializers[EntryIndex].ComputedKey := AComputedKey;
    FStaticDecoratorFieldInitializers[EntryIndex].Initializer := AInitializer;
    FStaticDecoratorFieldInitializers[EntryIndex].IsPrivate := AIsPrivate;
    FStaticDecoratorFieldInitializers[EntryIndex].IsStatic := AIsStatic;
    Exit;
  end;

  SetLength(FDecoratorFieldInitializers,
    Length(FDecoratorFieldInitializers) + 1);
  EntryIndex := High(FDecoratorFieldInitializers);
  FDecoratorFieldInitializers[EntryIndex].Name := AName;
  FDecoratorFieldInitializers[EntryIndex].ComputedKey := AComputedKey;
  FDecoratorFieldInitializers[EntryIndex].Initializer := AInitializer;
  FDecoratorFieldInitializers[EntryIndex].IsPrivate := AIsPrivate;
  FDecoratorFieldInitializers[EntryIndex].IsStatic := AIsStatic;
end;

procedure TGocciaClassValue.SetMethodInitializers(const AInitializers: array of TGocciaValue);
var
  Idx: Integer;
begin
  SetLength(FMethodInitializers, Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FMethodInitializers[Idx] := AInitializers[Idx];
end;

procedure TGocciaClassValue.AppendMethodInitializers(const AInitializers: array of TGocciaValue);
var
  OldLen, Idx: Integer;
begin
  OldLen := Length(FMethodInitializers);
  SetLength(FMethodInitializers, OldLen + Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FMethodInitializers[OldLen + Idx] := AInitializers[Idx];
end;

procedure TGocciaClassValue.AppendFieldInitializers(const AInitializers: array of TGocciaValue);
var
  OldLen, Idx: Integer;
begin
  OldLen := Length(FFieldInitializers);
  SetLength(FFieldInitializers, OldLen + Length(AInitializers));
  for Idx := 0 to High(AInitializers) do
    FFieldInitializers[OldLen + Idx] := AInitializers[Idx];
end;

procedure TGocciaClassValue.RunMethodInitializers(const AInstance: TGocciaValue);
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
begin
  for Idx := 0 to High(FMethodInitializers) do
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      DispatchCall(FMethodInitializers[Idx], Args, AInstance);
    finally
      Args.Free;
    end;
  end;
end;

procedure TGocciaClassValue.RunFieldInitializers(const AInstance: TGocciaValue);
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
begin
  for Idx := 0 to High(FFieldInitializers) do
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      DispatchCall(FFieldInitializers[Idx], Args, AInstance);
    finally
      Args.Free;
    end;
  end;
end;

procedure TGocciaClassValue.RunDecoratorFieldInitializers(const AInstance: TGocciaValue);
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
  InitResult, OriginalValue: TGocciaValue;
  PropertyKey: TGocciaValue;
  PropertyName: string;
begin
  for Idx := 0 to High(FDecoratorFieldInitializers) do
  begin
    if FDecoratorFieldInitializers[Idx].IsStatic then
      Continue;

    if Assigned(AInstance) and (AInstance is TGocciaObjectValue) then
    begin
      PropertyKey := FDecoratorFieldInitializers[Idx].ComputedKey;
      if PropertyKey is TGocciaSymbolValue then
        OriginalValue := TGocciaObjectValue(AInstance).GetSymbolProperty(
          TGocciaSymbolValue(PropertyKey))
      else
      begin
        if Assigned(PropertyKey) then
          PropertyName := PropertyKey.ToStringLiteral.Value
        else
          PropertyName := FDecoratorFieldInitializers[Idx].Name;
        OriginalValue := TGocciaObjectValue(AInstance).GetProperty(PropertyName);
      end;
      if not Assigned(OriginalValue) then
        OriginalValue := TGocciaUndefinedLiteralValue.UndefinedValue;

      Args := TGocciaArgumentsCollection.Create([OriginalValue]);
      try
        InitResult := DispatchCall(FDecoratorFieldInitializers[Idx].Initializer,
          Args, AInstance);
        if Assigned(InitResult) and not (InitResult is TGocciaUndefinedLiteralValue) then
        begin
          if PropertyKey is TGocciaSymbolValue then
            TGocciaObjectValue(AInstance).AssignSymbolProperty(
              TGocciaSymbolValue(PropertyKey), InitResult)
          else
            TGocciaObjectValue(AInstance).AssignProperty(PropertyName, InitResult);
        end;
      finally
        Args.Free;
      end;
    end;
  end;
end;

procedure TGocciaClassValue.RunDecoratorStaticFieldInitializers;
var
  Idx: Integer;
  Args: TGocciaArgumentsCollection;
  InitResult, OriginalValue: TGocciaValue;
  PropertyKey: TGocciaValue;
  PropertyName: string;
begin
  for Idx := 0 to High(FStaticDecoratorFieldInitializers) do
  begin
    PropertyKey := FStaticDecoratorFieldInitializers[Idx].ComputedKey;
    if FStaticDecoratorFieldInitializers[Idx].IsPrivate then
    begin
      if not FPrivateStaticProperties.TryGetValue(
        FStaticDecoratorFieldInitializers[Idx].Name, OriginalValue) then
        OriginalValue := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else if PropertyKey is TGocciaSymbolValue then
      OriginalValue := GetSymbolProperty(TGocciaSymbolValue(PropertyKey))
    else
    begin
      if Assigned(PropertyKey) then
        PropertyName := PropertyKey.ToStringLiteral.Value
      else
        PropertyName := FStaticDecoratorFieldInitializers[Idx].Name;
      OriginalValue := GetProperty(PropertyName);
    end;
    if not Assigned(OriginalValue) then
      OriginalValue := TGocciaUndefinedLiteralValue.UndefinedValue;

    Args := TGocciaArgumentsCollection.Create([OriginalValue]);
    try
      InitResult := DispatchCall(
        FStaticDecoratorFieldInitializers[Idx].Initializer, Args, Self);
      if Assigned(InitResult) and not (InitResult is TGocciaUndefinedLiteralValue) then
      begin
        if FStaticDecoratorFieldInitializers[Idx].IsPrivate then
          AddPrivateStaticProperty(
            FStaticDecoratorFieldInitializers[Idx].Name, InitResult)
        else if PropertyKey is TGocciaSymbolValue then
          AssignSymbolProperty(TGocciaSymbolValue(PropertyKey), InitResult)
        else
          SetProperty(PropertyName, InitResult);
      end;
    finally
      Args.Free;
    end;
  end;
end;

procedure TGocciaClassValue.AddPrivateStaticProperty(const AName: string; const AValue: TGocciaValue);
begin
  DeclarePrivateName(AName);
  if FPrivateStaticProperties.ContainsKey(AName) then
    FPrivateStaticProperties.AddOrSetValue(AName, AValue)
  else
  begin
    if not Extensible then
      ThrowTypeError('Cannot add private elements to a non-extensible object',
        SSuggestObjectNotExtensible);
    FPrivateStaticProperties.Add(AName, AValue);
    Inc(FPrivateNamesVersion);
  end;
end;

procedure TGocciaClassValue.AddPrivateStaticMethod(const AName: string; const AValue: TGocciaValue);
begin
  DeclarePrivateName(AName);
  FPrivateStaticMethods.AddOrSetValue(AName, AValue);
  Inc(FPrivateNamesVersion);
end;

procedure TGocciaClassValue.DeclarePrivateName(const AName: string;
  const AInternalKey: string);
var
  ExistingInternalKey: string;
begin
  if AName = '' then
    Exit;
  // The first internal key declared for a name wins.
  if FDeclaredPrivateNames.TryGetValue(AName, ExistingInternalKey) then
  begin
    if (ExistingInternalKey <> '') or (AInternalKey = '') then
      Exit;
    FDeclaredPrivateNames.AddOrSetValue(AName, AInternalKey);
  end
  else
    FDeclaredPrivateNames.Add(AName, AInternalKey);
  Inc(FPrivateNamesVersion);
  if AInternalKey = '' then
    Exit;
  if not Assigned(FDeclaredPrivateKeyNames) then
    FDeclaredPrivateKeyNames := TStringStringMap.Create;
  if not FDeclaredPrivateKeyNames.ContainsKey(AInternalKey) then
    FDeclaredPrivateKeyNames.Add(AInternalKey, AName);
end;

function TGocciaClassValue.TryGetDeclaredPrivateKeyName(
  const AInternalKey: string; out AName: string): Boolean;
begin
  Result := Assigned(FDeclaredPrivateKeyNames) and
    FDeclaredPrivateKeyNames.TryGetValue(AInternalKey, AName);
  if not Result then
    AName := '';
end;

function TGocciaClassValue.ResolveDeclaredPrivateKey(const AName: string;
  out AInternalKey: string): Boolean;
begin
  Result := FDeclaredPrivateNames.TryGetValue(AName, AInternalKey) and
    (AInternalKey <> '');
  if not Result then
    AInternalKey := '';
end;

// Appends AName to ANames[0..ACount-1] unless it is already there, compared
// by code units.
procedure AddUniquePrivateName(var ANames: TArray<string>; var ACount: Integer;
  const AName: string);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    if ANames[I] = AName then
      Exit;
  if ACount = Length(ANames) then
  begin
    if ACount = 0 then
      SetLength(ANames, 4)
    else
      SetLength(ANames, ACount * 2);
  end;
  ANames[ACount] := AName;
  Inc(ACount);
end;

procedure AddUniquePrivateNames(var ANames: TArray<string>;
  var ACount: Integer; const AKeys: array of string);
var
  I: Integer;
begin
  for I := 0 to High(AKeys) do
    AddUniquePrivateName(ANames, ACount, AKeys[I]);
end;

procedure TGocciaClassValue.CollectOwnPrivateNames(
  var ANames: TArray<string>; var ACount: Integer);
var
  DeclaredPair: TStringStringMap.TKeyValuePair;
begin
  // Each map is read only when it has entries: setting up an enumerator
  // costs more than the few names a class usually declares.
  if FDeclaredPrivateNames.CountFast > 0 then
    for DeclaredPair in FDeclaredPrivateNames do
      if DeclaredPair.Value <> '' then
        AddUniquePrivateName(ANames, ACount, DeclaredPair.Value)
      else
        AddUniquePrivateName(ANames, ACount, DeclaredPair.Key);
  if FPrivateStaticProperties.CountFast > 0 then
    AddUniquePrivateNames(ANames, ACount, FPrivateStaticProperties.Keys);
  if FPrivateStaticMethods.CountFast > 0 then
    AddUniquePrivateNames(ANames, ACount, FPrivateStaticMethods.Keys);
  if FPrivateGetters.CountFast > 0 then
    AddUniquePrivateNames(ANames, ACount, FPrivateGetters.Keys);
  if FPrivateSetters.CountFast > 0 then
    AddUniquePrivateNames(ANames, ACount, FPrivateSetters.Keys);
end;

function TGocciaClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := nil;
end;

function TGocciaClassValue.NativeInstanceDefaultPrototype: TGocciaObjectValue;
begin
  if (Self is TGocciaArrayClassValue) or
     (Self is TGocciaMapClassValue) or
     (Self is TGocciaSetClassValue) or
     (Self is TGocciaWeakMapClassValue) or
     (Self is TGocciaWeakSetClassValue) or
     (Self is TGocciaWeakRefClassValue) or
     (Self is TGocciaFinalizationRegistryClassValue) or
     (Self is TGocciaStringClassValue) or
     (Self is TGocciaNumberClassValue) or
     (Self is TGocciaBooleanClassValue) or
     (Self is TGocciaArrayBufferClassValue) or
     (Self is TGocciaSharedArrayBufferClassValue) or
     (Self is TGocciaDataViewClassValue) or
     (Self is TGocciaTextEncoderClassValue) or
     (Self is TGocciaTextDecoderClassValue) or
     (Self is TGocciaURLClassValue) or
     (Self is TGocciaURLSearchParamsClassValue) or
     (Self is TGocciaHeadersClassValue) or
     (Self is TGocciaResponseClassValue) or
     (Self is TGocciaFunctionConstructorClassValue) then
    Result := Prototype
  else
    Result := nil;
end;

function TGocciaClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := nil;
end;

function TGocciaClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := False;
end;

procedure TGocciaClassValue.ReplacePrototype(const APrototype: TGocciaObjectValue);
begin
  FClassPrototype := APrototype;
end;

procedure TGocciaClassValue.SetConstructorPrototype(
  const APrototype: TGocciaObjectValue);
begin
  FPrototype := APrototype;
end;

function TGocciaClassValue.GetConstructorPrototype: TGocciaObjectValue;
begin
  Result := FPrototype;
end;

procedure TGocciaClassValue.LinkNativeSuperConstructor(
  const ASuperConstructor: TGocciaObjectValue);
begin
  SetConstructorPrototype(ASuperConstructor);
  FNativeSuperConstructor := ASuperConstructor;
end;

procedure TGocciaClassValue.SetInferredName(const AName: string);
begin
  // Only infer name for anonymous classes — named classes keep their own name.
  // FName update is sufficient; GetOwnPropertyDescriptor synthesizes the descriptor.
  if (FName = '') or (FName = '<anonymous>') then
    FName := AName;
end;

procedure TGocciaClassValue.SetSourceText(const ASourceText: string);
begin
  FSourceText := ASourceText;
end;

function TGocciaClassValue.GetSourceText: string;
begin
  Result := FSourceText;
end;

function TGocciaClassValue.EstimatedInstancePropertyCapacity: Integer;
var
  WalkClass: TGocciaClassValue;
  I: Integer;
begin
  Result := 0;
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    for I := 0 to High(WalkClass.FDecoratorFieldInitializers) do
      if not WalkClass.FDecoratorFieldInitializers[I].IsStatic then
        Inc(Result);
    WalkClass := WalkClass.SuperClass;
  end;
end;

function TGocciaClassValue.HasDerivedConstructorKind: Boolean;
begin
  Result := Assigned(FSuperClass) or Assigned(FNativeSuperConstructor);
end;

function TGocciaClassValue.UsesOwnInstantiation: Boolean;
begin
  Result := False;
end;

// ES2026 §10.2.2 [[Construct]](argumentsList, newTarget)
function TGocciaClassValue.Instantiate(const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Instance: TGocciaObjectValue;
  WalkClass: TGocciaClassValue;
  NativeClass: TGocciaClassValue;
  NativeInstance: TGocciaObjectValue;
  NativeSuperConstructor: TGocciaObjectValue;
  NativeIntrinsicPrototype: TGocciaObjectValue;
  InstancePrototype: TGocciaObjectValue;
  EffectiveNewTarget: TGocciaValue;
  DelayNativePrototypeLookup: Boolean;
  NativeInstanceInitialized: Boolean;
  NativeInstanceConstructedByNativeSuper: Boolean;
  Chain: TGocciaImplicitConstructorChain;
  function ConstructNativeSuperInstance(
    const AConstructor: TGocciaObjectValue): TGocciaObjectValue;
  var
    SuperResult: TGocciaValue;
  begin
    if AConstructor = TGocciaFunctionBase.GetSharedPrototype then
      Exit(nil);

    SuperResult := ConstructValue(AConstructor, AArguments,
      EffectiveNewTarget);
    if SuperResult is TGocciaObjectValue then
      Exit(TGocciaObjectValue(SuperResult));

    ThrowTypeError('Superclass constructor did not return an object',
      SSuggestNotConstructorType);
    Result := nil;
  end;
begin
  if Assigned(ANewTarget) then
    EffectiveNewTarget := ANewTarget
  else
    EffectiveNewTarget := Self;

  NativeClass := nil;
  NativeInstance := nil;
  NativeSuperConstructor := nil;
  NativeIntrinsicPrototype := nil;
  DelayNativePrototypeLookup := False;
  NativeInstanceInitialized := False;
  NativeInstanceConstructedByNativeSuper := False;
  ResolveImplicitConstructorChain(Self, Chain);
  { Which built-in ends up allocating the receiver is decided by the same
    §13.3.7.3 hops the chain walk takes, so this follows them rather than the
    declared superclass: a retargeted constructor allocates from whatever it
    was retargeted onto, and stops reaching the built-in it was declared with. }
  WalkClass := Self;
  while Assigned(WalkClass) do
  begin
    CheckExecutionTimeout;
    IncrementInstructionCounter;
    CheckInstructionLimit;
    NativeIntrinsicPrototype := WalkClass.NativeInstanceDefaultPrototype;
    if Assigned(NativeIntrinsicPrototype) then
    begin
      NativeClass := WalkClass;
      Break;
    end;
    NativeSuperConstructor := ImplicitSuperConstructorTarget(WalkClass);
    if Assigned(NativeSuperConstructor) and
       not (NativeSuperConstructor is TGocciaClassValue) then
    begin
      NativeIntrinsicPrototype := nil;
      Break;
    end;
    NativeSuperConstructor := nil;
    WalkClass := ImplicitSuperConstructorClass(WalkClass);
  end;

  // These constructors perform observable validation/coercion before
  // OrdinaryCreateFromConstructor reaches GetPrototypeFromConstructor. TypedArray
  // does so only for object sources that are neither ArrayBuffer-backed nor
  // typed-array-backed.
  DelayNativePrototypeLookup := ShouldDelayNativePrototypeLookup(NativeClass,
    AArguments) or ShouldDelayNativeSuperPrototypeLookup(NativeSuperConstructor);

  // ES2026 §10.2.2 step 5: Let proto be ? GetPrototypeFromConstructor(newTarget)
  if Assigned(ANewTarget) and not DelayNativePrototypeLookup then
  begin
    if Assigned(NativeClass) then
      InstancePrototype := GetNativePrototypeFromConstructor(NativeClass,
        ANewTarget, NativeIntrinsicPrototype)
    else
      InstancePrototype := GetProtoFromConstructor(ANewTarget);
  end
  else
    InstancePrototype := FClassPrototype;

  if Assigned(NativeClass) then
  begin
    NativeInstance := NativeClass.CreateNativeInstance(AArguments);
    if DelayNativePrototypeLookup and
       (NativeInstance is TGocciaInstanceValue) then
    begin
      TGarbageCollector.Instance.AddTempRoot(NativeInstance);
      try
        TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(AArguments);
      finally
        TGarbageCollector.Instance.RemoveTempRoot(NativeInstance);
      end;
      NativeInstanceInitialized := True;
    end;
  end;

  if Assigned(NativeClass) and Assigned(ANewTarget) and
     DelayNativePrototypeLookup then
    InstancePrototype := GetNativePrototypeFromConstructor(NativeClass,
      ANewTarget, NativeIntrinsicPrototype);

  // ES2026 §10.2.2 step 6: Set proto on the instance before constructor runs
  if Assigned(NativeInstance) then
  begin
    Instance := NativeInstance;
    Instance.Prototype := InstancePrototype;
    if NativeInstance is TGocciaInstanceValue then
      TGocciaInstanceValue(NativeInstance).ClassValue := Self;
  end
  else
  begin
    Instance := TGocciaInstanceValue.Create(Self,
      EstimatedInstancePropertyCapacity);
    Instance.Prototype := InstancePrototype;
  end;

  if NativeInstanceInitialized and (NativeInstance is TGocciaInstanceValue) then
  begin
    TGarbageCollector.Instance.AddTempRoot(Instance);
    try
      TGocciaInstanceValue(NativeInstance).FinalizeNativeFromArguments(AArguments);
    finally
      TGarbageCollector.Instance.RemoveTempRoot(Instance);
    end;
  end;

  { §13.3.7.1 SuperCall step 3: super() reaching something that is not a
    constructor is a TypeError, and Object.setPrototypeOf is the only way to
    put one there. }
  if ImplicitSuperConstructorIsUnusable(Chain) then
    ThrowTypeError(SErrorSuperNotConstructor, SSuggestNotConstructorType);

  if (not Assigned(NativeInstance)) and Assigned(NativeSuperConstructor) then
  begin
    if NativeSuperConstructor = TGocciaFunctionBase.GetSharedPrototype then
      ThrowTypeError(SErrorSuperNotConstructor,
        SSuggestNotConstructorType);
    NativeInstance := ConstructNativeSuperInstance(NativeSuperConstructor);
    NativeInstanceConstructedByNativeSuper := Assigned(NativeInstance);
    if Assigned(NativeInstance) then
    begin
      Instance := NativeInstance;
      if Assigned(NativeClass) or not DelayNativePrototypeLookup then
        Instance.Prototype := InstancePrototype;
      if NativeInstance is TGocciaInstanceValue then
        TGocciaInstanceValue(NativeInstance).ClassValue := Self;
    end;
  end;

  if Assigned(NativeInstance) and (NativeInstance is TGocciaInstanceValue) then
    if not NativeInstanceConstructedByNativeSuper then
      if not NativeInstanceInitialized then
        TGocciaInstanceValue(NativeInstance).InitializeNativeFromArguments(AArguments);

  Result := Instance;
end;

function TGocciaClassValue.GetClassLength: Integer;
begin
  // A built-in class without a source constructor (Map, Set, WeakMap,
  // WeakSet, etc.) has length 0, which matches the spec for the built-in
  // collection constructors. Compiled classes and built-ins with a different
  // length override this.
  Result := 0;
end;

function TGocciaClassValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaClassValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if AName = PROP_PROTOTYPE then
  begin
    Result := FClassPrototype;
    Exit;
  end;

  if FStaticMethods.TryGetValue(AName, Result) then
    Exit;

  if AName = PROP_NAME then
  begin
    if FNameDeleted then
    begin
      Result := inherited GetPropertyWithContext(AName, AThisContext);
      Exit;
    end;

    Descriptor := inherited GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      Result := inherited GetPropertyWithContext(AName, AThisContext);
      Exit;
    end;

    if FName = '<anonymous>' then
      Result := TGocciaStringLiteralValue.Create('')
    else
      Result := TGocciaStringLiteralValue.Create(FName);
    Exit;
  end;

  if AName = PROP_LENGTH then
  begin
    if FLengthDeleted then
    begin
      Result := inherited GetPropertyWithContext(AName, AThisContext);
      Exit;
    end;

    Descriptor := inherited GetOwnPropertyDescriptor(AName);
    if Assigned(Descriptor) then
    begin
      Result := inherited GetPropertyWithContext(AName, AThisContext);
      Exit;
    end;

    Result := TGocciaNumberLiteralValue.Create(GetClassLength);
    Exit;
  end;

  // Fall through to own properties (inherited from TGocciaObjectValue) and
  // then up the constructor [[Prototype]] chain.
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

// A class reports "name" and "length" as own properties without storing them:
// GetOwnPropertyDescriptor synthesizes both. ES2026 §10.1.6.1
// OrdinaryDefineOwnProperty validates a definition against the current own
// property, so the synthesized one has to be in the property map before the
// inherited definition runs. Otherwise the definition is taken as adding a
// property, which a non-extensible class rejects (Object.freeze and
// Object.seal define every own key after preventing extensions) and which
// fills in a partial descriptor from the defaults instead of the current
// attributes. A deleted property stays absent.
procedure TGocciaClassValue.MaterializeIntrinsicProperty(const AName: string);
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  if (AName <> PROP_NAME) and (AName <> PROP_LENGTH) then
    Exit;
  if FProperties.ContainsKey(AName) then
    Exit;
  Descriptor := GetOwnPropertyDescriptor(AName);
  if Assigned(Descriptor) then
    FProperties.Add(AName, Descriptor);
end;

procedure TGocciaClassValue.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
begin
  if AName = PROP_PROTOTYPE then
  begin
    if ClassPrototypeDescriptorChangeAllowed(FClassPrototype, ADescriptor) then
    begin
      ADescriptor.Free;
      Exit;
    end;
    ADescriptor.Free;
    ThrowTypeError(Format(SErrorCannotRedefineNonConfigurable, [AName]),
      SSuggestCannotDeleteNonConfigurable);
  end;

  MaterializeIntrinsicProperty(AName);
  inherited DefineProperty(AName, ADescriptor);
end;

function TGocciaClassValue.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  if AName = PROP_PROTOTYPE then
  begin
    if ClassPrototypeDescriptorChangeAllowed(FClassPrototype, ADescriptor) then
    begin
      ADescriptor.Free;
      Exit(True);
    end;
    ADescriptor.Free;
    Exit(False);
  end;

  MaterializeIntrinsicProperty(AName);
  Result := inherited TryDefineProperty(AName, ADescriptor);
end;

procedure TGocciaClassValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  // An assignment to "name" or "length" is checked against the class's own
  // non-writable property, so store the synthesized one first. Once deleted,
  // the class has no own property and the assignment follows the prototype
  // chain like any other missing key. An assigned name changes only the
  // property: FName keeps the name the class was created with, which error
  // messages use, as Object.defineProperty already left it.
  if (AName = PROP_NAME) or (AName = PROP_LENGTH) then
  begin
    MaterializeIntrinsicProperty(AName);
    inherited SetProperty(AName, AValue);
    Exit;
  end;

  // Runtime property assignment (C.x = val) creates enumerable data properties
  inherited SetProperty(AName, AValue);
end;

function TGocciaClassValue.GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
begin
  if AName = PROP_PROTOTYPE then
    Result := TGocciaPropertyDescriptorData.Create(FClassPrototype, [])
  else if AName = PROP_NAME then
  begin
    // Check if .name was explicitly set (e.g. static name = 'Custom')
    Result := inherited GetOwnPropertyDescriptor(AName);
    if not Assigned(Result) and not FNameDeleted then
    begin
      // Synthesize from FName: { writable: false, enumerable: false, configurable: true }
      if (FName = '') or (FName = '<anonymous>') then
        Result := TGocciaPropertyDescriptorData.Create(
          TGocciaStringLiteralValue.Create(''), [pfConfigurable])
      else
        Result := TGocciaPropertyDescriptorData.Create(
          TGocciaStringLiteralValue.Create(FName), [pfConfigurable]);
    end;
  end
  else if AName = PROP_LENGTH then
  begin
    // Honour explicit own-property redefinitions (length is configurable, so
    // userland may override via Object.defineProperty); fall back to a
    // synthesized descriptor only when no own descriptor exists.
    Result := inherited GetOwnPropertyDescriptor(AName);
    if not Assigned(Result) and not FLengthDeleted then
      Result := TGocciaPropertyDescriptorData.Create(
        TGocciaNumberLiteralValue.Create(GetClassLength), [pfConfigurable]);
  end
  else
    Result := inherited GetOwnPropertyDescriptor(AName);
end;

function TGocciaClassValue.GetAllPropertyNames: TArray<string>;
var
  OwnNames: TArray<string>;
  Count: Integer;
  I: Integer;

  function IsArrayIndexKey(const AName: string): Boolean;
  var
    Digit, Index: UInt64;
    K: Integer;
  begin
    Index := 0;
    Result := False;
    if AName = '' then
      Exit;
    if (Length(AName) > 1) and (AName[1] = '0') then
      Exit;
    for K := 1 to Length(AName) do
    begin
      if (AName[K] < '0') or (AName[K] > '9') then
        Exit;
      Digit := Ord(AName[K]) - Ord('0');
      if Index > (High(UInt32) - Digit) div 10 then
        Exit;
      Index := Index * 10 + Digit;
    end;
    Result := Index < High(UInt32);
  end;

  procedure AppendName(const AName: string);
  var
    J: Integer;
  begin
    for J := 0 to Count - 1 do
      if Result[J] = AName then
        Exit;
    if Count >= Length(Result) then
      SetLength(Result, Count + 8);
    Result[Count] := AName;
    Inc(Count);
  end;

begin
  OwnNames := inherited GetAllPropertyNames;
  SetLength(Result, Length(OwnNames) + 3);
  Count := 0;

  for I := 0 to High(OwnNames) do
    if IsArrayIndexKey(OwnNames[I]) then
      AppendName(OwnNames[I]);

  if not FLengthDeleted then
    AppendName(PROP_LENGTH);
  if not FNameDeleted then
    AppendName(PROP_NAME);
  AppendName(PROP_PROTOTYPE);

  for I := 0 to High(OwnNames) do
    if not IsArrayIndexKey(OwnNames[I]) then
      AppendName(OwnNames[I]);

  SetLength(Result, Count);
end;

function TGocciaClassValue.GetOwnPropertyKeys: TArray<string>;
begin
  Result := GetAllPropertyNames;
end;

function TGocciaClassValue.HasOwnProperty(const AName: string): Boolean;
begin
  if AName = PROP_PROTOTYPE then
    Result := True
  else if (AName = PROP_NAME) and not FNameDeleted then
    Result := True
  else if (AName = PROP_LENGTH) and not FLengthDeleted then
    Result := True
  else
    Result := inherited HasOwnProperty(AName);
end;

function TGocciaClassValue.DeleteProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  Descriptor := inherited GetOwnPropertyDescriptor(AName);
  if Assigned(Descriptor) then
  begin
    Result := inherited DeleteProperty(AName);
    if Result then
    begin
      if AName = PROP_NAME then
        FNameDeleted := True
      else if AName = PROP_LENGTH then
        FLengthDeleted := True;
    end;
    Exit;
  end;

  if AName = PROP_PROTOTYPE then
    Exit(False);

  if AName = PROP_NAME then
  begin
    FNameDeleted := True;
    Exit(True);
  end;

  if AName = PROP_LENGTH then
  begin
    FLengthDeleted := True;
    Exit(True);
  end;

  Result := inherited DeleteProperty(AName);
end;

function TGocciaClassValue.GetOwnStaticSymbolDescriptor(const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
begin
  Result := inherited GetOwnSymbolPropertyDescriptor(ASymbol);
end;

// ES2026 §10.2.1 [[Call]] step 4: a class constructor throws when called
// without new. The built-ins that convert or construct on a call (String,
// Number, Boolean, Array, Object) override this in their own class value, so
// the behaviour is fixed when the constructor is created and the class's
// name plays no part in it.
function TGocciaClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  PreviousRealm: TGocciaRealm;
begin
  Result := nil;
  PreviousRealm := CurrentRealm;
  if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
    SetCurrentRealm(FCreationRealm);
  try
    ThrowTypeError(Format(SErrorClassConstructorRequiresNew, [FName]),
      SSuggestRequiresNew);
  finally
    if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
      SetCurrentRealm(PreviousRealm);
  end;
end;

{ TGocciaObjectClassValue }

// Boxes a primitive in the realm that created Object, so the wrapper's
// prototype comes from that realm.
function BoxInCreationRealm(const AValue: TGocciaValue;
  const ACreationRealm: TGocciaRealm): TGocciaValue;
var
  PreviousRealm: TGocciaRealm;
begin
  PreviousRealm := CurrentRealm;
  if Assigned(ACreationRealm) and (ACreationRealm <> PreviousRealm) then
    SetCurrentRealm(ACreationRealm);
  try
    Result := AValue.Box;
  finally
    if Assigned(ACreationRealm) and (ACreationRealm <> PreviousRealm) then
      SetCurrentRealm(PreviousRealm);
  end;
end;

// ES2026 §20.1.1.1 Object ( [ value ] ): direct Object construction boxes
// primitive inputs and returns object inputs unchanged. A construction with
// another NewTarget (a subclass of Object) allocates through it.
function TGocciaObjectClassValue.Instantiate(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if (not Assigned(ANewTarget) or (ANewTarget = Self)) and
     (AArguments.Length > 0) then
  begin
    Value := AArguments.GetElement(0);
    if not (Value is TGocciaUndefinedLiteralValue) and
       not (Value is TGocciaNullLiteralValue) then
    begin
      if Value.IsPrimitive then
        Exit(BoxInCreationRealm(Value, FCreationRealm));
      if Value is TGocciaObjectValue then
        Exit(Value);
    end;
  end;
  Result := inherited Instantiate(AArguments, ANewTarget);
end;

function TGocciaObjectClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Instantiate(AArguments);
end;

{ TGocciaArrayClassValue }

function TGocciaArrayClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaArrayValue.Create;
end;

function TGocciaArrayClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := Instantiate(AArguments);
end;

function TGocciaArrayClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaMapClassValue }

function TGocciaMapClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaMapValue.Create;
end;

function TGocciaMapClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := TGocciaMapValue.GetSharedPrototypeForRealm(ARealm);
end;

function TGocciaMapClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := True;
end;

{ TGocciaSetClassValue }

function TGocciaSetClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaSetValue.Create;
end;

function TGocciaSetClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := TGocciaSetValue.GetSharedPrototypeForRealm(ARealm);
end;

function TGocciaSetClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := True;
end;

{ TGocciaWeakMapClassValue }

function TGocciaWeakMapClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaWeakMapValue.Create;
end;

function TGocciaWeakMapClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := TGocciaWeakMapValue.GetSharedPrototypeForRealm(ARealm);
end;

function TGocciaWeakMapClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := True;
end;

{ TGocciaWeakSetClassValue }

function TGocciaWeakSetClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaWeakSetValue.Create;
end;

function TGocciaWeakSetClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := TGocciaWeakSetValue.GetSharedPrototypeForRealm(ARealm);
end;

function TGocciaWeakSetClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := True;
end;

{ TGocciaWeakRefClassValue }

function TGocciaWeakRefClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaWeakRefValue.Create;
end;

function TGocciaWeakRefClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := TGocciaWeakRefValue.GetSharedPrototypeForRealm(ARealm);
end;

function TGocciaWeakRefClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := True;
end;

function TGocciaWeakRefClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaFinalizationRegistryClassValue }

function TGocciaFinalizationRegistryClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaFinalizationRegistryValue.Create;
end;

function TGocciaFinalizationRegistryClassValue.IntrinsicPrototypeForRealm(
  const ARealm: TGocciaRealm): TGocciaObjectValue;
begin
  Result := TGocciaFinalizationRegistryValue.GetSharedPrototypeForRealm(ARealm);
end;

function TGocciaFinalizationRegistryClassValue.SupportsRealmIntrinsicPrototypeFallback: Boolean;
begin
  Result := True;
end;

function TGocciaFinalizationRegistryClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaArrayBufferClassValue }

function TGocciaArrayBufferClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaArrayBufferValue.Create(nil);
end;

function TGocciaArrayBufferClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaSharedArrayBufferClassValue }

function TGocciaSharedArrayBufferClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaSharedArrayBufferValue.Create(nil);
end;

function TGocciaSharedArrayBufferClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaDataViewClassValue }

function TGocciaDataViewClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaDataViewValue.Create(TGocciaClassValue(nil));
end;

function TGocciaDataViewClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaTextEncoderClassValue }

function TGocciaTextEncoderClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaTextEncoderValue.Create;
end;

{ TGocciaTextDecoderClassValue }

function TGocciaTextDecoderClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaTextDecoderValue.Create;
end;

{ TGocciaURLClassValue }

function TGocciaURLClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaURLValue.Create(nil);
end;

function TGocciaURLClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaURLSearchParamsClassValue }

function TGocciaURLSearchParamsClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaURLSearchParamsValue.Create(nil);
end;

{ TGocciaHeadersClassValue }

function TGocciaHeadersClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaHeadersValue.Create;
end;

{ TGocciaResponseClassValue }

function TGocciaResponseClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := TGocciaResponseValue.Create;
end;

{ TGocciaStringClassValue }

function TGocciaStringClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaStringLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaStringLiteralValue.Create('')
  else
    Prim := ToStringConstructorValue(AArguments.GetElement(0), False);
  Result := Prim.Box;
end;

// Symbol returns SymbolDescriptiveString here, unlike new String(symbol).
function TGocciaStringClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArguments.Length = 0 then
    Result := TGocciaStringLiteralValue.Create('')
  else
    Result := ToStringConstructorValue(AArguments.GetElement(0), True);
end;

function TGocciaStringClassValue.DefaultPrototypeForNewTarget(
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := DefaultNativePrototypeForNewTarget(ANewTarget,
    ACurrentRealmDefault, CONSTRUCTOR_STRING);
end;

function TGocciaStringClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaNumberClassValue }

function TGocciaNumberClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaNumberLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaNumberLiteralValue.ZeroValue
  else
    Prim := ToNumberConstructorValue(AArguments.GetElement(0));
  Result := Prim.Box;
end;

function TGocciaNumberClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArguments.Length = 0 then
    Result := TGocciaNumberLiteralValue.ZeroValue
  else
    Result := ToNumberConstructorValue(AArguments.GetElement(0));
end;

function TGocciaNumberClassValue.DefaultPrototypeForNewTarget(
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := DefaultNativePrototypeForNewTarget(ANewTarget,
    ACurrentRealmDefault, CONSTRUCTOR_NUMBER);
end;

function TGocciaNumberClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaBooleanClassValue }

function TGocciaBooleanClassValue.CreateNativeInstance(const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
var
  Prim: TGocciaBooleanLiteralValue;
begin
  if AArguments.Length = 0 then
    Prim := TGocciaBooleanLiteralValue.FalseValue
  else
    Prim := AArguments.GetElement(0).ToBooleanLiteral;
  Result := Prim.Box;
end;

function TGocciaBooleanClassValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArguments.Length = 0 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := AArguments.GetElement(0).ToBooleanLiteral;
end;

function TGocciaBooleanClassValue.DefaultPrototypeForNewTarget(
  const ANewTarget: TGocciaValue;
  const ACurrentRealmDefault: TGocciaObjectValue): TGocciaObjectValue;
begin
  Result := DefaultNativePrototypeForNewTarget(ANewTarget,
    ACurrentRealmDefault, CONSTRUCTOR_BOOLEAN);
end;

function TGocciaBooleanClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaFunctionConstructorClassValue }

constructor TGocciaFunctionConstructorClassValue.Create(const AName: string;
  const ASuperClass: TGocciaClassValue;
  const ADynamicKind: TGocciaDynamicFunctionKind);
begin
  inherited Create(AName, ASuperClass);
  FEnabled := False;
  FCapabilityAuditEmitter := nil;
  FCompileDynamicFunction := nil;
  FDynamicKind := ADynamicKind;
end;

function TGocciaFunctionConstructorClassValue.BuildFunction(
  const AArguments: TGocciaArgumentsCollection): TGocciaFunctionBase;
var
  ParamSources: array of string;
  BodyStr: string;
  I: Integer;
  PrototypeDescriptor: TGocciaPropertyDescriptor;
  PrototypeValue: TGocciaValue;
  PreviousRealm: TGocciaRealm;
begin
  if not FEnabled then
  begin
    if Assigned(FCapabilityAuditEmitter) then
      FCapabilityAuditEmitter(gckFunctionConstructor, gcdDeny, Name,
        'dynamic code generation is disabled');
    ThrowTypeError('Dynamic code generation is disabled. ' +
      'Pass --unsafe-function-constructor to enable the Function constructor');
  end;

  if Assigned(FCapabilityAuditEmitter) then
    FCapabilityAuditEmitter(gckFunctionConstructor, gcdAllow, Name,
      'dynamic code generation is enabled');

  if not Assigned(FCompileDynamicFunction) then
    ThrowTypeError('Function constructor is not available in this environment');

  // ES2026 §20.2.1.1: collect parameters and body from arguments
  if AArguments.Length = 0 then
  begin
    SetLength(ParamSources, 0);
    BodyStr := '';
  end
  else if AArguments.Length = 1 then
  begin
    SetLength(ParamSources, 0);
    BodyStr := AArguments.GetElement(0).ToStringLiteral.Value;
  end
  else
  begin
    SetLength(ParamSources, AArguments.Length - 1);
    for I := 0 to AArguments.Length - 2 do
      ParamSources[I] := AArguments.GetElement(I).ToStringLiteral.Value;
    BodyStr := AArguments.GetElement(AArguments.Length - 1).ToStringLiteral.Value;
  end;

  PreviousRealm := CurrentRealm;
  if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
    SetCurrentRealm(FCreationRealm);
  try
    Result := FCompileDynamicFunction(ParamSources, BodyStr, FDynamicKind);
  finally
    if Assigned(FCreationRealm) and (FCreationRealm <> PreviousRealm) then
      SetCurrentRealm(PreviousRealm);
  end;
  if FDynamicKind in [dfkGenerator, dfkAsyncGenerator] then
  begin
    PrototypeDescriptor := Result.GetOwnPropertyDescriptor(PROP_PROTOTYPE);
    if PrototypeDescriptor is TGocciaPropertyDescriptorData then
    begin
      PrototypeValue := TGocciaPropertyDescriptorData(
        PrototypeDescriptor).Value;
      PrototypeDescriptor.Free;
      Result.Properties.Add(PROP_PROTOTYPE,
        TGocciaPropertyDescriptorData.Create(PrototypeValue, [pfWritable]));
    end;
  end;
end;

function TGocciaFunctionConstructorClassValue.Call(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  // ES2026 §20.2.1.1: Function(...) is equivalent to new Function(...)
  Result := BuildFunction(AArguments);
end;

function TGocciaFunctionConstructorClassValue.CreateNativeInstance(
  const AArguments: TGocciaArgumentsCollection): TGocciaObjectValue;
begin
  Result := BuildFunction(AArguments);
end;

function TGocciaFunctionConstructorClassValue.GetClassLength: Integer;
begin
  Result := 1;
end;

{ TGocciaInstanceValue }

constructor TGocciaInstanceValue.Create(const AClass: TGocciaClassValue = nil;
  const APropertyCapacity: Integer = 0);
begin
  inherited Create(nil, APropertyCapacity);
  FClass := AClass;
end;

function TGocciaInstanceValue.TypeName: string;
begin
  if Assigned(FClass) then
    Result := FClass.Name
  else
    Result := CONSTRUCTOR_OBJECT;
end;

function TGocciaInstanceValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaInstanceValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
var
  Descriptor: TGocciaPropertyDescriptor;
  Args: TGocciaArgumentsCollection;
begin
  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    // A lazy placeholder (a member of a built-in prototype that a shim
    // provides, such as Number.prototype.toLocaleString) is built first.
    if Descriptor.ClassType = TGocciaLazyPropertyDescriptorData then
      Descriptor := MaterializeOwnLazyProperty(AName, Descriptor);
    if Descriptor is TGocciaPropertyDescriptorData then
      Result := TGocciaPropertyDescriptorData(Descriptor).Value
    else if Descriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Getter) and
         TGocciaPropertyDescriptorAccessor(Descriptor).Getter.IsCallable then
      begin
        Args := TGocciaArgumentsCollection.CreateWithCapacity(0);
        try
          // A getter is any callable, a callable Proxy included; only a
          // function object takes the direct path.
          if TGocciaPropertyDescriptorAccessor(Descriptor).Getter is TGocciaFunctionBase then
            Result := TGocciaFunctionBase(
              TGocciaPropertyDescriptorAccessor(Descriptor).Getter).Call(
              Args, AThisContext)
          else
            Result := InvokeCallable(
              TGocciaPropertyDescriptorAccessor(Descriptor).Getter, Args,
              AThisContext);
        finally
          Args.Free;
        end;
      end
      else
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    end
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  // Check the prototype chain with the receiver as context.
  Result := GetPropertyFromPrototype(AName, AThisContext);
end;

procedure TGocciaInstanceValue.AssignProperty(const AName: string; const AValue: TGocciaValue; const ACanCreate: Boolean = True);
var
  Descriptor: TGocciaPropertyDescriptor;
  Args: TGocciaArgumentsCollection;
  Proto: TGocciaObjectValue;
begin
  if FProperties.TryGetValue(AName, Descriptor) then
  begin
    if Descriptor.ClassType = TGocciaLazyPropertyDescriptorData then
      Descriptor := MaterializeOwnLazyPropertyHolding(AName, Descriptor,
        AValue, Self);
    if Descriptor is TGocciaPropertyDescriptorData then
    begin
      if not TGocciaPropertyDescriptorData(Descriptor).Writable then
        ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]), SSuggestCannotDeleteNonConfigurable);
      TGocciaPropertyDescriptorData(Descriptor).Value := AValue;
      Exit;
    end;
    // ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor steps 3-7: an own accessor
    // is assigned through its setter and refused when it has none. The
    // inherited method does both before it reaches the prototype chain.
    inherited AssignProperty(AName, AValue, ACanCreate);
    Exit;
  end;

  Proto := FPrototype;
  while Assigned(Proto) do
  begin
    if not UsesOrdinarySet(Proto) then
    begin
      AssignThroughExoticParent(Proto, AName, AValue);
      Exit;
    end;
    Descriptor := Proto.GetOwnPropertyDescriptorHolding(AName, AValue, Self);
    if Assigned(Descriptor) then
    begin
      if (Descriptor is TGocciaPropertyDescriptorAccessor) and
         Assigned(TGocciaPropertyDescriptorAccessor(Descriptor).Setter) and
         TGocciaPropertyDescriptorAccessor(Descriptor).Setter.IsCallable then
      begin
        Args := TGocciaArgumentsCollection.CreateWithCapacity(1);
        try
          Args.Add(AValue);
          // A setter is any callable, a callable Proxy included; only a
          // function object takes the direct path.
          if TGocciaPropertyDescriptorAccessor(Descriptor).Setter is TGocciaFunctionBase then
            TGocciaFunctionBase(
              TGocciaPropertyDescriptorAccessor(Descriptor).Setter).Call(
              Args, Self)
          else
            InvokeCallable(TGocciaPropertyDescriptorAccessor(Descriptor).Setter,
              Args, Self);
        finally
          Args.Free;
        end;
        Exit;
      end
      else if Descriptor is TGocciaPropertyDescriptorAccessor then
      begin
        ThrowTypeError(Format(SErrorSetPropertyOnlyGetter, [AName, ToStringTag]), SSuggestPropertyHasOnlyGetter);
      end
      else if (Descriptor is TGocciaPropertyDescriptorData) and
              (not TGocciaPropertyDescriptorData(Descriptor).Writable) then
        ThrowTypeError(Format(SErrorCannotAssignReadOnly, [AName]), SSuggestCannotDeleteNonConfigurable);
    end;
    Proto := Proto.Prototype;
  end;

  if not ACanCreate then
    ThrowTypeError(Format(SErrorCannotAssignNonExistent, [AName]), SSuggestCannotDeleteNonConfigurable);

  if not FExtensible then
    ThrowTypeError(Format(SErrorCannotAddPropertyNotExtensible, [AName]), SSuggestObjectNotExtensible);

  DefineProperty(AName, TGocciaPropertyDescriptorData.Create(AValue,
    [pfEnumerable, pfConfigurable, pfWritable]));
end;

procedure TGocciaInstanceValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  // Delegate to AssignProperty which has the setter logic
  AssignProperty(AName, AValue);
end;

function TGocciaInstanceValue.TryGetRawPrivateProperty(const AKey: string;
  out AValue: TGocciaValue): Boolean;
begin
  Result := Assigned(FPrivateProperties) and
    FPrivateProperties.TryGetValue(AKey, AValue);
end;

procedure TGocciaInstanceValue.SetRawPrivateProperty(const AKey: string;
  const AValue: TGocciaValue);
begin
  if not Assigned(FPrivateProperties) then
    FPrivateProperties := TGocciaValueMap.Create;
  FPrivateProperties.AddOrSetValue(AKey, AValue);
end;

procedure TGocciaInstanceValue.InitializeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
begin
  // No-op by default; native subclasses override to handle constructor arguments
end;

procedure TGocciaInstanceValue.FinalizeNativeFromArguments(const AArguments: TGocciaArgumentsCollection);
begin
  // No-op by default; native subclasses override when setup must finish after prototype lookup
end;

destructor TGocciaInstanceValue.Destroy;
begin
  FPrivateProperties.Free;
  inherited;
end;

procedure TGocciaInstanceValue.MarkReferences;
var
  ValPair: TGocciaValueMap.TKeyValuePair;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FClass) then
    FClass.MarkReferences;

  if Assigned(FPrivateProperties) then
    for ValPair in FPrivateProperties do
      if Assigned(ValPair.Value) then
        ValPair.Value.MarkReferences;
end;


initialization
  RegisterOrdinaryObjectClass(TGocciaInstanceValue);

end.
