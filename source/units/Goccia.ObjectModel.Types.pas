unit Goccia.ObjectModel.Types;

{$I Goccia.inc}

interface

uses
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

type
  TGocciaMemberKind = (
    gmkPrototypeMethod,
    gmkPrototypeGetter,
    gmkPrototypeSetter,
    gmkStaticMethod,
    gmkStaticGetter,
    gmkStaticSetter,
    gmkSymbolProperty,
    gmkSymbolMethod,
    gmkSymbolAccessor,
    gmkDataProperty,
    gmkPropertyAlias,
    gmkSymbolAlias
  );

  TGocciaMemberFlag = (
    gmfNoFunctionPrototype,
    gmfVariadic,
    gmfNotConstructable
  );
  TGocciaMemberFlags = set of TGocciaMemberFlag;

  TGocciaMemberDefinition = record
    ExposedName: string;
    Symbol: TGocciaValue;
    Kind: TGocciaMemberKind;
    MethodName: string;
    Callback: TGocciaNativeFunctionCallback;
    SetterCallback: TGocciaNativeFunctionCallback;
    Arity: Integer;
    PropertyFlags: TPropertyFlags;
    MemberFlags: TGocciaMemberFlags;
    DataValue: TGocciaValue;
    AliasTargetName: string;
  end;

implementation

end.
