unit Goccia.Builtins.GlobalFFI;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalFFI = class(TGocciaBuiltin)
  private
    class var FStaticMembers: array of TGocciaMemberDefinition;
  published
    function FFIOpen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIAlloc(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFIFree(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FFINullptrGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils,

  Goccia.FFI.DynamicLibrary,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FFILibrary,
  Goccia.Values.FFIPointer,
  Goccia.Values.ObjectPropertyDescriptor;

constructor TGocciaGlobalFFI.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('open', FFIOpen, 1, gmkStaticMethod);
    Members.AddNamedMethod('alloc', FFIAlloc, 1, gmkStaticMethod);
    Members.AddNamedMethod('free', FFIFree, 1, gmkStaticMethod);
    Members.AddAccessor('nullptr', FFINullptrGetter, nil, [pfConfigurable], gmkStaticGetter);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtConst);
end;

function TGocciaGlobalFFI.FFIOpen(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LibPath: string;
  Handle: TGocciaFFILibraryHandle;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('FFI.open requires a library path');

  LibPath := AArgs.GetElement(0).ToStringLiteral.Value;

  try
    Handle := TGocciaFFILibraryHandle.Create(LibPath);
  except
    on E: Exception do
      ThrowTypeError(E.Message);
  end;

  Result := TGocciaFFILibraryValue.Create(Handle);
end;

function TGocciaGlobalFFI.FFIAlloc(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Num: TGocciaNumberLiteralValue;
  Size: Integer;
  Mem: Pointer;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('FFI.alloc requires a size argument');

  Num := AArgs.GetElement(0).ToNumberLiteral;
  if Num.IsNaN or Num.IsInfinite or (Num.Value < 0) or (Num.Value <> Trunc(Num.Value)) then
    ThrowRangeError('Invalid allocation size');

  Size := Trunc(Num.Value);
  if Size = 0 then
    ThrowRangeError('Cannot allocate zero bytes');

  Mem := GetMem(Size);
  FillChar(Mem^, Size, 0);
  Result := TGocciaFFIPointerValue.Create(Mem, Size, fpoOwned);
end;

function TGocciaGlobalFFI.FFIFree(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Ptr: TGocciaFFIPointerValue;
begin
  if AArgs.Length < 1 then
    ThrowTypeError('FFI.free requires an FFIPointer argument');

  if not (AArgs.GetElement(0) is TGocciaFFIPointerValue) then
    ThrowTypeError('FFI.free requires an FFIPointer argument');

  Ptr := TGocciaFFIPointerValue(AArgs.GetElement(0));
  Ptr.FreeAllocation;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaGlobalFFI.FFINullptrGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaFFIPointerValue.NullPointer;
end;

end.
