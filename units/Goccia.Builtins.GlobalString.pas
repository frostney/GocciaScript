unit Goccia.Builtins.GlobalString;

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
  TGocciaGlobalString = class(TGocciaBuiltin)
  private
    class var FStaticMembers: array of TGocciaMemberDefinition;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  published
    function StringFromCharCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringFromCodePoint(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

constructor TGocciaGlobalString.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(StringFromCharCode, 1, gmkStaticMethod);
    Members.AddMethod(StringFromCodePoint, 1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
end;

// ES2026 §22.1.2.1 String.fromCharCode(...codeUnits)
function TGocciaGlobalString.StringFromCharCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultStr: string;
  I, CodeUnit: Integer;
begin
  ResultStr := '';
  I := 0;
  while I < AArgs.Length do
  begin
    CodeUnit := Trunc(AArgs.GetElement(I).ToNumberLiteral.Value);
    CodeUnit := CodeUnit and $FFFF;
    if CodeUnit < $80 then
      ResultStr := ResultStr + Chr(CodeUnit)
    else if CodeUnit < $800 then
      ResultStr := ResultStr + Chr($C0 or (CodeUnit shr 6)) + Chr($80 or (CodeUnit and $3F))
    else
      ResultStr := ResultStr + Chr($E0 or (CodeUnit shr 12)) + Chr($80 or ((CodeUnit shr 6) and $3F)) + Chr($80 or (CodeUnit and $3F));
    Inc(I);
  end;
  Result := TGocciaStringLiteralValue.Create(ResultStr);
end;

// ES2026 §22.1.2.2 String.fromCodePoint(...codePoints)
function TGocciaGlobalString.StringFromCodePoint(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  ResultStr: string;
  I: Integer;
  NumArg: TGocciaNumberLiteralValue;
  RawValue: Double;
  CodePoint: Cardinal;
begin
  ResultStr := '';
  I := 0;
  while I < AArgs.Length do
  begin
    NumArg := AArgs.GetElement(I).ToNumberLiteral;
    if NumArg.IsNaN or NumArg.IsInfinity or NumArg.IsNegativeInfinity then
      ThrowRangeError('Invalid code point');
    RawValue := NumArg.Value;
    if (RawValue < 0) or (RawValue > $10FFFF) or (RawValue <> Trunc(RawValue)) then
      ThrowRangeError(FloatToStr(RawValue) + ' is not a valid code point');
    CodePoint := Trunc(RawValue);

    if CodePoint < $80 then
      ResultStr := ResultStr + Chr(CodePoint)
    else if CodePoint < $800 then
      ResultStr := ResultStr + Chr($C0 or (CodePoint shr 6)) + Chr($80 or (CodePoint and $3F))
    else if CodePoint < $10000 then
      ResultStr := ResultStr + Chr($E0 or (CodePoint shr 12)) + Chr($80 or ((CodePoint shr 6) and $3F)) + Chr($80 or (CodePoint and $3F))
    else
      ResultStr := ResultStr + Chr($F0 or (CodePoint shr 18)) + Chr($80 or ((CodePoint shr 12) and $3F)) + Chr($80 or ((CodePoint shr 6) and $3F)) + Chr($80 or (CodePoint and $3F));
    Inc(I);
  end;
  Result := TGocciaStringLiteralValue.Create(ResultStr);
end;

end.
