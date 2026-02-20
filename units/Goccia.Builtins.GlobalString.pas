unit Goccia.Builtins.GlobalString;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalString = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);

    function StringFromCharCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringFromCodePoint(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction;

constructor TGocciaGlobalString.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringFromCharCode, 'fromCharCode', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(StringFromCodePoint, 'fromCodePoint', 1));
end;

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
