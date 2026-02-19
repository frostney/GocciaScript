unit Goccia.Builtins.JSON;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.JSON,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaJSONBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaJSONParser;
    FStringifier: TGocciaJSONStringifier;
  protected
    function JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

constructor TGocciaJSONBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaJSONParser.Create;
  FStringifier := TGocciaJSONStringifier.Create;

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONParse, 'parse', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(JSONStringify, 'stringify', 1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaJSONBuiltin.Destroy;
begin
  FParser.Free;
  FStringifier.Free;
  inherited;
end;

function TGocciaJSONBuiltin.JSONParse(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireExactly(AArgs, 1, 'JSON.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('JSON.parse: argument must be a string', 0, 0);

  try
    Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
  except
    on E: Exception do
      ThrowError('JSON.parse error: ' + E.Message, 0, 0);
  end;
end;

function TGocciaJSONBuiltin.JSONStringify(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSON.stringify', ThrowError);

  Value := AArgs.GetElement(0);

  if Value is TGocciaUndefinedLiteralValue then
  begin
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  try
    Result := TGocciaStringLiteralValue.Create(FStringifier.Stringify(Value));
  except
    on E: Exception do
      ThrowError('JSON.stringify error: ' + E.Message, 0, 0);
  end;
end;

end.
