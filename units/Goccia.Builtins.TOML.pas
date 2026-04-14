unit Goccia.Builtins.TOML;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.TOML,
  Goccia.Values.Primitives;

type
  TGocciaTOMLBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaTOMLParser;
  published
    function TOMLParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaTOMLBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaTOMLParser.Create;

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(TOMLParse, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('TOML'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaTOMLBuiltin.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TGocciaTOMLBuiltin.TOMLParse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'TOML.parse', ThrowError);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('TOML.parse: argument must be a string', 0, 0);

  try
    Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
  except
    on E: EGocciaTOMLParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

end.
