unit Goccia.Builtins.YAML;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives,
  Goccia.YAML;

type
  TGocciaYAMLBuiltin = class(TGocciaBuiltin)
  private
    class var FStaticMembers: array of TGocciaMemberDefinition;
    FParser: TGocciaYAMLParser;
  published
    function YAMLParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function YAMLParseDocuments(const AArgs: TGocciaArgumentsCollection;
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

constructor TGocciaYAMLBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaYAMLParser.Create;

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(YAMLParse, 1, gmkStaticMethod);
    Members.AddMethod(YAMLParseDocuments, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('YAML'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaYAMLBuiltin.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TGocciaYAMLBuiltin.YAMLParse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'YAML.parse', ThrowError);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('YAML.parse: argument must be a string', 0, 0);

  try
    Result := FParser.Parse(AArgs.GetElement(0).ToStringLiteral.Value);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaYAMLBuiltin.YAMLParseDocuments(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'YAML.parseDocuments', ThrowError);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowError('YAML.parseDocuments: argument must be a string', 0, 0);

  try
    Result := FParser.ParseDocuments(AArgs.GetElement(0).ToStringLiteral.Value);
  except
    on E: Exception do
      ThrowSyntaxError(E.Message);
  end;
end;

end.
