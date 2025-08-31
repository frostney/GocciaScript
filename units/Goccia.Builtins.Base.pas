unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Error, Goccia.Scope, SysUtils, Generics.Collections, Goccia.Arguments, Goccia.Values.Primitives;

type
  TGocciaBuiltin = class
  protected
    FName: string;
    FScope: TGocciaScope;
    FBuiltinObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowError;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
    destructor Destroy; override;

    // Helper method for creating TGocciaArguments with inferred function name
    function CreateArguments(Args: TObjectList<TGocciaValue>; const MethodName: string = ''): TGocciaArguments;

    property Name: string read FName;
    property BuiltinObject: TGocciaObjectValue read FBuiltinObject;
    property ThrowError: TGocciaThrowError read FThrowError;
  end;

implementation

constructor TGocciaBuiltin.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  FName := AName;
  FScope := AScope;
  FBuiltinObject := TGocciaObjectValue.Create;
  FThrowError := AThrowError;
end;

destructor TGocciaBuiltin.Destroy;
begin
  FBuiltinObject.Free;
  inherited;
end;

function TGocciaBuiltin.CreateArguments(Args: TObjectList<TGocciaValue>; const MethodName: string): TGocciaArguments;
begin
  if MethodName = '' then
    Result := TGocciaArguments.Create(Args, FName, FThrowError)
  else
    Result := TGocciaArguments.Create(Args, FName + '.' + MethodName, FThrowError);
end;

end.
