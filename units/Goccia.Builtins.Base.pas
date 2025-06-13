unit Goccia.Builtins.Base;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue, Goccia.Error, Goccia.Scope, SysUtils;

type
  TGocciaBuiltin = class
  protected
    FName: string;
    FBuiltinObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowError;
  public
    constructor Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
    destructor Destroy; override;

    property Name: string read FName;
    property BuiltinObject: TGocciaObjectValue read FBuiltinObject;
    property ThrowError: TGocciaThrowError read FThrowError;
  end;

implementation

constructor TGocciaBuiltin.Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
begin
  FName := AName;
  FBuiltinObject := TGocciaObjectValue.Create;
  FThrowError := AThrowError;
end;

destructor TGocciaBuiltin.Destroy;
begin
  FBuiltinObject.Free;
  inherited;
end;

end.