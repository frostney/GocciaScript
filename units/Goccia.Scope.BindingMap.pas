unit Goccia.Scope.BindingMap;

{$I Goccia.inc}

interface

uses
  ScopeMap,

  Goccia.Values.Primitives;

type
  TGocciaStringArray = array of string;

  TGocciaDeclarationType = (dtLet, dtConst, dtParameter);

  TLexicalBinding = record
  private
    function IsWritable: Boolean;
    function CanAccess: Boolean;
  public
    Value: TGocciaValue;
    DeclarationType: TGocciaDeclarationType;
    Initialized: Boolean;

    property Writable: Boolean read IsWritable;
    property IsAccessible: Boolean read CanAccess;
  end;

  TGocciaScopeBindingMap = TScopeMap<TLexicalBinding>;

implementation

{ TLexicalBinding }

function TLexicalBinding.IsWritable: Boolean;
begin
  Result := DeclarationType in [dtLet, dtParameter];
end;

function TLexicalBinding.CanAccess: Boolean;
begin
  Result := Initialized or (DeclarationType = dtParameter);
end;

end.
