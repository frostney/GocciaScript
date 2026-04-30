unit Goccia.Scope.BindingMap;

{$I Goccia.inc}

interface

uses
  OrderedStringMap,

  Goccia.Bytecode.Chunk,
  Goccia.Values.Primitives;

type
  TGocciaStringArray = array of string;

  TGocciaDeclarationType = (dtLet, dtConst, dtParameter, dtVar);

  TLexicalBinding = record
  private
    function IsWritable: Boolean;
    function CanAccess: Boolean;
  public
    Value: TGocciaValue;
    DeclarationType: TGocciaDeclarationType;
    Initialized: Boolean;
    { Strict-types annotation enforced on every assignment.  Default
      sltUntyped means no enforcement (typical untyped binding). }
    TypeHint: TGocciaLocalType;

    property Writable: Boolean read IsWritable;
    property IsAccessible: Boolean read CanAccess;
  end;

  TGocciaScopeBindingMap = TOrderedStringMap<TLexicalBinding>;

implementation

{ TLexicalBinding }

function TLexicalBinding.IsWritable: Boolean;
begin
  Result := DeclarationType in [dtLet, dtParameter, dtVar];
end;

function TLexicalBinding.CanAccess: Boolean;
begin
  Result := Initialized or (DeclarationType in [dtParameter, dtVar]);
end;

end.
