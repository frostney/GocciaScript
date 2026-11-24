unit Goccia.Scope.BindingMap;

{$I Goccia.inc}

interface

uses
  OrderedStringMap,

  Goccia.AST.Node,
  Goccia.Bytecode.Chunk,
  Goccia.Values.Primitives;

type
  TGocciaStringArray = array of string;

  { declared with the AST (Goccia.AST.Node), which the front end shares without the runtime; named here too for
    the runtime's units }
  TGocciaDeclarationType = Goccia.AST.Node.TGocciaDeclarationType;

const
  dtLet = Goccia.AST.Node.dtLet;
  dtConst = Goccia.AST.Node.dtConst;
  dtParameter = Goccia.AST.Node.dtParameter;
  dtVar = Goccia.AST.Node.dtVar;

type

  TLexicalBinding = record
  private
    function IsWritable: Boolean;
    function CanAccess: Boolean;
  public
    Value: TGocciaValue;
    DeclarationType: TGocciaDeclarationType;
    Initialized: Boolean;
    BuiltIn: Boolean;
    GlobalObjectBacked: Boolean;
    CanDelete: Boolean;
    { Strict-types annotation enforced on every assignment.  Default
      sltUntyped means no enforcement (typical untyped binding). }
    TypeHint: TGocciaLocalType;

    property Writable: Boolean read IsWritable;
    property IsAccessible: Boolean read CanAccess;
  end;

  PLexicalBinding = ^TLexicalBinding;

  TGocciaScopeBindingMap = TOrderedStringMap<TLexicalBinding>;

implementation

{ TLexicalBinding }

function TLexicalBinding.IsWritable: Boolean;
begin
  Result := DeclarationType in [dtLet, dtParameter, dtVar];
end;

function TLexicalBinding.CanAccess: Boolean;
begin
  Result := Initialized or (DeclarationType = dtVar);
end;

end.
