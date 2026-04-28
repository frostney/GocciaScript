unit Goccia.Builtins.GlobalWeakSet;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope;

type
  TGocciaGlobalWeakSet = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalWeakSet.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
end;

end.
