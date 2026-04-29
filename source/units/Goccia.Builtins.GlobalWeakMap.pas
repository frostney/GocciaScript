unit Goccia.Builtins.GlobalWeakMap;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope;

type
  TGocciaGlobalWeakMap = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalWeakMap.Create(const AName: string;
  const AScope: TGocciaScope;
  const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
end;

end.
