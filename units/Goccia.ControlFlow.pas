unit Goccia.ControlFlow;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaControlFlowKind = (cfkNormal, cfkReturn, cfkBreak);

  TGocciaControlFlow = record
    Kind: TGocciaControlFlowKind;
    Value: TGocciaValue;
    class function Normal(const AValue: TGocciaValue): TGocciaControlFlow; static; inline;
    class function Return(const AValue: TGocciaValue): TGocciaControlFlow; static; inline;
    class function Break: TGocciaControlFlow; static; inline;
  end;

implementation

class function TGocciaControlFlow.Normal(const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.Kind := cfkNormal;
  Result.Value := AValue;
end;

class function TGocciaControlFlow.Return(const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.Kind := cfkReturn;
  Result.Value := AValue;
end;

class function TGocciaControlFlow.Break: TGocciaControlFlow;
begin
  Result.Kind := cfkBreak;
  Result.Value := nil;
end;

end.
