unit Goccia.ControlFlow;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaControlFlowKind = (cfkNormal, cfkReturn, cfkBreak, cfkContinue);

  // Tagged-pointer representation: the control flow kind is encoded in the low
  // 2 bits of the value pointer. TGocciaValue instances are always 16-byte
  // aligned on 64-bit platforms, so the low 4 bits are guaranteed to be zero.
  // This makes the record 8 bytes (single register) with zero overhead for the
  // Normal case — Normal(value) is just the raw pointer, no bit manipulation.
  TGocciaControlFlow = record
  private
    FBits: PtrUInt;
    FTargetLabel: string;
    function GetKind: TGocciaControlFlowKind; inline;
    function GetValue: TGocciaValue; inline;
  public
    class function Normal(const AValue: TGocciaValue): TGocciaControlFlow; static; inline;
    class function Return(const AValue: TGocciaValue): TGocciaControlFlow; static; inline;
    class function Empty: TGocciaControlFlow; static; inline;
    class function Break(const ATargetLabel: string = ''; const AValue: TGocciaValue = nil): TGocciaControlFlow; static; inline;
    class function Continue(const ATargetLabel: string = ''; const AValue: TGocciaValue = nil): TGocciaControlFlow; static; inline;
    function UpdateEmpty(const AValue: TGocciaValue): TGocciaControlFlow; inline;
    property Kind: TGocciaControlFlowKind read GetKind;
    property Value: TGocciaValue read GetValue;
    property TargetLabel: string read FTargetLabel;
  end;

implementation

function TGocciaControlFlow.GetKind: TGocciaControlFlowKind;
begin
  Result := TGocciaControlFlowKind(FBits and 3);
end;

function TGocciaControlFlow.GetValue: TGocciaValue;
begin
  Result := TGocciaValue(FBits and not PtrUInt(3));
end;

class function TGocciaControlFlow.Normal(const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.FBits := PtrUInt(AValue);
  Result.FTargetLabel := '';
end;

class function TGocciaControlFlow.Return(const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.FBits := PtrUInt(AValue) or 1;
  Result.FTargetLabel := '';
end;

class function TGocciaControlFlow.Empty: TGocciaControlFlow;
begin
  Result.FBits := 0;
  Result.FTargetLabel := '';
end;

class function TGocciaControlFlow.Break(
  const ATargetLabel: string; const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.FBits := PtrUInt(AValue) or 2;
  Result.FTargetLabel := ATargetLabel;
end;

class function TGocciaControlFlow.Continue(
  const ATargetLabel: string; const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.FBits := PtrUInt(AValue) or 3;
  Result.FTargetLabel := ATargetLabel;
end;

function TGocciaControlFlow.UpdateEmpty(
  const AValue: TGocciaValue): TGocciaControlFlow;
begin
  if Assigned(Value) then
  begin
    Result := Self;
    Exit;
  end;

  Result.FBits := PtrUInt(AValue) or (FBits and 3);
  Result.FTargetLabel := FTargetLabel;
end;

end.
