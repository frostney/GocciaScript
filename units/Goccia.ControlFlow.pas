unit Goccia.ControlFlow;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaControlFlowKind = (cfkNormal, cfkReturn, cfkBreak);

  // Tagged-pointer representation: the control flow kind is encoded in the low
  // 2 bits of the value pointer. TGocciaValue instances are always 16-byte
  // aligned on 64-bit platforms, so the low 4 bits are guaranteed to be zero.
  // This makes the record 8 bytes (single register) with zero overhead for the
  // Normal case — Normal(value) is just the raw pointer, no bit manipulation.
  TGocciaControlFlow = record
  private
    FBits: PtrUInt;
    function GetKind: TGocciaControlFlowKind; inline;
    function GetValue: TGocciaValue; inline;
  public
    class function Normal(const AValue: TGocciaValue): TGocciaControlFlow; static; inline;
    class function Return(const AValue: TGocciaValue): TGocciaControlFlow; static; inline;
    class function Break: TGocciaControlFlow; static; inline;
    property Kind: TGocciaControlFlowKind read GetKind;
    property Value: TGocciaValue read GetValue;
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
end;

class function TGocciaControlFlow.Return(const AValue: TGocciaValue): TGocciaControlFlow;
begin
  Result.FBits := PtrUInt(AValue) or 1;
end;

class function TGocciaControlFlow.Break: TGocciaControlFlow;
begin
  Result.FBits := 2;
end;

end.
