unit Souffle.VM.Upvalue;

{$I Souffle.inc}

interface

uses
  Souffle.Heap,
  Souffle.Value;

type
  TSouffleUpvalue = class(TSouffleHeapObject)
  private
    FIsOpen: Boolean;
    FRegisterIndex: Integer;
    FClosed: TSouffleValue;
    FNext: TSouffleUpvalue;
  public
    constructor Create(const ARegisterIndex: Integer);

    procedure Close(const AValue: TSouffleValue);
    procedure MarkReferences; override;
    function DebugString: string; override;

    property IsOpen: Boolean read FIsOpen;
    property RegisterIndex: Integer read FRegisterIndex;
    property Closed: TSouffleValue read FClosed write FClosed;
    property Next: TSouffleUpvalue read FNext write FNext;
  end;

implementation

uses
  SysUtils;

{ TSouffleUpvalue }

constructor TSouffleUpvalue.Create(const ARegisterIndex: Integer);
begin
  inherited Create(SOUFFLE_HEAP_UPVALUE);
  FIsOpen := True;
  FRegisterIndex := ARegisterIndex;
  FClosed := SouffleNil;
  FNext := nil;
end;

procedure TSouffleUpvalue.Close(const AValue: TSouffleValue);
begin
  FClosed := AValue;
  FIsOpen := False;
end;

procedure TSouffleUpvalue.MarkReferences;
begin
  inherited;
  if not FIsOpen and SouffleIsReference(FClosed) and
     Assigned(FClosed.AsReference) then
    FClosed.AsReference.MarkReferences;
  if Assigned(FNext) then
    FNext.MarkReferences;
end;

function TSouffleUpvalue.DebugString: string;
begin
  if FIsOpen then
    Result := '<upvalue:open:R[' + IntToStr(FRegisterIndex) + ']>'
  else
    Result := '<upvalue:closed:' + SouffleValueToString(FClosed) + '>';
end;

end.
