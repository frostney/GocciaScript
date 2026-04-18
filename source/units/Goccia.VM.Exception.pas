unit Goccia.VM.Exception;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.Primitives;

type
  TGocciaBytecodeHandlerEntry = record
    CatchIP: Integer;
    CatchRegister: UInt8;
    FrameDepth: Integer;
  end;

  TGocciaBytecodeHandlerStack = class
  private
    FEntries: array of TGocciaBytecodeHandlerEntry;
    FCount: Integer;
  public
    procedure Push(const ACatchIP: Integer; const ACatchRegister: UInt8;
      const AFrameDepth: Integer);
    procedure Pop;
    function Peek: TGocciaBytecodeHandlerEntry;
    function IsEmpty: Boolean;
    property Count: Integer read FCount;
  end;

  EGocciaBytecodeThrow = class(Exception)
  private
    FThrownValue: TGocciaValue;
  public
    constructor Create(const AThrownValue: TGocciaValue);
    property ThrownValue: TGocciaValue read FThrownValue;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.ObjectValue;

procedure TGocciaBytecodeHandlerStack.Push(const ACatchIP: Integer;
  const ACatchRegister: UInt8; const AFrameDepth: Integer);
begin
  if FCount >= Length(FEntries) then
    SetLength(FEntries, FCount * 2 + 8);
  FEntries[FCount].CatchIP := ACatchIP;
  FEntries[FCount].CatchRegister := ACatchRegister;
  FEntries[FCount].FrameDepth := AFrameDepth;
  Inc(FCount);
end;

procedure TGocciaBytecodeHandlerStack.Pop;
begin
  if FCount > 0 then
    Dec(FCount);
end;

function TGocciaBytecodeHandlerStack.Peek: TGocciaBytecodeHandlerEntry;
begin
  if FCount = 0 then
    raise Exception.Create('TGocciaBytecodeHandlerStack.Peek: stack is empty');
  Result := FEntries[FCount - 1];
end;

function TGocciaBytecodeHandlerStack.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

constructor EGocciaBytecodeThrow.Create(const AThrownValue: TGocciaValue);
var
  MessageText: string;
  ErrorObject: TGocciaObjectValue;
  NameValue: TGocciaValue;
  DetailValue: TGocciaValue;
begin
  MessageText := 'Goccia VM throw';
  if Assigned(AThrownValue) then
  begin
    if AThrownValue is TGocciaObjectValue then
    begin
      ErrorObject := TGocciaObjectValue(AThrownValue);
      NameValue := ErrorObject.GetProperty(PROP_NAME);
      DetailValue := ErrorObject.GetProperty(PROP_MESSAGE);
      if Assigned(NameValue) and Assigned(DetailValue) and
         not (NameValue is TGocciaUndefinedLiteralValue) and
         not (DetailValue is TGocciaUndefinedLiteralValue) then
        MessageText := NameValue.ToStringLiteral.Value + ': ' +
          DetailValue.ToStringLiteral.Value
      else
        MessageText := AThrownValue.ToStringLiteral.Value;
    end
    else
      MessageText := AThrownValue.ToStringLiteral.Value;
  end;
  inherited Create(MessageText);
  FThrownValue := AThrownValue;
end;

end.
