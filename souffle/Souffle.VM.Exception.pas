unit Souffle.VM.Exception;

{$I Souffle.inc}

interface

uses
  SysUtils,

  Souffle.Value;

type
  TSouffleHandlerEntry = record
    CatchIP: Integer;
    FinallyIP: Integer;
    CatchRegister: UInt8;
    FrameIndex: Integer;
    BaseRegister: Integer;
  end;

  TSouffleHandlerStack = class
  private
    FEntries: array of TSouffleHandlerEntry;
    FCount: Integer;
    FCapacity: Integer;
  public
    constructor Create(const AInitialCapacity: Integer = 16);

    procedure Push(const ACatchIP, AFinallyIP: Integer;
      const ACatchRegister: UInt8;
      const AFrameIndex, ABaseRegister: Integer);
    procedure Pop;
    function Peek: TSouffleHandlerEntry; inline;
    function IsEmpty: Boolean; inline;

    property Count: Integer read FCount;
  end;

  ESouffleThrow = class(Exception)
  private
    FThrownValue: TSouffleValue;
  public
    constructor Create(const AValue: TSouffleValue);
    property ThrownValue: TSouffleValue read FThrownValue;
  end;

implementation

{ TSouffleHandlerStack }

constructor TSouffleHandlerStack.Create(const AInitialCapacity: Integer);
begin
  inherited Create;
  FCapacity := AInitialCapacity;
  SetLength(FEntries, FCapacity);
  FCount := 0;
end;

procedure TSouffleHandlerStack.Push(const ACatchIP, AFinallyIP: Integer;
  const ACatchRegister: UInt8;
  const AFrameIndex, ABaseRegister: Integer);
begin
  if FCount >= FCapacity then
  begin
    FCapacity := FCapacity * 2;
    SetLength(FEntries, FCapacity);
  end;
  FEntries[FCount].CatchIP := ACatchIP;
  FEntries[FCount].FinallyIP := AFinallyIP;
  FEntries[FCount].CatchRegister := ACatchRegister;
  FEntries[FCount].FrameIndex := AFrameIndex;
  FEntries[FCount].BaseRegister := ABaseRegister;
  Inc(FCount);
end;

procedure TSouffleHandlerStack.Pop;
begin
  if FCount > 0 then
    Dec(FCount);
end;

function TSouffleHandlerStack.Peek: TSouffleHandlerEntry;
begin
  Result := FEntries[FCount - 1];
end;

function TSouffleHandlerStack.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

{ ESouffleThrow }

constructor ESouffleThrow.Create(const AValue: TSouffleValue);
begin
  inherited Create('Souffle throw');
  FThrownValue := AValue;
end;

end.
