unit Goccia.CallStack;

{$I Goccia.inc}

interface

type
  TGocciaCallFrame = record
    FunctionName: string;
    FilePath: string;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaCallStack = class
  private class var
    FInstance: TGocciaCallStack;
  private
    FFrames: array of TGocciaCallFrame;
    FCount: Integer;
    FCapacity: Integer;
    procedure Grow;
  public
    class function Instance: TGocciaCallStack;
    class procedure Initialize;
    class procedure Shutdown;

    constructor Create;

    procedure Push(const AFunctionName, AFilePath: string; const ALine, AColumn: Integer);
    procedure Pop;

    { Captures the current call stack as a formatted string.
      AErrorName and AMessage form the first line: "ErrorName: message".
      ASkipTop omits the topmost N frames (e.g. 1 to skip the Error constructor). }
    function CaptureStackTrace(const AErrorName, AMessage: string; const ASkipTop: Integer = 0): string;

    property Count: Integer read FCount;
  end;

implementation

uses
  SysUtils;

const
  INITIAL_CAPACITY = 32;

{ TGocciaCallStack }

class function TGocciaCallStack.Instance: TGocciaCallStack;
begin
  Result := FInstance;
end;

class procedure TGocciaCallStack.Initialize;
begin
  if not Assigned(FInstance) then
    FInstance := TGocciaCallStack.Create;
end;

class procedure TGocciaCallStack.Shutdown;
begin
  FreeAndNil(FInstance);
end;

constructor TGocciaCallStack.Create;
begin
  FCount := 0;
  FCapacity := INITIAL_CAPACITY;
  SetLength(FFrames, FCapacity);
end;

procedure TGocciaCallStack.Grow;
begin
  FCapacity := FCapacity * 2;
  SetLength(FFrames, FCapacity);
end;

procedure TGocciaCallStack.Push(const AFunctionName, AFilePath: string; const ALine, AColumn: Integer);
begin
  if FCount >= FCapacity then
    Grow;
  FFrames[FCount].FunctionName := AFunctionName;
  FFrames[FCount].FilePath := AFilePath;
  FFrames[FCount].Line := ALine;
  FFrames[FCount].Column := AColumn;
  Inc(FCount);
end;

procedure TGocciaCallStack.Pop;
begin
  if FCount > 0 then
    Dec(FCount);
end;

function TGocciaCallStack.CaptureStackTrace(const AErrorName, AMessage: string; const ASkipTop: Integer = 0): string;
var
  I, EffectiveCount: Integer;
  Frame: TGocciaCallFrame;
  FuncName, Location: string;
begin
  if AMessage <> '' then
    Result := AErrorName + ': ' + AMessage
  else
    Result := AErrorName;

  if ASkipTop > 0 then
    EffectiveCount := FCount - ASkipTop
  else
    EffectiveCount := FCount;
  if EffectiveCount < 0 then
    EffectiveCount := 0;

  for I := EffectiveCount - 1 downto 0 do
  begin
    Frame := FFrames[I];
    if Frame.FunctionName <> '' then
      FuncName := Frame.FunctionName
    else
      FuncName := '<anonymous>';

    if Frame.FilePath <> '' then
      Location := Format('%s:%d:%d', [Frame.FilePath, Frame.Line, Frame.Column])
    else
      Location := Format('<unknown>:%d:%d', [Frame.Line, Frame.Column]);

    Result := Result + #10 + '    at ' + FuncName + ' (' + Location + ')';
  end;
end;

end.
