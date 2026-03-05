unit Souffle.VM.CallFrame;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode.Chunk,
  Souffle.VM.Closure;

type
  PSouffleVMCallFrame = ^TSouffleVMCallFrame;

  TSouffleVMCallFrame = record
    Template: TSouffleFunctionTemplate;
    Closure: TSouffleClosure;
    IP: Integer;
    BaseRegister: Integer;
    HandlerDepth: Integer;
    ReturnRegister: Integer;
    ArgCount: Integer;
    ArgSourceBase: Integer;
  end;

  TSouffleCallStack = class
  private
    FFrames: array of TSouffleVMCallFrame;
    FCount: Integer;
    FCapacity: Integer;
  public
    constructor Create(const AInitialCapacity: Integer = 64);

    function Push(const ATemplate: TSouffleFunctionTemplate;
      const AClosure: TSouffleClosure;
      const ABaseRegister: Integer;
      const AReturnRegister: Integer;
      const AHandlerDepth: Integer): PSouffleVMCallFrame;
    procedure Pop;
    function Peek: PSouffleVMCallFrame; inline;
    function GetFrame(const AIndex: Integer): PSouffleVMCallFrame; inline;
    function IsEmpty: Boolean; inline;

    property Count: Integer read FCount;
  end;

implementation

{ TSouffleCallStack }

constructor TSouffleCallStack.Create(const AInitialCapacity: Integer);
begin
  inherited Create;
  FCapacity := AInitialCapacity;
  SetLength(FFrames, FCapacity);
  FCount := 0;
end;

function TSouffleCallStack.Push(const ATemplate: TSouffleFunctionTemplate;
  const AClosure: TSouffleClosure;
  const ABaseRegister: Integer;
  const AReturnRegister: Integer;
  const AHandlerDepth: Integer): PSouffleVMCallFrame;
begin
  if FCount >= FCapacity then
  begin
    FCapacity := FCapacity * 2;
    SetLength(FFrames, FCapacity);
  end;
  FFrames[FCount].Template := ATemplate;
  FFrames[FCount].Closure := AClosure;
  FFrames[FCount].IP := 0;
  FFrames[FCount].BaseRegister := ABaseRegister;
  FFrames[FCount].ReturnRegister := AReturnRegister;
  FFrames[FCount].HandlerDepth := AHandlerDepth;
  FFrames[FCount].ArgCount := 0;
  FFrames[FCount].ArgSourceBase := 0;
  Result := @FFrames[FCount];
  Inc(FCount);
end;

procedure TSouffleCallStack.Pop;
begin
  if FCount > 0 then
    Dec(FCount);
end;

function TSouffleCallStack.Peek: PSouffleVMCallFrame;
begin
  {$IFDEF DEBUG}
  Assert(FCount > 0, 'TSouffleCallStack.Peek called on empty stack');
  {$ENDIF}
  if FCount = 0 then
    Exit(nil);
  Result := @FFrames[FCount - 1];
end;

function TSouffleCallStack.GetFrame(const AIndex: Integer): PSouffleVMCallFrame;
begin
  Result := @FFrames[AIndex];
end;

function TSouffleCallStack.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

end.
