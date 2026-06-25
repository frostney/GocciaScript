unit Goccia.CallStack;

{$I Goccia.inc}

interface

type
  // Resolves a deferred call frame's function template (stored as an opaque
  // pointer so this low-level unit stays decoupled from the bytecode units)
  // into its display name and source path. The bytecode VM registers one of
  // these so it can push a frame as a bare template pointer on the hot call
  // path and pay the string cost only when a stack trace is actually captured.
  TGocciaTemplateTraceResolver = procedure(const ATemplate: Pointer;
    out AName, ASourcePath: string);

  TGocciaCallFrame = record
    // When Template is assigned, this is a deferred bytecode VM frame:
    // FunctionName/FilePath are resolved lazily via the template resolver at
    // capture time, and FilePath carries only the module-path fallback used
    // when the template has no own source file. When Template is nil the frame
    // carries its strings directly (interpreter and native callers).
    Template: Pointer;
    FunctionName: string;
    FilePath: string;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaCallFrameArray = array of TGocciaCallFrame;

  TGocciaCallStack = class
  private
    // The resolver is stateless and identical for every engine, so it is held
    // once at class scope rather than per (threadvar) instance. This avoids any
    // construction-order or cross-thread timing dependence on a live instance.
    class var FTemplateResolver: TGocciaTemplateTraceResolver;
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
    // Hot-path push for the bytecode VM: stores the template pointer plus a
    // module-path fallback, deferring all string work to CaptureStackTrace.
    procedure PushTemplate(const ATemplate: Pointer; const AFallbackPath: string);
    procedure Pop;

    // Registers the resolver used to materialise deferred template frames.
    // Class-level: one registration applies to every thread's instance.
    class procedure SetTemplateResolver(const AResolver: TGocciaTemplateTraceResolver);

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

threadvar
  CallStackThreadInstance: TGocciaCallStack;

{ TGocciaCallStack }

class function TGocciaCallStack.Instance: TGocciaCallStack;
begin
  Result := CallStackThreadInstance;
end;

class procedure TGocciaCallStack.Initialize;
begin
  if not Assigned(CallStackThreadInstance) then
    CallStackThreadInstance := TGocciaCallStack.Create;
end;

class procedure TGocciaCallStack.Shutdown;
begin
  FreeAndNil(CallStackThreadInstance);
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
  FFrames[FCount].Template := nil;
  FFrames[FCount].FunctionName := AFunctionName;
  FFrames[FCount].FilePath := AFilePath;
  FFrames[FCount].Line := ALine;
  FFrames[FCount].Column := AColumn;
  Inc(FCount);
end;

procedure TGocciaCallStack.PushTemplate(const ATemplate: Pointer; const AFallbackPath: string);
begin
  if FCount >= FCapacity then
    Grow;
  FFrames[FCount].Template := ATemplate;
  FFrames[FCount].FunctionName := '';
  FFrames[FCount].FilePath := AFallbackPath;
  FFrames[FCount].Line := 0;
  FFrames[FCount].Column := 0;
  Inc(FCount);
end;

procedure TGocciaCallStack.Pop;
begin
  if FCount > 0 then
    Dec(FCount);
end;

class procedure TGocciaCallStack.SetTemplateResolver(const AResolver: TGocciaTemplateTraceResolver);
begin
  FTemplateResolver := AResolver;
end;

function TGocciaCallStack.CaptureStackTrace(const AErrorName, AMessage: string; const ASkipTop: Integer = 0): string;
var
  I, EffectiveCount: Integer;
  Frame: TGocciaCallFrame;
  FuncName, Location, ResolvedName, ResolvedPath: string;
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
    // Deferred bytecode VM frames carry only a template pointer; materialise
    // their name and source path now. The template's own source file wins;
    // an empty one falls back to the module path stored at push time.
    if Assigned(Frame.Template) and Assigned(FTemplateResolver) then
    begin
      FTemplateResolver(Frame.Template, ResolvedName, ResolvedPath);
      if ResolvedPath = '' then
        ResolvedPath := Frame.FilePath;
    end
    else
    begin
      ResolvedName := Frame.FunctionName;
      ResolvedPath := Frame.FilePath;
    end;

    if ResolvedName <> '' then
      FuncName := ResolvedName
    else
      FuncName := '<anonymous>';

    if ResolvedPath <> '' then
      Location := Format('%s:%d:%d', [ResolvedPath, Frame.Line, Frame.Column])
    else
      Location := Format('<unknown>:%d:%d', [Frame.Line, Frame.Column]);

    Result := Result + #10 + '    at ' + FuncName + ' (' + Location + ')';
  end;
end;

end.
