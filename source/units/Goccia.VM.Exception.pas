unit Goccia.VM.Exception;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.Primitives;

type
  TGocciaBytecodeHandlerKind = (bhkCatch, bhkFinally);

  TGocciaBytecodeHandlerEntry = record
    CatchIP: Integer;
    CatchRegister: UInt16;
    FrameDepth: Integer;
    Kind: TGocciaBytecodeHandlerKind;
  end;

  TGocciaBytecodeHandlerEntryArray = array of TGocciaBytecodeHandlerEntry;

  TGocciaBytecodeHandlerStack = class
  private
    FEntries: array of TGocciaBytecodeHandlerEntry;
    FCount: Integer;
  public
    procedure Push(const ACatchIP: Integer; const ACatchRegister: UInt16;
      const AFrameDepth: Integer;
      const AKind: TGocciaBytecodeHandlerKind = bhkCatch);
    procedure Pop;
    procedure CopyFrom(const AStartIndex: Integer;
      out AEntries: TGocciaBytecodeHandlerEntryArray);
    procedure RestoreFrom(const AEntries: TGocciaBytecodeHandlerEntryArray;
      const AFrameDepth: Integer);
    function Peek: TGocciaBytecodeHandlerEntry;
    function IsEmpty: Boolean;
    property Count: Integer read FCount;
  end;

  EGocciaBytecodeThrow = class(Exception)
  private
    FThrownValue: TGocciaValue;
    FSuggestion: string;
  public
    constructor Create(const AThrownValue: TGocciaValue;
      const ASuggestion: string = '');
    property ThrownValue: TGocciaValue read FThrownValue;
    { The engine-authored "Suggestion:" line of the diagnostic, carried across
      the VM unwind so a throw that escapes to a host runner renders the same
      hint the tree-walk evaluator's TGocciaThrowValue would. Empty for a
      user-authored `throw`. }
    property Suggestion: string read FSuggestion;
  end;

// A JS throw escaping the bytecode VM through a native builtin arrives as
// EGocciaBytecodeThrow, which a builtin's generic Exception→TypeError
// normalization would misclassify as an engine error. Call this first in
// such handlers: it re-raises a bytecode throw as the catchable
// TGocciaThrowValue and does nothing for any other exception.
procedure ReraiseBytecodeThrow(const AException: Exception);

// A JS throw that crosses an executor boundary arrives as a Pascal exception
// carrying the guest's completion value: EGocciaBytecodeThrow (a compiled
// callee's throw leaving the VM) or TGocciaThrowValue (the tree-walk
// evaluator's throw). Both bind the thrown value itself — ES2026 §14.15.3 —
// not a fresh Error synthesized from the Pascal message. This is the single
// place that knows the boundary-exception class list, so every
// exception→value/rejection/mark site can route through it: a new boundary
// class is then covered everywhere by extending this one function instead of
// every ladder by hand. Returns True and sets AValue to the identity-preserved
// thrown value for such an exception; returns False (AValue := nil) otherwise.
function UnwrapThrownValue(const AException: Exception;
  out AValue: TGocciaValue): Boolean;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue;

procedure ReraiseBytecodeThrow(const AException: Exception);
begin
  if AException is EGocciaBytecodeThrow then
    raise TGocciaThrowValue.Create(EGocciaBytecodeThrow(AException).ThrownValue,
      EGocciaBytecodeThrow(AException).Suggestion);
end;

function UnwrapThrownValue(const AException: Exception;
  out AValue: TGocciaValue): Boolean;
begin
  if AException is EGocciaBytecodeThrow then
    AValue := EGocciaBytecodeThrow(AException).ThrownValue
  else if AException is TGocciaThrowValue then
    AValue := TGocciaThrowValue(AException).Value
  else
  begin
    AValue := nil;
    Exit(False);
  end;
  Result := True;
end;

procedure TGocciaBytecodeHandlerStack.Push(const ACatchIP: Integer;
  const ACatchRegister: UInt16; const AFrameDepth: Integer;
  const AKind: TGocciaBytecodeHandlerKind);
begin
  if FCount >= Length(FEntries) then
    SetLength(FEntries, FCount * 2 + 8);
  FEntries[FCount].CatchIP := ACatchIP;
  FEntries[FCount].CatchRegister := ACatchRegister;
  FEntries[FCount].FrameDepth := AFrameDepth;
  FEntries[FCount].Kind := AKind;
  Inc(FCount);
end;

procedure TGocciaBytecodeHandlerStack.Pop;
begin
  if FCount > 0 then
    Dec(FCount);
end;

procedure TGocciaBytecodeHandlerStack.CopyFrom(const AStartIndex: Integer;
  out AEntries: TGocciaBytecodeHandlerEntryArray);
var
  I, StartIndex: Integer;
begin
  StartIndex := AStartIndex;
  if StartIndex < 0 then
    StartIndex := 0;
  if StartIndex > FCount then
    StartIndex := FCount;

  SetLength(AEntries, FCount - StartIndex);
  for I := 0 to High(AEntries) do
    AEntries[I] := FEntries[StartIndex + I];
end;

procedure TGocciaBytecodeHandlerStack.RestoreFrom(
  const AEntries: TGocciaBytecodeHandlerEntryArray;
  const AFrameDepth: Integer);
var
  I: Integer;
begin
  if Length(AEntries) = 0 then
    Exit;
  if FCount + Length(AEntries) > Length(FEntries) then
    SetLength(FEntries, (FCount + Length(AEntries)) * 2);
  for I := 0 to High(AEntries) do
  begin
    FEntries[FCount] := AEntries[I];
    FEntries[FCount].FrameDepth := AFrameDepth;
    Inc(FCount);
  end;
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

constructor EGocciaBytecodeThrow.Create(const AThrownValue: TGocciaValue;
  const ASuggestion: string);
var
  MessageText: string;
  ErrorObject: TGocciaObjectValue;
  NameValue, DetailValue: TGocciaValue;
begin
  // Pascal-side Exception.Message — consulted only when no higher-level
  // formatter handles the throw. Built from primitive `name`/`message`
  // properties when available, otherwise a static placeholder. Never invoke
  // user toString()/valueOf() here: exceptions that JS later catches must
  // not observe stringification (ES2026 §14.14 ThrowStatement).
  MessageText := 'Goccia VM throw';
  if Assigned(AThrownValue) and (AThrownValue is TGocciaObjectValue) then
  begin
    ErrorObject := TGocciaObjectValue(AThrownValue);
    NameValue := ErrorObject.GetProperty(PROP_NAME);
    DetailValue := ErrorObject.GetProperty(PROP_MESSAGE);
    if (NameValue is TGocciaStringLiteralValue) and
       (DetailValue is TGocciaStringLiteralValue) then
      MessageText := TGocciaStringLiteralValue(NameValue).Value + ': ' +
        TGocciaStringLiteralValue(DetailValue).Value;
  end;
  inherited Create(MessageText);
  FThrownValue := AThrownValue;
  FSuggestion := ASuggestion;
end;

end.
