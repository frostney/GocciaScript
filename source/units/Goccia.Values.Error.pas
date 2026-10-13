unit Goccia.Values.Error;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Values.Primitives;

type
  // TGocciaThrowValue is the exception type for JS throw statements and runtime
  // errors (ThrowTypeError, ThrowRangeError, etc.). It propagates naturally through
  // the call stack and is caught by EvaluateTry (JS try/catch), async wrappers, or
  // the top-level engine. Return and break use TGocciaControlFlow records instead.
  TGocciaThrowValue = class(Exception)
  private
    FValue: TGocciaValue;
    FSuggestion: string;
    FSuggestionIsHostOnly: Boolean;
    FRootSlot: Integer;
  public
    constructor Create(const AValue: TGocciaValue); overload;
    constructor Create(const AValue: TGocciaValue;
      const ASuggestion: string;
      const ASuggestionIsHostOnly: Boolean = False); overload;
    destructor Destroy; override;
    property Value: TGocciaValue read FValue;
    property Suggestion: string read FSuggestion;
    { True when Suggestion is host-side advice (a PermissionDenied's grant,
      option, or canonical path). Output handed back to guest code must
      never carry such a suggestion. }
    property SuggestionIsHostOnly: Boolean read FSuggestionIsHostOnly;
  end;

{ A thrown value that crosses native code lives in the Pascal exception
  carrying it (TGocciaThrowValue or EGocciaBytecodeThrow), and nothing else may
  reference it. Native code that catches the exception can run guest code
  before it rejects with or rethrows the value — closing an iterator after an
  abrupt completion (ES2026 §7.4.11) runs the iterator's `return()` — and a
  collection there would free the value. So the exception roots its value for
  as long as it exists: HoldThrownValue records it for the calling thread's
  collector and returns a slot, and ReleaseThrownValue drops that slot when the
  exception is destroyed (issue #1629). AOwner is the exception; a release by
  anyone else, or after the thread's slots were reset, does nothing. }
function HoldThrownValue(const AOwner: TObject;
  const AValue: TGocciaValue): Integer;
procedure ReleaseThrownValue(const AOwner: TObject; const ASlot: Integer);

implementation

uses
  Goccia.GarbageCollector,
  Goccia.ThreadCleanupRegistry;

type
  TGocciaThrownValueSlot = record
    Owner: TObject;
    Value: TGocciaValue;
  end;

  TGocciaThrownValueRoots = class(TGCRootSource)
  public
    procedure MarkRootReferences; override;
  end;

const
  THROWN_VALUE_INITIAL_CAPACITY = 8;

type
  // One threadvar, so that a hold or a release relocates it once.
  TGocciaThrownValueState = record
    Slots: array of TGocciaThrownValueSlot;
    Count: Integer;
    Roots: TGocciaThrownValueRoots;
  end;
  PGocciaThrownValueState = ^TGocciaThrownValueState;

threadvar
  GThrownValues: TGocciaThrownValueState;

{ TGocciaThrownValueRoots }

procedure TGocciaThrownValueRoots.MarkRootReferences;
var
  State: PGocciaThrownValueState;
  I: Integer;
begin
  State := @GThrownValues;
  for I := 0 to State^.Count - 1 do
    if Assigned(State^.Slots[I].Value) then
      State^.Slots[I].Value.MarkReferences;
end;

{ The slots belong to the collector the root source is registered with. When
  that collector is gone (an engine was torn down and another built on this
  thread), the values in them were swept with it, so they are dropped rather
  than handed to the new collector to mark. The exceptions that held them then
  release slots that no longer name them, which does nothing. }
function EnsureThrownValueRoots(const AState: PGocciaThrownValueState): Boolean;
var
  Collector: TGarbageCollector;
  I: Integer;
begin
  Collector := TGarbageCollector.Instance;
  if Assigned(Collector) and Assigned(AState^.Roots) and
     (AState^.Roots.RegisteredCollector = Collector) then
    Exit(True);

  FreeAndNil(AState^.Roots);
  for I := 0 to AState^.Count - 1 do
  begin
    AState^.Slots[I].Owner := nil;
    AState^.Slots[I].Value := nil;
  end;
  AState^.Count := 0;
  if not Assigned(Collector) then
    Exit(False);

  AState^.Roots := TGocciaThrownValueRoots.Create;
  Result := True;
end;

function HoldThrownValue(const AOwner: TObject;
  const AValue: TGocciaValue): Integer;
var
  State: PGocciaThrownValueState;
begin
  Result := -1;
  if not Assigned(AValue) then
    Exit;
  State := @GThrownValues;
  if not EnsureThrownValueRoots(State) then
    Exit;
  if State^.Count >= Length(State^.Slots) then
    SetLength(State^.Slots, State^.Count * 2 + THROWN_VALUE_INITIAL_CAPACITY);
  Result := State^.Count;
  State^.Slots[Result].Owner := AOwner;
  State^.Slots[Result].Value := AValue;
  Inc(State^.Count);
end;

procedure ReleaseThrownValue(const AOwner: TObject; const ASlot: Integer);
var
  State: PGocciaThrownValueState;
begin
  if ASlot < 0 then
    Exit;
  State := @GThrownValues;
  if (ASlot >= State^.Count) or (State^.Slots[ASlot].Owner <> AOwner) then
    Exit;
  State^.Slots[ASlot].Owner := nil;
  State^.Slots[ASlot].Value := nil;
  // Exceptions are destroyed newest first as a rule, so the slots shrink
  // back from the top; a slot freed out of order is reclaimed with the ones
  // above it.
  while (State^.Count > 0) and
        not Assigned(State^.Slots[State^.Count - 1].Owner) do
    Dec(State^.Count);
end;

procedure CleanupThrownValueThreadState;
var
  State: PGocciaThrownValueState;
begin
  State := @GThrownValues;
  FreeAndNil(State^.Roots);
  SetLength(State^.Slots, 0);
  State^.Count := 0;
end;

{ TGocciaThrowValue }

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue);
begin
  inherited Create('');
  FValue := AValue;
  FSuggestion := '';
  FSuggestionIsHostOnly := False;
  FRootSlot := HoldThrownValue(Self, AValue);
end;

constructor TGocciaThrowValue.Create(const AValue: TGocciaValue;
  const ASuggestion: string; const ASuggestionIsHostOnly: Boolean);
begin
  inherited Create('');
  FValue := AValue;
  FSuggestion := ASuggestion;
  FSuggestionIsHostOnly := ASuggestionIsHostOnly;
  FRootSlot := HoldThrownValue(Self, AValue);
end;

destructor TGocciaThrowValue.Destroy;
begin
  ReleaseThrownValue(Self, FRootSlot);
  inherited;
end;

initialization
  RegisterThreadvarCleanup(CleanupThrownValueThreadState);

end.
