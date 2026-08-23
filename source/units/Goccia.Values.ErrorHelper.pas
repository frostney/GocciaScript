unit Goccia.Values.ErrorHelper;

{$I Goccia.inc}

interface

uses
  Goccia.Realm,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

{ Records engine-trusted throw provenance on an error object for its runtime
  code frame: the top call frame's source location (skipping ASkipTop frames)
  and, when that module is in the engine's own source scope, the ±context
  window of its source. Bound to real frames, never to the guest-writable
  `.stack` string, so a guest-forged error object gets neither and renders no
  code frame. Every error factory must call this so the two executors and every
  host agree on what a genuine error's frame shows. Safe to call with no active
  call stack (records nothing). }
procedure AttachErrorSourceProvenance(const AError: TGocciaObjectValue;
  const ASkipTop: Integer);

{ Creates a JavaScript error object with the given name and message.
  ASkipTop controls how many frames to skip from the top of the call stack. }
function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;

{ True when AValue carries [[ErrorData]] — the slot every error constructor
  installs, including through a `class MyError extends Error` subclass. Merely
  inheriting from Error.prototype does not make an object an error.

  DOMException clears the slot on purpose, so it compares as an ordinary
  object through its enumerable name/message/code. That is a deliberate,
  project-level divergence from Vitest, which treats every DOMException as
  equal to every other one regardless of name or message. }
function IsErrorObject(const AValue: TGocciaValue): Boolean;

{ The Error.prototype an error with this name should be created against. Falls
  back to Error.prototype for any name without a dedicated intrinsic. }
function GetErrorPrototype(const AName: string): TGocciaObjectValue;

{ Creates a DOMException object with the standard legacy code for AName. }
function CreateDOMExceptionObject(const AName, AMessage: string;
  const ASkipTop: Integer = 0): TGocciaObjectValue;

{ Raises a TGocciaThrowValue with a TypeError }
procedure ThrowTypeError(const AMessage: string); overload;
procedure ThrowTypeError(const AMessage, ASuggestion: string); overload;
procedure ThrowTypeErrorInRealm(const AMessage, ASuggestion: string;
  const ARealm: TGocciaRealm); overload;

{ Raises a TGocciaThrowValue with a RangeError }
procedure ThrowRangeError(const AMessage: string); overload;
procedure ThrowRangeError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a ReferenceError }
procedure ThrowReferenceError(const AMessage: string); overload;
procedure ThrowReferenceError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a SyntaxError }
procedure ThrowSyntaxError(const AMessage: string); overload;
procedure ThrowSyntaxError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a DataCloneError (DOMException with code 25) }
procedure ThrowDataCloneError(const AMessage: string); overload;
procedure ThrowDataCloneError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with an InvalidCharacterError (DOMException with code 5) }
procedure ThrowInvalidCharacterError(const AMessage: string);

{ Raises a TGocciaThrowValue with a URIError }
procedure ThrowURIError(const AMessage: string); overload;
procedure ThrowURIError(const AMessage, ASuggestion: string); overload;

{ Raises a TGocciaThrowValue with a generic Error }
procedure ThrowError(const AMessage: string); overload;
procedure ThrowError(const AMessage, ASuggestion: string); overload;

implementation

uses
  Classes,

  Goccia.Builtins.Globals,
  Goccia.CallStack,
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Diagnostics.SourceRegistry,
  Goccia.GarbageCollector,
  Goccia.Values.Error,
  Goccia.Values.ObjectPropertyDescriptor;

const
  // Match FormatErrorWithSourceContext's context window so a captured excerpt
  // and the entry-file fallback render the same number of lines.
  ERROR_SOURCE_CONTEXT_BEFORE = 2;
  ERROR_SOURCE_CONTEXT_AFTER = 2;
  { TryGetGuestWindow bounds each retained line to 512 actual bytes. The joined
    excerpt replaces the per-line headers with one header plus LF separators,
    so five line caps are a conservative exact-representation ceiling. }
  ERROR_SOURCE_EXCERPT_CAP_BYTES =
    (ERROR_SOURCE_CONTEXT_BEFORE + 1 + ERROR_SOURCE_CONTEXT_AFTER) *
    GOCCIA_DIAGNOSTIC_EXCERPT_MAX_LINE_BYTES;

procedure AttachErrorSourceProvenance(const AError: TGocciaObjectValue;
  const ASkipTop: Integer);
var
  ErrorObject: TGocciaErrorObjectValue;
  Path, ExcerptText: string;
  Line, Col, FirstLine: Integer;
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
  GC: TGarbageCollector;
  ExcerptBytes: Int64;
  Reserved: Boolean;
begin
  // Provenance lives only on the error subclass; a factory that builds a plain
  // object simply carries none (and renders no frame).
  if not (AError is TGocciaErrorObjectValue) then
    Exit;
  ErrorObject := TGocciaErrorObjectValue(AError);
  if TGocciaCallStack.Instance = nil then
    Exit;
  if not TGocciaCallStack.Instance.TryGetTopThrowLocation(ASkipTop, Path,
    Line, Col) then
    Exit;
  ErrorObject.HasErrorSourceLocation := True;
  ErrorObject.ErrorSourcePath := Path;
  ErrorObject.ErrorSourceLine := Line;
  ErrorObject.ErrorSourceColumn := Col;
  ErrorObject.ErrorSourceExcerpt := '';
  ErrorObject.ErrorSourceExcerptFirstLine := 0;
  ErrorObject.ErrorSourcePrincipal := 0;
  // Capture the module's own ±context window from the ACTIVE scope — the engine
  // actually executing — and only for GUEST-owned source (TryGetGuestWindow
  // withholds transitively host-owned and cross-principal source). The excerpt travels
  // on the error so the frame renders later, even after the engine is freed;
  // its principal is stamped so a foreign renderer can refuse it.
  Scope := TGocciaDiagnosticSourceRegistry.Current;
  if not Assigned(Scope) then
    Exit;
  { Stamp the provenance principal even when no excerpt can be captured. The
    entry-file fallback is authorized against this same explicit identity, and
    a renderer supplied no/mismatched identity therefore gets location only. }
  ErrorObject.ErrorSourcePrincipal := Scope.Principal;
  Window := TStringList.Create;
  try
    if Scope.TryGetGuestWindow(Path, Line, ERROR_SOURCE_CONTEXT_BEFORE,
      ERROR_SOURCE_CONTEXT_AFTER, Window, FirstLine) then
    begin
      ExcerptText := Window.Text;
      ExcerptBytes := DiagnosticStringRetainedBytes(ExcerptText);
      if ExcerptBytes > ERROR_SOURCE_EXCERPT_CAP_BYTES then
        Exit;
      // Charge exactly the retained UTF-16 allocation against --max-memory;
      // the error object's destructor releases the same figure. The reservation
      // may collect before refusing, so pass the not-yet-published error as an
      // extra root. Refusal safely degrades to location-only.
      GC := TGarbageCollector.Instance;
      Reserved := False;
      if Assigned(GC) then
      begin
        if not GC.TryReserveExternalBytes(ExcerptBytes, ErrorObject) then
          Exit;
        Reserved := True;
      end;
      try
        ErrorObject.ErrorSourceExcerpt := ExcerptText;
        ErrorObject.ErrorSourceExcerptFirstLine := FirstLine;
        if Reserved then
          ErrorObject.ErrorSourceExcerptCharged := ExcerptBytes;
      except
        if Reserved then
          GC.ReleaseExternalBytes(ExcerptBytes);
        raise;
      end;
    end;
  finally
    Window.Free;
  end;
end;

function DOMExceptionLegacyCode(const AName: string): Integer;
begin
  if AName = DATA_CLONE_ERROR_NAME then
    Result := 25
  else if AName = INVALID_CHARACTER_ERROR_NAME then
    Result := 5
  else if AName = ABORT_ERROR_NAME then
    Result := 20
  else if AName = TIMEOUT_ERROR_NAME then
    Result := 23
  else if AName = INVALID_STATE_ERROR_NAME then
    Result := 11
  else
    Result := 0;
end;

function GetErrorPrototype(const AName: string): TGocciaObjectValue;
begin
  if AName = TYPE_ERROR_NAME then
    Result := GetTypeErrorProto
  else if AName = EVAL_ERROR_NAME then
    Result := GetEvalErrorProto
  else if AName = RANGE_ERROR_NAME then
    Result := GetRangeErrorProto
  else if AName = REFERENCE_ERROR_NAME then
    Result := GetReferenceErrorProto
  else if AName = SYNTAX_ERROR_NAME then
    Result := GetSyntaxErrorProto
  else if AName = URI_ERROR_NAME then
    Result := GetURIErrorProto
  else if AName = AGGREGATE_ERROR_NAME then
    Result := GetAggregateErrorProto
  else if AName = SUPPRESSED_ERROR_NAME then
    Result := GetSuppressedErrorProto
  else if AName = ERROR_NAME then
    Result := GetErrorProto
  else
    Result := GetErrorProto;
end;

function IsErrorObject(const AValue: TGocciaValue): Boolean;
begin
  Result := (AValue is TGocciaObjectValue) and
    TGocciaObjectValue(AValue).HasErrorData;
end;

function CreateDOMExceptionObject(const AName, AMessage: string;
  const ASkipTop: Integer): TGocciaObjectValue;
begin
  Result := CreateErrorObject(AName, AMessage, ASkipTop);
  Result.HasErrorData := False;
  if GetDOMExceptionProto <> nil then
    Result.Prototype := GetDOMExceptionProto;
  if Result.ErrorStack <> '' then
    Result.DefineProperty(PROP_STACK,
      TGocciaPropertyDescriptorData.Create(
        TGocciaStringLiteralValue.Create(Result.ErrorStack),
        [pfConfigurable, pfWritable]));
  Result.AssignProperty(PROP_CODE,
    TGocciaNumberLiteralValue.Create(DOMExceptionLegacyCode(AName)));
end;

// ES2026 §10.4.4.4 [[ErrorData]]
function CreateErrorObject(const AName, AMessage: string; const ASkipTop: Integer = 0): TGocciaObjectValue;
var
  Proto: TGocciaObjectValue;
  ResultRoot: TGocciaTempRoot;
begin
  Proto := GetErrorPrototype(AName);
  // An error subclass instance so provenance can be attached below without
  // enlarging every plain object (see TGocciaErrorObjectValue).
  { The error under construction must be a temp root: the property stores below
    grow the shaped map at its collecting growth gate, CaptureStackTrace and the
    diagnostic-excerpt reservation are GC safe points, and nothing else roots
    this value yet — without the root a collection taken at any of them frees the
    half-built error and the next field access dereferences freed memory (an
    Access violation under -O3/-O4, where the collection lands mid-build). Mirror
    the same guard the Error() constructor path already uses in Builtins.Globals. }
  InitializeTempRoot(ResultRoot);
  try
    if Assigned(Proto) then
      Result := TGocciaErrorObjectValue.Create(Proto)
    else
      Result := TGocciaErrorObjectValue.Create;
    AddTempRootIfNeeded(ResultRoot, Result);
    Result.HasErrorData := True;
    Result.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AName));
    Result.AssignProperty(PROP_MESSAGE, TGocciaStringLiteralValue.Create(AMessage));

    if (TGocciaCallStack.Instance <> nil) then
      Result.ErrorStack :=
        TGocciaCallStack.Instance.CaptureStackTrace(AName, AMessage, ASkipTop);
    AttachErrorSourceProvenance(Result, ASkipTop);
  finally
    RemoveTempRootIfNeeded(ResultRoot);
  end;
end;

function CreateErrorObjectInRealm(const AName, AMessage: string;
  const ASkipTop: Integer; const ARealm: TGocciaRealm): TGocciaObjectValue;
var
  PreviousRealm: TGocciaRealm;
begin
  if (not Assigned(ARealm)) or (ARealm = CurrentRealm) then
    Exit(CreateErrorObject(AName, AMessage, ASkipTop));

  PreviousRealm := CurrentRealm;
  SetCurrentRealm(ARealm);
  try
    Result := CreateErrorObject(AName, AMessage, ASkipTop);
  finally
    SetCurrentRealm(PreviousRealm);
  end;
end;

{ Shared raise helper — creates the error object and raises with optional suggestion }
procedure RaiseNativeError(const AErrorName, AMessage, ASuggestion: string);
begin
  raise TGocciaThrowValue.Create(
    CreateErrorObject(AErrorName, AMessage), ASuggestion);
end;

procedure ThrowTypeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(TYPE_ERROR_NAME, AMessage));
end;

procedure ThrowTypeError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(TYPE_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowTypeErrorInRealm(const AMessage, ASuggestion: string;
  const ARealm: TGocciaRealm);
begin
  raise TGocciaThrowValue.Create(
    CreateErrorObjectInRealm(TYPE_ERROR_NAME, AMessage, 0, ARealm),
    ASuggestion);
end;

procedure ThrowRangeError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(RANGE_ERROR_NAME, AMessage));
end;

procedure ThrowRangeError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(RANGE_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowReferenceError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(REFERENCE_ERROR_NAME, AMessage));
end;

procedure ThrowReferenceError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(REFERENCE_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowSyntaxError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(SYNTAX_ERROR_NAME, AMessage));
end;

procedure ThrowSyntaxError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(SYNTAX_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowDataCloneError(const AMessage: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateDOMExceptionObject(DATA_CLONE_ERROR_NAME, AMessage);
  raise TGocciaThrowValue.Create(ErrorObj);
end;

procedure ThrowDataCloneError(const AMessage, ASuggestion: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateDOMExceptionObject(DATA_CLONE_ERROR_NAME, AMessage);
  raise TGocciaThrowValue.Create(ErrorObj, ASuggestion);
end;

procedure ThrowInvalidCharacterError(const AMessage: string);
var
  ErrorObj: TGocciaObjectValue;
begin
  ErrorObj := CreateDOMExceptionObject(INVALID_CHARACTER_ERROR_NAME, AMessage);
  raise TGocciaThrowValue.Create(ErrorObj);
end;

procedure ThrowURIError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(URI_ERROR_NAME, AMessage));
end;

procedure ThrowURIError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(URI_ERROR_NAME, AMessage, ASuggestion);
end;

procedure ThrowError(const AMessage: string);
begin
  raise TGocciaThrowValue.Create(CreateErrorObject(ERROR_NAME, AMessage));
end;

procedure ThrowError(const AMessage, ASuggestion: string);
begin
  RaiseNativeError(ERROR_NAME, AMessage, ASuggestion);
end;

end.
