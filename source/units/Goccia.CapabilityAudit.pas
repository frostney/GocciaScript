unit Goccia.CapabilityAudit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  EGocciaCapabilityAuditDeliveryError = class(Exception);

  { Kinds named after a capability (ADR 0122) report every allow and deny
    decision of that capability; the rest report engine features that are not
    capabilities but are still worth auditing. }
  TGocciaCapabilityKind = (
    gckNetFetch,
    gckNetDispatch,
    gckReadFile,
    gckFFIOpen,
    gckImportNodeModules,
    gckImportProvider,
    gckFunctionConstructor,
    gckShadowRealm,
    gckSandboxFileSystem,
    gckCapabilitiesEffective,
    { A host's decision on a config's permission requests (ADR 0122): allow
      when they are trusted, accepted for the run, or not needed; deny when
      they are not trusted, changed, or ignored. }
    gckConfigPermissions
  );

  TGocciaCapabilityDecision = (
    gcdAllow,
    gcdDeny
  );

  TGocciaCapabilityAuditSource = record
    FilePath: string;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaCapabilityAuditEvent = record
    Kind: TGocciaCapabilityKind;
    Decision: TGocciaCapabilityDecision;
    Subject: string;
    Reason: string;
    Source: TGocciaCapabilityAuditSource;
    function ToJSON: string;
  end;

  TGocciaCapabilityAuditSink = procedure(
    const AEvent: TGocciaCapabilityAuditEvent) of object;

  TGocciaCapabilityAuditEmitter = procedure(
    const AKind: TGocciaCapabilityKind;
    const ADecision: TGocciaCapabilityDecision;
    const ASubject, AReason: string) of object;

  { An emitter for decisions reported after the fact — a fetch worker's hop
    decisions replayed when the request settles — which carry the source
    location of the call that caused them rather than whatever runs now. }
  TGocciaCapabilityAuditSourcedEmitter = procedure(
    const AKind: TGocciaCapabilityKind;
    const ADecision: TGocciaCapabilityDecision;
    const ASubject, AReason: string;
    const ASource: TGocciaCapabilityAuditSource) of object;

function CapabilityKindName(const AKind: TGocciaCapabilityKind): string;
function CapabilityDecisionName(
  const ADecision: TGocciaCapabilityDecision): string;

implementation

uses
  Goccia.JSON.Utils;

function CapabilityKindName(const AKind: TGocciaCapabilityKind): string;
begin
  case AKind of
    gckNetFetch:
      Result := 'net.fetch';
    gckNetDispatch:
      Result := 'net.dispatch';
    gckReadFile:
      Result := 'read.file';
    gckFFIOpen:
      Result := 'ffi.open';
    gckImportNodeModules:
      Result := 'import.node-modules';
    gckImportProvider:
      Result := 'import.provider';
    gckFunctionConstructor:
      Result := 'function.constructor';
    gckShadowRealm:
      Result := 'shadow-realm.construct';
    gckSandboxFileSystem:
      Result := 'sandbox.fs.path';
    gckCapabilitiesEffective:
      Result := 'capabilities.effective';
    gckConfigPermissions:
      Result := 'config.permissions';
  end;
end;

function CapabilityDecisionName(
  const ADecision: TGocciaCapabilityDecision): string;
begin
  case ADecision of
    gcdAllow:
      Result := 'allow';
    gcdDeny:
      Result := 'deny';
  end;
end;

function JSONIntegerOrNull(const AValue: Integer): string;
begin
  if AValue > 0 then
    Result := IntToStr(AValue)
  else
    Result := 'null';
end;

function TGocciaCapabilityAuditEvent.ToJSON: string;
begin
  Result :=
    '{"schemaVersion":1' +
    ',"kind":' + QuoteJSONString(CapabilityKindName(Kind)) +
    ',"decision":' + QuoteJSONString(CapabilityDecisionName(Decision)) +
    ',"subject":' + QuoteJSONString(Subject) +
    ',"reason":' + QuoteJSONString(Reason) +
    ',"source":{' +
      '"file":' + QuoteJSONString(Source.FilePath) +
      ',"line":' + JSONIntegerOrNull(Source.Line) +
      ',"column":' + JSONIntegerOrNull(Source.Column) +
    '}}';
end;

end.
