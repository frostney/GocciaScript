unit Goccia.CapabilityAudit;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  EGocciaCapabilityAuditDeliveryError = class(Exception);

  TGocciaCapabilityKind = (
    gckFetchHost,
    gckFetchDispatch,
    gckFFIOpen,
    gckFunctionConstructor,
    gckShadowRealm,
    gckSandboxFileSystem
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

function CapabilityKindName(const AKind: TGocciaCapabilityKind): string;
function CapabilityDecisionName(
  const ADecision: TGocciaCapabilityDecision): string;

implementation

uses
  Goccia.JSON.Utils;

function CapabilityKindName(const AKind: TGocciaCapabilityKind): string;
begin
  case AKind of
    gckFetchHost:
      Result := 'fetch.host';
    gckFetchDispatch:
      Result := 'fetch.dispatch';
    gckFFIOpen:
      Result := 'ffi.open';
    gckFunctionConstructor:
      Result := 'function.constructor';
    gckShadowRealm:
      Result := 'shadow-realm.construct';
    gckSandboxFileSystem:
      Result := 'sandbox.fs.path';
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
