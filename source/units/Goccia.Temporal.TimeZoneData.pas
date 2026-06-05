unit Goccia.Temporal.TimeZoneData;

{$I Goccia.inc}

interface

uses
  SysUtils;

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;

implementation

{$IFDEF GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
uses
  Classes,

  EmbeddedResourceReader,
  Generated.TimeZoneData;

const
  TIME_ZONE_RCDATA_RESOURCE_TYPE = MAKEINTRESOURCE(10);
  TIME_ZONE_DATA_MAGIC: TEmbeddedResourceMagic =
    (Ord('G'), Ord('O'), Ord('C'), Ord('C'), Ord('I'), Ord('A'), Ord('T'), Ord('Z'));

var
  CachedTZResource: TBytes;
  CachedTZResourceLoaded: Boolean;

function TryReadEmbeddedResource(out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  if CachedTZResourceLoaded then
  begin
    ABuffer := CachedTZResource;
    Result := Length(ABuffer) > 0;
    Exit;
  end;

  Result := False;
  SetLength(ABuffer, 0);
  Stream := nil;
  try
    Stream := TResourceStream.Create(HInstance, GeneratedTimeZoneDataResourceName,
      TIME_ZONE_RCDATA_RESOURCE_TYPE);
    if Stream.Size > High(Integer) then
    begin
      Stream.Free;
      Exit;
    end;

    BufferSize := Integer(Stream.Size);
    SetLength(ABuffer, BufferSize);
    if BufferSize > 0 then
      Stream.ReadBuffer(ABuffer[0], BufferSize);
    Stream.Free;
    Stream := nil;
    CachedTZResource := ABuffer;
    CachedTZResourceLoaded := True;
    Result := True;
  except
    Stream.Free;
  end;
end;

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
var
  Resource: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
begin
  Result := False;
  SetLength(ABytes, 0);

  if not TryReadEmbeddedResource(Resource) then
    Exit;

  if not TryReadEmbeddedResourceContainer(Resource, TIME_ZONE_DATA_MAGIC,
     Container) then
    Exit;

  if not TryFindEmbeddedResourceEntry(Resource, ATimeZone, Container, Entry) then
    Exit;

  Result := TryCopyEmbeddedResourceEntryData(Resource, Container, Entry, ABytes);
end;

{$ELSE}

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
begin
  Result := False;
  SetLength(ABytes, 0);
end;

{$ENDIF}

end.
