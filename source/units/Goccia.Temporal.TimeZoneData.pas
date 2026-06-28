unit Goccia.Temporal.TimeZoneData;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TEmbeddedTimeZoneFileNameArray = array of string;

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
function TryGetEmbeddedTimeZoneFileNames(out ANames: TEmbeddedTimeZoneFileNameArray): Boolean;
function TryCanonicalizeEmbeddedTimeZoneFileName(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;

implementation

{$IFDEF GOCCIA_TEMPORAL_EMBEDDED_TZDATA}
uses
  Classes,

  EmbeddedResourceReader,
  Generated.TimeZoneData,
  LazyPublishedCache;

const
  TIME_ZONE_RCDATA_RESOURCE_TYPE = MAKEINTRESOURCE(10);
  TIME_ZONE_DATA_MAGIC: TEmbeddedResourceMagic =
    (Ord('G'), Ord('O'), Ord('C'), Ord('C'), Ord('I'), Ord('A'), Ord('T'), Ord('Z'));

var
  TZResourceCache: TLazyPublishedCache<TBytes>;

function LoadTZResource(const AKey: string; out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  Result := False;
  SetLength(ABuffer, 0);
  Stream := nil;
  try
    Stream := TResourceStream.Create(HInstance, AKey,
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
    Result := True;
  except
    Stream.Free;
  end;
end;

function TryReadEmbeddedResource(out ABuffer: TBytes): Boolean;
begin
  if TZResourceCache.Ensure(GeneratedTimeZoneDataResourceName, @LoadTZResource) then
    ABuffer := TZResourceCache.Data
  else
    SetLength(ABuffer, 0);
  Result := Length(ABuffer) > 0;
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

function TryGetEmbeddedTimeZoneFileNames(out ANames: TEmbeddedTimeZoneFileNameArray): Boolean;
var
  Resource: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
  EntryIndex, NameOffset: Integer;
begin
  Result := False;
  SetLength(ANames, 0);

  if not TryReadEmbeddedResource(Resource) then
    Exit;

  if not TryReadEmbeddedResourceContainer(Resource, TIME_ZONE_DATA_MAGIC,
     Container) then
    Exit;

  SetLength(ANames, Container.EntryCount);
  for EntryIndex := 0 to Container.EntryCount - 1 do
  begin
    if not TryReadEmbeddedResourceEntry(Resource, Container, EntryIndex, Entry) then
    begin
      SetLength(ANames, 0);
      Exit;
    end;

    if Entry.NameOffset > High(Integer) - Container.NamesOffset then
    begin
      SetLength(ANames, 0);
      Exit;
    end;

    NameOffset := Container.NamesOffset + Entry.NameOffset;
    if not HasBytesAvailable(Resource, NameOffset, Entry.NameLength) then
    begin
      SetLength(ANames, 0);
      Exit;
    end;

    ANames[EntryIndex] := CopyStringFromBytes(Resource, NameOffset,
      Entry.NameLength);
  end;

  Result := True;
end;

function TryCanonicalizeEmbeddedTimeZoneFileName(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
var
  Resource: TBytes;
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
  EntryIndex, NameOffset: Integer;
  EntryName: string;
begin
  Result := False;
  ACanonicalTimeZone := '';

  if not TryReadEmbeddedResource(Resource) then
    Exit;

  if not TryReadEmbeddedResourceContainer(Resource, TIME_ZONE_DATA_MAGIC,
     Container) then
    Exit;

  for EntryIndex := 0 to Container.EntryCount - 1 do
  begin
    if not TryReadEmbeddedResourceEntry(Resource, Container, EntryIndex, Entry) then
      Exit;

    if Entry.NameOffset > High(Integer) - Container.NamesOffset then
      Exit;
    NameOffset := Container.NamesOffset + Entry.NameOffset;
    if not HasBytesAvailable(Resource, NameOffset, Entry.NameLength) then
      Exit;

    EntryName := CopyStringFromBytes(Resource, NameOffset, Entry.NameLength);
    if SameText(EntryName, ATimeZone) then
    begin
      ACanonicalTimeZone := EntryName;
      Result := True;
      Exit;
    end;
  end;
end;

initialization
  TZResourceCache.Init;

finalization
  TZResourceCache.Done;

{$ELSE}

function TryGetEmbeddedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
begin
  Result := False;
  SetLength(ABytes, 0);
end;

function TryGetEmbeddedTimeZoneFileNames(out ANames: TEmbeddedTimeZoneFileNameArray): Boolean;
begin
  Result := False;
  SetLength(ANames, 0);
end;

function TryCanonicalizeEmbeddedTimeZoneFileName(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
begin
  Result := False;
  ACanonicalTimeZone := '';
end;

{$ENDIF}

end.
