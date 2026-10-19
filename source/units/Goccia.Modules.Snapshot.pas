unit Goccia.Modules.Snapshot;

{ Heap snapshots of a run's module records (ADR 0148).

  A snapshot holds every record the engine's module loader caches, whatever
  its roots reach: a module the run evaluated must stay evaluated, so a later
  import of it runs no body. Each record's header names it (its path, the
  keys the loader caches it under) and, for a source module, the bytecode the
  engine compiled it to. A restore compiles that module from the same path
  without linking or running it, refuses one whose bytecode differs, and
  fills a fresh record from the payload: its environment, exports,
  namespace object and evaluation state. Once the whole snapshot restored,
  the records are cached in the restoring engine's loader as evaluated
  modules; a refused restore frees them.

  A data module (JSON, text, bytes) is written as its exported values, which
  the run may have changed, not read again. A record a host or a runtime
  module loader built refuses the snapshot until host module codecs land, as
  does loader state outside the records: a module still linking, one whose
  evaluation failed, a deferred namespace, a module source object, and
  import.meta objects. }

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Engine,
  Goccia.HeapSnapshot.Codec,
  Goccia.Modules;

type
  TGocciaModuleSnapshotRecords = class(TGocciaSnapshotModuleRecordCodec)
  private
    FEngine: TGocciaEngine;
    // Each record's cache keys: the writer's records, or the restored
    // records it keeps until Commit.
    FKeys: TObjectDictionary<TGocciaModule, TGocciaModuleKeyList>;
    FRestored: TGocciaModuleList;
    FSeenKeys: TDictionary<string, Boolean>;
    // A restored source module's file's modification time, read before the
    // module was compiled from it.
    FStamps: TDictionary<TGocciaModule, TDateTime>;
  public
    constructor Create(const AEngine: TGocciaEngine);
    // Frees the restored records not committed.
    destructor Destroy; override;
    function SchemaVersion: Word; override;
    procedure ListRecords(const AWriter: TGocciaHeapSnapshotWriter); override;
    procedure WriteHeader(const AWriter: TGocciaHeapSnapshotWriter;
      const ARecord: TObject); override;
    procedure WritePayload(const AWriter: TGocciaHeapSnapshotWriter;
      const ARecord: TObject); override;
    function Allocate(const AReader: TGocciaHeapSnapshotReader): TObject;
      override;
    procedure Fill(const AReader: TGocciaHeapSnapshotReader;
      const ARecord: TObject); override;
    procedure Finish(const AReader: TGocciaHeapSnapshotReader;
      const ARecord: TObject); override;
    // Caches the restored records in the engine's loader, which owns them
    // from then on. Call once the restore succeeded, after the reader is
    // done. The run's limits can stop it, leaving the loader as it was and
    // the records here.
    procedure Commit;
  end;

implementation

uses
  SysUtils,

  Goccia.Bytecode.Module,
  Goccia.ImportMeta,
  Goccia.InstructionLimit,
  Goccia.Timeout;

const
  RECORD_SOURCE = 1;
  RECORD_DATA = 2;
  // What a key held for a restored record costs, charged before it is kept.
  KEY_ENTRY_BYTES = 96;
  // What a restored record and its bookkeeping hold before its payload: the
  // record, its export maps and roots, and its entries here.
  RECORD_BYTES = 2048;

constructor TGocciaModuleSnapshotRecords.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  FEngine := AEngine;
  FKeys := TObjectDictionary<TGocciaModule, TGocciaModuleKeyList>.Create(
    [doOwnsValues]);
  FRestored := TGocciaModuleList.Create;
  FSeenKeys := TDictionary<string, Boolean>.Create;
  FStamps := TDictionary<TGocciaModule, TDateTime>.Create;
end;

destructor TGocciaModuleSnapshotRecords.Destroy;
var
  I: Integer;
begin
  for I := 0 to FRestored.Count - 1 do
    FRestored[I].Free;
  FRestored.Free;
  FKeys.Free;
  FSeenKeys.Free;
  FStamps.Free;
  inherited;
end;

function TGocciaModuleSnapshotRecords.SchemaVersion: Word;
begin
  Result := 1;
end;

procedure TGocciaModuleSnapshotRecords.ListRecords(
  const AWriter: TGocciaHeapSnapshotWriter);
var
  Keys: TGocciaModuleKeyList;
  Modules: TGocciaModuleList;
  RecordKeys: TGocciaModuleKeyList;
  Reason: string;
  I: Integer;
begin
  Reason := FEngine.ModuleLoader.HeapSnapshotRefusal;
  if Reason <> '' then
    AWriter.Refuse(Reason);
  if HasImportMetaObjects(FEngine.Realm) then
    AWriter.Refuse('an import.meta object, until a codec covers them');
  Keys := TGocciaModuleKeyList.Create;
  Modules := TGocciaModuleList.Create;
  try
    FEngine.ModuleLoader.ListCachedModules(Keys, Modules);
    for I := 0 to Modules.Count - 1 do
    begin
      AWriter.Poll;
      if not FKeys.TryGetValue(Modules[I], RecordKeys) then
      begin
        if Modules[I].SnapshotKind = mskNone then
          AWriter.Refuse('the module ' + Modules[I].Path + ', which a ' +
            'host or a runtime module loader built');
        if (Modules[I].SnapshotKind = mskSource) and
           not (Modules[I].CompiledModule is TGocciaBytecodeModule) then
          AWriter.Refuse('the module ' + Modules[I].Path + ', whose ' +
            'bytecode the engine does not keep');
        RecordKeys := TGocciaModuleKeyList.Create;
        FKeys.Add(Modules[I], RecordKeys);
        AWriter.AddModuleRecord(Modules[I]);
      end;
      RecordKeys.Add(Keys[I]);
    end;
  finally
    Modules.Free;
    Keys.Free;
  end;
end;

procedure TGocciaModuleSnapshotRecords.WriteHeader(
  const AWriter: TGocciaHeapSnapshotWriter; const ARecord: TObject);
var
  Module: TGocciaModule;
  RecordKeys: TGocciaModuleKeyList;
  I: Integer;
begin
  Module := TGocciaModule(ARecord);
  AWriter.WriteStr(Module.Path);
  AWriter.WriteBool(Module.IsHostOwned);
  if Module.SnapshotKind = mskSource then
  begin
    AWriter.WriteU8(RECORD_SOURCE);
    AWriter.WriteStr(TGocciaBytecodeModule(Module.CompiledModule).SourcePath);
    AWriter.WriteModuleIdentity(TGocciaBytecodeModule(Module.CompiledModule));
  end
  else
    AWriter.WriteU8(RECORD_DATA);
  RecordKeys := FKeys[Module];
  AWriter.WriteU32(RecordKeys.Count);
  for I := 0 to RecordKeys.Count - 1 do
    AWriter.WriteStr(RecordKeys[I]);
end;

procedure TGocciaModuleSnapshotRecords.WritePayload(
  const AWriter: TGocciaHeapSnapshotWriter; const ARecord: TObject);
begin
  TGocciaModule(ARecord).WriteSnapshot(AWriter);
end;

// The bytecode AHash names, compiled from ASourcePath when this engine has
// not compiled it yet (ACompiledNow).
function EnsureCompiled(const AReader: TGocciaHeapSnapshotReader;
  const AEngine: TGocciaEngine; const APath, ASourcePath, AHash: string;
  const AIsHostOwned: Boolean;
  out ACompiledNow: Boolean): TGocciaBytecodeModule;
var
  Index: Integer;
  Compiled: TGocciaBytecodeModule;
begin
  Compiled := nil;
  ACompiledNow := False;
  if not Assigned(AReader.Templates) then
    AReader.Refuse('no compiled modules to restore the module ' + APath +
      ' into');
  Index := AReader.Templates.FindModule(AHash);
  if Index < 0 then
  begin
    try
      Compiled := AEngine.CompileModuleForSnapshot(ASourcePath, AIsHostOwned);
    except
      // The run's limits end the restore as they end any work, and a
      // refusal is the restore's own.
      on E: TGocciaTimeoutError do
        raise;
      on E: TGocciaInstructionLimitError do
        raise;
      on E: EGocciaHeapSnapshotRefused do
        raise;
      on E: Exception do
        AReader.Refuse('the module ' + APath + ' does not compile here: ' +
          E.Message);
    end;
    AReader.Templates.AddModule(Compiled);
    Index := AReader.Templates.FindModule(AHash);
    if Index < 0 then
      AReader.Refuse('the module ' + APath + ' is not the one the snapshot ' +
        'ran');
    ACompiledNow := True;
  end;
  Result := AReader.Templates.ModuleAt(Index);
end;

function TGocciaModuleSnapshotRecords.Allocate(
  const AReader: TGocciaHeapSnapshotReader): TObject;
var
  Path, SourcePath, Hash, Key: string;
  IsHostOwned: Boolean;
  Kind: Byte;
  Count, I: Integer;
  Compiled: TGocciaBytecodeModule;
  Module: TGocciaModule;
  RecordKeys: TGocciaModuleKeyList;
  HasStamp, CompiledNow: Boolean;
  Stamp: TDateTime;
begin
  AReader.ReserveItems(1, RECORD_BYTES);
  Path := AReader.ReadStr;
  IsHostOwned := AReader.ReadBool;
  Kind := AReader.ReadU8;
  Compiled := nil;
  case Kind of
    RECORD_SOURCE:
      begin
        SourcePath := AReader.ReadStr;
        Hash := AReader.ReadStr;
      end;
    RECORD_DATA:
      ;
  else
    AReader.Refuse('an unknown kind of module record');
  end;
  Count := AReader.ReadItemCount(4);
  if Count = 0 then
    AReader.Refuse('a module record cached under no key');
  AReader.ReserveItems(Count, KEY_ENTRY_BYTES);
  RecordKeys := TGocciaModuleKeyList.Create;
  try
    for I := 0 to Count - 1 do
    begin
      AReader.Poll;
      Key := AReader.ReadStr;
      if Key = '' then
        AReader.Refuse('a module record cached under an empty key');
      if FSeenKeys.ContainsKey(Key) then
        AReader.Refuse('two module records cached under one key');
      if FEngine.ModuleLoader.HasCachedModule(Key) then
        AReader.Refuse('the module ' + Path + ', which this engine has ' +
          'loaded already');
      FSeenKeys.Add(Key, True);
      RecordKeys.Add(Key);
    end;
    HasStamp := False;
    Stamp := 0;
    if Kind = RECORD_SOURCE then
    begin
      // Read before the module is compiled: a file that changes after it
      // reads newer, and a later import reloads it. Bytecode this engine
      // compiled earlier says nothing about the file now, so the record
      // then keeps the run's time.
      HasStamp := FEngine.ModuleLoader.TryGetModuleLastModified(Path, Stamp);
      Compiled := EnsureCompiled(AReader, FEngine, Path, SourcePath, Hash,
        IsHostOwned, CompiledNow);
      HasStamp := HasStamp and CompiledNow;
    end;
  except
    RecordKeys.Free;
    raise;
  end;
  Module := TGocciaModule.Create(Path);
  FRestored.Add(Module);
  FKeys.Add(Module, RecordKeys);
  if HasStamp then
    FStamps.Add(Module, Stamp);
  Module.IsHostOwned := IsHostOwned;
  if Kind = RECORD_SOURCE then
    Module.SnapshotKind := mskSource
  else
    Module.SnapshotKind := mskData;
  Module.CompiledModule := Compiled;
  Result := Module;
end;

procedure TGocciaModuleSnapshotRecords.Fill(
  const AReader: TGocciaHeapSnapshotReader; const ARecord: TObject);
var
  Stamp: TDateTime;
begin
  TGocciaModule(ARecord).ReadSnapshot(AReader);
  // A source module this restore compiled from its file was checked against
  // it, and a later import compares the record's time with the file's: the
  // file's own time, not the writing run's, or a copy of the same file would
  // make that import evaluate the module again. A data module keeps the
  // run's, as its values are the run's.
  if FStamps.TryGetValue(TGocciaModule(ARecord), Stamp) then
    TGocciaModule(ARecord).LastModified := Stamp;
end;

procedure TGocciaModuleSnapshotRecords.Finish(
  const AReader: TGocciaHeapSnapshotReader; const ARecord: TObject);
begin
  TGocciaModule(ARecord).CheckSnapshotForwardings(AReader);
end;

procedure TGocciaModuleSnapshotRecords.Commit;
var
  Keys: array of TGocciaModuleKeyList;
  I: Integer;
begin
  SetLength(Keys, FRestored.Count);
  for I := 0 to FRestored.Count - 1 do
    Keys[I] := FKeys[FRestored[I]];
  FEngine.ModuleLoader.CacheRestoredModules(FRestored, Keys);
  // The loader owns them now.
  FRestored.Clear;
end;

end.
