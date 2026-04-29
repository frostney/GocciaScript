{
  TGocciaSourceRegistry — single canonical loader for script source.

  Every runner and worker calls Load(AName) to obtain a script source
  TStringList, regardless of where the source came from:

    - For names that have been Register()'d (stdin, multifile sections),
      Load returns a fresh clone of the in-memory source.
    - For other names, Load delegates to
      CreateUTF8FileTextLines(ReadUTF8FileText(AName)) — i.e. reads the
      file from disk.

  This unifies the source-loading path so consumers never branch on the
  origin of a source.  Load always returns a caller-owned TStringList,
  matching the existing "Source := ...; try ... finally Source.Free; end"
  pattern across all runners.

  Thread safety: FRegistered is mutated only on the main thread before
  any worker pool is started.  Workers only call Load, which performs
  read-only lookups on the map and clones the source TStringList for
  memory hits, or opens a fresh file handle for disk hits.  No locks
  are required as long as registration completes before workers spawn.
}

unit Goccia.ScriptLoader.SourceRegistry;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap;

type
  TGocciaSourceRegistry = class
  private
    FRegistered: TOrderedStringMap<TStringList>;
    FOwned: TObjectList<TStringList>;

    function CloneSource(const ASource: TStringList): TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    { Register an in-memory source under AName.  The registry takes
      ownership of AOwnedSource and frees it on Destroy.  Subsequent
      calls to Load(AName) return a fresh clone of this source. }
    procedure Register(const AName: string; const AOwnedSource: TStringList);

    { True iff AName has been registered as an in-memory source. }
    function IsRegistered(const AName: string): Boolean;

    { Single canonical source loader.  Memory hits return a fresh clone;
      misses delegate to CreateUTF8FileTextLines(ReadUTF8FileText(AName)).
      Always returns a caller-owned TStringList. }
    function Load(const AName: string): TStringList;
  end;

implementation

uses
  TextSemantics,

  Goccia.TextFiles;

{ TGocciaSourceRegistry }

constructor TGocciaSourceRegistry.Create;
begin
  inherited Create;
  FRegistered := TOrderedStringMap<TStringList>.Create;
  FOwned := TObjectList<TStringList>.Create(True);
end;

destructor TGocciaSourceRegistry.Destroy;
begin
  FRegistered.Free;
  FOwned.Free;
  inherited Destroy;
end;

function TGocciaSourceRegistry.CloneSource(
  const ASource: TStringList): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  Result.Capacity := ASource.Count;
  for I := 0 to ASource.Count - 1 do
    Result.Add(ASource[I]);
end;

procedure TGocciaSourceRegistry.Register(const AName: string;
  const AOwnedSource: TStringList);
begin
  FOwned.Add(AOwnedSource);
  FRegistered.AddOrSetValue(AName, AOwnedSource);
end;

function TGocciaSourceRegistry.IsRegistered(const AName: string): Boolean;
begin
  Result := FRegistered.ContainsKey(AName);
end;

function TGocciaSourceRegistry.Load(const AName: string): TStringList;
var
  Registered: TStringList;
begin
  if FRegistered.TryGetValue(AName, Registered) then
    Result := CloneSource(Registered)
  else
    Result := CreateUTF8FileTextLines(ReadUTF8FileText(AName));
end;

end.
