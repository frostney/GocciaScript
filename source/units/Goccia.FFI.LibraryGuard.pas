unit Goccia.FFI.LibraryGuard;

{$I Goccia.inc}

interface

uses
  Dynlibs;

type
  TGocciaFFILibraryGuard = class
  private
    FHandle: TLibHandle;
    FPath: string;
    FClosed: Boolean;
    FOwnerAttached: Boolean;
    FDependentCount: Integer;

    procedure Unload;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;

    procedure RetainDependent;
    procedure ReleaseDependent;
    procedure ReleaseOwner;
    function FindSymbol(const AName: string): CodePointer;
    procedure Close;

    property Path: string read FPath;
    property IsClosed: Boolean read FClosed;
  end;

implementation

uses
  SysUtils;

constructor TGocciaFFILibraryGuard.Create(const APath: string);
begin
  inherited Create;
  FPath := APath;
  FOwnerAttached := True;
  FHandle := LoadLibrary(APath);
  if FHandle = NilHandle then
    raise Exception.Create('Failed to load library: ' + APath);
end;

destructor TGocciaFFILibraryGuard.Destroy;
begin
  Unload;
  inherited;
end;

procedure TGocciaFFILibraryGuard.Unload;
begin
  if FHandle <> NilHandle then
  begin
    UnloadLibrary(FHandle);
    FHandle := NilHandle;
  end;
end;

procedure TGocciaFFILibraryGuard.RetainDependent;
begin
  // Logical close invalidates dependents but still defers physical unload.
  Inc(FDependentCount);
end;

procedure TGocciaFFILibraryGuard.ReleaseDependent;
begin
  Assert(FDependentCount > 0);
  Dec(FDependentCount);
  if FDependentCount <> 0 then Exit;

  if not FOwnerAttached then
  begin
    Free;
    Exit;
  end;

  if FClosed then
    Unload;
end;

procedure TGocciaFFILibraryGuard.ReleaseOwner;
begin
  Assert(FOwnerAttached);
  FOwnerAttached := False;
  if FDependentCount = 0 then
    Free;
end;

function TGocciaFFILibraryGuard.FindSymbol(const AName: string): CodePointer;
begin
  if FClosed then
    raise Exception.Create('Cannot look up symbol in closed library: ' + FPath);
  Result := GetProcAddress(FHandle, AName);
  if not Assigned(Result) then
    raise Exception.Create('Symbol not found: ' + AName + ' in ' + FPath);
end;

procedure TGocciaFFILibraryGuard.Close;
begin
  if FClosed then Exit;

  FClosed := True;
  if FDependentCount = 0 then
    Unload;
end;

end.
