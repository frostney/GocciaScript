unit Goccia.FFI.LibraryGuard;

{$I Goccia.inc}

interface

uses
  DynamicLibraries;

type
  TGocciaFFIDependentGuard = class
  protected
    function GetIsClosed: Boolean; virtual; abstract;
  public
    procedure RetainDependent; virtual; abstract;
    procedure ReleaseDependent; virtual; abstract;
    function ClosedErrorMessage: string; virtual; abstract;

    property IsClosed: Boolean read GetIsClosed;
  end;

  TGocciaFFILibraryGuard = class(TGocciaFFIDependentGuard)
  private
    FHandle: TLibHandle;
    FPath: string;
    FClosed: Boolean;
    FOwnerAttached: Boolean;
    FDependentCount: Integer;

    procedure Unload;
  protected
    function GetIsClosed: Boolean; override;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;

    procedure RetainDependent; override;
    procedure ReleaseDependent; override;
    procedure ReleaseOwner;
    function FindSymbol(const AName: string): Pointer;
    function ClosedErrorMessage: string; override;
    procedure Close;

    property Path: string read FPath;
  end;

implementation

uses
  SysUtils,

  Goccia.Error.Messages;

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

function TGocciaFFILibraryGuard.GetIsClosed: Boolean;
begin
  Result := FClosed;
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

function TGocciaFFILibraryGuard.FindSymbol(const AName: string): Pointer;
begin
  if FClosed then
    raise Exception.Create('Cannot look up symbol in closed library: ' + FPath);
  Result := GetProcedureAddress(FHandle, AName);
  if not Assigned(Result) then
    raise Exception.Create('Symbol not found: ' + AName + ' in ' + FPath);
end;

function TGocciaFFILibraryGuard.ClosedErrorMessage: string;
begin
  Result := SErrorFFIPointerLibraryClosed;
end;

procedure TGocciaFFILibraryGuard.Close;
begin
  if FClosed then Exit;

  FClosed := True;
  if FDependentCount = 0 then
    Unload;
end;

end.
