unit Goccia.FFI.DynamicLibrary;

{$I Goccia.inc}

interface

type
  TGocciaFFILibraryHandle = class
  private
    FHandle: TLibHandle;
    FPath: string;
    FClosed: Boolean;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;

    function FindSymbol(const AName: string): CodePointer;
    procedure Close;

    property Path: string read FPath;
    property IsClosed: Boolean read FClosed;
  end;

implementation

uses
  SysUtils,

  Dynlibs;

constructor TGocciaFFILibraryHandle.Create(const APath: string);
begin
  FPath := APath;
  FClosed := False;
  FHandle := LoadLibrary(APath);
  if FHandle = NilHandle then
    raise Exception.Create('Failed to load library: ' + APath);
end;

destructor TGocciaFFILibraryHandle.Destroy;
begin
  if not FClosed and (FHandle <> NilHandle) then
    UnloadLibrary(FHandle);
  inherited;
end;

function TGocciaFFILibraryHandle.FindSymbol(const AName: string): CodePointer;
begin
  if FClosed then
    raise Exception.Create('Cannot look up symbol in closed library: ' + FPath);
  Result := GetProcAddress(FHandle, AName);
  if not Assigned(Result) then
    raise Exception.Create('Symbol not found: ' + AName + ' in ' + FPath);
end;

procedure TGocciaFFILibraryHandle.Close;
begin
  if not FClosed and (FHandle <> NilHandle) then
  begin
    UnloadLibrary(FHandle);
    FHandle := NilHandle;
    FClosed := True;
  end;
end;

end.
