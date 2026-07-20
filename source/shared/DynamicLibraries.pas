unit DynamicLibraries;

{$I Shared.inc}

interface

{$IFDEF FPC}
uses
  DynLibs;
{$ENDIF}

type
  {$IFDEF LAKON}
  TLibHandle = NativeInt;
  {$ELSE}
    {$IFDEF FPC}
    TLibHandle = DynLibs.TLibHandle;
    {$ELSE}
    TLibHandle = NativeUInt;
    {$ENDIF}
  {$ENDIF}

const
  NilHandle = TLibHandle(0);

function LoadLibrary(const APath: string): TLibHandle;
procedure UnloadLibrary(const AHandle: TLibHandle);
function GetProcedureAddress(const AHandle: TLibHandle;
  const AName: string): Pointer;

implementation

{$IFNDEF FPC}
  {$IFNDEF LAKON}
uses
  Winapi.Windows;
  {$ENDIF}
{$ENDIF}

function LoadLibrary(const APath: string): TLibHandle;
begin
  {$IFDEF LAKON}
  Result := NilHandle;
  {$ELSE}
    {$IFDEF FPC}
    Result := DynLibs.LoadLibrary(APath);
    {$ELSE}
    Result := TLibHandle(Winapi.Windows.LoadLibraryW(PWideChar(APath)));
    {$ENDIF}
  {$ENDIF}
end;

procedure UnloadLibrary(const AHandle: TLibHandle);
begin
  {$IFNDEF LAKON}
    {$IFDEF FPC}
    DynLibs.UnloadLibrary(AHandle);
    {$ELSE}
    Winapi.Windows.FreeLibrary(HMODULE(AHandle));
    {$ENDIF}
  {$ENDIF}
end;

function GetProcedureAddress(const AHandle: TLibHandle;
  const AName: string): Pointer;
var
  {$IFNDEF LAKON}
  SymbolName: RawByteString;
  I: Integer;
  {$ENDIF}
begin
  {$IFDEF LAKON}
  Result := nil;
  {$ELSE}
  SetLength(SymbolName, Length(AName));
  for I := 1 to Length(AName) do
  begin
    if Ord(AName[I]) > $7F then
      Exit(nil);
    SymbolName[I] := AnsiChar(Ord(AName[I]));
  end;
    {$IFDEF FPC}
    Result := DynLibs.GetProcedureAddress(AHandle, SymbolName);
    {$ELSE}
    Result := Winapi.Windows.GetProcAddress(HMODULE(AHandle),
      PAnsiChar(SymbolName));
    {$ENDIF}
  {$ENDIF}
end;

end.
