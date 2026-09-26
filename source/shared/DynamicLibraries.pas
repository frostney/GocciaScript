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
{ The file the platform loader actually mapped for AHandle, where the
  platform can say (Windows: GetModuleFileNameW); empty elsewhere. }
function LoadedLibraryPath(const AHandle: TLibHandle): string;

implementation

{$IFNDEF LAKON}
  {$IFDEF FPC}
    {$IFDEF MSWINDOWS}
uses
  Windows;
    {$ENDIF}
  {$ELSE}
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

function LoadedLibraryPath(const AHandle: TLibHandle): string;
{$IFNDEF LAKON}
{$IFDEF MSWINDOWS}
const
  MAX_MODULE_PATH = 32768;
var
  Buffer: array of WideChar;
  Length_: Cardinal;
  WidePath: UnicodeString;
{$ENDIF}
{$ENDIF}
begin
  Result := '';
  {$IFNDEF LAKON}
  {$IFDEF MSWINDOWS}
  SetLength(Buffer, MAX_MODULE_PATH);
  Length_ := GetModuleFileNameW(HMODULE(AHandle), @Buffer[0],
    MAX_MODULE_PATH);
  if (Length_ > 0) and (Length_ < MAX_MODULE_PATH) then
  begin
    SetString(WidePath, PWideChar(@Buffer[0]), Length_);
    Result := string(WidePath);
  end;
  {$ENDIF}
  {$ENDIF}
end;

end.
