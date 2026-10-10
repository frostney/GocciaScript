unit Goccia.PrivateNames;

{$I Goccia.inc}

interface

{ Bytecode stores a private name under a storage key: the compiler emits
  '#slot:<class prefix>$name' (the prefix is a counter, '0$') and the VM
  rebinds it to '#slot:<brand token>:name' (the token is hexadecimal). Neither
  prefix contains '$' or ':', and a private name never contains ':', so the
  name starts after the first of those two separators. }

// Returns the private name a storage key stands for, without its '#':
// '#slot:0$p' and '#slot:00AB12:p' both give 'p'. Any other string is
// returned unchanged.
function PrivateStorageSourceName(const AStorageName: string): string;

// Returns how a class element is named to users: '#p' for a private storage
// key, the name itself otherwise.
function DisplayClassElementName(const AStorageName: string): string;

implementation

const
  PRIVATE_STORAGE_SLOT_PREFIX = '#slot:';

function PrivateNameStart(const AStorageName: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Copy(AStorageName, 1, Length(PRIVATE_STORAGE_SLOT_PREFIX)) <>
     PRIVATE_STORAGE_SLOT_PREFIX then
    Exit;
  for I := Length(PRIVATE_STORAGE_SLOT_PREFIX) + 1 to Length(AStorageName) do
    if (AStorageName[I] = '$') or (AStorageName[I] = ':') then
      Exit(I + 1);
end;

function PrivateStorageSourceName(const AStorageName: string): string;
var
  NameStart: Integer;
begin
  NameStart := PrivateNameStart(AStorageName);
  if NameStart = 0 then
    Exit(AStorageName);
  Result := Copy(AStorageName, NameStart, MaxInt);
end;

function DisplayClassElementName(const AStorageName: string): string;
var
  NameStart: Integer;
begin
  NameStart := PrivateNameStart(AStorageName);
  if NameStart = 0 then
    Exit(AStorageName);
  Result := '#' + Copy(AStorageName, NameStart, MaxInt);
end;

end.
