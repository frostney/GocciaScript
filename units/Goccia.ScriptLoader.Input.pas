unit Goccia.ScriptLoader.Input;

{$I Goccia.inc}

interface

uses
  Classes;

const
  STDIN_FILE_NAME = '<stdin>';
  STDIN_PATH_MARKER = '-';

function IsStdinPath(const APath: string): Boolean;
function ReadSourceFromText(var AInput: Text): TStringList;

implementation

uses
  SysUtils;

function IsStdinPath(const APath: string): Boolean;
begin
  Result := Trim(APath) = STDIN_PATH_MARKER;
end;

function ReadSourceFromText(var AInput: Text): TStringList;
var
  Line: string;
begin
  Result := TStringList.Create;
  while not EOF(AInput) do
  begin
    ReadLn(AInput, Line);
    Result.Add(Line);
  end;
end;

end.
