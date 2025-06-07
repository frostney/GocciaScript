unit TestUtils;

interface

{$I Goccia.inc}

uses
  Classes, SysUtils,
  Goccia.Interpreter,
  Goccia.Values.Base;


function RunFromString(const AText: String): TInterpreterResult;

implementation

function RunFromString(const AText: String): TInterpreterResult;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  Source.Add(AText);
  Result := RunGocciaScriptFromStringList(Source, 'test.goccia.js');
  Source.Free;
end;

end.