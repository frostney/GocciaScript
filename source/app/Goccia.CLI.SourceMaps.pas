unit Goccia.CLI.SourceMaps;

{$I Goccia.inc}

interface

uses
  Goccia.SourceMap;

procedure WriteSourceMapIfRequested(const ASourceMap: TGocciaSourceMap;
  const ARequested: Boolean; const AOutputPath, ASourceName: string;
  const APrintMessage: Boolean);

implementation

uses
  SysUtils;

procedure WriteSourceMapIfRequested(const ASourceMap: TGocciaSourceMap;
  const ARequested: Boolean; const AOutputPath, ASourceName: string;
  const APrintMessage: Boolean);
begin
  if not ARequested then
    Exit;
  if not Assigned(ASourceMap) then
    Exit;

  ASourceMap.FileName := ExtractFileName(ASourceName);
  ASourceMap.SetSourcePath(0, ExtractFileName(ASourceName));
  ASourceMap.SaveToFile(AOutputPath);

  if APrintMessage then
    WriteLn(SysUtils.Format('  Source map written to %s', [AOutputPath]));
end;

end.
