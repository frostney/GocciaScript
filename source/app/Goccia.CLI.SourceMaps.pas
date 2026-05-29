unit Goccia.CLI.SourceMaps;

{$I Goccia.inc}

interface

uses
  Goccia.SourceMap;

procedure WriteSourceMapIfAvailable(const ASourceMap: TGocciaSourceMap;
  const AMapOutputPath, AGeneratedFileName, ASourceName: string;
  const APrintMessage: Boolean);

implementation

uses
  SysUtils;

procedure WriteSourceMapIfAvailable(const ASourceMap: TGocciaSourceMap;
  const AMapOutputPath, AGeneratedFileName, ASourceName: string;
  const APrintMessage: Boolean);
begin
  if not Assigned(ASourceMap) then
    Exit;

  ASourceMap.FileName := ExtractFileName(AGeneratedFileName);
  ASourceMap.SetSourcePath(0, ExtractFileName(ASourceName));
  ASourceMap.SaveToFile(AMapOutputPath);

  if APrintMessage then
    WriteLn(SysUtils.Format('  Source map written to %s', [AMapOutputPath]));
end;

end.
