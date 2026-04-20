unit Goccia.Version;

{$I Goccia.inc}

interface

function GetVersion: string;
function GetCommit: string;
function GetBuildDate: string;

implementation

{$I Goccia.Version.Generated.inc}

function GetVersion: string;
begin
  Result := BAKED_VERSION;
end;

function GetCommit: string;
begin
  Result := BAKED_COMMIT;
end;

function GetBuildDate: string;
begin
  Result := BAKED_BUILD_DATE;
end;

end.
