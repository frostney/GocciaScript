unit Goccia.Version;

{$I Goccia.inc}

interface

function GetVersion: string;
function GetCommit: string;

implementation

uses
  Process,
  SysUtils;

var
  CachedVersion: string;
  CachedCommit: string;

function RunGit(const AArgs: array of string): string;
var
  Output: string;
begin
  if RunCommand('git', AArgs, Output) then
    Result := Trim(Output)
  else
    Result := '';
end;

function LooksLikeSemver(const AValue: string): Boolean;
var
  FirstDot: SizeInt;
begin
  FirstDot := Pos('.', AValue);
  Result := (FirstDot > 1) and (FirstDot < Length(AValue));
end;

procedure Initialize;
var
  Describe: string;
  DashPos: SizeInt;
begin
  Describe := RunGit(['describe', '--tags', '--always']);
  CachedCommit := RunGit(['rev-parse', '--short', 'HEAD']);
  if CachedCommit = '' then
    CachedCommit := 'unknown';

  if (Describe = '') or not LooksLikeSemver(Describe) then
  begin
    CachedVersion := '0.0.0-dev';
    Exit;
  end;

  DashPos := Pos('-', Describe);
  if DashPos > 0 then
    CachedVersion := Copy(Describe, 1, DashPos - 1) + '-dev'
  else
    CachedVersion := Describe;
end;

function GetVersion: string;
begin
  Result := CachedVersion;
end;

function GetCommit: string;
begin
  Result := CachedCommit;
end;

initialization
  Initialize;

end.
