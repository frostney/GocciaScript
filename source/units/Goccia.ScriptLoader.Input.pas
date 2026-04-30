unit Goccia.ScriptLoader.Input;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections;

const
  STDIN_FILE_NAME = '<stdin>';
  STDIN_PATH_MARKER = '-';

  { A multifile separator is a line whose trimmed content equals exactly
    "---" (three dashes, no more, no less, with optional surrounding
    whitespace). }
  MULTIFILE_SEPARATOR = '---';

function IsStdinPath(const APath: string): Boolean;
function ReadSourceFromText(var AInput: Text): TStringList;

{ True iff ALine is a multifile separator (its trimmed content equals
  MULTIFILE_SEPARATOR exactly). }
function IsMultifileSeparator(const ALine: string): Boolean;

{ True iff ASource contains at least one separator line.  Used to
  distinguish "input had separators (treat as multifile)" from "input
  had no separators (pass through as a single file)" — relying on the
  number of surviving sections in SplitMultifileSource is wrong when
  separators are present but every produced section trims to empty
  except one. }
function ContainsMultifileSeparator(const ASource: TStringList): Boolean;

{ Splits ASource into independent sections at separator lines.
  Returns a TObjectList<TStringList> with one TStringList per section.
  Sections that are entirely empty (leading separator, trailing
  separator, consecutive separators) are dropped silently.  Caller
  owns the returned list and the TStringLists inside. }
function SplitMultifileSource(const ASource: TStringList): TObjectList<TStringList>;

{ Builds the canonical name for a multifile section.

    BuildMultifileSectionName('/abs/foo.jsx', 2) = '/abs/foo[part2].jsx'
    BuildMultifileSectionName('<stdin>',      1) = '<stdin>[part1]'

  For names with a file extension, "[partN]" is inserted before the
  extension so ExtractFileExt and ChangeFileExt continue to work.
  For names without an extension, "[partN]" is appended.
  APartIndex is 1-based. }
function BuildMultifileSectionName(const AOriginalPath: string;
  const APartIndex: Integer): string;

{ True iff AName has a "[part<digits>]" suffix in the multifile
  section convention.  Recognises both
  "/abs/foo[part2].jsx" and "<stdin>[part1]". }
function IsMultifileSectionName(const AName: string): Boolean;

{ Returns the original (pre-split) name for a multifile section.
  Idempotent on names that are not section names.

    MultifileOriginalPath('/abs/foo[part2].jsx') = '/abs/foo.jsx'
    MultifileOriginalPath('<stdin>[part1]')      = '<stdin>'
    MultifileOriginalPath('/abs/foo.jsx')        = '/abs/foo.jsx'
}
function MultifileOriginalPath(const AName: string): string;

implementation

uses
  SysUtils;

const
  PART_INFIX_OPEN  = '[part';
  PART_INFIX_CLOSE = ']';

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

function IsMultifileSeparator(const ALine: string): Boolean;
begin
  Result := Trim(ALine) = MULTIFILE_SEPARATOR;
end;

function ContainsMultifileSeparator(const ASource: TStringList): Boolean;
var
  I: Integer;
begin
  for I := 0 to ASource.Count - 1 do
    if IsMultifileSeparator(ASource[I]) then
      Exit(True);
  Result := False;
end;

function SplitMultifileSource(const ASource: TStringList): TObjectList<TStringList>;
var
  I: Integer;
  Current: TStringList;

  function HasNonBlankLine(const ALines: TStringList): Boolean;
  var
    K: Integer;
  begin
    for K := 0 to ALines.Count - 1 do
      if Trim(ALines[K]) <> '' then
        Exit(True);
    Result := False;
  end;

  procedure FlushCurrent;
  begin
    if HasNonBlankLine(Current) then
      Result.Add(Current)
    else
      Current.Free;
    Current := TStringList.Create;
  end;

begin
  Result := TObjectList<TStringList>.Create(True);
  Current := TStringList.Create;
  try
    for I := 0 to ASource.Count - 1 do
    begin
      if IsMultifileSeparator(ASource[I]) then
        FlushCurrent
      else
        Current.Add(ASource[I]);
    end;
    FlushCurrent;
  finally
    Current.Free;
  end;
end;

{ Inserts "[partN]" before the extension when one is present, otherwise
  appends "[partN]".  Uses ExtractFileExt to locate the extension so the
  insertion is exactly at the boundary used by ExtractFileExt /
  ChangeFileExt elsewhere in the codebase. }
function BuildMultifileSectionName(const AOriginalPath: string;
  const APartIndex: Integer): string;
var
  Extension, Stem, Suffix: string;
begin
  Extension := ExtractFileExt(AOriginalPath);
  Suffix := PART_INFIX_OPEN + IntToStr(APartIndex) + PART_INFIX_CLOSE;
  if Extension = '' then
    Result := AOriginalPath + Suffix
  else
  begin
    Stem := Copy(AOriginalPath, 1, Length(AOriginalPath) - Length(Extension));
    Result := Stem + Suffix + Extension;
  end;
end;

{ Locates a "[part<digits>]" group either at the end of AName (no
  extension) or immediately before the file extension.  Returns the
  position of the opening bracket, or 0 if no canonical section
  marker is present. }
function FindSectionMarker(const AName: string): Integer;
var
  Extension: string;
  TailStart, ScanEnd, OpenPos, ClosePos, Digit: Integer;
begin
  Result := 0;
  Extension := ExtractFileExt(AName);
  ScanEnd := Length(AName) - Length(Extension);
  if ScanEnd <= 0 then
    Exit;

  ClosePos := ScanEnd;
  if (ClosePos < 1) or (AName[ClosePos] <> PART_INFIX_CLOSE) then
    Exit;

  TailStart := ClosePos - Length(PART_INFIX_OPEN);
  if TailStart < 1 then
    Exit;

  OpenPos := -1;
  while TailStart >= 1 do
  begin
    if Copy(AName, TailStart, Length(PART_INFIX_OPEN)) = PART_INFIX_OPEN then
    begin
      OpenPos := TailStart;
      Break;
    end;
    Dec(TailStart);
  end;
  if OpenPos < 1 then
    Exit;

  if ClosePos - (OpenPos + Length(PART_INFIX_OPEN)) < 1 then
    Exit;

  for Digit := OpenPos + Length(PART_INFIX_OPEN) to ClosePos - 1 do
    if not (AName[Digit] in ['0'..'9']) then
      Exit;

  Result := OpenPos;
end;

function IsMultifileSectionName(const AName: string): Boolean;
begin
  Result := FindSectionMarker(AName) > 0;
end;

function MultifileOriginalPath(const AName: string): string;
var
  Extension: string;
  OpenPos: Integer;
begin
  OpenPos := FindSectionMarker(AName);
  if OpenPos = 0 then
    Exit(AName);
  Extension := ExtractFileExt(AName);
  Result := Copy(AName, 1, OpenPos - 1) + Extension;
end;

end.
