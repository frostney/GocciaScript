unit Goccia.CLI.Help;

{$I Goccia.inc}

interface

uses
  Goccia.CLI.Options;

function GenerateHelpText(const AProgramName, AUsageLine: string;
  const AOptions: TGocciaOptionArray): string;

implementation

uses
  Classes,
  SysUtils,

  StringBuffer;

type
  TGroupEntry = record
    Header: string;
    Lines: TStringList;
  end;

function FindOrAddGroup(var AGroups: array of TGroupEntry;
  var ACount: Integer; const AHeader: string): Integer;
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    if AGroups[I].Header = AHeader then
      Exit(I);
  Result := ACount;
  AGroups[Result].Header := AHeader;
  AGroups[Result].Lines := TStringList.Create;
  Inc(ACount);
end;

function GenerateHelpText(const AProgramName, AUsageLine: string;
  const AOptions: TGocciaOptionArray): string;
const
  MAX_GROUPS = 32;
  COLUMN_GAP = 2;
var
  Buffer: TStringBuffer;
  Groups: array[0..MAX_GROUPS - 1] of TGroupEntry;
  GroupCount: Integer;
  MaxWidth, I, J, GroupIndex: Integer;
  FormattedName, Header, Padding: string;
  Option: TGocciaOptionBase;
begin
  GroupCount := 0;
  MaxWidth := 0;

  for I := 0 to High(AOptions) do
  begin
    Option := AOptions[I];
    FormattedName := Option.FormatForHelp;
    if Length(FormattedName) > MaxWidth then
      MaxWidth := Length(FormattedName);

    if Option.Group = '' then
      Header := 'Options:'
    else
      Header := Option.Group + ' Options:';

    GroupIndex := FindOrAddGroup(Groups, GroupCount, Header);
    Groups[GroupIndex].Lines.AddObject(FormattedName, Option);
  end;

  Buffer := TStringBuffer.Create(512);

  Buffer.Append('Usage: ' + AProgramName + ' ' + AUsageLine);
  Buffer.Append(sLineBreak);

  for I := 0 to GroupCount - 1 do
  begin
    Buffer.Append(sLineBreak);
    Buffer.Append(Groups[I].Header);
    Buffer.Append(sLineBreak);

    for J := 0 to Groups[I].Lines.Count - 1 do
    begin
      FormattedName := Groups[I].Lines[J];
      Option := TGocciaOptionBase(Groups[I].Lines.Objects[J]);

      Padding := StringOfChar(' ', MaxWidth - Length(FormattedName) + COLUMN_GAP);

      Buffer.Append('  ' + FormattedName + Padding + Option.HelpText);
      Buffer.Append(sLineBreak);
    end;

    Groups[I].Lines.Free;
  end;

  Result := Buffer.ToString;
end;

end.
