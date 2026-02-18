#!/usr/bin/env instantfpc

{$mode delphi}
{$H+}

uses
  Classes,
  SysUtils,

  FileUtils in 'units/FileUtils.pas';

type
  TUnitCategory = (ucSystem, ucThirdParty, ucProject, ucRelative);
  TRunMode = (rmFormat, rmCheck);

var
  Mode: TRunMode;
  InputFiles: TStringList;
  FormattedCount: Integer;
  CheckedCount: Integer;

{ ═══════════════════════════════════════════════════════════════════════════
  Uses-Clause Formatting
  ═══════════════════════════════════════════════════════════════════════════ }

function IsUsesKeyword(const ALine: string): Boolean;
var
  Trimmed: string;
begin
  Trimmed := Trim(ALine);
  if Length(Trimmed) < 4 then
    Exit(False);
  if LowerCase(Copy(Trimmed, 1, 4)) <> 'uses' then
    Exit(False);
  if Length(Trimmed) = 4 then
    Exit(True);
  Result := not (Trimmed[5] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
end;

function ClassifyUnit(const AName: string): TUnitCategory;
const
  SystemUnits: array[0..20] of string = (
    'classes', 'sysutils', 'generics.collections', 'generics.defaults',
    'dateutils', 'strutils', 'math', 'typinfo', 'process', 'types',
    'windows', 'ctypes', 'unix', 'baseunix', 'crt', 'dos', 'variants',
    'syncobjs', 'contnrs', 'fgl', 'character'
  );
var
  Lower: string;
  I: Integer;
begin
  Lower := LowerCase(Trim(AName));

  if Pos(' in ', Lower) > 0 then
    Exit(ucRelative);

  for I := Low(SystemUnits) to High(SystemUnits) do
    if Lower = SystemUnits[I] then
      Exit(ucSystem);

  if Pos('goccia.', Lower) = 1 then
    Exit(ucProject);

  Result := ucThirdParty;
end;

function CompareUnitsCI(AList: TStringList; AIdx1, AIdx2: Integer): Integer;
begin
  Result := CompareText(AList[AIdx1], AList[AIdx2]);
end;

function FormatUsesClause(const AUnits: TStringList): TStringList;
const
  SectionCount = 4;
var
  Lists: array[0..SectionCount - 1] of TStringList;
  I, J, SecIdx, NonEmpty: Integer;
  IsLast: Boolean;
begin
  Result := TStringList.Create;
  for I := 0 to SectionCount - 1 do
    Lists[I] := TStringList.Create;
  try
    for I := 0 to AUnits.Count - 1 do
    begin
      case ClassifyUnit(AUnits[I]) of
        ucSystem:     Lists[0].Add(AUnits[I]);
        ucThirdParty: Lists[1].Add(AUnits[I]);
        ucProject:    Lists[2].Add(AUnits[I]);
        ucRelative:   Lists[3].Add(AUnits[I]);
      end;
    end;

    for I := 0 to SectionCount - 1 do
      Lists[I].CustomSort(@CompareUnitsCI);

    NonEmpty := 0;
    for I := 0 to SectionCount - 1 do
      if Lists[I].Count > 0 then
        Inc(NonEmpty);

    SecIdx := 0;
    for I := 0 to SectionCount - 1 do
    begin
      if Lists[I].Count = 0 then
        Continue;

      Inc(SecIdx);

      for J := 0 to Lists[I].Count - 1 do
      begin
        IsLast := (SecIdx = NonEmpty) and (J = Lists[I].Count - 1);
        if IsLast then
          Result.Add('  ' + Lists[I][J] + ';')
        else
          Result.Add('  ' + Lists[I][J] + ',');
      end;

      if SecIdx < NonEmpty then
        Result.Add('');
    end;
  finally
    for I := 0 to SectionCount - 1 do
      Lists[I].Free;
  end;
end;

function ContainsDirective(const AText: string): Boolean;
begin
  Result := Pos('{$', UpperCase(AText)) > 0;
end;

function StripLineComment(const ALine: string): string;
var
  I: Integer;
  InStr: Boolean;
begin
  I := 1;
  InStr := False;
  while I <= Length(ALine) do
  begin
    if InStr then
    begin
      if (ALine[I] = '''') then
      begin
        if (I < Length(ALine)) and (ALine[I + 1] = '''') then
        begin
          Inc(I, 2);
          Continue;
        end;
        InStr := False;
      end;
    end
    else
    begin
      if ALine[I] = '''' then
        InStr := True
      else if (ALine[I] = '/') and (I < Length(ALine)) and (ALine[I + 1] = '/') then
      begin
        Result := Copy(ALine, 1, I - 1);
        Exit;
      end
      else if ALine[I] = '{' then
      begin
        Result := Copy(ALine, 1, I - 1);
        Exit;
      end;
    end;
    Inc(I);
  end;
  Result := ALine;
end;

function UpdateBlockState(const ALine: string; AInBlock: Boolean): Boolean;
var
  K: Integer;
  InStr: Boolean;
begin
  K := 1;
  InStr := False;
  while K <= Length(ALine) do
  begin
    if AInBlock then
    begin
      if ALine[K] = '}' then
        AInBlock := False;
    end
    else if InStr then
    begin
      if ALine[K] = '''' then
        InStr := False;
    end
    else if ALine[K] = '''' then
      InStr := True
    else if ALine[K] = '{' then
      AInBlock := True
    else if (ALine[K] = '/') and (K < Length(ALine)) and (ALine[K + 1] = '/') then
      Break;
    Inc(K);
  end;
  Result := AInBlock;
end;

procedure FormatUsesInLines(const AInput: TStringList; const AOutput: TStringList);
var
  I, J: Integer;
  UsesContent, AfterUses, FullBlock, BeforeSC: string;
  Units, Formatted: TStringList;
  InBlock: Boolean;
begin
  I := 0;
  InBlock := False;
  while I < AInput.Count do
  begin
    if InBlock then
    begin
      InBlock := UpdateBlockState(AInput[I], InBlock);
      AOutput.Add(AInput[I]);
      Inc(I);
      Continue;
    end;

    InBlock := UpdateBlockState(AInput[I], InBlock);

    if IsUsesKeyword(AInput[I]) then
    begin
      FullBlock := AInput[I];
      J := I;
      BeforeSC := StripLineComment(FullBlock);

      while (Pos(';', BeforeSC) = 0) and (J + 1 < AInput.Count) do
      begin
        Inc(J);
        FullBlock := FullBlock + #10 + AInput[J];
        BeforeSC := StripLineComment(AInput[J]);
      end;

      if ContainsDirective(FullBlock) then
      begin
        while I <= J do
        begin
          AOutput.Add(AInput[I]);
          Inc(I);
        end;
        Continue;
      end;

      AfterUses := Trim(AInput[I]);
      if LowerCase(AfterUses) = 'uses' then
        UsesContent := ''
      else
        UsesContent := Trim(Copy(AfterUses, 5, Length(AfterUses)));

      J := I;
      while (Pos(';', StripLineComment(UsesContent)) = 0) and (J + 1 < AInput.Count) do
      begin
        Inc(J);
        UsesContent := UsesContent + ' ' + Trim(AInput[J]);
      end;

      Units := TStringList.Create;
      try
        while Pos(',', UsesContent) > 0 do
        begin
          Units.Add(Trim(Copy(UsesContent, 1, Pos(',', UsesContent) - 1)));
          UsesContent := Trim(Copy(UsesContent, Pos(',', UsesContent) + 1, Length(UsesContent)));
        end;
        UsesContent := Trim(UsesContent);
        if (Length(UsesContent) > 0) and (UsesContent[Length(UsesContent)] = ';') then
          UsesContent := Trim(Copy(UsesContent, 1, Length(UsesContent) - 1));
        if UsesContent <> '' then
          Units.Add(UsesContent);

        if Units.Count > 0 then
        begin
          Formatted := FormatUsesClause(Units);
          try
            AOutput.Add('uses');
            AOutput.AddStrings(Formatted);
          finally
            Formatted.Free;
          end;
        end
        else
          AOutput.Add(AInput[I]);
      finally
        Units.Free;
      end;

      I := J + 1;
    end
    else
    begin
      AOutput.Add(AInput[I]);
      Inc(I);
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  Code Analysis Helpers
  ═══════════════════════════════════════════════════════════════════════════ }

function IsIdentChar(C: Char): Boolean;
begin
  Result := C in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;

function IsFuncDeclStart(const ALine: string): Boolean;
var
  Trimmed: string;
begin
  Trimmed := LowerCase(Trim(ALine));
  Result := (Pos('function ', Trimmed) = 1) or (Pos('procedure ', Trimmed) = 1) or
            (Pos('constructor ', Trimmed) = 1) or (Pos('destructor ', Trimmed) = 1) or
            (Pos('class function ', Trimmed) = 1) or (Pos('class procedure ', Trimmed) = 1);
end;

function IsModifier(const AWord: string): Boolean;
var
  Lower: string;
begin
  Lower := LowerCase(AWord);
  Result := (Lower = 'const') or (Lower = 'var') or (Lower = 'out') or (Lower = 'constref');
end;

function HasAPrefix(const AName: string): Boolean;
begin
  Result := (Length(AName) > 1) and (AName[1] = 'A') and (AName[2] in ['A'..'Z']);
end;

function IsPascalCase(const AName: string): Boolean;
begin
  if Length(AName) = 0 then
    Exit(True);
  Result := AName[1] in ['A'..'Z'];
end;

function IsPascalKeyword(const AWord: string): Boolean;
const
  Keywords: array[0..14] of string = (
    'as', 'at', 'do', 'if', 'in', 'is', 'of', 'on', 'or', 'to',
    'and', 'end', 'for', 'not', 'set'
 );
var
  Lower: string;
  I: Integer;
begin
  Lower := LowerCase(AWord);
  for I := Low(Keywords) to High(Keywords) do
    if Lower = Keywords[I] then
      Exit(True);
  Result := False;
end;

function ExtractFuncName(const ADeclText: string): string;
var
  Trimmed, NamePart: string;
  SpacePos, ParenPos, DotPos, Idx: Integer;
begin
  Result := '';
  Trimmed := Trim(ADeclText);

  if LowerCase(Copy(Trimmed, 1, 6)) = 'class ' then
    Trimmed := Trim(Copy(Trimmed, 7, Length(Trimmed)));

  SpacePos := Pos(' ', Trimmed);
  if SpacePos = 0 then
    Exit;
  NamePart := Trim(Copy(Trimmed, SpacePos + 1, Length(Trimmed)));

  ParenPos := Pos('(', NamePart);
  if ParenPos > 0 then
    NamePart := Trim(Copy(NamePart, 1, ParenPos - 1));
  ParenPos := Pos(';', NamePart);
  if ParenPos > 0 then
    NamePart := Trim(Copy(NamePart, 1, ParenPos - 1));
  ParenPos := Pos(':', NamePart);
  if ParenPos > 0 then
    NamePart := Trim(Copy(NamePart, 1, ParenPos - 1));

  DotPos := 0;
  for Idx := Length(NamePart) downto 1 do
    if NamePart[Idx] = '.' then
    begin
      DotPos := Idx;
      Break;
    end;

  if DotPos > 0 then
    Result := Copy(NamePart, DotPos + 1, Length(NamePart))
  else
    Result := NamePart;
end;

function IsExternalDecl(const ALines: TStringList; AStartLine: Integer): Boolean;
var
  DeclText: string;
  K, Depth: Integer;
begin
  DeclText := ALines[AStartLine];
  Depth := 0;
  for K := 1 to Length(DeclText) do
  begin
    if DeclText[K] = '(' then Inc(Depth)
    else if DeclText[K] = ')' then Dec(Depth);
  end;
  while (Depth > 0) and (AStartLine + 1 < ALines.Count) do
  begin
    Inc(AStartLine);
    DeclText := DeclText + ' ' + ALines[AStartLine];
    for K := 1 to Length(ALines[AStartLine]) do
    begin
      if ALines[AStartLine][K] = '(' then Inc(Depth)
      else if ALines[AStartLine][K] = ')' then Dec(Depth);
    end;
  end;
  Result := Pos(' external ', LowerCase(DeclText)) > 0;
end;

function ReplaceWordInLine(const ALine, AOld, ANew: string;
  var AInBlock: Boolean): string;
var
  I, OldLen: Integer;
  InStr: Boolean;
begin
  Result := '';
  OldLen := Length(AOld);
  I := 1;
  InStr := False;

  while I <= Length(ALine) do
  begin
    if AInBlock then
    begin
      if ALine[I] = '}' then
        AInBlock := False;
      Result := Result + ALine[I];
      Inc(I);
      Continue;
    end;

    if InStr then
    begin
      if ALine[I] = '''' then
      begin
        if (I < Length(ALine)) and (ALine[I + 1] = '''') then
        begin
          Result := Result + ALine[I] + ALine[I + 1];
          Inc(I, 2);
          Continue;
        end;
        InStr := False;
      end;
      Result := Result + ALine[I];
      Inc(I);
      Continue;
    end;

    if ALine[I] = '''' then
    begin
      InStr := True;
      Result := Result + ALine[I];
      Inc(I);
      Continue;
    end;

    if ALine[I] = '{' then
    begin
      if (I < Length(ALine)) and (ALine[I + 1] = '$') then
      begin
        Result := Result + ALine[I];
        Inc(I);
        Continue;
      end;
      AInBlock := True;
      Result := Result + ALine[I];
      Inc(I);
      Continue;
    end;

    if (I < Length(ALine)) and (ALine[I] = '/') and (ALine[I + 1] = '/') then
    begin
      Result := Result + Copy(ALine, I, Length(ALine) - I + 1);
      Exit;
    end;

    if (I + OldLen - 1 <= Length(ALine)) and
       (CompareText(Copy(ALine, I, OldLen), AOld) = 0) then
    begin
      if ((I = 1) or (not IsIdentChar(ALine[I - 1]) and (ALine[I - 1] <> '.'))) and
         ((I + OldLen > Length(ALine)) or not IsIdentChar(ALine[I + OldLen])) then
      begin
        Result := Result + ANew;
        Inc(I, OldLen);
        Continue;
      end;
    end;

    Result := Result + ALine[I];
    Inc(I);
  end;
end;

function StripCodeLine(const ALine: string; var AInBlock: Boolean): string;
var
  I: Integer;
  InStr: Boolean;
begin
  SetLength(Result, Length(ALine));
  for I := 1 to Length(ALine) do
    Result[I] := ' ';

  I := 1;
  InStr := False;
  while I <= Length(ALine) do
  begin
    if AInBlock then
    begin
      if ALine[I] = '}' then
        AInBlock := False;
      Inc(I);
      Continue;
    end;
    if InStr then
    begin
      if ALine[I] = '''' then
      begin
        if (I < Length(ALine)) and (ALine[I + 1] = '''') then
        begin
          Inc(I, 2);
          Continue;
        end;
        InStr := False;
      end;
      Inc(I);
      Continue;
    end;
    if ALine[I] = '''' then
    begin
      InStr := True;
      Inc(I);
      Continue;
    end;
    if ALine[I] = '{' then
    begin
      AInBlock := True;
      Inc(I);
      Continue;
    end;
    if (I < Length(ALine)) and (ALine[I] = '/') and (ALine[I + 1] = '/') then
      Break;
    Result[I] := ALine[I];
    Inc(I);
  end;
end;

function CountKeywordOnLine(const AStripped, AKeyword: string): Integer;
var
  Lower: string;
  P, KLen: Integer;
begin
  Result := 0;
  Lower := LowerCase(AStripped);
  KLen := Length(AKeyword);
  P := 1;
  while P + KLen - 1 <= Length(Lower) do
  begin
    if (Copy(Lower, P, KLen) = AKeyword) and
       ((P = 1) or not IsIdentChar(Lower[P - 1])) and
       ((P + KLen > Length(Lower)) or not IsIdentChar(Lower[P + KLen])) then
    begin
      Inc(Result);
      P := P + KLen;
    end
    else
      Inc(P);
  end;
end;

function FindFuncEnd(const ALines: TStringList; ADeclEnd: Integer): Integer;
var
  I, Depth: Integer;
  Stripped: string;
  InBlock, FoundBegin: Boolean;
begin
  Result := -1;
  Depth := 0;
  FoundBegin := False;
  InBlock := False;

  for I := ADeclEnd + 1 to ALines.Count - 1 do
  begin
    Stripped := StripCodeLine(ALines[I], InBlock);

    if not FoundBegin then
    begin
      if IsFuncDeclStart(Stripped) then
        Exit(-1);
      if (CountKeywordOnLine(Stripped, 'implementation') > 0) or
         (CountKeywordOnLine(Stripped, 'type') > 0) or
         (CountKeywordOnLine(Stripped, 'interface') > 0) then
        Exit(-1);
    end;

    Depth := Depth + CountKeywordOnLine(Stripped, 'begin')
                    + CountKeywordOnLine(Stripped, 'try')
                    + CountKeywordOnLine(Stripped, 'case')
                    - CountKeywordOnLine(Stripped, 'end');

    if (not FoundBegin) and (CountKeywordOnLine(Stripped, 'begin') > 0) then
      FoundBegin := True;

    if FoundBegin and (Depth <= 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function FindDeclEnd(const ALines: TStringList; ADeclStart: Integer): Integer;
var
  K, Depth: Integer;
begin
  Result := ADeclStart;
  Depth := 0;
  for K := 1 to Length(ALines[ADeclStart]) do
  begin
    if ALines[ADeclStart][K] = '(' then Inc(Depth)
    else if ALines[ADeclStart][K] = ')' then Dec(Depth);
  end;
  while (Depth > 0) and (Result + 1 < ALines.Count) do
  begin
    Inc(Result);
    for K := 1 to Length(ALines[Result]) do
    begin
      if ALines[Result][K] = '(' then Inc(Depth)
      else if ALines[Result][K] = ')' then Dec(Depth);
    end;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  Auto-Fix: PascalCase Function Names
  ═══════════════════════════════════════════════════════════════════════════ }

function FixFuncNames(const ALines: TStringList): Boolean;
var
  I, J, K: Integer;
  FuncName, NewName: string;
  OldNames, NewNames: TStringList;
  InBlock: Boolean;
begin
  Result := False;
  OldNames := TStringList.Create;
  NewNames := TStringList.Create;
  try
    for I := 0 to ALines.Count - 1 do
    begin
      if IsFuncDeclStart(ALines[I]) and not IsExternalDecl(ALines, I) then
      begin
        FuncName := ExtractFuncName(ALines[I]);
        if (FuncName <> '') and not IsPascalCase(FuncName) then
        begin
          NewName := UpCase(FuncName[1]) + Copy(FuncName, 2, Length(FuncName));
          K := OldNames.IndexOf(FuncName);
          if K = -1 then
          begin
            OldNames.Add(FuncName);
            NewNames.Add(NewName);
          end;
        end;
      end;
    end;

    if OldNames.Count > 0 then
    begin
      Result := True;
      for K := 0 to OldNames.Count - 1 do
      begin
        InBlock := False;
        for J := 0 to ALines.Count - 1 do
          ALines[J] := ReplaceWordInLine(ALines[J],
            OldNames[K], NewNames[K], InBlock);
      end;
    end;
  finally
    OldNames.Free;
    NewNames.Free;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  Auto-Fix: Parameter A Prefix
  ═══════════════════════════════════════════════════════════════════════════ }

procedure ParseParamNames(const ADeclText: string;
  const AOldNames, ANewNames: TStringList);
var
  ParenStart, ParenEnd, ColonPos, Depth, K, Sp: Integer;
  Inner, GroupStr, Rest, NamesStr, ParamName, FirstWord, NewName: string;
  Groups: TStringList;
  Ch: Char;
  Current: string;
begin
  ParenStart := Pos('(', ADeclText);
  if ParenStart = 0 then
    Exit;

  Depth := 0;
  ParenEnd := 0;
  for K := ParenStart to Length(ADeclText) do
  begin
    if ADeclText[K] = '(' then Inc(Depth)
    else if ADeclText[K] = ')' then
    begin
      Dec(Depth);
      if Depth = 0 then
      begin
        ParenEnd := K;
        Break;
      end;
    end;
  end;
  if ParenEnd = 0 then
    Exit;

  Inner := Trim(Copy(ADeclText, ParenStart + 1, ParenEnd - ParenStart - 1));
  if Inner = '' then
    Exit;

  Groups := TStringList.Create;
  try
    Depth := 0;
    Current := '';
    for K := 1 to Length(Inner) do
    begin
      Ch := Inner[K];
      if Ch in ['(', '['] then
      begin
        Inc(Depth);
        Current := Current + Ch;
      end
      else if Ch in [')', ']'] then
      begin
        Dec(Depth);
        Current := Current + Ch;
      end
      else if (Ch = ';') and (Depth = 0) then
      begin
        Groups.Add(Trim(Current));
        Current := '';
      end
      else
        Current := Current + Ch;
    end;
    if Trim(Current) <> '' then
      Groups.Add(Trim(Current));

    for K := 0 to Groups.Count - 1 do
    begin
      GroupStr := Trim(Groups[K]);
      if GroupStr = '' then
        Continue;

      Rest := GroupStr;
      FirstWord := '';
      Sp := Pos(' ', Rest);
      if Sp > 0 then
        FirstWord := Copy(Rest, 1, Sp - 1);
      if IsModifier(FirstWord) then
        Rest := Trim(Copy(Rest, Length(FirstWord) + 1, Length(Rest)));

      ColonPos := Pos(':', Rest);
      if ColonPos > 0 then
        NamesStr := Trim(Copy(Rest, 1, ColonPos - 1))
      else
        NamesStr := Trim(Rest);

      while Pos(',', NamesStr) > 0 do
      begin
        ParamName := Trim(Copy(NamesStr, 1, Pos(',', NamesStr) - 1));
        NamesStr := Trim(Copy(NamesStr, Pos(',', NamesStr) + 1, Length(NamesStr)));
        if (ParamName <> 'Self') and (Length(ParamName) > 1) and
           not HasAPrefix(ParamName) then
        begin
          NewName := 'A' + UpCase(ParamName[1]) + Copy(ParamName, 2, Length(ParamName));
          if not IsPascalKeyword(NewName) and (AOldNames.IndexOf(ParamName) = -1) then
          begin
            AOldNames.Add(ParamName);
            ANewNames.Add(NewName);
          end;
        end;
      end;

      ParamName := Trim(NamesStr);
      if (ParamName <> '') and (ParamName <> 'Self') and (Length(ParamName) > 1) and
         not HasAPrefix(ParamName) then
      begin
        NewName := 'A' + UpCase(ParamName[1]) + Copy(ParamName, 2, Length(ParamName));
        if not IsPascalKeyword(NewName) and (AOldNames.IndexOf(ParamName) = -1) then
        begin
          AOldNames.Add(ParamName);
          ANewNames.Add(NewName);
        end;
      end;
    end;
  finally
    Groups.Free;
  end;
end;

function FixParamNames(const ALines: TStringList): Boolean;
var
  I, J, K, DeclEnd, BodyEnd: Integer;
  DeclText: string;
  OldNames, NewNames: TStringList;
  InBlock: Boolean;
begin
  Result := False;
  I := 0;
  while I < ALines.Count do
  begin
    if IsFuncDeclStart(ALines[I]) and not IsExternalDecl(ALines, I) then
    begin
      DeclEnd := FindDeclEnd(ALines, I);

      DeclText := ALines[I];
      for J := I + 1 to DeclEnd do
        DeclText := DeclText + ' ' + Trim(ALines[J]);

      OldNames := TStringList.Create;
      NewNames := TStringList.Create;
      try
        ParseParamNames(DeclText, OldNames, NewNames);

        if OldNames.Count > 0 then
        begin
          Result := True;

          BodyEnd := FindFuncEnd(ALines, DeclEnd);
          if BodyEnd = -1 then
            BodyEnd := DeclEnd;

          for K := 0 to OldNames.Count - 1 do
          begin
            InBlock := False;
            for J := I to BodyEnd do
              ALines[J] := ReplaceWordInLine(ALines[J],
                OldNames[K], NewNames[K], InBlock);
          end;
        end;
      finally
        OldNames.Free;
        NewNames.Free;
      end;

      I := DeclEnd + 1;
    end
    else
      Inc(I);
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  Auto-Fix: Stray Spaces
  ═══════════════════════════════════════════════════════════════════════════ }

function FixStraySpaces(const ALines: TStringList): Boolean;
var
  I, J, SpaceStart: Integer;
  Line: string;
  InStr, InLineComment, InBlockComment: Boolean;
begin
  Result := False;
  InBlockComment := False;
  for I := 0 to ALines.Count - 1 do
  begin
    Line := ALines[I];
    InStr := False;
    InLineComment := False;
    J := 1;
    while J <= Length(Line) do
    begin
      if InLineComment then
        Break;
      if InStr then
      begin
        if Line[J] = '''' then
          InStr := False;
        Inc(J);
        Continue;
      end;
      if InBlockComment then
      begin
        if Line[J] = '}' then
          InBlockComment := False;
        Inc(J);
        Continue;
      end;
      if Line[J] = '''' then
        InStr := True
      else if Line[J] = '{' then
        InBlockComment := True
      else if (J + 1 <= Length(Line)) and (Line[J] = '/') and (Line[J + 1] = '/') then
        InLineComment := True
      else if (Line[J] = ' ') and (J > 1) and (Line[J - 1] <> ' ') and (not (Line[J - 1] in [#9, '(', ','])) then
      begin
        SpaceStart := J;
        while (J + 1 <= Length(Line)) and (Line[J + 1] = ' ') do
          Inc(J);
        if (J + 1 <= Length(Line)) and (Line[J + 1] in [';', ')', ',']) then
        begin
          Delete(Line, SpaceStart, J - SpaceStart + 1);
          Result := True;
          J := SpaceStart;
          Continue;
        end;
      end;
      Inc(J);
    end;
    ALines[I] := Line;
  end;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  File Processing
  ═══════════════════════════════════════════════════════════════════════════ }

function FormatFile(const AFilePath: string): Boolean;
var
  Lines, ResultLines: TStringList;
begin
  Result := False;
  Lines := TStringList.Create;
  ResultLines := TStringList.Create;
  try
    Lines.LoadFromFile(AFilePath);

    FormatUsesInLines(Lines, ResultLines);
    FixFuncNames(ResultLines);
    FixParamNames(ResultLines);
    FixStraySpaces(ResultLines);

    if ResultLines.Text <> Lines.Text then
    begin
      Result := True;
      if Mode = rmFormat then
        ResultLines.SaveToFile(AFilePath);
    end;
  finally
    Lines.Free;
    ResultLines.Free;
  end;
end;

procedure CollectProjectFiles(const ARoot: string; const AFiles: TStringList);
var
  DprFiles, PasFiles: TStringList;
  I: Integer;
  BuildPas, FormatPas: string;
begin
  DprFiles := FindAllFiles(ARoot, '.dpr');
  PasFiles := FindAllFiles(ARoot + '/units', '.pas');
  try
    for I := 0 to DprFiles.Count - 1 do
      AFiles.Add(DprFiles[I]);
    for I := 0 to PasFiles.Count - 1 do
      AFiles.Add(PasFiles[I]);
    BuildPas := ARoot + '/build.pas';
    if FileExists(BuildPas) then
      AFiles.Add(BuildPas);
    FormatPas := ARoot + '/format.pas';
    if FileExists(FormatPas) then
      AFiles.Add(FormatPas);
  finally
    DprFiles.Free;
    PasFiles.Free;
  end;
  AFiles.Sort;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  Main
  ═══════════════════════════════════════════════════════════════════════════ }

var
  I: Integer;
  Root, Rel: string;
  Changed: Boolean;
begin
  Mode := rmFormat;
  InputFiles := TStringList.Create;
  FormattedCount := 0;
  CheckedCount := 0;

  try
    Root := GetCurrentDir;

    for I := 1 to ParamCount do
    begin
      if ParamStr(I) = '--check' then
        Mode := rmCheck
      else
        InputFiles.Add(ExpandFileName(ParamStr(I)));
    end;

    if InputFiles.Count = 0 then
      CollectProjectFiles(Root, InputFiles);

    for I := 0 to InputFiles.Count - 1 do
    begin
      if not FileExists(InputFiles[I]) then
        Continue;

      Inc(CheckedCount);
      Rel := ExtractRelativePath(Root + '/', InputFiles[I]);

      Changed := FormatFile(InputFiles[I]);
      if Changed then
      begin
        Inc(FormattedCount);
        if Mode = rmCheck then
          WriteLn('  needs formatting: ', Rel)
        else
          WriteLn('  formatted: ', Rel);
      end;
    end;

    WriteLn('');
    if FormattedCount > 0 then
    begin
      if Mode = rmCheck then
        WriteLn(FormattedCount, ' of ', CheckedCount, ' file(s) need formatting.')
      else
        WriteLn(FormattedCount, ' of ', CheckedCount, ' file(s) formatted.');
    end
    else
      WriteLn(CheckedCount, ' file(s) checked. All formatted correctly.');

    if (Mode = rmCheck) and (FormattedCount > 0) then
      Halt(1);
  finally
    InputFiles.Free;
  end;
end.
