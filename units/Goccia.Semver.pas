unit Goccia.Semver;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  EGocciaSemverError = class(Exception);
  EGocciaSemverTypeError = class(EGocciaSemverError);

  TGocciaSemverOptions = record
    Loose: Boolean;
    IncludePrerelease: Boolean;
    RTL: Boolean;
  end;

  TGocciaSemverIdentifierKind = (sikNumeric, sikString);

  TGocciaSemverIdentifier = record
    Kind: TGocciaSemverIdentifierKind;
    NumericValue: Int64;
    TextValue: string;
  end;

  TGocciaSemverIdentifierArray = array of TGocciaSemverIdentifier;
  TGocciaSemverStringArray = array of string;
  TGocciaSemverStringMatrix = array of TGocciaSemverStringArray;

  TGocciaSemver = record
    Options: TGocciaSemverOptions;
    Raw: string;
    Version: string;
    Major: Int64;
    Minor: Int64;
    Patch: Int64;
    Prerelease: TGocciaSemverIdentifierArray;
    Build: TGocciaSemverStringArray;
  end;

  TGocciaSemverComparator = record
    Operator: string;
    IsAny: Boolean;
    Semver: TGocciaSemver;
    Value: string;
  end;

  TGocciaSemverComparatorArray = array of TGocciaSemverComparator;
  TGocciaSemverComparatorSets = array of TGocciaSemverComparatorArray;

  TGocciaSemverRange = record
    Options: TGocciaSemverOptions;
    Raw: string;
    Formatted: string;
    SetOfComparators: TGocciaSemverComparatorSets;
  end;

const
  SEMVER_SPEC_VERSION = '2.0.0';
  MAX_SEMVER_LENGTH = 256;
  MAX_SAFE_COMPONENT_LENGTH = 16;
  MAX_SAFE_INTEGER = 9007199254740991;

  RELEASE_TYPE_MAJOR = 'major';
  RELEASE_TYPE_PREMAJOR = 'premajor';
  RELEASE_TYPE_MINOR = 'minor';
  RELEASE_TYPE_PREMINOR = 'preminor';
  RELEASE_TYPE_PATCH = 'patch';
  RELEASE_TYPE_PREPATCH = 'prepatch';
  RELEASE_TYPE_PRERELEASE = 'prerelease';
  RELEASE_TYPE_RELEASE = 'release';
  RELEASE_TYPE_PRE = 'pre';

  RELEASE_TYPES: array[0..6] of string = (
    RELEASE_TYPE_MAJOR,
    RELEASE_TYPE_PREMAJOR,
    RELEASE_TYPE_MINOR,
    RELEASE_TYPE_PREMINOR,
    RELEASE_TYPE_PATCH,
    RELEASE_TYPE_PREPATCH,
    RELEASE_TYPE_PRERELEASE
  );

function DefaultSemverOptions: TGocciaSemverOptions;

function ParseSemver(const AVersion: string; const AOptions: TGocciaSemverOptions;
  out ASemver: TGocciaSemver): Boolean;
function MustParseSemver(const AVersion: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemver;

function Valid(const AVersion: string; const AOptions: TGocciaSemverOptions): string;
function Clean(const AVersion: string; const AOptions: TGocciaSemverOptions): string;
function Compare(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): Integer;
function CompareLoose(const ALeft, ARight: string): Integer;
function CompareBuild(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): Integer;
function MajorOf(const AVersion: string;
  const AOptions: TGocciaSemverOptions): Int64;
function MinorOf(const AVersion: string;
  const AOptions: TGocciaSemverOptions): Int64;
function PatchOf(const AVersion: string;
  const AOptions: TGocciaSemverOptions): Int64;
function PrereleaseOf(const AVersion: string; const AOptions: TGocciaSemverOptions;
  out AIdentifiers: TGocciaSemverIdentifierArray): Boolean;
function Diff(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): string;
function TryIncrement(const AVersion, ARelease: string;
  const AOptions: TGocciaSemverOptions; const AIdentifier: string;
  const AHasIdentifierBase: Boolean; const AIdentifierBase: Boolean;
  out AResult: string): Boolean;
function Increment(const AVersion, ARelease: string;
  const AOptions: TGocciaSemverOptions; const AIdentifier: string;
  const AHasIdentifierBase: Boolean; const AIdentifierBase: Boolean): string;
function Cmp(const ALeft, AOperator, ARight: string;
  const AOptions: TGocciaSemverOptions): Boolean;
function Coerce(const AVersion: string; const AOptions: TGocciaSemverOptions;
  out ASemver: TGocciaSemver): Boolean;

function ParseComparator(const AComparator: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverComparator;
function ParseRange(const ARange: string; const AOptions: TGocciaSemverOptions;
  out AParsedRange: TGocciaSemverRange): Boolean;
function MustParseRange(const ARange: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverRange;
function ValidRange(const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
function ComparatorIntersects(const ALeft, ARight: TGocciaSemverComparator;
  const AOptions: TGocciaSemverOptions): Boolean;
function Satisfies(const AVersion, ARange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
function RangeIntersects(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): Boolean;
function MaxSatisfying(const AVersions: array of string; const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
function MinSatisfying(const AVersions: array of string; const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
function MinVersion(const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
function Outside(const AVersion, ARange, AHilo: string;
  const AOptions: TGocciaSemverOptions): Boolean;
function GreaterThanRange(const AVersion, ARange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
function LessThanRange(const AVersion, ARange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
function SimplifyRange(const AVersions: array of string; const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
function ToComparators(const ARange: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverStringMatrix;
function IsSubset(const ASubRange, ADomainRange: string;
  const AOptions: TGocciaSemverOptions): Boolean;

function SemverToString(const ASemver: TGocciaSemver): string;
function ComparatorToString(const AComparator: TGocciaSemverComparator): string;
function RangeToString(const ARange: TGocciaSemverRange): string;

implementation

type
  TGocciaPartialSemver = record
    HasMajor: Boolean;
    HasMinor: Boolean;
    HasPatch: Boolean;
    MajorText: string;
    MinorText: string;
    PatchText: string;
    PrereleaseText: string;
    BuildText: string;
  end;

function DefaultSemverOptions: TGocciaSemverOptions;
begin
  Result.Loose := False;
  Result.IncludePrerelease := False;
  Result.RTL := False;
end;

function IsDigit(const AChar: Char): Boolean; inline;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function IsAlpha(const AChar: Char): Boolean; inline;
begin
  Result := ((AChar >= 'a') and (AChar <= 'z')) or
    ((AChar >= 'A') and (AChar <= 'Z'));
end;

function IsAlphaNumericDash(const AChar: Char): Boolean; inline;
begin
  Result := IsDigit(AChar) or IsAlpha(AChar) or (AChar = '-');
end;

function IsWhitespace(const AChar: Char): Boolean; inline;
begin
  Result := (AChar = ' ') or (AChar = #9) or (AChar = #10) or
    (AChar = #13);
end;

function TrimAndStripVersionPrefix(const AValue: string): string;
begin
  Result := Trim(AValue);
  while (Result <> '') and ((Result[1] = '=') or (Result[1] = 'v') or
    (Result[1] = 'V')) do
  begin
    Delete(Result, 1, 1);
    Result := TrimLeft(Result);
  end;
end;

function CollapseWhitespace(const AValue: string): string;
var
  I: Integer;
  InSpace: Boolean;
begin
  Result := '';
  InSpace := False;
  for I := 1 to Length(AValue) do
  begin
    if IsWhitespace(AValue[I]) then
    begin
      if not InSpace then
      begin
        Result := Result + ' ';
        InSpace := True;
      end;
    end
    else
    begin
      Result := Result + AValue[I];
      InSpace := False;
    end;
  end;
  Result := Trim(Result);
end;

function SplitByChar(const AValue: string; const ADelimiter: Char): TGocciaSemverStringArray;
var
  I, StartIndex, PartIndex: Integer;
begin
  SetLength(Result, 0);
  StartIndex := 1;
  PartIndex := 0;
  for I := 1 to Length(AValue) do
    if AValue[I] = ADelimiter then
    begin
      SetLength(Result, PartIndex + 1);
      Result[PartIndex] := Copy(AValue, StartIndex, I - StartIndex);
      Inc(PartIndex);
      StartIndex := I + 1;
    end;

  SetLength(Result, PartIndex + 1);
  Result[PartIndex] := Copy(AValue, StartIndex, Length(AValue) - StartIndex + 1);
end;

function SplitByString(const AValue, ADelimiter: string): TGocciaSemverStringArray;
var
  DelimiterPos, StartIndex, PartIndex: Integer;
begin
  if ADelimiter = '' then
  begin
    SetLength(Result, 1);
    Result[0] := AValue;
    Exit;
  end;

  SetLength(Result, 0);
  StartIndex := 1;
  PartIndex := 0;
  DelimiterPos := Pos(ADelimiter, AValue);
  while DelimiterPos > 0 do
  begin
    SetLength(Result, PartIndex + 1);
    Result[PartIndex] := Copy(AValue, StartIndex, DelimiterPos - StartIndex);
    Inc(PartIndex);
    StartIndex := DelimiterPos + Length(ADelimiter);
    DelimiterPos := Pos(ADelimiter, Copy(AValue, StartIndex, MaxInt));
    if DelimiterPos > 0 then
      DelimiterPos := StartIndex + DelimiterPos - 1;
  end;

  SetLength(Result, PartIndex + 1);
  Result[PartIndex] := Copy(AValue, StartIndex, Length(AValue) - StartIndex + 1);
end;

function StartsWith(const AValue, APrefix: string): Boolean;
begin
  Result := Copy(AValue, 1, Length(APrefix)) = APrefix;
end;

function EndsWith(const AValue, ASuffix: string): Boolean;
begin
  Result := (Length(AValue) >= Length(ASuffix)) and
    (Copy(AValue, Length(AValue) - Length(ASuffix) + 1, Length(ASuffix)) = ASuffix);
end;

function JoinStringArray(const AValues: TGocciaSemverStringArray;
  const ADelimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AValues) do
  begin
    if I > 0 then
      Result := Result + ADelimiter;
    Result := Result + AValues[I];
  end;
end;

function JoinIdentifierArray(const AValues: TGocciaSemverIdentifierArray;
  const ADelimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AValues) do
  begin
    if I > 0 then
      Result := Result + ADelimiter;
    if AValues[I].Kind = sikNumeric then
      Result := Result + IntToStr(AValues[I].NumericValue)
    else
      Result := Result + AValues[I].TextValue;
  end;
end;

function TryParseInt64Exact(const AValue: string; out ANumber: Int64): Boolean;
var
  Code: Integer;
begin
  Val(AValue, ANumber, Code);
  Result := Code = 0;
end;

function IsWildcardIdentifier(const AValue: string): Boolean;
begin
  Result := (AValue = '') or SameText(AValue, 'x') or (AValue = '*');
end;

function IsNumericIdentifierText(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := AValue <> '';
  if not Result then
    Exit;
  for I := 1 to Length(AValue) do
    if not IsDigit(AValue[I]) then
      Exit(False);
end;

function ValidateNumericComponent(const AValue: string): Int64;
begin
  if AValue = '' then
    raise EGocciaSemverTypeError.Create('Invalid numeric component');
  if (Length(AValue) > 1) and (AValue[1] = '0') then
    raise EGocciaSemverTypeError.Create('Invalid numeric component');
  if not TryParseInt64Exact(AValue, Result) then
    raise EGocciaSemverTypeError.Create('Invalid numeric component');
  if (Result < 0) or (Result > MAX_SAFE_INTEGER) then
    raise EGocciaSemverTypeError.Create('SemVer component exceeds max safe integer');
end;

function ValidateIdentifierCharacters(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := AValue <> '';
  if not Result then
    Exit;
  for I := 1 to Length(AValue) do
    if not IsAlphaNumericDash(AValue[I]) then
      Exit(False);
end;

function ParseIdentifiers(const AValue: string; const AAllowLeadingZeros: Boolean;
  out AIdentifiers: TGocciaSemverIdentifierArray): Boolean;
var
  Parts: TGocciaSemverStringArray;
  I: Integer;
  NumericValue: Int64;
begin
  if AValue = '' then
  begin
    SetLength(AIdentifiers, 0);
    Exit(True);
  end;

  Parts := SplitByChar(AValue, '.');
  SetLength(AIdentifiers, Length(Parts));
  for I := 0 to High(Parts) do
  begin
    if not ValidateIdentifierCharacters(Parts[I]) then
      Exit(False);
    if IsNumericIdentifierText(Parts[I]) then
    begin
      if (not AAllowLeadingZeros) and (Length(Parts[I]) > 1) and
        (Parts[I][1] = '0') then
        Exit(False);
      if not TryParseInt64Exact(Parts[I], NumericValue) then
        Exit(False);
      if NumericValue > MAX_SAFE_INTEGER then
        Exit(False);
      AIdentifiers[I].Kind := sikNumeric;
      AIdentifiers[I].NumericValue := NumericValue;
      AIdentifiers[I].TextValue := Parts[I];
    end
    else
    begin
      AIdentifiers[I].Kind := sikString;
      AIdentifiers[I].NumericValue := 0;
      AIdentifiers[I].TextValue := Parts[I];
    end;
  end;
  Result := True;
end;

function ParseBuildIdentifiers(const AValue: string;
  out AIdentifiers: TGocciaSemverStringArray): Boolean;
var
  Parts: TGocciaSemverStringArray;
  I: Integer;
begin
  if AValue = '' then
  begin
    SetLength(AIdentifiers, 0);
    Exit(True);
  end;

  Parts := SplitByChar(AValue, '.');
  SetLength(AIdentifiers, Length(Parts));
  for I := 0 to High(Parts) do
  begin
    if not ValidateIdentifierCharacters(Parts[I]) then
      Exit(False);
    AIdentifiers[I] := Parts[I];
  end;
  Result := True;
end;

function TryParseLoosePatchSuffix(const AValue: string; out ANumericPart,
  APrereleasePart: string): Boolean;
var
  I: Integer;
begin
  ANumericPart := '';
  APrereleasePart := '';
  I := 1;
  while (I <= Length(AValue)) and IsDigit(AValue[I]) do
  begin
    ANumericPart := ANumericPart + AValue[I];
    Inc(I);
  end;
  APrereleasePart := Copy(AValue, I, MaxInt);
  Result := (ANumericPart <> '') and (APrereleasePart <> '');
end;

function TryParsePartialSemver(const AValue: string;
  const AOptions: TGocciaSemverOptions; out APartial: TGocciaPartialSemver): Boolean;
var
  Working, BasePart, BuildPart, PrePart: string;
  HyphenPos, PlusPos: Integer;
  Parts: TGocciaSemverStringArray;
  NumericPart, LoosePrerelease: string;
begin
  FillChar(APartial, SizeOf(APartial), 0);
  Working := TrimAndStripVersionPrefix(AValue);
  if (Working = '') or (Length(Working) > MAX_SEMVER_LENGTH) then
    Exit(False);

  PlusPos := Pos('+', Working);
  if PlusPos > 0 then
  begin
    BuildPart := Copy(Working, PlusPos + 1, MaxInt);
    BasePart := Copy(Working, 1, PlusPos - 1);
  end
  else
  begin
    BuildPart := '';
    BasePart := Working;
  end;

  HyphenPos := Pos('-', BasePart);
  if HyphenPos > 0 then
  begin
    PrePart := Copy(BasePart, HyphenPos + 1, MaxInt);
    BasePart := Copy(BasePart, 1, HyphenPos - 1);
  end
  else
    PrePart := '';

  Parts := SplitByChar(BasePart, '.');
  if (Length(Parts) < 1) or (Length(Parts) > 3) then
    Exit(False);

  APartial.HasMajor := Length(Parts) >= 1;
  APartial.MajorText := Parts[0];
  if Length(Parts) >= 2 then
  begin
    APartial.HasMinor := True;
    APartial.MinorText := Parts[1];
  end;
  if Length(Parts) >= 3 then
  begin
    APartial.HasPatch := True;
    APartial.PatchText := Parts[2];
  end;

  if AOptions.Loose and (PrePart = '') and APartial.HasPatch and
    TryParseLoosePatchSuffix(APartial.PatchText, NumericPart, LoosePrerelease) then
  begin
    APartial.PatchText := NumericPart;
    PrePart := LoosePrerelease;
    if StartsWith(PrePart, '.') then
      Delete(PrePart, 1, 1);
  end;

  APartial.PrereleaseText := PrePart;
  APartial.BuildText := BuildPart;
  Result := True;
end;

function NormalizeVersion(const ASemver: TGocciaSemver): string;
begin
  Result := IntToStr(ASemver.Major) + '.' + IntToStr(ASemver.Minor) + '.' +
    IntToStr(ASemver.Patch);
  if Length(ASemver.Prerelease) > 0 then
    Result := Result + '-' + JoinIdentifierArray(ASemver.Prerelease, '.');
end;

function SemverToString(const ASemver: TGocciaSemver): string;
begin
  Result := NormalizeVersion(ASemver);
  if Length(ASemver.Build) > 0 then
    Result := Result + '+' + JoinStringArray(ASemver.Build, '.');
end;

function CompareIdentifiers(const ALeft,
  ARight: TGocciaSemverIdentifier): Integer;
var
  LeftIsNumeric, RightIsNumeric: Boolean;
  LeftNumeric, RightNumeric: Int64;
begin
  if (ALeft.Kind = sikNumeric) and (ARight.Kind = sikNumeric) then
  begin
    if ALeft.NumericValue < ARight.NumericValue then
      Exit(-1);
    if ALeft.NumericValue > ARight.NumericValue then
      Exit(1);
    Exit(0);
  end;

  LeftIsNumeric := ALeft.Kind = sikNumeric;
  RightIsNumeric := ARight.Kind = sikNumeric;
  if (not LeftIsNumeric) and IsNumericIdentifierText(ALeft.TextValue) then
  begin
    LeftIsNumeric := TryParseInt64Exact(ALeft.TextValue, LeftNumeric);
  end
  else
    LeftNumeric := ALeft.NumericValue;

  if (not RightIsNumeric) and IsNumericIdentifierText(ARight.TextValue) then
  begin
    RightIsNumeric := TryParseInt64Exact(ARight.TextValue, RightNumeric);
  end
  else
    RightNumeric := ARight.NumericValue;

  if LeftIsNumeric and RightIsNumeric then
  begin
    if LeftNumeric < RightNumeric then
      Exit(-1);
    if LeftNumeric > RightNumeric then
      Exit(1);
    Exit(0);
  end;

  if LeftIsNumeric and not RightIsNumeric then
    Exit(-1);
  if RightIsNumeric and not LeftIsNumeric then
    Exit(1);

  if ALeft.TextValue < ARight.TextValue then
    Result := -1
  else if ALeft.TextValue > ARight.TextValue then
    Result := 1
  else
    Result := 0;
end;

function CompareSemverMain(const ALeft, ARight: TGocciaSemver): Integer;
begin
  if ALeft.Major < ARight.Major then
    Exit(-1);
  if ALeft.Major > ARight.Major then
    Exit(1);
  if ALeft.Minor < ARight.Minor then
    Exit(-1);
  if ALeft.Minor > ARight.Minor then
    Exit(1);
  if ALeft.Patch < ARight.Patch then
    Exit(-1);
  if ALeft.Patch > ARight.Patch then
    Exit(1);
  Result := 0;
end;

function CompareSemverPrerelease(const ALeft, ARight: TGocciaSemver): Integer;
var
  I: Integer;
begin
  if (Length(ALeft.Prerelease) > 0) and (Length(ARight.Prerelease) = 0) then
    Exit(-1);
  if (Length(ALeft.Prerelease) = 0) and (Length(ARight.Prerelease) > 0) then
    Exit(1);
  if (Length(ALeft.Prerelease) = 0) and (Length(ARight.Prerelease) = 0) then
    Exit(0);

  I := 0;
  while True do
  begin
    if (I > High(ALeft.Prerelease)) and (I > High(ARight.Prerelease)) then
      Exit(0);
    if I > High(ALeft.Prerelease) then
      Exit(-1);
    if I > High(ARight.Prerelease) then
      Exit(1);
    Result := CompareIdentifiers(ALeft.Prerelease[I], ARight.Prerelease[I]);
    if Result <> 0 then
      Exit;
    Inc(I);
  end;
end;

function CompareSemverBuild(const ALeft, ARight: TGocciaSemver): Integer;
var
  I: Integer;
  LeftId, RightId: TGocciaSemverIdentifier;
begin
  I := 0;
  while True do
  begin
    if (I > High(ALeft.Build)) and (I > High(ARight.Build)) then
      Exit(0);
    if I > High(ALeft.Build) then
      Exit(-1);
    if I > High(ARight.Build) then
      Exit(1);
    LeftId.Kind := sikString;
    LeftId.TextValue := ALeft.Build[I];
    LeftId.NumericValue := 0;
    RightId.Kind := sikString;
    RightId.TextValue := ARight.Build[I];
    RightId.NumericValue := 0;
    Result := CompareIdentifiers(LeftId, RightId);
    if Result <> 0 then
      Exit;
    Inc(I);
  end;
end;

function ParseSemver(const AVersion: string; const AOptions: TGocciaSemverOptions;
  out ASemver: TGocciaSemver): Boolean;
var
  Partial: TGocciaPartialSemver;
begin
  Result := TryParsePartialSemver(AVersion, AOptions, Partial);
  if not Result then
    Exit;
  if not (Partial.HasMajor and Partial.HasMinor and Partial.HasPatch) then
    Exit(False);
  if IsWildcardIdentifier(Partial.MajorText) or IsWildcardIdentifier(Partial.MinorText) or
    IsWildcardIdentifier(Partial.PatchText) then
    Exit(False);

  try
    ASemver.Options := AOptions;
    ASemver.Raw := Trim(AVersion);
    ASemver.Major := ValidateNumericComponent(Partial.MajorText);
    ASemver.Minor := ValidateNumericComponent(Partial.MinorText);
    ASemver.Patch := ValidateNumericComponent(Partial.PatchText);
    if not ParseIdentifiers(Partial.PrereleaseText, False, ASemver.Prerelease) then
      Exit(False);
    if not ParseBuildIdentifiers(Partial.BuildText, ASemver.Build) then
      Exit(False);
    ASemver.Version := NormalizeVersion(ASemver);
    Result := True;
  except
    on EGocciaSemverError do
      Result := False;
  end;
end;

function MustParseSemver(const AVersion: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemver;
begin
  if not ParseSemver(AVersion, AOptions, Result) then
    raise EGocciaSemverTypeError.Create('Invalid Version: ' + AVersion);
end;

function Valid(const AVersion: string; const AOptions: TGocciaSemverOptions): string;
var
  SemverValue: TGocciaSemver;
begin
  if ParseSemver(AVersion, AOptions, SemverValue) then
    Result := SemverValue.Version
  else
    Result := '';
end;

function Clean(const AVersion: string; const AOptions: TGocciaSemverOptions): string;
var
  SemverValue: TGocciaSemver;
begin
  if ParseSemver(TrimAndStripVersionPrefix(AVersion), AOptions, SemverValue) then
    Result := SemverValue.Version
  else
    Result := '';
end;

function CompareSemverValues(const ALeft, ARight: TGocciaSemver): Integer;
begin
  Result := CompareSemverMain(ALeft, ARight);
  if Result <> 0 then
    Exit;
  Result := CompareSemverPrerelease(ALeft, ARight);
end;

function Compare(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): Integer;
var
  LeftSemver, RightSemver: TGocciaSemver;
begin
  LeftSemver := MustParseSemver(ALeft, AOptions);
  RightSemver := MustParseSemver(ARight, AOptions);
  Result := CompareSemverValues(LeftSemver, RightSemver);
end;

function CompareLoose(const ALeft, ARight: string): Integer;
var
  Options: TGocciaSemverOptions;
begin
  Options := DefaultSemverOptions;
  Options.Loose := True;
  Result := Compare(ALeft, ARight, Options);
end;

function CompareBuild(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): Integer;
var
  LeftSemver, RightSemver: TGocciaSemver;
begin
  LeftSemver := MustParseSemver(ALeft, AOptions);
  RightSemver := MustParseSemver(ARight, AOptions);
  Result := CompareSemverValues(LeftSemver, RightSemver);
  if Result = 0 then
    Result := CompareSemverBuild(LeftSemver, RightSemver);
end;

function MajorOf(const AVersion: string;
  const AOptions: TGocciaSemverOptions): Int64;
begin
  Result := MustParseSemver(AVersion, AOptions).Major;
end;

function MinorOf(const AVersion: string;
  const AOptions: TGocciaSemverOptions): Int64;
begin
  Result := MustParseSemver(AVersion, AOptions).Minor;
end;

function PatchOf(const AVersion: string;
  const AOptions: TGocciaSemverOptions): Int64;
begin
  Result := MustParseSemver(AVersion, AOptions).Patch;
end;

function PrereleaseOf(const AVersion: string; const AOptions: TGocciaSemverOptions;
  out AIdentifiers: TGocciaSemverIdentifierArray): Boolean;
var
  Parsed: TGocciaSemver;
begin
  if not ParseSemver(AVersion, AOptions, Parsed) then
    Exit(False);
  AIdentifiers := Parsed.Prerelease;
  Result := Length(AIdentifiers) > 0;
end;

function Diff(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): string;
var
  LeftSemver, RightSemver, HighVersion, LowVersion: TGocciaSemver;
  Comparison: Integer;
  HighHasPre, LowHasPre: Boolean;
  Prefix: string;
begin
  LeftSemver := MustParseSemver(ALeft, AOptions);
  RightSemver := MustParseSemver(ARight, AOptions);
  Comparison := CompareSemverValues(LeftSemver, RightSemver);
  if Comparison = 0 then
    Exit('');

  if Comparison > 0 then
  begin
    HighVersion := LeftSemver;
    LowVersion := RightSemver;
  end
  else
  begin
    HighVersion := RightSemver;
    LowVersion := LeftSemver;
  end;

  HighHasPre := Length(HighVersion.Prerelease) > 0;
  LowHasPre := Length(LowVersion.Prerelease) > 0;

  if LowHasPre and not HighHasPre then
  begin
    if (LowVersion.Patch = 0) and (LowVersion.Minor = 0) then
      Exit(RELEASE_TYPE_MAJOR);
    if CompareSemverMain(LowVersion, HighVersion) = 0 then
    begin
      if (LowVersion.Minor <> 0) and (LowVersion.Patch = 0) then
        Exit(RELEASE_TYPE_MINOR);
      Exit(RELEASE_TYPE_PATCH);
    end;
  end;

  if HighHasPre then
    Prefix := 'pre'
  else
    Prefix := '';

  if LeftSemver.Major <> RightSemver.Major then
    Exit(Prefix + RELEASE_TYPE_MAJOR);
  if LeftSemver.Minor <> RightSemver.Minor then
    Exit(Prefix + RELEASE_TYPE_MINOR);
  if LeftSemver.Patch <> RightSemver.Patch then
    Exit(Prefix + RELEASE_TYPE_PATCH);
  Result := RELEASE_TYPE_PRERELEASE;
end;

function IdentifierEqualsText(const AIdentifier: TGocciaSemverIdentifier;
  const AText: string): Boolean;
begin
  if AIdentifier.Kind = sikNumeric then
    Result := AText = IntToStr(AIdentifier.NumericValue)
  else
    Result := AIdentifier.TextValue = AText;
end;

function ReleaseTypeIsSupported(const ARelease: string): Boolean;
var
  ReleaseType: string;
begin
  if ARelease = RELEASE_TYPE_RELEASE then
    Exit(True);
  if ARelease = RELEASE_TYPE_PRE then
    Exit(True);
  for ReleaseType in RELEASE_TYPES do
    if ReleaseType = ARelease then
      Exit(True);
  Result := False;
end;

procedure ValidatePrereleaseIdentifier(const AIdentifier: string;
  const AOptions: TGocciaSemverOptions);
var
  DummyIdentifiers: TGocciaSemverIdentifierArray;
begin
  if AIdentifier = '' then
    raise EGocciaSemverError.Create('invalid increment argument: identifier is empty');
  if not ParseIdentifiers(AIdentifier, False, DummyIdentifiers) then
    raise EGocciaSemverError.Create('invalid identifier: ' + AIdentifier);
  if Length(DummyIdentifiers) <> 1 then
    raise EGocciaSemverError.Create('invalid identifier: ' + AIdentifier);
end;

procedure ResetPrerelease(var ASemver: TGocciaSemver);
begin
  SetLength(ASemver.Prerelease, 0);
end;

procedure SetPrereleaseIdentifier(var ASemver: TGocciaSemver;
  const AIdentifier: string; const AHasIdentifierBase, AIdentifierBase: Boolean);
var
  BaseValue: Int64;
begin
  if AHasIdentifierBase and AIdentifierBase then
    BaseValue := 1
  else
    BaseValue := 0;

  if Length(ASemver.Prerelease) = 0 then
  begin
    SetLength(ASemver.Prerelease, 1);
    ASemver.Prerelease[0].Kind := sikNumeric;
    ASemver.Prerelease[0].NumericValue := BaseValue;
    ASemver.Prerelease[0].TextValue := IntToStr(BaseValue);
  end
  else
  begin
    while Length(ASemver.Prerelease) > 0 do
    begin
      if ASemver.Prerelease[High(ASemver.Prerelease)].Kind = sikNumeric then
      begin
        Inc(ASemver.Prerelease[High(ASemver.Prerelease)].NumericValue);
        ASemver.Prerelease[High(ASemver.Prerelease)].TextValue :=
          IntToStr(ASemver.Prerelease[High(ASemver.Prerelease)].NumericValue);
        Break;
      end;
      if High(ASemver.Prerelease) = 0 then
        Break;
      SetLength(ASemver.Prerelease, Length(ASemver.Prerelease) - 1);
    end;
    if (Length(ASemver.Prerelease) = 1) and
      (ASemver.Prerelease[0].Kind <> sikNumeric) then
    begin
      SetLength(ASemver.Prerelease, 2);
      ASemver.Prerelease[1].Kind := sikNumeric;
      ASemver.Prerelease[1].NumericValue := BaseValue;
      ASemver.Prerelease[1].TextValue := IntToStr(BaseValue);
    end;
  end;

  if AIdentifier <> '' then
  begin
    if (Length(ASemver.Prerelease) > 0) and
      IdentifierEqualsText(ASemver.Prerelease[0], AIdentifier) then
    begin
      if (Length(ASemver.Prerelease) > 1) and
        (ASemver.Prerelease[1].Kind <> sikNumeric) then
      begin
        if AHasIdentifierBase and not AIdentifierBase then
          SetLength(ASemver.Prerelease, 1)
        else
        begin
          SetLength(ASemver.Prerelease, 2);
          ASemver.Prerelease[1].Kind := sikNumeric;
          ASemver.Prerelease[1].NumericValue := BaseValue;
          ASemver.Prerelease[1].TextValue := IntToStr(BaseValue);
        end;
      end;
    end
    else
    begin
      if AHasIdentifierBase and not AIdentifierBase then
        SetLength(ASemver.Prerelease, 1)
      else
        SetLength(ASemver.Prerelease, 2);
      ASemver.Prerelease[0].Kind := sikString;
      ASemver.Prerelease[0].NumericValue := 0;
      ASemver.Prerelease[0].TextValue := AIdentifier;
      if not (AHasIdentifierBase and not AIdentifierBase) then
      begin
        ASemver.Prerelease[1].Kind := sikNumeric;
        ASemver.Prerelease[1].NumericValue := BaseValue;
        ASemver.Prerelease[1].TextValue := IntToStr(BaseValue);
      end;
    end;
  end;
end;

function TryIncrement(const AVersion, ARelease: string;
  const AOptions: TGocciaSemverOptions; const AIdentifier: string;
  const AHasIdentifierBase: Boolean; const AIdentifierBase: Boolean;
  out AResult: string): Boolean;
var
  SemverValue: TGocciaSemver;
begin
  Result := False;
  try
    if not ReleaseTypeIsSupported(ARelease) then
      raise EGocciaSemverError.Create('invalid increment argument: ' + ARelease);
    SemverValue := MustParseSemver(AVersion, AOptions);
    if StartsWith(ARelease, 'pre') and (ARelease <> RELEASE_TYPE_PRE) then
      ValidatePrereleaseIdentifier(AIdentifier, AOptions);

    if ARelease = RELEASE_TYPE_PREMAJOR then
    begin
      ResetPrerelease(SemverValue);
      SemverValue.Patch := 0;
      SemverValue.Minor := 0;
      Inc(SemverValue.Major);
      SetPrereleaseIdentifier(SemverValue, AIdentifier, AHasIdentifierBase, AIdentifierBase);
    end
    else if ARelease = RELEASE_TYPE_PREMINOR then
    begin
      ResetPrerelease(SemverValue);
      SemverValue.Patch := 0;
      Inc(SemverValue.Minor);
      SetPrereleaseIdentifier(SemverValue, AIdentifier, AHasIdentifierBase, AIdentifierBase);
    end
    else if ARelease = RELEASE_TYPE_PREPATCH then
    begin
      ResetPrerelease(SemverValue);
      Inc(SemverValue.Patch);
      SetPrereleaseIdentifier(SemverValue, AIdentifier, AHasIdentifierBase, AIdentifierBase);
    end
    else if ARelease = RELEASE_TYPE_PRERELEASE then
    begin
      if Length(SemverValue.Prerelease) = 0 then
      begin
        Inc(SemverValue.Patch);
        ResetPrerelease(SemverValue);
      end;
      SetPrereleaseIdentifier(SemverValue, AIdentifier, AHasIdentifierBase, AIdentifierBase);
    end
    else if ARelease = RELEASE_TYPE_RELEASE then
    begin
      if Length(SemverValue.Prerelease) = 0 then
        raise EGocciaSemverError.Create('version ' + AVersion + ' is not a prerelease');
      ResetPrerelease(SemverValue);
    end
    else if ARelease = RELEASE_TYPE_MAJOR then
    begin
      if (SemverValue.Minor <> 0) or (SemverValue.Patch <> 0) or
        (Length(SemverValue.Prerelease) = 0) then
        Inc(SemverValue.Major);
      SemverValue.Minor := 0;
      SemverValue.Patch := 0;
      ResetPrerelease(SemverValue);
    end
    else if ARelease = RELEASE_TYPE_MINOR then
    begin
      if (SemverValue.Patch <> 0) or (Length(SemverValue.Prerelease) = 0) then
        Inc(SemverValue.Minor);
      SemverValue.Patch := 0;
      ResetPrerelease(SemverValue);
    end
    else if ARelease = RELEASE_TYPE_PATCH then
    begin
      if Length(SemverValue.Prerelease) = 0 then
        Inc(SemverValue.Patch);
      ResetPrerelease(SemverValue);
    end
    else if ARelease = RELEASE_TYPE_PRE then
    begin
      SetPrereleaseIdentifier(SemverValue, AIdentifier, AHasIdentifierBase, AIdentifierBase);
    end;

    SemverValue.Version := NormalizeVersion(SemverValue);
    SemverValue.Raw := SemverToString(SemverValue);
    AResult := SemverValue.Version;
    Result := True;
  except
    on EGocciaSemverError do
      Result := False;
  end;
end;

function Increment(const AVersion, ARelease: string;
  const AOptions: TGocciaSemverOptions; const AIdentifier: string;
  const AHasIdentifierBase: Boolean; const AIdentifierBase: Boolean): string;
begin
  if not TryIncrement(AVersion, ARelease, AOptions, AIdentifier,
    AHasIdentifierBase, AIdentifierBase, Result) then
    raise EGocciaSemverError.Create('invalid increment');
end;

function Cmp(const ALeft, AOperator, ARight: string;
  const AOptions: TGocciaSemverOptions): Boolean;
begin
  if AOperator = '===' then
    Exit(ALeft = ARight);
  if AOperator = '!==' then
    Exit(ALeft <> ARight);
  if (AOperator = '') or (AOperator = '=') or (AOperator = '==') then
    Exit(Compare(ALeft, ARight, AOptions) = 0);
  if AOperator = '!=' then
    Exit(Compare(ALeft, ARight, AOptions) <> 0);
  if AOperator = '>' then
    Exit(Compare(ALeft, ARight, AOptions) > 0);
  if AOperator = '>=' then
    Exit(Compare(ALeft, ARight, AOptions) >= 0);
  if AOperator = '<' then
    Exit(Compare(ALeft, ARight, AOptions) < 0);
  if AOperator = '<=' then
    Exit(Compare(ALeft, ARight, AOptions) <= 0);
    raise EGocciaSemverTypeError.Create('Invalid operator: ' + AOperator);
end;

function CoerceMatchFrom(const AVersion: string; const AStart: Integer;
  const AIncludePrerelease: Boolean; out AMatchText: string): Boolean;
var
  I, DotCount, EndIndex, ComponentStart, ComponentLength: Integer;
  Ch: Char;
begin
  Result := False;
  AMatchText := '';
  I := AStart;
  DotCount := 0;
  EndIndex := I - 1;
  ComponentStart := I;
  while I <= Length(AVersion) do
  begin
    Ch := AVersion[I];
    if IsDigit(Ch) then
      EndIndex := I
    else if Ch = '.' then
    begin
      Inc(DotCount);
      if DotCount > 2 then
        Break;
      EndIndex := I;
      ComponentLength := I - ComponentStart;
      if ComponentLength > MAX_SAFE_COMPONENT_LENGTH then
        Exit(False);
      ComponentStart := I + 1;
    end
    else
      Break;
    Inc(I);
  end;

  if EndIndex < AStart then
    Exit(False);
  AMatchText := Copy(AVersion, AStart, EndIndex - AStart + 1);
  Result := True;
end;

function Coerce(const AVersion: string; const AOptions: TGocciaSemverOptions;
  out ASemver: TGocciaSemver): Boolean;
var
  I: Integer;
  Candidate, MajorText, MinorText, PatchText, Working: string;
  Parts: TGocciaSemverStringArray;
begin
  Result := False;
  if AVersion = '' then
    Exit;

  if not AOptions.RTL then
  begin
    for I := 1 to Length(AVersion) do
      if IsDigit(AVersion[I]) and CoerceMatchFrom(AVersion, I,
        AOptions.IncludePrerelease, Candidate) then
      begin
        Parts := SplitByChar(Candidate, '.');
        if Length(Parts) > 3 then
          SetLength(Parts, 3);
        MajorText := Parts[0];
        if Length(Parts) >= 2 then
          MinorText := Parts[1]
        else
          MinorText := '0';
        if Length(Parts) >= 3 then
          PatchText := Parts[2]
        else
          PatchText := '0';
        Working := MajorText + '.' + MinorText + '.' + PatchText;
        Exit(ParseSemver(Working, AOptions, ASemver));
      end;
  end
  else
  begin
    for I := Length(AVersion) downto 1 do
      if IsDigit(AVersion[I]) and CoerceMatchFrom(AVersion, I,
        AOptions.IncludePrerelease, Candidate) then
      begin
        Parts := SplitByChar(Candidate, '.');
        if Length(Parts) > 3 then
          Parts := Copy(Parts, Length(Parts) - 3, 3);
        MajorText := Parts[0];
        if Length(Parts) >= 2 then
          MinorText := Parts[1]
        else
          MinorText := '0';
        if Length(Parts) >= 3 then
          PatchText := Parts[2]
        else
          PatchText := '0';
        Working := MajorText + '.' + MinorText + '.' + PatchText;
        Exit(ParseSemver(Working, AOptions, ASemver));
      end;
  end;
end;

function ComparatorToString(const AComparator: TGocciaSemverComparator): string;
begin
  if AComparator.IsAny then
    Result := ''
  else
    Result := AComparator.Operator + AComparator.Semver.Version;
end;

function ParseComparator(const AComparator: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverComparator;
var
  Working, VersionText: string;
begin
  Working := CollapseWhitespace(Trim(AComparator));
  Result.Operator := '';
  Result.IsAny := False;
  Result.Value := '';

  if Working = '' then
  begin
    Result.IsAny := True;
    Exit;
  end;

  if StartsWith(Working, '>=') or StartsWith(Working, '<=') then
  begin
    Result.Operator := Copy(Working, 1, 2);
    Delete(Working, 1, 2);
  end
  else if (Working[1] = '>') or (Working[1] = '<') or (Working[1] = '=') then
  begin
    Result.Operator := Working[1];
    Delete(Working, 1, 1);
  end;

  VersionText := Trim(Working);
  if VersionText = '' then
  begin
    Result.IsAny := True;
    Result.Operator := '';
    Exit;
  end;

  Result.Semver := MustParseSemver(VersionText, AOptions);
  if Result.Operator = '=' then
    Result.Operator := '';
  Result.Value := ComparatorToString(Result);
end;

function IsAnyComparator(const AComparator: TGocciaSemverComparator): Boolean;
begin
  Result := AComparator.IsAny or (AComparator.Value = '');
end;

function ParseIntComponent(const AValue: string): Int64;
begin
  Result := ValidateNumericComponent(AValue);
end;

function ExpandXRangeToken(const AToken: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverStringArray;
var
  Working, Operator, VersionText, LowerBoundSuffix, PrereleaseText: string;
  Partial: TGocciaPartialSemver;
  MajorValue, MinorValue, PatchValue: Int64;
  HasAnyX, MajorX, MinorX, PatchX: Boolean;
begin
  Working := Trim(AToken);
  SetLength(Result, 0);

  Operator := '';
  if StartsWith(Working, '>=') or StartsWith(Working, '<=') then
  begin
    Operator := Copy(Working, 1, 2);
    Delete(Working, 1, 2);
  end
  else if (Working <> '') and ((Working[1] = '>') or (Working[1] = '<') or
    (Working[1] = '=')) then
  begin
    Operator := Working[1];
    Delete(Working, 1, 1);
  end;

  VersionText := Trim(Working);
  if (VersionText = '') or (VersionText = '*') then
  begin
    SetLength(Result, 1);
    Result[0] := '';
    Exit;
  end;

  if not TryParsePartialSemver(VersionText, AOptions, Partial) then
  begin
    SetLength(Result, 1);
    Result[0] := Operator + VersionText;
    Exit;
  end;

  MajorX := not Partial.HasMajor or IsWildcardIdentifier(Partial.MajorText);
  MinorX := not Partial.HasMinor or MajorX or IsWildcardIdentifier(Partial.MinorText);
  PatchX := not Partial.HasPatch or MinorX or IsWildcardIdentifier(Partial.PatchText);
  HasAnyX := MajorX or MinorX or PatchX;
  if (Operator = '=') and HasAnyX then
    Operator := '';

  if AOptions.IncludePrerelease then
    PrereleaseText := '-0'
  else
    PrereleaseText := '';

  if MajorX then
  begin
    if (Operator = '>') or (Operator = '<') then
    begin
      SetLength(Result, 1);
      Result[0] := '<0.0.0-0';
    end
    else
    begin
      SetLength(Result, 1);
      Result[0] := '';
    end;
    Exit;
  end;

  MajorValue := ParseIntComponent(Partial.MajorText);
  if Partial.HasMinor and not IsWildcardIdentifier(Partial.MinorText) then
    MinorValue := ParseIntComponent(Partial.MinorText)
  else
    MinorValue := 0;
  if Partial.HasPatch and not IsWildcardIdentifier(Partial.PatchText) then
    PatchValue := ParseIntComponent(Partial.PatchText)
  else
    PatchValue := 0;

  if (Operator <> '') and HasAnyX then
  begin
    if MinorX then
      MinorValue := 0;
    PatchValue := 0;

    if Operator = '>' then
    begin
      Operator := '>=';
      if MinorX then
      begin
        Inc(MajorValue);
        MinorValue := 0;
        PatchValue := 0;
      end
      else
      begin
        Inc(MinorValue);
        PatchValue := 0;
      end;
    end
    else if Operator = '<=' then
    begin
      Operator := '<';
      if MinorX then
        Inc(MajorValue)
      else
        Inc(MinorValue);
    end;

    if Operator = '<' then
      LowerBoundSuffix := '-0'
    else
      LowerBoundSuffix := PrereleaseText;

    SetLength(Result, 1);
    Result[0] := Operator + IntToStr(MajorValue) + '.' + IntToStr(MinorValue) +
      '.' + IntToStr(PatchValue) + LowerBoundSuffix;
    Exit;
  end;

  if MinorX then
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.0.0' + PrereleaseText;
    Result[1] := '<' + IntToStr(MajorValue + 1) + '.0.0-0';
    Exit;
  end;

  if PatchX then
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue) +
      '.0' + PrereleaseText;
    Result[1] := '<' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue + 1) +
      '.0-0';
    Exit;
  end;

  SetLength(Result, 1);
  Result[0] := Operator + VersionText;
end;

function ExpandTildeToken(const AToken: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverStringArray;
var
  Partial: TGocciaPartialSemver;
  MajorValue, MinorValue, PatchValue: Int64;
  VersionText, LowerBoundSuffix: string;
begin
  VersionText := Trim(AToken);
  Delete(VersionText, 1, 1);
  if StartsWith(VersionText, '>') then
    Delete(VersionText, 1, 1);
  if not TryParsePartialSemver(VersionText, AOptions, Partial) then
  begin
    SetLength(Result, 1);
    Result[0] := AToken;
    Exit;
  end;

  if not Partial.HasMajor or IsWildcardIdentifier(Partial.MajorText) then
  begin
    SetLength(Result, 1);
    Result[0] := '';
    Exit;
  end;

  MajorValue := ParseIntComponent(Partial.MajorText);
  if Partial.HasMinor and not IsWildcardIdentifier(Partial.MinorText) then
    MinorValue := ParseIntComponent(Partial.MinorText)
  else
    MinorValue := 0;
  if Partial.HasPatch and not IsWildcardIdentifier(Partial.PatchText) then
    PatchValue := ParseIntComponent(Partial.PatchText)
  else
    PatchValue := 0;

  if AOptions.IncludePrerelease then
    LowerBoundSuffix := '-0'
  else
    LowerBoundSuffix := '';

  if not Partial.HasMinor or IsWildcardIdentifier(Partial.MinorText) then
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.0.0';
    Result[1] := '<' + IntToStr(MajorValue + 1) + '.0.0-0';
  end
  else if not Partial.HasPatch or IsWildcardIdentifier(Partial.PatchText) then
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue) +
      '.0';
    Result[1] := '<' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue + 1) +
      '.0-0';
  end
  else
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue) +
      '.' + IntToStr(PatchValue) + LowerBoundSuffix;
    Result[1] := '<' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue + 1) +
      '.0-0';
  end;
end;

function ExpandCaretToken(const AToken: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverStringArray;
var
  Partial: TGocciaPartialSemver;
  MajorValue, MinorValue, PatchValue: Int64;
  LowerBoundSuffix: string;
  VersionText: string;
begin
  VersionText := Trim(AToken);
  Delete(VersionText, 1, 1);
  if not TryParsePartialSemver(VersionText, AOptions, Partial) then
  begin
    SetLength(Result, 1);
    Result[0] := AToken;
    Exit;
  end;

  if not Partial.HasMajor or IsWildcardIdentifier(Partial.MajorText) then
  begin
    SetLength(Result, 1);
    Result[0] := '';
    Exit;
  end;

  MajorValue := ParseIntComponent(Partial.MajorText);
  if Partial.HasMinor and not IsWildcardIdentifier(Partial.MinorText) then
    MinorValue := ParseIntComponent(Partial.MinorText)
  else
    MinorValue := 0;
  if Partial.HasPatch and not IsWildcardIdentifier(Partial.PatchText) then
    PatchValue := ParseIntComponent(Partial.PatchText)
  else
    PatchValue := 0;

  if AOptions.IncludePrerelease then
    LowerBoundSuffix := '-0'
  else
    LowerBoundSuffix := '';

  if not Partial.HasMinor or IsWildcardIdentifier(Partial.MinorText) then
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.0.0' + LowerBoundSuffix;
    Result[1] := '<' + IntToStr(MajorValue + 1) + '.0.0-0';
    Exit;
  end;

  if not Partial.HasPatch or IsWildcardIdentifier(Partial.PatchText) then
  begin
    SetLength(Result, 2);
    Result[0] := '>=' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue) +
      '.0' + LowerBoundSuffix;
    if MajorValue = 0 then
      Result[1] := '<0.' + IntToStr(MinorValue + 1) + '.0-0'
    else
      Result[1] := '<' + IntToStr(MajorValue + 1) + '.0.0-0';
    Exit;
  end;

  SetLength(Result, 2);
  Result[0] := '>=' + IntToStr(MajorValue) + '.' + IntToStr(MinorValue) +
    '.' + IntToStr(PatchValue) + LowerBoundSuffix;
  if MajorValue = 0 then
  begin
    if MinorValue = 0 then
      Result[1] := '<0.0.' + IntToStr(PatchValue + 1) + '-0'
    else
      Result[1] := '<0.' + IntToStr(MinorValue + 1) + '.0-0';
  end
  else
    Result[1] := '<' + IntToStr(MajorValue + 1) + '.0.0-0';
end;

function AppendStringArray(const ALeft,
  ARight: TGocciaSemverStringArray): TGocciaSemverStringArray;
var
  LeftLength, I: Integer;
begin
  Result := ALeft;
  LeftLength := Length(Result);
  SetLength(Result, LeftLength + Length(ARight));
  for I := 0 to High(ARight) do
    Result[LeftLength + I] := ARight[I];
end;

function ExpandComparatorToken(const AToken: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverStringArray;
begin
  if (AToken = '') or (AToken = '*') then
  begin
    SetLength(Result, 1);
    Result[0] := '';
    Exit;
  end;

  if StartsWith(AToken, '~') then
    Exit(ExpandTildeToken(AToken, AOptions));
  if StartsWith(AToken, '^') then
    Exit(ExpandCaretToken(AToken, AOptions));
  Result := ExpandXRangeToken(AToken, AOptions);
end;

function ParseRangeSet(const ARangePart: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverComparatorArray;
var
  Working, LeftPart, RightPart: string;
  Tokens, ExpandedTokens: TGocciaSemverStringArray;
  I, Count: Integer;
begin
  Working := CollapseWhitespace(ARangePart);
  if Pos(' - ', Working) > 0 then
  begin
    LeftPart := Copy(Working, 1, Pos(' - ', Working) - 1);
    RightPart := Copy(Working, Pos(' - ', Working) + 3, MaxInt);
    Tokens := ExpandComparatorToken(LeftPart, AOptions);
    ExpandedTokens := ExpandComparatorToken(RightPart, AOptions);
    Tokens := AppendStringArray(Tokens, ExpandedTokens);
  end
  else
  begin
    Tokens := SplitByChar(Working, ' ');
    ExpandedTokens := nil;
    for I := 0 to High(Tokens) do
      ExpandedTokens := AppendStringArray(ExpandedTokens,
        ExpandComparatorToken(Tokens[I], AOptions));
    Tokens := ExpandedTokens;
  end;

  SetLength(Result, 0);
  Count := 0;
  for I := 0 to High(Tokens) do
  begin
    if Tokens[I] = '' then
      Continue;
    if Tokens[I] = '>=0.0.0' then
      Continue;
    if AOptions.IncludePrerelease and (Tokens[I] = '>=0.0.0-0') then
      Continue;
    SetLength(Result, Count + 1);
    Result[Count] := ParseComparator(Tokens[I], AOptions);
    Inc(Count);
  end;

  if Count = 0 then
  begin
    SetLength(Result, 1);
    Result[0].IsAny := True;
    Result[0].Operator := '';
    Result[0].Value := '';
  end;
end;

function RangeToString(const ARange: TGocciaSemverRange): string;
var
  I, J: Integer;
begin
  if ARange.Formatted <> '' then
    Exit(ARange.Formatted);

  Result := '';
  for I := 0 to High(ARange.SetOfComparators) do
  begin
    if I > 0 then
      Result := Result + '||';
    for J := 0 to High(ARange.SetOfComparators[I]) do
    begin
      if J > 0 then
        Result := Result + ' ';
      Result := Result + ComparatorToString(ARange.SetOfComparators[I][J]);
    end;
  end;
end;

function ParseRange(const ARange: string; const AOptions: TGocciaSemverOptions;
  out AParsedRange: TGocciaSemverRange): Boolean;
var
  Parts: TGocciaSemverStringArray;
  I, Count: Integer;
begin
  try
    AParsedRange.Options := AOptions;
    AParsedRange.Raw := CollapseWhitespace(Trim(ARange));
    Parts := SplitByString(AParsedRange.Raw, '||');
    SetLength(AParsedRange.SetOfComparators, 0);
    Count := 0;
    for I := 0 to High(Parts) do
    begin
      SetLength(AParsedRange.SetOfComparators, Count + 1);
      AParsedRange.SetOfComparators[Count] := ParseRangeSet(Trim(Parts[I]), AOptions);
      Inc(Count);
    end;
    AParsedRange.Formatted := RangeToString(AParsedRange);
    Result := Length(AParsedRange.SetOfComparators) > 0;
  except
    on EGocciaSemverError do
      Result := False;
  end;
end;

function MustParseRange(const ARange: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverRange;
begin
  if not ParseRange(ARange, AOptions, Result) then
    raise EGocciaSemverTypeError.Create('Invalid SemVer Range: ' + ARange);
end;

function ValidRange(const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
var
  ParsedRange: TGocciaSemverRange;
begin
  if ParseRange(ARange, AOptions, ParsedRange) then
  begin
    Result := RangeToString(ParsedRange);
    if Result = '' then
      Result := '*';
  end
  else
    Result := '';
end;

function ComparatorTest(const AComparator: TGocciaSemverComparator;
  const AVersion: TGocciaSemver; const AOptions: TGocciaSemverOptions): Boolean;
begin
  if AComparator.IsAny then
    Exit(True);
  Result := Cmp(AVersion.Version, AComparator.Operator, AComparator.Semver.Version, AOptions);
end;

function ComparatorIntersects(const ALeft, ARight: TGocciaSemverComparator;
  const AOptions: TGocciaSemverOptions): Boolean;
var
  LeftComparator, RightComparator: TGocciaSemverComparator;
begin
  LeftComparator := ALeft;
  RightComparator := ARight;

  if LeftComparator.Operator = '' then
  begin
    if LeftComparator.Value = '' then
      Exit(True);
    Exit(Satisfies(LeftComparator.Semver.Version,
      ComparatorToString(RightComparator), AOptions));
  end;

  if RightComparator.Operator = '' then
  begin
    if RightComparator.Value = '' then
      Exit(True);
    Exit(Satisfies(RightComparator.Semver.Version,
      ComparatorToString(LeftComparator), AOptions));
  end;

  if AOptions.IncludePrerelease and
    ((LeftComparator.Value = '<0.0.0-0') or (RightComparator.Value = '<0.0.0-0')) then
    Exit(False);
  if (not AOptions.IncludePrerelease) and
    (StartsWith(LeftComparator.Value, '<0.0.0') or StartsWith(RightComparator.Value, '<0.0.0')) then
    Exit(False);

  if StartsWith(LeftComparator.Operator, '>') and StartsWith(RightComparator.Operator, '>') then
    Exit(True);
  if StartsWith(LeftComparator.Operator, '<') and StartsWith(RightComparator.Operator, '<') then
    Exit(True);
  if (LeftComparator.Semver.Version = RightComparator.Semver.Version) and
    (Pos('=', LeftComparator.Operator) > 0) and (Pos('=', RightComparator.Operator) > 0) then
    Exit(True);
  if (CompareSemverValues(LeftComparator.Semver, RightComparator.Semver) < 0) and
    StartsWith(LeftComparator.Operator, '>') and StartsWith(RightComparator.Operator, '<') then
    Exit(True);
  if (CompareSemverValues(LeftComparator.Semver, RightComparator.Semver) > 0) and
    StartsWith(LeftComparator.Operator, '<') and StartsWith(RightComparator.Operator, '>') then
    Exit(True);
  Result := False;
end;

function TestComparatorSet(const ASet: TGocciaSemverComparatorArray;
  const AVersion: TGocciaSemver; const AOptions: TGocciaSemverOptions): Boolean;
var
  I: Integer;
  Allowed: TGocciaSemver;
begin
  for I := 0 to High(ASet) do
    if not ComparatorTest(ASet[I], AVersion, AOptions) then
      Exit(False);

  if (Length(AVersion.Prerelease) > 0) and not AOptions.IncludePrerelease then
  begin
    for I := 0 to High(ASet) do
    begin
      if ASet[I].IsAny then
        Continue;
      if Length(ASet[I].Semver.Prerelease) > 0 then
      begin
        Allowed := ASet[I].Semver;
        if (Allowed.Major = AVersion.Major) and
          (Allowed.Minor = AVersion.Minor) and
          (Allowed.Patch = AVersion.Patch) then
          Exit(True);
      end;
    end;
    Exit(False);
  end;

  Result := True;
end;

function Satisfies(const AVersion, ARange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
var
  ParsedRange: TGocciaSemverRange;
  ParsedVersion: TGocciaSemver;
  I: Integer;
begin
  if not ParseRange(ARange, AOptions, ParsedRange) then
    Exit(False);
  if not ParseSemver(AVersion, AOptions, ParsedVersion) then
    Exit(False);
  for I := 0 to High(ParsedRange.SetOfComparators) do
    if TestComparatorSet(ParsedRange.SetOfComparators[I], ParsedVersion, AOptions) then
      Exit(True);
  Result := False;
end;

function IsSatisfiableComparatorSet(const ASet: TGocciaSemverComparatorArray;
  const AOptions: TGocciaSemverOptions): Boolean;
var
  I, J: Integer;
begin
  for I := 0 to High(ASet) do
    for J := I + 1 to High(ASet) do
      if not ComparatorIntersects(ASet[I], ASet[J], AOptions) then
        Exit(False);
  Result := True;
end;

function RangeIntersects(const ALeft, ARight: string;
  const AOptions: TGocciaSemverOptions): Boolean;
var
  LeftRange, RightRange: TGocciaSemverRange;
  I, J, K, L: Integer;
  IntersectsAll: Boolean;
begin
  LeftRange := MustParseRange(ALeft, AOptions);
  RightRange := MustParseRange(ARight, AOptions);

  for I := 0 to High(LeftRange.SetOfComparators) do
    if IsSatisfiableComparatorSet(LeftRange.SetOfComparators[I], AOptions) then
      for J := 0 to High(RightRange.SetOfComparators) do
        if IsSatisfiableComparatorSet(RightRange.SetOfComparators[J], AOptions) then
        begin
          IntersectsAll := True;
          for K := 0 to High(LeftRange.SetOfComparators[I]) do
            for L := 0 to High(RightRange.SetOfComparators[J]) do
              if not ComparatorIntersects(LeftRange.SetOfComparators[I][K],
                RightRange.SetOfComparators[J][L], AOptions) then
              begin
                IntersectsAll := False;
                Break;
              end;
          if IntersectsAll then
            Exit(True);
        end;
  Result := False;
end;

function MaxSatisfying(const AVersions: array of string; const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
var
  ParsedRange: TGocciaSemverRange;
  I: Integer;
  MaxVersion, Current: string;
begin
  if not ParseRange(ARange, AOptions, ParsedRange) then
    Exit('');
  MaxVersion := '';
  for I := Low(AVersions) to High(AVersions) do
  begin
    Current := AVersions[I];
    if Satisfies(Current, ARange, AOptions) then
      if (MaxVersion = '') or (Compare(Current, MaxVersion, AOptions) > 0) then
        MaxVersion := Current;
  end;
  Result := MaxVersion;
end;

function MinSatisfying(const AVersions: array of string; const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
var
  ParsedRange: TGocciaSemverRange;
  I: Integer;
  MinValue, Current: string;
begin
  if not ParseRange(ARange, AOptions, ParsedRange) then
    Exit('');
  MinValue := '';
  for I := Low(AVersions) to High(AVersions) do
  begin
    Current := AVersions[I];
    if Satisfies(Current, ARange, AOptions) then
      if (MinValue = '') or (Compare(Current, MinValue, AOptions) < 0) then
        MinValue := Current;
  end;
  Result := MinValue;
end;

function MinVersion(const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
var
  ParsedRange: TGocciaSemverRange;
  MinValue: TGocciaSemver;
  I, J: Integer;
  ComparatorVersion: TGocciaSemver;
  SetMin: string;
begin
  ParsedRange := MustParseRange(ARange, AOptions);
  if Satisfies('0.0.0', ARange, AOptions) then
    Exit('0.0.0');
  if Satisfies('0.0.0-0', ARange, AOptions) then
    Exit('0.0.0-0');

  Result := '';
  for I := 0 to High(ParsedRange.SetOfComparators) do
  begin
    SetMin := '';
    for J := 0 to High(ParsedRange.SetOfComparators[I]) do
    begin
      if ParsedRange.SetOfComparators[I][J].IsAny then
        Continue;
      ComparatorVersion := ParsedRange.SetOfComparators[I][J].Semver;
      if ParsedRange.SetOfComparators[I][J].Operator = '>' then
      begin
        if Length(ComparatorVersion.Prerelease) = 0 then
          Inc(ComparatorVersion.Patch)
        else
        begin
          SetLength(ComparatorVersion.Prerelease,
            Length(ComparatorVersion.Prerelease) + 1);
          ComparatorVersion.Prerelease[High(ComparatorVersion.Prerelease)].Kind := sikNumeric;
          ComparatorVersion.Prerelease[High(ComparatorVersion.Prerelease)].NumericValue := 0;
          ComparatorVersion.Prerelease[High(ComparatorVersion.Prerelease)].TextValue := '0';
        end;
        ComparatorVersion.Version := NormalizeVersion(ComparatorVersion);
        SetMin := ComparatorVersion.Version;
      end
      else if (ParsedRange.SetOfComparators[I][J].Operator = '') or
        (ParsedRange.SetOfComparators[I][J].Operator = '>=') then
      begin
        if (SetMin = '') or (Compare(ComparatorVersion.Version, SetMin, AOptions) > 0) then
          SetMin := ComparatorVersion.Version;
      end;
    end;
    if (SetMin <> '') and ((Result = '') or (Compare(SetMin, Result, AOptions) < 0)) then
      Result := SetMin;
  end;

  if (Result <> '') and Satisfies(Result, ARange, AOptions) then
    Exit;
  Result := '';
end;

function Outside(const AVersion, ARange, AHilo: string;
  const AOptions: TGocciaSemverOptions): Boolean;
var
  ParsedRange: TGocciaSemverRange;
  ParsedVersion: TGocciaSemver;
  I, J: Integer;
  HighComparator, LowComparator, ComparatorValue: TGocciaSemverComparator;
  GreaterComparator, GreaterEqualComparator: string;
begin
  ParsedVersion := MustParseSemver(AVersion, AOptions);
  ParsedRange := MustParseRange(ARange, AOptions);
  if Satisfies(AVersion, ARange, AOptions) then
    Exit(False);

  if AHilo = '>' then
  begin
    GreaterComparator := '>';
    GreaterEqualComparator := '>=';
  end
  else if AHilo = '<' then
  begin
    GreaterComparator := '<';
    GreaterEqualComparator := '<=';
  end
  else
    raise EGocciaSemverTypeError.Create('Must provide a hilo val of "<" or ">"');

  for I := 0 to High(ParsedRange.SetOfComparators) do
  begin
    HighComparator := ParsedRange.SetOfComparators[I][0];
    LowComparator := ParsedRange.SetOfComparators[I][0];
    for J := 0 to High(ParsedRange.SetOfComparators[I]) do
    begin
      ComparatorValue := ParsedRange.SetOfComparators[I][J];
      if ComparatorValue.IsAny then
        Continue;
      if CompareSemverValues(ComparatorValue.Semver, HighComparator.Semver) > 0 then
        HighComparator := ComparatorValue;
      if CompareSemverValues(ComparatorValue.Semver, LowComparator.Semver) < 0 then
        LowComparator := ComparatorValue;
    end;

    if (HighComparator.Operator = GreaterComparator) or
      (HighComparator.Operator = GreaterEqualComparator) then
      Exit(False);
    if ((LowComparator.Operator = '') or (LowComparator.Operator = GreaterComparator)) and
      (Cmp(ParsedVersion.Version, '<=', LowComparator.Semver.Version, AOptions)) then
      Exit(False)
    else if (LowComparator.Operator = GreaterEqualComparator) and
      Cmp(ParsedVersion.Version, '<', LowComparator.Semver.Version, AOptions) then
      Exit(False);
  end;
  Result := True;
end;

function GreaterThanRange(const AVersion, ARange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
begin
  Result := Outside(AVersion, ARange, '>', AOptions);
end;

function LessThanRange(const AVersion, ARange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
begin
  Result := Outside(AVersion, ARange, '<', AOptions);
end;

function SimplifyRange(const AVersions: array of string; const ARange: string;
  const AOptions: TGocciaSemverOptions): string;
var
  MatchingRanges: array of record
    MinValue: string;
    MaxValue: string;
  end;
  SortedVersions: TGocciaSemverStringArray;
  I, RangeCount: Integer;
  FirstValue, PreviousValue, CurrentValue, Simplified, OriginalValue: string;
begin
  SetLength(SortedVersions, Length(AVersions));
  for I := Low(AVersions) to High(AVersions) do
    SortedVersions[I - Low(AVersions)] := AVersions[I];
  while True do
  begin
    Result := '';
    Break;
  end;

  FirstValue := '';
  PreviousValue := '';
  RangeCount := 0;
  for I := 0 to High(SortedVersions) do
  begin
    CurrentValue := SortedVersions[I];
    if Satisfies(CurrentValue, ARange, AOptions) then
    begin
      PreviousValue := CurrentValue;
      if FirstValue = '' then
        FirstValue := CurrentValue;
    end
    else if PreviousValue <> '' then
    begin
      SetLength(MatchingRanges, RangeCount + 1);
      MatchingRanges[RangeCount].MinValue := FirstValue;
      MatchingRanges[RangeCount].MaxValue := PreviousValue;
      Inc(RangeCount);
      PreviousValue := '';
      FirstValue := '';
    end;
  end;

  if FirstValue <> '' then
  begin
    SetLength(MatchingRanges, RangeCount + 1);
    MatchingRanges[RangeCount].MinValue := FirstValue;
    MatchingRanges[RangeCount].MaxValue := '';
    Inc(RangeCount);
  end;

  Simplified := '';
  for I := 0 to RangeCount - 1 do
  begin
    if Simplified <> '' then
      Simplified := Simplified + ' || ';
    if MatchingRanges[I].MinValue = MatchingRanges[I].MaxValue then
      Simplified := Simplified + MatchingRanges[I].MinValue
    else if (MatchingRanges[I].MaxValue = '') and
      (MatchingRanges[I].MinValue = SortedVersions[0]) then
      Simplified := Simplified + '*'
    else if MatchingRanges[I].MaxValue = '' then
      Simplified := Simplified + '>=' + MatchingRanges[I].MinValue
    else if MatchingRanges[I].MinValue = SortedVersions[0] then
      Simplified := Simplified + '<=' + MatchingRanges[I].MaxValue
    else
      Simplified := Simplified + MatchingRanges[I].MinValue + ' - ' +
        MatchingRanges[I].MaxValue;
  end;

  OriginalValue := ARange;
  if Length(Simplified) < Length(OriginalValue) then
    Result := Simplified
  else
    Result := OriginalValue;
end;

function ToComparators(const ARange: string;
  const AOptions: TGocciaSemverOptions): TGocciaSemverStringMatrix;
var
  ParsedRange: TGocciaSemverRange;
  I, J: Integer;
  ValueText: string;
begin
  ParsedRange := MustParseRange(ARange, AOptions);
  SetLength(Result, Length(ParsedRange.SetOfComparators));
  for I := 0 to High(ParsedRange.SetOfComparators) do
  begin
    SetLength(Result[I], Length(ParsedRange.SetOfComparators[I]));
    for J := 0 to High(ParsedRange.SetOfComparators[I]) do
    begin
      ValueText := ComparatorToString(ParsedRange.SetOfComparators[I][J]);
      Result[I][J] := Trim(ValueText);
    end;
  end;
end;

function HigherGreaterThanComparator(const ALeft,
  ARight: TGocciaSemverComparator; const AOptions: TGocciaSemverOptions): TGocciaSemverComparator;
var
  Comparison: Integer;
begin
  if ALeft.Value = '' then
    Exit(ARight);
  Comparison := CompareSemverValues(ALeft.Semver, ARight.Semver);
  if Comparison > 0 then
    Exit(ALeft);
  if Comparison < 0 then
    Exit(ARight);
  if (ARight.Operator = '>') and (ALeft.Operator = '>=') then
    Exit(ARight);
  Result := ALeft;
end;

function LowerLessThanComparator(const ALeft,
  ARight: TGocciaSemverComparator; const AOptions: TGocciaSemverOptions): TGocciaSemverComparator;
var
  Comparison: Integer;
begin
  if ALeft.Value = '' then
    Exit(ARight);
  Comparison := CompareSemverValues(ALeft.Semver, ARight.Semver);
  if Comparison < 0 then
    Exit(ALeft);
  if Comparison > 0 then
    Exit(ARight);
  if (ARight.Operator = '<') and (ALeft.Operator = '<=') then
    Exit(ARight);
  Result := ALeft;
end;

function SimpleSubset(const ASubSet, ADomainSet: TGocciaSemverComparatorArray;
  const AOptions: TGocciaSemverOptions; out AHasNonNull: Boolean): Boolean;
var
  EqComparator, GreaterThanComparator, LessThanComparator, DomainComparator: TGocciaSemverComparator;
  I: Integer;
begin
  AHasNonNull := False;
  EqComparator.Value := '';
  GreaterThanComparator.Value := '';
  LessThanComparator.Value := '';

  for I := 0 to High(ASubSet) do
  begin
    if (ASubSet[I].Operator = '>') or (ASubSet[I].Operator = '>=') then
      GreaterThanComparator := HigherGreaterThanComparator(GreaterThanComparator, ASubSet[I], AOptions)
    else if (ASubSet[I].Operator = '<') or (ASubSet[I].Operator = '<=') then
      LessThanComparator := LowerLessThanComparator(LessThanComparator, ASubSet[I], AOptions)
    else
      EqComparator := ASubSet[I];
  end;

  if (EqComparator.Value <> '') and (GreaterThanComparator.Value <> '') and
    (not Satisfies(EqComparator.Semver.Version, ComparatorToString(GreaterThanComparator), AOptions)) then
    Exit(True);
  if (EqComparator.Value <> '') and (LessThanComparator.Value <> '') and
    (not Satisfies(EqComparator.Semver.Version, ComparatorToString(LessThanComparator), AOptions)) then
    Exit(True);

  if EqComparator.Value <> '' then
  begin
    AHasNonNull := True;
    for DomainComparator in ADomainSet do
      if not Satisfies(EqComparator.Semver.Version, ComparatorToString(DomainComparator), AOptions) then
        Exit(False);
    Exit(True);
  end;

  AHasNonNull := True;
  for DomainComparator in ADomainSet do
  begin
    if (GreaterThanComparator.Value <> '') and
      ((DomainComparator.Operator = '>') or
      (DomainComparator.Operator = '>=')) then
      if HigherGreaterThanComparator(GreaterThanComparator, DomainComparator, AOptions).Value = DomainComparator.Value then
        Exit(False);
    if (LessThanComparator.Value <> '') and
      ((DomainComparator.Operator = '<') or (DomainComparator.Operator = '<=')) then
      if LowerLessThanComparator(LessThanComparator, DomainComparator, AOptions).Value = DomainComparator.Value then
        Exit(False);
  end;
  Result := True;
end;

function IsSubset(const ASubRange, ADomainRange: string;
  const AOptions: TGocciaSemverOptions): Boolean;
var
  SubRange, DomainRange: TGocciaSemverRange;
  I, J: Integer;
  HasNonNull, SetHasNonNull, SetIsSubset: Boolean;
begin
  if ASubRange = ADomainRange then
    Exit(True);

  SubRange := MustParseRange(ASubRange, AOptions);
  DomainRange := MustParseRange(ADomainRange, AOptions);
  HasNonNull := False;

  for I := 0 to High(SubRange.SetOfComparators) do
  begin
    SetIsSubset := False;
    SetHasNonNull := False;
    for J := 0 to High(DomainRange.SetOfComparators) do
      if SimpleSubset(SubRange.SetOfComparators[I], DomainRange.SetOfComparators[J],
        AOptions, SetHasNonNull) then
      begin
        SetIsSubset := True;
        HasNonNull := HasNonNull or SetHasNonNull;
        Break;
      end;
    if not SetIsSubset and HasNonNull then
      Exit(False);
  end;
  Result := True;
end;

end.
