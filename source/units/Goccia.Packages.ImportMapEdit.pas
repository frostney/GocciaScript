unit Goccia.Packages.ImportMapEdit;

{ Edits one member of the `imports` object of a JSON import map
  (`goccia.json`) and keeps every other byte: its formatting, key order,
  other settings, and line endings. Install mode uses it for `--add` and
  `--remove`. The text must be strict JSON, as the resolver requires. }

{$I Goccia.inc}

interface

type
  TGocciaImportMapEntry = record
    Key: string;
    Value: string;
  end;

  TGocciaImportMapEntries = array of TGocciaImportMapEntry;

{ The members of `imports`, in order. False with AError when the text is not
  a JSON object, `imports` is not an object, a key repeats, or a value is not
  a string. No `imports` member is an empty list. }
function TryReadImportMapEntries(const AText: string;
  out AEntries: TGocciaImportMapEntries; out AError: string): Boolean;

{ AText with `imports[AKey]` set to AValue: the value replaced in place, or
  the member appended (creating `imports` when missing). }
function TrySetImportMapEntry(const AText, AKey, AValue: string;
  out AResult, AError: string): Boolean;

{ AText without `imports[AKey]`. False with AError when there is no such
  member. }
function TryRemoveImportMapEntry(const AText, AKey: string;
  out AResult, AError: string): Boolean;

{ A new import map holding one entry. }
function NewImportMapText(const AKey, AValue: string): string;

implementation

uses
  SysUtils,

  Goccia.JSON.Utils;

const
  IMPORTS_KEY = 'imports';
  DEFAULT_INDENT = '  ';

type
  EImportMapEditError = class(Exception);

  TJSONMember = record
    Key: string;
    KeyStart: Integer;
    ValueStart: Integer;
    { One past the value's last character. }
    ValueEnd: Integer;
    ValueIsObject: Boolean;
    ValueIsString: Boolean;
    StringValue: string;
  end;

  TJSONObjectSpan = record
    Open: Integer;
    Close: Integer;
    Members: array of TJSONMember;
  end;

  TJSONScanner = record
    Text: string;
    Position: Integer;
    procedure Fail(const AProblem: string);
    procedure SkipWhitespace;
    function Peek: Char;
    function ReadString: string;
    procedure SkipValue;
    procedure ReadObject(out ASpan: TJSONObjectSpan);
  end;

procedure TJSONScanner.Fail(const AProblem: string);
begin
  raise EImportMapEditError.CreateFmt('%s at character %d', [AProblem,
    Position]);
end;

procedure TJSONScanner.SkipWhitespace;
begin
  while (Position <= Length(Text)) and
    CharInSet(Text[Position], [' ', #9, #10, #13]) do
    Inc(Position);
end;

function TJSONScanner.Peek: Char;
begin
  SkipWhitespace;
  if Position > Length(Text) then
    Result := #0
  else
    Result := Text[Position];
end;

function TJSONScanner.ReadString: string;
var
  Code, Digit, I: Integer;
begin
  if Peek <> '"' then
    Fail('expected a string');
  Inc(Position);
  Result := '';
  while True do
  begin
    if Position > Length(Text) then
      Fail('unterminated string');
    case Text[Position] of
      '"':
        begin
          Inc(Position);
          Exit;
        end;
      '\':
        begin
          Inc(Position);
          if Position > Length(Text) then
            Fail('unterminated escape');
          case Text[Position] of
            '"', '\', '/':
              Result := Result + Text[Position];
            'b':
              Result := Result + #8;
            'f':
              Result := Result + #12;
            'n':
              Result := Result + #10;
            'r':
              Result := Result + #13;
            't':
              Result := Result + #9;
            'u':
              begin
                Code := 0;
                for I := 1 to 4 do
                begin
                  if Position + I > Length(Text) then
                    Fail('truncated \u escape');
                  case Text[Position + I] of
                    '0'..'9':
                      Digit := Ord(Text[Position + I]) - Ord('0');
                    'a'..'f':
                      Digit := Ord(Text[Position + I]) - Ord('a') + 10;
                    'A'..'F':
                      Digit := Ord(Text[Position + I]) - Ord('A') + 10;
                  else
                    Digit := -1;
                  end;
                  if Digit < 0 then
                    Fail('malformed \u escape');
                  Code := Code * 16 + Digit;
                end;
                Result := Result + WideChar(Code);
                Inc(Position, 4);
              end;
          else
            Fail('unknown escape');
          end;
          Inc(Position);
        end;
      #0..#31:
        Fail('control character in string');
    else
      begin
        Result := Result + Text[Position];
        Inc(Position);
      end;
    end;
  end;
end;

procedure TJSONScanner.SkipValue;
var
  Span: TJSONObjectSpan;
begin
  case Peek of
    '"':
      ReadString;
    '{':
      ReadObject(Span);
    '[':
      begin
        Inc(Position);
        if Peek = ']' then
        begin
          Inc(Position);
          Exit;
        end;
        while True do
        begin
          SkipValue;
          case Peek of
            ',':
              Inc(Position);
            ']':
              begin
                Inc(Position);
                Exit;
              end;
          else
            Fail('expected "," or "]"');
          end;
        end;
      end;
    '-', '0'..'9', 't', 'f', 'n':
      while (Position <= Length(Text)) and CharInSet(Text[Position],
        ['-', '+', '.', '0'..'9', 'E', 'a'..'z']) do
        Inc(Position);
  else
    Fail('expected a value');
  end;
end;

procedure TJSONScanner.ReadObject(out ASpan: TJSONObjectSpan);
var
  Member: TJSONMember;
  I: Integer;
begin
  ASpan := Default(TJSONObjectSpan);
  if Peek <> '{' then
    Fail('expected an object');
  ASpan.Open := Position;
  Inc(Position);
  if Peek = '}' then
  begin
    ASpan.Close := Position;
    Inc(Position);
    Exit;
  end;
  while True do
  begin
    Member := Default(TJSONMember);
    Peek;
    Member.KeyStart := Position;
    Member.Key := ReadString;
    for I := 0 to High(ASpan.Members) do
      if ASpan.Members[I].Key = Member.Key then
        Fail(Format('the key "%s" appears twice', [Member.Key]));
    if Peek <> ':' then
      Fail('expected ":"');
    Inc(Position);
    Member.ValueIsObject := Peek = '{';
    Member.ValueIsString := Peek = '"';
    Member.ValueStart := Position;
    if Member.ValueIsString then
      Member.StringValue := ReadString
    else
      SkipValue;
    Member.ValueEnd := Position;
    SetLength(ASpan.Members, Length(ASpan.Members) + 1);
    ASpan.Members[High(ASpan.Members)] := Member;
    case Peek of
      ',':
        Inc(Position);
      '}':
        begin
          ASpan.Close := Position;
          Inc(Position);
          Exit;
        end;
    else
      Fail('expected "," or "}"');
    end;
  end;
end;

procedure ScanDocument(const AText: string; out ARoot: TJSONObjectSpan);
var
  Scanner: TJSONScanner;
begin
  Scanner.Text := AText;
  Scanner.Position := 1;
  Scanner.ReadObject(ARoot);
  if Scanner.Peek <> #0 then
    Scanner.Fail('unexpected text after the top-level object');
end;

function FindMember(const ASpan: TJSONObjectSpan; const AKey: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(ASpan.Members) do
    if ASpan.Members[I].Key = AKey then
      Exit(I);
  Result := -1;
end;

procedure ScanImports(const AText: string; const ARoot: TJSONObjectSpan;
  out AFound: Boolean; out AImports: TJSONObjectSpan);
var
  Index: Integer;
  Scanner: TJSONScanner;
begin
  AImports := Default(TJSONObjectSpan);
  Index := FindMember(ARoot, IMPORTS_KEY);
  AFound := Index >= 0;
  if not AFound then
    Exit;
  if not ARoot.Members[Index].ValueIsObject then
    raise EImportMapEditError.Create('"imports" is not an object');
  Scanner.Text := AText;
  Scanner.Position := ARoot.Members[Index].ValueStart;
  Scanner.ReadObject(AImports);
end;

function LineBreakOf(const AText: string): string;
begin
  if Pos(#13#10, AText) > 0 then
    Result := #13#10
  else
    Result := #10;
end;

{ The whitespace that starts the line holding AIndex. }
function LineIndent(const AText: string; const AIndex: Integer): string;
var
  LineStart, I: Integer;
begin
  LineStart := AIndex;
  while (LineStart > 1) and not CharInSet(AText[LineStart - 1], [#10, #13]) do
    Dec(LineStart);
  I := LineStart;
  while (I <= Length(AText)) and CharInSet(AText[I], [' ', #9]) do
    Inc(I);
  Result := Copy(AText, LineStart, I - LineStart);
end;

{ True when the key at AKeyStart is the first thing on its line. }
function StartsLine(const AText: string; const AKeyStart: Integer): Boolean;
var
  I: Integer;
begin
  I := AKeyStart - 1;
  while (I >= 1) and CharInSet(AText[I], [' ', #9]) do
    Dec(I);
  Result := (I < 1) or CharInSet(AText[I], [#10, #13]);
end;

function MemberText(const AKey, AValue: string): string;
begin
  Result := QuoteJSONString(AKey) + ': ' + AValue;
end;

{ AText with a member whose text is AMember added to the object ASpan. }
function AppendMember(const AText: string; const ASpan: TJSONObjectSpan;
  const AMember: string): string;
var
  Indent, Separator: string;
  Last: TJSONMember;
begin
  if Length(ASpan.Members) = 0 then
  begin
    Indent := LineIndent(AText, ASpan.Open);
    Exit(Copy(AText, 1, ASpan.Open) + LineBreakOf(AText) + Indent +
      DEFAULT_INDENT + AMember + LineBreakOf(AText) + Indent +
      Copy(AText, ASpan.Close, MaxInt));
  end;
  Last := ASpan.Members[High(ASpan.Members)];
  if StartsLine(AText, ASpan.Members[0].KeyStart) then
    Separator := ',' + LineBreakOf(AText) +
      LineIndent(AText, ASpan.Members[0].KeyStart)
  else
    Separator := ', ';
  Result := Copy(AText, 1, Last.ValueEnd - 1) + Separator + AMember +
    Copy(AText, Last.ValueEnd, MaxInt);
end;

function TryReadImportMapEntries(const AText: string;
  out AEntries: TGocciaImportMapEntries; out AError: string): Boolean;
var
  Found: Boolean;
  I: Integer;
  Imports, Root: TJSONObjectSpan;
begin
  AEntries := nil;
  AError := '';
  try
    ScanDocument(AText, Root);
    ScanImports(AText, Root, Found, Imports);
    SetLength(AEntries, Length(Imports.Members));
    for I := 0 to High(Imports.Members) do
    begin
      if not Imports.Members[I].ValueIsString then
        raise EImportMapEditError.CreateFmt(
          'import map entry "%s" is not a string', [Imports.Members[I].Key]);
      AEntries[I].Key := Imports.Members[I].Key;
      AEntries[I].Value := Imports.Members[I].StringValue;
    end;
    Result := True;
  except
    on E: EImportMapEditError do
    begin
      AError := E.Message;
      AEntries := nil;
      Result := False;
    end;
  end;
end;

function TrySetImportMapEntry(const AText, AKey, AValue: string;
  out AResult, AError: string): Boolean;
var
  Found: Boolean;
  Index: Integer;
  Imports, Root: TJSONObjectSpan;
  Indent: string;
begin
  AResult := '';
  AError := '';
  try
    ScanDocument(AText, Root);
    ScanImports(AText, Root, Found, Imports);
    if not Found then
    begin
      if (Length(Root.Members) > 0) and
         StartsLine(AText, Root.Members[0].KeyStart) then
        Indent := LineIndent(AText, Root.Members[0].KeyStart)
      else
        Indent := DEFAULT_INDENT;
      AResult := AppendMember(AText, Root, MemberText(IMPORTS_KEY, '{' +
        LineBreakOf(AText) + Indent + DEFAULT_INDENT +
        MemberText(AKey, QuoteJSONString(AValue)) + LineBreakOf(AText) +
        Indent + '}'));
      Exit(True);
    end;
    Index := FindMember(Imports, AKey);
    if Index >= 0 then
      AResult := Copy(AText, 1, Imports.Members[Index].ValueStart - 1) +
        QuoteJSONString(AValue) +
        Copy(AText, Imports.Members[Index].ValueEnd, MaxInt)
    else
      AResult := AppendMember(AText, Imports,
        MemberText(AKey, QuoteJSONString(AValue)));
    Result := True;
  except
    on E: EImportMapEditError do
    begin
      AError := E.Message;
      Result := False;
    end;
  end;
end;

function TryRemoveImportMapEntry(const AText, AKey: string;
  out AResult, AError: string): Boolean;
var
  Found: Boolean;
  Index: Integer;
  Imports, Root: TJSONObjectSpan;
begin
  AResult := '';
  AError := '';
  try
    ScanDocument(AText, Root);
    ScanImports(AText, Root, Found, Imports);
    Index := -1;
    if Found then
      Index := FindMember(Imports, AKey);
    if Index < 0 then
      raise EImportMapEditError.CreateFmt('the import map has no entry "%s"',
        [AKey]);
    if Length(Imports.Members) = 1 then
      { The only member: leave an empty object. }
      AResult := Copy(AText, 1, Imports.Open) +
        Copy(AText, Imports.Close, MaxInt)
    else if Index < High(Imports.Members) then
      { Up to the next key, which then starts where this one did. }
      AResult := Copy(AText, 1, Imports.Members[Index].KeyStart - 1) +
        Copy(AText, Imports.Members[Index + 1].KeyStart, MaxInt)
    else
      { The last member: from the end of the one before it. }
      AResult := Copy(AText, 1, Imports.Members[Index - 1].ValueEnd - 1) +
        Copy(AText, Imports.Members[Index].ValueEnd, MaxInt);
    Result := True;
  except
    on E: EImportMapEditError do
    begin
      AError := E.Message;
      Result := False;
    end;
  end;
end;

function NewImportMapText(const AKey, AValue: string): string;
begin
  Result := '{' + #10 + DEFAULT_INDENT + '"' + IMPORTS_KEY + '": {' + #10 +
    DEFAULT_INDENT + DEFAULT_INDENT + MemberText(AKey,
    QuoteJSONString(AValue)) + #10 + DEFAULT_INDENT + '}' + #10 + '}' + #10;
end;

end.
