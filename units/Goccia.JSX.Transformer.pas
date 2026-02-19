unit Goccia.JSX.Transformer;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils,

  Goccia.JSX.SourceMap,
  Goccia.Keywords.Contextual,
  Goccia.Keywords.Reserved;

type
  TGocciaJSXTransformResult = record
    Source: string;
    SourceMap: TGocciaSourceMap;
  end;

procedure WarnIfJSXExtensionMismatch(const AFilePath: string);

type
  TGocciaJSXTransformer = class
  private
    type
      TLastTokenKind = (ltkNone, ltkExpressionEnd, ltkOperator);
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FColumn: Integer;
    FOutputLine: Integer;
    FOutputColumn: Integer;
    FOutput: TStringBuilder;
    FSourceMap: TGocciaSourceMap;
    FFactoryName: string;
    FFragmentName: string;
    FLastTokenKind: TLastTokenKind;
    FHasJSX: Boolean;
    FFileName: string;

    function Peek: Char; inline;
    function PeekAt(const AOffset: Integer): Char; inline;
    function IsAtEnd: Boolean; inline;
    procedure AdvanceInput; inline;
    function CurrentChar: Char; inline;

    procedure Emit(const AText: string);
    procedure EmitMapped(const AText: string; const ASourceLine, ASourceColumn: Integer);
    procedure EmitChar(const AChar: Char); inline;
    procedure EmitNewline;
    procedure AddIdentityMapping;

    procedure CopyChar;
    procedure CopyString(const AQuote: Char);
    procedure CopyTemplate;
    procedure CopyLineComment;
    procedure CopyBlockComment;
    function CopyIdentifierOrKeyword: string;
    procedure CopyNumber;
    procedure CopyOperator;

    function IsJSXContext: Boolean;
    function IsJSXStart: Boolean;
    function IsIdentifierStart(const AChar: Char): Boolean; inline;
    function IsIdentifierPart(const AChar: Char): Boolean; inline;

    procedure TransformJSXElement;
    function ReadJSXTagName: string;
    procedure EmitJSXAttributes(out AHadAttributes: Boolean);
    procedure EmitJSXChildren(const ATagName: string);
    function CollectJSXText: string;
    procedure CopyJSXExpression;
    procedure ExpectJSXClosingTag(const ATagName: string);
    function TrimJSXWhitespace(const AText: string): string;
    function EscapeJSString(const AText: string): string;
    function FormatPropertyKey(const AName: string): string;

    procedure SkipWhitespace;
    procedure ScanPragmas;
    procedure TransformSource;
  public
    class function Transform(const ASource: string;
      const AFactoryName: string = 'createElement';
      const AFragmentName: string = 'Fragment'): TGocciaJSXTransformResult;
  end;

implementation

procedure WarnIfJSXExtensionMismatch(const AFilePath: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(AFilePath));
  if (Ext = '.js') or (Ext = '.ts') then
    WriteLn(Format('Warning: JSX syntax found in %s — consider using a .jsx or .tsx extension', [AFilePath]));
end;

class function TGocciaJSXTransformer.Transform(const ASource: string;
  const AFactoryName: string;
  const AFragmentName: string): TGocciaJSXTransformResult;
var
  Transformer: TGocciaJSXTransformer;
begin
  Transformer := TGocciaJSXTransformer.Create;
  try
    Transformer.FSource := ASource;
    Transformer.FPos := 1;
    Transformer.FLine := 1;
    Transformer.FColumn := 1;
    Transformer.FOutputLine := 1;
    Transformer.FOutputColumn := 1;
    Transformer.FFactoryName := AFactoryName;
    Transformer.FFragmentName := AFragmentName;
    Transformer.FLastTokenKind := ltkNone;
    Transformer.FHasJSX := False;
    Transformer.FOutput := TStringBuilder.Create;
    Transformer.FSourceMap := TGocciaSourceMap.Create;
    try
      Transformer.ScanPragmas;
      Transformer.TransformSource;
      Result.Source := Transformer.FOutput.ToString;
      if Transformer.FHasJSX then
        Result.SourceMap := Transformer.FSourceMap
      else
      begin
        Result.Source := ASource;
        Result.SourceMap := nil;
        Transformer.FSourceMap.Free;
      end;
    finally
      Transformer.FOutput.Free;
    end;
  finally
    Transformer.Free;
  end;
end;

function TGocciaJSXTransformer.Peek: Char;
begin
  if FPos > Length(FSource) then
    Result := #0
  else
    Result := FSource[FPos];
end;

function TGocciaJSXTransformer.PeekAt(const AOffset: Integer): Char;
var
  Idx: Integer;
begin
  Idx := FPos + AOffset;
  if (Idx < 1) or (Idx > Length(FSource)) then
    Result := #0
  else
    Result := FSource[Idx];
end;

function TGocciaJSXTransformer.IsAtEnd: Boolean;
begin
  Result := FPos > Length(FSource);
end;

procedure TGocciaJSXTransformer.AdvanceInput;
begin
  if FSource[FPos] = #10 then
  begin
    Inc(FLine);
    FColumn := 1;
  end
  else
    Inc(FColumn);
  Inc(FPos);
end;

function TGocciaJSXTransformer.CurrentChar: Char;
begin
  Result := FSource[FPos];
end;

procedure TGocciaJSXTransformer.Emit(const AText: string);
var
  I: Integer;
begin
  FOutput.Append(AText);
  for I := 1 to Length(AText) do
  begin
    if AText[I] = #10 then
    begin
      Inc(FOutputLine);
      FOutputColumn := 1;
    end
    else
      Inc(FOutputColumn);
  end;
end;

procedure TGocciaJSXTransformer.EmitMapped(const AText: string;
  const ASourceLine, ASourceColumn: Integer);
begin
  FSourceMap.AddMapping(FOutputLine, FOutputColumn, ASourceLine, ASourceColumn);
  Emit(AText);
end;

procedure TGocciaJSXTransformer.EmitChar(const AChar: Char);
begin
  FOutput.Append(AChar);
  if AChar = #10 then
  begin
    Inc(FOutputLine);
    FOutputColumn := 1;
  end
  else
    Inc(FOutputColumn);
end;

procedure TGocciaJSXTransformer.EmitNewline;
begin
  EmitChar(#10);
end;

procedure TGocciaJSXTransformer.AddIdentityMapping;
begin
  FSourceMap.AddMapping(FOutputLine, FOutputColumn, FLine, FColumn);
end;

procedure TGocciaJSXTransformer.CopyChar;
var
  C: Char;
begin
  C := CurrentChar;
  EmitChar(C);
  AdvanceInput;
end;

procedure TGocciaJSXTransformer.CopyString(const AQuote: Char);
begin
  CopyChar;
  while not IsAtEnd and (CurrentChar <> AQuote) do
  begin
    if CurrentChar = '\' then
      CopyChar;
    if not IsAtEnd then
      CopyChar;
  end;
  if not IsAtEnd then
    CopyChar;
  FLastTokenKind := ltkExpressionEnd;
end;

procedure TGocciaJSXTransformer.CopyTemplate;
var
  Depth: Integer;
begin
  CopyChar;
  while not IsAtEnd do
  begin
    if CurrentChar = '\' then
    begin
      CopyChar;
      if not IsAtEnd then
        CopyChar;
    end
    else if (CurrentChar = '$') and (PeekAt(1) = '{') then
    begin
      CopyChar;
      CopyChar;
      Depth := 1;
      while not IsAtEnd and (Depth > 0) do
      begin
        if CurrentChar = '{' then
          Inc(Depth)
        else if CurrentChar = '}' then
          Dec(Depth);
        if Depth > 0 then
          CopyChar;
      end;
      if not IsAtEnd then
        CopyChar;
    end
    else if CurrentChar = '`' then
    begin
      CopyChar;
      Break;
    end
    else
      CopyChar;
  end;
  FLastTokenKind := ltkExpressionEnd;
end;

procedure TGocciaJSXTransformer.CopyLineComment;
begin
  while not IsAtEnd and (CurrentChar <> #10) do
    CopyChar;
end;

procedure TGocciaJSXTransformer.CopyBlockComment;
begin
  CopyChar;
  CopyChar;
  while not IsAtEnd do
  begin
    if (CurrentChar = '*') and (PeekAt(1) = '/') then
    begin
      CopyChar;
      CopyChar;
      Exit;
    end;
    CopyChar;
  end;
end;

function TGocciaJSXTransformer.CopyIdentifierOrKeyword: string;
var
  Start: Integer;
begin
  Start := FPos;
  while not IsAtEnd and IsIdentifierPart(CurrentChar) do
    CopyChar;
  Result := Copy(FSource, Start, FPos - Start);

  if (Result = KEYWORD_RETURN) or (Result = KEYWORD_THROW) or (Result = KEYWORD_CASE) or
     (Result = KEYWORD_NEW) or (Result = KEYWORD_TYPEOF) or (Result = KEYWORD_VOID) or
     (Result = KEYWORD_DELETE) or (Result = KEYWORD_IN) or (Result = KEYWORD_INSTANCEOF) or
     (Result = KEYWORD_OF) or (Result = KEYWORD_YIELD) or (Result = KEYWORD_AWAIT) then
    FLastTokenKind := ltkOperator
  else
    FLastTokenKind := ltkExpressionEnd;
end;

procedure TGocciaJSXTransformer.CopyNumber;
begin
  if (CurrentChar = '0') and (PeekAt(1) in ['x', 'X', 'b', 'B', 'o', 'O']) then
  begin
    CopyChar;
    CopyChar;
    while not IsAtEnd and (CurrentChar in ['0'..'9', 'a'..'f', 'A'..'F', '_']) do
      CopyChar;
  end
  else
  begin
    while not IsAtEnd and (CurrentChar in ['0'..'9', '_']) do
      CopyChar;
    if not IsAtEnd and (CurrentChar = '.') and (PeekAt(1) in ['0'..'9']) then
    begin
      CopyChar;
      while not IsAtEnd and (CurrentChar in ['0'..'9', '_']) do
        CopyChar;
    end;
    if not IsAtEnd and (CurrentChar in ['e', 'E']) then
    begin
      CopyChar;
      if not IsAtEnd and (CurrentChar in ['+', '-']) then
        CopyChar;
      while not IsAtEnd and (CurrentChar in ['0'..'9']) do
        CopyChar;
    end;
  end;
  if not IsAtEnd and (CurrentChar in ['n']) then
    CopyChar;
  FLastTokenKind := ltkExpressionEnd;
end;

procedure TGocciaJSXTransformer.CopyOperator;
var
  C, Next: Char;
begin
  C := CurrentChar;
  Next := PeekAt(1);

  if ((C = '+') and (Next = '+')) or ((C = '-') and (Next = '-')) then
  begin
    CopyChar;
    CopyChar;
    FLastTokenKind := ltkExpressionEnd;
    Exit;
  end;

  if ((C = '=') and (Next = '>')) then
  begin
    CopyChar;
    CopyChar;
    FLastTokenKind := ltkOperator;
    Exit;
  end;

  if ((C = '&') and (Next = '&')) or ((C = '|') and (Next = '|')) or
     ((C = '?') and (Next = '?')) or ((C = '<') and (Next = '<')) or
     ((C = '>') and (Next = '>')) or ((C = '*') and (Next = '*')) then
  begin
    CopyChar;
    CopyChar;
    if not IsAtEnd and (CurrentChar = '=') then
      CopyChar;
    FLastTokenKind := ltkOperator;
    Exit;
  end;

  if (Next = '=') and (C in ['+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|', '^']) then
  begin
    CopyChar;
    CopyChar;
    if not IsAtEnd and (CurrentChar = '=') then
      CopyChar;
    FLastTokenKind := ltkOperator;
    Exit;
  end;

  CopyChar;

  if C in [')', ']', '}'] then
    FLastTokenKind := ltkExpressionEnd
  else
    FLastTokenKind := ltkOperator;
end;

function TGocciaJSXTransformer.IsJSXContext: Boolean;
begin
  Result := FLastTokenKind in [ltkNone, ltkOperator];
end;

function TGocciaJSXTransformer.IsJSXStart: Boolean;
begin
  if not IsJSXContext then
    Exit(False);
  if IsAtEnd then
    Exit(False);
  if CurrentChar <> '<' then
    Exit(False);
  Result := IsIdentifierStart(PeekAt(1)) or (PeekAt(1) = '>');
end;

function TGocciaJSXTransformer.IsIdentifierStart(const AChar: Char): Boolean;
begin
  Result := (AChar in ['a'..'z', 'A'..'Z', '_', '$']);
end;

function TGocciaJSXTransformer.IsIdentifierPart(const AChar: Char): Boolean;
begin
  Result := (AChar in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']);
end;

procedure TGocciaJSXTransformer.SkipWhitespace;
begin
  while not IsAtEnd and (CurrentChar in [' ', #9, #13, #10]) do
    AdvanceInput;
end;

function TGocciaJSXTransformer.ReadJSXTagName: string;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    while not IsAtEnd and IsIdentifierPart(CurrentChar) do
    begin
      SB.Append(CurrentChar);
      AdvanceInput;
    end;
    while not IsAtEnd and (CurrentChar = '.') do
    begin
      SB.Append('.');
      AdvanceInput;
      while not IsAtEnd and IsIdentifierPart(CurrentChar) do
      begin
        SB.Append(CurrentChar);
        AdvanceInput;
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TGocciaJSXTransformer.EscapeJSString(const AText: string): string;
var
  SB: TStringBuilder;
  I: Integer;
  C: Char;
begin
  SB := TStringBuilder.Create;
  try
    for I := 1 to Length(AText) do
    begin
      C := AText[I];
      case C of
        '\': SB.Append('\\');
        '"': SB.Append('\"');
        #10: SB.Append('\n');
        #13: SB.Append('\r');
        #9:  SB.Append('\t');
      else
        SB.Append(C);
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TGocciaJSXTransformer.FormatPropertyKey(const AName: string): string;
var
  I: Integer;
  NeedsQuoting: Boolean;
begin
  NeedsQuoting := False;
  for I := 1 to Length(AName) do
  begin
    if not (AName[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '$']) then
    begin
      NeedsQuoting := True;
      Break;
    end;
  end;
  if NeedsQuoting then
    Result := '"' + AName + '"'
  else
    Result := AName;
end;

function TGocciaJSXTransformer.TrimJSXWhitespace(const AText: string): string;
var
  Lines: TStringList;
  SB: TStringBuilder;
  I: Integer;
  Line: string;
  First: Boolean;
  HasNewline: Boolean;
begin
  if AText = '' then
    Exit('');

  HasNewline := (Pos(#10, AText) > 0) or (Pos(#13, AText) > 0);
  if not HasNewline then
    Exit(AText);

  Lines := TStringList.Create;
  SB := TStringBuilder.Create;
  try
    Lines.Text := AText;
    First := True;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];
      if I = 0 then
        Line := TrimRight(Line)
      else if I = Lines.Count - 1 then
        Line := TrimLeft(Line)
      else
        Line := Trim(Line);

      if Line <> '' then
      begin
        if not First then
          SB.Append(' ');
        SB.Append(Line);
        First := False;
      end;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
    Lines.Free;
  end;
end;

procedure TGocciaJSXTransformer.TransformJSXElement;
var
  TagName: string;
  StartLine, StartColumn: Integer;
  IsFragment, IsSelfClosing: Boolean;
  HadAttributes: Boolean;
  TagIsLowercase: Boolean;
begin
  FHasJSX := True;
  StartLine := FLine;
  StartColumn := FColumn;

  AdvanceInput;

  if CurrentChar = '>' then
  begin
    IsFragment := True;
    TagName := '';
    AdvanceInput;
  end
  else
  begin
    IsFragment := False;
    TagName := ReadJSXTagName;
  end;

  EmitMapped(FFactoryName + '(', StartLine, StartColumn);

  if IsFragment then
  begin
    Emit(FFragmentName);
    Emit(', null');
    EmitJSXChildren('');
    Emit(')');
    FLastTokenKind := ltkExpressionEnd;
    Exit;
  end;

  TagIsLowercase := (Length(TagName) > 0) and (TagName[1] in ['a'..'z']);
  if TagIsLowercase then
    Emit('"' + TagName + '"')
  else
    Emit(TagName);

  SkipWhitespace;

  IsSelfClosing := False;
  if not IsAtEnd and (CurrentChar = '/') and (PeekAt(1) = '>') then
  begin
    IsSelfClosing := True;
    AdvanceInput;
    AdvanceInput;
    Emit(', null)');
    FLastTokenKind := ltkExpressionEnd;
    Exit;
  end;

  if not IsAtEnd and (CurrentChar = '>') then
  begin
    AdvanceInput;
    Emit(', null');
    EmitJSXChildren(TagName);
    Emit(')');
    FLastTokenKind := ltkExpressionEnd;
    Exit;
  end;

  Emit(', ');
  EmitJSXAttributes(HadAttributes);

  SkipWhitespace;
  if not IsAtEnd and (CurrentChar = '/') and (PeekAt(1) = '>') then
  begin
    AdvanceInput;
    AdvanceInput;
    if not HadAttributes then
      Emit('null');
    Emit(')');
    FLastTokenKind := ltkExpressionEnd;
    Exit;
  end;

  if not IsAtEnd and (CurrentChar = '>') then
  begin
    AdvanceInput;
    if not HadAttributes then
      Emit('null');
    EmitJSXChildren(TagName);
    Emit(')');
    FLastTokenKind := ltkExpressionEnd;
    Exit;
  end;
end;

procedure TGocciaJSXTransformer.EmitJSXAttributes(out AHadAttributes: Boolean);
type
  TAttrSegmentKind = (askObject, askSpread);
  TAttrSegment = record
    Kind: TAttrSegmentKind;
    Content: string;
  end;
var
  Segments: array of TAttrSegment;
  SegmentCount: Integer;
  CurrentObjAttrs: TStringBuilder;
  AttrCount: Integer;
  AttrName: string;
  Depth: Integer;
  ValueStart: Integer;
  I: Integer;
  HasSpread: Boolean;
  RawExpr: string;
  SubResult: TGocciaJSXTransformResult;

  procedure FlushObjectAttrs;
  begin
    if AttrCount > 0 then
    begin
      CurrentObjAttrs.Append(' }');
      Inc(SegmentCount);
      SetLength(Segments, SegmentCount);
      Segments[SegmentCount - 1].Kind := askObject;
      Segments[SegmentCount - 1].Content := CurrentObjAttrs.ToString;
      CurrentObjAttrs.Clear;
      AttrCount := 0;
    end;
  end;

begin
  AHadAttributes := False;
  HasSpread := False;
  SegmentCount := 0;
  AttrCount := 0;
  CurrentObjAttrs := TStringBuilder.Create;
  try
    while not IsAtEnd do
    begin
      SkipWhitespace;
      if IsAtEnd or (CurrentChar = '>') or ((CurrentChar = '/') and (PeekAt(1) = '>')) then
        Break;

      AHadAttributes := True;

      if (CurrentChar = '{') and (PeekAt(1) = '.') and (PeekAt(2) = '.') and (PeekAt(3) = '.') then
      begin
        HasSpread := True;
        FlushObjectAttrs;

        AdvanceInput;
        AdvanceInput;
        AdvanceInput;
        AdvanceInput;

        Depth := 1;
        ValueStart := FPos;
        while not IsAtEnd and (Depth > 0) do
        begin
          if CurrentChar = '{' then
            Inc(Depth)
          else if CurrentChar = '}' then
            Dec(Depth);
          if Depth > 0 then
            AdvanceInput;
        end;
        Inc(SegmentCount);
        SetLength(Segments, SegmentCount);
        Segments[SegmentCount - 1].Kind := askSpread;
        Segments[SegmentCount - 1].Content := Trim(Copy(FSource, ValueStart, FPos - ValueStart));
        if not IsAtEnd then
          AdvanceInput;
        Continue;
      end;

      if (CurrentChar = '{') and (PeekAt(1) <> '.') then
      begin
        AdvanceInput;
        AttrName := '';
        while not IsAtEnd and IsIdentifierPart(CurrentChar) do
        begin
          AttrName := AttrName + CurrentChar;
          AdvanceInput;
        end;
        if not IsAtEnd and (CurrentChar = '}') then
          AdvanceInput;
        if AttrCount > 0 then
          CurrentObjAttrs.Append(',')
        else
          CurrentObjAttrs.Append('{');
        CurrentObjAttrs.Append(' ' + FormatPropertyKey(AttrName) + ': ' + AttrName);
        Inc(AttrCount);
        Continue;
      end;

      AttrName := '';
      while not IsAtEnd and IsIdentifierPart(CurrentChar) do
      begin
        AttrName := AttrName + CurrentChar;
        AdvanceInput;
      end;
      while not IsAtEnd and (CurrentChar = '-') do
      begin
        AttrName := AttrName + CurrentChar;
        AdvanceInput;
        while not IsAtEnd and IsIdentifierPart(CurrentChar) do
        begin
          AttrName := AttrName + CurrentChar;
          AdvanceInput;
        end;
      end;

      if AttrCount > 0 then
        CurrentObjAttrs.Append(',')
      else
        CurrentObjAttrs.Append('{');

      SkipWhitespace;

      if not IsAtEnd and (CurrentChar = '=') then
      begin
        AdvanceInput;
        SkipWhitespace;

        if not IsAtEnd and (CurrentChar = '"') then
        begin
          AdvanceInput;
          ValueStart := FPos;
          while not IsAtEnd and (CurrentChar <> '"') do
          begin
            if CurrentChar = '\' then
              AdvanceInput;
            if not IsAtEnd then
              AdvanceInput;
          end;
          CurrentObjAttrs.Append(' ' + FormatPropertyKey(AttrName) + ': "' + EscapeJSString(Copy(FSource, ValueStart, FPos - ValueStart)) + '"');
          if not IsAtEnd then
            AdvanceInput;
        end
        else if not IsAtEnd and (CurrentChar = '''') then
        begin
          AdvanceInput;
          ValueStart := FPos;
          while not IsAtEnd and (CurrentChar <> '''') do
          begin
            if CurrentChar = '\' then
              AdvanceInput;
            if not IsAtEnd then
              AdvanceInput;
          end;
          CurrentObjAttrs.Append(' ' + FormatPropertyKey(AttrName) + ': "' + EscapeJSString(Copy(FSource, ValueStart, FPos - ValueStart)) + '"');
          if not IsAtEnd then
            AdvanceInput;
        end
        else if not IsAtEnd and (CurrentChar = '{') then
        begin
          AdvanceInput;
          Depth := 1;
          ValueStart := FPos;
          while not IsAtEnd and (Depth > 0) do
          begin
            if CurrentChar = '{' then
              Inc(Depth)
            else if CurrentChar = '}' then
              Dec(Depth);
            if Depth > 0 then
              AdvanceInput;
          end;
          RawExpr := Trim(Copy(FSource, ValueStart, FPos - ValueStart));
          SubResult := TGocciaJSXTransformer.Transform(RawExpr, FFactoryName, FFragmentName);
          if Assigned(SubResult.SourceMap) then
            SubResult.SourceMap.Free;
          CurrentObjAttrs.Append(' ' + FormatPropertyKey(AttrName) + ': ' + SubResult.Source);
          if not IsAtEnd then
            AdvanceInput;
        end;
      end
      else
        CurrentObjAttrs.Append(' ' + FormatPropertyKey(AttrName) + ': true');

      Inc(AttrCount);
    end;

    FlushObjectAttrs;

    if SegmentCount = 0 then
    begin
      AHadAttributes := False;
      Exit;
    end;

    if not HasSpread and (SegmentCount = 1) and (Segments[0].Kind = askObject) then
      Emit(Segments[0].Content)
    else
    begin
      Emit('Object.assign({}');
      for I := 0 to SegmentCount - 1 do
      begin
        Emit(', ');
        Emit(Segments[I].Content);
      end;
      Emit(')');
    end;
  finally
    CurrentObjAttrs.Free;
  end;
end;

procedure TGocciaJSXTransformer.EmitJSXChildren(const ATagName: string);
var
  Text: string;
  ChildStartLine, ChildStartColumn: Integer;
begin
  while not IsAtEnd do
  begin
    if CurrentChar = '<' then
    begin
      if PeekAt(1) = '/' then
      begin
        if ATagName = '' then
        begin
          AdvanceInput;
          AdvanceInput;
          if not IsAtEnd and (CurrentChar = '>') then
            AdvanceInput;
        end
        else
          ExpectJSXClosingTag(ATagName);
        Exit;
      end;

      ChildStartLine := FLine;
      ChildStartColumn := FColumn;
      Emit(', ');
      FSourceMap.AddMapping(FOutputLine, FOutputColumn, ChildStartLine, ChildStartColumn);
      TransformJSXElement;
    end
    else if CurrentChar = '{' then
    begin
      ChildStartLine := FLine;
      ChildStartColumn := FColumn;
      AdvanceInput;
      Emit(', ');
      FSourceMap.AddMapping(FOutputLine, FOutputColumn, ChildStartLine, ChildStartColumn);
      CopyJSXExpression;
      if not IsAtEnd and (CurrentChar = '}') then
        AdvanceInput;
    end
    else
    begin
      Text := CollectJSXText;
      Text := TrimJSXWhitespace(Text);
      if Text <> '' then
      begin
        Emit(', ');
        Emit('"' + EscapeJSString(Text) + '"');
      end;
    end;
  end;
end;

function TGocciaJSXTransformer.CollectJSXText: string;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    while not IsAtEnd and (CurrentChar <> '<') and (CurrentChar <> '{') do
    begin
      SB.Append(CurrentChar);
      AdvanceInput;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TGocciaJSXTransformer.CopyJSXExpression;
var
  Depth: Integer;
  Quote: Char;
  SavedTokenKind: TLastTokenKind;
  IdStart: Integer;
  Ident: string;
begin
  Depth := 0;
  SavedTokenKind := FLastTokenKind;
  FLastTokenKind := ltkOperator;
  while not IsAtEnd do
  begin
    case CurrentChar of
      '{':
      begin
        Inc(Depth);
        EmitChar(CurrentChar);
        AdvanceInput;
        FLastTokenKind := ltkOperator;
      end;
      '}':
      begin
        if Depth = 0 then
          Break;
        Dec(Depth);
        EmitChar(CurrentChar);
        AdvanceInput;
        FLastTokenKind := ltkExpressionEnd;
      end;
      '''', '"':
      begin
        Quote := CurrentChar;
        EmitChar(CurrentChar);
        AdvanceInput;
        while not IsAtEnd and (CurrentChar <> Quote) do
        begin
          if CurrentChar = '\' then
          begin
            EmitChar(CurrentChar);
            AdvanceInput;
          end;
          if not IsAtEnd then
          begin
            EmitChar(CurrentChar);
            AdvanceInput;
          end;
        end;
        if not IsAtEnd then
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
        end;
        FLastTokenKind := ltkExpressionEnd;
      end;
      '`':
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
        while not IsAtEnd and (CurrentChar <> '`') do
        begin
          if CurrentChar = '\' then
          begin
            EmitChar(CurrentChar);
            AdvanceInput;
          end
          else if (CurrentChar = '$') and (PeekAt(1) = '{') then
          begin
            EmitChar(CurrentChar);
            AdvanceInput;
            EmitChar(CurrentChar);
            AdvanceInput;
            Inc(Depth);
          end;
          if not IsAtEnd and (CurrentChar <> '`') then
          begin
            EmitChar(CurrentChar);
            AdvanceInput;
          end;
        end;
        if not IsAtEnd then
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
        end;
        FLastTokenKind := ltkExpressionEnd;
      end;
      '<':
      begin
        if IsJSXStart then
          TransformJSXElement
        else
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
          FLastTokenKind := ltkOperator;
        end;
      end;
      ')', ']':
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
        FLastTokenKind := ltkExpressionEnd;
      end;
      '(', '[', ',', ':', '?', ';', '~', '!':
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
        FLastTokenKind := ltkOperator;
      end;
      '+', '-':
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
        if not IsAtEnd and (CurrentChar = PeekAt(-1)) then
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
          FLastTokenKind := ltkExpressionEnd;
        end
        else
          FLastTokenKind := ltkOperator;
      end;
      '=':
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
        if not IsAtEnd and (CurrentChar = '>') then
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
        end;
        FLastTokenKind := ltkOperator;
      end;
      ' ', #9, #13, #10:
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
      end;
    else
      if IsIdentifierStart(CurrentChar) then
      begin
        IdStart := FPos;
        while not IsAtEnd and IsIdentifierPart(CurrentChar) do
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
        end;
        Ident := Copy(FSource, IdStart, FPos - IdStart);
        if (Ident = KEYWORD_RETURN) or (Ident = KEYWORD_THROW) or (Ident = KEYWORD_CASE) or
           (Ident = KEYWORD_NEW) or (Ident = KEYWORD_TYPEOF) or (Ident = KEYWORD_VOID) or
           (Ident = KEYWORD_DELETE) or (Ident = KEYWORD_IN) or (Ident = KEYWORD_INSTANCEOF) or
           (Ident = KEYWORD_OF) or (Ident = KEYWORD_YIELD) or (Ident = KEYWORD_AWAIT) then
          FLastTokenKind := ltkOperator
        else
          FLastTokenKind := ltkExpressionEnd;
      end
      else if CurrentChar in ['0'..'9'] then
      begin
        while not IsAtEnd and (CurrentChar in ['0'..'9', '.', '_', 'a'..'f', 'A'..'F', 'n']) do
        begin
          EmitChar(CurrentChar);
          AdvanceInput;
        end;
        FLastTokenKind := ltkExpressionEnd;
      end
      else
      begin
        EmitChar(CurrentChar);
        AdvanceInput;
        FLastTokenKind := ltkOperator;
      end;
    end;
  end;
  FLastTokenKind := SavedTokenKind;
end;

procedure TGocciaJSXTransformer.ExpectJSXClosingTag(const ATagName: string);
var
  ClosingTag: string;
begin
  AdvanceInput;
  AdvanceInput;
  SkipWhitespace;
  ClosingTag := ReadJSXTagName;
  SkipWhitespace;
  if not IsAtEnd and (CurrentChar = '>') then
    AdvanceInput;
  if ClosingTag <> ATagName then
    raise Exception.CreateFmt('JSX: Expected closing tag </%s> but found </%s> at line %d', [ATagName, ClosingTag, FLine]);
end;

procedure TGocciaJSXTransformer.ScanPragmas;
var
  SavedPos, SavedLine, SavedColumn: Integer;
  CommentStart, PragmaPos: Integer;
  CommentText, PragmaValue: string;

  function ExtractFirstWord(const AText: string; AStart: Integer): string;
  var
    I, WordStart: Integer;
  begin
    Result := '';
    I := AStart;
    while (I <= Length(AText)) and (AText[I] in [' ', #9]) do
      Inc(I);
    WordStart := I;
    while (I <= Length(AText)) and not (AText[I] in [' ', #9, #10, #13, '*', '/']) do
      Inc(I);
    if I > WordStart then
      Result := Copy(AText, WordStart, I - WordStart);
  end;

  function IsPragmaDirective(const AComment: string; const ADirective: string): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    I := 1;
    while (I <= Length(AComment)) and (AComment[I] in ['/', '*', ' ', #9]) do
      Inc(I);
    if Copy(AComment, I, Length(ADirective)) = ADirective then
      Result := I;
  end;

  procedure CheckPragma(const AComment: string);
  begin
    PragmaPos := IsPragmaDirective(AComment, '@jsxFactory ');
    if PragmaPos > 0 then
    begin
      PragmaValue := ExtractFirstWord(AComment, PragmaPos + 12);
      if PragmaValue <> '' then
        FFactoryName := PragmaValue;
    end;

    PragmaPos := IsPragmaDirective(AComment, '@jsxFragment ');
    if PragmaPos > 0 then
    begin
      PragmaValue := ExtractFirstWord(AComment, PragmaPos + 13);
      if PragmaValue <> '' then
        FFragmentName := PragmaValue;
    end;
  end;

begin
  SavedPos := FPos;
  SavedLine := FLine;
  SavedColumn := FColumn;

  while not IsAtEnd do
  begin
    if CurrentChar in [' ', #9, #13, #10] then
    begin
      AdvanceInput;
      Continue;
    end;

    if (CurrentChar = '/') and (PeekAt(1) = '/') then
    begin
      CommentStart := FPos;
      while not IsAtEnd and (CurrentChar <> #10) do
        AdvanceInput;
      CommentText := Copy(FSource, CommentStart, FPos - CommentStart);
      CheckPragma(CommentText);
      Continue;
    end;

    if (CurrentChar = '/') and (PeekAt(1) = '*') then
    begin
      CommentStart := FPos;
      AdvanceInput;
      AdvanceInput;
      while not IsAtEnd do
      begin
        if (CurrentChar = '*') and (PeekAt(1) = '/') then
        begin
          AdvanceInput;
          AdvanceInput;
          Break;
        end;
        AdvanceInput;
      end;
      CommentText := Copy(FSource, CommentStart, FPos - CommentStart);
      CheckPragma(CommentText);
      Continue;
    end;

    Break;
  end;

  FPos := SavedPos;
  FLine := SavedLine;
  FColumn := SavedColumn;
end;

procedure TGocciaJSXTransformer.TransformSource;
var
  C: Char;
begin
  AddIdentityMapping;
  while not IsAtEnd do
  begin
    C := CurrentChar;

    if (C = '/') and (PeekAt(1) = '/') then
    begin
      CopyLineComment;
      Continue;
    end;

    if (C = '/') and (PeekAt(1) = '*') then
    begin
      CopyBlockComment;
      Continue;
    end;

    if C in ['''', '"'] then
    begin
      CopyString(C);
      Continue;
    end;

    if C = '`' then
    begin
      CopyTemplate;
      Continue;
    end;

    if C = #10 then
    begin
      CopyChar;
      AddIdentityMapping;
      FLastTokenKind := ltkOperator;
      Continue;
    end;

    if C = #13 then
    begin
      CopyChar;
      if not IsAtEnd and (CurrentChar = #10) then
        CopyChar;
      AddIdentityMapping;
      FLastTokenKind := ltkOperator;
      Continue;
    end;

    if C in [' ', #9] then
    begin
      CopyChar;
      Continue;
    end;

    if IsJSXStart then
    begin
      TransformJSXElement;
      Continue;
    end;

    if IsIdentifierStart(C) then
    begin
      CopyIdentifierOrKeyword;
      Continue;
    end;

    if C in ['0'..'9'] then
    begin
      CopyNumber;
      Continue;
    end;

    CopyOperator;
  end;
end;

end.
