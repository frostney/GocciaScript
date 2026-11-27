unit Goccia.YAML.Scanner;

{ The YAML 1.2.2 scanner (yaml.org/spec/1.2.2): text into the tokens
  Goccia.YAML.Events parses. It tracks the indentation levels, the flow
  nesting and the simple keys, the scalars that may turn out to be implicit
  keys once a ":" follows, so a token is handed out only when the tokens
  before it are certain.

  Ported from the scanner of saphyr-parser (github.com/saphyr-rs/saphyr,
  parser/src/scanner.rs at 7867251b), which descends from libyaml and passes
  the yaml-test-suite. Copyright 2015 Yuheng Chen and 2023 Ethiraric, used
  under the MIT license:

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.

  Where the port differs from saphyr: positions count UTF-16 code units, as
  the text does; flow nesting has no limit of its own, since the composer's
  depth guard stops deep nesting; a "\u" escape may name a surrogate, so a
  JSON-style pair spells its character; "%xx" escapes in a tag decode as
  UTF-8; a block-level key goes stale inside a flow collection too
  (StaleSimpleKeys); and the rules saphyr leaves out are kept: a flow
  collection's lines are deeper than the block node that holds it, except a
  closing bracket, which may sit at that node's indentation as other parsers
  allow; after a tab only a flow node follows "?" or ":" on the line; an empty
  key's value is no block collection on the same line; "|" and ">" start no
  plain scalar in a flow collection; and %TAG separates handle and prefix. }

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  EGocciaYAMLParseError = class(Exception);

  { a place in the text: Index from 0, Line from 1, Column from 0 }
  TGocciaYAMLMarker = record
    Index: Integer;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaYAMLScalarStyle = (yssPlain, yssSingleQuoted, yssDoubleQuoted,
    yssLiteral, yssFolded);

  TGocciaYAMLTokenKind = (
    ytkStreamStart,
    ytkStreamEnd,
    ytkVersionDirective,
    ytkTagDirective,
    ytkReservedDirective,
    ytkDocumentStart,
    ytkDocumentEnd,
    ytkByteOrderMark,
    ytkBlockSequenceStart,
    ytkBlockMappingStart,
    ytkBlockEnd,
    ytkFlowSequenceStart,
    ytkFlowSequenceEnd,
    ytkFlowMappingStart,
    ytkFlowMappingEnd,
    ytkBlockEntry,
    ytkFlowEntry,
    ytkKey,
    ytkValue,
    ytkAlias,
    ytkAnchor,
    ytkTag,
    ytkScalar
  );

  TGocciaYAMLToken = record
    Kind: TGocciaYAMLTokenKind;
    StartMark: TGocciaYAMLMarker;
    EndMark: TGocciaYAMLMarker;
    { a scalar's content, an anchor's or alias's name, a tag's suffix, a %TAG
      directive's prefix }
    Value: string;
    { a tag's handle, a %TAG directive's handle }
    Handle: string;
    Style: TGocciaYAMLScalarStyle;
    { a %YAML directive's version }
    MajorVersion: Integer;
    MinorVersion: Integer;
  end;

  TGocciaYAMLScanner = class
  private type
    { a token that becomes an implicit key if a ":" follows it in time }
    TSimpleKey = record
      Possible: Boolean;
      Required: Boolean;
      { the token's place among all the tokens, handed out or queued }
      TokenNumber: Integer;
      Mark: TGocciaYAMLMarker;
    end;

    TIndentLevel = record
      { the indentation this level replaced }
      Indent: Integer;
      { False for the one-column level an entry adds, which ends no block }
      NeedsBlockEnd: Boolean;
    end;

    TImplicitMappingState = (imsNone, imsPossible, imsInside);
  private
    FText: string;
    FPosition: Integer;
    FMark: TGocciaYAMLMarker;
    { the queued tokens: FTokenCount of them from FTokenHead on }
    FTokenBuffer: array of TGocciaYAMLToken;
    FTokenHead: Integer;
    FTokenCount: Integer;
    FStreamStartProduced: Boolean;
    FStreamEndProduced: Boolean;
    FAdjacentValueAllowedAt: Integer;
    FSimpleKeyAllowed: Boolean;
    FSimpleKeys: array of TSimpleKey;
    FSimpleKeyCount: Integer;
    FIndent: Integer;
    FIndents: array of TIndentLevel;
    FIndentCount: Integer;
    FFlowLevel: Integer;
    FTokensParsed: Integer;
    FTokenAvailable: Boolean;
    FLeadingWhitespace: Boolean;
    FFlowMappingStarted: array of Boolean;
    FImplicitFlowMappingStates: array of TImplicitMappingState;
    FFlowStateCount: Integer;
    { the columns of the block "?" indicators whose ":" has not come yet }
    FExplicitKeyColumns: array of Integer;
    FExplicitKeyCount: Integer;
    FPlainInterruptedByComment: Boolean;
    FPlainInterruptionMark: TGocciaYAMLMarker;
    FPlainWhitespaces: string;
    FPlainLeadingBreak: string;
    FPlainTrailingBreaks: string;

    procedure RaiseError(const AMark: TGocciaYAMLMarker; const AMessage: string);
    function Peek(const AOffset: Integer = 0): Char; {$IFDEF FPC}inline;{$ENDIF}
    function AtEnd: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsZ: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsBlank: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsBreak: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsBreakZ: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsBlankOrBreak: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsBlankOrBreakZ: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsFlow: Boolean; {$IFDEF FPC}inline;{$ENDIF}
    function NextIsDocumentStart: Boolean;
    function NextIsDocumentEnd: Boolean;
    function NextIsDocumentIndicator: Boolean;
    function NextCanBePlainScalar(const AInFlow: Boolean): Boolean;

    procedure SkipBlank; {$IFDEF FPC}inline;{$ENDIF}
    procedure SkipNonBlank; {$IFDEF FPC}inline;{$ENDIF}
    procedure SkipNonBlanks(const ACount: Integer);
    procedure SkipNewline;
    procedure SkipLinebreak;
    procedure SkipBreak;
    procedure ReadBreak(var AText: string);
    procedure SkipComment;
    procedure SkipBlanks;
    procedure SkipWhitespaceToEndOfLine(const ASkipTabs: Boolean;
      out AFoundTabs, AFoundSpaces: Boolean); overload;
    procedure SkipWhitespaceToEndOfLine(const ASkipTabs: Boolean); overload;
    procedure SkipToNextToken;

    function NewToken(const AKind: TGocciaYAMLTokenKind;
      const AStartMark, AEndMark: TGocciaYAMLMarker): TGocciaYAMLToken;
    procedure PushToken(const AKind: TGocciaYAMLTokenKind;
      const AStartMark, AEndMark: TGocciaYAMLMarker);
    procedure MakeRoomForToken;
    procedure QueueToken(const AToken: TGocciaYAMLToken);
    procedure InsertToken(const APosition: Integer; const AToken: TGocciaYAMLToken);
    function TakeToken: TGocciaYAMLToken;
    function LastSimpleKey: Integer; {$IFDEF FPC}inline;{$ENDIF}
    procedure PushSimpleKey;
    procedure PopSimpleKey;
    procedure PushIndent(const AIndent: Integer; const ANeedsBlockEnd: Boolean);
    procedure PopIndent;
    procedure StaleSimpleKeys;
    procedure SaveSimpleKey;
    procedure RemoveSimpleKey;
    procedure RollIndent(const AColumn, ATokenNumber: Integer;
      const AKind: TGocciaYAMLTokenKind; const AMark: TGocciaYAMLMarker);
    procedure UnrollIndent(const AColumn: Integer);
    procedure RollOneColumnIndent;
    procedure UnrollNonBlockIndents;
    procedure IncreaseFlowLevel;
    procedure DecreaseFlowLevel;
    procedure EndImplicitMapping(const AMark: TGocciaYAMLMarker);
    function FlowContentIndent: Integer;
    procedure DropExplicitKeysFrom(const AColumn: Integer);
    procedure SkipSeparationAfterIndicator(const AIndicator: Char;
      out ATabSeparated: Boolean);

    procedure FetchMoreTokens;
    procedure FetchNextToken;
    procedure FetchStreamStart;
    procedure FetchStreamEnd;
    procedure FetchDirective;
    procedure FetchDocumentIndicator(const AKind: TGocciaYAMLTokenKind);
    procedure FetchByteOrderMark;
    procedure FetchFlowCollectionStart(const AKind: TGocciaYAMLTokenKind);
    procedure FetchFlowCollectionEnd(const AKind: TGocciaYAMLTokenKind);
    procedure FetchFlowEntry;
    procedure FetchBlockEntry;
    procedure FetchKey;
    procedure FetchFlowValue;
    procedure FetchValue;
    procedure FetchAnchor(const AAlias: Boolean);
    procedure FetchTag;
    procedure FetchBlockScalar(const ALiteral: Boolean);
    procedure FetchFlowScalar(const ASingle: Boolean);
    procedure FetchPlainScalar;

    function ScanDirective: TGocciaYAMLToken;
    function ScanDirectiveName: string;
    function ScanVersionDirectiveNumber(const AMark: TGocciaYAMLMarker): Integer;
    function ScanTagHandle(const ADirective: Boolean;
      const AMark: TGocciaYAMLMarker): string;
    function ScanTagPrefix(const AMark: TGocciaYAMLMarker): string;
    function ScanVerbatimTag(const AMark: TGocciaYAMLMarker): string;
    function ScanTagShorthandSuffix(const AHead: string;
      const AMark: TGocciaYAMLMarker): string;
    function ScanURIEscapes(const AMark: TGocciaYAMLMarker): string;
    function ScanTag: TGocciaYAMLToken;
    function ScanAnchor(const AAlias: Boolean): TGocciaYAMLToken;
    function ScanBlockScalar(const ALiteral: Boolean): TGocciaYAMLToken;
    procedure SkipBlockScalarIndent(const AIndent: Integer; var ABreaks: string);
    procedure SkipBlockScalarFirstLineIndent(var AIndent: Integer;
      var ABreaks: string);
    function ScanFlowScalar(const ASingle: Boolean): TGocciaYAMLToken;
    function ScanEscapeSequence(const AStartMark: TGocciaYAMLMarker): string;
    function ScanPlainScalar: TGocciaYAMLToken;
  public
    constructor Create(const AText: string);
    { the next token; raises EGocciaYAMLParseError for text that is not
      YAML. After the stream-end token there are none. }
    function NextToken: TGocciaYAMLToken;

    property Mark: TGocciaYAMLMarker read FMark;
  end;

function YAMLMarker(const AIndex, ALine, AColumn: Integer): TGocciaYAMLMarker;

implementation

uses
  StringBuffer;

const
  CHARACTER_NIL = #0;
  CHARACTER_BYTE_ORDER_MARK = #$FEFF;
  { a simple key ends within this many code units, as libyaml's does }
  MAX_SIMPLE_KEY_LENGTH = 1024;
  { a %YAML version number has at most this many digits }
  MAX_VERSION_DIGITS = 9;
  MAX_CODE_POINT = $10FFFF;
  FIRST_SUPPLEMENTARY_CODE_POINT = $10000;
  FIRST_SURROGATE = $D800;
  LAST_SURROGATE = $DFFF;
  HIGH_SURROGATE_BASE = $D800;
  LOW_SURROGATE_BASE = $DC00;
  SURROGATE_PAYLOAD_BITS = 10;
  SURROGATE_PAYLOAD_MASK = $3FF;
  ESCAPE_HEX_DIGITS_X = 2;
  ESCAPE_HEX_DIGITS_U = 4;
  ESCAPE_HEX_DIGITS_LONG_U = 8;
  URI_ESCAPE_LENGTH = 3;
  DOCUMENT_INDICATOR_LENGTH = 3;

function YAMLMarker(const AIndex, ALine, AColumn: Integer): TGocciaYAMLMarker;
begin
  Result.Index := AIndex;
  Result.Line := ALine;
  Result.Column := AColumn;
end;

function IsBreak(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (C = #10) or (C = #13);
end;

function IsBreakZ(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (C = #10) or (C = #13) or (C = CHARACTER_NIL);
end;

function IsBlank(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (C = ' ') or (C = #9);
end;

function IsBlankOrBreakZ(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := IsBlank(C) or IsBreakZ(C);
end;

function IsFlowCharacter(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (C = ',') or (C = '[') or (C = ']') or (C = '{') or (C = '}');
end;

function IsDigitCharacter(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (C >= '0') and (C <= '9');
end;

function IsAlphaCharacter(const C: Char): Boolean;
begin
  Result := IsDigitCharacter(C) or ((C >= 'a') and (C <= 'z')) or
    ((C >= 'A') and (C <= 'Z')) or (C = '_') or (C = '-');
end;

function IsHexCharacter(const C: Char): Boolean;
begin
  Result := IsDigitCharacter(C) or ((C >= 'a') and (C <= 'f')) or
    ((C >= 'A') and (C <= 'F'));
end;

function HexValue(const C: Char): Integer;
begin
  if IsDigitCharacter(C) then
    Result := Ord(C) - Ord('0')
  else if (C >= 'a') and (C <= 'f') then
    Result := Ord(C) - Ord('a') + 10
  else
    Result := Ord(C) - Ord('A') + 10;
end;

{ not the end, a line break or a byte order mark }
function IsYAMLNonBreak(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := not IsBreakZ(C) and (C <> CHARACTER_BYTE_ORDER_MARK);
end;

function IsYAMLNonSpace(const C: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := IsYAMLNonBreak(C) and not IsBlank(C);
end;

function IsAnchorCharacter(const C: Char): Boolean;
begin
  Result := IsYAMLNonSpace(C) and not IsFlowCharacter(C);
end;

function IsWordCharacter(const C: Char): Boolean;
begin
  Result := IsAlphaCharacter(C) and (C <> '_');
end;

function IsURICharacter(const C: Char): Boolean;
begin
  Result := IsWordCharacter(C) or (Pos(C, '#;/?:@&=+$,_.!~*''()[]%') > 0);
end;

function IsTagCharacter(const C: Char): Boolean;
begin
  Result := IsURICharacter(C) and not IsFlowCharacter(C) and (C <> '!');
end;

{ the UTF-16 text of a code point }
function CodePointText(const ACodePoint: Cardinal): string;
var
  Offset: Cardinal;
begin
  if ACodePoint < FIRST_SUPPLEMENTARY_CODE_POINT then
    Exit(Char(ACodePoint));
  Offset := ACodePoint - FIRST_SUPPLEMENTARY_CODE_POINT;
  Result := Char(HIGH_SURROGATE_BASE + (Offset shr SURROGATE_PAYLOAD_BITS)) +
    Char(LOW_SURROGATE_BASE + (Offset and SURROGATE_PAYLOAD_MASK));
end;

{ TGocciaYAMLScanner }

constructor TGocciaYAMLScanner.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FPosition := 1;
  FMark := YAMLMarker(0, 1, 0);
  FSimpleKeyAllowed := True;
  FIndent := -1;
  FLeadingWhitespace := True;
end;

procedure TGocciaYAMLScanner.RaiseError(const AMark: TGocciaYAMLMarker;
  const AMessage: string);
begin
  raise EGocciaYAMLParseError.CreateFmt('Line %d, column %d: %s',
    [AMark.Line, AMark.Column + 1, AMessage]);
end;

function TGocciaYAMLScanner.Peek(const AOffset: Integer): Char;
begin
  if FPosition + AOffset <= Length(FText) then
    Result := FText[FPosition + AOffset]
  else
    Result := CHARACTER_NIL;
end;

function TGocciaYAMLScanner.AtEnd: Boolean;
begin
  Result := FPosition > Length(FText);
end;

function TGocciaYAMLScanner.NextIsZ: Boolean;
begin
  Result := Peek = CHARACTER_NIL;
end;

function TGocciaYAMLScanner.NextIsBlank: Boolean;
begin
  Result := IsBlank(Peek);
end;

function TGocciaYAMLScanner.NextIsBreak: Boolean;
begin
  Result := IsBreak(Peek);
end;

function TGocciaYAMLScanner.NextIsBreakZ: Boolean;
begin
  Result := IsBreakZ(Peek);
end;

function TGocciaYAMLScanner.NextIsBlankOrBreak: Boolean;
begin
  Result := IsBlank(Peek) or IsBreak(Peek);
end;

function TGocciaYAMLScanner.NextIsBlankOrBreakZ: Boolean;
begin
  Result := IsBlankOrBreakZ(Peek);
end;

function TGocciaYAMLScanner.NextIsFlow: Boolean;
begin
  Result := IsFlowCharacter(Peek);
end;

function TGocciaYAMLScanner.NextIsDocumentStart: Boolean;
begin
  Result := (Peek = '-') and (Peek(1) = '-') and (Peek(2) = '-') and
    IsBlankOrBreakZ(Peek(DOCUMENT_INDICATOR_LENGTH));
end;

function TGocciaYAMLScanner.NextIsDocumentEnd: Boolean;
begin
  Result := (Peek = '.') and (Peek(1) = '.') and (Peek(2) = '.') and
    IsBlankOrBreakZ(Peek(DOCUMENT_INDICATOR_LENGTH));
end;

function TGocciaYAMLScanner.NextIsDocumentIndicator: Boolean;
begin
  Result := NextIsDocumentStart or NextIsDocumentEnd;
end;

{ whether the next character may go on a plain scalar (7.3.3); not called on
  a blank or break }
function TGocciaYAMLScanner.NextCanBePlainScalar(const AInFlow: Boolean): Boolean;
var
  C, NextCharacter: Char;
begin
  C := Peek;
  NextCharacter := Peek(1);
  if (C = ':') and (IsBlankOrBreakZ(NextCharacter) or
     (AInFlow and IsFlowCharacter(NextCharacter))) then
    Exit(False);
  if C = CHARACTER_BYTE_ORDER_MARK then
    Exit(False);
  Result := not (AInFlow and IsFlowCharacter(C));
end;

procedure TGocciaYAMLScanner.SkipBlank;
begin
  Inc(FPosition);
  Inc(FMark.Index);
  Inc(FMark.Column);
end;

procedure TGocciaYAMLScanner.SkipNonBlank;
begin
  Inc(FPosition);
  Inc(FMark.Index);
  Inc(FMark.Column);
  FLeadingWhitespace := False;
end;

procedure TGocciaYAMLScanner.SkipNonBlanks(const ACount: Integer);
begin
  Inc(FPosition, ACount);
  Inc(FMark.Index, ACount);
  Inc(FMark.Column, ACount);
  FLeadingWhitespace := False;
end;

procedure TGocciaYAMLScanner.SkipNewline;
begin
  Inc(FPosition);
  Inc(FMark.Index);
  FMark.Column := 0;
  Inc(FMark.Line);
  FLeadingWhitespace := True;
end;

{ a line break (CR, LF or CRLF), if one is next }
procedure TGocciaYAMLScanner.SkipLinebreak;
begin
  if (Peek = #13) and (Peek(1) = #10) then
  begin
    SkipBlank;
    SkipNewline;
  end
  else if NextIsBreak then
    SkipNewline;
end;

{ the line break that is next }
procedure TGocciaYAMLScanner.SkipBreak;
begin
  if (Peek = #13) and (Peek(1) = #10) then
    SkipBlank;
  SkipNewline;
end;

procedure TGocciaYAMLScanner.ReadBreak(var AText: string);
begin
  SkipBreak;
  AText := AText + #10;
end;

{ from "#" to the end of the line }
procedure TGocciaYAMLScanner.SkipComment;
begin
  while IsYAMLNonBreak(Peek) do
    SkipBlank;
end;

procedure TGocciaYAMLScanner.SkipBlanks;
begin
  while NextIsBlank do
    SkipBlank;
end;

{ spaces, tabs when ASkipTabs, and a comment up to the end of the line; a
  comment must follow whitespace }
procedure TGocciaYAMLScanner.SkipWhitespaceToEndOfLine(const ASkipTabs: Boolean;
  out AFoundTabs, AFoundSpaces: Boolean);
var
  CommentNeedsWhitespace: Boolean;
begin
  AFoundTabs := False;
  AFoundSpaces := False;
  while True do
  begin
    if Peek = ' ' then
      AFoundSpaces := True
    else if (Peek = #9) and ASkipTabs then
      AFoundTabs := True
    else
      Break;
    SkipBlank;
  end;
  CommentNeedsWhitespace := False;
  if Peek = '#' then
  begin
    if not AFoundTabs and not AFoundSpaces then
      CommentNeedsWhitespace := True
    else
      SkipComment;
  end;
  if (FMark.Column > 0) and (Peek = CHARACTER_BYTE_ORDER_MARK) then
    RaiseError(FMark, 'a byte order mark must not appear inside a document');
  if CommentNeedsWhitespace then
    RaiseError(FMark, 'comments must be separated from other tokens by whitespace');
end;

procedure TGocciaYAMLScanner.SkipWhitespaceToEndOfLine(const ASkipTabs: Boolean);
var
  FoundTabs, FoundSpaces: Boolean;
begin
  SkipWhitespaceToEndOfLine(ASkipTabs, FoundTabs, FoundSpaces);
end;

{ whitespace, line breaks and comments up to the next token }
procedure TGocciaYAMLScanner.SkipToNextToken;
var
  ImmediateNextLine: Boolean;
begin
  while True do
  begin
    case Peek of
      #9:
        // tabs are not indentation: inside a block, a tab may stand where
        // the indentation is only on a line with no content
        if (FIndentCount > 0) and FLeadingWhitespace and
           (FMark.Column < FIndent) then
        begin
          SkipWhitespaceToEndOfLine(True);
          if not NextIsBreakZ then
            RaiseError(FMark,
              'tabs disallowed within this context (block indentation)');
        end
        else
          SkipBlank;
      ' ':
        SkipBlank;
      #10, #13:
        begin
          SkipLinebreak;
          if FFlowLevel = 0 then
            FSimpleKeyAllowed := True;
        end;
      '#':
        SkipComment;
    else
      Break;
    end;
  end;
  // a comment after a plain scalar ends it: a block line that would continue
  // it is an error (BS4K), unless an empty line came between
  if FPlainInterruptedByComment then
  begin
    FPlainInterruptedByComment := False;
    ImmediateNextLine := FMark.Line = FPlainInterruptionMark.Line + 1;
    if (FFlowLevel = 0) and ImmediateNextLine and (FMark.Column > FIndent) and
       not NextIsZ and not NextIsDocumentIndicator and
       NextCanBePlainScalar(False) then
      RaiseError(FPlainInterruptionMark, 'comment intercepting the multiline text');
  end;
end;

function TGocciaYAMLScanner.NewToken(const AKind: TGocciaYAMLTokenKind;
  const AStartMark, AEndMark: TGocciaYAMLMarker): TGocciaYAMLToken;
begin
  Result.Kind := AKind;
  Result.StartMark := AStartMark;
  Result.EndMark := AEndMark;
  Result.Value := '';
  Result.Handle := '';
  Result.Style := yssPlain;
  Result.MajorVersion := 0;
  Result.MinorVersion := 0;
end;

procedure TGocciaYAMLScanner.PushToken(const AKind: TGocciaYAMLTokenKind;
  const AStartMark, AEndMark: TGocciaYAMLMarker);
begin
  QueueToken(NewToken(AKind, AStartMark, AEndMark));
end;

{ room for one more token after the queued ones: the queue moves to the
  front of the buffer when that frees at least half of it, so each token is
  moved a bounded number of times, otherwise the buffer grows }
procedure TGocciaYAMLScanner.MakeRoomForToken;
var
  I: Integer;
begin
  if FTokenHead + FTokenCount < Length(FTokenBuffer) then
    Exit;
  if (FTokenHead > 0) and (FTokenHead >= FTokenCount) then
  begin
    for I := 0 to FTokenCount - 1 do
      FTokenBuffer[I] := FTokenBuffer[FTokenHead + I];
    for I := FTokenCount to FTokenHead + FTokenCount - 1 do
      FTokenBuffer[I] := Default(TGocciaYAMLToken);
    FTokenHead := 0;
  end
  else
    SetLength(FTokenBuffer, Length(FTokenBuffer) * 2 + 16);
end;

procedure TGocciaYAMLScanner.QueueToken(const AToken: TGocciaYAMLToken);
begin
  MakeRoomForToken;
  FTokenBuffer[FTokenHead + FTokenCount] := AToken;
  Inc(FTokenCount);
end;

procedure TGocciaYAMLScanner.InsertToken(const APosition: Integer;
  const AToken: TGocciaYAMLToken);
var
  I: Integer;
begin
  MakeRoomForToken;
  for I := FTokenHead + FTokenCount downto FTokenHead + APosition + 1 do
    FTokenBuffer[I] := FTokenBuffer[I - 1];
  FTokenBuffer[FTokenHead + APosition] := AToken;
  Inc(FTokenCount);
end;

function TGocciaYAMLScanner.TakeToken: TGocciaYAMLToken;
begin
  Result := FTokenBuffer[FTokenHead];
  FTokenBuffer[FTokenHead] := Default(TGocciaYAMLToken);
  Inc(FTokenHead);
  Dec(FTokenCount);
  if FTokenCount = 0 then
    FTokenHead := 0;
end;

function TGocciaYAMLScanner.LastSimpleKey: Integer;
begin
  Result := FSimpleKeyCount - 1;
end;

procedure TGocciaYAMLScanner.PushSimpleKey;
begin
  if FSimpleKeyCount = Length(FSimpleKeys) then
    SetLength(FSimpleKeys, FSimpleKeyCount * 2 + 4);
  FSimpleKeys[FSimpleKeyCount].Possible := False;
  FSimpleKeys[FSimpleKeyCount].Required := False;
  FSimpleKeys[FSimpleKeyCount].TokenNumber := 0;
  FSimpleKeys[FSimpleKeyCount].Mark := YAMLMarker(0, 0, 0);
  Inc(FSimpleKeyCount);
end;

procedure TGocciaYAMLScanner.PopSimpleKey;
begin
  if FSimpleKeyCount > 0 then
    Dec(FSimpleKeyCount);
end;

procedure TGocciaYAMLScanner.PushIndent(const AIndent: Integer;
  const ANeedsBlockEnd: Boolean);
begin
  if FIndentCount = Length(FIndents) then
    SetLength(FIndents, FIndentCount * 2 + 4);
  FIndents[FIndentCount].Indent := AIndent;
  FIndents[FIndentCount].NeedsBlockEnd := ANeedsBlockEnd;
  Inc(FIndentCount);
end;

procedure TGocciaYAMLScanner.PopIndent;
begin
  Dec(FIndentCount);
  FIndent := FIndents[FIndentCount].Indent;
end;

{ a block-level simple key, the first on the stack, is on one line and
  short, a flow collection key included (7.4.2: block-key separation stays on
  the line); one that can no longer be a key is marked so, and a required one
  is an error. Checking it inside a flow collection too, where saphyr waits
  for the collection to end, keeps the tokens of a long collection from
  queueing behind its opening bracket. Keys inside flow collections may span
  lines. }
procedure TGocciaYAMLScanner.StaleSimpleKeys;
begin
  if (FSimpleKeyCount > 0) and FSimpleKeys[0].Possible and
     ((FSimpleKeys[0].Mark.Line < FMark.Line) or
      (FSimpleKeys[0].Mark.Index + MAX_SIMPLE_KEY_LENGTH < FMark.Index)) then
  begin
    if FSimpleKeys[0].Required then
      RaiseError(FMark, 'simple key expect '':''');
    FSimpleKeys[0].Possible := False;
  end;
end;

{ the token about to be queued may be a simple key }
procedure TGocciaYAMLScanner.SaveSimpleKey;
var
  Key: Integer;
begin
  if not FSimpleKeyAllowed then
    Exit;
  Key := LastSimpleKey;
  FSimpleKeys[Key].Possible := True;
  FSimpleKeys[Key].Required := (FFlowLevel = 0) and (FIndent = FMark.Column) and
    (FIndentCount > 0) and FIndents[FIndentCount - 1].NeedsBlockEnd;
  FSimpleKeys[Key].TokenNumber := FTokensParsed + FTokenCount;
  FSimpleKeys[Key].Mark := FMark;
end;

procedure TGocciaYAMLScanner.RemoveSimpleKey;
var
  Key: Integer;
begin
  Key := LastSimpleKey;
  if Key < 0 then
    RaiseError(FMark, 'simple key expected');
  if FSimpleKeys[Key].Possible and FSimpleKeys[Key].Required then
    RaiseError(FMark, 'simple key expected');
  FSimpleKeys[Key].Possible := False;
end;

{ a block collection starts at AColumn when it is deeper than the current
  indentation; ATokenNumber places its start token before a queued key, -1
  queues it last }
procedure TGocciaYAMLScanner.RollIndent(const AColumn, ATokenNumber: Integer;
  const AKind: TGocciaYAMLTokenKind; const AMark: TGocciaYAMLMarker);
begin
  if FFlowLevel > 0 then
    Exit;
  // a one-column level an entry added turns out to start a block
  if (FIndent <= AColumn) and (FIndentCount > 0) and
     not FIndents[FIndentCount - 1].NeedsBlockEnd then
    PopIndent;
  if FIndent < AColumn then
  begin
    PushIndent(FIndent, True);
    FIndent := AColumn;
    if ATokenNumber >= 0 then
      InsertToken(ATokenNumber - FTokensParsed, NewToken(AKind, AMark, AMark))
    else
      PushToken(AKind, AMark, AMark);
  end;
end;

{ the block collections deeper than AColumn end }
procedure TGocciaYAMLScanner.UnrollIndent(const AColumn: Integer);
var
  NeedsBlockEnd: Boolean;
begin
  if FFlowLevel > 0 then
    Exit;
  while FIndent > AColumn do
  begin
    NeedsBlockEnd := FIndents[FIndentCount - 1].NeedsBlockEnd;
    PopIndent;
    if NeedsBlockEnd then
      PushToken(ytkBlockEnd, FMark, FMark);
  end;
  DropExplicitKeysFrom(AColumn + 1);
end;

{ an entry's content must be indented one column past it, without starting a
  block }
procedure TGocciaYAMLScanner.RollOneColumnIndent;
begin
  if (FFlowLevel = 0) and (FIndentCount > 0) and
     FIndents[FIndentCount - 1].NeedsBlockEnd then
  begin
    PushIndent(FIndent, False);
    Inc(FIndent);
  end;
end;

procedure TGocciaYAMLScanner.UnrollNonBlockIndents;
begin
  while (FIndentCount > 0) and not FIndents[FIndentCount - 1].NeedsBlockEnd do
    PopIndent;
end;

procedure TGocciaYAMLScanner.IncreaseFlowLevel;
begin
  PushSimpleKey;
  Inc(FFlowLevel);
end;

procedure TGocciaYAMLScanner.DecreaseFlowLevel;
begin
  if FFlowLevel > 0 then
  begin
    Dec(FFlowLevel);
    PopSimpleKey;
  end;
end;

{ a flow sequence's implicit single-pair mapping ([a: b]) ends at "," or "]" }
procedure TGocciaYAMLScanner.EndImplicitMapping(const AMark: TGocciaYAMLMarker);
var
  Top: Integer;
begin
  Top := FFlowStateCount - 1;
  if (Top >= 0) and (FImplicitFlowMappingStates[Top] = imsInside) then
  begin
    FFlowMappingStarted[Top] := False;
    FImplicitFlowMappingStates[Top] := imsPossible;
    PushToken(ytkFlowMappingEnd, AMark, AMark);
  end;
end;

{ the least column of a line inside a flow collection: deeper than the block
  node that holds it (7.4, s-flow-line-prefix(n)) }
function TGocciaYAMLScanner.FlowContentIndent: Integer;
begin
  if (FIndentCount > 0) and not FIndents[FIndentCount - 1].NeedsBlockEnd then
    Result := FIndents[FIndentCount - 1].Indent + 1
  else
    Result := FIndent + 1;
end;

{ explicit keys at AColumn or deeper wait for no ":" any more }
procedure TGocciaYAMLScanner.DropExplicitKeysFrom(const AColumn: Integer);
begin
  while (FExplicitKeyCount > 0) and
        (FExplicitKeyColumns[FExplicitKeyCount - 1] >= AColumn) do
    Dec(FExplicitKeyCount);
end;

{ the blanks and comment after a block "?" or ":"; after a tab only a flow
  node may follow on the line, not a block collection (8.2.2, 8.2.3: a
  compact collection starts after s-indent spaces) }
procedure TGocciaYAMLScanner.SkipSeparationAfterIndicator(const AIndicator: Char;
  out ATabSeparated: Boolean);
var
  FoundTabs, FoundSpaces: Boolean;
begin
  SkipWhitespaceToEndOfLine(True, FoundTabs, FoundSpaces);
  ATabSeparated := FoundTabs and not NextIsBreakZ;
  if ATabSeparated and (Peek = '-') and IsBlankOrBreakZ(Peek(1)) then
    RaiseError(FMark, '''' + AIndicator + ''' must be followed by a valid YAML whitespace');
end;

function TGocciaYAMLScanner.NextToken: TGocciaYAMLToken;
begin
  if FStreamEndProduced then
    RaiseError(FMark, 'unexpected end of stream');
  if not FTokenAvailable then
    FetchMoreTokens;
  if FTokenCount = 0 then
    RaiseError(FMark, 'did not find expected next token');
  Result := TakeToken;
  FTokenAvailable := False;
  Inc(FTokensParsed);
  if Result.Kind = ytkStreamEnd then
    FStreamEndProduced := True;
end;

{ queue tokens until the first can be handed out: no simple key that is
  still possible starts at it }
procedure TGocciaYAMLScanner.FetchMoreTokens;
var
  I: Integer;
  NeedMore: Boolean;
begin
  while True do
  begin
    if FTokenCount = 0 then
      NeedMore := True
    else
    begin
      NeedMore := False;
      StaleSimpleKeys;
      for I := 0 to FSimpleKeyCount - 1 do
        if FSimpleKeys[I].Possible and
           (FSimpleKeys[I].TokenNumber = FTokensParsed) then
        begin
          NeedMore := True;
          Break;
        end;
    end;
    if not NeedMore then
      Break;
    FetchNextToken;
  end;
  FTokenAvailable := True;
end;

procedure TGocciaYAMLScanner.FetchNextToken;
var
  C, NextCharacter: Char;
begin
  if not FStreamStartProduced then
  begin
    FetchStreamStart;
    Exit;
  end;
  SkipToNextToken;
  StaleSimpleKeys;
  UnrollIndent(FMark.Column);

  if NextIsZ then
  begin
    if not AtEnd then
      RaiseError(FMark, 'found unprintable character');
    FetchStreamEnd;
    Exit;
  end;

  // only the parser knows whether a byte order mark that starts a line is
  // between documents; anywhere else it is inside one
  if Peek = CHARACTER_BYTE_ORDER_MARK then
  begin
    if (FMark.Column = 0) and (FFlowLevel = 0) then
    begin
      FetchByteOrderMark;
      Exit;
    end;
    RaiseError(FMark, 'a byte order mark must not appear inside a document');
  end;

  if FMark.Column = 0 then
  begin
    if Peek = '%' then
    begin
      FetchDirective;
      Exit;
    end;
    if NextIsDocumentStart then
    begin
      FetchDocumentIndicator(ytkDocumentStart);
      Exit;
    end;
    if NextIsDocumentEnd then
    begin
      FetchDocumentIndicator(ytkDocumentEnd);
      SkipWhitespaceToEndOfLine(True);
      if not NextIsBreakZ then
        RaiseError(FMark, 'invalid content after document end marker');
      Exit;
    end;
  end;

  C := Peek;
  NextCharacter := Peek(1);
  // a closing "]" or "}" may sit at the indentation of the block node that
  // holds the collection, as libyaml, PyYAML, js-yaml and the yaml package
  // accept though 8.2.3 does not
  if (FFlowLevel > 0) and (FMark.Column < FlowContentIndent) and
     not (((C = ']') or (C = '}')) and (FMark.Column = FlowContentIndent - 1)) then
    RaiseError(FMark, 'invalid indentation in flow construct');
  if (FMark.Column < FIndent) and not ((FFlowLevel > 0) and ((C = ']') or (C = '}'))) then
    RaiseError(FMark, 'invalid indentation');

  case C of
    '[':
      FetchFlowCollectionStart(ytkFlowSequenceStart);
    '{':
      FetchFlowCollectionStart(ytkFlowMappingStart);
    ']':
      FetchFlowCollectionEnd(ytkFlowSequenceEnd);
    '}':
      FetchFlowCollectionEnd(ytkFlowMappingEnd);
    ',':
      FetchFlowEntry;
    '*':
      FetchAnchor(True);
    '&':
      FetchAnchor(False);
    '!':
      FetchTag;
    '''':
      FetchFlowScalar(True);
    '"':
      FetchFlowScalar(False);
    '%', '@', '`':
      RaiseError(FMark, 'unexpected character: `' + C + '''');
  else
    if (C = '-') and IsBlankOrBreakZ(NextCharacter) then
      FetchBlockEntry
    else if (C = '?') and IsBlankOrBreakZ(NextCharacter) then
      FetchKey
    else if (C = ':') and IsBlankOrBreakZ(NextCharacter) then
      FetchValue
    else if (C = ':') and (FFlowLevel > 0) and
            (IsFlowCharacter(NextCharacter) or
             (FMark.Index = FAdjacentValueAllowedAt)) then
      FetchFlowValue
    else if (C = '|') and (FFlowLevel = 0) then
      FetchBlockScalar(True)
    else if (C = '>') and (FFlowLevel = 0) then
      FetchBlockScalar(False)
    // "|" and ">" are indicators, which cannot start a plain scalar (7.3.3)
    else if (C = '|') or (C = '>') then
      RaiseError(FMark, 'a plain scalar cannot start with ''' + C + '''')
    else
      FetchPlainScalar;
  end;
end;

procedure TGocciaYAMLScanner.FetchStreamStart;
begin
  FIndent := -1;
  FStreamStartProduced := True;
  FSimpleKeyAllowed := True;
  PushToken(ytkStreamStart, FMark, FMark);
  PushSimpleKey;
end;

procedure TGocciaYAMLScanner.FetchStreamEnd;
var
  I: Integer;
begin
  if FMark.Column <> 0 then
  begin
    FMark.Column := 0;
    Inc(FMark.Line);
  end;
  // no more context comes: no simple key is possible any more
  for I := 0 to FSimpleKeyCount - 1 do
  begin
    if FSimpleKeys[I].Required and FSimpleKeys[I].Possible then
      RaiseError(FMark, 'simple key expected');
    FSimpleKeys[I].Possible := False;
  end;
  UnrollIndent(-1);
  RemoveSimpleKey;
  FSimpleKeyAllowed := False;
  PushToken(ytkStreamEnd, FMark, FMark);
end;

procedure TGocciaYAMLScanner.FetchDirective;
begin
  UnrollIndent(-1);
  RemoveSimpleKey;
  FSimpleKeyAllowed := False;
  QueueToken(ScanDirective);
end;

function TGocciaYAMLScanner.ScanDirective: TGocciaYAMLToken;
var
  Name: string;
  StartMark: TGocciaYAMLMarker;
begin
  StartMark := FMark;
  SkipNonBlank;
  Name := ScanDirectiveName;
  if Name = 'YAML' then
  begin
    SkipBlanks;
    Result := NewToken(ytkVersionDirective, StartMark, StartMark);
    Result.MajorVersion := ScanVersionDirectiveNumber(StartMark);
    if Peek <> '.' then
      RaiseError(StartMark,
        'while scanning a YAML directive, did not find expected digit or ''.'' character');
    SkipNonBlank;
    Result.MinorVersion := ScanVersionDirectiveNumber(StartMark);
    Result.EndMark := FMark;
  end
  else if Name = 'TAG' then
  begin
    SkipBlanks;
    Result := NewToken(ytkTagDirective, StartMark, StartMark);
    Result.Handle := ScanTagHandle(True, StartMark);
    if not NextIsBlank then
      RaiseError(StartMark,
        'while scanning TAG, did not find expected whitespace after the handle');
    SkipBlanks;
    Result.Value := ScanTagPrefix(StartMark);
    if not NextIsBlankOrBreakZ then
      RaiseError(StartMark,
        'while scanning TAG, did not find expected whitespace or line break');
    Result.EndMark := FMark;
  end
  else
  begin
    // a reserved directive: its parameters are ignored
    while NextIsBlank do
    begin
      SkipBlanks;
      while IsYAMLNonSpace(Peek) do
        SkipNonBlank;
    end;
    Result := NewToken(ytkReservedDirective, StartMark, FMark);
    Result.Value := Name;
  end;

  SkipWhitespaceToEndOfLine(True);
  if not NextIsBreakZ then
    RaiseError(StartMark,
      'while scanning a directive, did not find expected comment or line break');
  SkipLinebreak;
end;

function TGocciaYAMLScanner.ScanDirectiveName: string;
var
  Buffer: TStringBuffer;
  StartMark: TGocciaYAMLMarker;
begin
  StartMark := FMark;
  Buffer := TStringBuffer.Create;
  while IsYAMLNonSpace(Peek) do
  begin
    Buffer.AppendChar(Peek);
    SkipNonBlank;
  end;
  Result := Buffer.ToString;
  if Result = '' then
    RaiseError(StartMark,
      'while scanning a directive, could not find expected directive name');
  if not NextIsBlankOrBreakZ then
    RaiseError(StartMark,
      'while scanning a directive, found unexpected non-alphabetical character');
end;

function TGocciaYAMLScanner.ScanVersionDirectiveNumber(
  const AMark: TGocciaYAMLMarker): Integer;
var
  DigitCount: Integer;
begin
  Result := 0;
  DigitCount := 0;
  while IsDigitCharacter(Peek) do
  begin
    if DigitCount + 1 > MAX_VERSION_DIGITS then
      RaiseError(AMark,
        'while scanning a YAML directive, found extremely long version number');
    Inc(DigitCount);
    Result := Result * 10 + Ord(Peek) - Ord('0');
    SkipNonBlank;
  end;
  if DigitCount = 0 then
    RaiseError(AMark,
      'while scanning a YAML directive, did not find expected version number');
end;

procedure TGocciaYAMLScanner.FetchTag;
begin
  SaveSimpleKey;
  FSimpleKeyAllowed := False;
  QueueToken(ScanTag);
end;

function TGocciaYAMLScanner.ScanTag: TGocciaYAMLToken;
var
  Handle, Suffix: string;
  StartMark: TGocciaYAMLMarker;
begin
  StartMark := FMark;
  Handle := '';
  if Peek(1) = '<' then
    Suffix := ScanVerbatimTag(StartMark)
  else
  begin
    // "!suffix", "!handle!suffix" or "!!suffix"
    Handle := ScanTagHandle(False, StartMark);
    if (Length(Handle) >= 2) and (Handle[1] = '!') and
       (Handle[Length(Handle)] = '!') then
      Suffix := ScanTagShorthandSuffix('', StartMark)
    else
    begin
      Suffix := ScanTagShorthandSuffix(Handle, StartMark);
      Handle := '!';
      // the non-specific tag "!"
      if Suffix = '' then
      begin
        Handle := '';
        Suffix := '!';
      end;
    end;
  end;

  if not (IsBlankOrBreakZ(Peek) or ((FFlowLevel > 0) and NextIsFlow)) then
    RaiseError(StartMark,
      'while scanning a tag, did not find expected whitespace or line break');
  Result := NewToken(ytkTag, StartMark, FMark);
  Result.Handle := Handle;
  Result.Value := Suffix;
end;

function TGocciaYAMLScanner.ScanTagHandle(const ADirective: Boolean;
  const AMark: TGocciaYAMLMarker): string;
var
  Buffer: TStringBuffer;
begin
  if Peek <> '!' then
    RaiseError(AMark, 'while scanning a tag, did not find expected ''!''');
  Buffer := TStringBuffer.Create;
  Buffer.AppendChar('!');
  SkipNonBlank;
  while IsAlphaCharacter(Peek) do
  begin
    Buffer.AppendChar(Peek);
    SkipNonBlank;
  end;
  if Peek = '!' then
  begin
    Buffer.AppendChar('!');
    SkipNonBlank;
  end
  else if ADirective and (Buffer.ToString <> '!') then
    // a %TAG handle is "!", "!!" or "!name!"
    RaiseError(AMark, 'while parsing a tag directive, did not find expected ''!''');
  Result := Buffer.ToString;
end;

{ a %TAG prefix (6.8.2.2): a local "!..." or a global one }
function TGocciaYAMLScanner.ScanTagPrefix(const AMark: TGocciaYAMLMarker): string;
var
  Buffer: TStringBuffer;
begin
  Buffer := TStringBuffer.Create;
  if Peek = '!' then
  begin
    Buffer.AppendChar('!');
    SkipNonBlank;
  end
  else if not IsTagCharacter(Peek) then
    RaiseError(AMark, 'invalid global tag character')
  else if Peek = '%' then
    Buffer.Append(ScanURIEscapes(AMark))
  else
  begin
    Buffer.AppendChar(Peek);
    SkipNonBlank;
  end;
  while IsURICharacter(Peek) do
    if Peek = '%' then
      Buffer.Append(ScanURIEscapes(AMark))
    else
    begin
      Buffer.AppendChar(Peek);
      SkipNonBlank;
    end;
  Result := Buffer.ToString;
end;

{ "!<uri>"; the "!<" is next }
function TGocciaYAMLScanner.ScanVerbatimTag(const AMark: TGocciaYAMLMarker): string;
var
  Buffer: TStringBuffer;
begin
  SkipNonBlanks(2);
  Buffer := TStringBuffer.Create;
  while IsURICharacter(Peek) do
    if Peek = '%' then
      Buffer.Append(ScanURIEscapes(AMark))
    else
    begin
      Buffer.AppendChar(Peek);
      SkipNonBlank;
    end;
  if Peek <> '>' then
    RaiseError(AMark,
      'while scanning a verbatim tag, did not find the expected ''>''');
  SkipNonBlank;
  Result := Buffer.ToString;
end;

{ a tag's suffix: AHead, without its "!", and the tag characters after it }
function TGocciaYAMLScanner.ScanTagShorthandSuffix(const AHead: string;
  const AMark: TGocciaYAMLMarker): string;
var
  Buffer: TStringBuffer;
  SuffixLength: Integer;
begin
  SuffixLength := Length(AHead);
  Buffer := TStringBuffer.Create;
  if SuffixLength > 1 then
    Buffer.Append(Copy(AHead, 2, MaxInt));
  while IsTagCharacter(Peek) do
  begin
    if Peek = '%' then
      Buffer.Append(ScanURIEscapes(AMark))
    else
    begin
      Buffer.AppendChar(Peek);
      SkipNonBlank;
    end;
    Inc(SuffixLength);
  end;
  if SuffixLength = 0 then
    RaiseError(AMark, 'while parsing a tag, did not find expected tag URI');
  Result := Buffer.ToString;
end;

{ "%xx" escapes that spell one UTF-8 character }
function TGocciaYAMLScanner.ScanURIEscapes(const AMark: TGocciaYAMLMarker): string;
const
  UTF8_TWO_BYTE_LEAD_MASK = $E0;
  UTF8_TWO_BYTE_LEAD = $C0;
  UTF8_THREE_BYTE_LEAD_MASK = $F0;
  UTF8_THREE_BYTE_LEAD = $E0;
  UTF8_FOUR_BYTE_LEAD_MASK = $F8;
  UTF8_FOUR_BYTE_LEAD = $F0;
  UTF8_CONTINUATION_MASK = $C0;
  UTF8_CONTINUATION = $80;
  UTF8_PAYLOAD_BITS = 6;
  UTF8_CONTINUATION_PAYLOAD_MASK = $3F;
var
  ByteValue, Width: Integer;
  CodePoint: Cardinal;
begin
  Width := 0;
  CodePoint := 0;
  repeat
    if not ((Peek = '%') and IsHexCharacter(Peek(1)) and IsHexCharacter(Peek(2))) then
      RaiseError(AMark, 'while parsing a tag, found an invalid escape sequence');
    ByteValue := HexValue(Peek(1)) * 16 + HexValue(Peek(2));
    if Width = 0 then
    begin
      if ByteValue and UTF8_CONTINUATION = 0 then
      begin
        Width := 1;
        CodePoint := ByteValue;
      end
      else if ByteValue and UTF8_TWO_BYTE_LEAD_MASK = UTF8_TWO_BYTE_LEAD then
      begin
        Width := 2;
        CodePoint := ByteValue and not UTF8_TWO_BYTE_LEAD_MASK;
      end
      else if ByteValue and UTF8_THREE_BYTE_LEAD_MASK = UTF8_THREE_BYTE_LEAD then
      begin
        Width := 3;
        CodePoint := ByteValue and not UTF8_THREE_BYTE_LEAD_MASK;
      end
      else if ByteValue and UTF8_FOUR_BYTE_LEAD_MASK = UTF8_FOUR_BYTE_LEAD then
      begin
        Width := 4;
        CodePoint := ByteValue and not UTF8_FOUR_BYTE_LEAD_MASK;
      end
      else
        RaiseError(AMark, 'while parsing a tag, found an incorrect leading UTF-8 byte');
    end
    else
    begin
      if ByteValue and UTF8_CONTINUATION_MASK <> UTF8_CONTINUATION then
        RaiseError(AMark, 'while parsing a tag, found an incorrect trailing UTF-8 byte');
      CodePoint := (CodePoint shl UTF8_PAYLOAD_BITS) or
        Cardinal(ByteValue and UTF8_CONTINUATION_PAYLOAD_MASK);
    end;
    SkipNonBlanks(URI_ESCAPE_LENGTH);
    Dec(Width);
  until Width = 0;
  if (CodePoint > MAX_CODE_POINT) or
     ((CodePoint >= FIRST_SURROGATE) and (CodePoint <= LAST_SURROGATE)) then
    RaiseError(AMark, 'while parsing a tag, found an invalid UTF-8 codepoint');
  Result := CodePointText(CodePoint);
end;

procedure TGocciaYAMLScanner.FetchAnchor(const AAlias: Boolean);
begin
  SaveSimpleKey;
  FSimpleKeyAllowed := False;
  QueueToken(ScanAnchor(AAlias));
end;

function TGocciaYAMLScanner.ScanAnchor(const AAlias: Boolean): TGocciaYAMLToken;
var
  Buffer: TStringBuffer;
  StartMark: TGocciaYAMLMarker;
begin
  StartMark := FMark;
  SkipNonBlank;
  Buffer := TStringBuffer.Create;
  while IsAnchorCharacter(Peek) do
  begin
    Buffer.AppendChar(Peek);
    SkipNonBlank;
  end;
  if Buffer.Length = 0 then
    RaiseError(StartMark,
      'while scanning an anchor or alias, did not find expected alphabetic or numeric character');
  if AAlias then
    Result := NewToken(ytkAlias, StartMark, FMark)
  else
    Result := NewToken(ytkAnchor, StartMark, FMark);
  Result.Value := Buffer.ToString;
end;

procedure TGocciaYAMLScanner.FetchFlowCollectionStart(const AKind: TGocciaYAMLTokenKind);
var
  StartMark: TGocciaYAMLMarker;
  Top: Integer;
begin
  // "[" and "{" may start a simple key
  SaveSimpleKey;
  RollOneColumnIndent;
  IncreaseFlowLevel;
  FSimpleKeyAllowed := True;
  StartMark := FMark;
  SkipNonBlank;

  if FFlowStateCount = Length(FFlowMappingStarted) then
  begin
    SetLength(FFlowMappingStarted, FFlowStateCount * 2 + 4);
    SetLength(FImplicitFlowMappingStates, FFlowStateCount * 2 + 4);
  end;
  Top := FFlowStateCount;
  Inc(FFlowStateCount);
  FFlowMappingStarted[Top] := AKind = ytkFlowMappingStart;
  if AKind = ytkFlowMappingStart then
    FImplicitFlowMappingStates[Top] := imsNone
  else
    FImplicitFlowMappingStates[Top] := imsPossible;

  SkipWhitespaceToEndOfLine(True);
  PushToken(AKind, StartMark, FMark);
end;

procedure TGocciaYAMLScanner.FetchFlowCollectionEnd(const AKind: TGocciaYAMLTokenKind);
var
  StartMark: TGocciaYAMLMarker;
begin
  if FFlowLevel = 0 then
    RaiseError(FMark, 'misplaced bracket');
  RemoveSimpleKey;
  DecreaseFlowLevel;
  FSimpleKeyAllowed := False;
  if AKind = ytkFlowSequenceEnd then
    EndImplicitMapping(FMark);
  // each collection has its own state, so a nested mapping's "," or ":"
  // cannot end an outer sequence's implicit pair
  if FFlowStateCount > 0 then
    Dec(FFlowStateCount);

  StartMark := FMark;
  SkipNonBlank;
  SkipWhitespaceToEndOfLine(True);
  // a flow collection inside a flow collection may be a key, its value right
  // after the ":" ([ {a: b}:value ])
  if FFlowLevel > 0 then
    FAdjacentValueAllowedAt := FMark.Index;
  PushToken(AKind, StartMark, FMark);
end;

procedure TGocciaYAMLScanner.FetchFlowEntry;
var
  StartMark: TGocciaYAMLMarker;
begin
  RemoveSimpleKey;
  FSimpleKeyAllowed := True;
  EndImplicitMapping(FMark);
  StartMark := FMark;
  SkipNonBlank;
  SkipWhitespaceToEndOfLine(True);
  PushToken(ytkFlowEntry, StartMark, FMark);
end;

{ "-": a block sequence starts when it is deeper than the current level }
procedure TGocciaYAMLScanner.FetchBlockEntry;
var
  EntryMark: TGocciaYAMLMarker;
  FoundTabs, FoundSpaces: Boolean;
  LastToken: TGocciaYAMLToken;
begin
  if FFlowLevel > 0 then
    RaiseError(FMark, '"-" is only valid inside a block');
  if not FSimpleKeyAllowed then
    RaiseError(FMark, 'block sequence entries are not allowed in this context');

  // a node property alone on a column-0 line cannot own a sequence of
  // entries at column 0 inside a block (G9HC)
  if FTokenCount > 0 then
  begin
    LastToken := FTokenBuffer[FTokenHead + FTokenCount - 1];
    if (LastToken.Kind in [ytkAnchor, ytkTag]) and (FMark.Column = 0) and
       (LastToken.StartMark.Column = 0) and (FIndent > -1) then
      RaiseError(LastToken.StartMark, 'invalid indentation for anchor');
  end;

  EntryMark := FMark;
  SkipNonBlank;
  RollIndent(EntryMark.Column, -1, ytkBlockSequenceStart, EntryMark);
  SkipWhitespaceToEndOfLine(True, FoundTabs, FoundSpaces);
  if FoundTabs and (Peek = '-') and IsBlankOrBreakZ(Peek(1)) then
    RaiseError(FMark, '''-'' must be followed by a valid YAML whitespace');
  SkipWhitespaceToEndOfLine(False);
  if NextIsBreak or NextIsFlow then
    RollOneColumnIndent;

  RemoveSimpleKey;
  FSimpleKeyAllowed := True;
  PushToken(ytkBlockEntry, FMark, FMark);
end;

procedure TGocciaYAMLScanner.FetchByteOrderMark;
var
  StartMark: TGocciaYAMLMarker;
begin
  UnrollIndent(-1);
  RemoveSimpleKey;
  // a byte order mark is not content: the column stays where it was
  StartMark := FMark;
  Inc(FPosition);
  Inc(FMark.Index);
  FSimpleKeyAllowed := True;
  PushToken(ytkByteOrderMark, StartMark, FMark);
end;

procedure TGocciaYAMLScanner.FetchDocumentIndicator(const AKind: TGocciaYAMLTokenKind);
var
  StartMark: TGocciaYAMLMarker;
begin
  UnrollIndent(-1);
  RemoveSimpleKey;
  FSimpleKeyAllowed := False;
  StartMark := FMark;
  SkipNonBlanks(DOCUMENT_INDICATOR_LENGTH);
  PushToken(AKind, StartMark, FMark);
end;

procedure TGocciaYAMLScanner.FetchBlockScalar(const ALiteral: Boolean);
begin
  SaveSimpleKey;
  FSimpleKeyAllowed := True;
  QueueToken(ScanBlockScalar(ALiteral));
end;

{ a literal ("|") or folded (">") block scalar (8.1) }
function TGocciaYAMLScanner.ScanBlockScalar(const ALiteral: Boolean): TGocciaYAMLToken;
type
  TChomping = (chompClip, chompKeep, chompStrip);
var
  Chomping: TChomping;
  Content: TStringBuffer;
  Increment, Indent: Integer;
  LeadingBlank, TrailingBlank: Boolean;
  LeadingBreak, TrailingBreaks, Text: string;
  StartMark: TGocciaYAMLMarker;
begin
  StartMark := FMark;
  Chomping := chompClip;
  Increment := 0;
  Indent := 0;
  LeadingBlank := False;
  Content := TStringBuffer.Create;
  LeadingBreak := '';
  TrailingBreaks := '';

  SkipNonBlank;
  UnrollNonBlockIndents;

  // the header: a chomping indicator and an indentation indicator, in
  // either order
  if (Peek = '+') or (Peek = '-') then
  begin
    if Peek = '+' then
      Chomping := chompKeep
    else
      Chomping := chompStrip;
    SkipNonBlank;
    if IsDigitCharacter(Peek) then
    begin
      if Peek = '0' then
        RaiseError(StartMark,
          'while scanning a block scalar, found an indentation indicator equal to 0');
      Increment := Ord(Peek) - Ord('0');
      SkipNonBlank;
    end;
  end
  else if IsDigitCharacter(Peek) then
  begin
    if Peek = '0' then
      RaiseError(StartMark,
        'while scanning a block scalar, found an indentation indicator equal to 0');
    Increment := Ord(Peek) - Ord('0');
    SkipNonBlank;
    if (Peek = '+') or (Peek = '-') then
    begin
      if Peek = '+' then
        Chomping := chompKeep
      else
        Chomping := chompStrip;
      SkipNonBlank;
    end;
  end;

  SkipWhitespaceToEndOfLine(True);
  if not NextIsBreakZ then
    RaiseError(StartMark,
      'while scanning a block scalar, did not find expected comment or line break');
  if NextIsBreak then
    SkipBreak;
  if Peek = #9 then
    RaiseError(StartMark, 'a block scalar content cannot start with a tab');

  if Increment > 0 then
  begin
    if FIndent >= 0 then
      Indent := FIndent + Increment
    else
      Indent := Increment;
  end;

  // the leading empty lines, and the indentation when no indicator gave it
  if Indent = 0 then
    SkipBlockScalarFirstLineIndent(Indent, TrailingBreaks)
  else
    SkipBlockScalarIndent(Indent, TrailingBreaks);

  // the text ends with no content line
  if NextIsZ then
  begin
    case Chomping of
      chompKeep:
        if (TrailingBreaks = '') and (FMark.Line > StartMark.Line) and
           (FMark.Column > 0) then
          Text := #10
        else
          Text := TrailingBreaks;
    else
      // the break ending the header line is not content
      Text := '';
    end;
    Result := NewToken(ytkScalar, StartMark, FMark);
    if ALiteral then
      Result.Style := yssLiteral
    else
      Result.Style := yssFolded;
    Result.Value := Text;
    Exit;
  end;

  if (FMark.Column < Indent) and (FMark.Column > FIndent) then
    RaiseError(FMark, 'wrongly indented line in block scalar');

  while (FMark.Column = Indent) and IsYAMLNonBreak(Peek) do
  begin
    // a document marker ends the scalar, whether it starts a document or not
    if (Indent = 0) and NextIsDocumentIndicator then
      Break;

    // the first content character of a content line
    TrailingBlank := NextIsBlank;
    if not ALiteral and (LeadingBreak <> '') and not LeadingBlank and
       not TrailingBlank then
    begin
      Content.Append(TrailingBreaks);
      if TrailingBreaks = '' then
        Content.AppendChar(' ');
    end
    else
    begin
      Content.Append(LeadingBreak);
      Content.Append(TrailingBreaks);
    end;
    LeadingBreak := '';
    TrailingBreaks := '';
    LeadingBlank := NextIsBlank;

    while IsYAMLNonBreak(Peek) do
    begin
      Content.AppendChar(Peek);
      SkipBlank;
    end;

    if not NextIsBreak then
    begin
      // the end of the text ends the line as a line break would
      LeadingBreak := LeadingBreak + #10;
      Break;
    end;
    ReadBreak(LeadingBreak);
    SkipBlockScalarIndent(Indent, TrailingBreaks);
    // a last line of only the indentation, ended by the end of the text, is
    // an empty line
    if NextIsZ and (FMark.Column >= Indent) and (FMark.Column >= 1) then
      TrailingBreaks := TrailingBreaks + #10;
  end;

  // chomp the tail
  if Chomping <> chompStrip then
    Content.Append(LeadingBreak);
  if Chomping = chompKeep then
    Content.Append(TrailingBreaks);

  Result := NewToken(ytkScalar, StartMark, FMark);
  if ALiteral then
    Result.Style := yssLiteral
  else
    Result.Style := yssFolded;
  Result.Value := Content.ToString;
end;

{ the indentation and the empty lines before a content line }
procedure TGocciaYAMLScanner.SkipBlockScalarIndent(const AIndent: Integer;
  var ABreaks: string);
begin
  while True do
  begin
    // tabs are not indentation
    while (FMark.Column < AIndent) and (Peek = ' ') do
      SkipBlank;
    if NextIsBreak then
      ReadBreak(ABreaks)
    else
      Break;
  end;
end;

{ the indentation of the first content line, the deepest of the empty lines
  before it if deeper }
procedure TGocciaYAMLScanner.SkipBlockScalarFirstLineIndent(var AIndent: Integer;
  var ABreaks: string);
var
  MaxIndent: Integer;
begin
  MaxIndent := 0;
  while True do
  begin
    while Peek = ' ' do
      SkipBlank;
    if FMark.Column > MaxIndent then
      MaxIndent := FMark.Column;
    if NextIsBreak then
      ReadBreak(ABreaks)
    else
      Break;
  end;
  // at the top level the content may start at column 0; inside a block it
  // is indented
  if MaxIndent > FIndent + 1 then
    AIndent := MaxIndent
  else
    AIndent := FIndent + 1;
  if (FIndent > 0) and (AIndent < 1) then
    AIndent := 1;
end;

procedure TGocciaYAMLScanner.FetchFlowScalar(const ASingle: Boolean);
var
  Token: TGocciaYAMLToken;
begin
  SaveSimpleKey;
  FSimpleKeyAllowed := False;
  Token := ScanFlowScalar(ASingle);
  // a JSON-like key in a flow mapping may have its value right after the
  // ":" (7.4.2)
  SkipToNextToken;
  FAdjacentValueAllowedAt := FMark.Index;
  QueueToken(Token);
end;

{ a single-quoted or double-quoted scalar (7.3) }
function TGocciaYAMLScanner.ScanFlowScalar(const ASingle: Boolean): TGocciaYAMLToken;
var
  Content: TStringBuffer;
  LeadingBlanks: Boolean;
  LeadingBreak, TrailingBreaks, Whitespaces: string;
  StartMark: TGocciaYAMLMarker;
begin
  StartMark := FMark;
  Content := TStringBuffer.Create;
  LeadingBreak := '';
  TrailingBreaks := '';
  Whitespaces := '';

  // the opening quote
  SkipNonBlank;
  while True do
  begin
    if (FMark.Column = 0) and NextIsDocumentIndicator then
      RaiseError(StartMark,
        'while scanning a quoted scalar, found unexpected document indicator');
    if NextIsZ then
      RaiseError(StartMark,
        'while scanning a quoted scalar, found unexpected end of stream');
    if (FMark.Column < FIndent) or
       ((FFlowLevel > 0) and (FMark.Column < FlowContentIndent)) then
      RaiseError(StartMark, 'invalid indentation in quoted scalar');

    LeadingBlanks := False;
    // the characters up to whitespace or the closing quote, escapes
    // resolved
    while not IsBlankOrBreakZ(Peek) do
    begin
      if ASingle and (Peek = '''') and (Peek(1) = '''') then
      begin
        Content.AppendChar('''');
        SkipNonBlanks(2);
      end
      else if ASingle and (Peek = '''') then
        Break
      else if not ASingle and (Peek = '"') then
        Break
      else if not ASingle and (Peek = '\') and IsBreak(Peek(1)) then
      begin
        // an escaped line break joins the lines without a space
        SkipNonBlank;
        SkipLinebreak;
        LeadingBlanks := True;
        Break;
      end
      else if not ASingle and (Peek = '\') then
        Content.Append(ScanEscapeSequence(StartMark))
      else
      begin
        Content.AppendChar(Peek);
        SkipNonBlank;
      end;
    end;

    if (ASingle and (Peek = '''')) or (not ASingle and (Peek = '"')) then
      Break;

    while NextIsBlank or NextIsBreak do
    begin
      if NextIsBlank then
      begin
        if LeadingBlanks then
        begin
          if (Peek = #9) and (FMark.Column < FIndent) then
            RaiseError(FMark, 'tab cannot be used as indentation');
          SkipBlank;
        end
        else
        begin
          Whitespaces := Whitespaces + Peek;
          SkipBlank;
        end;
      end
      else if LeadingBlanks then
        ReadBreak(TrailingBreaks)
      else
      begin
        Whitespaces := '';
        ReadBreak(LeadingBreak);
        LeadingBlanks := True;
      end;
    end;

    // fold the line breaks, or keep the whitespace between words
    if LeadingBlanks then
    begin
      if LeadingBreak = '' then
      begin
        Content.Append(TrailingBreaks);
        TrailingBreaks := '';
      end
      else
      begin
        if TrailingBreaks = '' then
          Content.AppendChar(' ')
        else
        begin
          Content.Append(TrailingBreaks);
          TrailingBreaks := '';
        end;
        LeadingBreak := '';
      end;
    end
    else
    begin
      Content.Append(Whitespaces);
      Whitespaces := '';
    end;
  end;

  // the closing quote, then nothing but a comment, a flow indicator or a
  // ":" may follow on the line
  SkipNonBlank;
  SkipWhitespaceToEndOfLine(True);
  if not (((FFlowLevel > 0) and ((Peek = ',') or (Peek = '}') or (Peek = ']'))) or
          NextIsBreakZ or
          ((Peek = ':') and (FFlowLevel = 0) and (StartMark.Line = FMark.Line)) or
          ((Peek = ':') and (FFlowLevel > 0))) then
    RaiseError(FMark, 'invalid trailing content after double-quoted scalar');

  Result := NewToken(ytkScalar, StartMark, FMark);
  if ASingle then
    Result.Style := yssSingleQuoted
  else
    Result.Style := yssDoubleQuoted;
  Result.Value := Content.ToString;
end;

{ the escape sequence a "\" that is next starts (5.7) }
function TGocciaYAMLScanner.ScanEscapeSequence(const AStartMark: TGocciaYAMLMarker): string;
var
  CodePoint: Cardinal;
  DigitCount, I: Integer;
begin
  DigitCount := 0;
  case Peek(1) of
    '0':
      Result := #0;
    'a':
      Result := #7;
    'b':
      Result := #8;
    't', #9:
      Result := #9;
    'n':
      Result := #10;
    'v':
      Result := #11;
    'f':
      Result := #12;
    'r':
      Result := #13;
    'e':
      Result := #27;
    ' ':
      Result := ' ';
    '"':
      Result := '"';
    '/':
      Result := '/';
    '\':
      Result := '\';
    'N':
      Result := #$85;
    '_':
      Result := #$A0;
    'L':
      Result := #$2028;
    'P':
      Result := #$2029;
    'x':
      DigitCount := ESCAPE_HEX_DIGITS_X;
    'u':
      DigitCount := ESCAPE_HEX_DIGITS_U;
    'U':
      DigitCount := ESCAPE_HEX_DIGITS_LONG_U;
  else
    RaiseError(AStartMark,
      'while parsing a quoted scalar, found unknown escape character');
  end;
  SkipNonBlanks(2);
  if DigitCount = 0 then
    Exit;

  CodePoint := 0;
  for I := 0 to DigitCount - 1 do
  begin
    if not IsHexCharacter(Peek(I)) then
      RaiseError(AStartMark,
        'while parsing a quoted scalar, did not find expected hexadecimal number');
    if CodePoint > MAX_CODE_POINT then
      Break;
    CodePoint := CodePoint * 16 + Cardinal(HexValue(Peek(I)));
  end;
  // a "\u" surrogate stands for itself, so a JSON-style pair spells its
  // character
  if (CodePoint > MAX_CODE_POINT) or
     ((DigitCount <> ESCAPE_HEX_DIGITS_U) and (CodePoint >= FIRST_SURROGATE) and
      (CodePoint <= LAST_SURROGATE)) then
    RaiseError(AStartMark,
      'while parsing a quoted scalar, found invalid Unicode character escape code');
  Result := CodePointText(CodePoint);
  SkipNonBlanks(DigitCount);
end;

procedure TGocciaYAMLScanner.FetchPlainScalar;
begin
  SaveSimpleKey;
  FSimpleKeyAllowed := False;
  QueueToken(ScanPlainScalar);
end;

{ a plain scalar (7.3.3), on several lines where the context allows }
function TGocciaYAMLScanner.ScanPlainScalar: TGocciaYAMLToken;
var
  Content: TStringBuffer;
  EndMark, StartMark: TGocciaYAMLMarker;
  Indent: Integer;
  EndsAtTabLine, InFlow: Boolean;
begin
  UnrollNonBlockIndents;
  Indent := FIndent + 1;
  StartMark := FMark;
  InFlow := FFlowLevel > 0;
  if InFlow and (StartMark.Column < Indent) then
    RaiseError(StartMark, 'invalid indentation in flow construct');

  Content := TStringBuffer.Create;
  FPlainWhitespaces := '';
  FPlainLeadingBreak := '';
  FPlainTrailingBreaks := '';
  EndMark := FMark;

  while True do
  begin
    if ((FMark.Column = 0) and NextIsDocumentIndicator) or (Peek = '#') then
    begin
      // a comment after content and whitespace ends the scalar; a block
      // line right after it that would continue it is an error (BS4K)
      if (Peek = '#') and (Content.Length > 0) and (FPlainWhitespaces <> '') and
         not InFlow then
      begin
        FPlainInterruptedByComment := True;
        FPlainInterruptionMark := FMark;
      end;
      Break;
    end;

    if InFlow and (Content.Length = 0) and (Peek = '-') and
       IsFlowCharacter(Peek(1)) then
      RaiseError(FMark, 'plain scalar cannot start with ''-'' followed by ,[]{}');

    if not NextIsBlankOrBreakZ and NextCanBePlainScalar(InFlow) then
    begin
      if FLeadingWhitespace then
      begin
        if FPlainLeadingBreak = '' then
        begin
          Content.Append(FPlainTrailingBreaks);
          FPlainTrailingBreaks := '';
        end
        else
        begin
          if FPlainTrailingBreaks = '' then
            Content.AppendChar(' ')
          else
          begin
            Content.Append(FPlainTrailingBreaks);
            FPlainTrailingBreaks := '';
          end;
          FPlainLeadingBreak := '';
        end;
        FLeadingWhitespace := False;
      end
      else if FPlainWhitespaces <> '' then
      begin
        Content.Append(FPlainWhitespaces);
        FPlainWhitespaces := '';
      end;

      Content.AppendChar(Peek);
      SkipNonBlank;
      while not NextIsBlankOrBreakZ and NextCanBePlainScalar(InFlow) do
      begin
        Content.AppendChar(Peek);
        SkipNonBlank;
      end;
      EndMark := FMark;
    end;

    // the scalar ends at the end of the text, ": " or a flow indicator in
    // a flow collection
    if not (NextIsBlank or NextIsBreak) then
      Break;

    EndsAtTabLine := False;
    while NextIsBlankOrBreak do
    begin
      if NextIsBlank then
      begin
        if not FLeadingWhitespace then
        begin
          FPlainWhitespaces := FPlainWhitespaces + Peek;
          SkipBlank;
        end
        else if (FMark.Column < Indent) and (Peek = #9) and InFlow then
        begin
          // a line that has a tab where a flow collection's indentation
          // goes is no empty line of the scalar (l-empty): the scalar ends
          EndsAtTabLine := True;
          Break;
        end
        else if (FMark.Column < Indent) and (Peek = #9) then
        begin
          // a tab in the indentation is allowed only on an empty line
          SkipWhitespaceToEndOfLine(True);
          if not NextIsBreakZ then
            RaiseError(StartMark, 'while scanning a plain scalar, found a tab');
        end
        else
          SkipBlank;
      end
      else if FLeadingWhitespace then
      begin
        SkipBreak;
        FPlainTrailingBreaks := FPlainTrailingBreaks + #10;
      end
      else
      begin
        FPlainWhitespaces := '';
        SkipBreak;
        FPlainLeadingBreak := FPlainLeadingBreak + #10;
        FLeadingWhitespace := True;
      end;
    end;

    // a line indented no further than the parent ends the scalar; in a flow
    // collection the token there is then an indentation error
    if EndsAtTabLine or (FMark.Column < Indent) then
      Break;
  end;

  if FLeadingWhitespace then
    FSimpleKeyAllowed := True;
  // an empty plain scalar happens only on invalid text such as "{..."
  if Content.Length = 0 then
    RaiseError(StartMark, 'unexpected end of plain scalar');
  Result := NewToken(ytkScalar, StartMark, EndMark);
  Result.Style := yssPlain;
  Result.Value := Content.ToString;
end;

{ "?": an explicit key }
procedure TGocciaYAMLScanner.FetchKey;
var
  StartMark: TGocciaYAMLMarker;
  TabSeparated: Boolean;
begin
  StartMark := FMark;
  if FFlowLevel = 0 then
  begin
    if not FSimpleKeyAllowed then
      RaiseError(FMark, 'mapping keys are not allowed in this context');
    RollIndent(StartMark.Column, -1, ytkBlockMappingStart, StartMark);
    if FExplicitKeyCount = Length(FExplicitKeyColumns) then
      SetLength(FExplicitKeyColumns, FExplicitKeyCount * 2 + 4);
    FExplicitKeyColumns[FExplicitKeyCount] := StartMark.Column;
    Inc(FExplicitKeyCount);
  end
  else if FFlowStateCount > 0 then
    // the parser starts the mapping a key in a flow sequence makes
    FFlowMappingStarted[FFlowStateCount - 1] := True;

  RemoveSimpleKey;
  FSimpleKeyAllowed := FFlowLevel = 0;
  SkipNonBlank;
  if FFlowLevel = 0 then
  begin
    SkipSeparationAfterIndicator('?', TabSeparated);
    if TabSeparated then
      FSimpleKeyAllowed := False;
  end
  else
    // in a flow collection "?" is separated like any other flow token, tabs
    // included
    SkipToNextToken;
  PushToken(ytkKey, StartMark, FMark);
end;

{ a ":" in a flow collection that no blank follows }
procedure TGocciaYAMLScanner.FetchFlowValue;
var
  NextCharacter: Char;
begin
  // ["a":[]] is a JSON-like key's adjacent value; otherwise [a:[]] would
  // need a space before the value
  NextCharacter := Peek(1);
  if (FMark.Index <> FAdjacentValueAllowedAt) and
     ((NextCharacter = '[') or (NextCharacter = '{')) then
    RaiseError(FMark, ''':'' may not precede any of `[{` in flow mapping');
  FetchValue;
end;

{ ":": the queued simple key, if possible, becomes a key }
procedure TGocciaYAMLScanner.FetchValue;
var
  IsExplicitValue, IsImplicitFlowMapping, TabSeparated: Boolean;
  Key: TSimpleKey;
  StartMark: TGocciaYAMLMarker;
  Top: Integer;
begin
  if FSimpleKeyCount = 0 then
    RaiseError(FMark, 'simple key expected');
  Key := FSimpleKeys[LastSimpleKey];
  StartMark := FMark;
  Top := FFlowStateCount - 1;
  IsImplicitFlowMapping := (Top >= 0) and
    (FImplicitFlowMappingStates[Top] <> imsNone) and not FFlowMappingStarted[Top];
  if IsImplicitFlowMapping then
    FImplicitFlowMappingStates[Top] := imsInside;

  SkipNonBlank;
  TabSeparated := False;
  if FFlowLevel = 0 then
    SkipSeparationAfterIndicator(':', TabSeparated);

  if Key.Possible then
  begin
    InsertToken(Key.TokenNumber - FTokensParsed,
      NewToken(ytkKey, Key.Mark, Key.Mark));
    if IsImplicitFlowMapping then
    begin
      if Key.Mark.Line < StartMark.Line then
        RaiseError(StartMark, 'illegal placement of '':'' indicator');
      InsertToken(Key.TokenNumber - FTokensParsed,
        NewToken(ytkFlowMappingStart, Key.Mark, Key.Mark));
    end;
    RollIndent(Key.Mark.Column, Key.TokenNumber, ytkBlockMappingStart, Key.Mark);
    RollOneColumnIndent;
    // an implicit entry ends the explicit keys waiting at its level
    if FFlowLevel = 0 then
      DropExplicitKeysFrom(Key.Mark.Column);
    FSimpleKeys[LastSimpleKey].Possible := False;
    FSimpleKeyAllowed := False;
  end
  else
  begin
    if IsImplicitFlowMapping then
      PushToken(ytkFlowMappingStart, StartMark, StartMark);
    // the ":" follows an explicit key, or stands for an empty one
    IsExplicitValue := False;
    if FFlowLevel = 0 then
    begin
      if not FSimpleKeyAllowed then
        RaiseError(StartMark, 'mapping values are not allowed in this context');
      RollIndent(StartMark.Column, -1, ytkBlockMappingStart, StartMark);
      IsExplicitValue := (FExplicitKeyCount > 0) and
        (FExplicitKeyColumns[FExplicitKeyCount - 1] = StartMark.Column);
      if IsExplicitValue then
        Dec(FExplicitKeyCount);
    end;
    RollOneColumnIndent;
    // an explicit key's value may be a compact collection on the line
    // (8.2.2, s-l+block-indented); an empty key's, like an implicit key's,
    // may not (8.2.3)
    FSimpleKeyAllowed := (FFlowLevel = 0) and not TabSeparated and IsExplicitValue;
  end;
  PushToken(ytkValue, StartMark, StartMark);
end;

end.
