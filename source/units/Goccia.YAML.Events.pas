unit Goccia.YAML.Events;

{ The YAML 1.2.2 event parser (yaml.org/spec/1.2.2): the tokens of
  Goccia.YAML.Scanner into the stream of events the yaml-test-suite describes
  in its test.event files, one at a time. It checks the structure the scanner
  cannot see, resolves tag handles against the document's %TAG directives,
  and rejects an alias to an anchor the document has not defined.

  Ported from the parser of saphyr-parser (github.com/saphyr-rs/saphyr,
  parser/src/parser.rs at 7867251b), under the MIT license that
  Goccia.YAML.Scanner quotes. }

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  OrderedStringMap,

  Goccia.YAML.Scanner;

type
  TGocciaYAMLEventKind = (
    yekStreamStart,
    yekStreamEnd,
    yekDocumentStart,
    yekDocumentEnd,
    yekAlias,
    yekScalar,
    yekSequenceStart,
    yekSequenceEnd,
    yekMappingStart,
    yekMappingEnd
  );

  TGocciaYAMLEvent = record
    Kind: TGocciaYAMLEventKind;
    StartMark: TGocciaYAMLMarker;
    EndMark: TGocciaYAMLMarker;
    { a scalar's content, an alias's anchor name }
    Value: string;
    Style: TGocciaYAMLScalarStyle;
    { a node's anchor name, '' for none }
    Anchor: string;
    { a node's tag, resolved to its full name; HasTag tells "!<>" from none }
    HasTag: Boolean;
    Tag: string;
    { a collection written in flow style }
    Flow: Boolean;
    { a document started with "---" or ended with "..." }
    Explicit: Boolean;
  end;

  TGocciaYAMLEventParser = class
  private type
    TState = (
      psStreamStart,
      psImplicitDocumentStart,
      psDocumentStart,
      psDocumentContent,
      psDocumentEnd,
      psBlockNode,
      psBlockSequenceFirstEntry,
      psBlockSequenceEntry,
      psIndentlessSequenceEntry,
      psBlockMappingFirstKey,
      psBlockMappingKey,
      psBlockMappingValue,
      psFlowSequenceFirstEntry,
      psFlowSequenceEntry,
      psFlowSequenceEntryMappingKey,
      psFlowSequenceEntryMappingValue,
      psFlowSequenceEntryMappingEnd,
      psFlowMappingFirstKey,
      psFlowMappingKey,
      psFlowMappingValue,
      psFlowMappingEmptyValue,
      psEnd
    );
    TStateStack = TList<TState>;
    TAnchorNames = TOrderedStringMap<Boolean>;
  private
    FScanner: TGocciaYAMLScanner;
    FStates: TStateStack;
    FState: TState;
    FToken: TGocciaYAMLToken;
    FHasToken: Boolean;
    { where the mapping a flow-sequence pair makes ends }
    FPairEndMark: TGocciaYAMLMarker;
    FAnchors: TAnchorNames;
    FTagHandles: TStringStringMap;
    FStreamEndEmitted: Boolean;

    procedure RaiseError(const AMark: TGocciaYAMLMarker; const AMessage: string);
    function PeekToken: TGocciaYAMLToken;
    function FetchToken: TGocciaYAMLToken;
    procedure SkipToken;
    procedure PushState(const AState: TState);
    procedure PopState;
    function NewEvent(const AKind: TGocciaYAMLEventKind;
      const AStartMark, AEndMark: TGocciaYAMLMarker): TGocciaYAMLEvent;
    function EmptyScalar(const AMark: TGocciaYAMLMarker): TGocciaYAMLEvent;
    function ResolveTag(const AMark: TGocciaYAMLMarker;
      const AHandle, ASuffix: string): string;

    function StreamStart: TGocciaYAMLEvent;
    function DocumentStart(const AImplicit: Boolean): TGocciaYAMLEvent;
    procedure ProcessDirectives;
    function ExplicitDocumentStart: TGocciaYAMLEvent;
    function DocumentContent: TGocciaYAMLEvent;
    function DocumentEnd: TGocciaYAMLEvent;
    function ParseNode(const ABlock, AIndentlessSequence: Boolean): TGocciaYAMLEvent;
    function BlockMappingKey(const AFirst: Boolean): TGocciaYAMLEvent;
    function BlockMappingValue: TGocciaYAMLEvent;
    function FlowMappingKey(const AFirst: Boolean): TGocciaYAMLEvent;
    function FlowMappingValue(const AEmpty: Boolean): TGocciaYAMLEvent;
    function FlowSequenceEntry(const AFirst: Boolean): TGocciaYAMLEvent;
    function IndentlessSequenceEntry: TGocciaYAMLEvent;
    function BlockSequenceEntry(const AFirst: Boolean): TGocciaYAMLEvent;
    function FlowSequenceEntryMappingKey: TGocciaYAMLEvent;
    function FlowSequenceEntryMappingValue: TGocciaYAMLEvent;
    function FlowSequenceEntryMappingEnd: TGocciaYAMLEvent;
  public
    constructor Create(const AText: string);
    destructor Destroy; override;

    { the next event, until and including the stream end; raises
      EGocciaYAMLParseError for text that is not YAML }
    function NextEvent: TGocciaYAMLEvent;

    property StreamEnded: Boolean read FStreamEndEmitted;
  end;

{ AEvent in the yaml-test-suite's test.event notation }
function YAMLEventText(const AEvent: TGocciaYAMLEvent): string;

implementation

uses
  StringBuffer;

const
  TAG_HANDLE_PRIMARY = '!';
  TAG_HANDLE_SECONDARY = '!!';
  TAG_PREFIX_SECONDARY = 'tag:yaml.org,2002:';
  { the only YAML version this parser reads; a higher minor version is read
    as this one (6.8.1) }
  SUPPORTED_YAML_MAJOR_VERSION = 1;

{ TGocciaYAMLEventParser }

constructor TGocciaYAMLEventParser.Create(const AText: string);
begin
  inherited Create;
  FScanner := TGocciaYAMLScanner.Create(AText);
  FStates := TStateStack.Create;
  FState := psStreamStart;
  FAnchors := TAnchorNames.Create;
  FTagHandles := TStringStringMap.Create;
end;

destructor TGocciaYAMLEventParser.Destroy;
begin
  FTagHandles.Free;
  FAnchors.Free;
  FStates.Free;
  FScanner.Free;
  inherited;
end;

procedure TGocciaYAMLEventParser.RaiseError(const AMark: TGocciaYAMLMarker;
  const AMessage: string);
begin
  raise EGocciaYAMLParseError.CreateFmt('Line %d, column %d: %s',
    [AMark.Line, AMark.Column + 1, AMessage]);
end;

function TGocciaYAMLEventParser.PeekToken: TGocciaYAMLToken;
begin
  if not FHasToken then
  begin
    FToken := FScanner.NextToken;
    FHasToken := True;
  end;
  Result := FToken;
end;

function TGocciaYAMLEventParser.FetchToken: TGocciaYAMLToken;
begin
  Result := PeekToken;
  FHasToken := False;
end;

procedure TGocciaYAMLEventParser.SkipToken;
begin
  FHasToken := False;
end;

procedure TGocciaYAMLEventParser.PushState(const AState: TState);
begin
  FStates.Add(AState);
end;

procedure TGocciaYAMLEventParser.PopState;
begin
  FState := FStates[FStates.Count - 1];
  FStates.Delete(FStates.Count - 1);
end;

function TGocciaYAMLEventParser.NewEvent(const AKind: TGocciaYAMLEventKind;
  const AStartMark, AEndMark: TGocciaYAMLMarker): TGocciaYAMLEvent;
begin
  Result.Kind := AKind;
  Result.StartMark := AStartMark;
  Result.EndMark := AEndMark;
  Result.Value := '';
  Result.Style := yssPlain;
  Result.Anchor := '';
  Result.HasTag := False;
  Result.Tag := '';
  Result.Flow := False;
  Result.Explicit := False;
end;

function TGocciaYAMLEventParser.EmptyScalar(const AMark: TGocciaYAMLMarker): TGocciaYAMLEvent;
begin
  Result := NewEvent(yekScalar, AMark, AMark);
end;

function TGocciaYAMLEventParser.NextEvent: TGocciaYAMLEvent;
begin
  if FStreamEndEmitted then
    RaiseError(FScanner.Mark, 'unexpected end of stream');
  case FState of
    psStreamStart:
      Result := StreamStart;
    psImplicitDocumentStart:
      Result := DocumentStart(True);
    psDocumentStart:
      Result := DocumentStart(False);
    psDocumentContent:
      Result := DocumentContent;
    psDocumentEnd:
      Result := DocumentEnd;
    psBlockNode:
      Result := ParseNode(True, False);
    psBlockMappingFirstKey:
      Result := BlockMappingKey(True);
    psBlockMappingKey:
      Result := BlockMappingKey(False);
    psBlockMappingValue:
      Result := BlockMappingValue;
    psBlockSequenceFirstEntry:
      Result := BlockSequenceEntry(True);
    psBlockSequenceEntry:
      Result := BlockSequenceEntry(False);
    psFlowSequenceFirstEntry:
      Result := FlowSequenceEntry(True);
    psFlowSequenceEntry:
      Result := FlowSequenceEntry(False);
    psFlowMappingFirstKey:
      Result := FlowMappingKey(True);
    psFlowMappingKey:
      Result := FlowMappingKey(False);
    psFlowMappingValue:
      Result := FlowMappingValue(False);
    psIndentlessSequenceEntry:
      Result := IndentlessSequenceEntry;
    psFlowSequenceEntryMappingKey:
      Result := FlowSequenceEntryMappingKey;
    psFlowSequenceEntryMappingValue:
      Result := FlowSequenceEntryMappingValue;
    psFlowSequenceEntryMappingEnd:
      Result := FlowSequenceEntryMappingEnd;
    psFlowMappingEmptyValue:
      Result := FlowMappingValue(True);
  else
    Result := NewEvent(yekStreamEnd, FScanner.Mark, FScanner.Mark);
  end;
  if Result.Kind = yekStreamEnd then
    FStreamEndEmitted := True;
end;

function TGocciaYAMLEventParser.StreamStart: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  if Token.Kind <> ytkStreamStart then
    RaiseError(Token.StartMark, 'did not find expected <stream-start>');
  FState := psImplicitDocumentStart;
  SkipToken;
  Result := NewEvent(yekStreamStart, Token.StartMark, Token.EndMark);
end;

function TGocciaYAMLEventParser.DocumentStart(const AImplicit: Boolean): TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  while PeekToken.Kind in [ytkDocumentEnd, ytkByteOrderMark] do
    SkipToken;
  Token := PeekToken;
  case Token.Kind of
    ytkStreamEnd:
      begin
        FState := psEnd;
        SkipToken;
        Result := NewEvent(yekStreamEnd, Token.StartMark, Token.EndMark);
      end;
    ytkVersionDirective, ytkTagDirective, ytkReservedDirective, ytkDocumentStart:
      Result := ExplicitDocumentStart;
  else
    if AImplicit then
    begin
      ProcessDirectives;
      PushState(psDocumentEnd);
      FState := psBlockNode;
      FAnchors.Clear;
      Result := NewEvent(yekDocumentStart, Token.StartMark, Token.StartMark);
    end
    else
      Result := ExplicitDocumentStart;
  end;
end;

{ the directives before a document; %TAG handles apply to that document }
procedure TGocciaYAMLEventParser.ProcessDirectives;
var
  DocumentHandles: TStringStringMap;
  Pair: TStringStringMap.TKeyValuePair;
  Token: TGocciaYAMLToken;
  VersionDirectiveSeen: Boolean;
begin
  VersionDirectiveSeen := False;
  DocumentHandles := TStringStringMap.Create;
  try
    while True do
    begin
      Token := PeekToken;
      case Token.Kind of
        ytkVersionDirective:
          begin
            if VersionDirectiveSeen then
              RaiseError(Token.StartMark, 'duplicate version directive');
            VersionDirectiveSeen := True;
            if Token.MajorVersion <> SUPPORTED_YAML_MAJOR_VERSION then
              RaiseError(Token.StartMark, Format('unsupported YAML version %d.%d',
                [Token.MajorVersion, Token.MinorVersion]));
          end;
        ytkTagDirective:
          begin
            if DocumentHandles.ContainsKey(Token.Handle) then
              RaiseError(Token.StartMark,
                'the TAG directive must only be given at most once per handle in the same document');
            DocumentHandles.AddOrSetValue(Token.Handle, Token.Value);
          end;
        ytkReservedDirective:
          ;
      else
        Break;
      end;
      SkipToken;
    end;
    for Pair in DocumentHandles do
      FTagHandles.AddOrSetValue(Pair.Key, Pair.Value);
  finally
    DocumentHandles.Free;
  end;
end;

function TGocciaYAMLEventParser.ExplicitDocumentStart: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  ProcessDirectives;
  Token := PeekToken;
  case Token.Kind of
    ytkDocumentStart:
      begin
        PushState(psDocumentEnd);
        FState := psDocumentContent;
        SkipToken;
        FAnchors.Clear;
        Result := NewEvent(yekDocumentStart, Token.StartMark, Token.EndMark);
        Result.Explicit := True;
      end;
    ytkByteOrderMark:
      RaiseError(Token.StartMark,
        'a byte order mark must not appear inside a document');
  else
    RaiseError(Token.StartMark, 'did not find expected <document start>');
  end;
end;

function TGocciaYAMLEventParser.DocumentContent: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  if Token.Kind in [ytkVersionDirective, ytkTagDirective, ytkReservedDirective,
     ytkDocumentStart, ytkDocumentEnd, ytkByteOrderMark, ytkStreamEnd] then
  begin
    PopState;
    Result := EmptyScalar(Token.StartMark);
  end
  else
    Result := ParseNode(True, False);
end;

function TGocciaYAMLEventParser.DocumentEnd: TGocciaYAMLEvent;
var
  ByteOrderMark, Token: TGocciaYAMLToken;
  ExplicitEnd: Boolean;
begin
  // a byte order mark ends the document, and only byte order marks,
  // comments or document markers may follow it (9.2)
  if PeekToken.Kind = ytkByteOrderMark then
  begin
    ByteOrderMark := PeekToken;
    while PeekToken.Kind = ytkByteOrderMark do
      SkipToken;
    if not (PeekToken.Kind in [ytkDocumentStart, ytkDocumentEnd, ytkStreamEnd]) then
      RaiseError(ByteOrderMark.StartMark,
        'a byte order mark must not appear inside a document');
  end;

  Token := PeekToken;
  ExplicitEnd := Token.Kind = ytkDocumentEnd;
  if ExplicitEnd then
    SkipToken;
  FTagHandles.Clear;
  if ExplicitEnd then
    FState := psImplicitDocumentStart
  else
  begin
    case PeekToken.Kind of
      ytkVersionDirective, ytkTagDirective, ytkReservedDirective:
        RaiseError(PeekToken.StartMark,
          'missing explicit document end marker before directive');
      ytkDocumentStart, ytkStreamEnd:
        ;
    else
      RaiseError(PeekToken.StartMark, 'did not find expected <document start>');
    end;
    FState := psDocumentStart;
  end;
  Result := NewEvent(yekDocumentEnd, Token.StartMark, Token.EndMark);
  Result.Explicit := ExplicitEnd;
end;

{ a node: its anchor and tag, in either order, then its content }
function TGocciaYAMLEventParser.ParseNode(const ABlock,
  AIndentlessSequence: Boolean): TGocciaYAMLEvent;
var
  Anchor, Tag: string;
  EndMark, StartMark: TGocciaYAMLMarker;
  HasAnchor, HasTag: Boolean;
  Token: TGocciaYAMLToken;
begin
  Anchor := '';
  Tag := '';
  HasAnchor := False;
  HasTag := False;
  Token := PeekToken;
  StartMark := Token.StartMark;
  EndMark := StartMark;

  case Token.Kind of
    ytkAlias:
      begin
        PopState;
        SkipToken;
        if not FAnchors.ContainsKey(Token.Value) then
          RaiseError(Token.StartMark, 'while parsing node, found unknown anchor');
        Result := NewEvent(yekAlias, Token.StartMark, Token.EndMark);
        Result.Value := Token.Value;
        Exit;
      end;
    ytkAnchor:
      begin
        SkipToken;
        Anchor := Token.Value;
        HasAnchor := True;
        FAnchors.AddOrSetValue(Anchor, True);
        EndMark := Token.EndMark;
        if PeekToken.Kind = ytkTag then
        begin
          Token := FetchToken;
          Tag := ResolveTag(Token.StartMark, Token.Handle, Token.Value);
          HasTag := True;
          EndMark := Token.EndMark;
        end;
      end;
    ytkTag:
      begin
        SkipToken;
        Tag := ResolveTag(Token.StartMark, Token.Handle, Token.Value);
        HasTag := True;
        EndMark := Token.EndMark;
        if PeekToken.Kind = ytkAnchor then
        begin
          Token := FetchToken;
          Anchor := Token.Value;
          HasAnchor := True;
          FAnchors.AddOrSetValue(Anchor, True);
          EndMark := Token.EndMark;
        end;
      end;
  end;

  Token := PeekToken;
  if (Token.Kind = ytkBlockEntry) and AIndentlessSequence then
  begin
    FState := psIndentlessSequenceEntry;
    Result := NewEvent(yekSequenceStart, StartMark, Token.EndMark);
  end
  else if Token.Kind = ytkScalar then
  begin
    PopState;
    SkipToken;
    Result := NewEvent(yekScalar, StartMark, Token.EndMark);
    Result.Value := Token.Value;
    Result.Style := Token.Style;
  end
  else if Token.Kind = ytkFlowSequenceStart then
  begin
    FState := psFlowSequenceFirstEntry;
    Result := NewEvent(yekSequenceStart, StartMark, Token.EndMark);
    Result.Flow := True;
  end
  else if Token.Kind = ytkFlowMappingStart then
  begin
    FState := psFlowMappingFirstKey;
    Result := NewEvent(yekMappingStart, StartMark, Token.EndMark);
    Result.Flow := True;
  end
  else if (Token.Kind = ytkBlockSequenceStart) and ABlock then
  begin
    FState := psBlockSequenceFirstEntry;
    Result := NewEvent(yekSequenceStart, StartMark, Token.EndMark);
  end
  else if (Token.Kind = ytkBlockMappingStart) and ABlock then
  begin
    FState := psBlockMappingFirstKey;
    Result := NewEvent(yekMappingStart, StartMark, Token.EndMark);
  end
  else if HasTag or HasAnchor then
  begin
    // node properties with no content: an empty scalar (7.2)
    PopState;
    Result := NewEvent(yekScalar, StartMark, EndMark);
  end
  else
    RaiseError(Token.StartMark,
      'while parsing a node, did not find expected node content');
  Result.Anchor := Anchor;
  Result.HasTag := HasTag;
  Result.Tag := Tag;
end;

function TGocciaYAMLEventParser.BlockMappingKey(const AFirst: Boolean): TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  if AFirst then
  begin
    PeekToken;
    SkipToken;
  end;
  Token := PeekToken;
  case Token.Kind of
    ytkKey:
      begin
        SkipToken;
        Token := PeekToken;
        if Token.Kind in [ytkKey, ytkValue, ytkBlockEnd] then
        begin
          FState := psBlockMappingValue;
          Result := EmptyScalar(Token.StartMark);
        end
        else
        begin
          PushState(psBlockMappingValue);
          Result := ParseNode(True, True);
        end;
      end;
    // a value with no key (8.2.2, example 8.18)
    ytkValue:
      begin
        FState := psBlockMappingValue;
        Result := EmptyScalar(Token.StartMark);
      end;
    ytkBlockEnd:
      begin
        PopState;
        SkipToken;
        Result := NewEvent(yekMappingEnd, Token.StartMark, Token.EndMark);
      end;
  else
    RaiseError(Token.StartMark,
      'while parsing a block mapping, did not find expected key');
  end;
end;

function TGocciaYAMLEventParser.BlockMappingValue: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  if Token.Kind = ytkValue then
  begin
    SkipToken;
    if PeekToken.Kind in [ytkKey, ytkValue, ytkBlockEnd] then
    begin
      FState := psBlockMappingKey;
      Result := EmptyScalar(Token.StartMark);
    end
    else
    begin
      PushState(psBlockMappingKey);
      Result := ParseNode(True, True);
    end;
  end
  else
  begin
    FState := psBlockMappingKey;
    Result := EmptyScalar(Token.StartMark);
  end;
end;

function TGocciaYAMLEventParser.FlowMappingKey(const AFirst: Boolean): TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  if AFirst then
  begin
    PeekToken;
    SkipToken;
  end;
  Token := PeekToken;
  if Token.Kind <> ytkFlowMappingEnd then
  begin
    if not AFirst then
    begin
      if PeekToken.Kind = ytkFlowEntry then
        SkipToken
      else
        RaiseError(PeekToken.StartMark,
          'while parsing a flow mapping, did not find expected '','' or ''}''');
    end;
    Token := PeekToken;
    case Token.Kind of
      ytkKey:
        begin
          SkipToken;
          Token := PeekToken;
          if Token.Kind in [ytkValue, ytkFlowEntry, ytkFlowMappingEnd] then
          begin
            FState := psFlowMappingValue;
            Exit(EmptyScalar(Token.StartMark));
          end;
          PushState(psFlowMappingValue);
          Exit(ParseNode(False, False));
        end;
      ytkValue:
        begin
          FState := psFlowMappingValue;
          Exit(EmptyScalar(Token.StartMark));
        end;
      ytkFlowMappingEnd:
        ;
    else
      PushState(psFlowMappingEmptyValue);
      Exit(ParseNode(False, False));
    end;
  end;
  PopState;
  SkipToken;
  Result := NewEvent(yekMappingEnd, Token.StartMark, Token.EndMark);
end;

function TGocciaYAMLEventParser.FlowMappingValue(const AEmpty: Boolean): TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  if AEmpty then
  begin
    FState := psFlowMappingKey;
    Exit(EmptyScalar(Token.StartMark));
  end;
  if Token.Kind = ytkValue then
  begin
    SkipToken;
    if not (PeekToken.Kind in [ytkFlowEntry, ytkFlowMappingEnd]) then
    begin
      PushState(psFlowMappingKey);
      Exit(ParseNode(False, False));
    end;
  end;
  FState := psFlowMappingKey;
  Result := EmptyScalar(Token.StartMark);
end;

function TGocciaYAMLEventParser.FlowSequenceEntry(const AFirst: Boolean): TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  if AFirst then
  begin
    PeekToken;
    SkipToken;
  end;
  Token := PeekToken;
  if Token.Kind = ytkFlowSequenceEnd then
  begin
    PopState;
    SkipToken;
    Exit(NewEvent(yekSequenceEnd, Token.StartMark, Token.EndMark));
  end;
  if not AFirst then
  begin
    if Token.Kind = ytkFlowEntry then
      SkipToken
    else
      RaiseError(Token.StartMark,
        'while parsing a flow sequence, expected '','' or '']''');
  end;
  Token := PeekToken;
  case Token.Kind of
    ytkFlowSequenceEnd:
      begin
        PopState;
        SkipToken;
        Result := NewEvent(yekSequenceEnd, Token.StartMark, Token.EndMark);
      end;
    // a single-pair mapping as an entry ([? a : b])
    ytkKey:
      begin
        FState := psFlowSequenceEntryMappingKey;
        SkipToken;
        Result := NewEvent(yekMappingStart, Token.StartMark, Token.EndMark);
        Result.Flow := True;
      end;
  else
    PushState(psFlowSequenceEntry);
    Result := ParseNode(False, False);
  end;
end;

function TGocciaYAMLEventParser.IndentlessSequenceEntry: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  if Token.Kind = ytkBlockEntry then
  begin
    SkipToken;
    if PeekToken.Kind in [ytkBlockEntry, ytkKey, ytkValue, ytkBlockEnd] then
    begin
      FState := psIndentlessSequenceEntry;
      Result := EmptyScalar(Token.StartMark);
    end
    else
    begin
      PushState(psIndentlessSequenceEntry);
      Result := ParseNode(True, False);
    end;
  end
  else
  begin
    PopState;
    Result := NewEvent(yekSequenceEnd, Token.StartMark, Token.StartMark);
  end;
end;

function TGocciaYAMLEventParser.BlockSequenceEntry(const AFirst: Boolean): TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  if AFirst then
  begin
    PeekToken;
    SkipToken;
  end;
  Token := PeekToken;
  case Token.Kind of
    ytkBlockEnd:
      begin
        PopState;
        SkipToken;
        Result := NewEvent(yekSequenceEnd, Token.StartMark, Token.EndMark);
      end;
    ytkBlockEntry:
      begin
        SkipToken;
        if PeekToken.Kind in [ytkBlockEntry, ytkBlockEnd] then
        begin
          FState := psBlockSequenceEntry;
          Result := EmptyScalar(Token.StartMark);
        end
        else
        begin
          PushState(psBlockSequenceEntry);
          Result := ParseNode(True, False);
        end;
      end;
  else
    RaiseError(Token.StartMark,
      'while parsing a block collection, did not find expected ''-'' indicator');
  end;
end;

function TGocciaYAMLEventParser.FlowSequenceEntryMappingKey: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  // an empty key: the ":", "," or "]" stays for the value and end states
  // ([? : value], [? , x], [? ]); saphyr skipped it here
  if Token.Kind in [ytkValue, ytkFlowEntry, ytkFlowSequenceEnd] then
  begin
    FState := psFlowSequenceEntryMappingValue;
    Result := EmptyScalar(Token.StartMark);
  end
  else
  begin
    PushState(psFlowSequenceEntryMappingValue);
    Result := ParseNode(False, False);
  end;
end;

function TGocciaYAMLEventParser.FlowSequenceEntryMappingValue: TGocciaYAMLEvent;
var
  Token: TGocciaYAMLToken;
begin
  Token := PeekToken;
  if Token.Kind = ytkValue then
  begin
    SkipToken;
    Token := PeekToken;
    FPairEndMark := Token.EndMark;
    if Token.Kind in [ytkFlowEntry, ytkFlowSequenceEnd] then
    begin
      FState := psFlowSequenceEntryMappingEnd;
      Result := EmptyScalar(Token.StartMark);
    end
    else
    begin
      PushState(psFlowSequenceEntryMappingEnd);
      Result := ParseNode(False, False);
    end;
  end
  else
  begin
    FPairEndMark := Token.EndMark;
    FState := psFlowSequenceEntryMappingEnd;
    Result := EmptyScalar(Token.StartMark);
  end;
end;

function TGocciaYAMLEventParser.FlowSequenceEntryMappingEnd: TGocciaYAMLEvent;
begin
  FState := psFlowSequenceEntry;
  Result := NewEvent(yekMappingEnd, FPairEndMark, FPairEndMark);
end;

{ a tag's full name: "!!" is tag:yaml.org,2002: unless a %TAG directive
  names it, "!" is local, and a named handle must be declared }
function TGocciaYAMLEventParser.ResolveTag(const AMark: TGocciaYAMLMarker;
  const AHandle, ASuffix: string): string;
var
  Prefix: string;
begin
  if AHandle = TAG_HANDLE_SECONDARY then
  begin
    if not FTagHandles.TryGetValue(TAG_HANDLE_SECONDARY, Prefix) then
      Prefix := TAG_PREFIX_SECONDARY;
    Exit(Prefix + ASuffix);
  end;
  // the non-specific tag "!" (handle '', suffix "!") keeps its name even
  // when %TAG ! names a prefix
  if (AHandle = '') and (ASuffix = TAG_HANDLE_PRIMARY) then
    Exit(ASuffix);
  if FTagHandles.TryGetValue(AHandle, Prefix) then
    Exit(Prefix + ASuffix);
  if (Length(AHandle) >= 2) and (AHandle[1] = '!') and
     (AHandle[Length(AHandle)] = '!') then
    RaiseError(AMark, 'the handle wasn''t declared');
  Result := AHandle + ASuffix;
end;

{ YAMLEventText }

procedure AppendEscapedValue(var ABuffer: TStringBuffer; const AText: string);
var
  I: Integer;
begin
  for I := 1 to Length(AText) do
    case AText[I] of
      '\':
        ABuffer.Append('\\');
      #8:
        ABuffer.Append('\b');
      #9:
        ABuffer.Append('\t');
      #10:
        ABuffer.Append('\n');
      #13:
        ABuffer.Append('\r');
    else
      ABuffer.AppendChar(AText[I]);
    end;
end;

procedure AppendNodeProperties(var ABuffer: TStringBuffer;
  const AEvent: TGocciaYAMLEvent);
begin
  if AEvent.Anchor <> '' then
  begin
    ABuffer.Append(' &');
    ABuffer.Append(AEvent.Anchor);
  end;
  if AEvent.HasTag then
  begin
    ABuffer.Append(' <');
    ABuffer.Append(AEvent.Tag);
    ABuffer.AppendChar('>');
  end;
end;

function YAMLEventText(const AEvent: TGocciaYAMLEvent): string;
const
  SCALAR_STYLE_INDICATORS: array[TGocciaYAMLScalarStyle] of Char =
    (':', '''', '"', '|', '>');
var
  Buffer: TStringBuffer;
begin
  Buffer := TStringBuffer.Create;
  case AEvent.Kind of
    yekStreamStart:
      Buffer.Append('+STR');
    yekStreamEnd:
      Buffer.Append('-STR');
    yekDocumentStart:
      begin
        Buffer.Append('+DOC');
        if AEvent.Explicit then
          Buffer.Append(' ---');
      end;
    yekDocumentEnd:
      begin
        Buffer.Append('-DOC');
        if AEvent.Explicit then
          Buffer.Append(' ...');
      end;
    yekAlias:
      begin
        Buffer.Append('=ALI *');
        Buffer.Append(AEvent.Value);
      end;
    yekScalar:
      begin
        Buffer.Append('=VAL');
        AppendNodeProperties(Buffer, AEvent);
        Buffer.AppendChar(' ');
        Buffer.AppendChar(SCALAR_STYLE_INDICATORS[AEvent.Style]);
        AppendEscapedValue(Buffer, AEvent.Value);
      end;
    yekSequenceStart:
      begin
        Buffer.Append('+SEQ');
        if AEvent.Flow then
          Buffer.Append(' []');
        AppendNodeProperties(Buffer, AEvent);
      end;
    yekSequenceEnd:
      Buffer.Append('-SEQ');
    yekMappingStart:
      begin
        Buffer.Append('+MAP');
        if AEvent.Flow then
          Buffer.Append(' {}');
        AppendNodeProperties(Buffer, AEvent);
      end;
    yekMappingEnd:
      Buffer.Append('-MAP');
  end;
  Result := Buffer.ToString;
end;

end.
