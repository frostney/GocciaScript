unit Goccia.Error;

{$I Goccia.inc}

interface

uses
  Classes,
  SysUtils;

type
  TGocciaErrorSeverity = (gesError, gesWarning, gesHint);

  // Future: Native error subclasses (TypeError, RangeError, etc.) could inherit from
  // TGocciaError to unify Pascal-side and JS-side error handling. Currently, JS-visible
  // error objects are created separately via Goccia.Values.ErrorHelper.

  TGocciaError = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
    FFileName: string;
    FSourceLines: TStringList;
    FOwnsSourceLines: Boolean;
    FSeverity: TGocciaErrorSeverity;
    FSuggestion: string;
    procedure CopySourceLines(const ASourceLines: TStringList);
  public
    constructor Create(const AMessage: string; const ALine, AColumn: Integer;
      const AFileName: string; const ASourceLines: TStringList); overload;
    constructor Create(const AMessage: string; const ALine, AColumn: Integer;
      const AFileName: string; const ASourceLines: TStringList;
      const ASuggestion: string); overload;
    destructor Destroy; override;
    procedure SetSuggestion(const ASuggestion: string);
    function GetDetailedMessage: string; overload;
    function GetDetailedMessage(const AUseColor: Boolean): string; overload;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property FileName: string read FFileName;
    procedure TranslatePosition(const ANewLine, ANewColumn: Integer;
      const AOriginalSourceLines: TStringList);
    property Severity: TGocciaErrorSeverity read FSeverity write FSeverity;
    property Suggestion: string read FSuggestion write FSuggestion;
  end;

  TGocciaLexerError = class(TGocciaError);
  TGocciaSyntaxError = class(TGocciaError);
  TGocciaRuntimeError = class(TGocciaError);
  TGocciaTypeError = class(TGocciaRuntimeError);
  TGocciaReferenceError = class(TGocciaRuntimeError);

function ErrorDisplayName(const AError: TGocciaError): string;

implementation

uses
  StringBuffer,

  Goccia.Terminal.Colors;

const
  CONTEXT_LINES_BEFORE = 2;
  CONTEXT_LINES_AFTER = 2;

function ErrorDisplayName(const AError: TGocciaError): string;
begin
  if AError is TGocciaTypeError then
    Result := 'TypeError'
  else if AError is TGocciaReferenceError then
    Result := 'ReferenceError'
  else if AError is TGocciaSyntaxError then
    Result := 'SyntaxError'
  else if AError is TGocciaRuntimeError then
    Result := 'RuntimeError'
  else if AError is TGocciaLexerError then
    Result := 'SyntaxError'
  else
    Result := 'Error';
end;

procedure TGocciaError.CopySourceLines(const ASourceLines: TStringList);
begin
  if Assigned(ASourceLines) then
  begin
    FSourceLines := TStringList.Create;
    FSourceLines.Assign(ASourceLines);
    FOwnsSourceLines := True;
  end
  else
  begin
    FSourceLines := nil;
    FOwnsSourceLines := False;
  end;
end;

constructor TGocciaError.Create(const AMessage: string; const ALine, AColumn: Integer;
  const AFileName: string; const ASourceLines: TStringList);
begin
  inherited Create(AMessage);
  FLine := ALine;
  FColumn := AColumn;
  FFileName := AFileName;
  CopySourceLines(ASourceLines);
  FSeverity := gesError;
  FSuggestion := '';
end;

constructor TGocciaError.Create(const AMessage: string; const ALine, AColumn: Integer;
  const AFileName: string; const ASourceLines: TStringList;
  const ASuggestion: string);
begin
  inherited Create(AMessage);
  FLine := ALine;
  FColumn := AColumn;
  FFileName := AFileName;
  CopySourceLines(ASourceLines);
  FSeverity := gesError;
  FSuggestion := ASuggestion;
end;

destructor TGocciaError.Destroy;
begin
  if FOwnsSourceLines then
    FSourceLines.Free;
  inherited;
end;

procedure TGocciaError.SetSuggestion(const ASuggestion: string);
begin
  FSuggestion := ASuggestion;
end;

procedure TGocciaError.TranslatePosition(const ANewLine, ANewColumn: Integer;
  const AOriginalSourceLines: TStringList);
begin
  FLine := ANewLine;
  FColumn := ANewColumn;
  if FOwnsSourceLines then
    FSourceLines.Free;
  CopySourceLines(AOriginalSourceLines);
end;

function TGocciaError.GetDetailedMessage: string;
begin
  Result := GetDetailedMessage(False);
end;

function TGocciaError.GetDetailedMessage(const AUseColor: Boolean): string;
var
  Buffer: TStringBuffer;
  DisplayName: string;
  GutterWidth, LineNum, I: Integer;
  FirstContextLine, LastContextLine: Integer;
  LineStr, Gutter, CaretStr: string;
begin
  Buffer := TStringBuffer.Create(512);

  // Error header: SyntaxError: message
  DisplayName := ErrorDisplayName(Self);
  Buffer.Append(Colorize(DisplayName + ': ', ANSI_BOLD + ANSI_RED, AUseColor));
  Buffer.Append(Colorize(Message, ANSI_BOLD, AUseColor));
  Buffer.Append(sLineBreak);

  // Suggestion line
  if FSuggestion <> '' then
  begin
    Buffer.Append('  ');
    Buffer.Append(Colorize('Suggestion: ' + FSuggestion, ANSI_YELLOW, AUseColor));
    Buffer.Append(sLineBreak);
  end;

  // Location: --> file:line:column
  Buffer.Append('  ');
  Buffer.Append(Colorize(Format('--> %s:%d:%d', [FFileName, FLine, FColumn]), ANSI_CYAN, AUseColor));
  Buffer.Append(sLineBreak);

  // Source context lines
  if Assigned(FSourceLines) and (FLine > 0) and (FLine <= FSourceLines.Count) then
  begin
    FirstContextLine := FLine - CONTEXT_LINES_BEFORE;
    if FirstContextLine < 1 then
      FirstContextLine := 1;
    LastContextLine := FLine + CONTEXT_LINES_AFTER;
    if LastContextLine > FSourceLines.Count then
      LastContextLine := FSourceLines.Count;

    // Calculate gutter width based on largest line number
    GutterWidth := Length(IntToStr(LastContextLine));
    if GutterWidth < 4 then
      GutterWidth := 4;

    // Lines before
    for LineNum := FirstContextLine to FLine - 1 do
    begin
      Gutter := Format('%' + IntToStr(GutterWidth) + 'd | ', [LineNum]);
      Buffer.Append(Colorize(Gutter, ANSI_DIM, AUseColor));
      Buffer.Append(FSourceLines[LineNum - 1]);
      Buffer.Append(sLineBreak);
    end;

    // Error line (bold)
    LineStr := FSourceLines[FLine - 1];
    Gutter := Format('%' + IntToStr(GutterWidth) + 'd | ', [FLine]);
    Buffer.Append(Colorize(Gutter, ANSI_DIM, AUseColor));
    Buffer.Append(Colorize(LineStr, ANSI_BOLD, AUseColor));
    Buffer.Append(sLineBreak);

    // Caret line — preserve tabs from source so ^ aligns correctly
    CaretStr := '';
    for I := 1 to GutterWidth do
      CaretStr := CaretStr + ' ';
    CaretStr := CaretStr + ' | ';
    for I := 1 to FColumn - 1 do
      if (I <= Length(LineStr)) and (LineStr[I] = #9) then
        CaretStr := CaretStr + #9
      else
        CaretStr := CaretStr + ' ';
    Buffer.Append(Colorize(CaretStr + '^', ANSI_BOLD + ANSI_RED, AUseColor));
    Buffer.Append(sLineBreak);

    // Lines after
    for LineNum := FLine + 1 to LastContextLine do
    begin
      Gutter := Format('%' + IntToStr(GutterWidth) + 'd | ', [LineNum]);
      Buffer.Append(Colorize(Gutter, ANSI_DIM, AUseColor));
      Buffer.Append(FSourceLines[LineNum - 1]);
      Buffer.Append(sLineBreak);
    end;
  end;

  Result := Buffer.ToString;
end;

end.
