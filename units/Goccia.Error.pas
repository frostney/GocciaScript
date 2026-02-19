unit Goccia.Error;

{$I Goccia.inc}

interface

uses
  Classes,
  StrUtils,
  SysUtils,

  Goccia.Error.ThrowErrorCallback;

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
    FSeverity: TGocciaErrorSeverity;
    FSuggestion: string;
  public
    constructor Create(const AMessage: string; const ALine, AColumn: Integer;
      const AFileName: string; const ASourceLines: TStringList);
    procedure SetSuggestion(const ASuggestion: string);
    function GetDetailedMessage: string;
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

implementation

constructor TGocciaError.Create(const AMessage: string; const ALine, AColumn: Integer;
  const AFileName: string; const ASourceLines: TStringList);
begin
  inherited Create(AMessage);
  FLine := ALine;
  FColumn := AColumn;
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FSeverity := gesError;
  FSuggestion := '';
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
  FSourceLines := AOriginalSourceLines;
end;

function TGocciaError.GetDetailedMessage: string;
var
  FormattedMessage: string;
  LineStr: string;
  Caret: string;
  I: Integer;
begin
  FormattedMessage := Format('%s: %s%s', [
    ClassName,
    Message,
    IfThen(FSuggestion <> '', Format(' (%s)', [FSuggestion]), '')
  ]) + sLineBreak;

  FormattedMessage := FormattedMessage + Format('  --> %s:%d:%d', [FFileName, FLine, FColumn]) + sLineBreak;

  // Show context lines
  if Assigned(FSourceLines) and (FLine > 0) and (FLine <= FSourceLines.Count) then
  begin
    // Previous line
    if FLine > 1 then
      FormattedMessage := FormattedMessage + Format('%4d | %s', [FLine - 1, FSourceLines[FLine - 2]]) + sLineBreak;

    // Current line
    LineStr := FSourceLines[FLine - 1];
    FormattedMessage := FormattedMessage + Format('%4d | %s', [FLine, LineStr]) + sLineBreak;

    // Caret
    Caret := '     | ';
    for I := 1 to FColumn - 1 do
      Caret := Caret + ' ';
    Caret := Caret + '^';
    FormattedMessage := FormattedMessage + Caret + sLineBreak;

    // Next line
    if FLine < FSourceLines.Count then
      FormattedMessage := FormattedMessage + Format('%4d | %s', [FLine + 1, FSourceLines[FLine]]) + sLineBreak;
  end;

  Result := FormattedMessage;
end;

end.
