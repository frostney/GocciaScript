unit Goccia.Token;

{$I Goccia.inc}

interface

uses
  Goccia.SourceSpan;

type
  TGocciaTokenType = (
    // Literals
    gttNumber, gttBigInt, gttString, gttTemplate, gttTemplateHead,
    gttTemplateMiddle, gttTemplateTail, gttRegex,
    gttTrue, gttFalse, gttNull,
    // Identifiers
    gttIdentifier,
    // Keywords
    gttConst, gttLet, gttClass, gttEnum, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
    gttReturn, gttIf, gttElse, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak, gttContinue,
    gttThrow, gttTry, gttCatch, gttFinally,
    gttImport, gttExport, gttFrom, gttAs, gttVar, gttWith, gttFunction,
    // Operators
    gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower,
    gttEqual, gttNotEqual, gttLooseEqual, gttLooseNotEqual, gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttAnd, gttOr, gttNullishCoalescing, gttNot, gttTypeof, gttVoid, gttInstanceof, gttIn, gttDelete, gttAssign, gttPlusAssign, gttMinusAssign,
    gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign, gttNullishCoalescingAssign, gttLogicalAndAssign, gttLogicalOrAssign,
    gttIncrement, gttDecrement,
    gttQuestion, gttColon, gttOptionalChaining,
    // Bitwise operators
    gttBitwiseAnd, gttBitwiseOr, gttBitwiseXor, gttBitwiseNot,
    gttLeftShift, gttRightShift, gttUnsignedRightShift,
    gttBitwiseAndAssign, gttBitwiseOrAssign, gttBitwiseXorAssign,
    gttLeftShiftAssign, gttRightShiftAssign, gttUnsignedRightShiftAssign,
    // Punctuation
    gttLeftParen, gttRightParen, gttLeftBrace, gttRightBrace,
    gttLeftBracket, gttRightBracket, gttComma, gttDot, gttSemicolon,
    gttArrow, gttSpread, gttHash, gttAt,
    // Special
    gttEOF
  );

  TGocciaTemplateTokenValue = record
    Cooked: string;
    Raw: string;
    CookedValid: Boolean;
  end;

  TGocciaToken = class
  private
    FType: TGocciaTokenType;
    FLexeme: string;
    FSpan: TGocciaSourceSpan;
    FContainsEscape: Boolean;
    FTemplateValue: TGocciaTemplateTokenValue;
    function GetLine: Integer;
    function GetColumn: Integer;
    function GetEndColumn: Integer;
  public
    constructor Create(const AType: TGocciaTokenType; const ALexeme: string;
      const ACoordinates: IGocciaSourceCoordinates;
      const AStartOffset, AEndOffset: Integer;
      const AContainsEscape: Boolean = False);
    constructor CreateTemplate(const AType: TGocciaTokenType;
      const ACooked, ARaw: string; const ACookedValid: Boolean;
      const ACoordinates: IGocciaSourceCoordinates;
      const AStartOffset, AEndOffset: Integer);
    property TokenType: TGocciaTokenType read FType;
    property Lexeme: string read FLexeme;
    property Line: Integer read GetLine;
    property Column: Integer read GetColumn;
    property EndColumn: Integer read GetEndColumn;
    property Span: TGocciaSourceSpan read FSpan;
    property ContainsEscape: Boolean read FContainsEscape;
    property TemplateValue: TGocciaTemplateTokenValue read FTemplateValue;
  end;

implementation

constructor TGocciaToken.Create(const AType: TGocciaTokenType; const ALexeme: string;
  const ACoordinates: IGocciaSourceCoordinates;
  const AStartOffset, AEndOffset: Integer;
  const AContainsEscape: Boolean);
begin
  FType := AType;
  FLexeme := ALexeme;
  FSpan := TGocciaSourceSpan.InSource(ACoordinates, AStartOffset, AEndOffset);
  FContainsEscape := AContainsEscape;
end;

constructor TGocciaToken.CreateTemplate(const AType: TGocciaTokenType;
  const ACooked, ARaw: string; const ACookedValid: Boolean;
  const ACoordinates: IGocciaSourceCoordinates;
  const AStartOffset, AEndOffset: Integer);
begin
  FType := AType;
  FLexeme := '';
  FSpan := TGocciaSourceSpan.InSource(ACoordinates, AStartOffset, AEndOffset);
  FContainsEscape := False;
  FTemplateValue.Cooked := ACooked;
  FTemplateValue.Raw := ARaw;
  FTemplateValue.CookedValid := ACookedValid;
end;

function TGocciaToken.GetLine: Integer;
begin
  Result := FSpan.EndLine;
end;

function TGocciaToken.GetColumn: Integer;
begin
  Result := FSpan.StartColumn;
end;

function TGocciaToken.GetEndColumn: Integer;
begin
  Result := FSpan.EndColumn;
end;

end.
