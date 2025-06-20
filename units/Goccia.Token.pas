unit Goccia.Token;

{$I Goccia.inc}

interface

uses
  SysUtils, StrUtils, Classes;

type
  TGocciaTokenType = (
    // Literals
    gttNumber, gttString, gttTemplate, gttTemplateStart, gttTemplateMiddle, gttTemplateEnd,
    gttTrue, gttFalse, gttNull, gttUndefined,
    // Identifiers
    gttIdentifier,
    // Keywords
    gttConst, gttLet, gttClass, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
    gttReturn, gttIf, gttElse, gttThrow, gttTry, gttCatch, gttFinally,
    gttImport, gttExport, gttFrom, gttAs, gttDefault,
    // Operators
    gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower,
    gttEqual, gttNotEqual, gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttAnd, gttOr, gttNullishCoalescing, gttNot, gttTypeof, gttInstanceof, gttAssign, gttPlusAssign, gttMinusAssign,
    gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign,
    gttIncrement, gttDecrement,
    gttQuestion, gttColon,
    // Bitwise operators
    gttBitwiseAnd, gttBitwiseOr, gttBitwiseXor, gttBitwiseNot,
    gttLeftShift, gttRightShift, gttUnsignedRightShift,
    gttBitwiseAndAssign, gttBitwiseOrAssign, gttBitwiseXorAssign,
    gttLeftShiftAssign, gttRightShiftAssign, gttUnsignedRightShiftAssign,
    // Punctuation
    gttLeftParen, gttRightParen, gttLeftBrace, gttRightBrace,
    gttLeftBracket, gttRightBracket, gttComma, gttDot, gttSemicolon,
    gttArrow, gttSpread, gttHash,
    // Special
    gttEOF, gttNewLine
  );

  TGocciaToken = class
  private
    FType: TGocciaTokenType;
    FLexeme: string;
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(AType: TGocciaTokenType; const ALexeme: string;
      ALine, AColumn: Integer);
    property TokenType: TGocciaTokenType read FType;
    property Lexeme: string read FLexeme;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

implementation

constructor TGocciaToken.Create(AType: TGocciaTokenType; const ALexeme: string;
  ALine, AColumn: Integer);
begin
  FType := AType;
  FLexeme := ALexeme;
  FLine := ALine;
  FColumn := AColumn;
end;

end.
