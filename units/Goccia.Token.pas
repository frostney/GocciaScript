unit Goccia.Token;

{$I Goccia.inc}

interface

uses
  Classes,
  StrUtils,
  SysUtils;

type
  TGocciaTokenType = (
    // Literals
    gttNumber, gttString, gttTemplate,
    gttTrue, gttFalse, gttNull,
    // Identifiers
    gttIdentifier,
    // Keywords
    gttConst, gttLet, gttClass, gttExtends, gttNew, gttThis, gttSuper, gttStatic,
    gttReturn, gttIf, gttElse, gttFor, gttWhile, gttDo, gttSwitch, gttCase, gttDefault, gttBreak,
    gttThrow, gttTry, gttCatch, gttFinally,
    gttImport, gttExport, gttFrom, gttAs, gttGet, gttSet, gttVar, gttWith,
    // Operators
    gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower,
    gttEqual, gttNotEqual, gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttAnd, gttOr, gttNullishCoalescing, gttNot, gttTypeof, gttInstanceof, gttIn, gttDelete, gttAssign, gttPlusAssign, gttMinusAssign,
    gttStarAssign, gttSlashAssign, gttPercentAssign, gttPowerAssign,
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
    constructor Create(const AType: TGocciaTokenType; const ALexeme: string;
      const ALine, AColumn: Integer);
    property TokenType: TGocciaTokenType read FType;
    property Lexeme: string read FLexeme;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

implementation

constructor TGocciaToken.Create(const AType: TGocciaTokenType; const ALexeme: string;
  const ALine, AColumn: Integer);
begin
  FType := AType;
  FLexeme := ALexeme;
  FLine := ALine;
  FColumn := AColumn;
end;

end.
