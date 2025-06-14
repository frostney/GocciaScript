unit Goccia.Token;

{$I Goccia.inc}

interface

uses
  SysUtils, StrUtils, Classes;

type
  TGocciaTokenType = (
    // Literals
    gttNumber, gttString, gttTrue, gttFalse, gttNull, gttUndefined,
    // Identifiers
    gttIdentifier,
    // Keywords
    gttConst, gttLet, gttClass, gttExtends, gttNew, gttThis, gttSuper,
    gttReturn, gttIf, gttElse, gttThrow, gttTry, gttCatch, gttFinally,
    gttImport, gttExport, gttFrom, gttAs, gttDefault,
    // Operators
    gttPlus, gttMinus, gttStar, gttSlash, gttPercent, gttPower,
    gttEqual, gttNotEqual, gttLess, gttGreater, gttLessEqual, gttGreaterEqual,
    gttAnd, gttOr, gttNot, gttTypeof, gttAssign, gttPlusAssign, gttMinusAssign,
    gttQuestion, gttColon,
    // Punctuation
    gttLeftParen, gttRightParen, gttLeftBrace, gttRightBrace,
    gttLeftBracket, gttRightBracket, gttComma, gttDot, gttSemicolon,
    gttArrow, gttSpread,
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
