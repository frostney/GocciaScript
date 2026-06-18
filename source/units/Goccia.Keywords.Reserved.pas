unit Goccia.Keywords.Reserved;

{$I Goccia.inc}

interface

const
  KEYWORD_BREAK      = 'break';
  KEYWORD_CASE       = 'case';
  KEYWORD_CONTINUE   = 'continue';
  KEYWORD_CATCH      = 'catch';
  KEYWORD_CLASS      = 'class';
  KEYWORD_CONST      = 'const';
  KEYWORD_DEFAULT    = 'default';
  KEYWORD_DELETE     = 'delete';
  KEYWORD_DO         = 'do';
  KEYWORD_ELSE       = 'else';
  KEYWORD_ENUM       = 'enum';
  KEYWORD_EXPORT     = 'export';
  KEYWORD_EXTENDS    = 'extends';
  KEYWORD_FALSE      = 'false';
  KEYWORD_FINALLY    = 'finally';
  KEYWORD_FOR        = 'for';
  KEYWORD_FUNCTION   = 'function';
  KEYWORD_IF         = 'if';
  KEYWORD_IMPORT     = 'import';
  KEYWORD_IN         = 'in';
  KEYWORD_INSTANCEOF = 'instanceof';
  KEYWORD_LET        = 'let';
  KEYWORD_NEW        = 'new';
  KEYWORD_NULL       = 'null';
  KEYWORD_RETURN     = 'return';
  KEYWORD_STATIC     = 'static';
  KEYWORD_SUPER      = 'super';
  KEYWORD_SWITCH     = 'switch';
  KEYWORD_THIS       = 'this';
  KEYWORD_THROW      = 'throw';
  KEYWORD_TRUE       = 'true';
  KEYWORD_TRY        = 'try';
  KEYWORD_TYPEOF     = 'typeof';
  KEYWORD_VAR        = 'var';
  KEYWORD_VOID       = 'void';
  KEYWORD_WHILE      = 'while';
  KEYWORD_WITH       = 'with';
  KEYWORD_IMPLEMENTS = 'implements';
  KEYWORD_INTERFACE  = 'interface';
  KEYWORD_PACKAGE    = 'package';
  KEYWORD_PRIVATE    = 'private';
  KEYWORD_PROTECTED  = 'protected';
  KEYWORD_PUBLIC     = 'public';

  ReservedKeywords: array[0..35] of string = (
    KEYWORD_BREAK,
    KEYWORD_CASE,
    KEYWORD_CONTINUE,
    KEYWORD_CATCH,
    KEYWORD_CLASS,
    KEYWORD_CONST,
    KEYWORD_DEFAULT,
    KEYWORD_DELETE,
    KEYWORD_DO,
    KEYWORD_ELSE,
    KEYWORD_ENUM,
    KEYWORD_EXPORT,
    KEYWORD_EXTENDS,
    KEYWORD_FALSE,
    KEYWORD_FINALLY,
    KEYWORD_FOR,
    KEYWORD_FUNCTION,
    KEYWORD_IF,
    KEYWORD_IMPORT,
    KEYWORD_IN,
    KEYWORD_INSTANCEOF,
    KEYWORD_LET,
    KEYWORD_NEW,
    KEYWORD_NULL,
    KEYWORD_RETURN,
    KEYWORD_SUPER,
    KEYWORD_SWITCH,
    KEYWORD_THIS,
    KEYWORD_THROW,
    KEYWORD_TRUE,
    KEYWORD_TRY,
    KEYWORD_TYPEOF,
    KEYWORD_VAR,
    KEYWORD_VOID,
    KEYWORD_WHILE,
    KEYWORD_WITH
  );

  StrictModeReservedKeywords: array[0..6] of string = (
    KEYWORD_IMPLEMENTS,
    KEYWORD_INTERFACE,
    KEYWORD_PACKAGE,
    KEYWORD_PRIVATE,
    KEYWORD_PROTECTED,
    KEYWORD_PUBLIC,
    KEYWORD_STATIC
  );

function IsReservedKeyword(const AName: string): Boolean; inline;
function IsStrictModeReservedKeyword(const AName: string): Boolean; inline;

implementation

function IsReservedKeyword(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := Low(ReservedKeywords) to High(ReservedKeywords) do
    if AName = ReservedKeywords[I] then
      Exit(True);

  Result := False;
end;

function IsStrictModeReservedKeyword(const AName: string): Boolean;
var
  I: Integer;
begin
  for I := Low(StrictModeReservedKeywords) to High(StrictModeReservedKeywords) do
    if AName = StrictModeReservedKeywords[I] then
      Exit(True);

  Result := False;
end;

end.
