unit Goccia.Keywords.Contextual;

{$I Goccia.inc}

interface

const
  // Module system
  KEYWORD_AS           = 'as';
  KEYWORD_DEFER        = 'defer';
  KEYWORD_FROM         = 'from';
  KEYWORD_META         = 'meta';
  KEYWORD_SOURCE       = 'source';

  // Meta-properties
  KEYWORD_TARGET       = 'target';

  // Class bodies
  KEYWORD_STATIC       = 'static';
  KEYWORD_GET          = 'get';
  KEYWORD_SET          = 'set';
  KEYWORD_ACCESSOR     = 'accessor';

  // Async/generators
  KEYWORD_ASYNC        = 'async';
  KEYWORD_AWAIT        = 'await';
  KEYWORD_OF           = 'of';
  KEYWORD_YIELD        = 'yield';

  // Explicit Resource Management
  KEYWORD_USING        = 'using';

  // TC39 Pattern Matching
  KEYWORD_MATCH        = 'match';
  KEYWORD_IS           = 'is';
  KEYWORD_AND          = 'and';
  KEYWORD_OR           = 'or';
  KEYWORD_NOT          = 'not';

  // Types as Comments (parsed, ignored at runtime)
  KEYWORD_TYPE         = 'type';
  KEYWORD_INTERFACE    = 'interface';
  KEYWORD_IMPLEMENTS   = 'implements';
  KEYWORD_PUBLIC       = 'public';
  KEYWORD_PROTECTED    = 'protected';
  KEYWORD_PRIVATE      = 'private';
  KEYWORD_READONLY     = 'readonly';
  KEYWORD_OVERRIDE     = 'override';
  KEYWORD_ABSTRACT     = 'abstract';

  // Contextual keywords that the lexer tokenizes directly. Other contextual
  // keywords stay as identifiers so the parser can interpret them by context.
  TokenizedContextualKeywords: array[0..2] of string = (
    KEYWORD_AS,
    KEYWORD_FROM,
    KEYWORD_STATIC
  );

implementation

end.
