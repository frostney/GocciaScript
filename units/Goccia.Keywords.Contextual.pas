unit Goccia.Keywords.Contextual;

{$I Goccia.inc}

interface

const
  // Module system
  KEYWORD_AS           = 'as';
  KEYWORD_FROM         = 'from';

  // Class bodies
  KEYWORD_STATIC       = 'static';
  KEYWORD_GET          = 'get';
  KEYWORD_SET          = 'set';
  KEYWORD_ACCESSOR     = 'accessor';

  // Async/generators
  KEYWORD_AWAIT        = 'await';
  KEYWORD_OF           = 'of';
  KEYWORD_YIELD        = 'yield';

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

implementation

end.
