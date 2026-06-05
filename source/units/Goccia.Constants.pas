unit Goccia.Constants;

{$I Goccia.inc}

interface

type
  TGocciaDynamicFunctionKind = (
    dfkNormal,
    dfkGenerator,
    dfkAsync,
    dfkAsyncGenerator
  );

const
  // Literal value strings
  BOOLEAN_TRUE_LITERAL      = 'true';
  BOOLEAN_FALSE_LITERAL     = 'false';
  NULL_LITERAL              = 'null';
  UNDEFINED_LITERAL         = 'undefined';
  NAN_LITERAL               = 'NaN';
  INFINITY_LITERAL          = 'Infinity';
  NEGATIVE_INFINITY_LITERAL = '-Infinity';

  // Special identifier names
  IDENTIFIER_ARGUMENTS = 'arguments';
  USE_STRICT_DIRECTIVE = 'use strict';

  // Numeric constants
  ZERO_VALUE = 0.0;
  ONE_VALUE  = 1.0;

  EMPTY_STRING = '';

implementation

end.
