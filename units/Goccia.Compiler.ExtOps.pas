unit Goccia.Compiler.ExtOps;

{$I Goccia.inc}

interface

const
  GOCCIA_EXT_SPREAD_OBJ          = 1;  // A=target, C=source reg
  GOCCIA_EXT_OBJ_REST            = 2;  // A=dest, C=source reg, R[A+1]=exclusion keys
  GOCCIA_EXT_FINALIZE_ENUM       = 3;  // A=record (overwritten), C=name constant
  GOCCIA_EXT_DEF_GETTER          = 4;  // A=target, C=name constant, R[A+1]=closure
  GOCCIA_EXT_DEF_SETTER          = 5;  // A=target, C=name constant, R[A+1]=closure
  GOCCIA_EXT_DEF_STATIC_GETTER   = 6;  // A=target, C=name constant, R[A+1]=closure
  GOCCIA_EXT_DEF_STATIC_SETTER   = 7;  // A=target, C=name constant, R[A+1]=closure
  GOCCIA_EXT_REQUIRE_OBJECT      = 8;  // A=value (throws if null/undefined)
  GOCCIA_EXT_EVAL_CLASS          = 9;  // A=dest, C=class index
  GOCCIA_EXT_THROW_TYPE_ERROR    = 10; // C=message constant
  GOCCIA_EXT_SUPER_GET           = 11; // A=dest, C=prop constant, R[A+1]=super blueprint
  GOCCIA_EXT_SPREAD              = 12; // A=target array, C=source reg
  GOCCIA_EXT_REQUIRE_ITERABLE    = 13; // A=value (overwritten with array)

implementation

end.
