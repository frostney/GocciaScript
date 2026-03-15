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
  GOCCIA_EXT_THROW_TYPE_ERROR    = 10; // C=message constant
  GOCCIA_EXT_SUPER_GET           = 11; // A=dest, C=prop constant, R[A+1]=super blueprint
  GOCCIA_EXT_SPREAD              = 12; // A=target array, C=source reg
  GOCCIA_EXT_REQUIRE_ITERABLE    = 13; // A=value (overwritten with array)
  GOCCIA_EXT_DEFINE_GLOBAL       = 14; // A=value, C=name constant (create-or-update)

  GOCCIA_EXT_SETUP_AUTO_ACCESSOR      = 15; // A=unused, C=name constant, R[A+1]=init closure or nil
  GOCCIA_EXT_BEGIN_DECORATORS         = 16; // A=blueprint (converted to class value)
  GOCCIA_EXT_APPLY_ELEMENT_DECORATOR  = 17; // A=decorator fn, C=descriptor constant
  GOCCIA_EXT_APPLY_CLASS_DECORATOR    = 18; // A=decorator fn
  GOCCIA_EXT_FINISH_DECORATORS        = 19; // A=dest (receives wrapped class value)

  GOCCIA_EXT_DEF_COMPUTED_GETTER        = 20; // A=target, C=key reg, R[A+1]=closure
  GOCCIA_EXT_DEF_COMPUTED_SETTER        = 21; // A=target, C=key reg, R[A+1]=closure
  GOCCIA_EXT_DEF_COMPUTED_STATIC_GETTER = 22; // A=target, C=key reg, R[A+1]=closure
  GOCCIA_EXT_DEF_COMPUTED_STATIC_SETTER = 23; // A=target, C=key reg, R[A+1]=closure

  GOCCIA_EXT_SET_WRAPPED_SUPER = 24; // A=blueprint, C=super reg (stores non-blueprint super)

implementation

end.
