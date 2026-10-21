unit Goccia.Bytecode.Profile;

{$I Goccia.inc}

interface

const
  { These are wire assignments, independent of Pascal set layout. Adding a
    language flag requires an explicit assignment and a format revision. }
  BYTECODE_COMPAT_ASI = UInt32(1) shl 0;
  BYTECODE_COMPAT_VAR = UInt32(1) shl 1;
  BYTECODE_COMPAT_FUNCTION = UInt32(1) shl 2;
  BYTECODE_COMPAT_TRADITIONAL_FOR = UInt32(1) shl 3;
  BYTECODE_COMPAT_WHILE = UInt32(1) shl 4;
  BYTECODE_COMPAT_LOOSE_EQUALITY = UInt32(1) shl 5;
  BYTECODE_COMPAT_NON_STRICT = UInt32(1) shl 6;
  BYTECODE_COMPAT_ARGUMENTS = UInt32(1) shl 7;
  BYTECODE_COMPAT_LABEL = UInt32(1) shl 8;
  BYTECODE_COMPAT_FOR_IN = UInt32(1) shl 9;
  BYTECODE_COMPAT_MODULE_SOURCE = UInt32(1) shl 10;
  BYTECODE_COMPAT_KNOWN_MASK = UInt32($7FF);
  BYTECODE_PREPROCESSOR_JSX = UInt32(1);
  BYTECODE_PREPROCESSOR_KNOWN_MASK = BYTECODE_PREPROCESSOR_JSX;

type
  TGocciaBytecodeSourceType = (bstScript, bstModule);
  TGocciaBytecodeExecutionKind = (bekStandalone, bekLinkedModule);

  { Language settings are distinct from the binding layout of an artifact.
    Effective parser switches include unsupported-feature recovery: that
    option changes the generated program, not just diagnostic output. }
  TGocciaCompileProfile = record
    CompatibilityMask: UInt32;
    PreprocessorMask: UInt32;
    StrictTypes: Boolean;
    WarningUnsupportedFeatures: Boolean;
    SourceType: TGocciaBytecodeSourceType;
    function Matches(const AOther: TGocciaCompileProfile): Boolean;
    procedure Validate;
  end;

implementation

uses
  SysUtils;

function TGocciaCompileProfile.Matches(
  const AOther: TGocciaCompileProfile): Boolean;
begin
  Result := (CompatibilityMask = AOther.CompatibilityMask) and
    (PreprocessorMask = AOther.PreprocessorMask) and
    (StrictTypes = AOther.StrictTypes) and
    (WarningUnsupportedFeatures = AOther.WarningUnsupportedFeatures) and
    (SourceType = AOther.SourceType);
end;

procedure TGocciaCompileProfile.Validate;
begin
  if (CompatibilityMask and not BYTECODE_COMPAT_KNOWN_MASK) <> 0 then
    raise Exception.Create('Unknown bytecode compatibility profile flags');
  if (PreprocessorMask and not BYTECODE_PREPROCESSOR_KNOWN_MASK) <> 0 then
    raise Exception.Create('Unknown bytecode preprocessor profile flags');
  if not (SourceType in [bstScript, bstModule]) then
    raise Exception.Create('Invalid bytecode source type');
end;

end.
