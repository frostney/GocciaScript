unit Goccia.Bytecode.Profile.Options;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Profile,
  Goccia.SourcePipeline;

function BuildBytecodeCompileProfile(
  const AOptions: TGocciaSourcePipelineOptions;
  const AStrictTypes: Boolean): TGocciaCompileProfile;

implementation

uses
  SysUtils;

function BuildBytecodeCompileProfile(
  const AOptions: TGocciaSourcePipelineOptions;
  const AStrictTypes: Boolean): TGocciaCompileProfile;
begin
  Result.CompatibilityMask := 0;
  Result.PreprocessorMask := 0;
  Result.StrictTypes := AStrictTypes;
  Result.WarningUnsupportedFeatures := AOptions.WarningUnsupportedFeatures;
  case AOptions.SourceType of
    stScript: Result.SourceType := bstScript;
    stModule: Result.SourceType := bstModule;
  else
    raise Exception.Create('Bytecode compilation requires a settled source type');
  end;
  if ppJSX in AOptions.Preprocessors then
    Result.PreprocessorMask := BYTECODE_PREPROCESSOR_JSX;
  if cfASI in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_ASI;
  if cfVar in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_VAR;
  if cfFunction in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_FUNCTION;
  if cfTraditionalFor in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_TRADITIONAL_FOR;
  if cfWhileLoops in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_WHILE;
  if cfLooseEquality in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_LOOSE_EQUALITY;
  if cfNonStrictMode in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_NON_STRICT;
  if cfArgumentsObject in AOptions.Compatibility then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_ARGUMENTS;
  if AOptions.LabelStatementsEnabled or (cfLabel in AOptions.Compatibility) then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_LABEL;
  if AOptions.ForInLoopsEnabled or (cfForIn in AOptions.Compatibility) then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_FOR_IN;
  if AOptions.ExperimentalJSModuleSourceEnabled or
     (cfExperimentalJSModuleSource in AOptions.Compatibility) then
    Result.CompatibilityMask := Result.CompatibilityMask or BYTECODE_COMPAT_MODULE_SOURCE;
end;

end.
