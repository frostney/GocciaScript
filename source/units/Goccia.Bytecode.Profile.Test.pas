program Goccia.Bytecode.Profile.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Bytecode,
  Goccia.Bytecode.Binary,
  Goccia.Bytecode.Chunk,
  Goccia.Bytecode.Debug,
  Goccia.Bytecode.Module,
  Goccia.Bytecode.Profile,
  Goccia.Bytecode.Profile.Options,
  Goccia.Modules.LinkPlan,
  Goccia.SourcePipeline;

const
  TEST_RUNTIME_TAG = 'test';
  TEST_SOURCE_PATH = '/compiled/library.js';
  { Prefix preceding the profile: magic, version, two UTF-16 strings and
    HasDebugInfo. Mutation tests deliberately exercise the public wire format. }
  PROFILE_OFFSET = 6 + 4 + 2 * Length(TEST_RUNTIME_TAG) +
    4 + 2 * Length(TEST_SOURCE_PATH) + 1;

type
  TBytecodeProfileTests = class(TTestSuite)
  private
    procedure TestBufferOwnsLoadedData;
    procedure TestRejectsEveryTruncatedPrefix;
    procedure TestRejectsInvalidBufferBounds;
    procedure TestRejectsUnknownProfileAndBoolean;
    procedure TestEffectiveProfileFlags;
    procedure TestLinkedPlanRoundTrip;
    procedure TestRelocatesNestedSources;
    procedure TestRequiresExplicitCompileProfile;
  public
    procedure SetupTests; override;
  end;

function MakeModule: TGocciaBytecodeModule;
begin
  Result := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  Result.CompileProfile := Default(TGocciaCompileProfile);
  Result.TopLevel := TGocciaFunctionTemplate.Create('main');
  Result.TopLevel.MaxRegisters := 2;
  Result.TopLevel.StrictCode := False;
  Result.TopLevel.StrictThis := True;
  Result.TopLevel.SourceText := 'const value = "buffer";';
  Result.TopLevel.DerivedThisInitializedSlot := 1;
  Result.TopLevel.DebugInfo := TGocciaDebugInfo.Create(TEST_SOURCE_PATH);
  Result.TopLevel.EmitInstruction(EncodeABC(OP_LOAD_UNDEFINED, 0, 0, 0));
end;

function Rejection(const AData: Pointer; const ASize: SizeInt): string;
var
  Module: TGocciaBytecodeModule;
begin
  Result := '';
  try
    Module := LoadModuleFromBuffer(AData, ASize);
    Module.Free;
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

procedure TBytecodeProfileTests.SetupTests;
begin
  Test('Loaded buffer owns source and strictness metadata', TestBufferOwnsLoadedData);
  Test('Rejects every truncated buffer prefix', TestRejectsEveryTruncatedPrefix);
  Test('Rejects nil, negative and oversized buffers before reading', TestRejectsInvalidBufferBounds);
  Test('Rejects unknown profile bits and noncanonical booleans', TestRejectsUnknownProfileAndBoolean);
  Test('Profiles compare effective language flags and settled goals', TestEffectiveProfileFlags);
  Test('Linked module plan and hoisted function names round-trip', TestLinkedPlanRoundTrip);
  Test('Relocation updates nested function import referrers', TestRelocatesNestedSources);
  Test('Serialization requires an explicitly supplied compilation profile', TestRequiresExplicitCompileProfile);
end;

procedure TBytecodeProfileTests.TestBufferOwnsLoadedData;
var
  Bytes: TBytes;
  Module, Loaded: TGocciaBytecodeModule;
  Profile: TGocciaCompileProfile;
begin
  Module := MakeModule;
  try
    Profile := Module.CompileProfile;
    Profile.CompatibilityMask := BYTECODE_COMPAT_NON_STRICT or BYTECODE_COMPAT_ARGUMENTS;
    Profile.PreprocessorMask := BYTECODE_PREPROCESSOR_JSX;
    Profile.StrictTypes := True;
    Profile.WarningUnsupportedFeatures := True;
    Module.CompileProfile := Profile;
    Bytes := SaveModuleToBuffer(Module);
  finally
    Module.Free;
  end;
  Loaded := LoadModuleFromBuffer(Bytes);
  try
    FillChar(Bytes[0], Length(Bytes), $FF);
    Bytes := nil;
    Expect<Boolean>(Loaded.CompileProfile.Matches(Profile)).ToBe(True);
    Expect<Boolean>(Loaded.WasLoadedFromBytecode).ToBe(True);
    Expect<Boolean>(Loaded.TopLevel.StrictCode).ToBe(False);
    Expect<Boolean>(Loaded.TopLevel.StrictThis).ToBe(True);
    Expect<string>(Loaded.TopLevel.SourceText).ToBe('const value = "buffer";');
    Expect<Integer>(Loaded.TopLevel.DerivedThisInitializedSlot).ToBe(1);
    Expect<string>(Loaded.SourcePath).ToBe(TEST_SOURCE_PATH);
  finally
    Loaded.Free;
  end;
end;

procedure TBytecodeProfileTests.TestRejectsEveryTruncatedPrefix;
var
  Bytes: TBytes;
  Module: TGocciaBytecodeModule;
  I: Integer;
begin
  Module := MakeModule;
  try
    Bytes := SaveModuleToBuffer(Module);
  finally
    Module.Free;
  end;
  for I := 0 to Length(Bytes) - 1 do
    Expect<Boolean>(Rejection(@Bytes[0], I) <> '').ToBe(True);
  { A huge UTF-16 string declaration must fail before SetLength. }
  for I := 6 to 9 do
    Bytes[I] := $FF;
  Expect<Boolean>(Rejection(@Bytes[0], Length(Bytes)) <> '').ToBe(True);
end;

procedure TBytecodeProfileTests.TestRejectsInvalidBufferBounds;
var
  ByteValue: Byte;
begin
  ByteValue := 0;
  Expect<Boolean>(Rejection(nil, 1) <> '').ToBe(True);
  Expect<Boolean>(Rejection(@ByteValue, -1) <> '').ToBe(True);
  Expect<Boolean>(Rejection(@ByteValue, 64 * 1024 * 1024 + 1) <> '').ToBe(True);
  Expect<Boolean>(Rejection(nil, 0) <> '').ToBe(True);
end;

procedure TBytecodeProfileTests.TestRejectsUnknownProfileAndBoolean;
var
  Bytes: TBytes;
  Module: TGocciaBytecodeModule;
begin
  Module := MakeModule;
  try
    Bytes := SaveModuleToBuffer(Module);
  finally
    Module.Free;
  end;
  Bytes[PROFILE_OFFSET + 3] := $80;
  Expect<Boolean>(Pos('profile', Rejection(@Bytes[0], Length(Bytes))) > 0).ToBe(True);
  Bytes[PROFILE_OFFSET + 3] := 0;
  Bytes[PROFILE_OFFSET + 4] := $80;
  Expect<Boolean>(Pos('profile', Rejection(@Bytes[0], Length(Bytes))) > 0).ToBe(True);
  Bytes[PROFILE_OFFSET + 4] := 0;
  Bytes[PROFILE_OFFSET + 8] := 2;
  Expect<Boolean>(Pos('boolean', Rejection(@Bytes[0], Length(Bytes))) > 0).ToBe(True);
  Bytes[PROFILE_OFFSET + 8] := 0;
  Bytes[PROFILE_OFFSET + 9] := 2;
  Expect<Boolean>(Pos('boolean', Rejection(@Bytes[0], Length(Bytes))) > 0).ToBe(True);
end;

procedure TBytecodeProfileTests.TestEffectiveProfileFlags;
var
  Options: TGocciaSourcePipelineOptions;
  First, Second: TGocciaCompileProfile;
  Raised: Boolean;
begin
  Options := TGocciaSourcePipeline.DefaultOptions;
  Options.LabelStatementsEnabled := True;
  First := BuildBytecodeCompileProfile(Options, False);
  Options.LabelStatementsEnabled := False;
  Options.Compatibility := [cfLabel];
  Second := BuildBytecodeCompileProfile(Options, False);
  Expect<Boolean>(First.Matches(Second)).ToBe(True);
  Options.WarningUnsupportedFeatures := True;
  Second := BuildBytecodeCompileProfile(Options, False);
  Expect<Boolean>(First.Matches(Second)).ToBe(False);
  Options.WarningUnsupportedFeatures := False;
  Options.Compatibility := [cfLabel, cfNonStrictMode];
  Second := BuildBytecodeCompileProfile(Options, False);
  Expect<Boolean>(First.Matches(Second)).ToBe(False);
  Options.Compatibility := [cfLabel];
  Options.SourceType := stModule;
  Second := BuildBytecodeCompileProfile(Options, False);
  Expect<Boolean>(First.Matches(Second)).ToBe(False);
  Options.SourceType := stDetect;
  Raised := False;
  try
    Second := BuildBytecodeCompileProfile(Options, False);
  except
    on E: Exception do
      Raised := True;
  end;
  Expect<Boolean>(Raised).ToBe(True);
end;

procedure TBytecodeProfileTests.TestLinkedPlanRoundTrip;
var
  Bytes: TBytes;
  Module, Loaded: TGocciaBytecodeModule;
  Child: TGocciaFunctionTemplate;
  Profile: TGocciaCompileProfile;
begin
  Module := MakeModule;
  try
    Profile := Module.CompileProfile;
    Profile.SourceType := bstModule;
    Module.CompileProfile := Profile;
    Module.ExecutionKind := bekLinkedModule;
    Module.GlobalBackedTopLevel := True;
    Module.TopLevel.StrictCode := True;
    Module.LinkPlan := TGocciaModuleLinkPlan.Create;
    SetLength(Module.LinkPlan.Bindings, 1);
    Module.LinkPlan.Bindings[0].Name := 'read';
    Module.LinkPlan.Bindings[0].Kind := lbLet;
    SetLength(Module.LinkPlan.Declarations, 1);
    Module.LinkPlan.Declarations[0].Kind := ldLocalExport;
    SetLength(Module.LinkPlan.Declarations[0].Pairs, 1);
    Module.LinkPlan.Declarations[0].Pairs[0].Name := 'read';
    Module.LinkPlan.Declarations[0].Pairs[0].Value := 'read';
    Child := TGocciaFunctionTemplate.Create('read');
    Child.ModuleEnvironmentBinding := 'read';
    Module.TopLevel.AddFunction(Child);
    Bytes := SaveModuleToBuffer(Module);
  finally
    Module.Free;
  end;
  Loaded := LoadModuleFromBuffer(Bytes);
  try
    Expect<Boolean>(Loaded.ExecutionKind = bekLinkedModule).ToBe(True);
    Expect<string>(Loaded.TopLevel.GetFunction(0).ModuleEnvironmentBinding).ToBe('read');
    Expect<string>(Loaded.LinkPlan.Declarations[0].Pairs[0].Value).ToBe('read');
  finally
    Loaded.Free;
  end;
end;

procedure TBytecodeProfileTests.TestRelocatesNestedSources;
var
  Module: TGocciaBytecodeModule;
  Child: TGocciaFunctionTemplate;
begin
  Module := MakeModule;
  try
    Child := TGocciaFunctionTemplate.Create('child');
    Child.DebugInfo := TGocciaDebugInfo.Create(TEST_SOURCE_PATH);
    Module.TopLevel.AddFunction(Child);
    Module.RelocateSourcePath('/cache/library.js');
    Expect<string>(Module.SourcePath).ToBe('/cache/library.js');
    Expect<string>(Module.TopLevel.DebugInfo.SourceFile).ToBe('/cache/library.js');
    Expect<string>(Child.DebugInfo.SourceFile).ToBe('/cache/library.js');
  finally
    Module.Free;
  end;
end;

procedure TBytecodeProfileTests.TestRequiresExplicitCompileProfile;
var
  Module: TGocciaBytecodeModule;
  Bytes: TBytes;
  Reason: string;
begin
  Module := TGocciaBytecodeModule.Create(TEST_RUNTIME_TAG, TEST_SOURCE_PATH);
  try
    Module.TopLevel := TGocciaFunctionTemplate.Create('main');
    Expect<Boolean>(Module.HasCompileProfile).ToBe(False);
    Expect<Boolean>(Module.WasLoadedFromBytecode).ToBe(False);
    Reason := '';
    try
      Bytes := SaveModuleToBuffer(Module);
    except
      on E: Exception do
        Reason := E.Message;
    end;
    Expect<Boolean>(Pos('profile is required', Reason) > 0).ToBe(True);
    Module.CompileProfile := Default(TGocciaCompileProfile);
    Expect<Boolean>(Module.HasCompileProfile).ToBe(True);
    Bytes := SaveModuleToBuffer(Module);
    Expect<Boolean>(Length(Bytes) > 0).ToBe(True);
  finally
    Module.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TBytecodeProfileTests.Create('Bytecode Profile'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
