program Goccia.ScriptLoader.SourceRegistry.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.ScriptLoader.SourceRegistry,
  Goccia.TestSetup;

type
  TSourceRegistryTests = class(TTestSuite)
  private
    procedure TestLoadOfRegisteredNameReturnsClone;
    procedure TestLoadAfterFreeStillReturnsClone;
    procedure TestLoadOfUnregisteredFileReadsFromDisk;
    procedure TestRegisteredNameWinsOverDiskFile;
    procedure TestIsRegisteredReflectsRegistration;
    procedure TestRegisterTakesOwnership;
  public
    procedure SetupTests; override;
  end;

procedure TSourceRegistryTests.SetupTests;
begin
  Test('Load of a registered name returns a fresh clone',
    TestLoadOfRegisteredNameReturnsClone);
  Test('Cloned source can be freed without affecting subsequent loads',
    TestLoadAfterFreeStillReturnsClone);
  Test('Load of an unregistered name reads from disk',
    TestLoadOfUnregisteredFileReadsFromDisk);
  Test('Registered name takes precedence over disk file',
    TestRegisteredNameWinsOverDiskFile);
  Test('IsRegistered reflects registration state',
    TestIsRegisteredReflectsRegistration);
  Test('Register transfers ownership; destructor frees registered sources',
    TestRegisterTakesOwnership);
end;

procedure TSourceRegistryTests.TestLoadOfRegisteredNameReturnsClone;
var
  Registry: TGocciaSourceRegistry;
  Original: TStringList;
  Loaded: TStringList;
begin
  Registry := TGocciaSourceRegistry.Create;
  try
    Original := TStringList.Create;
    Original.Add('const x = 1;');
    Original.Add('const y = 2;');
    Registry.Register('<stdin>', Original);

    Loaded := Registry.Load('<stdin>');
    try
      Expect<Boolean>(Loaded <> Original).ToBe(True);
      Expect<Integer>(Loaded.Count).ToBe(2);
      Expect<string>(Loaded[0]).ToBe('const x = 1;');
      Expect<string>(Loaded[1]).ToBe('const y = 2;');
    finally
      Loaded.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TSourceRegistryTests.TestLoadAfterFreeStillReturnsClone;
var
  Registry: TGocciaSourceRegistry;
  Original: TStringList;
  FirstClone, SecondClone: TStringList;
begin
  Registry := TGocciaSourceRegistry.Create;
  try
    Original := TStringList.Create;
    Original.Add('a');
    Registry.Register('<stdin>', Original);

    FirstClone := Registry.Load('<stdin>');
    FirstClone.Free;

    SecondClone := Registry.Load('<stdin>');
    try
      Expect<Integer>(SecondClone.Count).ToBe(1);
      Expect<string>(SecondClone[0]).ToBe('a');
    finally
      SecondClone.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TSourceRegistryTests.TestLoadOfUnregisteredFileReadsFromDisk;
var
  Registry: TGocciaSourceRegistry;
  TempPath: string;
  TempStream: TFileStream;
  Text: AnsiString;
  Loaded: TStringList;
begin
  TempPath := GetTempFileName;
  Text := 'const v = 42;' + sLineBreak + 'v;';
  TempStream := TFileStream.Create(TempPath, fmCreate);
  try
    TempStream.WriteBuffer(Pointer(Text)^, Length(Text));
  finally
    TempStream.Free;
  end;

  Registry := TGocciaSourceRegistry.Create;
  try
    Loaded := Registry.Load(TempPath);
    try
      Expect<Integer>(Loaded.Count).ToBe(2);
      Expect<string>(Loaded[0]).ToBe('const v = 42;');
      Expect<string>(Loaded[1]).ToBe('v;');
    finally
      Loaded.Free;
    end;
  finally
    Registry.Free;
    DeleteFile(TempPath);
  end;
end;

procedure TSourceRegistryTests.TestRegisteredNameWinsOverDiskFile;
var
  Registry: TGocciaSourceRegistry;
  TempPath: string;
  TempStream: TFileStream;
  DiskText: AnsiString;
  RegisteredSource: TStringList;
  Loaded: TStringList;
begin
  TempPath := GetTempFileName;
  DiskText := 'from-disk';
  TempStream := TFileStream.Create(TempPath, fmCreate);
  try
    TempStream.WriteBuffer(Pointer(DiskText)^, Length(DiskText));
  finally
    TempStream.Free;
  end;

  Registry := TGocciaSourceRegistry.Create;
  try
    RegisteredSource := TStringList.Create;
    RegisteredSource.Add('from-registry');
    Registry.Register(TempPath, RegisteredSource);

    Loaded := Registry.Load(TempPath);
    try
      Expect<Integer>(Loaded.Count).ToBe(1);
      Expect<string>(Loaded[0]).ToBe('from-registry');
    finally
      Loaded.Free;
    end;
  finally
    Registry.Free;
    DeleteFile(TempPath);
  end;
end;

procedure TSourceRegistryTests.TestIsRegisteredReflectsRegistration;
var
  Registry: TGocciaSourceRegistry;
  Source: TStringList;
begin
  Registry := TGocciaSourceRegistry.Create;
  try
    Expect<Boolean>(Registry.IsRegistered('<stdin>')).ToBe(False);

    Source := TStringList.Create;
    Source.Add('x');
    Registry.Register('<stdin>', Source);

    Expect<Boolean>(Registry.IsRegistered('<stdin>')).ToBe(True);
    Expect<Boolean>(Registry.IsRegistered('something-else')).ToBe(False);
  finally
    Registry.Free;
  end;
end;

procedure TSourceRegistryTests.TestRegisterTakesOwnership;
var
  Registry: TGocciaSourceRegistry;
  Source: TStringList;
begin
  Registry := TGocciaSourceRegistry.Create;

  Source := TStringList.Create;
  Source.Add('owned-by-registry');
  Registry.Register('<stdin>', Source);

  // No explicit Source.Free — the registry must free it on Destroy.
  // If ownership leaked, FPC's heap trace would flag this as a leak when
  // run with -gh.
  Registry.Free;

  Expect<Boolean>(True).ToBe(True);
end;

begin
  TestRunnerProgram.AddSuite(TSourceRegistryTests.Create('ScriptLoader SourceRegistry'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
