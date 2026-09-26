program Goccia.CLI.Permissions.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  CLI.ConfigFile,
  CLI.Options,
  CLI.Parser,
  FileUtils,
  TestingPascalLibrary,

  Goccia.Capabilities,
  Goccia.CLI.Application,
  Goccia.CLI.Options,
  Goccia.CLI.Permissions;

type
  TPermissionsTests = class(TTestSuite)
  private
    FRoot: string;
    function WriteConfig(const ARelativePath, AText: string): string;
    function ReadRequest(const APath: string): TGocciaConfigPermissionRequest;
    function RequestError(const APath: string): string;
    procedure TestValuesTrueArrayFalse;
    procedure TestUnknownKeysRejected;
    procedure TestValueShapesRejected;
    procedure TestRelativeScopesResolveAgainstDeclaringFile;
    procedure TestChildOverridesBasePerKey;
    procedure TestNodeModulesCeiling;
    procedure TestInvalidScopeRejected;
    procedure TestJSON5AndTOMLFlatten;
    procedure TestRequestsGrants;
    procedure TestResolveDenyWinsEitherOrder;
    procedure TestResolveCommandLineDenySubtractsConfigAllow;
    procedure TestResolveFiltersUnhonoredRequests;
    procedure TestUnhonoredCommandLineAllowRaises;
    procedure TestCommandLineScopesResolveAgainstWorkingDirectory;
    procedure TestTryHandleCapabilityArgument;
    procedure TestDescribeCapabilities;
  protected
    procedure BeforeAll; override;
    procedure AfterAll; override;
  public
    procedure SetupTests; override;
  end;

procedure TPermissionsTests.SetupTests;
begin
  Test('Values: true, arrays, and false', TestValuesTrueArrayFalse);
  Test('Unknown permission keys are rejected', TestUnknownKeysRejected);
  Test('Values of the wrong shape are rejected', TestValueShapesRejected);
  Test('Relative scopes resolve against the declaring file',
    TestRelativeScopesResolveAgainstDeclaringFile);
  Test('A child config overrides its base key by key',
    TestChildOverridesBasePerKey);
  Test('node_modules=<dir> resolves its directory', TestNodeModulesCeiling);
  Test('A scope the capability rejects names the config',
    TestInvalidScopeRejected);
  Test('JSON5 and TOML permissions blocks flatten like JSON',
    TestJSON5AndTOMLFlatten);
  Test('A deny-only block requests no grants', TestRequestsGrants);
  Test('Resolution: deny wins in either order', TestResolveDenyWinsEitherOrder);
  Test('Resolution: a command-line deny subtracts from a config allow',
    TestResolveCommandLineDenySubtractsConfigAllow);
  Test('Resolution: requests a binary cannot honor are filtered',
    TestResolveFiltersUnhonoredRequests);
  Test('An unhonored command-line allow is a usage error',
    TestUnhonoredCommandLineAllowRaises);
  Test('Command-line scopes resolve against the working directory',
    TestCommandLineScopesResolveAgainstWorkingDirectory);
  Test('Custom argument parsers recognize the capability grammar',
    TestTryHandleCapabilityArgument);
  Test('DescribeCapabilities lists capabilities in prose',
    TestDescribeCapabilities);
end;

procedure TPermissionsTests.BeforeAll;
begin
  inherited BeforeAll;
  Randomize;
  FRoot := ExcludeTrailingPathDelimiter(ExpandFileName(
    IncludeTrailingPathDelimiter(GetTempDir(False)) + 'goccia-permissions-' +
    IntToStr(Random(MaxInt))));
  ForceDirectories(FRoot);
  EnsureConfigParsersRegistered;
end;

procedure DeleteDirectoryTree(const APath: string);
var
  SearchRec: TSearchRec;
  EntryPath: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(APath) + '*', faAnyFile,
    SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      EntryPath := IncludeTrailingPathDelimiter(APath) + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = faDirectory then
        DeleteDirectoryTree(EntryPath)
      else
        DeleteFile(EntryPath);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  RemoveDir(APath);
end;

procedure TPermissionsTests.AfterAll;
begin
  DeleteDirectoryTree(FRoot);
  inherited AfterAll;
end;

function TPermissionsTests.WriteConfig(const ARelativePath,
  AText: string): string;
begin
  Result := FRoot + PathDelim + ARelativePath;
  ForceDirectories(ExtractFileDir(Result));
  WriteUTF8FileText(Result, AText);
end;

function TPermissionsTests.ReadRequest(
  const APath: string): TGocciaConfigPermissionRequest;
begin
  Result := ReadConfigPermissionRequest(ParseConfigFile(APath), APath);
end;

function TPermissionsTests.RequestError(const APath: string): string;
begin
  Result := '';
  try
    ReadRequest(APath);
  except
    on E: Exception do
      Result := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TPermissionsTests.TestValuesTrueArrayFalse;
var
  Request: TGocciaConfigPermissionRequest;
  Path: string;
begin
  Path := WriteConfig('values/goccia.json', '{"permissions": {' +
    '"allow-ffi": true, "allow-net": ["a.test", "10.0.0.0/8"], ' +
    '"deny-net": false, "deny-read": []}}');
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.Allow[gcFFI].Unscoped).ToBe(True);
  Expect<Integer>(Length(Request.Allow[gcNet].Scopes)).ToBe(2);
  Expect<string>(Request.Allow[gcNet].Scopes[1]).ToBe('10.0.0.0/8');
  Expect<Boolean>(Request.Deny[gcNet].Declared).ToBe(True);
  Expect<Boolean>(Request.Deny[gcNet].RequestsAny).ToBe(False);
  Expect<Boolean>(Request.Deny[gcRead].Declared).ToBe(True);
  Expect<Boolean>(Request.Deny[gcRead].RequestsAny).ToBe(False);
  Expect<Boolean>(Request.Allow[gcRead].Declared).ToBe(False);
end;

procedure TPermissionsTests.TestUnknownKeysRejected;
var
  Path: string;
begin
  Path := WriteConfig('typo/goccia.json',
    '{"permissions": {"deny-nett": true}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': unknown permission "deny-nett" (valid: allow-read, allow-net, ' +
    'allow-ffi, allow-import, deny-read, deny-net, deny-ffi, deny-import)');
  Path := WriteConfig('case/goccia.json',
    '{"permissions": {"Allow-Net": true}}');
  Expect<Boolean>(Pos('unknown permission "Allow-Net"',
    RequestError(Path)) > 0).ToBe(True);
end;

procedure TPermissionsTests.TestValueShapesRejected;
var
  Path: string;
begin
  Path := WriteConfig('scalar/goccia.json',
    '{"permissions": {"allow-net": "a.test"}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "permissions.allow-net" must be true, false, or an array of ' +
    'strings');
  Path := WriteConfig('number/goccia.json',
    '{"permissions": {"allow-net": [8080]}}');
  Expect<Boolean>(Pos('must be true, false, or an array of strings',
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('import-true/goccia.json',
    '{"permissions": {"allow-import": true}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "permissions.allow-import" needs scopes: node_modules[=<dir>] ' +
    'or a provider such as github');
  Path := WriteConfig('not-object/goccia.json', '{"permissions": true}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "permissions" must be an object of allow-* and deny-* keys');
end;

procedure TPermissionsTests.TestRelativeScopesResolveAgainstDeclaringFile;
var
  Request: TGocciaConfigPermissionRequest;
  Path: string;
begin
  Path := WriteConfig('relative/sub/goccia.json', '{"permissions": {' +
    '"allow-read": ["../data", "./x/", "/abs/y"], ' +
    '"allow-ffi": ["lib"]}}');
  Request := ReadRequest(Path);
  Expect<string>(Request.Allow[gcRead].Scopes[0]).ToBe(FRoot + PathDelim +
    'relative' + PathDelim + 'data');
  Expect<string>(Request.Allow[gcRead].Scopes[1]).ToBe(FRoot + PathDelim +
    'relative' + PathDelim + 'sub' + PathDelim + 'x');
  {$IFNDEF MSWINDOWS}
  Expect<string>(Request.Allow[gcRead].Scopes[2]).ToBe('/abs/y');
  {$ENDIF}
  Expect<string>(Request.Allow[gcFFI].Scopes[0]).ToBe(FRoot + PathDelim +
    'relative' + PathDelim + 'sub' + PathDelim + 'lib');
end;

procedure TPermissionsTests.TestChildOverridesBasePerKey;
var
  Request: TGocciaConfigPermissionRequest;
  Path: string;
begin
  WriteConfig('extends/goccia.json', '{"permissions": {' +
    '"allow-read": ["./base-data"], "allow-net": ["base.test"], ' +
    '"allow-ffi": true}}');
  Path := WriteConfig('extends/child/goccia.json', '{"extends": ' +
    '"../goccia.json", "permissions": {"allow-net": ["child.test"], ' +
    '"allow-ffi": false}}');
  Request := ReadRequest(Path);
  { Inherited, resolved against the base file's directory. }
  Expect<Integer>(Length(Request.Allow[gcRead].Scopes)).ToBe(1);
  Expect<string>(Request.Allow[gcRead].Scopes[0]).ToBe(FRoot + PathDelim +
    'extends' + PathDelim + 'base-data');
  { Replaced, not merged. }
  Expect<Integer>(Length(Request.Allow[gcNet].Scopes)).ToBe(1);
  Expect<string>(Request.Allow[gcNet].Scopes[0]).ToBe('child.test');
  { Cancelled. }
  Expect<Boolean>(Request.Allow[gcFFI].RequestsAny).ToBe(False);
end;

procedure TPermissionsTests.TestNodeModulesCeiling;
var
  Request: TGocciaConfigPermissionRequest;
  Path: string;
begin
  Path := WriteConfig('nm/goccia.json', '{"permissions": {"allow-import": ' +
    '["node_modules=./x", "node_modules", "github"]}}');
  Request := ReadRequest(Path);
  Expect<string>(Request.Allow[gcImport].Scopes[0]).ToBe('node_modules=' +
    FRoot + PathDelim + 'nm' + PathDelim + 'x');
  Expect<string>(Request.Allow[gcImport].Scopes[1]).ToBe('node_modules');
  Expect<string>(Request.Allow[gcImport].Scopes[2]).ToBe('github');
end;

procedure TPermissionsTests.TestInvalidScopeRejected;
var
  Path: string;
begin
  Path := WriteConfig('bad-net/goccia.json',
    '{"permissions": {"allow-net": ["http://x"]}}');
  Expect<string>(RequestError(Path)).ToBe('TParseError: ' + Path +
    ': invalid scope in "permissions.allow-net": "http://x" (use host, ' +
    'host:port, *.domain, an IP, a CIDR range, or private)');
end;

procedure TPermissionsTests.TestJSON5AndTOMLFlatten;
var
  Request: TGocciaConfigPermissionRequest;
  Path: string;
begin
  Path := WriteConfig('json5/goccia.json5', '{ permissions: { ' +
    '"allow-net": ["a.test"], "deny-read": true, }, timeout: "5s" }');
  Request := ReadRequest(Path);
  Expect<string>(Request.Allow[gcNet].Scopes[0]).ToBe('a.test');
  Expect<Boolean>(Request.Deny[gcRead].Unscoped).ToBe(True);
  Path := WriteConfig('toml/goccia.toml', 'timeout = "500ms"' + LineEnding +
    '[permissions]' + LineEnding + 'allow-ffi = ["../fixtures/ffi"]' +
    LineEnding + 'deny-net = false' + LineEnding);
  Request := ReadRequest(Path);
  Expect<string>(Request.Allow[gcFFI].Scopes[0]).ToBe(FRoot + PathDelim +
    'fixtures' + PathDelim + 'ffi');
  Expect<Boolean>(Request.Deny[gcNet].Declared).ToBe(True);
end;

procedure TPermissionsTests.TestRequestsGrants;
var
  Request: TGocciaConfigPermissionRequest;
  Path: string;
begin
  Path := WriteConfig('deny-only/goccia.json',
    '{"permissions": {"deny-read": ["./secrets"], "deny-net": true}}');
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.RequestsGrants).ToBe(False);
  Path := WriteConfig('grants/goccia.json',
    '{"permissions": {"allow-net": ["a.test"]}}');
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.RequestsGrants).ToBe(True);
  Expect<Boolean>(Request.RequestsHonoredGrants([gcRead])).ToBe(False);
  Expect<Boolean>(Request.RequestsHonoredGrants([gcNet])).ToBe(True);
end;

function ParseCapabilityFlags(const AArgs: array of string):
  TGocciaCapabilityOptions;
var
  Positionals: TStringList;
begin
  Result := TGocciaCapabilityOptions.Create;
  Positionals := ParseArguments(AArgs, Result.Options);
  Positionals.Free;
end;

procedure TPermissionsTests.TestResolveDenyWinsEitherOrder;
var
  Options: TGocciaCapabilityOptions;
  Capabilities: TGocciaCapabilities;
begin
  Options := ParseCapabilityFlags(['--deny-net=a.test', '--allow-net']);
  try
    Capabilities := ResolveCapabilities(Options,
      TGocciaConfigPermissionRequest.Empty, True, ALL_CAPABILITIES, FRoot);
    Expect<Boolean>(Capabilities.AllowsNetHost('a.test', 443)).ToBe(False);
    Expect<Boolean>(Capabilities.AllowsNetHost('b.test', 443)).ToBe(True);
  finally
    Options.Free;
  end;
  Options := ParseCapabilityFlags(['--allow-net', '--deny-net=a.test']);
  try
    Capabilities := ResolveCapabilities(Options,
      TGocciaConfigPermissionRequest.Empty, True, ALL_CAPABILITIES, FRoot);
    Expect<Boolean>(Capabilities.AllowsNetHost('a.test', 443)).ToBe(False);
  finally
    Options.Free;
  end;
end;

procedure TPermissionsTests.TestResolveCommandLineDenySubtractsConfigAllow;
var
  Options: TGocciaCapabilityOptions;
  Capabilities: TGocciaCapabilities;
  Request: TGocciaConfigPermissionRequest;
begin
  Request := ReadRequest(WriteConfig('cli-deny/goccia.json',
    '{"permissions": {"allow-net": ["a.test", "b.test"], ' +
    '"deny-net": ["c.test"]}}'));
  Options := ParseCapabilityFlags(['--deny-net=b.test', '--allow-net=c.test',
    '--allow-net=d.test']);
  try
    Capabilities := ResolveCapabilities(Options, Request, True,
      ALL_CAPABILITIES, FRoot);
    Expect<Boolean>(Capabilities.AllowsNetHost('a.test', 443)).ToBe(True);
    Expect<Boolean>(Capabilities.AllowsNetHost('b.test', 443)).ToBe(False);
    { A config deny subtracts from a command-line allow too. }
    Expect<Boolean>(Capabilities.AllowsNetHost('c.test', 443)).ToBe(False);
    Expect<Boolean>(Capabilities.AllowsNetHost('d.test', 443)).ToBe(True);
    { Unaccepted config allows are dropped; its denies still apply. }
    Capabilities := ResolveCapabilities(Options, Request, False,
      ALL_CAPABILITIES, FRoot);
    Expect<Boolean>(Capabilities.AllowsNetHost('a.test', 443)).ToBe(False);
    Expect<Boolean>(Capabilities.AllowsNetHost('c.test', 443)).ToBe(False);
    Expect<Boolean>(Capabilities.AllowsNetHost('d.test', 443)).ToBe(True);
  finally
    Options.Free;
  end;
end;

procedure TPermissionsTests.TestResolveFiltersUnhonoredRequests;
var
  Capabilities: TGocciaCapabilities;
  Request: TGocciaConfigPermissionRequest;
  Warnings: TGocciaCapabilityScopes;
  Path: string;
begin
  Path := WriteConfig('unhonored/goccia.json',
    '{"permissions": {"allow-net": ["a.test"], "allow-read": true}}');
  Request := ReadRequest(Path);
  Capabilities := ResolveCapabilities(nil, Request, True, [gcNet], FRoot);
  Expect<Boolean>(Capabilities.AllowsNetHost('a.test', 443)).ToBe(True);
  Expect<Boolean>(Capabilities.Grants(gcRead)).ToBe(False);
  Warnings := UnsupportedRequestWarnings(Request, [gcNet], 'Sandbox');
  Expect<Integer>(Length(Warnings)).ToBe(1);
  Expect<string>(Warnings[0]).ToBe('Warning: ' + Path + ' requests ' +
    'allow-read, which Sandbox cannot grant; ignoring it');
end;

procedure TPermissionsTests.TestUnhonoredCommandLineAllowRaises;
var
  Options: TGocciaCapabilityOptions;
  Message: string;
  IsUsageError: Boolean;
begin
  Options := ParseCapabilityFlags(['--allow-read', '--deny-ffi']);
  try
    Message := '';
    IsUsageError := False;
    try
      Options.ValidateHonored('GocciaSandboxRunner', [gcNet]);
    except
      on E: TParseError do
      begin
        Message := E.Message;
        IsUsageError := E is TCLIUsageError;
      end;
    end;
    Expect<string>(Message).ToBe('GocciaSandboxRunner cannot grant read; it ' +
      'supports net. Remove --allow-read.');
    Expect<Boolean>(IsUsageError).ToBe(True);
  finally
    Options.Free;
  end;
  { A deny for an unhonored capability is accepted. }
  Options := ParseCapabilityFlags(['--deny-read']);
  try
    Options.ValidateHonored('GocciaBundler', []);
  finally
    Options.Free;
  end;
end;

procedure TPermissionsTests.TestCommandLineScopesResolveAgainstWorkingDirectory;
var
  Options: TGocciaCapabilityOptions;
  Capabilities: TGocciaCapabilities;
begin
  ForceDirectories(FRoot + PathDelim + 'cwd' + PathDelim + 'data');
  Options := ParseCapabilityFlags(['--allow-read=data',
    '--allow-import=node_modules=.']);
  try
    Capabilities := ResolveCapabilities(Options,
      TGocciaConfigPermissionRequest.Empty, True, ALL_CAPABILITIES,
      FRoot + PathDelim + 'cwd');
    Expect<Boolean>(Capabilities.AllowsPath(gcRead, FRoot + PathDelim +
      'cwd' + PathDelim + 'data' + PathDelim + 'x.json')).ToBe(True);
    Expect<Boolean>(Capabilities.AllowsPath(gcRead, FRoot + PathDelim +
      'other.json')).ToBe(False);
  finally
    Options.Free;
  end;
end;

procedure TPermissionsTests.TestTryHandleCapabilityArgument;
var
  Message: string;
begin
  Expect<Boolean>(TryHandleCapabilityArgument('--deny-read', 'Bare', []))
    .ToBe(True);
  Expect<Boolean>(TryHandleCapabilityArgument('--deny-net=a.test', 'Bare',
    [])).ToBe(True);
  Expect<Boolean>(TryHandleCapabilityArgument('--allow-reader', 'Bare', []))
    .ToBe(False);
  Expect<Boolean>(TryHandleCapabilityArgument('--print', 'Bare', []))
    .ToBe(False);
  Message := '';
  try
    TryHandleCapabilityArgument('--allow-net=a.test', 'Bare', []);
  except
    on E: TCLIUsageError do
      Message := E.Message;
  end;
  Expect<string>(Message).ToBe('Bare cannot grant net; it supports no ' +
    'capability flags. Remove --allow-net.');
end;

procedure TPermissionsTests.TestDescribeCapabilities;
begin
  Expect<string>(DescribeCapabilities([])).ToBe('no capability flags');
  Expect<string>(DescribeCapabilities([gcNet])).ToBe('net');
  Expect<string>(DescribeCapabilities([gcRead, gcNet])).ToBe('read and net');
  Expect<string>(DescribeCapabilities(ALL_CAPABILITIES))
    .ToBe('read, net, ffi, and import');
end;

begin
  TestRunnerProgram.AddSuite(TPermissionsTests.Create('CLI Permissions'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
