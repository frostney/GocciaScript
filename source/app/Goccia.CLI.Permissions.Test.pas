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
    procedure TestUnrepresentableValuesRejected;
    procedure TestMalformedBaseValueRejected;
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
    procedure TestUnsafeKeysAreRequests;
    procedure TestHashIgnoresKeyAndScopeOrder;
    procedure TestNormalizationDeduplicates;
    procedure TestUnscopedCollapsesToTrue;
    procedure TestRelativeAndAbsoluteScopesHashAlike;
    procedure TestHashSameAcrossFormats;
    procedure TestExtendsBaseScopesResolveAgainstBase;
    procedure TestDenyOnlyBlockRequestsNothing;
    procedure TestGoldenHash;
    procedure TestDescribeRequestAndChange;
    procedure TestSandboxSectionIsARequest;
    procedure TestEmptySandboxSectionIsARequest;
    procedure TestSandboxSectionErrors;
    procedure TestSandboxSectionExtendsAndTOML;
    procedure TestSandboxSectionWarnsWhereUnused;
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
  Test('null, object, and nested-array values are rejected, not ignored',
    TestUnrepresentableValuesRejected);
  Test('A malformed value in an extends base is rejected even when overridden',
    TestMalformedBaseValueRejected);
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
  Test('unsafe-* keys are requests; the nearest file wins',
    TestUnsafeKeysAreRequests);
  Test('The hash does not depend on key or scope order',
    TestHashIgnoresKeyAndScopeOrder);
  Test('Normalization removes duplicate scopes',
    TestNormalizationDeduplicates);
  Test('A capability with an unscoped entry collapses to true',
    TestUnscopedCollapsesToTrue);
  Test('./x and its absolute path hash alike',
    TestRelativeAndAbsoluteScopesHashAlike);
  Test('JSON, JSON5, and TOML blocks hash alike', TestHashSameAcrossFormats);
  Test('A base config''s scopes resolve against the base through extends',
    TestExtendsBaseScopesResolveAgainstBase);
  Test('A deny-only block requests no grants and still hashes',
    TestDenyOnlyBlockRequestsNothing);
  Test('Golden block and hash', TestGoldenHash);
  Test('Descriptions and changes since a trusted block',
    TestDescribeRequestAndChange);
  Test('A sandbox section is a request, hashed and described',
    TestSandboxSectionIsARequest);
  Test('An empty sandbox section is a request too',
    TestEmptySandboxSectionIsARequest);
  Test('Malformed sandbox sections are rejected', TestSandboxSectionErrors);
  Test('Sandbox sections: extends overrides per key; TOML tables',
    TestSandboxSectionExtendsAndTOML);
  Test('A binary that does not read the sandbox section warns',
    TestSandboxSectionWarnsWhereUnused);
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

procedure TPermissionsTests.TestUnrepresentableValuesRejected;
const
  SHAPE_ERROR = 'must be true, false, or an array of strings';
var
  Path: string;
begin
  Path := WriteConfig('object/goccia.json', '{"permissions": {' +
    '"allow-read": ["../outside"], "deny-read": {"path": "../outside"}}}');
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('null/goccia.json',
    '{"permissions": {"deny-read": null}}');
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('nested-array/goccia.json',
    '{"permissions": {"deny-read": [["x"]]}}');
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('null-element/goccia.json',
    '{"permissions": {"deny-net": ["a.test", null]}}');
  Expect<Boolean>(Pos('"permissions.deny-net" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('null-typo/goccia.json',
    '{"permissions": {"deny-nett": null}}');
  Expect<Boolean>(Pos('unknown permission "deny-nett"',
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('null-block/goccia.json', '{"permissions": null}');
  Expect<Boolean>(Pos('"permissions" must be an object',
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('toml-object/goccia.toml',
    '[permissions]' + LineEnding + 'deny-read = { a = 1 }' + LineEnding);
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('toml-nested/goccia.toml',
    '[permissions]' + LineEnding + 'deny-read = [["x"]]' + LineEnding);
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('json5-null/goccia.json5',
    '{ permissions: { "deny-read": null } }');
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('object-typo/goccia.json',
    '{"permissions": {"deny-nett": {"a": 1}}}');
  Expect<Boolean>(Pos('unknown permission "deny-nett"',
    RequestError(Path)) > 0).ToBe(True);
  Path := WriteConfig('toml-subtable/goccia.toml',
    '[permissions.deny-read]' + LineEnding + 'a = 1' + LineEnding);
  Expect<Boolean>(Pos('"permissions.deny-read" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  { A null in a child is an error, not a fall-through to its base. }
  WriteConfig('null-child/goccia.json', '{"permissions": {"allow-ffi": true}}');
  Path := WriteConfig('null-child/child/goccia.json',
    '{"extends": "../goccia.json", "permissions": {"allow-ffi": null}}');
  Expect<Boolean>(Pos('"permissions.allow-ffi" ' + SHAPE_ERROR,
    RequestError(Path)) > 0).ToBe(True);
  { An empty scope is an invalid value (exit 1), as on the command line. }
  Path := WriteConfig('empty-scope/goccia.json',
    '{"permissions": {"allow-net": [""]}}');
  Expect<string>(RequestError(Path)).ToBe('TParseError: ' + Path +
    ': "permissions.allow-net" has an empty scope');
end;

procedure TPermissionsTests.TestMalformedBaseValueRejected;
var
  Path: string;
begin
  WriteConfig('bad-base/goccia.json',
    '{"permissions": {"deny-read": {"x": 1}}}');
  Path := WriteConfig('bad-base/child/goccia.json', '{"extends": ' +
    '"../goccia.json", "permissions": {"deny-read": ["./secrets"]}}');
  Expect<Boolean>(Pos('must be true, false, or an array of strings',
    RequestError(Path)) > 0).ToBe(True);
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
  Expect<string>(Warnings[0]).ToBe('requests allow-read, which Sandbox ' +
    'cannot grant; ignoring it');
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
      Options.ValidateHonored('NetOnlyProgram', [gcNet]);
    except
      on E: TParseError do
      begin
        Message := E.Message;
        IsUsageError := E is TCLIUsageError;
      end;
    end;
    Expect<string>(Message).ToBe('NetOnlyProgram cannot grant read; it ' +
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

procedure TPermissionsTests.TestUnsafeKeysAreRequests;
var
  Base, Child: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Base := WriteConfig('unsafe/goccia.json',
    '{"unsafe-function-constructor": true, "unsafe-shadowrealm": true}');
  Request := ReadRequest(Base);
  Expect<Boolean>(Request.Unsafe = [gurFunctionConstructor, gurShadowRealm])
    .ToBe(True);
  Expect<Boolean>(Request.RequestsGrants).ToBe(True);
  { A binary that runs no code does not honor them. }
  Expect<Boolean>(Request.RequestsHonoredGrants(ALL_CAPABILITIES, False))
    .ToBe(False);
  Expect<Integer>(Length(UnsupportedRequestWarnings(Request, [], 'Bundler',
    False))).ToBe(2);

  Child := WriteConfig('unsafe/child/goccia.json',
    '{"extends": "../goccia.json", "unsafe-shadowrealm": false}');
  Request := ReadRequest(Child);
  Expect<Boolean>(Request.Unsafe = [gurFunctionConstructor]).ToBe(True);

  Child := WriteConfig('unsafe/bad/goccia.json',
    '{"unsafe-shadowrealm": "yes"}');
  Expect<Boolean>(Pos('"unsafe-shadowrealm" must be true or false',
    RequestError(Child)) > 0).ToBe(True);
end;

procedure TPermissionsTests.TestHashIgnoresKeyAndScopeOrder;
var
  First, Second: string;
begin
  First := WriteConfig('order-a/goccia.json', '{"permissions": {' +
    '"allow-net": ["b.test", "a.test"], "deny-net": ["10.0.0.0/8"]}, ' +
    '"unsafe-shadowrealm": true}');
  Second := WriteConfig('order-b/goccia.json', '{"unsafe-shadowrealm": ' +
    'true, "permissions": {"deny-net": ["10.0.0.0/8"], ' +
    '"allow-net": ["a.test", "b.test"]}}');
  Expect<string>(PermissionBlockHash(ReadRequest(First)))
    .ToBe(PermissionBlockHash(ReadRequest(Second)));
  Expect<string>(NormalizedPermissionBlock(ReadRequest(First))).ToBe(
    '{"permissions":{"allow-net":["a.test","b.test"],' +
    '"deny-net":["10.0.0.0/8"]},"unsafe":{"unsafe-shadowrealm":true},' +
    '"version":1}');
end;

procedure TPermissionsTests.TestNormalizationDeduplicates;
var
  Path: string;
begin
  Path := WriteConfig('dedupe/goccia.json', '{"permissions": {' +
    '"allow-net": ["A.test", "a.test", " a.test "], ' +
    '"allow-read": ["./x", "x", "./x/"]}}');
  Expect<string>(NormalizedPermissionBlock(ReadRequest(Path))).ToBe(
    '{"permissions":{"allow-net":["a.test"],"allow-read":["' +
    FRoot + PathDelim + 'dedupe' + PathDelim + 'x"]},"version":1}');
end;

procedure TPermissionsTests.TestUnscopedCollapsesToTrue;
var
  Path: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Path := WriteConfig('collapse/goccia.json',
    '{"permissions": {"allow-ffi": true, "deny-read": [], ' +
    '"deny-net": false}}');
  Request := ReadRequest(Path);
  Request.Allow[gcFFI].Scopes := ['/lib/a.so'];
  Expect<string>(NormalizedPermissionBlock(Request)).ToBe(
    '{"permissions":{"allow-ffi":true},"version":1}');
end;

procedure TPermissionsTests.TestRelativeAndAbsoluteScopesHashAlike;
var
  Relative, Absolute: string;
begin
  Relative := WriteConfig('same/goccia.json',
    '{"permissions": {"allow-read": ["./x"]}}');
  Absolute := WriteConfig('same/other/goccia.json',
    '{"permissions": {"allow-read": ["' +
    StringReplace(FRoot + PathDelim + 'same' + PathDelim + 'x', '\', '\\',
      [rfReplaceAll]) + '"]}}');
  Expect<string>(PermissionBlockHash(ReadRequest(Relative)))
    .ToBe(PermissionBlockHash(ReadRequest(Absolute)));
end;

procedure TPermissionsTests.TestHashSameAcrossFormats;
var
  JSONPath, JSON5Path, TOMLPath: string;
begin
  JSONPath := WriteConfig('formats/json/goccia.json',
    '{"permissions": {"allow-net": ["a.test"], "allow-read": ["../data"]},' +
    ' "unsafe-function-constructor": true}');
  JSON5Path := WriteConfig('formats/json5/goccia.json5',
    '{permissions: {"allow-read": ["../data"], "allow-net": ["a.test"],},' +
    ' "unsafe-function-constructor": true}');
  TOMLPath := WriteConfig('formats/toml/goccia.toml',
    'unsafe-function-constructor = true' + LineEnding +
    '[permissions]' + LineEnding +
    'allow-net = ["a.test"]' + LineEnding +
    'allow-read = ["../data"]' + LineEnding);
  Expect<string>(PermissionBlockHash(ReadRequest(JSON5Path)))
    .ToBe(PermissionBlockHash(ReadRequest(JSONPath)));
  Expect<string>(PermissionBlockHash(ReadRequest(TOMLPath)))
    .ToBe(PermissionBlockHash(ReadRequest(JSONPath)));
end;

procedure TPermissionsTests.TestExtendsBaseScopesResolveAgainstBase;
var
  Child, Flat: string;
begin
  WriteConfig('extends-hash/goccia.json',
    '{"permissions": {"allow-read": ["./fixtures"]}}');
  Child := WriteConfig('extends-hash/child/goccia.json',
    '{"extends": "../goccia.json", "permissions": {"allow-net": ["a.test"]}}');
  Flat := WriteConfig('extends-hash/flat/goccia.json',
    '{"permissions": {"allow-net": ["a.test"], ' +
    '"allow-read": ["../fixtures"]}}');
  Expect<string>(PermissionBlockHash(ReadRequest(Child)))
    .ToBe(PermissionBlockHash(ReadRequest(Flat)));
end;

procedure TPermissionsTests.TestDenyOnlyBlockRequestsNothing;
var
  Path: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Path := WriteConfig('deny-only-hash/goccia.json',
    '{"permissions": {"deny-net": true, "deny-read": ["./secret"]}}');
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.RequestsGrants).ToBe(False);
  Expect<Boolean>(Request.RequestsHonoredGrants(ALL_CAPABILITIES)).ToBe(False);
  Expect<Boolean>(Pos('"deny-net":true', NormalizedPermissionBlock(Request))
    > 0).ToBe(True);
end;

procedure TPermissionsTests.TestGoldenHash;
var
  Path: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Path := WriteConfig('golden/goccia.json', '{"unsafe-function-constructor":' +
    ' true, "permissions": {"deny-net": ["10.0.0.0/8"], ' +
    '"allow-net": ["example.com", "127.0.0.1"]}}');
  Request := ReadRequest(Path);
  Expect<string>(NormalizedPermissionBlock(Request)).ToBe(
    '{"permissions":{"allow-net":["127.0.0.1","example.com"],' +
    '"deny-net":["10.0.0.0/8"]},"unsafe":{"unsafe-function-constructor":' +
    'true},"version":1}');
  Expect<string>(PermissionBlockHash(Request)).ToBe(
    '4ba5043ddf965d0c60e5b1f80f8942c8abb3d5fe38dfd0eff92854c0839f29a2');
end;

procedure TPermissionsTests.TestDescribeRequestAndChange;
var
  Path, Previous: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Path := WriteConfig('describe/goccia.json', '{"permissions": {' +
    '"allow-net": ["b.test", "a.test"], "allow-ffi": true}, ' +
    '"unsafe-shadowrealm": true}');
  Request := ReadRequest(Path);
  Expect<string>(DescribePermissionRequest(Request, '    ')).ToBe(
    '    allow-ffi: any library' + sLineBreak +
    '    allow-net: a.test, b.test' + sLineBreak +
    '    unsafe-shadowrealm: true' + sLineBreak);

  Previous := '{"permissions":{"allow-ffi":true,"allow-net":["a.test"]},' +
    '"version":1}';
  Expect<string>(DescribePermissionChange(Previous, Request, '    ')).ToBe(
    '  - allow-net: a.test' + sLineBreak +
    '    allow-ffi: any library' + sLineBreak +
    '  + allow-net: a.test, b.test' + sLineBreak +
    '  + unsafe-shadowrealm: true' + sLineBreak);
  Expect<Integer>(Length(PermissionBlockLines('not json'))).ToBe(0);
end;


procedure TPermissionsTests.TestSandboxSectionIsARequest;
var
  Path, Directory: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Path := WriteConfig('sandbox/goccia.json', '{"sandbox": {' +
    '"copy": ["src", "fixtures=/data"], "copy-rw": ["out"], ' +
    '"diff": "unified", "diff-file": "changes.diff", ' +
    '"entry": "/src/main.js"}}');
  Directory := FRoot + PathDelim + 'sandbox' + PathDelim;
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.Sandbox.Declared).ToBe(True);
  Expect<string>(Request.Sandbox.SourcePath).ToBe(Path);
  Expect<Integer>(Length(Request.Sandbox.Inputs)).ToBe(3);
  Expect<string>(Request.Sandbox.Inputs[0].HostPath).ToBe(Directory + 'src');
  Expect<string>(Request.Sandbox.Inputs[0].SandboxPath).ToBe('');
  Expect<string>(Request.Sandbox.Inputs[1].SandboxPath).ToBe('/data');
  Expect<Boolean>(Request.Sandbox.Inputs[1].ReadWrite).ToBe(False);
  Expect<Boolean>(Request.Sandbox.Inputs[2].ReadWrite).ToBe(True);
  Expect<string>(Request.Sandbox.Entry).ToBe('/src/main.js');
  Expect<string>(Request.Sandbox.Diff).ToBe('unified');
  Expect<string>(Request.Sandbox.DiffFile).ToBe(Directory + 'changes.diff');

  Expect<Boolean>(Request.RequestsGrants).ToBe(True);
  Expect<Boolean>(Request.RequestsHonoredGrants(ALL_CAPABILITIES, True,
    False)).ToBe(False);
  Expect<Boolean>(Request.RequestsHonoredGrants([], False, True)).ToBe(True);

  { Inputs keep their order: a later one may overwrite an earlier one. }
  Expect<string>(NormalizedPermissionBlock(Request)).ToBe(
    '{"sandbox":{"copy":["' + StringReplace(Directory + 'src', '\', '\\',
    [rfReplaceAll]) + '","' + StringReplace(Directory + 'fixtures', '\',
    '\\', [rfReplaceAll]) + '=/data"],"copy-rw":["' +
    StringReplace(Directory + 'out', '\', '\\', [rfReplaceAll]) +
    '"],"diff":"unified","diff-file":"' + StringReplace(Directory +
    'changes.diff', '\', '\\', [rfReplaceAll]) +
    '","entry":"/src/main.js"},"version":1}');
  Expect<string>(DescribePermissionRequest(Request, '  ')).ToBe(
    '  sandbox.copy: ' + Directory + 'src, ' + Directory + 'fixtures=/data' +
    sLineBreak +
    '  sandbox.copy-rw: ' + Directory + 'out' + sLineBreak +
    '  sandbox.diff: unified' + sLineBreak +
    '  sandbox.diff-file: ' + Directory + 'changes.diff' + sLineBreak +
    '  sandbox.entry: /src/main.js' + sLineBreak);
end;

procedure TPermissionsTests.TestEmptySandboxSectionIsARequest;
var
  Path: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Path := WriteConfig('sandbox-empty/goccia.json',
    '{"sandbox": {}, "permissions": {"allow-net": ["a.test"]}}');
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.Sandbox.Declared).ToBe(True);
  Expect<Integer>(Length(Request.Sandbox.Inputs)).ToBe(0);
  Expect<string>(NormalizedPermissionBlock(Request)).ToBe(
    '{"permissions":{"allow-net":["a.test"]},"sandbox":{},"version":1}');
  Expect<string>(DescribePermissionRequest(Request, '')).ToBe(
    'allow-net: a.test' + sLineBreak +
    'sandbox: no inputs (sandbox mode only)' + sLineBreak);

  Path := WriteConfig('sandbox-diff-true/goccia.json',
    '{"sandbox": {"diff": true}}');
  Request := ReadRequest(Path);
  Expect<string>(Request.Sandbox.Diff).ToBe(SANDBOX_DIFF_DEFAULT);
  Expect<string>(NormalizedPermissionBlock(Request)).ToBe(
    '{"sandbox":{"diff":true},"version":1}');

  Path := WriteConfig('sandbox-none/goccia.json', '{"timeout": 5}');
  Expect<Boolean>(ReadRequest(Path).Sandbox.Declared).ToBe(False);
end;

procedure TPermissionsTests.TestSandboxSectionErrors;
var
  Path: string;
begin
  Path := WriteConfig('sandbox-files/goccia.json',
    '{"sandbox": {"files": [{"from": "src"}]}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "sandbox.files" was removed in GocciaScript 0.14.0; list ' +
    'host inputs as "copy" or "copy-rw" strings (<host>[=<sandbox>]) ' +
    'instead');

  Path := WriteConfig('sandbox-unknown/goccia.json',
    '{"sandbox": {"copyrw": ["out"]}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': unknown sandbox key "copyrw" (valid: copy, copy-rw, entry, ' +
    'diff, diff-file)');

  Path := WriteConfig('sandbox-flag/goccia.json', '{"sandbox": true}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "sandbox" must be an object with copy, copy-rw, entry, diff, ' +
    'diff-file keys');

  Path := WriteConfig('sandbox-number/goccia.json',
    '{"sandbox": {"copy": ["src", 5]}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "sandbox.copy" must be a string or an array of strings in ' +
    'the --copy grammar <host>[=<sandbox>]');

  Path := WriteConfig('sandbox-object/goccia.json',
    '{"sandbox": {"copy-rw": [{"from": "out"}]}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "sandbox.copy-rw" must be a string or an array of strings ' +
    'in the --copy grammar <host>[=<sandbox>]');

  Path := WriteConfig('sandbox-diff/goccia.json',
    '{"sandbox": {"diff": "patch"}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "sandbox.diff" must be true, false, "json", or "unified"');

  Path := WriteConfig('sandbox-host/goccia.json',
    '{"sandbox": {"copy": ["=/x"]}}');
  Expect<string>(RequestError(Path)).ToBe('EGocciaConfigPermissionError: ' +
    Path + ': "sandbox.copy" entry "=/x" names no host path');
end;

procedure TPermissionsTests.TestSandboxSectionExtendsAndTOML;
var
  Path, Base: string;
  Request: TGocciaConfigPermissionRequest;
begin
  Base := WriteConfig('sandbox-extends/goccia.json',
    '{"sandbox": {"copy": ["shared"], "copy-rw": ["out"]}}');
  Path := WriteConfig('sandbox-extends/child/goccia.json',
    '{"extends": "../goccia.json", "sandbox": {"copy": ["local"]}}');
  Request := ReadRequest(Path);
  Expect<Integer>(Length(Request.Sandbox.Inputs)).ToBe(2);
  Expect<string>(Request.Sandbox.Inputs[0].HostPath).ToBe(
    ExtractFilePath(Path) + 'local');
  { The base's copy-rw is inherited, resolved against the base. }
  Expect<string>(Request.Sandbox.Inputs[1].HostPath).ToBe(
    ExtractFilePath(Base) + 'out');
  Expect<Boolean>(Request.Sandbox.Inputs[1].ReadWrite).ToBe(True);
  Expect<string>(Request.Sandbox.SourcePath).ToBe(Path);

  Path := WriteConfig('sandbox-toml/goccia.toml',
    '[sandbox]' + LineEnding + 'copy = ["src=/app"]' + LineEnding +
    'diff = "json"' + LineEnding);
  Request := ReadRequest(Path);
  Expect<Boolean>(Request.Sandbox.Declared).ToBe(True);
  Expect<Integer>(Length(Request.Sandbox.Inputs)).ToBe(1);
  Expect<string>(Request.Sandbox.Inputs[0].SandboxPath).ToBe('/app');
  Expect<string>(Request.Sandbox.Diff).ToBe('json');
end;

procedure TPermissionsTests.TestSandboxSectionWarnsWhereUnused;
var
  Path: string;
  Request: TGocciaConfigPermissionRequest;
  Warnings: TGocciaCapabilityScopes;
begin
  Path := WriteConfig('sandbox-warn/goccia.json', '{"sandbox": {}}');
  Request := ReadRequest(Path);
  Warnings := UnsupportedRequestWarnings(Request, ALL_CAPABILITIES,
    'GocciaTestRunner', True, False);
  Expect<Integer>(Length(Warnings)).ToBe(1);
  Expect<string>(Warnings[0]).ToBe('declares a "sandbox" section, which ' +
    'GocciaTestRunner does not use; ignoring it');
  Expect<Integer>(Length(UnsupportedRequestWarnings(Request, ALL_CAPABILITIES,
    'GocciaRunner', True, True))).ToBe(0);
end;

begin
  TestRunnerProgram.AddSuite(TPermissionsTests.Create('CLI Permissions'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.
