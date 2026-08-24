program Goccia.Error.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Diagnostics.SourceRegistry,
  Goccia.Error,
  Goccia.Error.Detail,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TErrorTests = class(TTestSuite)
  private
    function CreateSourceLines: TStringList;
    function CreateSingleLineSource: TStringList;
    procedure TestGetDetailedMessageShowsJSFriendlyErrorName;
    procedure TestGetDetailedMessageShowsLocation;
    procedure TestGetDetailedMessageShowsContextLinesBefore;
    procedure TestGetDetailedMessageShowsContextLinesAfter;
    procedure TestGetDetailedMessageShowsCaretAtCorrectColumn;
    procedure TestGetDetailedMessageShowsSuggestionWhenSet;
    procedure TestGetDetailedMessageWithoutSuggestionOmitsSuggestionLine;
    procedure TestGetDetailedMessageWithColorContainsAnsiCodes;
    procedure TestGetDetailedMessageWithoutColorHasNoAnsiCodes;
    procedure TestErrorDisplayNameMapsSyntaxErrorCorrectly;
    procedure TestErrorDisplayNameMapsLexerErrorToSyntaxError;
    procedure TestLexerErrorInheritFromSyntaxError;
    procedure TestErrorDisplayNameMapsTypeErrorCorrectly;
    procedure TestErrorDisplayNameMapsReferenceErrorCorrectly;
    procedure TestGetDetailedMessageHandlesSingleSourceLine;
    procedure TestFormatThrowDetailRequiresExpectedPrincipal;
    procedure TestIdentifiedHostRegistrationUpgradesPathAlias;
    procedure TestHostRegistrationReconcilesDivergedPathAliasesAfterCwdChange;
    procedure TestGuestRegistrationCannotDowngradeHostExpandedAlias;
    procedure TestHostRegistrationUpgradesMixedGuestLiteralHostExpanded;
    procedure TestGuestRegistrationPreservesHostLiteralWithGuestExpanded;
  public
    procedure SetupTests; override;
  end;

function TErrorTests.CreateSourceLines: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('const x = 1;');
  Result.Add('const y = 2;');
  Result.Add('const z = abc;');
  Result.Add('const w = 4;');
  Result.Add('const v = 5;');
end;

function TErrorTests.CreateSingleLineSource: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('const x = abc;');
end;

procedure TErrorTests.SetupTests;
begin
  Test('GetDetailedMessage shows JS-friendly error name',
    TestGetDetailedMessageShowsJSFriendlyErrorName);
  Test('GetDetailedMessage shows location',
    TestGetDetailedMessageShowsLocation);
  Test('GetDetailedMessage shows 2 context lines before',
    TestGetDetailedMessageShowsContextLinesBefore);
  Test('GetDetailedMessage shows 2 context lines after',
    TestGetDetailedMessageShowsContextLinesAfter);
  Test('GetDetailedMessage shows caret at correct column',
    TestGetDetailedMessageShowsCaretAtCorrectColumn);
  Test('GetDetailedMessage shows suggestion when set',
    TestGetDetailedMessageShowsSuggestionWhenSet);
  Test('GetDetailedMessage without suggestion omits suggestion line',
    TestGetDetailedMessageWithoutSuggestionOmitsSuggestionLine);
  Test('GetDetailedMessage with color contains ANSI codes',
    TestGetDetailedMessageWithColorContainsAnsiCodes);
  Test('GetDetailedMessage without color has no ANSI codes',
    TestGetDetailedMessageWithoutColorHasNoAnsiCodes);
  Test('ErrorDisplayName maps SyntaxError correctly',
    TestErrorDisplayNameMapsSyntaxErrorCorrectly);
  Test('ErrorDisplayName maps LexerError to SyntaxError',
    TestErrorDisplayNameMapsLexerErrorToSyntaxError);
  Test('LexerError inherits from SyntaxError',
    TestLexerErrorInheritFromSyntaxError);
  Test('ErrorDisplayName maps TypeError correctly',
    TestErrorDisplayNameMapsTypeErrorCorrectly);
  Test('ErrorDisplayName maps ReferenceError correctly',
    TestErrorDisplayNameMapsReferenceErrorCorrectly);
  Test('GetDetailedMessage handles single source line',
    TestGetDetailedMessageHandlesSingleSourceLine);
  Test('FormatThrowDetail withholds a retained foreign excerpt without an ' +
    'explicit matching principal',
    TestFormatThrowDetailRequiresExpectedPrincipal);
  Test('an identified host registration upgrades a prior guest path alias',
    TestIdentifiedHostRegistrationUpgradesPathAlias);
  Test('a host registration reconciles literal and expanded aliases that ' +
    'diverged across a working-directory change',
    TestHostRegistrationReconcilesDivergedPathAliasesAfterCwdChange);
  Test('a guest registration cannot downgrade a host expanded alias while ' +
    'reconciling a guest literal alias',
    TestGuestRegistrationCannotDowngradeHostExpandedAlias);
  Test('a host registration upgrades a mixed guest-literal / host-expanded ' +
    'alias pair', TestHostRegistrationUpgradesMixedGuestLiteralHostExpanded);
  Test('a guest registration preserves a host literal alias while reconciling ' +
    'a guest expanded alias', TestGuestRegistrationPreservesHostLiteralWithGuestExpanded);
end;

procedure TErrorTests.TestGetDetailedMessageShowsJSFriendlyErrorName;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('SyntaxError', Output) > 0).ToBe(True);
      Expect<Boolean>(Pos('TGocciaSyntaxError', Output) > 0).ToBe(False);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageShowsLocation;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('--> test.js:3:5', Output) > 0).ToBe(True);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageShowsContextLinesBefore;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('const x = 1;', Output) > 0).ToBe(True);
      Expect<Boolean>(Pos('const y = 2;', Output) > 0).ToBe(True);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageShowsContextLinesAfter;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('const w = 4;', Output) > 0).ToBe(True);
      Expect<Boolean>(Pos('const v = 5;', Output) > 0).ToBe(True);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageShowsCaretAtCorrectColumn;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
  Lines: TStringList;
  CaretLine: string;
  CaretPos, I: Integer;
begin
  SourceLines := CreateSourceLines;
  try
    // Error at line 3, column 5
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('^', Output) > 0).ToBe(True);

      // Find the caret line and verify column alignment
      Lines := TStringList.Create;
      try
        Lines.Text := Output;
        CaretPos := -1;
        for I := 0 to Lines.Count - 1 do
          if Pos('^', Lines[I]) > 0 then
          begin
            CaretLine := Lines[I];
            CaretPos := Pos('^', CaretLine);
            Break;
          end;
        Expect<Boolean>(CaretPos > 0).ToBe(True);
        // The caret should be at gutter + ' | ' + (column - 1) spaces + '^'
        // Gutter is 4 chars wide, so: 4 + 3 + 4 + 1 = 12
        Expect<Integer>(CaretPos).ToBe(12);
      finally
        Lines.Free;
      end;
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageShowsSuggestionWhenSet;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines,
      'fix this');
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('Suggestion: fix this', Output) > 0).ToBe(True);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageWithoutSuggestionOmitsSuggestionLine;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('Suggestion', Output) > 0).ToBe(False);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageWithColorContainsAnsiCodes;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(True);
      Expect<Boolean>(Pos(#27, Output) > 0).ToBe(True);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageWithoutColorHasNoAnsiCodes;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSourceLines;
  try
    Error := TGocciaSyntaxError.Create('test', 3, 5, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos(#27, Output) > 0).ToBe(False);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestErrorDisplayNameMapsSyntaxErrorCorrectly;
var
  Error: TGocciaSyntaxError;
begin
  Error := TGocciaSyntaxError.Create('test', 1, 1, 'test.js', nil);
  try
    Expect<string>(ErrorDisplayName(Error)).ToBe('SyntaxError');
  finally
    Error.Free;
  end;
end;

procedure TErrorTests.TestErrorDisplayNameMapsLexerErrorToSyntaxError;
var
  Error: TGocciaLexerError;
begin
  Error := TGocciaLexerError.Create('test', 1, 1, 'test.js', nil);
  try
    Expect<string>(ErrorDisplayName(Error)).ToBe('SyntaxError');
  finally
    Error.Free;
  end;
end;

procedure TErrorTests.TestLexerErrorInheritFromSyntaxError;
var
  Error: TGocciaLexerError;
begin
  Error := TGocciaLexerError.Create('test', 1, 1, 'test.js', nil);
  try
    Expect<Boolean>(Error is TGocciaSyntaxError).ToBe(True);
  finally
    Error.Free;
  end;
end;

procedure TErrorTests.TestErrorDisplayNameMapsTypeErrorCorrectly;
var
  Error: TGocciaTypeError;
begin
  Error := TGocciaTypeError.Create('test', 1, 1, 'test.js', nil);
  try
    Expect<string>(ErrorDisplayName(Error)).ToBe('TypeError');
  finally
    Error.Free;
  end;
end;

procedure TErrorTests.TestErrorDisplayNameMapsReferenceErrorCorrectly;
var
  Error: TGocciaReferenceError;
begin
  Error := TGocciaReferenceError.Create('test', 1, 1, 'test.js', nil);
  try
    Expect<string>(ErrorDisplayName(Error)).ToBe('ReferenceError');
  finally
    Error.Free;
  end;
end;

procedure TErrorTests.TestGetDetailedMessageHandlesSingleSourceLine;
var
  SourceLines: TStringList;
  Error: TGocciaSyntaxError;
  Output: string;
begin
  SourceLines := CreateSingleLineSource;
  try
    Error := TGocciaSyntaxError.Create('test', 1, 11, 'test.js', SourceLines);
    try
      Output := Error.GetDetailedMessage(False);
      Expect<Boolean>(Pos('const x = abc;', Output) > 0).ToBe(True);
      Expect<Boolean>(Pos('^', Output) > 0).ToBe(True);
      Expect<Boolean>(Pos('--> test.js:1:11', Output) > 0).ToBe(True);
    finally
      Error.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

procedure TErrorTests.TestFormatThrowDetailRequiresExpectedPrincipal;
const
  SecretMarker = 'A_ENGINE_PRIVATE_SOURCE_MARKER';
var
  ErrorObject: TGocciaErrorObjectValue;
  ScopeA, ScopeB: TGocciaDiagnosticSourceScope;
  NameValue, MessageValue: TGocciaStringLiteralValue;
  Output: string;
begin
  ScopeA := TGocciaDiagnosticSourceScope.Create;
  ScopeB := TGocciaDiagnosticSourceScope.Create;
  NameValue := nil;
  MessageValue := nil;
  ErrorObject := TGocciaErrorObjectValue.Create;
  try
    ErrorObject.HasErrorData := True;
    { The error frees property descriptors, not their FValue references, so hold
      these managed values in locals and free them after the error. }
    NameValue := TGocciaStringLiteralValue.Create('Error');
    ErrorObject.AssignProperty('name', NameValue);
    MessageValue := TGocciaStringLiteralValue.Create('held across engines');
    ErrorObject.AssignProperty('message', MessageValue);
    ErrorObject.HasErrorSourceLocation := True;
    ErrorObject.ErrorSourcePath := 'engine-a-secret.js';
    ErrorObject.ErrorSourceLine := 2;
    ErrorObject.ErrorSourceColumn := 3;
    ErrorObject.ErrorSourceExcerpt := '// first' + sLineBreak +
      SecretMarker + sLineBreak + '// third';
    ErrorObject.ErrorSourceExcerptFirstLine := 1;
    ErrorObject.ErrorSourcePrincipal := ScopeA.Principal;

    { Execute has already restored the active scope before a host renders a
      retained throw. Absence of ambient execution state must never authorize
      the source stamped by another engine. }
    Expect<Boolean>(TGocciaDiagnosticSourceRegistry.Current = nil).ToBe(True);
    Output := FormatThrowDetail(ErrorObject, 'engine-b.js', nil, False,
      ScopeB.Principal);
    Expect<Boolean>(Pos('engine-a-secret.js:2:3', Output) > 0).ToBe(True);
    Expect<Boolean>(Pos(SecretMarker, Output) > 0).ToBe(False);

    Output := FormatThrowDetail(ErrorObject, 'engine-b.js', nil, False, 0);
    Expect<Boolean>(Pos('engine-a-secret.js:2:3', Output) > 0).ToBe(True);
    Expect<Boolean>(Pos(SecretMarker, Output) > 0).ToBe(False);

    { The owning renderer still gets the retained code frame. }
    Output := FormatThrowDetail(ErrorObject, 'engine-a-secret.js', nil, False,
      ScopeA.Principal);
    Expect<Boolean>(Pos(SecretMarker, Output) > 0).ToBe(True);
  finally
    ErrorObject.Free;
    NameValue.Free;
    MessageValue.Free;
    ScopeB.Free;
    ScopeA.Free;
  end;
end;

procedure TErrorTests.TestIdentifiedHostRegistrationUpgradesPathAlias;
const
  HostSecret = 'HOST_ONLY_SOURCE_LINE';
var
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
  FirstLine: Integer;
  PathSpelling: string;
begin
  { A guest first registers a file with no canonical identity (path-keyed), then
    the host registers the SAME file with a canonical identity. The identified
    host registration must reconcile the earlier path alias so a lookup by the
    path spelling no longer resolves to a guest-owned entry — otherwise the host
    source is disclosed through the path spelling. }
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    PathSpelling := 'shared-source.js';

    { Guest load: no canonical identity, so this is keyed under the path. }
    Scope.Register(PathSpelling, '// guest first line' + sLineBreak +
      '// guest second line', False);

    { The guest-owned source is readable through the path spelling. }
    Expect<Boolean>(Scope.TryGetGuestWindow(PathSpelling, 1, 0, 0, Window,
      FirstLine)).ToBe(True);

    { Host load of the same file, now with a canonical identity. }
    Scope.Register(PathSpelling, '// host first line' + sLineBreak +
      HostSecret, True, '#id:host-canonical');

    { After the host upgrade the path spelling must no longer yield a guest
      window: the alias was reconciled to the host-owned entry. }
    Expect<Boolean>(Scope.TryGetGuestWindow(PathSpelling, 1, 0, 0, Window,
      FirstLine)).ToBe(False);

    { And the canonical identity resolves host-owned as well. }
    Expect<Boolean>(Scope.TryGetGuestWindow('#id:host-canonical', 1, 0, 0,
      Window, FirstLine)).ToBe(False);
  finally
    Window.Free;
    Scope.Free;
  end;
end;

procedure TErrorTests.TestHostRegistrationReconcilesDivergedPathAliasesAfterCwdChange;
var
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
  FirstLine: Integer;
  OldDir, DirA, DirB, ExpandedInB: string;
begin
  { A relative spelling registered under one working directory and the same
    file's absolute expansion produced after a cwd change can leave the literal
    and expanded keys pointing at two SEPARATE guest entries. An identified host
    registration then names both spellings as one file. It must reconcile BOTH
    aliases — upgrading only the first (the literal, which TryGetGuestWindow
    consults first) would leave the expanded alias guest-owned and let a lookup
    by the expanded spelling disclose the host's file. }
  OldDir := GetCurrentDir;
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    { Two real, distinct directories so SetCurrentDir/ExpandFileName resolve. }
    DirA := GetCurrentDir;
    DirB := ExpandFileName('..');
    Expect<Boolean>(DirA <> DirB).ToBe(True);

    { Guest load 1, cwd = DirA: keyed under the literal 'shared-cwd.js' and
      DirA/shared-cwd.js. }
    SetCurrentDir(DirA);
    Scope.Register('shared-cwd.js', '// guest A line one' + sLineBreak +
      '// guest A line two', False);

    { Guest load 2, cwd = DirB: register the file's DirB expansion as an
      absolute spelling, minting a SEPARATE entry keyed under DirB/shared-cwd.js. }
    SetCurrentDir(DirB);
    ExpandedInB := ExpandFileName('shared-cwd.js');
    Scope.Register(ExpandedInB, '// guest B line one' + sLineBreak +
      '// guest B line two', False);

    { Both spellings currently disclose their own guest entry. }
    Expect<Boolean>(Scope.TryGetGuestWindow('shared-cwd.js', 1, 0, 0, Window,
      FirstLine)).ToBe(True);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(True);

    { Host load, still cwd = DirB: APath 'shared-cwd.js' resolves to the first
      entry, its expansion DirB/shared-cwd.js to the second — two DIFFERENT
      entries reconciled by one identified host registration. }
    Scope.Register('shared-cwd.js', '// host line one' + sLineBreak +
      '// host line two', True, '#id:host-cwd');

    { Neither the literal nor the diverged expanded alias may still yield a
      guest window: both were reconciled to the host-owned entry. }
    Expect<Boolean>(Scope.TryGetGuestWindow('shared-cwd.js', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow('#id:host-cwd', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
  finally
    SetCurrentDir(OldDir);
    Window.Free;
    Scope.Free;
  end;
end;

procedure TErrorTests.TestGuestRegistrationCannotDowngradeHostExpandedAlias;
var
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
  FirstLine: Integer;
  OldDir, DirA, DirB, ExpandedInB: string;
begin
  { The finding case (host-source disclosure). Set up a MIXED alias pair for one
    file: the literal spelling 'shared-mix.js' resolves to a GUEST entry, while
    its expansion in the current cwd resolves to a SEPARATE HOST entry. A later
    GUEST registration (AIsHost = False) naming both spellings as one file picks
    the guest literal as Unified. If reconciliation keyed ownership off AIsHost
    alone it would skip the host upgrade and then repoint the host expanded alias
    at the guest entry, so TryGetGuestWindow would disclose the host source
    through the expanded spelling. Host ownership must win from the reached host
    entry regardless of the new registration's direction. }
  OldDir := GetCurrentDir;
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    DirA := GetCurrentDir;
    DirB := ExpandFileName('..');
    Expect<Boolean>(DirA <> DirB).ToBe(True);

    { HOST load, cwd = DirB: register the file's DirB expansion as an absolute
      spelling, minting a HOST entry keyed under DirB/shared-mix.js. }
    SetCurrentDir(DirB);
    ExpandedInB := ExpandFileName('shared-mix.js');
    Scope.Register(ExpandedInB, '// host line one' + sLineBreak +
      '// host line two', True);

    { GUEST load, cwd = DirA: register the relative literal 'shared-mix.js',
      minting a SEPARATE GUEST entry keyed under the literal and DirA/shared-mix.js. }
    SetCurrentDir(DirA);
    Scope.Register('shared-mix.js', '// guest line one' + sLineBreak +
      '// guest line two', False);

    { Precondition: the literal spelling is guest (discloses), the host expanded
      spelling is host (withheld). }
    Expect<Boolean>(Scope.TryGetGuestWindow('shared-mix.js', 1, 0, 0, Window,
      FirstLine)).ToBe(True);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(False);

    { GUEST load, cwd = DirB, with a canonical identity: APath 'shared-mix.js'
      resolves to the guest literal entry, its expansion DirB/shared-mix.js to the
      host entry — a mixed pair reconciled by ONE identified GUEST registration. }
    SetCurrentDir(DirB);
    Scope.Register('shared-mix.js', '// guest reconcile line one' + sLineBreak +
      '// guest reconcile line two', False, '#id:host-mix');

    { No spelling may disclose after reconciliation: host ownership won from the
      host expanded entry even though the new registration was a guest. Reverting
      the host-wins derivation to key off AIsHost re-discloses both spellings. }
    Expect<Boolean>(Scope.TryGetGuestWindow('shared-mix.js', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow('#id:host-mix', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
  finally
    SetCurrentDir(OldDir);
    Window.Free;
    Scope.Free;
  end;
end;

procedure TErrorTests.TestHostRegistrationUpgradesMixedGuestLiteralHostExpanded;
var
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
  FirstLine: Integer;
  OldDir, DirA, DirB, ExpandedInB: string;
begin
  { Reverse direction on the same mixed topology: a guest literal alias and a
    host expanded alias reconciled by a HOST registration. Host ownership must
    still cover every reached alias so no spelling discloses. }
  OldDir := GetCurrentDir;
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    DirA := GetCurrentDir;
    DirB := ExpandFileName('..');
    Expect<Boolean>(DirA <> DirB).ToBe(True);

    SetCurrentDir(DirB);
    ExpandedInB := ExpandFileName('shared-mix-host.js');
    Scope.Register(ExpandedInB, '// host line one' + sLineBreak +
      '// host line two', True);

    SetCurrentDir(DirA);
    Scope.Register('shared-mix-host.js', '// guest line one' + sLineBreak +
      '// guest line two', False);

    Expect<Boolean>(Scope.TryGetGuestWindow('shared-mix-host.js', 1, 0, 0,
      Window, FirstLine)).ToBe(True);

    SetCurrentDir(DirB);
    Scope.Register('shared-mix-host.js', '// host reconcile line one' +
      sLineBreak + '// host reconcile line two', True, '#id:host-mix-host');

    Expect<Boolean>(Scope.TryGetGuestWindow('shared-mix-host.js', 1, 0, 0,
      Window, FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow('#id:host-mix-host', 1, 0, 0,
      Window, FirstLine)).ToBe(False);
  finally
    SetCurrentDir(OldDir);
    Window.Free;
    Scope.Free;
  end;
end;

procedure TErrorTests.TestGuestRegistrationPreservesHostLiteralWithGuestExpanded;
var
  Scope: TGocciaDiagnosticSourceScope;
  Window: TStringList;
  FirstLine: Integer;
  OldDir, DirA, DirB, ExpandedInB: string;
begin
  { The other mixed direction: the literal spelling resolves to a HOST entry
    (chosen as Unified) while the expanded spelling resolves to a SEPARATE GUEST
    entry. A guest registration reconciling the pair must keep the host literal
    host-owned AND fold the guest expanded entry to host — host wins from Unified,
    and the previously guest-owned expanded entry must not survive as guest. }
  OldDir := GetCurrentDir;
  Scope := TGocciaDiagnosticSourceScope.Create;
  Window := TStringList.Create;
  try
    DirA := GetCurrentDir;
    DirB := ExpandFileName('..');
    Expect<Boolean>(DirA <> DirB).ToBe(True);

    { HOST load, cwd = DirA: literal 'shared-hg.js' keyed to a HOST entry (its
      expansion is DirA/shared-hg.js, distinct from the DirB expansion below). }
    SetCurrentDir(DirA);
    Scope.Register('shared-hg.js', '// host line one' + sLineBreak +
      '// host line two', True);

    { GUEST load, cwd = DirB: register the DirB expansion as an absolute spelling,
      minting a SEPARATE GUEST entry keyed under DirB/shared-hg.js. }
    SetCurrentDir(DirB);
    ExpandedInB := ExpandFileName('shared-hg.js');
    Scope.Register(ExpandedInB, '// guest line one' + sLineBreak +
      '// guest line two', False);

    Expect<Boolean>(Scope.TryGetGuestWindow('shared-hg.js', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(True);

    { GUEST load, cwd = DirB, identified: APath 'shared-hg.js' resolves to the
      host literal entry (Unified), its expansion DirB/shared-hg.js to the guest
      entry. Host wins; neither spelling may disclose afterward. }
    Scope.Register('shared-hg.js', '// guest reconcile line one' + sLineBreak +
      '// guest reconcile line two', False, '#id:host-hg');

    Expect<Boolean>(Scope.TryGetGuestWindow('shared-hg.js', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow(ExpandedInB, 1, 0, 0, Window,
      FirstLine)).ToBe(False);
    Expect<Boolean>(Scope.TryGetGuestWindow('#id:host-hg', 1, 0, 0, Window,
      FirstLine)).ToBe(False);
  finally
    SetCurrentDir(OldDir);
    Window.Free;
    Scope.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TErrorTests.Create('Error'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
