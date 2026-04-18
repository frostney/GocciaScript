program Goccia.Error.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.Error;

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
    procedure TestErrorDisplayNameMapsTypeErrorCorrectly;
    procedure TestErrorDisplayNameMapsReferenceErrorCorrectly;
    procedure TestGetDetailedMessageHandlesSingleSourceLine;
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
  Test('ErrorDisplayName maps TypeError correctly',
    TestErrorDisplayNameMapsTypeErrorCorrectly);
  Test('ErrorDisplayName maps ReferenceError correctly',
    TestErrorDisplayNameMapsReferenceErrorCorrectly);
  Test('GetDetailedMessage handles single source line',
    TestGetDetailedMessageHandlesSingleSourceLine);
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

begin
  TestRunnerProgram.AddSuite(TErrorTests.Create('Error'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.
