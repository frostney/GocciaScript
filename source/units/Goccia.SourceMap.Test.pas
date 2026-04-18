program Goccia.SourceMap.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.GarbageCollector,
  Goccia.SourceMap,
  Goccia.SourceMap.Consumer,
  Goccia.TestSetup;

type
  TSourceMapTests = class(TTestSuite)
  private
    procedure TestVersionIsThree;
    procedure TestAddSource;
    procedure TestAddSourceDeduplicates;
    procedure TestAddName;
    procedure TestAddNameDeduplicates;
    procedure TestBasicMapping;
    procedure TestTranslateExactMatch;
    procedure TestTranslateColumnOffset;
    procedure TestTranslateNoMappings;
    procedure TestTranslateFallbackToPrecedingLine;
    procedure TestToJSONStructure;
    procedure TestToJSONMappings;
    procedure TestToJSONSourcesContent;
    procedure TestToJSONNoContentWhenEmpty;
    procedure TestRoundTripThroughConsumer;
    procedure TestRoundTripWithNames;
    procedure TestToInlineComment;
    procedure TestMultipleSourceFiles;
    procedure TestSemicolonSeparatedLines;
  public
    procedure SetupTests; override;
  end;

procedure TSourceMapTests.SetupTests;
begin
  Test('Version is always 3', TestVersionIsThree);
  Test('AddSource returns sequential indices', TestAddSource);
  Test('AddSource deduplicates identical paths', TestAddSourceDeduplicates);
  Test('AddName returns sequential indices', TestAddName);
  Test('AddName deduplicates identical names', TestAddNameDeduplicates);
  Test('AddMapping increases segment count', TestBasicMapping);
  Test('Translate finds exact line/column match', TestTranslateExactMatch);
  Test('Translate applies column offset', TestTranslateColumnOffset);
  Test('Translate returns false for empty map', TestTranslateNoMappings);
  Test('Translate falls back to preceding line', TestTranslateFallbackToPrecedingLine);
  Test('ToJSON has required structure', TestToJSONStructure);
  Test('ToJSON encodes mappings string', TestToJSONMappings);
  Test('ToJSON includes sourcesContent when set', TestToJSONSourcesContent);
  Test('ToJSON omits sourcesContent when empty', TestToJSONNoContentWhenEmpty);
  Test('Round-trip through consumer preserves positions', TestRoundTripThroughConsumer);
  Test('Round-trip preserves names', TestRoundTripWithNames);
  Test('ToInlineComment produces valid data URI', TestToInlineComment);
  Test('Multiple source files', TestMultipleSourceFiles);
  Test('Empty lines produce semicolons in mappings', TestSemicolonSeparatedLines);
end;

procedure TSourceMapTests.TestVersionIsThree;
var
  Map: TGocciaSourceMap;
begin
  Map := TGocciaSourceMap.Create('output.js');
  try
    Expect<Integer>(Map.Version).ToBe(3);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestAddSource;
var
  Map: TGocciaSourceMap;
begin
  Map := TGocciaSourceMap.Create;
  try
    Expect<Integer>(Map.AddSource('a.js')).ToBe(0);
    Expect<Integer>(Map.AddSource('b.js')).ToBe(1);
    Expect<Integer>(Map.AddSource('c.js')).ToBe(2);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestAddSourceDeduplicates;
var
  Map: TGocciaSourceMap;
begin
  Map := TGocciaSourceMap.Create;
  try
    Expect<Integer>(Map.AddSource('a.js')).ToBe(0);
    Expect<Integer>(Map.AddSource('a.js')).ToBe(0);
    Expect<Integer>(Map.AddSource('b.js')).ToBe(1);
    Expect<Integer>(Map.AddSource('a.js')).ToBe(0);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestAddName;
var
  Map: TGocciaSourceMap;
begin
  Map := TGocciaSourceMap.Create;
  try
    Expect<Integer>(Map.AddName('foo')).ToBe(0);
    Expect<Integer>(Map.AddName('bar')).ToBe(1);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestAddNameDeduplicates;
var
  Map: TGocciaSourceMap;
begin
  Map := TGocciaSourceMap.Create;
  try
    Expect<Integer>(Map.AddName('foo')).ToBe(0);
    Expect<Integer>(Map.AddName('foo')).ToBe(0);
    Expect<Integer>(Map.AddName('bar')).ToBe(1);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestBasicMapping;
var
  Map: TGocciaSourceMap;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    Expect<Integer>(Map.SegmentCount).ToBe(0);
    Map.AddMapping(1, 0, 0, 0, 0);
    Expect<Integer>(Map.SegmentCount).ToBe(1);
    Map.AddMapping(1, 5, 0, 0, 5);
    Expect<Integer>(Map.SegmentCount).ToBe(2);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestTranslateExactMatch;
var
  Map: TGocciaSourceMap;
  SrcLine, SrcCol: Integer;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    // Generated line 1, column 0 (0-based) -> source line 10 (0-based), column 5 (0-based)
    Map.AddMapping(1, 0, 0, 10, 5);

    // Translate takes 1-based input (matching parser convention).
    // Column 1 (1-based) matches generated column 0 (0-based).
    // Returns 1-based: source line 11, source col 5 + (1 - 0) = 6
    Expect<Boolean>(Map.Translate(1, 1, SrcLine, SrcCol)).ToBe(True);
    Expect<Integer>(SrcLine).ToBe(11);
    Expect<Integer>(SrcCol).ToBe(6);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestTranslateColumnOffset;
var
  Map: TGocciaSourceMap;
  SrcLine, SrcCol: Integer;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    // Mapping at gen line 1, col 0 (0-based) -> source line 10, col 5 (0-based)
    Map.AddMapping(1, 0, 0, 10, 5);

    // Query at gen line 1, col 4 (1-based) -> source col = 5 + (4 - 0) = 9 (1-based)
    Expect<Boolean>(Map.Translate(1, 4, SrcLine, SrcCol)).ToBe(True);
    Expect<Integer>(SrcLine).ToBe(11);
    Expect<Integer>(SrcCol).ToBe(9);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestTranslateNoMappings;
var
  Map: TGocciaSourceMap;
  SrcLine, SrcCol: Integer;
begin
  Map := TGocciaSourceMap.Create;
  try
    Expect<Boolean>(Map.Translate(1, 0, SrcLine, SrcCol)).ToBe(False);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestTranslateFallbackToPrecedingLine;
var
  Map: TGocciaSourceMap;
  SrcLine, SrcCol: Integer;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    // Only a mapping on line 1 -> source line 10 (0-based)
    Map.AddMapping(1, 0, 0, 10, 0);

    // Query on line 3 (1-based) should fall back to the line 1 mapping.
    // Returns 1-based: source line = 10 + 1 + (3 - 1) = 13
    Expect<Boolean>(Map.Translate(3, 5, SrcLine, SrcCol)).ToBe(True);
    Expect<Integer>(SrcLine).ToBe(13);
    Expect<Integer>(SrcCol).ToBe(5);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestToJSONStructure;
var
  Map: TGocciaSourceMap;
  JSON: string;
begin
  Map := TGocciaSourceMap.Create('output.js');
  try
    Map.AddSource('input.js');
    Map.AddMapping(1, 0, 0, 0, 0);
    JSON := Map.ToJSON;

    // Verify JSON structure contains required fields
    Expect<Boolean>(Pos('"version":3', JSON) > 0).ToBe(True);
    Expect<Boolean>(Pos('"file":"output.js"', JSON) > 0).ToBe(True);
    Expect<Boolean>(Pos('"sources":["input.js"]', JSON) > 0).ToBe(True);
    Expect<Boolean>(Pos('"names":[]', JSON) > 0).ToBe(True);
    Expect<Boolean>(Pos('"mappings":"', JSON) > 0).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestToJSONMappings;
var
  Map: TGocciaSourceMap;
  JSON: string;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    // Single mapping: gen line 1, col 0, source 0, line 0, col 0
    // Delta encoding: all zeros -> VLQ "AAAA"
    Map.AddMapping(1, 0, 0, 0, 0);
    JSON := Map.ToJSON;
    Expect<Boolean>(Pos('"mappings":"AAAA"', JSON) > 0).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestToJSONSourcesContent;
var
  Map: TGocciaSourceMap;
  JSON: string;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    Map.SetSourceContent(0, 'const x = 1;');
    Map.AddMapping(1, 0, 0, 0, 0);
    JSON := Map.ToJSON;
    Expect<Boolean>(Pos('"sourcesContent":["const x = 1;"]', JSON) > 0).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestToJSONNoContentWhenEmpty;
var
  Map: TGocciaSourceMap;
  JSON: string;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    Map.AddMapping(1, 0, 0, 0, 0);
    JSON := Map.ToJSON;
    // sourcesContent should not appear when no content is set
    Expect<Boolean>(Pos('sourcesContent', JSON) > 0).ToBe(False);
  finally
    Map.Free;
  end;
end;

// Build a source map, serialize to JSON, parse with consumer, verify positions
procedure TSourceMapTests.TestRoundTripThroughConsumer;
var
  Map: TGocciaSourceMap;
  Consumer: TGocciaSourceMapConsumer;
  JSON: string;
  SourceFile: string;
  SrcLine, SrcCol: Integer;
begin
  Map := TGocciaSourceMap.Create('output.js');
  try
    Map.AddSource('input.js');
    Map.AddMapping(1, 0, 0, 0, 0);     // gen 1:0 -> source 0:0
    Map.AddMapping(1, 10, 0, 5, 3);    // gen 1:10 -> source 5:3
    Map.AddMapping(2, 0, 0, 10, 0);    // gen 2:0 -> source 10:0
    JSON := Map.ToJSON;
  finally
    Map.Free;
  end;

  Consumer := TGocciaSourceMapConsumer.CreateFromJSON(JSON);
  try
    // First mapping
    Expect<Boolean>(Consumer.Translate(1, 0, SourceFile, SrcLine, SrcCol)).ToBe(True);
    Expect<string>(SourceFile).ToBe('input.js');
    Expect<Integer>(SrcLine).ToBe(0);
    Expect<Integer>(SrcCol).ToBe(0);

    // Second mapping
    Expect<Boolean>(Consumer.Translate(1, 10, SourceFile, SrcLine, SrcCol)).ToBe(True);
    Expect<string>(SourceFile).ToBe('input.js');
    Expect<Integer>(SrcLine).ToBe(5);
    Expect<Integer>(SrcCol).ToBe(3);

    // Third mapping
    Expect<Boolean>(Consumer.Translate(2, 0, SourceFile, SrcLine, SrcCol)).ToBe(True);
    Expect<string>(SourceFile).ToBe('input.js');
    Expect<Integer>(SrcLine).ToBe(10);
    Expect<Integer>(SrcCol).ToBe(0);
  finally
    Consumer.Free;
  end;
end;

procedure TSourceMapTests.TestRoundTripWithNames;
var
  Map: TGocciaSourceMap;
  Consumer: TGocciaSourceMapConsumer;
  JSON: string;
  SourceFile, Name: string;
  SrcLine, SrcCol: Integer;
  NameIdx: Integer;
begin
  Map := TGocciaSourceMap.Create('output.js');
  try
    Map.AddSource('input.js');
    NameIdx := Map.AddName('myFunction');
    Map.AddMapping(1, 0, 0, 5, 0, NameIdx);
    JSON := Map.ToJSON;
  finally
    Map.Free;
  end;

  Consumer := TGocciaSourceMapConsumer.CreateFromJSON(JSON);
  try
    Expect<Boolean>(Consumer.TranslateWithName(1, 0, SourceFile,
      SrcLine, SrcCol, Name)).ToBe(True);
    Expect<string>(Name).ToBe('myFunction');
    Expect<Integer>(SrcLine).ToBe(5);
    Expect<Integer>(SrcCol).ToBe(0);
  finally
    Consumer.Free;
  end;
end;

procedure TSourceMapTests.TestToInlineComment;
var
  Map: TGocciaSourceMap;
  Comment: string;
begin
  Map := TGocciaSourceMap.Create('output.js');
  try
    Map.AddSource('input.js');
    Map.AddMapping(1, 0, 0, 0, 0);
    Comment := Map.ToInlineComment;
    Expect<Boolean>(Pos('//# sourceMappingURL=data:application/json;charset=utf-8;base64,', Comment) = 1).ToBe(True);
  finally
    Map.Free;
  end;
end;

procedure TSourceMapTests.TestMultipleSourceFiles;
var
  Map: TGocciaSourceMap;
  Consumer: TGocciaSourceMapConsumer;
  JSON: string;
  SourceFile: string;
  SrcLine, SrcCol: Integer;
  SrcA, SrcB: Integer;
begin
  Map := TGocciaSourceMap.Create('bundle.js');
  try
    SrcA := Map.AddSource('a.js');
    SrcB := Map.AddSource('b.js');
    Map.AddMapping(1, 0, SrcA, 0, 0);   // gen 1:0 -> a.js 0:0
    Map.AddMapping(2, 0, SrcB, 10, 5);  // gen 2:0 -> b.js 10:5
    JSON := Map.ToJSON;
  finally
    Map.Free;
  end;

  Consumer := TGocciaSourceMapConsumer.CreateFromJSON(JSON);
  try
    Expect<Boolean>(Consumer.Translate(1, 0, SourceFile, SrcLine, SrcCol)).ToBe(True);
    Expect<string>(SourceFile).ToBe('a.js');
    Expect<Integer>(SrcLine).ToBe(0);

    Expect<Boolean>(Consumer.Translate(2, 0, SourceFile, SrcLine, SrcCol)).ToBe(True);
    Expect<string>(SourceFile).ToBe('b.js');
    Expect<Integer>(SrcLine).ToBe(10);
    Expect<Integer>(SrcCol).ToBe(5);
  finally
    Consumer.Free;
  end;
end;

procedure TSourceMapTests.TestSemicolonSeparatedLines;
var
  Map: TGocciaSourceMap;
  JSON: string;
  MappingsStart, MappingsEnd: Integer;
  Mappings: string;
begin
  Map := TGocciaSourceMap.Create;
  try
    Map.AddSource('input.js');
    // Mapping on line 1 and line 3 — line 2 is empty, should produce a ';'
    Map.AddMapping(1, 0, 0, 0, 0);
    Map.AddMapping(3, 0, 0, 2, 0);
    JSON := Map.ToJSON;

    // Extract the mappings value
    MappingsStart := Pos('"mappings":"', JSON);
    Expect<Boolean>(MappingsStart > 0).ToBe(True);
    MappingsStart := MappingsStart + Length('"mappings":"');
    MappingsEnd := MappingsStart;
    while (MappingsEnd <= Length(JSON)) and (JSON[MappingsEnd] <> '"') do
      Inc(MappingsEnd);
    Mappings := Copy(JSON, MappingsStart, MappingsEnd - MappingsStart);

    // Should have at least one semicolon separator for the empty line 2
    Expect<Boolean>(Pos(';;', Mappings) > 0).ToBe(True);
  finally
    Map.Free;
  end;
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TSourceMapTests.Create('SourceMap'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.
