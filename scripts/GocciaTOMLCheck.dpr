program GocciaTOMLCheck;

{$I ../units/Goccia.inc}

uses
  Classes,
  SysUtils,

  StringBuffer,

  Goccia.GarbageCollector,
  Goccia.TextFiles,
  Goccia.TOML,
  Goccia.Values.Primitives;

function EscapeJSONString(const AValue: string): string;
var
  Buffer: TStringBuffer;
  Ch: Char;
  I: Integer;
begin
  Buffer := TStringBuffer.Create(Length(AValue));
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    case Ch of
      '"': Buffer.Append('\"');
      '\': Buffer.Append('\\');
      '/': Buffer.Append('\/');
      #8: Buffer.Append('\b');
      #9: Buffer.Append('\t');
      #10: Buffer.Append('\n');
      #12: Buffer.Append('\f');
      #13: Buffer.Append('\r');
    else
      if Ord(Ch) < 32 then
        Buffer.Append('\u' + IntToHex(Ord(Ch), 4))
      else
        Buffer.AppendChar(Ch);
    end;
  end;
  Result := Buffer.ToString;
end;

function QuoteJSONString(const AValue: string): string;
begin
  Result := '"' + EscapeJSONString(AValue) + '"';
end;

function ScalarKindName(const AKind: TGocciaTOMLScalarKind): string;
begin
  case AKind of
    tskString:
      Result := 'string';
    tskInteger:
      Result := 'integer';
    tskFloat:
      Result := 'float';
    tskBool:
      Result := 'bool';
    tskDateTime:
      Result := 'datetime';
    tskDateTimeLocal:
      Result := 'datetime-local';
    tskDateLocal:
      Result := 'date-local';
    tskTimeLocal:
      Result := 'time-local';
  else
    Result := 'string';
  end;
end;

function SerializeNode(const ANode: TGocciaTOMLNode): string;
var
  Buffer: TStringBuffer;
  I: Integer;
  Pair: TGocciaTOMLNodeMap.TKeyValuePair;
begin
  case ANode.Kind of
    tnkScalar:
      Result := '{"type":' + QuoteJSONString(ScalarKindName(ANode.ScalarKind)) +
        ',"value":' + QuoteJSONString(ANode.CanonicalValue) + '}';
    tnkArray,
    tnkArrayOfTables:
      begin
        Buffer := TStringBuffer.Create;
        Buffer.AppendChar('[');
        for I := 0 to ANode.Items.Count - 1 do
        begin
          if I > 0 then
            Buffer.AppendChar(',');
          Buffer.Append(SerializeNode(ANode.Items[I]));
        end;
        Buffer.AppendChar(']');
        Result := Buffer.ToString;
      end;
    tnkTable:
      begin
        Buffer := TStringBuffer.Create;
        Buffer.AppendChar('{');
        I := 0;
        for Pair in ANode.Children do
        begin
          if I > 0 then
            Buffer.AppendChar(',');
          Buffer.Append(QuoteJSONString(Pair.Key));
          Buffer.AppendChar(':');
          Buffer.Append(SerializeNode(Pair.Value));
          Inc(I);
        end;
        Buffer.AppendChar('}');
        Result := Buffer.ToString;
      end;
  else
    Result := 'null';
  end;
end;

procedure WriteUTF8Line(const AText: string);
var
  OutputText: UTF8String;
  Stream: THandleStream;
begin
  OutputText := UTF8String(AText + #10);
  Stream := THandleStream.Create(TTextRec(Output).Handle);
  try
    if Length(OutputText) > 0 then
      Stream.WriteBuffer(Pointer(OutputText)^, Length(OutputText));
  finally
    Stream.Free;
  end;
end;

var
  ExitCode: Integer;
  Parser: TGocciaTOMLParser;
  Root: TGocciaTOMLNode;
  SourceText: string;
begin
  if ParamCount <> 1 then
    Halt(2);

  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;

  Parser := TGocciaTOMLParser.Create;
  try
    try
      SourceText := ReadUTF8FileText(ParamStr(1));
      Root := Parser.ParseDocument(SourceText);
      try
        WriteUTF8Line(SerializeNode(Root));
        ExitCode := 0;
      finally
        Root.Free;
      end;
    except
      on E: Exception do
      begin
        WriteUTF8Line(E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Parser.Free;
    TGarbageCollector.Shutdown;
  end;

  Halt(ExitCode);
end.
