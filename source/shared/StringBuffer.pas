unit StringBuffer;

{$I Shared.inc}

interface

const
  DEFAULT_CAPACITY = 64;

type
  TStringBuffer = record
  private
    FData: UnicodeString;
    FLen: Integer;
    FCap: Integer;
    function GetLength: Integer; {$IFDEF FPC}inline;{$ENDIF}
  public
    class function Create(const ACapacity: Integer = DEFAULT_CAPACITY): TStringBuffer; static; {$IFDEF FPC}inline;{$ENDIF}
    procedure Append(const S: UnicodeString); {$IFDEF FPC}inline;{$ENDIF}
    procedure AppendChar(const C: WideChar); {$IFDEF FPC}inline;{$ENDIF}
    procedure Clear; {$IFDEF FPC}inline;{$ENDIF}
    function ToString: UnicodeString; {$IFDEF FPC}inline;{$ENDIF}
    property Length: Integer read GetLength;
  end;

implementation

class function TStringBuffer.Create(const ACapacity: Integer): TStringBuffer;
begin
  Result.FLen := 0;
  if ACapacity > 0 then
    Result.FCap := ACapacity
  else
    Result.FCap := DEFAULT_CAPACITY;
  SetLength(Result.FData, Result.FCap);
end;

procedure TStringBuffer.AppendChar(const C: WideChar);
begin
  if FLen + 1 > FCap then
  begin
    FCap := FCap * 2;
    SetLength(FData, FCap);
  end;
  Inc(FLen);
  FData[FLen] := C;
end;

procedure TStringBuffer.Append(const S: UnicodeString);
var
  SLen, NewCap: Integer;
begin
  SLen := System.Length(S);
  if SLen = 0 then Exit;
  if FLen + SLen > FCap then
  begin
    NewCap := FCap;
    while NewCap < FLen + SLen do NewCap := NewCap * 2;
    FCap := NewCap;
    SetLength(FData, FCap);
  end;
  Move(S[1], FData[FLen + 1], SLen * SizeOf(WideChar));
  Inc(FLen, SLen);
end;

procedure TStringBuffer.Clear;
begin
  FLen := 0;
end;

function TStringBuffer.ToString: UnicodeString;
begin
  Result := Copy(FData, 1, FLen);
end;

function TStringBuffer.GetLength: Integer;
begin
  Result := FLen;
end;

end.
