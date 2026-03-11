unit StringBuffer;

{$I Goccia.inc}

interface

type
  TStringBuffer = record
  private
    FData: AnsiString;
    FLen: Integer;
    FCap: Integer;
    function GetLength: Integer; inline;
  public
    class function Create(ACapacity: Integer = 64): TStringBuffer; static; inline;
    procedure Append(const S: AnsiString); inline;
    procedure AppendChar(C: AnsiChar); inline;
    procedure Clear; inline;
    function ToString: AnsiString;
    property Length: Integer read GetLength;
  end;

implementation

class function TStringBuffer.Create(ACapacity: Integer): TStringBuffer;
begin
  Result.FLen := 0;
  Result.FCap := ACapacity;
  SetLength(Result.FData, Result.FCap);
end;

procedure TStringBuffer.AppendChar(C: AnsiChar);
begin
  if FLen + 1 > FCap then
  begin
    FCap := FCap * 2;
    SetLength(FData, FCap);
  end;
  Inc(FLen);
  FData[FLen] := C;
end;

procedure TStringBuffer.Append(const S: AnsiString);
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
  Move(S[1], FData[FLen + 1], SLen);
  Inc(FLen, SLen);
end;

procedure TStringBuffer.Clear;
begin
  FLen := 0;
end;

function TStringBuffer.ToString: AnsiString;
begin
  Result := Copy(FData, 1, FLen);
end;

function TStringBuffer.GetLength: Integer;
begin
  Result := FLen;
end;

end.
