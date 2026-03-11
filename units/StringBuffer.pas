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
    class function Create(const ACapacity: Integer = 64): TStringBuffer; static; inline;
    procedure Append(const S: AnsiString); inline;
    procedure AppendChar(const C: AnsiChar); inline;
    procedure Clear; inline;
    function ToString: AnsiString; inline;
    property Length: Integer read GetLength;
  end;

implementation

class function TStringBuffer.Create(const ACapacity: Integer): TStringBuffer;
begin
  Result.FLen := 0;
  if ACapacity > 0 then
    Result.FCap := ACapacity
  else
    Result.FCap := 64;
  SetLength(Result.FData, Result.FCap);
end;

procedure TStringBuffer.AppendChar(const C: AnsiChar);
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
