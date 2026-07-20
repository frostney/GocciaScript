unit Goccia.REPL.LineEditor;

{$I Goccia.inc}

interface

uses
  Classes;

type
  TLineReadResult = (lrLine, lrExit);

  TLineEditor = class
  private
    FHistory: TStringList;
    FHistoryIndex: Integer;
    {$IFDEF UNIX}
    procedure SetRawMode;
    procedure RestoreTerminal;
    function ReadByte(out AByte: Byte): Boolean;
    function ReadCharacter(out ACharacter: string): Boolean;
    procedure RedrawLine(const APrompt, ABuffer: string; ACursorPos: Integer);
    function ReadLineRaw(const APrompt: string; out ALine: string): TLineReadResult;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function ReadLine(const APrompt: string; out ALine: string): TLineReadResult;
    procedure AddToHistory(const ALine: string);
  end;

implementation

uses
  SysUtils
  {$IFDEF UNIX},
  TextEncoding,
  BaseUnix,
  termio
  {$ENDIF};

{$IFDEF UNIX}
var
  OriginalTermios: Termios;
  TermiosSaved: Boolean = False;

procedure TLineEditor.SetRawMode;
var
  Raw: Termios;
begin
  if not TermiosSaved then
  begin
    TCGetAttr(0, OriginalTermios);
    TermiosSaved := True;
  end;
  Raw := OriginalTermios;
  Raw.c_lflag := Raw.c_lflag and (not (ICANON or ECHO));
  Raw.c_iflag := Raw.c_iflag and (not (ICRNL or IXON));
  Raw.c_cc[VMIN] := 1;
  Raw.c_cc[VTIME] := 0;
  TCSetAttr(0, TCSAFLUSH, Raw);
end;

procedure TLineEditor.RestoreTerminal;
begin
  if TermiosSaved then
    TCSetAttr(0, TCSAFLUSH, OriginalTermios);
end;

function TLineEditor.ReadByte(out AByte: Byte): Boolean;
var
  BytesRead: TSSize;
begin
  AByte := 0;
  BytesRead := fpRead(0, AByte, 1);
  Result := BytesRead = 1;
end;

function TLineEditor.ReadCharacter(out ACharacter: string): Boolean;
var
  Bytes: TBytes;
  ByteValue: Byte;
  ExpectedLength, I: Integer;
begin
  ACharacter := '';
  if not ReadByte(ByteValue) then
    Exit(False);

  if ByteValue < $80 then
  begin
    ACharacter := Char(ByteValue);
    Exit(True);
  end;

  if (ByteValue >= $C2) and (ByteValue <= $DF) then
    ExpectedLength := 2
  else if (ByteValue >= $E0) and (ByteValue <= $EF) then
    ExpectedLength := 3
  else if (ByteValue >= $F0) and (ByteValue <= $F4) then
    ExpectedLength := 4
  else
    ExpectedLength := 1;

  SetLength(Bytes, ExpectedLength);
  Bytes[0] := ByteValue;
  I := 1;
  while (I < ExpectedLength) and ReadByte(ByteValue) do
  begin
    Bytes[I] := ByteValue;
    Inc(I);
  end;
  SetLength(Bytes, I);
  ACharacter := DecodeUTF8WithReplacement(Bytes);
  Result := True;
end;

procedure TLineEditor.RedrawLine(const APrompt, ABuffer: string; ACursorPos: Integer);
var
  Tail: Integer;
begin
  Write(#13);
  Write(APrompt);
  Write(ABuffer);
  Write(#27'[K');
  Tail := Length(ABuffer) - ACursorPos;
  if Tail > 0 then
    Write(#27'[' + IntToStr(Tail) + 'D');
end;

function TLineEditor.ReadLineRaw(const APrompt: string; out ALine: string): TLineReadResult;
var
  Buffer: string;
  Character, SavedLine: string;
  C: Char;
  CursorPos, DeleteLength: Integer;

  function PreviousCharacterLength: Integer;
  begin
    Result := 1;
    if (CursorPos >= 2) and
       (Ord(Buffer[CursorPos]) >= $DC00) and
       (Ord(Buffer[CursorPos]) <= $DFFF) and
       (Ord(Buffer[CursorPos - 1]) >= $D800) and
       (Ord(Buffer[CursorPos - 1]) <= $DBFF) then
      Result := 2;
  end;

  function NextCharacterLength: Integer;
  begin
    Result := 1;
    if (CursorPos + 2 <= Length(Buffer)) and
       (Ord(Buffer[CursorPos + 1]) >= $D800) and
       (Ord(Buffer[CursorPos + 1]) <= $DBFF) and
       (Ord(Buffer[CursorPos + 2]) >= $DC00) and
       (Ord(Buffer[CursorPos + 2]) <= $DFFF) then
      Result := 2;
  end;
begin
  Result := lrLine;
  Buffer := '';
  CursorPos := 0;
  SavedLine := '';
  FHistoryIndex := FHistory.Count;

  Write(APrompt);
  SetRawMode;
  try
    while True do
    begin
      if not ReadCharacter(Character) then
      begin
        WriteLn;
        ALine := Buffer;
        if Buffer = '' then
          Result := lrExit;
        Exit;
      end;
      if Length(Character) = 1 then
        C := Character[1]
      else
        C := #0;
      case C of
        #10, #13:
        begin
          WriteLn;
          ALine := Buffer;
          Exit;
        end;

        #3:
        begin
          WriteLn;
          Result := lrExit;
          ALine := '';
          Exit;
        end;

        #4:
        begin
          if Buffer = '' then
          begin
            WriteLn;
            Result := lrExit;
            ALine := '';
            Exit;
          end;
        end;

        #127, #8:
        begin
          if CursorPos > 0 then
          begin
            DeleteLength := PreviousCharacterLength;
            System.Delete(Buffer, CursorPos - DeleteLength + 1, DeleteLength);
            Dec(CursorPos, DeleteLength);
            RedrawLine(APrompt, Buffer, CursorPos);
          end;
        end;

        #27:
        begin
          if ReadCharacter(Character) and (Character = '[') and
             ReadCharacter(Character) and (Length(Character) = 1) then
          begin
            C := Character[1];
            case C of
              'A':
              begin
                if FHistoryIndex > 0 then
                begin
                  if FHistoryIndex = FHistory.Count then
                    SavedLine := Buffer;
                  Dec(FHistoryIndex);
                  Buffer := FHistory[FHistoryIndex];
                  CursorPos := Length(Buffer);
                  RedrawLine(APrompt, Buffer, CursorPos);
                end;
              end;
              'B':
              begin
                if FHistoryIndex < FHistory.Count then
                begin
                  Inc(FHistoryIndex);
                  if FHistoryIndex = FHistory.Count then
                    Buffer := SavedLine
                  else
                    Buffer := FHistory[FHistoryIndex];
                  CursorPos := Length(Buffer);
                  RedrawLine(APrompt, Buffer, CursorPos);
                end;
              end;
              'C':
              begin
                if CursorPos < Length(Buffer) then
                begin
                  Inc(CursorPos, NextCharacterLength);
                  Write(#27'[C');
                end;
              end;
              'D':
              begin
                if CursorPos > 0 then
                begin
                  Dec(CursorPos, PreviousCharacterLength);
                  Write(#27'[D');
                end;
              end;
              'H':
              begin
                CursorPos := 0;
                RedrawLine(APrompt, Buffer, CursorPos);
              end;
              'F':
              begin
                CursorPos := Length(Buffer);
                RedrawLine(APrompt, Buffer, CursorPos);
              end;
              '3':
              begin
                if ReadCharacter(Character) and (Character = '~') and
                   (CursorPos < Length(Buffer)) then
                begin
                  System.Delete(Buffer, CursorPos + 1, NextCharacterLength);
                  RedrawLine(APrompt, Buffer, CursorPos);
                end;
              end;
            end;
          end;
        end;

        #1:
        begin
          CursorPos := 0;
          RedrawLine(APrompt, Buffer, CursorPos);
        end;

        #5:
        begin
          CursorPos := Length(Buffer);
          RedrawLine(APrompt, Buffer, CursorPos);
        end;

        #21:
        begin
          Buffer := '';
          CursorPos := 0;
          RedrawLine(APrompt, Buffer, CursorPos);
        end;

        #11:
        begin
          Buffer := Copy(Buffer, 1, CursorPos);
          RedrawLine(APrompt, Buffer, CursorPos);
        end;

        #12:
        begin
          Write(#27'[2J'#27'[H');
          RedrawLine(APrompt, Buffer, CursorPos);
        end;
      else
        if (Length(Character) > 1) or (C >= ' ') then
        begin
          if CursorPos = Length(Buffer) then
          begin
            Buffer := Buffer + Character;
            Inc(CursorPos, Length(Character));
            Write(Character);
          end
          else
          begin
            System.Insert(Character, Buffer, CursorPos + 1);
            Inc(CursorPos, Length(Character));
            RedrawLine(APrompt, Buffer, CursorPos);
          end;
        end;
      end;
    end;
  finally
    RestoreTerminal;
  end;
end;
{$ENDIF}

constructor TLineEditor.Create;
begin
  FHistory := TStringList.Create;
  FHistoryIndex := 0;
end;

destructor TLineEditor.Destroy;
begin
  FHistory.Free;
  inherited;
end;

function TLineEditor.ReadLine(const APrompt: string; out ALine: string): TLineReadResult;
begin
  {$IFDEF UNIX}
  Result := ReadLineRaw(APrompt, ALine);
  {$ELSE}
  if System.Eof(Input) then
  begin
    Result := lrExit;
    ALine := '';
    Exit;
  end;
  Result := lrLine;
  Write(APrompt);
  System.ReadLn(ALine);
  {$ENDIF}
end;

procedure TLineEditor.AddToHistory(const ALine: string);
begin
  if (ALine <> '') and
    ((FHistory.Count = 0) or (FHistory[FHistory.Count - 1] <> ALine)) then
    FHistory.Add(ALine);
end;

end.
