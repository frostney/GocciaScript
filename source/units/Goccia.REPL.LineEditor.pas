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
    function ReadChar: Char;
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

function TLineEditor.ReadChar: Char;
var
  C: Char;
  BytesRead: TSSize;
begin
  BytesRead := fpRead(0, C, 1);
  if BytesRead <= 0 then
    Result := #4
  else
    Result := C;
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
  CursorPos: Integer;
  SavedLine: string;
  C: Char;
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
      C := ReadChar;
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
            System.Delete(Buffer, CursorPos, 1);
            Dec(CursorPos);
            RedrawLine(APrompt, Buffer, CursorPos);
          end;
        end;

        #27:
        begin
          C := ReadChar;
          if C = '[' then
          begin
            C := ReadChar;
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
                  Inc(CursorPos);
                  Write(#27'[C');
                end;
              end;
              'D':
              begin
                if CursorPos > 0 then
                begin
                  Dec(CursorPos);
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
                C := ReadChar;
                if (C = '~') and (CursorPos < Length(Buffer)) then
                begin
                  System.Delete(Buffer, CursorPos + 1, 1);
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
        if C >= ' ' then
        begin
          if CursorPos = Length(Buffer) then
          begin
            Buffer := Buffer + C;
            Inc(CursorPos);
            Write(C);
          end
          else
          begin
            System.Insert(C, Buffer, CursorPos + 1);
            Inc(CursorPos);
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
