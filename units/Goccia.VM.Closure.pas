unit Goccia.VM.Closure;

{$I Goccia.inc}

interface

uses
  Souffle.Bytecode.Chunk,

  Goccia.VM.Upvalue;

type
  TGocciaBytecodeClosure = class
  private
    FTemplate: TSouffleFunctionTemplate;
    FUpvalues: array of TGocciaBytecodeUpvalue;
  public
    constructor Create(const ATemplate: TSouffleFunctionTemplate;
      const AUpvalueCount: Integer = 0);
    destructor Destroy; override;
    procedure SetUpvalue(const AIndex: Integer; const AUpvalue: TGocciaBytecodeUpvalue);
    function GetUpvalue(const AIndex: Integer): TGocciaBytecodeUpvalue;
    function GetUpvalueCount: Integer;
    property Template: TSouffleFunctionTemplate read FTemplate;
    property UpvalueCount: Integer read GetUpvalueCount;
  end;

implementation

constructor TGocciaBytecodeClosure.Create(const ATemplate: TSouffleFunctionTemplate;
  const AUpvalueCount: Integer);
begin
  inherited Create;
  FTemplate := ATemplate;
  SetLength(FUpvalues, AUpvalueCount);
end;

destructor TGocciaBytecodeClosure.Destroy;
begin
  SetLength(FUpvalues, 0);
  inherited;
end;

procedure TGocciaBytecodeClosure.SetUpvalue(const AIndex: Integer;
  const AUpvalue: TGocciaBytecodeUpvalue);
begin
  if (AIndex < 0) or (AIndex >= Length(FUpvalues)) then
    Exit;
  FUpvalues[AIndex] := AUpvalue;
end;

function TGocciaBytecodeClosure.GetUpvalue(
  const AIndex: Integer): TGocciaBytecodeUpvalue;
begin
  if (AIndex >= 0) and (AIndex < Length(FUpvalues)) then
    Result := FUpvalues[AIndex]
  else
    Result := nil;
end;

function TGocciaBytecodeClosure.GetUpvalueCount: Integer;
begin
  Result := Length(FUpvalues);
end;

end.
