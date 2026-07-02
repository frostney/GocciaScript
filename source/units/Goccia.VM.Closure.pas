unit Goccia.VM.Closure;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.VM.Upvalue;

type
  TGocciaBytecodeClosure = class
  private
    FTemplate: TGocciaFunctionTemplate;
    FUpvalues: array of TGocciaBytecodeUpvalue;
    FHomeObject: TGocciaObjectValue;
    FHomeClass: TGocciaObjectValue;
    FFunctionValue: TGocciaValue;
    FGlobalScope: TGocciaScope;
    FNewTarget: TGocciaValue;
    FAllowsNewTarget: Boolean;
  public
    constructor Create(const ATemplate: TGocciaFunctionTemplate;
      const AUpvalueCount: Integer = 0);
    destructor Destroy; override;
    function Clone: TGocciaBytecodeClosure;
    procedure SetUpvalue(const AIndex: Integer; const AUpvalue: TGocciaBytecodeUpvalue);
    function GetUpvalue(const AIndex: Integer): TGocciaBytecodeUpvalue;
    function GetUpvalueCount: Integer;
    property Template: TGocciaFunctionTemplate read FTemplate;
    property UpvalueCount: Integer read GetUpvalueCount;
    property HomeObject: TGocciaObjectValue read FHomeObject write FHomeObject;
    property HomeClass: TGocciaObjectValue read FHomeClass write FHomeClass;
    property FunctionValue: TGocciaValue read FFunctionValue write FFunctionValue;
    property GlobalScope: TGocciaScope read FGlobalScope write FGlobalScope;
    property NewTarget: TGocciaValue read FNewTarget write FNewTarget;
    property AllowsNewTarget: Boolean read FAllowsNewTarget write FAllowsNewTarget;
  end;

implementation

constructor TGocciaBytecodeClosure.Create(const ATemplate: TGocciaFunctionTemplate;
  const AUpvalueCount: Integer);
begin
  inherited Create;
  FTemplate := ATemplate;
  FHomeObject := nil;
  FHomeClass := nil;
  FFunctionValue := nil;
  FGlobalScope := nil;
  FNewTarget := nil;
  FAllowsNewTarget := False;
  SetLength(FUpvalues, AUpvalueCount);
end;

destructor TGocciaBytecodeClosure.Destroy;
begin
  SetLength(FUpvalues, 0);
  inherited;
end;

function TGocciaBytecodeClosure.Clone: TGocciaBytecodeClosure;
var
  I: Integer;
begin
  Result := TGocciaBytecodeClosure.Create(FTemplate, Length(FUpvalues));
  Result.FHomeObject := FHomeObject;
  Result.FHomeClass := FHomeClass;
  Result.FFunctionValue := FFunctionValue;
  Result.FGlobalScope := FGlobalScope;
  Result.FNewTarget := FNewTarget;
  Result.FAllowsNewTarget := FAllowsNewTarget;
  for I := 0 to High(FUpvalues) do
    Result.FUpvalues[I] := FUpvalues[I];
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
