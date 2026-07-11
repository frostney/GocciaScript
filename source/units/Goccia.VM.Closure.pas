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
    // Marks static captures whose path crosses the function environment that
    // owns DynamicVarScope, allowing eval-created vars to shadow them.
    FDynamicVarUpvalues: array of Boolean;
    FHomeObject: TGocciaObjectValue;
    FHomeClass: TGocciaObjectValue;
    FFunctionValue: TGocciaValue;
    FGlobalScope: TGocciaScope;
    FDynamicVarScope: TGocciaScope;
    FNewTarget: TGocciaValue;
    FAllowsNewTarget: Boolean;
  public
    constructor Create(const ATemplate: TGocciaFunctionTemplate;
      const AUpvalueCount: Integer = 0);
    destructor Destroy; override;
    function Clone: TGocciaBytecodeClosure;
    procedure SetUpvalue(const AIndex: Integer; const AUpvalue: TGocciaBytecodeUpvalue);
    function GetUpvalue(const AIndex: Integer): TGocciaBytecodeUpvalue;
    procedure SetDynamicVarUpvalue(const AIndex: Integer;
      const AEnabled: Boolean);
    function IsDynamicVarUpvalue(const AIndex: Integer): Boolean;
    function GetUpvalueCount: Integer;
    property Template: TGocciaFunctionTemplate read FTemplate;
    property UpvalueCount: Integer read GetUpvalueCount;
    property HomeObject: TGocciaObjectValue read FHomeObject write FHomeObject;
    property HomeClass: TGocciaObjectValue read FHomeClass write FHomeClass;
    property FunctionValue: TGocciaValue read FFunctionValue write FFunctionValue;
    property GlobalScope: TGocciaScope read FGlobalScope write FGlobalScope;
    property DynamicVarScope: TGocciaScope read FDynamicVarScope write FDynamicVarScope;
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
  FDynamicVarScope := nil;
  FNewTarget := nil;
  FAllowsNewTarget := False;
  SetLength(FUpvalues, AUpvalueCount);
  SetLength(FDynamicVarUpvalues, AUpvalueCount);
end;

destructor TGocciaBytecodeClosure.Destroy;
begin
  SetLength(FUpvalues, 0);
  SetLength(FDynamicVarUpvalues, 0);
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
  Result.FDynamicVarScope := FDynamicVarScope;
  Result.FNewTarget := FNewTarget;
  Result.FAllowsNewTarget := FAllowsNewTarget;
  for I := 0 to High(FUpvalues) do
  begin
    Result.FUpvalues[I] := FUpvalues[I];
    Result.FDynamicVarUpvalues[I] := FDynamicVarUpvalues[I];
  end;
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

procedure TGocciaBytecodeClosure.SetDynamicVarUpvalue(const AIndex: Integer;
  const AEnabled: Boolean);
begin
  if (AIndex < 0) or (AIndex >= Length(FDynamicVarUpvalues)) then
    Exit;
  FDynamicVarUpvalues[AIndex] := AEnabled;
end;

function TGocciaBytecodeClosure.IsDynamicVarUpvalue(
  const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(FDynamicVarUpvalues)) and
    FDynamicVarUpvalues[AIndex];
end;

function TGocciaBytecodeClosure.GetUpvalueCount: Integer;
begin
  Result := Length(FUpvalues);
end;

end.
