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
    // The cells this closure captured, shared with the frame that opened
    // them and with every other closure that captured the same binding. The
    // collector owns them (TGocciaBytecodeCell), so the closure only marks
    // them and never frees one.
    FUpvalues: array of TGocciaBytecodeCell;
    FHomeObject: TGocciaObjectValue;
    FHomeClass: TGocciaObjectValue;
    // The class whose private names the closure's code resolves first: the
    // innermost class body the function was created in (ES2026 §10.2.3
    // OrdinaryFunctionCreate's PrivateEnvironment). Unlike HomeClass it does
    // not depend on the function being a method, so super stays unaffected.
    FPrivateClass: TGocciaObjectValue;
    FFunctionValue: TGocciaValue;
    FGlobalScope: TGocciaScope;
    FNewTarget: TGocciaValue;
    FAllowsNewTarget: Boolean;
  public
    constructor Create(const ATemplate: TGocciaFunctionTemplate;
      const AUpvalueCount: Integer = 0);
    destructor Destroy; override;
    function Clone: TGocciaBytecodeClosure;
    procedure SetUpvalue(const AIndex: Integer; const AUpvalue: TGocciaBytecodeCell);
    function GetUpvalue(const AIndex: Integer): TGocciaBytecodeCell;
    function GetUpvalueCount: Integer;
    property Template: TGocciaFunctionTemplate read FTemplate;
    property UpvalueCount: Integer read GetUpvalueCount;
    property HomeObject: TGocciaObjectValue read FHomeObject write FHomeObject;
    property HomeClass: TGocciaObjectValue read FHomeClass write FHomeClass;
    property PrivateClass: TGocciaObjectValue read FPrivateClass write FPrivateClass;
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
  FPrivateClass := nil;
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
  Result.FPrivateClass := FPrivateClass;
  Result.FFunctionValue := FFunctionValue;
  Result.FGlobalScope := FGlobalScope;
  Result.FNewTarget := FNewTarget;
  Result.FAllowsNewTarget := FAllowsNewTarget;
  for I := 0 to High(FUpvalues) do
    Result.FUpvalues[I] := FUpvalues[I];
end;

procedure TGocciaBytecodeClosure.SetUpvalue(const AIndex: Integer;
  const AUpvalue: TGocciaBytecodeCell);
begin
  if (AIndex < 0) or (AIndex >= Length(FUpvalues)) then
    Exit;
  FUpvalues[AIndex] := AUpvalue;
end;

function TGocciaBytecodeClosure.GetUpvalue(
  const AIndex: Integer): TGocciaBytecodeCell;
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
