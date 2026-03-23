unit Souffle.JSON;

{$I Goccia.inc}

interface

uses
  Souffle.Value,
  Souffle.VM;

type
  TSouffleJSONParser = class
  private
    FVM: TSouffleVM;
    FResult: TSouffleValue;
  public
    constructor Create(const AVM: TSouffleVM);
    function Parse(const AText: string): TSouffleValue;
  end;

implementation

uses
  Classes,
  Generics.Collections,
  SysUtils,

  GarbageCollector.Generic,
  JSONParser.Generic,
  Souffle.Compound,
  Souffle.Heap;

type
  TSouffleJSONVisitor = class(TAbstractJSONParser)
  private
    FVM: TSouffleVM;
    FStack: TList<TSouffleValue>;
    FKeyStack: TStringList;
    FResult: TSouffleValue;
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
    procedure EmitValue(const AValue: TSouffleValue);
  public
    constructor Create(const AVM: TSouffleVM);
    destructor Destroy; override;
    function Parse(const AText: string): TSouffleValue;
  end;

{ TSouffleJSONParser }

constructor TSouffleJSONParser.Create(const AVM: TSouffleVM);
begin
  inherited Create;
  FVM := AVM;
end;

function TSouffleJSONParser.Parse(const AText: string): TSouffleValue;
var
  Visitor: TSouffleJSONVisitor;
begin
  Visitor := TSouffleJSONVisitor.Create(FVM);
  try
    Result := Visitor.Parse(AText);
  finally
    Visitor.Free;
  end;
end;

{ TSouffleJSONVisitor }

constructor TSouffleJSONVisitor.Create(const AVM: TSouffleVM);
begin
  inherited Create;
  FVM := AVM;
  FStack := TList<TSouffleValue>.Create;
  FKeyStack := TStringList.Create;
  FResult := SouffleNil;
end;

destructor TSouffleJSONVisitor.Destroy;
begin
  FStack.Free;
  FKeyStack.Free;
  inherited;
end;

function TSouffleJSONVisitor.Parse(const AText: string): TSouffleValue;
begin
  DoParse(AText);
  Result := FResult;
end;

procedure TSouffleJSONVisitor.EmitValue(const AValue: TSouffleValue);
var
  Container: TSouffleValue;
  Key: string;
begin
  if FStack.Count = 0 then
    FResult := AValue
  else
  begin
    Container := FStack[FStack.Count - 1];
    if Container.AsReference is TSouffleArray then
      TSouffleArray(Container.AsReference).Push(AValue)
    else if Container.AsReference is TSouffleRecord then
    begin
      Key := FKeyStack[FKeyStack.Count - 1];
      FKeyStack.Delete(FKeyStack.Count - 1);
      TSouffleRecord(Container.AsReference).Put(Key, AValue);
    end;
  end;
end;

procedure TSouffleJSONVisitor.OnNull;
begin
  EmitValue(SouffleNilWithFlags(1));
end;

procedure TSouffleJSONVisitor.OnBoolean(const AValue: Boolean);
begin
  EmitValue(SouffleBoolean(AValue));
end;

procedure TSouffleJSONVisitor.OnString(const AValue: string);
begin
  EmitValue(SouffleString(AValue));
end;

procedure TSouffleJSONVisitor.OnInteger(const AValue: Int64);
begin
  EmitValue(SouffleInteger(AValue));
end;

procedure TSouffleJSONVisitor.OnFloat(const AValue: Double);
begin
  EmitValue(SouffleFloat(AValue));
end;

procedure TSouffleJSONVisitor.OnBeginObject;
var
  Rec: TSouffleRecord;
begin
  Rec := TSouffleRecord.Create(4);
  if Assigned(FVM) then
    Rec.Delegate := FVM.RecordDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Rec);
  FStack.Add(SouffleReference(Rec));
end;

procedure TSouffleJSONVisitor.OnObjectKey(const AKey: string);
begin
  FKeyStack.Add(AKey);
end;

procedure TSouffleJSONVisitor.OnEndObject;
var
  Val: TSouffleValue;
begin
  Val := FStack[FStack.Count - 1];
  FStack.Delete(FStack.Count - 1);
  EmitValue(Val);
end;

procedure TSouffleJSONVisitor.OnBeginArray;
var
  Arr: TSouffleArray;
begin
  Arr := TSouffleArray.Create(0);
  if Assigned(FVM) then
    Arr.Delegate := FVM.ArrayDelegate;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Arr);
  FStack.Add(SouffleReference(Arr));
end;

procedure TSouffleJSONVisitor.OnEndArray;
var
  Val: TSouffleValue;
begin
  Val := FStack[FStack.Count - 1];
  FStack.Delete(FStack.Count - 1);
  EmitValue(Val);
end;

end.
