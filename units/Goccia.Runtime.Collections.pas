unit Goccia.Runtime.Collections;

{$I Goccia.inc}

interface

uses
  Math,
  SysUtils,

  Souffle.Compound,
  Souffle.Heap,
  Souffle.Value;

type
  TSouffleMapEntry = record
    Key: TSouffleValue;
    Value: TSouffleValue;
  end;

  TGocciaSouffleMap = class(TSouffleHeapObject)
  private
    FEntries: array of TSouffleMapEntry;
    FCount: Integer;
  public
    constructor Create(const ACapacity: Integer = 4);

    function FindEntry(const AKey: TSouffleValue): Integer;
    procedure SetEntry(const AKey, AValue: TSouffleValue);
    function GetEntry(const AKey: TSouffleValue): TSouffleValue;
    function HasKey(const AKey: TSouffleValue): Boolean;
    function DeleteEntry(const AKey: TSouffleValue): Boolean;
    procedure Clear;

    function GetKeyAt(const AIndex: Integer): TSouffleValue; inline;
    function GetValueAt(const AIndex: Integer): TSouffleValue; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
  end;

  TGocciaSouffleSet = class(TSouffleHeapObject)
  private
    FItems: array of TSouffleValue;
    FCount: Integer;
  public
    constructor Create(const ACapacity: Integer = 4);

    function Contains(const AValue: TSouffleValue): Boolean;
    function Add(const AValue: TSouffleValue): Boolean;
    function Delete(const AValue: TSouffleValue): Boolean;
    procedure Clear;

    function GetItemAt(const AIndex: Integer): TSouffleValue; inline;

    procedure MarkReferences; override;
    function DebugString: string; override;

    property Count: Integer read FCount;
  end;

  TSouffleMapIterKind = (mikKeys, mikValues, mikEntries);

  TGocciaSouffleMapIterator = class(TSouffleHeapObject)
  private
    FMap: TGocciaSouffleMap;
    FIndex: Integer;
    FKind: TSouffleMapIterKind;
  public
    constructor Create(const AMap: TGocciaSouffleMap; const AKind: TSouffleMapIterKind);
    function Next(out ADone: Boolean): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

  TSouffleSetIterKind = (sikValues, sikEntries);

  TGocciaSouffleSetIterator = class(TSouffleHeapObject)
  private
    FSet: TGocciaSouffleSet;
    FIndex: Integer;
    FKind: TSouffleSetIterKind;
  public
    constructor Create(const ASet: TGocciaSouffleSet; const AKind: TSouffleSetIterKind);
    function Next(out ADone: Boolean): TSouffleValue;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

  TSoufflePromiseState = (spssPending, spssFulfilled, spssRejected);

  TSoufflePromise = class;

  TSoufflePromiseReaction = record
    OnFulfilled: TSouffleValue;
    OnRejected: TSouffleValue;
    ResultPromise: TSoufflePromise;
  end;

  TSoufflePromise = class(TSouffleHeapObject)
  private
    FState: TSoufflePromiseState;
    FResult: TSouffleValue;
    FReactions: array of TSoufflePromiseReaction;
    FReactionCount: Integer;
    FAlreadyResolved: Boolean;
  public
    constructor Create;
    procedure Resolve(const AValue: TSouffleValue);
    procedure Reject(const AReason: TSouffleValue);
    procedure AddReaction(const AOnFulfilled, AOnRejected: TSouffleValue;
      const AResultPromise: TSoufflePromise);
    function InvokeThen(const AOnFulfilled, AOnRejected: TSouffleValue): TSoufflePromise;
    procedure TriggerReactions;
    procedure SubscribeTo(const APromise: TSoufflePromise);
    procedure MarkReferences; override;
    function DebugString: string; override;

    property State: TSoufflePromiseState read FState;
    property PromiseResult: TSouffleValue read FResult;
    property AlreadyResolved: Boolean read FAlreadyResolved write FAlreadyResolved;
  end;

  TSouffleTypedArrayKind = (
    stakInt8, stakUint8, stakUint8Clamped,
    stakInt16, stakUint16,
    stakInt32, stakUint32,
    stakFloat32, stakFloat64
  );

  TSouffleArrayBuffer = class(TSouffleHeapObject)
  private
    function GetByteLength: Integer;
  public
    Data: TBytes;
    IsShared: Boolean;
    constructor Create(const AByteLength: Integer; const AShared: Boolean = False);
    function DebugString: string; override;
    property ByteLength: Integer read GetByteLength;
  end;

  TSouffleTypedArray = class(TSouffleHeapObject)
  public
    Buffer: TSouffleArrayBuffer;
    ByteOffset: Integer;
    ElementLength: Integer;
    Kind: TSouffleTypedArrayKind;

    constructor Create(const ABuffer: TSouffleArrayBuffer;
      const AByteOffset, ALength: Integer; const AKind: TSouffleTypedArrayKind);
    function ReadElement(const AIndex: Integer): Double;
    procedure WriteElement(const AIndex: Integer; const AValue: Double);
    procedure WriteElementClamped(const AIndex: Integer; const AValue: Double);
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

  TSoufflePromiseAllState = class(TSouffleHeapObject)
  public
    Results: TSouffleArray;
    Remaining: Integer;
    ResultPromise: TSoufflePromise;
    Settled: Boolean;
    procedure MarkReferences; override;
    function DebugString: string; override;
  end;

function BytesPerElement(const AKind: TSouffleTypedArrayKind): Integer; inline;
function SouffleSameValueZero(const A, B: TSouffleValue): Boolean;

implementation

uses
  GarbageCollector.Generic,

  Goccia.MicrotaskQueue;

function SouffleSameValueZero(const A, B: TSouffleValue): Boolean;
var
  FA, FB: Double;
begin
  if A.Kind <> B.Kind then
  begin
    { Cross-compare integer 0 and float -0.0/+0.0 }
    if (A.Kind = svkInteger) and (B.Kind = svkFloat) then
      Exit((A.AsInteger = 0) and (B.AsFloat = 0.0));
    if (A.Kind = svkFloat) and (B.Kind = svkInteger) then
      Exit((A.AsFloat = 0.0) and (B.AsInteger = 0));
    Exit(False);
  end;

  case A.Kind of
    svkNil:
      Result := A.Flags = B.Flags;
    svkBoolean:
      Result := A.AsBoolean = B.AsBoolean;
    svkInteger:
      Result := A.AsInteger = B.AsInteger;
    svkFloat:
    begin
      FA := A.AsFloat;
      FB := B.AsFloat;
      if IsNan(FA) and IsNan(FB) then
        Result := True
      else
        Result := FA = FB;
    end;
    svkString:
      Result := SouffleGetString(A) = SouffleGetString(B);
    svkReference:
      Result := A.AsReference = B.AsReference;
  else
    Result := False;
  end;
end;

{ TGocciaSouffleMap }

constructor TGocciaSouffleMap.Create(const ACapacity: Integer);
begin
  inherited Create(0);
  SetLength(FEntries, ACapacity);
  FCount := 0;
end;

function TGocciaSouffleMap.FindEntry(const AKey: TSouffleValue): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if SouffleSameValueZero(FEntries[I].Key, AKey) then
      Exit(I);
  Result := -1;
end;

procedure TGocciaSouffleMap.SetEntry(const AKey, AValue: TSouffleValue);
var
  Idx: Integer;
begin
  Idx := FindEntry(AKey);
  if Idx >= 0 then
    FEntries[Idx].Value := AValue
  else
  begin
    if FCount >= Length(FEntries) then
      SetLength(FEntries, FCount * 2 + 4);
    FEntries[FCount].Key := AKey;
    FEntries[FCount].Value := AValue;
    Inc(FCount);
  end;
end;

function TGocciaSouffleMap.GetEntry(const AKey: TSouffleValue): TSouffleValue;
var
  Idx: Integer;
begin
  Idx := FindEntry(AKey);
  if Idx >= 0 then
    Result := FEntries[Idx].Value
  else
    Result := SouffleNilWithFlags(0);
end;

function TGocciaSouffleMap.HasKey(const AKey: TSouffleValue): Boolean;
begin
  Result := FindEntry(AKey) >= 0;
end;

function TGocciaSouffleMap.DeleteEntry(const AKey: TSouffleValue): Boolean;
var
  Idx, I: Integer;
begin
  Idx := FindEntry(AKey);
  if Idx < 0 then
    Exit(False);
  for I := Idx to FCount - 2 do
    FEntries[I] := FEntries[I + 1];
  Dec(FCount);
  FEntries[FCount].Key := SouffleNil;
  FEntries[FCount].Value := SouffleNil;
  Result := True;
end;

procedure TGocciaSouffleMap.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    FEntries[I].Key := SouffleNil;
    FEntries[I].Value := SouffleNil;
  end;
  FCount := 0;
end;

function TGocciaSouffleMap.GetKeyAt(const AIndex: Integer): TSouffleValue;
begin
  Result := FEntries[AIndex].Key;
end;

function TGocciaSouffleMap.GetValueAt(const AIndex: Integer): TSouffleValue;
begin
  Result := FEntries[AIndex].Value;
end;

procedure TGocciaSouffleMap.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCount - 1 do
  begin
    if SouffleIsReference(FEntries[I].Key) and Assigned(FEntries[I].Key.AsReference)
       and not FEntries[I].Key.AsReference.GCMarked then
      FEntries[I].Key.AsReference.MarkReferences;
    if SouffleIsReference(FEntries[I].Value) and Assigned(FEntries[I].Value.AsReference)
       and not FEntries[I].Value.AsReference.GCMarked then
      FEntries[I].Value.AsReference.MarkReferences;
  end;
end;

function TGocciaSouffleMap.DebugString: string;
begin
  Result := Format('SouffleMap(%d)', [FCount]);
end;

{ TGocciaSouffleSet }

constructor TGocciaSouffleSet.Create(const ACapacity: Integer);
begin
  inherited Create(0);
  SetLength(FItems, ACapacity);
  FCount := 0;
end;

function TGocciaSouffleSet.Contains(const AValue: TSouffleValue): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if SouffleSameValueZero(FItems[I], AValue) then
      Exit(True);
  Result := False;
end;

function TGocciaSouffleSet.Add(const AValue: TSouffleValue): Boolean;
begin
  if Contains(AValue) then
    Exit(False);
  if FCount >= Length(FItems) then
    SetLength(FItems, FCount * 2 + 4);
  FItems[FCount] := AValue;
  Inc(FCount);
  Result := True;
end;

function TGocciaSouffleSet.Delete(const AValue: TSouffleValue): Boolean;
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I := 0 to FCount - 1 do
    if SouffleSameValueZero(FItems[I], AValue) then
    begin
      Idx := I;
      Break;
    end;
  if Idx < 0 then
    Exit(False);
  for I := Idx to FCount - 2 do
    FItems[I] := FItems[I + 1];
  Dec(FCount);
  FItems[FCount] := SouffleNil;
  Result := True;
end;

procedure TGocciaSouffleSet.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FItems[I] := SouffleNil;
  FCount := 0;
end;

function TGocciaSouffleSet.GetItemAt(const AIndex: Integer): TSouffleValue;
begin
  Result := FItems[AIndex];
end;

procedure TGocciaSouffleSet.MarkReferences;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FCount - 1 do
    if SouffleIsReference(FItems[I]) and Assigned(FItems[I].AsReference)
       and not FItems[I].AsReference.GCMarked then
      FItems[I].AsReference.MarkReferences;
end;

function TGocciaSouffleSet.DebugString: string;
begin
  Result := Format('SouffleSet(%d)', [FCount]);
end;

{ TGocciaSouffleMapIterator }

constructor TGocciaSouffleMapIterator.Create(const AMap: TGocciaSouffleMap;
  const AKind: TSouffleMapIterKind);
begin
  inherited Create(0);
  FMap := AMap;
  FIndex := 0;
  FKind := AKind;
end;

function TGocciaSouffleMapIterator.Next(out ADone: Boolean): TSouffleValue;
var
  Arr: TSouffleArray;
begin
  if FIndex >= FMap.Count then
  begin
    ADone := True;
    Exit(SouffleNilWithFlags(0));
  end;
  ADone := False;
  case FKind of
    mikKeys:
      Result := FMap.GetKeyAt(FIndex);
    mikValues:
      Result := FMap.GetValueAt(FIndex);
    mikEntries:
    begin
      Arr := TSouffleArray.Create(2);
      Arr.Push(FMap.GetKeyAt(FIndex));
      Arr.Push(FMap.GetValueAt(FIndex));
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Arr);
      Result := SouffleReference(Arr);
    end;
  end;
  Inc(FIndex);
end;

procedure TGocciaSouffleMapIterator.MarkReferences;
begin
  inherited;
  if Assigned(FMap) and not FMap.GCMarked then
    FMap.MarkReferences;
end;

function TGocciaSouffleMapIterator.DebugString: string;
begin
  Result := 'MapIterator';
end;

{ TGocciaSouffleSetIterator }

constructor TGocciaSouffleSetIterator.Create(const ASet: TGocciaSouffleSet;
  const AKind: TSouffleSetIterKind);
begin
  inherited Create(0);
  FSet := ASet;
  FIndex := 0;
  FKind := AKind;
end;

function TGocciaSouffleSetIterator.Next(out ADone: Boolean): TSouffleValue;
var
  Arr: TSouffleArray;
begin
  if FIndex >= FSet.Count then
  begin
    ADone := True;
    Exit(SouffleNilWithFlags(0));
  end;
  ADone := False;
  case FKind of
    sikValues:
      Result := FSet.GetItemAt(FIndex);
    sikEntries:
    begin
      Arr := TSouffleArray.Create(2);
      Arr.Push(FSet.GetItemAt(FIndex));
      Arr.Push(FSet.GetItemAt(FIndex));
      if Assigned(TGarbageCollector.Instance) then
        TGarbageCollector.Instance.AllocateObject(Arr);
      Result := SouffleReference(Arr);
    end;
  end;
  Inc(FIndex);
end;

procedure TGocciaSouffleSetIterator.MarkReferences;
begin
  inherited;
  if Assigned(FSet) and not FSet.GCMarked then
    FSet.MarkReferences;
end;

function TGocciaSouffleSetIterator.DebugString: string;
begin
  Result := 'SetIterator';
end;

{ TSoufflePromise }

constructor TSoufflePromise.Create;
begin
  inherited Create(0);
  FState := spssPending;
  FResult := SouffleNilWithFlags(0);
  FReactionCount := 0;
  FAlreadyResolved := False;
end;

procedure TSoufflePromise.Resolve(const AValue: TSouffleValue);
var
  Task: TSouffleMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  if FState <> spssPending then Exit;

  { Self-resolution check }
  if SouffleIsReference(AValue) and Assigned(AValue.AsReference) and
     (AValue.AsReference = Self) then
  begin
    FState := spssRejected;
    FResult := SouffleNil; { TODO: create TypeError }
    TriggerReactions;
    Exit;
  end;

  { Promise chaining: if value is another promise, subscribe }
  if SouffleIsReference(AValue) and Assigned(AValue.AsReference) and
     (AValue.AsReference is TSoufflePromise) then
  begin
    Queue := TGocciaMicrotaskQueue.Instance;
    if Assigned(Queue) then
    begin
      Task.Handler := SouffleNil;
      Task.Value := AValue;
      Task.ResultPromise := Self;
      Task.ReactionType := prtThenableResolve;
      Queue.EnqueueSouffle(Task);
    end;
    Exit;
  end;

  FState := spssFulfilled;
  FResult := AValue;
  TriggerReactions;
end;

procedure TSoufflePromise.Reject(const AReason: TSouffleValue);
begin
  if FState <> spssPending then Exit;
  FState := spssRejected;
  FResult := AReason;
  TriggerReactions;
end;

procedure TSoufflePromise.AddReaction(const AOnFulfilled, AOnRejected: TSouffleValue;
  const AResultPromise: TSoufflePromise);
begin
  if FReactionCount >= Length(FReactions) then
    SetLength(FReactions, FReactionCount * 2 + 4);
  FReactions[FReactionCount].OnFulfilled := AOnFulfilled;
  FReactions[FReactionCount].OnRejected := AOnRejected;
  FReactions[FReactionCount].ResultPromise := AResultPromise;
  Inc(FReactionCount);
end;

function TSoufflePromise.InvokeThen(
  const AOnFulfilled, AOnRejected: TSouffleValue): TSoufflePromise;
var
  Task: TSouffleMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  Result := TSoufflePromise.Create;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.AllocateObject(Result);
  Queue := TGocciaMicrotaskQueue.Instance;

  case FState of
    spssPending:
      AddReaction(AOnFulfilled, AOnRejected, Result);
    spssFulfilled:
    begin
      Task.Handler := AOnFulfilled;
      Task.Value := FResult;
      Task.ResultPromise := Result;
      Task.ReactionType := prtFulfill;
      if Assigned(Queue) then
        Queue.EnqueueSouffle(Task);
    end;
    spssRejected:
    begin
      Task.Handler := AOnRejected;
      Task.Value := FResult;
      Task.ResultPromise := Result;
      Task.ReactionType := prtReject;
      if Assigned(Queue) then
        Queue.EnqueueSouffle(Task);
    end;
  end;
end;

procedure TSoufflePromise.TriggerReactions;
var
  I: Integer;
  Task: TSouffleMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  if FReactionCount = 0 then Exit;
  Queue := TGocciaMicrotaskQueue.Instance;
  if not Assigned(Queue) then Exit;

  for I := 0 to FReactionCount - 1 do
  begin
    case FState of
      spssFulfilled:
      begin
        Task.Handler := FReactions[I].OnFulfilled;
        Task.Value := FResult;
        Task.ResultPromise := FReactions[I].ResultPromise;
        Task.ReactionType := prtFulfill;
        Queue.EnqueueSouffle(Task);
      end;
      spssRejected:
      begin
        Task.Handler := FReactions[I].OnRejected;
        Task.Value := FResult;
        Task.ResultPromise := FReactions[I].ResultPromise;
        Task.ReactionType := prtReject;
        Queue.EnqueueSouffle(Task);
      end;
    end;
  end;
  FReactionCount := 0;
end;

procedure TSoufflePromise.SubscribeTo(const APromise: TSoufflePromise);
var
  Task: TSouffleMicrotask;
  Queue: TGocciaMicrotaskQueue;
begin
  case APromise.FState of
    spssFulfilled, spssRejected:
    begin
      Queue := TGocciaMicrotaskQueue.Instance;
      if Assigned(Queue) then
      begin
        Task.Handler := SouffleNil;
        Task.Value := APromise.FResult;
        Task.ResultPromise := Self;
        if APromise.FState = spssFulfilled then
          Task.ReactionType := prtFulfill
        else
          Task.ReactionType := prtReject;
        Queue.EnqueueSouffle(Task);
      end;
    end;
    spssPending:
    begin
      APromise.AddReaction(SouffleNil, SouffleNil, Self);
    end;
  end;
end;

procedure TSoufflePromise.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;
  if SouffleIsReference(FResult) and Assigned(FResult.AsReference) and
     not FResult.AsReference.GCMarked then
    FResult.AsReference.MarkReferences;
  for I := 0 to FReactionCount - 1 do
  begin
    if SouffleIsReference(FReactions[I].OnFulfilled) and
       Assigned(FReactions[I].OnFulfilled.AsReference) and
       not FReactions[I].OnFulfilled.AsReference.GCMarked then
      FReactions[I].OnFulfilled.AsReference.MarkReferences;
    if SouffleIsReference(FReactions[I].OnRejected) and
       Assigned(FReactions[I].OnRejected.AsReference) and
       not FReactions[I].OnRejected.AsReference.GCMarked then
      FReactions[I].OnRejected.AsReference.MarkReferences;
    if Assigned(FReactions[I].ResultPromise) and
       not FReactions[I].ResultPromise.GCMarked then
      FReactions[I].ResultPromise.MarkReferences;
  end;
end;

{ TSouffleArrayBuffer }

constructor TSouffleArrayBuffer.Create(const AByteLength: Integer;
  const AShared: Boolean);
begin
  inherited Create(0);
  SetLength(Data, AByteLength);
  if AByteLength > 0 then
    FillChar(Data[0], AByteLength, 0);
  IsShared := AShared;
end;

function TSouffleArrayBuffer.GetByteLength: Integer;
begin
  Result := Length(Data);
end;

function TSouffleArrayBuffer.DebugString: string;
begin
  Result := Format('ArrayBuffer(%d)', [Length(Data)]);
end;

{ TSouffleTypedArray }

function BytesPerElement(const AKind: TSouffleTypedArrayKind): Integer;
begin
  case AKind of
    stakInt8, stakUint8, stakUint8Clamped: Result := 1;
    stakInt16, stakUint16: Result := 2;
    stakInt32, stakUint32, stakFloat32: Result := 4;
    stakFloat64: Result := 8;
  else
    Result := 1;
  end;
end;

constructor TSouffleTypedArray.Create(const ABuffer: TSouffleArrayBuffer;
  const AByteOffset, ALength: Integer; const AKind: TSouffleTypedArrayKind);
begin
  inherited Create(0);
  Buffer := ABuffer;
  ByteOffset := AByteOffset;
  ElementLength := ALength;
  Kind := AKind;
end;

function TSouffleTypedArray.ReadElement(const AIndex: Integer): Double;
var
  Offset: Integer;
  I8: ShortInt;
  U8: Byte;
  I16: SmallInt;
  U16: Word;
  I32: LongInt;
  U32: LongWord;
  I64: Int64;
  F32: Single;
  F64: Double;
begin
  Offset := ByteOffset + AIndex * BytesPerElement(Kind);
  case Kind of
    stakInt8:
    begin
      I8 := ShortInt(Buffer.Data[Offset]);
      Result := I8;
    end;
    stakUint8, stakUint8Clamped:
    begin
      U8 := Buffer.Data[Offset];
      Result := U8;
    end;
    stakInt16:
    begin
      Move(Buffer.Data[Offset], I16, 2);
      Result := I16;
    end;
    stakUint16:
    begin
      Move(Buffer.Data[Offset], U16, 2);
      Result := U16;
    end;
    stakInt32:
    begin
      Move(Buffer.Data[Offset], I32, 4);
      I64 := I32;
      Result := I64;
    end;
    stakUint32:
    begin
      Move(Buffer.Data[Offset], U32, 4);
      I64 := U32;
      Result := I64;
    end;
    stakFloat32:
    begin
      Move(Buffer.Data[Offset], F32, 4);
      Result := F32;
    end;
    stakFloat64:
    begin
      Move(Buffer.Data[Offset], F64, 8);
      Result := F64;
    end;
  else
    Result := 0;
  end;
end;

procedure TSouffleTypedArray.WriteElement(const AIndex: Integer; const AValue: Double);
var
  Offset: Integer;
  I8: ShortInt;
  U8: Byte;
  I16: SmallInt;
  U16: Word;
  I32: LongInt;
  U32: LongWord;
  F32: Single;
  F64: Double;
begin
  Offset := ByteOffset + AIndex * BytesPerElement(Kind);
  case Kind of
    stakInt8:
    begin
      I8 := ShortInt(Trunc(AValue));
      Buffer.Data[Offset] := Byte(I8);
    end;
    stakUint8:
    begin
      U8 := Byte(Trunc(AValue));
      Buffer.Data[Offset] := U8;
    end;
    stakUint8Clamped:
      WriteElementClamped(AIndex, AValue);
    stakInt16:
    begin
      I16 := SmallInt(Trunc(AValue));
      Move(I16, Buffer.Data[Offset], 2);
    end;
    stakUint16:
    begin
      U16 := Word(Trunc(AValue));
      Move(U16, Buffer.Data[Offset], 2);
    end;
    stakInt32:
    begin
      I32 := LongInt(Trunc(AValue));
      Move(I32, Buffer.Data[Offset], 4);
    end;
    stakUint32:
    begin
      U32 := LongWord(Trunc(AValue));
      Move(U32, Buffer.Data[Offset], 4);
    end;
    stakFloat32:
    begin
      F32 := AValue;
      Move(F32, Buffer.Data[Offset], 4);
    end;
    stakFloat64:
    begin
      F64 := AValue;
      Move(F64, Buffer.Data[Offset], 8);
    end;
  end;
end;

procedure TSouffleTypedArray.WriteElementClamped(const AIndex: Integer; const AValue: Double);
var
  Offset, Clamped: Integer;
begin
  Offset := ByteOffset + AIndex * BytesPerElement(Kind);
  if IsNan(AValue) then
    Clamped := 0
  else if AValue <= 0 then
    Clamped := 0
  else if AValue >= 255 then
    Clamped := 255
  else
    Clamped := Round(AValue);
  Buffer.Data[Offset] := Byte(Clamped);
end;

procedure TSouffleTypedArray.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(Buffer) and not Buffer.GCMarked then
    Buffer.MarkReferences;
end;

function TSouffleTypedArray.DebugString: string;
begin
  Result := Format('TypedArray(%d)', [ElementLength]);
end;

{ TSoufflePromiseAllState }

procedure TSoufflePromiseAllState.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(Results) and not Results.GCMarked then
    Results.MarkReferences;
  if Assigned(ResultPromise) and not ResultPromise.GCMarked then
    ResultPromise.MarkReferences;
end;

function TSoufflePromiseAllState.DebugString: string;
begin
  Result := 'PromiseAllState';
end;

function TSoufflePromise.DebugString: string;
begin
  case FState of
    spssPending: Result := 'Promise(pending)';
    spssFulfilled: Result := 'Promise(fulfilled)';
    spssRejected: Result := 'Promise(rejected)';
  end;
end;

end.
