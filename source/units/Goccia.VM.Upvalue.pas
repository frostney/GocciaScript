unit Goccia.VM.Upvalue;

{$I Goccia.inc}

interface

uses
  Goccia.GarbageCollector,
  Goccia.VM.Registers;

type
  // The storage of one captured local binding. A frame opens a cell when a
  // closure, a direct eval or a mapped arguments object first captures the
  // binding, and from then on the frame's local-cell window, every closure
  // that captured it (each closure's upvalue slot holds the cell itself, and a
  // nested closure or a generator's clone shares its parent's), a suspended
  // generator's continuation and a mapped arguments object can all hold it at
  // once. No single holder outlives the others, so the collector owns the cell:
  // each holder marks it, and a cell none of them marks is freed with the rest
  // of the garbage. Its size is charged to the collector like a value's, so
  // `--max-memory` accounts for it (issue #1618).
  TGocciaBytecodeCell = class(TGCManagedObject)
  private
    FValue: TGocciaRegister;
  public
    constructor Create(const AValue: TGocciaRegister);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure MarkReferences; override;
    property Value: TGocciaRegister read FValue write FValue;
  end;

implementation

constructor TGocciaBytecodeCell.Create(const AValue: TGocciaRegister);
begin
  inherited Create;
  FValue := AValue;
end;

// Registration charges the cell's bytes but does not test the memory limit
// itself: a cell is opened in the middle of building a closure or an arguments
// object, where a throw would strand the half-built structure. The value that
// structure allocates next tests the limit against a total that includes the
// cell.
procedure TGocciaBytecodeCell.AfterConstruction;
var
  GC: TGarbageCollector;
begin
  inherited;
  GCIndex := -1;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.RegisterObject(Self);
end;

// The sweep takes a cell off the collector's list (GCIndex -1) before it frees
// it, and no cell is ever pinned or rooted, so a swept cell has nothing left to
// unregister from. Skipping the collector lookup then matters because a
// closure-heavy loop frees one cell per capturing call.
procedure TGocciaBytecodeCell.BeforeDestruction;
begin
  if GCIndex >= 0 then
    inherited;
end;

procedure TGocciaBytecodeCell.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  MarkRegisterReferences(FValue);
end;

end.
