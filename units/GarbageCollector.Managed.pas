unit GarbageCollector.Managed;

{$I Goccia.inc}

interface

type
  TGCManagedObject = class
  private
    FGCMarked: Boolean;
  public
    procedure MarkReferences; virtual;
    property GCMarked: Boolean read FGCMarked write FGCMarked;
  end;

implementation

{ TGCManagedObject }

procedure TGCManagedObject.MarkReferences;
begin
  FGCMarked := True;
end;

end.
