unit GarbageCollector.Managed;

{$I Goccia.inc}

interface

type
  TGCManagedObject = class
  private class var
    FCurrentMark: Cardinal;
  private
    FGCMark: Cardinal;
    FGCIndex: Integer;
    function GetGCMarked: Boolean; inline;
    procedure SetGCMarked(const AValue: Boolean); inline;
  public
    class procedure AdvanceMark; static; inline;
    procedure MarkReferences; virtual;
    property GCMarked: Boolean read GetGCMarked write SetGCMarked;
    property GCIndex: Integer read FGCIndex write FGCIndex;
  end;

implementation

{ TGCManagedObject }

class procedure TGCManagedObject.AdvanceMark;
begin
  Inc(FCurrentMark);
end;

function TGCManagedObject.GetGCMarked: Boolean;
begin
  Result := FGCMark = FCurrentMark;
end;

procedure TGCManagedObject.SetGCMarked(const AValue: Boolean);
begin
  if AValue then
    FGCMark := FCurrentMark;
end;

procedure TGCManagedObject.MarkReferences;
begin
  FGCMark := FCurrentMark;
end;

initialization
  TGCManagedObject.FCurrentMark := 1;

end.
