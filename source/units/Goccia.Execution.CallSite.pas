unit Goccia.Execution.CallSite;

{$I Goccia.inc}

interface

type
  TGocciaCallSite = record
    FilePath: string;
    Line: Integer;
    Column: Integer;
    Assigned: Boolean;
  end;

procedure EnterGocciaCallSite(const AFilePath: string;
  const ALine, AColumn: Integer; out APrevious: TGocciaCallSite);
procedure LeaveGocciaCallSite(const APrevious: TGocciaCallSite);
function CurrentGocciaCallSite(out ACallSite: TGocciaCallSite): Boolean;

implementation

threadvar
  ActiveCallSite: TGocciaCallSite;

procedure EnterGocciaCallSite(const AFilePath: string;
  const ALine, AColumn: Integer; out APrevious: TGocciaCallSite);
begin
  APrevious := ActiveCallSite;
  ActiveCallSite.FilePath := AFilePath;
  ActiveCallSite.Line := ALine;
  ActiveCallSite.Column := AColumn;
  ActiveCallSite.Assigned := True;
end;

procedure LeaveGocciaCallSite(const APrevious: TGocciaCallSite);
begin
  ActiveCallSite := APrevious;
end;

function CurrentGocciaCallSite(out ACallSite: TGocciaCallSite): Boolean;
begin
  ACallSite := ActiveCallSite;
  Result := ACallSite.Assigned;
end;

end.
