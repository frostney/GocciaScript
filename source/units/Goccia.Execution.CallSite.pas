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
  const ALine, AColumn: Integer; var APrevious: TGocciaCallSite);
procedure LeaveGocciaCallSite(const APrevious: TGocciaCallSite);
function CurrentGocciaCallSite(out ACallSite: TGocciaCallSite): Boolean;

implementation

type
  PGocciaCallSite = ^TGocciaCallSite;

threadvar
  ActiveCallSite: TGocciaCallSite;

{ Enter and Leave run around every native call the bytecode VM makes. They
  resolve the threadvar once and copy the record field by field: a whole-record
  assignment of a record holding a string goes through the compiler's
  RTTI-driven copy helper, which costs several times the copy itself.
  APrevious is a var parameter for the same reason: Enter overwrites every
  field, and an out parameter would be finalized and re-initialized first. }

procedure EnterGocciaCallSite(const AFilePath: string;
  const ALine, AColumn: Integer; var APrevious: TGocciaCallSite);
var
  Active: PGocciaCallSite;
begin
  Active := @ActiveCallSite;
  APrevious.FilePath := Active^.FilePath;
  APrevious.Line := Active^.Line;
  APrevious.Column := Active^.Column;
  APrevious.Assigned := Active^.Assigned;
  Active^.FilePath := AFilePath;
  Active^.Line := ALine;
  Active^.Column := AColumn;
  Active^.Assigned := True;
end;

procedure LeaveGocciaCallSite(const APrevious: TGocciaCallSite);
var
  Active: PGocciaCallSite;
begin
  Active := @ActiveCallSite;
  Active^.FilePath := APrevious.FilePath;
  Active^.Line := APrevious.Line;
  Active^.Column := APrevious.Column;
  Active^.Assigned := APrevious.Assigned;
end;

function CurrentGocciaCallSite(out ACallSite: TGocciaCallSite): Boolean;
begin
  ACallSite := ActiveCallSite;
  Result := ACallSite.Assigned;
end;

end.
