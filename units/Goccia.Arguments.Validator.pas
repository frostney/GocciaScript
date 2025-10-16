unit Goccia.Arguments.Validator;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection, Goccia.Error.ThrowErrorCallback, SysUtils;

type
  // Validation logic for argument requirements
  TGocciaArgumentValidator = class
  public
    class procedure RequireExactly(Collection: TGocciaArgumentsCollection; ExpectedCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
    class procedure RequireAtLeast(Collection: TGocciaArgumentsCollection; MinCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
    class procedure RequireAtMost(Collection: TGocciaArgumentsCollection; MaxCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
    class procedure RequireBetween(Collection: TGocciaArgumentsCollection; MinCount, MaxCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
    class procedure RequireNone(Collection: TGocciaArgumentsCollection; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
  end;

implementation

{ TGocciaArgumentValidator }

class procedure TGocciaArgumentValidator.RequireExactly(Collection: TGocciaArgumentsCollection; ExpectedCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
begin
  if Collection.Length <> ExpectedCount then
  begin
    if ExpectedCount = 0 then
      ThrowError(Format('%s expects no arguments but got %d', [FunctionName, Collection.Length]), 0, 0)
    else if ExpectedCount = 1 then
      ThrowError(Format('%s expects 1 argument but got %d', [FunctionName, Collection.Length]), 0, 0)
    else
      ThrowError(Format('%s expects %d arguments but got %d', [FunctionName, ExpectedCount, Collection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireAtLeast(Collection: TGocciaArgumentsCollection; MinCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
begin
  if Collection.Length < MinCount then
  begin
    if MinCount = 1 then
      ThrowError(Format('%s expects at least 1 argument but got %d', [FunctionName, Collection.Length]), 0, 0)
    else
      ThrowError(Format('%s expects at least %d arguments but got %d', [FunctionName, MinCount, Collection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireAtMost(Collection: TGocciaArgumentsCollection; MaxCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
begin
  if Collection.Length > MaxCount then
  begin
    if MaxCount = 0 then
      ThrowError(Format('%s expects no arguments but got %d', [FunctionName, Collection.Length]), 0, 0)
    else if MaxCount = 1 then
      ThrowError(Format('%s expects at most 1 argument but got %d', [FunctionName, Collection.Length]), 0, 0)
    else
      ThrowError(Format('%s expects at most %d arguments but got %d', [FunctionName, MaxCount, Collection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireBetween(Collection: TGocciaArgumentsCollection; MinCount, MaxCount: Integer; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
begin
  if (Collection.Length < MinCount) or (Collection.Length > MaxCount) then
  begin
    if MinCount = MaxCount then
      RequireExactly(Collection, MinCount, FunctionName, ThrowError)
    else
      ThrowError(Format('%s expects between %d and %d arguments but got %d', [FunctionName, MinCount, MaxCount, Collection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireNone(Collection: TGocciaArgumentsCollection; const FunctionName: string; ThrowError: TGocciaThrowErrorCallback);
begin
  RequireExactly(Collection, 0, FunctionName, ThrowError);
end;

end.