unit Goccia.Arguments.Validator;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Error.ThrowErrorCallback;

type
  // Validation logic for argument requirements
  TGocciaArgumentValidator = class
  public
    class procedure RequireExactly(const ACollection: TGocciaArgumentsCollection; const AExpectedCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
    class procedure RequireAtLeast(const ACollection: TGocciaArgumentsCollection; const AMinCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
    class procedure RequireAtMost(const ACollection: TGocciaArgumentsCollection; const AMaxCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
    class procedure RequireBetween(const ACollection: TGocciaArgumentsCollection; const AMinCount, AMaxCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
    class procedure RequireNone(const ACollection: TGocciaArgumentsCollection; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  SysUtils;

{ TGocciaArgumentValidator }

class procedure TGocciaArgumentValidator.RequireExactly(const ACollection: TGocciaArgumentsCollection; const AExpectedCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
begin
  if ACollection.Length <> AExpectedCount then
  begin
    if AExpectedCount = 0 then
      AThrowError(Format('%s expects no arguments but got %d', [AFunctionName, ACollection.Length]), 0, 0)
    else if AExpectedCount = 1 then
      AThrowError(Format('%s expects 1 argument but got %d', [AFunctionName, ACollection.Length]), 0, 0)
    else
      AThrowError(Format('%s expects %d arguments but got %d', [AFunctionName, AExpectedCount, ACollection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireAtLeast(const ACollection: TGocciaArgumentsCollection; const AMinCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
begin
  if ACollection.Length < AMinCount then
  begin
    if AMinCount = 1 then
      AThrowError(Format('%s expects at least 1 argument but got %d', [AFunctionName, ACollection.Length]), 0, 0)
    else
      AThrowError(Format('%s expects at least %d arguments but got %d', [AFunctionName, AMinCount, ACollection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireAtMost(const ACollection: TGocciaArgumentsCollection; const AMaxCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
begin
  if ACollection.Length > AMaxCount then
  begin
    if AMaxCount = 0 then
      AThrowError(Format('%s expects no arguments but got %d', [AFunctionName, ACollection.Length]), 0, 0)
    else if AMaxCount = 1 then
      AThrowError(Format('%s expects at most 1 argument but got %d', [AFunctionName, ACollection.Length]), 0, 0)
    else
      AThrowError(Format('%s expects at most %d arguments but got %d', [AFunctionName, AMaxCount, ACollection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireBetween(const ACollection: TGocciaArgumentsCollection; const AMinCount, AMaxCount: Integer; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
begin
  if (ACollection.Length < AMinCount) or (ACollection.Length > AMaxCount) then
  begin
    if AMinCount = AMaxCount then
      RequireExactly(ACollection, AMinCount, AFunctionName, AThrowError)
    else
      AThrowError(Format('%s expects between %d and %d arguments but got %d', [AFunctionName, AMinCount, AMaxCount, ACollection.Length]), 0, 0);
  end;
end;

class procedure TGocciaArgumentValidator.RequireNone(const ACollection: TGocciaArgumentsCollection; const AFunctionName: string; const AThrowError: TGocciaThrowErrorCallback);
begin
  RequireExactly(ACollection, 0, AFunctionName, AThrowError);
end;

end.