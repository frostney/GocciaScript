program tcustomattr;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

type
  TMyAttribute = class(TCustomAttribute)
    constructor Create;
    constructor Create(aArg: String);
    constructor Create(aArg: TGUID);
    constructor Create(aArg: LongInt);
  end;

  {$M+}
  [TMyAttribute]
  TTestClass = class
  private
    fTest: LongInt;
  published
    [TMyAttribute('Test')]
    property Test: LongInt read fTest;
  end;
  {$M-}

  [TMyAttribute(1234)]
  [TMy('Hello World')]
  TTestEnum = (
    teOne,
    teTwo
  );

  [TMyAttribute(IInterface), TMy(42)]
  TLongInt = type LongInt;

constructor TMyAttribute.Create;
begin
end;

constructor TMyAttribute.Create(aArg: String);
begin
end;

constructor TMyAttribute.Create(aArg: LongInt);
begin
end;

constructor TMyAttribute.Create(aArg: TGUID);
begin
end;

begin

end.