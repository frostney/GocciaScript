unit Goccia.Values.ObjectPropertyDescriptor;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.UndefinedValue;

type
  // Property descriptor type (mutually exclusive)
  TPropertyDescriptorType = (pdData, pdAccessor);

  // Property descriptor flags (common to both types)
  TPropertyFlag = (pfEnumerable, pfConfigurable, pfWritable);
  TPropertyFlags = set of TPropertyFlag;

  TGocciaPropertyDescriptor = class
  private
    FFlags: TPropertyFlags;
    function GetEnumerable: Boolean; inline;
    function GetConfigurable: Boolean; inline;
    function GetWritable: Boolean; inline;
  public
    constructor Create(AFlags: TPropertyFlags);

    property Enumerable: Boolean read GetEnumerable;
    property Configurable: Boolean read GetConfigurable;
    property Writable: Boolean read GetWritable;
  end;

  TGocciaPropertyDescriptorData = class(TGocciaPropertyDescriptor)
  private
    FValue: TGocciaValue;
    FWritable: Boolean;
  public
    constructor Create(AValue: TGocciaValue; AFlags: TPropertyFlags);

    property Value: TGocciaValue read FValue;
    property Writable: Boolean read FWritable;
  end;

  TGocciaPropertyDescriptorAccessor = class(TGocciaPropertyDescriptor)
  private
    FGetter: TGocciaValue;
    FSetter: TGocciaValue;

    function GetWritable: Boolean; inline;
  public
    constructor Create(AGetter: TGocciaValue; ASetter: TGocciaValue; AFlags: TPropertyFlags);

    property Getter: TGocciaValue read FGetter;
    property Setter: TGocciaValue read FSetter;
  end;

implementation

constructor TGocciaPropertyDescriptor.Create(AFlags: TPropertyFlags);
begin
  FFlags := AFlags;
end;

function TGocciaPropertyDescriptor.GetEnumerable: Boolean;
begin
  Result := pfEnumerable in FFlags;
end;

function TGocciaPropertyDescriptor.GetConfigurable: Boolean;
begin
  Result := pfConfigurable in FFlags;
end;

function TGocciaPropertyDescriptor.GetWritable: Boolean;
begin
  Result := pfWritable in FFlags;
end;

constructor TGocciaPropertyDescriptorData.Create(AValue: TGocciaValue; AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
  FValue := AValue;
end;

constructor TGocciaPropertyDescriptorAccessor.Create(AGetter: TGocciaValue; ASetter: TGocciaValue; AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
  FGetter := AGetter;
  FSetter := ASetter;
end;

function TGocciaPropertyDescriptorAccessor.GetWritable: Boolean;
begin
  Result := FSetter <> nil;
end;


end.