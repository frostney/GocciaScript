unit Goccia.Values.ObjectPropertyDescriptor;

{$I Goccia.inc}

interface

uses
  OrderedMap,

  Goccia.Values.Primitives;

type
  TPropertyFlag = (pfEnumerable, pfConfigurable, pfWritable);
  TPropertyFlags = set of TPropertyFlag;

  TGocciaPropertyDescriptor = class
  private
    FFlags: TPropertyFlags;
    function GetEnumerable: Boolean; inline;
    function GetConfigurable: Boolean; inline;
    function GetWritable: Boolean; inline;
  public
    constructor Create(const AFlags: TPropertyFlags);

    property Flags: TPropertyFlags read FFlags;
    property Enumerable: Boolean read GetEnumerable;
    property Configurable: Boolean read GetConfigurable;
    property Writable: Boolean read GetWritable;
  end;

  TGocciaPropertyDescriptorData = class(TGocciaPropertyDescriptor)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);

    property Value: TGocciaValue read FValue;
  end;

  TGocciaPropertyDescriptorAccessor = class(TGocciaPropertyDescriptor)
  private
    FGetter: TGocciaValue;
    FSetter: TGocciaValue;

    function GetWritable: Boolean; inline;
  public
    constructor Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);

    property Getter: TGocciaValue read FGetter;
    property Setter: TGocciaValue read FSetter;
  end;

  TGocciaPropertyMap = TOrderedMap<TGocciaPropertyDescriptor>;

implementation

constructor TGocciaPropertyDescriptor.Create(const AFlags: TPropertyFlags);
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

constructor TGocciaPropertyDescriptorData.Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
  FValue := AValue;
end;

constructor TGocciaPropertyDescriptorAccessor.Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);
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
