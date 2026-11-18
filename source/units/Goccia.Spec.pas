unit Goccia.Spec;

{$I Goccia.inc}

interface

uses
  Goccia.Spec.Data,
  Goccia.Values.ObjectValue;

function CreateSpecObject: TGocciaObjectValue;
function CreateProposalObject: TGocciaObjectValue;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.Primitives;

const
  PROP_LINK = 'link';

function CreateFeatureEntryObject(const AEntry: TGocciaFeatureEntry): TGocciaObjectValue;
var
  Obj: TGocciaObjectValue;
  Roots: TGocciaActiveRootFrame;
begin
  // Each builder here holds what it is building in a local while the strings
  // that go into it reserve their payloads, and a reservation can collect;
  // the caller stores the result before its next allocation.
  Roots.Initialize;
  try
    Obj := TGocciaObjectValue.Create;
    Roots.Add(Obj);
    Obj.AssignProperty(PROP_NAME, TGocciaStringLiteralValue.Create(AEntry.Name));
    Obj.AssignProperty(PROP_LINK, TGocciaStringLiteralValue.Create(AEntry.Link));
  finally
    Roots.Clear;
  end;
  Result := Obj;
end;

function CreateFeatureArray(const AEntries: array of TGocciaFeatureEntry): TGocciaArrayValue;
var
  Arr: TGocciaArrayValue;
  I: Integer;
  Roots: TGocciaActiveRootFrame;
begin
  Roots.Initialize;
  try
    Arr := TGocciaArrayValue.Create;
    Roots.Add(Arr);
    for I := Low(AEntries) to High(AEntries) do
      Arr.Elements.Add(CreateFeatureEntryObject(AEntries[I]));
  finally
    Roots.Clear;
  end;
  Result := Arr;
end;

procedure DefineReadOnlyProperty(const AParent: TGocciaObjectValue;
  const AName: string; const AValue: TGocciaValue);
begin
  AParent.DefineProperty(AName,
    TGocciaPropertyDescriptorData.Create(AValue, [pfEnumerable]));
end;

function CreateGroupsObject(const AGroups: TGocciaFeatureGroups): TGocciaObjectValue;
var
  Obj: TGocciaObjectValue;
  Roots: TGocciaActiveRootFrame;
  I: Integer;
begin
  Roots.Initialize;
  try
    Obj := TGocciaObjectValue.Create;
    Roots.Add(Obj);
    for I := 0 to High(AGroups) do
      DefineReadOnlyProperty(Obj, AGroups[I].Key,
        CreateFeatureArray(AGroups[I].Entries));
  finally
    Roots.Clear;
  end;
  Result := Obj;
end;

function CreateSpecObject: TGocciaObjectValue;
begin
  Result := CreateGroupsObject(SpecFeatureGroups);
end;

function CreateProposalObject: TGocciaObjectValue;
begin
  Result := CreateGroupsObject(ProposalFeatureGroups);
end;

end.
