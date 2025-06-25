unit Goccia.Values.Interfaces;

{$I Goccia.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Goccia.Values.Core, Goccia.Values.ObjectPropertyDescriptor;

type
  IValueOf = interface
    function ValueOf: TGocciaValue;
  end;

  IStringTag = interface
    function ToStringTag: string;
  end;

  IPropertyMethods = interface
    function GetProperty(const AName: string): TGocciaValue;
    function GetOwnPropertyDescriptor(const AName: string): TGocciaPropertyDescriptor;
    function HasProperty(const AName: string): Boolean;
    function HasOwnProperty(const AName: string): Boolean;
    procedure DefineProperty(const AName: string; ADescriptor: TGocciaPropertyDescriptor);
    procedure DefineProperties(const AProperties: TDictionary<string, TGocciaPropertyDescriptor>);
    procedure AssignProperty(const AName: string; AValue: TGocciaValue; ACanCreate: Boolean = True);
    function DeleteProperty(const AName: string): Boolean;
    function GetOwnPropertyNames: TStringList;
    function GetOwnPropertyKeys: TStringList;
    function GetOwnPropertySymbols: TStringList;
    function GetOwnPropertyEnumerable: TStringList;
    function GetOwnPropertyWritable: TStringList;
    function GetOwnPropertyConfigurable: TStringList;
  end;

  IIndexMethods = interface
    function GetLength: Integer;
    function GetElement(const AIndex: Integer): TGocciaValue;
    function SetElement(const AIndex: Integer; AValue: TGocciaValue): Boolean;
  end;

  IFunctionMethods = interface
    function Call(const AArguments: TObjectList<TGocciaValue>; const AThisValue: TGocciaValue): TGocciaValue;
    function Apply(const AArguments: TObjectList<TGocciaValue>; const AThisValue: TGocciaValue): TGocciaValue;
    function Bind(const AArguments: TObjectList<TGocciaValue>; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

end.