unit Goccia.Arguments.Callbacks;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  TGocciaArrayCallbackArgs = class(TGocciaArgumentsCollection)
  private
    FElement: TGocciaValue;
    FIndex: TGocciaNumberLiteralValue;
    FThisArray: TGocciaValue;
  public
    constructor Create(const AThisArray: TGocciaValue);

    function GetElement(const AIndex: Integer): TGocciaValue; override;
    function GetLength: Integer; override;

    property Element: TGocciaValue read FElement write FElement;
    property Index: TGocciaNumberLiteralValue read FIndex write FIndex;
  end;

  TGocciaReduceCallbackArgs = class(TGocciaArgumentsCollection)
  private
    FAccumulator: TGocciaValue;
    FElement: TGocciaValue;
    FIndex: TGocciaNumberLiteralValue;
    FThisArray: TGocciaValue;
  public
    constructor Create(const AThisArray: TGocciaValue);

    function GetElement(const AIndex: Integer): TGocciaValue; override;
    function GetLength: Integer; override;

    property Accumulator: TGocciaValue read FAccumulator write FAccumulator;
    property Element: TGocciaValue read FElement write FElement;
    property Index: TGocciaNumberLiteralValue read FIndex write FIndex;
  end;

implementation

{ TGocciaArrayCallbackArgs }

constructor TGocciaArrayCallbackArgs.Create(const AThisArray: TGocciaValue);
begin
  FThisArray := AThisArray;
end;

function TGocciaArrayCallbackArgs.GetElement(const AIndex: Integer): TGocciaValue;
begin
  case AIndex of
    0: Result := FElement;
    1: Result := FIndex;
    2: Result := FThisArray;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaArrayCallbackArgs.GetLength: Integer;
begin
  Result := 3;
end;

{ TGocciaReduceCallbackArgs }

constructor TGocciaReduceCallbackArgs.Create(const AThisArray: TGocciaValue);
begin
  FThisArray := AThisArray;
end;

function TGocciaReduceCallbackArgs.GetElement(const AIndex: Integer): TGocciaValue;
begin
  case AIndex of
    0: Result := FAccumulator;
    1: Result := FElement;
    2: Result := FIndex;
    3: Result := FThisArray;
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

function TGocciaReduceCallbackArgs.GetLength: Integer;
begin
  Result := 4;
end;

end.
