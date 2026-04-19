unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaConsoleOutputCallback = procedure(const AMethod, ALine: string) of object;

  TGocciaConsole = class(TGocciaBuiltin)
  private
    FTimers: TGocciaObjectValue;
    FCounters: TGocciaObjectValue;
    FGroupDepth: Integer;
    FOutputLines: TStrings;
    FOutputCallback: TGocciaConsoleOutputCallback;
    FLogCallback: TGocciaConsoleOutputCallback;
    FEnabled: Boolean;

    function FormatArgs(const AArgs: TGocciaArgumentsCollection): string;
    function GroupPrefix: string;
    function FormatValue(const AValue: TGocciaValue): string;
    procedure EmitLine(const AMethod, ALine: string);
    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    function GetOutputLines: TStrings;
    procedure SetOutputLines(const AValue: TStrings);
    function GetOutputCallback: TGocciaConsoleOutputCallback;
    procedure SetOutputCallback(const AValue: TGocciaConsoleOutputCallback);
    function GetLogCallback: TGocciaConsoleOutputCallback;
    procedure SetLogCallback(const AValue: TGocciaConsoleOutputCallback);
  protected
  published
    function ConsoleLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleWarn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleError(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleDebug(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleDir(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleAssert(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleCount(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleCountReset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleTimeEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleTimeLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleGroup(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleGroupEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleTrace(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ConsoleTable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OutputLines: TStrings read GetOutputLines write SetOutputLines;
    property OutputCallback: TGocciaConsoleOutputCallback read GetOutputCallback write SetOutputCallback;
    property LogCallback: TGocciaConsoleOutputCallback read GetLogCallback write SetLogCallback;
  end;

implementation

uses
  SysUtils,

  StringBuffer,
  TimingUtils,

  Goccia.Values.ArrayValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaConsole.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FTimers := TGocciaObjectValue.Create;
  FCounters := TGocciaObjectValue.Create;
  FGroupDepth := 0;
  FEnabled := True;
  FOutputLines := nil;
  FOutputCallback := nil;
  FLogCallback := nil;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(ConsoleLog, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleWarn, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleError, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleInfo, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleDebug, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleDir, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleAssert, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleCount, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleCountReset, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleTime, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleTimeEnd, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleTimeLog, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleClear, 0, gmkStaticMethod);
    Members.AddMethod(ConsoleGroup, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleGroupEnd, 0, gmkStaticMethod);
    Members.AddMethod(ConsoleTrace, -1, gmkStaticMethod);
    Members.AddMethod(ConsoleTable, -1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

procedure TGocciaConsole.EmitLine(const AMethod, ALine: string);
begin
  if FEnabled then
  begin
    if Assigned(FOutputCallback) then
      FOutputCallback(AMethod, ALine)
    else if Assigned(FOutputLines) then
      FOutputLines.Add(ALine)
    else
      WriteLn(ALine);
  end;
  if Assigned(FLogCallback) then
    FLogCallback(AMethod, ALine);
end;

function TGocciaConsole.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TGocciaConsole.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

function TGocciaConsole.GetOutputLines: TStrings;
begin
  Result := FOutputLines;
end;

procedure TGocciaConsole.SetOutputLines(const AValue: TStrings);
begin
  FOutputLines := AValue;
end;

function TGocciaConsole.GetOutputCallback: TGocciaConsoleOutputCallback;
begin
  Result := FOutputCallback;
end;

procedure TGocciaConsole.SetOutputCallback(const AValue: TGocciaConsoleOutputCallback);
begin
  FOutputCallback := AValue;
end;

function TGocciaConsole.GetLogCallback: TGocciaConsoleOutputCallback;
begin
  Result := FLogCallback;
end;

procedure TGocciaConsole.SetLogCallback(const AValue: TGocciaConsoleOutputCallback);
begin
  FLogCallback := AValue;
end;

function TGocciaConsole.FormatValue(const AValue: TGocciaValue): string;
var
  Arr: TGocciaArrayValue;
  Obj: TGocciaObjectValue;
  I: Integer;
  Key: string;
  SB: TStringBuffer;
  First: Boolean;
begin
  if AValue is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(AValue);
    SB := TStringBuffer.Create;
    SB.AppendChar('[');
    for I := 0 to Arr.Elements.Count - 1 do
    begin
      if I > 0 then
        SB.Append(', ');
      SB.Append(FormatValue(Arr.Elements[I]));
    end;
    SB.AppendChar(']');
    Result := SB.ToString;
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    SB := TStringBuffer.Create;
    SB.AppendChar('{');
    First := True;
    for Key in Obj.GetEnumerablePropertyNames do
    begin
      if not First then
        SB.Append(', ');
      First := False;
      SB.Append(Key);
      SB.Append(': ');
      SB.Append(FormatValue(Obj.GetProperty(Key)));
    end;
    SB.AppendChar('}');
    Result := SB.ToString;
  end
  else if AValue is TGocciaStringLiteralValue then
    Result := '''' + AValue.ToStringLiteral.Value + ''''
  else
    Result := AValue.ToStringLiteral.Value;
end;

function TGocciaConsole.FormatArgs(const AArgs: TGocciaArgumentsCollection): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AArgs.Length - 1 do
  begin
    if I > 0 then
      Result := Result + ' ';
    Result := Result + AArgs.GetElement(I).ToStringLiteral.Value;
  end;
end;

function TGocciaConsole.GroupPrefix: string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to FGroupDepth do
    Result := Result + '  ';
end;

function TGocciaConsole.ConsoleLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  EmitLine('log', GroupPrefix + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleWarn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  EmitLine('warn', GroupPrefix + 'Warning: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleError(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  EmitLine('error', GroupPrefix + 'Error: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  EmitLine('info', GroupPrefix + 'Info: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleDebug(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  EmitLine('debug', GroupPrefix + 'Debug: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleDir(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length >= 1 then
  begin
    Value := AArgs.GetElement(0);
    EmitLine('dir', GroupPrefix + FormatValue(Value));
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleAssert(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Condition: TGocciaValue;
  Msg: string;
  I: Integer;
begin
  if AArgs.Length >= 1 then
  begin
    Condition := AArgs.GetElement(0);
    if not Condition.ToBooleanLiteral.Value then
    begin
      Msg := 'Assertion failed:';
      if AArgs.Length >= 2 then
      begin
        for I := 1 to AArgs.Length - 1 do
          Msg := Msg + ' ' + AArgs.GetElement(I).ToStringLiteral.Value;
      end;
      EmitLine('assert', GroupPrefix + Msg);
    end;
  end
  else
    EmitLine('assert', GroupPrefix + 'Assertion failed');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleCount(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LabelStr: string;
  Current: TGocciaValue;
  CountVal: Integer;
begin
  if (AArgs.Length >= 1) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    LabelStr := AArgs.GetElement(0).ToStringLiteral.Value
  else
    LabelStr := 'default';

  Current := FCounters.GetProperty(LabelStr);
  if (Current = nil) or (Current is TGocciaUndefinedLiteralValue) then
    CountVal := 1
  else
    CountVal := Trunc(Current.ToNumberLiteral.Value) + 1;

  FCounters.AssignProperty(LabelStr, TGocciaNumberLiteralValue.Create(CountVal));
  EmitLine('count', GroupPrefix + LabelStr + ': ' + IntToStr(CountVal));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleCountReset(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LabelStr: string;
begin
  if (AArgs.Length >= 1) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    LabelStr := AArgs.GetElement(0).ToStringLiteral.Value
  else
    LabelStr := 'default';

  FCounters.AssignProperty(LabelStr, TGocciaNumberLiteralValue.Create(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleTime(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LabelStr: string;
begin
  if (AArgs.Length >= 1) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    LabelStr := AArgs.GetElement(0).ToStringLiteral.Value
  else
    LabelStr := 'default';

  FTimers.AssignProperty(LabelStr, TGocciaNumberLiteralValue.Create(GetMilliseconds));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleTimeEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LabelStr: string;
  StartTime: TGocciaValue;
  Elapsed: Double;
begin
  if (AArgs.Length >= 1) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    LabelStr := AArgs.GetElement(0).ToStringLiteral.Value
  else
    LabelStr := 'default';

  StartTime := FTimers.GetProperty(LabelStr);
  if (StartTime <> nil) and not (StartTime is TGocciaUndefinedLiteralValue) then
  begin
    Elapsed := GetMilliseconds - StartTime.ToNumberLiteral.Value;
    EmitLine('timeEnd', GroupPrefix + LabelStr + ': ' + FormatFloat('0.###', Elapsed) + 'ms');
    FTimers.DeleteProperty(LabelStr);
  end
  else
    EmitLine('timeEnd', GroupPrefix + 'Timer ''' + LabelStr + ''' does not exist');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleTimeLog(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  LabelStr: string;
  StartTime: TGocciaValue;
  Elapsed: Double;
begin
  if (AArgs.Length >= 1) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    LabelStr := AArgs.GetElement(0).ToStringLiteral.Value
  else
    LabelStr := 'default';

  StartTime := FTimers.GetProperty(LabelStr);
  if (StartTime <> nil) and not (StartTime is TGocciaUndefinedLiteralValue) then
  begin
    Elapsed := GetMilliseconds - StartTime.ToNumberLiteral.Value;
    EmitLine('timeLog', GroupPrefix + LabelStr + ': ' + FormatFloat('0.###', Elapsed) + 'ms');
  end
  else
    EmitLine('timeLog', GroupPrefix + 'Timer ''' + LabelStr + ''' does not exist');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleGroup(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length > 0 then
    EmitLine('group', GroupPrefix + FormatArgs(AArgs));
  Inc(FGroupDepth);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleGroupEnd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if FGroupDepth > 0 then
    Dec(FGroupDepth);
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleTrace(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length > 0 then
    EmitLine('trace', GroupPrefix + 'Trace: ' + FormatArgs(AArgs))
  else
    EmitLine('trace', GroupPrefix + 'Trace');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleTable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length >= 1 then
    EmitLine('table', GroupPrefix + FormatValue(AArgs.GetElement(0)));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
