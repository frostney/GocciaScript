unit Goccia.Builtins.Console;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaConsole = class(TGocciaBuiltin)
  private
    FTimers: TGocciaObjectValue;
    FCounters: TGocciaObjectValue;
    FGroupDepth: Integer;

    function FormatArgs(const AArgs: TGocciaArgumentsCollection): string;
    function GroupPrefix: string;
    function FormatValue(const AValue: TGocciaValue): string;
  protected
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
  end;

implementation

uses
  SysUtils,

  TimingUtils,

  Goccia.Values.ArrayValue,
  Goccia.Values.NativeFunction;

constructor TGocciaConsole.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FTimers := TGocciaObjectValue.Create;
  FCounters := TGocciaObjectValue.Create;
  FGroupDepth := 0;

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleLog, 'log', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleWarn, 'warn', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleError, 'error', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleInfo, 'info', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleDebug, 'debug', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleDir, 'dir', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleAssert, 'assert', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleCount, 'count', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleCountReset, 'countReset', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleTime, 'time', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleTimeEnd, 'timeEnd', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleTimeLog, 'timeLog', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleClear, 'clear', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleGroup, 'group', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleGroupEnd, 'groupEnd', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleTrace, 'trace', -1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(ConsoleTable, 'table', -1));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

function TGocciaConsole.FormatValue(const AValue: TGocciaValue): string;
var
  Arr: TGocciaArrayValue;
  Obj: TGocciaObjectValue;
  I: Integer;
  Key: string;
  SB: TStringBuilder;
  First: Boolean;
begin
  if AValue is TGocciaArrayValue then
  begin
    Arr := TGocciaArrayValue(AValue);
    SB := TStringBuilder.Create;
    try
      SB.Append('[');
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if I > 0 then
          SB.Append(', ');
        SB.Append(FormatValue(Arr.Elements[I]));
      end;
      SB.Append(']');
      Result := SB.ToString;
    finally
      SB.Free;
    end;
  end
  else if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    SB := TStringBuilder.Create;
    try
      SB.Append('{');
      First := True;
      for Key in Obj.GetEnumerablePropertyNames do
      begin
        if not First then
          SB.Append(', ');
        First := False;
        SB.Append(Key).Append(': ').Append(FormatValue(Obj.GetProperty(Key)));
      end;
      SB.Append('}');
      Result := SB.ToString;
    finally
      SB.Free;
    end;
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
  WriteLn(GroupPrefix + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleWarn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  WriteLn(GroupPrefix + 'Warning: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleError(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  WriteLn(GroupPrefix + 'Error: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleInfo(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  WriteLn(GroupPrefix + 'Info: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleDebug(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  WriteLn(GroupPrefix + 'Debug: ' + FormatArgs(AArgs));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleDir(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Value: TGocciaValue;
begin
  if AArgs.Length >= 1 then
  begin
    Value := AArgs.GetElement(0);
    WriteLn(GroupPrefix + FormatValue(Value));
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
      WriteLn(GroupPrefix + Msg);
    end;
  end
  else
    WriteLn(GroupPrefix + 'Assertion failed');
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

  FCounters.AssignProperty(LabelStr, TGocciaNumberLiteralValue.Create(CountVal * 1.0));
  WriteLn(GroupPrefix + LabelStr + ': ' + IntToStr(CountVal));
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
    WriteLn(GroupPrefix + LabelStr + ': ' + FormatFloat('0.###', Elapsed) + 'ms');
    FTimers.DeleteProperty(LabelStr);
  end
  else
    WriteLn(GroupPrefix + 'Timer ''' + LabelStr + ''' does not exist');
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
    WriteLn(GroupPrefix + LabelStr + ': ' + FormatFloat('0.###', Elapsed) + 'ms');
  end
  else
    WriteLn(GroupPrefix + 'Timer ''' + LabelStr + ''' does not exist');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleClear(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleGroup(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length > 0 then
    WriteLn(GroupPrefix + FormatArgs(AArgs));
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
    WriteLn(GroupPrefix + 'Trace: ' + FormatArgs(AArgs))
  else
    WriteLn(GroupPrefix + 'Trace');
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaConsole.ConsoleTable(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if AArgs.Length >= 1 then
    WriteLn(GroupPrefix + FormatValue(AArgs.GetElement(0)));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.
