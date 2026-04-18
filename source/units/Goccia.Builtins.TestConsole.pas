unit Goccia.Builtins.TestConsole;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTestConsole = class
  private
    function NoOp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    procedure Silence(const AConsoleObject: TGocciaObjectValue);
  end;

implementation

uses
  Goccia.Values.NativeFunction;

function TGocciaTestConsole.NoOp(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaTestConsole.Silence(const AConsoleObject: TGocciaObjectValue);
begin
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'log', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'warn', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'error', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'info', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'debug', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'dir', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'assert', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'count', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'countReset', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'time', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'timeEnd', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'timeLog', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'clear', 0));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'group', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'groupEnd', 0));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'trace', -1));
  AConsoleObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NoOp, 'table', -1));
end;

end.
