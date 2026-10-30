#!/usr/bin/env -S instantfpc -B -Fu./source/shared

{$mode delphi}
{$H+}

uses
  SysUtils,

  FPCSafeSEH in 'source/shared/FPCSafeSEH.pas';

begin
  try
    if not (((ParamStr(1) = '--apply') and (ParamCount = 4)) or
      ((ParamStr(1) = '--check') and (ParamCount = 3))) then
      raise Exception.Create('Usage: safeseh --apply executable map object-directory | --check executable map');
    if ParamStr(1) = '--apply' then
      FinalizeFPCExecutable(ParamStr(2), ParamStr(3), ParamStr(4))
    else
      CheckFPCExecutable(ParamStr(2), ParamStr(3));
    WriteLn('SafeSEH verified: ', ParamStr(2));
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'SafeSEH failed: ', E.Message);
      Halt(1);
    end;
  end;
end.
