unit Goccia.Bytecode.Binary;

{$I Goccia.inc}

interface

uses
  Souffle.Bytecode.Binary,

  Goccia.Bytecode.Module;

type
  TGocciaBytecodeWriter = TSouffleBytecodeWriter;
  TGocciaBytecodeReader = TSouffleBytecodeReader;

procedure SaveModuleToFile(const AModule: TGocciaBytecodeModule; const AFileName: string); inline;
function LoadModuleFromFile(const AFileName: string): TGocciaBytecodeModule; inline;

implementation

procedure SaveModuleToFile(const AModule: TGocciaBytecodeModule; const AFileName: string);
begin
  Souffle.Bytecode.Binary.SaveModuleToFile(AModule, AFileName);
end;

function LoadModuleFromFile(const AFileName: string): TGocciaBytecodeModule;
begin
  Result := Souffle.Bytecode.Binary.LoadModuleFromFile(AFileName);
end;

end.
