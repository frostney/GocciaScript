unit Goccia.FFI.Call;

// FFI call dispatch has two mechanisms:
//
// 1. Homogeneous signatures (all-int, all-single, all-double): typed function
//    pointer tables — FPC generates correct ABI code from the declaration.
//
// 2. General signatures: a descriptor-driven machine state and one assembly
//    invocation per ABI load the planned GPR, FPR, and stack placements.

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.FFI.ABI,
  Goccia.FFI.Types;

procedure FFIInvokeCompiled(
  const AFunc: Pointer;
  const APlan: TGocciaFFICompiledSignature;
  const AArguments: array of TBytes;
  out AResult: TBytes);

procedure FFIDispatchCall(
  const AFunc: Pointer;
  const AArgCount: Integer;
  const AArgClass: TGocciaFFIArgClass;
  const AReturnClass: TGocciaFFIReturnClass;
  const AIntArgs: array of NativeInt;
  const ASingleArgs: array of Single;
  const ADoubleArgs: array of Double;
  out AResult: TGocciaFFIResult);

implementation

// ==========================================================================
// Homogeneous function pointer types (arity 0-8 × N/S/D × 4 returns)
// ==========================================================================

type
  TFFIProc0  = procedure; cdecl;
  TFFIFuncN0 = function: NativeInt; cdecl;
  TFFIFuncS0 = function: Single; cdecl;
  TFFIFuncD0 = function: Double; cdecl;

  TFFIProc1N  = procedure(A0: NativeInt); cdecl;
  TFFIFuncN1N = function(A0: NativeInt): NativeInt; cdecl;
  TFFIFuncS1N = function(A0: NativeInt): Single; cdecl;
  TFFIFuncD1N = function(A0: NativeInt): Double; cdecl;

  TFFIProc1S  = procedure(A0: Single); cdecl;
  TFFIFuncN1S = function(A0: Single): NativeInt; cdecl;
  TFFIFuncS1S = function(A0: Single): Single; cdecl;
  TFFIFuncD1S = function(A0: Single): Double; cdecl;

  TFFIProc1D  = procedure(A0: Double); cdecl;
  TFFIFuncN1D = function(A0: Double): NativeInt; cdecl;
  TFFIFuncS1D = function(A0: Double): Single; cdecl;
  TFFIFuncD1D = function(A0: Double): Double; cdecl;

  TFFIProc2N  = procedure(A0, A1: NativeInt); cdecl;
  TFFIFuncN2N = function(A0, A1: NativeInt): NativeInt; cdecl;
  TFFIFuncS2N = function(A0, A1: NativeInt): Single; cdecl;
  TFFIFuncD2N = function(A0, A1: NativeInt): Double; cdecl;

  TFFIProc2S  = procedure(A0, A1: Single); cdecl;
  TFFIFuncN2S = function(A0, A1: Single): NativeInt; cdecl;
  TFFIFuncS2S = function(A0, A1: Single): Single; cdecl;
  TFFIFuncD2S = function(A0, A1: Single): Double; cdecl;

  TFFIProc2D  = procedure(A0, A1: Double); cdecl;
  TFFIFuncN2D = function(A0, A1: Double): NativeInt; cdecl;
  TFFIFuncS2D = function(A0, A1: Double): Single; cdecl;
  TFFIFuncD2D = function(A0, A1: Double): Double; cdecl;

  TFFIProc3N  = procedure(A0, A1, A2: NativeInt); cdecl;
  TFFIFuncN3N = function(A0, A1, A2: NativeInt): NativeInt; cdecl;
  TFFIFuncS3N = function(A0, A1, A2: NativeInt): Single; cdecl;
  TFFIFuncD3N = function(A0, A1, A2: NativeInt): Double; cdecl;

  TFFIProc3S  = procedure(A0, A1, A2: Single); cdecl;
  TFFIFuncN3S = function(A0, A1, A2: Single): NativeInt; cdecl;
  TFFIFuncS3S = function(A0, A1, A2: Single): Single; cdecl;
  TFFIFuncD3S = function(A0, A1, A2: Single): Double; cdecl;

  TFFIProc3D  = procedure(A0, A1, A2: Double); cdecl;
  TFFIFuncN3D = function(A0, A1, A2: Double): NativeInt; cdecl;
  TFFIFuncS3D = function(A0, A1, A2: Double): Single; cdecl;
  TFFIFuncD3D = function(A0, A1, A2: Double): Double; cdecl;

  TFFIProc4N  = procedure(A0, A1, A2, A3: NativeInt); cdecl;
  TFFIFuncN4N = function(A0, A1, A2, A3: NativeInt): NativeInt; cdecl;
  TFFIFuncS4N = function(A0, A1, A2, A3: NativeInt): Single; cdecl;
  TFFIFuncD4N = function(A0, A1, A2, A3: NativeInt): Double; cdecl;

  TFFIProc4S  = procedure(A0, A1, A2, A3: Single); cdecl;
  TFFIFuncN4S = function(A0, A1, A2, A3: Single): NativeInt; cdecl;
  TFFIFuncS4S = function(A0, A1, A2, A3: Single): Single; cdecl;
  TFFIFuncD4S = function(A0, A1, A2, A3: Single): Double; cdecl;

  TFFIProc4D  = procedure(A0, A1, A2, A3: Double); cdecl;
  TFFIFuncN4D = function(A0, A1, A2, A3: Double): NativeInt; cdecl;
  TFFIFuncS4D = function(A0, A1, A2, A3: Double): Single; cdecl;
  TFFIFuncD4D = function(A0, A1, A2, A3: Double): Double; cdecl;

  TFFIProc5N  = procedure(A0, A1, A2, A3, A4: NativeInt); cdecl;
  TFFIFuncN5N = function(A0, A1, A2, A3, A4: NativeInt): NativeInt; cdecl;
  TFFIFuncS5N = function(A0, A1, A2, A3, A4: NativeInt): Single; cdecl;
  TFFIFuncD5N = function(A0, A1, A2, A3, A4: NativeInt): Double; cdecl;

  TFFIProc5S  = procedure(A0, A1, A2, A3, A4: Single); cdecl;
  TFFIFuncN5S = function(A0, A1, A2, A3, A4: Single): NativeInt; cdecl;
  TFFIFuncS5S = function(A0, A1, A2, A3, A4: Single): Single; cdecl;
  TFFIFuncD5S = function(A0, A1, A2, A3, A4: Single): Double; cdecl;

  TFFIProc5D  = procedure(A0, A1, A2, A3, A4: Double); cdecl;
  TFFIFuncN5D = function(A0, A1, A2, A3, A4: Double): NativeInt; cdecl;
  TFFIFuncS5D = function(A0, A1, A2, A3, A4: Double): Single; cdecl;
  TFFIFuncD5D = function(A0, A1, A2, A3, A4: Double): Double; cdecl;

  TFFIProc6N  = procedure(A0, A1, A2, A3, A4, A5: NativeInt); cdecl;
  TFFIFuncN6N = function(A0, A1, A2, A3, A4, A5: NativeInt): NativeInt; cdecl;
  TFFIFuncS6N = function(A0, A1, A2, A3, A4, A5: NativeInt): Single; cdecl;
  TFFIFuncD6N = function(A0, A1, A2, A3, A4, A5: NativeInt): Double; cdecl;

  TFFIProc6S  = procedure(A0, A1, A2, A3, A4, A5: Single); cdecl;
  TFFIFuncN6S = function(A0, A1, A2, A3, A4, A5: Single): NativeInt; cdecl;
  TFFIFuncS6S = function(A0, A1, A2, A3, A4, A5: Single): Single; cdecl;
  TFFIFuncD6S = function(A0, A1, A2, A3, A4, A5: Single): Double; cdecl;

  TFFIProc6D  = procedure(A0, A1, A2, A3, A4, A5: Double); cdecl;
  TFFIFuncN6D = function(A0, A1, A2, A3, A4, A5: Double): NativeInt; cdecl;
  TFFIFuncS6D = function(A0, A1, A2, A3, A4, A5: Double): Single; cdecl;
  TFFIFuncD6D = function(A0, A1, A2, A3, A4, A5: Double): Double; cdecl;

  TFFIProc7N  = procedure(A0, A1, A2, A3, A4, A5, A6: NativeInt); cdecl;
  TFFIFuncN7N = function(A0, A1, A2, A3, A4, A5, A6: NativeInt): NativeInt; cdecl;
  TFFIFuncS7N = function(A0, A1, A2, A3, A4, A5, A6: NativeInt): Single; cdecl;
  TFFIFuncD7N = function(A0, A1, A2, A3, A4, A5, A6: NativeInt): Double; cdecl;

  TFFIProc7S  = procedure(A0, A1, A2, A3, A4, A5, A6: Single); cdecl;
  TFFIFuncN7S = function(A0, A1, A2, A3, A4, A5, A6: Single): NativeInt; cdecl;
  TFFIFuncS7S = function(A0, A1, A2, A3, A4, A5, A6: Single): Single; cdecl;
  TFFIFuncD7S = function(A0, A1, A2, A3, A4, A5, A6: Single): Double; cdecl;

  TFFIProc7D  = procedure(A0, A1, A2, A3, A4, A5, A6: Double); cdecl;
  TFFIFuncN7D = function(A0, A1, A2, A3, A4, A5, A6: Double): NativeInt; cdecl;
  TFFIFuncS7D = function(A0, A1, A2, A3, A4, A5, A6: Double): Single; cdecl;
  TFFIFuncD7D = function(A0, A1, A2, A3, A4, A5, A6: Double): Double; cdecl;

  TFFIProc8N  = procedure(A0, A1, A2, A3, A4, A5, A6, A7: NativeInt); cdecl;
  TFFIFuncN8N = function(A0, A1, A2, A3, A4, A5, A6, A7: NativeInt): NativeInt; cdecl;
  TFFIFuncS8N = function(A0, A1, A2, A3, A4, A5, A6, A7: NativeInt): Single; cdecl;
  TFFIFuncD8N = function(A0, A1, A2, A3, A4, A5, A6, A7: NativeInt): Double; cdecl;

  TFFIProc8S  = procedure(A0, A1, A2, A3, A4, A5, A6, A7: Single); cdecl;
  TFFIFuncN8S = function(A0, A1, A2, A3, A4, A5, A6, A7: Single): NativeInt; cdecl;
  TFFIFuncS8S = function(A0, A1, A2, A3, A4, A5, A6, A7: Single): Single; cdecl;
  TFFIFuncD8S = function(A0, A1, A2, A3, A4, A5, A6, A7: Single): Double; cdecl;

  TFFIProc8D  = procedure(A0, A1, A2, A3, A4, A5, A6, A7: Double); cdecl;
  TFFIFuncN8D = function(A0, A1, A2, A3, A4, A5, A6, A7: Double): NativeInt; cdecl;
  TFFIFuncS8D = function(A0, A1, A2, A3, A4, A5, A6, A7: Double): Single; cdecl;
  TFFIFuncD8D = function(A0, A1, A2, A3, A4, A5, A6, A7: Double): Double; cdecl;

// ==========================================================================
// Homogeneous dispatch helpers (one per arity × class)
// ==========================================================================

procedure Dispatch0(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc0(AFunc)(); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN0(AFunc)();
    frcSingle:  AResult.AsSingle := TFFIFuncS0(AFunc)();
    frcDouble:  AResult.AsDouble := TFFIFuncD0(AFunc)();
  end;
end;

// ==========================================================================
// Homogeneous dispatch helpers (Dispatch1N..Dispatch8D)
// Kept in a separate include for readability.
// ==========================================================================

procedure Dispatch1N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc1N(AFunc)(A[0]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN1N(AFunc)(A[0]);
    frcSingle:  AResult.AsSingle := TFFIFuncS1N(AFunc)(A[0]);
    frcDouble:  AResult.AsDouble := TFFIFuncD1N(AFunc)(A[0]);
  end;
end;

procedure Dispatch1S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc1S(AFunc)(A[0]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN1S(AFunc)(A[0]);
    frcSingle:  AResult.AsSingle := TFFIFuncS1S(AFunc)(A[0]);
    frcDouble:  AResult.AsDouble := TFFIFuncD1S(AFunc)(A[0]);
  end;
end;

procedure Dispatch1D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc1D(AFunc)(A[0]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN1D(AFunc)(A[0]);
    frcSingle:  AResult.AsSingle := TFFIFuncS1D(AFunc)(A[0]);
    frcDouble:  AResult.AsDouble := TFFIFuncD1D(AFunc)(A[0]);
  end;
end;

procedure Dispatch2N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc2N(AFunc)(A[0],A[1]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN2N(AFunc)(A[0],A[1]); frcSingle: AResult.AsSingle:=TFFIFuncS2N(AFunc)(A[0],A[1]); frcDouble: AResult.AsDouble:=TFFIFuncD2N(AFunc)(A[0],A[1]); end; end;
procedure Dispatch2S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc2S(AFunc)(A[0],A[1]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN2S(AFunc)(A[0],A[1]); frcSingle: AResult.AsSingle:=TFFIFuncS2S(AFunc)(A[0],A[1]); frcDouble: AResult.AsDouble:=TFFIFuncD2S(AFunc)(A[0],A[1]); end; end;
procedure Dispatch2D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc2D(AFunc)(A[0],A[1]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN2D(AFunc)(A[0],A[1]); frcSingle: AResult.AsSingle:=TFFIFuncS2D(AFunc)(A[0],A[1]); frcDouble: AResult.AsDouble:=TFFIFuncD2D(AFunc)(A[0],A[1]); end; end;

procedure Dispatch3N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc3N(AFunc)(A[0],A[1],A[2]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN3N(AFunc)(A[0],A[1],A[2]); frcSingle: AResult.AsSingle:=TFFIFuncS3N(AFunc)(A[0],A[1],A[2]); frcDouble: AResult.AsDouble:=TFFIFuncD3N(AFunc)(A[0],A[1],A[2]); end; end;
procedure Dispatch3S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc3S(AFunc)(A[0],A[1],A[2]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN3S(AFunc)(A[0],A[1],A[2]); frcSingle: AResult.AsSingle:=TFFIFuncS3S(AFunc)(A[0],A[1],A[2]); frcDouble: AResult.AsDouble:=TFFIFuncD3S(AFunc)(A[0],A[1],A[2]); end; end;
procedure Dispatch3D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc3D(AFunc)(A[0],A[1],A[2]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN3D(AFunc)(A[0],A[1],A[2]); frcSingle: AResult.AsSingle:=TFFIFuncS3D(AFunc)(A[0],A[1],A[2]); frcDouble: AResult.AsDouble:=TFFIFuncD3D(AFunc)(A[0],A[1],A[2]); end; end;

procedure Dispatch4N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc4N(AFunc)(A[0],A[1],A[2],A[3]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN4N(AFunc)(A[0],A[1],A[2],A[3]); frcSingle: AResult.AsSingle:=TFFIFuncS4N(AFunc)(A[0],A[1],A[2],A[3]); frcDouble: AResult.AsDouble:=TFFIFuncD4N(AFunc)(A[0],A[1],A[2],A[3]); end; end;
procedure Dispatch4S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc4S(AFunc)(A[0],A[1],A[2],A[3]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN4S(AFunc)(A[0],A[1],A[2],A[3]); frcSingle: AResult.AsSingle:=TFFIFuncS4S(AFunc)(A[0],A[1],A[2],A[3]); frcDouble: AResult.AsDouble:=TFFIFuncD4S(AFunc)(A[0],A[1],A[2],A[3]); end; end;
procedure Dispatch4D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc4D(AFunc)(A[0],A[1],A[2],A[3]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN4D(AFunc)(A[0],A[1],A[2],A[3]); frcSingle: AResult.AsSingle:=TFFIFuncS4D(AFunc)(A[0],A[1],A[2],A[3]); frcDouble: AResult.AsDouble:=TFFIFuncD4D(AFunc)(A[0],A[1],A[2],A[3]); end; end;

procedure Dispatch5N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc5N(AFunc)(A[0],A[1],A[2],A[3],A[4]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN5N(AFunc)(A[0],A[1],A[2],A[3],A[4]); frcSingle: AResult.AsSingle:=TFFIFuncS5N(AFunc)(A[0],A[1],A[2],A[3],A[4]); frcDouble: AResult.AsDouble:=TFFIFuncD5N(AFunc)(A[0],A[1],A[2],A[3],A[4]); end; end;
procedure Dispatch5S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc5S(AFunc)(A[0],A[1],A[2],A[3],A[4]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN5S(AFunc)(A[0],A[1],A[2],A[3],A[4]); frcSingle: AResult.AsSingle:=TFFIFuncS5S(AFunc)(A[0],A[1],A[2],A[3],A[4]); frcDouble: AResult.AsDouble:=TFFIFuncD5S(AFunc)(A[0],A[1],A[2],A[3],A[4]); end; end;
procedure Dispatch5D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc5D(AFunc)(A[0],A[1],A[2],A[3],A[4]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN5D(AFunc)(A[0],A[1],A[2],A[3],A[4]); frcSingle: AResult.AsSingle:=TFFIFuncS5D(AFunc)(A[0],A[1],A[2],A[3],A[4]); frcDouble: AResult.AsDouble:=TFFIFuncD5D(AFunc)(A[0],A[1],A[2],A[3],A[4]); end; end;

procedure Dispatch6N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc6N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN6N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); frcSingle: AResult.AsSingle:=TFFIFuncS6N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); frcDouble: AResult.AsDouble:=TFFIFuncD6N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); end; end;
procedure Dispatch6S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc6S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN6S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); frcSingle: AResult.AsSingle:=TFFIFuncS6S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); frcDouble: AResult.AsDouble:=TFFIFuncD6S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); end; end;
procedure Dispatch6D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc6D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN6D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); frcSingle: AResult.AsSingle:=TFFIFuncS6D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); frcDouble: AResult.AsDouble:=TFFIFuncD6D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5]); end; end;

procedure Dispatch7N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc7N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN7N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); frcSingle: AResult.AsSingle:=TFFIFuncS7N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); frcDouble: AResult.AsDouble:=TFFIFuncD7N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); end; end;
procedure Dispatch7S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc7S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN7S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); frcSingle: AResult.AsSingle:=TFFIFuncS7S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); frcDouble: AResult.AsDouble:=TFFIFuncD7S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); end; end;
procedure Dispatch7D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc7D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN7D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); frcSingle: AResult.AsSingle:=TFFIFuncS7D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); frcDouble: AResult.AsDouble:=TFFIFuncD7D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6]); end; end;

procedure Dispatch8N(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of NativeInt; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc8N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN8N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); frcSingle: AResult.AsSingle:=TFFIFuncS8N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); frcDouble: AResult.AsDouble:=TFFIFuncD8N(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); end; end;
procedure Dispatch8S(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc8S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN8S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); frcSingle: AResult.AsSingle:=TFFIFuncS8S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); frcDouble: AResult.AsDouble:=TFFIFuncD8S(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); end; end;
procedure Dispatch8D(const AFunc: Pointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin case AReturnClass of frcVoid: begin TFFIProc8D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); AResult.AsInt:=0; end; frcInteger: AResult.AsInt:=TFFIFuncN8D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); frcSingle: AResult.AsSingle:=TFFIFuncS8D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); frcDouble: AResult.AsDouble:=TFFIFuncD8D(AFunc)(A[0],A[1],A[2],A[3],A[4],A[5],A[6],A[7]); end; end;

// ==========================================================================
// Main dispatch entry point for homogeneous signatures.
// ==========================================================================

procedure FFIDispatchCall(
  const AFunc: Pointer;
  const AArgCount: Integer;
  const AArgClass: TGocciaFFIArgClass;
  const AReturnClass: TGocciaFFIReturnClass;
  const AIntArgs: array of NativeInt;
  const ASingleArgs: array of Single;
  const ADoubleArgs: array of Double;
  out AResult: TGocciaFFIResult);
begin
  FillChar(AResult, SizeOf(AResult), 0);

  if AArgCount = 0 then
  begin
    Dispatch0(AFunc, AReturnClass, AResult);
    Exit;
  end;

  case AArgClass of
    facInteger:
      case AArgCount of
        1: Dispatch1N(AFunc, AReturnClass, AIntArgs, AResult);
        2: Dispatch2N(AFunc, AReturnClass, AIntArgs, AResult);
        3: Dispatch3N(AFunc, AReturnClass, AIntArgs, AResult);
        4: Dispatch4N(AFunc, AReturnClass, AIntArgs, AResult);
        5: Dispatch5N(AFunc, AReturnClass, AIntArgs, AResult);
        6: Dispatch6N(AFunc, AReturnClass, AIntArgs, AResult);
        7: Dispatch7N(AFunc, AReturnClass, AIntArgs, AResult);
        8: Dispatch8N(AFunc, AReturnClass, AIntArgs, AResult);
      end;
    facSingle:
      case AArgCount of
        1: Dispatch1S(AFunc, AReturnClass, ASingleArgs, AResult);
        2: Dispatch2S(AFunc, AReturnClass, ASingleArgs, AResult);
        3: Dispatch3S(AFunc, AReturnClass, ASingleArgs, AResult);
        4: Dispatch4S(AFunc, AReturnClass, ASingleArgs, AResult);
        5: Dispatch5S(AFunc, AReturnClass, ASingleArgs, AResult);
        6: Dispatch6S(AFunc, AReturnClass, ASingleArgs, AResult);
        7: Dispatch7S(AFunc, AReturnClass, ASingleArgs, AResult);
        8: Dispatch8S(AFunc, AReturnClass, ASingleArgs, AResult);
      end;
    facDouble:
      case AArgCount of
        1: Dispatch1D(AFunc, AReturnClass, ADoubleArgs, AResult);
        2: Dispatch2D(AFunc, AReturnClass, ADoubleArgs, AResult);
        3: Dispatch3D(AFunc, AReturnClass, ADoubleArgs, AResult);
        4: Dispatch4D(AFunc, AReturnClass, ADoubleArgs, AResult);
        5: Dispatch5D(AFunc, AReturnClass, ADoubleArgs, AResult);
        6: Dispatch6D(AFunc, AReturnClass, ADoubleArgs, AResult);
        7: Dispatch7D(AFunc, AReturnClass, ADoubleArgs, AResult);
        8: Dispatch8D(AFunc, AReturnClass, ADoubleArgs, AResult);
      end;
  end;
end;

// ===========================================================================
// Generalized descriptor-driven call frame
// ===========================================================================

type
  {$IF defined(GOCCIA_CPU_64)}
  {$IFDEF FPC}
    {$PUSH}{$PACKRECORDS 8}
  {$ELSE}
    {$ALIGN 8}
  {$ENDIF}
  TGocciaFFIMachineState = record
    FuncPtr: Pointer;             //   0
    GPR: array[0..7] of UInt64;        //   8
    FPR: array[0..7] of UInt64;        //  72
    StackData: Pointer;               // 136
    StackSize: NativeUInt;               // 144
    HiddenResult: Pointer;            // 152
    RetGPR: array[0..1] of UInt64;     // 160
    RetFPR: array[0..3] of UInt64;     // 176
  end;
  {$IFDEF FPC}
    {$POP}
  {$ENDIF}
  {$ELSE}
  {$IFDEF FPC}
    {$PUSH}{$PACKRECORDS 4}
  {$ELSE}
    {$ALIGN 4}
  {$ENDIF}
  TGocciaFFIMachineState = record
    FuncPtr: Pointer;             //  0
    StackData: Pointer;               //  4
    StackSize: LongWord;              //  8
    RetGPR: array[0..1] of LongWord;  // 12
    RetFPR: UInt64;                    // 20
    ReturnFloatSize: LongWord;        // 28
  end;
  {$IFDEF FPC}
    {$POP}
  {$ELSE}
    {$ALIGN 8}
  {$ENDIF}
  {$ENDIF}

{$IF defined(GOCCIA_CPU_64)}
{$IF (defined(GOCCIA_CPU_X86_64))}
{$IFDEF FPC}
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure FFIInvokeMachine(var AState: TGocciaFFIMachineState); assembler;
{$IFDEF FPC}
  nostackframe;
{$ENDIF}
asm
  push rbx
  push rsi
  push rdi
  push r12
  mov rbx, rcx

  mov r12, [rbx + 144]
  add r12, 40
  sub rsp, r12

  mov rsi, [rbx + 136]
  lea rdi, [rsp + 32]
  mov rcx, [rbx + 144]
  rep movsb

  mov r11, [rbx]
  mov rcx, [rbx + 8]
  mov rdx, [rbx + 16]
  mov r8, [rbx + 24]
  mov r9, [rbx + 32]
  movq xmm0, [rbx + 72]
  movq xmm1, [rbx + 80]
  movq xmm2, [rbx + 88]
  movq xmm3, [rbx + 96]
  call r11

  mov [rbx + 160], rax
  mov [rbx + 168], rdx
  movq [rbx + 176], xmm0
  movq [rbx + 184], xmm1

  add rsp, r12
  pop r12
  pop rdi
  pop rsi
  pop rbx
  ret
end;
{$ELSE}
procedure FFIInvokeMachine(var AState: TGocciaFFIMachineState); assembler;
{$IFDEF FPC}
  nostackframe;
{$ENDIF}
asm
  push rbx
  push r12
  mov rbx, rdi

  mov r12, [rbx + 144]
  add r12, 8
  sub rsp, r12

  mov rsi, [rbx + 136]
  mov rdi, rsp
  mov rcx, [rbx + 144]
  rep movsb

  mov r11, [rbx]
  movq xmm0, [rbx + 72]
  movq xmm1, [rbx + 80]
  movq xmm2, [rbx + 88]
  movq xmm3, [rbx + 96]
  movq xmm4, [rbx + 104]
  movq xmm5, [rbx + 112]
  movq xmm6, [rbx + 120]
  movq xmm7, [rbx + 128]
  mov rdi, [rbx + 8]
  mov rsi, [rbx + 16]
  mov rdx, [rbx + 24]
  mov rcx, [rbx + 32]
  mov r8, [rbx + 40]
  mov r9, [rbx + 48]
  mov al, 8
  call r11

  mov [rbx + 160], rax
  mov [rbx + 168], rdx
  movq [rbx + 176], xmm0
  movq [rbx + 184], xmm1

  add rsp, r12
  pop r12
  pop rbx
  ret
end;
{$ENDIF}
{$ENDIF}

{$IF (defined(GOCCIA_CPU_AARCH64))}
procedure FFIInvokeMachine(var AState: TGocciaFFIMachineState); assembler; nostackframe;
asm
  sub sp, sp, #32
  stp x19, x20, [sp]
  str x30, [sp, #16]
  mov x19, x0

  ldr x20, [x19, #144]
  sub sp, sp, x20
  ldr x9, [x19, #136]
  mov x10, sp
  mov x11, x20
.Lffi_copy_stack:
  cbz x11, .Lffi_stack_ready
  ldr x12, [x9], #8
  str x12, [x10], #8
  sub x11, x11, #8
  b .Lffi_copy_stack
.Lffi_stack_ready:
  ldr x16, [x19, #0]
  ldr x8, [x19, #152]
  ldr d0, [x19, #72]
  ldr d1, [x19, #80]
  ldr d2, [x19, #88]
  ldr d3, [x19, #96]
  ldr d4, [x19, #104]
  ldr d5, [x19, #112]
  ldr d6, [x19, #120]
  ldr d7, [x19, #128]
  ldr x0, [x19, #8]
  ldr x1, [x19, #16]
  ldr x2, [x19, #24]
  ldr x3, [x19, #32]
  ldr x4, [x19, #40]
  ldr x5, [x19, #48]
  ldr x6, [x19, #56]
  ldr x7, [x19, #64]
  blr x16

  str x0, [x19, #160]
  str x1, [x19, #168]
  str d0, [x19, #176]
  str d1, [x19, #184]
  str d2, [x19, #192]
  str d3, [x19, #200]

  add sp, sp, x20
  ldr x30, [sp, #16]
  ldp x19, x20, [sp]
  add sp, sp, #32
  ret
end;
{$ENDIF}
{$ENDIF}

{$IF (defined(GOCCIA_CPU_X86))}
{$IFDEF FPC}
{$ENDIF}
procedure FFIInvokeMachine(var AState: TGocciaFFIMachineState); assembler;
{$IFDEF FPC}
  nostackframe;
{$ENDIF}
asm
  push ebx
  push esi
  push edi
  mov ebx, eax

  mov ecx, [ebx + 8]
{$IFDEF MSWINDOWS}
  test ecx, ecx
  je @@stack_ready
  mov edx, ecx
@@probe_page:
  cmp edx, 4096
  jbe @@probe_tail
  sub esp, 4096
  test byte ptr [esp], 0
  sub edx, 4096
  jmp @@probe_page
@@probe_tail:
  sub esp, edx
  test byte ptr [esp], 0
@@stack_ready:
{$ELSE}
  sub esp, ecx
{$ENDIF}
  mov esi, [ebx + 4]
  mov edi, esp
  mov ecx, [ebx + 8]
  rep movsb

  call dword ptr [ebx]
  mov [ebx + 12], eax
  mov [ebx + 16], edx
  cmp dword ptr [ebx + 28], 4
  je @@store_single
  cmp dword ptr [ebx + 28], 8
  je @@store_double
  jmp @@return_stored
@@store_single:
  fstp dword ptr [ebx + 20]
  jmp @@return_stored
@@store_double:
  fstp qword ptr [ebx + 20]
@@return_stored:
  mov ecx, [ebx + 8]
  add esp, ecx
  pop edi
  pop esi
  pop ebx
end;
{$ENDIF}

procedure ValidateMachineStateLayout;
var
  State: TGocciaFFIMachineState;
  Base: NativeUInt;
begin
  Base := NativeUInt(@State);
  {$IF defined(GOCCIA_CPU_64)}
  if (NativeUInt(@State.GPR[0]) - Base <> 8) or
     (NativeUInt(@State.FPR[0]) - Base <> 72) or
     (NativeUInt(@State.StackData) - Base <> 136) or
     (NativeUInt(@State.StackSize) - Base <> 144) or
     (NativeUInt(@State.HiddenResult) - Base <> 152) or
     (NativeUInt(@State.RetGPR[0]) - Base <> 160) or
     (NativeUInt(@State.RetFPR[0]) - Base <> 176) then
    raise EInvalidOpException.Create('FFI 64-bit machine-state layout mismatch');
  {$ELSE}
  if (NativeUInt(@State.StackData) - Base <> 4) or
     (NativeUInt(@State.StackSize) - Base <> 8) or
     (NativeUInt(@State.RetGPR[0]) - Base <> 12) or
     (NativeUInt(@State.RetFPR) - Base <> 20) or
     (NativeUInt(@State.ReturnFloatSize) - Base <> 28) then
    raise EInvalidOpException.Create('FFI i386 machine-state layout mismatch');
  {$ENDIF}
end;

procedure CopyToPlacement(var AState: TGocciaFFIMachineState;
  var AStackData: TBytes; const APlacement: TGocciaFFIPlacement;
  const ASource; const ASize: Integer);
begin
  case APlacement.Kind of
    fpkGPR:
      {$IF defined(GOCCIA_CPU_64)}
      Move(ASource, AState.GPR[APlacement.RegisterIndex], ASize)
      {$ELSE}
      raise EInvalidOpException.Create('i386 arguments cannot use GPR placements')
      {$ENDIF};
    fpkFPR:
      {$IF defined(GOCCIA_CPU_64)}
      Move(ASource, AState.FPR[APlacement.RegisterIndex], ASize)
      {$ELSE}
      raise EInvalidOpException.Create('i386 arguments cannot use FPR placements')
      {$ENDIF};
    fpkStack:
      Move(ASource, AStackData[APlacement.StackOffset], ASize);
  end;
end;

procedure CopyDarwinARM64ScalarArgumentToPlacement(
  var AState: TGocciaFFIMachineState; var AStackData: TBytes;
  const APlacement: TGocciaFFIPlacement;
  const AType: TGocciaFFITypeDescriptor; const ASource);
var
  BooleanValue: Byte;
  Signed8: ShortInt;
  Signed16: SmallInt;
  Signed32: LongInt;
  Unsigned8: Byte;
  Unsigned16: Word;
  Unsigned32: LongWord;
begin
  if (APlacement.Kind <> fpkGPR) or
     (AType.Kind <> ftkScalar) then
  begin
    CopyToPlacement(AState, AStackData, APlacement, ASource,
      APlacement.Size);
    Exit;
  end;

  case AType.ScalarType of
    fftBool:
    begin
      Move(ASource, BooleanValue, SizeOf(BooleanValue));
      if BooleanValue <> 0 then
        Unsigned32 := 1
      else
        Unsigned32 := 0;
      CopyToPlacement(AState, AStackData, APlacement, Unsigned32,
        SizeOf(Unsigned32));
    end;
    fftI8:
    begin
      Move(ASource, Signed8, SizeOf(Signed8));
      Signed32 := Signed8;
      CopyToPlacement(AState, AStackData, APlacement, Signed32,
        SizeOf(Signed32));
    end;
    fftI16:
    begin
      Move(ASource, Signed16, SizeOf(Signed16));
      Signed32 := Signed16;
      CopyToPlacement(AState, AStackData, APlacement, Signed32,
        SizeOf(Signed32));
    end;
    fftU8:
    begin
      Move(ASource, Unsigned8, SizeOf(Unsigned8));
      Unsigned32 := Unsigned8;
      CopyToPlacement(AState, AStackData, APlacement, Unsigned32,
        SizeOf(Unsigned32));
    end;
    fftU16:
    begin
      Move(ASource, Unsigned16, SizeOf(Unsigned16));
      Unsigned32 := Unsigned16;
      CopyToPlacement(AState, AStackData, APlacement, Unsigned32,
        SizeOf(Unsigned32));
    end;
  else
    CopyToPlacement(AState, AStackData, APlacement, ASource,
      APlacement.Size);
  end;
end;

procedure FFIInvokeCompiled(const AFunc: Pointer;
  const APlan: TGocciaFFICompiledSignature;
  const AArguments: array of TBytes; out AResult: TBytes);
var
  State: TGocciaFFIMachineState;
  StackData, ScratchData, PointerBytes: TBytes;
  ArgumentPlan: TGocciaFFIArgumentPlan;
  ReturnPlan: TGocciaFFIReturnPlan;
  Placement: TGocciaFFIPlacement;
  IndirectPointer: Pointer;
  I, J, CopySize: Integer;
begin
  if Length(AArguments) <> APlan.ArgumentCount then
    raise EArgumentException.Create('FFI native argument count does not match plan');
  FillChar(State, SizeOf(State), 0);
  State.FuncPtr := AFunc;
  SetLength(StackData, APlan.StackSize);
  if Length(StackData) > 0 then
    FillChar(StackData[0], Length(StackData), 0);
  SetLength(ScratchData, APlan.ScratchSize);
  if Length(ScratchData) > 0 then
    FillChar(ScratchData[0], Length(ScratchData), 0);
  SetLength(PointerBytes, SizeOf(Pointer));

  ReturnPlan := APlan.ReturnPlan;
  SetLength(AResult, ReturnPlan.TypeDescriptor.Size);
  if Length(AResult) > 0 then
    FillChar(AResult[0], Length(AResult), 0);
  if ReturnPlan.UsesHiddenPointer then
  begin
    if Length(AResult) = 0 then
      raise EInvalidOpException.Create('FFI hidden return has no storage');
    case APlan.ABI of
      fabiSysVX64, fabiWin64:
        {$IF defined(GOCCIA_CPU_64)}
        State.GPR[0] := UInt64(NativeUInt(@AResult[0]))
        {$ELSE}
        raise EInvalidOpException.Create('64-bit FFI plan on i386')
        {$ENDIF};
      fabiAAPCS64, fabiDarwinARM64:
        {$IF defined(GOCCIA_CPU_64)}
        State.HiddenResult := @AResult[0]
        {$ELSE}
        raise EInvalidOpException.Create('ARM64 FFI plan on i386')
        {$ENDIF};
      fabiI386Win:
      begin
        IndirectPointer := @AResult[0];
        Move(IndirectPointer, StackData[0], SizeOf(Pointer));
      end;
    end;
  end;

  for I := 0 to APlan.ArgumentCount - 1 do
  begin
    ArgumentPlan := APlan.Arguments[I];
    if Length(AArguments[I]) < ArgumentPlan.TypeDescriptor.Size then
      raise EArgumentException.Create('FFI native argument buffer is too small');
    if ArgumentPlan.Indirect then
    begin
      if ArgumentPlan.TypeDescriptor.Size > 0 then
        Move(AArguments[I][0], ScratchData[ArgumentPlan.IndirectCopyOffset],
          ArgumentPlan.TypeDescriptor.Size);
      IndirectPointer := @ScratchData[ArgumentPlan.IndirectCopyOffset];
      Move(IndirectPointer, PointerBytes[0], SizeOf(Pointer));
      Placement := ArgumentPlan.Placements[0];
      CopyToPlacement(State, StackData, Placement, PointerBytes[0],
        SizeOf(Pointer));
      Continue;
    end;
    for J := 0 to High(ArgumentPlan.Placements) do
    begin
      Placement := ArgumentPlan.Placements[J];
      CopySize := Placement.Size;
      if CopySize > 0 then
      begin
        if APlan.ABI = fabiDarwinARM64 then
          CopyDarwinARM64ScalarArgumentToPlacement(State, StackData,
            Placement, ArgumentPlan.TypeDescriptor,
            AArguments[I][Placement.ValueOffset])
        else
          CopyToPlacement(State, StackData, Placement,
            AArguments[I][Placement.ValueOffset], CopySize);
      end;
    end;
  end;

  if Length(StackData) > 0 then
    State.StackData := @StackData[0]
  else
    State.StackData := nil;
  State.StackSize := Length(StackData);
  {$IF (defined(GOCCIA_CPU_X86))}
  if (Length(ReturnPlan.Placements) > 0) and
     (ReturnPlan.Placements[0].Kind = fpkFPR) then
    State.ReturnFloatSize := ReturnPlan.Placements[0].Size;
  {$ENDIF}
  FFIInvokeMachine(State);

  if ReturnPlan.UsesHiddenPointer then Exit;
  for I := 0 to High(ReturnPlan.Placements) do
  begin
    Placement := ReturnPlan.Placements[I];
    case Placement.Kind of
      fpkGPR:
        Move(State.RetGPR[Placement.RegisterIndex],
          AResult[Placement.ValueOffset], Placement.Size);
      fpkFPR:
        {$IF defined(GOCCIA_CPU_64)}
        Move(State.RetFPR[Placement.RegisterIndex],
          AResult[Placement.ValueOffset], Placement.Size)
        {$ELSE}
        Move(State.RetFPR, AResult[Placement.ValueOffset], Placement.Size)
        {$ENDIF};
      fpkStack:
        raise EInvalidOpException.Create('FFI return cannot use stack placement');
    end;
  end;
end;

initialization
  ValidateMachineStateLayout;

end.
