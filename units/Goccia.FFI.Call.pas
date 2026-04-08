unit Goccia.FFI.Call;

// Hand-rolled FFI call dispatch. Defines typed function pointer types for
// every supported arity (0-8) x argument class (integer/single/double) x
// return class (void/integer/single/double), then dispatches to the correct
// one at runtime. FPC generates the correct platform ABI code for each
// typed function pointer, so this works on x86-64, AArch64, and x86-32
// without any assembly or libffi dependency.

{$I Goccia.inc}

interface

uses
  Goccia.FFI.Types;

procedure FFIDispatchCall(
  const AFunc: CodePointer;
  const AArgCount: Integer;
  const AArgClass: TGocciaFFIArgClass;
  const AReturnClass: TGocciaFFIReturnClass;
  const AIntArgs: array of PtrInt;
  const ASingleArgs: array of Single;
  const ADoubleArgs: array of Double;
  out AResult: TGocciaFFIResult);

implementation

// ==========================================================================
// Function pointer type definitions
//
// Naming convention:
//   TFFIProc{N}{C}  = procedure with N args of class C; cdecl
//   TFFIFunc{R}{N}{C} = function returning R with N args of class C; cdecl
//
//   N = argument count (0-8)
//   C = argument class: N=PtrInt (native integer), S=Single, D=Double
//       (omitted for 0-arg types)
//   R = return class: N=PtrInt, S=Single, D=Double
// ==========================================================================

type
  // -- 0 arguments ----------------------------------------------------------
  TFFIProc0  = procedure; cdecl;
  TFFIFuncN0 = function: PtrInt; cdecl;
  TFFIFuncS0 = function: Single; cdecl;
  TFFIFuncD0 = function: Double; cdecl;

  // -- 1 argument, integer class --------------------------------------------
  TFFIProc1N  = procedure(A0: PtrInt); cdecl;
  TFFIFuncN1N = function(A0: PtrInt): PtrInt; cdecl;
  TFFIFuncS1N = function(A0: PtrInt): Single; cdecl;
  TFFIFuncD1N = function(A0: PtrInt): Double; cdecl;

  // -- 1 argument, single class ---------------------------------------------
  TFFIProc1S  = procedure(A0: Single); cdecl;
  TFFIFuncN1S = function(A0: Single): PtrInt; cdecl;
  TFFIFuncS1S = function(A0: Single): Single; cdecl;
  TFFIFuncD1S = function(A0: Single): Double; cdecl;

  // -- 1 argument, double class ---------------------------------------------
  TFFIProc1D  = procedure(A0: Double); cdecl;
  TFFIFuncN1D = function(A0: Double): PtrInt; cdecl;
  TFFIFuncS1D = function(A0: Double): Single; cdecl;
  TFFIFuncD1D = function(A0: Double): Double; cdecl;

  // -- 2 arguments, integer class -------------------------------------------
  TFFIProc2N  = procedure(A0, A1: PtrInt); cdecl;
  TFFIFuncN2N = function(A0, A1: PtrInt): PtrInt; cdecl;
  TFFIFuncS2N = function(A0, A1: PtrInt): Single; cdecl;
  TFFIFuncD2N = function(A0, A1: PtrInt): Double; cdecl;

  // -- 2 arguments, single class --------------------------------------------
  TFFIProc2S  = procedure(A0, A1: Single); cdecl;
  TFFIFuncN2S = function(A0, A1: Single): PtrInt; cdecl;
  TFFIFuncS2S = function(A0, A1: Single): Single; cdecl;
  TFFIFuncD2S = function(A0, A1: Single): Double; cdecl;

  // -- 2 arguments, double class --------------------------------------------
  TFFIProc2D  = procedure(A0, A1: Double); cdecl;
  TFFIFuncN2D = function(A0, A1: Double): PtrInt; cdecl;
  TFFIFuncS2D = function(A0, A1: Double): Single; cdecl;
  TFFIFuncD2D = function(A0, A1: Double): Double; cdecl;

  // -- 3 arguments, integer class -------------------------------------------
  TFFIProc3N  = procedure(A0, A1, A2: PtrInt); cdecl;
  TFFIFuncN3N = function(A0, A1, A2: PtrInt): PtrInt; cdecl;
  TFFIFuncS3N = function(A0, A1, A2: PtrInt): Single; cdecl;
  TFFIFuncD3N = function(A0, A1, A2: PtrInt): Double; cdecl;

  // -- 3 arguments, single class --------------------------------------------
  TFFIProc3S  = procedure(A0, A1, A2: Single); cdecl;
  TFFIFuncN3S = function(A0, A1, A2: Single): PtrInt; cdecl;
  TFFIFuncS3S = function(A0, A1, A2: Single): Single; cdecl;
  TFFIFuncD3S = function(A0, A1, A2: Single): Double; cdecl;

  // -- 3 arguments, double class --------------------------------------------
  TFFIProc3D  = procedure(A0, A1, A2: Double); cdecl;
  TFFIFuncN3D = function(A0, A1, A2: Double): PtrInt; cdecl;
  TFFIFuncS3D = function(A0, A1, A2: Double): Single; cdecl;
  TFFIFuncD3D = function(A0, A1, A2: Double): Double; cdecl;

  // -- 4 arguments, integer class -------------------------------------------
  TFFIProc4N  = procedure(A0, A1, A2, A3: PtrInt); cdecl;
  TFFIFuncN4N = function(A0, A1, A2, A3: PtrInt): PtrInt; cdecl;
  TFFIFuncS4N = function(A0, A1, A2, A3: PtrInt): Single; cdecl;
  TFFIFuncD4N = function(A0, A1, A2, A3: PtrInt): Double; cdecl;

  // -- 4 arguments, single class --------------------------------------------
  TFFIProc4S  = procedure(A0, A1, A2, A3: Single); cdecl;
  TFFIFuncN4S = function(A0, A1, A2, A3: Single): PtrInt; cdecl;
  TFFIFuncS4S = function(A0, A1, A2, A3: Single): Single; cdecl;
  TFFIFuncD4S = function(A0, A1, A2, A3: Single): Double; cdecl;

  // -- 4 arguments, double class --------------------------------------------
  TFFIProc4D  = procedure(A0, A1, A2, A3: Double); cdecl;
  TFFIFuncN4D = function(A0, A1, A2, A3: Double): PtrInt; cdecl;
  TFFIFuncS4D = function(A0, A1, A2, A3: Double): Single; cdecl;
  TFFIFuncD4D = function(A0, A1, A2, A3: Double): Double; cdecl;

  // -- 5 arguments, integer class -------------------------------------------
  TFFIProc5N  = procedure(A0, A1, A2, A3, A4: PtrInt); cdecl;
  TFFIFuncN5N = function(A0, A1, A2, A3, A4: PtrInt): PtrInt; cdecl;
  TFFIFuncS5N = function(A0, A1, A2, A3, A4: PtrInt): Single; cdecl;
  TFFIFuncD5N = function(A0, A1, A2, A3, A4: PtrInt): Double; cdecl;

  // -- 5 arguments, single class --------------------------------------------
  TFFIProc5S  = procedure(A0, A1, A2, A3, A4: Single); cdecl;
  TFFIFuncN5S = function(A0, A1, A2, A3, A4: Single): PtrInt; cdecl;
  TFFIFuncS5S = function(A0, A1, A2, A3, A4: Single): Single; cdecl;
  TFFIFuncD5S = function(A0, A1, A2, A3, A4: Single): Double; cdecl;

  // -- 5 arguments, double class --------------------------------------------
  TFFIProc5D  = procedure(A0, A1, A2, A3, A4: Double); cdecl;
  TFFIFuncN5D = function(A0, A1, A2, A3, A4: Double): PtrInt; cdecl;
  TFFIFuncS5D = function(A0, A1, A2, A3, A4: Double): Single; cdecl;
  TFFIFuncD5D = function(A0, A1, A2, A3, A4: Double): Double; cdecl;

  // -- 6 arguments, integer class -------------------------------------------
  TFFIProc6N  = procedure(A0, A1, A2, A3, A4, A5: PtrInt); cdecl;
  TFFIFuncN6N = function(A0, A1, A2, A3, A4, A5: PtrInt): PtrInt; cdecl;
  TFFIFuncS6N = function(A0, A1, A2, A3, A4, A5: PtrInt): Single; cdecl;
  TFFIFuncD6N = function(A0, A1, A2, A3, A4, A5: PtrInt): Double; cdecl;

  // -- 6 arguments, single class --------------------------------------------
  TFFIProc6S  = procedure(A0, A1, A2, A3, A4, A5: Single); cdecl;
  TFFIFuncN6S = function(A0, A1, A2, A3, A4, A5: Single): PtrInt; cdecl;
  TFFIFuncS6S = function(A0, A1, A2, A3, A4, A5: Single): Single; cdecl;
  TFFIFuncD6S = function(A0, A1, A2, A3, A4, A5: Single): Double; cdecl;

  // -- 6 arguments, double class --------------------------------------------
  TFFIProc6D  = procedure(A0, A1, A2, A3, A4, A5: Double); cdecl;
  TFFIFuncN6D = function(A0, A1, A2, A3, A4, A5: Double): PtrInt; cdecl;
  TFFIFuncS6D = function(A0, A1, A2, A3, A4, A5: Double): Single; cdecl;
  TFFIFuncD6D = function(A0, A1, A2, A3, A4, A5: Double): Double; cdecl;

  // -- 7 arguments, integer class -------------------------------------------
  TFFIProc7N  = procedure(A0, A1, A2, A3, A4, A5, A6: PtrInt); cdecl;
  TFFIFuncN7N = function(A0, A1, A2, A3, A4, A5, A6: PtrInt): PtrInt; cdecl;
  TFFIFuncS7N = function(A0, A1, A2, A3, A4, A5, A6: PtrInt): Single; cdecl;
  TFFIFuncD7N = function(A0, A1, A2, A3, A4, A5, A6: PtrInt): Double; cdecl;

  // -- 7 arguments, single class --------------------------------------------
  TFFIProc7S  = procedure(A0, A1, A2, A3, A4, A5, A6: Single); cdecl;
  TFFIFuncN7S = function(A0, A1, A2, A3, A4, A5, A6: Single): PtrInt; cdecl;
  TFFIFuncS7S = function(A0, A1, A2, A3, A4, A5, A6: Single): Single; cdecl;
  TFFIFuncD7S = function(A0, A1, A2, A3, A4, A5, A6: Single): Double; cdecl;

  // -- 7 arguments, double class --------------------------------------------
  TFFIProc7D  = procedure(A0, A1, A2, A3, A4, A5, A6: Double); cdecl;
  TFFIFuncN7D = function(A0, A1, A2, A3, A4, A5, A6: Double): PtrInt; cdecl;
  TFFIFuncS7D = function(A0, A1, A2, A3, A4, A5, A6: Double): Single; cdecl;
  TFFIFuncD7D = function(A0, A1, A2, A3, A4, A5, A6: Double): Double; cdecl;

  // -- 8 arguments, integer class -------------------------------------------
  TFFIProc8N  = procedure(A0, A1, A2, A3, A4, A5, A6, A7: PtrInt); cdecl;
  TFFIFuncN8N = function(A0, A1, A2, A3, A4, A5, A6, A7: PtrInt): PtrInt; cdecl;
  TFFIFuncS8N = function(A0, A1, A2, A3, A4, A5, A6, A7: PtrInt): Single; cdecl;
  TFFIFuncD8N = function(A0, A1, A2, A3, A4, A5, A6, A7: PtrInt): Double; cdecl;

  // -- 8 arguments, single class --------------------------------------------
  TFFIProc8S  = procedure(A0, A1, A2, A3, A4, A5, A6, A7: Single); cdecl;
  TFFIFuncN8S = function(A0, A1, A2, A3, A4, A5, A6, A7: Single): PtrInt; cdecl;
  TFFIFuncS8S = function(A0, A1, A2, A3, A4, A5, A6, A7: Single): Single; cdecl;
  TFFIFuncD8S = function(A0, A1, A2, A3, A4, A5, A6, A7: Single): Double; cdecl;

  // -- 8 arguments, double class --------------------------------------------
  TFFIProc8D  = procedure(A0, A1, A2, A3, A4, A5, A6, A7: Double); cdecl;
  TFFIFuncN8D = function(A0, A1, A2, A3, A4, A5, A6, A7: Double): PtrInt; cdecl;
  TFFIFuncS8D = function(A0, A1, A2, A3, A4, A5, A6, A7: Double): Single; cdecl;
  TFFIFuncD8D = function(A0, A1, A2, A3, A4, A5, A6, A7: Double): Double; cdecl;

// ==========================================================================
// Dispatch helpers — one per arity to keep the main dispatch readable
// ==========================================================================

procedure Dispatch0(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc0(AFunc)(); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN0(AFunc)();
    frcSingle:  AResult.AsSingle := TFFIFuncS0(AFunc)();
    frcDouble:  AResult.AsDouble := TFFIFuncD0(AFunc)();
  end;
end;

procedure Dispatch1N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc1N(AFunc)(A[0]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN1N(AFunc)(A[0]);
    frcSingle:  AResult.AsSingle := TFFIFuncS1N(AFunc)(A[0]);
    frcDouble:  AResult.AsDouble := TFFIFuncD1N(AFunc)(A[0]);
  end;
end;

procedure Dispatch1S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc1S(AFunc)(A[0]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN1S(AFunc)(A[0]);
    frcSingle:  AResult.AsSingle := TFFIFuncS1S(AFunc)(A[0]);
    frcDouble:  AResult.AsDouble := TFFIFuncD1S(AFunc)(A[0]);
  end;
end;

procedure Dispatch1D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc1D(AFunc)(A[0]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN1D(AFunc)(A[0]);
    frcSingle:  AResult.AsSingle := TFFIFuncS1D(AFunc)(A[0]);
    frcDouble:  AResult.AsDouble := TFFIFuncD1D(AFunc)(A[0]);
  end;
end;

procedure Dispatch2N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc2N(AFunc)(A[0], A[1]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN2N(AFunc)(A[0], A[1]);
    frcSingle:  AResult.AsSingle := TFFIFuncS2N(AFunc)(A[0], A[1]);
    frcDouble:  AResult.AsDouble := TFFIFuncD2N(AFunc)(A[0], A[1]);
  end;
end;

procedure Dispatch2S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc2S(AFunc)(A[0], A[1]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN2S(AFunc)(A[0], A[1]);
    frcSingle:  AResult.AsSingle := TFFIFuncS2S(AFunc)(A[0], A[1]);
    frcDouble:  AResult.AsDouble := TFFIFuncD2S(AFunc)(A[0], A[1]);
  end;
end;

procedure Dispatch2D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc2D(AFunc)(A[0], A[1]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN2D(AFunc)(A[0], A[1]);
    frcSingle:  AResult.AsSingle := TFFIFuncS2D(AFunc)(A[0], A[1]);
    frcDouble:  AResult.AsDouble := TFFIFuncD2D(AFunc)(A[0], A[1]);
  end;
end;

procedure Dispatch3N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc3N(AFunc)(A[0], A[1], A[2]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN3N(AFunc)(A[0], A[1], A[2]);
    frcSingle:  AResult.AsSingle := TFFIFuncS3N(AFunc)(A[0], A[1], A[2]);
    frcDouble:  AResult.AsDouble := TFFIFuncD3N(AFunc)(A[0], A[1], A[2]);
  end;
end;

procedure Dispatch3S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc3S(AFunc)(A[0], A[1], A[2]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN3S(AFunc)(A[0], A[1], A[2]);
    frcSingle:  AResult.AsSingle := TFFIFuncS3S(AFunc)(A[0], A[1], A[2]);
    frcDouble:  AResult.AsDouble := TFFIFuncD3S(AFunc)(A[0], A[1], A[2]);
  end;
end;

procedure Dispatch3D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc3D(AFunc)(A[0], A[1], A[2]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN3D(AFunc)(A[0], A[1], A[2]);
    frcSingle:  AResult.AsSingle := TFFIFuncS3D(AFunc)(A[0], A[1], A[2]);
    frcDouble:  AResult.AsDouble := TFFIFuncD3D(AFunc)(A[0], A[1], A[2]);
  end;
end;

procedure Dispatch4N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc4N(AFunc)(A[0], A[1], A[2], A[3]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN4N(AFunc)(A[0], A[1], A[2], A[3]);
    frcSingle:  AResult.AsSingle := TFFIFuncS4N(AFunc)(A[0], A[1], A[2], A[3]);
    frcDouble:  AResult.AsDouble := TFFIFuncD4N(AFunc)(A[0], A[1], A[2], A[3]);
  end;
end;

procedure Dispatch4S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc4S(AFunc)(A[0], A[1], A[2], A[3]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN4S(AFunc)(A[0], A[1], A[2], A[3]);
    frcSingle:  AResult.AsSingle := TFFIFuncS4S(AFunc)(A[0], A[1], A[2], A[3]);
    frcDouble:  AResult.AsDouble := TFFIFuncD4S(AFunc)(A[0], A[1], A[2], A[3]);
  end;
end;

procedure Dispatch4D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc4D(AFunc)(A[0], A[1], A[2], A[3]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN4D(AFunc)(A[0], A[1], A[2], A[3]);
    frcSingle:  AResult.AsSingle := TFFIFuncS4D(AFunc)(A[0], A[1], A[2], A[3]);
    frcDouble:  AResult.AsDouble := TFFIFuncD4D(AFunc)(A[0], A[1], A[2], A[3]);
  end;
end;

procedure Dispatch5N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc5N(AFunc)(A[0], A[1], A[2], A[3], A[4]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN5N(AFunc)(A[0], A[1], A[2], A[3], A[4]);
    frcSingle:  AResult.AsSingle := TFFIFuncS5N(AFunc)(A[0], A[1], A[2], A[3], A[4]);
    frcDouble:  AResult.AsDouble := TFFIFuncD5N(AFunc)(A[0], A[1], A[2], A[3], A[4]);
  end;
end;

procedure Dispatch5S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc5S(AFunc)(A[0], A[1], A[2], A[3], A[4]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN5S(AFunc)(A[0], A[1], A[2], A[3], A[4]);
    frcSingle:  AResult.AsSingle := TFFIFuncS5S(AFunc)(A[0], A[1], A[2], A[3], A[4]);
    frcDouble:  AResult.AsDouble := TFFIFuncD5S(AFunc)(A[0], A[1], A[2], A[3], A[4]);
  end;
end;

procedure Dispatch5D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc5D(AFunc)(A[0], A[1], A[2], A[3], A[4]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN5D(AFunc)(A[0], A[1], A[2], A[3], A[4]);
    frcSingle:  AResult.AsSingle := TFFIFuncS5D(AFunc)(A[0], A[1], A[2], A[3], A[4]);
    frcDouble:  AResult.AsDouble := TFFIFuncD5D(AFunc)(A[0], A[1], A[2], A[3], A[4]);
  end;
end;

procedure Dispatch6N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc6N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN6N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
    frcSingle:  AResult.AsSingle := TFFIFuncS6N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
    frcDouble:  AResult.AsDouble := TFFIFuncD6N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
  end;
end;

procedure Dispatch6S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc6S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN6S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
    frcSingle:  AResult.AsSingle := TFFIFuncS6S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
    frcDouble:  AResult.AsDouble := TFFIFuncD6S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
  end;
end;

procedure Dispatch6D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc6D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN6D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
    frcSingle:  AResult.AsSingle := TFFIFuncS6D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
    frcDouble:  AResult.AsDouble := TFFIFuncD6D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5]);
  end;
end;

procedure Dispatch7N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc7N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN7N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
    frcSingle:  AResult.AsSingle := TFFIFuncS7N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
    frcDouble:  AResult.AsDouble := TFFIFuncD7N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
  end;
end;

procedure Dispatch7S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc7S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN7S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
    frcSingle:  AResult.AsSingle := TFFIFuncS7S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
    frcDouble:  AResult.AsDouble := TFFIFuncD7S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
  end;
end;

procedure Dispatch7D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc7D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN7D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
    frcSingle:  AResult.AsSingle := TFFIFuncS7D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
    frcDouble:  AResult.AsDouble := TFFIFuncD7D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6]);
  end;
end;

procedure Dispatch8N(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of PtrInt; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc8N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN8N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
    frcSingle:  AResult.AsSingle := TFFIFuncS8N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
    frcDouble:  AResult.AsDouble := TFFIFuncD8N(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
  end;
end;

procedure Dispatch8S(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Single; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc8S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN8S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
    frcSingle:  AResult.AsSingle := TFFIFuncS8S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
    frcDouble:  AResult.AsDouble := TFFIFuncD8S(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
  end;
end;

procedure Dispatch8D(const AFunc: CodePointer; const AReturnClass: TGocciaFFIReturnClass; const A: array of Double; out AResult: TGocciaFFIResult);
begin
  case AReturnClass of
    frcVoid:    begin TFFIProc8D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]); AResult.AsInt := 0; end;
    frcInteger: AResult.AsInt := TFFIFuncN8D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
    frcSingle:  AResult.AsSingle := TFFIFuncS8D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
    frcDouble:  AResult.AsDouble := TFFIFuncD8D(AFunc)(A[0], A[1], A[2], A[3], A[4], A[5], A[6], A[7]);
  end;
end;

// ==========================================================================
// Main dispatch entry point
// ==========================================================================

procedure FFIDispatchCall(
  const AFunc: CodePointer;
  const AArgCount: Integer;
  const AArgClass: TGocciaFFIArgClass;
  const AReturnClass: TGocciaFFIReturnClass;
  const AIntArgs: array of PtrInt;
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

end.
