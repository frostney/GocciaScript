unit Goccia.Shims;

{$I Goccia.inc}

{ Provides backwards-compatible JavaScript globals as GocciaScript shims.
  Modern APIs (e.g. Uint8Array.fromBase64/toBase64) are implemented natively;
  legacy APIs (e.g. atob/btoa) are shimmed on top of them here.

  Each shim's Source is a valid GocciaScript module with a single named export.
  LoadShimValue evaluates the module in a child scope and returns the exported
  value, which the engine binds into the global scope.

  To add a new shim, append a TGocciaShimDefinition entry to DEFAULT_SHIMS. }

interface

uses
  Classes,

  Goccia.Interpreter,
  Goccia.Values.Primitives;

type
  TGocciaShimDefinition = record
    Name: string;      // Exported name (must match the export in Source)
    FileName: string;  // Virtual filename for error reporting
    Source: string;     // GocciaScript module source with a single named export
  end;

function DefaultShimCount: Integer;
function DefaultShim(const AIndex: Integer): TGocciaShimDefinition;

{ Populate AShims with the names of all default shims.
  Call before RegisterBuiltIns so Goccia.shims reflects the names. }
procedure RegisterDefaultShimNames(const AShims: TStringList);

{ Evaluate a shim module in a child scope and return its exported value.
  Uses the interpreter for evaluation — consistent with how
  InjectGlobalsFromModule already works in both engine modes. }
function LoadShimValue(const AInterpreter: TGocciaInterpreter;
  const AShim: TGocciaShimDefinition): TGocciaValue;

implementation

uses
  Generics.Collections,

  Goccia.AST.Node,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Scope,
  Goccia.Token;

const
  DEFAULT_SHIMS: array[0..7] of TGocciaShimDefinition = (
    ( // WHATWG HTML spec §8.3 — legacy btoa(data) via Uint8Array.toBase64
      Name: 'btoa';
      FileName: '<shim:btoa>';
      Source:
        'export const btoa = (...args: string[]): string => {'#10 +
        '  if (args.length === 0)'#10 +
        '    throw new TypeError('#10 +
        '      "Failed to execute ''btoa'': 1 argument required, but only 0 present."'#10 +
        '    );'#10 +
        '  const str: string = String(args[0]);'#10 +
        '  const codePoints: number[] = Array.from({ length: str.length }, (_: undefined, i: number): number | undefined =>'#10 +
        '    str.codePointAt(i)'#10 +
        '  ).filter((cp: number | undefined): boolean => cp !== undefined);'#10 +
        '  codePoints.forEach((cp: number): void => {'#10 +
        '    if (cp > 0xff)'#10 +
        '      throw new DOMException('#10 +
        '        "Failed to execute ''btoa'': The string to be encoded contains characters outside of the Latin1 range.",'#10 +
        '        "InvalidCharacterError"'#10 +
        '      );'#10 +
        '  });'#10 +
        '  return new Uint8Array(codePoints).toBase64();'#10 +
        '};'
    ),
    ( // WHATWG HTML spec §8.3 — legacy atob(data) via Uint8Array.fromBase64
      Name: 'atob';
      FileName: '<shim:atob>';
      Source:
        'export const atob = (...args: string[]): string => {'#10 +
        '  if (args.length === 0)'#10 +
        '    throw new TypeError('#10 +
        '      "Failed to execute ''atob'': 1 argument required, but only 0 present."'#10 +
        '    );'#10 +
        '  let str: string = String(args[0]);'#10 +
        '  str = str.replace(/[\t\n\f\r ]/g, "");'#10 +
        '  if (str.length > 0 && str.length % 4 === 0) {'#10 +
        '    if (str[str.length - 1] === "=") {'#10 +
        '      str = str.slice(0, -1);'#10 +
        '      if (str.length > 0 && str[str.length - 1] === "=")'#10 +
        '        str = str.slice(0, -1);'#10 +
        '    }'#10 +
        '  }'#10 +
        '  if (str.length % 4 === 1)'#10 +
        '    throw new DOMException('#10 +
        '      "Failed to execute ''atob'': The string to be decoded is not correctly encoded.",'#10 +
        '      "InvalidCharacterError"'#10 +
        '    );'#10 +
        '  [...str].forEach((ch: string): void => {'#10 +
        '    const c: number = ch.charCodeAt(0);'#10 +
        '    if (!(c >= 65 && c <= 90) && !(c >= 97 && c <= 122) &&'#10 +
        '        !(c >= 48 && c <= 57) && c !== 43 && c !== 47)'#10 +
        '      throw new DOMException('#10 +
        '        "Failed to execute ''atob'': The string to be decoded is not correctly encoded.",'#10 +
        '        "InvalidCharacterError"'#10 +
        '      );'#10 +
        '  });'#10 +
        '  const remainder: number = str.length % 4;'#10 +
        '  if (remainder === 2) str += "==";'#10 +
        '  else if (remainder === 3) str += "=";'#10 +
        '  if (str === "") return "";'#10 +
        '  return Uint8Array.fromBase64(str).reduce('#10 +
        '    (acc: string, b: number): string => acc + String.fromCharCode(b),'#10 +
        '    ""'#10 +
        '  );'#10 +
        '};'
    ),
    ( // ES2027 §19.2.5 — legacy parseInt via Number.parseInt
      Name: 'parseInt';
      FileName: '<shim:parseInt>';
      Source: 'export const parseInt = Number.parseInt;'
    ),
    ( // ES2027 §19.2.4 — legacy parseFloat via Number.parseFloat
      Name: 'parseFloat';
      FileName: '<shim:parseFloat>';
      Source: 'export const parseFloat = Number.parseFloat;'
    ),
    ( // ES2027 §19.2.2 — legacy isNaN (coerces argument to number)
      Name: 'isNaN';
      FileName: '<shim:isNaN>';
      Source:
        'export const isNaN = (x) =>' +
        ' Number.isNaN(Number(x));'
    ),
    ( // ES2027 §19.2.3 — legacy isFinite (coerces argument to number)
      Name: 'isFinite';
      FileName: '<shim:isFinite>';
      Source:
        'export const isFinite = (x) =>' +
        ' Number.isFinite(Number(x));'
    ),
    ( // Legacy Date constructor via Temporal
      Name: 'Date';
      FileName: '<shim:Date>';
      Source:
        'export const Date = class Date {'#10 +
        '  #ms;'#10 +
        '  static now(): number { return Temporal.Now.instant().epochMilliseconds; }'#10 +
        '  static parse(str: string): number {'#10 +
        '    return Temporal.Instant.from(String(str)).epochMilliseconds;'#10 +
        '  }'#10 +
        '  constructor(...args: any[]) {'#10 +
        '    if (args.length === 0) {'#10 +
        '      this.#ms = Temporal.Now.instant().epochMilliseconds;'#10 +
        '    } else if (args.length === 1) {'#10 +
        '      const v = args[0];'#10 +
        '      this.#ms = typeof v === "number" ? v : Temporal.Instant.from(String(v)).epochMilliseconds;'#10 +
        '    } else {'#10 +
        '      const y: number = args[0] >= 0 && args[0] <= 99 ? 1900 + args[0] : args[0];'#10 +
        '      const dt = new Temporal.PlainDateTime('#10 +
        '        y, (args[1] ?? 0) + 1, args[2] ?? 1,'#10 +
        '        args[3] ?? 0, args[4] ?? 0, args[5] ?? 0, args[6] ?? 0'#10 +
        '      );'#10 +
        '      this.#ms = dt.toZonedDateTime(Temporal.Now.timeZoneId()).epochMilliseconds;'#10 +
        '    }'#10 +
        '  }'#10 +
        '  #local() { return new Temporal.ZonedDateTime(this.#ms * 1000000, Temporal.Now.timeZoneId()); }'#10 +
        '  #utc() { return new Temporal.ZonedDateTime(this.#ms * 1000000, "UTC"); }'#10 +
        '  getTime(): number { return this.#ms; }'#10 +
        '  valueOf(): number { return this.#ms; }'#10 +
        '  getFullYear(): number { return this.#local().year; }'#10 +
        '  getMonth(): number { return this.#local().month - 1; }'#10 +
        '  getDate(): number { return this.#local().day; }'#10 +
        '  getDay(): number { return this.#local().dayOfWeek % 7; }'#10 +
        '  getHours(): number { return this.#local().hour; }'#10 +
        '  getMinutes(): number { return this.#local().minute; }'#10 +
        '  getSeconds(): number { return this.#local().second; }'#10 +
        '  getMilliseconds(): number { return this.#local().millisecond; }'#10 +
        '  getUTCFullYear(): number { return this.#utc().year; }'#10 +
        '  getUTCMonth(): number { return this.#utc().month - 1; }'#10 +
        '  getUTCDate(): number { return this.#utc().day; }'#10 +
        '  getUTCDay(): number { return this.#utc().dayOfWeek % 7; }'#10 +
        '  getUTCHours(): number { return this.#utc().hour; }'#10 +
        '  getUTCMinutes(): number { return this.#utc().minute; }'#10 +
        '  getUTCSeconds(): number { return this.#utc().second; }'#10 +
        '  getUTCMilliseconds(): number { return this.#utc().millisecond; }'#10 +
        '  getTimezoneOffset(): number { return -(this.#local().offsetNanoseconds / 60000000000); }'#10 +
        '  toISOString(): string { return Temporal.Instant.fromEpochMilliseconds(this.#ms).toString(); }'#10 +
        '  toJSON(): string { return this.toISOString(); }'#10 +
        '  toString(): string {'#10 +
        '    const z = this.#local();'#10 +
        '    const pad = (n: number): string => n < 10 ? "0" + String(n) : String(n);'#10 +
        '    const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];'#10 +
        '    const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];'#10 +
        '    return days[z.dayOfWeek % 7] + " " + months[z.month - 1] + " " + pad(z.day) + " " +'#10 +
        '      String(z.year) + " " + pad(z.hour) + ":" + pad(z.minute) + ":" + pad(z.second) + " GMT" + z.offset;'#10 +
        '  }'#10 +
        '};'
    ),
    ( // ES2026 §20.1.3.2 — legacy hasOwnProperty via Object.hasOwn
      Name: 'hasOwnProperty';
      FileName: '<shim:hasOwnProperty>';
      Source:
        'export const hasOwnProperty = ((proto) => {'#10 +
        '  class _H { hasOwnProperty(prop) { return Object.hasOwn(this, prop); } }'#10 +
        '  const fn = new _H().hasOwnProperty;'#10 +
        '  Object.defineProperty(proto, "hasOwnProperty", {'#10 +
        '    value: fn, writable: true, enumerable: false, configurable: true'#10 +
        '  });'#10 +
        '  return fn;'#10 +
        '})(Object.getPrototypeOf({}));'
    )
  );

function DefaultShimCount: Integer;
begin
  Result := Length(DEFAULT_SHIMS);
end;

function DefaultShim(const AIndex: Integer): TGocciaShimDefinition;
begin
  Result := DEFAULT_SHIMS[AIndex];
end;

procedure RegisterDefaultShimNames(const AShims: TStringList);
var
  I: Integer;
begin
  for I := Low(DEFAULT_SHIMS) to High(DEFAULT_SHIMS) do
    AShims.Add(DEFAULT_SHIMS[I].Name);
end;

function LoadShimValue(const AInterpreter: TGocciaInterpreter;
  const AShim: TGocciaShimDefinition): TGocciaValue;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  ModuleScope: TGocciaScope;
  Context: TGocciaEvaluationContext;
  I: Integer;
begin
  Lexer := TGocciaLexer.Create(AShim.Source, AShim.FileName);
  try
    Parser := TGocciaParser.Create(Lexer.ScanTokens, AShim.FileName,
      Lexer.SourceLines);
    try
      ProgramNode := Parser.Parse;
      try
        ModuleScope := AInterpreter.GlobalScope.CreateChild(skModule,
          'Shim:' + AShim.Name);
        Context := AInterpreter.CreateEvaluationContext;
        Context.Scope := ModuleScope;
        for I := 0 to ProgramNode.Body.Count - 1 do
          EvaluateStatement(ProgramNode.Body[I], Context);
        Result := ModuleScope.GetValue(AShim.Name);
      finally
        ProgramNode.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

end.
