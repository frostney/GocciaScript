unit Goccia.Environment.Types;

{$I Goccia.inc}

interface

type
  TGocciaGlobalBuiltin = (
    ggConsole,
    ggMath,
    ggGlobalObject,
    ggGlobalArray,
    ggGlobalNumber,
    ggPromise,
    ggJSON,
    ggSymbol,
    ggSet,
    ggMap,
    ggPerformance,
    ggTestAssertions,
    ggBenchmark,
    ggTemporal,
    ggJSX,
    ggArrayBuffer
  );

  TGocciaGlobalBuiltins = set of TGocciaGlobalBuiltin;

const
  GOCCIA_DEFAULT_GLOBALS: TGocciaGlobalBuiltins = [
    ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber,
    ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggPerformance,
    ggTemporal, ggJSX, ggArrayBuffer
  ];

implementation

end.
