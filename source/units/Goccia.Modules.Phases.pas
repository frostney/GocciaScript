unit Goccia.Modules.Phases;

{$I Goccia.inc}

interface

type
  // Shared by parsed requests, bytecode link plans and runtime records.
  // The ordinal values are part of the bytecode encoding.
  TGocciaImportCallPhase = (icpEvaluation, icpSource, icpDefer);

implementation

end.
