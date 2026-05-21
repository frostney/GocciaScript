unit Goccia.RegExp.&Program;

{$I Goccia.inc}

interface

type
  TGocciaRegExpNamedGroup = record
    Name: string;
    Index: Integer;
    DisjunctionPath: array of Integer;
  end;

  TGocciaRegExpNamedGroups = array of TGocciaRegExpNamedGroup;

  TRegExpCharRange = record
    Lo: Cardinal;
    Hi: Cardinal;
  end;

  TRegExpCharRangeArray = array of TRegExpCharRange;

  TRegExpCharClass = record
    Ranges: TRegExpCharRangeArray;
  end;

  TRegExpProgram = record
    Code: array of UInt32;
    CharClasses: array of TRegExpCharClass;
    CaptureCount: Integer;
    FullUnicode: Boolean;
    NamedGroups: TGocciaRegExpNamedGroups;
  end;

implementation

end.
