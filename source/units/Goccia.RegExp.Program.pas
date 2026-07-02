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

  TRegExpStringSequence = record
    CodePoints: array of Cardinal;
  end;

  TRegExpStringSequenceArray = array of TRegExpStringSequence;

  TRegExpCharRange = record
    Lo: Cardinal;
    Hi: Cardinal;
  end;

  TRegExpCharRangeArray = array of TRegExpCharRange;

  TRegExpCharClass = record
    Ranges: TRegExpCharRangeArray;
    PageFirstRange: array of Integer;
  end;

  TRegExpStringSet = record
    CharClassIndex: Integer;
    Strings: TRegExpStringSequenceArray;
  end;

  TRegExpStringSetArray = array of TRegExpStringSet;

const
  REGEXP_CHAR_CLASS_PAGE_BITS = 8;
  REGEXP_CHAR_CLASS_PAGE_COUNT =
    ($10FFFF shr REGEXP_CHAR_CLASS_PAGE_BITS) + 1;

type
  TRegExpStartCheck = record
    Enabled: Boolean;
    HasNonLatin1: Boolean;
    Latin1Bits: array[0..7] of UInt32;
  end;

  TRegExpProgram = record
    Code: array of UInt32;
    CharClasses: array of TRegExpCharClass;
    CaptureCount: Integer;
    FullUnicode: Boolean;
    StartCheck: TRegExpStartCheck;
    NamedGroups: TGocciaRegExpNamedGroups;
    StringSets: TRegExpStringSetArray;
  end;

implementation

end.
