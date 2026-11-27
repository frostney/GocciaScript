unit Goccia.RegExp.&Program;

{$I Goccia.inc}

interface

type
  TRegExpCodeArray = array of UInt32;

  TGocciaRegExpNamedGroup = record
    Name: string;
    Index: Integer;
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

  TRegExpCharClassArray = array of TRegExpCharClass;

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
    Code: TRegExpCodeArray;
    CharClasses: TRegExpCharClassArray;
    CaptureCount: Integer;
    FullUnicode: Boolean;
    StartCheck: TRegExpStartCheck;
    NamedGroups: TGocciaRegExpNamedGroups;
    StringSets: TRegExpStringSetArray;
    // The program reads a capture with RX_BACKREF, so whether a path fails at
    // an instruction and position also depends on the captures it carries.
    HasBackreferences: Boolean;
  end;

function RegExpProgramBytes(const AProgram: TRegExpProgram): Int64;

implementation

function RegExpProgramBytes(const AProgram: TRegExpProgram): Int64;
var
  CharacterClass: TRegExpCharClass;
  Group: TGocciaRegExpNamedGroup;
  StringSet: TRegExpStringSet;
  Sequence: TRegExpStringSequence;
begin
  Result := Int64(Length(AProgram.Code)) * SizeOf(UInt32) +
    Int64(Length(AProgram.CharClasses)) * SizeOf(TRegExpCharClass) +
    Int64(Length(AProgram.NamedGroups)) * SizeOf(TGocciaRegExpNamedGroup) +
    Int64(Length(AProgram.StringSets)) * SizeOf(TRegExpStringSet);
  for CharacterClass in AProgram.CharClasses do
    Inc(Result, Int64(Length(CharacterClass.Ranges)) * SizeOf(TRegExpCharRange) +
      Int64(Length(CharacterClass.PageFirstRange)) * SizeOf(Integer));
  for Group in AProgram.NamedGroups do
    Inc(Result, Int64(Length(Group.Name)) * SizeOf(Char));
  for StringSet in AProgram.StringSets do
  begin
    Inc(Result, Int64(Length(StringSet.Strings)) * SizeOf(TRegExpStringSequence));
    for Sequence in StringSet.Strings do
      Inc(Result, Int64(Length(Sequence.CodePoints)) * SizeOf(Cardinal));
  end;
end;

end.
