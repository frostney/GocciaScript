#!/usr/bin/env python3
"""Exercise format.pas on sample units in a disposable directory.

Run on a host with FPC: python3 scripts/test-format.py
"""
from pathlib import Path
import shutil
import subprocess
import tempfile

REPO = Path(__file__).resolve().parents[1]
FPC = shutil.which("fpc")
if not FPC:
    raise SystemExit("fpc is required")

UNSORTED = """unit Sample;

interface

uses
  SysUtils,
  Zeta in 'lib//z{x}.pas',
  Goccia.Values,
  Classes,
  TextSemantics;

implementation

end.
"""

SORTED = """unit Sample;

interface

uses
  Classes,
  SysUtils,

  TextSemantics,

  Goccia.Values,

  Zeta in 'lib//z{x}.pas';

implementation

end.
"""

# A comment opened on the uses line closes after the clause's first line; the
# clause is kept, and the later clause is still sorted.
OPEN_COMMENT = """unit Sample;

interface

uses { the units
  this file needs }
  SysUtils,
  Classes;

implementation

uses
  SysUtils,
  Classes;

end.
"""

OPEN_COMMENT_SORTED = OPEN_COMMENT.replace(
    "implementation\n\nuses\n  SysUtils,\n  Classes;",
    "implementation\n\nuses\n  Classes,\n  SysUtils;")

# A comment before the closing semicolon does not hide it, so the later clause
# is still sorted.
COMMENT_BEFORE_SEMICOLON = """unit Sample;

interface

uses
  Classes,
  SysUtils { a note };

implementation

uses
  SysUtils,
  Classes;

end.
"""

COMMENT_BEFORE_SEMICOLON_SORTED = COMMENT_BEFORE_SEMICOLON.replace(
    "implementation\n\nuses\n  SysUtils,\n  Classes;",
    "implementation\n\nuses\n  Classes,\n  SysUtils;")

# A (* *) comment opened after the clause is still a comment until its *),
# whatever it holds, so a uses line inside it is not formatted and the later
# clause is still sorted.
TRAILING_PAREN_COMMENT = """unit Sample;

interface

uses
  Classes,
  SysUtils; (* an older version, with a } brace, read
something first, then
uses
  Zeta, Alpha;
*)

implementation

uses
  SysUtils,
  Classes;

end.
"""

TRAILING_PAREN_COMMENT_SORTED = TRAILING_PAREN_COMMENT.replace(
    "implementation\n\nuses\n  SysUtils,\n  Classes;",
    "implementation\n\nuses\n  Classes,\n  SysUtils;")

# A comment between clauses is tracked across lines, so commented-out code
# holding a uses line is left alone and the later clause is still sorted.
COMMENTED_OUT_CODE = """unit Sample;

interface

uses
  Classes,
  SysUtils;

{ Disabled until the port lands:

uses
  Zeta, Alpha;
}

implementation

uses
  SysUtils,
  Classes;

end.
"""

COMMENTED_OUT_CODE_SORTED = COMMENTED_OUT_CODE.replace(
    "implementation\n\nuses\n  SysUtils,\n  Classes;",
    "implementation\n\nuses\n  Classes,\n  SysUtils;")

COMMENTED = {
    "line comment": """  Classes,
  // A comment between units, with a comma, and a semicolon;
  SysUtils,""",
    "block comment": """  Classes,
  { A comment between units, with a comma, and a semicolon; }
  SysUtils,""",
    "parenthesised comment": """  Classes,
  (* A comment between units, with a comma, and a semicolon; *)
  SysUtils,""",
    "directive": """  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes,
  SysUtils,""",
    "multi-line block comment": """  Classes,
  { A comment that spans lines, with a comma,
    and a semicolon; }
  SysUtils,""",
}

COMMENTED_UNIT = """unit Sample;

interface

uses
{units}
  Goccia.Values,
  TextSemantics;

type
  TSample = class
  public
    procedure Run(const AValue: Integer);
  end;

implementation

procedure TSample.Run(const AValue: Integer);
begin
  WriteLn(AValue, ', done');
end;

end.
"""

with tempfile.TemporaryDirectory(prefix="goccia-format-check-") as directory:
    root = Path(directory)
    tool_source = root / "format-tool.pas"
    tool_source.write_text((REPO / "format.pas").read_text().replace("#!/", "//#!/", 1))
    subprocess.run([FPC, "-Fu./source/shared", f"-FU{root}", f"-FE{root}",
                    str(tool_source)], cwd=REPO, check=True, capture_output=True)
    tool = root / "format-tool"

    def format_text(text):
        sample = root / "Sample.pas"
        sample.write_text(text)
        result = subprocess.run([str(tool), str(sample)], cwd=root,
                                capture_output=True, text=True, timeout=60)
        assert result.returncode == 0, result.stdout + result.stderr
        return sample.read_text()

    assert format_text(UNSORTED) == SORTED, format_text(UNSORTED)

    # A comment can belong to the unit beside it, so a uses clause holding one
    # is left as written, as one holding a compiler directive is.
    for name, units in COMMENTED.items():
        text = COMMENTED_UNIT.replace("{units}", units)
        assert format_text(text) == text, (name, format_text(text))

    assert format_text(OPEN_COMMENT) == OPEN_COMMENT_SORTED, format_text(OPEN_COMMENT)
    assert format_text(COMMENT_BEFORE_SEMICOLON) == COMMENT_BEFORE_SEMICOLON_SORTED, (
        format_text(COMMENT_BEFORE_SEMICOLON))
    assert format_text(TRAILING_PAREN_COMMENT) == TRAILING_PAREN_COMMENT_SORTED, (
        format_text(TRAILING_PAREN_COMMENT))
    assert format_text(COMMENTED_OUT_CODE) == COMMENTED_OUT_CODE_SORTED, (
        format_text(COMMENTED_OUT_CODE))

print("format.pas behaves as expected")
