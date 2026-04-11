unit Goccia.URL.Parser;

// WHATWG URL Standard §4 URLs
// https://url.spec.whatwg.org/

{$I Goccia.inc}

interface

const
  // Sentinel value meaning the port component is null
  URL_NULL_PORT = -1;

type
  // WHATWG URL Standard §4.1 URL representation
  // The path is stored as segments for normal URLs, or as an opaque string
  TGocciaURLPathSegments = array of string;

  TGocciaURLRecord = record
    Scheme: string;                      // e.g. 'https' — lowercased, no ':'
    Username: string;                    // percent-encoded
    Password: string;                    // percent-encoded
    Host: string;                        // serialized host: domain, IP, empty string
    HasNullHost: Boolean;               // true = host is null (no authority component)
    Port: Integer;                       // URL_NULL_PORT = null
    PathSegments: TGocciaURLPathSegments;// path segments for non-opaque paths
    HasOpaquePath: Boolean;              // true for non-special no-authority URLs
    OpaquePath: string;                  // opaque path when HasOpaquePath is true
    Query: string;                       // without leading '?', '' means empty query
    HasNullQuery: Boolean;              // true = query is null
    Fragment: string;                    // without leading '#', '' means empty fragment
    HasNullFragment: Boolean;           // true = fragment is null
    IsValid: Boolean;                    // false if parsing failed
  end;

// WHATWG URL Standard §4.4 URL parsing
function ParseURL(const AInput: string): TGocciaURLRecord; overload;
function ParseURLWithBase(const AInput: string;
  const ABase: TGocciaURLRecord): TGocciaURLRecord;

// WHATWG URL Standard §4.5 URL serializing
function SerializeURL(const AURL: TGocciaURLRecord;
  const AExcludeFragment: Boolean = False): string;
function SerializePath(const AURL: TGocciaURLRecord): string;

// WHATWG URL Standard §4.6 URL origin
function SerializeOrigin(const AURL: TGocciaURLRecord): string;

// Scheme utilities
function IsSpecialScheme(const AScheme: string): Boolean;
function DefaultPortForScheme(const AScheme: string): Integer;
function URLHasCredentials(const AURL: TGocciaURLRecord): Boolean;

// Percent-encode/decode
function PercentEncodeForUserinfo(const AInput: string): string;
function PercentEncodeForPath(const AInput: string): string;
function PercentEncodeForQuery(const AInput: string;
  const AIsSpecial: Boolean): string;
function PercentEncodeForFragment(const AInput: string): string;
function PercentDecodeString(const AInput: string): string;

// application/x-www-form-urlencoded serialization
function SerializeFormEncoded(const AName, AValue: string): string;
function ParseFormEncoded(const AInput: string;
  out ANames: TArray<string>; out AValues: TArray<string>): Boolean;

implementation

uses
  SysUtils;

// ---------------------------------------------------------------------------
// Character helpers
// ---------------------------------------------------------------------------

function IsASCIIAlpha(const C: Char): Boolean; inline;
begin
  Result := (C in ['A'..'Z', 'a'..'z']);
end;

function IsASCIIDigit(const C: Char): Boolean; inline;
begin
  Result := (C in ['0'..'9']);
end;

function IsASCIIAlphanumeric(const C: Char): Boolean; inline;
begin
  Result := (C in ['A'..'Z', 'a'..'z', '0'..'9']);
end;

function IsASCIIHexDigit(const C: Char): Boolean; inline;
begin
  Result := (C in ['0'..'9', 'A'..'F', 'a'..'f']);
end;

function IsASCIIWhitespace(const C: Char): Boolean; inline;
begin
  // ASCII tab, LF, CR, space
  Result := (C = #9) or (C = #10) or (C = #13) or (C = ' ');
end;

function ASCIILower(const C: Char): Char; inline;
begin
  if C in ['A'..'Z'] then
    Result := Chr(Ord(C) + 32)
  else
    Result := C;
end;

function ASCIILowerString(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
    if Result[I] in ['A'..'Z'] then
      Result[I] := Chr(Ord(Result[I]) + 32);
end;

function HexVal(const C: Char): Byte; inline;
begin
  case C of
    '0'..'9': Result := Ord(C) - Ord('0');
    'A'..'F': Result := Ord(C) - Ord('A') + 10;
    'a'..'f': Result := Ord(C) - Ord('a') + 10;
  else
    Result := 0;
  end;
end;

// ---------------------------------------------------------------------------
// WHATWG URL Standard §1.3 Percent-encode/decode
// ---------------------------------------------------------------------------

const
  HEX_DIGITS = '0123456789ABCDEF';

// §1.3.1 percent-encode after encoding
function PercentEncode(const AByte: Byte): string; inline;
begin
  Result := '%' + HEX_DIGITS[(AByte shr 4) + 1] + HEX_DIGITS[(AByte and $0F) + 1];
end;

// §1.3.2 percent-decode
function PercentDecodeString(const AInput: string): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  I := 1;
  while I <= Length(AInput) do
  begin
    if (AInput[I] = '%') and (I + 2 <= Length(AInput)) and
       IsASCIIHexDigit(AInput[I + 1]) and IsASCIIHexDigit(AInput[I + 2]) then
    begin
      B := (HexVal(AInput[I + 1]) shl 4) or HexVal(AInput[I + 2]);
      Result := Result + Chr(B);
      Inc(I, 3);
    end
    else
    begin
      Result := Result + AInput[I];
      Inc(I);
    end;
  end;
end;

// WHATWG URL Standard §4.1 Percent-encode sets

// C0 percent-encode set: U+0000–U+001F and > U+007E
function InC0PercentEncodeSet(const B: Byte): Boolean; inline;
begin
  Result := (B < $21) or (B > $7E);
end;

// Fragment percent-encode set: C0 + {space U+0020, " U+0022, < U+003C, > U+003E, ` U+0060}
function InFragmentPercentEncodeSet(const B: Byte): Boolean; inline;
begin
  Result := InC0PercentEncodeSet(B) or
            (B = $20) or (B = $22) or (B = $3C) or (B = $3E) or (B = $60);
end;

// Query percent-encode set: C0 + {space, ", #, <, >}
function InQueryPercentEncodeSet(const B: Byte): Boolean; inline;
begin
  Result := InC0PercentEncodeSet(B) or
            (B = $20) or (B = $22) or (B = $23) or (B = $3C) or (B = $3E);
end;

// Special-query percent-encode set: query + {'}
function InSpecialQueryPercentEncodeSet(const B: Byte): Boolean; inline;
begin
  Result := InQueryPercentEncodeSet(B) or (B = $27);
end;

// Path percent-encode set: query + {?, `, {, }}
function InPathPercentEncodeSet(const B: Byte): Boolean; inline;
begin
  Result := InQueryPercentEncodeSet(B) or
            (B = $3F) or (B = $60) or (B = $7B) or (B = $7D);
end;

// Userinfo percent-encode set: path + {/, :, ;, =, @, [, \, ], ^, |}
function InUserinfoPercentEncodeSet(const B: Byte): Boolean; inline;
begin
  Result := InPathPercentEncodeSet(B) or
            (B = $2F) or (B = $3A) or (B = $3B) or (B = $3D) or
            (B = $40) or (B = $5B) or (B = $5C) or (B = $5D) or
            (B = $5E) or (B = $7C);
end;

// application/x-www-form-urlencoded percent-encode set (for form data)
function InFormPercentEncodeSet(const B: Byte): Boolean; inline;
begin
  // Everything except: * - . 0-9 A-Z _ a-z
  Result := not (
    (B = $2A) or (B = $2D) or (B = $2E) or
    (B >= $30) and (B <= $39) or
    (B >= $41) and (B <= $5A) or
    (B = $5F) or
    (B >= $61) and (B <= $7A)
  );
end;

function PercentEncodeForUserinfo(const AInput: string): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 1 to Length(AInput) do
  begin
    B := Ord(AInput[I]);
    if (B > $7F) or InUserinfoPercentEncodeSet(B) then
      Result := Result + PercentEncode(B)
    else
      Result := Result + AInput[I];
  end;
end;

function PercentEncodeForPath(const AInput: string): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 1 to Length(AInput) do
  begin
    B := Ord(AInput[I]);
    if (B > $7F) or InPathPercentEncodeSet(B) then
      Result := Result + PercentEncode(B)
    else
      Result := Result + AInput[I];
  end;
end;

function PercentEncodeForQuery(const AInput: string;
  const AIsSpecial: Boolean): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 1 to Length(AInput) do
  begin
    B := Ord(AInput[I]);
    if AIsSpecial then
    begin
      if (B > $7F) or InSpecialQueryPercentEncodeSet(B) then
        Result := Result + PercentEncode(B)
      else
        Result := Result + AInput[I];
    end
    else
    begin
      if (B > $7F) or InQueryPercentEncodeSet(B) then
        Result := Result + PercentEncode(B)
      else
        Result := Result + AInput[I];
    end;
  end;
end;

function PercentEncodeForFragment(const AInput: string): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 1 to Length(AInput) do
  begin
    B := Ord(AInput[I]);
    if (B > $7F) or InFragmentPercentEncodeSet(B) then
      Result := Result + PercentEncode(B)
    else
      Result := Result + AInput[I];
  end;
end;

// application/x-www-form-urlencoded encoding
function EncodeFormComponent(const AInput: string): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 1 to Length(AInput) do
  begin
    B := Ord(AInput[I]);
    if B = $20 then
      Result := Result + '+'         // space -> +
    else if (B > $7F) or InFormPercentEncodeSet(B) then
      Result := Result + PercentEncode(B)
    else
      Result := Result + AInput[I];
  end;
end;

function DecodeFormComponent(const AInput: string): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  I := 1;
  while I <= Length(AInput) do
  begin
    if AInput[I] = '+' then
    begin
      Result := Result + ' ';
      Inc(I);
    end
    else if (AInput[I] = '%') and (I + 2 <= Length(AInput)) and
            IsASCIIHexDigit(AInput[I + 1]) and IsASCIIHexDigit(AInput[I + 2]) then
    begin
      B := (HexVal(AInput[I + 1]) shl 4) or HexVal(AInput[I + 2]);
      Result := Result + Chr(B);
      Inc(I, 3);
    end
    else
    begin
      Result := Result + AInput[I];
      Inc(I);
    end;
  end;
end;

// WHATWG URL Standard §5.1 application/x-www-form-urlencoded serializing
function SerializeFormEncoded(const AName, AValue: string): string;
begin
  Result := EncodeFormComponent(AName) + '=' + EncodeFormComponent(AValue);
end;

// WHATWG URL Standard §5.2 application/x-www-form-urlencoded parsing
function ParseFormEncoded(const AInput: string;
  out ANames: TArray<string>; out AValues: TArray<string>): Boolean;
var
  Sequences, Pair: TArray<string>;
  I, EqPos: Integer;
  S, Name, Value: string;
begin
  SetLength(ANames, 0);
  SetLength(AValues, 0);

  // Split by '&'
  if AInput = '' then
  begin
    Result := True;
    Exit;
  end;

  // Split on '&' separator
  SetLength(Sequences, 0);
  S := '';
  for I := 1 to Length(AInput) do
  begin
    if AInput[I] = '&' then
    begin
      SetLength(Sequences, Length(Sequences) + 1);
      Sequences[High(Sequences)] := S;
      S := '';
    end
    else
      S := S + AInput[I];
  end;
  SetLength(Sequences, Length(Sequences) + 1);
  Sequences[High(Sequences)] := S;

  for I := 0 to High(Sequences) do
  begin
    S := Sequences[I];
    if S = '' then Continue;
    // Split on first '='
    EqPos := Pos('=', S);
    if EqPos > 0 then
    begin
      Name := Copy(S, 1, EqPos - 1);
      Value := Copy(S, EqPos + 1, Length(S) - EqPos);
    end
    else
    begin
      Name := S;
      Value := '';
    end;
    Name := DecodeFormComponent(Name);
    Value := DecodeFormComponent(Value);
    SetLength(ANames, Length(ANames) + 1);
    SetLength(AValues, Length(AValues) + 1);
    ANames[High(ANames)] := Name;
    AValues[High(AValues)] := Value;
  end;
  Result := True;
end;

// ---------------------------------------------------------------------------
// WHATWG URL Standard §3 Hosts
// ---------------------------------------------------------------------------

// Check if a string is a valid Windows drive letter (e.g. "C:" or "C|")
function IsWindowsDriveLetter(const S: string): Boolean;
begin
  Result := (Length(S) = 2) and IsASCIIAlpha(S[1]) and ((S[2] = ':') or (S[2] = '|'));
end;

// Check if a string is a normalized Windows drive letter (e.g. "C:")
function IsNormalizedWindowsDriveLetter(const S: string): Boolean;
begin
  Result := (Length(S) = 2) and IsASCIIAlpha(S[1]) and (S[2] = ':');
end;

// Normalize a Windows drive letter: replace '|' with ':'
function NormalizeWindowsDriveLetter(const S: string): string;
begin
  Result := S;
  if (Length(Result) = 2) and (Result[2] = '|') then
    Result[2] := ':';
end;

// WHATWG §3.5 host parsing (simplified: no IDNA/Punycode)
// Returns false if the host is invalid
function ParseHost(const AInput: string; const AIsSpecial: Boolean;
  out AHost: string): Boolean;
var
  I: Integer;
  S: string;
begin
  AHost := '';
  if AInput = '' then
  begin
    Result := not AIsSpecial; // empty host allowed for non-special only
    Exit;
  end;

  // IPv6 address
  if (Length(AInput) > 0) and (AInput[1] = '[') then
  begin
    if (Length(AInput) < 2) or (AInput[Length(AInput)] <> ']') then
    begin
      Result := False;
      Exit;
    end;
    // For now, keep IPv6 as-is (simplified)
    AHost := AInput;
    Result := True;
    Exit;
  end;

  // Opaque host (for non-special schemes): just validate no forbidden chars
  if not AIsSpecial then
  begin
    for I := 1 to Length(AInput) do
    begin
      if AInput[I] in [#0, ' ', '#', '/', ':', '<', '>', '?', '@', '[', '\', ']', '^', '|'] then
      begin
        Result := False;
        Exit;
      end;
    end;
    // Percent-decode and keep
    AHost := AInput;
    Result := True;
    Exit;
  end;

  // Domain: percent-decode and lowercase
  S := PercentDecodeString(AInput);
  AHost := ASCIILowerString(S);

  // Validate: no forbidden host code points
  for I := 1 to Length(AHost) do
  begin
    if AHost[I] in [#0, #9, #10, #13, ' ', '#', '/', '<', '>', '?', '@', '[', '\', ']', '^', '|'] then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := AHost <> '';
end;

// ---------------------------------------------------------------------------
// WHATWG URL Standard §4 special schemes
// ---------------------------------------------------------------------------

// WHATWG URL §4.1 special scheme list
function IsSpecialScheme(const AScheme: string): Boolean;
begin
  Result := (AScheme = 'ftp') or (AScheme = 'file') or
            (AScheme = 'http') or (AScheme = 'https') or
            (AScheme = 'ws') or (AScheme = 'wss');
end;

// WHATWG URL §4.1 default port
function DefaultPortForScheme(const AScheme: string): Integer;
begin
  if AScheme = 'ftp' then Result := 21
  else if (AScheme = 'http') or (AScheme = 'ws') then Result := 80
  else if (AScheme = 'https') or (AScheme = 'wss') then Result := 443
  else Result := URL_NULL_PORT;
end;

function URLHasCredentials(const AURL: TGocciaURLRecord): Boolean;
begin
  Result := (AURL.Username <> '') or (AURL.Password <> '');
end;

// ---------------------------------------------------------------------------
// WHATWG URL Standard §4.5 URL serializing
// ---------------------------------------------------------------------------

// WHATWG URL §4.5.1 URL path serializing
function SerializePath(const AURL: TGocciaURLRecord): string;
var
  I: Integer;
begin
  if AURL.HasOpaquePath then
  begin
    Result := AURL.OpaquePath;
    Exit;
  end;
  Result := '';
  for I := 0 to High(AURL.PathSegments) do
    Result := Result + '/' + AURL.PathSegments[I];
  if Length(AURL.PathSegments) = 0 then
    Result := '';
end;

// WHATWG URL §4.5 URL serializer
function SerializeURL(const AURL: TGocciaURLRecord;
  const AExcludeFragment: Boolean): string;
begin
  // Step 1: output = url.scheme + ':'
  Result := AURL.Scheme + ':';

  // Step 2: if host is not null
  if not AURL.HasNullHost then
  begin
    Result := Result + '//';
    // If url includes credentials
    if URLHasCredentials(AURL) then
    begin
      Result := Result + AURL.Username;
      if AURL.Password <> '' then
        Result := Result + ':' + AURL.Password;
      Result := Result + '@';
    end;
    // Host
    Result := Result + AURL.Host;
    // Port
    if AURL.Port <> URL_NULL_PORT then
      Result := Result + ':' + IntToStr(AURL.Port);
  end
  // Step 3: file with null host → '//'
  else if AURL.Scheme = 'file' then
    Result := Result + '//';

  // Step 4/5: path
  if AURL.HasOpaquePath then
    Result := Result + AURL.OpaquePath
  else
  begin
    // If url.host is null, path has more than one segment, first is ''
    if AURL.HasNullHost and (Length(AURL.PathSegments) > 1) and
       (AURL.PathSegments[0] = '') then
      Result := Result + '/.';
    Result := Result + SerializePath(AURL);
  end;

  // Step 6: query
  if not AURL.HasNullQuery then
    Result := Result + '?' + AURL.Query;

  // Step 7: fragment
  if not AExcludeFragment and not AURL.HasNullFragment then
    Result := Result + '#' + AURL.Fragment;
end;

// WHATWG URL §4.6 URL origin
function SerializeOrigin(const AURL: TGocciaURLRecord): string;
begin
  // Tuple origins for special schemes (except file)
  if (AURL.Scheme = 'http') or (AURL.Scheme = 'https') or
     (AURL.Scheme = 'ftp') or (AURL.Scheme = 'ws') or (AURL.Scheme = 'wss') then
  begin
    Result := AURL.Scheme + '://' + AURL.Host;
    if AURL.Port <> URL_NULL_PORT then
      Result := Result + ':' + IntToStr(AURL.Port);
    Exit;
  end;
  // Opaque origin
  Result := 'null';
end;

// ---------------------------------------------------------------------------
// WHATWG URL Standard §4.4 URL parsing — state machine
// ---------------------------------------------------------------------------

type
  TURLParserState = (
    upsSchemeStart, upsScheme, upsNoScheme, upsSpecialRelativeOrAuthority,
    upsPathOrAuthority, upsRelative, upsRelativeSlash, upsSpecialAuthoritySlashes,
    upsSpecialAuthorityIgnoreSlashes, upsAuthority, upsHost, upsHostname,
    upsPort, upsFile, upsFileSlash, upsFileHost, upsPathStart, upsPath,
    upsOpaquePath, upsQuery, upsFragment, upsDone, upsFailure
  );

// Shorten a URL's path: WHATWG §4.5.4 shorten a URL's path
procedure ShortenPath(const AScheme: string; var APath: TGocciaURLPathSegments);
var
  N: Integer;
begin
  N := Length(APath);
  if N = 0 then Exit;

  // For 'file' with a normalized Windows drive letter at path[0], keep it
  if (AScheme = 'file') and (N = 1) and IsNormalizedWindowsDriveLetter(APath[0]) then
    Exit;

  // Remove last path segment
  SetLength(APath, N - 1);
end;

// Append a string to the path segments
procedure AppendPathSegment(var APath: TGocciaURLPathSegments;
  const ASegment: string);
var
  N: Integer;
begin
  N := Length(APath);
  SetLength(APath, N + 1);
  APath[N] := ASegment;
end;

// Check if a string starts with a Windows drive letter followed by / \ ? #
function StartsWithWindowsDriveLetter(const S: string): Boolean;
begin
  Result := (Length(S) >= 2) and IsASCIIAlpha(S[1]) and
            ((S[2] = ':') or (S[2] = '|')) and
            ((Length(S) = 2) or (S[3] in ['/', '\', '?', '#']));
end;

const
  // Sentinel for EOF
  EOF_CHAR = #0;

function ParseURLInternal(const AInput: string; const AHasBase: Boolean;
  const ABase: TGocciaURLRecord; const AStateOverride: TURLParserState;
  const AHasStateOverride: Boolean): TGocciaURLRecord;
var
  Input: string;
  URL: TGocciaURLRecord;
  State: TURLParserState;
  I, Len: Integer;
  C: Char;
  Buffer: string;
  AtSignSeen, InsideBrackets, PasswordTokenSeen: Boolean;
  IsSpecial: Boolean;
  PortNum: Integer;
  Host: string;
  Segment: string;
  J: Integer;

  function Peek(const AOffset: Integer): Char;
  begin
    if (I + AOffset >= 1) and (I + AOffset <= Len) then
      Result := Input[I + AOffset]
    else
      Result := EOF_CHAR;
  end;

  function CurrentChar: Char;
  begin
    if (I >= 1) and (I <= Len) then
      Result := Input[I]
    else
      Result := EOF_CHAR;
  end;

  function RemainingStartsWith(const S: string): Boolean;
  var
    K: Integer;
  begin
    Result := False;
    if I + Length(S) - 1 > Len then Exit;
    for K := 1 to Length(S) do
      if Input[I + K - 1] <> S[K] then Exit;
    Result := True;
  end;

begin
  // Initialize the URL record
  FillChar(URL, SizeOf(URL), 0);
  URL.HasNullHost := True;
  URL.HasNullQuery := True;
  URL.HasNullFragment := True;
  URL.Port := URL_NULL_PORT;

  // §4.4 step 1: strip leading/trailing ASCII whitespace
  Input := AInput;
  while (Length(Input) > 0) and IsASCIIWhitespace(Input[1]) do
    Delete(Input, 1, 1);
  while (Length(Input) > 0) and IsASCIIWhitespace(Input[Length(Input)]) do
    SetLength(Input, Length(Input) - 1);

  // §4.4 step 2: remove ASCII tabs and newlines
  J := 1;
  while J <= Length(Input) do
  begin
    if Input[J] in [#9, #10, #13] then
      Delete(Input, J, 1)
    else
      Inc(J);
  end;

  Len := Length(Input);

  State := AStateOverride;
  if not AHasStateOverride then
    State := upsSchemeStart;

  Buffer := '';
  AtSignSeen := False;
  InsideBrackets := False;
  PasswordTokenSeen := False;
  I := 1;
  IsSpecial := False;

  // Main state machine loop
  while State <> upsDone do
  begin
    C := CurrentChar;
    // EOF is represented as a sentinel EOF_CHAR when I > Len

    case State of
      // §4.4.1 scheme start state
      upsSchemeStart:
      begin
        if IsASCIIAlpha(C) then
        begin
          Buffer := Buffer + ASCIILower(C);
          State := upsScheme;
          Inc(I);
        end
        else if not AHasStateOverride then
        begin
          State := upsNoScheme;
          // Do not advance pointer
        end
        else
        begin
          State := upsFailure;
        end;
      end;

      // §4.4.2 scheme state
      upsScheme:
      begin
        if IsASCIIAlphanumeric(C) or (C = '+') or (C = '-') or (C = '.') then
        begin
          Buffer := Buffer + ASCIILower(C);
          Inc(I);
        end
        else if C = ':' then
        begin
          URL.Scheme := Buffer;
          Buffer := '';
          IsSpecial := IsSpecialScheme(URL.Scheme);

          if AHasStateOverride then
          begin
            // State override: just update scheme and return
            // (simplified — skip full state override logic for setters)
            State := upsDone;
          end
          else if URL.Scheme = 'file' then
          begin
            State := upsFile;
            Inc(I);
          end
          else if IsSpecial and AHasBase and (ABase.Scheme = URL.Scheme) then
          begin
            State := upsSpecialRelativeOrAuthority;
            Inc(I);
          end
          else if IsSpecial then
          begin
            State := upsSpecialAuthoritySlashes;
            Inc(I);
          end
          else if (I < Len) and (Input[I + 1] = '/') then
          begin
            State := upsPathOrAuthority;
            Inc(I, 2);
          end
          else
          begin
            URL.HasOpaquePath := True;
            State := upsOpaquePath;
            Inc(I);
          end;
        end
        else if not AHasStateOverride then
        begin
          // No colon found — rewind and treat as no scheme
          Buffer := '';
          URL.Scheme := '';
          State := upsNoScheme;
          I := 1;
        end
        else
        begin
          State := upsFailure;
        end;
      end;

      // §4.4.3 no scheme state
      upsNoScheme:
      begin
        if not AHasBase then
        begin
          State := upsFailure;
        end
        else if ABase.HasOpaquePath then
        begin
          if C <> '#' then
          begin
            State := upsFailure;
          end
          else
          begin
            // Copy base path and query, set fragment
            URL.Scheme := ABase.Scheme;
            URL.HasOpaquePath := True;
            URL.OpaquePath := ABase.OpaquePath;
            URL.Query := ABase.Query;
            URL.HasNullQuery := ABase.HasNullQuery;
            URL.Fragment := '';
            URL.HasNullFragment := False;
            State := upsFragment;
            Inc(I);
          end;
        end
        else if ABase.Scheme <> 'file' then
        begin
          State := upsRelative;
        end
        else
        begin
          State := upsFile;
        end;
      end;

      // §4.4.4 special relative or authority state
      upsSpecialRelativeOrAuthority:
      begin
        if (C = '/') and (Peek(1) = '/') then
        begin
          State := upsSpecialAuthorityIgnoreSlashes;
          Inc(I, 2);
        end
        else
        begin
          State := upsRelative;
          // Do not advance
        end;
      end;

      // §4.4.5 path or authority state
      upsPathOrAuthority:
      begin
        if C = '/' then
        begin
          State := upsAuthority;
          Inc(I);
        end
        else
        begin
          State := upsPath;
          // Do not advance
        end;
      end;

      // §4.4.6 relative state
      upsRelative:
      begin
        // Copy base scheme
        URL.Scheme := ABase.Scheme;
        IsSpecial := IsSpecialScheme(URL.Scheme);

        if C = '/' then
        begin
          State := upsRelativeSlash;
          Inc(I);
        end
        else if IsSpecial and (C = '\') then
        begin
          State := upsRelativeSlash;
          Inc(I);
        end
        else
        begin
          // Copy base URL components
          URL.Username := ABase.Username;
          URL.Password := ABase.Password;
          URL.Host := ABase.Host;
          URL.HasNullHost := ABase.HasNullHost;
          URL.Port := ABase.Port;
          URL.PathSegments := Copy(ABase.PathSegments);
          URL.Query := ABase.Query;
          URL.HasNullQuery := ABase.HasNullQuery;

          if C = '?' then
          begin
            URL.Query := '';
            URL.HasNullQuery := False;
            State := upsQuery;
            Inc(I);
          end
          else if C = '#' then
          begin
            URL.Fragment := '';
            URL.HasNullFragment := False;
            State := upsFragment;
            Inc(I);
          end
          else if C <> EOF_CHAR then
          begin
            URL.Query := '';
            URL.HasNullQuery := True;
            ShortenPath(URL.Scheme, URL.PathSegments);
            State := upsPath;
            // Do not advance
          end
          else
          begin
            State := upsDone;
          end;
        end;
      end;

      // §4.4.7 relative slash state
      upsRelativeSlash:
      begin
        if IsSpecial and ((C = '/') or (C = '\')) then
        begin
          State := upsSpecialAuthorityIgnoreSlashes;
          Inc(I);
        end
        else if C = '/' then
        begin
          State := upsAuthority;
          Inc(I);
        end
        else
        begin
          URL.Username := ABase.Username;
          URL.Password := ABase.Password;
          URL.Host := ABase.Host;
          URL.HasNullHost := ABase.HasNullHost;
          URL.Port := ABase.Port;
          State := upsPath;
          // Do not advance
        end;
      end;

      // §4.4.8 special authority slashes state
      upsSpecialAuthoritySlashes:
      begin
        if (C = '/') and (Peek(1) = '/') then
        begin
          State := upsSpecialAuthorityIgnoreSlashes;
          Inc(I, 2);
        end
        else
        begin
          State := upsSpecialAuthorityIgnoreSlashes;
          // Do not advance
        end;
      end;

      // §4.4.9 special authority ignore slashes state
      upsSpecialAuthorityIgnoreSlashes:
      begin
        if (C = '/') or (C = '\') then
          Inc(I)
        else
          State := upsAuthority;
        // Do not advance in the else case
      end;

      // §4.4.10 authority state
      upsAuthority:
      begin
        if C = '@' then
        begin
          // If we already saw a '@', the current buffer should be prefixed with %40
          if AtSignSeen then
            Buffer := '%40' + Buffer;
          AtSignSeen := True;

          // §4.4.10 step 2: parse buffer as credentials (username[:password])
          URL.Username := '';
          URL.Password := '';
          PasswordTokenSeen := False;
          for J := 1 to Length(Buffer) do
          begin
            if (Buffer[J] = ':') and not PasswordTokenSeen then
              PasswordTokenSeen := True
            else if PasswordTokenSeen then
              URL.Password := URL.Password + PercentEncodeForUserinfo(Buffer[J])
            else
              URL.Username := URL.Username + PercentEncodeForUserinfo(Buffer[J]);
          end;

          Buffer := '';
          Inc(I);
        end
        else if (C in [EOF_CHAR, '/', '?', '#']) or (IsSpecial and (C = '\')) then
        begin
          // End of authority — buffer contains host[:port] accumulated since last '@'
          if AtSignSeen and (Buffer = '') then
          begin
            State := upsFailure;
          end
          else
          begin
            // Rewind so host state re-reads the buffer content from the input stream.
            // The buffer was accumulated character by character; moving I back by
            // buffer.length positions it at the first character of the host.
            Dec(I, Length(Buffer));
            Buffer := '';
            State := upsHost;
          end;
        end
        else
        begin
          Buffer := Buffer + C;
          Inc(I);
        end;
      end;

      // §4.4.11 host state / §4.4.12 hostname state
      upsHost, upsHostname:
      begin
        if (C = ':') and not InsideBrackets then
        begin
          if IsSpecial and (Buffer = '') then
          begin
            State := upsFailure;
          end
          else if AHasStateOverride and (State = upsHostname) then
          begin
            State := upsDone;
          end
          else
          begin
            if not ParseHost(Buffer, IsSpecial, Host) then
            begin
              State := upsFailure;
            end
            else
            begin
              URL.Host := Host;
              URL.HasNullHost := False;
              Buffer := '';
              State := upsPort;
              Inc(I);
            end;
          end;
        end
        else if (C in [EOF_CHAR, '/', '?', '#']) or (IsSpecial and (C = '\')) then
        begin
          if IsSpecial and (Buffer = '') then
          begin
            State := upsFailure;
          end
          else if AHasStateOverride and (Buffer = '') and
                  (URLHasCredentials(URL) or (URL.Port <> URL_NULL_PORT)) then
          begin
            State := upsDone;
          end
          else
          begin
            if not ParseHost(Buffer, IsSpecial, Host) then
            begin
              State := upsFailure;
            end
            else
            begin
              URL.Host := Host;
              URL.HasNullHost := False;
              Buffer := '';
              State := upsPathStart;
              // Do not advance
            end;
          end;
        end
        else
        begin
          if C = '[' then InsideBrackets := True;
          if C = ']' then InsideBrackets := False;
          Buffer := Buffer + C;
          Inc(I);
        end;
      end;

      // §4.4.13 port state
      upsPort:
      begin
        if IsASCIIDigit(C) then
        begin
          Buffer := Buffer + C;
          Inc(I);
        end
        else if (C in [EOF_CHAR, '/', '?', '#']) or (IsSpecial and (C = '\')) or
                AHasStateOverride then
        begin
          if Buffer <> '' then
          begin
            PortNum := StrToIntDef(Buffer, -1);
            if (PortNum < 0) or (PortNum > 65535) then
            begin
              State := upsFailure;
            end
            else
            begin
              Buffer := '';
              // Set port (null if default for scheme)
              if PortNum = DefaultPortForScheme(URL.Scheme) then
                URL.Port := URL_NULL_PORT
              else
                URL.Port := PortNum;
            end;
          end;
          if State <> upsFailure then
          begin
            if AHasStateOverride then
              State := upsDone
            else
              State := upsPathStart;
            // Do not advance
          end;
        end
        else
        begin
          State := upsFailure;
        end;
      end;

      // §4.4.14 file state
      upsFile:
      begin
        URL.Scheme := 'file';
        IsSpecial := True;
        URL.Host := '';
        URL.HasNullHost := False; // file has a host (empty string)

        if (C = '/') or (C = '\') then
        begin
          State := upsFileSlash;
          Inc(I);
        end
        else if AHasBase and (ABase.Scheme = 'file') then
        begin
          // Copy base
          URL.Host := ABase.Host;
          URL.HasNullHost := ABase.HasNullHost;
          URL.PathSegments := Copy(ABase.PathSegments);
          URL.Query := ABase.Query;
          URL.HasNullQuery := ABase.HasNullQuery;

          if C = '?' then
          begin
            URL.Query := '';
            URL.HasNullQuery := False;
            State := upsQuery;
            Inc(I);
          end
          else if C = '#' then
          begin
            URL.Fragment := '';
            URL.HasNullFragment := False;
            State := upsFragment;
            Inc(I);
          end
          else if C <> EOF_CHAR then
          begin
            URL.Query := '';
            URL.HasNullQuery := True;
            // If input does not start with windows drive letter
            if not StartsWithWindowsDriveLetter(Copy(Input, I, Length(Input))) then
              ShortenPath(URL.Scheme, URL.PathSegments);
            State := upsPath;
            // Do not advance
          end
          else
          begin
            State := upsDone;
          end;
        end
        else
        begin
          State := upsPath;
          // Do not advance
        end;
      end;

      // §4.4.15 file slash state
      upsFileSlash:
      begin
        if (C = '/') or (C = '\') then
        begin
          State := upsFileHost;
          Inc(I);
        end
        else
        begin
          if AHasBase and (ABase.Scheme = 'file') then
          begin
            // Copy base host
            URL.Host := ABase.Host;
            URL.HasNullHost := ABase.HasNullHost;
            // If base has Windows drive letter, keep it
            if (Length(ABase.PathSegments) > 0) and
               IsNormalizedWindowsDriveLetter(ABase.PathSegments[0]) then
              AppendPathSegment(URL.PathSegments, ABase.PathSegments[0]);
          end;
          State := upsPath;
          // Do not advance
        end;
      end;

      // §4.4.16 file host state
      upsFileHost:
      begin
        if (C in [EOF_CHAR, '/', '\', '?', '#']) then
        begin
          // end of host
          if IsWindowsDriveLetter(Buffer) then
          begin
            // Buffer looks like a drive letter — treat as path
            State := upsPath;
            // Do not advance (keep buffer for path processing)
          end
          else if Buffer = '' then
          begin
            URL.Host := '';
            URL.HasNullHost := False; // empty string host, not null
            if AHasStateOverride then
              State := upsDone
            else
              State := upsPathStart;
            // Do not advance
          end
          else
          begin
            if not ParseHost(Buffer, IsSpecial, Host) then
            begin
              State := upsFailure;
            end
            else
            begin
              if Host = 'localhost' then Host := '';
              URL.Host := Host;
              URL.HasNullHost := False;
              Buffer := '';
              if AHasStateOverride then
                State := upsDone
              else
                State := upsPathStart;
              // Do not advance
            end;
          end;
        end
        else
        begin
          Buffer := Buffer + C;
          Inc(I);
        end;
      end;

      // §4.4.17 path start state
      upsPathStart:
      begin
        if IsSpecial then
        begin
          // Special URLs: '/' or '\' → path state, otherwise path state
          if C = '\' then
            Inc(I)  // ignore backslash for special
          else if C <> '/' then
          begin
            // Don't advance — path state handles adding slash
          end;
          State := upsPath;
          if C = '/' then Inc(I);
        end
        else if not AHasStateOverride and (C = '?') then
        begin
          URL.Query := '';
          URL.HasNullQuery := False;
          State := upsQuery;
          Inc(I);
        end
        else if not AHasStateOverride and (C = '#') then
        begin
          URL.Fragment := '';
          URL.HasNullFragment := False;
          State := upsFragment;
          Inc(I);
        end
        else if C <> EOF_CHAR then
        begin
          State := upsPath;
          if C <> '/' then
          begin
            // Do not advance
          end
          else
            Inc(I);
        end
        else
          State := upsDone;
      end;

      // §4.4.18 path state
      upsPath:
      begin
        if (C = EOF_CHAR) or (C = '/') or (IsSpecial and (C = '\')) or
           (not AHasStateOverride and ((C = '?') or (C = '#'))) then
        begin
          // Process buffer as a path segment
          if (ASCIILowerString(Buffer) = '..') or
             (Buffer = '.%2e') or (Buffer = '.%2E') or
             (Buffer = '%2e.') or (Buffer = '%2E.') or
             (Buffer = '%2e%2e') or (Buffer = '%2E%2E') or
             (Buffer = '%2E%2e') or (Buffer = '%2e%2E') then
          begin
            // Double dot: go up
            ShortenPath(URL.Scheme, URL.PathSegments);
            if (C <> '/') and not (IsSpecial and (C = '\')) then
              AppendPathSegment(URL.PathSegments, '');
          end
          else if (Buffer = '.') or (Buffer = '%2e') or (Buffer = '%2E') then
          begin
            // Single dot: stay (but add empty segment if at end)
            if (C <> '/') and not (IsSpecial and (C = '\')) then
              AppendPathSegment(URL.PathSegments, '');
          end
          else
          begin
            // Special case: file scheme with Windows drive letter
            if (URL.Scheme = 'file') and (Length(URL.PathSegments) = 0) and
               IsWindowsDriveLetter(Buffer) then
              Buffer := Buffer[1] + ':'; // normalize | to :
            AppendPathSegment(URL.PathSegments, Buffer);
          end;
          Buffer := '';

          if C = '?' then
          begin
            URL.Query := '';
            URL.HasNullQuery := False;
            State := upsQuery;
            Inc(I);
          end
          else if C = '#' then
          begin
            URL.Fragment := '';
            URL.HasNullFragment := False;
            State := upsFragment;
            Inc(I);
          end
          else if C = EOF_CHAR then
          begin
            State := upsDone;
          end
          else
          begin
            // '/' or special '\': continue in path state
            Inc(I);
          end;
        end
        else
        begin
          // Add encoded character to buffer
          Buffer := Buffer + PercentEncodeForPath(C);
          Inc(I);
        end;
      end;

      // §4.4.19 opaque path state
      upsOpaquePath:
      begin
        if C = '?' then
        begin
          URL.Query := '';
          URL.HasNullQuery := False;
          State := upsQuery;
          Inc(I);
        end
        else if C = '#' then
        begin
          URL.Fragment := '';
          URL.HasNullFragment := False;
          State := upsFragment;
          Inc(I);
        end
        else if C = EOF_CHAR then
        begin
          State := upsDone;
        end
        else
        begin
          URL.OpaquePath := URL.OpaquePath + PercentEncodeForPath(C);
          Inc(I);
        end;
      end;

      // §4.4.20 query state
      upsQuery:
      begin
        if C = '#' then
        begin
          // Encode query up to this point
          URL.Query := URL.Query + PercentEncodeForQuery(Buffer, IsSpecial);
          Buffer := '';
          URL.Fragment := '';
          URL.HasNullFragment := False;
          State := upsFragment;
          Inc(I);
        end
        else if C = EOF_CHAR then
        begin
          URL.Query := URL.Query + PercentEncodeForQuery(Buffer, IsSpecial);
          Buffer := '';
          State := upsDone;
        end
        else
        begin
          Buffer := Buffer + C;
          Inc(I);
        end;
      end;

      // §4.4.21 fragment state
      upsFragment:
      begin
        if C = EOF_CHAR then
        begin
          URL.Fragment := URL.Fragment + PercentEncodeForFragment(Buffer);
          Buffer := '';
          State := upsDone;
        end
        else
        begin
          Buffer := Buffer + C;
          Inc(I);
        end;
      end;

      upsFailure:
      begin
        URL.IsValid := False;
        Exit(URL);
      end;

      upsDone:
        Break;
    end; // case

    // Safety: if we're past the end and not done, go to done
    if (I > Len + 1) then
      State := upsDone;
  end; // while

  // Handle remaining buffer in query/fragment states
  if Buffer <> '' then
  begin
    case State of
      upsQuery:
        URL.Query := URL.Query + PercentEncodeForQuery(Buffer, IsSpecial);
      upsFragment:
        URL.Fragment := URL.Fragment + PercentEncodeForFragment(Buffer);
    end;
  end;

  // For special schemes, ensure path is non-empty (add '' segment = '/')
  if IsSpecial and (Length(URL.PathSegments) = 0) then
    AppendPathSegment(URL.PathSegments, '');

  URL.IsValid := True;
  Result := URL;
end;

// ---------------------------------------------------------------------------
// Public parsing functions
// ---------------------------------------------------------------------------

// WHATWG URL §4.4 basic URL parser — no base URL
function ParseURL(const AInput: string): TGocciaURLRecord;
var
  DummyBase: TGocciaURLRecord;
begin
  FillChar(DummyBase, SizeOf(DummyBase), 0);
  Result := ParseURLInternal(AInput, False, DummyBase, upsSchemeStart, False);
end;

// WHATWG URL §4.4 basic URL parser — with base URL
function ParseURLWithBase(const AInput: string;
  const ABase: TGocciaURLRecord): TGocciaURLRecord;
begin
  Result := ParseURLInternal(AInput, True, ABase, upsSchemeStart, False);
end;

end.
