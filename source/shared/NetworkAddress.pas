unit NetworkAddress;

{ Socket-free IP address parsing and classification.

  Split out of HTTPClient so layers that must compile without BSD sockets
  (the capability set, which the Lakon WASM lane builds) can answer "is this
  an IP literal", "is it private", and "is it inside this CIDR range" without
  pulling the socket closure. HTTPClient re-exports IsPrivateNetworkAddress
  under its original name. }

{$I Shared.inc}

interface

type
  TNetworkAddressFamily = (nafIPv4, nafIPv6);

  TNetworkAddress = record
    Family: TNetworkAddressFamily;
    { IPv4 uses Bytes[0..3]; IPv6 uses all sixteen. }
    Bytes: array[0..15] of Byte;
  end;

{ Strict dotted-quad IPv4: four decimal parts of at most three digits, each at
  most 255. Shorthand (`127.1`), hex (`0x7f.0.0.1`), and bare-integer
  (`2130706433`) spellings are refused rather than reinterpreted — those forms
  are a classic way to smuggle a loopback address past a naive textual filter. }
function TryParseIPv4(const AValue: string; out AOctets: array of Byte): Boolean;

{ RFC 4291 textual IPv6, with `::` compression and an optional trailing
  embedded IPv4. Surrounding brackets and a `%zone` suffix are accepted and
  discarded. }
function TryParseIPv6(const AValue: string; out ABytes: array of Byte): Boolean;

{ Either family. An IPv4-mapped IPv6 address (`::ffff:a.b.c.d`, also written
  `::ffff:a9fe:a9fe`) is returned as the IPv4 address it names, so every
  consumer judges one host by one spelling. }
function TryParseIPAddress(const AValue: string;
  out AAddress: TNetworkAddress): Boolean;

{ `<address>/<prefix>`; the prefix may not exceed the family's bit width. An
  IPv4-mapped network with a prefix of at least 96 becomes the IPv4 network
  it names. }
function TryParseCIDR(const AValue: string; out ANetwork: TNetworkAddress;
  out APrefixLength: Integer): Boolean;

function IsAddressInNetwork(const AAddress, ANetwork: TNetworkAddress;
  const APrefixLength: Integer): Boolean;

function AddressesEqual(const A, B: TNetworkAddress): Boolean;

{ True when the address belongs to a range that is not routable on the public
  internet: RFC 1918, loopback, "this host", link-local (which includes the
  cloud metadata endpoint 169.254.169.254), CGNAT, the IETF protocol block,
  IPv4 multicast and above, and IPv6 loopback/unspecified/ULA/link-local and
  IPv4-mapped space. }
function IsPrivateIPAddress(const AAddress: TNetworkAddress): Boolean;

{ The textual form of IsPrivateIPAddress. Callers pass a *resolved* address,
  so text that is not an address literal at all means resolution produced
  something unexpected; it is classified private (deny-biased) rather than let
  an unclassifiable target through. }
function IsPrivateNetworkAddress(const AAddressText: string): Boolean;

implementation

uses
  SysUtils;

function TryParseIPv4(const AValue: string; out AOctets: array of Byte): Boolean;
var
  I, Part, Digits, Value: Integer;
  Ch: Char;
begin
  Part := 0;
  Value := 0;
  Digits := 0;
  for I := 1 to Length(AValue) do
  begin
    Ch := AValue[I];
    if (Ch >= '0') and (Ch <= '9') then
    begin
      Inc(Digits);
      if Digits > 3 then
        Exit(False);
      Value := Value * 10 + (Ord(Ch) - Ord('0'));
      if Value > 255 then
        Exit(False);
    end
    else if Ch = '.' then
    begin
      if (Digits = 0) or (Part > 2) then
        Exit(False);
      AOctets[Part] := Byte(Value);
      Inc(Part);
      Value := 0;
      Digits := 0;
    end
    else
      Exit(False);
  end;
  if (Digits = 0) or (Part <> 3) then
    Exit(False);
  AOctets[3] := Byte(Value);
  Result := True;
end;

function HexDigitValue(const ACh: Char): Integer;
begin
  case ACh of
    '0'..'9':
      Result := Ord(ACh) - Ord('0');
    'a'..'f':
      Result := Ord(ACh) - Ord('a') + 10;
    'A'..'F':
      Result := Ord(ACh) - Ord('A') + 10;
  else
    Result := -1;
  end;
end;

function TryParseIPv6(const AValue: string; out ABytes: array of Byte): Boolean;
const
  GROUP_COUNT = 8;
var
  Text, Group: string;
  Groups: array[0..GROUP_COUNT - 1] of Integer;
  HeadCount, TailCount, I, GroupValue, DigitValue, PercentPos,
    CompressionPos: Integer;
  Tail: array[0..GROUP_COUNT - 1] of Integer;
  Octets: array[0..3] of Byte;
  HasCompression: Boolean;

  function ParseGroups(const APart: string; var AGroups: array of Integer;
    out ACount: Integer; const AAllowIPv4Tail: Boolean): Boolean;
  var
    Start, K, Stop, DigitIndex: Integer;
    Piece: string;
  begin
    ACount := 0;
    if APart = '' then
      Exit(True);
    Start := 1;
    K := 1;
    while K <= Length(APart) + 1 do
    begin
      if (K > Length(APart)) or (APart[K] = ':') then
      begin
        Stop := K - 1;
        Piece := Copy(APart, Start, Stop - Start + 1);
        if Piece = '' then
          Exit(False);
        if (K > Length(APart)) and AAllowIPv4Tail and (Pos('.', Piece) > 0) then
        begin
          if not TryParseIPv4(Piece, Octets) then
            Exit(False);
          if ACount + 2 > GROUP_COUNT then
            Exit(False);
          AGroups[ACount] := (Octets[0] shl 8) or Octets[1];
          AGroups[ACount + 1] := (Octets[2] shl 8) or Octets[3];
          Inc(ACount, 2);
        end
        else
        begin
          if (Length(Piece) > 4) or (ACount >= GROUP_COUNT) then
            Exit(False);
          GroupValue := 0;
          for DigitIndex := 1 to Length(Piece) do
          begin
            DigitValue := HexDigitValue(Piece[DigitIndex]);
            if DigitValue < 0 then
              Exit(False);
            GroupValue := (GroupValue shl 4) or DigitValue;
          end;
          AGroups[ACount] := GroupValue;
          Inc(ACount);
        end;
        Start := K + 1;
      end;
      Inc(K);
    end;
    Result := True;
  end;

begin
  Result := False;
  Text := AValue;
  if (Length(Text) >= 2) and (Text[1] = '[') and (Text[Length(Text)] = ']') then
    Text := Copy(Text, 2, Length(Text) - 2);
  PercentPos := Pos('%', Text);
  if PercentPos > 0 then
  begin
    if PercentPos = Length(Text) then
      Exit;
    Text := Copy(Text, 1, PercentPos - 1);
  end;
  if (Text = '') or (Pos(':', Text) = 0) then
    Exit;

  CompressionPos := Pos('::', Text);
  HasCompression := CompressionPos > 0;
  if HasCompression then
  begin
    if Pos('::', Copy(Text, CompressionPos + 2, MaxInt)) > 0 then
      Exit;
    Group := Copy(Text, 1, CompressionPos - 1);
    if not ParseGroups(Group, Groups, HeadCount, False) then
      Exit;
    Group := Copy(Text, CompressionPos + 2, MaxInt);
    if not ParseGroups(Group, Tail, TailCount, True) then
      Exit;
    if HeadCount + TailCount > GROUP_COUNT - 1 then
      Exit;
    for I := HeadCount to GROUP_COUNT - 1 do
      Groups[I] := 0;
    for I := 0 to TailCount - 1 do
      Groups[GROUP_COUNT - TailCount + I] := Tail[I];
  end
  else
  begin
    if not ParseGroups(Text, Groups, HeadCount, True) then
      Exit;
    if HeadCount <> GROUP_COUNT then
      Exit;
  end;

  for I := 0 to GROUP_COUNT - 1 do
  begin
    ABytes[I * 2] := Byte(Groups[I] shr 8);
    ABytes[I * 2 + 1] := Byte(Groups[I] and $FF);
  end;
  Result := True;
end;

function IsIPv4MappedAddress(const AAddress: TNetworkAddress): Boolean;
var
  I: Integer;
begin
  if AAddress.Family <> nafIPv6 then
    Exit(False);
  for I := 0 to 9 do
    if AAddress.Bytes[I] <> 0 then
      Exit(False);
  Result := (AAddress.Bytes[10] = $FF) and (AAddress.Bytes[11] = $FF);
end;

{ The IPv4 address in bytes AOffset..AOffset+3 of an IPv6 address. }
function EmbeddedIPv4(const AAddress: TNetworkAddress;
  const AOffset: Integer): TNetworkAddress;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Family := nafIPv4;
  for I := 0 to 3 do
    Result.Bytes[I] := AAddress.Bytes[AOffset + I];
end;

function TryParseRawIPAddress(const AValue: string;
  out AAddress: TNetworkAddress): Boolean;
var
  Octets: array[0..3] of Byte;
  I: Integer;
begin
  FillChar(AAddress, SizeOf(AAddress), 0);
  if TryParseIPv4(AValue, Octets) then
  begin
    AAddress.Family := nafIPv4;
    for I := 0 to 3 do
      AAddress.Bytes[I] := Octets[I];
    Exit(True);
  end;
  AAddress.Family := nafIPv6;
  Result := TryParseIPv6(AValue, AAddress.Bytes);
end;

function TryParseIPAddress(const AValue: string;
  out AAddress: TNetworkAddress): Boolean;
begin
  Result := TryParseRawIPAddress(AValue, AAddress);
  if Result and IsIPv4MappedAddress(AAddress) then
    AAddress := EmbeddedIPv4(AAddress, 12);
end;

function TryParseCIDR(const AValue: string; out ANetwork: TNetworkAddress;
  out APrefixLength: Integer): Boolean;
var
  SlashPos, MaxPrefix: Integer;
begin
  Result := False;
  APrefixLength := 0;
  FillChar(ANetwork, SizeOf(ANetwork), 0);
  SlashPos := Pos('/', AValue);
  if (SlashPos <= 1) or (SlashPos = Length(AValue)) then
    Exit;
  if not TryParseRawIPAddress(Copy(AValue, 1, SlashPos - 1), ANetwork) then
    Exit;
  if not TryStrToInt(Copy(AValue, SlashPos + 1, MaxInt), APrefixLength) then
    Exit;
  if ANetwork.Family = nafIPv4 then
    MaxPrefix := 32
  else
    MaxPrefix := 128;
  Result := (APrefixLength >= 0) and (APrefixLength <= MaxPrefix);
  if Result and IsIPv4MappedAddress(ANetwork) and (APrefixLength >= 96) then
  begin
    ANetwork := EmbeddedIPv4(ANetwork, 12);
    APrefixLength := APrefixLength - 96;
  end;
end;

function IsAddressInNetwork(const AAddress, ANetwork: TNetworkAddress;
  const APrefixLength: Integer): Boolean;
var
  FullBytes, RemainingBits, I: Integer;
  Mask: Byte;
begin
  if AAddress.Family <> ANetwork.Family then
    Exit(False);
  FullBytes := APrefixLength div 8;
  RemainingBits := APrefixLength mod 8;
  for I := 0 to FullBytes - 1 do
    if AAddress.Bytes[I] <> ANetwork.Bytes[I] then
      Exit(False);
  if RemainingBits > 0 then
  begin
    Mask := Byte($FF shl (8 - RemainingBits));
    if (AAddress.Bytes[FullBytes] and Mask) <>
       (ANetwork.Bytes[FullBytes] and Mask) then
      Exit(False);
  end;
  Result := True;
end;

function AddressesEqual(const A, B: TNetworkAddress): Boolean;
var
  I, Count: Integer;
begin
  if A.Family <> B.Family then
    Exit(False);
  if A.Family = nafIPv4 then
    Count := 4
  else
    Count := 16;
  for I := 0 to Count - 1 do
    if A.Bytes[I] <> B.Bytes[I] then
      Exit(False);
  Result := True;
end;

function IsPrivateIPAddress(const AAddress: TNetworkAddress): Boolean;
var
  I: Integer;
  AllZeroBeforeLast, AllZero: Boolean;
begin
  if AAddress.Family = nafIPv4 then
  begin
    Result :=
      (AAddress.Bytes[0] = 10) or                                  // 10/8
      (AAddress.Bytes[0] = 127) or                                 // loopback
      (AAddress.Bytes[0] = 0) or                                   // this host
      ((AAddress.Bytes[0] = 172) and (AAddress.Bytes[1] >= 16) and
       (AAddress.Bytes[1] <= 31)) or                               // 172.16/12
      ((AAddress.Bytes[0] = 192) and (AAddress.Bytes[1] = 168)) or // 192.168/16
      ((AAddress.Bytes[0] = 169) and (AAddress.Bytes[1] = 254)) or // link-local
      ((AAddress.Bytes[0] = 100) and (AAddress.Bytes[1] >= 64) and
       (AAddress.Bytes[1] <= 127)) or                              // CGNAT
      ((AAddress.Bytes[0] = 192) and (AAddress.Bytes[1] = 0) and
       (AAddress.Bytes[2] = 0)) or                                 // IETF proto
      (AAddress.Bytes[0] >= 224);                                  // multicast +
    Exit;
  end;

  AllZeroBeforeLast := True;
  for I := 0 to 14 do
    if AAddress.Bytes[I] <> 0 then
    begin
      AllZeroBeforeLast := False;
      Break;
    end;
  AllZero := AllZeroBeforeLast and (AAddress.Bytes[15] = 0);
  if AllZero then
    Exit(True);                                                    // ::
  if AllZeroBeforeLast and (AAddress.Bytes[15] = 1) then
    Exit(True);                                                    // ::1
  if (AAddress.Bytes[0] and $FE) = $FC then
    Exit(True);                                                    // fc00::/7
  if (AAddress.Bytes[0] = $FE) and ((AAddress.Bytes[1] and $C0) = $80) then
    Exit(True);                                                    // fe80::/10
  if (AAddress.Bytes[0] = $FE) and ((AAddress.Bytes[1] and $C0) = $C0) then
    Exit(True);                                                    // fec0::/10
  if AAddress.Bytes[0] = $FF then
    Exit(True);                                                    // ff00::/8
  { ::ffff:0:0/96 — an address that reaches here unconverted names an IPv4
    host the IPv6 checks cannot classify, so the block is private. }
  if IsIPv4MappedAddress(AAddress) then
    Exit(True);
  { ::/96 — the deprecated IPv4-compatible form (::127.0.0.1). }
  AllZero := True;
  for I := 0 to 11 do
    if AAddress.Bytes[I] <> 0 then
    begin
      AllZero := False;
      Break;
    end;
  if AllZero then
    Exit(True);
  { 64:ff9b::/96 — NAT64 translates to the embedded IPv4 host;
    64:ff9b:1::/48 is local-use NAT64 and never public. }
  if (AAddress.Bytes[0] = $00) and (AAddress.Bytes[1] = $64) and
     (AAddress.Bytes[2] = $FF) and (AAddress.Bytes[3] = $9B) then
  begin
    if (AAddress.Bytes[4] = $00) and (AAddress.Bytes[5] = $01) then
      Exit(True);
    AllZero := True;
    for I := 4 to 11 do
      if AAddress.Bytes[I] <> 0 then
      begin
        AllZero := False;
        Break;
      end;
    if AllZero then
      Exit(IsPrivateIPAddress(EmbeddedIPv4(AAddress, 12)));
  end;
  { 2002::/16 — 6to4 relays to the IPv4 host in bytes 2..5. }
  if (AAddress.Bytes[0] = $20) and (AAddress.Bytes[1] = $02) then
    Exit(IsPrivateIPAddress(EmbeddedIPv4(AAddress, 2)));
  Result := False;
end;

function IsPrivateNetworkAddress(const AAddressText: string): Boolean;
var
  Address: TNetworkAddress;
  Normalized: string;
begin
  Normalized := LowerCase(Trim(AAddressText));
  if Normalized = '' then
    Exit(True);
  if not TryParseIPAddress(Normalized, Address) then
    Exit(True);
  Result := IsPrivateIPAddress(Address);
end;

end.
