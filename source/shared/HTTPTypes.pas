unit HTTPTypes;

// The wire-facing TYPES of the HTTP client, split out of HTTPClient
// so abstraction layers (Goccia.FetchManager's abstract base, the
// fetch builtin's request assembly) can name THTTPHeaders without
// pulling the socket implementation into their unit closure —
// HTTPClient itself does not compile on runtimes without BSD
// sockets (the Lakon WASM lane). HTTPClient re-exports these under
// their original names, so existing native consumers are untouched.

{$I Shared.inc}

interface

uses
  SysUtils;

type
  THTTPHeader = record
    Name: string;
    Value: string;
  end;

  THTTPHeaders = array of THTTPHeader;

  THTTPResponse = record
    StatusCode: Integer;
    StatusText: string;
    Headers: THTTPHeaders;
    Body: TBytes;
    FinalURL: string;
    Redirected: Boolean;
  end;

  EHTTPError = class(Exception);

implementation

end.
