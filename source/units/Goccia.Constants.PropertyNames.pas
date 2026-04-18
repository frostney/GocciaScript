unit Goccia.Constants.PropertyNames;

{$I Goccia.inc}

interface

const
  PROP_LENGTH       = 'length';
  PROP_NAME         = 'name';
  PROP_PATH         = 'path';
  PROP_FILE_NAME    = 'fileName';
  PROP_CONTENT      = 'content';
  PROP_MESSAGE      = 'message';
  PROP_CAUSE        = 'cause';
  PROP_CODE         = 'code';
  PROP_STACK        = 'stack';
  PROP_ERRORS       = 'errors';
  PROP_ERROR        = 'error';
  PROP_CONSTRUCTOR  = 'constructor';
  PROP_PROTOTYPE    = 'prototype';
  PROP_TO_STRING    = 'toString';
  PROP_VALUE_OF     = 'valueOf';
  PROP_FREEZE       = 'freeze';
  PROP_DESCRIPTION  = 'description';
  PROP_SOURCE       = 'source';
  PROP_FLAGS        = 'flags';
  PROP_LAST_INDEX   = 'lastIndex';
  PROP_GLOBAL       = 'global';
  PROP_IGNORE_CASE  = 'ignoreCase';
  PROP_MULTILINE    = 'multiline';
  PROP_DOT_ALL      = 'dotAll';
  PROP_UNICODE      = 'unicode';
  PROP_STICKY       = 'sticky';
  PROP_SIZE         = 'size';
  PROP_INDEX        = 'index';
  PROP_INPUT        = 'input';
  PROP_DONE         = 'done';
  PROP_READ         = 'read';
  PROP_VALUE        = 'value';
  PROP_VALUES       = 'values';
  PROP_TAG_NAME     = 'tagName';
  PROP_NEXT         = 'next';
  PROP_RETURN       = 'return';
  PROP_WRITABLE     = 'writable';
  PROP_ENUMERABLE   = 'enumerable';
  PROP_CONFIGURABLE = 'configurable';
  PROP_GET          = 'get';
  PROP_SET          = 'set';
  PROP_INIT         = 'init';
  PROP_KIND         = 'kind';
  PROP_STATIC       = 'static';
  PROP_PRIVATE      = 'private';
  PROP_ACCESS       = 'access';
  PROP_METADATA     = 'metadata';
  PROP_ADD_INITIALIZER = 'addInitializer';
  PROP_THEN         = 'then';
  PROP_RESOLVES     = 'resolves';
  PROP_REJECTS      = 'rejects';
  PROP_BYTE_LENGTH      = 'byteLength';
  PROP_BYTE_OFFSET      = 'byteOffset';
  PROP_BUFFER           = 'buffer';
  PROP_MAX_BYTE_LENGTH  = 'maxByteLength';
  PROP_RESIZABLE        = 'resizable';
  PROP_DETACHED         = 'detached';
  PROP_BYTES_PER_ELEMENT = 'BYTES_PER_ELEMENT';
  PROP_FROM             = 'from';
  PROP_OF               = 'of';
  PROP_URL              = 'url';
  PROP_RESOLVE          = 'resolve';
  PROP_HAS_OWN_PROPERTY       = 'hasOwnProperty';
  PROP_IS_PROTOTYPE_OF        = 'isPrototypeOf';
  PROP_PROPERTY_IS_ENUMERABLE = 'propertyIsEnumerable';
  PROP_TO_LOCALE_STRING       = 'toLocaleString';
  PROP_STRICT_TYPES           = 'strictTypes';
  PROP_NOW                    = 'now';
  PROP_EXTENSION              = 'extension';
  PROP_QUOTE                  = 'quote';
  PROP_REPLACER               = 'replacer';
  PROP_SPACE                  = 'space';
  PROP_TO_JSON                = 'toJSON';
  PROP_TO_JSON5               = 'toJSON5';
  PROP_TIME_ORIGIN            = 'timeOrigin';
  PROP_GROUPS                 = 'groups';
  PROP_UNICODE_SETS           = 'unicodeSets';
  PROP_HAS_INDICES            = 'hasIndices';
  PROP_TYPE                   = 'type';
  PROP_CALLS                  = 'calls';
  PROP_RESULTS                = 'results';
  PROP_INSTANCES              = 'instances';
  PROP_LAST_CALL              = 'lastCall';
  PROP_IS_NULL                = 'isNull';
  PROP_FFI_ADDRESS            = 'address';
  PROP_OPEN                   = 'open';
  PROP_NULLPTR                = 'nullptr';
  PROP_SUFFIX                 = 'suffix';
  PROP_MODE                   = 'mode';
  PROP_PADDING                = 'padding';
  PROP_ALPHABET               = 'alphabet';
  PROP_OMIT_PADDING           = 'omitPadding';
  PROP_LAST_CHUNK_HANDLING    = 'lastChunkHandling';
  PROP_WRITTEN                = 'written';
  PROP_TO_BASE64              = 'toBase64';
  PROP_TO_HEX                 = 'toHex';
  PROP_FROM_BASE64            = 'fromBase64';
  PROP_FROM_HEX               = 'fromHex';
  PROP_SET_FROM_BASE64        = 'setFromBase64';
  PROP_SET_FROM_HEX           = 'setFromHex';
  PROP_RAW                    = 'raw';
  PROP_RAW_JSON               = 'rawJSON';
  PROP_ENCODING               = 'encoding';
  PROP_FATAL                  = 'fatal';
  PROP_IGNORE_BOM             = 'ignoreBOM';
  PROP_ENCODE                 = 'encode';
  PROP_ENCODE_INTO            = 'encodeInto';
  PROP_DECODE                 = 'decode';
  PROP_SUPPRESSED             = 'suppressed';
  PROP_DISPOSED               = 'disposed';
  PROP_DISPOSE_ASYNC          = 'disposeAsync';
  PROP_USE                    = 'use';
  PROP_ADOPT                  = 'adopt';
  PROP_DEFER                  = 'defer';
  PROP_MOVE                   = 'move';

  // URL and URLSearchParams properties
  PROP_HREF             = 'href';
  PROP_ORIGIN           = 'origin';
  PROP_PROTOCOL         = 'protocol';
  PROP_USERNAME         = 'username';
  PROP_PASSWORD         = 'password';
  PROP_HOST             = 'host';
  PROP_HOSTNAME         = 'hostname';
  PROP_PORT             = 'port';
  PROP_PATHNAME         = 'pathname';
  PROP_SEARCH           = 'search';
  PROP_SEARCH_PARAMS    = 'searchParams';
  PROP_HASH             = 'hash';
  PROP_PARSE            = 'parse';
  PROP_CAN_PARSE        = 'canParse';

  // URLSearchParams prototype method names
  PROP_APPEND    = 'append';
  PROP_DELETE    = 'delete';
  PROP_GET_ALL   = 'getAll';
  PROP_SORT      = 'sort';
  PROP_KEYS      = 'keys';
  PROP_ENTRIES   = 'entries';
  PROP_FOR_EACH  = 'forEach';

  // Temporal property names
  PROP_YEAR                   = 'year';
  PROP_MONTH                  = 'month';
  PROP_MONTH_CODE             = 'monthCode';
  PROP_DAY                    = 'day';
  PROP_HOUR                   = 'hour';
  PROP_MINUTE                 = 'minute';
  PROP_SECOND                 = 'second';
  PROP_MILLISECOND            = 'millisecond';
  PROP_MICROSECOND            = 'microsecond';
  PROP_NANOSECOND             = 'nanosecond';
  PROP_CALENDAR_ID            = 'calendarId';

  // Proxy handler trap names
  PROP_HAS                       = 'has';
  PROP_DELETE_PROPERTY            = 'deleteProperty';
  PROP_APPLY                     = 'apply';
  PROP_CONSTRUCT                 = 'construct';
  PROP_OWN_KEYS                  = 'ownKeys';
  PROP_GET_OWN_PROPERTY_DESCRIPTOR = 'getOwnPropertyDescriptor';
  PROP_DEFINE_PROPERTY           = 'defineProperty';
  PROP_GET_PROTOTYPE_OF          = 'getPrototypeOf';
  PROP_SET_PROTOTYPE_OF          = 'setPrototypeOf';
  PROP_IS_EXTENSIBLE             = 'isExtensible';
  PROP_PREVENT_EXTENSIONS        = 'preventExtensions';
  PROP_REVOCABLE                 = 'revocable';
  PROP_REVOKE                    = 'revoke';
  PROP_PROXY                     = 'proxy';
  PROP_REVOKER_INTERNAL          = '__revoker';

  // Fetch API properties
  PROP_STATUS              = 'status';
  PROP_STATUS_TEXT          = 'statusText';
  PROP_OK                  = 'ok';
  PROP_HEADERS             = 'headers';
  PROP_BODY_USED           = 'bodyUsed';
  PROP_REDIRECTED          = 'redirected';
  PROP_TEXT                = 'text';
  PROP_JSON                = 'json';
  PROP_ARRAY_BUFFER_METHOD = 'arrayBuffer';
  PROP_METHOD              = 'method';

implementation

end.
