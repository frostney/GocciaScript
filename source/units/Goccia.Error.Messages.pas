unit Goccia.Error.Messages;

{$I Goccia.inc}

interface

resourcestring
  // Scope errors
  SErrorIdentifierAlreadyDeclared = 'Identifier ''%s'' has already been declared';
  SErrorCannotAccessBeforeInit = 'Cannot access ''%s'' before initialization';
  SErrorAssignToConstant = 'Assignment to constant variable ''%s''';
  SErrorUndefinedVariable = 'Undefined variable: %s';

  // Type errors — function calls
  SErrorMemberNotFunction = '%s.%s is not a function';
  SErrorNotFunction = '''%s'' is not a function';
  SErrorValueNotFunction = '%s is not a function';
  SErrorSymbolToNumber = 'Cannot convert a Symbol value to a number';
  SErrorSymbolToString = 'Cannot convert a Symbol value to a string';
  SErrorBigIntToNumber = 'Cannot convert a BigInt value to a number';
  SErrorBigIntMixedTypes = 'Cannot mix BigInt and other types, use explicit conversions';
  SErrorBigIntUnaryPlus = 'Cannot convert a BigInt value to a number';
  SErrorBigIntNegativeExponent = 'Exponent must be non-negative';
  SErrorBigIntDivisionByZero = 'Division by zero';
  SErrorBigIntUnsignedRightShift = 'Cannot use unsigned right shift with BigInt';
  SErrorBigIntInvalidConversion = 'Cannot convert %s to a BigInt';
  SErrorBigIntNotInteger = 'The number %s cannot be converted to a BigInt because it is not an integer';
  SErrorBigIntRequiresBigIntValue = '%s requires a BigInt value';
  SErrorBigIntInvalidRadix = 'toString() radix must be between 2 and 36';
  SErrorBigIntInvalidIndex = 'Invalid index';

  // Type errors — property access
  SErrorCannotReadPropertyOf = 'Cannot read property ''%s'' of %s';
  SErrorCannotReadPropertiesOf = 'Cannot read properties of %s (reading ''%s'')';

  // Type errors — constructors
  SErrorNotConstructor = '''%s'' is not a constructor';
  SErrorValueNotConstructor = '%s is not a constructor';

  // Type errors — iterables and spread
  SErrorSpreadRequiresIterable = 'Spread syntax requires an iterable';
  SErrorNotIterable = '''%s'' is not iterable';

  // Type errors — iterator protocol
  SErrorIteratorResultNotObject = 'Iterator result %s is not an object';

  // Type errors — destructuring
  SErrorCannotDestructure = 'Cannot destructure %s';
  SErrorCannotDestructureType = 'Cannot destructure value of type ''%s''';

  // Type errors — classes, decorators, private fields
  SErrorDecoratorMustBeFunction = 'Decorator must be a function';
  SErrorPrivateGetterMissing = 'Private accessor #%s was defined without a getter';
  SErrorPrivateSetterMissing = 'Private accessor #%s was defined without a setter';
  SErrorPrivateFieldInaccessible = 'Private field #%s is not accessible';
  SErrorEnumInitializer = 'Enum member initializer must evaluate to a Number, String, or Symbol value';

  // Type errors — async, disposal, tagged templates
  SErrorAwaitPromiseUnsettled = 'await: Promise did not settle after microtask drain';
  SErrorUsingOutsideBlock = 'using declaration outside of a block scope';
  SErrorNotDisposable = 'Value is not disposable (missing [Symbol.dispose])';
  SErrorNotAsyncDisposable = 'Value is not disposable (missing [Symbol.asyncDispose] and [Symbol.dispose])';

  // Type errors — delete
  SErrorCannotDeletePropertyOf = 'Cannot delete property ''%s'' of %s';

  // Array errors
  SErrorSpeciesNotConstructor = 'Species is not a constructor';
  SErrorInvalidArrayLength = 'Invalid array length';
  SErrorReduceEmptyArray = 'Reduce of empty array with no initial value';
  SErrorInvalidArrayWithIndex = 'Invalid index for Array.with';

  // Iterator helper errors — protocol
  SErrorIteratorNextNonIterator = 'Iterator.prototype.next called on non-iterator';
  SErrorIteratorMapNonIterator = 'Iterator.prototype.map called on non-iterator';
  SErrorIteratorFilterNonIterator = 'Iterator.prototype.filter called on non-iterator';
  SErrorIteratorTakeNonIterator = 'Iterator.prototype.take called on non-iterator';
  SErrorIteratorDropNonIterator = 'Iterator.prototype.drop called on non-iterator';
  SErrorIteratorFlatMapNonIterator = 'Iterator.prototype.flatMap called on non-iterator';
  SErrorIteratorForEachNonIterator = 'Iterator.prototype.forEach called on non-iterator';
  SErrorIteratorReduceNonIterator = 'Iterator.prototype.reduce called on non-iterator';
  SErrorIteratorToArrayNonIterator = 'Iterator.prototype.toArray called on non-iterator';
  SErrorIteratorSomeNonIterator = 'Iterator.prototype.some called on non-iterator';
  SErrorIteratorEveryNonIterator = 'Iterator.prototype.every called on non-iterator';
  SErrorIteratorFindNonIterator = 'Iterator.prototype.find called on non-iterator';

  // Iterator helper errors — argument validation
  SErrorIteratorMapCallable = 'Iterator.prototype.map requires a callable argument';
  SErrorIteratorFilterCallable = 'Iterator.prototype.filter requires a callable argument';
  SErrorIteratorFlatMapCallable = 'Iterator.prototype.flatMap requires a callable argument';
  SErrorIteratorForEachCallable = 'Iterator.prototype.forEach requires a callable argument';
  SErrorIteratorReduceCallable = 'Iterator.prototype.reduce requires a callable argument';
  SErrorIteratorSomeCallable = 'Iterator.prototype.some requires a callable argument';
  SErrorIteratorEveryCallable = 'Iterator.prototype.every requires a callable argument';
  SErrorIteratorFindCallable = 'Iterator.prototype.find requires a callable argument';
  SErrorIteratorTakeRequiresArg = 'Iterator.prototype.take requires an argument';
  SErrorIteratorDropRequiresArg = 'Iterator.prototype.drop requires an argument';
  SErrorIteratorTakeNonNegative = 'Iterator.prototype.take argument must be non-negative';
  SErrorIteratorDropNonNegative = 'Iterator.prototype.drop argument must be non-negative';
  SErrorReduceEmptyIterator = 'Reduce of empty iterator with no initial value';

  // Iterator.from / Iterator.concat / Iterator.zip / Iterator.zipKeyed
  SErrorIteratorZipInvalidMode = 'Iterator.zip: invalid mode "%s"';
  SErrorIteratorZipKeyedPropertyNotIterable = 'Iterator.zipKeyed: property "%s" value must be iterable';
  SErrorIteratorZipKeyedInvalidMode = 'Iterator.zipKeyed: invalid mode "%s"';
  SErrorIteratorFromRequiresArg = 'Iterator.from requires an argument';
  SErrorIteratorFromRequiresIterable = 'Iterator.from requires an iterable or iterator-like object';
  SErrorIteratorConcatNotIterable = 'Iterator.concat requires all arguments to be iterable';
  SErrorIteratorZipRequiresArg = 'Iterator.zip requires an argument';
  SErrorIteratorZipFirstIterable = 'Iterator.zip: first argument must be iterable';
  SErrorIteratorZipItemIterable = 'Iterator.zip: all items in iterables must be iterable';
  SErrorIteratorZipOptionsObject = 'Iterator.zip: options must be an object';
  SErrorIteratorZipModeString = 'Iterator.zip: mode must be a string';
  SErrorIteratorZipPaddingIterable = 'Iterator.zip: padding must be iterable';
  SErrorIteratorZipKeyedRequiresArg = 'Iterator.zipKeyed requires an argument';
  SErrorIteratorZipKeyedFirstObject = 'Iterator.zipKeyed: first argument must be an object';
  SErrorIteratorZipKeyedOptionsObject = 'Iterator.zipKeyed: options must be an object';
  SErrorIteratorZipKeyedModeString = 'Iterator.zipKeyed: mode must be a string';
  SErrorIteratorZipKeyedPaddingObject = 'Iterator.zipKeyed: padding must be an object';

  // Global array errors
  SErrorArrayFromMapFn = 'Array.from: when provided, the second argument must be a function';
  SErrorIteratorInvalid = '[Symbol.iterator] did not return a valid iterator';
  SErrorIteratorReturnObject = 'Iterator return() must return an object';
  SErrorAsyncIteratorNextNotCallable = 'Async iterator .next is not callable';

  // Temporal API errors — Duration
  SErrorTemporalDurationFromArg = 'Temporal.Duration.from requires a string, Duration, or object';
  SErrorTemporalDurationCompareArg = 'Temporal.Duration.compare requires Duration arguments';
  SErrorInvalidISODuration = 'Invalid ISO duration string';

  // Temporal API errors — Instant
  SErrorTemporalInstantRequiresEpoch = 'Temporal.Instant requires epochNanoseconds argument';
  SErrorTemporalInstantRequiresBigInt = 'Temporal.Instant epochNanoseconds must be a BigInt';
  SErrorTemporalInstantOutOfRange = 'Temporal.Instant epochNanoseconds out of range';
  SErrorInvalidISOInstant = 'Invalid ISO instant string';
  SErrorTemporalInstantFromArg = 'Temporal.Instant.from requires a string or Instant';
  SErrorTemporalInstantFromEpochMillis = 'Temporal.Instant.fromEpochMilliseconds requires an argument';
  SErrorTemporalInstantFromEpochNanos = 'Temporal.Instant.fromEpochNanoseconds requires an argument';
  SErrorTemporalInstantCompareArg = 'Temporal.Instant.compare requires Instant arguments';

  // Temporal API errors — PlainDate
  SErrorTemporalPlainDateArgs = 'Temporal.PlainDate requires year, month, day arguments';
  SErrorInvalidISODate = 'Invalid ISO date string';
  SErrorTemporalPlainDateFromProps = 'Temporal.PlainDate.from requires year, month, day properties';
  SErrorTemporalPlainDateFromArg = 'Temporal.PlainDate.from requires a string, PlainDate, or object';
  SErrorTemporalPlainDateCompareArg = 'Temporal.PlainDate.compare requires PlainDate arguments';

  // Temporal API errors — PlainTime
  SErrorInvalidISOTime = 'Invalid ISO time string';
  SErrorTemporalPlainTimeFromArg = 'Temporal.PlainTime.from requires a string, PlainTime, or object';
  SErrorTemporalPlainTimeCompareArg = 'Temporal.PlainTime.compare requires PlainTime arguments';

  // Temporal API errors — PlainDateTime
  SErrorTemporalPlainDateTimeArgs = 'Temporal.PlainDateTime requires at least year, month, day arguments';
  SErrorInvalidISODateTime = 'Invalid ISO date-time string';
  SErrorTemporalPlainDateTimeFromArg = 'Temporal.PlainDateTime.from requires a string, PlainDateTime, or object';
  SErrorTemporalPlainDateTimeCompareArg = 'Temporal.PlainDateTime.compare requires PlainDateTime arguments';

  // Temporal API errors — PlainYearMonth
  SErrorTemporalPlainYearMonthArgs = 'Temporal.PlainYearMonth requires year, month arguments';
  SErrorTemporalPlainYearMonthFromYear = 'Temporal.PlainYearMonth.from requires year property';
  SErrorInvalidMonthCodeYearMonth = 'Invalid monthCode for Temporal.PlainYearMonth.from';
  SErrorTemporalPlainYearMonthFromMonth = 'Temporal.PlainYearMonth.from requires monthCode or month property';
  SErrorTemporalPlainYearMonthFromArg = 'Temporal.PlainYearMonth.from requires a string, PlainYearMonth, or object';
  SErrorTemporalPlainYearMonthCompareArg = 'Temporal.PlainYearMonth.compare requires PlainYearMonth arguments';
  SErrorInvalidISOYearMonth = 'Invalid ISO year-month string';
  SErrorMonthCodeOutOfRange = 'monthCode month out of range in Temporal.PlainYearMonth.from';
  SErrorMonthCodeMismatch = 'month and monthCode must match in Temporal.PlainYearMonth.from';

  // Temporal API errors — PlainMonthDay
  SErrorTemporalPlainMonthDayArgs = 'Temporal.PlainMonthDay requires month, day arguments';
  SErrorTemporalPlainMonthDayFromDay = 'Temporal.PlainMonthDay.from requires day property';
  SErrorInvalidMonthCodeMonthDay = 'Invalid monthCode for Temporal.PlainMonthDay.from';
  SErrorTemporalPlainMonthDayFromMonth = 'Temporal.PlainMonthDay.from requires a valid month property';
  SErrorTemporalPlainMonthDayFromArg = 'Temporal.PlainMonthDay.from requires a string, PlainMonthDay, or object';
  SErrorTemporalPlainMonthDayCompareArg = 'Temporal.PlainMonthDay.compare requires PlainMonthDay arguments';
  SErrorInvalidISOMonthDay = 'Invalid ISO month-day string';

  // Temporal API errors — ZonedDateTime
  SErrorTemporalZonedDateTimeArgs = 'Temporal.ZonedDateTime requires epochNanoseconds and timeZone arguments';
  SErrorTemporalZonedDateTimeRequiresBigInt = 'Temporal.ZonedDateTime epochNanoseconds must be a BigInt';
  SErrorTemporalZonedDateTimeFromArg = 'Temporal.ZonedDateTime.from requires a string or ZonedDateTime';
  SErrorTemporalZonedDateTimeCompareArg = 'Temporal.ZonedDateTime.compare requires ZonedDateTime or string arguments';
  SErrorInvalidISOZonedDateTime = 'Invalid ISO zoned date-time string';
  SErrorTemporalZonedDateTimeRequiresTZ = 'Temporal.ZonedDateTime.from requires a timezone annotation (e.g., [UTC])';
  SErrorTemporalZonedDateTimeCompareTZ = 'Temporal.ZonedDateTime.compare requires a timezone annotation (e.g., [UTC])';

  // Temporal API errors — range
  SErrorTemporalMonthOutOfRange = 'Month %d out of range';
  SErrorTemporalDayOutOfRange = 'Day %d out of range';
  SErrorTemporalDayOutOfRangeForMonth = 'Day %d out of range for month %d';

  // Temporal prototype errors — shared
  SErrorInvalidDurationString = 'Invalid duration string';
  SErrorInvalidTimeString = 'Invalid time string';
  SErrorTemporalValueOf = 'Temporal.%s.prototype.valueOf cannot be used; use %s instead';
  SErrorTemporalWithRequiresObject = '%s.prototype.with requires an object argument';
  SErrorTemporalAddRequiresDuration = '%s.prototype.add requires a Duration or string';
  SErrorTemporalSubtractRequiresDuration = '%s.prototype.subtract requires a Duration or string';
  SErrorTemporalRoundRequiresStringOrOptions = '%s.prototype.round requires a string or options object';
  SErrorTemporalRoundRequiresSmallestUnit = 'round() requires a smallestUnit option';
  SErrorTemporalInvalidUnitFor = 'Invalid unit for %s: %s';
  SErrorTemporalInvalidISOStringFor = 'Invalid ISO %s string for %s';

  // Temporal prototype errors — Duration
  SErrorDurationMixedSigns = 'Duration fields must not have mixed signs';
  SErrorDurationCalendarOutOfRange = 'Duration calendar fields (years, months, weeks) must have absolute value less than 2^32';
  SErrorDurationTimeOutOfRange = 'Duration time fields out of range: normalized seconds must have absolute value less than 2^53';
  SErrorDurationRoundRequiresUnit = 'round() requires at least a smallestUnit or largestUnit';
  SErrorDurationRoundLargestSmallerThanSmallest = 'largestUnit must not be smaller than smallestUnit';
  SErrorDurationRoundRequiresRelativeTo = 'Duration with years or months requires relativeTo for round()';
  SErrorDurationTotalRequiresUnit = 'total() requires a unit option';
  SErrorDurationTotalRequiresRelativeTo = 'Duration with years or months requires relativeTo for total()';
  SErrorDurationTotalRequiresStringOrOptions = 'Duration.prototype.total requires a string or options object';
  SErrorTemporalInvalidRelativeTo = 'Invalid relativeTo value for %s';
  SErrorInvalidDurationAddArg = 'Invalid argument to Duration.prototype.add';
  SErrorInvalidDurationSubtractArg = 'Invalid argument to Duration.prototype.subtract';

  // Temporal prototype errors — Instant
  SErrorInstantAddNoCalendar = 'Instant.prototype.add does not support years, months, or weeks';
  SErrorInstantSubtractNoCalendar = 'Instant.prototype.subtract does not support years, months, or weeks';

  // Temporal prototype errors — PlainDate
  SErrorInvalidDate = 'Invalid date: %s';
  SErrorPlainDateToZonedRequiresTZ = 'PlainDate.prototype.toZonedDateTime requires a timeZone';
  SErrorPlainDateToZonedRequiresStringOrOptions = 'PlainDate.prototype.toZonedDateTime requires a timezone string or options object';

  // Temporal prototype errors — PlainTime
  SErrorInvalidTime = 'Invalid time';

  // Temporal prototype errors — PlainDateTime
  SErrorInvalidDateInPlainDateTime = 'Invalid date in PlainDateTime';
  SErrorInvalidTimeInPlainDateTime = 'Invalid time in PlainDateTime';
  SErrorPlainDateTimeToZonedRequiresTZ = 'PlainDateTime.prototype.toZonedDateTime requires a timeZone';
  SErrorPlainDateTimeToZonedRequiresStringOrOptions = 'PlainDateTime.prototype.toZonedDateTime requires a timezone string or options object';
  SErrorPlainDateTimeToZonedUnknownTZ = 'PlainDateTime.prototype.toZonedDateTime: unknown timezone %s';
  SErrorPlainDateTimeWithPlainTimeArg = 'PlainDateTime.prototype.withPlainTime requires a PlainTime or string';

  // Temporal prototype errors — PlainYearMonth
  SErrorInvalidMonth = 'Invalid month: %s';
  SErrorInvalidYearMonthStringFor = 'Invalid year-month string for %s';
  SErrorPlainYearMonthToPlainDateRequiresDay = 'PlainYearMonth.prototype.toPlainDate requires an object with a day property';
  SErrorMonthCodeOutOfRangeIn = 'monthCode month out of range in %s';
  SErrorMonthCodeMismatchIn = 'month and monthCode must match in %s';

  // Temporal prototype errors — PlainMonthDay
  SErrorInvalidMonthDay = 'Invalid month-day: %s';
  SErrorInvalidMonthDayStringFor = 'Invalid month-day string for %s';
  SErrorInvalidMonthCodeFor = 'Invalid monthCode for %s';
  SErrorPlainMonthDayToPlainDateRequiresYear = 'PlainMonthDay.prototype.toPlainDate requires an object with a year property';

  // Temporal prototype errors — ZonedDateTime
  SErrorZonedDateTimeRequiresTZ = 'ZonedDateTime requires a timezone';
  SErrorUnknownTimezone = 'Unknown timezone: %s';
  SErrorZonedDateTimeRequiresTZAnnotation = 'ZonedDateTime requires a timezone annotation in the string';
  SErrorInvalidDateStringForZDT = 'Invalid date string for ZonedDateTime.prototype.withPlainDate';
  SErrorZDTWithPlainDateArg = 'ZonedDateTime.prototype.withPlainDate requires a PlainDate or string';
  SErrorInvalidTimeStringForZDT = 'Invalid time string for ZonedDateTime.prototype.withPlainTime';
  SErrorZDTWithPlainTimeArg = 'ZonedDateTime.prototype.withPlainTime requires a PlainTime or string';
  SErrorZDTWithTimeZoneArg = 'ZonedDateTime.prototype.withTimeZone requires a timezone argument';
  SErrorZDTWithTimeZoneNonEmpty = 'ZonedDateTime.prototype.withTimeZone requires a non-empty timezone';
  SErrorInvalidZDTRoundUnit = 'Invalid unit for ZonedDateTime.prototype.round';

  // Temporal options errors
  SErrorInvalidSmallestUnit = 'Invalid smallestUnit: %s';
  SErrorInvalidLargestUnit = 'Invalid largestUnit: %s';
  SErrorInvalidRoundingMode = 'Invalid roundingMode: %s';
  SErrorRoundingIncrementMin = 'roundingIncrement must be >= 1';
  SErrorRoundingIncrementDivisor = 'roundingIncrement %d does not divide the maximum value %d for this unit';
  SErrorInvalidOverflow = 'Invalid overflow option: %s';
  SErrorInvalidFractionalDigits = 'Invalid fractionalSecondDigits: %s';
  SErrorFractionalDigitsRange = 'fractionalSecondDigits must be 0-9 or "auto"';
  SErrorCannotConvertCalendarUnit = 'Cannot convert calendar unit to nanoseconds';

  // TypedArray errors
  SErrorTypedArrayRequiresTypedArray = '%s requires a TypedArray';
  SErrorTypedArraySetRequiresArg = 'TypedArray.prototype.set requires at least one argument';
  SErrorTypedArraySetOffsetNonNegative = 'TypedArray.prototype.set offset must be >= 0';
  SErrorTypedArraySetArgType = 'TypedArray.prototype.set argument must be an array or typed array';
  SErrorTypedArraySourceTooLarge = 'Source is too large';
  SErrorTypedArrayFindCallable = 'TypedArray.prototype.find requires a callable argument';
  SErrorTypedArrayFindIndexCallable = 'TypedArray.prototype.findIndex requires a callable argument';
  SErrorTypedArrayFindLastCallable = 'TypedArray.prototype.findLast requires a callable argument';
  SErrorTypedArrayFindLastIndexCallable = 'TypedArray.prototype.findLastIndex requires a callable argument';
  SErrorTypedArrayEveryCallable = 'TypedArray.prototype.every requires a callable argument';
  SErrorTypedArraySomeCallable = 'TypedArray.prototype.some requires a callable argument';
  SErrorTypedArrayForEachCallable = 'TypedArray.prototype.forEach requires a callable argument';
  SErrorTypedArrayMapCallable = 'TypedArray.prototype.map requires a callable argument';
  SErrorTypedArrayFilterCallable = 'TypedArray.prototype.filter requires a callable argument';
  SErrorTypedArrayReduceCallable = 'TypedArray.prototype.reduce requires a callable argument';
  SErrorTypedArrayReduceRightCallable = 'TypedArray.prototype.reduceRight requires a callable argument';
  SErrorReduceEmptyTypedArray = 'Reduce of empty typed array with no initial value';
  SErrorTypedArrayFromRequiresArg = 'TypedArray.from requires at least one argument';
  SErrorTypedArrayFromMapFn = 'TypedArray.from mapfn must be a function';
  SErrorTypedArrayFromSource = 'TypedArray.from requires an array-like or iterable source';
  SErrorInvalidTypedArrayIndex = 'Invalid index';
  SErrorInvalidTypedArrayLength = 'Invalid typed array length';
  SErrorTypedArrayStartOffsetNonNegative = 'Start offset of %s must be non-negative';
  SErrorTypedArrayStartOffsetMultiple = 'Start offset of %s should be a multiple of %d';
  SErrorTypedArrayStartOffsetBounds = 'Start offset is outside the bounds of the buffer';
  SErrorTypedArrayByteLengthMultiple = 'Byte length of %s should be a multiple of %d';
  SErrorBigIntTypedArrayRequiresBigInt = 'Cannot convert a non-BigInt value to a BigInt; use BigInt() to wrap';
  SErrorBigIntTypedArrayCannotMix = 'Cannot mix BigInt and non-BigInt typed arrays';

  // Proxy trap errors
  SErrorProxyRevoked = 'Cannot perform operation on a revoked proxy';
  SErrorProxyTrapNotFunction = 'Proxy handler trap ''%s'' is not a function';
  SErrorProxyTrapNotCallable = 'Proxy trap is not callable';
  SErrorProxyGetNonConfigurableValue = 'Proxy get: value mismatch for non-configurable, non-writable property ''%s''';
  SErrorProxyGetNoGetter = 'Proxy get: must return undefined for non-configurable accessor without getter ''%s''';
  SErrorProxySetReturnedFalse = 'Proxy set handler returned false for property ''%s''';
  SErrorProxySetNonConfigurableValue = 'Proxy set: cannot change value of non-configurable, non-writable property ''%s''';
  SErrorProxySetNoSetter = 'Proxy set: cannot set non-configurable accessor property ''%s'' without a setter';
  SErrorProxyHasNonConfigurable = 'Proxy has trap returned false for non-configurable property ''%s''';
  SErrorProxyHasNonExtensible = 'Proxy has trap returned false for property on non-extensible target';
  SErrorProxyHasSymbolNonConfigurable = 'Proxy has trap returned false for non-configurable symbol property';
  SErrorProxyHasSymbolNonExtensible = 'Proxy has trap returned false for symbol property on non-extensible target';
  SErrorProxyGetSymbolNonConfigurable = 'Proxy get: value mismatch for non-configurable, non-writable symbol property';
  SErrorProxyGetSymbolNoGetter = 'Proxy get: must return undefined for non-configurable accessor without getter';
  SErrorProxySetSymbolNonConfigurable = 'Proxy set: cannot change value of non-configurable, non-writable symbol property';
  SErrorProxySetSymbolNoSetter = 'Proxy set: cannot set non-configurable accessor symbol property without a setter';
  SErrorProxyDeleteNonConfigurable = 'Proxy deleteProperty trap returned true for non-configurable property ''%s''';
  SErrorProxyDeleteNonExtensible = 'Proxy deleteProperty trap returned true for property on non-extensible target';
  SErrorProxyGetOwnNonConfigurable = 'Proxy getOwnPropertyDescriptor returned undefined for non-configurable property ''%s''';
  SErrorProxyGetOwnNonExtensible = 'Proxy getOwnPropertyDescriptor returned undefined for property on non-extensible target';
  SErrorProxyGetOwnReturnType = 'Proxy getOwnPropertyDescriptor must return an object or undefined';
  SErrorProxyDefineReturnedFalse = 'Proxy defineProperty handler returned false for property ''%s''';
  SErrorProxyDefineNonObject = 'Cannot define property on non-object target';
  SErrorProxyOwnKeysArray = 'Proxy ownKeys must return an array';
  SErrorProxyOwnKeysTypes = 'Proxy ownKeys trap result must contain only strings and symbols';
  SErrorProxyOwnKeysMissing = 'Proxy ownKeys trap result must include non-configurable property ''%s''';
  SErrorProxyGetProtoReturnType = 'Proxy getPrototypeOf trap must return an object or null';
  SErrorProxyGetProtoMismatch = 'Proxy getPrototypeOf trap result does not match non-extensible target prototype';
  SErrorProxySetProtoNonExtensible = 'Proxy setPrototypeOf trap returned true but target is non-extensible';
  SErrorProxyIsExtensibleMismatch = 'Proxy isExtensible trap result does not match target extensibility';
  SErrorProxyPreventExtensionsFalse = 'Proxy preventExtensions handler returned false';
  SErrorProxyPreventExtensionsStillExtensible = 'Proxy preventExtensions trap returned true but target is still extensible';
  SErrorProxyApplyNonFunction = 'Proxy apply trap called on non-function target';
  SErrorProxyTargetNotCallable = 'Proxy target is not callable';
  SErrorProxyTargetNotConstructor = 'Proxy target is not a constructor';
  SErrorProxyConstructReturnType = 'Proxy construct handler must return an object';

  // FFI errors
  SErrorFFIFuncArgCount = 'FFI function %s expects %d arguments, got %d';
  SErrorFFIArgMustBeBufferOrNull = 'FFI argument %d must be an ArrayBuffer, TypedArray, FFIPointer, or null';
  SErrorFFIBindRequiresNameAndSig = 'bind requires a function name and a signature object';
  SErrorFFIBindSigObject = 'bind second argument must be a signature object { args: [...], returns: "..." }';
  SErrorFFIUnknownType = 'Unknown FFI type: %s';
  SErrorFFIVoidNotValidArg = 'void is not a valid argument type';
  SErrorFFISigArgsMustBeArray = 'signature args must be an array of type strings';
  SErrorFFIUnknownReturnType = 'Unknown FFI return type: %s';
  SErrorFFISigReturnsMustBeString = 'signature returns must be a type string';
  SErrorFFIBindRequiresLibrary = 'bind requires an FFILibrary';
  SErrorFFIBindLibraryClosed = 'Cannot bind from a closed library';
  SErrorFFISymbolRequiresLibrary = 'symbol requires an FFILibrary';
  SErrorFFISymbolLibraryClosed = 'Cannot look up symbol in a closed library';
  SErrorFFISymbolRequiresName = 'symbol requires a symbol name';
  SErrorFFICloseRequiresLibrary = 'close requires an FFILibrary';
  SErrorFFIPathRequiresLibrary = 'FFILibrary.path requires an FFILibrary';
  SErrorFFIClosedRequiresLibrary = 'FFILibrary.closed requires an FFILibrary';

  // URLSearchParams errors
  SErrorURLSearchParamsNotSequence = 'Failed to construct URLSearchParams: The provided value cannot be converted to a sequence';
  SErrorURLSearchParamsPairSize = 'Failed to construct URLSearchParams: Sequence initializer must contain pairs with exactly two items';
  SErrorURLSearchParamsAppendNotInstance = 'URLSearchParams.prototype.append: not a URLSearchParams';
  SErrorURLSearchParamsDeleteNotInstance = 'URLSearchParams.prototype.delete: not a URLSearchParams';
  SErrorURLSearchParamsGetNotInstance = 'URLSearchParams.prototype.get: not a URLSearchParams';
  SErrorURLSearchParamsGetAllNotInstance = 'URLSearchParams.prototype.getAll: not a URLSearchParams';
  SErrorURLSearchParamsHasNotInstance = 'URLSearchParams.prototype.has: not a URLSearchParams';
  SErrorURLSearchParamsSetNotInstance = 'URLSearchParams.prototype.set: not a URLSearchParams';
  SErrorURLSearchParamsSortNotInstance = 'URLSearchParams.prototype.sort: not a URLSearchParams';
  SErrorURLSearchParamsToStringNotInstance = 'URLSearchParams.prototype.toString: not a URLSearchParams';
  SErrorURLSearchParamsKeysNotInstance = 'URLSearchParams.prototype.keys: not a URLSearchParams';
  SErrorURLSearchParamsValuesNotInstance = 'URLSearchParams.prototype.values: not a URLSearchParams';
  SErrorURLSearchParamsEntriesNotInstance = 'URLSearchParams.prototype.entries: not a URLSearchParams';
  SErrorURLSearchParamsForEachNotInstance = 'URLSearchParams.prototype.forEach: not a URLSearchParams';
  SErrorURLSearchParamsForEachNotCallable = 'URLSearchParams.prototype.forEach: callback is not a function';
  SErrorURLSearchParamsSizeNotInstance = 'URLSearchParams size getter: not a URLSearchParams';
  SErrorURLSearchParamsIteratorNotInstance = 'URLSearchParams[Symbol.iterator]: not a URLSearchParams';

  // URL errors
  SErrorURLNotURL = 'URL %s: not a URL';
  SErrorURLConstructorRequiresArg = 'URL constructor: 1 argument required';
  SErrorInvalidBaseURL = 'Invalid base URL: %s';
  SErrorInvalidURL = 'Invalid URL: %s';

  // String method errors
  SErrorSymbolReplaceNotCallable = '@@replace is not callable';
  SErrorReplaceAllRequiresGlobalRegExp = 'String.prototype.replaceAll requires a global RegExp';
  SErrorSymbolSplitNotCallable = '@@split is not callable';
  SErrorSymbolMatchNotCallable = '@@match is not callable';
  SErrorMatchAllRequiresGlobalRegExp = 'String.prototype.matchAll requires a global RegExp';
  SErrorSymbolMatchAllNotCallable = '@@matchAll is not callable';
  SErrorSymbolSearchNotCallable = '@@search is not callable';
  SErrorInvalidRepeatCount = 'Invalid count value: %s';
  SErrorInvalidNormalizationForm = 'The normalization form should be one of NFC, NFD, NFKC, NFKD';
  SErrorStringPrototypeRequiresNonNullish = 'String.prototype method requires that ''this'' not be null or undefined';
  SErrorIsWellFormedRequiresNonNullish = 'String.prototype.isWellFormed requires that ''this'' not be null or undefined';
  SErrorToWellFormedRequiresNonNullish = 'String.prototype.toWellFormed requires that ''this'' not be null or undefined';

  // ArrayBuffer errors
  SErrorRequiresArrayBuffer = '%s requires an ArrayBuffer';
  SErrorInvalidArrayBufferLength = 'Invalid array buffer length';
  SErrorArrayBufferExceedsMaxByteLength = 'ArrayBuffer byte length must not exceed maxByteLength';
  SErrorCannotResizeDetachedArrayBuffer = 'Cannot resize a detached ArrayBuffer';
  SErrorCannotResizeFixedLengthArrayBuffer = 'Cannot resize a fixed-length ArrayBuffer';
  SErrorArrayBufferResizeExceedsMax = 'ArrayBuffer resize exceeds maxByteLength';
  SErrorCannotTransferDetachedArrayBuffer = 'Cannot transfer a detached ArrayBuffer';
  SErrorCannotSliceDetachedArrayBuffer = 'Cannot slice a detached ArrayBuffer';

  // RegExp errors
  SErrorRegExpEscapeRequiresString = 'RegExp.escape requires a string argument';
  SErrorRegExpEscapeArgMustBeString = 'First argument to RegExp.escape must be a string';
  SErrorRegExpExecNonRegExp = 'RegExp.prototype.exec called on non-RegExp object';
  SErrorRegExpTestNonRegExp = 'RegExp.prototype.test called on non-RegExp object';
  SErrorRegExpToStringNonRegExp = 'RegExp.prototype.toString called on non-RegExp object';
  SErrorRegExpMatchNonRegExp = 'RegExp.prototype[Symbol.match] called on non-RegExp object';
  SErrorRegExpMatchAllNonRegExp = 'RegExp.prototype[Symbol.matchAll] called on non-RegExp object';
  SErrorRegExpReplaceNonRegExp = 'RegExp.prototype[Symbol.replace] called on non-RegExp object';
  SErrorRegExpSearchNonRegExp = 'RegExp.prototype[Symbol.search] called on non-RegExp object';
  SErrorRegExpSplitNonRegExp = 'RegExp.prototype[Symbol.split] called on non-RegExp object';

  // VM runtime errors
  SErrorCannotAssignReadOnly = 'Cannot assign to read only property ''%s''';
  SErrorTypeNotAssignable = 'Type ''%s'' is not assignable to type ''%s''';
  SErrorModuleNotAvailableInVM = 'Module loading is not available in TGocciaVM';
  SErrorEnumMemberType = 'Enum member ''%s'' must be a number, string, or symbol';
  SErrorClassDecoratorReturn = 'Class decorator must return a class or undefined';
  SErrorMethodDecoratorReturn = 'Method decorator must return a function or undefined';
  SErrorGetterDecoratorReturn = 'Getter decorator must return a function or undefined';
  SErrorSetterDecoratorReturn = 'Setter decorator must return a function or undefined';
  SErrorFieldDecoratorReturn = 'Field decorator must return a function or undefined';
  SErrorAccessorDecoratorReturn = 'Accessor decorator must return an object or undefined';
  SErrorCannotReadPropertiesOfNull = 'Cannot read properties of null (reading ''%s'')';
  SErrorCannotReadPropertiesOfUndefined = 'Cannot read properties of undefined (reading ''%s'')';
  SErrorCannotSetPropertiesOfNull = 'Cannot set properties of null (setting ''%s'')';
  SErrorCannotSetPropertiesOfUndefined = 'Cannot set properties of undefined (setting ''%s'')';
  SErrorPrivateAccessorNoGetter = 'Private accessor %s was defined without a getter';
  SErrorPrivateAccessorNoSetter = 'Private accessor %s was defined without a setter';
  SErrorPrivateFieldNotAccessible = 'Private field %s is not accessible';
  SErrorCannotUseInOperator = 'Cannot use ''in'' operator to search for ''%s'' in %s';
  SErrorCannotDestructureNotObject = 'Cannot destructure %s as it is not an object';
  SErrorMaxCallStackExceeded = 'Maximum call stack size exceeded';

  // Uint8Array encoding errors
  SErrorRequiresUint8Array = '%s requires that |this| be a Uint8Array';
  SErrorInvalidAlphabet = 'Invalid alphabet: expected "base64" or "base64url"';
  SErrorInvalidLastChunkHandling = 'Invalid lastChunkHandling: expected "loose", "strict", or "stop-before-partial"';
  SErrorInvalidBase64Character = 'Invalid character in base64 string';
  SErrorBase64IncompleteChunk = 'Invalid base64 string: incomplete chunk';
  SErrorBase64NonZeroPaddingBits = 'Invalid base64 string: non-zero padding bits';
  SErrorBase64MalformedPadding = 'Invalid base64 string: malformed padding';
  SErrorBase64IncompleteChunkStrict = 'Invalid base64 string: incomplete chunk in strict mode';
  SErrorBase64UnexpectedPadding = 'Invalid base64 string: unexpected padding';
  SErrorToBase64OptionsNotObject = 'Uint8Array.prototype.toBase64: options must be an object';
  SErrorSetFromBase64RequiresString = 'Uint8Array.prototype.setFromBase64 requires a string argument';
  SErrorSetFromBase64FirstArgString = 'Uint8Array.prototype.setFromBase64: first argument must be a string';
  SErrorSetFromBase64OptionsNotObject = 'Uint8Array.prototype.setFromBase64: options must be an object';
  SErrorSetFromHexRequiresString = 'Uint8Array.prototype.setFromHex requires a string argument';
  SErrorSetFromHexFirstArgString = 'Uint8Array.prototype.setFromHex: first argument must be a string';
  SErrorInvalidHexOddLength = 'Invalid hex string: odd length';
  SErrorInvalidHexCharacter = 'Invalid character in hex string';
  SErrorFromBase64RequiresString = 'Uint8Array.fromBase64 requires a string argument';
  SErrorFromBase64FirstArgString = 'Uint8Array.fromBase64: first argument must be a string';
  SErrorFromBase64OptionsNotObject = 'Uint8Array.fromBase64: options must be an object';
  SErrorFromHexRequiresString = 'Uint8Array.fromHex requires a string argument';
  SErrorFromHexFirstArgString = 'Uint8Array.fromHex: first argument must be a string';

  // TextDecoder errors
  SErrorTextDecoderInvalidEncoding = 'TextDecoder: The encoding label provided (%s) is invalid.';
  SErrorTextDecoderOptionsMustBeObject = 'TextDecoder: options must be an object or undefined';
  SErrorTextDecoderEncodingIllegalInvocation = 'TextDecoder.prototype.encoding: illegal invocation';
  SErrorTextDecoderFatalIllegalInvocation = 'TextDecoder.prototype.fatal: illegal invocation';
  SErrorTextDecoderIgnoreBOMIllegalInvocation = 'TextDecoder.prototype.ignoreBOM: illegal invocation';
  SErrorTextDecoderDecodeIllegalInvocation = 'TextDecoder.prototype.decode: illegal invocation';
  SErrorTextDecoderDecodeInputType = 'TextDecoder.prototype.decode: input must be an ArrayBuffer or ArrayBufferView';
  SErrorTextDecoderDecodeInvalidUTF8 = 'TextDecoder.prototype.decode: invalid UTF-8 byte sequence';

  // SharedArrayBuffer errors
  SErrorInvalidSharedArrayBufferLength = 'Invalid shared array buffer length';
  SErrorRequiresSharedArrayBuffer = '%s requires a SharedArrayBuffer';
  SErrorSharedArrayBufferSpeciesReturnedThis = 'SharedArrayBuffer subclass returned this from species constructor';

  // Reflect errors
  SErrorReflectTargetMustBeObject = '%s: target must be an object';
  SErrorReflectApplyTargetMustBeFunction = 'Reflect.apply: target must be a function';
  SErrorReflectConstructTargetMustBeConstructor = 'Reflect.construct: target must be a constructor';
  SErrorReflectConstructNewTargetMustBeConstructor = 'Reflect.construct: newTarget must be a constructor';
  SErrorReflectDefinePropertyAttrsMustBeObject = 'Reflect.defineProperty: attributes must be an object';
  SErrorReflectSetPrototypeOfProtoType = 'Reflect.setPrototypeOf: proto must be an object or null';

  // URI errors
  SErrorURIMalformed = 'URI malformed';

  // Proxy constructor errors
  SErrorProxyRequiresNew = 'Constructor Proxy requires ''new''';
  SErrorProxyNonObjectTargetOrHandler = 'Cannot create proxy with a non-object as target or handler';

  // Class errors
  SErrorClassSetterOnlyAccessor = 'Cannot set property on class with getter-only accessor';
  SErrorClassConstructorRequiresNew = 'Class constructor %s cannot be invoked without ''new''';

  // JSON errors
  SErrorJSONParseArgMustBeString = 'JSON.parse: argument must be a string';
  SErrorJSONRawJSONEmptyString = 'JSON.rawJSON: empty string is not valid JSON';
  SErrorJSONRawJSONLeadingWhitespace = 'JSON.rawJSON: input must not have leading whitespace';
  SErrorJSONRawJSONTrailingWhitespace = 'JSON.rawJSON: input must not have trailing whitespace';
  SErrorJSONRawJSONInvalid = 'JSON.rawJSON: %s';
  SErrorJSONRawJSONMustBePrimitive = 'JSON.rawJSON: value must be a JSON primitive (not an object or array)';
  SErrorJSONCircularStructure = 'Converting circular structure to JSON';
  SErrorJSONStringifyError = 'JSON.stringify error: %s';

  // DisposableStack errors
  SErrorDisposableStackIncompatibleReceiver = 'DisposableStack method called on incompatible receiver';
  SErrorDisposableStackAlreadyDisposed = 'DisposableStack has already been disposed';
  SErrorValueNotDisposable = 'The value is not disposable (missing [Symbol.dispose])';
  SErrorValueNotAsyncDisposable = 'The value is not disposable (missing [Symbol.asyncDispose] and [Symbol.dispose])';
  SErrorOnDisposeMustBeFunction = 'The onDispose argument must be a function';

  // Global function errors — queueMicrotask
  SErrorQueueMicrotaskArgRequired = 'Failed to execute ''queueMicrotask'': 1 argument required, but only 0 present.';
  SErrorQueueMicrotaskNotFunction = 'Failed to execute ''queueMicrotask'': parameter 1 is not of type ''Function''.';

  // Global function errors — structuredClone
  SErrorStructuredCloneArgRequired = 'Failed to execute ''structuredClone'': 1 argument required, but only 0 present.';
  SErrorStructuredCloneNotCloneable = '%s could not be cloned.';
  SErrorStructuredCloneValueNotCloneable = 'value could not be cloned.';

  // Property descriptor errors
  SErrorPropertyDescriptorMustBeObject = 'property descriptor must be an object';
  SErrorGetterMustBeFunctionOrUndefined = 'getter must be a function or undefined';
  SErrorSetterMustBeFunctionOrUndefined = 'setter must be a function or undefined';
  SErrorDescriptorMixedAccessorData = 'descriptor cannot have both accessor and data properties';

  // JSON5 errors
  SErrorCircularStructureToJSON5 = 'Converting circular structure to JSON5';
  SErrorJSON5ParseArgMustBeString = 'JSON5.parse: argument must be a string';

  // String static method errors
  SErrorInvalidCodePoint = 'Invalid code point';
  SErrorNotValidCodePoint = '%s is not a valid code point';
  SErrorCannotConvertToObject = 'Cannot convert %s to object';

  // Promise errors
  SErrorPromiseResolverUndefinedNotFunction = 'Promise resolver undefined is not a function';
  SErrorPromiseResolverNotFunction = 'Promise resolver is not a function';
  SErrorPromiseTryRequiresCallback = 'Promise.try requires a callback function';
  SErrorAllPromisesRejected = 'All promises were rejected';

  // Object static method errors
  SErrorCannotConvertValueToObject = 'Cannot convert value to object';
  SErrorObjectPrototypeMustBeObjectOrNull = 'Object prototype may only be an Object or null';

  // Object property errors
  SErrorCannotConvertNullOrUndefined = 'Cannot convert undefined or null to object';
  SErrorReadOnlyPropertyFrozen = 'Cannot assign to read only property ''%s'' of frozen object';
  SErrorSetPropertyOnlyGetter = 'Cannot set property %s of #<%s> which has only a getter';
  SErrorProtoChainDepthExceeded = 'Prototype chain depth exceeded safety limit while setting ''%s''';
  SErrorCannotAssignNonExistent = 'Cannot assign to non-existent property ''%s''';
  SErrorCannotAddPropertyNotExtensible = 'Cannot add property ''%s'', object is not extensible';
  SErrorCannotRedefineNonConfigurable = 'Cannot redefine non-configurable property ''%s''';
  SErrorCannotAssignFrozenObject = 'Cannot assign to property of frozen object';
  SErrorSetSymbolOnlyGetter = 'Cannot set property which has only a getter';
  SErrorReadOnlySymbolProperty = 'Cannot assign to read only symbol property';
  SErrorCannotAddSymbolNotExtensible = 'Cannot add symbol property, object is not extensible';

  // TextEncoder errors
  SErrorTextEncoderIllegalInvocation = '%s: illegal invocation';
  SErrorTextEncoderEncodeIntoArgs = 'TextEncoder.prototype.encodeInto requires source and destination arguments';
  SErrorTextEncoderEncodeIntoDestType = 'TextEncoder.prototype.encodeInto: destination must be a Uint8Array';

  // Promise prototype errors
  SErrorPromiseThenNonPromise = 'Promise.prototype.then called on non-Promise';
  SErrorPromiseFinallyNonPromise = 'Promise.prototype.finally called on non-Promise';
  SErrorThenNotFunction = 'then is not a function';
  SErrorPromiseChainingCycle = 'Chaining cycle detected for promise';

  // Disposal method errors
  SErrorDisposePropertyNotFunction = 'Property [Symbol.%s] is not a function';

  // Symbol prototype errors
  SErrorSymbolConstructorRequiresCall = 'Symbol is not a constructor';
  SErrorSymbolProtoToStringRequiresSymbol = 'Symbol.prototype.toString requires that ''this'' be a Symbol';
  SErrorSymbolProtoDescriptionRequiresSymbol = 'Symbol.prototype.description requires that ''this'' be a Symbol';
  SErrorSymbolProtoValueOfRequiresSymbol = 'Symbol.prototype.valueOf requires that ''this'' be a Symbol';
  SErrorSymbolProtoToPrimitiveRequiresSymbol = 'Symbol.prototype[@@toPrimitive] requires that ''this'' be a Symbol';

  // Iterator zip strict errors
  SErrorIteratorZipStrictLengthMismatch = 'Iterator.zip: iterables have different lengths in strict mode';
  SErrorIteratorZipKeyedStrictLengthMismatch = 'Iterator.zipKeyed: iterables have different lengths in strict mode';

  // Iterator lazy errors
  SErrorIteratorFlatMapMustReturnIterable = 'Iterator.prototype.flatMap callback must return an iterable';

  // Generic iterator errors
  SErrorIteratorReturnMustBeCallable = 'Iterator return property must be callable';
  SErrorIteratorNextMustBeCallable = 'Iterator next property must be callable';

  // Iterator concat errors
  SErrorIteratorConcatMustReturnObject = 'Iterator.concat: [Symbol.iterator]() must return an object';

  // FFIPointer errors
  SErrorFFIPointerRequiresFFIPointer = 'FFIPointer.%s requires an FFIPointer';

  // Performance errors
  SErrorPerformanceIncompatibleReceiver = 'Method Performance called on incompatible receiver';
  SErrorIllegalConstructor = 'Illegal constructor';

  // Math.sumPrecise errors
  SErrorMathSumPreciseNotNumber = 'Math.sumPrecise: element is not a number';
  SErrorMathSumPreciseNotIterable = 'Math.sumPrecise: argument is not iterable';

  // JSONL errors
  SErrorJSONLParseArgType = 'JSONL.parse: argument must be a string or Uint8Array';
  SErrorJSONLParseChunkArgType = 'JSONL.parseChunk: argument must be a string or Uint8Array';

  // ToPrimitive errors
  SErrorCannotConvertToPrimitive = 'Cannot convert object to primitive value';

  // Number method errors
  SErrorToExponentialArgRange = 'toExponential() argument must be between 0 and 100';

  // Set brand-check errors
  SErrorSetHasNonSet = 'Set.prototype.has called on non-Set object';
  SErrorSetAddNonSet = 'Set.prototype.add called on non-Set object';
  SErrorSetDeleteNonSet = 'Set.prototype.delete called on non-Set object';
  SErrorSetClearNonSet = 'Set.prototype.clear called on non-Set object';
  SErrorSetForEachNonSet = 'Set.prototype.forEach called on non-Set object';
  SErrorSetValuesNonSet = 'Set.prototype.values called on non-Set object';
  SErrorSetKeysNonSet = 'Set.prototype.keys called on non-Set object';
  SErrorSetEntriesNonSet = 'Set.prototype.entries called on non-Set object';
  SErrorSetIteratorNonSet = 'Set.prototype[Symbol.iterator] called on non-Set object';
  SErrorSetUnionNonSet = 'Set.prototype.union called on non-Set object';
  SErrorSetIntersectionNonSet = 'Set.prototype.intersection called on non-Set object';
  SErrorSetDifferenceNonSet = 'Set.prototype.difference called on non-Set object';
  SErrorSetSymmetricDifferenceNonSet = 'Set.prototype.symmetricDifference called on non-Set object';
  SErrorSetIsSubsetOfNonSet = 'Set.prototype.isSubsetOf called on non-Set object';
  SErrorSetIsSupersetOfNonSet = 'Set.prototype.isSupersetOf called on non-Set object';
  SErrorSetIsDisjointFromNonSet = 'Set.prototype.isDisjointFrom called on non-Set object';

  // Map brand-check errors
  SErrorMapGetNonMap = 'Map.prototype.get called on non-Map object';
  SErrorMapSetNonMap = 'Map.prototype.set called on non-Map object';
  SErrorMapHasNonMap = 'Map.prototype.has called on non-Map object';
  SErrorMapDeleteNonMap = 'Map.prototype.delete called on non-Map object';
  SErrorMapClearNonMap = 'Map.prototype.clear called on non-Map object';
  SErrorMapForEachNonMap = 'Map.prototype.forEach called on non-Map object';
  SErrorMapKeysNonMap = 'Map.prototype.keys called on non-Map object';
  SErrorMapValuesNonMap = 'Map.prototype.values called on non-Map object';
  SErrorMapEntriesNonMap = 'Map.prototype.entries called on non-Map object';
  SErrorMapIteratorNonMap = 'Map.prototype[Symbol.iterator] called on non-Map object';
  SErrorMapGetOrInsertNonMap = 'Map.prototype.getOrInsert called on non-Map object';
  SErrorMapGetOrInsertComputedNonMap = 'Map.prototype.getOrInsertComputed called on non-Map object';

  // WeakMap/WeakSet brand-check and key validation errors
  SErrorWeakMapGetNonWeakMap = 'WeakMap.prototype.get called on non-WeakMap object';
  SErrorWeakMapSetNonWeakMap = 'WeakMap.prototype.set called on non-WeakMap object';
  SErrorWeakMapHasNonWeakMap = 'WeakMap.prototype.has called on non-WeakMap object';
  SErrorWeakMapDeleteNonWeakMap = 'WeakMap.prototype.delete called on non-WeakMap object';
  SErrorWeakMapGetOrInsertNonWeakMap = 'WeakMap.prototype.getOrInsert called on non-WeakMap object';
  SErrorWeakMapGetOrInsertComputedNonWeakMap = 'WeakMap.prototype.getOrInsertComputed called on non-WeakMap object';
  SErrorWeakSetAddNonWeakSet = 'WeakSet.prototype.add called on non-WeakSet object';
  SErrorWeakSetHasNonWeakSet = 'WeakSet.prototype.has called on non-WeakSet object';
  SErrorWeakSetDeleteNonWeakSet = 'WeakSet.prototype.delete called on non-WeakSet object';
  SErrorWeakCollectionInvalidKey = '%s: key must be an object or non-registered symbol';
  SErrorWeakMapConstructorEntryNotObject = 'WeakMap constructor requires each entry to be an object';
  SErrorWeakCollectionConstructorNotIterable = '%s constructor requires an iterable';

  // Set/Map forEach errors
  SErrorSetForEachNotCallable = 'Set.prototype.forEach: callback is not a function';
  SErrorMapForEachNotCallable = 'Map.prototype.forEach: callback is not a function';

  // Set operation argument errors
  SErrorSetOperationRequiresSetLike = 'Set.prototype.%s: argument must be a Set or set-like object';
  SErrorSetLikeSizeMustBeNumber = 'Set.prototype.%s: set-like size must be a number';
  SErrorSetLikeSizeNonNegative = 'Set.prototype.%s: set-like size must be non-negative';
  SErrorSetLikeHasNotCallable = 'Set.prototype.%s: set-like has must be a function';
  SErrorSetLikeKeysNotCallable = 'Set.prototype.%s: set-like keys must be a function';
  SErrorSetLikeKeysIterator = 'Set.prototype.%s: set-like keys must return an iterator';

  // Map upsert errors
  SErrorMapGetOrInsertComputedNotCallable = 'Map.getOrInsertComputed: callbackfn is not a function';
  SErrorWeakMapGetOrInsertComputedNotCallable = 'WeakMap.getOrInsertComputed: callbackfn is not a function';

  // Function body errors
  SErrorIllegalBreakStatement = 'Illegal break statement';
  SErrorIllegalContinueStatement = 'Illegal continue statement';

  // Function.prototype errors
  SErrorFunctionApplyNonFunction = 'Function.prototype.apply called on non-function';

  // Array utility errors
  SErrorInvalidArrayIndexFmt = 'Invalid array index: %d';

  // import.meta errors
  SErrorImportMetaResolveRequiresArg = 'import.meta.resolve requires a specifier argument';

  // Property assignment errors
  SErrorCannotSetPropertyOnNonObject = 'Cannot set property on non-object';

  // Semver errors
  SErrorExpectedArrayOfVersions = 'Expected an array of versions';

  // Symbol.keyFor errors
  SErrorSymbolKeyForRequiresSymbol = 'Symbol.keyFor requires that the first argument be a symbol';

  // FFI.open errors
  SErrorFFIOpenRequiresPath = 'FFI.open requires a library path';

  // Array method errors
  SErrorArrayMethodCalledOnNonArray = 'Array.%s called on non-array';
  SErrorArrayMethodExpectsCallback = 'Array.%s expects callback function';
  SErrorCallbackMustBeFunction = 'Callback must be a function';
  SErrorCallbackMustNotBeUndefined = 'Callback must not be undefined';
  SErrorArrayFlatExpectsNumber = 'Array.flat expects depth argument to be a number';
  SErrorArrayWithRequiresArgs = 'Array.with requires index and value arguments';
  SErrorArrayCopyWithinRequiresTarget = 'Array.copyWithin requires a target argument';
  SErrorCustomSortMustBeFunction = 'Custom sort function must be a function';

  // Object static method errors — Object.create / defineProperty / defineProperties
  SErrorObjectCreateCalledOnNonObject = 'Object.create called on non-object';
  SErrorObjectDefinePropertyCalledOnNonObject = 'Object.defineProperty called on non-object';
  SErrorObjectDefinePropertyDescriptorMustBeObject = 'Object.defineProperty: descriptor must be an object';
  SErrorObjectDefinePropertiesCalledOnNonObject = 'Object.defineProperties called on non-object';
  SErrorObjectDefinePropertiesMustBeObject = 'Object.defineProperties: properties must be an object';
  SErrorObjectFromEntriesRequiresIterable = 'Object.fromEntries requires an iterable of key-value pairs';
  SErrorObjectFromEntriesRequiresPairs = 'Object.fromEntries requires each entry to have at least 2 elements';
  SErrorObjectGroupByRequiresIterable = 'Object.groupBy requires an iterable as first argument';
  SErrorObjectGroupByRequiresCallback = 'Object.groupBy requires a callback function as second argument';
  SErrorSetPrototypeOfTrapReturnedFalse = 'setPrototypeOf trap returned false';
  SErrorSetPrototypeOfNonExtensible = 'Object.setPrototypeOf called on non-extensible object';
  SErrorSetPrototypeOfCyclic = 'Cyclic __proto__ value';

  // Test assertion errors
  SErrorFunctionExpectsStringFirst = '%s expects first argument to be a string';
  SErrorFunctionExpectsFunctionSecond = '%s expects second argument to be a function';
  SErrorFunctionExpectsTableArray = '%s expects a table array';
  SErrorFunctionExpectsFunctionArg = '%s expects a function argument';
  SErrorToThrowExpectsFunction = 'toThrow expects actual value to be a function';
  SErrorToHaveBeenCalledTimesExpectsInt = 'toHaveBeenCalledTimes expects a non-negative integer';
  SErrorToHaveBeenNthCalledWithRequiresArg = 'toHaveBeenNthCalledWith requires at least 1 argument (call index)';
  SErrorToHaveBeenNthCalledWithExpectsInt = 'toHaveBeenNthCalledWith expects a positive integer index';
  SErrorToHaveReturnedTimesExpectsInt = 'toHaveReturnedTimes expects a non-negative integer';
  SErrorToHaveNthReturnedWithExpectsInt = 'toHaveNthReturnedWith expects a positive integer index';
  SErrorMockExpectsFunctionOrNoArgs = 'mock() expects a function argument or no arguments';
  SErrorSpyOnExpectsObject = 'spyOn expects first argument to be an object';
  SErrorSpyOnExpectsString = 'spyOn expects second argument to be a string (method name)';
  SErrorSpyOnNonExistentProperty = 'spyOn: cannot spy on non-existent property "%s"';
  SErrorSpyOnPropertyNotFunction = 'spyOn: property "%s" is not a function';
  SErrorTestTodoExpectsString = 'test.todo expects first argument to be a string';
  SErrorOnTestFinishedExpectsFunction = 'onTestFinished expects a function argument';

  // Other builtin errors — YAML / TOML / Map / Math / Semver / JSON5
  SErrorYAMLParseArgMustBeString = 'YAML.parse: argument must be a string';
  SErrorYAMLParseDocumentsArgMustBeString = 'YAML.parseDocuments: argument must be a string';
  SErrorTOMLParseArgMustBeString = 'TOML.parse: argument must be a string';
  SErrorMapGroupByRequiresIterable = 'Map.groupBy requires an iterable as first argument';
  SErrorMapGroupByRequiresCallback = 'Map.groupBy requires a callback function as second argument';
  SErrorMathClampInvalidRange = 'Invalid range in Math.clamp';
  SErrorSemverInvalidIncrement = 'invalid increment';
  SErrorJSON5StringifyError = 'JSON5.stringify error: %s';

  // GC memory limit
  SErrorMemoryLimitExceeded = 'Allocation failed: GC heap size exceeds the configured memory limit';

  // Fetch API
  SErrorFetchRequiresURL = 'fetch requires a URL argument';
  SErrorFetchUnsupportedMethod = 'fetch does not support method ''%s''; only GET and HEAD are allowed';
  SErrorFetchNoAllowedHosts = 'fetch requires allowed hosts to be configured';
  SErrorFetchHostNotAllowed = 'fetch request to ''%s'' blocked: host is not in the allowed hosts list';
  SErrorHeadersNotHeaders = 'Headers method called on non-Headers object';
  SErrorHeadersForEachNotCallable = 'Headers.prototype.forEach: callback is not a function';
  SErrorResponseNotResponse = 'Response method called on non-Response object';
  SErrorResponseBodyAlreadyUsed = 'body has already been consumed';

implementation

end.
