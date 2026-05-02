"""GocciaScript-compatible test262 harness include mapping."""

INCLUDE_MAP: dict[str, str] = {
    "assert.js": "assert.js",
    "sta.js": "assert.js",
    "compareArray.js": "assert.js",
    "propertyHelper.js": "propertyHelper.js",
    "compareIterator.js": "compareIterator.js",
    "isConstructor.js": "isConstructor.js",
    "deepEqual.js": "deepEqual.js",
    "nans.js": "nans.js",
    "testTypedArray.js": "testTypedArray.js",
    "testBigIntTypedArray.js": "testTypedArray.js",
    "temporalHelpers.js": "temporalHelpers.js",
    "promiseHelper.js": "promiseHelper.js",
    "dateConstants.js": "dateConstants.js",
    "assertRelativeDateMs.js": "assertRelativeDateMs.js",
    "decimalToHexString.js": "decimalToHexString.js",
    "fnGlobalObject.js": "fnGlobalObject.js",
    "nativeFunctionMatcher.js": "nativeFunctionMatcher.js",
    "wellKnownIntrinsicObjects.js": "wellKnownIntrinsicObjects.js",
    "byteConversionValues.js": "byteConversionValues.js",
    "proxyTrapsHelper.js": "proxyTrapsHelper.js",
    "regExpUtils.js": "regExpUtils.js",
    "detachArrayBuffer.js": "detachArrayBuffer.js",
    "doneprintHandle.js": "doneprintHandle.js",
}

AVAILABLE_INCLUDES = frozenset(INCLUDE_MAP)
