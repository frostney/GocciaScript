#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling orderedstringmap
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/OrderedStringMap.o  -x assembler build/OrderedStringMap.s
if [ $? != 0 ]; then DoExitAsm orderedstringmap; fi
rm build/OrderedStringMap.s
echo Assembling goccia.garbagecollector
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.GarbageCollector.o  -x assembler build/Goccia.GarbageCollector.s
if [ $? != 0 ]; then DoExitAsm goccia.garbagecollector; fi
rm build/Goccia.GarbageCollector.s
echo Assembling goccia.profiler
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Profiler.o  -x assembler build/Goccia.Profiler.s
if [ $? != 0 ]; then DoExitAsm goccia.profiler; fi
rm build/Goccia.Profiler.s
echo Assembling stringbuffer
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/StringBuffer.o  -x assembler build/StringBuffer.s
if [ $? != 0 ]; then DoExitAsm stringbuffer; fi
rm build/StringBuffer.s
echo Assembling goccia.textfiles
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.TextFiles.o  -x assembler build/Goccia.TextFiles.s
if [ $? != 0 ]; then DoExitAsm goccia.textfiles; fi
rm build/Goccia.TextFiles.s
echo Assembling goccia.values.primitives
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Primitives.o  -x assembler build/Goccia.Values.Primitives.s
if [ $? != 0 ]; then DoExitAsm goccia.values.primitives; fi
rm build/Goccia.Values.Primitives.s
echo Assembling goccia.arguments.collection
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Arguments.Collection.o  -x assembler build/Goccia.Arguments.Collection.s
if [ $? != 0 ]; then DoExitAsm goccia.arguments.collection; fi
rm build/Goccia.Arguments.Collection.s
echo Assembling goccia.values.nativefunctioncallback
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.NativeFunctionCallback.o  -x assembler build/Goccia.Values.NativeFunctionCallback.s
if [ $? != 0 ]; then DoExitAsm goccia.values.nativefunctioncallback; fi
rm build/Goccia.Values.NativeFunctionCallback.s
echo Assembling goccia.objectmodel.types
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.ObjectModel.Types.o  -x assembler build/Goccia.ObjectModel.Types.s
if [ $? != 0 ]; then DoExitAsm goccia.objectmodel.types; fi
rm build/Goccia.ObjectModel.Types.s
echo Assembling goccia.values.nativefunction
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.NativeFunction.o  -x assembler build/Goccia.Values.NativeFunction.s
if [ $? != 0 ]; then DoExitAsm goccia.values.nativefunction; fi
rm build/Goccia.Values.NativeFunction.s
echo Assembling goccia.controlflow
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.ControlFlow.o  -x assembler build/Goccia.ControlFlow.s
if [ $? != 0 ]; then DoExitAsm goccia.controlflow; fi
rm build/Goccia.ControlFlow.s
echo Assembling goccia.error.throwerrorcallback
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Error.ThrowErrorCallback.o  -x assembler build/Goccia.Error.ThrowErrorCallback.s
if [ $? != 0 ]; then DoExitAsm goccia.error.throwerrorcallback; fi
rm build/Goccia.Error.ThrowErrorCallback.s
echo Assembling goccia.modules
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Modules.o  -x assembler build/Goccia.Modules.s
if [ $? != 0 ]; then DoExitAsm goccia.modules; fi
rm build/Goccia.Modules.s
echo Assembling goccia.scope.bindingmap
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Scope.BindingMap.o  -x assembler build/Goccia.Scope.BindingMap.s
if [ $? != 0 ]; then DoExitAsm goccia.scope.bindingmap; fi
rm build/Goccia.Scope.BindingMap.s
echo Assembling goccia.token
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Token.o  -x assembler build/Goccia.Token.s
if [ $? != 0 ]; then DoExitAsm goccia.token; fi
rm build/Goccia.Token.s
echo Assembling goccia.terminal.colors
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Terminal.Colors.o  -x assembler build/Goccia.Terminal.Colors.s
if [ $? != 0 ]; then DoExitAsm goccia.terminal.colors; fi
rm build/Goccia.Terminal.Colors.s
echo Assembling goccia.error
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Error.o  -x assembler build/Goccia.Error.s
if [ $? != 0 ]; then DoExitAsm goccia.error; fi
rm build/Goccia.Error.s
echo Assembling goccia.scope
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Scope.o  -x assembler build/Goccia.Scope.s
if [ $? != 0 ]; then DoExitAsm goccia.scope; fi
rm build/Goccia.Scope.s
echo Assembling goccia.evaluator.context
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.Context.o  -x assembler build/Goccia.Evaluator.Context.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.context; fi
rm build/Goccia.Evaluator.Context.s
echo Assembling goccia.ast.node
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.AST.Node.o  -x assembler build/Goccia.AST.Node.s
if [ $? != 0 ]; then DoExitAsm goccia.ast.node; fi
rm build/Goccia.AST.Node.s
echo Assembling goccia.coverage
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Coverage.o  -x assembler build/Goccia.Coverage.s
if [ $? != 0 ]; then DoExitAsm goccia.coverage; fi
rm build/Goccia.Coverage.s
echo Assembling goccia.builtins.base
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Builtins.Base.o  -x assembler build/Goccia.Builtins.Base.s
if [ $? != 0 ]; then DoExitAsm goccia.builtins.base; fi
rm build/Goccia.Builtins.Base.s
echo Assembling timingutils
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/TimingUtils.o  -x assembler build/TimingUtils.s
if [ $? != 0 ]; then DoExitAsm timingutils; fi
rm build/TimingUtils.s
echo Assembling goccia.timeout
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Timeout.o  -x assembler build/Goccia.Timeout.s
if [ $? != 0 ]; then DoExitAsm goccia.timeout; fi
rm build/Goccia.Timeout.s
echo Assembling goccia.values.error
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Error.o  -x assembler build/Goccia.Values.Error.s
if [ $? != 0 ]; then DoExitAsm goccia.values.error; fi
rm build/Goccia.Values.Error.s
echo Assembling goccia.vm.exception
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.VM.Exception.o  -x assembler build/Goccia.VM.Exception.s
if [ $? != 0 ]; then DoExitAsm goccia.vm.exception; fi
rm build/Goccia.VM.Exception.s
echo Assembling goccia.values.promisevalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.PromiseValue.o  -x assembler build/Goccia.Values.PromiseValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.promisevalue; fi
rm build/Goccia.Values.PromiseValue.s
echo Assembling goccia.microtaskqueue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.MicrotaskQueue.o  -x assembler build/Goccia.MicrotaskQueue.s
if [ $? != 0 ]; then DoExitAsm goccia.microtaskqueue; fi
rm build/Goccia.MicrotaskQueue.s
echo Assembling goccia.uri
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.URI.o  -x assembler build/Goccia.URI.s
if [ $? != 0 ]; then DoExitAsm goccia.uri; fi
rm build/Goccia.URI.s
echo Assembling goccia.arguments.callbacks
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Arguments.Callbacks.o  -x assembler build/Goccia.Arguments.Callbacks.s
if [ $? != 0 ]; then DoExitAsm goccia.arguments.callbacks; fi
rm build/Goccia.Arguments.Callbacks.s
echo Assembling goccia.values.holevalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.HoleValue.o  -x assembler build/Goccia.Values.HoleValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.holevalue; fi
rm build/Goccia.Values.HoleValue.s
echo Assembling goccia.utils
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Utils.o  -x assembler build/Goccia.Utils.s
if [ $? != 0 ]; then DoExitAsm goccia.utils; fi
rm build/Goccia.Utils.s
echo Assembling goccia.values.setvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.SetValue.o  -x assembler build/Goccia.Values.SetValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.setvalue; fi
rm build/Goccia.Values.SetValue.s
echo Assembling goccia.url.parser
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.URL.Parser.o  -x assembler build/Goccia.URL.Parser.s
if [ $? != 0 ]; then DoExitAsm goccia.url.parser; fi
rm build/Goccia.URL.Parser.s
echo Assembling goccia.values.urlvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.URLValue.o  -x assembler build/Goccia.Values.URLValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.urlvalue; fi
rm build/Goccia.Values.URLValue.s
echo Assembling goccia.values.urlsearchparamsvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.URLSearchParamsValue.o  -x assembler build/Goccia.Values.URLSearchParamsValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.urlsearchparamsvalue; fi
rm build/Goccia.Values.URLSearchParamsValue.s
echo Assembling goccia.values.iterator.concrete
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Iterator.Concrete.o  -x assembler build/Goccia.Values.Iterator.Concrete.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iterator.concrete; fi
rm build/Goccia.Values.Iterator.Concrete.s
echo Assembling goccia.values.iterator.generic
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Iterator.Generic.o  -x assembler build/Goccia.Values.Iterator.Generic.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iterator.generic; fi
rm build/Goccia.Values.Iterator.Generic.s
echo Assembling goccia.values.iterator.concat
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Iterator.Concat.o  -x assembler build/Goccia.Values.Iterator.Concat.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iterator.concat; fi
rm build/Goccia.Values.Iterator.Concat.s
echo Assembling goccia.values.iterator.lazy
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Iterator.Lazy.o  -x assembler build/Goccia.Values.Iterator.Lazy.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iterator.lazy; fi
rm build/Goccia.Values.Iterator.Lazy.s
echo Assembling goccia.values.iterator.zip
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Iterator.Zip.o  -x assembler build/Goccia.Values.Iterator.Zip.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iterator.zip; fi
rm build/Goccia.Values.Iterator.Zip.s
echo Assembling goccia.values.iteratorvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.IteratorValue.o  -x assembler build/Goccia.Values.IteratorValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iteratorvalue; fi
rm build/Goccia.Values.IteratorValue.s
echo Assembling goccia.values.mapvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.MapValue.o  -x assembler build/Goccia.Values.MapValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.mapvalue; fi
rm build/Goccia.Values.MapValue.s
echo Assembling goccia.evaluator.comparison
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.Comparison.o  -x assembler build/Goccia.Evaluator.Comparison.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.comparison; fi
rm build/Goccia.Evaluator.Comparison.s
echo Assembling goccia.utils.arrays
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Utils.Arrays.o  -x assembler build/Goccia.Utils.Arrays.s
if [ $? != 0 ]; then DoExitAsm goccia.utils.arrays; fi
rm build/Goccia.Utils.Arrays.s
echo Assembling goccia.values.booleanobjectvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.BooleanObjectValue.o  -x assembler build/Goccia.Values.BooleanObjectValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.booleanobjectvalue; fi
rm build/Goccia.Values.BooleanObjectValue.s
echo Assembling goccia.values.numberobjectvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.NumberObjectValue.o  -x assembler build/Goccia.Values.NumberObjectValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.numberobjectvalue; fi
rm build/Goccia.Values.NumberObjectValue.s
echo Assembling goccia.regexp.unicode
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.RegExp.Unicode.o  -x assembler build/Goccia.RegExp.Unicode.s
if [ $? != 0 ]; then DoExitAsm goccia.regexp.unicode; fi
rm build/Goccia.RegExp.Unicode.s
echo Assembling goccia.regexp.engine
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.RegExp.Engine.o  -x assembler build/Goccia.RegExp.Engine.s
if [ $? != 0 ]; then DoExitAsm goccia.regexp.engine; fi
rm build/Goccia.RegExp.Engine.s
echo Assembling goccia.regexp.runtime
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.RegExp.Runtime.o  -x assembler build/Goccia.RegExp.Runtime.s
if [ $? != 0 ]; then DoExitAsm goccia.regexp.runtime; fi
rm build/Goccia.RegExp.Runtime.s
echo Assembling goccia.values.iterator.regexp
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.Iterator.RegExp.o  -x assembler build/Goccia.Values.Iterator.RegExp.s
if [ $? != 0 ]; then DoExitAsm goccia.values.iterator.regexp; fi
rm build/Goccia.Values.Iterator.RegExp.s
echo Assembling goccia.values.stringobjectvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.StringObjectValue.o  -x assembler build/Goccia.Values.StringObjectValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.stringobjectvalue; fi
rm build/Goccia.Values.StringObjectValue.s
echo Assembling goccia.values.classhelper
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ClassHelper.o  -x assembler build/Goccia.Values.ClassHelper.s
if [ $? != 0 ]; then DoExitAsm goccia.values.classhelper; fi
rm build/Goccia.Values.ClassHelper.s
echo Assembling goccia.values.toprimitive
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ToPrimitive.o  -x assembler build/Goccia.Values.ToPrimitive.s
if [ $? != 0 ]; then DoExitAsm goccia.values.toprimitive; fi
rm build/Goccia.Values.ToPrimitive.s
echo Assembling goccia.values.arrayvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ArrayValue.o  -x assembler build/Goccia.Values.ArrayValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.arrayvalue; fi
rm build/Goccia.Values.ArrayValue.s
echo Assembling goccia.values.sharedarraybuffervalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.SharedArrayBufferValue.o  -x assembler build/Goccia.Values.SharedArrayBufferValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.sharedarraybuffervalue; fi
rm build/Goccia.Values.SharedArrayBufferValue.s
echo Assembling goccia.builtins.globals
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Builtins.Globals.o  -x assembler build/Goccia.Builtins.Globals.s
if [ $? != 0 ]; then DoExitAsm goccia.builtins.globals; fi
rm build/Goccia.Builtins.Globals.s
echo Assembling goccia.callstack
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.CallStack.o  -x assembler build/Goccia.CallStack.s
if [ $? != 0 ]; then DoExitAsm goccia.callstack; fi
rm build/Goccia.CallStack.s
echo Assembling goccia.values.errorhelper
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ErrorHelper.o  -x assembler build/Goccia.Values.ErrorHelper.s
if [ $? != 0 ]; then DoExitAsm goccia.values.errorhelper; fi
rm build/Goccia.Values.ErrorHelper.s
echo Assembling goccia.values.arraybuffervalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ArrayBufferValue.o  -x assembler build/Goccia.Values.ArrayBufferValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.arraybuffervalue; fi
rm build/Goccia.Values.ArrayBufferValue.s
echo Assembling goccia.values.autoaccessor
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.AutoAccessor.o  -x assembler build/Goccia.Values.AutoAccessor.s
if [ $? != 0 ]; then DoExitAsm goccia.values.autoaccessor; fi
rm build/Goccia.Values.AutoAccessor.s
echo Assembling goccia.float16
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Float16.o  -x assembler build/Goccia.Float16.s
if [ $? != 0 ]; then DoExitAsm goccia.float16; fi
rm build/Goccia.Float16.s
echo Assembling goccia.values.typedarrayvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.TypedArrayValue.o  -x assembler build/Goccia.Values.TypedArrayValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.typedarrayvalue; fi
rm build/Goccia.Values.TypedArrayValue.s
echo Assembling goccia.values.textdecodervalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.TextDecoderValue.o  -x assembler build/Goccia.Values.TextDecoderValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.textdecodervalue; fi
rm build/Goccia.Values.TextDecoderValue.s
echo Assembling goccia.values.textencodervalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.TextEncoderValue.o  -x assembler build/Goccia.Values.TextEncoderValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.textencodervalue; fi
rm build/Goccia.Values.TextEncoderValue.s
echo Assembling goccia.values.classvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ClassValue.o  -x assembler build/Goccia.Values.ClassValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.classvalue; fi
rm build/Goccia.Values.ClassValue.s
echo Assembling goccia.evaluator.bitwise
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.Bitwise.o  -x assembler build/Goccia.Evaluator.Bitwise.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.bitwise; fi
rm build/Goccia.Evaluator.Bitwise.s
echo Assembling goccia.evaluator.arithmetic
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.Arithmetic.o  -x assembler build/Goccia.Evaluator.Arithmetic.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.arithmetic; fi
rm build/Goccia.Evaluator.Arithmetic.s
echo Assembling goccia.evaluator.assignment
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.Assignment.o  -x assembler build/Goccia.Evaluator.Assignment.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.assignment; fi
rm build/Goccia.Evaluator.Assignment.s
echo Assembling goccia.evaluator.decorators
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.Decorators.o  -x assembler build/Goccia.Evaluator.Decorators.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.decorators; fi
rm build/Goccia.Evaluator.Decorators.s
echo Assembling goccia.values.proxyvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ProxyValue.o  -x assembler build/Goccia.Values.ProxyValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.proxyvalue; fi
rm build/Goccia.Values.ProxyValue.s
echo Assembling goccia.evaluator.typeoperations
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.TypeOperations.o  -x assembler build/Goccia.Evaluator.TypeOperations.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator.typeoperations; fi
rm build/Goccia.Evaluator.TypeOperations.s
echo Assembling goccia.error.suggestions
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Error.Suggestions.o  -x assembler build/Goccia.Error.Suggestions.s
if [ $? != 0 ]; then DoExitAsm goccia.error.suggestions; fi
rm build/Goccia.Error.Suggestions.s
echo Assembling goccia.lexer
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Lexer.o  -x assembler build/Goccia.Lexer.s
if [ $? != 0 ]; then DoExitAsm goccia.lexer; fi
rm build/Goccia.Lexer.s
echo Assembling goccia.parser
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Parser.o  -x assembler build/Goccia.Parser.s
if [ $? != 0 ]; then DoExitAsm goccia.parser; fi
rm build/Goccia.Parser.s
echo Assembling goccia.values.asyncfunctionvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.AsyncFunctionValue.o  -x assembler build/Goccia.Values.AsyncFunctionValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.asyncfunctionvalue; fi
rm build/Goccia.Values.AsyncFunctionValue.s
echo Assembling goccia.values.enumvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.EnumValue.o  -x assembler build/Goccia.Values.EnumValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.enumvalue; fi
rm build/Goccia.Values.EnumValue.s
echo Assembling goccia.evaluator
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Evaluator.o  -x assembler build/Goccia.Evaluator.s
if [ $? != 0 ]; then DoExitAsm goccia.evaluator; fi
rm build/Goccia.Evaluator.s
echo Assembling goccia.values.functionvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.FunctionValue.o  -x assembler build/Goccia.Values.FunctionValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.functionvalue; fi
rm build/Goccia.Values.FunctionValue.s
echo Assembling goccia.ast.statements
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.AST.Statements.o  -x assembler build/Goccia.AST.Statements.s
if [ $? != 0 ]; then DoExitAsm goccia.ast.statements; fi
rm build/Goccia.AST.Statements.s
echo Assembling goccia.importmeta
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.ImportMeta.o  -x assembler build/Goccia.ImportMeta.s
if [ $? != 0 ]; then DoExitAsm goccia.importmeta; fi
rm build/Goccia.ImportMeta.s
echo Assembling goccia.ast.expressions
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.AST.Expressions.o  -x assembler build/Goccia.AST.Expressions.s
if [ $? != 0 ]; then DoExitAsm goccia.ast.expressions; fi
rm build/Goccia.AST.Expressions.s
echo Assembling goccia.sharedprototype
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.SharedPrototype.o  -x assembler build/Goccia.SharedPrototype.s
if [ $? != 0 ]; then DoExitAsm goccia.sharedprototype; fi
rm build/Goccia.SharedPrototype.s
echo Assembling goccia.objectmodel
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.ObjectModel.o  -x assembler build/Goccia.ObjectModel.s
if [ $? != 0 ]; then DoExitAsm goccia.objectmodel; fi
rm build/Goccia.ObjectModel.s
echo Assembling goccia.arguments.arraylike
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Arguments.ArrayLike.o  -x assembler build/Goccia.Arguments.ArrayLike.s
if [ $? != 0 ]; then DoExitAsm goccia.arguments.arraylike; fi
rm build/Goccia.Arguments.ArrayLike.s
echo Assembling goccia.values.functionbase
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.FunctionBase.o  -x assembler build/Goccia.Values.FunctionBase.s
if [ $? != 0 ]; then DoExitAsm goccia.values.functionbase; fi
rm build/Goccia.Values.FunctionBase.s
echo Assembling goccia.values.objectvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ObjectValue.o  -x assembler build/Goccia.Values.ObjectValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.objectvalue; fi
rm build/Goccia.Values.ObjectValue.s
echo Assembling goccia.values.symbolvalue
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.SymbolValue.o  -x assembler build/Goccia.Values.SymbolValue.s
if [ $? != 0 ]; then DoExitAsm goccia.values.symbolvalue; fi
rm build/Goccia.Values.SymbolValue.s
echo Assembling goccia.values.objectpropertydescriptor
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Values.ObjectPropertyDescriptor.o  -x assembler build/Goccia.Values.ObjectPropertyDescriptor.s
if [ $? != 0 ]; then DoExitAsm goccia.values.objectpropertydescriptor; fi
rm build/Goccia.Values.ObjectPropertyDescriptor.s
echo Assembling goccia.spec
/Library/Developer/CommandLineTools/usr/bin/clang -x assembler -c -target arm64-apple-macosx11.0.0 -o build/Goccia.Spec.o  -x assembler build/Goccia.Spec.s
if [ $? != 0 ]; then DoExitAsm goccia.spec; fi
rm build/Goccia.Spec.s
