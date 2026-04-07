unit Goccia.Error.Suggestions;

{$I Goccia.inc}

interface

resourcestring
  // Semicolons
  SSuggestAddSemicolon = 'Add a '';'' at the end of the statement';

  // Closing delimiters
  SSuggestCloseBlock = 'Add a matching ''}'' to close the block';
  SSuggestCloseObject = 'Add a matching ''}'' to close the object';
  SSuggestCloseClassBody = 'Add a matching ''}'' to close the class body';
  SSuggestCloseEnum = 'Add a matching ''}'' to close the enum';
  SSuggestCloseImportList = 'Add a matching ''}'' to close the import list';
  SSuggestCloseExportList = 'Add a matching ''}'' to close the export list';
  SSuggestCloseSwitchBody = 'Add a matching ''}'' to close the switch body';
  SSuggestCloseDestructuringPattern = 'Add a matching ''}'' to close the destructuring pattern';
  SSuggestCloseParenArguments = 'Add a '')'' to close the argument list';
  SSuggestCloseParenExpression = 'Add a '')'' to close the expression';
  SSuggestCloseParenCondition = 'Add a '')'' to close the condition';
  SSuggestCloseParenParameterList = 'Add a '')'' to close the parameter list';
  SSuggestCloseParenSetterParameter = 'Add a '')'' to close the setter parameter list';
  SSuggestCloseParenForOf = 'Add a '')'' to close the for...of expression';
  SSuggestCloseParenCatchClause = 'Add a '')'' to close the catch clause';
  SSuggestCloseParenDecoratorExpression = 'Add a '')'' to close the decorator expression';
  SSuggestCloseParenConstructorArguments = 'Add a '')'' to close the constructor arguments';
  SSuggestCloseParenSwitchExpression = 'Add a '')'' to close the switch expression';
  SSuggestCloseBracketArray = 'Add a '']'' to close the array';
  SSuggestCloseBracketComputedProperty = 'Add a '']'' to close the computed property access';
  SSuggestCloseBracketComputedPropertyName = 'Add a '']'' to close the computed property name';
  SSuggestCloseBracketComputedPropertyKey = 'Add a '']'' to close the computed property key';
  SSuggestCloseBracketDestructuringPattern = 'Add a '']'' to close the destructuring pattern';

  // Opening delimiters
  SSuggestOpenParenCondition = 'Add ''('' before the condition';
  SSuggestOpenParenExpression = 'Add ''('' before the expression';
  SSuggestOpenParenGetterParameterList = 'Add ''('' to start the getter parameter list';
  SSuggestOpenParenSetterParameterList = 'Add ''('' to start the setter parameter list';
  SSuggestOpenParenMethodParameterList = 'Add ''('' to start the method parameter list';
  SSuggestOpenParenCatchClause = 'Add ''('' to start the catch clause';
  SSuggestOpenParenSwitchExpression = 'Add ''('' before the switch expression';
  SSuggestOpenBraceBlock = 'Add ''{'' to start the block';
  SSuggestOpenBraceGetterBody = 'Add ''{'' to start the getter body';
  SSuggestOpenBraceSetterBody = 'Add ''{'' to start the setter body';
  SSuggestOpenBraceMethodBody = 'Add ''{'' to start the method body';
  SSuggestOpenBraceTryBlock = 'Add ''{'' to start the try block';
  SSuggestOpenBraceCatchBlock = 'Add ''{'' to start the catch block';
  SSuggestOpenBraceFinallyBlock = 'Add ''{'' to start the finally block';
  SSuggestOpenBraceClassBody = 'Add ''{'' to start the class body';
  SSuggestOpenBraceEnumBody = 'Add ''{'' to start the enum body (e.g., enum Color { Red = 0, Green = 1 })';
  SSuggestOpenBraceImportList = 'Add ''{'' to start the import list';
  SSuggestOpenBraceExportOrDeclaration = 'Add ''{'' or a declaration after export';
  SSuggestOpenBraceSwitchBody = 'Add ''{'' to start the switch body';
  SSuggestAddWhileAfterDo = 'Add ''while (condition)'' after the do block';

  // Identifiers and names
  SSuggestProvideVariableName = 'Provide a name for the variable (e.g., let myVar = ...)';
  SSuggestProvideParameterName = 'Provide a name for the parameter';
  SSuggestProvideRestParameterName = 'Provide a name for the rest parameter (e.g., ...args)';
  SSuggestProvideClassName = 'Provide a name for the class';
  SSuggestProvideSuperclassName = 'Provide the name of the class to extend (e.g., class Foo extends Bar { ... })';
  SSuggestProvideGetterPropertyName = 'Provide the getter property name';
  SSuggestProvideSetterPropertyName = 'Provide the setter property name';
  SSuggestProvideImportName = 'Provide the name of the binding to import';
  SSuggestProvideLocalName = 'Provide a local name for the imported/exported binding';
  SSuggestProvideExportName = 'Provide the name of the binding to export';
  SSuggestProvideExportedName = 'Provide a name for the exported binding';
  SSuggestProvideModulePath = 'Provide the module path as a string (e.g., from "module-name")';
  SSuggestProvideCatchParameter = 'Provide a name for the error variable (e.g., catch (e) { ... })';
  SSuggestPrivateFieldMustFollow = 'Private field names must follow the # symbol';
  SSuggestPropertyNameIdentifier = 'Property names must be identifiers. Use bracket notation for special names (e.g., obj["name"])';
  SSuggestPropertyKeysFormat = 'Property keys can be identifiers, strings, numbers, or computed [expression]';

  // Keywords
  SSuggestTernaryColon = 'The ternary operator requires '':'' between the two values (condition ? yes : no)';
  SSuggestAddColonPropertyValue = 'Add '':'' between the property name and value';
  SSuggestAddColonAfterCase = 'Add '':'' after the case value';
  SSuggestAddColonAfterDefault = 'Add '':'' after default';
  SSuggestArrowFunctionSyntax = 'Arrow functions use ''=> '' after the parameter list';
  SSuggestNamespaceImportAs = 'Namespace imports require ''as'' and a local name (e.g., import * as name from ...)';
  SSuggestAddFromAfterImport = 'Add ''from "module-path"'' after the import';
  SSuggestAddFromAfterImportList = 'Add ''from "module-path"'' after the import list';

  // Expressions and operators
  SSuggestExpressionExpected = 'Expected a value, variable, or expression here';
  SSuggestValidAssignmentTarget = 'Only variables and properties can be assigned to';
  SSuggestValidIncrementTarget = 'Only variables and properties can be incremented or decremented (e.g., x++ or obj.x--)';

  // Enums
  SSuggestEnumName = 'Enums require a name: enum MyEnum { ... }';
  SSuggestEnumMemberName = 'Enum members must be identifiers (e.g., enum Color { Red = 0 })';
  SSuggestEnumExplicitValues = 'GocciaScript enums require explicit values: MemberName = value';

  // Classes and types
  SSuggestClassMemberSyntax = 'Class members can be methods, properties, or accessors. Type modifiers (public, private, readonly) are supported as comments';
  SSuggestClassMemberExpectedSyntax = 'Class members can be: methods name() {}, properties name = value, or typed declarations name: Type;';
  SSuggestDecoratorsOnlyClasses = 'Decorators can only be applied to class declarations or class elements';
  SSuggestGetterNoParameters = 'Getters must have no parameters — remove the parameters';
  SSuggestSetterOneParameter = 'Setters take exactly one parameter: set name(value) { ... }';
  SSuggestProvideSetterParameterName = 'Provide a name for the setter parameter';
  SSuggestAddPropertyInitializer = 'Add ''= value'' to initialize the property';
  SSuggestAddConstInitializer = 'Add ''= value'' after the variable name';
  SSuggestComputedPropertyNeedsValue = 'Add '': value'' after the computed property (e.g., [key]: value)';

  // Imports and exports
  SSuggestStringImportAs = 'Use: import { "name" as localName } from "module"';
  SSuggestStringExportAs = 'Use: export { localName as "export-name" }';
  SSuggestDestructuringExportDeclareFirst = 'Declare first, then export: const x = ...; export { x };';

  // Destructuring
  SSuggestDestructuringRequiresInitializer = 'Destructuring declarations require an initializer — add ''= expression'' after the pattern';
  SSuggestDestructuringArrayOrObject = 'Use array destructuring [a, b] or object destructuring { x, y }';
  SSuggestDestructuringInvalidTarget = 'Destructuring targets must be variables or properties, not expressions';
  SSuggestObjectPatternPropertyName = 'Use: { name }, { name: alias }, or { name = default }';

  // Control flow
  SSuggestSwitchCaseOrDefault = 'Switch bodies contain case/default labels: case value: ... or default: ...';
  SSuggestMissingCatchOrFinally = 'Add catch (e) { ... } or finally { ... } after the try block';
  SSuggestDeclarePrivateField = 'Declare the private field in the class body, or move this code inside the class';
  SSuggestNumberFormatValid = 'Valid formats: integers (123), decimals (1.23), hex (0xFF), binary (0b101), octal (0o777)';

  // Lexer errors — strings, templates, regex
  SSuggestCloseBlockComment = 'Add "*/" to close the block comment';
  SSuggestCloseString = 'Add a closing quote to end the string';
  SSuggestCloseTemplate = 'Add a closing backtick to end the template literal';
  SSuggestCloseRegex = 'Add a closing / to end the regex, followed by optional flags (g, i, m, s, u, y)';
  SSuggestValidRegexFlags = 'Valid regex flags are: g (global), i (case-insensitive), m (multiline), s (dotAll), u (unicode), y (sticky)';
  SSuggestDuplicateRegexFlag = 'Each regex flag can only appear once';
  SSuggestRegexSuffixFlags = 'Only flag characters (g, i, m, s, u, y) are allowed after the closing /';

  // Lexer errors — numbers
  SSuggestHexNumberFormat = 'Hex numbers start with 0x followed by hex digits (e.g., 0xFF)';
  SSuggestBinaryNumberFormat = 'Binary numbers start with 0b followed by 0 or 1 (e.g., 0b1010)';
  SSuggestOctalNumberFormat = 'Octal numbers start with 0o followed by digits 0-7 (e.g., 0o755)';
  SSuggestScientificNotation = 'Scientific notation needs digits after e (e.g., 1e10 or 3.14e-2)';

  // Lexer errors — escapes and characters
  SSuggestUnicodeEscapeFormat = 'Unicode escapes use \uXXXX (4 hex digits) or \u{XXXXXX} (1-6 hex digits)';
  SSuggestUnicodeHexDigits = 'Unicode escapes must contain only hex digits (0-9, a-f, A-F)';
  SSuggestUnicodeCodePointRange = 'Code points must be in range U+0000 to U+10FFFF';
  SSuggestHexEscapeFormat = 'Hex escapes use \xXX (exactly 2 hex digits)';
  SSuggestInvalidCharacter = 'This character is not valid in GocciaScript. Check for typos';
  SSuggestInvalidDoubleDot = 'Did you mean "..." (spread operator)? Two dots is not valid syntax';

implementation

end.
