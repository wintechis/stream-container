// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_CodePoint_Unicode from "../Data.CodePoint.Unicode/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Number from "../Data.Number/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Regex_Flags from "../Data.String.Regex.Flags/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var elem = /* #__PURE__ */ Data_Array.elem(Data_String_CodePoints.eqCodePoint);
var show = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ Data_Show.showArray(Data_Show.showString));
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var elem1 = /* #__PURE__ */ Data_Array.elem(Data_Eq.eqChar);
var show1 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ Data_Show.showArray(Data_Show.showChar));
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var mempty = /* #__PURE__ */ Data_Monoid.mempty(Data_String_Regex_Flags.monoidRegexFlags);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var notElem = /* #__PURE__ */ Data_Array.notElem(Data_String_CodePoints.eqCodePoint);
var notElem1 = /* #__PURE__ */ Data_Array.notElem(Data_Eq.eqChar);
var takeWhile1 = function (predicate) {
    return Parsing_String.consumeWith(function (s) {
        var value = Data_String_CodePoints.takeWhile(predicate)(s);
        var len = Data_String_CodeUnits.length(value);
        var $27 = len > 0;
        if ($27) {
            return new Data_Either.Right({
                consumed: value,
                remainder: Data_String_CodeUnits.drop(Data_String_CodeUnits.length(value))(s),
                value: value
            });
        };
        return new Data_Either.Left("Expected character satisfying predicate");
    });
};
var takeWhile = function (predicate) {
    return Parsing_String.consumeWith(function (s) {
        var value = Data_String_CodePoints.takeWhile(predicate)(s);
        return new Data_Either.Right({
            consumed: value,
            remainder: Data_String_CodeUnits.drop(Data_String_CodeUnits.length(value))(s),
            value: value
        });
    });
};
var whiteSpace = /* #__PURE__ */ takeWhile(Data_CodePoint_Unicode.isSpace);
var skipSpaces = /* #__PURE__ */ Data_Functor["void"](Parsing.functorParserT)(whiteSpace);
var satisfyCP = function (p) {
    return Parsing_String.satisfy(function ($32) {
        return p(Data_String_CodePoints.codePointFromChar($32));
    });
};
var space = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isSpace))("space");
var upper = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isUpper))("uppercase letter");
var oneOfCodePoints = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfyCodePoint(Data_Function.flip(elem)(ss)))(function (v) {
        return "one of " + show(map(Data_String_CodePoints.singleton)(ss));
    });
};
var oneOf = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfy(Data_Function.flip(elem1)(ss)))(function (v) {
        return "one of " + show1(ss);
    });
};
var octDigit = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isOctDigit))("oct digit");
var numberRegex = /* #__PURE__ */ Data_Either.either(Partial_Unsafe.unsafeCrashWith)(identity)(/* #__PURE__ */ Parsing_String.regex("[+-]?[0-9]*(\\.[0-9]*)?([eE][+-]?[0-9]*(\\.[0-9]*)?)?")(mempty));
var number = /* #__PURE__ */ (function () {
    return alt(Parsing_Combinators.choice(Data_Foldable.foldableArray)([ applySecond(Parsing_String.string("Infinity"))(pure(Data_Number.infinity)), applySecond(Parsing_String.string("+Infinity"))(pure(Data_Number.infinity)), applySecond(Parsing_String.string("-Infinity"))(pure(-Data_Number.infinity)), applySecond(Parsing_String.string("NaN"))(pure(Data_Number.nan)), Parsing_Combinators.tryRethrow(bind(numberRegex)(function (section) {
        var v = Data_Number.fromString(section);
        if (v instanceof Data_Maybe.Nothing) {
            return Parsing.fail("Expected Number");
        };
        if (v instanceof Data_Maybe.Just) {
            return pure(v.value0);
        };
        throw new Error("Failed pattern match at Parsing.String.Basic (line 118, column 9 - line 120, column 27): " + [ v.constructor.name ]);
    })) ]))(Parsing.fail("Expected Number"));
})();
var noneOfCodePoints = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfyCodePoint(Data_Function.flip(notElem)(ss)))(function (v) {
        return "none of " + show(map(Data_String_CodePoints.singleton)(ss));
    });
};
var noneOf = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfy(Data_Function.flip(notElem1)(ss)))(function (v) {
        return "none of " + show1(ss);
    });
};
var lower = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isLower))("lowercase letter");
var letter = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isAlpha))("letter");
var intDecimalRegex = /* #__PURE__ */ Data_Either.either(Partial_Unsafe.unsafeCrashWith)(identity)(/* #__PURE__ */ Parsing_String.regex("[+-]?[0-9]+")(mempty));
var intDecimal = /* #__PURE__ */ Parsing_Combinators.tryRethrow(/* #__PURE__ */ bind(/* #__PURE__ */ alt(intDecimalRegex)(/* #__PURE__ */ Parsing.fail("Expected Int")))(function (section) {
    var v = Data_Int.fromString(section);
    if (v instanceof Data_Maybe.Nothing) {
        return Parsing.fail("Expected Int");
    };
    if (v instanceof Data_Maybe.Just) {
        return pure(v.value0);
    };
    throw new Error("Failed pattern match at Parsing.String.Basic (line 140, column 3 - line 142, column 21): " + [ v.constructor.name ]);
}));
var hexDigit = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isHexDigit))("hex digit");
var digit = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isDecDigit))("digit");
var alphaNum = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isAlphaNum))("letter or digit");
export {
    digit,
    hexDigit,
    octDigit,
    letter,
    space,
    lower,
    upper,
    alphaNum,
    intDecimal,
    number,
    takeWhile,
    takeWhile1,
    whiteSpace,
    skipSpaces,
    oneOf,
    oneOfCodePoints,
    noneOf,
    noneOfCodePoints
};
