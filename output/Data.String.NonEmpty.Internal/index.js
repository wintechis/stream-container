// Generated by purs version 0.15.10
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var composeKleisliFlipped = /* #__PURE__ */ Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var NonEmptyString = function (x) {
    return x;
};
var NonEmptyReplacement = function (x) {
    return x;
};
var toUpper = function (v) {
    return Data_String_Common.toUpper(v);
};
var toString = function (v) {
    return v;
};
var toLower = function (v) {
    return Data_String_Common.toLower(v);
};
var showNonEmptyString = {
    show: function (v) {
        return "(NonEmptyString.unsafeFromString " + (show(v) + ")");
    }
};
var show1 = /* #__PURE__ */ Data_Show.show(showNonEmptyString);
var showNonEmptyReplacement = {
    show: function (v) {
        return "(NonEmptyReplacement " + (show1(v) + ")");
    }
};
var semigroupNonEmptyString = Data_Semigroup.semigroupString;
var semigroupNonEmptyReplacement = semigroupNonEmptyString;
var replaceAll = function (pat) {
    return function (v) {
        return function (v1) {
            return Data_String_Common.replaceAll(pat)(v)(v1);
        };
    };
};
var replace = function (pat) {
    return function (v) {
        return function (v1) {
            return Data_String_Common.replace(pat)(v)(v1);
        };
    };
};
var prependString = function (s1) {
    return function (v) {
        return s1 + v;
    };
};
var ordNonEmptyString = Data_Ord.ordString;
var ordNonEmptyReplacement = ordNonEmptyString;
var nonEmptyNonEmpty = function (dictIsSymbol) {
    var reflectSymbol = Data_Symbol.reflectSymbol(dictIsSymbol);
    return {
        nes: function (p) {
            return reflectSymbol(p);
        }
    };
};
var nes = function (dict) {
    return dict.nes;
};
var makeNonEmptyBad = function () {
    return {
        nes: function (v) {
            return "";
        }
    };
};
var localeCompare = function (v) {
    return function (v1) {
        return Data_String_Common.localeCompare(v)(v1);
    };
};
var liftS = function (f) {
    return function (v) {
        return f(v);
    };
};
var joinWith1 = function (dictFoldable1) {
    var intercalate = Data_Foldable.intercalate(dictFoldable1.Foldable0())(Data_Monoid.monoidString);
    return function (v) {
        var $59 = intercalate(v);
        return function ($60) {
            return NonEmptyString($59($60));
        };
    };
};
var joinWith = function (dictFoldable) {
    var intercalate = Data_Foldable.intercalate(dictFoldable)(Data_Monoid.monoidString);
    return function (splice) {
        var $61 = intercalate(splice);
        return function ($62) {
            return $61($62);
        };
    };
};
var join1With = function (dictFoldable1) {
    var joinWith2 = joinWith(dictFoldable1.Foldable0());
    return function (splice) {
        var $63 = joinWith2(splice);
        return function ($64) {
            return NonEmptyString($63($64));
        };
    };
};
var fromString = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just(v);
};
var stripPrefix = function (pat) {
    return composeKleisliFlipped(fromString)(liftS(Data_String_CodeUnits.stripPrefix(pat)));
};
var stripSuffix = function (pat) {
    return composeKleisliFlipped(fromString)(liftS(Data_String_CodeUnits.stripSuffix(pat)));
};
var trim = function (v) {
    return fromString(Data_String_Common.trim(v));
};
var unsafeFromString = function () {
    return function ($65) {
        return fromJust(fromString($65));
    };
};
var eqNonEmptyString = Data_Eq.eqString;
var eqNonEmptyReplacement = eqNonEmptyString;
var contains = function ($66) {
    return liftS(Data_String_CodeUnits.contains($66));
};
var appendString = function (v) {
    return function (s2) {
        return v + s2;
    };
};
export {
    nes,
    NonEmptyString,
    NonEmptyReplacement,
    fromString,
    unsafeFromString,
    toString,
    appendString,
    prependString,
    stripPrefix,
    stripSuffix,
    contains,
    localeCompare,
    replace,
    replaceAll,
    toLower,
    toUpper,
    trim,
    joinWith,
    join1With,
    joinWith1,
    liftS,
    eqNonEmptyString,
    ordNonEmptyString,
    semigroupNonEmptyString,
    showNonEmptyString,
    makeNonEmptyBad,
    nonEmptyNonEmpty,
    eqNonEmptyReplacement,
    ordNonEmptyReplacement,
    semigroupNonEmptyReplacement,
    showNonEmptyReplacement
};
