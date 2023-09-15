// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_NonEmpty_CodeUnits from "../Data.String.NonEmpty.CodeUnits/index.js";
import * as Data_String_NonEmpty_Internal from "../Data.String.NonEmpty.Internal/index.js";
import * as Parsing from "../Parsing/index.js";
import * as URI_Common from "../URI.Common/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_String_NonEmpty_Internal.showNonEmptyString);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var RegName = function (x) {
    return x;
};
var unsafeToString = function (v) {
    return v;
};
var unsafeFromString = RegName;
var toString = function (v) {
    return URI_Common["decodeURIComponent$prime"](v);
};
var showRegName = {
    show: function (v) {
        return "(RegName.unsafeFromString " + (show(v) + ")");
    }
};
var semigroupRegName = Data_String_NonEmpty_Internal.semigroupNonEmptyString;
var regNameChar = /* #__PURE__ */ alt(URI_Common.unreserved)(URI_Common.subDelims);
var print = function ($14) {
    return Data_String_NonEmpty_Internal.toString(unsafeToString($14));
};
var parser = /* #__PURE__ */ (function () {
    var p = alt(URI_Common.pctEncoded)(map(Data_String_NonEmpty_CodeUnits.singleton)(regNameChar));
    return map((function () {
        var $15 = Data_String_NonEmpty_Internal.join1With(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray)("");
        return function ($16) {
            return RegName($15($16));
        };
    })())(Data_Array_NonEmpty.some(Parsing.alternativeParserT)(Parsing.lazyParserT)(p));
})();
var ordRegName = Data_String_NonEmpty_Internal.ordNonEmptyString;
var fromString = /* #__PURE__ */ (function () {
    var $17 = URI_Common["printEncoded$prime"](regNameChar);
    return function ($18) {
        return RegName($17($18));
    };
})();
var eqRegName = Data_String_NonEmpty_Internal.eqNonEmptyString;
export {
    fromString,
    toString,
    unsafeFromString,
    unsafeToString,
    parser,
    print,
    regNameChar,
    eqRegName,
    ordRegName,
    semigroupRegName,
    showRegName
};