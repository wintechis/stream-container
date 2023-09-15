// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_NonEmpty_CodeUnits from "../Data.String.NonEmpty.CodeUnits/index.js";
import * as Data_String_NonEmpty_Internal from "../Data.String.NonEmpty.Internal/index.js";
import * as JSURI from "../JSURI/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as URI_Common from "../URI.Common/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_String_NonEmpty_Internal.showNonEmptyString);
var show1 = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var join1With = /* #__PURE__ */ Data_String_NonEmpty_Internal.join1With(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray);
var some = /* #__PURE__ */ Data_Array_NonEmpty.some(Parsing.alternativeParserT)(Parsing.lazyParserT);
var PathSegmentNZNC = function (x) {
    return x;
};
var PathSegmentNZ = function (x) {
    return x;
};
var PathSegment = function (x) {
    return x;
};
var unsafeSegmentToString = function (v) {
    return v;
};
var unsafeSegmentNZToString = function (v) {
    return v;
};
var unsafeSegmentNZNCToString = function (v) {
    return v;
};
var unsafeSegmentNZNCFromString = PathSegmentNZNC;
var unsafeSegmentNZFromString = PathSegmentNZ;
var unsafeSegmentFromString = PathSegment;
var showPathSegmentNZNC = {
    show: function (v) {
        return "(Segment.unsafeSegmentNZNCToString " + (show(v) + ")");
    }
};
var showPathSegmentNZ = {
    show: function (v) {
        return "(Segment.unsafeSegmentNZFromString " + (show(v) + ")");
    }
};
var showPathSegment = {
    show: function (v) {
        return "(Segment.unsafeSegmentToString " + (show1(v) + ")");
    }
};
var segmentToString = function (v) {
    return fromJust(JSURI["decodeURIComponent"](v));
};
var segmentNZToString = function (v) {
    return URI_Common["decodeURIComponent$prime"](v);
};
var segmentNZNCToString = function (v) {
    return URI_Common["decodeURIComponent$prime"](v);
};
var segmentNCChar = /* #__PURE__ */ alt(URI_Common.unreserved)(/* #__PURE__ */ alt(URI_Common.subDelims)(/* #__PURE__ */ Parsing_String["char"]("@")));
var segmentNZNCFromString = /* #__PURE__ */ (function () {
    var $31 = URI_Common["printEncoded$prime"](segmentNCChar);
    return function ($32) {
        return PathSegmentNZNC($31($32));
    };
})();
var segmentChar = /* #__PURE__ */ alt(segmentNCChar)(/* #__PURE__ */ Parsing_String["char"](":"));
var segmentFromString = /* #__PURE__ */ (function () {
    var $33 = URI_Common.printEncoded(segmentChar);
    return function ($34) {
        return PathSegment($33($34));
    };
})();
var segmentNZFromString = /* #__PURE__ */ (function () {
    var $35 = URI_Common["printEncoded$prime"](segmentChar);
    return function ($36) {
        return PathSegmentNZ($35($36));
    };
})();
var printSegmentNZNC = function ($37) {
    return Data_String_NonEmpty_Internal.toString(unsafeSegmentNZNCToString($37));
};
var printSegmentNZ = function ($38) {
    return Data_String_NonEmpty_Internal.toString(unsafeSegmentNZToString($38));
};
var printSegment = unsafeSegmentToString;
var parseSegmentNZNC = /* #__PURE__ */ map(/* #__PURE__ */ (function () {
    var $39 = join1With("");
    return function ($40) {
        return PathSegmentNZNC($39($40));
    };
})())(/* #__PURE__ */ some(/* #__PURE__ */ alt(URI_Common.pctEncoded)(/* #__PURE__ */ map(Data_String_NonEmpty_CodeUnits.singleton)(segmentNCChar))));
var parseSegmentNZ = /* #__PURE__ */ map(/* #__PURE__ */ (function () {
    var $41 = join1With("");
    return function ($42) {
        return PathSegmentNZ($41($42));
    };
})())(/* #__PURE__ */ some(/* #__PURE__ */ alt(URI_Common.pctEncoded)(/* #__PURE__ */ map(Data_String_NonEmpty_CodeUnits.singleton)(segmentChar))));
var parseSegment = /* #__PURE__ */ map(/* #__PURE__ */ (function () {
    var $43 = Data_String_NonEmpty_Internal.joinWith(Data_List_Types.foldableList)("");
    return function ($44) {
        return PathSegment($43($44));
    };
})())(/* #__PURE__ */ Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT)(/* #__PURE__ */ alt(URI_Common.pctEncoded)(/* #__PURE__ */ map(Data_String_NonEmpty_CodeUnits.singleton)(segmentChar))));
var ordPathSegmentNZNC = Data_String_NonEmpty_Internal.ordNonEmptyString;
var ordPathSegmentNZ = Data_String_NonEmpty_Internal.ordNonEmptyString;
var ordPathSegment = Data_Ord.ordString;
var eqPathSegmentNZNC = Data_String_NonEmpty_Internal.eqNonEmptyString;
var eqPathSegmentNZ = Data_String_NonEmpty_Internal.eqNonEmptyString;
var eqPathSegment = Data_Eq.eqString;
export {
    segmentFromString,
    segmentToString,
    unsafeSegmentFromString,
    unsafeSegmentToString,
    parseSegment,
    printSegment,
    segmentNZFromString,
    segmentNZToString,
    unsafeSegmentNZFromString,
    unsafeSegmentNZToString,
    parseSegmentNZ,
    printSegmentNZ,
    segmentNZNCFromString,
    segmentNZNCToString,
    unsafeSegmentNZNCFromString,
    unsafeSegmentNZNCToString,
    parseSegmentNZNC,
    printSegmentNZNC,
    segmentChar,
    segmentNCChar,
    eqPathSegment,
    ordPathSegment,
    showPathSegment,
    eqPathSegmentNZ,
    ordPathSegmentNZ,
    showPathSegmentNZ,
    eqPathSegmentNZNC,
    ordPathSegmentNZNC,
    showPathSegmentNZNC
};
