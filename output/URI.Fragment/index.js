// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_NonEmpty_CodeUnits from "../Data.String.NonEmpty.CodeUnits/index.js";
import * as Data_String_NonEmpty_Internal from "../Data.String.NonEmpty.Internal/index.js";
import * as JSURI from "../JSURI/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as URI_Common from "../URI.Common/index.js";
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showString);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var Fragment = function (x) {
    return x;
};
var unsafeToString = function (v) {
    return v;
};
var unsafeFromString = Fragment;
var toString = function (v) {
    return fromJust(JSURI["decodeURIComponent"](v));
};
var showFragment = {
    show: function (v) {
        return "(Fragment.unsafeFromString " + (show(v) + ")");
    }
};
var semigroupFragment = Data_Semigroup.semigroupString;
var print = function (v) {
    return "#" + v;
};
var ordFragment = Data_Ord.ordString;
var monoidFragment = Data_Monoid.monoidString;
var fragmentChar = /* #__PURE__ */ alt(URI_Common.unreserved)(/* #__PURE__ */ alt(URI_Common.subDelims)(/* #__PURE__ */ alt(/* #__PURE__ */ Parsing_String["char"](":"))(/* #__PURE__ */ alt(/* #__PURE__ */ Parsing_String["char"]("@"))(/* #__PURE__ */ alt(/* #__PURE__ */ Parsing_String["char"]("/"))(/* #__PURE__ */ Parsing_String["char"]("?"))))));
var fromString = /* #__PURE__ */ (function () {
    var $18 = URI_Common.printEncoded(fragmentChar);
    return function ($19) {
        return Fragment($18($19));
    };
})();
var parser = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT)(/* #__PURE__ */ Parsing_String["char"]("#"))(/* #__PURE__ */ map(/* #__PURE__ */ (function () {
    var $20 = Data_String_NonEmpty_Internal.joinWith(Data_List_Types.foldableList)("");
    return function ($21) {
        return Fragment($20($21));
    };
})())(/* #__PURE__ */ Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT)(/* #__PURE__ */ alt(URI_Common.pctEncoded)(/* #__PURE__ */ map(Data_String_NonEmpty_CodeUnits.singleton)(fragmentChar)))));
var eqFragment = Data_Eq.eqString;
export {
    fromString,
    toString,
    unsafeFromString,
    unsafeToString,
    parser,
    print,
    fragmentChar,
    eqFragment,
    ordFragment,
    semigroupFragment,
    monoidFragment,
    showFragment
};