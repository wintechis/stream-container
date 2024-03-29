// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as URI_Path_Segment from "../URI.Path.Segment/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var Path = function (x) {
    return x;
};
var semigroupPath = Data_Semigroup.semigroupArray;
var print = function (v) {
    if (Data_Array["null"](v)) {
        return "";
    };
    if (Data_Boolean.otherwise) {
        return "/" + Data_String_Common.joinWith("/")(map(URI_Path_Segment.unsafeSegmentToString)(v));
    };
    throw new Error("Failed pattern match at URI.Path (line 35, column 1 - line 35, column 24): " + [ v.constructor.name ]);
};
var parser = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT)(/* #__PURE__ */ (function () {
    var $23 = Data_Array.fromFoldable(Data_List_Types.foldableList);
    return function ($24) {
        return Path($23($24));
    };
})())(/* #__PURE__ */ Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT)(/* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT)(/* #__PURE__ */ Parsing_String["char"]("/"))(URI_Path_Segment.parseSegment)));
var ordPath = /* #__PURE__ */ Data_Ord.ordArray(URI_Path_Segment.ordPathSegment);
var monoidPath = Data_Monoid.monoidArray;
var genericPath = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showPath = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericPath)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Show.showArray(URI_Path_Segment.showPathSegment)))({
        reflectSymbol: function () {
            return "Path";
        }
    }))
};
var eqPath = /* #__PURE__ */ Data_Eq.eqArray(URI_Path_Segment.eqPathSegment);
export {
    Path,
    parser,
    print,
    eqPath,
    ordPath,
    semigroupPath,
    monoidPath,
    genericPath,
    showPath
};
