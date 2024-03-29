// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as URI_Path_Segment from "../URI.Path.Segment/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Functor.functorArray);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var map1 = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var fromFoldable = /* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList);
var manyRec = /* #__PURE__ */ Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(/* #__PURE__ */ Data_Tuple.eqTuple(URI_Path_Segment.eqPathSegmentNZ)(/* #__PURE__ */ Data_Eq.eqArray(URI_Path_Segment.eqPathSegment))));
var compare = /* #__PURE__ */ Data_Ord.compare(/* #__PURE__ */ Data_Maybe.ordMaybe(/* #__PURE__ */ Data_Tuple.ordTuple(URI_Path_Segment.ordPathSegmentNZ)(/* #__PURE__ */ Data_Ord.ordArray(URI_Path_Segment.ordPathSegment))));
var PathAbsolute = function (x) {
    return x;
};
var print = function (v) {
    if (v instanceof Data_Maybe.Nothing) {
        return "/";
    };
    if (v instanceof Data_Maybe.Just && v.value0.value1.length === 0) {
        return "/" + URI_Path_Segment.printSegmentNZ(v.value0.value0);
    };
    if (v instanceof Data_Maybe.Just) {
        return "/" + (URI_Path_Segment.printSegmentNZ(v.value0.value0) + ("/" + Data_String_Common.joinWith("/")(map(URI_Path_Segment.printSegment)(v.value0.value1))));
    };
    throw new Error("Failed pattern match at URI.Path.Absolute (line 52, column 9 - line 61, column 53): " + [ v.constructor.name ]);
};
var parse = /* #__PURE__ */ bind(/* #__PURE__ */ Parsing_String["char"]("/"))(function () {
    return bind(Parsing_Combinators.optionMaybe(URI_Path_Segment.parseSegmentNZ))(function (v) {
        if (v instanceof Data_Maybe.Just) {
            return map1((function () {
                var $57 = Data_Tuple.Tuple.create(v.value0);
                return function ($58) {
                    return PathAbsolute(Data_Maybe.Just.create($57(fromFoldable($58))));
                };
            })())(manyRec(applySecond(Parsing_String["char"]("/"))(URI_Path_Segment.parseSegment)));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return pure(Data_Maybe.Nothing.value);
        };
        throw new Error("Failed pattern match at URI.Path.Absolute (line 40, column 34 - line 48, column 34): " + [ v.constructor.name ]);
    });
});
var genericPathAbsolute = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showPathAbsolute = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericPathAbsolute)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Maybe.showMaybe(/* #__PURE__ */ Data_Tuple.showTuple(URI_Path_Segment.showPathSegmentNZ)(/* #__PURE__ */ Data_Show.showArray(URI_Path_Segment.showPathSegment)))))({
        reflectSymbol: function () {
            return "PathAbsolute";
        }
    }))
};
var eqPathAbsolute = {
    eq: function (x) {
        return function (y) {
            return eq(x)(y);
        };
    }
};
var ordPathAbsolute = {
    compare: function (x) {
        return function (y) {
            return compare(x)(y);
        };
    },
    Eq0: function () {
        return eqPathAbsolute;
    }
};
export {
    PathAbsolute,
    parse,
    print,
    eqPathAbsolute,
    ordPathAbsolute,
    genericPathAbsolute,
    showPathAbsolute
};
