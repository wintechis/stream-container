// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Lens_Lens from "../Data.Lens.Lens/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as URI_Common from "../URI.Common/index.js";
import * as URI_HierarchicalPart from "../URI.HierarchicalPart/index.js";
import * as URI_Query from "../URI.Query/index.js";
import * as URI_Scheme from "../URI.Scheme/index.js";
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var applyFirst = /* #__PURE__ */ Control_Apply.applyFirst(Parsing.applyParserT);
var apply = /* #__PURE__ */ Control_Apply.apply(Parsing.applyParserT);
var map1 = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var wrapParser = /* #__PURE__ */ URI_Common.wrapParser(Data_Identity.monadIdentity);
var genericShowArgsProduct = /* #__PURE__ */ Data_Show_Generic.genericShowArgsProduct(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(URI_Scheme.showScheme));
var AbsoluteURIIsSymbol = {
    reflectSymbol: function () {
        return "AbsoluteURI";
    }
};
var eq = /* #__PURE__ */ Data_Eq.eq(URI_Scheme.eqScheme);
var compare = /* #__PURE__ */ Data_Ord.compare(URI_Scheme.ordScheme);
var AbsoluteURI = /* #__PURE__ */ (function () {
    function AbsoluteURI(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    AbsoluteURI.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new AbsoluteURI(value0, value1, value2);
            };
        };
    };
    return AbsoluteURI;
})();
var print = function (opts) {
    return function (v) {
        return Data_String_Common.joinWith("")(Data_Array.catMaybes([ new Data_Maybe.Just(URI_Scheme.print(v.value0)), new Data_Maybe.Just(URI_HierarchicalPart.print(opts)(v.value1)), map(function ($153) {
            return URI_Query.print(opts.printQuery($153));
        })(v.value2) ]));
    };
};
var parser = function (opts) {
    return applyFirst(apply(apply(map1(AbsoluteURI.create)(URI_Scheme.parser))(URI_HierarchicalPart.parser(opts)))(Parsing_Combinators.optionMaybe(wrapParser(opts.parseQuery)(URI_Query.parser))))(Parsing_String.eof);
};
var genericAbsoluteURI = {
    to: function (x) {
        return new AbsoluteURI(x.value0, x.value1.value0, x.value1.value1);
    },
    from: function (x) {
        return new Data_Generic_Rep.Product(x.value0, new Data_Generic_Rep.Product(x.value1, x.value2));
    }
};
var genericShow = /* #__PURE__ */ Data_Show_Generic.genericShow(genericAbsoluteURI);
var showAbsoluteURI = function (dictShow) {
    var showHierarchicalPart = URI_HierarchicalPart.showHierarchicalPart(dictShow);
    return function (dictShow1) {
        var showHierarchicalPart1 = showHierarchicalPart(dictShow1);
        return function (dictShow2) {
            var showHierarchicalPart2 = showHierarchicalPart1(dictShow2);
            return function (dictShow3) {
                var genericShowArgsProduct1 = Data_Show_Generic.genericShowArgsProduct(Data_Show_Generic.genericShowArgsArgument(showHierarchicalPart2(dictShow3)));
                return function (dictShow4) {
                    return {
                        show: genericShow(Data_Show_Generic.genericShowConstructor(genericShowArgsProduct(genericShowArgsProduct1(Data_Show_Generic.genericShowArgsArgument(Data_Maybe.showMaybe(dictShow4)))))(AbsoluteURIIsSymbol))
                    };
                };
            };
        };
    };
};
var eqAbsoluteURI = function (dictEq) {
    var eqHierarchicalPart = URI_HierarchicalPart.eqHierarchicalPart(dictEq);
    return function (dictEq1) {
        var eqHierarchicalPart1 = eqHierarchicalPart(dictEq1);
        return function (dictEq2) {
            var eqHierarchicalPart2 = eqHierarchicalPart1(dictEq2);
            return function (dictEq3) {
                var eq1 = Data_Eq.eq(eqHierarchicalPart2(dictEq3));
                return function (dictEq4) {
                    var eq2 = Data_Eq.eq(Data_Maybe.eqMaybe(dictEq4));
                    return {
                        eq: function (x) {
                            return function (y) {
                                return eq(x.value0)(y.value0) && eq1(x.value1)(y.value1) && eq2(x.value2)(y.value2);
                            };
                        }
                    };
                };
            };
        };
    };
};
var ordAbsoluteURI = function (dictOrd) {
    var ordHierarchicalPart = URI_HierarchicalPart.ordHierarchicalPart(dictOrd);
    var eqAbsoluteURI1 = eqAbsoluteURI(dictOrd.Eq0());
    return function (dictOrd1) {
        var ordHierarchicalPart1 = ordHierarchicalPart(dictOrd1);
        var eqAbsoluteURI2 = eqAbsoluteURI1(dictOrd1.Eq0());
        return function (dictOrd2) {
            var ordHierarchicalPart2 = ordHierarchicalPart1(dictOrd2);
            var eqAbsoluteURI3 = eqAbsoluteURI2(dictOrd2.Eq0());
            return function (dictOrd3) {
                var compare1 = Data_Ord.compare(ordHierarchicalPart2(dictOrd3));
                var eqAbsoluteURI4 = eqAbsoluteURI3(dictOrd3.Eq0());
                return function (dictOrd4) {
                    var compare2 = Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd4));
                    var eqAbsoluteURI5 = eqAbsoluteURI4(dictOrd4.Eq0());
                    return {
                        compare: function (x) {
                            return function (y) {
                                var v = compare(x.value0)(y.value0);
                                if (v instanceof Data_Ordering.LT) {
                                    return Data_Ordering.LT.value;
                                };
                                if (v instanceof Data_Ordering.GT) {
                                    return Data_Ordering.GT.value;
                                };
                                var v1 = compare1(x.value1)(y.value1);
                                if (v1 instanceof Data_Ordering.LT) {
                                    return Data_Ordering.LT.value;
                                };
                                if (v1 instanceof Data_Ordering.GT) {
                                    return Data_Ordering.GT.value;
                                };
                                return compare2(x.value2)(y.value2);
                            };
                        },
                        Eq0: function () {
                            return eqAbsoluteURI5;
                        }
                    };
                };
            };
        };
    };
};
var _scheme = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value0;
    })(function (v) {
        return function (s) {
            return new AbsoluteURI(s, v.value1, v.value2);
        };
    })(dictStrong);
};
var _query = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value2;
    })(function (v) {
        return function (q) {
            return new AbsoluteURI(v.value0, v.value1, q);
        };
    })(dictStrong);
};
var _hierPart = function (dictStrong) {
    return Data_Lens_Lens.lens(function (v) {
        return v.value1;
    })(function (v) {
        return function (h) {
            return new AbsoluteURI(v.value0, h, v.value2);
        };
    })(dictStrong);
};
export {
    AbsoluteURI,
    parser,
    print,
    _scheme,
    _hierPart,
    _query,
    eqAbsoluteURI,
    ordAbsoluteURI,
    genericAbsoluteURI,
    showAbsoluteURI
};
export {
    Authority,
    HierarchicalPartAuth,
    HierarchicalPartNoAuth,
    IPv4Address,
    IPv6Address,
    NameAddress,
    Path,
    PathAbsolute,
    PathRootless,
    _IPv4Address,
    _IPv6Address,
    _NameAddress,
    _authority,
    _hierPath,
    _hosts,
    _path,
    _userInfo
} from "../URI.HierarchicalPart/index.js";