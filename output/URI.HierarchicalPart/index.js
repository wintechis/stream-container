// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Lens_Internal_Wander from "../Data.Lens.Internal.Wander/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Parsing from "../Parsing/index.js";
import * as URI_Authority from "../URI.Authority/index.js";
import * as URI_Common from "../URI.Common/index.js";
import * as URI_Path from "../URI.Path/index.js";
import * as URI_Path_Absolute from "../URI.Path.Absolute/index.js";
import * as URI_Path_Rootless from "../URI.Path.Rootless/index.js";
var apply = /* #__PURE__ */ Control_Apply.apply(Parsing.applyParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var wrapParser = /* #__PURE__ */ URI_Common.wrapParser(Data_Identity.monadIdentity);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var HierarchicalPartAuthIsSymbol = {
    reflectSymbol: function () {
        return "HierarchicalPartAuth";
    }
};
var HierarchicalPartNoAuthIsSymbol = {
    reflectSymbol: function () {
        return "HierarchicalPartNoAuth";
    }
};
var HierarchicalPartAuth = /* #__PURE__ */ (function () {
    function HierarchicalPartAuth(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    HierarchicalPartAuth.create = function (value0) {
        return function (value1) {
            return new HierarchicalPartAuth(value0, value1);
        };
    };
    return HierarchicalPartAuth;
})();
var HierarchicalPartNoAuth = /* #__PURE__ */ (function () {
    function HierarchicalPartNoAuth(value0) {
        this.value0 = value0;
    };
    HierarchicalPartNoAuth.create = function (value0) {
        return new HierarchicalPartNoAuth(value0);
    };
    return HierarchicalPartNoAuth;
})();
var print = function (opts) {
    return function (v) {
        if (v instanceof HierarchicalPartAuth) {
            return URI_Authority.print(opts)(v.value0) + URI_Path.print(opts.printPath(v.value1));
        };
        if (v instanceof HierarchicalPartNoAuth) {
            return Data_Maybe.maybe("")((function () {
                var $146 = Data_Either.either(URI_Path_Absolute.print)(URI_Path_Rootless.print);
                return function ($147) {
                    return $146(opts.printHierPath($147));
                };
            })())(v.value0);
        };
        throw new Error("Failed pattern match at URI.HierarchicalPart (line 119, column 14 - line 123, column 80): " + [ v.constructor.name ]);
    };
};
var parser = function (opts) {
    var withAuth = apply(map(HierarchicalPartAuth.create)(URI_Authority.parser(opts)))(wrapParser(opts.parsePath)(URI_Path.parser));
    var noAuthPath = alt(map(Data_Maybe.Just.create)(wrapParser(function ($148) {
        return opts.parseHierPath(Data_Either.Left.create($148));
    })(URI_Path_Absolute.parse)))(alt(map(Data_Maybe.Just.create)(wrapParser(function ($149) {
        return opts.parseHierPath(Data_Either.Right.create($149));
    })(URI_Path_Rootless.parse)))(pure(Data_Maybe.Nothing.value)));
    var withoutAuth = map(HierarchicalPartNoAuth.create)(noAuthPath);
    return alt(withAuth)(withoutAuth);
};
var genericHierarchicalPart = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new HierarchicalPartAuth(x.value0.value0, x.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new HierarchicalPartNoAuth(x.value0);
        };
        throw new Error("Failed pattern match at URI.HierarchicalPart (line 50, column 1 - line 50, column 101): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof HierarchicalPartAuth) {
            return new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1));
        };
        if (x instanceof HierarchicalPartNoAuth) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at URI.HierarchicalPart (line 50, column 1 - line 50, column 101): " + [ x.constructor.name ]);
    }
};
var genericShow = /* #__PURE__ */ Data_Show_Generic.genericShow(genericHierarchicalPart);
var showHierarchicalPart = function (dictShow) {
    var showAuthority = URI_Authority.showAuthority(dictShow);
    return function (dictShow1) {
        var genericShowArgsProduct = Data_Show_Generic.genericShowArgsProduct(Data_Show_Generic.genericShowArgsArgument(showAuthority(dictShow1)));
        return function (dictShow2) {
            var genericShowSum = Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(genericShowArgsProduct(Data_Show_Generic.genericShowArgsArgument(dictShow2)))(HierarchicalPartAuthIsSymbol));
            return function (dictShow3) {
                return {
                    show: genericShow(genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Maybe.showMaybe(dictShow3)))(HierarchicalPartNoAuthIsSymbol)))
                };
            };
        };
    };
};
var eqHierarchicalPart = function (dictEq) {
    var eqAuthority = URI_Authority.eqAuthority(dictEq);
    return function (dictEq1) {
        var eq = Data_Eq.eq(eqAuthority(dictEq1));
        return function (dictEq2) {
            var eq1 = Data_Eq.eq(dictEq2);
            return function (dictEq3) {
                var eq2 = Data_Eq.eq(Data_Maybe.eqMaybe(dictEq3));
                return {
                    eq: function (x) {
                        return function (y) {
                            if (x instanceof HierarchicalPartAuth && y instanceof HierarchicalPartAuth) {
                                return eq(x.value0)(y.value0) && eq1(x.value1)(y.value1);
                            };
                            if (x instanceof HierarchicalPartNoAuth && y instanceof HierarchicalPartNoAuth) {
                                return eq2(x.value0)(y.value0);
                            };
                            return false;
                        };
                    }
                };
            };
        };
    };
};
var ordHierarchicalPart = function (dictOrd) {
    var ordAuthority = URI_Authority.ordAuthority(dictOrd);
    var eqHierarchicalPart1 = eqHierarchicalPart(dictOrd.Eq0());
    return function (dictOrd1) {
        var compare = Data_Ord.compare(ordAuthority(dictOrd1));
        var eqHierarchicalPart2 = eqHierarchicalPart1(dictOrd1.Eq0());
        return function (dictOrd2) {
            var compare1 = Data_Ord.compare(dictOrd2);
            var eqHierarchicalPart3 = eqHierarchicalPart2(dictOrd2.Eq0());
            return function (dictOrd3) {
                var compare2 = Data_Ord.compare(Data_Maybe.ordMaybe(dictOrd3));
                var eqHierarchicalPart4 = eqHierarchicalPart3(dictOrd3.Eq0());
                return {
                    compare: function (x) {
                        return function (y) {
                            if (x instanceof HierarchicalPartAuth && y instanceof HierarchicalPartAuth) {
                                var v = compare(x.value0)(y.value0);
                                if (v instanceof Data_Ordering.LT) {
                                    return Data_Ordering.LT.value;
                                };
                                if (v instanceof Data_Ordering.GT) {
                                    return Data_Ordering.GT.value;
                                };
                                return compare1(x.value1)(y.value1);
                            };
                            if (x instanceof HierarchicalPartAuth) {
                                return Data_Ordering.LT.value;
                            };
                            if (y instanceof HierarchicalPartAuth) {
                                return Data_Ordering.GT.value;
                            };
                            if (x instanceof HierarchicalPartNoAuth && y instanceof HierarchicalPartNoAuth) {
                                return compare2(x.value0)(y.value0);
                            };
                            throw new Error("Failed pattern match at URI.HierarchicalPart (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
                        };
                    },
                    Eq0: function () {
                        return eqHierarchicalPart4;
                    }
                };
            };
        };
    };
};
var _path = function (dictWander) {
    return Data_Lens_Internal_Wander.wander(dictWander)(function (dictApplicative) {
        var map1 = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        var pure1 = Control_Applicative.pure(dictApplicative);
        return function (f) {
            return function (v) {
                if (v instanceof HierarchicalPartAuth) {
                    return map1(HierarchicalPartAuth.create(v.value0))(f(v.value1));
                };
                return pure1(v);
            };
        };
    });
};
var _hierPath = function (dictWander) {
    return Data_Lens_Internal_Wander.wander(dictWander)(function (dictApplicative) {
        var map1 = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        var pure1 = Control_Applicative.pure(dictApplicative);
        return function (f) {
            return function (v) {
                if (v instanceof HierarchicalPartNoAuth) {
                    return map1(HierarchicalPartNoAuth.create)(f(v.value0));
                };
                return pure1(v);
            };
        };
    });
};
var _authority = function (dictWander) {
    return Data_Lens_Internal_Wander.wander(dictWander)(function (dictApplicative) {
        var map1 = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        var pure1 = Control_Applicative.pure(dictApplicative);
        return function (f) {
            return function (v) {
                if (v instanceof HierarchicalPartAuth) {
                    return map1(Data_Function.flip(HierarchicalPartAuth.create)(v.value1))(f(v.value0));
                };
                return pure1(v);
            };
        };
    });
};
export {
    HierarchicalPartAuth,
    HierarchicalPartNoAuth,
    parser,
    print,
    _authority,
    _path,
    _hierPath,
    eqHierarchicalPart,
    ordHierarchicalPart,
    genericHierarchicalPart,
    showHierarchicalPart
};
export {
    Authority,
    IPv4Address,
    IPv6Address,
    NameAddress,
    _IPv4Address,
    _IPv6Address,
    _NameAddress,
    _hosts,
    _userInfo
} from "../URI.Authority/index.js";
export {
    Path
} from "../URI.Path/index.js";
export {
    PathAbsolute
} from "../URI.Path.Absolute/index.js";
export {
    PathRootless
} from "../URI.Path.Rootless/index.js";
