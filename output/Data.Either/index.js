// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var Left = /* #__PURE__ */ (function () {
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    return Left;
})();
var Right = /* #__PURE__ */ (function () {
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    return Right;
})();
var showEither = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (dictShow1) {
        var show1 = Data_Show.show(dictShow1);
        return {
            show: function (v) {
                if (v instanceof Left) {
                    return "(Left " + (show(v.value0) + ")");
                };
                if (v instanceof Right) {
                    return "(Right " + (show1(v.value0) + ")");
                };
                throw new Error("Failed pattern match at Data.Either (line 173, column 1 - line 175, column 46): " + [ v.constructor.name ]);
            }
        };
    };
};
var note$prime = function (f) {
    return Data_Maybe["maybe$prime"](function ($138) {
        return Left.create(f($138));
    })(Right.create);
};
var note = function (a) {
    return Data_Maybe.maybe(new Left(a))(Right.create);
};
var genericEither = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new Left(x.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new Right(x.value0);
        };
        throw new Error("Failed pattern match at Data.Either (line 33, column 1 - line 33, column 56): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof Left) {
            return new Data_Generic_Rep.Inl(x.value0);
        };
        if (x instanceof Right) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at Data.Either (line 33, column 1 - line 33, column 56): " + [ x.constructor.name ]);
    }
};
var functorEither = {
    map: function (f) {
        return function (m) {
            if (m instanceof Left) {
                return new Left(m.value0);
            };
            if (m instanceof Right) {
                return new Right(f(m.value0));
            };
            throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [ m.constructor.name ]);
        };
    }
};
var map = /* #__PURE__ */ Data_Functor.map(functorEither);
var invariantEither = {
    imap: /* #__PURE__ */ Data_Functor_Invariant.imapF(functorEither)
};
var fromRight$prime = function (v) {
    return function (v1) {
        if (v1 instanceof Right) {
            return v1.value0;
        };
        return v(Data_Unit.unit);
    };
};
var fromRight = function (v) {
    return function (v1) {
        if (v1 instanceof Right) {
            return v1.value0;
        };
        return v;
    };
};
var fromLeft$prime = function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return v1.value0;
        };
        return v(Data_Unit.unit);
    };
};
var fromLeft = function (v) {
    return function (v1) {
        if (v1 instanceof Left) {
            return v1.value0;
        };
        return v;
    };
};
var extendEither = {
    extend: function (v) {
        return function (v1) {
            if (v1 instanceof Left) {
                return new Left(v1.value0);
            };
            return new Right(v(v1));
        };
    },
    Functor0: function () {
        return functorEither;
    }
};
var eqEither = function (dictEq) {
    var eq = Data_Eq.eq(dictEq);
    return function (dictEq1) {
        var eq1 = Data_Eq.eq(dictEq1);
        return {
            eq: function (x) {
                return function (y) {
                    if (x instanceof Left && y instanceof Left) {
                        return eq(x.value0)(y.value0);
                    };
                    if (x instanceof Right && y instanceof Right) {
                        return eq1(x.value0)(y.value0);
                    };
                    return false;
                };
            }
        };
    };
};
var ordEither = function (dictOrd) {
    var compare = Data_Ord.compare(dictOrd);
    var eqEither1 = eqEither(dictOrd.Eq0());
    return function (dictOrd1) {
        var compare1 = Data_Ord.compare(dictOrd1);
        var eqEither2 = eqEither1(dictOrd1.Eq0());
        return {
            compare: function (x) {
                return function (y) {
                    if (x instanceof Left && y instanceof Left) {
                        return compare(x.value0)(y.value0);
                    };
                    if (x instanceof Left) {
                        return Data_Ordering.LT.value;
                    };
                    if (y instanceof Left) {
                        return Data_Ordering.GT.value;
                    };
                    if (x instanceof Right && y instanceof Right) {
                        return compare1(x.value0)(y.value0);
                    };
                    throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
                };
            },
            Eq0: function () {
                return eqEither2;
            }
        };
    };
};
var eq1Either = function (dictEq) {
    var eqEither1 = eqEither(dictEq);
    return {
        eq1: function (dictEq1) {
            return Data_Eq.eq(eqEither1(dictEq1));
        }
    };
};
var ord1Either = function (dictOrd) {
    var ordEither1 = ordEither(dictOrd);
    var eq1Either1 = eq1Either(dictOrd.Eq0());
    return {
        compare1: function (dictOrd1) {
            return Data_Ord.compare(ordEither1(dictOrd1));
        },
        Eq10: function () {
            return eq1Either1;
        }
    };
};
var either = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Left) {
                return v(v2.value0);
            };
            if (v2 instanceof Right) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var hush = /* #__PURE__ */ (function () {
    return either(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create);
})();
var isLeft = /* #__PURE__ */ either(/* #__PURE__ */ Data_Function["const"](true))(/* #__PURE__ */ Data_Function["const"](false));
var isRight = /* #__PURE__ */ either(/* #__PURE__ */ Data_Function["const"](false))(/* #__PURE__ */ Data_Function["const"](true));
var choose = function (dictAlt) {
    var alt = Control_Alt.alt(dictAlt);
    var map1 = Data_Functor.map(dictAlt.Functor0());
    return function (a) {
        return function (b) {
            return alt(map1(Left.create)(a))(map1(Right.create)(b));
        };
    };
};
var boundedEither = function (dictBounded) {
    var bottom = Data_Bounded.bottom(dictBounded);
    var ordEither1 = ordEither(dictBounded.Ord0());
    return function (dictBounded1) {
        var ordEither2 = ordEither1(dictBounded1.Ord0());
        return {
            top: new Right(Data_Bounded.top(dictBounded1)),
            bottom: new Left(bottom),
            Ord0: function () {
                return ordEither2;
            }
        };
    };
};
var blush = /* #__PURE__ */ (function () {
    return either(Data_Maybe.Just.create)(Data_Function["const"](Data_Maybe.Nothing.value));
})();
var applyEither = {
    apply: function (v) {
        return function (v1) {
            if (v instanceof Left) {
                return new Left(v.value0);
            };
            if (v instanceof Right) {
                return map(v.value0)(v1);
            };
            throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorEither;
    }
};
var apply = /* #__PURE__ */ Control_Apply.apply(applyEither);
var bindEither = {
    bind: /* #__PURE__ */ either(function (e) {
        return function (v) {
            return new Left(e);
        };
    })(function (a) {
        return function (f) {
            return f(a);
        };
    }),
    Apply0: function () {
        return applyEither;
    }
};
var semigroupEither = function (dictSemigroup) {
    var append1 = Data_Semigroup.append(dictSemigroup);
    return {
        append: function (x) {
            return function (y) {
                return apply(map(append1)(x))(y);
            };
        }
    };
};
var applicativeEither = /* #__PURE__ */ (function () {
    return {
        pure: Right.create,
        Apply0: function () {
            return applyEither;
        }
    };
})();
var monadEither = {
    Applicative0: function () {
        return applicativeEither;
    },
    Bind1: function () {
        return bindEither;
    }
};
var altEither = {
    alt: function (v) {
        return function (v1) {
            if (v instanceof Left) {
                return v1;
            };
            return v;
        };
    },
    Functor0: function () {
        return functorEither;
    }
};
export {
    Left,
    Right,
    either,
    choose,
    isLeft,
    isRight,
    fromLeft,
    fromLeft$prime,
    fromRight,
    fromRight$prime,
    note,
    note$prime,
    hush,
    blush,
    functorEither,
    genericEither,
    invariantEither,
    applyEither,
    applicativeEither,
    altEither,
    bindEither,
    monadEither,
    extendEither,
    showEither,
    eqEither,
    eq1Either,
    ordEither,
    ord1Either,
    boundedEither,
    semigroupEither
};