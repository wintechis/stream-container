// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Bitraversable from "../Data.Bitraversable/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var foldr = /* #__PURE__ */ Data_Foldable.foldr(Data_Foldable.foldableMaybe);
var foldl = /* #__PURE__ */ Data_Foldable.foldl(Data_Foldable.foldableMaybe);
var foldMap = /* #__PURE__ */ Data_Foldable.foldMap(Data_Foldable.foldableMaybe);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var This = /* #__PURE__ */ (function () {
    function This(value0) {
        this.value0 = value0;
    };
    This.create = function (value0) {
        return new This(value0);
    };
    return This;
})();
var That = /* #__PURE__ */ (function () {
    function That(value0) {
        this.value0 = value0;
    };
    That.create = function (value0) {
        return new That(value0);
    };
    return That;
})();
var Both = /* #__PURE__ */ (function () {
    function Both(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Both.create = function (value0) {
        return function (value1) {
            return new Both(value0, value1);
        };
    };
    return Both;
})();
var thisOrBoth = function (v) {
    return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
            return new This(v);
        };
        if (v1 instanceof Data_Maybe.Just) {
            return new Both(v, v1.value0);
        };
        throw new Error("Failed pattern match at Data.These (line 110, column 1 - line 110, column 52): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var $$this = function (v) {
    if (v instanceof This) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
};
var theseRight = function (v) {
    if (v instanceof Both) {
        return new Data_Maybe.Just(v.value1);
    };
    if (v instanceof That) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
};
var theseLeft = function (v) {
    if (v instanceof Both) {
        return new Data_Maybe.Just(v.value0);
    };
    if (v instanceof This) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
};
var these = function (v) {
    return function (v1) {
        return function (v2) {
            return function (v3) {
                if (v3 instanceof This) {
                    return v(v3.value0);
                };
                if (v3 instanceof That) {
                    return v1(v3.value0);
                };
                if (v3 instanceof Both) {
                    return v2(v3.value0)(v3.value1);
                };
                throw new Error("Failed pattern match at Data.These (line 105, column 1 - line 105, column 79): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name ]);
            };
        };
    };
};
var thatOrBoth = function (v) {
    return function (v1) {
        if (v1 instanceof Data_Maybe.Nothing) {
            return new That(v);
        };
        if (v1 instanceof Data_Maybe.Just) {
            return new Both(v1.value0, v);
        };
        throw new Error("Failed pattern match at Data.These (line 114, column 1 - line 114, column 52): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var that = function (v) {
    if (v instanceof That) {
        return new Data_Maybe.Just(v.value0);
    };
    return Data_Maybe.Nothing.value;
};
var swap = /* #__PURE__ */ (function () {
    return these(That.create)(This.create)(Data_Function.flip(Both.create));
})();
var showThese = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (dictShow1) {
        var show1 = Data_Show.show(dictShow1);
        return {
            show: function (v) {
                if (v instanceof This) {
                    return "(This " + (show(v.value0) + ")");
                };
                if (v instanceof That) {
                    return "(That " + (show1(v.value0) + ")");
                };
                if (v instanceof Both) {
                    return "(Both " + (show(v.value0) + (" " + (show1(v.value1) + ")")));
                };
                throw new Error("Failed pattern match at Data.These (line 98, column 1 - line 101, column 63): " + [ v.constructor.name ]);
            }
        };
    };
};
var semigroupThese = function (dictSemigroup) {
    var append1 = Data_Semigroup.append(dictSemigroup);
    return function (dictSemigroup1) {
        var append2 = Data_Semigroup.append(dictSemigroup1);
        return {
            append: function (v) {
                return function (v1) {
                    if (v instanceof This && v1 instanceof This) {
                        return new This(append1(v.value0)(v1.value0));
                    };
                    if (v instanceof This && v1 instanceof That) {
                        return new Both(v.value0, v1.value0);
                    };
                    if (v instanceof This && v1 instanceof Both) {
                        return new Both(append1(v.value0)(v1.value0), v1.value1);
                    };
                    if (v instanceof That && v1 instanceof This) {
                        return new Both(v1.value0, v.value0);
                    };
                    if (v instanceof That && v1 instanceof That) {
                        return new That(append2(v.value0)(v1.value0));
                    };
                    if (v instanceof That && v1 instanceof Both) {
                        return new Both(v1.value0, append2(v.value0)(v1.value1));
                    };
                    if (v instanceof Both && v1 instanceof This) {
                        return new Both(append1(v.value0)(v1.value0), v.value1);
                    };
                    if (v instanceof Both && v1 instanceof That) {
                        return new Both(v.value0, append2(v.value1)(v1.value0));
                    };
                    if (v instanceof Both && v1 instanceof Both) {
                        return new Both(append1(v.value0)(v1.value0), append2(v.value1)(v1.value1));
                    };
                    throw new Error("Failed pattern match at Data.These (line 23, column 1 - line 32, column 56): " + [ v.constructor.name, v1.constructor.name ]);
                };
            }
        };
    };
};
var maybeThese = function (v) {
    return function (v1) {
        if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Nothing) {
            return new Data_Maybe.Just(new This(v.value0));
        };
        if (v instanceof Data_Maybe.Nothing && v1 instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new That(v1.value0));
        };
        if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new Both(v.value0, v1.value0));
        };
        if (v instanceof Data_Maybe.Nothing && v1 instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Data.These (line 120, column 14 - line 124, column 30): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var isThis = function ($308) {
    return Data_Maybe.isJust($$this($308));
};
var isThat = function ($309) {
    return Data_Maybe.isJust(that($309));
};
var functorThese = {
    map: function (v) {
        return function (v1) {
            if (v1 instanceof Both) {
                return new Both(v1.value0, v(v1.value1));
            };
            if (v1 instanceof That) {
                return new That(v(v1.value0));
            };
            if (v1 instanceof This) {
                return new This(v1.value0);
            };
            throw new Error("Failed pattern match at Data.These (line 34, column 1 - line 37, column 26): " + [ v.constructor.name, v1.constructor.name ]);
        };
    }
};
var map = /* #__PURE__ */ Data_Functor.map(functorThese);
var invariantThese = {
    imap: /* #__PURE__ */ Data_Functor_Invariant.imapF(functorThese)
};
var fromThese = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof This) {
                return new Data_Tuple.Tuple(v2.value0, v1);
            };
            if (v2 instanceof That) {
                return new Data_Tuple.Tuple(v, v2.value0);
            };
            if (v2 instanceof Both) {
                return new Data_Tuple.Tuple(v2.value0, v2.value1);
            };
            throw new Error("Failed pattern match at Data.These (line 130, column 1 - line 130, column 58): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var foldableThese = {
    foldr: function (f) {
        return function (z) {
            var $310 = foldr(f)(z);
            return function ($311) {
                return $310(theseRight($311));
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            var $312 = foldl(f)(z);
            return function ($313) {
                return $312(theseRight($313));
            };
        };
    },
    foldMap: function (dictMonoid) {
        var foldMap1 = foldMap(dictMonoid);
        return function (f) {
            var $314 = foldMap1(f);
            return function ($315) {
                return $314(theseRight($315));
            };
        };
    }
};
var traversableThese = {
    traverse: function (dictApplicative) {
        var pure = Control_Applicative.pure(dictApplicative);
        var map1 = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        return function (v) {
            return function (v1) {
                if (v1 instanceof This) {
                    return pure(new This(v1.value0));
                };
                if (v1 instanceof That) {
                    return map1(That.create)(v(v1.value0));
                };
                if (v1 instanceof Both) {
                    return map1(Both.create(v1.value0))(v(v1.value1));
                };
                throw new Error("Failed pattern match at Data.These (line 47, column 1 - line 53, column 37): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    sequence: function (dictApplicative) {
        var pure = Control_Applicative.pure(dictApplicative);
        var map1 = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        return function (v) {
            if (v instanceof This) {
                return pure(new This(v.value0));
            };
            if (v instanceof That) {
                return map1(That.create)(v.value0);
            };
            if (v instanceof Both) {
                return map1(Both.create(v.value0))(v.value1);
            };
            throw new Error("Failed pattern match at Data.These (line 47, column 1 - line 53, column 37): " + [ v.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorThese;
    },
    Foldable1: function () {
        return foldableThese;
    }
};
var extendEither = {
    extend: function (v) {
        return function (v1) {
            if (v1 instanceof This) {
                return new This(v1.value0);
            };
            return map(Data_Function["const"](v(v1)))(v1);
        };
    },
    Functor0: function () {
        return functorThese;
    }
};
var eqThese = function (dictEq) {
    var eq = Data_Eq.eq(dictEq);
    return function (dictEq1) {
        var eq1 = Data_Eq.eq(dictEq1);
        return {
            eq: function (x) {
                return function (y) {
                    if (x instanceof This && y instanceof This) {
                        return eq(x.value0)(y.value0);
                    };
                    if (x instanceof That && y instanceof That) {
                        return eq1(x.value0)(y.value0);
                    };
                    if (x instanceof Both && y instanceof Both) {
                        return eq(x.value0)(y.value0) && eq1(x.value1)(y.value1);
                    };
                    return false;
                };
            }
        };
    };
};
var ordThese = function (dictOrd) {
    var compare = Data_Ord.compare(dictOrd);
    var eqThese1 = eqThese(dictOrd.Eq0());
    return function (dictOrd1) {
        var compare1 = Data_Ord.compare(dictOrd1);
        var eqThese2 = eqThese1(dictOrd1.Eq0());
        return {
            compare: function (x) {
                return function (y) {
                    if (x instanceof This && y instanceof This) {
                        return compare(x.value0)(y.value0);
                    };
                    if (x instanceof This) {
                        return Data_Ordering.LT.value;
                    };
                    if (y instanceof This) {
                        return Data_Ordering.GT.value;
                    };
                    if (x instanceof That && y instanceof That) {
                        return compare1(x.value0)(y.value0);
                    };
                    if (x instanceof That) {
                        return Data_Ordering.LT.value;
                    };
                    if (y instanceof That) {
                        return Data_Ordering.GT.value;
                    };
                    if (x instanceof Both && y instanceof Both) {
                        var v = compare(x.value0)(y.value0);
                        if (v instanceof Data_Ordering.LT) {
                            return Data_Ordering.LT.value;
                        };
                        if (v instanceof Data_Ordering.GT) {
                            return Data_Ordering.GT.value;
                        };
                        return compare1(x.value1)(y.value1);
                    };
                    throw new Error("Failed pattern match at Data.These (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
                };
            },
            Eq0: function () {
                return eqThese2;
            }
        };
    };
};
var both = function (v) {
    if (v instanceof Both) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
    };
    return Data_Maybe.Nothing.value;
};
var isBoth = function ($316) {
    return Data_Maybe.isJust(both($316));
};
var bifunctorThese = {
    bimap: function (v) {
        return function (v1) {
            return function (v2) {
                if (v2 instanceof This) {
                    return new This(v(v2.value0));
                };
                if (v2 instanceof That) {
                    return new That(v1(v2.value0));
                };
                if (v2 instanceof Both) {
                    return new Both(v(v2.value0), v1(v2.value1));
                };
                throw new Error("Failed pattern match at Data.These (line 55, column 1 - line 58, column 42): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
            };
        };
    }
};
var bifoldableThese = {
    bifoldr: function (f) {
        return function (g) {
            return function (z) {
                return these(function (v) {
                    return f(v)(z);
                })(function (v) {
                    return g(v)(z);
                })(function (x) {
                    return function (y) {
                        return f(x)(g(y)(z));
                    };
                });
            };
        };
    },
    bifoldl: function (f) {
        return function (g) {
            return function (z) {
                return these(function (v) {
                    return f(z)(v);
                })(function (v) {
                    return g(z)(v);
                })(function (x) {
                    return function (y) {
                        return g(f(z)(x))(y);
                    };
                });
            };
        };
    },
    bifoldMap: function (dictMonoid) {
        var append1 = Data_Semigroup.append(dictMonoid.Semigroup0());
        return function (f) {
            return function (g) {
                return these(f)(g)(function (x) {
                    return function (y) {
                        return append1(f(x))(g(y));
                    };
                });
            };
        };
    }
};
var bitraversableThese = {
    bitraverse: function (dictApplicative) {
        var Apply0 = dictApplicative.Apply0();
        var map1 = Data_Functor.map(Apply0.Functor0());
        var apply = Control_Apply.apply(Apply0);
        return function (v) {
            return function (v1) {
                return function (v2) {
                    if (v2 instanceof This) {
                        return map1(This.create)(v(v2.value0));
                    };
                    if (v2 instanceof That) {
                        return map1(That.create)(v1(v2.value0));
                    };
                    if (v2 instanceof Both) {
                        return apply(map1(Both.create)(v(v2.value0)))(v1(v2.value1));
                    };
                    throw new Error("Failed pattern match at Data.These (line 65, column 1 - line 69, column 44): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
                };
            };
        };
    },
    bisequence: function (dictApplicative) {
        return Data_Bitraversable.bitraverse(bitraversableThese)(dictApplicative)(identity)(identity);
    },
    Bifunctor0: function () {
        return bifunctorThese;
    },
    Bifoldable1: function () {
        return bifoldableThese;
    }
};
var assoc = function (v) {
    if (v instanceof This && v.value0 instanceof This) {
        return new This(v.value0.value0);
    };
    if (v instanceof This && v.value0 instanceof That) {
        return new That(new This(v.value0.value0));
    };
    if (v instanceof This && v.value0 instanceof Both) {
        return new Both(v.value0.value0, new This(v.value0.value1));
    };
    if (v instanceof That) {
        return new That(new That(v.value0));
    };
    if (v instanceof Both && v.value0 instanceof This) {
        return new Both(v.value0.value0, new That(v.value1));
    };
    if (v instanceof Both && v.value0 instanceof That) {
        return new That(new Both(v.value0.value0, v.value1));
    };
    if (v instanceof Both && v.value0 instanceof Both) {
        return new Both(v.value0.value0, new Both(v.value0.value1, v.value1));
    };
    throw new Error("Failed pattern match at Data.These (line 184, column 9 - line 191, column 41): " + [ v.constructor.name ]);
};
var applyThese = function (dictSemigroup) {
    var append1 = Data_Semigroup.append(dictSemigroup);
    return {
        apply: function (v) {
            return function (v1) {
                if (v instanceof This) {
                    return new This(v.value0);
                };
                if (v instanceof That && v1 instanceof This) {
                    return new This(v1.value0);
                };
                if (v instanceof That && v1 instanceof That) {
                    return new That(v.value0(v1.value0));
                };
                if (v instanceof That && v1 instanceof Both) {
                    return new Both(v1.value0, v.value0(v1.value1));
                };
                if (v instanceof Both && v1 instanceof This) {
                    return new This(append1(v.value0)(v1.value0));
                };
                if (v instanceof Both && v1 instanceof That) {
                    return new Both(v.value0, v.value1(v1.value0));
                };
                if (v instanceof Both && v1 instanceof Both) {
                    return new Both(append1(v.value0)(v1.value0), v.value1(v1.value1));
                };
                throw new Error("Failed pattern match at Data.These (line 71, column 1 - line 78, column 52): " + [ v.constructor.name, v1.constructor.name ]);
            };
        },
        Functor0: function () {
            return functorThese;
        }
    };
};
var bindThese = function (dictSemigroup) {
    var append1 = Data_Semigroup.append(dictSemigroup);
    var applyThese1 = applyThese(dictSemigroup);
    return {
        bind: function (v) {
            return function (v1) {
                if (v instanceof This) {
                    return new This(v.value0);
                };
                if (v instanceof That) {
                    return v1(v.value0);
                };
                if (v instanceof Both) {
                    var v2 = v1(v.value1);
                    if (v2 instanceof This) {
                        return new This(append1(v.value0)(v2.value0));
                    };
                    if (v2 instanceof That) {
                        return new Both(v.value0, v2.value0);
                    };
                    if (v2 instanceof Both) {
                        return new Both(append1(v.value0)(v2.value0), v2.value1);
                    };
                    throw new Error("Failed pattern match at Data.These (line 87, column 5 - line 90, column 34): " + [ v2.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.These (line 83, column 1 - line 90, column 34): " + [ v.constructor.name, v1.constructor.name ]);
            };
        },
        Apply0: function () {
            return applyThese1;
        }
    };
};
var applicativeThese = function (dictSemigroup) {
    var applyThese1 = applyThese(dictSemigroup);
    return {
        pure: That.create,
        Apply0: function () {
            return applyThese1;
        }
    };
};
var monadThese = function (dictSemigroup) {
    var applicativeThese1 = applicativeThese(dictSemigroup);
    var bindThese1 = bindThese(dictSemigroup);
    return {
        Applicative0: function () {
            return applicativeThese1;
        },
        Bind1: function () {
            return bindThese1;
        }
    };
};
export {
    This,
    That,
    Both,
    these,
    thisOrBoth,
    thatOrBoth,
    maybeThese,
    fromThese,
    theseLeft,
    theseRight,
    $$this as this,
    that,
    both,
    isThis,
    isThat,
    isBoth,
    swap,
    assoc,
    eqThese,
    ordThese,
    semigroupThese,
    functorThese,
    invariantThese,
    foldableThese,
    traversableThese,
    bifunctorThese,
    bifoldableThese,
    bitraversableThese,
    applyThese,
    applicativeThese,
    bindThese,
    monadThese,
    extendEither,
    showThese
};
