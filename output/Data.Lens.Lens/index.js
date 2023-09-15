// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Lens_Internal_Indexed from "../Data.Lens.Internal.Indexed/index.js";
import * as Data_Lens_Internal_Shop from "../Data.Lens.Internal.Shop/index.js";
import * as Data_Lens_Types from "../Data.Lens.Types/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Profunctor from "../Data.Profunctor/index.js";
import * as Data_Profunctor_Strong from "../Data.Profunctor.Strong/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var lift2 = /* #__PURE__ */ Control_Apply.lift2(Control_Apply.applyFn);
var un = /* #__PURE__ */ Data_Newtype.un();
var withLens = function (l) {
    return function (f) {
        var v = l(new Data_Lens_Internal_Shop.Shop(identity, function (v1) {
            return function (b) {
                return b;
            };
        }));
        return f(v.value0)(v.value1);
    };
};
var withIndexedLens = function (l) {
    return function (f) {
        var v = l(new Data_Lens_Internal_Shop.Shop(identity, function (v1) {
            return function (b) {
                return b;
            };
        }));
        return f(v.value0)(v.value1);
    };
};
var lensStore = function (l) {
    return withLens(l)(lift2(Data_Tuple.Tuple.create));
};
var lens$prime = function (to) {
    return function (dictStrong) {
        var dimap = Data_Profunctor.dimap(dictStrong.Profunctor0());
        var first = Data_Profunctor_Strong.first(dictStrong);
        return function (pab) {
            return dimap(to)(function (v) {
                return v.value1(v.value0);
            })(first(pab));
        };
    };
};
var lens = function (get) {
    return function (set) {
        return function (dictStrong) {
            return lens$prime(function (s) {
                return new Data_Tuple.Tuple(get(s), function (b) {
                    return set(s)(b);
                });
            })(dictStrong);
        };
    };
};
var ilens$prime = function (to) {
    return function (dictStrong) {
        var dimap = Data_Profunctor.dimap(dictStrong.Profunctor0());
        var first = Data_Profunctor_Strong.first(dictStrong);
        return function (pab) {
            return dimap(to)(function (v) {
                return v.value1(v.value0);
            })(first(un(Data_Lens_Internal_Indexed.Indexed)(pab)));
        };
    };
};
var ilens = function (get) {
    return function (set) {
        return function (dictStrong) {
            return ilens$prime(function (s) {
                return new Data_Tuple.Tuple(get(s), function (b) {
                    return set(s)(b);
                });
            })(dictStrong);
        };
    };
};
var cloneLens = function (l) {
    return function (dictStrong) {
        return withLens(l)(function (x) {
            return function (y) {
                return function (p) {
                    return lens(x)(y)(dictStrong)(p);
                };
            };
        });
    };
};
var cloneIndexedLens = function (l) {
    return function (dictStrong) {
        return withIndexedLens(l)(function (x) {
            return function (y) {
                return function (p) {
                    return ilens(x)(y)(dictStrong)(p);
                };
            };
        });
    };
};
export {
    lens,
    lens$prime,
    withLens,
    cloneLens,
    ilens,
    ilens$prime,
    withIndexedLens,
    cloneIndexedLens,
    lensStore
};
