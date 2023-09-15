// Generated by purs version 0.15.10
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
var Tagged = function (x) {
    return x;
};
var taggedProfunctor = {
    dimap: function (v) {
        return function (g) {
            return function (v1) {
                return g(v1);
            };
        };
    }
};
var taggedCostrong = {
    unfirst: function (v) {
        return v.value0;
    },
    unsecond: function (v) {
        return v.value1;
    },
    Profunctor0: function () {
        return taggedProfunctor;
    }
};
var taggedClosed = {
    closed: function (v) {
        return Data_Function["const"](v);
    },
    Profunctor0: function () {
        return taggedProfunctor;
    }
};
var taggedChoice = {
    left: function (v) {
        return new Data_Either.Left(v);
    },
    right: function (v) {
        return new Data_Either.Right(v);
    },
    Profunctor0: function () {
        return taggedProfunctor;
    }
};
var newtypeTagged = {
    Coercible0: function () {
        return undefined;
    }
};
var functorTagged = {
    map: function (f) {
        return function (m) {
            return f(m);
        };
    }
};
var foldableTagged = {
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    },
    foldr: function (f) {
        return function (b) {
            return function (v) {
                return f(v)(b);
            };
        };
    },
    foldl: function (f) {
        return function (b) {
            return function (v) {
                return f(b)(v);
            };
        };
    }
};
var traversableTagged = {
    sequence: function (dictApplicative) {
        var map = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        return function (v) {
            return map(Tagged)(v);
        };
    },
    traverse: function (dictApplicative) {
        var map = Data_Functor.map((dictApplicative.Apply0()).Functor0());
        return function (f) {
            return function (v) {
                return map(Tagged)(f(v));
            };
        };
    },
    Functor0: function () {
        return functorTagged;
    },
    Foldable1: function () {
        return foldableTagged;
    }
};
var eqTagged = function (dictEq) {
    var eq = Data_Eq.eq(dictEq);
    return {
        eq: function (x) {
            return function (y) {
                return eq(x)(y);
            };
        }
    };
};
var ordTagged = function (dictOrd) {
    var compare = Data_Ord.compare(dictOrd);
    var eqTagged1 = eqTagged(dictOrd.Eq0());
    return {
        compare: function (x) {
            return function (y) {
                return compare(x)(y);
            };
        },
        Eq0: function () {
            return eqTagged1;
        }
    };
};
var eq1Tagged = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqTagged(dictEq));
    }
};
var ord1Tagged = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordTagged(dictOrd));
    },
    Eq10: function () {
        return eq1Tagged;
    }
};
export {
    Tagged,
    newtypeTagged,
    eqTagged,
    eq1Tagged,
    ordTagged,
    ord1Tagged,
    functorTagged,
    taggedProfunctor,
    taggedChoice,
    taggedCostrong,
    taggedClosed,
    foldableTagged,
    traversableTagged
};
