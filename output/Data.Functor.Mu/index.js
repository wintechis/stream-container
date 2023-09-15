// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_TacitString from "../Data.TacitString/index.js";
var In = function (x) {
    return x;
};
var unroll = function (v) {
    return v;
};
var showMu = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (dictFunctor) {
        var mapFlipped = Data_Functor.mapFlipped(dictFunctor);
        return {
            show: function (v) {
                return show(mapFlipped(v)((function () {
                    var $36 = Data_Show.show(showMu(dictShow)(dictFunctor));
                    return function ($37) {
                        return Data_TacitString.hush($36($37));
                    };
                })()));
            }
        };
    };
};
var semigroupMu = function (dictAlt) {
    var alt = Control_Alt.alt(dictAlt);
    return {
        append: function (v) {
            return function (v1) {
                return alt(v)(v1);
            };
        }
    };
};
var roll = In;
var transMu = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (η) {
        var $38 = map(transMu(dictFunctor)(η));
        return function ($39) {
            return roll($38(η(unroll($39))));
        };
    };
};
var newtypeMu = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidMu = function (dictPlus) {
    var semigroupMu1 = semigroupMu(dictPlus.Alt0());
    return {
        mempty: Control_Plus.empty(dictPlus),
        Semigroup0: function () {
            return semigroupMu1;
        }
    };
};
var eqMu = function (dictEq1) {
    var eq1 = Data_Eq.eq1(dictEq1);
    return {
        eq: function (v) {
            return function (v1) {
                return eq1(eqMu(dictEq1))(v)(v1);
            };
        }
    };
};
var ordMu = function (dictEq1) {
    var eqMu1 = eqMu(dictEq1);
    return function (dictOrd1) {
        var compare1 = Data_Ord.compare1(dictOrd1);
        return {
            compare: function (v) {
                return function (v1) {
                    return compare1(ordMu(dictEq1)(dictOrd1))(v)(v1);
                };
            },
            Eq0: function () {
                return eqMu1;
            }
        };
    };
};
export {
    In,
    roll,
    unroll,
    transMu,
    newtypeMu,
    eqMu,
    ordMu,
    showMu,
    semigroupMu,
    monoidMu
};