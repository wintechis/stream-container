// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Function_Uncurried from "../Data.Function.Uncurried/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit);
var warn = function () {
    return {};
};
var traceTime = function () {
    return Data_Function_Uncurried.runFn2($foreign["_traceTime"]);
};
var trace = function () {
    return function (a) {
        return function (k) {
            return $foreign["_trace"](a, k);
        };
    };
};
var trace1 = /* #__PURE__ */ trace();
var traceM = function () {
    return function (dictMonad) {
        var discard1 = discard(dictMonad.Bind1());
        var pure = Control_Applicative.pure(dictMonad.Applicative0());
        return function (s) {
            return discard1(pure(Data_Unit.unit))(function () {
                return trace1(s)(function (v) {
                    return pure(Data_Unit.unit);
                });
            });
        };
    };
};
var spy = function () {
    return function (tag) {
        return function (a) {
            return $foreign["_spy"](tag, a);
        };
    };
};
var spy1 = /* #__PURE__ */ spy();
var spyWith = function () {
    return function (msg) {
        return function (f) {
            return function (a) {
                return Data_Function["const"](a)(spy1(msg)(f(a)));
            };
        };
    };
};
var $$debugger = function () {
    return function (f) {
        return $foreign["_debugger"](f);
    };
};
export {
    trace,
    traceM,
    traceTime,
    spy,
    spyWith,
    $$debugger as debugger,
    warn
};
