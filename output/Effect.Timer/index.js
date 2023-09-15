// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Ord from "../Data.Ord/index.js";
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordInt);
var TimeoutId = function (x) {
    return x;
};
var IntervalId = function (x) {
    return x;
};
var setTimeout = $foreign.setTimeoutImpl;
var setInterval = $foreign.setIntervalImpl;
var eqTimeoutId = {
    eq: function (x) {
        return function (y) {
            return x === y;
        };
    }
};
var ordTimeoutId = {
    compare: function (x) {
        return function (y) {
            return compare(x)(y);
        };
    },
    Eq0: function () {
        return eqTimeoutId;
    }
};
var eqIntervalId = {
    eq: function (x) {
        return function (y) {
            return x === y;
        };
    }
};
var ordIntervalId = {
    compare: function (x) {
        return function (y) {
            return compare(x)(y);
        };
    },
    Eq0: function () {
        return eqIntervalId;
    }
};
var clearTimeout = $foreign.clearTimeoutImpl;
var clearInterval = $foreign.clearIntervalImpl;
export {
    setTimeout,
    clearTimeout,
    setInterval,
    clearInterval,
    eqTimeoutId,
    ordTimeoutId,
    eqIntervalId,
    ordIntervalId
};
