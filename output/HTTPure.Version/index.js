// Generated by purs version 0.15.10
import * as Node_HTTP from "../Node.HTTP/index.js";
var HTTP0_9 = /* #__PURE__ */ (function () {
    function HTTP0_9() {

    };
    HTTP0_9.value = new HTTP0_9();
    return HTTP0_9;
})();
var HTTP1_0 = /* #__PURE__ */ (function () {
    function HTTP1_0() {

    };
    HTTP1_0.value = new HTTP1_0();
    return HTTP1_0;
})();
var HTTP1_1 = /* #__PURE__ */ (function () {
    function HTTP1_1() {

    };
    HTTP1_1.value = new HTTP1_1();
    return HTTP1_1;
})();
var HTTP2_0 = /* #__PURE__ */ (function () {
    function HTTP2_0() {

    };
    HTTP2_0.value = new HTTP2_0();
    return HTTP2_0;
})();
var HTTP3_0 = /* #__PURE__ */ (function () {
    function HTTP3_0() {

    };
    HTTP3_0.value = new HTTP3_0();
    return HTTP3_0;
})();
var Other = /* #__PURE__ */ (function () {
    function Other(value0) {
        this.value0 = value0;
    };
    Other.create = function (value0) {
        return new Other(value0);
    };
    return Other;
})();
var showVersion = {
    show: function (v) {
        if (v instanceof HTTP0_9) {
            return "HTTP/0.9";
        };
        if (v instanceof HTTP1_0) {
            return "HTTP/1.0";
        };
        if (v instanceof HTTP1_1) {
            return "HTTP/1.1";
        };
        if (v instanceof HTTP2_0) {
            return "HTTP/2.0";
        };
        if (v instanceof HTTP3_0) {
            return "HTTP/3.0";
        };
        if (v instanceof Other) {
            return "HTTP/" + v.value0;
        };
        throw new Error("Failed pattern match at HTTPure.Version (line 25, column 1 - line 31, column 44): " + [ v.constructor.name ]);
    }
};
var read = function ($16) {
    return (function (v) {
        if (v === "0.9") {
            return HTTP0_9.value;
        };
        if (v === "1.0") {
            return HTTP1_0.value;
        };
        if (v === "1.1") {
            return HTTP1_1.value;
        };
        if (v === "2.0") {
            return HTTP2_0.value;
        };
        if (v === "3.0") {
            return HTTP3_0.value;
        };
        return new Other(v);
    })(Node_HTTP.httpVersion($16));
};
var eqVersion = {
    eq: function (x) {
        return function (y) {
            if (x instanceof HTTP0_9 && y instanceof HTTP0_9) {
                return true;
            };
            if (x instanceof HTTP1_0 && y instanceof HTTP1_0) {
                return true;
            };
            if (x instanceof HTTP1_1 && y instanceof HTTP1_1) {
                return true;
            };
            if (x instanceof HTTP2_0 && y instanceof HTTP2_0) {
                return true;
            };
            if (x instanceof HTTP3_0 && y instanceof HTTP3_0) {
                return true;
            };
            if (x instanceof Other && y instanceof Other) {
                return x.value0 === y.value0;
            };
            return false;
        };
    }
};
export {
    HTTP0_9,
    HTTP1_0,
    HTTP1_1,
    HTTP2_0,
    HTTP3_0,
    Other,
    read,
    eqVersion,
    showVersion
};
