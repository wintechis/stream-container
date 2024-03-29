// Generated by purs version 0.15.10
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
import * as URI_Common from "../URI.Common/index.js";
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var apply = /* #__PURE__ */ Control_Apply.apply(Parsing.applyParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var applyFirst = /* #__PURE__ */ Control_Apply.applyFirst(Parsing.applyParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var apply1 = /* #__PURE__ */ Control_Apply.apply(Data_Maybe.applyMaybe);
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordInt);
var IPv4Address = /* #__PURE__ */ (function () {
    function IPv4Address(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    IPv4Address.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new IPv4Address(value0, value1, value2, value3);
                };
            };
        };
    };
    return IPv4Address;
})();
var toInt = function (s) {
    var v = Data_Int.fromString(s);
    if (v instanceof Data_Maybe.Just && (v.value0 >= 0 && v.value0 <= 255)) {
        return new Data_Either.Right(v.value0);
    };
    return new Data_Either.Left("Invalid IPv4 address octet");
};
var showIPv4Address = {
    show: function (v) {
        return "(IPv4Address.unsafeFromInts " + (show(v.value0) + (" " + (show(v.value1) + (" " + (show(v.value2) + (" " + (show(v.value3) + ")")))))));
    }
};
var print = function (v) {
    return show(v.value0) + ("." + (show(v.value1) + ("." + (show(v.value2) + ("." + show(v.value3))))));
};
var nzDigit = /* #__PURE__ */ Parsing_String.satisfy(function (c) {
    return c >= "1" && c <= "9";
});
var octet = /* #__PURE__ */ URI_Common.wrapParser(Data_Identity.monadIdentity)(toInt)(/* #__PURE__ */ alt(/* #__PURE__ */ Parsing_Combinators["try"](/* #__PURE__ */ apply(/* #__PURE__ */ apply(/* #__PURE__ */ map(function (x) {
    return function (y) {
        return function (z) {
            return Data_String_CodeUnits.fromCharArray([ x, y, z ]);
        };
    };
})(nzDigit))(Parsing_String_Basic.digit))(Parsing_String_Basic.digit)))(/* #__PURE__ */ alt(/* #__PURE__ */ Parsing_Combinators["try"](/* #__PURE__ */ apply(/* #__PURE__ */ map(function (x) {
    return function (y) {
        return Data_String_CodeUnits.fromCharArray([ x, y ]);
    };
})(nzDigit))(Parsing_String_Basic.digit)))(/* #__PURE__ */ map(Data_String_CodeUnits.singleton)(Parsing_String_Basic.digit))));
var parser = /* #__PURE__ */ bind(/* #__PURE__ */ applyFirst(octet)(/* #__PURE__ */ Parsing_String["char"](".")))(function (o1) {
    return bind(applyFirst(octet)(Parsing_String["char"](".")))(function (o2) {
        return bind(applyFirst(octet)(Parsing_String["char"](".")))(function (o3) {
            return bind(octet)(function (o4) {
                return pure(new IPv4Address(o1, o2, o3, o4));
            });
        });
    });
});
var fromInts = function (o1) {
    return function (o2) {
        return function (o3) {
            return function (o4) {
                var check = function (i) {
                    if (i >= 0 && i <= 255) {
                        return new Data_Maybe.Just(i);
                    };
                    if (Data_Boolean.otherwise) {
                        return Data_Maybe.Nothing.value;
                    };
                    throw new Error("Failed pattern match at URI.Host.IPv4Address (line 38, column 3 - line 38, column 28): " + [ i.constructor.name ]);
                };
                return apply1(apply1(apply1(map1(IPv4Address.create)(check(o1)))(check(o2)))(check(o3)))(check(o4));
            };
        };
    };
};
var unsafeFromInts = function (o1) {
    return function (o2) {
        return function (o3) {
            return function (o4) {
                var v = fromInts(o1)(o2)(o3)(o4);
                if (v instanceof Data_Maybe.Just) {
                    return v.value0;
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Partial_Unsafe.unsafeCrashWith("IPv4Address octet was out of range");
                };
                throw new Error("Failed pattern match at URI.Host.IPv4Address (line 51, column 3 - line 53, column 68): " + [ v.constructor.name ]);
            };
        };
    };
};
var eqIPv4Address = {
    eq: function (x) {
        return function (y) {
            return x.value0 === y.value0 && x.value1 === y.value1 && x.value2 === y.value2 && x.value3 === y.value3;
        };
    }
};
var ordIPv4Address = {
    compare: function (x) {
        return function (y) {
            var v = compare(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v1 = compare(x.value1)(y.value1);
            if (v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v2 = compare(x.value2)(y.value2);
            if (v2 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v2 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return compare(x.value3)(y.value3);
        };
    },
    Eq0: function () {
        return eqIPv4Address;
    }
};
export {
    fromInts,
    unsafeFromInts,
    parser,
    print,
    eqIPv4Address,
    ordIPv4Address,
    showIPv4Address
};
