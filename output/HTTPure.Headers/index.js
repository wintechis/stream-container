// Generated by purs version 0.15.10
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_FoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_String_CaseInsensitive from "../Data.String.CaseInsensitive/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_TraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Effect from "../Effect/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
import * as HTTPure_Lookup from "../HTTPure.Lookup/index.js";
import * as Node_HTTP from "../Node.HTTP/index.js";
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var $$void = /* #__PURE__ */ Data_Functor["void"](Effect.functorEffect);
var traverseWithIndex = /* #__PURE__ */ Data_TraversableWithIndex.traverseWithIndex(Data_Map_Internal.traversableWithIndexMap)(Effect.applicativeEffect);
var foldMapWithIndex = /* #__PURE__ */ Data_FoldableWithIndex.foldMapWithIndex(Data_Map_Internal.foldableWithIndexMap)(Data_Monoid.monoidString);
var union = /* #__PURE__ */ Data_Map_Internal.union(Data_String_CaseInsensitive.ordCaseInsensitiveString);
var lookup = /* #__PURE__ */ HTTPure_Lookup.lookup(HTTPure_Lookup.lookupMapCaseInsensitiveString);
var Headers = function (x) {
    return x;
};
var write = function (response) {
    return function (v) {
        var writeField = function (key) {
            return function (value) {
                return Node_HTTP.setHeader(response)(unwrap(key))(value);
            };
        };
        return $$void(traverseWithIndex(writeField)(v));
    };
};
var toString = function (v) {
    var showField = function (key) {
        return function (value) {
            return unwrap(key) + (": " + (value + "\x0a"));
        };
    };
    return foldMapWithIndex(showField)(v) + "\x0a";
};
var semigroupHeaders = {
    append: function (v) {
        return function (v1) {
            return union(v1)(v);
        };
    }
};
var newtypeHeaders = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidHeaders = {
    mempty: Data_Map_Internal.empty,
    Semigroup0: function () {
        return semigroupHeaders;
    }
};
var mempty = /* #__PURE__ */ Data_Monoid.mempty(monoidHeaders);
var lookupHeaders = {
    lookup: function (v) {
        return function (key) {
            return lookup(v)(key);
        };
    }
};
var header = function (key) {
    var $43 = Data_Map_Internal.singleton(key);
    return function ($44) {
        return Headers($43($44));
    };
};
var headers = /* #__PURE__ */ Data_Foldable.foldMap(Data_Foldable.foldableArray)(monoidHeaders)(/* #__PURE__ */ Data_Tuple.uncurry(header));
var read = /* #__PURE__ */ (function () {
    var header$prime = function (key) {
        if (Data_String_Common.toLower(key) === "set-cookie") {
            return Data_Function["const"](mempty);
        };
        if (Data_Boolean.otherwise) {
            return header(key);
        };
        throw new Error("Failed pattern match at HTTPure.Headers (line 64, column 3 - line 64, column 41): " + [ key.constructor.name ]);
    };
    var $45 = Foreign_Object.foldMap(monoidHeaders)(header$prime);
    return function ($46) {
        return $45(Node_HTTP.requestHeaders($46));
    };
})();
var genericHeaders = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showHeaders = {
    show: /* #__PURE__ */ Data_Show_Generic.genericShow(genericHeaders)(/* #__PURE__ */ Data_Show_Generic.genericShowConstructor(/* #__PURE__ */ Data_Show_Generic.genericShowArgsArgument(/* #__PURE__ */ Data_Map_Internal.showMap(Data_String_CaseInsensitive.showCaseInsensitiveString)(Data_Show.showString)))({
        reflectSymbol: function () {
            return "Headers";
        }
    }))
};
var eqHeaders = /* #__PURE__ */ Data_Map_Internal.eqMap(Data_String_CaseInsensitive.eqCaseInsensitiveString)(Data_Eq.eqString);
var empty = Data_Map_Internal.empty;
export {
    Headers,
    empty,
    headers,
    header,
    read,
    toString,
    write,
    newtypeHeaders,
    genericHeaders,
    lookupHeaders,
    showHeaders,
    eqHeaders,
    semigroupHeaders,
    monoidHeaders
};
