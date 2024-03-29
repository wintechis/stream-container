// Generated by purs version 0.15.10
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_String_CaseInsensitive from "../Data.String.CaseInsensitive/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
var lookup1 = /* #__PURE__ */ Data_Map_Internal.lookup(Data_String_CaseInsensitive.ordCaseInsensitiveString);
var lookupObject = {
    lookup: /* #__PURE__ */ Data_Function.flip(Foreign_Object.lookup)
};
var lookupMapCaseInsensitiveString = {
    lookup: function (set) {
        return function (key) {
            return lookup1(key)(set);
        };
    }
};
var lookupArray = {
    lookup: Data_Array.index
};
var lookup = function (dict) {
    return dict.lookup;
};
var has = function (dictLookup) {
    var lookup2 = lookup(dictLookup);
    return function (set) {
        return function (key) {
            return Data_Maybe.isJust(lookup2(set)(key));
        };
    };
};
var at = function (dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    return function (dictLookup) {
        var lookup2 = lookup(dictLookup);
        return function (set) {
            var $9 = Data_Maybe.fromMaybe(mempty);
            var $10 = lookup2(set);
            return function ($11) {
                return $9($10($11));
            };
        };
    };
};
export {
    at,
    has,
    lookup,
    lookupArray,
    lookupObject,
    lookupMapCaseInsensitiveString
};
