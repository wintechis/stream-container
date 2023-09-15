// Generated by purs version 0.15.10
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_String_Pattern from "../Data.String.Pattern/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Foreign_Object from "../Foreign.Object/index.js";
import * as HTTPure_Utils from "../HTTPure.Utils/index.js";
import * as Node_HTTP from "../Node.HTTP/index.js";
var read = /* #__PURE__ */ (function () {
    var split$prime = function ($5) {
        return Data_String_Common.split(Data_String_Pattern.Pattern($5));
    };
    var nonempty = Data_Array.filter(Data_Eq.notEq(Data_Eq.eqString)(""));
    var last = (function () {
        var $6 = Data_String_Common.joinWith("");
        var $7 = Data_Maybe.fromMaybe([  ]);
        return function ($8) {
            return $6($7(Data_Array.tail($8)));
        };
    })();
    var first = (function () {
        var $9 = Data_Maybe.fromMaybe("");
        return function ($10) {
            return $9(Data_Array.head($10));
        };
    })();
    var decode = function ($11) {
        return HTTPure_Utils.urlDecode(HTTPure_Utils.replacePlus($11));
    };
    var decodeKeyValue = Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple)(decode)(decode);
    var toTuple = function (item) {
        var itemParts = split$prime("=")(item);
        return decodeKeyValue(new Data_Tuple.Tuple(first(itemParts), last(itemParts)));
    };
    var toObject = (function () {
        var $12 = Foreign_Object.fromFoldable(Data_Foldable.foldableArray);
        var $13 = Data_Functor.map(Data_Functor.functorArray)(toTuple);
        return function ($14) {
            return $12($13($14));
        };
    })();
    var $15 = split$prime("&");
    var $16 = split$prime("?");
    return function ($17) {
        return toObject(nonempty($15(last($16(Node_HTTP.requestURL($17))))));
    };
})();
export {
    read
};