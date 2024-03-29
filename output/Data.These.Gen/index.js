// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Monad_Gen from "../Control.Monad.Gen/index.js";
import * as Control_Monad_Gen_Common from "../Control.Monad.Gen.Common/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_These from "../Data.These/index.js";
var genThese = function (dictMonadGen) {
    var Apply0 = ((dictMonadGen.Monad0()).Bind1()).Apply0();
    var apply = Control_Apply.apply(Apply0);
    var map = Data_Functor.map(Apply0.Functor0());
    var genMaybe = Control_Monad_Gen_Common.genMaybe(dictMonadGen);
    return function (dictMonadRec) {
        var filtered = Control_Monad_Gen.filtered(dictMonadRec)(dictMonadGen);
        return function (ga) {
            return function (gb) {
                return filtered(apply(map(Data_These.maybeThese)(genMaybe(ga)))(genMaybe(gb)));
            };
        };
    };
};
export {
    genThese
};
