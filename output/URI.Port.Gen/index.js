// Generated by purs version 0.15.10
import * as Control_Monad_Gen_Class from "../Control.Monad.Gen.Class/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as URI_Port from "../URI.Port/index.js";
var genPort = function (dictMonadGen) {
    return Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(URI_Port.unsafeFromInt)(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(65535));
};
export {
    genPort
};
