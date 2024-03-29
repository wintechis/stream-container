// Generated by purs version 0.15.10
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
import * as Test_QuickCheck_Arbitrary from "../Test.QuickCheck.Arbitrary/index.js";
import * as Test_QuickCheck_Gen from "../Test.QuickCheck.Gen/index.js";
var quickCheck$prime = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(Test_QuickCheck.testableBoolean));
var map = /* #__PURE__ */ Data_Functor.map(Test_QuickCheck_Gen.functorGen);
var checkBooleanAlgebraGen = function (dictBooleanAlgebra) {
    var HeytingAlgebra0 = dictBooleanAlgebra.HeytingAlgebra0();
    var disj = Data_HeytingAlgebra.disj(HeytingAlgebra0);
    var not = Data_HeytingAlgebra.not(HeytingAlgebra0);
    var tt = Data_HeytingAlgebra.tt(HeytingAlgebra0);
    return function (dictEq) {
        var eq = Data_Eq.eq(dictEq);
        return function (gen) {
            var excludedMiddle = function (a) {
                return eq(disj(a)(not(a)))(tt);
            };
            return function __do() {
                Effect_Console.log("Checking 'Excluded middle' law for BooleanAlgebra")();
                return quickCheck$prime(1000)(map(excludedMiddle)(gen))();
            };
        };
    };
};
var checkBooleanAlgebra = function (dictArbitrary) {
    var arbitrary = Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary);
    return function (dictBooleanAlgebra) {
        var checkBooleanAlgebraGen1 = checkBooleanAlgebraGen(dictBooleanAlgebra);
        return function (dictEq) {
            var checkBooleanAlgebraGen2 = checkBooleanAlgebraGen1(dictEq);
            return function (v) {
                return checkBooleanAlgebraGen2(arbitrary);
            };
        };
    };
};
export {
    checkBooleanAlgebra,
    checkBooleanAlgebraGen
};
