// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
import * as Test_QuickCheck_Arbitrary from "../Test.QuickCheck.Arbitrary/index.js";
import * as Test_QuickCheck_Gen from "../Test.QuickCheck.Gen/index.js";
var quickCheck$prime = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(Test_QuickCheck.testableBoolean));
var lift2 = /* #__PURE__ */ Control_Apply.lift2(Test_QuickCheck_Gen.applyGen);
var checkFieldGen = function (dictField) {
    var mod = Data_EuclideanRing.mod(dictField.EuclideanRing0());
    var zero = Data_Semiring.zero(((dictField.DivisionRing1()).Ring0()).Semiring0());
    return function (dictEq) {
        var eq = Data_Eq.eq(dictEq);
        return function (gen) {
            var multiplicativeInverse = function (x) {
                return function (y) {
                    return eq(mod(x)(y))(zero);
                };
            };
            return function __do() {
                Effect_Console.log("Checking 'Non-zero multiplicative inverse' law for Field")();
                return quickCheck$prime(1000)(lift2(multiplicativeInverse)(gen)(gen))();
            };
        };
    };
};
var checkField = function (dictField) {
    var checkFieldGen1 = checkFieldGen(dictField);
    return function (dictArbitrary) {
        var arbitrary = Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary);
        return function (dictEq) {
            var checkFieldGen2 = checkFieldGen1(dictEq);
            return function (v) {
                return checkFieldGen2(arbitrary);
            };
        };
    };
};
export {
    checkField,
    checkFieldGen
};
