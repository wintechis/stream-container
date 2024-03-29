// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
import * as Test_QuickCheck_Arbitrary from "../Test.QuickCheck.Arbitrary/index.js";
import * as Test_QuickCheck_Gen from "../Test.QuickCheck.Gen/index.js";
var quickCheck$prime = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(Test_QuickCheck.testableBoolean));
var map = /* #__PURE__ */ Data_Functor.map(Test_QuickCheck_Gen.functorGen);
var lift2 = /* #__PURE__ */ Control_Apply.lift2(Test_QuickCheck_Gen.applyGen);
var lift3 = /* #__PURE__ */ Control_Apply.lift3(Test_QuickCheck_Gen.applyGen);
var checkOrdGen = function (dictOrd) {
    var lessThanOrEq = Data_Ord.lessThanOrEq(dictOrd);
    var Eq0 = dictOrd.Eq0();
    var eq = Data_Eq.eq(Eq0);
    var notEq = Data_Eq.notEq(Eq0);
    return function (gen) {
        var transitivity = function (a) {
            return function (b) {
                return function (c) {
                    var $18 = lessThanOrEq(a)(b) && lessThanOrEq(b)(c);
                    if ($18) {
                        return lessThanOrEq(a)(c);
                    };
                    return true;
                };
            };
        };
        var reflexivity = function (a) {
            return lessThanOrEq(a)(a);
        };
        var antisymmetry = function (a) {
            return function (b) {
                var $19 = lessThanOrEq(a)(b) && lessThanOrEq(b)(a);
                if ($19) {
                    return eq(a)(b);
                };
                return notEq(a)(b);
            };
        };
        return function __do() {
            Effect_Console.log("Checking 'Reflexivity' law for Ord")();
            quickCheck$prime(1000)(map(reflexivity)(gen))();
            Effect_Console.log("Checking 'Antisymmetry' law for Ord")();
            quickCheck$prime(1000)(lift2(antisymmetry)(gen)(gen))();
            Effect_Console.log("Checking 'Transitivity' law for Ord")();
            return quickCheck$prime(1000)(lift3(transitivity)(gen)(gen)(gen))();
        };
    };
};
var checkOrd = function (dictArbitrary) {
    var arbitrary = Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary);
    return function (dictOrd) {
        var checkOrdGen1 = checkOrdGen(dictOrd);
        return function (v) {
            return checkOrdGen1(arbitrary);
        };
    };
};
export {
    checkOrd,
    checkOrdGen
};
