// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Extend from "../Control.Extend/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
import * as Test_QuickCheck_Arbitrary from "../Test.QuickCheck.Arbitrary/index.js";
import * as Test_QuickCheck_Gen from "../Test.QuickCheck.Gen/index.js";
import * as Test_QuickCheck_Laws from "../Test.QuickCheck.Laws/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(Test_QuickCheck_Laws.eqB);
var quickCheck$prime = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(Test_QuickCheck.testableBoolean));
var map = /* #__PURE__ */ Data_Functor.map(Test_QuickCheck_Gen.functorGen);
var lift2 = /* #__PURE__ */ Control_Apply.lift2(Test_QuickCheck_Gen.applyGen);
var checkComonadGen = function (dictComonad) {
    var extract = Control_Comonad.extract(dictComonad);
    var extend = Control_Extend.extend(dictComonad.Extend0());
    return function (dictEq) {
        var eq1 = Data_Eq.eq(dictEq);
        return function (gen) {
            return function (cogen) {
                var rightIdentity = function (f) {
                    return function (x) {
                        return eq(extract(extend(f)(x)))(f(x));
                    };
                };
                var leftIdentity = function (x) {
                    return eq1(extend(extract)(x))(x);
                };
                return function __do() {
                    Effect_Console.log("Checking 'Left identity' law for Comonad")();
                    quickCheck$prime(1000)(map(leftIdentity)(gen))();
                    Effect_Console.log("Checking 'Right identity' law for Comonad")();
                    return quickCheck$prime(1000)(lift2(rightIdentity)(cogen)(gen))();
                };
            };
        };
    };
};
var checkComonad = function (dictComonad) {
    var checkComonadGen1 = checkComonadGen(dictComonad);
    return function (dictArbitrary) {
        var arbitrary = Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary);
        return function (dictCoarbitrary) {
            var arbitrary1 = Test_QuickCheck_Arbitrary.arbitrary(Test_QuickCheck_Arbitrary.arbFunction(dictCoarbitrary)(Test_QuickCheck_Laws.arbitraryB));
            return function (dictEq) {
                var checkComonadGen2 = checkComonadGen1(dictEq);
                return function (v) {
                    return checkComonadGen2(arbitrary)(arbitrary1);
                };
            };
        };
    };
};
export {
    checkComonad,
    checkComonadGen
};
