// Generated by purs version 0.15.10
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
import * as Test_QuickCheck_Arbitrary from "../Test.QuickCheck.Arbitrary/index.js";
import * as Test_QuickCheck_Gen from "../Test.QuickCheck.Gen/index.js";
import * as Test_QuickCheck_Laws from "../Test.QuickCheck.Laws/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var quickCheck$prime = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(Test_QuickCheck.testableBoolean));
var map = /* #__PURE__ */ Data_Functor.map(Test_QuickCheck_Gen.functorGen);
var quickCheck$prime1 = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(/* #__PURE__ */ Test_QuickCheck.testableFunction(/* #__PURE__ */ Test_QuickCheck_Arbitrary.arbFunction(Test_QuickCheck_Laws.coarbitraryB)(Test_QuickCheck_Laws.arbitraryA))(/* #__PURE__ */ Test_QuickCheck.testableFunction(/* #__PURE__ */ Test_QuickCheck_Arbitrary.arbFunction(Test_QuickCheck_Laws.coarbitraryA)(Test_QuickCheck_Laws.arbitraryB))(Test_QuickCheck.testableBoolean))));
var checkFunctorGen = function (dictFunctor) {
    var map1 = Data_Functor.map(dictFunctor);
    return function (dictEq) {
        var eq = Data_Eq.eq(dictEq);
        return function (gen) {
            var identity1 = function (f) {
                return eq(map1(identity)(f))(f);
            };
            var composition = function (x) {
                return function (f) {
                    return function (g) {
                        return eq(map1(function ($30) {
                            return f(g($30));
                        })(x))((function (v) {
                            return map1(f)(v);
                        })((function (v) {
                            return map1(g)(v);
                        })(x)));
                    };
                };
            };
            return function __do() {
                Effect_Console.log("Checking 'Identity' law for Functor")();
                quickCheck$prime(1000)(map(identity1)(gen))();
                Effect_Console.log("Checking 'Composition' law for Functor")();
                return quickCheck$prime1(1000)(map(composition)(gen))();
            };
        };
    };
};
var checkFunctor = function (dictFunctor) {
    var checkFunctorGen1 = checkFunctorGen(dictFunctor);
    return function (dictArbitrary) {
        var arbitrary = Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary);
        return function (dictEq) {
            var checkFunctorGen2 = checkFunctorGen1(dictEq);
            return function (v) {
                return checkFunctorGen2(arbitrary);
            };
        };
    };
};
export {
    checkFunctor,
    checkFunctorGen
};