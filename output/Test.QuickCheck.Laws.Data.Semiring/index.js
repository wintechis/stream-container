// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
import * as Test_QuickCheck_Arbitrary from "../Test.QuickCheck.Arbitrary/index.js";
import * as Test_QuickCheck_Gen from "../Test.QuickCheck.Gen/index.js";
var quickCheck$prime = /* #__PURE__ */ Test_QuickCheck["quickCheck$prime"](/* #__PURE__ */ Test_QuickCheck.testableGen(Test_QuickCheck.testableBoolean));
var lift3 = /* #__PURE__ */ Control_Apply.lift3(Test_QuickCheck_Gen.applyGen);
var map = /* #__PURE__ */ Data_Functor.map(Test_QuickCheck_Gen.functorGen);
var lift2 = /* #__PURE__ */ Control_Apply.lift2(Test_QuickCheck_Gen.applyGen);
var checkSemiringGen = function (dictSemiring) {
    var mul = Data_Semiring.mul(dictSemiring);
    var add = Data_Semiring.add(dictSemiring);
    var one = Data_Semiring.one(dictSemiring);
    var zero = Data_Semiring.zero(dictSemiring);
    return function (dictEq) {
        var eq = Data_Eq.eq(dictEq);
        return function (gen) {
            var rightDistribution = function (a) {
                return function (b) {
                    return function (c) {
                        return eq(mul(add(a)(b))(c))(add(mul(a)(c))(mul(b)(c)));
                    };
                };
            };
            var leftDistribution = function (a) {
                return function (b) {
                    return function (c) {
                        return eq(mul(a)(add(b)(c)))(add(mul(a)(b))(mul(a)(c)));
                    };
                };
            };
            var identityMultiplication = function (a) {
                return eq(mul(one)(a))(a) && eq(mul(a)(one))(a);
            };
            var identityAddition = function (a) {
                return eq(add(zero)(a))(a) && eq(add(a)(zero))(a);
            };
            var commutativeAddition = function (a) {
                return function (b) {
                    return eq(add(a)(b))(add(b)(a));
                };
            };
            var associativeMultiplication = function (a) {
                return function (b) {
                    return function (c) {
                        return eq(mul(mul(a)(b))(c))(mul(a)(mul(b)(c)));
                    };
                };
            };
            var associativeAddition = function (a) {
                return function (b) {
                    return function (c) {
                        return eq(add(add(a)(b))(c))(add(a)(add(b)(c)));
                    };
                };
            };
            var annihiliation = function (a) {
                return eq(mul(a)(zero))(zero) && eq(mul(zero)(a))(zero);
            };
            return function __do() {
                Effect_Console.log("Checking 'Associativity' law for Semiring addition")();
                quickCheck$prime(1000)(lift3(associativeAddition)(gen)(gen)(gen))();
                Effect_Console.log("Checking 'Identity' law for Semiring addition")();
                quickCheck$prime(1000)(map(identityAddition)(gen))();
                Effect_Console.log("Checking 'Commutative' law for Semiring addition")();
                quickCheck$prime(1000)(lift2(commutativeAddition)(gen)(gen))();
                Effect_Console.log("Checking 'Associativity' law for Semiring multiplication")();
                quickCheck$prime(1000)(lift3(associativeMultiplication)(gen)(gen)(gen))();
                Effect_Console.log("Checking 'Identity' law for Semiring multiplication")();
                quickCheck$prime(1000)(map(identityMultiplication)(gen))();
                Effect_Console.log("Checking 'Left distribution' law for Semiring")();
                quickCheck$prime(1000)(lift3(leftDistribution)(gen)(gen)(gen))();
                Effect_Console.log("Checking 'Right distribution' law for Semiring")();
                quickCheck$prime(1000)(lift3(rightDistribution)(gen)(gen)(gen))();
                Effect_Console.log("Checking 'Annihilation' law for Semiring")();
                return quickCheck$prime(1000)(map(annihiliation)(gen))();
            };
        };
    };
};
var checkSemiring = function (dictSemiring) {
    var checkSemiringGen1 = checkSemiringGen(dictSemiring);
    return function (dictArbitrary) {
        var arbitrary = Test_QuickCheck_Arbitrary.arbitrary(dictArbitrary);
        return function (dictEq) {
            var checkSemiringGen2 = checkSemiringGen1(dictEq);
            return function (v) {
                return checkSemiringGen2(arbitrary);
            };
        };
    };
};
export {
    checkSemiring,
    checkSemiringGen
};
